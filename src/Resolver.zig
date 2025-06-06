const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const math = std.math;
const meta = std.meta;
const heap = std.heap;

const assert = std.debug.assert;

const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const RAst = @import("RAst.zig");
const InternPool = @import("InternPool.zig");
const Resolver = @This();

gpa: mem.Allocator,
arena_state: heap.ArenaAllocator,

ast: *const Ast,
ip: *InternPool,

nodes: RAst.Nodes,
string_bytes: std.ArrayListUnmanaged(u8),
extra_data: std.ArrayListUnmanaged(u32),

node_types: std.ArrayListUnmanaged(InternPool.Type),
scratch: std.ArrayListUnmanaged(u32),

files: std.ArrayListUnmanaged(File),
current_scope: *Scope,

flow: struct {
    src: Ast.Node.Index,
    value: InternPool.Index,
},
errors: std.ArrayListUnmanaged(RAst.Error),

pub const Ref = enum(u32) {
    _,
    pub fn initNode(node: RAst.Node.Index) Ref {
        return @enumFromInt(@intFromEnum(node));
    }

    pub inline fn initValue(value: InternPool.Index) Ref {
        return @enumFromInt(@intFromEnum(value) | (1 << 31));
    }

    pub inline fn toNode(r: Ref) ?RAst.Node.Index {
        if (@intFromEnum(r) >> 31 != 0) return null;
        return @enumFromInt(@as(u31, @truncate(@intFromEnum(r))));
    }

    pub inline fn toValue(r: Ref) ?InternPool.Index {
        if (@intFromEnum(r) >> 31 == 0) return null;
        return @enumFromInt(@as(u31, @truncate(@intFromEnum(r))));
    }

    fn convertToNode(
        ref: Ref,
        r: *Resolver,
        src_node: Ast.Node.Index,
    ) ResolveError!RAst.Node.Index {
        if (ref.toNode()) |n| return n;

        const value = ref.toValue().?;
        return r.convertValueToNode(src_node, value);
    }
};

const File = struct {
    src: []const u8,
    name: []const u8,
    dir: fs.Dir,
    args: []const Ref,

    const Index = enum(u32) { _ };
};

const Scope = enum {
    root,
    func,
    block,

    fn toFull(s: *Scope, T: type) *T {
        switch (T) {
            Fn => assert(s.* == .func or s.* == .root),
            Block => assert(s.* == .block),
            else => unreachable,
        }

        return @alignCast(@fieldParentPtr("tag", s));
    }

    inline fn locals(s: *Scope) *Locals {
        return switch (s.*) {
            .func, .root => &s.toFull(Fn).locals,
            .block => &s.toFull(Block).locals,
        };
    }

    fn parentFn(s: *Scope) *Fn {
        var scope = s;
        while (scope.* == .block) : (scope = s.toFull(Block).parent) {}

        return s.toFull(Fn);
    }

    const Locals = std.StringHashMapUnmanaged(Ref);

    const Fn = struct {
        tag: Scope,
        ret_ty: ?InternPool.Type,

        locals: Locals,

        const init: Fn = .{
            .tag = .func,
            .ret_ty = null,
            .locals = .empty,
        };
    };

    const Block = struct {
        tag: Scope = .block,

        parent: *Scope,
        locals: Locals,

        fn init(parent: *Scope) Block {
            return .{
                .tag = .block,
                .parent = parent,
                .locals = .empty,
            };
        }
    };
};

pub fn resolve(
    gpa: mem.Allocator,
    path: []const u8,
    arg_types: []const InternPool.Type,
) mem.Allocator.Error!RAst {
    var r: Resolver = .{
        .gpa = gpa,
        .arena_state = .init(gpa),

        .ast = ast,
        .ip = .init(gpa),

        .nodes = .empty,
        .string_bytes = .empty,
        .extra_data = .empty,

        .node_types = .empty,
        .scratch = .empty,

        .file = undefined,
        .current_scope = undefined,

        .flow = undefined,
        .errors = .empty,
    };
    defer {
        r.arena_state.deinit();
        r.node_types.deinit(gpa);
        r.scratch.deinit(gpa);
        r.errors.deinit(gpa);
    }

    const args = try gpa.alloc(arg_types.len, Ref);
    defer gpa.free(args);

    for (arg_types, 0..) |ty, i| {
        args[i] = try r.addNodeRef(ty, .{
            .tag = .arg,
            .data = .{ .arg = @intCast(i) },
        });
    }

    r.file = .{
        .dir = dir,
        .args = args,
    };

    const root_scope = try r.arena().create(Scope.Root);
    root_scope.* = .{
        .tag = .root,
        .ret_ty = null,
        .locals = .empty,
    };
    r.current_scope = &root_scope.tag;

    // RAst.StringIndex.empty
    try r.string_bytes.append(gpa, 0);

    // Reserved slots of RAst.ExtraIndex.
    const reserved_count = @typeInfo(RAst.ExtraIndex).@"enum".fields.len;
    try r.extra_data.ensureTotalCapacity(gpa, ast.nodes.len + reserved_count);
    r.extra_data.items.len += reserved_count;

    r.extra_data.items[0] = 0;
    r.maxLocals().* = 0;

    try r.nodes.ensureTotalCapacity(gpa, ast.nodes.len);
    _ = r.resolveBlock(ast.rootNodes()) catch |err| blk: switch (err) {
        error.Return => {
            const node = r.convertValueToNode(
                r.flow.src,
                r.flow.value,
            ) catch |e| switch (e) {
                error.OutOfMemory => |oom| return oom,
                else => break :blk,
            };

            break :blk r.addNode(.none, .{
                .tag = .ret,
                .data = .{ .node = node },
            });
        },
    };

    const ret_ty = r.current_scope.toFull(Scope.Fn).ret_ty;
    if (ret_ty) |ty| {
        r.extra_data.items[@intFromEnum(RAst.ExtraIndex.ret_type)] = @intFromEnum(ty);
    } else {
        r.extra_data.items[@intFromEnum(RAst.ExtraIndex.ret_type)] =
            @intFromEnum(InternPool.Type.none);
    }

    const errors_index: u32 = @intFromEnum(RAst.ExtraIndex.errors);
    if (r.errors.items.len == 0) {
        r.extra_data.items[errors_index] = 0;
    } else {
        try r.extra_data.ensureUnusedCapacity(
            gpa,
            1 + r.errors.items.len * @typeInfo(RAst.Error).@"struct".fields.len,
        );

        r.extra_data.items[errors_index] = r.addExtraAssumeCapacity(.{
            @as(u32, @intCast(r.errors.items.len)),
        });

        for (r.errors.items) |err| {
            _ = r.addExtraAssumeCapacity(err);
        }
    }

    return .{
        .nodes = r.nodes.toOwnedSlice(),
        .string_bytes = try r.string_bytes.toOwnedSlice(gpa),
        .extra_data = try r.extra_data.toOwnedSlice(gpa),
    };
}

fn maxLocals(r: *Resolver) *u32 {
    return &r.extra_data.items[@intFromEnum(RAst.ExtraIndex.max_locals)];
}

const ResolveError = error{ Fail, Return } || mem.Allocator.Error;

fn resolveBlock(
    r: *Resolver,
    nodes: []const Ast.Node.Index,
) (mem.Allocator.Error || error{Return})!Ref {
    const scratch_index: u32 = @intCast(r.scratch.items.len);
    for (nodes, 0..) |node, i| {
        const ref = r.resolveNode(node) catch |err| switch (err) {
            error.Fail => continue,
            error.Return => |e| {
                if (i != nodes.len - 1) try r.appendError(
                    nodes[i + 1],
                    "unreachable code",
                    .{},
                );

                if (scratch_index == r.scratch.items.len) return e;

                const expr = r.convertValueToNode(
                    r.flow.src,
                    r.flow.value,
                ) catch |er| switch (er) {
                    error.Fail => break,
                    error.OutOfMemory => |oom| return oom,
                };

                try r.appendScratchNode(.none, .{
                    .tag = .ret,
                    .data = .{ .node = expr },
                });

                break;
            },
            error.OutOfMemory => |oom| return oom,
        };

        const ref_ty = r.refType(ref);
        if (ref_ty != .none) {
            try r.appendError(
                node,
                "value of type {s} not discarded",
                .{@tagName(ref_ty)},
            );

            continue;
        }

        if (ref.toNode()) |r_node| {
            try r.scratch.append(r.gpa, @intFromEnum(r_node));
        }
    }

    const slice = try r.appendExtraFromScratch(RAst.Node.Index, scratch_index);
    return r.addNodeRef(.none, .{
        .tag = .block,
        .data = .{ .block = slice },
    });
}

fn resolveNode(r: *Resolver, node: Ast.Node.Index) ResolveError!Ref {
    switch (r.ast.full(node)) {
        .literal_int => |literal| {
            var big_int: math.big.int.Managed = try .init(r.gpa);
            defer big_int.deinit();

            big_int.setString(10, literal) catch |err| switch (err) {
                error.OutOfMemory => |oom| return oom,
                else => unreachable, // a literal_int is always a valid integer
            };

            const index = try r.ip.addBigInt(big_int.toConst());
            return .initValue(index);
        },
        .identifier => |identifier| {
            var scope: *Scope = r.current_scope;
            while (true) {
                const ref = scope.locals().get(identifier) orelse {
                    if (scope.* != .block) break;

                    scope = scope.toFull(Scope.Block).parent;
                    continue;
                };

                return ref;
            }

            return r.throw(node, "'{s}' not in scope", .{identifier});
        },
        .assign => |assign| {
            const ref = try r.resolveNode(assign.rhs);
            try r.current_scope.locals().put(r.arena(), assign.identifier, ref);

            return .initValue(.none);
        },
        // NOTE: probably remove the inline
        inline .add, .sub, .mul, .pow, .div => |binop, tag| {
            const lhs_ref = try r.resolveNode(binop.lhs);
            const rhs_ref = try r.resolveNode(binop.rhs);

            const lhs_ty = r.refType(lhs_ref);
            const rhs_ty = r.refType(rhs_ref);

            var res: math.big.int.Mutable = undefined;
            if (lhs_ty == .static_int and rhs_ty == .static_int) {
                const lhs = lhs_ref.toValue().?.unwrapStaticInt(r.ip);
                const rhs = rhs_ref.toValue().?.unwrapStaticInt(r.ip);

                if (tag == .pow) {
                    if (!rhs.positive) {
                        return r.throw(binop.rhs, "power cannot be negative", .{});
                    }

                    const power = rhs.toInt(u32) catch {
                        return r.throw(
                            binop.rhs,
                            "power of {d} is too high",
                            .{rhs},
                        );
                    };

                    const limbs_len = math.big.int.calcPowLimbsBufferLen(
                        lhs.bitCountAbs(),
                        power,
                    );

                    const buf = try r.arena().alloc(math.big.Limb, limbs_len);

                    res.limbs = try r.arena().alloc(math.big.Limb, limbs_len);
                    res.pow(lhs, power, buf);
                } else {
                    const limbs_len = switch (tag) {
                        .add, .sub => @max(lhs.limbs.len, rhs.limbs.len) + 1,
                        .mul => lhs.limbs.len + rhs.limbs.len,
                        .div => lhs.limbs.len,
                        else => unreachable,
                    };

                    res.limbs = try r.arena().alloc(math.big.Limb, limbs_len);
                    switch (tag) {
                        .add => res.add(lhs, rhs),
                        .sub => res.sub(lhs, rhs),
                        .mul => {
                            const buf = try r.arena().alloc(
                                math.big.Limb,
                                math.big.int.calcMulLimbsBufferLen(
                                    lhs.limbs.len,
                                    rhs.limbs.len,
                                    1,
                                ),
                            );

                            res.mul(lhs, rhs, buf, null);
                        },
                        .div => {
                            var res_r: math.big.int.Mutable = .{
                                .len = undefined,
                                .positive = undefined,
                                .limbs = try r.arena().alloc(
                                    math.big.Limb,
                                    rhs.limbs.len,
                                ),
                            };

                            const buf = try r.gpa.alloc(
                                math.big.Limb,
                                math.big.int.calcDivLimbsBufferLen(
                                    lhs.limbs.len,
                                    rhs.limbs.len,
                                ),
                            );

                            res.divTrunc(&res_r, lhs, rhs, buf);
                        },
                        else => unreachable,
                    }
                }

                const index = try r.ip.addBigInt(res.toConst());
                return .initValue(index);
            }

            if (lhs_ty == .i32) {
                const lhs_node = lhs_ref.toNode().?;
                if (rhs_ty == .i32) {
                    const rhs_node = rhs_ref.toNode();
                    return r.addi32BinopRef(
                        tag,
                        lhs_node,
                        rhs_node,
                    );
                }

                if (rhs_ty == .static_int) {
                    const rhs_node = try rhs_ref.convertToNode(r, binop.rhs);
                    return r.addBinopRefI32(
                        tag,
                        lhs_node,
                        rhs_node,
                    );
                }
            }

            if (rhs_ty == .i32) {
                const rhs_node = try lhs_ref.toNode().?;
                if (lhs_ty == .static_int) {
                    const lhs_node = try lhs_ref.convertToNode(r, binop.lhs);
                    return r.addBinopRefI32(
                        tag,
                        lhs_node,
                        rhs_node,
                    );
                }
            }

            return r.throw(node, "unable to {s} {s} and {s}", .{
                @tagName(tag),
                @tagName(lhs_ty),
                @tagName(rhs_ty),
            });
        },
        .ret_expr => |expr| {
            return r.resolveRetExpr(expr, r.current_scope.* == .root);
        },
        .builtin_call => |call| {
            if (mem.eql(u8, call.identifier, "arg")) {
                return r.resolveBuiltinArg(node, call.args);
            }

            if (mem.eql(u8, call.identifier, "exit")) {
                if (call.args.len != 1) return r.throw(
                    node,
                    "expected 1 argument got {d}",
                    .{call.args.len},
                );

                return r.resolveRetExpr(call.args[0], true);
            }

            if (mem.eql(u8, call.identifier, "run")) {
                if (call.args.len < 1) return r.throw(
                    node,
                    "expected at least 1 argument",
                    .{call.args.len},
                );

                if (r.ast.nodeTag(call.args[0]) != .literal_str) return r.throw(
                    call.args[0],
                    "expected a string literal",
                    .{},
                );
                const literal = r.ast.nodeTokenSlice(call.args[0]);

                var dir = r.file.dir;
                var file_path = literal;

                const file_slash = mem.lastIndexOfScalar(u8, literal, '/');
                if (file_slash) |slash| {
                    dir = dir.openDir(literal[0..slash]) catch unreachable;
                    file_path = file_path[slash + 1 ..];
                }

                const src = dir.readFileAlloc(r.gpa, file_path, math.maxInt(u32)) catch {
                    unreachable;
                };
                const ast = Parser.parse(r.gpa, src);

                const prev_file = r.file;
                defer r.file = prev_file;
            }

            return r.throw(node, "not a builtin", .{});
        },
        .fn_decl => {
            return .initValue(try r.ip.addFunc(node));
        },
        .call => |call| {
            const callee = try r.resolveNode(call.callee);
            const callee_ty = r.refType(callee);
            if (callee_ty != .func) return r.throw(
                node,
                "{s} not callable",
                .{@tagName(callee_ty)},
            );

            const decl = r.ast.full(callee.toValue().?.unwrapFunc(r.ip)).fn_decl;
            if (decl.args.len != call.args.len) return r.throw(
                node,
                "expected {d} arguments got {d}",
                .{ decl.args.len, call.args.len },
            );

            const func_scope: *Scope = blk: {
                const ptr = try r.arena().create(Scope.Fn);
                ptr.* = .init;

                break :blk &ptr.tag;
            };

            const locals = func_scope.locals();

            var runtime_arg_i: u32 = 0;
            const scratch_index: u32 = @intCast(r.scratch.items.len);
            for (call.args, decl.args) |carg, darg| {
                const identifier = r.ast.tokenSlice(darg);
                const arg_ref = try r.resolveNode(carg);

                if (arg_ref.toNode()) |n| {
                    try r.scratch.append(r.gpa, @intFromEnum(n));

                    const ty = r.refType(arg_ref);
                    try locals.put(r.arena(), identifier, try r.addNodeRef(ty, .{
                        .tag = .arg,
                        .data = .{ .arg = runtime_arg_i },
                    }));

                    runtime_arg_i += 1;
                } else {
                    try locals.put(r.arena(), identifier, arg_ref);
                }
            }

            const args = try r.appendExtraFromScratch(scratch_index, RAst.Node.Index);

            const old_scope = r.current_scope;
            r.current_scope = func_scope;
            defer r.current_scope = old_scope;

            const body_ref = try r.resolveNode(decl.body);
            if (args == .empty) if (body_ref.toValue() != null) return body_ref;

            return r.addNodeRef(r.refType(body_ref), .{
                .tag = .call,
                .data = .{
                    .call = .{
                        .args = args,
                        .body = body_ref,
                    },
                },
            });
        },
        inline else => |_, tag| return r.throw(
            node,
            "'{s}' not implemented",
            .{@tagName(tag)},
        ),
    }
}

fn convertValueToNode(
    r: *Resolver,
    src_node: Ast.Node.Index,
    value: InternPool.Index,
) (error{Fail} || mem.Allocator.Error)!RAst.Node.Index {
    switch (value.ty(r.ip)) {
        .type, .@"fn" => |ty| return r.throw(
            src_node,
            "unable to convert comptime only value of type {s} to runtime value",
            .{@tagName(ty)},
        ),
        .static_int => {
            const int = value.unwrapStaticInt(r.ip);
            const int_i32 = int.toInt(i32) orelse {
                return r.throw(
                    src_node,
                    "{s} cannot represent value {d}",
                    .{
                        @tagName(InternPool.Type.i32),
                        int,
                    },
                );
            };

            return r.addNode(.i32, .{
                .tag = .i32,
                .data = .{ .i32 = int_i32 },
            });
        },
        .static_float => unreachable, // TODO
        .static_str => unreachable, // TODO
        .none => unreachable, // TODO: figure out what to do here
        .i32, .f32 => unreachable, // i32 and f32 comptime values cannot exist
    }
}

fn resolveRetExpr(
    r: *Resolver,
    expr: Ast.Node.Index,
    exit: bool,
) ResolveError!Ref {
    if (expr == 0) {
        r.control_flow_value = .none;
        return error.Return;
    }

    const ref = try r.resolveNode(expr);
    if (ref.toValue()) |value| {
        r.control_flow_value = value;
        return error.Return;
    }

    const func_scope = r.current_scope.parentFn();
    func_scope.ret_ty = r.nodeType(ref.toNode().?);

    return r.addNodeRef(.none, .{
        .tag = if (exit) .exit else .ret,
        .data = .{
            .ref = .initNode(ref.toNode().?),
        },
    });
}

fn resolveBuiltinArg(
    r: *Resolver,
    node: Ast.Node.Index,
    args: []const Ast.Node.Index,
) ResolveError!Ref {
    if (r.current_scope.* != .root) return r.throw(
        node,
        "arg builtin can be used only in the root scope",
        .{},
    );

    if (args.len != 1) return r.throw(
        node,
        "expected 1 argument got {d}",
        .{args.len},
    );

    if (r.arg_types.len == 0) {
        return r.throw(
            node,
            "compiler options don't specify any input arguments",
            .{},
        );
    }

    const arg_ref = try r.resolveNode(args[0]);
    const ty = r.refType(arg_ref);
    if (ty != .static_int) {
        return r.throw(args[0], "expected {s} got {s}", .{
            @tagName(InternPool.Type.static_int),
            @tagName(ty),
        });
    }

    const int = arg_ref.toValue().?.unwrapStaticInt(r.ip);
    const index = int.toInt(u32) catch {
        return r.throw(
            args[0],
            "expected the value to be in range 0 to {d} (exclusive) got {d}",
            .{ r.arg_types.len, int },
        );
    };

    if (index >= r.arg_types.len) {
        return r.throw(
            args[0],
            "expected the value to be in range 0 to {d} (exclusive) got {d}",
            .{
                r.arg_types.len,
                index,
            },
        );
    }

    return r.file.args[index];
}

inline fn arena(r: *Resolver) mem.Allocator {
    return r.arena_state.allocator();
}

fn appendExtraFromScratch(
    r: *Resolver,
    T: type,
    start: u32,
) mem.Allocator.Error!RAst.ExtraSlice(T) {
    if (start == r.scratch.items.len) return .empty;
    const slice = r.scratch.items[start..];

    const index: RAst.ExtraSlice(T) = @enumFromInt(r.extra_data.items.len);
    try r.extra_data.ensureUnusedCapacity(r.gpa, slice.len + 1);
    r.extra_data.appendAssumeCapacity(@intCast(slice.len));
    r.extra_data.appendSliceAssumeCapacity(slice);
    r.scratch.items.len = start;

    return index;
}

fn appendScratchNode(
    r: *Resolver,
    ty: InternPool.Type,
    node: RAst.Node,
) mem.Allocator.Error!void {
    const index = try r.addNode(ty, node);
    try r.scratch.append(r.gpa, @intFromEnum(index));
}

inline fn nodeType(r: *const Resolver, node: RAst.Node.Index) InternPool.Type {
    return r.node_types.items[@intFromEnum(node)];
}

fn refType(r: *const Resolver, ref: Ref) InternPool.Type {
    if (ref.toNode()) |node| return r.nodeType(node);
    return ref.toValue().?.ty(r.ip);
}

fn addNodeRef(
    r: *Resolver,
    ty: InternPool.Type,
    node: RAst.Node,
) mem.Allocator.Error!Ref {
    const index = try r.addNode(ty, node);
    return @enumFromInt(@intFromEnum(index));
}

fn addi32BinopRef(
    r: *Resolver,
    tag: Ast.Node.Tag,
    lhs: RAst.Node.Index,
    rhs: RAst.Node.Index,
) mem.Allocator.Error!RAst.Node {
    const node_tag: RAst.Node.Tag = switch (tag) {
        .add => .addi32,
        .sub => .subi32,
        .mul => .muli32,
        .pow => .powi32,
        .div => .divi32,
        else => unreachable,
    };

    return r.addNode(.i32, .{
        .tag = node_tag,
        .data = .{ .binop = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
    });
}

fn reserveNode(r: *Resolver) mem.Allocator.Error!RAst.Node.Index {
    assert(r.nodes.len == r.node_types.items.len);

    _ = try r.node_types.addOne(r.gpa);
    return @enumFromInt(try r.nodes.addOne(r.gpa));
}

fn addNode(
    r: *Resolver,
    ty: InternPool.Type,
    node: RAst.Node,
) mem.Allocator.Error!RAst.Node.Index {
    const index: u32 = @intFromEnum(try r.reserveNode());

    r.nodes.set(index, node);
    r.node_types.items[index] = ty;

    return @enumFromInt(index);
}

fn printString(
    r: *Resolver,
    comptime fmt_str: []const u8,
    args: anytype,
) mem.Allocator.Error!RAst.StringIndex {
    const string: RAst.StringIndex = @enumFromInt(r.string_bytes.items.len);
    try r.string_bytes.writer(r.gpa).print(fmt_str ++ "\x00", args);

    return string;
}

fn appendError(
    r: *Resolver,
    node: Ast.Node.Index,
    comptime fmt_str: []const u8,
    args: anytype,
) mem.Allocator.Error!void {
    const message = try r.printString(fmt_str, args);
    try r.errors.append(r.gpa, .{
        .message = message,
        .src = .{ .node = node },
    });
}

fn throw(
    r: *Resolver,
    node: Ast.Node.Index,
    comptime fmt_str: []const u8,
    args: anytype,
) (error{Fail} || mem.Allocator.Error) {
    try r.appendError(node, fmt_str, args);
    return error.Fail;
}

fn addExtra(r: *Resolver, extra: anytype) mem.Allocator.Error!u32 {
    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    try r.extra_data.ensureUnusedCapacity(r.gpa, fields.len);

    return r.addExtraAssumeCapacity(extra);
}

fn addExtraAssumeCapacity(r: *Resolver, extra: anytype) u32 {
    const index: u32 = @intCast(r.extra_data.items.len);

    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    r.extra_data.items.len += fields.len;
    r.setExtra(@enumFromInt(index), extra);

    return index;
}

fn setExtra(r: *Resolver, index: RAst.ExtraIndex, extra: anytype) void {
    var i: u32 = @intFromEnum(index);
    inline for (@typeInfo(@TypeOf(extra)).@"struct".fields) |field| {
        if (field.type == u32) {
            r.extra_data.items[i] = @field(extra, field.name);
        } else if (@typeInfo(field.type) == .@"enum") {
            r.extra_data.items[i] = @intFromEnum(@field(extra, field.name));
        } else {
            r.extra_data.items[i] = @bitCast(@field(extra, field.name));
        }

        i += 1;
    }
}
