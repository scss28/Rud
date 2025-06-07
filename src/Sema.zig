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
const Span = @import("Span.zig");

const Sema = @This();

gpa: mem.Allocator,
arena_state: heap.ArenaAllocator,

result_arena_state: heap.ArenaAllocator,
file: *File,

ip: InternPool,

nodes: RAst.Nodes,
string_bytes: std.ArrayListUnmanaged(u8),
extra_data: std.ArrayListUnmanaged(u32),

current_scope: *Scope,
node_types: std.ArrayListUnmanaged(InternPool.Type),
scratch: std.ArrayListUnmanaged(u32),

flow: struct {
    src: Ast.Node.Index,
    value: InternPool.Index,
},
errors: std.ArrayListUnmanaged(Error),

pub const File = struct {
    ast: Ast,
    name: []const u8,
    cwd: fs.Dir,
    args: []const Ref,
};

pub const Error = struct {
    message: RAst.StringIndex,
    file: *File,
    src: Src,

    pub const Src = union(enum) {
        node: Ast.Node.Index,
        token: Ast.TokenIndex,
    };

    pub inline fn messageSlice(e: Error, r: AnalysisResult) []const u8 {
        return mem.sliceTo(r.rast.string_bytes[@intFromEnum(e.message)..], 0);
    }

    pub fn span(e: Error) Span {
        return switch (e.src) {
            .node => |node| e.file.ast.nodeSpan(node),
            .token => |token| e.file.ast.tokenSpan(token),
        };
    }
};

const Ref = enum(u32) {
    _,
    fn initNode(node: RAst.Node.Index) Ref {
        return @enumFromInt(@intFromEnum(node));
    }

    inline fn initValue(value: InternPool.Index) Ref {
        return @enumFromInt(@intFromEnum(value) | (1 << 31));
    }

    inline fn toNode(r: Ref) ?RAst.Node.Index {
        if (@intFromEnum(r) >> 31 != 0) return null;
        return @enumFromInt(@as(u31, @truncate(@intFromEnum(r))));
    }

    inline fn toValue(r: Ref) ?InternPool.Index {
        if (@intFromEnum(r) >> 31 == 0) return null;
        return @enumFromInt(@as(u31, @truncate(@intFromEnum(r))));
    }

    fn convertToNode(
        ref: Ref,
        s: *Sema,
        src_node: Ast.Node.Index,
    ) AnalysisError!RAst.Node.Index {
        if (ref.toNode()) |n| return n;

        const value = ref.toValue().?;
        return s.convertValueToNode(src_node, value);
    }
};

const Scope = enum {
    root,
    @"fn",
    block,

    fn toFull(s: *Scope, T: type) *T {
        switch (T) {
            Fn => assert(s.* == .@"fn" or s.* == .root),
            Block => assert(s.* == .block),
            else => unreachable,
        }

        return @alignCast(@fieldParentPtr("tag", s));
    }

    inline fn locals(s: *Scope) *Locals {
        return switch (s.*) {
            .@"fn", .root => &s.toFull(Fn).locals,
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
            .tag = .@"fn",
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

pub const AnalysisResult = struct {
    arena_state: heap.ArenaAllocator,

    rast: RAst,
    errors: []const Error,

    fn init(s: *Sema) mem.Allocator.Error!AnalysisResult {
        const rast: RAst = .{
            .nodes = s.nodes.toOwnedSlice(),
            .string_bytes = try s.string_bytes.toOwnedSlice(s.gpa),
            .extra_data = try s.extra_data.toOwnedSlice(s.gpa),
        };

        return .{
            .arena_state = s.result_arena_state,
            .rast = rast,
            .errors = try s.errors.toOwnedSlice(s.gpa),
        };
    }

    pub fn deinit(r: *AnalysisResult, gpa: mem.Allocator) void {
        r.arena_state.deinit();
        r.rast.deinit(gpa);
        gpa.free(r.errors);

        r.* = undefined;
    }
};

pub fn analyze(
    gpa: mem.Allocator,
    dir: fs.Dir,
    path: []const u8,
    arg_types: []const InternPool.Type,
) !AnalysisResult {
    const ip: InternPool = try .init(gpa);
    var s: Sema = .{
        .gpa = gpa,
        .arena_state = .init(gpa),

        .result_arena_state = .init(gpa),
        .file = undefined,
        .ip = ip,

        .nodes = .empty,
        .string_bytes = .empty,
        .extra_data = .empty,

        .current_scope = undefined,
        .node_types = .empty,
        .scratch = .empty,
        .flow = undefined,
        .errors = .empty,
    };
    defer {
        s.arena_state.deinit();
        s.ip.deinit(s.gpa);
        s.node_types.deinit(s.gpa);
        s.scratch.deinit(s.gpa);
    }
    errdefer {
        s.result_arena_state.deinit();
        s.string_bytes.deinit(s.gpa);
        s.errors.deinit(s.gpa);
        s.nodes.deinit(s.gpa);
        s.extra_data.deinit(s.gpa);
    }

    // RAst.StringIndex.empty
    try s.string_bytes.append(s.gpa, 0);

    // Reserved slots of RAst.ExtraIndex.
    const reserved_count = @typeInfo(RAst.ExtraIndex).@"enum".fields.len;
    try s.extra_data.ensureTotalCapacity(s.gpa, reserved_count);
    s.extra_data.items.len += reserved_count;
    s.extra_data.items[0] = 0;

    const args = try s.arena().alloc(Ref, arg_types.len);
    for (arg_types, 0..) |ty, i| {
        args[i] = try s.addNodeRef(ty, .{
            .tag = .arg,
            .data = .{ .arg = @intCast(i) },
        });
    }

    const root_scope = try s.arena().create(Scope.Fn);
    root_scope.* = .{
        .tag = .root,
        .ret_ty = null,
        .locals = .empty,
    };
    s.current_scope = &root_scope.tag;

    const root_ref = s.analyzeFile(dir, path, args) catch |err| switch (err) {
        error.Fail => return .init(&s),
        else => |e| return e,
    };

    if (root_ref.toValue()) |value| {
        const node =
            s.convertValueToNode(s.flow.src, value) catch |err| switch (err) {
                error.Fail => return .init(&s),
                error.OutOfMemory => |oom| return oom,
            };

        const ret_node = try s.addNode(.none, .{
            .tag = .exit,
            .data = .{ .node = node },
        });
        try s.extra_data.append(s.gpa, 1);
        try s.extra_data.append(s.gpa, @intFromEnum(ret_node));

        _ = try s.addNode(.none, .{
            .tag = .block,
            .data = .{ .block = @enumFromInt(s.extra_data.items.len - 2) },
        });
    }

    const ret_ty = s.current_scope.toFull(Scope.Fn).ret_ty;
    if (ret_ty) |ty| {
        s.extra_data.items[@intFromEnum(RAst.ExtraIndex.ret_type)] = @intFromEnum(ty);
    } else {
        s.extra_data.items[@intFromEnum(RAst.ExtraIndex.ret_type)] =
            @intFromEnum(InternPool.Type.none);
    }

    return .init(&s);
}

fn analyzeFile(
    s: *Sema,
    dir: fs.Dir,
    path: []const u8,
    args: []const Ref,
) !Ref {
    var cwd = dir;
    var name = path;

    const file_slash = mem.lastIndexOfScalar(u8, path, '/');
    if (file_slash) |slash| {
        cwd = try dir.openDir(path[0..slash], .{});
        name = path[slash + 1 ..];
    }

    const src = try cwd.readFileAllocOptions(
        s.result_arena_state.allocator(),
        name,
        math.maxInt(u32),
        null,
        1,
        0,
    );

    const ast = try Parser.parse(s.result_arena_state.allocator(), src);
    const file = try s.result_arena_state.allocator().create(File);
    file.* = .{
        .ast = ast,
        .name = name,
        .cwd = cwd,
        .args = args,
    };

    if (ast.errors.len != 0) {
        try s.errors.ensureUnusedCapacity(s.gpa, ast.errors.len);
        for (ast.errors) |err| s.errors.appendAssumeCapacity(.{
            .file = file,
            .src = .{ .token = err.token },
            .message = try s.printString("{s}", .{err.message}),
        });

        return error.Fail;
    }

    const old_file = s.file;
    s.file = file;
    defer s.file = old_file;

    const block = s.analyzeBlock(ast.rootNodes()) catch |err| switch (err) {
        error.Return => return .initValue(s.flow.value),
        error.OutOfMemory => |oom| return oom,
    };

    return .initNode(block);
}

fn analyzeBlock(
    s: *Sema,
    nodes: []const Ast.Node.Index,
) (mem.Allocator.Error || error{Return})!RAst.Node.Index {
    const scratch_index: u32 = @intCast(s.scratch.items.len);
    for (nodes, 0..) |node, i| {
        const ref = s.analyzeNode(node) catch |err| switch (err) {
            error.Fail => continue,
            error.Return => |e| {
                if (i != nodes.len - 1) try s.appendError(
                    .{ .node = nodes[i + 1] },
                    "unreachable code",
                    .{},
                );

                if (scratch_index == s.scratch.items.len) return e;

                const expr = s.convertValueToNode(
                    s.flow.src,
                    s.flow.value,
                ) catch |er| switch (er) {
                    error.Fail => break,
                    error.OutOfMemory => |oom| return oom,
                };

                try s.appendScratchNode(.none, .{
                    .tag = .ret,
                    .data = .{ .node = expr },
                });

                break;
            },
            error.OutOfMemory => |oom| return oom,
        };

        const ref_ty = s.refType(ref);
        if (ref_ty != .none) {
            try s.appendError(
                .{ .node = node },
                "value of type '{s}' ignored",
                .{@tagName(ref_ty)},
            );

            continue;
        }

        if (ref.toNode()) |r_node| {
            try s.scratch.append(s.gpa, @intFromEnum(r_node));
        }
    }

    const slice = try s.appendExtraFromScratch(RAst.Node.Index, scratch_index);
    return s.addNode(.none, .{
        .tag = .block,
        .data = .{ .block = slice },
    });
}

const AnalysisError = error{ Fail, Return } || mem.Allocator.Error;

fn analyzeNode(
    s: *Sema,
    node: Ast.Node.Index,
) AnalysisError!Ref {
    const ast = s.file.ast;
    switch (s.file.ast.full(node)) {
        .literal_int => |literal| {
            var big_int: math.big.int.Managed = try .init(s.gpa);
            defer big_int.deinit();

            big_int.setString(10, literal) catch |err| switch (err) {
                error.OutOfMemory => |oom| return oom,
                else => unreachable, // a literal_int is always a valid integer
            };

            const value = try s.ip.addBigInt(s.gpa, big_int.toConst());
            return .initValue(value);
        },
        .literal_str => |literal| {
            const value = try s.ip.addStr(s.gpa, literal);
            return .initValue(value);
        },
        .literal_float => |literal| {
            var real: math.big.Rational = try .init(s.gpa);
            defer real.deinit();

            real.setFloatString(literal) catch |err| switch (err) {
                error.OutOfMemory => |oom| return oom,
                else => unreachable, // a literal_float is always a valid float
            };

            const value = try s.ip.addReal(s.gpa, real);
            return .initValue(value);
        },
        .identifier => |identifier| {
            var scope: *Scope = s.current_scope;
            while (true) {
                const ref = scope.locals().get(identifier) orelse {
                    if (scope.* != .block) break;

                    scope = scope.toFull(Scope.Block).parent;
                    continue;
                };

                return ref;
            }

            return s.throwSrcNode(node, "'{s}' not in scope", .{identifier});
        },
        .assign => |assign| {
            const ref = try s.analyzeNode(assign.rhs);
            try s.current_scope.locals().put(s.arena(), assign.identifier, ref);

            return .initValue(.none);
        },
        // NOTE: probably remove the inline
        inline .add, .sub, .mul, .pow, .div => |binop, tag| {
            const lhs_ref = try s.analyzeNode(binop.lhs);
            const rhs_ref = try s.analyzeNode(binop.rhs);

            const lhs_ty = s.refType(lhs_ref);
            const rhs_ty = s.refType(rhs_ref);

            var res: math.big.int.Mutable = undefined;
            if (lhs_ty == .int and rhs_ty == .int) {
                const lhs = lhs_ref.toValue().?.unwrapBigInt(&s.ip);
                const rhs = rhs_ref.toValue().?.unwrapBigInt(&s.ip);

                if (tag == .pow) {
                    if (!rhs.positive) {
                        return s.throwSrcNode(binop.rhs, "power cannot be negative", .{});
                    }

                    const power = rhs.toInt(u32) catch {
                        return s.throwSrcNode(
                            binop.rhs,
                            "power of {d} is too high",
                            .{rhs},
                        );
                    };

                    const limbs_len = math.big.int.calcPowLimbsBufferLen(
                        lhs.bitCountAbs(),
                        power,
                    );

                    const buf = try s.arena().alloc(math.big.Limb, limbs_len);

                    res.limbs = try s.arena().alloc(math.big.Limb, limbs_len);
                    res.pow(lhs, power, buf);
                } else {
                    const limbs_len = switch (tag) {
                        .add, .sub => @max(lhs.limbs.len, rhs.limbs.len) + 1,
                        .mul => lhs.limbs.len + rhs.limbs.len,
                        .div => lhs.limbs.len,
                        else => unreachable,
                    };

                    res.limbs = try s.arena().alloc(math.big.Limb, limbs_len);
                    switch (tag) {
                        .add => res.add(lhs, rhs),
                        .sub => res.sub(lhs, rhs),
                        .mul => {
                            const buf = try s.arena().alloc(
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
                                .limbs = try s.arena().alloc(
                                    math.big.Limb,
                                    rhs.limbs.len,
                                ),
                            };

                            const buf = try s.gpa.alloc(
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

                const value = try s.ip.addBigInt(s.gpa, res.toConst());
                return .initValue(value);
            }

            if (lhs_ty == .real and tag != .pow) {
                if (rhs_ty == .real) {
                    // TODO: avoid allocating
                    var lhs = try lhs_ref.toValue().?.unwrapRealDupe(s.arena(), &s.ip);
                    const rhs = try rhs_ref.toValue().?.unwrapRealDupe(s.arena(), &s.ip);

                    return s.addRealBinopRef(tag, &lhs, rhs);
                }

                if (rhs_ty == .int) {
                    var lhs = try lhs_ref.toValue().?.unwrapRealDupe(s.arena(), &s.ip);
                    const rhs: InternPool.Real = .{
                        .p = try rhs_ref.toValue().?
                            .unwrapBigInt(&s.ip)
                            .toManaged(s.arena()),
                        .q = try .initSet(s.arena(), 1),
                    };
                    return s.addRealBinopRef(tag, &lhs, rhs);
                }

                if (rhs_ty == .f32) {
                    const lhs_node = try rhs_ref.convertToNode(s, binop.lhs);
                    const rhs_node = rhs_ref.toNode().?;
                    return s.addBinopRef(.f32, tag, lhs_node, rhs_node);
                }
            }

            if (rhs_ty == .real and tag != .pow) {
                if (lhs_ty == .int) {
                    var lhs: InternPool.Real = .{
                        .p = try lhs_ref.toValue().?
                            .unwrapBigInt(&s.ip)
                            .toManaged(s.arena()),
                        .q = try .initSet(s.arena(), 1),
                    };
                    const rhs = try rhs_ref.toValue().?.unwrapRealDupe(s.arena(), &s.ip);
                    return s.addRealBinopRef(tag, &lhs, rhs);
                }

                if (lhs_ty == .f32) {
                    const lhs_node = lhs_ref.toNode().?;
                    const rhs_node = try rhs_ref.convertToNode(s, binop.rhs);
                    return s.addBinopRef(.f32, tag, lhs_node, rhs_node);
                }
            }

            if (lhs_ty == .i32) {
                const lhs_node = lhs_ref.toNode().?;
                if (rhs_ty == .i32) {
                    const rhs_node = rhs_ref.toNode().?;
                    return s.addBinopRef(
                        .i32,
                        tag,
                        lhs_node,
                        rhs_node,
                    );
                }

                if (rhs_ty == .int) {
                    const rhs_node = try rhs_ref.convertToNode(s, binop.rhs);
                    return s.addBinopRef(
                        .i32,
                        tag,
                        lhs_node,
                        rhs_node,
                    );
                }
            }

            if (rhs_ty == .i32) {
                const rhs_node = rhs_ref.toNode().?;
                if (lhs_ty == .int) {
                    const lhs_node = try lhs_ref.convertToNode(s, binop.lhs);
                    return s.addBinopRef(
                        .i32,
                        tag,
                        lhs_node,
                        rhs_node,
                    );
                }
            }

            return s.throwSrcNode(node, "unable to {s} {s} and {s}", .{
                @tagName(tag),
                @tagName(lhs_ty),
                @tagName(rhs_ty),
            });
        },
        .ret_expr => |expr| {
            return s.analyzeRetExpr(node, expr, s.current_scope.* == .root);
        },
        .builtin_call => |call| {
            if (mem.eql(u8, call.identifier, "arg")) {
                if (s.current_scope.* != .root) return s.throwSrcNode(
                    node,
                    "arg builtin can be used only in the root scope",
                    .{},
                );

                if (call.args.len != 1) return s.throwSrcNode(
                    node,
                    "expected 1 argument got {d}",
                    .{call.args.len},
                );

                const f = s.file.*;
                if (f.args.len == 0) {
                    return s.throwSrcNode(
                        node,
                        "file invocation doesn't specify any input arguments",
                        .{},
                    );
                }

                const arg_ref = try s.analyzeNode(call.args[0]);
                const ty = s.refType(arg_ref);
                if (ty != .int) {
                    return s.throwSrcNode(call.args[0], "expected {s} got {s}", .{
                        @tagName(InternPool.Type.int),
                        @tagName(ty),
                    });
                }

                const int = arg_ref.toValue().?.unwrapBigInt(&s.ip);
                const index = int.toInt(u32) catch {
                    return s.throwSrcNode(
                        call.args[0],
                        "expected the index to be in range 0 to {d} (exclusive) got {d}",
                        .{ f.args.len, int },
                    );
                };

                if (index >= f.args.len) {
                    return s.throwSrcNode(
                        call.args[0],
                        "argument {d} not provided for the invocation of this file",
                        .{index},
                    );
                }

                return f.args[index];
            }

            if (mem.eql(u8, call.identifier, "exit")) {
                if (call.args.len != 1) return s.throwSrcNode(
                    node,
                    "expected 1 argument got {d}",
                    .{call.args.len},
                );

                return s.analyzeRetExpr(node, call.args[0], true);
            }

            if (mem.eql(u8, call.identifier, "run")) {
                if (call.args.len == 0) return s.throwSrcNode(
                    node,
                    "expected at least 1 argument",
                    .{},
                );

                if (ast.nodeTag(call.args[0]) != .literal_str) return s.throwSrcNode(
                    call.args[0],
                    "expected a string literal",
                    .{},
                );

                const literal = ast.nodeTokenSlice(call.args[0]);

                const args = try s.arena().alloc(Ref, call.args.len - 1);
                for (call.args[1..], 0..) |arg, i| {
                    args[i] = try s.analyzeNode(arg);
                }

                const root_scope = try s.arena().create(Scope.Fn);
                root_scope.* = .{
                    .tag = .root,
                    .ret_ty = null,
                    .locals = .empty,
                };

                const old_scope = s.current_scope;
                s.current_scope = &root_scope.tag;
                defer s.current_scope = old_scope;

                const path = literal[1 .. literal.len - 1];
                return s.analyzeFile(s.file.cwd, path, args) catch |err| switch (err) {
                    error.Fail, error.OutOfMemory => |e| e,
                    else => |e| {
                        return s.throwSrcNode(
                            node,
                            "unable to open file '{s}': {any}",
                            .{ path, e },
                        );
                    },
                };
            }

            return s.throwSrcNode(node, "not a builtin", .{});
        },
        .fn_decl => {
            return .initValue(try s.ip.addFn(s.gpa, node));
        },
        .call => |call| {
            const callee = try s.analyzeNode(call.callee);
            const callee_ty = s.refType(callee);
            if (callee_ty != .@"fn") return s.throwSrcNode(
                node,
                "{s} not callable",
                .{@tagName(callee_ty)},
            );

            const decl = ast.full(callee.toValue().?.unwrapFn(&s.ip)).fn_decl;
            if (decl.args.len != call.args.len) return s.throwSrcNode(
                node,
                "expected {d} arguments got {d}",
                .{ decl.args.len, call.args.len },
            );

            const fn_scope: *Scope = blk: {
                const ptr = try s.arena().create(Scope.Fn);
                ptr.* = .init;

                break :blk &ptr.tag;
            };

            const locals = fn_scope.locals();

            var runtime_arg_i: u32 = 0;
            const scratch_index: u32 = @intCast(s.scratch.items.len);
            for (call.args, decl.args) |carg, darg| {
                const identifier = ast.tokenSlice(darg);
                const arg_ref = try s.analyzeNode(carg);

                if (arg_ref.toNode()) |n| {
                    try s.scratch.append(s.gpa, @intFromEnum(n));

                    const ty = s.refType(arg_ref);
                    try locals.put(s.arena(), identifier, try s.addNodeRef(ty, .{
                        .tag = .arg,
                        .data = .{ .arg = runtime_arg_i },
                    }));

                    runtime_arg_i += 1;
                } else {
                    try locals.put(s.arena(), identifier, arg_ref);
                }
            }

            const args = try s.appendExtraFromScratch(RAst.Node.Index, scratch_index);

            const old_scope = s.current_scope;
            s.current_scope = fn_scope;
            defer s.current_scope = old_scope;

            const body_ref = try s.analyzeNode(decl.body);
            if (args == .empty) if (body_ref.toValue() != null) return body_ref;

            const body = try body_ref.convertToNode(s, decl.body);
            return s.addNodeRef(s.refType(body_ref), .{
                .tag = .call,
                .data = .{
                    .call = .{
                        .args = args,
                        .body = body,
                    },
                },
            });
        },
        inline else => |_, tag| return s.throwSrcNode(
            node,
            "'{s}' not implemented",
            .{@tagName(tag)},
        ),
    }
}

fn addRealBinopRef(
    s: *Sema,
    tag: Ast.Node.Tag,
    lhs: *InternPool.Real,
    rhs: InternPool.Real,
) AnalysisError!Ref {
    switch (tag) {
        .add => try lhs.add(lhs.*, rhs),
        .sub => try lhs.sub(lhs.*, rhs),
        .mul => try lhs.mul(lhs.*, rhs),
        .div => try lhs.div(lhs.*, rhs),
        else => unreachable,
    }

    const value = try s.ip.addReal(s.gpa, lhs.*);
    return .initValue(value);
}

fn analyzeRetExpr(
    s: *Sema,
    src_node: Ast.Node.Index,
    expr: Ast.Node.Index,
    exit: bool,
) AnalysisError!Ref {
    if (expr == 0) {
        s.flow.src = src_node;
        s.flow.value = .none;
        return error.Return;
    }

    const ref = try s.analyzeNode(expr);
    if (ref.toValue()) |value| {
        s.flow.src = src_node;
        s.flow.value = value;
        return error.Return;
    }

    const fn_scope = s.current_scope.parentFn();
    fn_scope.ret_ty = s.nodeType(ref.toNode().?);

    return s.addNodeRef(.none, .{
        .tag = if (exit) .exit else .ret,
        .data = .{ .node = ref.toNode().? },
    });
}

fn addBinopRef(
    s: *Sema,
    comptime ty: InternPool.Type,
    tag: Ast.Node.Tag,
    lhs: RAst.Node.Index,
    rhs: RAst.Node.Index,
) mem.Allocator.Error!Ref {
    const node_tag = switch (tag) {
        inline .add, .sub, .mul, .pow, .div => |t| @field(
            RAst.Node.Tag,
            @tagName(t) ++ @tagName(ty),
        ),
        else => unreachable,
    };

    return s.addNodeRef(ty, .{
        .tag = node_tag,
        .data = .{ .binop = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
    });
}

fn appendExtraFromScratch(
    s: *Sema,
    T: type,
    start: u32,
) mem.Allocator.Error!RAst.ExtraSlice(T) {
    if (start == s.scratch.items.len) return .empty;
    const slice = s.scratch.items[start..];

    const index: RAst.ExtraSlice(T) = @enumFromInt(s.extra_data.items.len);
    try s.extra_data.ensureUnusedCapacity(s.gpa, slice.len + 1);
    s.extra_data.appendAssumeCapacity(@intCast(slice.len));
    s.extra_data.appendSliceAssumeCapacity(slice);
    s.scratch.items.len = start;

    return index;
}

fn appendScratchNode(
    s: *Sema,
    ty: InternPool.Type,
    node: RAst.Node,
) mem.Allocator.Error!void {
    const index = try s.addNode(ty, node);
    try s.scratch.append(s.gpa, @intFromEnum(index));
}

inline fn nodeType(s: *const Sema, node: RAst.Node.Index) InternPool.Type {
    return s.node_types.items[@intFromEnum(node)];
}

fn refType(s: *const Sema, ref: Ref) InternPool.Type {
    if (ref.toNode()) |node| return s.nodeType(node);
    return ref.toValue().?.ty(&s.ip);
}

fn addNodeRef(
    s: *Sema,
    ty: InternPool.Type,
    node: RAst.Node,
) mem.Allocator.Error!Ref {
    const index = try s.addNode(ty, node);
    return @enumFromInt(@intFromEnum(index));
}

fn reserveNode(s: *Sema) mem.Allocator.Error!RAst.Node.Index {
    assert(s.nodes.len == s.node_types.items.len);

    _ = try s.node_types.addOne(s.gpa);
    return @enumFromInt(try s.nodes.addOne(s.gpa));
}

fn convertValueToNode(
    s: *Sema,
    src_node: Ast.Node.Index,
    value: InternPool.Index,
) (error{Fail} || mem.Allocator.Error)!RAst.Node.Index {
    switch (value.ty(&s.ip)) {
        .int => {
            const int = value.unwrapBigInt(&s.ip);
            const int_i32 = int.toInt(i32) catch {
                return s.throwSrcNode(
                    src_node,
                    "{s} cannot represent value {d}",
                    .{
                        @tagName(InternPool.Type.i32),
                        int,
                    },
                );
            };

            return s.addNode(.i32, .{
                .tag = .i32,
                .data = .{ .i32 = int_i32 },
            });
        },
        .real => {
            // TODO: try to not allocate here...
            const real = try value.unwrapRealDupe(s.arena(), &s.ip);
            const float = real.toFloat(f32) catch |err| switch (err) {
                error.OutOfMemory => |oom| return oom,
            };

            return s.addNode(.f32, .{
                .tag = .f32,
                .data = .{ .f32 = float },
            });
        },
        .str => unreachable, // TODO
        .i32, .f32 => unreachable, // i32 and f32 comptime values cannot exist
        else => |ty| return s.throwSrcNode(
            src_node,
            "unable to convert comptime value of type {s} to runtime value",
            .{@tagName(ty)},
        ),
    }
}

fn addNode(
    s: *Sema,
    ty: InternPool.Type,
    node: RAst.Node,
) mem.Allocator.Error!RAst.Node.Index {
    const index: u32 = @intFromEnum(try s.reserveNode());

    s.nodes.set(index, node);
    s.node_types.items[index] = ty;

    return @enumFromInt(index);
}

inline fn arena(s: *Sema) mem.Allocator {
    return s.arena_state.allocator();
}

fn printString(
    s: *Sema,
    comptime fmt_str: []const u8,
    args: anytype,
) mem.Allocator.Error!RAst.StringIndex {
    const string: RAst.StringIndex = @enumFromInt(s.string_bytes.items.len);
    try s.string_bytes.writer(s.gpa).print(fmt_str ++ "\x00", args);

    return string;
}

fn appendError(
    s: *Sema,
    src: Error.Src,
    comptime fmt_str: []const u8,
    args: anytype,
) mem.Allocator.Error!void {
    const message = try s.printString(fmt_str, args);
    try s.errors.append(s.gpa, .{
        .file = s.file,
        .message = message,
        .src = src,
    });
}

fn throw(
    s: *Sema,
    src: Error.Src,
    comptime fmt_str: []const u8,
    args: anytype,
) (error{Fail} || mem.Allocator.Error) {
    try s.appendError(src, fmt_str, args);
    return error.Fail;
}

fn throwSrcNode(
    s: *Sema,
    node: Ast.Node.Index,
    comptime fmt_str: []const u8,
    args: anytype,
) (error{Fail} || mem.Allocator.Error) {
    try s.appendError(.{ .node = node }, fmt_str, args);
    return error.Fail;
}

fn addExtra(s: *Sema, extra: anytype) mem.Allocator.Error!u32 {
    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    try s.extra_data.ensureUnusedCapacity(s.gpa, fields.len);

    return s.addExtraAssumeCapacity(extra);
}

fn addExtraAssumeCapacity(s: *Sema, extra: anytype) u32 {
    const index: u32 = @intCast(s.extra_data.items.len);

    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    s.extra_data.items.len += fields.len;
    s.setExtra(@enumFromInt(index), extra);

    return index;
}

fn setExtra(s: *Sema, index: RAst.ExtraIndex, extra: anytype) void {
    var i: u32 = @intFromEnum(index);
    inline for (@typeInfo(@TypeOf(extra)).@"struct".fields) |field| {
        if (field.type == u32) {
            s.extra_data.items[i] = @field(extra, field.name);
        } else if (@typeInfo(field.type) == .@"enum") {
            s.extra_data.items[i] = @intFromEnum(@field(extra, field.name));
        } else {
            s.extra_data.items[i] = @bitCast(@field(extra, field.name));
        }

        i += 1;
    }
}
