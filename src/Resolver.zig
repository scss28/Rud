const std = @import("std");
const mem = std.mem;
const math = std.math;
const meta = std.meta;
const heap = std.heap;

const assert = std.debug.assert;

const Ast = @import("Ast.zig");
const RAst = @import("RAst.zig");
const InternPool = @import("InternPool.zig");
const Resolver = @This();

gpa: mem.Allocator,
arena_state: heap.ArenaAllocator,

ast: *const Ast,
ip: *InternPool,
arg_types_len: usize,

nodes: RAst.Nodes,
string_bytes: std.ArrayListUnmanaged(u8),
extra_data: std.ArrayListUnmanaged(u32),

node_types: std.ArrayListUnmanaged(InternPool.Type),
scratch: std.ArrayListUnmanaged(u32),
current_scope: *Scope,
local_stack: std.ArrayListUnmanaged(InternPool.Type),

control_flow_value: InternPool.Index,
errors: std.ArrayListUnmanaged(RAst.Error),

const Scope = enum {
    func,
    block,

    fn toFull(s: *Scope, T: type) *T {
        switch (T) {
            Fn => assert(s.* == .func),
            Block => assert(s.* == .block),
            else => unreachable,
        }

        return @alignCast(@fieldParentPtr("tag", s));
    }

    inline fn locals(s: *Scope) *Locals {
        return switch (s.*) {
            .func => &s.toFull(Fn).locals,
            .block => &s.toFull(Block).locals,
        };
    }

    fn parentFn(s: *Scope) *Fn {
        var scope = s;
        while (scope.* != .func) : (scope = s.toFull(Block).parent) {}

        return s.toFull(Fn);
    }

    const Locals = std.StringHashMapUnmanaged(LocalRef);

    const Fn = struct {
        tag: Scope = .func,
        ret_ty: ?InternPool.Type = null,

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

    const LocalRef = enum(u32) {
        _,

        inline fn toValue(l: LocalRef) ?InternPool.Index {
            if (@intFromEnum(l) >> 31 == 0) return null;
            return @enumFromInt(@as(u31, @truncate(@intFromEnum(l))));
        }

        fn toStackIndex(l: LocalRef) ?u32 {
            if (@intFromEnum(l) >> 31 != 0) return null;
            return @as(u31, @truncate(@intFromEnum(l)));
        }
    };
};

pub fn resolve(
    gpa: mem.Allocator,
    ast: *const Ast,
    ip: *InternPool,
    arg_types: []const InternPool.Type,
) mem.Allocator.Error!RAst {
    var r: Resolver = .{
        .gpa = gpa,
        .arena_state = .init(gpa),

        .ast = ast,
        .ip = ip,
        .arg_types_len = arg_types.len,

        .nodes = .empty,
        .string_bytes = .empty,
        .extra_data = .empty,

        .node_types = .empty,
        .scratch = .empty,
        .current_scope = undefined,

        .local_stack = .empty,

        .control_flow_value = .none,
        .errors = .empty,
    };
    defer {
        r.arena_state.deinit();
        r.node_types.deinit(gpa);
        r.scratch.deinit(gpa);
        r.local_stack.deinit(gpa);
        r.errors.deinit(gpa);
    }

    const root_scope = try r.arena().create(Scope.Fn);
    root_scope.* = .init;
    r.current_scope = &root_scope.tag;

    try r.local_stack.appendSlice(gpa, arg_types);

    // RAst.StringIndex.empty
    try r.string_bytes.append(gpa, 0);

    // Reserved slots of RAst.ExtraIndex.
    const reserved_count = @typeInfo(RAst.ExtraIndex).@"enum".fields.len;
    try r.extra_data.ensureTotalCapacity(gpa, ast.nodes.len + reserved_count);
    r.extra_data.items.len += reserved_count;

    try r.nodes.ensureTotalCapacity(gpa, ast.nodes.len);
    const root_node = try r.reserveNode();
    for (ast.rootNodes()) |node| {
        const ref = r.resolveNode(node) catch |err| switch (err) {
            error.Fail => continue,
            error.Return => {
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
            try r.scratch.append(gpa, @intFromEnum(r_node));
        }
    }

    const root_nodes_index = try r.appendExtraFromScratch(0);
    r.nodes.items(.data)[@intFromEnum(root_node)] = .{ .root = .{
        .start = @intFromEnum(root_nodes_index),
        .end = @intCast(r.extra_data.items.len),
    } };

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

const ResolveError = error{ Fail, Return } || mem.Allocator.Error;

fn resolveNode(r: *Resolver, node: Ast.Node.Index) ResolveError!RAst.Ref {
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
                const local = scope.locals().get(identifier) orelse {
                    if (scope.* == .func) break;

                    scope = scope.toFull(Scope.Block).parent;
                    continue;
                };

                if (local.toValue()) |value| {
                    return .initValue(value);
                } else {
                    const index = local.toStackIndex().?;
                    const ty = r.local_stack.items[index];
                    return r.addNodeRef(ty, .{
                        .tag = .get,
                        .data = .{ .local_index = index },
                    });
                }
            }

            return r.throw(node, "'{s}' not in scope", .{identifier});
        },
        .assign => |assign| {
            const ref = try r.resolveNode(assign.rhs);
            const gop = try r.current_scope.locals().getOrPut(
                r.arena(),
                assign.identifier,
            );
            if (ref.toValue()) |value| {
                gop.value_ptr.* = @enumFromInt(@intFromEnum(value) | (1 << 31));
                return .initValue(.none);
            }

            if (!gop.found_existing) {
                gop.value_ptr.* = @enumFromInt(r.local_stack.items.len);
                _ = try r.local_stack.addOne(r.gpa);
            }

            const n = ref.toNode().?;
            const local_index: RAst.LocalIndex = @intFromEnum(gop.value_ptr.*);
            r.local_stack.items[local_index] = r.nodeType(n);

            return r.addNodeRef(.none, .{
                .tag = .set,
                .data = .{ .set = .{
                    .local = local_index,
                    .ref = .initNode(n),
                } },
            });
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
                if (rhs_ty == .i32) {
                    return r.addBinopRefI32(tag, lhs_ref, rhs_ref);
                }

                if (rhs_ty == .static_int) {
                    const rhs = rhs_ref.toValue().?.unwrapStaticInt(r.ip);
                    const rhs_i32 = rhs.toInt(i32) catch {
                        return r.throw(
                            binop.rhs,
                            "{s} cannot represent value {d}",
                            .{
                                @tagName(InternPool.Type.i32),
                                rhs,
                            },
                        );
                    };

                    const rhs_value_i32 = try r.ip.addInt(rhs_i32);
                    return r.addBinopRefI32(tag, lhs_ref, .initValue(rhs_value_i32));
                }
            }

            if (rhs_ty == .i32) {
                if (lhs_ty == .static_int) {
                    const lhs = lhs_ref.toValue().?.unwrapStaticInt(r.ip);
                    const lhs_i32 = lhs.toInt(i32) catch {
                        return r.throw(
                            binop.rhs,
                            "{s} cannot represent value {d}",
                            .{
                                @tagName(InternPool.Type.i32),
                                lhs,
                            },
                        );
                    };

                    const lhs_value_i32 = try r.ip.addInt(lhs_i32);
                    return r.addBinopRefI32(tag, .initValue(lhs_value_i32), rhs_ref);
                }
            }

            return r.throw(node, "unable to {s} {s} and {s}", .{
                @tagName(tag),
                @tagName(lhs_ty),
                @tagName(rhs_ty),
            });
        },
        .ret_expr => |expr| {
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
                .tag = .ret,
                .data = .{
                    .ref = .initNode(ref.toNode().?),
                },
            });
        },
        .builtin_call => |call| {
            if (mem.eql(u8, call.identifier, "arg")) {
                if (call.args.len != 1) return r.throw(
                    node,
                    "expected 1 argument got {d}",
                    .{call.args.len},
                );

                if (r.arg_types_len == 0) {
                    return r.throw(
                        node,
                        "compiler options don't specify any input arguments",
                        .{},
                    );
                }

                const arg_ref = try r.resolveNode(call.args[0]);
                const ty = r.refType(arg_ref);
                if (ty != .static_int) {
                    return r.throw(call.args[0], "expected {s} got {s}", .{
                        @tagName(InternPool.Type.static_int),
                        @tagName(ty),
                    });
                }

                const int = arg_ref.toValue().?.unwrapStaticInt(r.ip);
                const index = int.toInt(u32) catch {
                    return r.throw(
                        call.args[0],
                        "expected the value to be in range 0 to {d} (exclusive) got {d}",
                        .{ r.arg_types_len, int },
                    );
                };

                if (index >= r.arg_types_len) {
                    return r.throw(
                        call.args[0],
                        "expected the value to be in range 0 to {d} (exclusive) got {d}",
                        .{
                            r.arg_types_len,
                            index,
                        },
                    );
                }

                return r.addNodeRef(r.local_stack.items[index], .{
                    .tag = .get,
                    .data = .{ .local_index = index },
                });
            }

            return r.throw(node, "not a builtin", .{});
        },
        inline else => |_, tag| return r.throw(
            node,
            "'{s}' not implemented",
            .{@tagName(tag)},
        ),
    }
}

inline fn arena(r: *Resolver) mem.Allocator {
    return r.arena_state.allocator();
}

fn appendExtraFromScratch(r: *Resolver, start: u32) mem.Allocator.Error!RAst.ExtraIndex {
    const index: RAst.ExtraIndex = @enumFromInt(r.extra_data.items.len);
    try r.extra_data.appendSlice(r.gpa, r.scratch.items[start..]);
    r.scratch.items.len = start;

    return index;
}

inline fn nodeType(r: *const Resolver, node: RAst.Node.Index) InternPool.Type {
    return r.node_types.items[@intFromEnum(node)];
}

fn refType(r: *const Resolver, ref: RAst.Ref) InternPool.Type {
    if (ref.toNode()) |node| return r.nodeType(node);
    return ref.toValue().?.ty(r.ip);
}

fn addNodeRef(
    r: *Resolver,
    ty: InternPool.Type,
    node: RAst.Node,
) mem.Allocator.Error!RAst.Ref {
    const index = try r.addNode(ty, node);
    return @enumFromInt(@intFromEnum(index));
}

fn addBinopRefI32(
    r: *Resolver,
    tag: Ast.Node.Tag,
    lhs: RAst.Ref,
    rhs: RAst.Ref,
) mem.Allocator.Error!RAst.Ref {
    const node_tag: RAst.Node.Tag = switch (tag) {
        .add => .addi32,
        .sub => .subi32,
        .mul => .muli32,
        .pow => .powi32,
        .div => .divi32,
        else => unreachable,
    };

    return r.addNodeRef(.i32, .{
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
        .node = node,
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
