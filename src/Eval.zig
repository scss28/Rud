const std = @import("std");
const mem = std.mem;
const io = std.io;
const heap = std.heap;
const math = std.math;
const meta = std.meta;
const fmt = std.fmt;

const Ast = @import("Ast.zig");
const State = @import("State.zig");
const Eval = @This();

state: *State,
ast: *const Ast,

err: Error,

const Error = struct {
    node: Ast.Node.Index,
    message: []const u8,
};

pub inline fn init(state: *State, ast: *const Ast) Eval {
    return .{
        .state = state,
        .ast = ast,
        .err = undefined,
    };
}

pub inline fn deinit(self: *Eval) void {
    if (!self.state.eval_arena.reset(.retain_capacity)) {
        _ = self.state.eval_arena.reset(.free_all);
    }
}

pub fn eval(
    self: *Eval,
    node: Ast.Node.Index,
) (error{Eval} || mem.Allocator.Error)!State.Value {
    switch (self.ast.full(node)) {
        .identifier => |slice| {
            return self.state.vars.get(slice) orelse self.throwPrint(
                node,
                "uninitialized variable '{s}'",
                .{slice},
            );
        },
        .number => |slice| {
            const num = fmt.parseFloat(f32, slice) catch unreachable;
            return .{ .num = num };
        },
        inline .add, .sub, .mul, .pow, .div => |binop, tag| {
            const lhs = try self.evalExpect(binop.lhs, .num);
            const rhs = try self.evalExpect(binop.rhs, .num);

            const num = switch (tag) {
                .add => lhs + rhs,
                .sub => lhs - rhs,
                .mul => lhs * rhs,
                .pow => math.pow(State.Float, lhs, rhs),
                .div => lhs / rhs,
                else => unreachable,
            };

            return .{ .num = num };
        },
        .assign => |assign| {
            const value = try self.eval(assign.rhs);
            try self.state.setVar(assign.identifier, value);

            return .nil;
        },
        .call => |call| {
            if (self.ast.nodeTag(call.callee) == .identifier) blk: {
                const identifier = self.ast.nodeTokenSlice(call.callee);

                const builtin = meta.stringToEnum(
                    meta.DeclEnum(builtins),
                    identifier,
                ) orelse break :blk;

                switch (builtin) {
                    inline else => |tag| {
                        const func = @field(builtins, @tagName(tag));
                        const fn_info = @typeInfo(@TypeOf(func)).@"fn";

                        const args_len = @typeInfo(fn_info.params[1].type.?).array.len;
                        if (fn_info.params.len == 3) {
                            if (call.args.len < args_len) return self.throwPrint(
                                node,
                                "expected at least {d} arguments got {d}",
                                .{
                                    args_len,
                                    call.args,
                                },
                            );

                            return func(
                                self,
                                call.args[0..args_len].*,
                                call.args[args_len..],
                            );
                        } else {
                            if (call.args.len != args_len) return self.throwPrint(
                                node,
                                "expected {d} arguments got {d}",
                                .{
                                    args_len,
                                    call.args,
                                },
                            );

                            return func(self, call.args[0..args_len].*);
                        }
                    },
                }
            }

            return self.throw(call.callee, "not a function");
        },
    }
}

fn evalExpect(
    self: *Eval,
    node: Ast.Node.Index,
    comptime tag: State.Value.Tag,
) (error{Eval} || mem.Allocator.Error)!meta.TagPayload(State.Value, tag) {
    const value = try self.eval(node);
    return switch (value) {
        tag => |v| v,
        else => |found| self.throwPrint(node, "expected {s} found {s}", .{
            @tagName(tag),
            @tagName(found),
        }),
    };
}

inline fn arena(self: *const Eval) mem.Allocator {
    return self.state.eval_arena.allocator();
}

fn throw(self: *Eval, node: Ast.Node.Index, message: []const u8) error{Eval} {
    self.err = .{
        .node = node,
        .message = message,
    };
    return error.Eval;
}

fn throwPrint(
    self: *Eval,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) (mem.Allocator.Error || error{Eval}) {
    const message = try fmt.allocPrint(self.arena(), format, args);
    return self.throw(node, message);
}

const builtins = struct {
    pub fn lerp(
        env: *Eval,
        args: [3]Ast.Node.Index,
    ) (error{Eval} || mem.Allocator.Error)!State.Value {
        const a = try env.evalExpect(args[0], .num);
        const b = try env.evalExpect(args[1], .num);
        const t = try env.evalExpect(args[2], .num);

        return .{ .num = math.lerp(a, b, t) };
    }

    pub fn max(
        env: *Eval,
        args: [2]Ast.Node.Index,
        varargs: []const Ast.Node.Index,
    ) (error{Eval} || mem.Allocator.Error)!State.Value {
        var mx = try env.evalExpect(args[0], .num);

        const arg2 = try env.evalExpect(args[1], .num);
        if (arg2 > mx) mx = arg2;

        for (varargs) |node| {
            const arg = try env.evalExpect(node, .num);
            if (arg > mx) mx = arg;
        }

        return .{ .num = mx };
    }
};
