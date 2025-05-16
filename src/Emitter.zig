const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;

const Ast = @import("Ast.zig");
const Ir = @import("Ir.zig");
const Instr = Ir.Instr;

const Emitter = @This();

gpa: mem.Allocator,

index: Ast.Node.Index,
ast: *const Ast,

instrs: Ir.Instrs,
consts: Ir.Consts,
string_bytes: std.ArrayListUnmanaged(u8),
errors: std.ArrayListUnmanaged(Ir.Error),

vars: std.StringArrayHashMapUnmanaged(Type),

pub fn emit(gpa: mem.Allocator, ast: *const Ast) mem.Allocator.Error!Ir {
    var e: Emitter = .{
        .gpa = gpa,
        .index = 0,
        .ast = ast,
        .instrs = .{},
        .consts = .{},
        .string_bytes = .{},
        .errors = .{},
        .vars = .{},
    };
    defer {
        e.vars.deinit(gpa);
    }

    for (ast.rootNodes()) |node| {
        const ty = e.inspectNode(node) catch |err| switch (err) {
            error.OutOfMemory => |oom| return oom,
            error.Emit => continue,
        };

        if (ty != .nil) {
            const err = e.throw(
                "top level expression should not produce a value",
                node,
            );

            if (err == error.OutOfMemory) return error.OutOfMemory;
        }
    }

    return .{
        .instrs = e.instrs.toOwnedSlice(),
        .consts = e.consts.toOwnedSlice(),
        .string_bytes = try e.string_bytes.toOwnedSlice(gpa),
        .errors = try e.errors.toOwnedSlice(gpa),
    };
}

const Type = enum {
    nil,
    int,
    float,
    str,
};

fn inspectNode(
    self: *Emitter,
    node: Ast.Node.Index,
) (mem.Allocator.Error || error{Emit})!Type {
    switch (self.ast.full(node)) {
        .identifier => |slice| {
            return self.vars.get(slice) orelse self.throw(
                "variable not initialized",
                node,
            );
        },
        .literal_int => |slice| {
            const int = fmt.parseInt(Ir.Const.Int, slice, 10) catch unreachable;
            try self.appendInstr(.{
                .tag = .pushi,
                .data = try self.addConst(.{ .int = int }),
            });

            return .int;
        },
        .literal_float => |slice| {
            const float = fmt.parseFloat(Ir.Const.Float, slice) catch unreachable;
            try self.appendInstr(.{
                .tag = .pushf,
                .data = try self.addConst(.{ .float = float }),
            });

            return .float;
        },
        inline .add, .sub, .mul, .pow, .div => |binop, tag| {
            const lhs = try self.inspectNode(binop.lhs);
            const rhs = try self.inspectNode(binop.rhs);

            if (lhs != .int or lhs != rhs) {
                const message = fmt.comptimePrint(
                    "unable to perform {s}",
                    .{@tagName(tag)},
                );
                return self.throw(message, node);
            }

            return .int;
        },
        .assign => |assign| {
            const ty = try self.inspectNode(assign.rhs);
            const res = try self.vars.getOrPut(self.gpa, assign.identifier);
            res.value_ptr.* = ty;

            try self.appendInstr(.{
                .tag = .store,
                .data = @intCast(res.index),
            });

            return .nil;
        },
        else => @panic("not implemented"),
    }
}

fn appendInstr(self: *Emitter, instr: Instr) mem.Allocator.Error!void {
    try self.instrs.append(self.gpa, instr);
}

fn addConst(self: *Emitter, c: Ir.Const) mem.Allocator.Error!Ir.Const.Index {
    const index: Ir.Const.Index = @intCast(self.consts.len);
    try self.consts.append(self.gpa, c);
    return index;
}

fn throw(
    self: *Emitter,
    message: []const u8,
    node: Ast.Node.Index,
) (error{Emit} || mem.Allocator.Error) {
    try self.errors.append(self.gpa, .{
        .message = message,
        .node = node,
    });

    return error.Emit;
}
