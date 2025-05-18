const std = @import("std");
const fmt = std.fmt;
const meta = std.meta;
const mem = std.mem;

const assert = std.debug.assert;

const Ast = @import("Ast.zig");
const Ir = @import("Ir.zig");
const Instr = Ir.Instr;

const Emitter = @This();

const call_identifier_to_instr_tag: std.StaticStringMap(Instr.Tag) = .initComptime(.{
    .{ "max", .max },
});

gpa: mem.Allocator,

index: Ast.Node.Index,
ast: *const Ast,

instrs: Ir.Instrs,
string_bytes: std.ArrayListUnmanaged(u8),
extra_data: std.ArrayListUnmanaged(u32),

vars: std.StringArrayHashMapUnmanaged(Type),
errors: std.ArrayListUnmanaged(Ir.Error),

pub fn emit(gpa: mem.Allocator, ast: *const Ast) mem.Allocator.Error!Ir {
    var e: Emitter = .{
        .gpa = gpa,

        .index = 0,
        .ast = ast,

        .instrs = .{},
        .extra_data = .{},
        .string_bytes = .{},

        .errors = .{},
        .vars = .{},
    };
    defer {
        e.errors.deinit(gpa);
        e.vars.deinit(gpa);
    }

    // StringIndex.empty
    try e.string_bytes.append(gpa, 0);

    // Reserved slots of ExtraIndex
    const reserved_count = @typeInfo(Ir.ExtraIndex).@"enum".fields.len;
    try e.extra_data.ensureTotalCapacity(gpa, ast.nodes.len + reserved_count);
    e.extra_data.items.len += reserved_count;

    for (ast.rootNodes()) |node| {
        const ty = e.inspectNode(node) catch |err| switch (err) {
            error.OutOfMemory => |oom| return oom,
            error.Emit => continue,
        };

        if (ty != .nil) {
            const err = e.throw(node, "expected {s} got {s}", .{
                @tagName(Type.nil),
                @tagName(ty),
            });

            if (err == error.OutOfMemory) return error.OutOfMemory;
        }
    }

    const errors_index: u32 = @intFromEnum(Ir.ExtraIndex.errors);
    if (e.errors.items.len == 0) {
        e.extra_data.items[errors_index] = 0;
    } else {
        try e.extra_data.ensureUnusedCapacity(
            gpa,
            1 + e.errors.items.len * @typeInfo(Ir.Error).@"struct".fields.len,
        );

        e.extra_data.items[errors_index] = e.addExtraAssumeCapacity(.{
            @as(u32, @intCast(e.errors.items.len)),
        });

        for (e.errors.items) |err| {
            _ = e.addExtraAssumeCapacity(err);
        }
    }

    const var_count_index: u32 = @intFromEnum(Ir.ExtraIndex.var_count);
    e.extra_data.items[var_count_index] = @intCast(e.vars.entries.len);

    return .{
        .instrs = e.instrs.toOwnedSlice(),
        .string_bytes = try e.string_bytes.toOwnedSlice(gpa),
        .extra_data = try e.extra_data.toOwnedSlice(gpa),
    };
}

const Type = union(enum) {
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
            const index = self.vars.getIndex(slice) orelse return self.throw(
                node,
                "variable '{s}' not initialized",
                .{slice},
            );

            try self.appendInstr(.{
                .tag = .load,
                .data = @intCast(index),
            });

            return self.vars.entries.items(.value)[index];
        },
        .literal_int => |slice| {
            const int = fmt.parseInt(Ir.consts.Int, slice, 10) catch unreachable;
            try self.appendInstr(.{
                .tag = .pushi,
                .data = try self.addExtra(.{ .int = int }),
            });

            return .int;
        },
        .literal_float => |slice| {
            const float = fmt.parseFloat(Ir.consts.Float, slice) catch unreachable;
            try self.appendInstr(.{
                .tag = .pushf,
                .data = try self.addExtra(.{ .float = float }),
            });

            return .float;
        },
        .literal_str => |slice| {
            try self.string_bytes.ensureUnusedCapacity(self.gpa, slice.len - 1);
            const index: u32 = @intCast(self.string_bytes.items.len);
            var i: usize = 1;
            while (i < slice.len - 1) : (i += 1) {
                var byte = slice[i];
                if (byte == '\\') {
                    defer i += 1;
                    byte = switch (slice[i + 1]) {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        else => return self.throw(
                            node,
                            "invalid escape character '{c}'",
                            .{slice[i + 1]},
                        ),
                    };
                }

                self.string_bytes.appendAssumeCapacity(byte);
            }
            self.string_bytes.appendAssumeCapacity(0);

            try self.appendInstr(.{
                .tag = .pushs,
                .data = index,
            });

            return .str;
        },
        inline .add, .sub, .mul, .pow, .div => |binop, tag| {
            const lhs = try self.inspectNode(binop.lhs);
            const rhs = try self.inspectNode(binop.rhs);

            if (lhs == .int and rhs == .int) {
                const instr_tag = switch (tag) {
                    .add => .addi,
                    .sub => .subi,
                    .mul => .muli,
                    .pow => .powi,
                    .div => .divi,
                    else => unreachable,
                };
                try self.appendInstr(.tagOnly(instr_tag));

                return .int;
            } else if (lhs == .float and rhs == .float) {
                const instr_tag = switch (tag) {
                    .add => .addf,
                    .sub => .subf,
                    .mul => .mulf,
                    .pow => .powf,
                    .div => .divf,
                    else => unreachable,
                };
                try self.appendInstr(.tagOnly(instr_tag));

                return .float;
            } else {
                return self.throw(
                    node,
                    "unable to perform {s} on {s} and {s}",
                    .{
                        @tagName(tag),
                        @tagName(lhs),
                        @tagName(rhs),
                    },
                );
            }
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
        .builtin_call => |bcall| {
            const tag = meta.stringToEnum(enum { print }, bcall.identifier) orelse {
                return self.throw(node, "not a builtin", .{});
            };
            switch (tag) {
                .print => {
                    if (bcall.args.len != 1) return self.throw(
                        node,
                        "expected 1 argument",
                        .{},
                    );

                    const ty = try self.inspectNode(bcall.args[0]);
                    if (ty != .str) return self.throw(
                        bcall.args[0],
                        "expected {s} argument got {s}",
                        .{ @tagName(Type.str), @tagName(ty) },
                    );

                    try self.appendInstr(.tagOnly(.prints));
                    return .nil;
                },
            }
        },
        inline else => |_, tag| return self.throw(
            node,
            "{s} not implemenented",
            .{@tagName(tag)},
        ),
    }
}

fn inspectCallArgs(
    self: *Emitter,
    call: Ast.Node.Full.Call,
    types: []const Type,
) (mem.Allocator.Error || error{Emit})!void {
    if (call.args.len != types.len) {
        return self.throw(call.callee, "expected {d} arguments got {d}", .{
            types.len,
            call.args.len,
        });
    }

    for (call.args, types) |arg, ty| {
        const arg_ty = try self.inspectNode(arg);
        if (arg_ty != ty) {
            return self.throw(arg, "expected argument type {s} got {s}", .{
                @tagName(ty),
                @tagName(arg_ty),
            });
        }
    }
}

fn appendInstr(self: *Emitter, instr: Instr) mem.Allocator.Error!void {
    try self.instrs.append(self.gpa, instr);
}

fn addExtra(self: *Emitter, extra: anytype) mem.Allocator.Error!u32 {
    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    try self.extra_data.ensureUnusedCapacity(self.gpa, fields.len);

    return self.addExtraAssumeCapacity(extra);
}

fn addExtraAssumeCapacity(self: *Emitter, extra: anytype) u32 {
    const index: u32 = @intCast(self.extra_data.items.len);

    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    self.extra_data.items.len += fields.len;
    self.setExtra(index, extra);

    return index;
}

fn setExtra(self: *Emitter, index: u32, extra: anytype) void {
    var i = index;
    inline for (@typeInfo(@TypeOf(extra)).@"struct".fields) |field| {
        if (field.type == u32) {
            self.extra_data.items[i] = @field(extra, field.name);
        } else if (@typeInfo(field.type) == .@"enum") {
            self.extra_data.items[i] = @intFromEnum(@field(extra, field.name));
        } else {
            self.extra_data.items[i] = @bitCast(@field(extra, field.name));
        }

        i += 1;
    }
}

fn printString(
    self: *Emitter,
    comptime fmt_str: []const u8,
    args: anytype,
) mem.Allocator.Error!Ir.StringIndex {
    const string: Ir.StringIndex = @enumFromInt(self.string_bytes.items.len);
    try self.string_bytes.writer(self.gpa).print(fmt_str ++ "\x00", args);

    return string;
}

fn throw(
    self: *Emitter,
    node: Ast.Node.Index,
    comptime fmt_str: []const u8,
    args: anytype,
) (error{Emit} || mem.Allocator.Error) {
    const message = try self.printString(fmt_str, args);
    try self.errors.append(self.gpa, .{
        .message = message,
        .node = node,
    });

    return error.Emit;
}
