const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const fmt = std.fmt;
const hash_map = std.hash_map;

const Ast = @import("Ast.zig");
const State = @This();

gpa: mem.Allocator,
string_bytes: std.ArrayListUnmanaged(u8),
vars: std.AutoHashMapUnmanaged(u32, Value),

eval_arena: heap.ArenaAllocator,

pub const Float = f64;
pub const Value = union(Tag) {
    num: Float,
    nil,

    pub const Tag = enum {
        num,
        nil,
    };

    pub fn format(
        self: Value,
        comptime _: []const u8,
        _: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .num => |num| try writer.print("{d}", .{num}),
            .nil => try writer.writeAll("nil"),
        }
    }
};

pub inline fn init(gpa: mem.Allocator) State {
    return .{
        .gpa = gpa,
        .string_bytes = .{},
        .vars = .{},
        .eval_arena = .init(gpa),
    };
}

pub fn deinit(self: *State) void {
    self.string_bytes.deinit(self.gpa);
    self.vars.deinit(self.gpa);
    self.eval_arena.deinit();
}

pub fn setVar(
    self: *State,
    name: []const u8,
    value: Value,
) mem.Allocator.Error!void {
    const res = try self.vars.getOrPutAdapted(
        self.gpa,
        name,
        std.hash_map.StringIndexAdapter{ .bytes = &self.string_bytes },
    );

    if (!res.found_existing) {
        res.key_ptr.* = @intCast(self.string_bytes.items.len);
        try self.string_bytes.appendSlice(self.gpa, name);
    }

    res.value_ptr.* = value;
}

pub fn getVar(self: *State, name: []const u8) ?Value {
    return self.vars.getAdapted(
        name,
        hash_map.StringIndexAdapter{ .bytes = &self.string_bytes },
    );
}
