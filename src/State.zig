const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const fmt = std.fmt;
const hash_map = std.hash_map;

const Ast = @import("Ast.zig");
const State = @This();

gpa: mem.Allocator,
arena_state: heap.ArenaAllocator,
vars: std.StringHashMapUnmanaged(Value),

eval_arena: heap.ArenaAllocator,

pub const Float = f64;
pub const Value = union(Tag) {
    num: Float,
    str: *Str,
    nil,

    pub const Tag = enum {
        num,
        str,
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
            .str => |str| try writer.print("{s}", .{str.slice()}),
            .nil => try writer.writeAll("nil"),
        }
    }

    pub const Str = packed struct {
        len: u32,
        capacity: u32,
        data: void,

        pub fn create(
            gpa: mem.Allocator,
            init_str: []const u8,
        ) mem.Allocator.Error!*Str {
            const bytes = try gpa.allocWithOptions(
                u8,
                @sizeOf(Str) + init_str.len,
                @alignOf(u32),
                null,
            );

            const ptr: *Str = @ptrCast(bytes);
            ptr.* = .{
                .len = init_str.len,
                .capacity = init_str.len,
                .data = {},
            };
            @memcpy(ptr.slice(), init_str);

            return ptr;
        }

        pub fn slice(self: *Str) []u8 {
            return @as([*]u8, @alignCast(@ptrCast(&self.data)))[0..self.len];
        }
    };
};

pub inline fn init(gpa: mem.Allocator) State {
    return .{
        .gpa = gpa,
        .arena_state = .init(gpa),
        .vars = .{},
        .eval_arena = .init(gpa),
    };
}

pub fn deinit(self: *State) void {
    self.arena_state.deinit();
    self.vars.deinit(self.gpa);
    self.eval_arena.deinit();
}

pub fn setVar(
    self: *State,
    name: []const u8,
    value: Value,
) mem.Allocator.Error!void {
    const res = try self.vars.getOrPut(self.gpa, name);
    if (!res.found_existing) {
        res.key_ptr.* = try self.arena_state.allocator().dupe(u8, name);
    }

    res.value_ptr.* = value;
}
