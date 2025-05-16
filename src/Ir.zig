const std = @import("std");
const mem = std.mem;

const Ast = @import("Ast.zig");
const Ir = @This();

pub const Error = struct {
    message: []const u8,
    node: Ast.Node.Index,
};
pub const Const = union(enum) {
    pub const Int = i64;
    pub const Float = f64;

    int: Int,
    float: Float,
    str: StringSlice,

    pub const Index = u32;
};

pub const StringSlice = struct { start: u32, end: u32 };
pub const Instrs = std.MultiArrayList(Instr);
pub const Consts = std.MultiArrayList(Const);

instrs: Instrs.Slice,
consts: Consts.Slice,
string_bytes: []const u8,

errors: []const Error,

pub fn deinit(self: *Ir, gpa: mem.Allocator) void {
    self.instrs.deinit(gpa);
    self.consts.deinit(gpa);
    gpa.free(self.string_bytes);
    gpa.free(self.errors);

    self.* = undefined;
}

pub const Instr = struct {
    tag: Tag,
    data: u32,

    const Tag = enum {
        pushi,
        pushf,
        load,
        store,
    };

    pub const Index = u32;
};
