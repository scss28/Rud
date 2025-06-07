const std = @import("std");
const mem = std.mem;
const math = std.math;
const meta = std.meta;
const assert = std.debug.assert;

const Ast = @import("Ast.zig");
const InternPool = @import("InternPool.zig");
const RAst = @This();

nodes: Nodes.Slice,
string_bytes: []const u8,
extra_data: []const u32,

pub const ExtraIndex = enum(u32) {
    none,
    errors,
    max_locals,
    ret_type,
    _,
};

pub fn ExtraSlice(T: type) type {
    return enum(u32) {
        empty,
        _,

        fn toSlice(s: ExtraSlice(T), r: *const RAst) []const T {
            const i: u32 = @intFromEnum(s);
            const len = r.extra_data[i];
            return @ptrCast(r.extra_data[i + 1 .. i + 1 + len]);
        }
    };
}

pub const StringIndex = enum(u32) { empty, _ };
pub const Nodes = std.MultiArrayList(Node);

pub const Node = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum {
        arg,
        i32,
        f32,

        addi32,
        subi32,
        muli32,
        powi32,
        divi32,

        addf32,
        subf32,
        mulf32,
        powf32,
        divf32,

        block,
        call,
        ret,

        exit,
    };

    pub const Data = union {
        block: ExtraSlice(Index),

        i32: i32,
        f32: f32,
        str: StringIndex,
        arg: u32,
        node: Index,
        binop: struct {
            lhs: Index,
            rhs: Index,
        },
        call: struct {
            args: ExtraSlice(Index),
            body: Index,
        },
    };

    pub const Index = enum(u32) { _ };
};

pub fn deinit(r: *RAst, gpa: mem.Allocator) void {
    r.nodes.deinit(gpa);
    gpa.free(r.string_bytes);
    gpa.free(r.extra_data);
}

pub inline fn maxLocals(r: *const RAst) u32 {
    return r.extra_data[@intFromEnum(ExtraIndex.max_locals)];
}

pub inline fn retType(r: *const RAst) InternPool.Type {
    return @enumFromInt(r.extra_data[@intFromEnum(ExtraIndex.ret_type)]);
}

pub inline fn nodeTag(r: *const RAst, node: Node.Index) Node.Tag {
    return r.nodes.items(.tag)[@intFromEnum(node)];
}

pub inline fn nodeData(r: *const RAst, node: Node.Index) Node.Data {
    return r.nodes.items(.data)[@intFromEnum(node)];
}

pub fn rootNodes(r: *const RAst) []const Node.Index {
    const root = r.nodes.items(.data)[r.nodes.len - 1].block;
    return root.toSlice(r);
}
