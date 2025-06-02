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
    errors,
    max_locals,
    ret_type,
    _,
};
pub const StringIndex = enum(u32) { empty, _ };

pub const Error = struct {
    message: StringIndex,
    node: Ast.Node.Index,
};
pub const Nodes = std.MultiArrayList(Node);
pub const LocalIndex = u32;

pub const Ref = enum(u32) {
    _,

    pub fn initNode(node: Node.Index) Ref {
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
};

pub const Node = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum {
        get,
        set,

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

        call,
        ret,
    };

    pub const Data = union {
        root: struct { start: u32, end: u32 },
        ref: Ref,
        local_index: LocalIndex,
        set: struct {
            local: LocalIndex,
            ref: Ref,
        },
        binop: struct {
            lhs: Ref,
            rhs: Ref,
        },
        call: struct {
            args: ExtraIndex,
            expr: Ref,
        },
    };

    pub const Index = enum(u32) { _ };
};

pub fn deinit(r: *RAst, gpa: mem.Allocator) void {
    r.nodes.deinit(gpa);
    gpa.free(r.string_bytes);
    gpa.free(r.extra_data);
}

pub fn errors(r: *const RAst) []const Error {
    const index = r.extra_data[@intFromEnum(ExtraIndex.errors)];
    if (index == 0) return &.{};

    const len = r.extra_data[index];
    const ptr: [*]const Error = @ptrCast(r.extra_data[index + 1 ..].ptr);
    return ptr[0..len];
}

pub inline fn string(r: *const RAst, index: StringIndex) []const u8 {
    return mem.sliceTo(r.string_bytes[@intFromEnum(index)..], 0);
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
    const root = r.nodes.items(.data)[0].root;
    return @ptrCast(r.extra_data[root.start..root.end]);
}
