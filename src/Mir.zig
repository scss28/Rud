const std = @import("std");
const mem = std.mem;
const math = std.math;
const meta = std.meta;

const assert = std.debug.assert;

const InternPool = @import("InternPool.zig");
const Ast = @import("Ast.zig");
const Mir = @This();

pub const Error = struct {
    message: StringIndex,
    node: Ast.Node.Index,
};

pub const Instrs = std.MultiArrayList(Instr);
pub const StringIndex = enum(u32) { empty, _ };

instrs: Instrs.Slice,
string_bytes: []const u8,
extra_data: []const u32,

pub const Type = enum {
    int,
    float,
    nil,
};

pub const ExtraIndex = enum(u32) {
    errors,
    max_vars,

    arg_types,
    ret_type,

    _,
};

const Ref = enum(u32) {
    none = math.maxInt(u32),
};

pub const Instr = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum {
        no_op,

        load,
        store,

        addi,
        subi,
        muli,
        powi,
        divi,

        addf,
        subf,
        mulf,
        powf,
        divf,

        ldarg,

        call,
        ret,
    };

    pub const Data = union {
        binop: struct { lhs: Ref, rhs: Ref },
        call: struct { args: ExtraIndex, body: Index },
    };
    pub const Index = u32;
    pub const none: Index = 0;
};

pub fn deinit(self: *Mir, gpa: mem.Allocator) void {
    self.instrs.deinit(gpa);
    gpa.free(self.string_bytes);
    gpa.free(self.extra_data);
}

pub fn errors(self: *const Mir) []const Error {
    const index = self.extra_data[@intFromEnum(ExtraIndex.errors)];
    if (index == 0) return &.{};

    const len = self.extra_data[index];
    const ptr: [*]const Error = @ptrCast(self.extra_data[index + 1 ..].ptr);
    return ptr[0..len];
}

pub inline fn string(self: *const Mir, index: StringIndex) []const u8 {
    return mem.sliceTo(self.string_bytes[@intFromEnum(index)..], 0);
}

pub inline fn maxVars(self: *const Mir) u32 {
    return self.extra_data[@intFromEnum(ExtraIndex.max_vars)];
}

pub inline fn retType(self: *const Mir) Type {
    return @enumFromInt(self.extra_data[@intFromEnum(ExtraIndex.ret_type)]);
}

pub inline fn instrTag(self: *const Mir, instr: Instr.Index) Instr.Tag {
    return self.instrs.items(.tag)[instr];
}

pub inline fn instrData(self: *const Mir, instr: Instr.Index) Instr.Data {
    return self.instrs.items(.data)[instr];
}

pub fn rootNodes(self: *const Mir) []const Instr.Index {
    const data = self.instrs.items(.data)[0];
    return self.extra_data[data.lhs..data.rhs];
}
