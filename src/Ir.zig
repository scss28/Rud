const std = @import("std");
const mem = std.mem;
const meta = std.meta;

const assert = std.debug.assert;

const Ast = @import("Ast.zig");
const Ir = @This();

pub const Error = struct {
    message: StringIndex,
    node: Ast.Node.Index,
};

pub const consts = union {
    pub const Int = i32;
    pub const Float = f32;
};

pub const Instrs = std.MultiArrayList(Instr);

instrs: Instrs.Slice,
string_bytes: []const u8,
extra_data: []const u32,

pub const ExtraIndex = enum(u32) {
    errors, // if extra_data[errors] == 0 then no compile errors
    var_count,
    _,
};

pub const StringIndex = enum(u32) {
    empty,
    _,
};

pub fn deinit(self: *Ir, gpa: mem.Allocator) void {
    self.instrs.deinit(gpa);
    gpa.free(self.string_bytes);
    gpa.free(self.extra_data);

    self.* = undefined;
}

pub inline fn extraData(self: *const Ir, T: type, index: ExtraIndex) T {
    const i: u32 = @intFromEnum(index);
    if (T == u32) return self.extra_data[i];
    if (@typeInfo(T) == .@"enum") return @enumFromInt(self.extra_data[i]);

    return @bitCast(self.extra_data[i]);
}

pub inline fn string(self: *const Ir, index: StringIndex) []const u8 {
    return mem.sliceTo(self.string_bytes[@intFromEnum(index)..], 0);
}

pub inline fn varCount(self: *const Ir) u32 {
    return self.extra_data[@intFromEnum(ExtraIndex.var_count)];
}

pub fn errorSlice(self: *const Ir) ?[]const Error {
    const index = self.extra_data[@intFromEnum(ExtraIndex.errors)];
    if (index == 0) return null;

    const len = self.extra_data[index];
    const ptr: [*]const Error = @ptrCast(self.extra_data[index + 1 ..].ptr);
    return ptr[0..len];
}

pub fn instrTag(self: *const Ir, instr: Instr.Index) Instr.Tag {
    return self.instrs.items(.tag)[instr];
}

pub fn instrData(self: *const Ir, instr: Instr.Index) Instr.Data {
    return self.instrs.items(.data)[instr];
}

pub fn instrDataExtra(self: *const Ir, T: type, instr: Instr.Index) T {
    const index: ExtraIndex = @enumFromInt(self.instrData(instr));
    return self.extraData(T, index);
}

pub const Instr = struct {
    tag: Tag,
    data: Data,

    const Tag = enum {
        pushi,
        pushf,
        pushs,

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

        prints,
        printi,
        printf,

        jmp,
    };
    const Data = u32;

    pub inline fn tagOnly(tag: Tag) Instr {
        return .{
            .tag = tag,
            .data = undefined,
        };
    }

    pub const Index = u32;
};
