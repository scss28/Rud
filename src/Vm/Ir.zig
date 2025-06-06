const std = @import("std");
const mem = std.mem;

const root = @import("root");
const RAst = root.RAst;
const InternPool = root.InternPool;

const Ir = @This();

pub const Instrs = std.MultiArrayList(Instr);

instrs: Instrs.Slice,
string_bytes: []const u8,
extra_data: []const u32,

pub const ExtraIndex = enum(u32) {
    max_locals,
    _,
};

pub const StringIndex = enum(u32) {
    empty,
    _,
};

pub const Instr = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum {
        pushi32,

        get,
        set,

        addi32,
        subi32,
        muli32,
        powi32,
        divi32,

        call,
        ret,

        exit,
    };

    pub const Data = union {
        instr: Index,
        locals_index: u32,
        i32: i32,
        extra_index: ExtraIndex,
    };
    pub const Index = enum(u32) { _ };
};

pub fn emit(
    gpa: mem.Allocator,
    rast: *const RAst,
    ip: *const InternPool,
) mem.Allocator.Error!Ir {
    var e: Emitter = .{
        .gpa = gpa,
        .rast = rast,
        .ip = ip,
        .instrs = .empty,
        .extra_data = .empty,
        .string_bytes = .empty,
        .defered_calls = .empty,
    };
    defer {
        e.defered_calls.deinit(gpa);
    }

    const reserved_count = @typeInfo(ExtraIndex).@"enum".fields.len;
    try e.extra_data.ensureTotalCapacity(gpa, reserved_count);
    e.extra_data.items.len += reserved_count;

    e.extra_data.items[@intFromEnum(ExtraIndex.max_locals)] = rast.maxLocals();

    for (rast.rootNodes()) |ref| try e.emitInstrRef(ref);
    try e.appendInstr(.{
        .tag = .exit,
        .data = undefined,
    });

    // While loop because the defered calls may get added during iteration.
    var i: u32 = 0;
    while (i < e.defered_calls.items.len) : (i += 1) {
        const dcall = e.defered_calls.items[i];

        const instr: u32 = @intCast(e.instrs.len);
        for (0..dcall.arg_count) |j| {
            try e.appendInstr(.{
                .tag = .set,
                .data = .{ .locals_index = @intCast(j) },
            });
        }

        try e.emitInstrNode(dcall.call_expr);
        try e.appendInstr(.{
            .tag = .ret,
            .data = undefined,
        });
        e.extra_data.items[@intFromEnum(dcall.call_instr_extra)] = instr;
    }

    return .{
        .instrs = e.instrs.toOwnedSlice(),
        .extra_data = try e.extra_data.toOwnedSlice(gpa),
        .string_bytes = try e.string_bytes.toOwnedSlice(gpa),
    };
}

pub fn deinit(ir: *Ir, gpa: mem.Allocator) void {
    ir.instrs.deinit(gpa);
    gpa.free(ir.extra_data);
    gpa.free(ir.string_bytes);
}

pub fn instrTag(ir: *const Ir, instr: Instr.Index) Instr.Tag {
    return ir.instrs.items(.tag)[@intFromEnum(instr)];
}

pub fn instrData(ir: *const Ir, instr: Instr.Index) Instr.Data {
    return ir.instrs.items(.data)[@intFromEnum(instr)];
}

pub fn maxLocals(ir: *const Ir) u32 {
    return ir.extra_data[@intFromEnum(ExtraIndex.max_locals)];
}

const Emitter = struct {
    gpa: mem.Allocator,

    rast: *const RAst,
    ip: *const InternPool,

    instrs: std.MultiArrayList(Instr),
    extra_data: std.ArrayListUnmanaged(u32),
    string_bytes: std.ArrayListUnmanaged(u8),

    defered_calls: std.ArrayListUnmanaged(struct {
        call_instr_extra: ExtraIndex,
        call_expr: RAst.Node.Index,
        arg_count: u32,
    }),

    fn emitInstrRef(e: *Emitter, ref: RAst.Ref) mem.Allocator.Error!void {
        if (ref.toValue()) |value| {
            switch (value.ty(e.ip)) {
                .i32 => {
                    try e.appendInstr(.{
                        .tag = .pushi32,
                        .data = .{ .i32 = value.unwrapInt(i32, e.ip) },
                    });
                },
                .f32 => unreachable, // TODO
                else => unreachable,
            }

            return;
        }

        try e.emitInstrNode(ref.toNode().?);
    }

    fn emitInstrNode(e: *Emitter, node: RAst.Node.Index) mem.Allocator.Error!void {
        switch (e.rast.nodeTag(node)) {
            .get => {
                const index = e.rast.nodeData(node).local_index;
                try e.appendInstr(.{
                    .tag = .get,
                    .data = .{ .locals_index = index },
                });
            },
            .set => {
                const set = e.rast.nodeData(node).set;
                try e.emitInstrRef(set.ref);
                try e.appendInstr(.{
                    .tag = .set,
                    .data = .{ .locals_index = set.local },
                });
            },
            inline .addi32, .subi32, .muli32, .powi32, .divi32 => |tag| {
                const binop = e.rast.nodeData(node).binop;
                try e.emitInstrRef(binop.lhs);
                try e.emitInstrRef(binop.rhs);

                try e.appendInstr(.{
                    .tag = @field(Instr.Tag, @tagName(tag)),
                    .data = undefined,
                });
            },
            .call => {
                const call = e.rast.nodeData(node).call;
                const args = e.rast.extraSlice(call.args_and_offset);
                for (args) |arg| try e.emitInstrNode(arg);

                const offset = args.ptr[args.len];

                const extra_index: ExtraIndex = @enumFromInt(e.extra_data.items.len);
                try e.extra_data.appendSlice(e.gpa, &.{ 0, @intFromEnum(offset) });

                try e.appendInstr(.{
                    .tag = .call,
                    .data = .{ .extra_index = extra_index },
                });

                try e.defered_calls.append(e.gpa, .{
                    .call_instr_extra = extra_index,
                    .call_expr = call.expr,
                    .arg_count = @intCast(args.len),
                });
            },
            inline .ret, .exit => |tag| {
                try e.emitInstrRef(e.rast.nodeData(node).ref);
                try e.appendInstr(.{
                    .tag = @field(Instr.Tag, @tagName(tag)),
                    .data = undefined,
                });
            },
            else => unreachable, // TODO
        }
    }

    fn appendInstr(e: *Emitter, instr: Instr) mem.Allocator.Error!void {
        try e.instrs.append(e.gpa, instr);
    }
};
