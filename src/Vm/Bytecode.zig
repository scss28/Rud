const std = @import("std");
const mem = std.mem;
const meta = std.meta;

const root = @import("root");
const RAst = root.RAst;
const InternPool = root.InternPool;

const Bytecode = @This();

pub const OpCode = union(enum) {
    pub const Binop = struct {
        r: u8,
        a: u8,
        b: u8,
    };

    load: struct {
        r: u8,
        value: usize,
    },

    addi32: Binop,
    subi32: Binop,
    muli32: Binop,
    powi32: Binop,
    divi32: Binop,

    exit,
};

pub fn emit(gpa: mem.Allocator, rast: *const RAst, ip: *const InternPool) ![]const u8 {
    var e: Emitter = .{
        .gpa = gpa,
        .rast = rast,
        .ip = ip,
        .code = .empty,
        .free_register = @enumFromInt(0),
    };

    for (rast.rootNodes()) |node| e.emitNode(0, node);

    return try e.code.toOwnedSlice(gpa);
}

const Emitter = struct {
    gpa: mem.Allocator,
    rast: *const RAst,
    ip: *const InternPool,
    code: std.ArrayListUnmanaged(u8),

    register: u8,

    fn emitNode(
        e: *Emitter,
        r: u8,
        node: RAst.Node.Index,
    ) mem.Allocator.Error!void {
        switch (e.rast.nodeTag(node)) {
            .i32 => {
                const int = e.rast.nodeData().i32;
                try e.encodeOpCode(.{
                    .tag = .load,
                });
            },
            inline .addi32, .subi32, .muli32, .powi32, .divi32 => |tag| {
                const binop = e.rast.nodeData(node).binop;

                defer e.register = r + 1;
                const a = e.pushRegister();
                try e.emitRef(a, binop.lhs);

                const b = e.getRegister();
                try e.emitRef(b, binop.rhs);

                try e.encodeOpCode(@unionInit(OpCode, @tagName(tag), .{
                    .r = r,
                    .a = a,
                    .b = b,
                }));

                return r;
            },
            .call => {
                const call = e.rast.nodeData(node).call;
                const args = call.args.toSlice(e.rast);
                for (args, 0..) |arg, i| try e.emitNode(@intCast(i), arg);

                e.encodeOpCode(.{
                    .call = .{
                        .r = r,
                    },
                });
            },
            inline .ret, .exit => |_| {},
            else => unreachable, // TODO
        }
    }

    fn encodeOpCode(e: *Emitter, op: OpCode) !void {
        switch (op) {
            inline else => |payload, tag| {
                const Payload = @TypeOf(payload);

                try e.code.ensureUnusedCapacity(e.gpa, @sizeOf(Payload) + 1);
                e.code.appendAssumeCapacity(@intFromEnum(tag));

                for (@typeInfo(Payload).@"struct".fields) |field| {
                    const bytes = mem.asBytes(&@field(payload, field.name));
                    e.code.appendSliceAssumeCapacity(bytes);
                }
            },
        }
    }
};
