const std = @import("std");
const mem = std.mem;
const io = std.io;
const meta = std.meta;
const math = std.math;

const assert = std.debug.assert;

const Ir = @import("Ir.zig");
const Vm = @This();

gpa: mem.Allocator,

index: Ir.Instr.Index,
ir: *const Ir,

vars: []usize,
stack: Stack(usize),
call_stack: Stack(u32),

fn Stack(Slot: type) type {
    return struct {
        ptr: usize,
        buf: []Slot,

        fn push(self: *Stack, value: anytype) void {
            const T = @TypeOf(value);

            assert(@sizeOf(T) <= @sizeOf(usize));
            const UInt = meta.Int(.unsigned, @bitSizeOf(T));

            self.buf[self.ptr] = @as(UInt, @bitCast(value));
            self.ptr += 1;
        }

        fn pop(self: *Stack, T: type) T {
            assert(@sizeOf(T) <= @sizeOf(usize));
            const UInt = meta.Int(.unsigned, @bitSizeOf(T));

            self.ptr -= 1;
            return @bitCast(@as(UInt, @truncate(self.buf[self.ptr])));
        }
    };
}

pub const Options = struct {
    stack_size: usize = 1_024,
};

pub fn init(gpa: mem.Allocator, ir: *const Ir, options: Options) mem.Allocator.Error!Vm {
    const vars = try gpa.alloc(usize, ir.varCount());
    const stack_buf = try gpa.alloc(usize, options.stack_size / @sizeOf(usize));
    return .{
        .gpa = gpa,
        .index = 0,
        .ir = ir,
        .vars = vars,
        .stack = .{
            .ptr = 0,
            .buf = stack_buf,
        },
    };
}

pub fn deinit(self: *Vm) void {
    self.gpa.free(self.stack.buf);
    self.gpa.free(self.vars);
}

pub fn run(self: *Vm) void {
    var instr: Ir.Instr.Index = 0;
    while (true) {
        switch (self.ir.instrTag(self.index)) {
            .pushi => {
                const int = self.ir.instrDataExtra(Ir.consts.Int, self.index);
                self.stack.push(int);
            },
            .pushf => {
                const float = self.ir.instrDataExtra(Ir.consts.Float, self.index);
                self.stack.push(float);
            },
            .pushs => {
                const index = self.ir.instrData(self.index);
                self.stack.push(index);
            },
            .load => {
                const i = self.ir.instrData(self.index);
                self.stack.push(self.vars[i]);
            },
            .store => {
                const i = self.ir.instrData(self.index);
                const value = self.stack.pop(usize);
                self.vars[i] = value;
            },
            inline .addi,
            .addf,
            .subi,
            .subf,
            .muli,
            .mulf,
            .powi,
            .powf,
            .divi,
            .divf,
            => |tag| {
                const Operand = switch (tag) {
                    .addi, .subi, .muli, .powi, .divi => Ir.consts.Int,
                    .addf, .subf, .mulf, .powf, .divf => Ir.consts.Float,
                    else => unreachable,
                };

                const rhs: Operand = self.stack.pop(Operand);
                const lhs: Operand = self.stack.pop(Operand);

                const value = switch (tag) {
                    .addi, .addf => lhs + rhs,
                    .subi, .subf => lhs - rhs,
                    .muli, .mulf => lhs * rhs,
                    .powi, .powf => math.pow(Operand, lhs, rhs),
                    .divi, .divf => @divTrunc(lhs, rhs),
                    else => unreachable,
                };

                self.stack.push(value);
            },
            .itf => {
                const float: Ir.consts.Float =
                    @floatFromInt(self.stack.pop(Ir.consts.Int));
                self.stack.push(float);
            },
            .fti => {
                const int: Ir.consts.Int =
                    @intFromFloat(self.stack.pop(Ir.consts.Float));
                self.stack.push(int);
            },
            .prints => {
                const index = self.stack.pop(u32);
                const str = self.ir.string(@enumFromInt(index));
                io.getStdOut().writeAll(str) catch {};
            },
            .printi => {
                const int = self.stack.pop(Ir.consts.Int);
                io.getStdOut().writer().print("{d}", .{int}) catch {};
            },
            .printf => {
                const float = self.stack.pop(Ir.consts.Float);
                io.getStdOut().writer().print("{d}", .{float}) catch {};
            },
            .printn => {
                io.getStdOut().writer().print("nil", .{}) catch {};
            },
            .jmp => {
                instr = self.ir.instrData(self.index);
                continue;
            },
            .call => {
                self.call_stack.push(instr + 1);
                instr = self.ir.instrData(self.index);
                continue;
            },
            .ret => {
                const index = self.call_stack.pop(Ir.Instr.Index);
                instr = index;
            },
            .exit => break,
        }

        instr += 1;
    }
}
