const std = @import("std");
const mem = std.mem;
const io = std.io;
const meta = std.meta;
const math = std.math;

const Ir = @import("Ir.zig");
const Vm = @This();

gpa: mem.Allocator,

index: Ir.Instr.Index,
ir: *const Ir,

vars: []usize,
stack: Stack,

const Stack = struct {
    ptr: usize,
    buf: []usize,

    fn push(self: *Stack, value: usize) void {
        self.buf[self.ptr] = value;
        self.ptr += 1;
    }

    fn pop(self: *Stack) usize {
        self.ptr -= 1;
        return self.buf[self.ptr];
    }
};

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

pub fn next(self: *Vm) bool {
    if (self.index >= self.ir.instrs.len) return false;
    switch (self.ir.instrTag(self.index)) {
        .pushi => {
            const UInt = meta.Int(.unsigned, @bitSizeOf(Ir.consts.Int));
            const int = self.ir.instrDataExtra(Ir.consts.Int, self.index);
            self.stack.push(@as(UInt, @bitCast(int)));
        },
        .pushf => {
            const UInt = meta.Int(.unsigned, @bitSizeOf(Ir.consts.Float));
            const float = self.ir.instrDataExtra(Ir.consts.Float, self.index);
            self.stack.push(@as(UInt, @bitCast(float)));
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
            const value = self.stack.pop();
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

            const UInt = meta.Int(.unsigned, @bitSizeOf(Operand));

            const rhs: Operand = @bitCast(@as(UInt, @truncate(self.stack.pop())));
            const lhs: Operand = @bitCast(@as(UInt, @truncate(self.stack.pop())));

            const value = switch (tag) {
                .addi, .addf => lhs + rhs,
                .subi, .subf => lhs - rhs,
                .muli, .mulf => lhs * rhs,
                .powi, .powf => math.pow(Operand, lhs, rhs),
                .divi, .divf => @divTrunc(lhs, rhs),
                else => unreachable,
            };

            self.stack.push(@as(UInt, @bitCast(value)));
        },
        .prints => {
            const index: u32 = @truncate(self.stack.pop());
            const str = self.ir.string(@enumFromInt(index));
            io.getStdOut().writeAll(str) catch {};
        },
        .printi => {},
        .printf => {},
        .jmp => {
            self.index = self.ir.instrData(self.index);
            return true;
        },
    }

    self.index += 1;
    return true;
}
