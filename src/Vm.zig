const std = @import("std");
const math = std.math;

const assert = std.debug.assert;

pub const Ir = @import("Vm/Ir.zig");
const Vm = @This();

ir: *const Ir,
instr: Ir.Instr.Index,

stack_buf: []usize,
stack_ptr: u32,

locals_buf: []usize,
locals_offset: u32,

call_stack_buf: []u32,
call_stack_ptr: u32,

pub fn init(
    ir: *const Ir,
    stack_buf: []usize,
    locals_buf: []usize,
    call_stack_buf: []u32,
) Vm {
    assert(locals_buf.len >= ir.maxLocals());
    return .{
        .ir = ir,
        .instr = @enumFromInt(0),
        .stack_buf = stack_buf,
        .stack_ptr = 0,
        .locals_buf = locals_buf,
        .locals_offset = 0,
        .call_stack_buf = call_stack_buf,
        .call_stack_ptr = 0,
    };
}

pub fn next(vm: *Vm) bool {
    switch (vm.ir.instrTag(vm.instr)) {
        .pushi32 => {
            const int = vm.ir.instrData(vm.instr).i32;
            vm.push(@as(u32, @bitCast(int)));
        },
        .set => {
            const index = vm.ir.instrData(vm.instr).locals_index;
            vm.locals()[index] = vm.pop();
        },
        .get => {
            const index = vm.ir.instrData(vm.instr).locals_index;
            vm.push(vm.locals()[index]);
        },
        inline .addi32, .subi32, .muli32, .powi32, .divi32 => |tag| {
            const rhs: i32 = @bitCast(@as(u32, @truncate(vm.pop())));
            const lhs: i32 = @bitCast(@as(u32, @truncate(vm.pop())));

            const res = switch (tag) {
                .addi32 => lhs + rhs,
                .subi32 => lhs - rhs,
                .muli32 => lhs * rhs,
                .powi32 => math.powi(i32, lhs, rhs) catch unreachable,
                .divi32 => @divTrunc(lhs, rhs),
                else => unreachable,
            };

            vm.push(@as(u32, @bitCast(res)));
        },
        .call => {
            vm.call_stack_buf[vm.call_stack_ptr] = @intFromEnum(vm.instr) + 1;
            vm.call_stack_buf[vm.call_stack_ptr + 1] = vm.locals_offset;
            vm.call_stack_ptr += 2;

            const extra_index: u32 = @intFromEnum(vm.ir.instrData(vm.instr).extra_index);
            const extra = vm.ir.extra_data[extra_index .. extra_index + 2];
            vm.locals_offset += extra[1];

            vm.instr = @enumFromInt(extra[0]);
            return true;
        },
        .ret => {
            vm.call_stack_ptr -= 2;
            vm.instr = @enumFromInt(vm.call_stack_buf[vm.call_stack_ptr]);
            vm.locals_offset = vm.call_stack_buf[vm.call_stack_ptr + 1];
        },
        .exit => return false,
    }

    vm.instr = @enumFromInt(@intFromEnum(vm.instr) + 1);
    return true;
}

fn push(vm: *Vm, value: usize) void {
    vm.stack_buf[vm.stack_ptr] = value;
    vm.stack_ptr += 1;
}

fn pop(vm: *Vm) usize {
    vm.stack_ptr -= 1;
    return vm.stack_buf[vm.stack_ptr];
}

fn locals(vm: *Vm) []usize {
    return vm.locals_buf[vm.locals_offset..];
}
