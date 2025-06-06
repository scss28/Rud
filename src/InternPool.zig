const std = @import("std");
const mem = std.mem;
const math = std.math;
const meta = std.meta;
const heap = std.heap;

const assert = std.debug.assert;

const Ast = @import("Ast.zig");
const InternPool = @This();

values: std.MultiArrayList(ComptimeValue),
numerics: std.ArrayListUnmanaged(usize),

pub fn init(gpa: mem.Allocator) mem.Allocator.Error!InternPool {
    var values: std.MultiArrayList(ComptimeValue) = .empty;
    try values.ensureUnusedCapacity(
        gpa,
        @typeInfo(Index).@"enum".fields.len,
    );
    values.len = values.capacity;

    return .{
        .values = values,
        .numerics = .empty,
    };
}

pub fn deinit(ip: *InternPool, gpa: mem.Allocator) void {
    ip.values.deinit(gpa);
    ip.numerics.deinit(gpa);

    ip.* = undefined;
}

pub fn addBigInt(
    ip: *InternPool,
    gpa: mem.Allocator,
    int: math.big.int.Const,
) mem.Allocator.Error!Index {
    const index: u32 = @intCast(ip.numerics.items.len);
    try ip.numerics.ensureUnusedCapacity(gpa, 1 + int.limbs.len);

    ip.numerics.appendAssumeCapacity(int.limbs.len);
    ip.numerics.appendSliceAssumeCapacity(int.limbs);

    return ip.addValue(gpa, .{
        .tag = if (int.positive) .int_positive else .int_negative,
        .data = index,
    });
}

pub fn addFn(
    ip: *InternPool,
    gpa: mem.Allocator,
    decl: Ast.Node.Index,
) mem.Allocator.Error!Index {
    return ip.addValue(gpa, .{
        .tag = .@"fn",
        .data = decl,
    });
}

fn addValue(
    ip: *InternPool,
    gpa: mem.Allocator,
    value: ComptimeValue,
) mem.Allocator.Error!Index {
    const index = ip.values.len;
    try ip.values.append(gpa, value);

    return @enumFromInt(index);
}

pub const ComptimeValue = struct {
    tag: Tag,
    data: u32,

    pub const Tag = enum {
        /// `data` is index into `numerics`
        int_positive,
        /// `data` is index into `numerics`
        int_negative,

        @"fn",
    };
};

pub const Index = enum(u32) {
    type_i32,
    type_f32,

    none,
    type_none,
    type_static_int,
    type_static_str,
    type_type,
    type_fn,
    _,

    pub fn toType(i: Index, ip: *const InternPool) ?Type {
        switch (i) {
            .type_i32,
            .type_f32,
            .type_static_int,
            .type_static_str,
            .type,
            => {},
            else => {
                const tag = ip.values.items(.tag)[@intFromEnum(i)];
                switch (tag) {
                    else => return null,
                }
            },
        }

        return @enumFromInt(@intFromEnum(i));
    }

    pub fn ty(i: Index, ip: *const InternPool) Type {
        switch (i) {
            .type_none,
            .type_i32,
            .type_f32,
            .type_static_int,
            .type_static_str,
            .type_type,
            => return .type,
            .none => return .none,
            else => |index| {
                const tag = ip.values.items(.tag)[@intFromEnum(index)];
                return switch (tag) {
                    .int_positive, .int_negative => .static_int,
                    .@"fn" => .@"fn",
                };
            },
        }
    }

    pub fn unwrapInt(i: Index, T: type, ip: *const InternPool) T {
        switch (T) {
            i32 => {
                assert(ip.values.items(.tag)[@intFromEnum(i)] == .i32);

                const data = ip.values.items(.data)[@intFromEnum(i)];
                return @bitCast(data);
            },
            else => unreachable,
        }
    }

    pub fn unwrapStaticInt(i: Index, ip: *const InternPool) math.big.int.Const {
        const tag = ip.values.items(.tag)[@intFromEnum(i)];
        const positive = switch (tag) {
            .int_positive => true,
            .int_negative => false,
            else => unreachable,
        };

        const limbs_index = ip.values.items(.data)[@intFromEnum(i)];

        const start = limbs_index + 1;
        const end = start + ip.numerics.items[limbs_index];
        const limbs = ip.numerics.items[start..end];

        return .{
            .positive = positive,
            .limbs = limbs,
        };
    }

    pub fn unwrapFn(i: Index, ip: *const InternPool) Ast.Node.Index {
        assert(ip.values.items(.tag)[@intFromEnum(i)] == .@"fn");
        return ip.values.items(.data)[@intFromEnum(i)];
    }
};

pub const Type = enum(u32) {
    i32 = @intFromEnum(Index.type_i32),
    f32 = @intFromEnum(Index.type_f32),

    none = @intFromEnum(Index.type_none),
    static_int = @intFromEnum(Index.type_static_int),
    static_str = @intFromEnum(Index.type_static_str),
    type = @intFromEnum(Index.type_type),
    @"fn" = @intFromEnum(Index.type_fn),
    _,
};
