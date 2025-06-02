const std = @import("std");
const mem = std.mem;
const math = std.math;
const meta = std.meta;
const heap = std.heap;

const assert = std.debug.assert;

const InternPool = @This();

gpa: mem.Allocator,

values: std.MultiArrayList(Value),
numerics: std.ArrayListUnmanaged(usize),

pub fn init(gpa: mem.Allocator) mem.Allocator.Error!InternPool {
    var values: std.MultiArrayList(Value) = .empty;
    try values.ensureUnusedCapacity(
        gpa,
        @typeInfo(Index).@"enum".fields.len,
    );
    values.len = values.capacity;

    return .{
        .gpa = gpa,
        .values = values,
        .numerics = .empty,
    };
}

pub fn deinit(ip: *InternPool) void {
    ip.values.deinit(ip.gpa);
    ip.numerics.deinit(ip.gpa);

    ip.* = undefined;
}

pub fn addInt(ip: *InternPool, int: anytype) mem.Allocator.Error!Index {
    const T = @TypeOf(int);
    switch (T) {
        i32 => {
            return ip.addValue(.{
                .tag = .i32,
                .data = @bitCast(int),
            });
        },
        else => unreachable,
    }
}

pub fn getInt(ip: *InternPool, index: Index, T: type) T {
    switch (T) {
        i32 => {
            assert(ip.values.items(.tag) == .i32);

            const data = ip.values.items(.data)[@intFromEnum(index)];
            return @bitCast(data);
        },
        else => unreachable,
    }
}

pub fn addBigInt(ip: *InternPool, int: math.big.int.Const) mem.Allocator.Error!Index {
    const index: u32 = @intCast(ip.numerics.items.len);
    try ip.numerics.ensureUnusedCapacity(ip.gpa, 1 + int.limbs.len);

    ip.numerics.appendAssumeCapacity(int.limbs.len);
    ip.numerics.appendSliceAssumeCapacity(int.limbs);

    return ip.addValue(.{
        .tag = if (int.positive) .int_positive else .int_negative,
        .data = index,
    });
}

fn addValue(ip: *InternPool, value: Value) mem.Allocator.Error!Index {
    const index = ip.values.len;
    try ip.values.append(ip.gpa, value);

    return @enumFromInt(index);
}

pub const Value = struct {
    tag: Tag,
    data: u32,

    pub const Tag = enum {
        i32,

        /// `data` is index into `numerics`
        int_positive,
        /// `data` is index into `numerics`
        int_negative,
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
                    .i32 => .i32,
                    .int_positive, .int_negative => .static_int,
                };
            },
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
};

pub const Type = enum(u32) {
    i32 = @intFromEnum(Index.type_i32),
    f32 = @intFromEnum(Index.type_f32),

    none = @intFromEnum(Index.type_none),
    static_int = @intFromEnum(Index.type_static_int),
    static_str = @intFromEnum(Index.type_static_str),
    type = @intFromEnum(Index.type_type),
    _,
};
