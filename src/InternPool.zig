const std = @import("std");
const mem = std.mem;
const math = std.math;
const meta = std.meta;
const heap = std.heap;

const assert = std.debug.assert;

const Ast = @import("Ast.zig");

pub const Real = math.big.Rational;
pub const BigInt = math.big.int.Const;

const InternPool = @This();

values: std.MultiArrayList(ComptimeValue),
numerics: std.ArrayListUnmanaged(usize),
string_bytes: std.ArrayListUnmanaged(u8),

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
        .string_bytes = .empty,
    };
}

pub fn deinit(ip: *InternPool, gpa: mem.Allocator) void {
    ip.values.deinit(gpa);
    ip.numerics.deinit(gpa);
    ip.string_bytes.deinit(gpa);

    ip.* = undefined;
}

pub fn addBigInt(
    ip: *InternPool,
    gpa: mem.Allocator,
    int: BigInt,
) mem.Allocator.Error!Index {
    const index = try ip.addIntLimbs(gpa, int);
    return ip.addValue(gpa, .{
        .tag = if (int.positive) .int_positive else .int_negative,
        .data = index,
    });
}

fn addIntLimbs(
    ip: *InternPool,
    gpa: mem.Allocator,
    int: BigInt,
) mem.Allocator.Error!u32 {
    const index: u32 = @intCast(ip.numerics.items.len);
    try ip.numerics.ensureUnusedCapacity(gpa, 1 + int.limbs.len);

    ip.numerics.appendAssumeCapacity(int.limbs.len);
    ip.numerics.appendSliceAssumeCapacity(int.limbs);

    return index;
}

pub fn addReal(
    ip: *InternPool,
    gpa: mem.Allocator,
    real: Real,
) mem.Allocator.Error!Index {
    const index: u32 = @intCast(ip.numerics.items.len);

    const p = real.p.toConst();
    const q = real.q.toConst();
    try ip.numerics.ensureUnusedCapacity(gpa, 1 + p.limbs.len + q.limbs.len);

    const metadata = ip.numerics.addOneAssumeCapacity();
    @as(*[2]bool, @alignCast(@ptrCast(metadata))).* = .{ p.positive, q.positive };

    _ = try ip.addIntLimbs(gpa, real.p.toConst());
    _ = try ip.addIntLimbs(gpa, real.q.toConst());

    return ip.addValue(gpa, .{
        .tag = .real,
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

pub fn addStr(
    ip: *InternPool,
    gpa: mem.Allocator,
    str: []const u8,
) mem.Allocator.Error!Index {
    const string_index: u32 = @intCast(ip.string_bytes.items.len);

    try ip.string_bytes.ensureUnusedCapacity(gpa, 1 + str.len);
    ip.string_bytes.appendSliceAssumeCapacity(str);
    ip.string_bytes.appendAssumeCapacity(0);

    return ip.addValue(gpa, .{
        .tag = .str,
        .data = string_index,
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
        str,
        real,
    };
};

pub const Index = enum(u32) {
    type_i32,
    type_f32,

    none,
    type_none,
    type_int,
    type_real,
    type_str,
    type_type,
    type_fn,
    _,

    pub fn toType(i: Index, ip: *const InternPool) ?Type {
        switch (i) {
            .type_i32,
            .type_f32,
            .type_int,
            .type_str,
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
            .type_int,
            .type_str,
            .type_type,
            => return .type,
            .none => return .none,
            else => |index| {
                const tag = ip.values.items(.tag)[@intFromEnum(index)];
                return switch (tag) {
                    .int_positive, .int_negative => .int,
                    .real => .real,
                    .@"fn" => .@"fn",
                    .str => .str,
                };
            },
        }
    }

    pub fn unwrapInteger(i: Index, T: type, ip: *const InternPool) T {
        switch (T) {
            i32 => {
                assert(ip.values.items(.tag)[@intFromEnum(i)] == .i32);

                const data = ip.values.items(.data)[@intFromEnum(i)];
                return @bitCast(data);
            },
            else => unreachable,
        }
    }

    pub fn unwrapBigInt(i: Index, ip: *const InternPool) BigInt {
        const tag = ip.values.items(.tag)[@intFromEnum(i)];
        const positive = switch (tag) {
            .int_positive => true,
            .int_negative => false,
            else => unreachable,
        };

        const limbs_index = ip.values.items(.data)[@intFromEnum(i)];
        const limbs = bigIntLimbs(ip, limbs_index);

        return .{
            .positive = positive,
            .limbs = limbs,
        };
    }

    fn bigIntLimbs(ip: *const InternPool, index: u32) []const math.big.Limb {
        const start = index + 1;
        const end = start + ip.numerics.items[index];
        return ip.numerics.items[start..end];
    }

    pub fn unwrapFn(i: Index, ip: *const InternPool) Ast.Node.Index {
        assert(ip.values.items(.tag)[@intFromEnum(i)] == .@"fn");
        return ip.values.items(.data)[@intFromEnum(i)];
    }

    pub fn unwrapStr(i: Index, ip: *const InternPool) [:0]const u8 {
        assert(ip.values.items(.tag)[@intFromEnum(i)] == .str);
        const index = ip.value.items(.data)[@intFromEnum(i)];
        return mem.sliceTo(ip.string_bytes.items[index..], 0);
    }

    pub fn unwrapRealDupe(
        i: Index,
        gpa: mem.Allocator,
        ip: *const InternPool,
    ) mem.Allocator.Error!Real {
        assert(ip.values.items(.tag)[@intFromEnum(i)] == .real);
        const index = ip.values.items(.data)[@intFromEnum(i)];

        const metadata: *const [2]bool =
            @alignCast(@ptrCast(&ip.numerics.items[index..]));
        const p_len = ip.numerics.items[index + 1];
        const p_limbs = bigIntLimbs(ip, index + 1);
        const q_limbs = bigIntLimbs(ip, index + 2 + @as(u32, @intCast(p_len)));

        const p: BigInt = .{
            .limbs = p_limbs,
            .positive = metadata[0],
        };

        const q: BigInt = .{
            .limbs = q_limbs,
            .positive = metadata[1],
        };

        return .{
            .p = try p.toManaged(gpa),
            .q = try q.toManaged(gpa),
        };
    }
};

pub const Type = enum(u32) {
    i32 = @intFromEnum(Index.type_i32),
    f32 = @intFromEnum(Index.type_f32),

    none = @intFromEnum(Index.type_none),
    int = @intFromEnum(Index.type_int),
    real = @intFromEnum(Index.type_real),
    str = @intFromEnum(Index.type_str),
    type = @intFromEnum(Index.type_type),
    @"fn" = @intFromEnum(Index.type_fn),
    _,
};
