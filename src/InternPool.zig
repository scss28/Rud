const std = @import("std");
const InternPool = @This();

pub const Index = enum(u32) {
    i32,
    f32,
    _,
};

pub const Key = union(enum) {
    type_i32,
    type_f32,
    int: struct {
        ty: Index,
        value: i32,
    },
    float: struct {
        ty: Index,
        value: f32,
    },
};

pub fn get(self: *InternPool, key: Key) Index {
    switch (key) {
        .type_i32 => .i32,
        .type_f32 => .f32,
    }
}
