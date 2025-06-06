const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const heap = std.heap;

const Ast = @import("Ast.zig");

arena_state: heap.ArenaAllocator,
files: std.ArrayListUnmanaged(File),

root: File.Index,

errors: std.ArrayListUnmanaged(Error),

pub const Error = struct {
    message: RAst.StringIndex,
    file: File.Index,
    src: Src,

    pub const Src = union(enum) {
        node: Ast.Node.Index,
        token: Ast.TokenIndex,
    };
};

const CompileResult = struct {
    errors: []const Error,
};
