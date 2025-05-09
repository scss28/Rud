const std = @import("std");
const mem = std.mem;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

const Parser = @import("Parser.zig");
const Ast = @This();

root: Node.Index,

src: [:0]const u8,
tokens: Tokens.Slice,
nodes: Nodes.Slice,

err: ?Error,

pub const Error = struct {
    message: []const u8,
    token: TokenIndex,
};

pub const Nodes = std.MultiArrayList(Node);
pub const Tokens = std.MultiArrayList(Token);
pub const TokenIndex = u32;

pub const Node = struct {
    tag: Tag,
    token: TokenIndex,
    data: Data,

    pub const Tag = enum {
        identifier,
        number,
        add,
        sub,
        mul,
        div,
    };

    pub const Data = struct {
        lhs: u32,
        rhs: u32,
    };

    pub const Index = u32;

    pub const Full = union(Tag) {
        const Slice = []const u8;
        const Binop = struct {
            lhs: Node.Index,
            rhs: Node.Index,
        };

        identifier: Slice,
        number: Slice,
        add: Binop,
        sub: Binop,
        mul: Binop,
        div: Binop,
    };
};

pub fn parse(gpa: mem.Allocator, src: [:0]const u8) mem.Allocator.Error!Ast {
    return Parser.parse(gpa, src);
}

pub fn full(self: *const Ast, node: Node.Index) Node.Full {
    switch (self.nodes.items(.tag)[node]) {
        inline .identifier, .number => |tag| {
            const token = self.nodes.items(.token)[node];
            const slice = self.tokens.items(.span)[token].slice(self.src);
            return @unionInit(Node.Full, @tagName(tag), slice);
        },
        inline .add, .sub, .mul, .div => |tag| {
            const data = self.nodes.items(.data)[node];
            return @unionInit(Node.Full, @tagName(tag), .{
                .lhs = data.lhs,
                .rhs = data.rhs,
            });
        },
    }
}

pub fn deinit(self: *Ast, gpa: mem.Allocator) void {
    self.nodes.deinit(gpa);
    self.tokens.deinit(gpa);

    self.* = undefined;
}
