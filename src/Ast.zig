const std = @import("std");
const mem = std.mem;

const Span = @import("Span.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

const Parser = @import("Parser.zig");
const Ast = @This();

root: Node.Index,

src: [:0]const u8,
tokens: Tokens.Slice,
nodes: Nodes.Slice,
extra_data: []const u32,

err: ?Error,

pub const Error = struct {
    message: []const u8,
    token: TokenIndex,
};

pub const Nodes = std.MultiArrayList(Node);
pub const Tokens = std.MultiArrayList(Token);
pub const TokenIndex = u32;
pub const ExtraDataIndex = u32;

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
        pow,
        div,
        assign,
        call,
    };

    pub const Data = struct {
        lhs: u32,
        rhs: u32,
    };

    pub const Index = u32;

    pub const Full = union(Tag) {
        pub const Slice = []const u8;
        pub const Binop = struct {
            lhs: Node.Index,
            rhs: Node.Index,
        };
        pub const Assign = struct {
            identifier: Slice,
            rhs: Node.Index,
        };
        pub const Call = struct {
            callee: Node.Index,
            args: []const Node.Index,
        };

        identifier: Slice,
        number: Slice,
        add: Binop,
        sub: Binop,
        mul: Binop,
        pow: Binop,
        div: Binop,
        assign: Assign,
        call: Call,
    };
};

pub inline fn parse(
    gpa: mem.Allocator,
    src: [:0]const u8,
) mem.Allocator.Error!Ast {
    return Parser.parse(gpa, src);
}

pub inline fn nodeTag(self: *const Ast, node: Node.Index) Node.Tag {
    return self.nodes.items(.tag)[node];
}

pub inline fn tokenSpan(self: *const Ast, token: TokenIndex) Span {
    return self.tokens.items(.span)[token];
}

pub inline fn tokenSlice(self: *const Ast, token: TokenIndex) []const u8 {
    return self.tokenSpan(token).slice(self.src);
}

pub inline fn nodeToken(self: *const Ast, node: Node.Index) TokenIndex {
    return self.nodes.items(.token)[node];
}

pub inline fn nodeTokenSpan(self: *const Ast, node: Node.Index) Span {
    return self.tokenSpan(self.nodeToken(node));
}

pub inline fn nodeTokenSlice(self: *const Ast, node: Node.Index) []const u8 {
    return self.tokenSlice(self.nodeToken(node));
}

pub inline fn extraSlice(
    self: *const Ast,
    index: ExtraDataIndex,
) []const Node.Index {
    const len = self.extra_data[index];
    return self.extra_data[index + 1 .. index + 1 + len];
}

pub fn full(self: *const Ast, node: Node.Index) Node.Full {
    switch (self.nodes.items(.tag)[node]) {
        inline .identifier, .number => |tag| {
            const token = self.nodes.items(.token)[node];
            const slice = self.tokenSlice(token);

            return @unionInit(Node.Full, @tagName(tag), slice);
        },
        inline .add, .sub, .mul, .pow, .div => |tag| {
            const data = self.nodes.items(.data)[node];
            return @unionInit(Node.Full, @tagName(tag), .{
                .lhs = data.lhs,
                .rhs = data.rhs,
            });
        },
        .assign => {
            const data = self.nodes.items(.data)[node];
            const identifier = self.tokenSlice(data.lhs);

            return .{ .assign = .{
                .identifier = identifier,
                .rhs = data.rhs,
            } };
        },
        .call => {
            const data = self.nodes.items(.data)[node];
            const args = self.extraSlice(data.rhs);

            return .{ .call = .{
                .callee = data.lhs,
                .args = args,
            } };
        },
    }
}

pub fn deinit(self: *Ast, gpa: mem.Allocator) void {
    self.nodes.deinit(gpa);
    self.tokens.deinit(gpa);

    self.* = undefined;
}
