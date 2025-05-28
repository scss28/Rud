const std = @import("std");
const mem = std.mem;
const meta = std.meta;

const Span = @import("Span.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

const Parser = @import("Parser.zig");
const Ast = @This();

pub const Error = struct {
    message: []const u8,
    token: TokenIndex,
};

pub const Nodes = std.MultiArrayList(Node);
pub const Tokens = std.MultiArrayList(Token);
pub const TokenIndex = u32;
pub const ExtraDataIndex = u32;

src: [:0]const u8,
tokens: Tokens.Slice,
nodes: Nodes.Slice,
extra_data: []const u32,

errors: []Error,

pub const Node = struct {
    tag: Tag,
    token: TokenIndex,
    data: Data,

    pub const Tag = enum {
        identifier,
        literal_int,
        literal_float,
        literal_str,
        add,
        sub,
        mul,
        pow,
        div,
        assign,
        call,
        builtin_call,
        block,
        fn_decl,
        ret_expr,
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
        pub const BuiltinCall = struct {
            identifier: Slice,
            args: []const Node.Index,
        };
        pub const Block = struct {
            nodes: []const Node.Index,
        };
        pub const FnDecl = struct {
            args: []const TokenIndex,
            body: Node.Index,
        };
        pub const RetExpr = Node.Index;

        identifier: Slice,
        literal_int: Slice,
        literal_float: Slice,
        literal_str: Slice,
        add: Binop,
        sub: Binop,
        mul: Binop,
        pow: Binop,
        div: Binop,
        assign: Assign,
        call: Call,
        builtin_call: BuiltinCall,
        block: Block,
        fn_decl: FnDecl,
        ret_expr: RetExpr,
    };
};

pub fn deinit(self: *Ast, gpa: mem.Allocator) void {
    self.tokens.deinit(gpa);
    self.nodes.deinit(gpa);

    gpa.free(self.extra_data);
    gpa.free(self.errors);

    self.* = undefined;
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

pub inline fn nodeData(self: *const Ast, node: Node.Index) Node.Data {
    return self.nodes.items(.data)[node];
}

pub inline fn nodeTokenSpan(self: *const Ast, node: Node.Index) Span {
    return self.tokenSpan(self.nodeToken(node));
}

pub inline fn nodeTokenSlice(self: *const Ast, node: Node.Index) []const u8 {
    return self.tokenSlice(self.nodeToken(node));
}

pub fn nodeSpan(self: *const Ast, node: Node.Index) Span {
    switch (self.nodeTag(node)) {
        .identifier,
        .literal_int,
        .literal_float,
        .literal_str,
        => {
            return self.nodeTokenSpan(node);
        },
        .add, .sub, .mul, .pow, .div => {
            const data = self.nodeData(node);
            return .{
                .start = self.nodeSpan(data.lhs).start,
                .end = self.nodeSpan(data.rhs).end,
            };
        },
        .assign => {
            const data = self.nodeData(node);
            return .{
                .start = self.tokenSpan(data.lhs).start,
                .end = self.nodeSpan(data.rhs).end,
            };
        },
        .call => {
            const data = self.nodeData(node);
            return .{
                .start = self.nodeSpan(data.lhs).start,
                .end = self.nodeTokenSpan(node).end,
            };
        },
        .builtin_call => {
            const data = self.nodeData(node);
            return .{
                .start = self.tokenSpan(data.lhs).start,
                .end = self.nodeTokenSpan(node).end,
            };
        },
        .block, .fn_decl => {
            const start = self.nodeTokenSpan(node).start;

            const data = self.nodeData(node);
            const end = self.tokenSpan(data.rhs).end;
            return .{ .start = start, .end = end };
        },
        .ret_expr => {
            const data = self.nodeData(node);
            return .{
                .start = self.nodeTokenSpan(node).start,
                .end = self.nodeSpan(data.lhs).end,
            };
        },
    }
}

pub inline fn extraSlice(self: *const Ast, index: ExtraDataIndex) []const Node.Index {
    const len = self.extra_data[index];
    return self.extra_data[index + 1 .. index + 1 + len];
}

pub inline fn rootNodes(self: *const Ast) []const Node.Index {
    const data = self.nodeData(0);
    return self.extra_data[data.lhs..data.rhs];
}

pub fn full(self: *const Ast, node: Node.Index) Node.Full {
    switch (self.nodeTag(node)) {
        inline else => |tag| {
            const value: meta.TagPayload(Node.Full, tag) = payload: switch (tag) {
                .identifier,
                .literal_int,
                .literal_float,
                .literal_str,
                => self.nodeTokenSlice(node),
                .add, .sub, .mul, .pow, .div => {
                    const data = self.nodeData(node);
                    break :payload .{
                        .lhs = data.lhs,
                        .rhs = data.rhs,
                    };
                },
                .assign => {
                    const data = self.nodeData(node);
                    const identifier = self.tokenSlice(data.lhs);
                    break :payload .{
                        .identifier = identifier,
                        .rhs = data.rhs,
                    };
                },
                .call => {
                    const data = self.nodeData(node);
                    const args = self.extraSlice(data.rhs);
                    break :payload .{
                        .callee = data.lhs,
                        .args = args,
                    };
                },
                .builtin_call => {
                    const data = self.nodeData(node);
                    const args = self.extraSlice(data.rhs);

                    break :payload .{
                        .identifier = self.tokenSlice(data.lhs),
                        .args = args,
                    };
                },
                .block => {
                    const data = self.nodeData(node);
                    const nodes = self.extraSlice(data.lhs);
                    break :payload .{ .nodes = nodes };
                },
                .fn_decl => {
                    const data = self.nodeData(node);
                    const args = self.extraSlice(data.lhs);
                    break :payload .{
                        .args = args,
                        .body = data.rhs,
                    };
                },
                .ret_expr => self.nodeData(node).lhs,
            };

            return @unionInit(Node.Full, @tagName(tag), value);
        },
    }
}
