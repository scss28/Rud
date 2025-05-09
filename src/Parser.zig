const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

const Ast = @import("Ast.zig");
const Node = Ast.Node;

const Parser = @This();

gpa: mem.Allocator,

src: [:0]const u8,
index: u32,
tokens: Ast.Tokens.Slice,
nodes: Ast.Nodes,

err: ?Ast.Error,

pub fn parse(gpa: mem.Allocator, src: [:0]const u8) mem.Allocator.Error!Ast {
    const tokens = blk: {
        var tokens: std.MultiArrayList(Token) = .{};
        var tokenizer: Tokenizer = .{
            .index = 0,
            .src = src,
        };

        while (true) {
            const token = tokenizer.next();
            try tokens.append(gpa, token);

            if (token.tag == .invalid) {
                const token_index: u32 = @intCast(tokens.len - 1);
                return .{
                    .root = undefined,
                    .src = src,
                    .tokens = tokens.toOwnedSlice(),
                    .nodes = .{ .ptrs = undefined, .len = 0, .capacity = 0 },
                    .err = .{
                        .message = "invalid token",
                        .token = token_index,
                    },
                };
            }

            if (token.tag == .eof) break;
        }

        break :blk tokens.toOwnedSlice();
    };

    var p: Parser = .{
        .gpa = gpa,
        .src = src,
        .index = 0,
        .tokens = tokens,
        .nodes = .{},
        .err = null,
    };

    const root = p.nextNode() catch |err| switch (err) {
        error.OutOfMemory => |oom| return oom,
        error.Syntax => undefined,
    };

    if (p.err == null and p.currentTokenTag() != .eof) {
        p.throw("expected end of expression", p.index) catch {};
    }

    return .{
        .root = root,
        .src = src,
        .tokens = p.tokens,
        .nodes = p.nodes.toOwnedSlice(),
        .err = p.err,
    };
}

const Prec = u8;
const OpInfo = struct { Prec, Node.Tag };
const op_map: std.EnumMap(Token.Tag, OpInfo) = .init(.{
    .plus = .{ 1, .add },
    .minus = .{ 1, .sub },

    .star = .{ 2, .mul },
    .slash = .{ 2, .div },
});

fn nextNode(self: *Parser) (mem.Allocator.Error || error{Syntax})!Node.Index {
    var node = try self.nextLeaf();

    var last_prec: Prec = 255;
    while (true) {
        const token_tag = self.currentTokenTag();

        const prec, const node_tag = op_map.get(token_tag) orelse break;
        defer last_prec = prec;

        const token = self.eatTokenAny();

        const next_node = try self.nextLeaf();
        if (prec > last_prec) {
            const data = self.nodes.items(.data)[node];
            const rhs = try self.addNode(.{
                .tag = node_tag,
                .token = token,
                .data = .{
                    .lhs = data.rhs,
                    .rhs = next_node,
                },
            });

            self.nodes.items(.data)[node] = .{
                .lhs = data.lhs,
                .rhs = rhs,
            };
        } else {
            node = try self.addNode(.{
                .tag = node_tag,
                .token = token,
                .data = .{
                    .lhs = node,
                    .rhs = next_node,
                },
            });
        }
    }

    return node;
}

fn nextLeaf(self: *Parser) (mem.Allocator.Error || error{Syntax})!Node.Index {
    return switch (self.currentTokenTag()) {
        .identifier => try self.addNode(.{
            .tag = .identifier,
            .token = self.eatTokenAny(),
            .data = undefined,
        }),
        .number => try self.addNode(.{
            .tag = .number,
            .token = self.eatTokenAny(),
            .data = undefined,
        }),
        .lparen => {
            _ = self.eatTokenAny();

            const node = try self.nextNode();
            _ = try self.eatToken(.rparen);

            return node;
        },
        else => self.throw("unexpected token", self.index),
    };
}

fn eatToken(self: *Parser, comptime tag: Token.Tag) error{Syntax}!Ast.TokenIndex {
    defer self.index += 1;
    if (self.currentTokenTag() != tag) {
        return self.throw(
            fmt.comptimePrint("expected {s}", .{@tagName(tag)}),
            self.index,
        );
    }
    return self.index;
}

fn eatTokenAny(self: *Parser) Ast.TokenIndex {
    defer self.index += 1;
    return self.index;
}

fn currentTokenTag(self: *Parser) Token.Tag {
    return self.tokens.items(.tag)[self.index];
}

fn addNode(self: *Parser, node: Node) mem.Allocator.Error!Node.Index {
    const index: u32 = try self.reserveNode();
    self.nodes.set(index, node);

    return index;
}

fn reserveNode(self: *Parser) mem.Allocator.Error!Node.Index {
    return @intCast(try self.nodes.addOne(self.gpa));
}

fn throw(
    self: *Parser,
    message: []const u8,
    token: Ast.TokenIndex,
) error{Syntax} {
    self.err = .{
        .message = message,
        .token = token,
    };

    return error.Syntax;
}
