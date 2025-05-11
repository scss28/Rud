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
extra_data: std.ArrayListUnmanaged(Node.Index),

scratch: std.ArrayListUnmanaged(Node.Index),

errors: std.ArrayListUnmanaged(Ast.Error),

pub fn parse(gpa: mem.Allocator, src: [:0]const u8) mem.Allocator.Error!Ast {
    var errors: std.ArrayListUnmanaged(Ast.Error) = .{};
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
                try errors.append(gpa, .{
                    .message = "invalid token",
                    .token = @intCast(tokens.len - 1),
                });
            }

            if (token.tag == .eof) break;
        }

        break :blk tokens.toOwnedSlice();
    };

    if (errors.items.len > 0) {
        return .{
            .src = src,
            .tokens = tokens,
            .nodes = .{ .ptrs = undefined, .len = 0, .capacity = 0 },
            .extra_data = &.{},
            .errors = try errors.toOwnedSlice(gpa),
        };
    }

    var p: Parser = .{
        .gpa = gpa,
        .src = src,
        .index = 0,
        .tokens = tokens,
        .nodes = .{},
        .extra_data = .{},
        .scratch = .{},
        .errors = errors,
    };
    defer p.scratch.deinit(gpa);

    const root = try p.addNode(undefined);
    outer: while (p.currentTokenTag() != .eof) {
        while (p.eatToken(.newline)) |_| {}

        const node = p.nextNode() catch |err| switch (err) {
            error.OutOfMemory => |oom| return oom,
            error.Syntax => {
                while (true) {
                    const token = p.eatTokenAny();
                    switch (p.tokens.items(.tag)[token]) {
                        .newline => break,
                        .eof => break :outer,
                        else => {},
                    }
                }

                continue;
            },
        };
        _ = try p.appendScratch(node);

        switch (p.currentTokenTag()) {
            .newline => {},
            .eof => break,
            else => {
                const err = p.throw("expected newline", p.eatTokenAny());
                if (err == error.OutOfMemory) return error.OutOfMemory;

                while (true) {
                    const token = p.eatTokenAny();
                    switch (p.tokens.items(.tag)[token]) {
                        .newline => break,
                        .eof => break :outer,
                        else => {},
                    }
                }
            },
        }
    }

    const start: u32 = @intCast(p.extra_data.items.len);
    const end = start + @as(u32, @intCast(p.scratch.items.len));
    p.nodes.items(.data)[root] = .{
        .lhs = start,
        .rhs = end,
    };
    try p.extra_data.appendSlice(gpa, p.scratch.items);

    return .{
        .src = src,
        .tokens = p.tokens,
        .nodes = p.nodes.toOwnedSlice(),
        .extra_data = try p.extra_data.toOwnedSlice(gpa),
        .errors = try p.errors.toOwnedSlice(gpa),
    };
}

const Prec = u8;
const OpInfo = struct { Prec, Node.Tag };
const op_map: std.EnumMap(Token.Tag, OpInfo) = .init(.{
    .plus = .{ 1, .add },
    .minus = .{ 1, .sub },

    .star = .{ 2, .mul },
    .slash = .{ 2, .div },
    .double_star = .{ 3, .pow },
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
    const node = try self.nextSingleLeaf();
    switch (self.currentTokenTag()) {
        .lparen => {
            _ = try self.assertToken(.lparen);
            const args = try self.parseCallArgs(.rparen);
            const token = try self.assertToken(.rparen);

            return self.addNode(.{
                .tag = .call,
                .token = token,
                .data = .{
                    .lhs = node,
                    .rhs = args,
                },
            });
        },
        else => return node,
    }
}

fn nextSingleLeaf(self: *Parser) (mem.Allocator.Error || error{Syntax})!Node.Index {
    return switch (self.currentTokenTag()) {
        .identifier => {
            const identifier_token = self.eatTokenAny();
            const token = self.eatToken(.colon_eq) orelse return self.addNode(.{
                .tag = .identifier,
                .token = identifier_token,
                .data = undefined,
            });

            const rhs = try self.nextNode();
            return self.addNode(.{
                .tag = .assign,
                .token = token,
                .data = .{
                    .lhs = identifier_token,
                    .rhs = rhs,
                },
            });
        },
        .number => try self.addNode(.{
            .tag = .number,
            .token = self.eatTokenAny(),
            .data = undefined,
        }),
        .lparen => {
            _ = self.eatTokenAny();

            const node = try self.nextNode();
            _ = try self.assertToken(.rparen);

            return node;
        },
        else => self.throw("expected an expression", self.index),
    };
}

/// Parse arguments serparated by commas with a specified delimiter.
///
/// Returns an index to `extra_data` at which is the len of args followed by
/// the node indices of the args themselves.
fn parseCallArgs(self: *Parser, delimiter: Token.Tag) !Ast.ExtraDataIndex {
    const start = try self.appendScratch(undefined);
    while (true) {
        if (self.currentTokenTag() == delimiter) break;

        const node = try self.nextNode();
        _ = try self.appendScratch(node);

        if (self.eatToken(.comma) == null) break;
    }

    const args = self.scratch.items[start..];
    args[0] = @intCast(self.scratch.items.len - start - 1);

    const index: Ast.ExtraDataIndex = @intCast(self.extra_data.items.len);
    try self.extra_data.appendSlice(self.gpa, args);
    self.scratch.items.len = start;

    return index;
}

fn appendScratch(self: *Parser, node: Node.Index) mem.Allocator.Error!u32 {
    const index = self.scratch.items.len;
    try self.scratch.append(self.gpa, node);

    return @intCast(index);
}

fn assertToken(
    self: *Parser,
    comptime tag: Token.Tag,
) (error{Syntax} || mem.Allocator.Error)!Ast.TokenIndex {
    const token = self.eatToken(tag) orelse {
        return self.throw(
            fmt.comptimePrint("expected {s}", .{@tagName(tag)}),
            self.index,
        );
    };

    return token;
}

inline fn eatToken(self: *Parser, comptime tag: Token.Tag) ?Ast.TokenIndex {
    if (self.currentTokenTag() != tag) return null;
    return self.eatTokenAny();
}

inline fn eatTokenAny(self: *Parser) Ast.TokenIndex {
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
) (error{Syntax} || mem.Allocator.Error) {
    try self.errors.append(self.gpa, .{
        .message = message,
        .token = token,
    });

    return error.Syntax;
}
