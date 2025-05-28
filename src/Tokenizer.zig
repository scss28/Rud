const std = @import("std");
const Span = @import("Span.zig");
const Tokenizer = @This();

index: u32,
src: [:0]const u8,

pub const Token = struct {
    tag: Tag,
    span: Span,

    pub const Tag = enum {
        identifier,
        literal_int,
        literal_float,
        literal_str,

        plus,
        minus,
        star,
        double_star,
        slash,

        eq,
        colon_eq,
        at,
        comma,

        lparen,
        rparen,
        lbrace,
        rbrace,

        keyword_fn,
        keyword_ret,

        newline,
        invalid,
        eof,

        const keyword_map: std.StaticStringMap(Tag) = .initComptime(.{
            .{ "fn", .keyword_fn },
            .{ "ret", .keyword_ret },
        });
    };
};

pub fn next(self: *Tokenizer) Token {
    var start = self.index;
    const tag: Token.Tag = tag: switch (blk: {
        defer self.index += 1;
        break :blk self.src[self.index];
    }) {
        'a'...'z', 'A'...'Z', '_' => {
            identifier: switch (self.src[self.index]) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => {
                    self.index += 1;
                    continue :identifier self.src[self.index];
                },
                else => {
                    if (Token.Tag.keyword_map.get(self.src[start..self.index])) |tag| {
                        break :tag tag;
                    }

                    break :tag .identifier;
                },
            }
        },
        '0'...'9' => {
            var dot_parsed = false;
            number: switch (self.src[self.index]) {
                '0'...'9' => {
                    self.index += 1;
                    continue :number self.src[self.index];
                },
                '.' => if (!dot_parsed) {
                    switch (self.src[self.index + 1]) {
                        '0'...'9' => {
                            dot_parsed = true;
                            self.index += 2;

                            continue :number self.src[self.index];
                        },
                        else => {},
                    }
                },
                else => {},
            }

            break :tag if (dot_parsed) .literal_float else .literal_int;
        },
        '"' => {
            while (true) switch (blk: {
                defer self.index += 1;
                break :blk self.src[self.index];
            }) {
                '\\' => {
                    if (self.src[self.index] == '"') self.index += 1;
                },
                '"' => break,
                0 => break :tag .eof,
                else => {},
            };

            break :tag .literal_str;
        },
        '+' => .plus,
        '-' => .minus,
        '*' => {
            if (self.src[self.index] == '*') {
                self.index += 1;
                break :tag .double_star;
            }

            break :tag .star;
        },
        '/' => .slash,
        '(' => .lparen,
        ')' => .rparen,
        '{' => .lbrace,
        '}' => .rbrace,
        '=' => .eq,
        '@' => .at,
        ',' => .comma,
        ':' => {
            if (self.src[self.index] == '=') {
                self.index += 1;
                break :tag .colon_eq;
            }

            break :tag .invalid;
        },
        '\n' => .newline,
        ' ', '\t' => {
            start = self.index;

            defer self.index += 1;
            continue :tag self.src[self.index];
        },
        '#' => {
            while (true) : (self.index += 1) switch (self.src[self.index]) {
                '\n', 0 => break,
                else => {},
            };

            start = self.index;

            defer self.index += 1;
            continue :tag self.src[self.index];
        },
        0 => .eof,
        else => .invalid,
    };

    return .{
        .tag = tag,
        .span = .{
            .start = start,
            .end = self.index,
        },
    };
}
