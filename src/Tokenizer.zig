const Span = @import("Span.zig");
const Tokenizer = @This();

index: u32,
src: [:0]const u8,

pub const Token = struct {
    tag: Tag,
    span: Span,

    pub const Tag = enum {
        identifier,
        number,

        plus,
        minus,
        star,
        slash,

        lparen,
        rparen,

        invalid,
        eof,
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
                else => break :tag .identifier,
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

            break :tag .number;
        },
        '+' => .plus,
        '-' => .minus,
        '*' => .star,
        '/' => .slash,
        '(' => .lparen,
        ')' => .rparen,
        ' ', '\t', '\n' => {
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
