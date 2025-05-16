const Span = @This();

start: u32,
end: u32,

pub const Loc = struct {
    line: u32,
    char: u32,
};

pub fn loc(self: Span, code: [:0]const u8) Loc {
    var line: u32 = 1;
    var character: u32 = 1;

    for (0..self.start) |i| {
        character += 1;
        if (code[i] == '\n') {
            character = 1;
            line += 1;
        }
    }

    return .{ .line = line, .char = character };
}

pub fn slice(self: Span, src: [:0]const u8) []const u8 {
    return src[self.start..self.end];
}
