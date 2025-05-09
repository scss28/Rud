const Span = @This();

start: u32,
end: u32,

pub fn slice(self: Span, src: [:0]const u8) []const u8 {
    return src[self.start..self.end];
}
