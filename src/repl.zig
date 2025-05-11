const std = @import("std");
const debug = std.debug;
const posix = std.posix;
const mem = std.mem;
const heap = std.heap;
const io = std.io;
const builtin = @import("builtin");

const Span = @import("Span.zig");
const Ast = @import("Ast.zig");
const State = @import("State.zig");
const Eval = @import("Eval.zig");

const quit_keywords: std.StaticStringMap(void) = .initComptime(.{
    .{ "quit", {} },
    .{ "q", {} },
    .{ "exit", {} },
});

pub fn run(gpa: mem.Allocator) !void {
    var state: State = .init(gpa);
    defer state.deinit();

    const in = io.getStdIn().reader();
    const out = io.getStdOut().writer();
    while (true) {
        try out.writeAll("> ");

        var bytes: std.ArrayListUnmanaged(u8) = .{};
        defer bytes.deinit(gpa);

        const delimiter = if (builtin.os.tag == .windows) '\r' else '\n';
        try in.streamUntilDelimiter(bytes.writer(gpa), delimiter, null);
        try bytes.append(gpa, 0);

        if (builtin.os.tag == .windows) _ = try in.readByte();

        const src = bytes.items[0 .. bytes.items.len - 1 :0];
        if (src.len == 0) continue;
        if (quit_keywords.has(src)) break;

        var ast: Ast = try .parse(gpa, src);
        defer ast.deinit(gpa);

        // DEBUG
        defer if (builtin.mode == .Debug) {
            for (ast.tokens.items(.tag), 0..) |tag, i| {
                out.print("{d}: {s}\n", .{ i, @tagName(tag) }) catch unreachable;
            }

            out.writeByte('\n') catch unreachable;
        };

        if (ast.errors.len > 0) {
            const err = ast.errors[0];
            try writeError(out, err.message, ast.tokenSpan(err.token));

            continue;
        }

        var eval: Eval = .init(&state, &ast);
        defer eval.deinit();

        const node = ast.rootNodes()[0];
        const value = eval.eval(node) catch |err| switch (err) {
            error.OutOfMemory => |oom| return oom,
            error.Eval => {
                const span = ast.nodeSpan(eval.err.node);
                try writeError(out, eval.err.message, span);

                continue;
            },
        };
        if (value != .nil) try out.print("= {}\n\n", .{value});
    }
}

fn writeError(writer: anytype, message: []const u8, span: Span) !void {
    try writer.writeAll("  ");
    for (0..span.start) |_| try writer.writeByte(' ');
    for (0..span.end - span.start) |_| try writer.writeByte('^');

    try writer.print(" error: {s}\n\n", .{message});
}
