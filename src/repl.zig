const std = @import("std");
const debug = std.debug;
const posix = std.posix;
const mem = std.mem;
const heap = std.heap;
const io = std.io;
const builtin = @import("builtin");

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

    const in = io.getStdIn().reader();
    const out = io.getStdOut().writer();
    while (true) {
        try out.writeAll("> ");

        var bytes: std.ArrayListUnmanaged(u8) = .{};
        defer bytes.deinit(gpa);

        try in.streamUntilDelimiter(bytes.writer(gpa), '\n', null);
        try bytes.append(gpa, 0);

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

        if (ast.err) |err| {
            const start = ast.tokens.items(.span)[err.token].start;
            try writeError(out, err.message, start);

            continue;
        }

        var eval: Eval = .init(&state, &ast);
        defer eval.deinit();

        const value = eval.eval(ast.root) catch |err| switch (err) {
            error.OutOfMemory => |oom| return oom,
            error.Eval => {
                const start = ast.nodeTokenSpan(eval.err.node).start;
                try writeError(out, eval.err.message, start);

                continue;
            },
        };
        if (value != .nil) try out.print("= {}\n\n", .{value});
    }
}

fn writeError(writer: anytype, message: []const u8, start: u32) !void {
    try writer.writeAll("  ");
    for (0..start) |_| try writer.writeByte(' ');

    try writer.writeAll("^ ");
    try writer.print("error: {s}\n\n", .{message});
}
