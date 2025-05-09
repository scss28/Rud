const std = @import("std");
const posix = std.posix;
const mem = std.mem;
const fmt = std.fmt;
const heap = std.heap;
const io = std.io;
const builtin = @import("builtin");

const Ast = @import("Ast.zig");

const quit_keywords: std.StaticStringMap(void) = .initComptime(.{
    .{ "quit", {} },
    .{ "q", {} },
    .{ "exit", {} },
});

pub fn main() !void {
    const gpa = heap.smp_allocator;

    var env: Env = .{
        .gpa = gpa,
        .vars = .{},
        .last_node = undefined,
    };

    // Put terminal into funny mode.
    // {
    //     const in = io.getStdIn();

    //     var termios = try posix.tcgetattr(in.handle);
    //     termios.lflag.ICANON = false;
    //     termios.lflag.ECHO = false;

    //     try posix.tcsetattr(in.handle, .NOW, termios);
    // }

    // var input: std.ArrayListUnmanaged(u8) = .{};
    // var history: std.ArrayListUnmanaged(struct {
    //     start: u32,
    //     end: u32,
    // }) = .{};

    const in = io.getStdIn().reader();
    const out = io.getStdOut().writer();
    while (true) {
        try out.writeAll("> ");

        // const src = blk: {
        //     var buf: [1024]u8 = undefined;
        //     var i: u32 = 0;

        //     var input_ptr: u32 = @intCast(history.items.len);
        //     while (i < buf) {
        //         const byte = try in.readByte();
        //         switch (byte) {
        //             // backspace
        //             '\x08', '\x7f' => {
        //                 if (i == 0) continue;
        //                 try out.writeAll("\x1b[1D \x1b[1D");

        //                 i -= 1;
        //             },
        //             '\x1b' => {
        //                 _ = try in.readByte(); // [

        //                 const next_byte = try in.readByte();
        //                 switch (next_byte) {
        //                     // up
        //                     'A', 'B' => {
        //                         switch (next_byte) {
        //                             'A' => {
        //                                         input_ptr -|=
        //                             },
        //                             'B' => {
        //                                 const max = history.items.len -| 1;
        //                                 if (history_index <= max) continue;

        //                                 history_index += 1;
        //                             },
        //                             else => unreachable,
        //                         }

        //                         const len = input.items.len - start;
        //                         for (0..len) |_| {
        //                             try out.writeAll("\x1b[1D");
        //                         }
        //                         try out.writeAll(" \x1b[1D");

        //                         input.items.len = start;
        //                         const entry = history.items[history_index];
        //                         const slice = input.items[entry.start..entry.end];
        //                         try input.appendSlice(gpa, slice);

        //                         try out.writeAll(slice);
        //                     },
        //                     else => {},
        //                 }
        //             },
        //             else => {
        //                 try out.writeByte(byte);
        //                 if (byte == '\n') break;

        //                 buf[i] = byte;
        //                 i += 1;
        //             },
        //         }
        //     }

        //     try input.append(gpa, 0);
        //     try history.append(gpa, .{
        //         .start = start,
        //         .end = @intCast(input.items.len - 1),
        //     });
        //     break :blk input.items[start .. input.items.len - 1 :0];
        // };

        var bytes: std.ArrayListUnmanaged(u8) = .{};
        defer bytes.deinit(gpa);

        try in.streamUntilDelimiter(bytes.writer(gpa), '\n', null);
        try bytes.append(gpa, 0);

        const src = bytes.items[0 .. bytes.items.len - 1 :0];
        if (src.len == 0) continue;
        if (quit_keywords.has(src)) break;

        var ast: Ast = try .parse(gpa, src);
        defer ast.deinit(gpa);

        defer if (builtin.mode == .Debug) {
            for (ast.tokens.items(.tag), 0..) |tag, i| {
                out.print("{d}: {s}\n", .{
                    i,
                    @tagName(tag),
                }) catch unreachable;
            }

            out.writeByte('\n') catch unreachable;
        };

        if (ast.err) |err| {
            const start = ast.tokens.items(.span)[err.token].start;
            try writeError(out, err.message, start);

            continue;
        }

        const number = env.eval(ast, ast.root) catch |err| {
            const message = switch (err) {
                error.UndefinedVariable => "undefined variable",
            };

            const token = ast.nodes.items(.token)[env.last_node];
            try writeError(out, message, ast.tokens.items(.span)[token].start);

            continue;
        };
        try out.print("= {d}\n\n", .{number});
    }
}

fn writeError(writer: anytype, message: []const u8, start: u32) !void {
    try writer.writeAll("  ");
    for (0..start) |_| {
        try writer.writeByte(' ');
    }
    try writer.writeAll("^ ");
    try writer.print("error: {s}\n\n", .{message});
}

const Env = struct {
    gpa: mem.Allocator,
    vars: std.StringArrayHashMapUnmanaged(f32),

    last_node: Ast.Node.Index,

    const EvalError = error{UndefinedVariable};

    fn eval(self: *Env, ast: Ast, node: Ast.Node.Index) EvalError!f32 {
        self.last_node = node;
        switch (ast.full(node)) {
            .identifier => |slice| {
                return self.vars.get(slice) orelse error.UndefinedVariable;
            },
            .number => |slice| {
                const number = fmt.parseFloat(f32, slice) catch unreachable;
                return number;
            },
            inline .add, .sub, .mul, .div => |binop, tag| {
                const lhs = try self.eval(ast, binop.lhs);
                const rhs = try self.eval(ast, binop.rhs);

                return switch (tag) {
                    .add => lhs + rhs,
                    .sub => lhs - rhs,
                    .mul => lhs * rhs,
                    .div => lhs / rhs,
                    else => unreachable,
                };
            },
        }
    }
};
