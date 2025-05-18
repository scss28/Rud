const std = @import("std");
const io = std.io;
const fs = std.fs;
const math = std.math;
const mem = std.mem;
const meta = std.meta;
const process = std.process;
const debug = std.debug;
const heap = std.heap;
const log = std.log;
const builtin = @import("builtin");

const Parser = @import("Parser.zig");
const Emitter = @import("Emitter.zig");
const Ir = @import("Ir.zig");
const Vm = @import("Vm.zig");
const State = @import("State.zig");
const Eval = @import("Eval.zig");
const Span = @import("Span.zig");

const help =
    \\Usage: {s} [command]
    \\
    \\Commands:
    \\
    \\  repl (default)  Enter a repl
    \\  run [path]      Run code from a file
    \\  build [path]    Run code from a file
    \\  help            Print this help and exit
    \\
;

const Command = enum {
    repl,
    run,
    build,
    help,
};

pub fn main() !void {
    var gpa_state = if (debug.runtime_safety) heap.DebugAllocator(.{}).init else {};
    defer if (debug.runtime_safety) {
        _ = gpa_state.deinit();
    };

    const gpa = if (debug.runtime_safety) gpa_state.allocator() else heap.smp_allocator;

    var args = try process.argsWithAllocator(gpa);
    defer args.deinit();

    const program = args.next() orelse "miti";
    const first_arg = args.next() orelse {
        log.info(help, .{program});
        return;
    };

    const command = meta.stringToEnum(Command, first_arg) orelse {
        log.info(help, .{program});
        log.err("unknown command: {s}", .{first_arg});
        return;
    };

    switch (command) {
        .repl => {},
        .build => {
            const path = args.next() orelse {
                log.err("expected a path argument", .{});
                return;
            };

            var ir = try build(gpa, path);
            defer ir.deinit(gpa);

            const out_file = try fs.cwd().createFile("out.ir", .{});
            defer out_file.close();
        },
        .run => {
            const path = args.next() orelse {
                log.err("expected a path argument", .{});
                return;
            };

            var ir = try build(gpa, path);
            defer ir.deinit(gpa);

            var vm: Vm = try .init(gpa, &ir, .{});
            defer vm.deinit();

            while (vm.next()) {}
        },
        .help => {
            log.info(help, .{program});
        },
    }
}

fn build(gpa: mem.Allocator, path: []const u8) !Ir {
    const src = try fs.cwd().readFileAllocOptions(
        gpa,
        path,
        math.maxInt(u32),
        null,
        1,
        0,
    );
    defer gpa.free(src);

    var ast = try Parser.parse(gpa, src);
    defer ast.deinit(gpa);

    const out = io.getStdOut().writer();

    if (builtin.mode == .Debug) {
        try out.writeAll("----- TOKENS ------\n");
        for (ast.tokens.items(.tag), 0..) |token_tag, i| {
            try out.print("{d}. {s}\n", .{ i, @tagName(token_tag) });
        }
    }

    if (ast.errors.len > 0) {
        for (ast.errors) |err| {
            const span = ast.tokenSpan(err.token);
            try writeError(out, err.message, span, src);
        }

        process.exit(1);
    }

    if (builtin.mode == .Debug) {
        try out.writeAll("------ AST -------\n");
        for (ast.rootNodes(), 0..) |node, i| {
            try out.print("{d}. {s}\n", .{ i, @tagName(ast.nodeTag(node)) });
        }
    }

    const ir = try Emitter.emit(gpa, &ast);
    if (ir.errorSlice()) |errors| {
        for (errors) |err| {
            const span = ast.nodeSpan(err.node);
            const message = ir.string(err.message);
            try writeError(out, message, span, src);
        }

        process.exit(1);
    }

    if (builtin.mode == .Debug) {
        try out.writeAll("------ IR -------\n");
        for (0..ir.instrs.len) |i| {
            const instr = ir.instrs.get(i);
            try out.print("{d}. {s} {d}\n", .{ i, @tagName(instr.tag), instr.data });
        }
    }

    return ir;
}

fn writeError(writer: anytype, message: []const u8, span: Span, src: [:0]const u8) !void {
    const loc = span.loc(src);

    var line_start = span.start;
    while (line_start > 0) switch (src[line_start - 1]) {
        '\n' => break,
        else => line_start -= 1,
    };

    var line_end = span.end;
    while (line_end < src.len) switch (src[line_end]) {
        '\n', 0 => break,
        else => line_end += 1,
    };

    try writer.print("{s}\n", .{src[line_start..line_end]});
    for (line_start..span.start) |_| try writer.writeByte(' ');

    for (span.start..span.end) |_| try writer.writeByte('^');
    try writer.print(" error: {s} ({d}:{d})\n", .{ message, loc.line, loc.char });
}
