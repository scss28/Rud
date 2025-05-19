const std = @import("std");
const fmt = std.fmt;
const time = std.time;
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

            const pre_comptime: time.Instant = try .now();
            var ir = try build(gpa, path);
            defer ir.deinit(gpa);

            const ns = (try time.Instant.now()).since(pre_comptime);
            try io.getStdOut().writer().print(
                "\x1b[2mCompilation finished in {d:.3} s\x1b[m\n",
                .{@as(f64, @floatFromInt(ns)) / time.ns_per_s},
            );

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
            try writeError(out, err.message, span, path, src);
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
            try writeError(out, message, span, path, src);
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

fn writeError(
    writer: anytype,
    message: []const u8,
    span: Span,
    path: []const u8,
    src: [:0]const u8,
) !void {
    const loc = span.loc(src);

    try writer.print("\x1b[1;91merror\x1b[m \x1b[1m{s}:{d}:{d}\x1b[m:\n", .{
        path,
        loc.line,
        loc.char,
    });

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

    const offset = fmt.count("{d}", .{loc.line});
    if (line_start != 0) {
        var previous_line_start = line_start - 1;
        while (line_start > 0) switch (src[previous_line_start - 1]) {
            '\n' => break,
            else => previous_line_start -= 1,
        };

        for (0..offset) |_| try writer.writeByte(' ');

        try writer.print("\x1b[2m |\x1b[m {s}\n", .{
            src[previous_line_start .. line_start - 1],
        });
    }

    try writer.print("\x1b[2m{d} |\x1b[m {s}\n", .{
        loc.line,
        src[line_start..line_end],
    });

    for (0..offset + 3) |_| try writer.writeByte(' ');
    for (line_start..span.start) |_| try writer.writeByte(' ');

    try writer.writeAll("\x1b[33;1m");
    for (span.start..span.end) |_| try writer.writeByte('^');
    try writer.print(" \x1b[0;1m{s}\x1b[m\n\n", .{message});
}
