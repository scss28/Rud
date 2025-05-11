const std = @import("std");
const fs = std.fs;
const math = std.math;
const mem = std.mem;
const meta = std.meta;
const process = std.process;
const debug = std.debug;
const heap = std.heap;
const log = std.log;

const repl = @import("repl.zig");
const Ast = @import("Ast.zig");
const State = @import("State.zig");
const Eval = @import("Eval.zig");

const help =
    \\Usage: {s} [command]
    \\
    \\Commands:
    \\
    \\  repl (default)  Enter a repl
    \\  run [path]      Run code from a file
    \\  help            Print this help and exit
    \\
;

const Command = enum {
    repl,
    run,
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
        try repl.run(gpa);
        return;
    };

    const command = meta.stringToEnum(Command, first_arg) orelse {
        log.info(help, .{program});
        log.err("unknown command: {s}", .{first_arg});
        return;
    };

    switch (command) {
        .repl => {
            try repl.run(gpa);
        },
        .run => {
            const path = args.next() orelse {
                log.err("expected a path argument", .{});
                return;
            };

            const src = try fs.cwd().readFileAllocOptions(
                gpa,
                path,
                math.maxInt(u32),
                null,
                1,
                0,
            );
            defer gpa.free(src);

            var ast = try Ast.parse(gpa, src);
            defer ast.deinit(gpa);

            if (ast.errors.len > 0) {
                for (ast.errors) |err| {
                    log.err("{s}", .{err.message});
                }

                return;
            }

            var state: State = .init(gpa);
            defer state.deinit();

            var last_value: State.Value = .nil;

            const nodes = ast.rootNodes();
            for (nodes) |node| {
                var eval: Eval = .init(&state, &ast);
                defer eval.deinit();

                last_value = eval.eval(node) catch |err| switch (err) {
                    error.OutOfMemory => |oom| return oom,
                    error.Eval => {
                        log.err("{s}", .{eval.err.message});
                        return;
                    },
                };
            }

            log.info("{}", .{last_value});
        },
        .help => {
            log.info(help, .{program});
        },
    }
}
