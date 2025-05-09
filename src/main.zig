const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const process = std.process;
const debug = std.debug;
const heap = std.heap;
const log = std.log;

const repl = @import("repl.zig");
const help =
    \\Usage: {s} [command]
    \\
    \\Commands:
    \\
    \\  repl (default)  Enter a repl
    \\  help            Print this help and exit
    \\
;

const Command = enum {
    repl,
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
        .help => {
            log.info(help, .{program});
        },
        .repl => {
            try repl.run(gpa);
        },
    }
}
