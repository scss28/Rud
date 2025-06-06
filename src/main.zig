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

pub const Ast = @import("Ast.zig");
pub const Parser = @import("Parser.zig");

pub const RAst = @import("RAst.zig");
const Sema = @import("Sema.zig");

pub const Vm = @import("Vm.zig");

pub const InternPool = @import("InternPool.zig");
pub const Span = @import("Span.zig");

pub fn main() !void {
    var gpa_state = if (debug.runtime_safety) heap.DebugAllocator(.{}).init else {};
    defer if (debug.runtime_safety) {
        _ = gpa_state.deinit();
    };

    const gpa = if (debug.runtime_safety) gpa_state.allocator() else heap.smp_allocator;

    var args = try process.argsWithAllocator(gpa);
    defer args.deinit();

    _ = args.skip();
    const path: []const u8 = args.next() orelse {
        log.err("expected a path argument", .{});
        process.exit(1);
    };

    var file_path = path;
    var arg_types: std.ArrayListUnmanaged(InternPool.Type) = .empty;
    defer arg_types.deinit(gpa);

    if (mem.indexOfScalar(u8, path, '(')) |lp| {
        var end = lp;
        if (path[lp + 1] != ')') while (true) {
            if (path[end] == ')') break;
            end += 1;

            var start = end;
            while (path[start] == ' ') {
                start += 1;
            }

            end = start;
            while (path[end] != ',' and path[end] != ')') end += 1;

            const ty = path[start..end];
            if (mem.eql(u8, ty, "i32")) {
                try arg_types.append(gpa, .i32);
            } else {
                log.err("'{s}' not a valid arg type", .{ty});
                process.exit(1);
            }
        };

        file_path = path[0..lp];
    }

    var res = try Sema.analyze(gpa, fs.cwd(), file_path, arg_types.items);
    defer res.deinit(gpa);

    if (res.errors.len != 0) {
        const writer = io.getStdOut().writer();
        for (res.errors) |e| {
            const message = e.messageSlice(res);
            const src = e.file.ast.src;
            const file_name = e.file.name;

            const span = e.span();
            const loc = span.loc(src);

            try writer.print("\x1b[1;91merror\x1b[m \x1b[1m{s}:{d}:{d}\x1b[m:\n", .{
                file_name,
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

        process.exit(1);
    }

    const rast = res.rast;
    for (rast.rootNodes()) |node| {
        log.info("{s}", .{@tagName(rast.nodeTag(node))});
    }

    // var vm_ir: Vm.Ir = try .emit(gpa, &rast, &ip);
    //     defer vm_ir.deinit(gpa);

    // if (builtin.mode == .Debug) {
    //     try out.writeAll("\nIr:\n");
    //     for (vm_ir.instrs.items(.tag), 0..) |tag, i| {
    //         try out.print("{d}.\t{s}\n", .{ i, @tagName(tag) });
    //     }
    // }

    // var stack_buf: [64]usize = undefined;
    // var locals_buf: [64]usize = undefined;
    // var call_stack_buf: [64]u32 = undefined;

    // locals_buf[0] = @as(u32, @bitCast(@as(i32, 5)));
    // locals_buf[1] = @as(u32, @bitCast(@as(i32, 5)));

    // var vm: Vm = .init(&vm_ir, &stack_buf, &locals_buf, &call_stack_buf);
    // while (vm.next()) {}

    // switch (rast.retType()) {
    //     .i32 => {
    //         const int: i32 = @bitCast(@as(u32, @truncate(stack_buf[0])));
    //         std.debug.print("{d}\n", .{int});
    //     },
    //     .f32 => {
    //         const float: f32 = @bitCast(@as(u32, @truncate(stack_buf[0])));
    //         std.debug.print("{d}\n", .{float});
    //     },
    //     .none => {},
    //     else => unreachable,
    // }
}
