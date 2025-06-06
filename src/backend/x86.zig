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

const root = @import("root");
const RAst = root.RAst;
const InternPool = root.InternPool;

pub fn emit(writer: io.AnyWriter, rast: *const RAst, ip: *const InternPool) !void {
    var e: Emitter = .{
        .rast = rast,
        .ip = ip,
    };

    for (rast.rootNodes()) |ref| {
        try e.emitRef(writer, ref);
    }
}

const Emitter = struct {
    rast: *const RAst,
    ip: *const InternPool,

    fn emitRef(e: *Emitter, writer: io.AnyWriter, ref: *const RAst.Ref) !void {
        _ = e;
        _ = writer;
        if (ref.toValue()) |v| switch (v.ty()) {
            .int => {
                unreachable; // todo
            },
            .static_int, .type => unreachable,
        };
    }
};
