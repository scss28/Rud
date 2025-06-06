const std = @import("std");
const io = std.io;

const RAst = @import("root").RAst;

pub fn emit(writer: anytype, rast: *const RAst) !void {
    var e: Emitter(@TypeOf(writer)) = .{
        .rast = rast,
        .writer = writer,
    };

    for (rast.rootNodes()) |ref| {
        try e.emitRef(writer, ref);
    }
}

fn Emitter(Writer: anytype) type {
    return struct {
        writer: Writer,
        rast: *const RAst,
        fn emitNode(e: *Emitter, node: RAst.Node.Index) !void {
            switch (e.rast.nodeTag(node)) {}
        }
    };
}
