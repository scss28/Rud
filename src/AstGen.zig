const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const math = std.math;
const heap = std.heap;
const meta = std.meta;

const Ast = @import("Ast.zig");
const Mir = @import("Mir.zig");
const AstGen = @This();

gpa: mem.Allocator,
arena_state: heap.ArenaAllocator,

ast: *const Ast,

instrs: Mir.Instrs,
string_bytes: std.ArrayListUnmanaged(u8),
extra_data: std.ArrayListUnmanaged(u32),

scratch: std.ArrayListUnmanaged(u32),

scope: *Scope,
var_stack_ptr: VarStackIndex,
pool: Pool,

errors: std.ArrayListUnmanaged(Mir.Error),
args: std.ArrayListUnmanaged(Mir.Type),

const VarStackIndex = u32;
const Scope = struct {
    tag: Tag,
    parent: ?*Scope,
    vars: std.StringHashMapUnmanaged(Var),

    const Var = struct {
        pooled: Pool.Index,
        index: VarStackIndex,
    };

    const Tag = enum {
        func,
        block,
    };
};

const Pool = struct {
    data: std.ArrayListUnmanaged(usize),

    fn get(self: Pool, gpa: mem.Allocator, key: Key) Index {
        _ = self;
    }

    const Key = union(enum) {
        int: struct {
            literal: []const u8,
        },
    };

    const Index = enum(u32) {
        none = math.maxInt(u32),
        _,
    };
};

const Ref = enum(u32) {
    none = math.maxInt(u32),

    fn initPooled(index: Pool.Index) Ref {
        return @enumFromInt(@intFromEnum(index));
    }

    fn initInstr(instr: Mir.Instr.Index) Ref {
        return @enumFromInt(@intFromEnum(instr) | 1 << 31);
    }

    fn toType(self: Ref) ?Pool.Index {
        if (@intFromEnum(self) >> 31 == 0) {
            return @enumFromInt(@as(u31, @truncate(@intFromEnum(self))));
        }

        return null;
    }

    fn toInstr(self: Ref) ?Mir.Instr.Index {
        if (@intFromEnum(self) >> 31 != 0) {
            return @enumFromInt(@as(u31, @truncate(@intFromEnum(self))));
        }

        return null;
    }
};

pub fn gen(gpa: mem.Allocator, ast: *const Ast) mem.Allocator.Error!Mir {
    var g: AstGen = .{
        .gpa = gpa,
        .ast = ast,
        .arena_state = .init(gpa),

        .instrs = .{},
        .string_bytes = .{},
        .extra_data = .{},

        .scope = undefined,
        .var_stack_ptr = 0,

        .scratch = .{},
        .errors = .{},
    };
    defer {
        g.errors.deinit(gpa);
        g.scratch.deinit(gpa);
        g.arena_state.deinit();
    }

    g.scope = try g.arena_state.allocator().create(Scope);
    g.scope.* = .{
        .tag = .func,
        .parent = null,
        .vars = .{},
    };

    // StringIndex.empty
    try g.string_bytes.append(gpa, 0);

    // Reserved slots of ExtraIndex
    const reserved_count = @typeInfo(Mir.ExtraIndex).@"enum".fields.len;
    try g.extra_data.ensureTotalCapacity(gpa, ast.nodes.len + reserved_count);
    g.extra_data.items.len += reserved_count;

    try g.instrs.ensureTotalCapacity(gpa, ast.nodes.len);

    const root = try g.reserveInstr();

    var ret_type: Mir.Type = .nil;
    for (ast.rootNodes()) |node| {
        var instr, const value, const flow =
            g.genNode(node) catch |err| switch (err) {
                error.Gen => continue,
                error.OutOfMemory => |oom| return oom,
            };

        if (flow == .ret) {
            if (!value.runtime()) {
                instr = g.valueToInstr(node, value) catch |err| switch (err) {
                    error.Gen => break,
                    error.OutOfMemory => |oom| return oom,
                };
            }

            const ret_instr = try g.addInstr(.{
                .tag = .ret,
                .data = .{
                    .lhs = instr,
                    .rhs = undefined,
                },
            });

            try g.scratch.append(g.gpa, ret_instr);

            ret_type = switch (value) {
                .int, .comp_int => .int,
                .float, .comp_str => .float,
                .nil => .nil,
                else => unreachable,
            };
            break;
        }
        if (instr != Mir.Instr.none) try g.scratch.append(gpa, instr);

        if (value != .nil) {
            const err = g.throw(
                node,
                "expression needs to return null",
                .{},
            );
            if (err == error.OutOfMemory) return error.OutOfMemory;
        }
    }

    g.extra_data.items[@intFromEnum(Mir.ExtraIndex.ret_type)] = @intFromEnum(ret_type);

    const root_start: u32 = @intCast(g.extra_data.items.len);
    try g.extra_data.appendSlice(gpa, g.scratch.items);

    g.instrs.set(root, .{
        .tag = undefined,
        .data = .{
            .lhs = root_start,
            .rhs = @intCast(g.extra_data.items.len),
        },
    });

    const errors_index: u32 = @intFromEnum(Mir.ExtraIndex.errors);
    if (g.errors.items.len == 0) {
        g.extra_data.items[errors_index] = 0;
    } else {
        try g.extra_data.ensureUnusedCapacity(
            gpa,
            1 + g.errors.items.len * @typeInfo(Mir.Error).@"struct".fields.len,
        );

        g.extra_data.items[errors_index] = g.addExtraAssumeCapacity(.{
            @as(u32, @intCast(g.errors.items.len)),
        });

        for (g.errors.items) |err| {
            _ = g.addExtraAssumeCapacity(err);
        }
    }

    g.extra_data.items[@intFromEnum(Mir.ExtraIndex.max_vars)] = g.var_stack_ptr;

    return .{
        .instrs = g.instrs.toOwnedSlice(),
        .string_bytes = try g.string_bytes.toOwnedSlice(gpa),
        .extra_data = try g.extra_data.toOwnedSlice(gpa),
    };
}

fn valueToInstr(
    self: *AstGen,
    src_node: Ast.Node.Index,
    value: Value,
) Error!Mir.Instr.Index {
    return switch (value) {
        .comp_int => |int| self.addInstr(.{
            .tag = .int,
            .data = .{
                .lhs = @bitCast(int),
                .rhs = undefined,
            },
        }),
        .comp_float => |float| self.addInstr(.{
            .tag = .float,
            .data = .{
                .lhs = @bitCast(float),
                .rhs = undefined,
            },
        }),
        else => self.throw(src_node, "unable to convert {s} to a runtime value", .{
            @tagName(value),
        }),
    };
}

const Error = error{Gen} || mem.Allocator.Error;
const ControllFlow = enum { ret, pass };

fn genNode(self: *AstGen, node: Ast.Node.Index) Error!struct {
    Mir.Instr.Index,
    Value,
    ControllFlow,
} {
    switch (self.ast.full(node)) {
        .identifier => |slice| {
            const v = self.getVar(slice) orelse return self.throw(
                node,
                "variable '{s}' not initialized",
                .{slice},
            );

            if (v.value.runtime()) return .{
                try self.addInstr(.{
                    .tag = .load,
                    .data = .{
                        .lhs = v.index,
                        .rhs = undefined,
                    },
                }),
                v.value,
                .pass,
            };

            return .{
                Mir.Instr.none,
                v.value,
                .pass,
            };
        },
        .assign => |assign| {
            const instr, const value = try self.genNodeAssertPass(assign.rhs);
            const @"var" = try self.scope.vars.getOrPut(
                self.arena_state.allocator(),
                assign.identifier,
            );

            if (!@"var".found_existing) {
                @"var".value_ptr.index = self.var_stack_ptr;
                self.var_stack_ptr += 1;
            }
            @"var".value_ptr.value = value;

            if (value.runtime()) {
                return .{
                    try self.addInstr(.{
                        .tag = .store,
                        .data = .{
                            .lhs = @"var".value_ptr.index,
                            .rhs = instr,
                        },
                    }),
                    .nil,
                    .pass,
                };
            }

            return .{
                Mir.Instr.none,
                .nil,
                .pass,
            };
        },
        .literal_int => |slice| {
            const int = fmt.parseInt(i32, slice, 10) catch unreachable;
            return .{ Mir.Instr.none, .{ .comp_int = int }, .pass };
        },
        .literal_float => |slice| {
            const float = fmt.parseFloat(f32, slice) catch unreachable;
            return .{ Mir.Instr.none, .{ .comp_float = float }, .pass };
        },
        .fn_decl => {
            return .{ 0, .{ .func = node }, .pass };
        },
        .ret_expr => |expr| {
            const instr, const value = try self.genNodeAssertPass(expr);
            return .{ instr, value, .ret };
        },
        .call => |call| {
            _, const value = try self.genNodeAssertPass(call.callee);
            if (value != .func) return self.throw(
                call.callee,
                "{s} not callable",
                .{@tagName(value)},
            );

            const fn_decl = self.ast.full(value.func).fn_decl;
            if (call.args.len != fn_decl.args.len) return self.throw(
                node,
                "expected {d} arguments got {d}",
                .{ fn_decl.args.len, call.args.len },
            );

            const func_scope = try self.arena_state.allocator().create(Scope);
            func_scope.* = .{
                .tag = .func,
                .parent = self.scope,
                .vars = .{},
            };

            const scratch_start = self.scratch.items.len;
            try self.scratch.ensureUnusedCapacity(self.gpa, call.args.len);

            for (fn_decl.args, call.args) |token, arg| {
                const instr_arg, const value_arg = try self.genNodeAssertPass(arg);
                try self.scratch.append(self.gpa, instr_arg);

                const identifier = self.ast.tokenSlice(token);
                try self.setVar(func_scope, identifier, value_arg);
            }

            const args: u32 = @intCast(self.extra_data.items.len);
            try self.extra_data.appendSlice(
                self.gpa,
                self.scratch.items[scratch_start..],
            );
            self.scratch.items.len = scratch_start;

            const outer_scope = self.scope;
            defer self.scope = outer_scope;

            self.scope = func_scope;

            const instr_body, const value_body =
                try self.genNodeAssertPass(fn_decl.body);
            if (!value_body.runtime()) return .{
                Mir.Instr.none,
                value_body,
                .pass,
            };

            const instr_call = try self.addInstr(.{
                .tag = .call,
                .data = .{
                    .lhs = args,
                    .rhs = instr_body,
                },
            });

            return .{ instr_call, value_body, .pass };
        },
        inline .add, .sub, .mul, .pow, .div => |binop, tag| {
            var lhs_instr, const lhs_value = try self.genNodeAssertPass(binop.lhs);
            var rhs_instr, const rhs_value = try self.genNodeAssertPass(binop.rhs);

            const lhs_runtime = lhs_value.runtime();
            const rhs_runtime = rhs_value.runtime();

            blk: {
                if (lhs_runtime or rhs_runtime) {
                    if (!lhs_value.tagEq(rhs_value)) break :blk;

                    const instr_tag: Mir.Instr.Tag = switch (lhs_value) {
                        .int, .comp_int => switch (tag) {
                            .add => .addi,
                            .sub => .subi,
                            .mul => .muli,
                            .pow => .powi,
                            .div => .divi,
                            else => unreachable,
                        },
                        .float, .comp_float => switch (tag) {
                            .add => .addf,
                            .sub => .subf,
                            .mul => .mulf,
                            .pow => .powf,
                            .div => .divf,
                            else => unreachable,
                        },
                        else => break :blk,
                    };

                    var value: Value = undefined;
                    if (!lhs_runtime) {
                        lhs_instr = switch (lhs_value) {
                            .comp_int => |int| try self.addInstr(.{
                                .tag = .int,
                                .data = .{
                                    .lhs = @bitCast(int),
                                    .rhs = undefined,
                                },
                            }),
                            .comp_float => |float| try self.addInstr(.{
                                .tag = .int,
                                .data = .{
                                    .lhs = @bitCast(float),
                                    .rhs = undefined,
                                },
                            }),
                            else => unreachable,
                        };
                        value = rhs_value;
                    }

                    if (!rhs_runtime) {
                        rhs_instr = switch (rhs_value) {
                            .comp_int => |int| try self.addInstr(.{
                                .tag = .int,
                                .data = .{
                                    .lhs = @bitCast(int),
                                    .rhs = undefined,
                                },
                            }),
                            .comp_float => |float| try self.addInstr(.{
                                .tag = .int,
                                .data = .{
                                    .lhs = @bitCast(float),
                                    .rhs = undefined,
                                },
                            }),
                            else => unreachable,
                        };
                        value = lhs_value;
                    }

                    return .{
                        try self.addInstr(.{
                            .tag = instr_tag,
                            .data = .{
                                .lhs = lhs_instr,
                                .rhs = rhs_instr,
                            },
                        }),
                        value,
                        .pass,
                    };
                } else switch (lhs_value) {
                    .comp_int => |lhs| {
                        switch (rhs_value) {
                            .comp_int => |rhs| return .{
                                0,
                                .{ .comp_int = switch (tag) {
                                    .add => lhs + rhs,
                                    .sub => lhs - rhs,
                                    .mul => lhs * rhs,
                                    .pow => math.pow(i32, lhs, rhs),
                                    .div => @divTrunc(lhs, rhs),
                                    else => unreachable,
                                } },
                                .pass,
                            },
                            .comp_float => |rhs| {
                                const lhs_f: f32 = @floatFromInt(lhs);
                                return .{
                                    0,
                                    .{ .comp_float = switch (tag) {
                                        .add => lhs_f + rhs,
                                        .sub => lhs_f - rhs,
                                        .mul => lhs_f * rhs,
                                        .pow => math.pow(f32, lhs_f, rhs),
                                        .div => lhs_f / rhs,
                                        else => unreachable,
                                    } },
                                    .pass,
                                };
                            },
                            else => break :blk,
                        }
                    },
                    .comp_float => |lhs| {
                        const rhs: f32 = switch (rhs_value) {
                            .comp_int => |rhs| @floatFromInt(rhs),
                            .comp_float => |rhs| rhs,
                            else => break :blk,
                        };

                        return .{ 0, .{ .comp_float = switch (tag) {
                            .add => lhs + rhs,
                            .sub => lhs - rhs,
                            .mul => lhs * rhs,
                            .pow => math.pow(f32, lhs, rhs),
                            .div => lhs / rhs,
                            else => unreachable,
                        } }, .pass };
                    },
                    else => break :blk,
                }
            }

            return self.throw(node, "unable to {s} {s} and {s}", .{
                @tagName(tag),
                @tagName(lhs_value),
                @tagName(rhs_value),
            });
        },
        .builtin_call => |bcall| {
            const tag = meta.stringToEnum(enum {
                arg,
            }, bcall.identifier) orelse {
                return self.throw(node, "not a builtin", .{});
            };

            switch (tag) {
                .arg => {
                    if (bcall.args.len != 2) return self.throw(
                        node,
                        "expected 2 argument",
                        .{},
                    );

                    if (self.ast.nodeTag(bcall.args[0]) != .identifier) {
                        return self.throw(
                            bcall.args[0],
                            "expected a type",
                            .{},
                        );
                    }

                    const identifier = self.ast.nodeTokenSlice(bcall.args[0]);
                    const ty = meta.stringToEnum(Mir.Type, identifier) orelse {
                        return self.throw(
                            bcall.args[0],
                            "expected a type",
                            .{},
                        );
                    };
                    _ = ty;

                    return .{ 0, .nil, .pass };
                },
            }
        },
        inline else => |_, tag| return self.throw(
            node,
            "{s} not implemenented",
            .{@tagName(tag)},
        ),
    }
}

fn genNodeAssertPass(self: *AstGen, node: Ast.Node.Index) Error!struct {
    Mir.Instr.Index,
    Value,
} {
    const instr, const value, const flow = try self.genNode(node);
    if (flow != .pass) return self.throw(node, "illegal control flow", .{});

    return .{ instr, value };
}

fn setVar(
    self: *AstGen,
    scope: *Scope,
    identifier: []const u8,
    value: Value,
) Error!void {
    const @"var" = try scope.vars.getOrPut(self.arena_state.allocator(), identifier);
    if (!@"var".found_existing) {
        @"var".value_ptr.index = self.var_stack_ptr;
        self.var_stack_ptr += 1;
    }

    @"var".value_ptr.value = value;
}

fn getVar(self: *AstGen, identifier: []const u8) ?Scope.Var {
    var outside_fn = false;
    var scope: ?*Scope = self.scope;
    while (scope) |s| : (scope = s.parent) {
        if (s.vars.get(identifier)) |@"var"| {
            if (@"var".value.runtime() and outside_fn) continue;
            return @"var";
        }

        if (s.tag == .func) outside_fn = true;
    }

    return null;
}

fn addInstr(self: *AstGen, instr: Mir.Instr) mem.Allocator.Error!Mir.Instr.Index {
    const index: u32 = try self.reserveInstr();
    self.instrs.set(index, instr);

    return index;
}

fn reserveInstr(self: *AstGen) mem.Allocator.Error!Mir.Instr.Index {
    return @intCast(try self.instrs.addOne(self.gpa));
}

fn printString(
    self: *AstGen,
    comptime fmt_str: []const u8,
    args: anytype,
) mem.Allocator.Error!Mir.StringIndex {
    const string: Mir.StringIndex = @enumFromInt(self.string_bytes.items.len);
    try self.string_bytes.writer(self.gpa).print(fmt_str ++ "\x00", args);

    return string;
}

fn throw(
    self: *AstGen,
    node: Ast.Node.Index,
    comptime fmt_str: []const u8,
    args: anytype,
) Error {
    const message = try self.printString(fmt_str, args);
    try self.errors.append(self.gpa, .{
        .message = message,
        .node = node,
    });

    return error.Gen;
}

fn addExtra(self: AstGen, extra: anytype) mem.Allocator.Error!u32 {
    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    try self.extra_data.ensureUnusedCapacity(self.gpa, fields.len);

    return self.addExtraAssumeCapacity(extra);
}

fn addExtraAssumeCapacity(self: *AstGen, extra: anytype) u32 {
    const index: u32 = @intCast(self.extra_data.items.len);

    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    self.extra_data.items.len += fields.len;
    self.setExtra(index, extra);

    return index;
}

fn setExtra(self: *AstGen, index: u32, extra: anytype) void {
    var i = index;
    inline for (@typeInfo(@TypeOf(extra)).@"struct".fields) |field| {
        if (field.type == u32) {
            self.extra_data.items[i] = @field(extra, field.name);
        } else if (@typeInfo(field.type) == .@"enum") {
            self.extra_data.items[i] = @intFromEnum(@field(extra, field.name));
        } else {
            self.extra_data.items[i] = @bitCast(@field(extra, field.name));
        }

        i += 1;
    }
}
