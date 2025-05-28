// const std = @import("std");
// const heap = std.heap;
// const fmt = std.fmt;
// const meta = std.meta;
// const mem = std.mem;
//
// const assert = std.debug.assert;
//
// const Ast = @import("Ast.zig");
// const Ir = @import("Ir.zig");
// const Instr = Ir.Instr;
//
// const Emitter = @This();
//
// gpa: mem.Allocator,
// arena_state: heap.ArenaAllocator,
//
// index: Ast.Node.Index,
// ast: *const Ast,
//
// instrs: Ir.Instrs,
// string_bytes: std.ArrayListUnmanaged(u8),
// extra_data: std.ArrayListUnmanaged(u32),
//
// scope: *Scope,
// var_stack_ptr: VarStackIndex,
//
// // call_cache: std.AutoHashMapUnmanaged(void, CallData),
//
// errors: std.ArrayListUnmanaged(Ir.Error),
//
// const CallData = struct {
//     func_instr: Instr.Index,
//     value: Value,
// };
//
// const VarStackIndex = u32;
//
// const Scope = struct {
//     tag: Tag,
//     parent: ?*Scope,
//     vars: std.StringHashMapUnmanaged(Var),
//
//     const Var = struct {
//         value: Value,
//         index: VarStackIndex,
//     };
//
//     const Tag = enum {
//         func,
//         block,
//     };
// };
//
// pub fn emit(gpa: mem.Allocator, ast: *const Ast) mem.Allocator.Error!Ir {
//     var e: Emitter = .{
//         .gpa = gpa,
//         .arena_state = .init(gpa),
//
//         .index = 0,
//         .ast = ast,
//
//         .instrs = .{},
//         .extra_data = .{},
//         .string_bytes = .{},
//
//         .scope = undefined,
//         .var_stack = .{},
//
//         .errors = .{},
//     };
//     defer {
//         e.errors.deinit(gpa);
//         e.var_stack.deinit(gpa);
//         e.arena_state.deinit();
//     }
//
//     e.scope = try e.arena_state.allocator().create(Scope);
//     e.scope.* = .{
//         .tag = .func,
//         .parent = null,
//         .vars = .{},
//     };
//
//     // StringIndex.empty
//     try e.string_bytes.append(gpa, 0);
//
//     // Reserved slots of ExtraIndex
//     const reserved_count = @typeInfo(Ir.ExtraIndex).@"enum".fields.len;
//     try e.extra_data.ensureTotalCapacity(gpa, ast.nodes.len + reserved_count);
//     e.extra_data.items.len += reserved_count;
//
//     for (ast.rootNodes()) |node| {
//         const res = e.inspectNode(node) catch |err| switch (err) {
//             error.OutOfMemory => |oom| return oom,
//             error.Emit => continue,
//         };
//
//         e.assertType(node, .nil, res) catch |err| switch (err) {
//             error.OutOfMemory => |oom| return oom,
//             error.Emit => continue,
//         };
//     }
//
//     try e.instrs.append(gpa, .{
//         .tag = .exit,
//         .data = 0,
//     });
//
//     const errors_index: u32 = @intFromEnum(Ir.ExtraIndex.errors);
//     if (e.errors.items.len == 0) {
//         e.extra_data.items[errors_index] = 0;
//     } else {
//         try e.extra_data.ensureUnusedCapacity(
//             gpa,
//             1 + e.errors.items.len * @typeInfo(Ir.Error).@"struct".fields.len,
//         );
//
//         e.extra_data.items[errors_index] = e.addExtraAssumeCapacity(.{
//             @as(u32, @intCast(e.errors.items.len)),
//         });
//
//         for (e.errors.items) |err| {
//             _ = e.addExtraAssumeCapacity(err);
//         }
//     }
//
//     const var_count_index: u32 = @intFromEnum(Ir.ExtraIndex.var_count);
//     e.extra_data.items[var_count_index] = @intCast(e.var_stack.capacity);
//
//     return .{
//         .instrs = e.instrs.toOwnedSlice(),
//         .string_bytes = try e.string_bytes.toOwnedSlice(gpa),
//         .extra_data = try e.extra_data.toOwnedSlice(gpa),
//     };
// }
//
// const Value = union(enum) {
//     nil,
//     int,
//     float,
//     cstr,
//     func: Ast.Node.Index,
//
//     inline fn isRuntime(self: Value) bool {
//         return switch (self) {
//             .nil, .int, .float, .cstr => true,
//             func => false,
//         };
//     }
// };
//
// fn setVar(
//     self: *Emitter,
//     scope: *Scope,
//     identifier: []const u8,
//     value: Value,
// ) (mem.Allocator.Error || error{Emit})!void {
//     const v = try scope.vars.getOrPut(self.gpa, identifier);
//     if (!v.found_existing) {
//         v.value_ptr.index = self.var_stack_ptr;
//         self.var_stack_ptr += 1;
//     }
//
//     v.value_ptr.value = value;
// }
//
// fn getVar(self: *Emitter, identifier: []const u8) ?Scope.Var {
//     var outside_fn = false;
//     var scope: ?*Scope = self.scope;
//     while (scope) |s| : (scope = s.parent) {
//         if (s.vars.get(identifier)) |v| {
//             if (v == .runtime and outside_fn) continue;
//             return v;
//         }
//
//         if (s.tag == .function) outside_fn = true;
//     }
//
//     return null;
// }
//
// fn inspectNode(
//     self: *Emitter,
//     node: Ast.Node.Index,
// ) (mem.Allocator.Error || error{Emit})!InspectResult {
//     switch (self.ast.full(node)) {
//         .identifier => |slice| {
//             const v = self.getVar(slice) orelse return self.throw(
//                 node,
//                 "variable '{s}' not initialized",
//                 .{slice},
//             );
//
//             switch (v) {
//                 .runtime => |index| {
//                     try self.appendInstr(.{
//                         .tag = .load,
//                         .data = @intCast(index),
//                     });
//
//                     return .initType(self.var_stack.items[index]);
//                 },
//                 .comp => |comp| return .initComp(comp),
//             }
//         },
//         .literal_int => |slice| {
//             const int = fmt.parseInt(Ir.consts.Int, slice, 10) catch unreachable;
//             try self.appendInstr(.{
//                 .tag = .pushi,
//                 .data = try self.addExtra(.{ .int = int }),
//             });
//
//             return .initType(.int);
//         },
//         .literal_float => |slice| {
//             const float = fmt.parseFloat(Ir.consts.Float, slice) catch unreachable;
//             try self.appendInstr(.{
//                 .tag = .pushf,
//                 .data = try self.addExtra(.{ .float = float }),
//             });
//
//             return .initType(.float);
//         },
//         .literal_str => |slice| {
//             try self.string_bytes.ensureUnusedCapacity(self.gpa, slice.len - 1);
//             const index: u32 = @intCast(self.string_bytes.items.len);
//             var i: usize = 1;
//             while (i < slice.len - 1) : (i += 1) {
//                 const byte = switch (slice[i]) {
//                     '\\' => blk: {
//                         defer i += 1;
//                         break :blk try self.escapeCharacter(node, slice[i + 1]);
//                     },
//                     else => |b| b,
//                 };
//
//                 self.string_bytes.appendAssumeCapacity(byte);
//             }
//             self.string_bytes.appendAssumeCapacity(0);
//
//             try self.appendInstr(.{
//                 .tag = .pushs,
//                 .data = index,
//             });
//             return .initType(.cstr);
//         },
//         inline .add, .sub, .mul, .pow, .div => |binop, tag| {
//             const lhs = try self.inspectNode(binop.lhs);
//             const rhs = try self.inspectNode(binop.rhs);
//
//             if (lhs == .int and rhs == .int) {
//                 const instr_tag = switch (tag) {
//                     .add => .addi,
//                     .sub => .subi,
//                     .mul => .muli,
//                     .pow => .powi,
//                     .div => .divi,
//                     else => unreachable,
//                 };
//                 try self.appendInstr(.tagOnly(instr_tag));
//
//                 return .initType(.int);
//             } else if (lhs == .float and rhs == .float) {
//                 const instr_tag = switch (tag) {
//                     .add => .addf,
//                     .sub => .subf,
//                     .mul => .mulf,
//                     .pow => .powf,
//                     .div => .divf,
//                     else => unreachable,
//                 };
//                 try self.appendInstr(.tagOnly(instr_tag));
//
//                 return .initType(.float);
//             }
//
//             return self.throw(
//                 node,
//                 "unable to {s} {s} and {s}",
//                 .{
//                     @tagName(tag),
//                     @tagName(lhs),
//                     @tagName(rhs),
//                 },
//             );
//         },
//         .assign => |assign| {
//             const res = try self.inspectNode(assign.rhs);
//             try self.setVar(self.scope, assign.identifier, res);
//
//             return .initType(.nil);
//         },
//         .call => |call| {
//             const value = try self.inspectNode(call.callee);
//             const fn_decl = switch (value) {
//                 .function => |index| self.ast.full(index).fn_decl,
//                 else => return self.throw(call.callee, "{s} not callable", .{
//                     @tagName(value.type),
//                 }),
//             };
//
//             if (fn_decl.args.len != call.args.len) return self.throw(
//                 node,
//                 "expected {d} arguments got {d}",
//                 .{ fn_decl.args.len, call.args.len },
//             );
//
//             const func_scope = try self.arena_state.allocator().create(Scope);
//             func_scope.* = .{
//                 .tag = .function,
//                 .parent = self.scope,
//                 .vars = .{},
//             };
//
//             for (fn_decl.args, call.args) |identifier, arg| {
//                 const arg_res = try self.inspectNode(arg);
//                 try self.setVar(func_scope, identifier, arg_res);
//             }
//
//             const outer_scope = self.scope;
//             defer self.scope = outer_scope;
//             self.scope = func_scope;
//
//             try self.appendInstr(.{
//                 .tag = .call,
//                 .data = @intCast(self.instrs.len),
//             });
//             const func_res = try self.inspectNode(fn_decl.body);
//
//             return func_res;
//         },
//         .builtin_call => |bcall| {
//             const tag = meta.stringToEnum(enum {
//                 print,
//                 int,
//                 float,
//             }, bcall.identifier) orelse {
//                 return self.throw(node, "not a builtin", .{});
//             };
//             switch (tag) {
//                 .print => {
//                     if (bcall.args.len < 1) return self.throw(
//                         node,
//                         "expected at least 1 argument",
//                         .{},
//                     );
//
//                     if (self.ast.nodeTag(bcall.args[0]) != .literal_str) {
//                         return self.throw(
//                             bcall.args[0],
//                             "first argument must be a str literal",
//                             .{},
//                         );
//                     }
//
//                     const slice = self.ast.nodeTokenSlice(bcall.args[0]);
//
//                     try self.string_bytes.ensureUnusedCapacity(self.gpa, slice.len - 1);
//                     var scratch: std.ArrayListUnmanaged(u8) = try .initCapacity(
//                         self.gpa,
//                         slice.len - 1,
//                     );
//                     defer scratch.deinit(self.gpa);
//
//                     var i: usize = 1;
//                     var arg_i: usize = 1;
//                     while (i < slice.len - 1) : (i += 1) {
//                         const byte = switch (slice[i]) {
//                             '\\' => blk: {
//                                 defer i += 1;
//                                 break :blk try self.escapeCharacter(
//                                     bcall.args[0],
//                                     slice[i + 1],
//                                 );
//                             },
//                             '{' => blk: {
//                                 i += 1;
//                                 if (slice[i + 1] == '{') break :blk '{';
//                                 if (scratch.items.len > 0) {
//                                     const index: u32 =
//                                         @intCast(self.string_bytes.items.len);
//
//                                     try scratch.append(self.gpa, 0);
//                                     try self.string_bytes.appendSlice(
//                                         self.gpa,
//                                         scratch.items,
//                                     );
//
//                                     scratch.clearRetainingCapacity();
//
//                                     try self.appendInstr(.{
//                                         .tag = .pushs,
//                                         .data = index,
//                                     });
//                                     try self.appendInstr(.tagOnly(.prints));
//                                 }
//
//                                 while (i < slice.len and slice[i] != '}') i += 1;
//                                 if (slice[i] != '}') return self.throw(
//                                     bcall.args[0],
//                                     "missing closing '}}' for format string",
//                                     .{},
//                                 );
//
//                                 if (arg_i == bcall.args.len) return self.throw(
//                                     node,
//                                     "missing argument for format string",
//                                     .{},
//                                 );
//
//                                 const res = try self.inspectNode(bcall.args[arg_i]);
//                                 if (res != .type) {
//                                     return self.throw(
//                                         bcall.args[arg_i],
//                                         "printing not implemented for {s}",
//                                         .{@tagName(res)},
//                                     );
//                                 }
//                                 arg_i += 1;
//
//                                 const instr_tag: Instr.Tag = switch (res.type) {
//                                     .int => .printi,
//                                     .float => .printf,
//                                     .cstr => .prints,
//                                     .nil => .printn,
//                                 };
//
//                                 try self.appendInstr(.tagOnly(instr_tag));
//
//                                 continue;
//                             },
//                             else => |b| b,
//                         };
//
//                         try scratch.append(self.gpa, byte);
//                     }
//
//                     if (scratch.items.len > 0) {
//                         const index: u32 = @intCast(self.string_bytes.items.len);
//
//                         try scratch.append(self.gpa, 0);
//                         try self.string_bytes.appendSlice(self.gpa, scratch.items);
//
//                         try self.appendInstr(.{
//                             .tag = .pushs,
//                             .data = index,
//                         });
//                         try self.appendInstr(.tagOnly(.prints));
//                     }
//
//                     return .initType(.nil);
//                 },
//                 .int => {
//                     if (bcall.args.len != 1) return self.throw(
//                         node,
//                         "expected 1 argument",
//                         .{},
//                     );
//
//                     const res = try self.inspectNode(bcall.args[0]);
//                     try self.assertType(bcall.args[0], .float, res);
//
//                     try self.appendInstr(.tagOnly(.fti));
//                     return .initType(.int);
//                 },
//                 .float => {
//                     if (bcall.args.len != 1) return self.throw(
//                         node,
//                         "expected 1 argument",
//                         .{},
//                     );
//
//                     const res = try self.inspectNode(bcall.args[0]);
//                     try self.assertType(bcall.args[0], .int, res);
//
//                     try self.appendInstr(.tagOnly(.itf));
//                     return .initType(.float);
//                 },
//             }
//         },
//         inline else => |_, tag| return self.throw(
//             node,
//             "{s} not implemenented",
//             .{@tagName(tag)},
//         ),
//     }
// }
//
// fn assertType(
//     self: *Emitter,
//     node: Ast.Node.Index,
//     expected: Type,
//     res: InspectResult,
// ) (mem.Allocator.Error || error{Emit})!void {
//     if (res == .ty) {
//         if (!res.ty.eq(expected)) return self.throw(node, "expected {s} got {s}", .{
//             @tagName(expected),
//             @tagName(res),
//         }) else return;
//     }
//
//     return self.throw(node, "expected {s} got {s}", .{
//         @tagName(expected),
//         @tagName(res),
//     });
// }
//
// fn escapeCharacter(
//     self: *Emitter,
//     node: Ast.Node.Index,
//     char: u8,
// ) (mem.Allocator.Error || error{Emit})!u8 {
//     return switch (char) {
//         'n' => '\n',
//         't' => '\t',
//         'r' => '\r',
//         '"' => '"',
//         else => return self.throw(
//             node,
//             "invalid escape character '{c}'",
//             .{char},
//         ),
//     };
// }
//
// fn addInstr(self: *Emitter, instr: Instr) mem.Allocator.Error!Instr.Index {
//     const index: Instr.Index = @intCast(self.instrs.len);
//     try self.scratch.append(self.gpa, instr);
//
//     return index;
// }
//
// fn appendInstr(self: *Emitter, instr: Instr) mem.Allocator.Error!void {
//     _ = try self.addInstr(instr);
// }
//
// fn addExtra(self: *Emitter, extra: anytype) mem.Allocator.Error!u32 {
//     const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
//     try self.extra_data.ensureUnusedCapacity(self.gpa, fields.len);
//
//     return self.addExtraAssumeCapacity(extra);
// }
//
// fn addExtraAssumeCapacity(self: *Emitter, extra: anytype) u32 {
//     const index: u32 = @intCast(self.extra_data.items.len);
//
//     const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
//     self.extra_data.items.len += fields.len;
//     self.setExtra(index, extra);
//
//     return index;
// }
//
// fn setExtra(self: *Emitter, index: u32, extra: anytype) void {
//     var i = index;
//     inline for (@typeInfo(@TypeOf(extra)).@"struct".fields) |field| {
//         if (field.type == u32) {
//             self.extra_data.items[i] = @field(extra, field.name);
//         } else if (@typeInfo(field.type) == .@"enum") {
//             self.extra_data.items[i] = @intFromEnum(@field(extra, field.name));
//         } else {
//             self.extra_data.items[i] = @bitCast(@field(extra, field.name));
//         }
//
//         i += 1;
//     }
// }
//
// fn printString(
//     self: *Emitter,
//     comptime fmt_str: []const u8,
//     args: anytype,
// ) mem.Allocator.Error!Ir.StringIndex {
//     const string: Ir.StringIndex = @enumFromInt(self.string_bytes.items.len);
//     try self.string_bytes.writer(self.gpa).print(fmt_str ++ "\x00", args);
//
//     return string;
// }
//
// fn throw(
//     self: *Emitter,
//     node: Ast.Node.Index,
//     comptime fmt_str: []const u8,
//     args: anytype,
// ) (error{Emit} || mem.Allocator.Error) {
//     const message = try self.printString(fmt_str, args);
//     try self.errors.append(self.gpa, .{
//         .message = message,
//         .node = node,
//     });
//
//     return error.Emit;
// }
