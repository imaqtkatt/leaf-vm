const std = @import("std");

const Frame = @import("Frame.zig");
const Gc = @import("Gc.zig");
const Module = @import("Module.zig");
const Registry = Module.Registry;
const Stack = @import("stack.zig").Stack;
const Value = @import("value.zig").Value;
const object = @import("object.zig");
const opcodes = @import("opcodes.zig");

const MAX_STACK: usize = 4096;
const MAX_FRAMES: usize = 1024;

// TODO: change it to a fixed Frame buffer
var frame_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const frame_allocator = frame_arena.allocator();

gc: *Gc,
registry: *Registry,
stack: Stack(Value, MAX_STACK),
frame: ?*Frame,

pub const RuntimeError = error{
    type_error,
    arity_error,
};

const Runtime = @This();

pub inline fn boot(gc: *Gc, registry: *Registry, mainModule: []const u8) !*Runtime {
    const ptr = try gc.allocator.create(Runtime);
    ptr.gc = gc;
    ptr.registry = registry;
    ptr.stack = Stack(Value, MAX_STACK){};

    const main = ptr.registry.fetchModule(mainModule) orelse unreachable;
    const mainFn = main.functions.get("main") orelse unreachable;
    if (mainFn.arity > 0) return RuntimeError.arity_error;
    try ptr.pushFrame(main, mainFn, null);

    return ptr;
}

pub fn deinit(self: *Runtime) void {
    frame_arena.deinit();
    self.gc.allocator.destroy(self);
}

inline fn pushFrame(self: *Runtime, module: *Module, function: *object.FunctionObject, prev: ?*Frame) anyerror!void {
    const frame_info = try self.stack.pushFrame(function.locals);
    self.frame = try Frame.init(frame_allocator, module, function, frame_info, prev);
}

pub inline fn run(self: *Runtime) anyerror!void {
    while (self.frame) |frame| {
        const bytecode = frame.function.bytecode[frame.ip];
        frame.ip += 1;

        switch (bytecode) {
            opcodes.ret => {
                try self.stack.popFrame(frame.frame_info);
                self.frame = frame.prev;
            },
            opcodes.const_0 => try self.stack.push(Value.CONST_0),
            opcodes.const_1 => try self.stack.push(Value.CONST_1),
            opcodes.i_const_0 => try self.stack.push(Value.I_CONST_0),
            opcodes.i_const_1 => try self.stack.push(Value.I_CONST_1),
            opcodes.load_const => {
                const index = self.read_u16();
                const constant = frame.module.constants[index];
                try self.stack.push(constant);
            },
            opcodes.load_fn => {
                const index = self.read_u16();
                const fn_name = frame.module.references.functions[index];
                const fun = frame.module.functions.get(fn_name) orelse unreachable;
                try self.stack.push(Value.fromFunction(fun));
            },
            opcodes.load_0 => try self.stack.push(self.stack.load(0)),
            opcodes.load_1 => try self.stack.push(self.stack.load(1)),
            opcodes.load_2 => try self.stack.push(self.stack.load(2)),
            opcodes.load_3 => try self.stack.push(self.stack.load(3)),
            opcodes.load_n => {
                const index = self.read_u16();
                try self.stack.push(self.stack.load(index));
            },
            opcodes.store_0 => self.stack.store(0, try self.stack.pop()),
            opcodes.store_1 => self.stack.store(1, try self.stack.pop()),
            opcodes.store_2 => self.stack.store(2, try self.stack.pop()),
            opcodes.store_3 => self.stack.store(3, try self.stack.pop()),
            opcodes.store_n => {
                const index = self.read_u16();
                self.stack.store(index, try self.stack.pop());
            },
            opcodes.discard => _ = try self.stack.pop(),
            opcodes.dup => {
                const value = try self.stack.pop();
                try self.stack.push(value);
                try self.stack.push(value);
            },
            opcodes.add => {
                const rhs = try self.stack.pop();
                const lhs = try self.stack.pop();

                if (lhs.isNumber() and rhs.isNumber()) {
                    const result = lhs.asF64() + rhs.asF64();
                    try self.stack.push(Value.fromF64(result));
                } else if (lhs.isInt() and rhs.isInt()) {
                    const result = lhs.asInt() + rhs.asInt();
                    try self.stack.push(Value.fromInt(result));
                } else {
                    return RuntimeError.type_error;
                }
            },
            opcodes.sub => {
                const rhs = try self.stack.pop();
                const lhs = try self.stack.pop();

                if (lhs.isNumber() and rhs.isNumber()) {
                    const result = lhs.asF64() - rhs.asF64();
                    try self.stack.push(Value.fromF64(result));
                } else if (lhs.isInt() and rhs.isInt()) {
                    const result = lhs.asInt() - rhs.asInt();
                    try self.stack.push(Value.fromInt(result));
                } else {
                    return RuntimeError.type_error;
                }
            },
            opcodes.mul => @panic("unimplemented"),
            opcodes.div => @panic("unimplemented"),
            opcodes.n_to_i => {
                const value = try self.stack.pop();
                if (!value.isNumber()) return RuntimeError.type_error;
                const float = value.asF64();
                const raw_int: i32 = @intFromFloat(float);
                try self.stack.push(Value.fromInt(raw_int));
            },

            opcodes.i_add => try self.integerArith(.add),
            opcodes.i_sub => try self.integerArith(.sub),
            opcodes.i_mul => try self.integerArith(.mul),
            opcodes.i_div => try self.integerArith(.div),
            opcodes.i_to_n => {
                const val = try self.stack.pop();
                if (!val.isInt()) return RuntimeError.type_error;
                const asFloat: f64 = @floatFromInt(val.asInt());
                try self.stack.push(Value.fromF64(asFloat));
            },
            opcodes.i_shl => @panic("unimplemented"),
            opcodes.i_shr => @panic("unimplemented"),
            opcodes.i_ushr => @panic("unimplemented"),
            opcodes.i_neg => {
                const b = try self.stack.pop();
                if (!b.isInt()) return RuntimeError.type_error;
            },
            opcodes.i_and => try self.integerLogical(.@"and"),
            opcodes.i_or => try self.integerLogical(.@"or"),
            opcodes.i_xor => try self.integerLogical(.xor),
            opcodes.i_rem => try self.integerArith(.rem),
            opcodes.call_fn => {
                const module_idx = self.read_u16();
                const function_idx = self.read_u16();
                const refs = frame.module.references;

                const module_name = refs.modules[module_idx];
                const function_name = refs.functions[function_idx];

                const module = self.registry.fetchModule(module_name) orelse unreachable;
                const fun = module.functions.get(function_name) orelse unreachable;

                const args = try self.stack.popN(fun.arity);

                try self.pushFrame(module, fun, frame);

                for (0..fun.arity) |i| self.stack.store(i, args[i]);
            },
            opcodes.call_self => {
                const function_idx = self.read_u16();
                const function_name = frame.module.references.functions[function_idx];

                const fun = frame.module.functions.get(function_name) orelse unreachable;

                const args = try self.stack.popN(fun.arity);

                try self.pushFrame(frame.module, fun, frame);

                for (0..fun.arity) |i| self.stack.store(i, args[i]);
            },
            opcodes.apply => {
                const maybeFun = try self.stack.pop();

                if (maybeFun.isPartial()) {
                    _ = self.read_u16();
                    @panic("unimplemented"); // TODO: implement
                } else if (maybeFun.isFunction()) {
                    @panic("unimplemented");
                } else {
                    return RuntimeError.type_error;
                }
            },
            opcodes.if_zero => {
                const value = try self.stack.pop();
                if (!value.asBool()) frame.ip = self.read_u16() else frame.ip += 2;
            },
            opcodes.if_nil => {
                const value = try self.stack.pop();
                if (value.isNil()) frame.ip = self.read_u16() else frame.ip += 2;
            },
            opcodes.if_not_nil => {
                const value = try self.stack.pop();
                if (!value.isNil()) frame.ip = self.read_u16() else frame.ip += 2;
            },
            opcodes.recur => {
                const argc = frame.function.arity;
                const args = try self.stack.popN(argc);

                try self.pushFrame(frame.module, frame.function, frame);

                for (0..argc) |i| self.stack.store(i, args[i]);
            },
            opcodes.cmp_eq => {
                const rhs = try self.stack.pop();
                const lhs = try self.stack.pop();

                if (lhs.tag() != rhs.tag()) return RuntimeError.type_error;

                const cmp = lhs.equal(rhs);
                try self.stack.push(Value.fromBool(cmp));
            },
            opcodes.cmp_gt => {
                const rhs = try self.stack.pop();
                const lhs = try self.stack.pop();

                if (lhs.isNumber() and rhs.isNumber()) {
                    const value = lhs.asF64() > rhs.asF64();
                    try self.stack.push(Value.fromBool(value));
                } else if (lhs.isInt() and rhs.isInt()) {
                    const value = lhs.asInt() > rhs.asInt();
                    try self.stack.push(Value.fromBool(value));
                } else {
                    return RuntimeError.type_error;
                }
            },
            opcodes.cmp_lt => {
                const rhs = try self.stack.pop();
                const lhs = try self.stack.pop();

                if (lhs.isNumber() and rhs.isNumber()) {
                    const value = lhs.asF64() < rhs.asF64();
                    try self.stack.push(Value.fromBool(value));
                } else if (lhs.isInt() and rhs.isInt()) {
                    const value = lhs.asInt() < rhs.asInt();
                    try self.stack.push(Value.fromBool(value));
                } else {
                    return RuntimeError.type_error;
                }
            },
            opcodes.and_ => {
                const rhs = try self.stack.pop();
                const lhs = try self.stack.pop();

                const value = lhs.asBool() and rhs.asBool();
                try self.stack.push(Value.fromBool(value));
            },
            opcodes.or_ => {
                const rhs = try self.stack.pop();
                const lhs = try self.stack.pop();

                const value = lhs.asBool() or rhs.asBool();
                try self.stack.push(Value.fromBool(value));
            },
            else => @panic("unimplemented"),
        }
    }
}

const integerArithType = enum {
    add,
    sub,
    mul,
    div,
    rem,
};

fn integerArith(self: *Runtime, comptime T: integerArithType) anyerror!void {
    const rhs = try self.stack.pop();
    if (!rhs.isInt()) return RuntimeError.type_error;
    const lhs = try self.stack.pop();
    if (!lhs.isInt()) return RuntimeError.type_error;

    const result = switch (T) {
        .add => lhs.asInt() +% rhs.asInt(),
        .sub => lhs.asInt() -% rhs.asInt(),
        .mul => lhs.asInt() *% rhs.asInt(),
        .div => @divTrunc(lhs.asInt(), rhs.asInt()),
        .rem => @rem(lhs.asInt(), rhs.asInt()),
    };

    try self.stack.push(Value.fromInt(result));
}

const integerLogicalType = enum {
    @"and",
    @"or",
    xor,
};

fn integerLogical(self: *Runtime, comptime T: integerLogicalType) anyerror!void {
    const rhs = try self.stack.pop();
    if (!rhs.isInt()) return RuntimeError.type_error;
    const lhs = try self.stack.pop();
    if (!lhs.isInt()) return RuntimeError.type_error;

    const result = switch (T) {
        .@"and" => lhs.asInt() & rhs.asInt(),
        .@"or" => lhs.asInt() | rhs.asInt(),
        .xor => lhs.asInt() ^ rhs.asInt(),
    };

    try self.stack.push(Value.fromInt(result));
}

fn read_u16(self: *Runtime) u16 {
    const frame = self.frame orelse unreachable;
    const byte_0 = @as(u16, frame.function.bytecode[frame.ip]);
    const byte_1 = @as(u16, frame.function.bytecode[frame.ip + 1]);
    frame.ip += 2;

    return (byte_0 << 8) | byte_1;
}

fn applyFunction(self: *Runtime, fun: *object.FunctionObject, argc: u16) anyerror!void {
    if (fun.arity == argc) {
        const args = try self.stack.popN(fun.arity);

        try self.pushFrame(fun, self.frame);

        for (0..fun.arity) |i| self.stack.store(i, args[i]);
    } else if (fun.arity < argc) {
        const args = try self.stack.popN(argc);
        const partial = try self.gc.allocPartial(fun, args);
        try self.stack.push(partial);
    } else {
        return RuntimeError.arity_error;
    }
}

fn applyPartial(self: *Runtime, partial: *object.PartialObject, argc: u16) anyerror!void {
    const function = partial.function;
    const arity = function.arity;
    const applied = partial.applied_values.len;
    const new_argc = applied + argc;

    if (arity == new_argc) {
        const args = try self.stack.popN(argc);

        const new_frame = try Frame.init(self.gc.allocator, self.frame, function, function.locals);

        for (0..applied) |i| self.stack.store(i, partial.applied_values[i]);
        for (applied..new_argc) |i| self.stack.store(i, args[i]);

        self.frame = new_frame;
    } else if (arity < new_argc) {
        @panic("unimplemented");
    } else {
        return RuntimeError.arity_error;
    }
}

pub fn debugStack(self: *Runtime, writer: *std.Io.Writer) !void {
    for (self.stack.slice()) |item| {
        try item.format(writer);
        try writer.flush();
    }
}

test "run function" {
    const reader = @import("reader.zig");
    const Function = reader.Function;
    const Constant = reader.Constant;

    var constant_pool_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const constant_pool_allocator = constant_pool_arena.allocator();
    defer constant_pool_arena.deinit();

    var gc = Gc.init(std.testing.allocator);
    defer gc.deinit();

    const mainFn: Function = .{
        .name = "main",
        .arity = 0,
        .locals = 0,
        .bytecode = &[_]u8{
            opcodes.i_const_1,
            opcodes.i_const_1,
            opcodes.i_add,
            //
            opcodes.ret,
        },
    };

    const mainModule: reader.Module = .{
        .name = "main",
        .references = .{
            .modules = @constCast(&[_][]const u8{}),
            .functions = @constCast(&[_][]const u8{}),
        },
        .constants = @constCast(&[_]Constant{}),
        .functions = @constCast(&[_]Function{mainFn}),
    };

    var registry = Registry.init(constant_pool_allocator);
    defer registry.deinit();
    try registry.insertModule(mainModule);

    const rt = try Runtime.boot(&gc, &registry, "main");
    defer rt.deinit();

    rt.run() catch |e| switch (e) {
        RuntimeError.type_error => std.debug.print("type_error\n", .{}),
        else => std.debug.print("unknown error", .{}),
    };

    for (rt.stack.slice()) |item| {
        std.debug.print("result = {any}\n", .{item.asInt()});
    }
}
