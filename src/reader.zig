const std = @import("std");
const object = @import("object.zig");
const Value = @import("value.zig").Value;
const Opcode = @import("opcodes.zig").Opcode;
const RuntimeModule = @import("Module.zig");

pub const Module = struct {
    name: []const u8,
    references: References,
    constants: []Constant,
    functions: []Function,
};

pub const References = struct {
    modules: [][]const u8,
    functions: [][]const u8,
};

pub const Function = struct {
    name: []const u8,
    arity: u8,
    locals: u16,
    bytecode: []const u8,

    pub fn init(self: Function, allocator: std.mem.Allocator) !*object.FunctionObject {
        return try object.FunctionObject.init(allocator, self.name, self.arity, self.locals, self.bytecode);
    }
};

pub const Constant = union(enum) {
    string: ConstantString,
    number: ConstantNumber,
    integer: ConstantInteger,

    pub fn init(self: Constant, _: std.mem.Allocator) !Value {
        return switch (self) {
            .string => unreachable,
            .number => |number| Value.fromF64(@floatFromInt(number.value)),
            .integer => |integer| Value.fromInt(@intCast(integer.value)),
        };
    }
};

pub const ConstantString = struct {
    data: []u8,
};

pub const ConstantNumber = struct {
    value: u64,
};

pub const ConstantInteger = struct {
    value: u32,
};

test "allocates function" {
    var constant_pool_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer constant_pool_arena.deinit();

    const constant_pool_allocator = constant_pool_arena.allocator();

    const function: Function = .{
        .name = @constCast("aaaa"),
        .arity = 0,
        .locals = 2,
        .bytecode = @constCast(&[_]u8{
            0x03,
            0x04,
            0x20,
            0x00,
        }),
    };
    const rtFunction = try function.init(constant_pool_allocator);

    std.debug.print("rtFunction = {any}\n", .{rtFunction});
    std.debug.print("rtFunction.name = {s}\n", .{rtFunction.name});
}
