const std = @import("std");

const Allocator = std.mem.Allocator;

const object = @import("object.zig");
const Value = @import("value.zig").Value;
const Opcode = @import("opcodes.zig").Opcode;
const RuntimeModule = @import("Module.zig");

pub const Module = struct {
    name: []const u8,
    references: References,
    reference_indexes: []RefIndex,
    constants: []Constant,
    functions: []Function,

    pub fn read(_: *std.Io.Reader, _: Allocator) !Module {
        @panic("unimplemented");
    }
};

pub const References = struct {
    modules: [][]const u8,
    functions: [][]const u8,

    pub fn read(_: *std.Io.Reader, _: Allocator) !References {
        @panic("unimplemented");
    }
};

pub const RefIndex = struct {
    module_idx: u16,
    function_idx: u16,
};

pub const Function = struct {
    name: []const u8,
    arity: u8,
    locals: u16,
    bytecode: []const u8,

    pub fn init(self: Function, allocator: Allocator) !*object.FunctionObject {
        return try object.FunctionObject.init(allocator, self.name, self.arity, self.locals, self.bytecode);
    }

    pub fn read(_: *std.Io.Reader, _: Allocator) !Function {
        @panic("unimplemented");
    }
};

pub const Constant = union(enum) {
    string: ConstantString,
    number: ConstantNumber,
    integer: ConstantInteger,

    pub fn init(self: Constant, _: Allocator) !Value {
        return switch (self) {
            .string => unreachable,
            .number => |number| Value.fromF64(@floatFromInt(number.value)),
            .integer => |integer| Value.fromInt(@intCast(integer.value)),
        };
    }

    pub fn read(reader: *std.Io.Reader, allocator: Allocator) !Constant {
        const tag = try reader.takeByte();
        switch (tag) {
            0x1 => return .{ .string = try ConstantString.read(reader, allocator) },
            0x2 => return .{ .number = try ConstantNumber.read(reader) },
            0x3 => return .{ .integer = try ConstantInteger.read(reader) },
            else => @panic("unexpected tag"),
        }
    }
};

pub const ConstantString = struct {
    data: []u8,

    pub fn read(reader: *std.Io.Reader, allocator: Allocator) !ConstantString {
        const data = try readLengthPrefixedString(reader, allocator);
        return .{ .data = data };
    }
};

pub const ConstantNumber = struct {
    value: u64,

    pub fn read(reader: *std.Io.Reader) !ConstantNumber {
        const data = try reader.takeInt(u64, .big);
        return .{ .value = data };
    }
};

pub const ConstantInteger = struct {
    value: u32,

    pub fn read(reader: *std.Io.Reader) !ConstantInteger {
        const data = try reader.takeInt(u32, .big);
        return .{ .value = data };
    }
};

fn readLengthPrefixedString(reader: *std.Io.Reader, allocator: Allocator) ![]u8 {
    const len = try reader.takeInt(u32, .big);
    const data = try reader.readAlloc(allocator, len);
    return data;
}

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

test "read string constant" {
    const child_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(child_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const buf = "\x01\x00\x00\x00\x05hello";
    var reader = std.Io.Reader.fixed(buf);

    const value = try Constant.read(&reader, allocator);

    switch (value) {
        .string => |s| {
            try std.testing.expectEqual(5, s.data.len);
            // TODO: how?
            // try std.testing.expectEqual("hello", s.data);
        },
        else => try std.testing.expect(false),
    }
}

test "read number constant" {
    const allocator = std.testing.allocator;

    const buf = "\x02\x40\x45\x00\x00\x00\x00\x00\x00";
    var reader = std.Io.Reader.fixed(buf);

    const value = try Constant.read(&reader, allocator);

    switch (value) {
        .number => |num| {
            const float: f64 = @bitCast(num.value);
            try std.testing.expectEqual(42.0, float);
        },
        else => try std.testing.expect(false),
    }
}

test "read int constant" {
    const allocator = std.testing.allocator;

    const buf = "\x03\x00\x00\x10\x00";
    var reader = std.Io.Reader.fixed(buf);

    const value = try Constant.read(&reader, allocator);

    switch (value) {
        .integer => |int| {
            const i: i32 = @bitCast(int.value);
            try std.testing.expectEqual(0x1000, i);
        },
        else => try std.testing.expect(false),
    }
}
