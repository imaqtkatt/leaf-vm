const std = @import("std");

const Allocator = std.mem.Allocator;

const value = @import("value.zig");
const Value = value.Value;
const ValueTag = value.ValueTag;

pub const ObjectHeader = struct {
    marked: std.atomic.Value(bool),
    object: *Object,
    next: ?*ObjectHeader,

    pub fn init(allocator: Allocator, object: *Object) !*ObjectHeader {
        const ptr = try allocator.create(ObjectHeader);
        ptr.* = .{
            .marked = std.atomic.Value(bool).init(false),
            .object = object,
            .next = null,
        };
        return ptr;
    }

    pub fn deinit(self: *ObjectHeader, allocator: Allocator) void {
        self.object.deinit(allocator);
        allocator.destroy(self);
    }
};

pub const Object = union(enum) {
    array: *ArrayObject,
    string: *StringObject,
    function: *FunctionObject,
    partial: *PartialObject,

    pub fn deinit(self: *Object, allocator: Allocator) void {
        switch (self.*) {
            .array => |a| a.deinit(allocator),
            .string => |s| s.deinit(allocator),
            .function => |f| f.deinit(allocator),
            .partial => |p| p.deinit(allocator),
        }
        allocator.destroy(self);
    }
};

/// An Array with fixed length.
pub const ArrayObject = struct {
    len: u32,
    data: []Value,

    pub fn init(allocator: Allocator, len: u32) !*Object {
        const object = try allocator.create(Object);
        const array_object = try allocator.create(ArrayObject);

        array_object.data = try allocator.alloc(Value, len);
        array_object.len = len;
        @memset(array_object.data, Value.NIL);

        object.* = .{ .array = array_object };
        return object;
    }

    pub fn deinit(self: *ArrayObject, allocator: Allocator) void {
        allocator.free(self.data);
        allocator.destroy(self);
    }
};

/// A String object, constant or dinamically created.
pub const StringObject = struct {
    len: u32,
    constant: bool,
    data: []u8,

    pub fn init(allocator: Allocator, len: u32, constant: bool, data: []u8) !*StringObject {
        const ptr = try allocator.create(StringObject);
        ptr.len = len;
        ptr.constant = constant;
        ptr.data = try allocator.dupe(u8, data);
        return ptr;
    }

    pub fn deinit(self: *StringObject, allocator: Allocator) void {
        if (!self.constant) allocator.free(self.data);
        allocator.destroy(self);
    }
};

/// A Function that exists in the Runtime.
pub const FunctionObject = struct {
    name: []const u8,
    arity: u8,
    locals: u16,
    bytecode: []const u8,

    // name and code must be already allocated somewhere
    pub fn init(allocator: Allocator, name: []const u8, arity: u8, locals: u16, code: []const u8) !*FunctionObject {
        const ptr = try allocator.create(FunctionObject);
        ptr.* = .{ .name = name, .arity = arity, .locals = locals, .bytecode = code };
        return ptr;
    }

    pub fn deinit(self: *FunctionObject, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

/// A Function with partially applied arguments.
pub const PartialObject = struct {
    function: *FunctionObject,
    applied_values: []Value,

    pub fn init(allocator: Allocator, function: *FunctionObject, applied_values: []Value) !*Object {
        const object = try allocator.create(Object);
        const ptr = try allocator.create(PartialObject);

        ptr.function = function;
        ptr.applied_values = applied_values;

        object.* = .{ .partial = ptr };
        return object;
    }

    pub fn deinit(self: *PartialObject, allocator: Allocator) void {
        allocator.free(self.applied_values);
        allocator.destroy(self);
    }
};

test "allocate array object" {
    const allocator = std.testing.allocator;
    const array_len = 5;

    const arr = try ArrayObject.init(allocator, array_len);

    for (arr.array.data) |v| try std.testing.expect(v.isNil());

    const arr_header = try ObjectHeader.init(allocator, arr);
    defer arr_header.deinit(allocator);

    const arr_value = Value.fromHeap(arr_header);
    try std.testing.expectEqual(arr_value.tag(), ValueTag.obj);
}
