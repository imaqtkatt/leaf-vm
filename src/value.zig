const std = @import("std");

const object = @import("object.zig");
const ObjectHeader = object.ObjectHeader;
const FunctionObject = object.FunctionObject;
const PartialObject = object.PartialObject;

const QNAN: u64 = 0x7FF8_0000_0000_0000;

const TAG_MASK: u64 = 0x7FFF_0000_0000_0000;
const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;
const INT_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

const TAG_INT: u64 = 0x7FF9_0000_0000_0000;
const TAG_NIL: u64 = 0x7FFA_0000_0000_0000;
const TAG_FUN: u64 = 0x7FFB_0000_0000_0000;
const TAG_PAR: u64 = 0x7FFC_0000_0000_0000;
const TAG_OBJ: u64 = 0x7FFD_0000_0000_0000;

pub const ValueTag = enum(u64) {
    num,

    int = TAG_INT,
    nil = TAG_NIL,
    fun = TAG_FUN,
    par = TAG_PAR,
    obj = TAG_OBJ,
};

/// A nan-boxed value that handles immediates and heap allocated objects.
pub const Value = struct {
    raw: u64,

    pub const FALSE: Value = .{ .raw = TAG_INT | 0 };
    pub const TRUE: Value = .{ .raw = TAG_INT | 1 };
    pub const NIL: Value = .{ .raw = TAG_NIL };
    pub const CONST_0: Value = Value.fromF64(0.0);
    pub const CONST_1: Value = Value.fromF64(1.0);
    pub const I_CONST_0: Value = Value.fromInt(0);
    pub const I_CONST_1: Value = Value.fromInt(1);
    pub const I_CONST_n1: Value = Value.fromInt(-1);

    pub inline fn tag(self: Value) ValueTag {
        return if (self.isNumber()) .num else @enumFromInt(self.raw & TAG_MASK);
    }

    pub inline fn fromF64(value: f64) Value {
        return .{ .raw = @bitCast(value) };
    }

    pub inline fn asF64(self: Value) f64 {
        return @bitCast(self.raw);
    }

    pub inline fn isNumber(self: Value) bool {
        return self.raw & QNAN != QNAN;
    }

    pub inline fn fromInt(value: i32) Value {
        return .{ .raw = TAG_INT | @as(u64, @as(u32, @bitCast(value))) };
    }

    pub inline fn asInt(self: Value) i32 {
        return @bitCast(@as(u32, @truncate(self.raw & INT_MASK)));
    }

    pub inline fn isInt(self: Value) bool {
        return self.raw & TAG_MASK == TAG_INT;
    }

    pub inline fn fromBool(value: bool) Value {
        return if (value) TRUE else FALSE;
    }

    /// Coerces a Value to a bool.
    /// nil => false
    /// 0 | 0.0 => false
    /// heap => true
    pub inline fn asBool(self: Value) bool {
        return switch (self.tag()) {
            .num => self.asF64() != 0.0,
            .int => self.asInt() != 0,
            .nil => false,
            .fun => true,
            .par => true,
            .obj => true,
        };
    }

    pub inline fn isNil(self: Value) bool {
        return self.raw & TAG_MASK == TAG_NIL;
    }

    // don't use ObjectHeader since functions outlive the runtime.
    pub inline fn fromFunction(function: *FunctionObject) Value {
        return .{ .raw = TAG_FUN | (@intFromPtr(function) & PAYLOAD_MASK) };
    }

    pub inline fn isFunction(self: Value) bool {
        return self.raw & TAG_FUN == TAG_FUN;
    }

    pub inline fn asFunction(self: Value) *FunctionObject {
        return @ptrFromInt(self.raw & PAYLOAD_MASK);
    }

    pub inline fn fromPartial(partial: *ObjectHeader) Value {
        return .{ .raw = TAG_PAR | (@intFromPtr(partial) & PAYLOAD_MASK) };
    }

    pub inline fn isPartial(self: Value) bool {
        return self.raw & TAG_PAR == TAG_PAR;
    }

    pub inline fn asPartial(self: Value) *PartialObject {
        const header: *ObjectHeader = @ptrFromInt(self.raw & PAYLOAD_MASK);
        return header.object.partial;
    }

    pub inline fn isObject(self: Value) bool {
        return self.raw & TAG_MASK == TAG_OBJ;
    }

    pub inline fn fromHeap(header: *ObjectHeader) Value {
        return .{ .raw = TAG_OBJ | (@intFromPtr(header) & PAYLOAD_MASK) };
    }

    pub inline fn asHeader(self: Value) *ObjectHeader {
        return @ptrFromInt(self.raw & PAYLOAD_MASK);
    }

    pub fn format(self: Value, writer: *std.Io.Writer) !void {
        switch (self.tag()) {
            .num => try writer.printFloat(self.asF64(), .{}),
            .int => try writer.printInt(self.asInt(), 10, std.fmt.Case.lower, .{}),
            .nil => try writer.printAscii("nil", .{}),
            .fun => try writer.print("<fun:{*}>", .{self.asFunction()}),
            .par => try writer.print("<par:{*}>", .{self.asPartial()}),
            .obj => try writer.print("<obj:{*}>", .{self.asHeader()}),
        }
    }

    pub fn equal(self: Value, other: Value) bool {
        return switch (self.tag()) {
            .num => if (other.isNumber()) self.asF64() == other.asF64() else false,
            .int => if (other.isInt()) self.asInt() == other.asInt() else false,
            .nil => other.isNil(),
            .fun => if (other.isFunction()) self.asFunction() == other.asFunction() else false,
            .par => if (other.isPartial()) self.asPartial() == other.asPartial() else false,
            .obj => if (other.isObject()) self.asHeader().object == other.asHeader().object else false,
        };
    }
};

test "immediate values" {
    const v14_n = Value.fromF64(41.25);

    try std.testing.expect(v14_n.isNumber());
    try std.testing.expect(!v14_n.isInt());
    try std.testing.expectEqual(v14_n.asF64(), 41.25);

    const v14_i = Value.fromInt(42);

    try std.testing.expect(v14_i.isInt());
    try std.testing.expectEqual(v14_i.asInt(), 42);

    const vTrue = Value.fromBool(true);

    try std.testing.expect(vTrue.isInt());
    try std.testing.expectEqual(vTrue.asBool(), true);

    const vFalse = Value.fromBool(false);

    try std.testing.expect(vFalse.isInt());
    try std.testing.expectEqual(vFalse.asBool(), false);

    const vFalseNumber = Value.fromF64(0.0);
    try std.testing.expectEqual(vFalseNumber.asBool(), false);
}
