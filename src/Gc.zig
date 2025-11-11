const std = @import("std");

const object = @import("object.zig");
const ObjectHeader = object.ObjectHeader;
const ArrayObject = object.ArrayObject;
const FunctionObject = object.FunctionObject;
const Value = @import("value.zig").Value;

var debug_runtime_allocator = std.heap.DebugAllocator(.{}).init;
const dbg_runtime_allocator = debug_runtime_allocator.allocator();

const default_gc_threshold: usize = 1024 * 1024;

const Gc = @This();

allocator: std.mem.Allocator,
gc_head: ?*ObjectHeader,

allocated_bytes: usize,
gc_threshold: usize,

pub var default: Gc = .{
    .allocated_bytes = 0,
    .gc_head = null,
    .gc_threshold = default_gc_threshold,
    .allocator = std.heap.c_allocator,
};

pub var debug: Gc = .{
    .allocated_bytes = 0,
    .gc_head = null,
    .gc_threshold = default_gc_threshold,
    .allocator = dbg_runtime_allocator,
};

pub fn init(allocator: std.mem.Allocator) Gc {
    return .{
        .allocated_bytes = 0,
        .gc_head = null,
        .gc_threshold = default_gc_threshold,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Gc) void {
    self.sweep();
    self.* = undefined;
}

pub fn deinitDebug(self: *Gc) void {
    self.sweep();
    if (debug_runtime_allocator.deinit() == .leak) @panic("gc leak");
    self.* = undefined;
}

// TODO: make this non-recursive
pub fn mark(roots: std.ArrayList(Value)) void {
    for (roots.items) |value| {
        switch (value.tag()) {
            .num => {},
            .int => {},
            .nil => {},
            else => markValue(value),
        }
    }
}

fn markValue(value: Value) void {
    const header = value.asHeader();
    if (header.marked.swap(true, .seq_cst)) return;
    traceValue(value);
}

fn traceValue(value: Value) void {
    const header = value.asHeader();

    switch (header.object.*) {
        .array => |arr| for (arr.data) |item| markValue(item),
        .function => {},
        .partial => |par| for (par.applied_values) |v| markValue(v),
        .string => {},
    }
}

pub fn sweep(self: *Gc) void {
    var prev: ?*ObjectHeader = null;
    var current = self.gc_head;

    while (current) |header| {
        if (header.marked.swap(false, .seq_cst)) {
            prev = header;
            current = header.next;
        } else {
            const next = header.next;
            header.deinit(self.allocator);
            if (prev) |p| p.next = next else self.gc_head = next;
            current = next;
        }
    }
}

pub fn allocArray(self: *Gc, len: u32) !Value {
    const ptr = try ArrayObject.init(self.allocator, len);
    const header = try ObjectHeader.init(self.allocator, ptr);

    self.allocated_bytes += @sizeOf(ObjectHeader) + @sizeOf(ArrayObject) + (@sizeOf(Value) * len);

    header.next = self.gc_head;
    self.gc_head = header;

    return Value.fromHeap(header);
}

pub fn allocPartial(self: *Gc, function: *FunctionObject, applied: []Value) !Value {
    const ptr = try object.PartialObject.init(self.allocator, function, applied);
    const header = try ObjectHeader.init(self.allocator, ptr);

    self.allocated_bytes += @sizeOf(ObjectHeader) + @sizeOf(object.PartialObject);

    header.next = self.gc_head;
    self.gc_head = header;

    return Value.fromPartial(header);
}

test "allocates array" {
    const allocator = std.testing.allocator;

    var gc = Gc.init(allocator);
    defer gc.deinit();

    const array = try gc.allocArray(5);

    // try std.testing.expect(!array.isImmediate());

    const header = array.asHeader();
    switch (header.object.*) {
        .array => |arr| try std.testing.expect(arr.len == 5),
        else => try std.testing.expect(false),
    }

    // gc.deinit();
}
