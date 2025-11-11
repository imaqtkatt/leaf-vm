pub const StackError = error{
    stack_overflow,
    stack_underflow,
};

pub const FrameInfo = struct {
    stack_base: usize,
    local_base: usize,
};

pub fn Stack(comptime T: type, comptime N: usize) type {
    return struct {
        data: [N]T = undefined,
        ptr: usize = 0,

        // TODO: also store stack_base or inspect bytecode before run?
        local_base: usize = 0,

        const Self = @This();

        pub fn pushFrame(self: *Self, local_count: usize) StackError!FrameInfo {
            const old_local_base = self.local_base;
            const old_stack_base = self.ptr;

            const new_local_base = self.ptr;
            const new_stack_base = self.ptr + local_count;

            if (new_stack_base >= N) return StackError.stack_overflow;
            self.ptr = new_stack_base;

            self.local_base = new_local_base;

            return .{ .stack_base = old_stack_base, .local_base = old_local_base };
        }

        pub fn popFrame(self: *Self, f: FrameInfo) StackError!void {
            const retVal = try self.pop();

            self.ptr = f.stack_base;
            self.local_base = f.local_base;

            try self.push(retVal);
        }

        pub inline fn push(self: *Self, value: T) StackError!void {
            if (self.ptr >= N) return StackError.stack_overflow;

            self.data[self.ptr] = value;
            self.ptr += 1;
        }

        pub inline fn pop(self: *Self) StackError!T {
            if (self.ptr == 0) return StackError.stack_underflow;
            self.ptr -= 1;
            return self.data[self.ptr];
        }

        pub inline fn popN(self: *Self, count: usize) StackError![]T {
            if (self.ptr < count) return StackError.stack_underflow;

            const values = self.data[self.ptr - count .. self.ptr];
            self.ptr -= count;
            return values;
        }

        pub inline fn load(self: *Self, index: usize) T {
            return self.data[self.local_base + index];
        }

        pub inline fn store(self: *Self, index: usize, item: T) void {
            self.data[self.local_base + index] = item;
        }

        pub inline fn slice(self: *Self) []T {
            return self.data[0..self.ptr];
        }
    };
}

test "test stack underflow" {
    const std = @import("std");

    var stack = Stack(u32, 1024){};

    try stack.push(41);
    try stack.push(42);

    const val_42 = try stack.pop();
    try std.testing.expectEqual(val_42, 42);

    _ = try stack.pop();

    _ = stack.pop() catch |err| switch (err) {
        StackError.stack_underflow => try std.testing.expect(true),
        else => try std.testing.expect(false),
    };
}

test "test subn" {
    const std = @import("std");

    var stack = Stack(u32, 16){};

    try stack.push(8);
    try stack.push(9);
    try stack.push(10);

    const vals = try stack.popN(2);

    std.debug.print("vals = {any}\n", .{vals});

    try std.testing.expect(stack.ptr == 1);
}
