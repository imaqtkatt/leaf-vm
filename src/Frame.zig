const std = @import("std");
const Value = @import("value.zig").Value;
const Module = @import("Module.zig");
const FunctionObject = @import("object.zig").FunctionObject;
const FrameInfo = @import("stack.zig").FrameInfo;

const Frame = @This();

ip: usize,
frame_info: FrameInfo,
module: *Module,
function: *FunctionObject,
prev: ?*Frame,

pub inline fn init(allocator: std.mem.Allocator, module: *Module, function: *FunctionObject, f: FrameInfo, prev: ?*Frame) !*Frame {
    const ptr = try allocator.create(Frame);
    ptr.* = .{ .ip = 0, .frame_info = f, .module = module, .function = function, .prev = prev };
    return ptr;
}
