const std = @import("std");

const object = @import("object.zig");
const reader = @import("reader.zig");
const Value = @import("value.zig").Value;

/// A Module that exists in the Runtime.
const Module = @This();

name: []const u8,
references: reader.References,
constants: []Value,
functions: std.StringArrayHashMapUnmanaged(*object.FunctionObject),

pub fn init(allocator: std.mem.Allocator, module: reader.Module) !*Module {
    const ptr = try allocator.create(Module);

    ptr.name = module.name;
    ptr.references = module.references;

    ptr.constants = try allocator.alloc(Value, module.constants.len);
    for (module.constants, 0..) |constant, idx| {
        ptr.constants[idx] = try constant.init(allocator);
    }

    ptr.functions = std.StringArrayHashMapUnmanaged(*object.FunctionObject).empty;
    try ptr.functions.ensureTotalCapacity(allocator, module.functions.len);
    for (module.functions) |function| {
        const rtFunction = try function.init(allocator);
        ptr.functions.putAssumeCapacity(function.name, rtFunction);
    }

    return ptr;
}

pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
    // allocator.free(self.name);
    // allocator.free(self.references);
    allocator.free(self.constants);
    allocator.free(self.functions);

    allocator.destroy(self);
}

pub const Registry = struct {
    allocator: std.mem.Allocator,
    modules: std.StringHashMap(*Module),

    pub fn init(allocator: std.mem.Allocator) Registry {
        const modules = std.StringHashMap(*Module).init(allocator);
        return .{ .allocator = allocator, .modules = modules };
    }

    pub fn deinit(self: *Registry) void {
        self.modules.deinit();
        self.* = undefined;
    }

    pub fn insertModule(self: *Registry, module: reader.Module) !void {
        const rtModule = try Module.init(self.allocator, module);
        try self.modules.putNoClobber(rtModule.name, rtModule);
    }

    pub fn fetchModule(self: *Registry, name: []const u8) ?*Module {
        return self.modules.get(name);
    }

    pub const LinkError = error{
        unknown_module,
        unknown_function,
    };

    // TODO: fetching the modules and functions from modules is clearly a bottleneck
    // find a good way to remove this overhead by making this "link" step.
    //
    // I think the best way would be to add an indirection to the module:function names.
    // the instruction [call_fn idx] would point to a new structure that points to the
    // module:function names, the link function would change this structure to handle
    // both module and function pointers.
    pub fn linkModules(_: *Registry) LinkError!void {
        @panic("unimplemented");
    }
};
