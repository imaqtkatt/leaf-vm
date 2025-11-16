const std = @import("std");
const Allocator = std.mem.Allocator;

const object = @import("object.zig");
const reader = @import("reader.zig");
const Value = @import("value.zig").Value;

/// A Module that exists in the Runtime.
const Module = @This();

name: []const u8,
references: reader.References,
reference_indexes: []RefIndex,
constants: []Value,
functions: std.StringArrayHashMapUnmanaged(*object.FunctionObject),

pub fn init(allocator: Allocator, module: reader.Module) !*Module {
    const ptr = try allocator.create(Module);

    ptr.name = module.name;
    ptr.references = module.references;

    ptr.reference_indexes = try allocator.alloc(RefIndex, module.reference_indexes.len);
    for (module.reference_indexes, 0..) |ref_index, idx| {
        ptr.reference_indexes[idx] = .{
            .module_idx = ref_index.module_idx,
            .module = undefined,
            .function_idx = ref_index.function_idx,
            .function = undefined,
        };
    }

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

pub const RefIndex = struct {
    module_idx: u16,
    module: *Module,

    function_idx: u16,
    function: *object.FunctionObject,
};

pub const Registry = struct {
    allocator: Allocator,
    modules: std.StringHashMap(*Module),

    pub fn init(allocator: Allocator) Registry {
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
        UnknownModule,
        UnknownFunction,
    };

    pub fn linkModules(self: *Registry) LinkError!void {
        var iter = self.modules.valueIterator();

        while (iter.next()) |entry| {
            const module = entry.*;
            const references = module.references;

            for (module.reference_indexes) |*ref_idx| {
                const module_name = references.modules[ref_idx.module_idx];
                const function_name = references.functions[ref_idx.function_idx];

                const ref_module = self.modules.get(module_name) orelse return LinkError.UnknownModule;
                ref_idx.module = ref_module;

                const ref_function = ref_module.functions.get(function_name) orelse return LinkError.UnknownFunction;
                ref_idx.function = ref_function;
            }
        }
        // @panic("unimplemented");
    }
};
