const std = @import("std");
const builtin = @import("builtin");

const leaf = @import("leaf");

const Gc = @import("Gc.zig");
const Module = @import("Module.zig");
const opcodes = @import("opcodes.zig");
const reader = @import("reader.zig");
const Function = reader.Function;
const Constant = reader.Constant;
const Runtime = @import("runtime.zig");
const Stack = @import("stack.zig").Stack;
const Value = @import("value.zig").Value;

pub fn main() !void {
    // Prints to stderr, ignoring potential errors.
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    // try leaf.bufferedPrint();

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var gpa = std.heap.DebugAllocator(.{}).init;
    const gpa_allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) @panic("leak");

    // Constants, Modules and Functions live within the constant_pool_arena
    var constant_pool_arena = std.heap.ArenaAllocator.init(gpa_allocator);
    const constant_pool_allocator = constant_pool_arena.allocator();
    defer constant_pool_arena.deinit();

    const gc = if (builtin.mode == .Debug) &Gc.debug else &Gc.default;
    defer if (builtin.mode == .Debug) gc.deinitDebug() else gc.deinit();

    var registry = Module.Registry.init(constant_pool_allocator);

    try registry.insertModule(moduleExample());
    defer registry.deinit();

    try registry.linkModules();

    const runtime = try Runtime.boot(gc, &registry, "main");
    defer runtime.deinit();

    try runtime.run();

    try runtime.debugStack(stdout);
    _ = try stdout.writeByte('\n');
    try stdout.flush();
}

fn moduleExample() reader.Module {
    return .{
        .name = "main",
        .references = .{
            .modules = @constCast(&[_][]const u8{
                "main",
            }),
            .functions = @constCast(&[_][]const u8{
                "main",
                "fib",
            }),
        },
        .reference_indexes = @constCast(&[_]reader.RefIndex{
            reader.RefIndex{ .module_idx = 0, .function_idx = 1 },
        }),
        .constants = @constCast(&[_]Constant{
            Constant{ .integer = .{ .value = 35 } },
            Constant{ .integer = .{ .value = 2 } },
        }),
        .functions = @constCast(&[_]Function{
            Function{
                .name = "main",
                .arity = 0,
                .locals = 0,
                .bytecode = @constCast(&[_]u8{
                    opcodes.load_const,
                    0x00,
                    0x00,
                    opcodes.call_self,
                    0x00,
                    0x01,
                    opcodes.ret,
                }),
            },
            Function{
                .name = "fib",
                .arity = 1,
                .locals = 1,
                .bytecode = @constCast(&[_]u8{
                    opcodes.load_0,
                    opcodes.i_const_1,
                    opcodes.cmp_gt,
                    opcodes.if_zero,
                    0x00,
                    0x16,
                    //
                    opcodes.load_0,
                    opcodes.i_const_1,
                    opcodes.sub,
                    opcodes.call_self,
                    0x00,
                    0x01,
                    opcodes.load_0,
                    opcodes.load_const,
                    0x00,
                    0x01,
                    opcodes.sub,
                    opcodes.call_self,
                    0x00,
                    0x01,
                    opcodes.add,
                    opcodes.ret,
                    //
                    opcodes.load_0,
                    opcodes.ret,
                }),
            },
        }),
    };
}

test "simple test" {
    const gpa = std.testing.allocator;
    var list: std.ArrayList(i32) = .empty;
    defer list.deinit(gpa); // Try commenting this out and see if zig detects the memory leak!
    try list.append(gpa, 42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
