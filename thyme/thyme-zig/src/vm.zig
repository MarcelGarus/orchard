const std = @import("std");
const Map = std.AutoArrayHashMap;
const ArrayList = std.ArrayList;
const Ally = std.mem.Allocator;
const builtin = @import("builtin");

const compiler = @import("compiler.zig");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Instruction = @import("instruction.zig").Instruction;
const Object = @import("object.zig");

// Depending on the system, choose a different implementation.
const Impl = switch (builtin.cpu.arch) {
    .x86_64 => @import("vm_x86_64.zig"), // a JIT compiler
    else => @import("vm_interpreter.zig"), // an interpreter
};

state: Impl,

const Vm = @This();

pub fn init(heap: *Heap, ally: Ally) !Vm {
    return .{ .state = try Impl.init(heap, ally) };
}

pub fn eval(vm: *Vm, ally: Ally, env: anytype, code: []const u8) !Object {
    const fun = try compiler.code_to_fun(ally, vm.heap, env, code);
    return try vm.call(ally, fun, .{});
}

pub fn call(vm: *Vm, ally: Ally, fun: Object, args: []const Object) !Object {
    std.debug.print("calling function {f}\n", .{fun});

    if (fun.num_params() != args.len)
        @panic("called function with wrong number of params");

    return Impl.call(&vm.state, ally, fun, args);
}
