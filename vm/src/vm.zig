// The Virtual Machine compiles instructions that exist as heap objects into machine code for better
// performance.
//
// The VM in this file doesn't know how to execute instructions; it delegates that to a
// CPU-architecture-specific implementation. While the way that instructions are mapped to machine
// code differs among architectures, the data they operate on does not:
//
// - The heap contains heap objects in a format that is documented in heap.zig.
// - The data stack is a memory region that instructions such as push_word or add operate on.
// - The call stack is a memory region that is used to support nested evaluations.
//
// Essentially, the role of the *Vm struct is just to hold data. The various VM implementations then contain
// the logic that operates on that data.

const std = @import("std");
const ArrayList = std.ArrayList;
const Ally = std.mem.Allocator;
const builtin = @import("builtin");

const compiler = @import("compiler.zig");
const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Instruction = @import("instruction.zig").Instruction;
const Val = @import("value.zig");

// Depending on the system, choose a different implementation.
// The implementation should provide the following fields/functions:
// - heap: *Heap
// - init(heap: *Heap, ally: Ally) !Impl
// - deduplicate(impl: *Impl, checkpoint: Checkpoint, obj: Obj) Obj
// - garbage_collect(impl: *Impl, checkpoint: Checkpoint, keep: Obj) Obj
const Impl = switch (builtin.cpu.arch) {
    // .x86_64 => @import("vm_x86_64.zig"), // a JIT compiler
    else => @import("vm_interpreter.zig"), // an interpreter
};
const Vm = @This();

impl: Impl,

pub fn init(heap: *Heap, ally: Ally) !Vm {
    return .{ .impl = try Impl.init(heap, ally) };
}

pub fn get_heap(vm: *Vm) *Heap {
    return vm.impl.heap;
}
pub fn push(vm: *Vm, word: Word) !void {
    try vm.impl.push(word);
}
pub fn pop(vm: *Vm) Word {
    return vm.impl.pop();
}

pub fn eval(vm: *Vm, ally: Ally, common: compiler.CommonObjects, env: anytype, code: []const u8) !Val.Value {
    const check = vm.impl.heap.checkpoint();
    const fun = try compiler.code_to_lambda(ally, vm.impl.heap, common, env, code);
    const fun_ = try vm.deduplicate(check, fun);
    const result = try vm.call(Val.Lambda.from(fun_), &[_]Val.Value{});
    return result;
}

pub fn run(vm: *Vm, instructions: Obj) !void {
    try vm.impl.run(instructions);
}

pub fn call(vm: *Vm, lambda: Val.Lambda, args: []const Val.Value) !Val.Value {
    vm.impl.heap.dump_stats();
    const instructions = lambda.obj.child(1);
    const closure = lambda.obj.child(2);
    const num_params = lambda.obj.child(3).children().len;
    if (num_params != args.len) @panic("called function with wrong number of params");
    for (args) |arg| try vm.impl.push(arg.obj.address);
    try vm.impl.push(closure.address);
    try vm.impl.run(instructions);
    return .{ .obj = .{ .address = vm.impl.pop() } };
}

pub fn garbage_collect(vm: *Vm, checkpoint: Heap.Checkpoint, keep: Obj) !Obj {
    return try vm.impl.garbage_collect(checkpoint, keep);
}

pub fn deduplicate(vm: *Vm, checkpoint: Heap.Checkpoint, obj: Obj) !Obj {
    return vm.impl.deduplicate(checkpoint, obj);
}
