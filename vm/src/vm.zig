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

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Instruction = @import("instruction.zig").Instruction;

// Depending on the system, choose a different implementation.
// The implementation should provide the following fields/functions:
// - init(heap: *Heap, ally: Ally) !Impl
// - heap: *Heap
// - push(impl: *Impl, ally: Word) !void
// - pop(impl: *Impl) Word
// - run(impl: *Impl, instructions: Obj) Obj
// - garbage_collect(impl: *Impl, checkpoint: Checkpoint, keep: Obj) Obj
// - deduplicate(impl: *Impl, checkpoint: Checkpoint, obj: Obj) Obj
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

pub fn run(vm: *Vm, instructions: Obj) !void {
    try vm.impl.run(instructions);
}

pub fn garbage_collect(vm: *Vm, checkpoint: Heap.Checkpoint, keep: Obj) !Obj {
    return try vm.impl.garbage_collect(checkpoint, keep);
}

pub fn deduplicate(vm: *Vm, checkpoint: Heap.Checkpoint, obj: Obj) !Obj {
    return vm.impl.deduplicate(checkpoint, obj);
}
