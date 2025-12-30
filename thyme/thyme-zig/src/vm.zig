// The Virtual Machine compiles instructions that exist as heap objects into
// machine code for better performance.
//
// The VM in this file doesn't know how to execute instructions; it delegates
// that to a CPU-architecture-specific implementation. While the way that
// instructions are mapped to machine code differs among architectures, the data
// they operate on does not:
//
// - The heap contains heap objects in a format that is documented in heap.zig.
// - The data stack is a memory region that instructions such as push_word or
//   add operate on.
// - The call stack is a memory region that is used to support nested
//   evaluations.
//
// Essentially, the role of the *Vm struct is just to hold data. The various VM
// implementations then contain the logic that operates on that data.

const std = @import("std");
const ArrayList = std.ArrayList;
const Ally = std.mem.Allocator;
const builtin = @import("builtin");

const compiler = @import("compiler.zig");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Instruction = @import("instruction.zig").Instruction;
const object_mod = @import("object.zig");

// Depending on the system, choose a different JIT implementation.
const Jit = switch (builtin.cpu.arch) {
    //.x86_64 => @import("vm_x86_64.zig"), // a JIT compiler
    else => @import("vm_interpreter.zig"), // an interpreter
};

ally: Ally,

heap: *Heap,
data_stack: Stack,
call_stack: Stack,

jit_ally: Jit.Ally, // Allocator that is used for jitted code.
jitted: ArrayList(JittedEntry),
const JittedEntry = struct { address: Address, jitted: Jit.Jitted };

const Vm = @This();

const Stack = struct {
    memory: []Word,
    used: usize,

    pub fn init(ally: Ally, size: usize) !Stack {
        return .{ .memory = try ally.alloc(Word, size), .used = 0 };
    }
    pub fn push(self: *Stack, word: Word) !void {
        if (self.memory.len == self.used) @panic("stack overflow");
        self.memory[self.used] = word;
        self.used += 1;
    }
    pub fn pop(self: *Stack) Word {
        self.used -= 1;
        return self.memory[self.used];
    }
    pub fn pop_n(self: *Stack, n: usize) void {
        self.used -= n;
    }
    pub fn get(self: *Stack, offset: usize) Word {
        return self.memory[self.used - 1 - offset];
    }
    pub fn is_empty(self: Stack) bool {
        return self.used == 0;
    }
};

pub fn init(heap: *Heap, ally: Ally) !Vm {
    return .{
        .ally = ally,
        .heap = heap,
        .data_stack = try Stack.init(ally, 1000000),
        .call_stack = try Stack.init(ally, 1000000),
        .jit_ally = try Jit.Ally.init(ally),
        .jitted = ArrayList(JittedEntry).empty,
    };
}

pub fn get_jitted(vm: *Vm, instructions: Address) !Jit.Jitted {
    for (vm.jitted.items) |entry|
        if (entry.address == instructions) return entry.jitted;
    const parsed = try Instruction.parse_instructions(vm.ally, vm.heap.*, instructions);
    std.debug.print("jitting\n", .{});
    const jitted = try Jit.compile(vm.jit_ally, parsed);
    try vm.jitted.append(vm.ally, .{ .address = instructions, .jitted = jitted });
    return jitted;
}

pub fn eval(vm: *Vm, ally: Ally, env: anytype, code: []const u8) !Address {
    if (!vm.data_stack.is_empty()) @panic("eval in eval");
    const fun = try compiler.code_to_fun(ally, vm.heap, env, code);
    // {
    //     var buffer: [64]u8 = undefined;
    //     const bw = std.debug.lockStderrWriter(&buffer);
    //     defer std.debug.unlockStderrWriter();
    //     try bw.print("evaling ", .{});
    //     try vm.heap.format(fun, bw);
    //     try bw.print("\n", .{});
    // }
    const result = try vm.call(fun, &[_]Address{});
    if (!vm.data_stack.is_empty()) @panic("bad stack");
    return result;
}

pub fn call(vm: *Vm, fun: Address, args: []const Address) !Address {
    //std.debug.print("calling function {}\n", .{fun});

    if (object_mod.get_int(vm.heap.*, vm.heap.load(fun, 2)) != args.len)
        @panic("called function with wrong number of params");

    for (args) |arg| try vm.data_stack.push(@intCast(arg));
    try vm.run(vm.heap.load(fun, 1));
    return vm.data_stack.pop();
}

pub fn run(vm: *Vm, instructions: Address) error{ OutOfMemory, ParseError, BadEval }!void {
    //std.debug.print("running instructions at {x}\n", .{instructions});
    const jitted = try vm.get_jitted(instructions);
    try Jit.run(vm, jitted);
}

pub fn garbage_collect(vm: *Vm, checkpoint: Heap.Checkpoint, keep: Address) !Address {
    const mapped = vm.heap.garbage_collect(vm.ally, checkpoint, keep);

    var i: usize = 0;
    while (i < vm.jitted.items.len) {
        if (vm.jitted.items[i].address >= checkpoint.used) {
            _ = vm.jitted.swapRemove(i);
        } else {
            i += 1;
        }
    }

    return mapped;
}
