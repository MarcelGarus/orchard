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
const Word = Heap.Word;
const Obj = Heap.Obj;
const Instruction = @import("instruction.zig").Instruction;
const Val = @import("value.zig");

// Depending on the system, choose a different JIT implementation.
const Jit = switch (builtin.cpu.arch) {
    // .x86_64 => @import("vm_x86_64.zig"), // a JIT compiler
    else => @import("vm_interpreter.zig"), // an interpreter
};

ally: Ally,

heap: *Heap,
data_stack: Stack,
call_stack: Stack,

jit_ally: Jit.Ally, // Allocator that is used for jitted code.
jitted: ArrayList(JittedEntry),
const JittedEntry = struct { obj: Obj, jitted: Jit.Jitted };

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

pub fn get_jitted(vm: *Vm, instructions: Obj) !Jit.Jitted {
    for (vm.jitted.items) |entry| if (entry.obj == instructions) return entry.jitted;
    const parsed = try Instruction.parse_instructions(vm.ally, instructions);
    std.debug.print("jitting\n", .{});
    const jitted = try Jit.compile(vm.jit_ally, parsed);
    try vm.jitted.append(vm.ally, .{ .obj = instructions, .jitted = jitted });
    return jitted;
}

pub fn eval(vm: *Vm, ally: Ally, common: compiler.CommonObjects, env: anytype, code: []const u8) !Val.Value {
    if (!vm.data_stack.is_empty()) @panic("eval in eval");
    const check = vm.heap.checkpoint();
    const fun = try compiler.code_to_lambda(ally, vm.heap, common, env, code);
    const fun_ = (try vm.heap.deduplicate(ally, check)).get(fun) orelse fun;
    // {
    //     var buffer: [64]u8 = undefined;
    //     const bw = std.debug.lockStderrWriter(&buffer);
    //     defer std.debug.unlockStderrWriter();
    //     try bw.print("evaling ", .{});
    //     try vm.heap.format(fun_, bw);
    //     try bw.print("\n", .{});
    // }
    const result = try vm.call(Val.Lambda.from(fun_), &[_]Val.Value{});
    if (!vm.data_stack.is_empty()) @panic("bad stack");
    return result;
}

pub fn call(vm: *Vm, lambda: Val.Lambda, args: []const Val.Value) !Val.Value {
    //std.debug.print("calling function {}\n", .{fun});
    vm.heap.dump_stats();

    const type_ = lambda.obj.child(0);
    const kind = type_.child(0);
    if (!std.mem.eql(u8, Val.get_symbol(kind), "lambda")) @panic("called non-fun");

    const num_params = type_.child(1).word(0);
    if (num_params != args.len) @panic("called function with wrong number of params");

    const instructions = lambda.obj.child(1);
    const closure = lambda.obj.child(2);
    for (args) |arg| try vm.data_stack.push(arg.obj.address);
    try vm.data_stack.push(closure.address);
    try vm.run(instructions);
    return .{ .obj = .{ .address = vm.data_stack.pop() } };
}

pub fn run(vm: *Vm, instructions: Obj) error{ OutOfMemory, ParseError, BadEval }!void {
    //std.debug.print("running instructions at {x}\n", .{instructions});
    //vm.heap.dump_obj(instructions);
    // vm.heap.dump_stats();
    const jitted = try vm.get_jitted(instructions);
    try Jit.run(vm, jitted);
}

pub fn garbage_collect(vm: *Vm, checkpoint: Heap.Checkpoint, keep: Obj) !Obj {
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
