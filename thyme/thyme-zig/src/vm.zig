const std = @import("std");
const ArrayList = std.ArrayList;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const objects = @import("objects.zig");

heap: *Heap,
data_stack: ArrayList(Word),
call_stack: ArrayList(Address),

const Vm = @This();

const Instruction = union(enum) {
    push_word: Word,
    push_address: Address,
    push_from_stack: usize,
    load,
};

pub fn eval(vm: *Vm, instructions: Address) void {
    const allocation = vm.heap.get(instructions);
    switch (allocation.tag) {
        objects.TAG_NIL => {},
        objects.TAG_STRUCT => {
            const instruction = objects.field(vm.heap, instructions, "head");
            const rest = objects.field(vm.heap, instructions, "tail");
            const variant = objects.get_variant(Heap, instruction);
            objects.is_symbol_literal(Heap, variant, "push_word");
        },
        else => @panic("bad eval"),
    }
}
