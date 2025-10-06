const std = @import("std");
const ArrayList = std.ArrayList;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const Instruction = @import("instruction.zig").Instruction;
const Object = @import("object.zig");

heap: *Heap,
data_stack: ArrayList(Word),
call_stack: ArrayList(Address),

const Vm = @This();

pub fn init(heap: *Heap) Vm {
    return .{
        .heap = heap,
        .data_stack = ArrayList(Word).empty,
        .call_stack = ArrayList(Address).empty,
    };
}

pub fn eval(vm: *Vm, instructions: Object) !void {
    const parsed = try Instruction.parse_first(instructions) orelse return;
    switch (parsed.instruction) {
        .push_word => |word| try vm.data_stack.append(vm.heap.ally, word),
        .push_address => |object| try vm.data_stack.append(
            vm.heap.ally,
            object.address.address,
        ),
        .push_from_stack => |offset| try vm.data_stack.append(
            vm.heap.ally,
            vm.data_stack.items[vm.data_stack.items.len - 1 - offset],
        ),
        .add => {
            const b: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_add);
            const a: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_add);
            try vm.data_stack.append(vm.heap.ally, @intCast(a + b));
        },
        .subtract => {
            const b: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_subtract);
            const a: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_subtract);
            try vm.data_stack.append(vm.heap.ally, @intCast(a - b));
        },
        .multiply => {
            const b: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_multiply);
            const a: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_multiply);
            try vm.data_stack.append(vm.heap.ally, @intCast(a * b));
        },
        .divide => {
            const b: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_divide);
            const a: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_divide);
            try vm.data_stack.append(vm.heap.ally, @intCast(@divTrunc(a, b)));
        },
        .modulo => {
            const b: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_modulo);
            const a: i64 = @intCast(vm.data_stack.pop() orelse return error.bad_modulo);
            try vm.data_stack.append(vm.heap.ally, @intCast(@mod(a, b)));
        },
        .if_not_zero => |if_| {
            const condition = vm.data_stack.pop() orelse return error.bad_if;
            try eval(vm, if (condition != 0) if_.then else if_.else_);
        },
        .new => |new| {
            const pointers = try vm.heap.ally.alloc(Address, new.num_pointers);
            const literals = try vm.heap.ally.alloc(Word, new.num_literals);
            for (0..new.num_literals) |i|
                literals[new.num_literals - 1 - i] = vm.data_stack.pop() orelse return error.bad_new;
            for (0..new.num_pointers) |i|
                pointers[new.num_pointers - 1 - i] = .{
                    .address = vm.data_stack.pop() orelse return error.bad_new,
                };
            const address = try vm.heap.new(.{
                .tag = new.tag,
                .pointers = pointers,
                .literals = literals,
            });
            try vm.data_stack.append(vm.heap.ally, address.address);
        },
        else => @panic("todo"),
    }
    try eval(vm, parsed.rest);
}

pub fn dump(vm: Vm) void {
    std.debug.print("VM:", .{});
    for (vm.data_stack.items) |data| {
        std.debug.print(" {}", .{data});
    }
    std.debug.print("\n", .{});
}
