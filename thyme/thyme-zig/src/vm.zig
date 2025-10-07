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
    const parsed = Instruction.parse_first(instructions) catch |e| {
        std.debug.print("Couldn't parse instruction:\n", .{});
        instructions.dump(0);
        return e;
    } orelse return;
    std.debug.print("{any}\n", .{vm.data_stack});
    std.debug.print("Running {any}\n", .{parsed.instruction});
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
        .pop => |amount| vm.data_stack.items.len -= amount,
        .pop_below_top => |amount| {
            const top = vm.data_stack.pop() orelse return error.bad_instruction;
            vm.data_stack.items.len -= amount;
            try vm.data_stack.append(vm.heap.ally, top);
        },
        .add => {
            const b: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            const a: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            try vm.data_stack.append(vm.heap.ally, @bitCast(a +% b));
        },
        .subtract => {
            const b: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            const a: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            try vm.data_stack.append(vm.heap.ally, @bitCast(a -% b));
        },
        .multiply => {
            const b: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            const a: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            try vm.data_stack.append(vm.heap.ally, @bitCast(a *% b));
        },
        .divide => {
            const b: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            const a: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            try vm.data_stack.append(vm.heap.ally, @bitCast(@divTrunc(a, b)));
        },
        .modulo => {
            const b: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            const a: i64 = @bitCast(vm.data_stack.pop() orelse return error.bad_instruction);
            try vm.data_stack.append(vm.heap.ally, @bitCast(@mod(a, b)));
        },
        .if_not_zero => |if_| {
            const condition = vm.data_stack.pop() orelse return error.bad_if;
            try eval(vm, if (condition != 0) if_.then else if_.else_);
        },
        .new => |new| {
            const pointers = try vm.heap.ally.alloc(Address, new.num_pointers);
            const literals = try vm.heap.ally.alloc(Word, new.num_literals);
            for (0..new.num_literals) |i|
                literals[new.num_literals - 1 - i] = vm.data_stack.pop() orelse return error.bad_instruction;
            for (0..new.num_pointers) |i|
                pointers[new.num_pointers - 1 - i] = .{
                    .address = vm.data_stack.pop() orelse return error.bad_instruction,
                };
            const address = try vm.heap.new(.{
                .tag = new.tag,
                .pointers = pointers,
                .literals = literals,
            });
            try vm.data_stack.append(vm.heap.ally, address.address);
        },
        .tag => {
            const address = Address{ .address = vm.data_stack.pop() orelse return error.bad_instruction };
            const tag = vm.heap.get(address).tag;
            try vm.data_stack.append(vm.heap.ally, @intCast(tag));
        },
        .num_pointers => {
            const address = Address{ .address = vm.data_stack.pop() orelse return error.bad_instruction };
            const num_pointers = vm.heap.get(address).pointers.len;
            try vm.data_stack.append(vm.heap.ally, @intCast(num_pointers));
        },
        .num_literals => {
            const address = Address{ .address = vm.data_stack.pop() orelse return error.bad_instruction };
            const num_literals = vm.heap.get(address).literals.len;
            try vm.data_stack.append(vm.heap.ally, @intCast(num_literals));
        },
        .load => {
            const offset: usize = @intCast(vm.data_stack.pop() orelse return error.bad_instruction);
            const base = Address{ .address = @intCast(vm.data_stack.pop() orelse return error.bad_instruction) };
            const word = vm.heap.load(base, offset);
            try vm.data_stack.append(vm.heap.ally, word);
        },
        .eval => {
            const called = Object{
                .heap = vm.heap,
                .address = .{ .address = vm.data_stack.pop() orelse return error.bad_instruction },
            };
            try vm.eval(called);
        },
        .crash => {
            @panic("crashed");
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
