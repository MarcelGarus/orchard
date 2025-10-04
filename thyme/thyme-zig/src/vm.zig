const std = @import("std");
const ArrayList = std.ArrayList;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
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

pub const Instruction = union(enum) {
    // Pushes a word to the stack.
    push_word: Word,

    // Pushes the address of an object to the stack.
    push_address: Object,

    // Pushes a word that is somewhere on the stack to the top of the stack. The
    // offset is relative to the top of the stack: 0 duplicates the top element,
    // 1 pushes the element below that, etc.
    push_from_stack: usize,

    // Pops two words. Pushes their sum.
    add,

    subtract,

    multiply,

    divide,

    modulo,

    shift_left,

    shift_right,

    // Pops a word. If the word is not zero, evaluates the then instructions.
    // Otherwise, the else instructions.
    if_not_zero: struct { then: Object, else_: Object },

    // New.
    new: struct { tag: u8, num_pointers: usize, num_literals: usize },

    tag,

    num_pointers,

    num_literals,

    load,
};

pub fn new_instructions(vm: *Vm, instructions: []const Instruction) !Object {
    if (instructions.len == 0) {
        return try Object.new_nil(vm.heap);
    } else {
        return try Object.new_struct(vm.heap, .{
            .head = try vm.new_instruction(instructions[0]),
            .tail = try vm.new_instructions(instructions[1..]),
        });
    }
}
pub fn new_instruction(vm: *Vm, instruction: Instruction) !Object {
    const heap = vm.heap;
    return switch (instruction) {
        .push_word => |word| try Object.new_enum(
            heap,
            "push_word",
            try Object.new_int(heap, @bitCast(word)),
        ),
        .push_address => |object| try Object.new_enum(heap, "push_address", object),
        .push_from_stack => |offset| try Object.new_enum(
            heap,
            "push_from_stack",
            try Object.new_int(heap, @intCast(offset)),
        ),
        .add => try Object.new_enum(heap, "add", try Object.new_nil(heap)),
        .subtract => try Object.new_enum(heap, "subtract", try Object.new_nil(heap)),
        .multiply => try Object.new_enum(heap, "multiply", try Object.new_nil(heap)),
        .divide => try Object.new_enum(heap, "divide", try Object.new_nil(heap)),
        .modulo => try Object.new_enum(heap, "modulo", try Object.new_nil(heap)),
        .shift_left => try Object.new_enum(heap, "shift_left", try Object.new_nil(heap)),
        .shift_right => try Object.new_enum(heap, "shift_right", try Object.new_nil(heap)),
        .if_not_zero => |if_| try Object.new_enum(
            heap,
            "if_not_zero",
            try Object.new_struct(heap, .{ .then = if_.then, .@"else" = if_.else_ }),
        ),
        .new => |new| try Object.new_enum(
            heap,
            "new",
            try Object.new_struct(heap, .{
                .tag = try Object.new_int(heap, @intCast(new.tag)),
                .num_pointers = try Object.new_int(heap, @intCast(new.num_pointers)),
                .num_literals = try Object.new_int(heap, @intCast(new.num_literals)),
            }),
        ),
        .tag => try Object.new_enum(heap, "tag", try Object.new_nil(heap)),
        .num_pointers => try Object.new_enum(heap, "num_pointers", try Object.new_nil(heap)),
        .num_literals => try Object.new_enum(heap, "num_literals", try Object.new_nil(heap)),
        .load => try Object.new_enum(heap, "load", try Object.new_nil(heap)),
    };
}

const ParseResult = struct { instruction: Instruction, rest: Object };
fn parse_first_instruction(instructions: Object) !?ParseResult {
    switch (instructions.kind()) {
        .nil => return null,
        .struct_ => {
            const instruction = instructions.field_by_name("head");
            const rest = instructions.field_by_name("tail");
            const variant = instruction.variant();
            const payload = instruction.payload();
            const inst: Instruction =
                if (variant.is_symbol("push_word"))
                    .{ .push_word = @bitCast(payload.int_value()) }
                else if (variant.is_symbol("push_address"))
                    .{ .push_address = payload }
                else if (variant.is_symbol("push_from_stack"))
                    .{ .push_from_stack = @intCast(payload.int_value()) }
                else if (variant.is_symbol("add"))
                    .{ .add = {} }
                else if (variant.is_symbol("subtract"))
                    .{ .subtract = {} }
                else if (variant.is_symbol("multiply"))
                    .{ .multiply = {} }
                else if (variant.is_symbol("divide"))
                    .{ .divide = {} }
                else if (variant.is_symbol("modulo"))
                    .{ .modulo = {} }
                else if (variant.is_symbol("shift_left"))
                    .{ .shift_left = {} }
                else if (variant.is_symbol("shift_right"))
                    .{ .shift_right = {} }
                else if (variant.is_symbol("if_not_zero"))
                    .{ .if_not_zero = .{
                        .then = payload.field_by_name("then"),
                        .else_ = payload.field_by_name("else"),
                    } }
                else if (variant.is_symbol("new"))
                    .{ .new = .{
                        .tag = @truncate(@as(u64, @bitCast(payload.field_by_name("tag").int_value()))),
                        .num_pointers = @intCast(payload.field_by_name("num_pointers").int_value()),
                        .num_literals = @intCast(payload.field_by_name("num_literals").int_value()),
                    } }
                else if (variant.is_symbol("tag"))
                    .{ .tag = {} }
                else if (variant.is_symbol("num_pointers"))
                    .{ .num_pointers = {} }
                else if (variant.is_symbol("num_literals"))
                    .{ .num_literals = {} }
                else if (variant.is_symbol("load"))
                    .{ .load = {} }
                else
                    return error.unknown_instruction;
            return .{ .instruction = inst, .rest = rest };
        },
        else => return error.bad_eval,
    }
}

pub fn eval(vm: *Vm, instructions: Object) !void {
    const parsed = try parse_first_instruction(instructions) orelse return;
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
