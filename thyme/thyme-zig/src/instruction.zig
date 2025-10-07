const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const Object = @import("object.zig");

pub const Instruction = union(enum) {
    // Pushes a word to the stack.
    push_word: Word,

    // Pushes the address of an object to the stack.
    push_address: Object,

    // Pushes a word that is somewhere on the stack to the top of the stack. The
    // offset is relative to the top of the stack: 0 duplicates the top element,
    // 1 pushes the element below that, etc.
    push_from_stack: usize,

    pop: usize,

    pop_below_top: usize,

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

    eval,

    crash,

    pub fn new_instruction(heap: *Heap, instruction: Instruction) !Object {
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
            .pop => |amount| try Object.new_enum(heap, "pop", try Object.new_int(heap, @intCast(amount))),
            .pop_below_top => |amount| try Object.new_enum(heap, "pop_below_top", try Object.new_int(heap, @intCast(amount))),
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
            .eval => try Object.new_enum(heap, "eval", try Object.new_nil(heap)),
            .crash => try Object.new_enum(heap, "crash", try Object.new_nil(heap)),
        };
    }
    pub fn new_instructions(heap: *Heap, instructions: []const Instruction) !Object {
        if (instructions.len == 0) {
            return try Object.new_nil(heap);
        } else {
            return try Object.new_struct(heap, .{
                .head = try new_instruction(heap, instructions[0]),
                .tail = try new_instructions(heap, instructions[1..]),
            });
        }
    }

    const ParseResult = struct { instruction: Instruction, rest: Object };
    pub fn parse_first(instructions: Object) !?ParseResult {
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
                    else if (variant.is_symbol("pop"))
                        .{ .pop = @intCast(payload.int_value()) }
                    else if (variant.is_symbol("pop_below_top"))
                        .{ .pop_below_top = @intCast(payload.int_value()) }
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
                    else if (variant.is_symbol("eval"))
                        .{ .eval = {} }
                    else if (variant.is_symbol("crash"))
                        .{ .crash = {} }
                    else
                        return error.unknown_instruction;
                return .{ .instruction = inst, .rest = rest };
            },
            else => return error.bad_eval,
        }
    }
};
