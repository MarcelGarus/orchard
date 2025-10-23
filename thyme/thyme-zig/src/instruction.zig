const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

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

    compare,

    // Pops a word. If the word is not zero, evaluates the then instructions.
    // Otherwise, the else instructions.
    if_not_zero: struct { then: []const Instruction, else_: []const Instruction },

    // New.
    new: struct { tag: u8, num_pointers: usize, num_literals: usize },

    tag,

    num_pointers,

    num_literals,

    load,

    eval,

    crash,

    pub fn new_instruction(heap: *Heap, instruction: Instruction) error{OutOfMemory}!Object {
        return switch (instruction) {
            .push_word => |word| try Object.new_struct(heap, .{
                .push_word = try Object.new_int(heap, @bitCast(word)),
            }),
            .push_address => |object| try Object.new_struct(heap, .{ .push_address = object }),
            .push_from_stack => |offset| try Object.new_struct(heap, .{
                .push_from_stack = try Object.new_int(heap, @intCast(offset)),
            }),
            .pop => |amount| try Object.new_struct(heap, .{
                .pop = try Object.new_int(heap, @intCast(amount)),
            }),
            .pop_below_top => |amount| try Object.new_struct(heap, .{
                .pop_below_top = try Object.new_int(heap, @intCast(amount)),
            }),
            .add => try Object.new_struct(heap, .{ .add = try Object.new_nil(heap) }),
            .subtract => try Object.new_struct(heap, .{ .subtract = try Object.new_nil(heap) }),
            .multiply => try Object.new_struct(heap, .{ .multiply = try Object.new_nil(heap) }),
            .divide => try Object.new_struct(heap, .{ .divide = try Object.new_nil(heap) }),
            .modulo => try Object.new_struct(heap, .{ .modulo = try Object.new_nil(heap) }),
            .shift_left => try Object.new_struct(heap, .{ .shift_left = try Object.new_nil(heap) }),
            .shift_right => try Object.new_struct(heap, .{ .shift_right = try Object.new_nil(heap) }),
            .compare => try Object.new_struct(heap, .{ .compare = try Object.new_nil(heap) }),
            .if_not_zero => |if_| try Object.new_struct(heap, .{
                .if_not_zero = try Object.new_struct(heap, .{
                    .then = try new_instructions(heap, if_.then),
                    .@"else" = try new_instructions(heap, if_.else_),
                }),
            }),
            .new => |new| try Object.new_struct(heap, .{
                .new = try Object.new_struct(heap, .{
                    .tag = try Object.new_int(heap, @intCast(new.tag)),
                    .num_pointers = try Object.new_int(heap, @intCast(new.num_pointers)),
                    .num_literals = try Object.new_int(heap, @intCast(new.num_literals)),
                }),
            }),
            .tag => try Object.new_struct(heap, .{ .tag = try Object.new_nil(heap) }),
            .num_pointers => try Object.new_struct(heap, .{ .num_pointers = try Object.new_nil(heap) }),
            .num_literals => try Object.new_struct(heap, .{ .num_literals = try Object.new_nil(heap) }),
            .load => try Object.new_struct(heap, .{ .load = try Object.new_nil(heap) }),
            .eval => try Object.new_struct(heap, .{ .eval = try Object.new_nil(heap) }),
            .crash => try Object.new_struct(heap, .{ .crash = try Object.new_nil(heap) }),
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
    pub fn parse_all(ally: Ally, instructions: Object) ![]const Instruction {
        var object = instructions;
        var out = ArrayList(Instruction).empty;
        while (try parse_first(ally, object)) |result| {
            try out.append(ally, result.instruction);
            object = result.rest;
        }
        return out.items;
    }
    pub fn parse_first(ally: Ally, instructions: Object) error{ OutOfMemory, UnknownInstruction, BadEval }!?ParseResult {
        switch (instructions.kind()) {
            .nil => return null,
            .struct_ => {
                const instruction = instructions.field_by_name("head");
                const rest = instructions.field_by_name("tail");
                const first_field = instruction.field_by_index(0);
                const variant = first_field.key;
                const payload = first_field.value;
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
                    else if (variant.is_symbol("compare"))
                        .{ .compare = {} }
                    else if (variant.is_symbol("if_not_zero"))
                        .{ .if_not_zero = .{
                            .then = try Instruction.parse_all(ally, payload.field_by_name("then")),
                            .else_ = try Instruction.parse_all(ally, payload.field_by_name("else")),
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
                        return error.UnknownInstruction;
                return .{ .instruction = inst, .rest = rest };
            },
            else => {
                std.debug.print("not an instruction: {any}\n", .{instructions.kind()});
                return error.BadEval;
            },
        }
    }

    pub fn format(instr: Instruction, writer: *Writer) !void {
        try instr.format_indented(writer, 0);
    }
    pub fn format_indented(instr: Instruction, writer: *Writer, indentation: usize) !void {
        for (0..indentation) |_| try writer.print("  ", .{});
        switch (instr) {
            .push_word => |word| try writer.print("push_word {x}\n", .{word}),
            .push_address => |object| try writer.print("push_address {x}\n", .{object.address}),
            .push_from_stack => |offset| try writer.print("push_from_stack {}\n", .{offset}),
            .pop => |amount| try writer.print("pop {}\n", .{amount}),
            .pop_below_top => |amount| try writer.print("pop_below_top {}\n", .{amount}),
            .add => try writer.print("add\n", .{}),
            .subtract => try writer.print("subtract\n", .{}),
            .multiply => try writer.print("multiply\n", .{}),
            .divide => try writer.print("divide\n", .{}),
            .modulo => try writer.print("modulo\n", .{}),
            .shift_left => try writer.print("shift_left\n", .{}),
            .shift_right => try writer.print("shift_right\n", .{}),
            .compare => try writer.print("compare\n", .{}),
            .if_not_zero => |if_| {
                try writer.print("if_not_zero\n", .{});
                for (0..(indentation + 1)) |_| try writer.print("  ", .{});
                try writer.print("then\n", .{});
                for (if_.then) |instruction|
                    try instruction.format_indented(writer, indentation + 2);
                for (0..(indentation + 1)) |_| try writer.print("  ", .{});
                try writer.print("else\n", .{});
                for (if_.else_) |instruction|
                    try instruction.format_indented(writer, indentation + 2);
            },
            .new => |new| try writer.print(
                "new [{}] {} pointers, {} literals\n",
                .{ new.tag, new.num_pointers, new.num_literals },
            ),
            .tag => try writer.print("tag\n", .{}),
            .num_pointers => try writer.print("num_pointers\n", .{}),
            .num_literals => try writer.print("num_literals\n", .{}),
            .load => try writer.print("load\n", .{}),
            .eval => try writer.print("eval\n", .{}),
            .crash => try writer.print("crash\n", .{}),
        }
    }
};
