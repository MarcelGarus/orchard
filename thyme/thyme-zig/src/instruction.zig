const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const object_mod = @import("object.zig");

pub const Instruction = union(enum) {
    // Pushes a word to the stack.
    push_word: Word,

    // Pushes an address to the stack.
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
    if_not_zero: If,

    // New.
    new: New,

    tag,

    has_pointers,

    num_words,

    load,

    heap_checkpoint,

    // Pops a word, the object to keep. Pops another word, a heap checkpoint.
    // Collects garbage in the heap starting at the checkpoint, only keeping
    // objects that are needed.
    // (checkpoint object -> object)
    collect_garbage,

    eval,

    crash,

    const Object = struct { address: Address };
    const If = struct { then: []const Instruction, else_: []const Instruction };
    const New = struct { tag: u8, has_pointers: bool, num_words: usize };

    pub fn new_instruction(heap: *Heap, instruction: Instruction) error{OutOfMemory}!Address {
        switch (instruction) {
            inline else => |payload| {
                const symbol = try object_mod.new_symbol(heap, @tagName(instruction));
                const boxed_payloads = switch (comptime @TypeOf(payload)) {
                    void => .{},
                    Word => .{try object_mod.new_int(heap, @bitCast(payload))},
                    Object => .{payload.address},
                    usize => .{try object_mod.new_int(heap, @intCast(payload))},
                    If => .{
                        try new_instructions(heap, payload.then),
                        try new_instructions(heap, payload.else_),
                    },
                    New => .{
                        try object_mod.new_int(heap, @intCast(payload.tag)),
                        try object_mod.new_int(heap, if (payload.has_pointers) 1 else 0),
                        try object_mod.new_int(heap, @intCast(payload.num_words)),
                    },
                    else => @compileError("Handle type " ++ @typeName(@TypeOf(payload))),
                };
                var builder = try heap.object_builder(@intFromEnum(object_mod.Tag.composite));
                try builder.emit_pointer(symbol);
                inline for (boxed_payloads) |p| try builder.emit_pointer(p);
                return builder.finish();
            },
        }
    }
    pub fn new_instructions(heap: *Heap, instructions: []const Instruction) !Address {
        if (instructions.len == 0) {
            var builder = try heap.object_builder(2);
            return builder.finish();
        } else {
            const head = try new_instruction(heap, instructions[0]);
            const tail = try new_instructions(heap, instructions[1..]);
            var builder = try heap.object_builder(0);
            try builder.emit_pointer(head);
            try builder.emit_pointer(tail);
            return builder.finish();
        }
    }

    const ParseResult = struct { instruction: Instruction, rest: Address };
    pub fn parse_instructions(ally: Ally, heap: Heap, instructions: Address) ![]const Instruction {
        var current = instructions;
        var out = ArrayList(Instruction).empty;
        while (true) {
            if (heap.get(current).words.len == 0) {
                break; // nil
            } else {
                const instruction = heap.load(current, 0);
                const rest = heap.load(current, 1);
                try out.append(ally, try parse_instruction(ally, heap, instruction));
                current = rest;
            }
        }
        return out.items;
    }
    pub fn parse_instruction(
        ally: Ally,
        heap: Heap,
        instruction: Address,
    ) error{ ParseError, OutOfMemory }!Instruction {
        const instruction_words = heap.get(instruction).words;
        const variant = object_mod.get_symbol(heap, instruction_words[0]);
        inline for (@typeInfo(Instruction).@"union".fields) |field| {
            if (std.mem.eql(u8, variant, field.name)) {
                const payload = switch (field.type) {
                    void => {},
                    Word => @as(Word, @intCast(object_mod.get_int(heap, instruction_words[1]))),
                    Object => Object{ .address = instruction_words[1] },
                    usize => @as(usize, @intCast(object_mod.get_int(heap, instruction_words[1]))),
                    If => If{
                        .then = try Instruction.parse_instructions(ally, heap, instruction_words[1]),
                        .else_ = try Instruction.parse_instructions(ally, heap, instruction_words[2]),
                    },
                    New => New{
                        .tag = @truncate(
                            @as(u64, @bitCast(object_mod.get_int(heap, instruction_words[1]))),
                        ),
                        .has_pointers = object_mod.get_int(heap, instruction_words[2]) != 0,
                        .num_words = @intCast(object_mod.get_int(heap, instruction_words[3])),
                    },
                    else => @compileError("Handle type " ++ @typeName(field.type)),
                };
                return @unionInit(Instruction, field.name, payload);
            }
        }
        return error.ParseError;
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
            .if_not_zero => |if_| {
                try writer.print("if_not_zero then\n", .{});
                for (if_.then) |instruction|
                    try instruction.format_indented(writer, indentation + 1);
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("else\n", .{});
                for (if_.else_) |instruction|
                    try instruction.format_indented(writer, indentation + 1);
            },
            .new => |new| try writer.print(
                "new [{}] with {} {s}\n",
                .{ new.tag, new.num_words, if (new.has_pointers) "pointers" else "literals" },
            ),
            inline else => try writer.print("{s}\n", .{@tagName(instr)}),
        }
    }
};
