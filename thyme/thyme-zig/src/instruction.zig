const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const object_mod = @import("object.zig");

pub const Instruction = union(enum) {
    word: Word, // Pushes the word to the stack.
    address: Object, // Pushes the address to the stack.
    stack: usize, // Pushes a word from the stack to the stack. Offset 0 = top element, 1 = below top, etc.
    pop: usize, // Pops the given number of words from the stack.
    popover: usize, // Keeps the top element on the stack, but pops the given number of words below that.
    add, // a b -> (a+b)
    subtract, // a b -> (a-b)
    multiply, // a b -> (a*b)
    divide, // a b -> (a/b)
    modulo, // a b -> (a%b)
    shl, // a b -> (a<<b). Stands for shift left.
    shr, // a b -> (a>>b). Stands for shift right.
    compare, // Pops two words. Stack before: a b. If a == b, pushes 1. If a > b, pushes 1. If a < b, pushes 2.
    @"if": If, // Pops a word. If not 0, runs then instructions. Otherwise, else instructions.
    new: New, // Creates a new heap object.
    flatptro, // Pops an address. An object of the form [a [b [c []]]] becomes [a b c].
    flatlito, // Pops an address. An object of the form [[a] [[b] [[c] []]]] becomes [a b c].
    points, // Pops an address. Returns 1 or 0, depending on whether the object contains pointers.
    size, // Pops an address. Returns the size of the object.
    load, // addr offset -> ... Loads a word at the offset from the object.
    heapsize, // Pushes the size of the heap onto the stack. Can be used as a checkpoint for gc.
    gc, // heapsize obj -> obj. Collects garbage starting at the heapsize, only keeping dependencies of obj.
    eval, // Pops an address, which should point to an object containing instructions. Runs those.
    crash, // Pops a message. Crashes with the message.

    const Object = struct { address: Address };
    const If = struct { then: []const Instruction, else_: []const Instruction };
    const New = struct { has_pointers: bool, num_words: usize };

    pub fn new_instruction(ally: Ally, heap: *Heap, instruction: Instruction) error{OutOfMemory}!Address {
        switch (instruction) {
            inline else => |payload| {
                const symbol = try object_mod.new_symbol(heap, @tagName(instruction));
                const payloads = switch (comptime @TypeOf(payload)) {
                    void => .{},
                    Word => .{obj: {
                        var b = try heap.object_builder();
                        try b.emit_literal(@bitCast(payload));
                        break :obj b.finish();
                    }},
                    Object => .{payload.address},
                    usize => .{obj: {
                        var b = try heap.object_builder();
                        try b.emit_literal(@bitCast(payload));
                        break :obj b.finish();
                    }},
                    If => .{
                        try new_instructions(ally, heap, payload.then),
                        try new_instructions(ally, heap, payload.else_),
                    },
                    New => .{
                        obj: {
                            var b = try heap.object_builder();
                            try b.emit_literal(if (payload.has_pointers) 1 else 0);
                            break :obj b.finish();
                        },
                        obj: {
                            var b = try heap.object_builder();
                            try b.emit_literal(@intCast(payload.num_words));
                            break :obj b.finish();
                        },
                    },
                    else => @compileError("Handle type " ++ @typeName(@TypeOf(payload))),
                };
                const instr_obj = obj: {
                    var b = try heap.object_builder();
                    try b.emit_pointer(symbol);
                    inline for (payloads) |p| try b.emit_pointer(p);
                    break :obj b.finish();
                };
                return instr_obj;
            },
        }
    }
    pub fn new_instructions(
        ally: Ally,
        heap: *Heap,
        instructions: []const Instruction,
    ) !Address {
        var objs = try ally.alloc(Address, instructions.len);
        for (0.., instructions) |i, instruction| objs[i] = try new_instruction(ally, heap, instruction);
        const instr = try heap.new(.{ .has_pointers = true, .words = objs });
        ally.free(objs);
        return instr;
    }

    const ParseResult = struct { instruction: Instruction, rest: Address };
    pub fn parse_instruction(
        ally: Ally,
        heap: Heap,
        instruction: Address,
    ) error{ ParseError, OutOfMemory }!Instruction {
        const words = heap.get(instruction).words;
        const name = object_mod.get_symbol(heap, words[0]);
        inline for (@typeInfo(Instruction).@"union".fields) |field| {
            if (std.mem.eql(u8, name, field.name)) {
                const payload: field.type = switch (field.type) {
                    void => {},
                    Word => @intCast(heap.load(words[1], 0)),
                    Object => Object{ .address = words[1] },
                    usize => @intCast(heap.load(words[1], 0)),
                    If => If{
                        .then = try Instruction.parse_instructions(ally, heap, words[1]),
                        .else_ = try Instruction.parse_instructions(ally, heap, words[2]),
                    },
                    New => New{
                        .has_pointers = heap.load(words[1], 0) != 0,
                        .num_words = @intCast(heap.load(words[2], 0)),
                    },
                    else => @compileError("Handle type " ++ @typeName(field.type)),
                };
                return @unionInit(Instruction, field.name, payload);
            }
        }
        std.debug.print("unknown instruction {s}\n", .{name});
        return error.ParseError;
    }
    pub fn parse_instructions(
        ally: Ally,
        heap: Heap,
        instructions: Address,
    ) error{ ParseError, OutOfMemory }![]const Instruction {
        const words = heap.get(instructions).words;
        var out = try ally.alloc(Instruction, words.len);
        for (0.., words) |i, word| out[i] = try parse_instruction(ally, heap, word);
        return out;
    }

    pub fn format(instr: Instruction, writer: *Writer) !void {
        try instr.format_indented(writer, 0);
    }
    pub fn format_indented(instr: Instruction, writer: *Writer, indentation: usize) !void {
        for (0..indentation) |_| try writer.print("  ", .{});
        switch (instr) {
            .word => |word| try writer.print("push_word {x}\n", .{word}),
            .address => |object| try writer.print("push_address {x}\n", .{object.address}),
            .stack => |offset| try writer.print("push_from_stack {}\n", .{offset}),
            .pop => |amount| try writer.print("pop {}\n", .{amount}),
            .popover => |amount| try writer.print("pop_below_top {}\n", .{amount}),
            .@"if" => |if_| {
                try writer.print("if_not_zero then\n", .{});
                for (if_.then) |instruction|
                    try instruction.format_indented(writer, indentation + 1);
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("else\n", .{});
                for (if_.else_) |instruction|
                    try instruction.format_indented(writer, indentation + 1);
            },
            .new => |new| try writer.print(
                "new with {} {s}\n",
                .{ new.num_words, if (new.has_pointers) "pointers" else "literals" },
            ),
            inline else => try writer.print("{s}\n", .{@tagName(instr)}),
        }
    }
};
