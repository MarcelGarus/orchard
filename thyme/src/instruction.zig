const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Val = @import("value.zig");
const new_symbol = Val.new_symbol;
const get_symbol = Val.get_symbol;

pub const Instruction = union(enum) {
    word: Word, // Pushes the word to the stack.
    address: Obj, // Pushes the address to the stack.
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

    const If = struct { then: []const Instruction, else_: []const Instruction };
    const New = struct { has_pointers: bool, num_words: usize };

    pub fn new_instruction(ally: Ally, heap: *Heap, instruction: Instruction) error{OutOfMemory}!Obj {
        switch (instruction) {
            inline else => |payload| {
                const symbol = try new_symbol(heap, @tagName(instruction));
                const payloads = switch (comptime @TypeOf(payload)) {
                    void => .{},
                    Word => .{try heap.new_leaf(&.{@bitCast(payload)})},
                    Obj => .{payload},
                    If => .{
                        try new_instructions(ally, heap, payload.then),
                        try new_instructions(ally, heap, payload.else_),
                    },
                    New => .{
                        try heap.new_leaf(&.{if (payload.has_pointers) 1 else 0}),
                        try heap.new_leaf(&.{@intCast(payload.num_words)}),
                    },
                    else => @compileError("Handle type " ++ @typeName(@TypeOf(payload))),
                };
                const instr_obj = obj: {
                    var b = try heap.build_inner();
                    try b.emit(symbol);
                    inline for (payloads) |p| try b.emit(p);
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
    ) !Obj {
        var objs = try ally.alloc(Obj, instructions.len);
        for (0.., instructions) |i, instruction| objs[i] = try new_instruction(ally, heap, instruction);
        const instr = try heap.new_inner(objs);
        ally.free(objs);
        return instr;
    }

    const ParseResult = struct { instruction: Instruction, rest: Obj };
    pub fn parse_instruction(ally: Ally, instruction: Obj) error{ ParseError, OutOfMemory }!Instruction {
        const children = instruction.children();
        const name = get_symbol(children[0]);
        inline for (@typeInfo(Instruction).@"union".fields) |field| {
            if (std.mem.eql(u8, name, field.name)) {
                const payload: field.type = switch (field.type) {
                    void => {},
                    Word => children[1].word(0),
                    Obj => children[1],
                    If => If{
                        .then = try Instruction.parse_instructions(ally, children[1]),
                        .else_ = try Instruction.parse_instructions(ally, children[2]),
                    },
                    New => New{
                        .has_pointers = children[1].word(0) != 0,
                        .num_words = @intCast(children[2].word(0)),
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
        instructions: Obj,
    ) error{ ParseError, OutOfMemory }![]const Instruction {
        const children = instructions.children();
        var out = try ally.alloc(Instruction, children.len);
        for (0.., children) |i, child| out[i] = try parse_instruction(ally, child);
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
