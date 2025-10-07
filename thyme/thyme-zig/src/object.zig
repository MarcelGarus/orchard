// Objects
//
// Given that we have a heap for immutable allocations, we can now build object
// abstractions on top of that.

const std = @import("std");
const Writer = std.io.Writer;

const Ast = @import("ast.zig");
const ast_to_ir = @import("ast_to_ir.zig");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Allocation = Heap.Allocation;
const Instruction = @import("instruction.zig").Instruction;
const Ir = @import("ir.zig");
const ir_to_instructions = @import("ir_to_instructions.zig");
const str_to_ast = @import("str_to_ast.zig");

pub const TAG_NIL = 0;
pub const TAG_INT = 1;
pub const TAG_SYMBOL = 2;
pub const TAG_STRUCT = 3;
pub const TAG_ENUM = 4;
pub const TAG_FUN = 5;
pub const TAG_LAMBDA = 6;

heap: *Heap,
address: Address,

const Object = @This();

pub fn get_allocation(object: Object) Allocation {
    return object.heap.get(object.address);
}

const Kind = enum { nil, int, symbol, struct_, enum_, fun, lambda };

pub fn kind(object: Object) Kind {
    const allocation = object.get_allocation();
    return switch (allocation.tag) {
        TAG_NIL => .nil,
        TAG_INT => .int,
        TAG_SYMBOL => .symbol,
        TAG_STRUCT => .struct_,
        TAG_ENUM => .enum_,
        TAG_FUN => .fun,
        TAG_LAMBDA => .lambda,
        else => @panic("unknown tag"),
    };
}

// Nil

pub fn new_nil(heap: *Heap) !Object {
    return .{
        .heap = heap,
        .address = try heap.new_fancy(TAG_NIL, .{}),
    };
}

// Int

pub fn new_int(heap: *Heap, val: i64) !Object {
    return .{
        .heap = heap,
        .address = try heap.new_fancy(TAG_INT, .{
            @as(Word, @bitCast(val)),
        }),
    };
}

pub fn int_value(int: Object) i64 {
    if (int.kind() != .int) @panic("not an int");
    return @bitCast(int.get_allocation().literals[0]);
}

// Symbol

pub fn new_symbol(heap: *Heap, val: []const u8) !Object {
    const num_words = (val.len + 7) / 8;
    const words = try heap.ally.alloc(Word, num_words);
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(val.len - i * 8, 8)) |j|
            w |= @as(Word, val[i * 8 + j]) << @intCast(j * 8);
        words[i] = w;
    }
    const address = try heap.new(.{
        .tag = TAG_SYMBOL,
        .pointers = &[_]Address{},
        .literals = words,
    });
    heap.ally.free(words);
    return .{ .heap = heap, .address = address };
}

pub fn is_symbol(symbol: Object, check: []const u8) bool {
    if (symbol.kind() != .symbol) @panic("not a symbol");
    const num_words = (check.len + 7) / 8;
    const words = symbol.heap.ally.alloc(Word, num_words) catch
        @panic("oom");
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(check.len - i * 8, 8)) |j|
            w |= @as(Word, check[i * 8 + j]) << @intCast(j * 8);
        words[i] = w;
    }
    const matches = std.mem.eql(Word, symbol.get_allocation().literals, words);
    symbol.heap.ally.free(words);
    return matches;
}

// Struct

pub fn new_struct(heap: *Heap, fields: anytype) !Object {
    const field_types = @typeInfo(@TypeOf(fields)).@"struct".fields;
    const words = try heap.ally.alloc(Address, 2 * field_types.len);
    inline for (0.., field_types) |i, type_| {
        words[2 * i] = (try Object.new_symbol(heap, type_.name)).address;
        words[2 * i + 1] = @as(Object, @field(fields, type_.name)).address;
    }
    const address = try heap.new(.{
        .tag = TAG_STRUCT,
        .pointers = words,
        .literals = &[_]Word{},
    });
    return .{ .heap = heap, .address = address };
}

pub fn num_fields(struct_: Object) usize {
    if (struct_.kind() != .struct_) @panic("not a struct");
    return struct_.get_allocation().pointers.len / 2;
}

const Field = struct { key: Object, value: Object };

pub fn field_by_index(struct_: Object, index: usize) Field {
    if (struct_.kind() != .struct_) @panic("not a struct");
    const pointers = struct_.get_allocation().pointers;
    const key_addr = pointers[2 * index];
    const val_addr = pointers[2 * index + 1];
    const key = Object{ .heap = struct_.heap, .address = key_addr };
    const value = Object{ .heap = struct_.heap, .address = val_addr };
    return .{ .key = key, .value = value };
}

pub fn field_by_name(struct_: Object, comptime name: []const u8) Object {
    if (struct_.kind() != .struct_) @panic("not a struct");
    for (0..struct_.num_fields()) |i| {
        const field = struct_.field_by_index(i);
        if (field.key.is_symbol(name))
            return field.value;
    }
    @panic("field not in struct");
}

// Enum

pub fn new_enum(heap: *Heap, comptime variant_: []const u8, payload_: Object) !Object {
    const address = try heap.new_fancy(TAG_ENUM, .{
        (try Object.new_symbol(heap, variant_)).address,
        payload_.address,
    });
    return .{ .heap = heap, .address = address };
}

pub fn variant(enum_: Object) Object {
    if (enum_.kind() != .enum_) @panic("not an enum");
    return .{
        .heap = enum_.heap,
        .address = enum_.get_allocation().pointers[0],
    };
}

pub fn payload(enum_: Object) Object {
    if (enum_.kind() != .enum_) @panic("not an enum");
    return .{
        .heap = enum_.heap,
        .address = enum_.get_allocation().pointers[1],
    };
}

// Fun

pub fn new_fun(heap: *Heap, num_params_: usize, ir: Object, instructions_: Object) !Object {
    const address = try heap.new_fancy(TAG_FUN, .{
        ir.address,
        instructions_.address,
        @as(Word, @intCast(num_params_)),
    });
    return .{ .heap = heap, .address = address };
}
pub fn new_fun_from_ir(heap: *Heap, ir: Ir) !Object {
    return try Object.new_fun(
        heap,
        ir.params.len,
        try Object.new_nil(heap),
        try ir_to_instructions.compile(heap, ir),
    );
}
pub fn new_fun_from_ast(heap: *Heap, ast: Ast.Expr) !Object {
    const ir = try ast_to_ir.compile(heap.ally, ast, heap);
    return try new_fun_from_ir(heap, ir);
}
pub fn new_fun_from_code(heap: *Heap, code: []const u8) !Object {
    const ast = try str_to_ast.parse(heap.ally, code);
    return try new_fun_from_ast(heap, ast);
}

pub fn num_params(fun: Object) usize {
    if (fun.kind() != .fun) @panic("not an fun");
    return @intCast(fun.get_allocation().literals[0]);
}

pub fn instructions(fun: Object) Object {
    if (fun.kind() != .fun) @panic("not an fun");
    return .{
        .heap = fun.heap,
        .address = fun.get_allocation().pointers[1],
    };
}

// Lambda

pub fn new_lambda(heap: *Heap, fun: Object, closure: Object) !Object {
    const address = try heap.new_fancy(TAG_LAMBDA, .{
        fun.address,
        closure.address,
    });
    return .{ .heap = heap, .address = address };
}

pub fn format(object: Object, writer: *Writer) !void {
    try object.format_indented(writer, 0);
}
pub fn format_indented(object: Object, writer: *Writer, indentation: usize) error{WriteFailed}!void {
    for (0..indentation) |_| try writer.print("  ", .{});
    switch (object.kind()) {
        .nil => try writer.print("nil\n", .{}),
        .int => try writer.print("{}\n", .{object.int_value()}),
        .symbol => {
            try writer.print("symbol ", .{});
            try object.format_symbol(writer);
            try writer.print("\n", .{});
        },
        .struct_ => {
            try writer.print("&\n", .{});
            for (0..object.num_fields()) |i| {
                const field = object.field_by_index(i);
                for (0..(indentation + 1)) |_| try writer.print("  ", .{});
                try field.key.format_symbol(writer);
                try writer.print(":\n", .{});
                try field.value.format_indented(writer, indentation + 2);
            }
        },
        .enum_ => {
            try writer.print("| ", .{});
            try object.variant().format_symbol(writer);
            try writer.print(":\n", .{});
            try object.payload().format_indented(writer, indentation + 2);
        },
        .fun => {
            try writer.print("fun\n", .{});
            for (0..(indentation + 1)) |_| try writer.print("  ", .{});
            try writer.print("{}\n", .{object.num_params()});
            for (0..(indentation + 1)) |_| try writer.print("  ", .{});
            try writer.print("instructions:\n", .{});
            try object.instructions().format_instructions(writer, indentation + 2);
        },
        .lambda => try writer.print("lambda\n", .{}),
    }
}

pub fn format_symbol(symbol: Object, writer: *Writer) !void {
    for (symbol.get_allocation().literals) |word| {
        for (0..8) |i| {
            const c: u8 = @intCast((word >> @intCast(8 * i)) & 0xff);
            if (c == 0) break;
            if (c >= 32 and c <= 150)
                try writer.print("{c}", .{c})
            else
                try writer.print("?", .{});
        }
    }
}

pub fn format_instructions(instrs: Object, writer: *Writer, indentation: usize) error{WriteFailed}!void {
    const parsed = Instruction.parse_first(instrs) catch {
        try instrs.format_indented(writer, indentation);
        return;
    } orelse return;
    try parsed.instruction.format_indented(writer, indentation);
    try parsed.rest.format_instructions(writer, indentation);
}
