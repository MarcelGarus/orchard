// Objects
//
// Given that we have a heap for immutable allocations, we can now build object
// abstractions on top of that.

const std = @import("std");
const Writer = std.io.Writer;

const compiler = @import("compiler.zig");
const Ir = compiler.Ir;
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Allocation = Heap.Allocation;

pub const TAG_NIL = 0;
pub const TAG_INT = 1;
pub const TAG_SYMBOL = 2;
pub const TAG_STRUCT = 3;
pub const TAG_FUN = 4;
pub const TAG_CLOSURE = 5;
pub const TAG_LAMBDA = 6;

heap: *Heap,
address: Address,

const Object = @This();

pub fn get_allocation(object: Object) Allocation {
    return object.heap.get(object.address);
}

const Kind = enum { nil, int, symbol, struct_, fun, closure, lambda };

pub fn kind(object: Object) Kind {
    const allocation = object.get_allocation();
    return switch (allocation.tag) {
        TAG_NIL => .nil,
        TAG_INT => .int,
        TAG_SYMBOL => .symbol,
        TAG_STRUCT => .struct_,
        TAG_FUN => .fun,
        TAG_CLOSURE => .closure,
        TAG_LAMBDA => .lambda,
        else => {
            std.debug.print("tag: {}\n", .{allocation.tag});
            @panic("unknown tag");
        },
    };
}

// Nil

pub fn new_nil(heap: *Heap) !Object {
    return .{
        .heap = heap,
        .address = try heap.new(.{
            .tag = TAG_NIL,
            .pointers = &[_]Word{},
            .literals = &[_]Word{},
        }),
    };
}

// Int

pub fn new_int(heap: *Heap, val: i64) !Object {
    return .{
        .heap = heap,
        .address = try heap.new(.{
            .tag = TAG_INT,
            .pointers = &[_]Word{},
            .literals = &[_]Word{@as(Word, @bitCast(val))},
        }),
    };
}

pub fn int_value(int: Object) i64 {
    if (int.kind() != .int) @panic("not an int");
    return @bitCast(int.get_allocation().literals[0]);
}

// Symbol

pub fn new_symbol(heap: *Heap, val: []const u8) !Object {
    var builder = try heap.object_builder(TAG_SYMBOL);
    const num_words = (val.len + 7) / 8;
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(val.len - i * 8, 8)) |j|
            w |= @as(Word, val[i * 8 + j]) << @intCast(j * 8);
        try builder.emit_literal(w);
    }
    return .{ .heap = heap, .address = try builder.finish() };
}

pub fn is_symbol(symbol: Object, check: []const u8) bool {
    if (symbol.kind() != .symbol) @panic("not a symbol");
    const num_words = (check.len + 7) / 8;
    const allocation = symbol.get_allocation();
    if (allocation.literals.len != num_words) return false;
    return std.mem.eql(
        u8,
        @as([*]const u8, @ptrCast(allocation.literals.ptr))[0..check.len],
        check,
    );
}

// Struct

pub fn new_struct(heap: *Heap, fields: anytype) !Object {
    const field_types = @typeInfo(@TypeOf(fields)).@"struct".fields;
    var field_names: [field_types.len]Object = undefined;
    inline for (0.., field_types) |i, type_| {
        field_names[i] = try Object.new_symbol(heap, type_.name);
    }
    var builder = try heap.object_builder(TAG_STRUCT);
    inline for (field_names, field_types) |name, type_| {
        try builder.emit_pointer(name.address);
        try builder.emit_pointer(@as(Object, @field(fields, type_.name)).address);
    }
    return .{ .heap = heap, .address = try builder.finish() };
}

pub fn num_fields(struct_: Object) usize {
    if (struct_.kind() != .struct_) @panic("not a struct");
    return struct_.get_allocation().pointers.len / 2;
}

const Field = struct { key: Object, value: Object };

pub fn field_by_index(struct_: Object, index: usize) Field {
    if (struct_.kind() != .struct_) @panic("not a struct");
    const words = struct_.get_allocation().pointers;
    const key_addr = words[2 * index];
    const val_addr = words[2 * index + 1];
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

// Fun

pub fn new_fun(heap: *Heap, num_params_: usize, ir_: Object, instructions_: Object) !Object {
    const address = try heap.new(.{
        .tag = TAG_FUN,
        .pointers = &[_]Word{ ir_.address, instructions_.address },
        .literals = &[_]Word{num_params_},
    });
    return .{ .heap = heap, .address = address };
}

pub fn num_params(fun: Object) usize {
    if (fun.kind() != .fun) @panic("not a fun");
    return @intCast(fun.get_allocation().literals[0]);
}

pub fn ir(fun: Object) Object {
    if (fun.kind() != .fun) @panic("not a fun");
    return .{ .heap = fun.heap, .address = fun.get_allocation().pointers[0] };
}

pub fn instructions(fun: Object) Object {
    if (fun.kind() != .fun) @panic("not a fun");
    return .{ .heap = fun.heap, .address = fun.get_allocation().pointers[1] };
}

// Closure

fn num_captured(closure: Object) usize {
    return closure.get_allocation().pointers.len;
}

fn captured(closure: Object, index: usize) Object {
    return .{ .heap = closure.heap, .address = closure.get_allocation().pointers[index] };
}

// Lambda

pub fn new_lambda(heap: *Heap, fun: Object, closure: Object) !Object {
    const address = try heap.new(.{
        .tag = TAG_LAMBDA,
        .pointers = &[_]Word{ fun.address, closure.address },
        .literals = &[_]Word{},
    });
    return .{ .heap = heap, .address = address };
}

pub fn fun_of_lambda(lambda: Object) Object {
    if (lambda.kind() != .lambda) @panic("not an lambda");
    const address: Address = @intCast(lambda.get_allocation().pointers[0]);
    return Object{ .heap = lambda.heap, .address = address };
}

pub fn closure_of_lambda(lambda: Object) Object {
    if (lambda.kind() != .lambda) @panic("not an lambda");
    const address: Address = @intCast(lambda.get_allocation().pointers[1]);
    return Object{ .heap = lambda.heap, .address = address };
}

// Formatting

pub fn format(object: Object, writer: *Writer) !void {
    try object.format_indented(writer, 0);
}
pub fn format_indented(object: Object, writer: *Writer, indentation: usize) error{WriteFailed}!void {
    switch (object.kind()) {
        .nil => try writer.print("nil", .{}),
        .int => try writer.print("{}", .{object.int_value()}),
        .symbol => {
            try writer.print("<", .{});
            try object.format_symbol(writer);
            try writer.print(">", .{});
        },
        .struct_ => {
            try writer.print("[\n", .{});
            for (0..object.num_fields()) |i| {
                const field = object.field_by_index(i);
                for (0..(indentation + 1)) |_| try writer.print("  ", .{});
                try field.key.format_symbol(writer);
                try writer.print(": ", .{});
                try field.value.format_indented(writer, indentation + 1);
                try writer.print("\n", .{});
            }
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("]", .{});
        },
        .fun => {
            try writer.print("fun\n", .{});
            for (0..(indentation + 1)) |_| try writer.print("  ", .{});
            try writer.print("{}\n", .{object.num_params()});
            for (0..(indentation + 1)) |_| try writer.print("  ", .{});
            try writer.print("ir: ", .{});
            const ir_ = @as(*Ir, @ptrFromInt(@as(usize, @bitCast(object.ir().int_value())))).*;
            try ir_.format_indented(writer, indentation + 1);
            for (0..(indentation + 1)) |_| try writer.print("  ", .{});
            try writer.print("instructions:\n", .{});
            var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
            const ally = debug_ally.allocator();
            const instrs = @import("instruction.zig").Instruction.parse_all(ally, object.instructions()) catch
                return error.WriteFailed;
            for (instrs) |instruction| {
                try instruction.format_indented(writer, indentation + 2);
            }
        },
        .closure => {
            try writer.print("closure", .{});
            for (0..object.num_captured()) |i| {
                try writer.print("\n", .{});
                for (0..(indentation + 1)) |_| try writer.print("  ", .{});
                try object.captured(i).format_indented(writer, indentation + 1);
            }
        },
        .lambda => {
            try writer.print("lambda\n", .{});
            for (0..(indentation + 1)) |_| try writer.print("  ", .{});
            try object.fun_of_lambda().format_indented(writer, indentation + 1);
            // for (0..(indentation + 1)) |_| try writer.print("  ", .{});
            // try object.closure_of_lambda().format_indented(writer, indentation + 1);
        },
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
