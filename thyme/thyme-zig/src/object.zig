// Objects
//
// Given that we have a heap for immutable allocations, we can now build object
// abstractions on top of that.

const std = @import("std");

const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Allocation = Heap.Allocation;

pub const TAG_NIL = 0;
pub const TAG_INT = 1;
pub const TAG_SYMBOL = 2;
pub const TAG_STRUCT = 3;
pub const TAG_ENUM = 4;
pub const TAG_LAMBDA = 5;

heap: *Heap,
address: Address,

const Object = @This();

pub fn get_allocation(object: Object) Allocation {
    return object.heap.get(object.address);
}

const Kind = enum { nil, int, symbol, struct_, enum_ };

pub fn kind(object: Object) Kind {
    const allocation = object.get_allocation();
    return switch (allocation.tag) {
        TAG_NIL => .nil,
        TAG_INT => .int,
        TAG_SYMBOL => .symbol,
        TAG_STRUCT => .struct_,
        TAG_ENUM => .enum_,
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

pub fn new_symbol(heap: *Heap, comptime val: []const u8) !Object {
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

pub fn is_symbol(symbol: Object, comptime check: []const u8) bool {
    if (symbol.kind() != .symbol) @panic("not a symbol");
    const num_words = (check.len + 7) / 8;
    const words = symbol.heap.ally.alloc(Word, num_words) catch
        @panic("oom");
    inline for (0..num_words) |i| {
        var w: Word = 0;
        inline for (0..@min(check.len - i * 8, 8)) |j|
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

// Dumping

pub fn dump(object: Object, indentation: usize) void {
    for (0..indentation) |_| std.debug.print("  ", .{});
    switch (object.kind()) {
        .nil => std.debug.print("nil\n", .{}),
        .int => std.debug.print("{}\n", .{object.int_value()}),
        .symbol => {
            std.debug.print("symbol ", .{});
            object.dump_symbol();
            std.debug.print("\n", .{});
        },
        .struct_ => {
            std.debug.print("&\n", .{});
            for (0..object.num_fields()) |i| {
                const field = object.field_by_index(i);
                for (0..(indentation + 1)) |_| std.debug.print("  ", .{});
                field.key.dump_symbol();
                std.debug.print(":\n", .{});
                field.value.dump(indentation + 2);
            }
        },
        .enum_ => {
            std.debug.print("| ", .{});
            object.variant().dump_symbol();
            std.debug.print(":\n", .{});
            object.payload().dump(indentation + 2);
        },
    }
}

pub fn dump_symbol(symbol: Object) void {
    for (symbol.get_allocation().literals) |word| {
        for (0..8) |i| {
            const c: u8 = @intCast((word >> @intCast(8 * i)) & 0xff);
            if (c == 0) break;
            if (c >= 32 and c <= 150)
                std.debug.print("{c}", .{c})
            else
                std.debug.print("?", .{});
        }
    }
}
