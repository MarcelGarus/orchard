// Objects
//
// Given that we have a heap for immutable allocations, we can now build object
// abstractions on top of that.

const std = @import("std");

const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;

const TAG_NIL = 0;
const TAG_INT = 1;
const TAG_SYMBOL = 2;
const TAG_STRUCT = 3;
const TAG_ENUM = 4;
const TAG_LAMBDA = 5;

pub fn new_nil(heap: *Heap) !Address {
    return try heap.new_fancy(TAG_NIL, .{});
}

pub fn new_int(heap: *Heap, int: i64) !Address {
    return try heap.new_fancy(TAG_INT, .{@as(Word, @bitCast(int))});
}
pub fn int_value(heap: *Heap, address: Address) i64 {
    return @bitCast(heap.get(address).literals[0]);
}

pub fn new_symbol(heap: *Heap, symbol: []const u8) !Address {
    const num_words = (symbol.len + 7) / 8;
    const words = try heap.ally.alloc(Word, num_words);
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(symbol.len - i * 8, 8)) |j|
            w |= @as(Word, symbol[i * 8 + j]) << @intCast(j * 8);
        words[i] = w;
    }
    const address = try heap.new(.{
        .tag = TAG_SYMBOL,
        .pointers = &[_]Address{},
        .literals = words,
    });
    heap.ally.free(words);
    return address;
}
// pub fn string_get(heap: *Heap, str: Object, index: Word) u8 {
//     const w = heap.get(str + 1 + @divFloor(index, 8));
//     const char = w >> @intCast(@mod(index, 8) * 8) & 0xff;
//     return @intCast(char);
// }
pub fn is_symbol_literal(heap: *Heap, symbol: Address, check: []const u8) bool {
    const num_words = (check.len + 7) / 8;
    const words = heap.ally.alloc(Word, num_words) catch @panic("oom");
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(check.len - i * 8, 8)) |j|
            w |= @as(Word, check[i * 8 + j]) << @intCast(j * 8);
        words[i] = w;
    }

    const matches = std.mem.eql(Word, words, heap.get(symbol).literals);
    heap.ally.free(words);
    return matches;
}

pub fn new_struct(heap: *Heap, fields: anytype) !Address {
    const field_types = @typeInfo(@TypeOf(fields)).@"struct".fields;
    const words = try heap.ally.alloc(Address, 2 * field_types.len);
    inline for (0.., field_types) |i, type_| {
        words[2 * i] = (try new_symbol(heap, type_.name));
        words[2 * i + 1] = @field(fields, type_.name);
    }
    return try heap.new(.{
        .tag = TAG_STRUCT,
        .pointers = words,
        .literals = &[_]Word{},
    });
}
pub fn field(heap: *Heap, struct_: Address, name: []const u8) Address {
    const pointers = heap.get(struct_).pointers;
    for (0..pointers.len / 2) |i| {
        const key = pointers[2 * i];
        const val = pointers[2 * i + 1];
        if (is_symbol_literal(heap, key, name)) return val;
    }
    @panic("field not in struct");
}

pub fn new_enum(heap: *Heap, variant: []const u8, payload: Address) !Address {
    return try heap.new_fancy(TAG_ENUM, .{
        try new_symbol(heap, variant),
        payload,
    });
}
pub fn get_variant(heap: Heap, enum_: Address) Address {
    return heap.get(enum_).pointers[0];
}
pub fn get_payload(heap: Heap, enum_: Address) Address {
    return heap.get(enum_).pointers[1];
}

fn new_ll(heap: *Heap, head: Address, tail: Address) Address {
    return try new_struct(heap, .{ .head = head, .tail = tail });
}

fn list_leaf_node(heap: *Heap, value: Address) !Address {
    return try new_enum(heap, "leaf", value);
}
fn list_empty_node(heap: *Heap) !Address {
    return try new_enum(
        heap,
        "empty",
        try new_struct(heap, .{}),
    );
}
fn list_inner_node(heap: *Heap, left: Address, right: Address) !Address {
    return try new_enum(
        heap,
        "inner",
        try new_struct(heap, .{
            .left = left,
            .right = right,
            .length = try new_int(
                heap,
                @intCast(list_len(heap, left) + list_len(heap, right)),
            ),
        }),
    );
}
pub fn new_empty_list(heap: *Heap) !Address {
    return list_empty_node(heap);
}
pub fn list_len(heap: *Heap, list: Address) i64 {
    const variant = get_variant(heap.*, list);
    if (is_symbol_literal(heap, variant, "empty")) {
        return 0;
    } else if (is_symbol_literal(heap, variant, "leaf")) {
        return 1;
    } else if (is_symbol_literal(heap, variant, "inner")) {
        const data = get_payload(heap.*, list);
        return int_value(heap, field(heap, data, "length"));
    } else {
        @panic("unknown list variant");
    }
}
pub fn list_push(heap: *Heap, list: Address, item: Address) !Address {
    const variant = get_variant(heap.*, list);
    if (is_symbol_literal(heap, variant, "empty")) {
        return try list_leaf_node(heap, item);
    } else if (is_symbol_literal(heap, variant, "leaf")) {
        return try list_inner_node(heap, list, try list_leaf_node(heap, item));
    } else if (is_symbol_literal(heap, variant, "inner")) {
        const data = get_payload(heap.*, list);
        const left = field(heap, data, "left");
        const right = field(heap, data, "right");
        if (list_len(heap, left) != list_len(heap, right)) {
            return try list_inner_node(
                heap,
                left,
                try list_push(heap, right, item),
            );
        } else {
            return try list_inner_node(heap, list, try list_leaf_node(heap, item));
        }
    } else {
        @panic("unknown list variant");
    }
}
pub fn list_get(heap: *Heap, list: Address, index: Word) Word {
    const variant = get_variant(heap, list);
    if (is_symbol_literal(heap, variant, "empty")) {
        @panic("tried to get item of empty list");
    } else if (is_symbol_literal(heap, variant, "leaf")) {
        return get_payload(heap, list);
    } else if (is_symbol_literal(heap, variant, "inner")) {
        const data = get_payload(heap, list);
        const left = field(heap, data, "left");
        const right = field(heap, data, "right");
        const left_len = list_len(heap, data, "left");
        return if (index < left_len)
            list_get(heap, left, index)
        else
            return list_get(heap, right, index - left_len);
    } else {
        @panic("unknown list variant");
    }
}

pub fn dump_object(heap: Heap, object: Address, indentation: usize) void {
    const allocation = heap.get(object);
    const tag = allocation.tag;
    for (0..indentation) |_| std.debug.print("  ", .{});
    switch (tag) {
        TAG_NIL => std.debug.print("nil\n", .{}),
        TAG_INT => std.debug.print("{}\n", .{allocation.literals[0]}),
        TAG_SYMBOL => {
            std.debug.print("symbol ", .{});
            dump_symbol(heap, object);
            std.debug.print("\n", .{});
        },
        TAG_STRUCT => {
            const num_fields: usize = allocation.pointers.len / 2;
            std.debug.print("&\n", .{});
            for (0..num_fields) |i| {
                const key = allocation.pointers[2 * i];
                const value = allocation.pointers[2 * i + 1];
                for (0..(indentation + 1)) |_| std.debug.print("  ", .{});
                dump_symbol(heap, key);
                std.debug.print(":\n", .{});
                dump_object(heap, value, indentation + 2);
            }
        },
        TAG_ENUM => {
            std.debug.print("| ", .{});
            dump_symbol(heap, get_variant(heap, object));
            std.debug.print(":\n", .{});
            dump_object(heap, get_payload(heap, object), indentation + 2);
        },
        else => @panic("unknown tag"),
    }
}
pub fn dump_symbol(heap: Heap, symbol: Address) void {
    const data = heap.get(symbol);
    for (data.literals) |word| {
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
