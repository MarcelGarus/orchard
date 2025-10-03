// The Heap

const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Writer = std.io.Writer;
const writeSliceEndian = Writer.writeSliceEndian;

pub const Word = u64;
pub const Object = Word;
const Heap = @This();

ally: Allocator,
memory: ArrayList(Word),

pub fn init(ally: Allocator) Heap {
    return .{
        .ally = ally,
        .memory = ArrayList(Word).empty,
    };
}

pub fn get(heap: Heap, address: Word) Word {
    return heap.memory.items[@intCast(address)];
}

pub fn dump(heap: Heap) void {
    for (0.., heap.memory.items) |i, w| {
        std.debug.print("{:3} |", .{i});
        const bytes: []const u8 = @ptrCast(&w);
        for (bytes) |byte| std.debug.print(" {x:02}", .{byte});
        std.debug.print(" | {}\n", .{w});
    }
    var i: usize = 0;
    while (i < heap.memory.items.len) {
        std.debug.print("{:3} | ", .{i});
        const tag = heap.get_tag(@intCast(i));
        if (tag == TAG_WORD) {
            std.debug.print("word {}", .{heap.get(@intCast(i + 1))});
            i += 2;
        } else if (tag == TAG_WORDS) {
            std.debug.print("words ", .{});
            heap.dump_string(i);
            const len: usize = @intCast(heap.get_not_tag(@intCast(i)));
            i += 1 + len;
        } else if (tag == TAG_ARRAY) {
            const len: usize = @intCast(heap.get_not_tag(@intCast(i)));
            std.debug.print("array", .{});
            for (0..len) |j|
                std.debug.print(" *{}", .{heap.get(@intCast(i + 1 + j))});
            i += 1 + len;
        } else if (tag == TAG_STRUCT) {
            const num_fields: usize = @intCast(heap.get_not_tag(@intCast(i)));
            std.debug.print("struct", .{});
            for (0..num_fields) |j|
                std.debug.print(" *{}: *{}", .{
                    heap.get(@intCast(i + 1 + 2 * j)),
                    heap.get(@intCast(i + 1 + 2 * j + 1)),
                });
            i += 1 + 2 * num_fields;
        } else if (tag == TAG_ENUM) {
            std.debug.print("enum *{}: *{}", .{
                heap.get(@intCast(i + 1)),
                heap.get(@intCast(i + 2)),
            });
            i += 3;
        } else {
            @panic("unknown tag");
        }
        std.debug.print("\n", .{});
    }
}
pub fn dump_string(heap: Heap, str: Object) void {
    const len: usize = @intCast(heap.get_not_tag(str));
    for (0..len) |j| {
        const w = heap.get(@intCast(str + 1 + j));
        for (0..8) |k| {
            const c: u8 = @intCast((w >> @intCast(8 * k)) & 0xff);
            if (c == 0) break;
            if (true or c >= 32 and c <= 150)
                std.debug.print("{c}", .{c})
            else
                std.debug.print("?", .{});
        }
    }
}
pub fn dump_object(heap: Heap, object: Object, indentation: usize) void {
    const tag = heap.get_tag(object);
    for (0..indentation) |_| std.debug.print("  ", .{});
    if (tag == TAG_WORD) {
        std.debug.print("word {}\n", .{heap.get(object + 1)});
    } else if (tag == TAG_WORDS) {
        std.debug.print("words ", .{});
        heap.dump_string(object);
        std.debug.print("\n", .{});
    } else if (tag == TAG_ARRAY) {
        const len: usize = @intCast(heap.get_not_tag(@intCast(object)));
        std.debug.print("array\n", .{});
        for (0..len) |j|
            heap.dump_object(heap.get(@intCast(object + 1 + j)), indentation + 1);
    } else if (tag == TAG_STRUCT) {
        const num_fields: usize = @intCast(heap.get_not_tag(@intCast(object)));
        std.debug.print("struct\n", .{});
        for (0..num_fields) |j| {
            const key = heap.get(@intCast(object + 1 + 2 * j));
            const value = heap.get(@intCast(object + 1 + 2 * j + 1));
            for (0..(indentation + 1)) |_| std.debug.print("  ", .{});
            heap.dump_string(key);
            std.debug.print(":\n", .{});
            heap.dump_object(value, indentation + 2);
        }
    } else if (tag == TAG_ENUM) {
        std.debug.print("enum ", .{});
        heap.dump_string(heap.variant(object));
        std.debug.print(":\n", .{});
        heap.dump_object(heap.payload(object), indentation + 1);
    } else {
        @panic("unknown tag");
    }
}

// Memory layouts of heap objects:
// word:   0 word
// words:  1&length word word ...
// array:  2&length item item ...
// struct: 3&num_fields key value key value ...
// enum:   4 variant(words) payload
// lambda: 5 ir(ir) closure(array)

const TAG_MASK = 0xff_00000000000000;
const NOT_TAG_MASK = 0x00_ffffffffffffff;

const TAG_WORD = 0x00_00000000000000;
const TAG_WORDS = 0x01_00000000000000;
const TAG_ARRAY = 0x02_00000000000000;
const TAG_STRUCT = 0x03_00000000000000;
const TAG_ENUM = 0x04_00000000000000;
const TAG_LAMBDA = 0x05_00000000000000;

pub fn get_tag(heap: Heap, address: Word) Word {
    return heap.get(address) & TAG_MASK;
}
pub fn get_not_tag(heap: Heap, address: Word) Word {
    return heap.get(address) & NOT_TAG_MASK;
}

pub fn word(heap: *Heap, w: Word) !Object {
    const object: Object = @intCast(heap.memory.items.len);
    try heap.memory.append(heap.ally, TAG_WORD);
    try heap.memory.append(heap.ally, w);
    return object;
}
pub fn word_value(heap: *Heap, w: Object) Word {
    return heap.get(w + 1);
}

pub fn words(heap: *Heap, w: []const Word) !Object {
    const object: Object = @intCast(heap.memory.items.len);
    try heap.memory.append(heap.ally, TAG_WORDS | @as(Word, @intCast(w.len)));
    for (w) |ww| try heap.memory.append(heap.ally, ww);
    return object;
}

pub fn string(heap: *Heap, str: []const u8) !Object {
    const num_words = (str.len + 7) / 8;
    const object: Object = @intCast(heap.memory.items.len);
    try heap.memory.append(heap.ally, TAG_WORDS | @as(Word, @intCast(num_words)));
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(str.len - i * 8, 8)) |j|
            w |= @as(Word, str[i * 8 + j]) << @intCast(j * 8);
        try heap.memory.append(heap.ally, @intCast(w));
    }
    return object;
}
pub fn string_get(heap: *Heap, str: Object, index: Word) u8 {
    const w = heap.get(str + 1 + @divFloor(index, 8));
    const char = w >> @intCast(@mod(index, 8) * 8) & 0xff;
    return @intCast(char);
}
pub fn string_equals(heap: *Heap, str: Object, matches: []const u8) bool {
    const num_words_heap = heap.get_not_tag(str);
    const num_words_match = @divFloor(matches.len + 7, 8);
    if (num_words_heap != num_words_match)
        return false;
    for (0..matches.len) |i| {
        if (heap.string_get(str, i) != matches[i])
            return false;
    }
    return true;
}

pub fn array(heap: *Heap, items: []const Object) !Object {
    const object: Object = @intCast(heap.memory.items.len);
    try heap.memory.append(heap.ally, TAG_ARRAY | @as(Word, @intCast(items.len)));
    for (items) |item| try heap.memory.append(heap.ally, item);
    return object;
}

pub fn struct_(heap: *Heap, fields: anytype) !Object {
    const field_types = @typeInfo(@TypeOf(fields)).@"struct".fields;
    var field_names: [field_types.len]Object = .{@as(Object, undefined)} ** field_types.len;
    inline for (0.., field_types) |i, type_| {
        field_names[i] = try heap.string(type_.name);
    }

    const object: Object = @intCast(heap.memory.items.len);
    try heap.memory.append(heap.ally, TAG_STRUCT | @as(Word, @intCast(field_types.len)));
    inline for (0.., field_types) |i, field_type| {
        try heap.memory.append(heap.ally, field_names[i]);
        try heap.memory.append(heap.ally, @field(fields, field_type.name));
    }
    return object;
}
pub fn field(heap: *Heap, s: Object, name: []const u8) Object {
    const len: usize = @intCast(heap.get_not_tag(s));
    for (0..len) |i| {
        const name_ = heap.get(s + 1 + 2 * @as(Word, @intCast(i)));
        if (heap.string_equals(name_, name))
            return heap.get(s + 1 + 2 * @as(Word, @intCast(i)) + 1);
    }
    @panic("field not in struct");
}

pub fn enum_(heap: *Heap, var_: []const u8, pay: Object) !Object {
    const variant_str = try heap.string(var_);
    const object: Object = @intCast(heap.memory.items.len);
    try heap.memory.append(heap.ally, TAG_ENUM);
    try heap.memory.append(heap.ally, variant_str);
    try heap.memory.append(heap.ally, pay);
    return object;
}
pub fn variant(heap: Heap, en: Object) Word {
    return heap.get(en + 1);
}
pub fn payload(heap: Heap, en: Object) Word {
    return heap.get(en + 2);
}

pub fn lambda(heap: *Heap, ir: Object, closure: Object) !Object {
    const object: Object = @intCast(heap.memory.items.len);
    try heap.memory.append(heap.ally, TAG_LAMBDA);
    try heap.memory.append(heap.ally, ir);
    try heap.memory.append(heap.ally, closure);
    return object;
}

fn list_leaf_node(heap: *Heap, value: Object) !Object {
    return try heap.enum_("leaf", value);
}
fn list_empty_node(heap: *Heap) !Object {
    const empty_struct = try heap.struct_(.{});
    return try heap.enum_("empty", empty_struct);
}
fn list_inner_node(heap: *Heap, left: Object, right: Object) !Object {
    return try heap.enum_(
        "inner",
        try heap.struct_(.{
            .left = left,
            .right = right,
            .length = try heap.word(heap.list_len(left) + heap.list_len(right)),
        }),
    );
}
pub fn empty_list(heap: *Heap) !Object {
    return heap.list_empty_node();
}
pub fn list_len(heap: *Heap, list: Object) Word {
    const var_ = heap.variant(list);
    if (heap.string_equals(var_, "empty")) {
        return 0;
    } else if (heap.string_equals(var_, "leaf")) {
        return 1;
    } else if (heap.string_equals(var_, "inner")) {
        const data = heap.payload(list);
        return heap.word_value(heap.field(data, "length"));
    } else {
        @panic("unknown list variant");
    }
}
pub fn list_push(heap: *Heap, list: Object, item: Object) !Object {
    const var_ = heap.variant(list);
    if (heap.string_equals(var_, "empty")) {
        return try heap.list_leaf_node(item);
    } else if (heap.string_equals(var_, "leaf")) {
        return try heap.list_inner_node(list, try heap.list_leaf_node(item));
    } else if (heap.string_equals(var_, "inner")) {
        const data = heap.payload(list);
        const left = heap.field(data, "left");
        const right = heap.field(data, "right");
        if (heap.list_len(left) != heap.list_len(right)) {
            return try heap.list_inner_node(
                left,
                try heap.list_push(right, item),
            );
        } else {
            return try heap.list_inner_node(list, try heap.list_leaf_node(item));
        }
    } else {
        @panic("unknown list variant");
    }
}
pub fn list_get(heap: *Heap, list: Object, index: Word) Word {
    const var_ = heap.variant(list);
    if (heap.string_equals(var_, "empty")) {
        @panic("tried to get item of empty list");
    } else if (heap.string_equals(var_, "leaf")) {
        return heap.payload(list);
    } else if (heap.string_equals(var_, "inner")) {
        const data = heap.payload(list);
        const left = heap.field(data, "left");
        const right = heap.field(data, "right");
        const left_len = heap.list_len(data, "left");
        return if (index < left_len)
            heap.list_get(left, index)
        else
            return heap.list_get(right, index - left_len);
    } else {
        @panic("unknown list variant");
    }
}
