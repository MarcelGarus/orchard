// The Heap
//
// This is a heap for immutable objects. When you allocate an object, you have
// to provide data to initialize the memory with. Afterwards, this memory can no
// longer be changed.
//
// While the heap has no understanding of what kinds objects it stores (structs,
// enums, lambdas, etc.), it does know how objects are connected. It knows what
// parts of the memory are pointers to other objects and what parts are literal
// values. Using this information, the heap can deduplicate objects and do
// garbage collection.

const std = @import("std");
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;
const Ally = std.mem.Allocator;
const Writer = std.io.Writer;
const writeSliceEndian = Writer.writeSliceEndian;

pub const Word = u64;
pub const Address = Word;
const Heap = @This();

// The heap is word-based: Rather than addressing bytes, you always address
// entire words.

memory: []Word,
used: usize,

pub fn init(ally: Ally, capacity: usize) !Heap {
    return .{
        .memory = try ally.alloc(Word, capacity),
        .used = 0,
    };
}

pub fn is_full(heap: Heap) bool {
    return heap.memory.len == heap.used;
}

// The user-visible payload of an allocation. Note that because the allocation
// has separate lists of pointers and literals, you have no low-level control of
// the memory layout and can't mix pointers and literals.
pub const Allocation = struct {
    tag: u8,
    has_pointers: bool,
    words: []const Word,
};

// In the memory, every allocation has the following layout:
//
// [header][word][word]...

const Header = packed struct {
    num_words: u48, // 6 bytes, max 281474976710655 words, ca. 2 PiB
    tag: u8, // gratis extra payload of allocation
    meta: packed struct {
        // meta stuff used by the heap itself
        marked: u1 = 0, // marking for mark-sweep garbage collection
        has_pointers: u1 = 0,
        padding: u6 = 0,
    },
};
comptime {
    if (@sizeOf(Header) != @sizeOf(Word))
        @compileError("bad header layout");
}

pub fn object_builder(heap: *Heap, tag: u8) !ObjectBuilder {
    return try ObjectBuilder.init(heap, tag);
}
pub const ObjectBuilder = struct {
    heap: *Heap,
    start: usize,
    num_pointers: usize = 0,
    num_literals: usize = 0,

    pub fn init(heap: *Heap, tag: u8) !ObjectBuilder {
        const start = heap.used;
        if (heap.is_full()) return error.OutOfMemory;
        heap.memory[start] = @bitCast(Header{
            .tag = tag,
            .num_words = 0,
            .meta = .{},
        });
        return .{ .heap = heap, .start = start };
    }
    fn emit(builder: *ObjectBuilder, word: Word) !void {
        const offset = builder.start + 1 + builder.num_pointers + builder.num_literals;
        if (offset == builder.heap.memory.len) return error.OutOfMemory;
        builder.heap.memory[offset] = word;
    }
    pub fn emit_pointer(builder: *ObjectBuilder, pointer: Address) !void {
        if (builder.num_literals > 0) @panic("pointer after literal");
        try builder.emit(pointer);
        builder.num_pointers += 1;
    }
    pub fn emit_literal(builder: *ObjectBuilder, word: Word) !void {
        if (builder.num_pointers > 0) @panic("literal after pointer");
        try builder.emit(word);
        builder.num_literals += 1;
    }
    pub fn finish(builder: *ObjectBuilder) !Address {
        const header: *Header = @ptrCast(&builder.heap.memory[builder.start]);
        const num_words = builder.num_pointers + builder.num_literals;
        header.meta.has_pointers = if (builder.num_pointers > 0) 1 else 0;
        header.num_words = @intCast(num_words);
        builder.heap.used += 1 + num_words;
        return builder.start;
    }
};

pub fn new(heap: *Heap, allocation: Allocation) !Address {
    var builder = try heap.object_builder(allocation.tag);
    if (allocation.has_pointers) {
        for (allocation.words) |word| try builder.emit_pointer(word);
    } else {
        for (allocation.words) |word| try builder.emit_literal(word);
    }
    return try builder.finish();
}

pub fn get(heap: Heap, address: Address) Allocation {
    const header: Header = @bitCast(heap.memory[address]);
    const words: []const Address = @ptrCast(
        heap.memory[address + 1 ..][0..@intCast(header.num_words)],
    );
    return .{
        .tag = header.tag,
        .has_pointers = header.meta.has_pointers == 1,
        .words = words,
    };
}
pub fn load(heap: Heap, base: Address, word_index: usize) Word {
    return heap.memory[base + 1 + word_index];
}

const Checkpoint = struct { used: Word };

pub fn checkpoint(heap: Heap) Checkpoint {
    return .{ .used = heap.used };
}

pub fn deduplicate(heap: *Heap, ally: Ally, from: Checkpoint) !Map(Address, Address) {
    var read = from.used;
    var write = from.used;
    var map = Map(Address, Address).init(ally);
    while (read < heap.used) {
        // Adjust the pointers using the mapping so far.
        const header: Header = @bitCast(heap.memory[read]);
        if (header.meta.has_pointers == 1) {
            for (0..header.num_words) |i| {
                const pointer = heap.memory[read + 1 + i];
                if (map.get(pointer)) |to| heap.memory[read + 1 + i] = to;
            }
        }
        const total_words = 1 + header.num_words;
        // Compare to existing objects.
        var it = map.iterator();
        while (it.next()) |mapping| {
            const candidate = mapping.value_ptr.*;
            const candidate_header: Header = @bitCast(heap.memory[candidate]);
            if (header.num_words != candidate_header.num_words) continue;
            if (std.mem.eql(
                Word,
                heap.memory[read..][0..total_words],
                heap.memory[candidate..][0..total_words],
            )) {
                try map.put(read, candidate);
                break;
            }
        } else {
            // Not equal to an existing one. Copy it to the beginning of the
            // compressed heap.
            if (read > write) {
                for (0..total_words) |i|
                    heap.memory[write + i] = heap.memory[read + i];
            }
            try map.put(read, write);
            write += 1 + header.num_words;
        }

        read += total_words;
    }
    heap.used = write;
    return map;
}

pub fn garbage_collect(
    heap: *Heap,
    ally: Ally,
    from: Checkpoint,
    keep: Address,
) !Map(Address, Address) {
    heap.mark(from.used, keep);
    return try heap.sweep(ally, from.used);
}
fn mark(heap: *Heap, boundary: usize, address: usize) void {
    if (address < boundary) return;
    const header: *Header = @ptrCast(&heap.memory[address]);
    if (header.meta.marked == 1) return;
    header.meta.marked = 1;
    if (header.meta.has_pointers == 1)
        for (0..header.num_words) |i|
            heap.mark(boundary, @intCast(heap.memory[address + 1 + i]));
}
fn sweep(heap: *Heap, ally: Ally, boundary: usize) !Map(Address, Address) {
    var read: usize = @intCast(boundary);
    var write: usize = @intCast(boundary);
    var map = Map(Address, Address).init(ally);
    while (read < heap.used) {
        const header: *Header = @ptrCast(&heap.memory[read]);
        const size = 1 + header.num_words;
        if (header.meta.marked == 1) {
            header.meta.marked = 0;
            if (read != write) {
                for (0..header.num_words) |i| {
                    const pointer = heap.memory[read + 1 + i];
                    if (map.get(pointer)) |to| heap.memory[read + 1 + i] = to;
                }
                for (0..size) |i| heap.memory[write + i] = heap.memory[read + i];
                try map.put(read, write);
            }
            read += size;
            write += size;
        } else {
            read += size;
        }
    }
    heap.used = write;
    return map;
}

pub fn dump_raw(heap: Heap) void {
    for (0.., heap.memory[0..heap.used]) |i, w| {
        std.debug.print("{x:3} |", .{i});
        const bytes: []const u8 = @ptrCast(&w);
        for (bytes) |byte| std.debug.print(" {x:02}", .{byte});
        std.debug.print(" | {}\n", .{w});
    }
}
pub fn dump(heap: Heap) void {
    var i: usize = 0;
    while (i < heap.used) {
        std.debug.print("{x:3} | ", .{i});
        const header: Header = @bitCast(heap.memory[i]);
        std.debug.print("[{x}] ({} words)", .{ header.tag, header.num_words });
        for (0..header.num_words) |j|
            std.debug.print(" {x}", .{heap.memory[i + 1 + j]});
        std.debug.print("\n", .{});
        i += 1 + header.num_words;
    }
}
pub fn dump_stats(heap: Heap) void {
    const num_words = heap.used;
    std.debug.print("{} words", .{num_words});

    std.debug.print(", ", .{});
    const num_bytes = num_words * 8;
    for ([_][]const u8{ "B", "KiB", "Mib", "GiB", "TiB" }, 0..) |unit, index| {
        const amount = num_bytes / std.math.pow(usize, 1024, index);
        if (amount < 1024) {
            std.debug.print("{} {s}", .{ amount, unit });
            break;
        }
    } else std.debug.print("a lot of memory", .{});

    var num_objects: usize = 0;
    var i: usize = 0;
    while (i < num_words) {
        num_objects += 1;
        const header: Header = @bitCast(heap.memory[i]);
        i += 1 + header.num_words;
    }
    std.debug.print(", {} objects\n", .{num_objects});
}

pub fn format(heap: Heap, object: Address, writer: *Writer) !void {
    try heap.format_indented(object, writer, 0);
}
pub fn format_indented(heap: Heap, object: Address, writer: *Writer, indentation: usize) error{WriteFailed}!void {
    if (indentation > 100) return error.WriteFailed;
    try writer.print("{x}: {}", .{ object, heap.get(object).tag });
    try writer.print("[\n", .{});
    const allocation = heap.get(object);
    const object_mod = @import("object.zig");
    const compiler_mod = @import("compiler.zig");
    if (allocation.tag == @intFromEnum(object_mod.Tag.fun)) {
        for (0..(indentation + 1)) |_| try writer.print("  ", .{});
        const ir_ptr = allocation.words[0];
        switch (@as(object_mod.Tag, @enumFromInt(heap.get(ir_ptr).tag))) {
            .int => {
                const ir: *compiler_mod.Ir = @ptrFromInt(@as(usize, @bitCast(
                    object_mod.get_int(heap, allocation.words[0]),
                )));
                try ir.format_indented(heap, writer, indentation + 1);
            },
            .composite => try writer.print("function without IR\n", .{}),
            else => unreachable,
        }
    } else {
        if (allocation.has_pointers) {
            for (allocation.words) |pointer| {
                for (0..(indentation + 1)) |_| try writer.print("  ", .{});
                try heap.format_indented(pointer, writer, indentation + 1);
                try writer.print("\n", .{});
            }
        } else {
            for (allocation.words) |literal| {
                for (0..(indentation + 1)) |_| try writer.print("  ", .{});
                try writer.print("{:<19} | {x:<16} | ", .{ literal, literal });
                for (0..8) |i| {
                    const c: u8 = @truncate(literal >> @intCast(i * 8));
                    if (c == 0) break;
                    try writer.print("{c}", .{c});
                }
                try writer.print("\n", .{});
            }
        }
    }
    for (0..indentation) |_| try writer.print("  ", .{});
    try writer.print("]", .{});
}
