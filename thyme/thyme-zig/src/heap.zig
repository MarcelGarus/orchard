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
    return .{ .memory = try ally.alloc(Word, capacity), .used = 0 };
}

pub fn is_full(heap: Heap) bool {
    return heap.memory.len == heap.used;
}

// The user-visible payload of an allocation. Note that because the allocation
// has separate lists of pointers and literals, you have no low-level control of
// the memory layout and can't mix pointers and literals.
pub const Allocation = struct {
    has_pointers: bool,
    words: []const Word,
};

// In the memory, every allocation has the following layout:
//
// [header][word][word]...

const Header = packed struct {
    num_words: u48, // 6 bytes, max 281474976710655 words, ca. 2 PiB
    padding1: u8 = 0,
    padding2: u6 = 0,
    marked: u1 = 0, // marking for mark-sweep garbage collection
    has_pointers: u1 = 0,
};
comptime {
    if (@sizeOf(Header) != @sizeOf(Word)) @compileError("bad header layout");
}

pub fn object_builder(heap: *Heap) !ObjectBuilder {
    return try ObjectBuilder.init(heap);
}
pub const ObjectBuilder = struct {
    heap: *Heap,
    start: usize,
    num_pointers: usize = 0,
    num_literals: usize = 0,

    pub fn init(heap: *Heap) !ObjectBuilder {
        const start = heap.used;
        if (heap.is_full()) return error.OutOfMemory;
        heap.memory[start] = @bitCast(Header{ .num_words = 0 });
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
    pub fn finish(builder: *ObjectBuilder) Address {
        const header: *Header = @ptrCast(&builder.heap.memory[builder.start]);
        const num_words = builder.num_pointers + builder.num_literals;
        header.has_pointers = if (builder.num_pointers > 0) 1 else 0;
        header.num_words = @intCast(num_words);
        builder.heap.used += 1 + num_words;
        return builder.start;
    }
};

pub fn new(heap: *Heap, allocation: Allocation) !Address {
    var builder = try heap.object_builder();
    if (allocation.has_pointers) {
        for (allocation.words) |word| try builder.emit_pointer(word);
    } else {
        for (allocation.words) |word| try builder.emit_literal(word);
    }
    return builder.finish();
}
pub fn new_literals(heap: *Heap, literals: []const Word) !Address {
    var builder = try heap.object_builder();
    for (literals) |literal| try builder.emit_literal(literal);
    return builder.finish();
}
pub fn new_pointers(heap: *Heap, pointers: []const Word) !Address {
    var builder = try heap.object_builder();
    for (pointers) |pointer| try builder.emit_pointer(pointer);
    return builder.finish();
}

pub fn get(heap: Heap, address: Address) Allocation {
    const header: Header = @bitCast(heap.memory[address]);
    const words: []const Address = @ptrCast(
        heap.memory[address + 1 ..][0..@intCast(header.num_words)],
    );
    return .{ .has_pointers = header.has_pointers == 1, .words = words };
}
pub fn load(heap: Heap, base: Address, word_index: usize) Word {
    return heap.memory[base + 1 + word_index];
}

pub const Checkpoint = struct { used: Word };

pub fn checkpoint(heap: Heap) Checkpoint {
    return .{ .used = heap.used };
}

pub fn restore(heap: *Heap, checkpoint_: Checkpoint) void {
    heap.used = checkpoint_.used;
}

pub fn deduplicate(heap: *Heap, ally: Ally, from: Checkpoint) !Map(Address, Address) {
    var read = from.used;
    var write = from.used;
    var map = Map(Address, Address).init(ally);
    while (read < heap.used) {
        // Adjust the pointers using the mapping so far.
        const header: Header = @bitCast(heap.memory[read]);
        if (header.has_pointers == 1) {
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
) !Address {
    heap.mark(from.used, keep);
    return try heap.sweep(ally, from.used, keep);
}
fn mark(heap: *Heap, boundary: Address, address: Address) void {
    if (address < boundary) return;
    const header: *Header = @ptrCast(&heap.memory[address]);
    if (header.marked == 1) return;
    header.marked = 1;
    if (header.has_pointers == 1)
        for (0..header.num_words) |i|
            heap.mark(boundary, @intCast(heap.memory[address + 1 + i]));
}
fn sweep(heap: *Heap, ally: Ally, boundary: Address, keep: Address) !Address {
    var read = boundary;
    var write = boundary;
    var mapping = Map(Address, Address).init(ally);
    defer mapping.deinit();
    while (read < heap.used) {
        const header: *Header = @ptrCast(&heap.memory[read]);
        const size = 1 + header.num_words;
        if (header.marked == 1) {
            header.marked = 0;
            if (read != write) {
                if (header.has_pointers == 1) {
                    for (0..header.num_words) |i| {
                        const pointer = heap.memory[read + 1 + i];
                        if (mapping.get(pointer)) |to| heap.memory[read + 1 + i] = to;
                    }
                }
                for (0..size) |i| heap.memory[write + i] = heap.memory[read + i];
                try mapping.put(read, write);
            }
            read += size;
            write += size;
        } else {
            read += size;
        }
    }
    heap.used = write;
    return mapping.get(keep) orelse keep;
}

pub fn copy_to_other(heap: *Heap, ally: Ally, other: *Heap, address: Address) Address {
    heap.mark(0, address);
    var cursor = 0;
    var mapping = Map(Address, Address).init(ally);
    defer mapping.deinit();
    while (cursor < heap.used) {
        const header: *Header = @ptrCast(&heap.memory[cursor]);
        const size = 1 + header.num_words;
        if (header.marked == 1) {
            header.marked = 0;
            var b = other.object_builder();
            for (0..header.num_words) |i| {
                const word = heap.load(cursor, i);
                if (header.has_pointers)
                    b.emit_pointer(mapping.get(word) orelse unreachable)
                else
                    b.emit_literal(word);
            }
            try mapping.put(cursor, b.finish());
        }
        cursor += size;
    }
    return mapping.get(address);
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
        std.debug.print("({} words)", .{header.num_words});
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

const max_nesting = 30;
pub fn format(heap: Heap, object: Address, writer: *Writer) !void {
    var nesting = [_]Parent{.{ .first = false, .color = -1 }} ** max_nesting;
    try heap.format_indented(object, writer, &nesting, 0);
}
const Parent = struct { first: bool, color: i8 };
fn print_indentation(writer: *Writer, parents: *[max_nesting]Parent, indentation: usize) !void {
    for (parents[0..indentation]) |p| {
        try writer.print("{c}[{d}m", .{ 27, @as(usize, switch (p.color) {
            0 => 93,
            1 => 91,
            2 => 94,
            3 => 92,
            else => unreachable,
        }) });
        try if (p.first) writer.print("𜸖", .{}) else writer.print("│", .{});
        try writer.print("{c}[0m", .{27});
    }
    for (0..indentation) |j| parents[j].first = false;
    for (indentation..max_nesting + 1) |_| try writer.print(" ", .{});
    for (indentation..max_nesting) |j| parents[j].color = -1;
}
pub fn format_indented(
    heap: Heap,
    object: Address,
    writer: *Writer,
    parents: *[max_nesting]Parent,
    indentation: usize,
) error{WriteFailed}!void {
    const allocation = heap.get(object);

    parents[indentation] = .{
        .first = true,
        .color = find_color: {
            if (indentation == 0) break :find_color 0;
            const top = parents[indentation].color;
            const left = parents[indentation - 1].color;
            break :find_color if (top != left and parents[indentation - 1].first) left else @mod(top + 1, 4);
        },
    };
    const indent = indentation + 1;

    if (indent == max_nesting) {
        try print_indentation(writer, parents, indent);
        try writer.print("...", .{});
        return;
    }

    if (allocation.words.len == 0) {
        try print_indentation(writer, parents, indent);
        try writer.print("nothing", .{});
        return;
    }

    if (allocation.has_pointers) {
        for (0.., allocation.words) |i, pointer| {
            if (i > 0) try writer.print("\n", .{});
            // for (nesting[0..indent]) |c| try writer.print("{c}", .{c});
            try heap.format_indented(pointer, writer, parents, indent);
        }
    } else {
        for (0.., allocation.words) |i, literal| {
            if (i > 0) try writer.print("\n", .{});
            try print_indentation(writer, parents, indent);
            try writer.print("{:<19} | {x:<16} | ", .{ literal, literal });
            for (0..8) |j| {
                const c: u8 = @truncate(literal >> @intCast(j * 8));
                if (c == 0) break;
                try writer.print("{c}", .{if (c >= 32 and c <= 126) c else '.'});
            }
        }
    }
}

pub fn dump_obj(heap: Heap, address: Address) void {
    var buffer: [64]u8 = undefined;
    const bw = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    heap.format(address, bw) catch return;
    bw.print("\n", .{}) catch return;
}
