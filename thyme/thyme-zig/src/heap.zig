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
const Allocator = std.mem.Allocator;
const Writer = std.io.Writer;
const writeSliceEndian = Writer.writeSliceEndian;

pub const Word = u64;
pub const Address = Word;
const Heap = @This();

// The heap is word-based: Rather than addressing bytes, you always address
// entire words.

ally: Allocator,
memory: ArrayList(Word),

pub fn init(ally: Allocator) Heap {
    return .{
        .ally = ally,
        .memory = ArrayList(Word).empty,
    };
}

// The user-visible payload of an allocation. Note that because the allocation
// has separate lists of pointers and literals, you have no low-level control of
// the memory layout and can't mix pointers and literals.
pub const Allocation = struct {
    tag: u8,
    pointers: []const Address, // point to other heap allocations
    literals: []const Word, // word literals
};

// In the memory, every allocation has the following layout:
//
// [header][pointers][literals]

const Header = packed struct {
    num_pointers: u24,
    num_literals: u24,
    tag: u8,
    meta: packed struct {
        // meta stuff used by the heap itself
        marked: u1 = 0, // marking for mark-sweep garbage collection
        padding: u7 = 0,
    },
};
comptime {
    if (@sizeOf(Header) != @sizeOf(Word))
        @compileError("bad header layout");
}

pub fn new(heap: *Heap, allocation: Allocation) !Address {
    const address: Address = @intCast(heap.memory.items.len);
    const header = Header{
        .num_pointers = @intCast(allocation.pointers.len),
        .num_literals = @intCast(allocation.literals.len),
        .tag = allocation.tag,
        .meta = .{},
    };
    try heap.memory.append(heap.ally, @bitCast(header));
    for (allocation.pointers) |pointer|
        try heap.memory.append(heap.ally, pointer);
    for (allocation.literals) |literal|
        try heap.memory.append(heap.ally, literal);
    return address;
}

pub fn get(heap: Heap, address: Address) Allocation {
    const header: Header = @bitCast(heap.memory.items[address]);
    const pointers: []const Address = @ptrCast(
        heap.memory.items[address + 1 ..][0..@intCast(header.num_pointers)],
    );
    const literals: []const Word = @ptrCast(
        heap.memory.items[address + 1 ..][@intCast(header.num_pointers)..][0..@intCast(header.num_literals)],
    );
    return .{
        .tag = header.tag,
        .pointers = pointers,
        .literals = literals,
    };
}
pub fn load(heap: Heap, base: Address, word_index: usize) Word {
    return heap.memory.items[base + 1 + word_index];
}

const Checkpoint = struct { address: Word };

pub fn checkpoint(heap: Heap) Checkpoint {
    return .{ .address = heap.memory.items.len };
}

pub const Mapping = struct { from: Address, to: Address };

pub fn deduplicate(heap: *Heap, from: Checkpoint, ally: Allocator) !ArrayList(Mapping) {
    var read = from.address;
    var write = from.address;
    var map = ArrayList(Mapping).empty;
    while (read < heap.memory.items.len) {
        // Adjust the pointers using the mapping so far.
        const header: Header = @bitCast(heap.memory.items[read]);
        for (0..header.num_pointers) |j| {
            const pointer = heap.memory.items[read + 1 + j];
            for (map.items) |mapping| {
                if (mapping.from == pointer) {
                    heap.memory.items[read + 1 + j] = mapping.to;
                    break;
                }
            }
        }
        const total_words = 1 + header.num_pointers + header.num_literals;
        // Compare to existing objects.
        for (map.items) |mapping| {
            const candidate = mapping.to;
            const candidate_header: Header = @bitCast(heap.memory.items[candidate]);
            if (header.num_pointers != candidate_header.num_pointers) continue;
            if (header.num_literals != candidate_header.num_literals) continue;
            if (std.mem.eql(
                Word,
                heap.memory.items[read..][0..total_words],
                heap.memory.items[candidate..][0..total_words],
            )) {
                try map.append(ally, .{ .from = read, .to = candidate });
                break;
            }
        } else {
            // Not equal to an existing one. Copy it to the beginning of the
            // compressed heap.
            if (read > write) {
                for (0..total_words) |i|
                    heap.memory.items[write + i] = heap.memory.items[read + i];
            }
            try map.append(ally, .{ .from = read, .to = write });
            write += 1 + header.num_pointers + header.num_literals;
        }

        read += total_words;
    }
    heap.memory.items.len = write;
    return map;
}

pub fn garbage_collect(heap: *Heap, from: Checkpoint, keep: Address) !ArrayList(Mapping) {
    heap.mark(from.address, keep);
    return try heap.sweep(from.address);
}
fn mark(heap: *Heap, boundary: usize, address: usize) void {
    if (address < boundary) return;
    const header: *Header = @ptrCast(&heap.memory.items[address]);
    if (header.meta.marked == 1) return;
    header.meta.marked = 1;
    for (0..header.num_pointers) |i|
        heap.mark(boundary, @intCast(heap.memory.items[address + 1 + i]));
}
fn sweep(heap: *Heap, boundary: usize) !ArrayList(Mapping) {
    var read: usize = @intCast(boundary);
    var write: usize = @intCast(boundary);
    var map = ArrayList(Mapping).empty;
    while (read < heap.memory.items.len) {
        const header: *Header = @ptrCast(&heap.memory.items[read]);
        const size = 1 + header.num_pointers + header.num_literals;
        if (header.meta.marked == 1) {
            header.meta.marked = 0;
            if (read != write) {
                for (0..header.num_pointers) |j| {
                    const pointer = heap.memory.items[read + 1 + j];
                    for (map.items) |mapping| {
                        if (mapping.from == pointer) {
                            heap.memory.items[read + 1 + j] = mapping.to;
                            break;
                        }
                    }
                }
                for (0..size) |j|
                    heap.memory.items[write + j] = heap.memory.items[read + j];
                try map.append(heap.ally, .{ .from = read, .to = write });
            }
            read += size;
            write += size;
        } else {
            read += size;
        }
    }
    heap.memory.items.len = write;
    return map;
}

pub fn dump_raw(heap: Heap) void {
    for (0.., heap.memory.items) |i, w| {
        std.debug.print("{x:3} |", .{i});
        const bytes: []const u8 = @ptrCast(&w);
        for (bytes) |byte| std.debug.print(" {x:02}", .{byte});
        std.debug.print(" | {}\n", .{w});
    }
}
pub fn dump(heap: Heap) void {
    var i: usize = 0;
    while (i < heap.memory.items.len) {
        std.debug.print("{x:3} | ", .{i});
        const header: Header = @bitCast(heap.memory.items[i]);
        for (0..header.num_pointers) |j|
            std.debug.print(" *{x}", .{heap.memory.items[i + 1 + j]});
        for (0..header.num_literals) |j|
            std.debug.print(" {x}", .{heap.memory.items[i + 1 + header.num_pointers + j]});
        std.debug.print("\n", .{});
        i += 1 + header.num_pointers + header.num_literals;
    }
}
pub fn dump_stats(heap: Heap) void {
    const num_words = heap.memory.items.len;
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
        const header: Header = @bitCast(heap.memory.items[i]);
        i += 1 + header.num_pointers + header.num_literals;
    }
    std.debug.print(", {} objects\n", .{num_objects});
}
