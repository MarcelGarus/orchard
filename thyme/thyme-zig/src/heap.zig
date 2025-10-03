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
pub const Address = packed struct { address: Word };
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

// The user-visible payload of an allocation. Note that the allocation has a
// separate list of pointers and literals. In particular, you have no low-level
// control of the memory layout and can't intertwine pointers and literals.
const Allocation = struct {
    tag: u8, // space you can use to tag types of objects
    pointers: []const Address, // point to other heap allocations
    literals: []const Word, // word literals
};

// In the memory, every allocation has the following layout:
//
// [header][pointers][literals]

const Header = packed struct {
    num_words: u24, // total number of words after the header
    num_pointers: u24, // how many of those words are pointers
    meta: packed struct {
        // meta stuff used by the heap itself
        marked: u1 = 0, // marking for mark-sweep garbage collection
        padding: u7 = 0,
    },
    tag: u8,
};
comptime {
    if (@sizeOf(Header) != @sizeOf(Word))
        @compileError("bad header layout");
}

pub fn new(heap: *Heap, allocation: Allocation) !Address {
    const id: Address = .{ .address = @intCast(heap.memory.items.len) };
    const header = Header{
        .num_words = @intCast(allocation.pointers.len + allocation.literals.len),
        .num_pointers = @intCast(allocation.pointers.len),
        .tag = allocation.tag,
        .meta = .{},
    };
    try heap.memory.append(heap.ally, @bitCast(header));
    for (allocation.pointers) |pointer|
        try heap.memory.append(heap.ally, pointer.address);
    for (allocation.literals) |literal|
        try heap.memory.append(heap.ally, literal);
    return id;
}

pub fn new_fancy(heap: *Heap, tag: u8, words: anytype) !Address {
    const field_infos = @typeInfo(@TypeOf(words)).@"struct".fields;
    comptime var had_literal = false;
    comptime var num_pointers = field_infos.len;
    inline for (0.., field_infos) |i, field| {
        const value = @field(words, field.name);
        if (@TypeOf(value) == Address) {
            if (had_literal) {
                @compileLog(field_infos, @TypeOf(value));
                @compileError("you can only store pointers followed by words, not mix them");
            }
        } else if (@TypeOf(value) == Word or @TypeOf(value) == comptime_int) {
            if (!had_literal) {
                had_literal = true;
                num_pointers = i;
            }
        } else {
            @compileLog(@TypeOf(value));
            @compileError("can only contain pointers and words");
        }
    }
    const id: Address = .{ .address = @intCast(heap.memory.items.len) };
    const header = Header{
        .num_words = @intCast(field_infos.len),
        .num_pointers = @intCast(num_pointers),
        .meta = .{},
        .tag = tag,
    };
    try heap.memory.append(heap.ally, @bitCast(header));
    inline for (field_infos) |field| {
        const value = @field(words, field.name);
        if (@TypeOf(value) == Address)
            try heap.memory.append(heap.ally, value.address)
        else if (@TypeOf(value) == Word or @TypeOf(value) == comptime_int)
            try heap.memory.append(heap.ally, value)
        else
            unreachable;
    }
    return id;
}

pub fn get(heap: Heap, id: Address) Allocation {
    const header: Header = @bitCast(heap.memory.items[id.address]);
    const pointers: []const Address = @ptrCast(
        heap.memory.items[id.address + 1 ..][0..@intCast(header.num_pointers)],
    );
    const literals: []const Word = @ptrCast(
        heap.memory.items[id.address + 1 ..][0..@intCast(header.num_words)][@intCast(header.num_pointers)..],
    );
    return .{
        .tag = header.tag,
        .pointers = pointers,
        .literals = literals,
    };
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
                if (mapping.from.address == pointer) {
                    heap.memory.items[read + 1 + j] = mapping.to.address;
                    break;
                }
            }
        }
        // Compare to existing objects.
        for (map.items) |mapping| {
            const candidate = mapping.to.address;
            const candidate_header: Header = @bitCast(heap.memory.items[candidate]);
            if (header.num_words != candidate_header.num_words) continue;
            const total_words = 1 + header.num_words;
            if (std.mem.eql(
                Word,
                heap.memory.items[read..][0..total_words],
                heap.memory.items[candidate..][0..total_words],
            )) {
                try map.append(ally, .{
                    .from = .{ .address = read },
                    .to = .{ .address = candidate },
                });
                break;
            }
        } else {
            // Not equal to an existing one. Copy it to the beginning of the
            // compressed heap.
            if (read > write) {
                for (0..(1 + header.num_words)) |i|
                    heap.memory.items[write + i] = heap.memory.items[read + i];
            }
            try map.append(ally, .{
                .from = .{ .address = read },
                .to = .{ .address = write },
            });
            write += 1 + header.num_words;
        }

        read += 1 + header.num_words;

        // std.debug.print(" *{}", .{heap.memory.items[i + 1 + j]});
        // for (header.num_pointers..header.num_words) |j|
        // std.debug.print(" {}", .{heap.memory.items[i + 1 + j]});
        // std.debug.print("\n", .{});
        // i += 1 + header.num_words;
    }
    heap.memory.items.len = write;
    return map;
}

pub fn dump_raw(heap: Heap) void {
    for (0.., heap.memory.items) |i, w| {
        std.debug.print("{:3} |", .{i});
        const bytes: []const u8 = @ptrCast(&w);
        for (bytes) |byte| std.debug.print(" {x:02}", .{byte});
        std.debug.print(" | {}\n", .{w});
    }
}
pub fn dump(heap: Heap) void {
    var i: usize = 0;
    while (i < heap.memory.items.len) {
        std.debug.print("{:3} | ", .{i});
        const header: Header = @bitCast(heap.memory.items[i]);
        std.debug.print("[{}]", .{header.tag});
        for (0..header.num_pointers) |j|
            std.debug.print(" *{}", .{heap.memory.items[i + 1 + j]});
        for (header.num_pointers..header.num_words) |j|
            std.debug.print(" {}", .{heap.memory.items[i + 1 + j]});
        std.debug.print("\n", .{});
        i += 1 + header.num_words;
    }
}
