// The Heap
//
// This is a heap for immutable objects. When you allocate an object, you have to provide data to initialize
// the memory with. Afterwards, this memory can no longer be changed.
// While the heap has no understanding of what kinds of objects it stores (structs, enums, lambdas, etc.),
// it does know how objects are connected. It knows what parts of the memory are pointers to other
// objects and what parts are literal values. Using this information, the heap can deduplicate objects and
// do garbage collection.
//
// The heap supports two kinds of objects:
// - Leaf objects contain literal words.
// - Inner objects contain pointers to other objects.

const std = @import("std");
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;
const Ally = std.mem.Allocator;
const Writer = std.io.Writer;
const writeSliceEndian = Writer.writeSliceEndian;

const Heap = @This();

comptime {
    if (@sizeOf(usize) != @sizeOf(u64)) @compileError("The heap only works on 64-bit systems.");
}

// The heap is word-based.
pub const Word = usize;

// In memory, every object has the following layout:
//
// [header][word][word]...

pub const Header = packed struct {
    num_words: u48, // 6 bytes, max 281474976710655 words, ca. 2 PiB
    padding1: u8 = 0,
    padding2: u6 = 0,
    marked: u1 = 0, // marking for mark-sweep garbage collection
    is_inner: u1, // is this an inner object (1) or a leaf object (0)?
};
comptime {
    if (@sizeOf(Header) != @sizeOf(Word)) @compileError("bad header layout");
}

pub const Obj = packed struct {
  address: Word,

  pub fn is_same(a: Obj, b: Obj) bool {
    return a.address == b.address;
  }

  pub fn is_inner(self: Obj) bool {
    const header: *Header = @ptrFromInt(self.address);
    return header.is_inner == 1;
  }
  pub fn is_leaf(self: Obj) bool {
    return !self.is_inner();
  }

  pub fn size(self: Obj) usize {
    const header: *Header = @ptrFromInt(self.address);
    return @intCast(header.num_words);
  }

  pub fn words(self: Obj) []const Word {
    if (!self.is_leaf()) unreachable;
    const header: *Header = @ptrFromInt(self.address);
    return @as([*]Word, @ptrFromInt(self.address))[1..][0..header.num_words];
  }

  pub fn children(self: Obj) []const Obj {
    if (!self.is_inner()) unreachable;
    const header: *Header = @ptrFromInt(self.address);
    return @as([*]Obj, @ptrFromInt(self.address))[1..][0..header.num_words];
  }

  pub fn word(self: Obj, index: usize) Word {
    return self.words()[index];
  }
  pub fn child(self: Obj, index: usize) Obj {
    return self.children()[index];
  }
};

memory: []Word,
used: usize,

pub fn init(ally: Ally, capacity: usize) !Heap {
    const memory = try ally.alloc(Word, capacity);
    return .{ .memory = memory, .used = 0 };
}

pub fn is_full(heap: Heap) bool {
    return heap.used == heap.memory.len;
}
fn emit(heap: *Heap, word: Word) !void {
  if (heap.is_full()) return error.OutOfMemory;
  heap.memory[heap.used] = word;
  heap.used += 1;
}

pub fn build_leaf(heap: *Heap) !LeafObjBuilder {
  const start = heap.used;
  try heap.emit(@bitCast(Header{ .num_words = 0, .is_inner = 0 }));
  return .{ .heap = heap, .start = start, .num_words = 0 };
}
pub const LeafObjBuilder = struct {
    heap: *Heap,
    start: Word,
    num_words: usize,

    pub fn emit(builder: *LeafObjBuilder, word: Word) !void {
        try builder.heap.emit(word);
        builder.num_words += 1;
    }
    pub fn finish(builder: *LeafObjBuilder) Obj {
        const header: *Header = @ptrCast(&builder.heap.memory[builder.start]);
        header.num_words = @intCast(builder.num_words);
        return .{ .address = @intFromPtr(header) };
    }
};
pub fn new_leaf(heap: *Heap, words: []const Word) !Obj {
    var builder = try heap.build_leaf();
    for (words) |word| try builder.emit(word);
    return builder.finish();
}

pub fn build_inner(heap: *Heap) !InnerObjBuilder {
  const start = heap.used;
  try heap.emit(@bitCast(Header{ .num_words = 0, .is_inner = 1 }));
  return .{ .heap = heap, .start = start, .num_words = 0 };
}
pub const InnerObjBuilder = struct {
    heap: *Heap,
    start: usize,
    num_words: usize,

    pub fn emit(builder: *InnerObjBuilder, obj: Obj) !void {
        try builder.heap.emit(obj.address);
        builder.num_words += 1;
    }
    pub fn finish(builder: *InnerObjBuilder) Obj {
        const header: *Header = @ptrCast(&builder.heap.memory[builder.start]);
        header.num_words = @intCast(builder.num_words);
        return .{ .address = @intFromPtr(header) };
    }
};
pub fn new_inner(heap: *Heap, objs: []const Obj) !Obj {
    var builder = try heap.build_inner();
    for (objs) |obj| try builder.emit(obj);
    return builder.finish();
}

pub const Checkpoint = struct { address: Word };

pub fn checkpoint(heap: Heap) Checkpoint {
    return .{ .address = @intFromPtr(heap.memory.ptr) + 8 * heap.used };
}
pub fn restore(heap: *Heap, checkpoint_: Checkpoint) void {
    heap.used = (checkpoint_.address - @intFromPtr(heap.memory.ptr)) / 8;
}

pub fn deduplicate(heap: *Heap, ally: Ally, from: Checkpoint) !Map(Obj, Obj) {
    var read = from.address;
    var write = from.address;
    var map = Map(Obj, Obj).init(ally);
    while (read < @intFromPtr(heap.memory.ptr) + heap.used) {
        // Adjust the pointers using the mapping so far.
        const header: Header = @as(*Header, @ptrFromInt(read)).*;
        if (header.is_inner == 1) {
            for (0..header.num_words) |i| {
                const ptr: *Obj = @ptrFromInt(read + 8 * (1 + i));
                if (map.get(ptr.*)) |to| ptr.* = to;
            }
        }
        const total_words = 1 + header.num_words;
        // Compare to existing objects.
        var it = map.iterator();
        while (it.next()) |mapping| {
            const candidate = mapping.value_ptr.*;
            const candidate_header: Header = @as(*Header, @ptrFromInt(candidate.address)).*;
            if (header.num_words != candidate_header.num_words) continue;
            if (std.mem.eql(
                Word,
                @as([*]Word, @ptrFromInt(read))[0..total_words],
                @as([*]Word, @ptrFromInt(candidate.address))[0..total_words],
            )) {
                try map.put(@bitCast(read), candidate);
                break;
            }
        } else {
            // Not equal to an existing one. Copy it to the beginning of the compressed heap.
            if (read > write) {
                for (0..total_words) |i|
                    @as(*Word, @ptrFromInt(write + 8 * i)).* = @as(*Word, @ptrFromInt(read + 8 * i)).*;
            }
            try map.put(@bitCast(read), @bitCast(write));
            write += 8 * (1 + header.num_words);
        }
        read += 8 * total_words;
    }
    heap.used = write - @intFromPtr(heap.memory.ptr);
    return map;
}

pub fn garbage_collect(
    heap: *Heap,
    ally: Ally,
    from: Checkpoint,
    keep: Obj,
) !Obj {
    mark(from.address, keep.address);
    return try heap.sweep(ally, from.address, keep.address);
}
fn mark(boundary: Word, address: Word) void {
    if (address < boundary) return;
    const header: *Header = @ptrFromInt(address);
    if (header.marked == 1) return;
    header.marked = 1;
    if (header.is_inner == 1)
        for (0..header.num_words) |i|
            mark(boundary, address + 8 * (1 + i));
}
fn sweep(heap: *Heap, ally: Ally, boundary: Word, keep: Word) !Obj {
    var read = boundary;
    var write = boundary;
    var mapping = Map(Word, Word).init(ally);
    defer mapping.deinit();
    while (read < heap.used) {
        const header: *Header = @ptrFromInt(read);
        const size = 1 + header.num_words;
        if (header.marked == 1) {
            header.marked = 0;
            if (read != write) {
                if (header.is_inner == 1) {
                    for (0..header.num_words) |i| {
                        const ptr: *Word = @ptrFromInt(read + 8 * (1 + i));
                        if (mapping.get(ptr.*)) |to| ptr.* = to;
                    }
                }
                for (0..size) |i| @as(*Word, @ptrFromInt(write + 8 * i)).* = @as(*Word, @ptrFromInt(read + 8 * i)).*;
                try mapping.put(read, write);
            }
            read += 8 * size;
            write += 8 * size;
        } else {
            read += 8 * size;
        }
    }
    heap.used = write;
    return .{ .address = mapping.get(keep) orelse keep };
}

// pub fn copy_to_other(heap: *Heap, ally: Ally, other: *Heap, address: Ptr) Ptr {
//     heap.mark(0, address);
//     var cursor = 0;
//     var mapping = Map(Ptr, Ptr).init(ally);
//     defer mapping.deinit();
//     while (cursor < heap.used) {
//         const header: *Header = @ptrCast(&heap.memory[cursor]);
//         const size = 1 + header.num_words;
//         if (header.marked == 1) {
//             header.marked = 0;
//             var b = other.object_builder();
//             for (0..header.num_words) |i| {
//                 const word = heap.load(cursor, i);
//                 if (header.has_pointers)
//                     b.emit_pointer(mapping.get(word) orelse unreachable)
//                 else
//                     b.emit_literal(word);
//             }
//             try mapping.put(cursor, b.finish());
//         }
//         cursor += size;
//     }
//     return mapping.get(address);
// }

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

const max_nesting = 10;
pub fn format(heap: Heap, object: Obj, writer: *Writer) !void {
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
    obj: Obj,
    writer: *Writer,
    parents: *[max_nesting]Parent,
    indentation: usize,
) error{WriteFailed}!void {
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

    if (obj.size() == 0) {
        try print_indentation(writer, parents, indent);
        try writer.print("nothing", .{});
        return;
    }

    if (obj.is_inner()) {
        for (0.., obj.children()) |i, child| {
            if (i > 0) try writer.print("\n", .{});
            try heap.format_indented(child, writer, parents, indent);
        }
      }
else {
        for (0.., obj.words()) |i, literal| {
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

pub fn dump_obj(heap: Heap, obj: Obj) void {
    var buffer: [64]u8 = undefined;
    const bw = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    heap.format(obj, bw) catch return;
    bw.print("\n", .{}) catch return;
}
