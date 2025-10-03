const std = @import("std");

const thyme_zig = @import("thyme_zig");

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Object = @import("objects.zig");

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = Heap.init(ally);
    const start_of_heap = heap.checkpoint();

    const num = try Object.new_int(&heap, 42);
    _ = try Object.new_symbol(&heap, "foobar");
    _ = try Object.new_struct(&heap, .{ .x = num, .y = num });

    const code = try Object.new_struct(&heap, .{
        .head = try Object.new_enum(
            &heap,
            "push_word",
            (try Object.new_int(&heap, 42)),
        ),
        .tail = try Object.new_nil(&heap),
    });
    code.dump(0);

    _ = try heap.deduplicate(start_of_heap, ally);
    // heap.dump_raw();
    // heap.dump();
}
