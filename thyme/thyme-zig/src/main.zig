const std = @import("std");

const thyme_zig = @import("thyme_zig");

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Object = Heap.Object;

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();
    var heap = Heap.init(ally);

    const x = try heap.word(42);
    const y = try heap.word(123);
    _ = try heap.struct_(.{ .x = x, .y = y });

    const list0 = try heap.empty_list();
    // heap.dump();
    const list1 = try heap.list_push(list0, x);
    const list2 = try heap.list_push(list1, x);
    // heap.dump();
    std.debug.print("list at {}\n", .{list2});
    const list3 = try heap.list_push(list2, x);
    // heap.dump();
    const list4 = try heap.list_push(list3, x);
    // heap.dump();
    std.debug.print("len = {}\n", .{heap.list_len(list4)});
    heap.dump_object(list4, 0);
    // _ = list1;

    // _ = try heap.new_enum("int", x);

    // _ = try heap.new_struct(.{
    //     .left = x,
    //     .right = y,
    //     .length = 2,
    // });

    // heap.dump();
}
