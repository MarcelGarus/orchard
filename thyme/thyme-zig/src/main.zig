const std = @import("std");

const thyme_zig = @import("thyme_zig");

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Object = Heap.Object;
const objects = @import("objects.zig");
const new_empty_list = objects.new_empty_list;
const new_nil = objects.new_nil;
const list_push = objects.list_push;
const new_int = objects.new_int;
const new_symbol = objects.new_symbol;
const new_struct = objects.new_struct;
const new_ll = objects.new_ll;
const new_enum = objects.new_enum;

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = Heap.init(ally);
    const start_of_heap = heap.checkpoint();

    const num = try new_int(&heap, 42);
    _ = try new_symbol(&heap, "foobar");
    _ = try new_struct(&heap, .{ .x = num, .y = num });

    const list0 = try new_empty_list(&heap);
    const list1 = try list_push(&heap, list0, num);
    const list2 = try list_push(&heap, list1, num);
    const list3 = try list_push(&heap, list2, num);
    _ = list3;

    const code = try new_struct(&heap, .{
        .head = try new_enum(
            &heap,
            "push_word",
            try new_int(&heap, 42),
        ),
        .tail = try new_nil(&heap),
    });
    objects.dump_object(heap, code, 0);

    _ = try heap.deduplicate(start_of_heap, ally);
    // heap.dump_raw();
    // heap.dump();

    // const x = try heap.word(42);
    // const y = try heap.word(123);
    // _ = try heap.struct_(.{ .x = x, .y = y });

    // const list0 = try heap.empty_list();
    // // heap.dump();
    // const list1 = try heap.list_push(list0, x);
    // const list2 = try heap.list_push(list1, x);
    // // heap.dump();
    // std.debug.print("list at {}\n", .{list2});
    // const list3 = try heap.list_push(list2, x);
    // // heap.dump();
    // const list4 = try heap.list_push(list3, x);
    // // heap.dump();
    // std.debug.print("len = {}\n", .{heap.list_len(list4)});
    // heap.dump_object(list4, 0);
    // _ = list1;

    // _ = try heap.new_enum("int", x);

    // _ = try heap.new_struct(.{
    //     .left = x,
    //     .right = y,
    //     .length = 2,
    // });

    // heap.dump();
}
