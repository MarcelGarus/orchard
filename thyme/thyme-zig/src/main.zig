const std = @import("std");

const thyme_zig = @import("thyme_zig");

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Object = @import("object.zig");
const Vm = @import("vm.zig");

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = Heap.init(ally);
    const start_of_heap = heap.checkpoint();

    const num = try Object.new_int(&heap, 42);
    _ = try Object.new_symbol(&heap, "foobar");
    _ = try Object.new_struct(&heap, .{ .x = num, .y = num });

    \\foo = \ a -> \ b -> * a b
    \\foo 2
    ;

    var vm = Vm.init(&heap);
    const code = try vm.new_instructions(&[_]Vm.Instruction{
        .{ .push_word = 42 },
        .{ .push_word = 2 },
        .add,
        .{ .push_word = 2 },
        .divide,
        .{ .if_not_zero = .{
            .then = try vm.new_instructions(&[_]Vm.Instruction{
                .{ .push_word = 13 },
            }),
            .else_ = try vm.new_instructions(&[_]Vm.Instruction{
                .{ .push_word = 14 },
            }),
        } },
        .{ .new = .{
            .tag = 2,
            .num_pointers = 0,
            .num_literals = 0,
        } },
    });
    code.dump(0);
    try vm.eval(code);
    vm.dump();

    _ = try heap.deduplicate(start_of_heap, ally);
    // heap.dump_raw();
    heap.dump();
}
