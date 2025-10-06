const std = @import("std");

const thyme_zig = @import("thyme_zig");

const ast = @import("ast.zig");
const compile = @import("ast_to_ir.zig").compile;
const Heap = @import("heap.zig");
const Word = Heap.Word;
const Ir = @import("ir.zig");
const Object = @import("object.zig");
const parse = @import("str_to_ast.zig").parse;
const Vm = @import("vm.zig");

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = Heap.init(ally);
    const start_of_heap = heap.checkpoint();

    var vm = Vm.init(&heap);

    const the_ast = try parse(ally,
        \\# flub
        \\foo = 1
        \\bar = {& x: foo y: 3}
        \\baz = :true
        \\baz
        \\% case true 2
    );
    ast.dump(the_ast, 0);

    const the_ir = try compile(ally, the_ast, &heap);
    heap.dump();
    the_ir.dump(0);
    std.debug.print("{any}\n", .{the_ir});

    // const instructions = try vm.new_instructions(&[_]Vm.Instruction{
    //     .{ .push_word = 42 },
    //     .{ .push_word = 2 },
    //     .add,
    //     .{ .push_word = 2 },
    //     .divide,
    //     .{ .if_not_zero = .{
    //         .then = try vm.new_instructions(&[_]Vm.Instruction{
    //             .{ .push_word = 13 },
    //         }),
    //         .else_ = try vm.new_instructions(&[_]Vm.Instruction{
    //             .{ .push_word = 14 },
    //         }),
    //     } },
    //     .{ .new = .{
    //         .tag = 2,
    //         .num_pointers = 0,
    //         .num_literals = 0,
    //     } },
    // });
    // instructions.dump(0);
    // try vm.eval(instructions);
    vm.dump();

    _ = try heap.deduplicate(start_of_heap, ally);
    heap.dump();
    // heap.garbage_collect(start_of_heap, insturctions.address);
    // heap.dump_raw();
    // heap.dump();
}
