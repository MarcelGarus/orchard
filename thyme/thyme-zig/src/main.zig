const std = @import("std");

const thyme_zig = @import("thyme_zig");

const ast = @import("ast.zig");
const compile = @import("ast_to_ir.zig").compile;
const eval = @import("vm.zig").eval;
const Heap = @import("heap.zig");
const Word = Heap.Word;
const Instruction = @import("instruction.zig").Instruction;
const Ir = @import("ir.zig");
const Object = @import("object.zig");
const parse = @import("str_to_ast.zig").parse;

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = Heap.init(ally);
    const start_of_heap = heap.checkpoint();

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
    the_ir.dump(0);
    std.debug.print("{any}\n", .{the_ir});
    const fun = try Ir.new_fun(&heap, the_ir);

    std.debug.print("running\n", .{});
    fun.dump(0);
    _ = try eval(&heap, fun, .{});

    _ = try heap.deduplicate(start_of_heap, ally);
    heap.dump();
    // heap.garbage_collect(start_of_heap, insturctions.address);
    // heap.dump_raw();
    // heap.dump();
}
