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

    const add = try Object.new_lambda_from_ir(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const a = try builder.param();
        const b = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        try body.assert_is_int(a);
        try body.assert_is_int(b);
        const a_val = try body.get_int_value(a);
        const b_val = try body.get_int_value(b);
        const res_val = try body.add(a_val, b_val);
        const res = try body.new_int(res_val);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const foo = try eval(&heap, .{ .add = add },
        \\|a b| {
        \\  add(add(a, b), b)
        \\}
    );

    const bar = try eval(&heap, .{ .foo = foo },
        \\foo(3, 2)
    );
    std.debug.print("{f}", .{bar});

    _ = start_of_heap;
    // _ = try heap.deduplicate(start_of_heap, ally);
    // heap.dump();

    // heap.garbage_collect(start_of_heap, insturctions.address);
    // heap.dump_raw();
    // heap.dump();
}
