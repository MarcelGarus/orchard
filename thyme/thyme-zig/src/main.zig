const std = @import("std");

const thyme_zig = @import("thyme_zig");

const compiler = @import("compiler.zig");
const Ir = compiler.Ir;
const ir_to_lambda = compiler.ir_to_lambda;
const eval = @import("vm.zig").eval;
const Heap = @import("heap.zig");
const Word = Heap.Word;
const Object = @import("object.zig");

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = Heap.init(ally);
    const start_of_heap = heap.checkpoint();

    const expected_int_symbol = try Object.new_symbol(&heap, "expected int");

    const crash = try ir_to_lambda(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const message = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        const res = try body.crash(message);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const int_add = try ir_to_lambda(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const a = try builder.param();
        const b = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        try body.assert_is_int(a, expected_int_symbol);
        try body.assert_is_int(b, expected_int_symbol);
        const a_val = try body.get_int_value(a);
        const b_val = try body.get_int_value(b);
        const res_val = try body.add(a_val, b_val);
        const res = try body.new_int(res_val);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const int_subtract = try ir_to_lambda(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const a = try builder.param();
        const b = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        try body.assert_is_int(a, expected_int_symbol);
        try body.assert_is_int(b, expected_int_symbol);
        const a_val = try body.get_int_value(a);
        const b_val = try body.get_int_value(b);
        const res_val = try body.subtract(a_val, b_val);
        const res = try body.new_int(res_val);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const int_multiply = try ir_to_lambda(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const a = try builder.param();
        const b = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        try body.assert_is_int(a, expected_int_symbol);
        try body.assert_is_int(b, expected_int_symbol);
        const a_val = try body.get_int_value(a);
        const b_val = try body.get_int_value(b);
        const res_val = try body.multiply(a_val, b_val);
        const res = try body.new_int(res_val);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const int_divide = try ir_to_lambda(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const a = try builder.param();
        const b = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        try body.assert_is_int(a, expected_int_symbol);
        try body.assert_is_int(b, expected_int_symbol);
        const a_val = try body.get_int_value(a);
        const b_val = try body.get_int_value(b);
        const res_val = try body.divide(a_val, b_val);
        const res = try body.new_int(res_val);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const if_not_zero = try ir_to_lambda(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const condition = try builder.param();
        const then_ = try builder.param();
        const else_ = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        try body.assert_is_int(condition, expected_int_symbol);
        const condition_val = try body.get_int_value(condition);
        const res = try body.if_not_zero(
            condition_val,
            then: {
                var inner = body.child_body();
                break :then inner.finish(then_);
            },
            else_: {
                var inner = body.child_body();
                break :else_ inner.finish(else_);
            },
        );
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const int_compare_to_num = try ir_to_lambda(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const a = try builder.param();
        const b = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        try body.assert_is_int(a, expected_int_symbol);
        try body.assert_is_int(b, expected_int_symbol);
        const a_val = try body.get_int_value(a);
        const b_val = try body.get_int_value(b);
        const diff = try body.compare(a_val, b_val);
        const res = try body.new_int(diff);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const file = try std.fs.cwd().openFile("code.thyme", .{});
    const code = try file.readToEndAlloc(ally, 1000000);

    const builtins = try Object.new_struct(&heap, .{
        .crash = crash,
        .add = int_add,
        .subtract = int_subtract,
        .multiply = int_multiply,
        .divide = int_divide,
        .if_not_zero = if_not_zero,
        .int_compare_to_num = int_compare_to_num,
    });

    const result = try eval(&heap, .{ .builtins = builtins }, code);

    std.debug.print("{f}\n", .{result});

    heap.dump_stats();
    _ = try heap.garbage_collect(start_of_heap, result.address);
    _ = try heap.deduplicate(start_of_heap, ally);
    heap.dump_stats();
    // heap.dump_raw();
    // heap.dump();
}
