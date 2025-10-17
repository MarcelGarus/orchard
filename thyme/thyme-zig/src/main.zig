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
        try body.assert_is_int(a);
        try body.assert_is_int(b);
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
        try body.assert_is_int(a);
        try body.assert_is_int(b);
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
        try body.assert_is_int(a);
        try body.assert_is_int(b);
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
        try body.assert_is_int(a);
        try body.assert_is_int(b);
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
        try body.assert_is_int(condition);
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
        try body.assert_is_int(a);
        try body.assert_is_int(b);
        const a_val = try body.get_int_value(a);
        const b_val = try body.get_int_value(b);
        const diff = try body.compare(a_val, b_val);
        const res = try body.new_int(diff);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const result = try eval(&heap, .{
        .crash = crash,
        .add = int_add,
        .subtract = int_subtract,
        .multiply = int_multiply,
        .divide = int_divide,
        .if_not_zero = if_not_zero,
        .int_compare_to_num = int_compare_to_num,
    std.debug.print("{f}", .{result});

    // const list_get = try eval(&heap, .{
    //     .crash = crash,
    //     .@"unreachable" = unreachable_,
    //     .subtract = int_subtract,
    //     .equals = int_equals,
    //     .round_up_to_power_of = round_up_to_power_of,
    // },
    // );
    // _ = list_get;
    //   push_rec (\ push_rec items length item
    //     ())
    //   push (\ list item
    //     (push_rec
    //       push_rec (list :items) (round_up_to_power_of (list :length) 2)) item)
    //   list_0 (& length 0 items :nil)
    //   list_1 (\ a (push list_0 a) (& length 1 item a))
    //   list_2 (\ a b (push (list_1 a) b))
    //   list_3 (\ a b c (push (list_2 a b) c))
    //   list_4 (\ a b c d (push (list_3 a b c) d))

    _ = start_of_heap;
    // _ = try heap.deduplicate(start_of_heap, ally);
    // heap.dump();
    }, code);


    heap.dump_stats();
    _ = try heap.garbage_collect(start_of_heap, result.address);
    _ = try heap.deduplicate(start_of_heap, ally);
    heap.dump_stats();
    // heap.dump_raw();
    // heap.dump();
}
