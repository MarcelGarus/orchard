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

    const crash = try Object.new_lambda_from_ir(&heap, ir: {
        var builder = Ir.Builder.init(ally);
        const message = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        const res = try body.crash(message);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const int_add = try Object.new_lambda_from_ir(&heap, ir: {
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

    const int_subtract = try Object.new_lambda_from_ir(&heap, ir: {
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

    const int_multiply = try Object.new_lambda_from_ir(&heap, ir: {
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

    const if_not_zero = try Object.new_lambda_from_ir(&heap, ir: {
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

    const int_compare_to_num = try Object.new_lambda_from_ir(&heap, ir: {
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
        .if_not_zero = if_not_zero,
        .int_compare_to_num = int_compare_to_num,
    },
        \\unreachable = || crash(0)  # TODO: better message
        \\
        \\equals = |a, b| if_not_zero(subtract(a, b), [false], [true])
        \\not_equals = |a, b| if_not_zero(subtract(a, b), [true], [false])
        \\is_less = |a, b| equals(int_compare_to_num(a, b), 2)
        \\is_greater = |a, b| equals(int_compare_to_num(a, b), 1)
        \\is_less_equal = |a, b|
        \\  if_not_zero(subtract(int_compare_to_num(a, b), 1), [true], [false])
        \\is_greater_equal = |a, b|
        \\  if_not_zero(subtract(int_compare_to_num(a, b), 2), [true], [false])
        \\
        \\loop = {
        \\  rec = |rec, state, body| {
        \\    body(state) % {
        \\      break: result -> result
        \\      continue: next -> rec(rec, next, body)
        \\    }
        \\  }
        \\  |initial_state, body| rec(rec, initial_state, body)
        \\}
        \\
        \\round_up_to_power_of = |number, base| {
        \\  loop(1, |candidate| {
        \\    is_greater_equal(candidate, number) % {
        \\      true -> [break: candidate]
        \\      false -> [continue: multiply(candidate, base)]
        \\    }
        \\  })
        \\}
        \\
        \\round_up_to_power_of(12, 2)
    );
    std.debug.print("{f}", .{result});

    // const foo = try eval(&heap, .{ .add = int_add },
    //     \\|a b| {
    //     \\  add(add(a, b), b)
    //     \\}
    // );

    // const bar = try eval(&heap, .{ .foo = foo },
    //     \\foo(3, 2)
    // );
    // std.debug.print("{f}", .{bar});

    // # List stuff.
    //
    // Lists are stored as a binary tree of minimal height, where nodes are
    // filled from the left. The length tells us exactly the layout of this
    // tree. For example, a list of 5 items results in the following tree:
    // [length: 5
    //  items: [
    //    left: [left: [left: 0 right: 1] right: [left: 2 right: 3]]
    //    right: [left: [left: 4 right: nil] right: nil]
    //  ]
    // ]
    // const list_get = try eval(&heap, .{
    //     .crash = crash,
    //     .@"unreachable" = unreachable_,
    //     .subtract = int_subtract,
    //     .equals = int_equals,
    //     .round_up_to_power_of = round_up_to_power_of,
    // },
    //     \\rec = |rec, items, length, index| {
    //     \\  equals(length, 0) % {
    //     \\    true -> unreachable()
    //     \\    false -> {
    //     \\      num_left = divide(length, 2)  # TODO: shift
    //     \\      num_right = subtract(length, num_left)
    //     \\      is_less(index, num_left) % {
    //     \\        true -> rec(rec, items.left, num_left, index)
    //     \\        false -> rec(rec, items.right, num_right, subtract(index, num_left))
    //     \\      }
    //     \\    }
    //     \\  }
    //     \\}
    //     \\|list, index| {
    //     \\  is_greater_equal(index, list.length) % {
    //     \\    true -> crash(0)  # TODO: out of bounds
    //     \\    false ->
    //     \\      rec(rec, list.items, round_up_to_power_of(list.length, 2), index)
    //     \\  }
    //     \\}
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

    // heap.garbage_collect(start_of_heap, insturctions.address);
    // heap.dump_raw();
    // heap.dump();
}
