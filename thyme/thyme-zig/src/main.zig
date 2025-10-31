const std = @import("std");

const thyme_zig = @import("thyme_zig");

const compiler = @import("compiler.zig");
const Ir = compiler.Ir;
const ir_to_lambda = compiler.ir_to_lambda;
const ir_to_fun = compiler.ir_to_fun;
const instructions_to_fun = compiler.instructions_to_fun;
const Heap = @import("heap.zig");
const Word = Heap.Word;
const Instruction = @import("instruction.zig").Instruction;
const Object = @import("object.zig");
const Vm = @import("vm.zig");

pub fn main() !void {
    std.debug.print("Hi.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = try Heap.init(ally, 200000);
    const start_of_heap = heap.checkpoint();
    var vm = try Vm.init(&heap, ally);

    const expected_int_symbol = try Object.new_symbol(&heap, "expected int");

    // const foo = try ir_to_fun(ally, &heap, ir: {
    //     var builder = Ir.Builder.init(ally);
    //     const a = try builder.param();
    //     var body = builder.body();
    //     const b = try body.word(1);
    //     const sum = try body.add(a, b);
    //     const body_ = body.finish(sum);
    //     break :ir builder.finish(body_);
    // });

    // const object = try vm.call(ally, foo, &[_]Object{.{ .address = 4, .heap = &heap }});
    // std.debug.print("Returned: {}\n", .{object.address});

    // if (true) {
    //     return;
    // }

    // "collect_garbage" accepts a lambda that takes zero arguments. Makes a
    // heap checkpoint, calls the lambda, and does a garbage collection, freeing
    // everything that the lambda allocated except the return value.
    const unchecked_collect_garbage = try instructions_to_fun(&heap, 2, &[_]Instruction{
        // stack: (lambda)
        .heap_checkpoint, // (lambda checkpoint)
        .{ .push_from_stack = 1 }, // (lambda checkpoint lambda)
        .{ .push_word = 1 }, // (lambda checkpoint lambda 1)
        .load, // (lambda checkpoint closure)
        .{ .push_from_stack = 2 }, // (lambda checkpoint closure lambda)
        .{ .push_word = 0 }, // (lambda checkpoint closure lambda 0)
        .load, // (lambda checkpoint closure fun)
        .{ .push_word = 1 }, // (lambda checkpoint closure fun 1)
        .load, // (lambda checkpoint closure instructions)
        .eval, // (lambda checkpoint result)
        .collect_garbage, // (lambda result)
        .{ .pop_below_top = 1 }, // (result)
    });
    const collect_garbage = try ir_to_lambda(ally, &heap, ir: {
        var builder = Ir.Builder.init(ally);
        const lambda = try builder.param();
        _ = try builder.param(); // closure
        var body = builder.body();
        const zero = try body.word(0);
        const one = try body.word(1);
        const two = try body.word(2);
        const tag = try body.tag(lambda);
        const lambda_tag = try body.word(Object.TAG_LAMBDA);
        const compared = try body.compare(tag, lambda_tag);
        _ = try body.if_not_zero(
            compared,
            then: {
                var inner = body.child_body();
                const message = try inner.object(try Object.new_symbol(&heap, "expected lambda"));
                break :then inner.finish(message);
            },
            else_: {
                var inner = body.child_body();
                break :else_ inner.finish(zero);
            },
        );
        const fun = try body.load(lambda, zero);
        const num_args = try body.load(fun, two);
        const only_takes_closure = try body.compare(num_args, one);
        _ = try body.if_not_zero(
            only_takes_closure,
            then: {
                var inner = body.child_body();
                const message = try inner.object(try Object.new_symbol(&heap, "expected lambda with zero arguments"));
                break :then inner.finish(message);
            },
            else_: {
                var inner = body.child_body();
                break :else_ inner.finish(zero);
            },
        );
        const unchecked = try body.object(unchecked_collect_garbage);
        const args = try ally.alloc(compiler.Ir.Id, 1);
        args[0] = lambda;
        const result = try body.call(unchecked, args);
        const body_ = body.finish(result);
        break :ir builder.finish(body_);
    });

    const crash = try ir_to_lambda(ally, &heap, ir: {
        var builder = Ir.Builder.init(ally);
        const message = try builder.param();
        const closure = try builder.param();
        _ = closure;
        var body = builder.body();
        const res = try body.crash(message);
        const body_ = body.finish(res);
        break :ir builder.finish(body_);
    });

    const int_add = try ir_to_lambda(ally, &heap, ir: {
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

    const int_subtract = try ir_to_lambda(ally, &heap, ir: {
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

    const int_multiply = try ir_to_lambda(ally, &heap, ir: {
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

    const int_divide = try ir_to_lambda(ally, &heap, ir: {
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

    const if_not_zero = try ir_to_lambda(ally, &heap, ir: {
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

    const int_compare_to_num = try ir_to_lambda(ally, &heap, ir: {
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
        .collect_garbage = collect_garbage,
        .crash = crash,
        .add = int_add,
        .subtract = int_subtract,
        .multiply = int_multiply,
        .divide = int_divide,
        .if_not_zero = if_not_zero,
        .int_compare_to_num = int_compare_to_num,
    });

    const result = try vm.eval(ally, .{ .builtins = builtins }, code);

    std.debug.print("{f}\n", .{result});

    heap.dump_stats();
    _ = try heap.garbage_collect(ally, start_of_heap, result.address);
    _ = try heap.deduplicate(ally, start_of_heap);
    heap.dump_stats();
    // heap.dump_raw();
    // heap.dump();
}
