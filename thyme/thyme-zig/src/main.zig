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
const Vm = @import("vm.zig");

pub fn main() !void {
    std.debug.print("Welcome to Thyme.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = try Heap.init(ally, 200000);
    const start_of_heap = heap.checkpoint();
    var vm = try Vm.init(&heap, ally);

    // const compare_symbols_rec_fun = try ir_to_fun(ally, heap, ir: {
    //     var builder = Ir.Builder.init(ally);
    //     const a = try builder.param(); // a symbol object
    //     const b = try builder.param(); // a symbol object
    //     const cursor = try builder.param(); // a literal word
    //     const rec = try builder.param(); // a reference to this function
    //     var body = builder.body();
    //     const len = try body.get_symbol_len_in_words(a);
    //     const diff = try body.subtract(len, cursor);
    //     const res = try body.if_not_zero(
    //         diff,
    //         not_done: {
    //             var inner = body.child_body();
    //             const a_word = try inner.load(a, cursor);
    //             const b_word = try inner.load(b, cursor);
    //             const word_diff = try inner.subtract(a_word, b_word);
    //             const res = try inner.if_not_zero(
    //                 word_diff,
    //                 word_differs: {
    //                     var innerer = body.child_body();
    //                     const zero = try innerer.word(0);
    //                     break :word_differs innerer.finish(zero);
    //                 },
    //                 word_same: {
    //                     var innerer = body.child_body();
    //                     const one = try innerer.word(1);
    //                     const next_cursor = try innerer.add(cursor, one);
    //                     var args = try ally.alloc(Id, 4);
    //                     args[0] = a;
    //                     args[1] = b;
    //                     args[2] = next_cursor;
    //                     args[3] = rec;
    //                     const res = try innerer.call(rec, args);
    //                     break :word_same innerer.finish(res);
    //                 },
    //             );
    //             break :not_done inner.finish(res);
    //         },
    //         done_comparing: {
    //             var inner = body.child_body();
    //             const one = try inner.word(1);
    //             break :done_comparing inner.finish(one);
    //         },
    //     );
    //     const body_ = body.finish(res);
    //     break :ir builder.finish(body_);
    // });
    // const compare_symbols_fun = try ir_to_fun(ally, heap, ir: {
    //     var builder = Ir.Builder.init(ally);
    //     const a = try builder.param(); // a symbol object
    //     const b = try builder.param(); // a symbol object
    //     var body = builder.body();
    //     const rec = try body.object(compare_symbols_rec_fun);
    //     var args = try ally.alloc(Id, 4);
    //     args[0] = a;
    //     args[1] = b;
    //     args[2] = try body.word(0);
    //     args[3] = rec;
    //     const res = try body.call(rec, args);
    //     const body_ = body.finish(res);
    //     break :ir builder.finish(body_);
    // });

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

    const builtins = try compiler.create_builtins(ally, &heap);
    const file = try std.fs.cwd().openFile("code.thyme", .{});
    const code = try file.readToEndAlloc(ally, 1000000);
    const result = try vm.eval(ally, builtins, code);

    {
        var buffer: [64]u8 = undefined;
        const bw = std.debug.lockStderrWriter(&buffer);
        defer std.debug.unlockStderrWriter();
        try heap.format(result, bw);
        try bw.print("\n", .{});
    }

    heap.dump_stats();
    _ = try heap.garbage_collect(ally, start_of_heap, result);
    _ = try heap.deduplicate(ally, start_of_heap);
    heap.dump_stats();

    // heap.dump_raw();
    // heap.dump();
}
