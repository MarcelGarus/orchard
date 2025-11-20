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
const graphics = @import("graphics.zig");
const object_mod = @import("object.zig");
const Address = Heap.Address;

pub fn main() !void {
    std.debug.print("Welcome to Thyme.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = try Heap.init(ally, 200000);
    const start_of_heap = heap.checkpoint();
    var vm = try Vm.init(&heap, ally);

    const builtins = try compiler.create_builtins(ally, &heap);
    const file = try std.fs.cwd().openFile("code.thyme", .{});
    const code = try file.readToEndAlloc(ally, 1000000);
    const render_lambda = try vm.eval(ally, builtins, code);

    {
        var buffer: [64]u8 = undefined;
        const bw = std.debug.lockStderrWriter(&buffer);
        defer std.debug.unlockStderrWriter();
        try heap.format(render_lambda, bw);
        try bw.print("\n", .{});
    }

    graphics.init();
    defer graphics.deinit();
    while (!graphics.should_close()) {
        const size = graphics.get_size();
        const width_obj = try object_mod.new_int(&heap, @intCast(size.width));
        const height_obj = try object_mod.new_int(&heap, @intCast(size.height));
        const drawing_instructions_obj = try vm.call(
            heap.load(render_lambda, 0),
            &[_]Address{ width_obj, height_obj, heap.load(render_lambda, 1) },
        );
        const drawing_instructions = try graphics.DrawingInstruction.parse_all(
            ally,
            drawing_instructions_obj,
            vm.heap.*,
        );
        std.debug.print("{any}\n", .{drawing_instructions});
        try graphics.render(ally, drawing_instructions);
    }

    _ = start_of_heap;
    //heap.dump_stats();
    //_ = try heap.garbage_collect(ally, start_of_heap, result);
    //_ = try heap.deduplicate(ally, start_of_heap);
    //heap.dump_stats();

    // heap.dump_raw();
    // heap.dump();
}
