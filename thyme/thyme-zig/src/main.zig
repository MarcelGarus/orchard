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
const Graphics = @import("graphics.zig");
const object_mod = @import("object.zig");
const Address = Heap.Address;

pub fn main() !void {
    std.debug.print("Welcome to Thyme.\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = try Heap.init(ally, 200000);
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

    var gfx = try Graphics.init(ally);
    defer gfx.deinit();
    while (!gfx.should_close()) {
        const frame_checkpoint = heap.checkpoint();
        defer heap.restore(frame_checkpoint);

        var frame_arena = std.heap.ArenaAllocator.init(ally);
        defer frame_arena.deinit();
        const frame_ally = frame_arena.allocator();

        const size = gfx.get_size();
        const width_obj = try object_mod.new_int(&heap, @intCast(size.width));
        const height_obj = try object_mod.new_int(&heap, @intCast(size.height));
        std.debug.print("calling\n", .{});
        const drawing_instructions_obj = try vm.call(
            heap.load(render_lambda, 0),
            &[_]Address{ width_obj, height_obj, heap.load(render_lambda, 1) },
        );
        {
            var buffer: [64]u8 = undefined;
            const bw = std.debug.lockStderrWriter(&buffer);
            defer std.debug.unlockStderrWriter();
            try heap.format(drawing_instructions_obj, bw);
            try bw.print("\n", .{});
        }
        std.debug.print("parsing\n", .{});
        const drawing_instructions = try Graphics.DrawingInstruction.parse_all(
            frame_ally,
            drawing_instructions_obj,
            vm.heap.*,
        );
        std.debug.print("rendering\n", .{});
        try gfx.render(size, drawing_instructions);
        gfx.poll_events();
    }

    //heap.dump_stats();
    //_ = try heap.garbage_collect(ally, start_of_heap, result);
    //_ = try heap.deduplicate(ally, start_of_heap);
    //heap.dump_stats();

    // heap.dump_raw();
    // heap.dump();
}
