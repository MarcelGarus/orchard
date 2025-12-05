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

    var heap = try Heap.init(ally, 1000000);
    const start_of_heap = heap.checkpoint();
    var vm = try Vm.init(&heap, ally);

    const builtins = try compiler.create_builtins(ally, &heap);
    const file = try std.fs.cwd().openFile("code.thyme", .{});
    const code = try file.readToEndAlloc(ally, 1000000);
    var app = try vm.eval(ally, builtins, code);

    {
        var buffer: [64]u8 = undefined;
        const bw = std.debug.lockStderrWriter(&buffer);
        defer std.debug.unlockStderrWriter();
        try heap.format(app, bw);
        try bw.print("\n", .{});
    }

    //if (true) return;

    var gfx = try Graphics.init(ally);
    defer gfx.deinit();
    while (!gfx.should_close()) {
        { // Render a frame.
            const frame_checkpoint = heap.checkpoint();
            defer heap.restore(frame_checkpoint);

            var frame_arena = std.heap.ArenaAllocator.init(ally);
            defer frame_arena.deinit();
            const frame_ally = frame_arena.allocator();

            const size = gfx.get_size();
            //const mouse = gfx.get_mouse_pos();
            const render_lambda = heap.load(app, 0);
            try object_mod.assert_lambda(&heap, render_lambda);
            const drawing_instructions_obj = try vm.call(
                heap.load(render_lambda, 0),
                &[_]Address{
                    try object_mod.new_int(&heap, size.width),
                    try object_mod.new_int(&heap, size.height),
                    heap.load(render_lambda, 1),
                },
            );
            // {
            //     var buffer: [64]u8 = undefined;
            //     const bw = std.debug.lockStderrWriter(&buffer);
            //     defer std.debug.unlockStderrWriter();
            //     try heap.format(drawing_instructions_obj, bw);
            //     try bw.print("\n", .{});
            // }
            const drawing_instructions = try Graphics.DrawingInstruction.parse_all(
                frame_ally,
                drawing_instructions_obj,
                vm.heap.*,
            );
            try gfx.render(size, drawing_instructions);
        }

        { // Poll for updates.
            gfx.poll_events();

            const update_lambda = heap.load(app, 1);
            try object_mod.assert_lambda(&heap, update_lambda);
            for (gfx.event_queue.items) |event| {
                switch (event) {
                    .entered_char => |char| {
                        std.debug.print("Entered char: {d}\n", .{char.codepoint});
                        app = try vm.call(
                            heap.load(update_lambda, 0),
                            &[_]Address{
                                try object_mod.new_int(&heap, @intCast(char.codepoint)),
                                heap.load(update_lambda, 1),
                            },
                        );
                    },
                    .pressed_key => |key| {
                        const forward = key.control_pressed or key.keycode == 257 or key.keycode == 259;
                        if (forward) {
                            std.debug.print("Pressed key: {d}\n", .{key.keycode});
                            app = try vm.call(
                                heap.load(update_lambda, 0),
                                &[_]Address{
                                    try object_mod.new_int(
                                        &heap,
                                        if (key.keycode == 257) 10 else @intCast(key.keycode),
                                    ),
                                    heap.load(update_lambda, 1),
                                },
                            );
                        } else {
                            std.debug.print("Ignoring key: {d}\n", .{key.keycode});
                        }
                    },
                }

                // {
                //     var buffer: [64]u8 = undefined;
                //     const bw = std.debug.lockStderrWriter(&buffer);
                //     defer std.debug.unlockStderrWriter();
                //     try heap.format(app, bw);
                //     try bw.print("\n", .{});
                // }
            }
            if (gfx.event_queue.items.len > 0) {
                gfx.event_queue.items.len = 0;
                // app = try vm.garbage_collect(start_of_heap, app);
                _ = start_of_heap;
            }
        }
    }

    //heap.dump_stats();
    //_ = try vm.garbage_collect(start_of_heap, result);
    //_ = try vm.deduplicate(start_of_heap);
    //heap.dump_stats();

    // heap.dump_raw();
    // heap.dump();
}
