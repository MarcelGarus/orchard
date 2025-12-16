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

    // Cached drawing instructions.
    var previous_size = gfx.get_size();
    var drawing_instructions: ?[]const Graphics.DrawingInstruction = null;
    var drawing_instructions_ally: ?std.heap.ArenaAllocator = null;

    while (!gfx.should_close()) {
        const there_are_events = gfx.event_queue.items.len > 0;

        if (there_are_events) {
            const update_lambda = heap.load(app, 1);
            try object_mod.assert_lambda(&heap, update_lambda);
            for (gfx.event_queue.items) |event| {
                // TODO: do the decision of handling in thyme
                const thyme_event = event: switch (event) {
                    .entered_char => |char| {
                        std.debug.print("Char: {d}\n", .{char.codepoint});
                        const kind = try object_mod.new_symbol(&heap, "char");
                        const codepoint = try object_mod.new_int(&heap, @intCast(char.codepoint));
                        var b = try heap.object_builder(0);
                        try b.emit_pointer(kind);
                        try b.emit_pointer(codepoint);
                        break :event b.finish();
                    },
                    .pressed_key => |key| {
                        std.debug.print("Key: {any}\n", .{key});
                        const kind = try object_mod.new_symbol(&heap, "key");
                        const keycode = try object_mod.new_int(&heap, @intCast(key.keycode));
                        const control_pressed = try object_mod.new_int(&heap, if (key.control_pressed) 1 else 0);
                        const shift_pressed = try object_mod.new_int(&heap, if (key.shift_pressed) 1 else 0);
                        var b = try heap.object_builder(0);
                        try b.emit_pointer(kind);
                        try b.emit_pointer(keycode);
                        try b.emit_pointer(control_pressed);
                        try b.emit_pointer(shift_pressed);
                        break :event b.finish();
                    },
                };
                app = try vm.call(heap.load(update_lambda, 0), &[_]Address{ thyme_event, heap.load(update_lambda, 1) });

                // {
                //     var buffer: [64]u8 = undefined;
                //     const bw = std.debug.lockStderrWriter(&buffer);
                //     defer std.debug.unlockStderrWriter();
                //     try heap.format(app, bw);
                //     try bw.print("\n", .{});
                // }
            }
            gfx.event_queue.items.len = 0;
            // app = try vm.garbage_collect(start_of_heap, app);
            _ = start_of_heap;
        }

        const size = gfx.get_size();
        if (there_are_events or size.width != previous_size.width or size.height != previous_size.height) {
            if (drawing_instructions) |_| {
                // Clear cached drawing instructions.
                drawing_instructions = null;
                drawing_instructions_ally.?.deinit();
            }
        }
        previous_size = size;

        if (drawing_instructions == null) {
            std.debug.print("rendering\n", .{});
            // There are no cached drawing instructions; render a frame.
            const frame_checkpoint = heap.checkpoint();
            defer heap.restore(frame_checkpoint);
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
            drawing_instructions_ally = std.heap.ArenaAllocator.init(ally);
            drawing_instructions = try Graphics.DrawingInstruction.parse_all(
                drawing_instructions_ally.?.allocator(),
                drawing_instructions_obj,
                vm.heap.*,
            );
        }

        try gfx.render(size, drawing_instructions.?);

        gfx.poll_events();
    }

    //heap.dump_stats();
    //_ = try vm.garbage_collect(start_of_heap, result);
    //_ = try vm.deduplicate(start_of_heap);
    //heap.dump_stats();

    // heap.dump_raw();
    // heap.dump();
}
