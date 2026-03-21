const std = @import("std");
const objects_compiler = @import("objects_compiler.zig");
const Heap = @import("heap.zig");
const Word = Heap.Word;
const Vm = @import("vm.zig");
const Graphics = @import("graphics.zig");
const Val = @import("pear_value.zig");
const Obj = Heap.Obj;
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn main() !void {
    std.debug.print("Orchard!\n", .{});

    var debug_ally = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = debug_ally.allocator();

    var heap = try Heap.init(ally, 100_000_000);
    // const start_of_heap = heap.checkpoint();
    var vm = try Vm.init(&heap, ally);

    const obj_defs = step: {
        std.debug.print("Zig loads objects into the heap.                            ", .{});
        var timer = try std.time.Timer.start();
        vm.impl.instruction_count = 0;
        const obj_defs = try objects_compiler.create(
            ally,
            vm.get_heap(),
            @embedFile("bootstrap.objects"),
        );
        std.debug.print("{} ms, ", .{timer.read() / std.time.ns_per_ms});
        std.debug.print("{} instructions, ", .{vm.impl.instruction_count});
        vm.get_heap().dump_stats();
        break :step obj_defs;
    };
    const fun = Val.from(obj_defs.get("main-fun").?);
    heap.dump_obj(fun.obj);
    const int = try Val.new_string(&heap, "hello");
    const res = try fun.call(&vm, &.{int});
    // _ = res;
    heap.dump_obj(res.obj);
    // std.debug.print("res: {f}\n", res);

    // const sloe_defs = step: {
    //     std.debug.print("Zig compiles Sloe into an Olive compiler.                   ", .{});
    //     var timer = try std.time.Timer.start();
    //     vm.impl.instruction_count = 0;
    //     const sloe_defs = try sloe_compiler.eval(
    //         ally,
    //         &vm,
    //         @embedFile("code.sloe"),
    //     );
    //     std.debug.print("{} ms, ", .{timer.read() / std.time.ns_per_ms});
    //     std.debug.print("{} instructions, ", .{vm.impl.instruction_count});
    //     vm.get_heap().dump_stats();
    //     break :step sloe_defs;
    // };

    // const compile_olive = step: {
    //     std.debug.print("Sloe compiles Olive into an optimizing Olive compiler.      ", .{});
    //     var timer = try std.time.Timer.start();
    //     vm.impl.instruction_count = 0;
    //     const olive_defs = try Val.from(sloe_defs.get("compile_olive").?).call(&vm, &.{
    //         try Val.new_string(&heap, @embedFile("code.olive")),
    //     });
    //     std.debug.print("{} ms, ", .{timer.read() / std.time.ns_per_ms});
    //     std.debug.print("{} instructions, ", .{vm.impl.instruction_count});
    //     vm.get_heap().dump_stats();
    //     const compile_olive = olive_defs.get_field("compile_olive");
    //     break :step compile_olive;
    // };

    // const optimized_olive_defs = step: {
    //     std.debug.print("Olive compiles Olive into a fast optimizing Olive compiler. ", .{});
    //     var timer = try std.time.Timer.start();
    //     const optimized_olive_defs = try compile_olive.call(&vm, &.{
    //         try Val.new_string(&heap, @embedFile("test.olive")),
    //     });
    //     std.debug.print("{} ms, ", .{timer.read() / std.time.ns_per_ms});
    //     vm.get_heap().dump_stats();
    //     if (true) return;
    //     heap.dump_obj(optimized_olive_defs.obj);
    //     break :step optimized_olive_defs;
    // };

    // if (true) return;

    // const compile_pear = optimized_olive_defs.get_field("compile_pear");

    // std.debug.print("Using Olive to compile Pear...\n", .{});
    // const file = try std.fs.cwd().openFile("../pear/test.pear", .{});
    // const code = try file.readToEndAlloc(ally, 1000000);
    // const compiled = try compile_pear.call(&vm, &.{try Val.new_string(&heap, code)});
    // std.debug.print("Compiled: {any}\n", .{compiled});
    // heap.dump_obj(compiled.obj);
    // vm.get_heap().dump_stats();

    // std.debug.print("Running Pear...\n", .{});
    // const result = try compiled.call(&vm, &.{});
    // std.debug.print("Result: {any}\n", .{result});
    // heap.dump_obj(result.obj);
    // vm.get_heap().dump_stats();

    if (true) return;

    // app = try handle_tasks(ally, &vm, app);
    // std.debug.print("Output: {f}\n", .{app});
    // const eval = app.kind().struct_.get_field("eval");
    // const code_str = try Val.String.new(&heap, code);
    // const result = try vm.call(eval.kind().lambda, &.{code_str.as_value()});

    // var gfx = try Graphics.init(ally);
    // defer gfx.deinit();

    // // Cached drawing instructions.
    // var previous_size = gfx.get_size();
    // var drawing_instructions: ?[]const Graphics.DrawingInstruction = null;
    // var drawing_instructions_ally: ?std.heap.ArenaAllocator = null;

    // while (!gfx.should_close()) {
    //     const there_are_events = gfx.event_queue.items.len > 0;

    //     if (there_are_events) {
    //         const update_lambda = heap.load(app, 2);
    //         try object_mod.assert_lambda(&heap, update_lambda);
    //         for (gfx.event_queue.items) |event| {
    //             // TODO: do the decision of handling in Pear
    //             const pear_event = event: switch (event) {
    //                 .entered_char => |char| {
    //                     std.debug.print("Char: {d}\n", .{char.codepoint});
    //                     const kind = try Symbol.new(&heap, "char");
    //                     const codepoint = try Int.new(&heap, @intCast(char.codepoint));
    //                     var b = try heap.object_builder(0);
    //                     try b.emit_pointer(kind);
    //                     try b.emit_pointer(codepoint);
    //                     break :event b.finish();
    //                 },
    //                 .pressed_key => |key| {
    //                     std.debug.print("Key: {any}\n", .{key});
    //                     const kind = try Symbol.new(&heap, "key");
    //                     const keycode = try Int.new(&heap, @intCast(key.keycode));
    //                     const control_pressed = try Int.new(&heap, if (key.control_pressed) 1 else 0);
    //                     const shift_pressed = try Int.new(&heap, if (key.shift_pressed) 1 else 0);
    //                     const alt_pressed = try Int.new(&heap, if (key.alt_pressed) 1 else 0);
    //                     break :event try heap.new_inner(.{ kind, keycode, control_pressed, shift_pressed, alt_pressed });
    //                 },
    //             };
    //             app = try vm.call(heap.load(update_lambda, 0), &[_]Address{ pear_event, heap.load(update_lambda, 1) });
    //             app = try handle_tasks(ally, &vm, app);

    //             {
    //                 var buffer: [64]u8 = undefined;
    //                 const bw = std.debug.lockStderrWriter(&buffer);
    //                 defer std.debug.unlockStderrWriter();
    //                 try heap.format(heap.load(app, 3), bw);
    //                 try bw.print("\n", .{});
    //             }
    //         }
    //         gfx.event_queue.items.len = 0;
    //         app = try vm.garbage_collect(start_of_heap, app);
    //         // _ = start_of_heap;
    //     }

    //     const size = gfx.get_size();
    //     if (there_are_events or size.width != previous_size.width or size.height != previous_size.height) {
    //         if (drawing_instructions) |_| {
    //             // Clear cached drawing instructions.
    //             drawing_instructions = null;
    //             drawing_instructions_ally.?.deinit();
    //         }
    //     }
    //     previous_size = size;

    //     if (drawing_instructions == null) {
    //         std.debug.print("rendering\n", .{});
    //         // There are no cached drawing instructions; render a frame.
    //         const frame_checkpoint = heap.checkpoint();
    //         defer heap.restore(frame_checkpoint);
    //         const render_lambda = heap.load(app, 1);
    //         try object_mod.assert_lambda(&heap, render_lambda);
    //         const drawing_instructions_obj = try vm.call(
    //             heap.load(render_lambda, 0),
    //             &[_]Address{
    //                 try Int.new(&heap, size.width),
    //                 try Int.new(&heap, size.height),
    //                 heap.load(render_lambda, 1),
    //             },
    //         );
    //         drawing_instructions_ally = std.heap.ArenaAllocator.init(ally);
    //         drawing_instructions = try Graphics.DrawingInstruction.parse_all(
    //             drawing_instructions_ally.?.allocator(),
    //             drawing_instructions_obj,
    //             vm.heap.*,
    //         );
    //     }

    //     try gfx.render(size, drawing_instructions.?);

    //     gfx.poll_events();
    // }

    //heap.dump_stats();
    //_ = try vm.garbage_collect(start_of_heap, result);
    //_ = try vm.deduplicate(start_of_heap);
    //heap.dump_stats();

    // heap.dump_raw();
    // heap.dump();
}

// fn handle_tasks(ally: Ally, vm: *Vm, app_: Address) !Address {
//     const heap = vm.heap;
//     var app = app_;
//     while (true) {
//         const symbol = app.get(0).as_symbol();
//         if (std.mem.eql(u8, symbol, "done")) {
//             break;
//         }
//         if (std.mem.eql(u8, symbol, "read file")) {
//             const file_name = object_mod.get_symbol(heap.*, heap.load(app, 1));
//             std.debug.print("reading: {s}\n", .{file_name});
//             const content = try std.fs.cwd().readFileAlloc(ally, file_name, 1000);
//             const content_on_heap = try create_linked_list_of_bytes(heap, content);
//             const lambda = heap.load(app, 2);
//             const fun = heap.load(lambda, 0);
//             const closure = heap.load(lambda, 1);
//             app = try vm.call(fun, &[_]Word{ content_on_heap, closure });
//         }
//         if (std.mem.eql(u8, symbol, "write file")) {
//             const file_name = object_mod.get_symbol(heap.*, heap.load(app, 1));
//             std.debug.print("writing: {s}\n", .{file_name});
//             const content = try linked_list_of_bytes_to_slice(ally, heap.*, heap.load(app, 2));
//             try std.fs.cwd().writeFile(.{ .sub_path = file_name, .data = content });
//             const lambda = heap.load(app, 3);
//             const fun = heap.load(lambda, 0);
//             const closure = heap.load(lambda, 1);
//             app = try vm.call(fun, &[_]Word{closure});
//         }
//     }
//     return app;
// }

// fn create_linked_list_of_bytes(heap: *Heap, content: []const u8) !Address {
//     if (content.len == 0) {
//         return object_mod.new_nil(heap);
//     } else {
//         const byte = try Int.new(heap, @intCast(content[0]));
//         const rest = try create_linked_list_of_bytes(heap, content[1..]);
//         var b = try heap.object_builder(0);
//         try b.emit_pointer(byte);
//         try b.emit_pointer(rest);
//         return b.finish();
//     }
// }

// fn linked_list_of_bytes_to_slice(ally: Ally, heap: Heap, address: Address) ![]const u8 {
//     var list = ArrayList(u8).empty;
//     var cursor = address;
//     while (heap.get(cursor).words.len > 0) {
//         try list.append(ally, @intCast(object_mod.get_int(heap, heap.load(cursor, 0))));
//         cursor = heap.load(cursor, 1);
//     }
//     return list.items;
// }
