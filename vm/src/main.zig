const std = @import("std");
const object_loader = @import("object_loader.zig");
const Heap = @import("heap.zig");
const Word = Heap.Word;
const Vm = @import("vm.zig");
const Graphics = @import("graphics.zig");
const Value = @import("value.zig");
const Ir = @import("ir.zig");
const Obj = Heap.Obj;
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Io = std.Io;

const BootstrapStep = struct {
    starttime: Io.Timestamp,
    vm: *Vm,

    fn start(io: Io, comptime name: []const u8, vm: *Vm) !BootstrapStep {
        std.debug.print(name, .{});
        for (name.len..30) |_| std.debug.print(" ", .{});
        const now = Io.Timestamp.now(io, .real);
        // vm.impl.instruction_count = 0;
        return .{ .starttime = now, .vm = vm };
    }
    fn end(self: *BootstrapStep, io: Io) void {
        const now = Io.Timestamp.now(io, .real);
        std.debug.print("{} ms, ", .{now.toMilliseconds() - self.starttime.toMilliseconds()});
        // std.debug.print("{} instructions, ", .{self.vm.impl.instruction_count});
        self.vm.get_heap().dump_stats();
    }
};

fn is_same(a: Obj, b: Obj) bool {
    if (a.address == b.address) return true;
    if (a.is_inner() != b.is_inner()) return false;
    if (a.size() != b.size()) return false;
    if (a.is_inner()) {
        for (a.children(), b.children()) |ac, bc| if (!is_same(ac, bc)) return false;
    } else {
        for (a.words(), b.words()) |aw, bw| if (aw != bw) return false;
    }
    return true;
}
pub fn main(init: std.process.Init) !void {
    // std.debug.print("Orchard!\n", .{});

    const ally = init.gpa;
    const io = init.io;

    // const stdout_buf = try ally.alloc(u8, 1000);
    // const stdin_buf = try ally.alloc(u8, 1000);
    // var stdout_w = Io.File.stdout().writer(io, stdout_buf);
    // var stdin_r = Io.File.stdin().reader(io, stdin_buf);
    // const stdout = &stdout_w.interface;
    // const stdin = &stdin_r.interface;

    var heap = try Heap.init(ally, 200_000_000);
    const start_of_heap = heap.checkpoint();
    var vm = try Vm.init(&heap, ally);

    const objects_code = try Io.Dir.cwd().readFileAlloc(io, "src/bootstrap.objects", ally, Io.Limit.unlimited);
    const olive_code = try Io.Dir.cwd().readFileAlloc(io, "src/bootstrap.olive", ally, Io.Limit.unlimited);
    const pear_code = try Io.Dir.cwd().readFileAlloc(io, "src/bootstrap.pear", ally, Io.Limit.unlimited);

    const compile_olive = step: {
        var step = try BootstrapStep.start(io, "Loading the Olive compiler.", &vm);
        defer step.end(io);
        break :step Value.from(try object_loader.load(ally, vm.get_heap(), objects_code));
    };
    const olive = step: {
        var step = try BootstrapStep.start(io, "Compiling Olive.", &vm);
        defer step.end(io);
        const result = try compile_olive.call(&vm, &.{
            try Value.new_string(&heap, olive_code),
        });
        break :step Value.from(try vm.garbage_collect(start_of_heap, result.obj));
    };
    const olive_self_hosted = step: {
        var step = try BootstrapStep.start(io, "Self-compiling Olive.", &vm);
        defer step.end(io);
        break :step try olive.get_field("compile_olive").call(&vm, &.{
            try Value.new_string(&heap, olive_code),
        });
    };
    const olive_self_hosted_2 = step: {
        var step = try BootstrapStep.start(io, "Self-compiling Olive.", &vm);
        defer step.end(io);
        break :step try olive_self_hosted.get_field("compile_olive").call(&vm, &.{
            try Value.new_string(&heap, olive_code),
        });
    };
    _ = {
        var step = try BootstrapStep.start(io, "Confirming self-hosting.", &vm);
        defer step.end(io);
        if (!is_same(olive_self_hosted.obj, olive_self_hosted_2.obj)) {
            @panic("Not the same.");
        }
    };
    const compile_pear = step: {
        var step = try BootstrapStep.start(io, "Keeping only Pear compiler.", &vm);
        defer step.end(io);
        break :step Value.from(try vm.garbage_collect(start_of_heap, olive_self_hosted_2.get_field("compile_pear").obj));
    };
    const pear = step: {
        var step = try BootstrapStep.start(io, "Compiling Pear.", &vm);
        defer step.end(io);
        const result = try compile_pear.call(&vm, &.{try Value.new_string(&heap, pear_code)});
        break :step Value.from(try vm.garbage_collect(start_of_heap, result.obj));
    };
    // std.debug.print("pear:\n{f}\n", .{pear.obj});
    const pear_export = try pear.call(&vm, &.{});

    // pear_export.obj.dump();
    std.debug.print("pear export:\n{f}\n", .{pear_export});

    // const optimized_pear = try pear_export.get_field("optimize").call(&vm, &.{
    //     pear_export,
    // });

    // std.debug.print("optimized:\n{f}\n", .{optimized_pear});

    // const result = try pear_export.call(&vm, &.{try Val.new_int(&heap, 2)});
    // std.debug.print("result:\n{f}\n", .{result});

    std.debug.print("entering rendering mode\n", .{});

    var app = pear_export;

    // if (true) return;

    var gfx = try Graphics.init(ally);
    defer gfx.deinit();

    // // Cached drawing instructions.
    var previous_size = gfx.get_size();
    var drawing_instructions: ?[]const Graphics.DrawingInstruction = null;
    var drawing_instructions_ally: ?std.heap.ArenaAllocator = null;

    while (!gfx.should_close()) {
        const there_are_events = gfx.event_queue.items.len > 0;

        if (there_are_events) {
            for (gfx.event_queue.items) |event| {
                const pear_event = switch (event) {
                    .entered_char => |char| try Value.new_enum(
                        &heap,
                        "char",
                        try Value.new_int(&heap, @intCast(char.codepoint)),
                    ),
                    .pressed_key => |key| try Value.new_enum(
                        &heap,
                        "pressed-key",
                        try Value.new_struct(&heap, .{
                            .keycode = try Value.new_int(&heap, @intCast(key.keycode)),
                            .control = try Value.new_enum(
                                &heap,
                                if (key.control) "true" else "false",
                                try Value.new_struct(&heap, .{}),
                            ),
                            .shift = try Value.new_enum(
                                &heap,
                                if (key.shift) "true" else "false",
                                try Value.new_struct(&heap, .{}),
                            ),
                            .alt = try Value.new_enum(
                                &heap,
                                if (key.alt) "true" else "false",
                                try Value.new_struct(&heap, .{}),
                            ),
                        }),
                    ),
                };
                std.debug.print("Event: {f}\n", .{pear_event});
                app = try app.get_field("update").call(&vm, &.{pear_event});
            }
            gfx.event_queue.items.len = 0;
            app = Value.from(try vm.garbage_collect(start_of_heap, app.obj));
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

            const instructions = try app.get_field("render").call(&vm, &[_]Value{
                try Value.new_struct(&heap, .{
                    .width = try Value.new_int(&heap, size.width),
                    .height = try Value.new_int(&heap, size.height),
                }),
            });
            // std.debug.print("Rendered: {f}\n", .{instructions});

            drawing_instructions_ally = std.heap.ArenaAllocator.init(ally);
            drawing_instructions = try Graphics.parse_drawing_instructions(
                drawing_instructions_ally.?.allocator(),
                instructions,
            );
            // try Graphics.dump_drawing_instructions(drawing_instructions.?);
            // std.debug.print("{f}\n", .{app.get_field("state")});
        }

        // drawing_instructions = &.{
        //     .{
        //         .fill_path = .{
        //             .path = &.{
        //                 .{ .move_to = .{ .x = 10, .y = 10 } },
        //                 .{ .line_to = .{ .x = 100, .y = 10 } },
        //                 .{ .line_to = .{ .x = 50, .y = 100 } },
        //             },
        //             .color = .{ .r = 255, .g = 10, .b = 0 },
        //         },
        //     },
        // };

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
