const Value = @import("value.zig");
const object_loader = @import("object_loader.zig");
const Heap = @import("heap.zig");
const Vm = @import("vm.zig");
const Graphics = @import("graphics.zig");

const std = @import("std");
const Ally = std.mem.Allocator;
const Io = std.Io;

// Similar to Smalltalk, I want all my data to be part of a value / an "image"
// that can evolve over time. Unlike Smalltalk, I want to support multiple
// clients working on the same value. For example, I might have a version of
// Orchard on my laptop and my phone and data (notes, calendar events, ...)
// should sync between them.
//
// To do that, I separate two pieces of state:
// - Global state that is syncronized between devices.
// - Local state that is per-client.
//
// Both the global store and the local state are just values, so it's easy to
// move a piece from the local to the global state.
//
// If Orchard instances would just proclaim "this is the new global state", then
// it would be difficult to merge updates (for example, if there's some network
// latency so that concurrent updates happen). To fix that, Orchard instances
// only emit actions ("add avocado to the shopping list"), which can be applied
// to the state. Then, multiple clients can work on the same global state.
//
// +-------+
// | store | <-----> Orchard instance on laptop
// |       | <-----> Orchard instance on phone
// |       | <-----> headless daemon that fetches e.g. RSS feeds
// +-------+
//
// There are multiple ways of doing synchronization (including peer-to-peer),
// but here's a simpler server-client scenario:
//
// | Our client fetches some server state S1 and starts with it.
// S1
// | Perform actions A and B: Send them to the server. Also eagerly apply them
// | locally so that everything feels instantaneous.
// S1 + A + B
// | The server sends us A, so we know it has been "committed" to the server
// | state. New server state S2 = S1 + A.
// S2 + B
// | The server sends us X, an action by another device. We use S3 = S2 + X as
// | the new baseline of our history, applying all uncommitted changes to that.
// S3 + B
// | The server sends us B. S4 = S3 + B.
// S4
//
// In the end, the server serializes actions (A, X, B) and this is kept
// consistent between all clients.

// TODO: For now, I don't do any synchronization yet. I just read the global
// state from a file and when an action is applied, I calculate the new state
// and write it into the file.
const FileStore = struct {
    path: []const u8,
    apply: Value, // (state, action) -> state
    state: Value,

    fn init(ally: Ally, io: Io, heap: *Heap, path: []const u8, apply: Value) !@This() {
        return .{
            .path = path,
            .apply = apply,
            .state = Value.from(try object_loader.load(
                ally,
                heap,
                try Io.Dir.cwd().readFileAlloc(io, path, ally, .unlimited),
            )),
        };
    }
    fn add_action(self: *@This(), ally: Ally, io: Io, vm: anytype, action: Value) !void {
        self.state = try self.apply.call(vm, &.{ self.state, action });
        std.debug.print("Action: {f}\n", .{action});
        std.debug.print("global state: {f}\n", .{self.state});

        const out = try std.Io.Dir.cwd().createFile(io, self.path, .{});
        var out_buf: [1000]u8 = undefined;
        var out_w = out.writer(io, &out_buf);
        try Heap.file_out(self.state.obj, ally, &out_w.interface);
    }
};

// Given some changing data source, app instances work like this:
//
// +--------------+             +--------------+             +----------------------+
// | global state | --create--> | local state  | --render--> | drawing instructions |
// +--------------+             +--------------+             +----------------------+
//                                |          ^
// +------------------------+     |          |
// | events                 | ----+--handle--+
// | - key presses          |
// | - global state updates |
// | - ...                  |
// +------------------------+

pub fn run(ally: Ally, io: Io, heap: *Heap, vm: anytype, app_: Value, data_file: []const u8) !void {
    var global = try FileStore.init(ally, io, heap, data_file, app_.field("apply"));
    std.debug.print("global state: {f}\n", .{global.state});

    var app = app_;
    var state = try app.field("create").call(vm, &.{global.state});

    std.debug.print("entering rendering mode\n", .{});

    var gfx = try Graphics.init(ally);
    defer gfx.deinit();

    // Cached drawing instructions.
    var previous_size = gfx.get_size();
    var drawing_instructions: ?[]const Graphics.DrawingInstruction = null;
    var drawing_instructions_ally: ?std.heap.ArenaAllocator = null;

    while (!gfx.should_close()) {
        const there_are_events = gfx.event_queue.items.len > 0;

        if (there_are_events) {
            var global_state_changed = false;
            for (gfx.event_queue.items) |event| {
                const pear_event = switch (event) {
                    .entered_char => |char| blk: {
                        var buf: [4]u8 = undefined;
                        const len = std.unicode.utf8Encode(@intCast(char.codepoint), &buf) catch break :blk try Value.new_enum(heap, "char", try Value.new_string(heap, "?"));
                        break :blk try Value.new_enum(
                            heap,
                            "char",
                            try Value.new_string(heap, buf[0..len]),
                        );
                    },
                    .pressed_key => |key| try Value.new_enum(
                        heap,
                        "pressed-key",
                        try Value.new_struct(heap, .{
                            .keycode = try Value.new_int(heap, @intCast(key.keycode)),
                            .control = try Value.new_bool(heap, key.control),
                            .shift = try Value.new_bool(heap, key.shift),
                            .alt = try Value.new_bool(heap, key.alt),
                        }),
                    ),
                };
                std.debug.print("Event: {f}\n", .{pear_event});
                var result = try app.field("handle").call(vm, &.{ state, pear_event });
                state = result.field("state");
                if (result.field("action").option()) |action| {
                    try global.add_action(ally, io, vm, action);
                    global_state_changed = true;
                }
            }
            gfx.event_queue.items.len = 0;

            if (global_state_changed) {
                const event = try Value.new_enum(heap, "new-state", global.state);
                state = (try app.field("handle").call(vm, &.{ state, event })).field("state");
            }

            const mapped = Value.from(try vm.garbage_collect(
                heap.start(),
                (try Value.new_struct(heap, .{
                    .global_state = global.state,
                    .global_apply = global.apply,
                    .app = app,
                    .local_state = state,
                })).obj,
            ));
            global.state = mapped.field("global_state");
            global.apply = mapped.field("global_apply");
            app = mapped.field("app");
            state = mapped.field("local_state");
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

            const instructions = try app.field("render").call(vm, &[_]Value{
                state,
                try Value.new_struct(heap, .{
                    .width = try Value.new_float(heap, size.width),
                    .height = try Value.new_float(heap, size.height),
                }),
            });
            // std.debug.print("Rendered: {f}\n", .{instructions});

            drawing_instructions_ally = std.heap.ArenaAllocator.init(ally);
            drawing_instructions = try Graphics.parse_drawing_instructions(
                drawing_instructions_ally.?.allocator(),
                instructions,
            );
            // try Graphics.dump_drawing_instructions(drawing_instructions.?);
            // std.debug.print("{f}\n", .{app.field("state")});
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
}
