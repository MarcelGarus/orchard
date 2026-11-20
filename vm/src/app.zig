//! Yam app state, event encoding, and the window event loop.
const std = @import("std");
const Heap = @import("heap.zig");
const Obj = Heap.Obj;
const Vm = @import("vm.zig");
const Graphics = @import("graphics.zig");
const Self = @This();
const Notebook = @import("notebook.zig");

// App { render: fun(Size) DrawCommand, handle: fun(Event) AppResult }.
value: Obj,

// Variant order matches the Yam Event enum.
pub const Event = union(enum(u64)) {
    window_resized: Graphics.Size,
    key_pressed: KeyPressed,
    char_entered: CharEntered,
    mouse_pressed: Graphics.Position,
    mouse_moved: Graphics.Position,
    mouse_scrolled: MouseScrolled,
    mouse_released: Graphics.Position,
    mouse_left: void,
    save_completed: void,
    save_failed: []const u8,

    pub const MouseScrolled = struct { position: Graphics.Position, delta: Graphics.Offset };
    pub const KeyPressed = struct { keycode: i32, control: bool, shift: bool, alt: bool };
    pub const CharEntered = struct { codepoint: u32 };
};

pub fn init(value: Obj) Self {
    return .{ .value = value };
}

// A Yam function pairs a raw descriptor with its captured environment. The
// descriptor keeps executable code in slot 4, after its reflected metadata.
// Captures are passed after the function's explicit argument.
fn call(vm: anytype, closure: Obj, arg: Obj) !Obj {
    var fuel: usize = Vm.max_fuel;
    return switch (try vm.call(.{ .obj = closure.child(0).child(4) }, &.{ arg, closure.child(1) }, &fuel)) {
        .returned => |value| value,
        .crashed => |message| {
            std.debug.print("App crashed: {f}\n", .{message});
            return error.ProgramCrashed;
        },
        .out_of_fuel => error.VmOutOfFuel,
        .out_of_memory => error.VmOutOfMemory,
    };
}

fn float_object(heap: *Heap, value: f64) !Obj {
    return heap.new_leaf(&.{@bitCast(value)});
}

fn pair_object(heap: *Heap, x: f64, y: f64) !Obj {
    return heap.new_inner(&.{ try float_object(heap, x), try float_object(heap, y) });
}

fn bool_object(heap: *Heap, value: bool) !Obj {
    return heap.new_inner(&.{ try heap.new_leaf(&.{@intFromBool(value)}), try heap.new_inner(&.{}) });
}

pub fn event_object(heap: *Heap, event: Event) !Obj {
    const tag = @intFromEnum(std.meta.activeTag(event));
    const payload = switch (event) {
        .window_resized => |size| try pair_object(heap, size.width, size.height),
        .key_pressed => |key| try heap.new_inner(&.{
            try heap.new_leaf(&.{@bitCast(@as(i64, key.keycode))}),
            try bool_object(heap, key.control),
            try bool_object(heap, key.shift),
            try bool_object(heap, key.alt),
        }),
        .char_entered => |char| try heap.new_inner(&.{try heap.new_leaf(&.{char.codepoint})}),
        .mouse_pressed => |pos| try pair_object(heap, pos.x, pos.y),
        .mouse_released => |pos| try pair_object(heap, pos.x, pos.y),
        .mouse_left, .save_completed => try heap.new_inner(&.{}),
        .save_failed => |message| try heap.new_symbol(message),
        .mouse_moved => |pos| try pair_object(heap, pos.x, pos.y),
        .mouse_scrolled => |scroll| try heap.new_inner(&.{
            try pair_object(heap, scroll.position.x, scroll.position.y),
            try pair_object(heap, scroll.delta.x, scroll.delta.y),
        }),
    };
    return heap.new_inner(&.{ try heap.new_leaf(&.{tag}), payload });
}

pub fn handle(self: *Self, vm: anytype, event: Event) !Obj {
    const result = try call(vm, self.value.child(1), try event_object(vm.heap, event));
    if (!result.is_inner() or result.size() != 2 or !result.child(1).is_inner())
        return error.InvalidAppResult;
    self.value = result.child(0);
    return result.child(1);
}

pub fn save_command(io: std.Io, allocator: std.mem.Allocator, command: Obj, source_path: ?[]const u8) !void {
    if (!command.is_inner() or command.size() != 2 or !command.child(0).is_leaf() or
        command.child(0).size() != 1 or command.child(0).word(0) != 0)
        return error.InvalidAppCommand;
    const path = source_path orelse return error.MissingNotebookSource;
    const notebook = try Notebook.decode(allocator, command.child(1));
    try Notebook.save(.cwd(), io, path, allocator, notebook);
}

pub fn process_event(self: *Self, io: std.Io, allocator: std.mem.Allocator, vm: anytype, event: Event, source_path: ?[]const u8) !void {
    const commands = try self.handle(vm, event);
    for (commands.children()) |command| {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        save_command(io, arena.allocator(), command, source_path) catch |err| {
            std.debug.print("Save failed: {s}\n", .{@errorName(err)});
            _ = try self.handle(vm, .{ .save_failed = @errorName(err) });
            continue;
        };
        std.debug.print("Saved {s}\n", .{source_path.?});
        _ = try self.handle(vm, .save_completed);
    }
}

pub fn render(self: Self, vm: anytype, size: Graphics.Size) !Obj {
    return call(vm, self.value.child(0), try pair_object(vm.heap, size.width, size.height));
}

pub fn run(io: std.Io, ally: std.mem.Allocator, vm: anytype, value: Obj, source_path: ?[]const u8) !void {
    var app = Self.init(value);
    var gfx = try Graphics.init(ally);
    defer gfx.deinit();
    var previous_size = gfx.get_size();
    var first_frame = true;
    const state_start = vm.heap.checkpoint();

    while (!gfx.should_close()) {
        const size = gfx.get_size();
        const resized = size.width != previous_size.width or size.height != previous_size.height;
        const redraw = first_frame or resized or gfx.event_queue.items.len != 0;
        if (first_frame or resized) try app.process_event(io, ally, vm, .{ .window_resized = size }, source_path);
        if (first_frame) try app.process_event(io, ally, vm, .{ .mouse_moved = gfx.get_mouse_position() }, source_path);
        for (gfx.event_queue.items) |event| try app.process_event(io, ally, vm, event, source_path);
        gfx.event_queue.clearRetainingCapacity();
        previous_size = size;
        first_frame = false;

        if (redraw) {
            // Collect obsolete app states and events before allocating frame data.
            app.value = try vm.garbage_collect(state_start, app.value);
            const frame_checkpoint = vm.heap.checkpoint();
            defer vm.heap.restore(frame_checkpoint);
            var frame = std.heap.ArenaAllocator.init(ally);
            defer frame.deinit();
            const draw_command = try app.render(vm, size);
            const commands = try Graphics.parse(frame.allocator(), draw_command);
            try gfx.render(size, commands);
        }
        gfx.wait_events();
    }
}
