//! Yam app state, event encoding, and the window event loop.
const std = @import("std");
const Heap = @import("heap.zig");
const Obj = Heap.Obj;
const Vm = @import("vm.zig");
const Graphics = @import("graphics.zig");
const Self = @This();

// App { render: fun(Size) DrawCommand, handle: fun(Event) App }.
value: Obj,

// Variant order matches the Yam Event enum.
pub const Event = union(enum(u64)) {
    window_resized: Graphics.Size,
    key_pressed: KeyPressed,
    char_entered: CharEntered,
    mouse_clicked: Graphics.Position,
    mouse_moved: Graphics.Position,

    pub const KeyPressed = struct { keycode: i32, control: bool, shift: bool, alt: bool };
    pub const CharEntered = struct { codepoint: u32 };
};

pub fn init(value: Obj) Self {
    return .{ .value = value };
}

// A Yam closure contains its VM function and captured environment. The
// environment is passed after the function's explicit argument.
fn call(vm: anytype, closure: Obj, arg: Obj) !Obj {
    var fuel: usize = Vm.max_fuel;
    return switch (try vm.call(.{ .obj = closure.child(0) }, &.{ arg, closure.child(1) }, &fuel)) {
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
        .mouse_clicked => |pos| try pair_object(heap, pos.x, pos.y),
        .mouse_moved => |pos| try pair_object(heap, pos.x, pos.y),
    };
    return heap.new_inner(&.{ try heap.new_leaf(&.{tag}), payload });
}

pub fn handle(self: *Self, vm: anytype, event: Event) !void {
    self.value = try call(vm, self.value.child(1), try event_object(vm.heap, event));
}

pub fn render(self: Self, vm: anytype, size: Graphics.Size) !Obj {
    return call(vm, self.value.child(0), try pair_object(vm.heap, size.width, size.height));
}

pub fn run(ally: std.mem.Allocator, vm: anytype, value: Obj) !void {
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
        if (first_frame or resized) try app.handle(vm, .{ .window_resized = size });
        if (first_frame) try app.handle(vm, .{ .mouse_moved = gfx.get_mouse_position() });
        for (gfx.event_queue.items) |event| try app.handle(vm, event);
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
            gfx.render(size, commands);
        }
        gfx.wait_events();
    }
}
