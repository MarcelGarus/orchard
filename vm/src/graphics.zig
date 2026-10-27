//! Window input, draw command decoding, and OpenGL rendering.
const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const builtin = @import("builtin");
const gl = @cImport({
    @cInclude("glad/glad.h");
    @cInclude("GLFW/glfw3.h");
});
const nvg = @import("nanovg");
const Obj = @import("heap.zig").Obj;

const Graphics = @This();

pub const Size = struct { width: f64, height: f64 };
pub const Position = struct { x: f64, y: f64 };
pub const TransformationMatrix = struct { a: f64, b: f64, c: f64, d: f64, e: f64, f: f64 };
pub const Color = struct { r: u8, g: u8, b: u8 };
pub const Path = []const PathSegment;
pub const PathSegment = union(enum) { move_to: Position, line_to: Position };
pub const DrawCommand = union(enum) {
    fill_path: struct { path: Path, color: Color },
    transform: struct { by: TransformationMatrix, children: []const DrawCommand },
};

pub fn float(value: Obj) f64 {
    return @bitCast(value.word(0));
}

fn position(value: Obj) Position {
    return .{ .x = float(value.child(0)), .y = float(value.child(1)) };
}

fn color(value: Obj) Color {
    return .{
        .r = @intCast(value.child(0).word(0)),
        .g = @intCast(value.child(1).word(0)),
        .b = @intCast(value.child(2).word(0)),
    };
}

fn matrix(value: Obj) TransformationMatrix {
    return .{
        .a = float(value.child(0)),
        .b = float(value.child(1)),
        .c = float(value.child(2)),
        .d = float(value.child(3)),
        .e = float(value.child(4)),
        .f = float(value.child(5)),
    };
}

fn path(ally: Ally, value: Obj) !Path {
    // Path { segments: List[PathSegment] }; move_to = 0, line_to = 1.
    const segments = value.child(0).children();
    const result = try ally.alloc(PathSegment, segments.len);
    for (segments, result) |segment, *out| {
        const pos = position(segment.child(1));
        out.* = switch (segment.child(0).word(0)) {
            0 => .{ .move_to = pos },
            1 => .{ .line_to = pos },
            else => return error.UnknownPathSegment,
        };
    }
    return result;
}

fn append_commands(ally: Ally, commands: *std.ArrayList(DrawCommand), value: Obj) error{ OutOfMemory, UnknownPathSegment, UnknownDrawCommand }!void {
    // DrawCommand: nothing = 0, fill_path = 1, transform = 2, all = 3.
    const payload = value.child(1);
    switch (value.child(0).word(0)) {
        0 => {},
        1 => try commands.append(ally, .{ .fill_path = .{
            .path = try path(ally, payload.child(0)),
            .color = color(payload.child(1)),
        } }),
        2 => {
            var children: std.ArrayList(DrawCommand) = .empty;
            try append_commands(ally, &children, payload.child(1));
            try commands.append(ally, .{ .transform = .{
                .by = matrix(payload.child(0)),
                .children = children.items,
            } });
        },
        3 => for (payload.children()) |item| try append_commands(ally, commands, item),
        else => return error.UnknownDrawCommand,
    }
}

/// Allocates command lists and paths in ally; use a frame arena to free them together.
pub fn parse(ally: Ally, value: Obj) ![]const DrawCommand {
    var commands: std.ArrayList(DrawCommand) = .empty;
    try append_commands(ally, &commands, value);
    return commands.items;
}

ally: Ally,
window: *gl.GLFWwindow,
scale: f32,
vg: nvg,
event_queue: ArrayList(Event),

pub const Event = @import("app.zig").Event;

fn glfwErrorCallback(error_code: c_int, description: [*c]const u8) callconv(.c) void {
    std.debug.print("GLFW Error [{d}]: {s}\n", .{ error_code, description });
}
pub fn init(ally: Ally) !*Graphics {
    _ = gl.glfwSetErrorCallback(glfwErrorCallback);
    if (gl.glfwInit() == gl.GLFW_FALSE) return error.GLFWInitFailed;
    errdefer gl.glfwTerminate();
    gl.glfwWindowHint(gl.GLFW_CONTEXT_VERSION_MAJOR, 2);
    gl.glfwWindowHint(gl.GLFW_CONTEXT_VERSION_MINOR, 0);
    gl.glfwWindowHint(gl.GLFW_SAMPLES, 4);

    var scale: f32 = 1;
    if (!builtin.target.os.tag.isDarwin()) {
        const monitor = gl.glfwGetPrimaryMonitor() orelse return error.NoPrimaryMonitor;
        gl.glfwGetMonitorContentScale(monitor, &scale, null);
    }
    scale *= 0.7;

    const window = gl.glfwCreateWindow(
        @as(i32, @intFromFloat(scale * 500)),
        @as(i32, @intFromFloat(scale * 300)),
        "Orchard",
        null,
        null,
    ) orelse return error.WindowCreationFailed;
    errdefer gl.glfwDestroyWindow(window);
    _ = gl.glfwSetCharCallback(window, charCallback);
    _ = gl.glfwSetKeyCallback(window, keyCallback);
    _ = gl.glfwSetMouseButtonCallback(window, mouseButtonCallback);
    _ = gl.glfwSetCursorPosCallback(window, cursorPosCallback);
    gl.glfwMakeContextCurrent(window);
    if (gl.gladLoadGL() == 0) return error.GLADInitFailed;
    gl.glfwSwapInterval(1);
    gl.glfwSetTime(0);
    const vg = try nvg.gl.init(ally, .{ .stencil_strokes = true, .debug = true });
    errdefer vg.deinit();

    // GLFW callbacks retrieve the graphics state through the window user pointer.
    const graphics = try ally.create(Graphics);
    graphics.* = .{
        .ally = ally,
        .window = window,
        .scale = scale,
        .vg = vg,
        .event_queue = ArrayList(Event).empty,
    };
    gl.glfwSetWindowUserPointer(window, graphics);
    return graphics;
}

fn charCallback(window: ?*gl.GLFWwindow, codepoint: c_uint) callconv(.c) void {
    const graphics: *Graphics = @ptrCast(@alignCast(gl.glfwGetWindowUserPointer(window)));
    graphics.event_queue.append(graphics.ally, .{
        .char_entered = .{ .codepoint = codepoint },
    }) catch @panic("couldn't append event");
}

fn keyCallback(window: ?*gl.GLFWwindow, key: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.c) void {
    _ = scancode;
    const graphics: *Graphics = @ptrCast(@alignCast(gl.glfwGetWindowUserPointer(window)));
    if (action == gl.GLFW_PRESS or action == gl.GLFW_REPEAT) {
        graphics.event_queue.append(graphics.ally, .{
            .key_pressed = .{
                .keycode = @intCast(key),
                .control = mods & gl.GLFW_MOD_CONTROL != 0,
                .alt = mods & gl.GLFW_MOD_ALT != 0,
                .shift = mods & gl.GLFW_MOD_SHIFT != 0,
            },
        }) catch @panic("couldn't append event");
    }
}

fn cursorPosCallback(window: ?*gl.GLFWwindow, x: f64, y: f64) callconv(.c) void {
    const graphics: *Graphics = @ptrCast(@alignCast(gl.glfwGetWindowUserPointer(window)));
    const scale: f64 = @floatCast(graphics.scale);
    const event = Event{ .mouse_moved = .{ .x = x / scale, .y = y / scale } };
    // Only the latest position matters until the next frame. Preserve the
    // ordering of movement relative to clicks and keyboard events.
    const items = graphics.event_queue.items;
    if (items.len > 0 and items[items.len - 1] == .mouse_moved) {
        items[items.len - 1] = event;
    } else {
        graphics.event_queue.append(graphics.ally, event) catch @panic("couldn't append event");
    }
}

fn mouseButtonCallback(window: ?*gl.GLFWwindow, button: c_int, action: c_int, mods: c_int) callconv(.c) void {
    _ = mods;
    if (button != gl.GLFW_MOUSE_BUTTON_LEFT or action != gl.GLFW_PRESS) return;

    const graphics: *Graphics = @ptrCast(@alignCast(gl.glfwGetWindowUserPointer(window)));
    graphics.event_queue.append(graphics.ally, .{
        .mouse_clicked = graphics.get_mouse_position(),
    }) catch @panic("couldn't append event");
}

pub fn deinit(self: *Graphics) void {
    self.event_queue.deinit(self.ally);
    self.vg.deinit();
    gl.glfwDestroyWindow(self.window);
    gl.glfwTerminate();
    self.ally.destroy(self);
}

pub fn should_close(self: Graphics) bool {
    return gl.glfwWindowShouldClose(self.window) == gl.GLFW_TRUE;
}

pub fn get_mouse_position(self: Graphics) Position {
    var double_mx: f64 = undefined;
    var double_my: f64 = undefined;
    gl.glfwGetCursorPos(self.window, &double_mx, &double_my);
    const scale: f64 = @floatCast(self.scale);
    return .{ .x = double_mx / scale, .y = double_my / scale };
}

pub fn get_size(self: Graphics) Size {
    var int_width: i32 = undefined;
    var int_height: i32 = undefined;
    gl.glfwGetWindowSize(self.window, &int_width, &int_height);
    const scale: f64 = @floatCast(self.scale);
    return .{
        .width = @as(f64, @floatFromInt(int_width)) / scale,
        .height = @as(f64, @floatFromInt(int_height)) / scale,
    };
}

pub fn wait_events(self: Graphics) void {
    _ = self;
    // Sleep until input or a window event requires another frame.
    gl.glfwWaitEvents();
}

pub fn render(self: Graphics, size: Size, commands: []const DrawCommand) void {
    if (size.width <= 0 or size.height <= 0) return;
    var fb_width: i32 = undefined;
    var fb_height: i32 = undefined;
    gl.glfwGetFramebufferSize(self.window, &fb_width, &fb_height);
    if (fb_width <= 0 or fb_height <= 0) return;
    const pixel_ratio = @as(f32, @floatFromInt(fb_width)) / @as(f32, @floatCast(size.width));

    gl.glViewport(0, 0, fb_width, fb_height);
    gl.glClearColor(1, 1, 1, 1);
    gl.glClear(gl.GL_COLOR_BUFFER_BIT | gl.GL_DEPTH_BUFFER_BIT | gl.GL_STENCIL_BUFFER_BIT);

    self.vg.beginFrame(@floatCast(size.width), @floatCast(size.height), pixel_ratio);

    render_commands(self.vg, commands);

    self.vg.endFrame();
    gl.glfwSwapBuffers(self.window);
}

fn render_commands(vg: nvg, commands: []const DrawCommand) void {
    for (commands) |command| render_command(vg, command);
}
fn render_command(vg: nvg, command: DrawCommand) void {
    switch (command) {
        .fill_path => |args| {
            vg.beginPath();
            for (args.path) |segment| {
                switch (segment) {
                    .move_to => |pos| vg.moveTo(@floatCast(pos.x), @floatCast(pos.y)),
                    .line_to => |pos| vg.lineTo(@floatCast(pos.x), @floatCast(pos.y)),
                }
            }
            vg.fillColor(nvg.rgb(args.color.r, args.color.g, args.color.b));
            vg.fill();
        },
        .transform => |t| {
            vg.save();
            vg.transform(
                @floatCast(t.by.a),
                @floatCast(t.by.b),
                @floatCast(t.by.c),
                @floatCast(t.by.d),
                @floatCast(t.by.e),
                @floatCast(t.by.f),
            );
            render_commands(vg, t.children);
            vg.restore();
        },
    }
}
