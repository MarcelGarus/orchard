const std = @import("std");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const object_mod = @import("object.zig");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const builtin = @import("builtin");
const gl = @cImport({
    @cInclude("glad/glad.h");
    @cInclude("GLFW/glfw3.h");
});
const nvg = @import("nanovg");

window: ?*gl.GLFWwindow = null,
scale: f32,
vg: nvg,

const Graphics = @This();

pub const Color = struct { r: u8, g: u8, b: u8 };

pub const DrawingInstruction = union(enum) {
    draw_rectangle: struct {
        x: i64,
        y: i64,
        width: i64,
        height: i64,
        color: Color,
    },

    pub fn parse_all(ally: Ally, obj: Address, heap: Heap) ![]const DrawingInstruction {
        var current = obj;
        var out = ArrayList(DrawingInstruction).empty;
        while (heap.get(current).words.len != 0) {
            const words = heap.get(current).words;
            try out.append(ally, try parse(words[0], heap));
            current = words[1];
        }
        return out.items;
    }
    pub fn parse(obj: Address, heap: Heap) !DrawingInstruction {
        const symbol_obj = heap.load(obj, 0);
        const symbol = object_mod.get_symbol(heap, symbol_obj);

        if (std.mem.eql(u8, symbol, "draw rectangle")) {
            const x = object_mod.get_int(heap, heap.load(obj, 1));
            const y = object_mod.get_int(heap, heap.load(obj, 2));
            const width = object_mod.get_int(heap, heap.load(obj, 3));
            const height = object_mod.get_int(heap, heap.load(obj, 4));
            const r: u8 = @intCast(object_mod.get_int(heap, heap.load(obj, 5)));
            const g: u8 = @intCast(object_mod.get_int(heap, heap.load(obj, 6)));
            const b: u8 = @intCast(object_mod.get_int(heap, heap.load(obj, 7)));
            return .{ .draw_rectangle = .{
                .x = x,
                .y = y,
                .width = width,
                .height = height,
                .color = .{ .r = r, .g = g, .b = b },
            } };
        }
        @panic("unknown drawing instruction");
    }
};

fn keyCallback(window: ?*gl.GLFWwindow, key: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.c) void {
    _ = scancode;
    _ = mods;
    if (key == gl.GLFW_KEY_ESCAPE and action == gl.GLFW_PRESS)
        gl.glfwSetWindowShouldClose(window, gl.GL_TRUE);
}

pub fn init(ally: Ally) !Graphics {
    var window: ?*gl.GLFWwindow = null;

    if (gl.glfwInit() == gl.GLFW_FALSE) return error.GLFWInitFailed;
    gl.glfwWindowHint(gl.GLFW_CONTEXT_VERSION_MAJOR, 2);
    gl.glfwWindowHint(gl.GLFW_CONTEXT_VERSION_MINOR, 0);
    gl.glfwWindowHint(gl.GLFW_SAMPLES, 4);

    const monitor = gl.glfwGetPrimaryMonitor();
    var scale: f32 = 1;
    if (!builtin.target.os.tag.isDarwin())
        gl.glfwGetMonitorContentScale(monitor, &scale, null);
    window = gl.glfwCreateWindow(
        @as(i32, @intFromFloat(scale * 1000)),
        @as(i32, @intFromFloat(scale * 600)),
        "Thyme",
        null,
        null,
    );
    if (window == null) return error.GLFWInitFailed;

    _ = gl.glfwSetKeyCallback(window, keyCallback);

    gl.glfwMakeContextCurrent(window);

    if (gl.gladLoadGL() == 0) return error.GLADInitFailed;

    const vg = try nvg.gl.init(ally, .{ .stencil_strokes = true, .debug = true });

    gl.glfwSwapInterval(0);

    gl.glfwSetTime(0);

    return .{ .window = window, .scale = scale, .vg = vg };
}

pub fn deinit(self: Graphics) void {
    defer gl.glfwTerminate();
    defer gl.glfwDestroyWindow(self.window);
    defer self.vg.deinit();
}

pub fn should_close(self: Graphics) bool {
    return gl.glfwWindowShouldClose(self.window) == gl.GLFW_TRUE;
}

pub const Position = struct { x: usize, y: usize };
pub fn get_mouse_pos(self: Graphics) Position {
    var double_mx: f64 = undefined;
    var double_my: f64 = undefined;
    gl.glfwGetCursorPos(self.window, &double_mx, &double_my);
    const mx = @as(f32, @floatCast(double_mx)) / self.scale;
    const my = @as(f32, @floatCast(double_my)) / self.scale;
    return .{ .mx = @intFromFloat(mx), .my = @intFromFloat(my) };
}

pub const Size = struct { width: usize, height: usize };
pub fn get_size(self: Graphics) Size {
    var int_width: i32 = undefined;
    var int_height: i32 = undefined;
    gl.glfwGetWindowSize(self.window, &int_width, &int_height);
    const width = @as(f32, @floatFromInt(int_width)) / self.scale;
    const height = @as(f32, @floatFromInt(int_height)) / self.scale;
    return .{ .width = @intFromFloat(width), .height = @intFromFloat(height) };
}

pub fn get_time(self: Graphics) f32 {
    _ = self;
    const double_t = gl.glfwGetTime();
    const t = @as(f32, @floatCast(double_t));
    return t;
}

pub fn poll_events(self: Graphics) void {
    _ = self;
    gl.glfwPollEvents();
}

pub fn render(self: Graphics, size: Size, instructions: []const DrawingInstruction) !void {
    // Calculate pixel ratio for hi-dpi devices.
    var fb_width: i32 = undefined;
    var fb_height: i32 = undefined;
    gl.glfwGetFramebufferSize(self.window, &fb_width, &fb_height);
    const pxRatio = @as(f32, @floatFromInt(fb_width)) / @as(f32, @floatFromInt(size.width));

    // Update and render
    gl.glViewport(0, 0, fb_width, fb_height);
    gl.glClearColor(1, 1, 1, 1);
    gl.glClear(gl.GL_COLOR_BUFFER_BIT | gl.GL_DEPTH_BUFFER_BIT | gl.GL_STENCIL_BUFFER_BIT);

    self.vg.beginFrame(@floatFromInt(size.width), @floatFromInt(size.height), pxRatio);

    drawEyes(self.vg, @as(f32, @floatFromInt(size.width)) - 250, 50, 150, 100, 50, 50, 1);
    drawGraph(
        self.vg,
        0,
        @as(f32, @floatFromInt(size.height)) / 2,
        @floatFromInt(size.width),
        @as(f32, @floatFromInt(size.height)) / 2,
        1,
    );
    try render_all(self.vg, instructions);

    self.vg.endFrame();

    gl.glfwSwapBuffers(self.window);

    //const width: usize = @intCast(rl.getScreenWidth());
    //const height: usize = @intCast(rl.getScreenHeight());
    //
    //var image = rl.Image.genColor(@intCast(width), @intCast(height), rl.Color.white);
    //defer image.unload();
    //try render_all(&image, instructions);
    //
    //rl.beginDrawing();
    //var rl_texture = try rl.Texture.fromImage(image);
    //rl.drawTexturePro(
    //    rl_texture,
    //    .{ .x = 0, .y = 0, .width = @floatFromInt(width), .height = @floatFromInt(height) },
    //    .{ .x = 0, .y = 0, .width = @floatFromInt(width), .height = @floatFromInt(height) },
    //    .{ .x = 0.0, .y = 0.0 },
    //    0.0,
    //    .white,
    //);
    //defer rl_texture.unload();
    //rl.endDrawing();
}

fn render_all(vg: nvg, instructions: []const DrawingInstruction) !void {
    for (instructions) |instruction| try render_single(vg, instruction);
}
fn render_single(vg: nvg, instruction: DrawingInstruction) !void {
    switch (instruction) {
        .draw_rectangle => |args| {
            const x: f32 = @floatFromInt(args.x);
            const y: f32 = @floatFromInt(args.y);
            const width: f32 = @floatFromInt(args.width);
            const height: f32 = @floatFromInt(args.height);
            vg.beginPath();
            vg.moveTo(x, y);
            vg.lineTo(x + width, y);
            vg.lineTo(x + width, y + height);
            vg.lineTo(x, y + height);
            vg.fillColor(nvg.rgb(args.color.r, args.color.g, args.color.b));
            vg.fill();
        },
    }
}

fn drawEyes(vg: nvg, x: f32, y: f32, w: f32, h: f32, mx: f32, my: f32, t: f32) void {
    const ex = w * 0.23;
    const ey = h * 0.5;
    const lx = x + ex;
    const ly = y + ey;
    const rx = x + w - ex;
    const ry = y + ey;
    const br = (if (ex < ey) ex else ey) * 0.5;
    const blink = 1 - std.math.pow(f32, @sin(t * 0.5), 200) * 0.8;

    var bg = vg.linearGradient(x, y + h * 0.5, x + w * 0.1, y + h, nvg.rgba(0, 0, 0, 32), nvg.rgba(0, 0, 0, 16));
    vg.beginPath();
    vg.ellipse(lx + 3.0, ly + 16.0, ex, ey);
    vg.ellipse(rx + 3.0, ry + 16.0, ex, ey);
    vg.fillPaint(bg);
    vg.fill();

    bg = vg.linearGradient(x, y + h * 0.25, x + w * 0.1, y + h, nvg.rgba(220, 220, 220, 255), nvg.rgba(128, 128, 128, 255));
    vg.beginPath();
    vg.ellipse(lx, ly, ex, ey);
    vg.ellipse(rx, ry, ex, ey);
    vg.fillPaint(bg);
    vg.fill();

    var dx = (mx - rx) / (ex * 10);
    var dy = (my - ry) / (ey * 10);
    var d = @sqrt(dx * dx + dy * dy);
    if (d > 1.0) {
        dx /= d;
        dy /= d;
    }
    dx *= ex * 0.4;
    dy *= ey * 0.5;
    vg.beginPath();
    vg.ellipse(lx + dx, ly + dy + ey * 0.25 * (1 - blink), br, br * blink);
    vg.fillColor(nvg.rgba(32, 32, 32, 255));
    vg.fill();

    dx = (mx - rx) / (ex * 10);
    dy = (my - ry) / (ey * 10);
    d = @sqrt(dx * dx + dy * dy);
    if (d > 1.0) {
        dx /= d;
        dy /= d;
    }
    dx *= ex * 0.4;
    dy *= ey * 0.5;
    vg.beginPath();
    vg.ellipse(rx + dx, ry + dy + ey * 0.25 * (1 - blink), br, br * blink);
    vg.fillColor(nvg.rgba(32, 32, 32, 255));
    vg.fill();

    var gloss = vg.radialGradient(lx - ex * 0.25, ly - ey * 0.5, ex * 0.1, ex * 0.75, nvg.rgba(255, 255, 255, 128), nvg.rgba(255, 255, 255, 0));
    vg.beginPath();
    vg.ellipse(lx, ly, ex, ey);
    vg.fillPaint(gloss);
    vg.fill();

    gloss = vg.radialGradient(rx - ex * 0.25, ry - ey * 0.5, ex * 0.1, ex * 0.75, nvg.rgba(255, 255, 255, 128), nvg.rgba(255, 255, 255, 0));
    vg.beginPath();
    vg.ellipse(rx, ry, ex, ey);
    vg.fillPaint(gloss);
    vg.fill();
}

fn drawGraph(vg: nvg, x: f32, y: f32, w: f32, h: f32, t: f32) void {
    const dx = w / 5.0;

    const samples = [_]f32{
        (1 + @sin(t * 1.2345 + @cos(t * 0.33457) * 0.44)) * 0.5,
        (1 + @sin(t * 0.68363 + @cos(t * 1.3) * 1.55)) * 0.5,
        (1 + @sin(t * 1.1642 + @cos(t * 0.33457) * 1.24)) * 0.5,
        (1 + @sin(t * 0.56345 + @cos(t * 1.63) * 0.14)) * 0.5,
        (1 + @sin(t * 1.6245 + @cos(t * 0.254) * 0.3)) * 0.5,
        (1 + @sin(t * 0.345 + @cos(t * 0.03) * 0.6)) * 0.5,
    };

    var sx: [6]f32 = undefined;
    var sy: [6]f32 = undefined;
    for (samples, 0..) |sample, i| {
        sx[i] = x + @as(f32, @floatFromInt(i)) * dx;
        sy[i] = y + h * sample * 0.8;
    }

    // Graph background
    var bg = vg.linearGradient(x, y, x, y + h, nvg.rgba(0, 160, 192, 0), nvg.rgba(0, 160, 192, 64));
    vg.beginPath();
    vg.moveTo(sx[0], sy[0]);
    var i: u32 = 1;
    while (i < 6) : (i += 1)
        vg.bezierTo(sx[i - 1] + dx * 0.5, sy[i - 1], sx[i] - dx * 0.5, sy[i], sx[i], sy[i]);
    vg.lineTo(x + w, y + h);
    vg.lineTo(x, y + h);
    vg.fillPaint(bg);
    vg.fill();

    // Graph line
    vg.beginPath();
    vg.moveTo(sx[0], sy[0] + 2);
    i = 1;
    while (i < 6) : (i += 1)
        vg.bezierTo(sx[i - 1] + dx * 0.5, sy[i - 1] + 2, sx[i] - dx * 0.5, sy[i] + 2, sx[i], sy[i] + 2);
    vg.strokeColor(nvg.rgba(0, 0, 0, 32));
    vg.strokeWidth(3.0);
    vg.stroke();

    vg.beginPath();
    vg.moveTo(sx[0], sy[0]);

    i = 1;
    while (i < 6) : (i += 1)
        vg.bezierTo(sx[i - 1] + dx * 0.5, sy[i - 1], sx[i] - dx * 0.5, sy[i], sx[i], sy[i]);
    vg.strokeColor(nvg.rgba(0, 160, 192, 255));
    vg.strokeWidth(3.0);
    vg.stroke();

    // Graph sample pos
    i = 0;
    while (i < 6) : (i += 1) {
        bg = vg.radialGradient(sx[i], sy[i] + 2, 3.0, 8.0, nvg.rgba(0, 0, 0, 32), nvg.rgba(0, 0, 0, 0));
        vg.beginPath();
        vg.rect(sx[i] - 10, sy[i] - 10 + 2, 20, 20);
        vg.fillPaint(bg);
        vg.fill();
    }

    vg.beginPath();
    i = 0;
    while (i < 6) : (i += 1)
        vg.circle(sx[i], sy[i], 4.0);
    vg.fillColor(nvg.rgba(0, 160, 192, 255));
    vg.fill();
    vg.beginPath();
    i = 0;
    while (i < 6) : (i += 1)
        vg.circle(sx[i], sy[i], 2.0);
    vg.fillColor(nvg.rgba(220, 220, 220, 255));
    vg.fill();

    vg.strokeWidth(1.0);
}
