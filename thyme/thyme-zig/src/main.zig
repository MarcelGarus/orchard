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
        const drawing_instructions_obj = try vm.call(
            heap.load(render_lambda, 0),
            &[_]Address{ width_obj, height_obj, heap.load(render_lambda, 1) },
        );
        const drawing_instructions = try Graphics.DrawingInstruction.parse_all(
            frame_ally,
            drawing_instructions_obj,
            vm.heap.*,
        );

        std.debug.print("{any}\n", .{drawing_instructions});
        try gfx.render(size, drawing_instructions);
        gfx.poll_events();
    }

    //heap.dump_stats();
    //_ = try heap.garbage_collect(ally, start_of_heap, result);
    //_ = try heap.deduplicate(ally, start_of_heap);
    //heap.dump_stats();

    // heap.dump_raw();
    // heap.dump();

    //    var window: ?*gl.GLFWwindow = null;
    //
    //    if (gl.glfwInit() == gl.GLFW_FALSE) return error.GLFWInitFailed;
    //    defer gl.glfwTerminate();
    //    gl.glfwWindowHint(gl.GLFW_CONTEXT_VERSION_MAJOR, 2);
    //    gl.glfwWindowHint(gl.GLFW_CONTEXT_VERSION_MINOR, 0);
    //    gl.glfwWindowHint(gl.GLFW_SAMPLES, 4);
    //
    //    const monitor = gl.glfwGetPrimaryMonitor();
    //    var scale: f32 = 1;
    //    if (!builtin.target.os.tag.isDarwin())
    //        gl.glfwGetMonitorContentScale(monitor, &scale, null);
    //    window = gl.glfwCreateWindow(
    //        @as(i32, @intFromFloat(scale * 1000)),
    //        @as(i32, @intFromFloat(scale * 600)),
    //        "Thyme",
    //        null,
    //        null,
    //    );
    //    if (window == null) return error.GLFWInitFailed;
    //    defer gl.glfwDestroyWindow(window);
    //
    //    _ = gl.glfwSetKeyCallback(window, keyCallback);
    //
    //    gl.glfwMakeContextCurrent(window);
    //
    //    if (gl.gladLoadGL() == 0) return error.GLADInitFailed;
    //
    //    var vg = try nvg.gl.init(ally, .{ .stencil_strokes = true, .debug = true });
    //    defer vg.deinit();
    //
    //    gl.glfwSwapInterval(0);
    //
    //    gl.glfwSetTime(0);
    //
    //    while (gl.glfwWindowShouldClose(window) == gl.GLFW_FALSE) {
    //        const double_t = gl.glfwGetTime();
    //        const t = @as(f32, @floatCast(double_t));
    //
    //        // Mouse position.
    //        var double_mx: f64 = undefined;
    //        var double_my: f64 = undefined;
    //        gl.glfwGetCursorPos(window, &double_mx, &double_my);
    //        const mx = @as(f32, @floatCast(double_mx)) / scale;
    //        const my = @as(f32, @floatCast(double_my)) / scale;
    //
    //        // Width and height.
    //        var int_width: i32 = undefined;
    //        var int_height: i32 = undefined;
    //        gl.glfwGetWindowSize(window, &int_width, &int_height);
    //        const width = @as(f32, @floatFromInt(int_width)) / scale;
    //        const height = @as(f32, @floatFromInt(int_height)) / scale;
    //
    //        // Calculate pixel ratio for hi-dpi devices.
    //        var fb_width: i32 = undefined;
    //        var fb_height: i32 = undefined;
    //        gl.glfwGetFramebufferSize(window, &fb_width, &fb_height);
    //        const pxRatio = @as(f32, @floatFromInt(fb_width)) / width;
    //
    //        // Update and render
    //        gl.glViewport(0, 0, fb_width, fb_height);
    //        gl.glClearColor(1, 1, 1, 1);
    //        gl.glClear(gl.GL_COLOR_BUFFER_BIT | gl.GL_DEPTH_BUFFER_BIT | gl.GL_STENCIL_BUFFER_BIT);
    //
    //        vg.beginFrame(width, height, pxRatio);
    //
    //        drawEyes(vg, width - 250, 50, 150, 100, mx, my, t);
    //        drawGraph(vg, 0, height / 2, width, height / 2, t);
    //
    //        vg.endFrame();
    //
    //        gl.glfwSwapBuffers(window);
    //        gl.glfwPollEvents();
    //    }
}

//fn drawEyes(vg: nvg, x: f32, y: f32, w: f32, h: f32, mx: f32, my: f32, t: f32) void {
//    const ex = w * 0.23;
//    const ey = h * 0.5;
//    const lx = x + ex;
//    const ly = y + ey;
//    const rx = x + w - ex;
//    const ry = y + ey;
//    const br = (if (ex < ey) ex else ey) * 0.5;
//    const blink = 1 - std.math.pow(f32, @sin(t * 0.5), 200) * 0.8;
//
//    var bg = vg.linearGradient(x, y + h * 0.5, x + w * 0.1, y + h, nvg.rgba(0, 0, 0, 32), nvg.rgba(0, 0, 0, 16));
//    vg.beginPath();
//    vg.ellipse(lx + 3.0, ly + 16.0, ex, ey);
//    vg.ellipse(rx + 3.0, ry + 16.0, ex, ey);
//    vg.fillPaint(bg);
//    vg.fill();
//
//    bg = vg.linearGradient(x, y + h * 0.25, x + w * 0.1, y + h, nvg.rgba(220, 220, 220, 255), nvg.rgba(128, 128, 128, 255));
//    vg.beginPath();
//    vg.ellipse(lx, ly, ex, ey);
//    vg.ellipse(rx, ry, ex, ey);
//    vg.fillPaint(bg);
//    vg.fill();
//
//    var dx = (mx - rx) / (ex * 10);
//    var dy = (my - ry) / (ey * 10);
//    var d = @sqrt(dx * dx + dy * dy);
//    if (d > 1.0) {
//        dx /= d;
//        dy /= d;
//    }
//    dx *= ex * 0.4;
//    dy *= ey * 0.5;
//    vg.beginPath();
//    vg.ellipse(lx + dx, ly + dy + ey * 0.25 * (1 - blink), br, br * blink);
//    vg.fillColor(nvg.rgba(32, 32, 32, 255));
//    vg.fill();
//
//    dx = (mx - rx) / (ex * 10);
//    dy = (my - ry) / (ey * 10);
//    d = @sqrt(dx * dx + dy * dy);
//    if (d > 1.0) {
//        dx /= d;
//        dy /= d;
//    }
//    dx *= ex * 0.4;
//    dy *= ey * 0.5;
//    vg.beginPath();
//    vg.ellipse(rx + dx, ry + dy + ey * 0.25 * (1 - blink), br, br * blink);
//    vg.fillColor(nvg.rgba(32, 32, 32, 255));
//    vg.fill();
//
//    var gloss = vg.radialGradient(lx - ex * 0.25, ly - ey * 0.5, ex * 0.1, ex * 0.75, nvg.rgba(255, 255, 255, 128), nvg.rgba(255, 255, 255, 0));
//    vg.beginPath();
//    vg.ellipse(lx, ly, ex, ey);
//    vg.fillPaint(gloss);
//    vg.fill();
//
//    gloss = vg.radialGradient(rx - ex * 0.25, ry - ey * 0.5, ex * 0.1, ex * 0.75, nvg.rgba(255, 255, 255, 128), nvg.rgba(255, 255, 255, 0));
//    vg.beginPath();
//    vg.ellipse(rx, ry, ex, ey);
//    vg.fillPaint(gloss);
//    vg.fill();
//}
//
//fn drawGraph(vg: nvg, x: f32, y: f32, w: f32, h: f32, t: f32) void {
//    const dx = w / 5.0;
//
//    const samples = [_]f32{
//        (1 + @sin(t * 1.2345 + @cos(t * 0.33457) * 0.44)) * 0.5,
//        (1 + @sin(t * 0.68363 + @cos(t * 1.3) * 1.55)) * 0.5,
//        (1 + @sin(t * 1.1642 + @cos(t * 0.33457) * 1.24)) * 0.5,
//        (1 + @sin(t * 0.56345 + @cos(t * 1.63) * 0.14)) * 0.5,
//        (1 + @sin(t * 1.6245 + @cos(t * 0.254) * 0.3)) * 0.5,
//        (1 + @sin(t * 0.345 + @cos(t * 0.03) * 0.6)) * 0.5,
//    };
//
//    var sx: [6]f32 = undefined;
//    var sy: [6]f32 = undefined;
//    for (samples, 0..) |sample, i| {
//        sx[i] = x + @as(f32, @floatFromInt(i)) * dx;
//        sy[i] = y + h * sample * 0.8;
//    }
//
//    // Graph background
//    var bg = vg.linearGradient(x, y, x, y + h, nvg.rgba(0, 160, 192, 0), nvg.rgba(0, 160, 192, 64));
//    vg.beginPath();
//    vg.moveTo(sx[0], sy[0]);
//    var i: u32 = 1;
//    while (i < 6) : (i += 1)
//        vg.bezierTo(sx[i - 1] + dx * 0.5, sy[i - 1], sx[i] - dx * 0.5, sy[i], sx[i], sy[i]);
//    vg.lineTo(x + w, y + h);
//    vg.lineTo(x, y + h);
//    vg.fillPaint(bg);
//    vg.fill();
//
//    // Graph line
//    vg.beginPath();
//    vg.moveTo(sx[0], sy[0] + 2);
//    i = 1;
//    while (i < 6) : (i += 1)
//        vg.bezierTo(sx[i - 1] + dx * 0.5, sy[i - 1] + 2, sx[i] - dx * 0.5, sy[i] + 2, sx[i], sy[i] + 2);
//    vg.strokeColor(nvg.rgba(0, 0, 0, 32));
//    vg.strokeWidth(3.0);
//    vg.stroke();
//
//    vg.beginPath();
//    vg.moveTo(sx[0], sy[0]);
//
//    i = 1;
//    while (i < 6) : (i += 1)
//        vg.bezierTo(sx[i - 1] + dx * 0.5, sy[i - 1], sx[i] - dx * 0.5, sy[i], sx[i], sy[i]);
//    vg.strokeColor(nvg.rgba(0, 160, 192, 255));
//    vg.strokeWidth(3.0);
//    vg.stroke();
//
//    // Graph sample pos
//    i = 0;
//    while (i < 6) : (i += 1) {
//        bg = vg.radialGradient(sx[i], sy[i] + 2, 3.0, 8.0, nvg.rgba(0, 0, 0, 32), nvg.rgba(0, 0, 0, 0));
//        vg.beginPath();
//        vg.rect(sx[i] - 10, sy[i] - 10 + 2, 20, 20);
//        vg.fillPaint(bg);
//        vg.fill();
//    }
//
//    vg.beginPath();
//    i = 0;
//    while (i < 6) : (i += 1)
//        vg.circle(sx[i], sy[i], 4.0);
//    vg.fillColor(nvg.rgba(0, 160, 192, 255));
//    vg.fill();
//    vg.beginPath();
//    i = 0;
//    while (i < 6) : (i += 1)
//        vg.circle(sx[i], sy[i], 2.0);
//    vg.fillColor(nvg.rgba(220, 220, 220, 255));
//    vg.fill();
//
//    vg.strokeWidth(1.0);
//}
//
