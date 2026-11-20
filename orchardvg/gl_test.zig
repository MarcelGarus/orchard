const std = @import("std");
const ovg = @import("orchardvg");
const gl = ovg.gl.gl;
const egl = @cImport({
    @cInclude("EGL/egl.h");
    @cInclude("EGL/eglext.h");
});

const Surface = struct {
    display: egl.EGLDisplay,
    surface: egl.EGLSurface,
    context: egl.EGLContext,

    fn init() !Surface {
        const display = egl.eglGetPlatformDisplay(egl.EGL_PLATFORM_SURFACELESS_MESA, null, null);
        if (display == null) return error.NoEGLDisplay;
        if (egl.eglInitialize(display, null, null) == 0) return error.EGLInitializeFailed;
        errdefer _ = egl.eglTerminate(display);
        if (egl.eglBindAPI(egl.EGL_OPENGL_API) == 0) return error.EGLBindAPIFailed;
        const attributes = [_]egl.EGLint{
            egl.EGL_SURFACE_TYPE,    egl.EGL_PBUFFER_BIT,
            egl.EGL_RENDERABLE_TYPE, egl.EGL_OPENGL_BIT,
            egl.EGL_RED_SIZE,        8,
            egl.EGL_GREEN_SIZE,      8,
            egl.EGL_BLUE_SIZE,       8,
            egl.EGL_ALPHA_SIZE,      8,
            egl.EGL_STENCIL_SIZE,    8,
            egl.EGL_NONE,
        };
        var config: egl.EGLConfig = undefined;
        var count: egl.EGLint = 0;
        if (egl.eglChooseConfig(display, &attributes, &config, 1, &count) == 0 or count == 0)
            return error.NoEGLConfig;
        const pbuffer = [_]egl.EGLint{ egl.EGL_WIDTH, 128, egl.EGL_HEIGHT, 128, egl.EGL_NONE };
        const surface = egl.eglCreatePbufferSurface(display, config, &pbuffer);
        if (surface == null) return error.NoEGLSurface;
        errdefer _ = egl.eglDestroySurface(display, surface);
        const context_attributes = [_]egl.EGLint{egl.EGL_NONE};
        const context = egl.eglCreateContext(display, config, null, &context_attributes);
        if (context == null) return error.NoEGLContext;
        errdefer _ = egl.eglDestroyContext(display, context);
        if (egl.eglMakeCurrent(display, surface, surface, context) == 0) return error.EGLMakeCurrentFailed;
        if (gl.gladLoadGL() == 0) return error.GLADInitFailed;
        return .{ .display = display, .surface = surface, .context = context };
    }

    fn deinit(self: Surface) void {
        _ = egl.eglMakeCurrent(self.display, null, null, null);
        _ = egl.eglDestroyContext(self.display, self.context);
        _ = egl.eglDestroySurface(self.display, self.surface);
        _ = egl.eglTerminate(self.display);
    }
};

fn begin(vg: ovg, size: i32) void {
    gl.glViewport(0, 0, size, size);
    gl.glColorMask(gl.GL_TRUE, gl.GL_TRUE, gl.GL_TRUE, gl.GL_TRUE);
    gl.glStencilMask(0xFF);
    gl.glClearColor(0, 0, 0, 1);
    gl.glClear(gl.GL_COLOR_BUFFER_BIT | gl.GL_STENCIL_BUFFER_BIT);
    vg.beginFrame(@floatFromInt(size), @floatFromInt(size), 1);
}

fn rect(vg: ovg, x: f32, y: f32, width: f32, height: f32, color: ovg.Color) void {
    vg.beginPath();
    vg.rect(x, y, width, height);
    vg.fillColor(color);
    vg.fill();
}

fn pixel(x: i32, y: i32, size: i32, expected: [4]u8) !void {
    var actual: [4]u8 = undefined;
    gl.glReadPixels(x, size - 1 - y, 1, 1, gl.GL_RGBA, gl.GL_UNSIGNED_BYTE, &actual);
    try std.testing.expectEqualSlices(u8, &expected, &actual);
    try std.testing.expectEqual(@as(gl.GLenum, gl.GL_NO_ERROR), gl.glGetError());
}

const red = ovg.rgb(255, 0, 0);
const green = ovg.rgb(0, 255, 0);
const blue = ovg.rgb(0, 0, 255);
const white = ovg.rgb(255, 255, 255);

test "nested path masks intersect and restore their cached parents" {
    const surface = try Surface.init();
    defer surface.deinit();
    const vg = try ovg.gl.init(std.testing.allocator, .{ .debug = true });
    defer vg.deinit();
    begin(vg, 128);
    vg.beginPath();
    vg.rect(0, 0, 64, 128);
    try vg.pushClip();
    rect(vg, 0, 0, 128, 128, red);
    vg.beginPath();
    vg.circle(64, 64, 40);
    try vg.pushClip();
    rect(vg, 0, 0, 128, 128, blue);
    try vg.popClip();
    rect(vg, 8, 8, 12, 12, green);
    try vg.popClip();
    rect(vg, 100, 100, 12, 12, white);
    vg.endFrame();
    try pixel(40, 64, 128, .{ 0, 0, 255, 255 });
    try pixel(80, 64, 128, .{ 0, 0, 0, 255 });
    try pixel(12, 12, 128, .{ 0, 255, 0, 255 });
    try pixel(12, 64, 128, .{ 255, 0, 0, 255 });
    try pixel(104, 104, 128, .{ 255, 255, 255, 255 });
    const stats = ovg.gl.clipStats(vg);
    try std.testing.expectEqual(@as(usize, 2), stats.rasterizations);
    try std.testing.expectEqual(@as(usize, 2), stats.max_depth);
}

test "one clip rasterization serves a thousand child fills" {
    const surface = try Surface.init();
    defer surface.deinit();
    const vg = try ovg.gl.init(std.testing.allocator, .{ .debug = true });
    defer vg.deinit();
    begin(vg, 128);
    vg.beginPath();
    vg.circle(64, 64, 48);
    try vg.pushClip();
    for (0..1000) |i| rect(vg, @floatFromInt(48 + i % 32), @floatFromInt(48 + (i / 32) % 32), 1, 1, white);
    try vg.popClip();
    vg.endFrame();
    try pixel(64, 64, 128, .{ 255, 255, 255, 255 });
    try pixel(0, 0, 128, .{ 0, 0, 0, 255 });
    const stats = ovg.gl.clipStats(vg);
    try std.testing.expectEqual(@as(usize, 1), stats.rasterizations);
    try std.testing.expectEqual(@as(usize, 1000), stats.draws);
}

test "transformed Bezier masks, empty clips, cancellation, and resizing" {
    const surface = try Surface.init();
    defer surface.deinit();
    const vg = try ovg.gl.init(std.testing.allocator, .{ .debug = true });
    defer vg.deinit();
    begin(vg, 128);
    vg.translate(8, 8);
    vg.beginPath();
    vg.moveTo(8, 8);
    vg.bezierTo(8, 88, 88, 88, 88, 8);
    vg.closePath();
    try vg.pushClip();
    vg.resetTransform();
    rect(vg, 0, 0, 128, 128, blue);
    try vg.popClip();
    vg.endFrame();
    try pixel(56, 40, 128, .{ 0, 0, 255, 255 });
    try pixel(4, 40, 128, .{ 0, 0, 0, 255 });
    try pixel(56, 100, 128, .{ 0, 0, 0, 255 });
    begin(vg, 64);
    vg.beginPath();
    try vg.pushClip();
    rect(vg, 0, 0, 64, 64, red);
    try vg.popClip();
    vg.endFrame();
    try pixel(32, 32, 64, .{ 0, 0, 0, 255 });
    begin(vg, 64);
    vg.beginPath();
    vg.circle(32, 32, 20);
    try vg.pushClip();
    vg.cancelFrame();
    begin(vg, 64);
    rect(vg, 0, 0, 64, 64, green);
    vg.endFrame();
    try pixel(2, 2, 64, .{ 0, 255, 0, 255 });
    try std.testing.expectError(error.UnbalancedClip, vg.popClip());
}

test "clip nesting exceeds the stencil bit count and siblings reuse mask storage" {
    const surface = try Surface.init();
    defer surface.deinit();
    const vg = try ovg.gl.init(std.testing.allocator, .{ .debug = true });
    defer vg.deinit();
    begin(vg, 128);
    for (0..16) |_| {
        vg.beginPath();
        vg.circle(64, 64, 48);
        try vg.pushClip();
    }
    rect(vg, 0, 0, 128, 128, red);
    for (0..16) |_| try vg.popClip();
    vg.beginPath();
    vg.rect(0, 0, 8, 8);
    try vg.pushClip();
    rect(vg, 0, 0, 128, 128, green);
    try vg.popClip();
    vg.endFrame();
    try pixel(64, 64, 128, .{ 255, 0, 0, 255 });
    try pixel(4, 4, 128, .{ 0, 255, 0, 255 });
    try pixel(120, 120, 128, .{ 0, 0, 0, 255 });
    try std.testing.expectEqual(@as(usize, 17), ovg.gl.clipStats(vg).rasterizations);
    try std.testing.expectEqual(@as(usize, 16), ovg.gl.clipStats(vg).max_depth);
}

test "concave masks and holes survive child fills" {
    const surface = try Surface.init();
    defer surface.deinit();
    const vg = try ovg.gl.init(std.testing.allocator, .{ .debug = true });
    defer vg.deinit();
    begin(vg, 128);
    vg.beginPath();
    vg.rect(8, 8, 112, 112);
    vg.circle(64, 64, 24);
    vg.pathWinding(.cw);
    try vg.pushClip();
    rect(vg, 0, 0, 128, 128, red);
    rect(vg, 0, 62, 128, 4, white);
    try vg.popClip();
    vg.beginPath();
    vg.moveTo(0, 0);
    vg.lineTo(48, 0);
    vg.lineTo(48, 16);
    vg.lineTo(16, 16);
    vg.lineTo(16, 48);
    vg.lineTo(0, 48);
    vg.closePath();
    try vg.pushClip();
    rect(vg, 0, 0, 128, 128, green);
    try vg.popClip();
    vg.endFrame();
    try pixel(64, 64, 128, .{ 0, 0, 0, 255 });
    try pixel(20, 64, 128, .{ 255, 255, 255, 255 });
    try pixel(4, 64, 128, .{ 0, 0, 0, 255 });
    try pixel(8, 8, 128, .{ 0, 255, 0, 255 });
    try pixel(40, 8, 128, .{ 0, 255, 0, 255 });
    try pixel(8, 40, 128, .{ 0, 255, 0, 255 });
    try pixel(40, 40, 128, .{ 255, 0, 0, 255 });
    try std.testing.expectEqual(@as(usize, 2), ovg.gl.clipStats(vg).rasterizations);
}

test "public API exposes fill paths and clipping without stroke or scissor state" {
    inline for (.{ "stroke", "strokeWidth", "strokeColor", "strokePaint", "lineCap", "lineJoin", "miterLimit", "LineCap", "LineJoin", "scissor", "intersectScissor", "resetScissor" }) |name| {
        try std.testing.expect(!@hasDecl(ovg, name));
    }
    try std.testing.expect(!@hasDecl(ovg.internal, "Scissor"));
    try std.testing.expect(!@hasDecl(ovg.internal.Context, "expandStroke"));
    try std.testing.expect(!@hasField(ovg.gl.Options, "stencil_strokes"));
}

test "compact fill uniforms preserve gradients and image paints under path clips" {
    const surface = try Surface.init();
    defer surface.deinit();
    const vg = try ovg.gl.init(std.testing.allocator, .{ .debug = true });
    defer vg.deinit();
    const texels = [_]u8{ 255, 0, 0, 255, 0, 255, 0, 255 };
    const image = vg.createImageRGBA(2, 1, .{ .nearest = true }, &texels);
    try std.testing.expect(image.handle != 0);
    defer vg.deleteImage(image);
    begin(vg, 128);
    vg.beginPath();
    vg.rect(0, 0, 128, 128);
    try vg.pushClip();
    vg.beginPath();
    vg.rect(0, 0, 128, 64);
    vg.fillPaint(vg.linearGradient(0, 0, 128, 0, red, blue));
    vg.fill();
    vg.beginPath();
    vg.rect(0, 64, 128, 64);
    vg.fillPaint(vg.imagePattern(0, 64, 128, 64, 0, image, 1));
    vg.fill();
    try vg.popClip();
    vg.endFrame();
    var left: [4]u8 = undefined;
    var right: [4]u8 = undefined;
    gl.glReadPixels(8, 128 - 1 - 32, 1, 1, gl.GL_RGBA, gl.GL_UNSIGNED_BYTE, &left);
    gl.glReadPixels(120, 128 - 1 - 32, 1, 1, gl.GL_RGBA, gl.GL_UNSIGNED_BYTE, &right);
    try std.testing.expect(left[0] > 220 and left[1] == 0 and left[2] < 35);
    try std.testing.expect(right[2] > 220 and right[1] == 0 and right[0] < 35);
    try pixel(16, 96, 128, .{ 255, 0, 0, 255 });
    try pixel(112, 96, 128, .{ 0, 255, 0, 255 });
    try std.testing.expectEqual(@as(usize, 1), ovg.gl.clipStats(vg).rasterizations);
}
