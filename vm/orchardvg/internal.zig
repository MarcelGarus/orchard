//! OrchardVG: modified NanoVG Zig fork for Orchard. See LICENSE.txt.
const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.array_list.Managed;

const nvg = @import("orchardvg.zig");
const Color = nvg.Color;
const Paint = nvg.Paint;
const Image = nvg.Image;
const ImageFlags = nvg.ImageFlags;

// Length proportional to radius of a cubic bezier handle for 90deg arcs.
const kappa90 = 4.0 * (@sqrt(2.0) - 1.0) / 3.0; // 0.5522847493

pub const Context = struct {
    allocator: Allocator,
    params: Params,
    commands: ArrayList(f32),
    commandx: f32 = 0,
    commandy: f32 = 0,
    states: ArrayList(State),
    cache: PathCache,
    tess_tol: f32,
    dist_tol: f32,
    device_px_ratio: f32 = 1,
    clip_depth: usize = 0,
    draw_call_count: u32 = 0,
    fill_tri_count: u32 = 0,

    pub fn init(allocator: Allocator, params: Params) !*Context {
        var ctx = try allocator.create(Context);
        ctx.* = Context{
            .allocator = allocator,
            .params = params,
            .commands = ArrayList(f32).init(allocator),
            .states = ArrayList(State).init(allocator),
            .cache = try PathCache.init(allocator),
            .tess_tol = undefined,
            .dist_tol = undefined,
            .device_px_ratio = undefined,
        };
        errdefer ctx.deinit();

        try ctx.commands.ensureTotalCapacity(256);
        try ctx.states.ensureTotalCapacity(32);

        ctx.save();
        ctx.reset();

        ctx.setDevicePixelRatio(1);

        try ctx.params.renderCreate(ctx.params.user_ptr);

        return ctx;
    }

    pub fn deinit(ctx: *Context) void {
        ctx.commands.deinit();
        ctx.states.deinit();
        ctx.cache.deinit();

        _ = ctx.params.renderDelete(ctx.params.user_ptr);

        ctx.allocator.destroy(ctx);
    }

    fn setDevicePixelRatio(ctx: *Context, ratio: f32) void {
        ctx.tess_tol = 0.25 / ratio;
        ctx.dist_tol = 0.01 / ratio;
        ctx.device_px_ratio = ratio;
    }

    pub fn getState(ctx: *Context) *State {
        return &ctx.states.items[ctx.states.items.len - 1];
    }

    pub fn save(ctx: *Context) void {
        const state = ctx.states.addOne() catch return;
        if (ctx.states.items.len > 1) {
            state.* = ctx.states.items[ctx.states.items.len - 2];
        }
    }

    pub fn restore(ctx: *Context) void {
        _ = ctx.states.pop();
    }

    pub fn reset(ctx: *Context) void {
        var state = ctx.getState();
        state.* = std.mem.zeroes(State);

        setPaintColor(&state.fill, nvg.rgbaf(1, 1, 1, 1));

        state.composite_operation = nvg.CompositeOperationState.initOperation(.source_over);
        state.alpha = 1;
        nvg.transformIdentity(&state.xform);
    }

    pub fn globalAlpha(ctx: *Context, alpha: f32) void {
        ctx.getState().alpha = alpha;
    }

    pub fn transform(ctx: *Context, a: f32, b: f32, _c: f32, d: f32, e: f32, f: f32) void {
        const state = ctx.getState();
        var t: [6]f32 = .{ a, b, _c, d, e, f };
        nvg.transformPremultiply(&state.xform, &t);
    }

    pub fn resetTransform(ctx: *Context) void {
        const state = ctx.getState();
        nvg.transformIdentity(&state.xform);
    }

    pub fn translate(ctx: *Context, x: f32, y: f32) void {
        const state = ctx.getState();
        var t: [6]f32 = undefined;
        nvg.transformTranslate(&t, x, y);
        nvg.transformPremultiply(&state.xform, &t);
    }

    pub fn rotate(ctx: *Context, angle: f32) void {
        const state = ctx.getState();
        var t: [6]f32 = undefined;
        nvg.transformRotate(&t, angle);
        nvg.transformPremultiply(&state.xform, &t);
    }

    pub fn skewX(ctx: *Context, angle: f32) void {
        const state = ctx.getState();
        var t: [6]f32 = undefined;
        nvg.transformSkewX(&t, angle);
        nvg.transformPremultiply(&state.xform, &t);
    }

    pub fn skewY(ctx: *Context, angle: f32) void {
        const state = ctx.getState();
        var t: [6]f32 = undefined;
        nvg.transformSkewY(&t, angle);
        nvg.transformPremultiply(&state.xform, &t);
    }

    pub fn scale(ctx: *Context, x: f32, y: f32) void {
        const state = ctx.getState();
        var t: [6]f32 = undefined;
        nvg.transformScale(&t, x, y);
        nvg.transformPremultiply(&state.xform, &t);
    }

    pub fn currentTransform(ctx: *Context, xform: *[6]f32) void {
        const state = ctx.getState();
        @memcpy(xform, &state.xform);
    }

    pub fn fillColor(ctx: *Context, color: Color) void {
        const state = ctx.getState();
        setPaintColor(&state.fill, color);
    }

    pub fn fillPaint(ctx: *Context, paint: Paint) void {
        const state = ctx.getState();
        state.fill = paint;
        nvg.transformMultiply(&state.fill.xform, &state.xform);
    }

    pub fn createImageRGBA(ctx: *Context, w: u32, h: u32, flags: ImageFlags, data: ?[]const u8) Image {
        return Image{ .handle = ctx.params.renderCreateTexture(ctx.params.user_ptr, .rgba, @intCast(w), @intCast(h), flags, data) catch 0 };
    }

    pub fn createImageAlpha(ctx: *Context, w: u32, h: u32, flags: ImageFlags, data: []const u8) Image {
        return Image{ .handle = ctx.params.renderCreateTexture(ctx.params.user_ptr, .alpha, @intCast(w), @intCast(h), flags, data) catch 0 };
    }

    pub fn updateImage(ctx: *Context, image: Image, data: []const u8) void {
        var w: u32 = undefined;
        var h: u32 = undefined;
        _ = ctx.params.renderGetTextureSize(ctx.params.user_ptr, image.handle, &w, &h);
        _ = ctx.params.renderUpdateTexture(ctx.params.user_ptr, image.handle, 0, 0, w, h, data);
    }

    pub fn beginFrame(ctx: *Context, window_width: f32, window_height: f32, device_pixel_ratio: f32) void {
        ctx.clip_depth = 0;
        ctx.states.clearRetainingCapacity();
        ctx.save();
        ctx.reset();

        ctx.setDevicePixelRatio(device_pixel_ratio);

        ctx.params.renderViewport(ctx.params.user_ptr, window_width, window_height, device_pixel_ratio);

        ctx.draw_call_count = 0;
        ctx.fill_tri_count = 0;
    }

    pub fn cancelFrame(ctx: *Context) void {
        ctx.clip_depth = 0;
        ctx.params.renderCancel(ctx.params.user_ptr);
    }

    pub fn endFrame(ctx: *Context) void {
        std.debug.assert(ctx.clip_depth == 0);
        ctx.params.renderFlush(ctx.params.user_ptr);
    }

    fn appendCommands(ctx: *Context, vals_src: anytype) void {
        var vals: [vals_src.len]f32 = vals_src;
        const state = ctx.getState();

        ctx.commands.ensureUnusedCapacity(vals.len) catch return;

        if (vals_src.len >= 3) {
            ctx.commandx = vals[vals.len - 2];
            ctx.commandy = vals[vals.len - 1];
        }

        // transform commands
        var i: u32 = 0;
        while (i < vals.len) {
            switch (Command.fromValue(vals[i])) {
                .move_to => {
                    transformPoint(&vals[i + 1], &vals[i + 2], state.xform, vals[i + 1], vals[i + 2]);
                    i += 3;
                },
                .line_to => {
                    transformPoint(&vals[i + 1], &vals[i + 2], state.xform, vals[i + 1], vals[i + 2]);
                    i += 3;
                },
                .bezier_to => {
                    transformPoint(&vals[i + 1], &vals[i + 2], state.xform, vals[i + 1], vals[i + 2]);
                    transformPoint(&vals[i + 3], &vals[i + 4], state.xform, vals[i + 3], vals[i + 4]);
                    transformPoint(&vals[i + 5], &vals[i + 6], state.xform, vals[i + 5], vals[i + 6]);
                    i += 7;
                },
                .winding => i += 2,
                .close => i += 1,
            }
        }

        ctx.commands.appendSliceAssumeCapacity(&vals);
        ctx.cache.clear();
    }

    fn tesselateBezier(ctx: *Context, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32, x4: f32, y4: f32, level: u8) void {
        if (level > 10) return;

        const x12 = (x1 + x2) * 0.5;
        const y12 = (y1 + y2) * 0.5;
        const x23 = (x2 + x3) * 0.5;
        const y23 = (y2 + y3) * 0.5;
        const x34 = (x3 + x4) * 0.5;
        const y34 = (y3 + y4) * 0.5;
        const x123 = (x12 + x23) * 0.5;
        const y123 = (y12 + y23) * 0.5;

        const dx = x4 - x1;
        const dy = y4 - y1;

        const d2 = @abs(((x2 - x4) * dy - (y2 - y4) * dx));
        const d3 = @abs(((x3 - x4) * dy - (y3 - y4) * dx));

        if ((d2 + d3) * (d2 + d3) < ctx.tess_tol * (dx * dx + dy * dy)) {
            ctx.cache.addPoint(x4, y4, ctx.dist_tol);
            return;
        }

        const x234 = (x23 + x34) * 0.5;
        const y234 = (y23 + y34) * 0.5;
        const x1234 = (x123 + x234) * 0.5;
        const y1234 = (y123 + y234) * 0.5;

        ctx.tesselateBezier(x1, y1, x12, y12, x123, y123, x1234, y1234, level + 1);
        ctx.tesselateBezier(x1234, y1234, x234, y234, x34, y34, x4, y4, level + 1);
    }

    fn flattenPaths(ctx: *Context) void {
        const cache = &ctx.cache;

        if (cache.paths.items.len > 0)
            return;

        // Flatten
        var i: u32 = 0;
        while (i < ctx.commands.items.len) {
            switch (Command.fromValue(ctx.commands.items[i])) {
                .move_to => {
                    cache.addPath();
                    const p = ctx.commands.items[i + 1 ..];
                    cache.addPoint(p[0], p[1], ctx.dist_tol);
                    i += 3;
                },
                .line_to => {
                    const p = ctx.commands.items[i + 1 ..];
                    cache.addPoint(p[0], p[1], ctx.dist_tol);
                    i += 3;
                },
                .bezier_to => {
                    if (cache.lastPoint()) |last| {
                        const cp1 = ctx.commands.items[i + 1 ..];
                        const cp2 = ctx.commands.items[i + 3 ..];
                        const p = ctx.commands.items[i + 5 ..];
                        ctx.tesselateBezier(last.x, last.y, cp1[0], cp1[1], cp2[0], cp2[1], p[0], p[1], 0);
                    }
                    i += 7;
                },
                .close => i += 1,
                .winding => {
                    cache.pathWinding(@enumFromInt(@as(u2, @intFromFloat(ctx.commands.items[i + 1]))));
                    i += 2;
                },
            }
        }

        cache.bounds[0] = 1e6;
        cache.bounds[1] = 1e6;
        cache.bounds[2] = -1e6;
        cache.bounds[3] = -1e6;

        // Normalize contours and classify convexity from point positions.
        for (cache.paths.items) |*path| {
            var points = cache.points.items[path.first..][0..path.count];
            if (points.len > 1 and ptEquals(points[0].x, points[0].y, points[points.len - 1].x, points[points.len - 1].y, ctx.dist_tol)) {
                path.count -= 1;
                points.len -= 1;
            }
            if (points.len > 2) {
                const area = polyArea(points);
                if ((path.winding == .ccw and area < 0) or (path.winding == .cw and area > 0))
                    polyReverse(points);
            }
            path.convex = points.len >= 3;
            for (points, 0..) |point, point_index| {
                cache.bounds[0] = @min(cache.bounds[0], point.x);
                cache.bounds[1] = @min(cache.bounds[1], point.y);
                cache.bounds[2] = @max(cache.bounds[2], point.x);
                cache.bounds[3] = @max(cache.bounds[3], point.y);
                if (path.convex) {
                    const previous = points[(point_index + points.len - 1) % points.len];
                    const next = points[(point_index + 1) % points.len];
                    if (cross(point.x - previous.x, point.y - previous.y, next.x - point.x, next.y - point.y) <= 0) path.convex = false;
                }
            }
        }
    }

    fn expandFill(ctx: *Context) !void {
        const cache = &ctx.cache;

        // Calculate max vertex usage.
        var cverts: u32 = 0;
        for (cache.paths.items) |path| {
            cverts += path.count;
        }

        var verts = try cache.allocTempVerts(cverts);

        for (cache.paths.items) |*path| {
            if (path.count == 0) continue;
            const pts = cache.points.items[path.first..][0..path.count];

            // Calculate shape vertices.
            var dst = ArrayList(Vertex).fromOwnedSlice(ctx.allocator, verts);
            dst.clearRetainingCapacity();

            for (pts) |p| {
                dst.addOneAssumeCapacity().set(p.x, p.y);
            }

            path.fill = dst.items;
            verts = verts[dst.items.len..verts.len];
        }
    }

    pub fn addPath(ctx: *Context, path: nvg.Path) void {
        var i: usize = 0;
        for (path.verbs) |verb| {
            switch (verb) {
                .move => {
                    ctx.moveTo(path.points[i + 0], path.points[i + 1]);
                    i += 2;
                },
                .line => {
                    ctx.lineTo(path.points[i + 0], path.points[i + 1]);
                    i += 2;
                },
                .quad => {
                    ctx.quadTo(
                        path.points[i + 0],
                        path.points[i + 1],
                        path.points[i + 2],
                        path.points[i + 3],
                    );
                    i += 4;
                },
                .bezier => {
                    ctx.bezierTo(
                        path.points[i + 0],
                        path.points[i + 1],
                        path.points[i + 2],
                        path.points[i + 3],
                        path.points[i + 4],
                        path.points[i + 5],
                    );
                    i += 6;
                },
                .close => ctx.closePath(),
            }
        }
    }

    pub fn beginPath(ctx: *Context) void {
        ctx.commands.clearRetainingCapacity();
        ctx.cache.clear();
    }

    pub fn moveTo(ctx: *Context, x: f32, y: f32) void {
        ctx.appendCommands(.{ Command.move_to.toValue(), x, y });
    }

    pub fn lineTo(ctx: *Context, x: f32, y: f32) void {
        ctx.appendCommands(.{ Command.line_to.toValue(), x, y });
    }

    pub fn bezierTo(ctx: *Context, c1x: f32, c1y: f32, c2x: f32, c2y: f32, x: f32, y: f32) void {
        ctx.appendCommands(.{ Command.bezier_to.toValue(), c1x, c1y, c2x, c2y, x, y });
    }

    pub fn quadTo(ctx: *Context, cx: f32, cy: f32, x: f32, y: f32) void {
        const x0 = ctx.commandx;
        const y0 = ctx.commandy;
        // zig fmt: off
        ctx.appendCommands(.{
            Command.bezier_to.toValue(),
            x0 + 2.0/3.0*(cx - x0), y0 + 2.0/3.0*(cy - y0),
            x + 2.0/3.0*(cx - x), y + 2.0/3.0*(cy - y),
            x, y
        });
        // zig fmt: on
    }

    pub fn arcTo(ctx: *Context, x1: f32, y1: f32, x2: f32, y2: f32, radius: f32) void {
        const x0: f32 = ctx.commandx;
        const y0: f32 = ctx.commandy;

        if (ctx.commands.items.len == 0) {
            return;
        }

        // Handle degenerate cases.
        if (ptEquals(x0, y0, x1, y1, ctx.dist_tol) or
            ptEquals(x1, y1, x2, y2, ctx.dist_tol) or
            distPtSeg(x1, y1, x0, y0, x2, y2) < ctx.dist_tol * ctx.dist_tol or
            radius < ctx.dist_tol)
        {
            ctx.lineTo(x1, y1);
            return;
        }

        // Calculate tangential circle to lines (x0,y0)-(x1,y1) and (x1,y1)-(x2,y2).
        var dx0 = x0 - x1;
        var dy0 = y0 - y1;
        var dx1 = x2 - x1;
        var dy1 = y2 - y1;
        _ = normalize(&dx0, &dy0);
        _ = normalize(&dx1, &dy1);
        const a = std.math.acos(dx0 * dx1 + dy0 * dy1);
        const d = radius / @tan(a / 2.0);
        if (d > 10000.0) {
            ctx.lineTo(x1, y1);
            return;
        }

        if (cross(dx0, dy0, dx1, dy1) > 0.0) {
            ctx.arc(
                x1 + dx0 * d + dy0 * radius,
                y1 + dy0 * d + -dx0 * radius,
                radius,
                std.math.atan2(dx0, -dy0),
                std.math.atan2(-dx1, dy1),
                .cw,
            );
        } else {
            ctx.arc(
                x1 + dx0 * d + -dy0 * radius,
                y1 + dy0 * d + dx0 * radius,
                radius,
                std.math.atan2(-dx0, dy0),
                std.math.atan2(dx1, -dy1),
                .ccw,
            );
        }
    }

    pub fn closePath(ctx: *Context) void {
        ctx.appendCommands(.{Command.close.toValue()});
    }

    pub fn pathWinding(ctx: *Context, dir: nvg.Winding) void {
        ctx.appendCommands(.{ Command.winding.toValue(), @as(f32, @floatFromInt(@intFromEnum(dir))) });
    }

    pub fn arc(ctx: *Context, cx: f32, cy: f32, r: f32, a0: f32, a1: f32, dir: nvg.Winding) void {
        const move: Command = if (ctx.commands.items.len > 0) .line_to else .move_to;

        // Clamp angles
        var da = a1 - a0;
        if (dir == .cw) {
            if (@abs(da) >= std.math.pi * 2.0) {
                da = std.math.pi * 2.0;
            } else {
                while (da < 0.0) da += std.math.pi * 2.0;
            }
        } else {
            if (@abs(da) >= std.math.pi * 2.0) {
                da = -std.math.pi * 2.0;
            } else {
                while (da > 0.0) da -= std.math.pi * 2.0;
            }
        }

        // Split arc into max 90 degree segments.
        const ndivs = std.math.clamp(@round(@abs(da) / (std.math.pi * 0.5)), 1, 5);
        const hda = (da / ndivs) / 2.0;
        var kappa = @abs(4.0 / 3.0 * (1.0 - @cos(hda)) / @sin(hda));

        if (dir == .ccw)
            kappa = -kappa;

        var px: f32 = 0;
        var py: f32 = 0;
        var ptanx: f32 = 0;
        var ptany: f32 = 0;
        var i: f32 = 0;
        while (i <= ndivs) : (i += 1) {
            const a = a0 + da * (i / ndivs);
            const dx = @cos(a);
            const dy = @sin(a);
            const x = cx + dx * r;
            const y = cy + dy * r;
            const tanx = -dy * r * kappa;
            const tany = dx * r * kappa;

            if (i == 0) {
                ctx.appendCommands(.{ move.toValue(), x, y });
            } else {
                ctx.appendCommands(.{ Command.bezier_to.toValue(), px + ptanx, py + ptany, x - tanx, y - tany, x, y });
            }
            px = x;
            py = y;
            ptanx = tanx;
            ptany = tany;
        }
    }

    pub fn rect(ctx: *Context, x: f32, y: f32, w: f32, h: f32) void {
        ctx.appendCommands(.{
            Command.move_to.toValue(), x,     y,
            Command.line_to.toValue(), x,     y + h,
            Command.line_to.toValue(), x + w, y + h,
            Command.line_to.toValue(), x + w, y,
            Command.close.toValue(),
        });
    }

    pub fn roundedRect(ctx: *Context, x: f32, y: f32, w: f32, h: f32, r: f32) void {
        ctx.roundedRectVarying(x, y, w, h, r, r, r, r);
    }

    pub fn roundedRectVarying(ctx: *Context, x: f32, y: f32, w: f32, h: f32, radTopLeft: f32, radTopRight: f32, radBottomRight: f32, radBottomLeft: f32) void {
        if (radTopLeft < 0.1 and radTopRight < 0.1 and radBottomRight < 0.1 and radBottomLeft < 0.1) {
            ctx.rect(x, y, w, h);
        } else {
            const halfw = @abs(w) * 0.5;
            const halfh = @abs(h) * 0.5;
            const rxBL = @min(radBottomLeft, halfw) * sign(w);
            const ryBL = @min(radBottomLeft, halfh) * sign(h);
            const rxBR = @min(radBottomRight, halfw) * sign(w);
            const ryBR = @min(radBottomRight, halfh) * sign(h);
            const rxTR = @min(radTopRight, halfw) * sign(w);
            const ryTR = @min(radTopRight, halfh) * sign(h);
            const rxTL = @min(radTopLeft, halfw) * sign(w);
            const ryTL = @min(radTopLeft, halfh) * sign(h);
            // zig fmt: off
            ctx.appendCommands(.{
                Command.move_to.toValue(), x, y + ryTL,
                Command.line_to.toValue(), x, y + h - ryBL,
                Command.bezier_to.toValue(), x, y + h - ryBL*(1 - kappa90), x + rxBL*(1 - kappa90), y + h, x + rxBL, y + h,
                Command.line_to.toValue(), x + w - rxBR, y + h,
                Command.bezier_to.toValue(), x + w - rxBR*(1 - kappa90), y + h, x + w, y + h - ryBR*(1 - kappa90), x + w, y + h - ryBR,
                Command.line_to.toValue(), x + w, y + ryTR,
                Command.bezier_to.toValue(), x + w, y + ryTR*(1 - kappa90), x + w - rxTR*(1 - kappa90), y, x + w - rxTR, y,
                Command.line_to.toValue(), x + rxTL, y,
                Command.bezier_to.toValue(), x + rxTL*(1 - kappa90), y, x, y + ryTL*(1 - kappa90), x, y + ryTL,
                Command.close.toValue(),
            });
            // zig fmt: on
        }
    }

    pub fn ellipse(ctx: *Context, cx: f32, cy: f32, rx: f32, ry: f32) void {
        // zig fmt: off
        ctx.appendCommands(.{
            Command.move_to.toValue(), cx-rx, cy,
            Command.bezier_to.toValue(), cx-rx, cy+ry*kappa90, cx-rx*kappa90, cy+ry, cx, cy+ry,
            Command.bezier_to.toValue(), cx+rx*kappa90, cy+ry, cx+rx, cy+ry*kappa90, cx+rx, cy,
            Command.bezier_to.toValue(), cx+rx, cy-ry*kappa90, cx+rx*kappa90, cy-ry, cx, cy-ry,
            Command.bezier_to.toValue(), cx-rx*kappa90, cy-ry, cx-rx, cy-ry*kappa90, cx-rx, cy,
            Command.close.toValue(),
        });
        // zig fmt: on
    }

    pub fn pushClip(ctx: *Context) !void {
        ctx.flattenPaths();
        if (ctx.cache.paths.items.len > 0) try ctx.expandFill();
        const bounds = if (ctx.cache.paths.items.len > 0) ctx.cache.bounds else [4]f32{ 0, 0, 0, 0 };
        try ctx.params.renderPushClip(ctx.params.user_ptr, bounds, ctx.cache.paths.items);
        ctx.clip_depth += 1;
    }

    pub fn popClip(ctx: *Context) !void {
        if (ctx.clip_depth == 0) return error.UnbalancedClip;
        try ctx.params.renderPopClip(ctx.params.user_ptr);
        ctx.clip_depth -= 1;
    }

    pub fn imageSize(ctx: *Context, image: i32, w: *u32, h: *u32) void {
        _ = ctx.params.renderGetTextureSize(ctx.params.user_ptr, image, w, h);
    }

    pub fn deleteImage(ctx: *Context, image: i32) void {
        ctx.params.renderDeleteTexture(ctx.params.user_ptr, image);
    }

    pub fn linearGradient(ctx: *Context, sx: f32, sy: f32, ex: f32, ey: f32, icol: Color, ocol: Color) Paint {
        _ = ctx;
        var p = std.mem.zeroes(Paint);
        const large = 1e5;

        // Calculate transform aligned to the line
        var dx = ex - sx;
        var dy = ey - sy;
        const d = @sqrt(dx * dx + dy * dy);
        if (d > 0.0001) {
            dx /= d;
            dy /= d;
        } else {
            dx = 0;
            dy = 1;
        }

        p.xform[0] = dy;
        p.xform[1] = -dx;
        p.xform[2] = dx;
        p.xform[3] = dy;
        p.xform[4] = sx - dx * large;
        p.xform[5] = sy - dy * large;

        p.extent[0] = large;
        p.extent[1] = large + d * 0.5;

        p.radius = 0;

        p.feather = @max(1, d);

        p.inner_color = icol;
        p.outer_color = ocol;

        return p;
    }

    pub fn radialGradient(ctx: *Context, cx: f32, cy: f32, inr: f32, outr: f32, icol: Color, ocol: Color) Paint {
        _ = ctx;
        const r = (inr + outr) * 0.5;
        const f = (outr - inr);
        var p = std.mem.zeroes(Paint);

        nvg.transformIdentity(&p.xform);
        p.xform[4] = cx;
        p.xform[5] = cy;

        p.extent[0] = r;
        p.extent[1] = r;

        p.radius = r;

        p.feather = @max(1, f);

        p.inner_color = icol;
        p.outer_color = ocol;

        return p;
    }

    pub fn boxGradient(ctx: *Context, x: f32, y: f32, w: f32, h: f32, r: f32, f: f32, icol: Color, ocol: Color) Paint {
        _ = ctx;
        var p = std.mem.zeroes(Paint);

        nvg.transformIdentity(&p.xform);
        p.xform[4] = x + w * 0.5;
        p.xform[5] = y + h * 0.5;

        p.extent[0] = w * 0.5;
        p.extent[1] = h * 0.5;

        p.radius = r;

        p.feather = @max(1, f);

        p.inner_color = icol;
        p.outer_color = ocol;

        return p;
    }

    pub fn imagePattern(ctx: *Context, ox: f32, oy: f32, ex: f32, ey: f32, angle: f32, image: Image, alpha: f32) nvg.Paint {
        _ = ctx;
        var p: Paint = std.mem.zeroes(Paint);

        nvg.transformRotate(&p.xform, angle);
        p.xform[4] = ox;
        p.xform[5] = oy;

        p.extent[0] = ex;
        p.extent[1] = ey;

        p.image = image;

        p.inner_color = nvg.rgbaf(1, 1, 1, alpha);
        p.outer_color = nvg.rgbaf(1, 1, 1, alpha);

        return p;
    }

    pub fn imageBlur(ctx: *Context, ex: f32, ey: f32, image: Image, blur_x: f32, blur_y: f32) Paint {
        _ = ctx;
        var p: Paint = std.mem.zeroes(Paint);

        nvg.transformIdentity(&p.xform);

        p.extent[0] = ex;
        p.extent[1] = ey;

        p.blur[0] = blur_x / ex;
        p.blur[1] = blur_y / ey;

        p.image = image;

        p.inner_color = nvg.rgbaf(1, 1, 1, 1);
        p.outer_color = nvg.rgbaf(1, 1, 1, 1);

        return p;
    }

    pub fn indexedImagePattern(ctx: *Context, ox: f32, oy: f32, ex: f32, ey: f32, angle: f32, image: Image, colormap: Image, alpha: f32) Paint {
        _ = ctx;
        var p: Paint = std.mem.zeroes(Paint);

        nvg.transformRotate(&p.xform, angle);
        p.xform[4] = ox;
        p.xform[5] = oy;

        p.extent[0] = ex;
        p.extent[1] = ey;

        p.image = image;
        p.colormap = colormap;

        p.inner_color = nvg.rgbaf(1, 1, 1, alpha);
        p.outer_color = nvg.rgbaf(1, 1, 1, alpha);

        return p;
    }

    pub fn fill(ctx: *Context) void {
        const state = ctx.getState();
        var fill_paint = state.fill;

        // Apply global alpha
        fill_paint.inner_color.a *= state.alpha;
        fill_paint.outer_color.a *= state.alpha;

        ctx.flattenPaths();
        if (ctx.cache.paths.items.len == 0) return;

        ctx.expandFill() catch return;

        ctx.params.renderFill(ctx.params.user_ptr, &fill_paint, state.composite_operation, ctx.cache.bounds, ctx.cache.paths.items);

        // Count triangles
        for (ctx.cache.paths.items) |path| {
            if (path.fill.len >= 2) ctx.fill_tri_count += @intCast(path.fill.len - 2);
            ctx.draw_call_count += 2;
        }
    }
};

const Command = enum(i32) {
    move_to = 0,
    line_to = 1,
    bezier_to = 2,
    close = 3,
    winding = 4,

    fn fromValue(val: f32) Command {
        return @enumFromInt(@as(i32, @intFromFloat(val)));
    }

    fn toValue(command: Command) f32 {
        return @floatFromInt(@intFromEnum(command));
    }
};

pub const TextureType = enum(u8) {
    none = 0x0,
    alpha = 0x01,
    rgba = 0x02,
};

pub const Vertex = struct {
    x: f32,
    y: f32,

    fn set(self: *Vertex, x: f32, y: f32) void {
        self.x = x;
        self.y = y;
    }
};

pub const Path = struct {
    first: u32,
    count: u32,
    fill: []Vertex,
    winding: nvg.Winding,
    convex: bool,
};

pub const Params = struct {
    user_ptr: *anyopaque,
    renderCreate: *const fn (uptr: *anyopaque) anyerror!void,
    renderCreateTexture: *const fn (uptr: *anyopaque, tex_type: TextureType, w: u32, h: u32, image_flags: ImageFlags, data: ?[]const u8) anyerror!i32,
    renderDeleteTexture: *const fn (uptr: *anyopaque, image: i32) void,
    renderUpdateTexture: *const fn (uptr: *anyopaque, image: i32, x: u32, y: u32, w: u32, h: u32, data: ?[]const u8) i32,
    renderGetTextureSize: *const fn (uptr: *anyopaque, image: i32, w: *u32, h: *u32) i32,
    renderViewport: *const fn (uptr: *anyopaque, width: f32, height: f32, device_pixel_ratio: f32) void,
    renderCancel: *const fn (uptr: *anyopaque) void,
    renderPushClip: *const fn (uptr: *anyopaque, bounds: [4]f32, paths: []const Path) anyerror!void,
    renderPopClip: *const fn (uptr: *anyopaque) anyerror!void,
    renderFlush: *const fn (uptr: *anyopaque) void,
    renderFill: *const fn (uptr: *anyopaque, paint: *Paint, composite_operation: nvg.CompositeOperationState, bounds: [4]f32, paths: []const Path) void,
    renderDelete: *const fn (uptr: *anyopaque) void,
};

const State = struct {
    composite_operation: nvg.CompositeOperationState,
    fill: Paint,
    alpha: f32,
    xform: [6]f32,
};

const Point = struct { x: f32, y: f32 };

const PathCache = struct {
    allocator: Allocator,
    points: ArrayList(Point),
    paths: ArrayList(Path),
    verts: ArrayList(Vertex),
    bounds: [4]f32 = [_]f32{0} ** 4,

    fn init(allocator: Allocator) !PathCache {
        var cache = PathCache{
            .allocator = allocator,
            .points = try ArrayList(Point).initCapacity(allocator, 128),
            .paths = try ArrayList(Path).initCapacity(allocator, 16),
            .verts = try ArrayList(Vertex).initCapacity(allocator, 256),
        };
        errdefer cache.deinit();
        try cache.points.ensureTotalCapacity(128);
        try cache.paths.ensureTotalCapacity(16);
        try cache.verts.ensureTotalCapacity(256);
        return cache;
    }

    fn deinit(cache: *PathCache) void {
        cache.points.deinit();
        cache.paths.deinit();
        cache.verts.deinit();
    }

    pub fn clear(cache: *PathCache) void {
        cache.points.clearRetainingCapacity();
        cache.paths.clearRetainingCapacity();
    }

    fn allocTempVerts(cache: *PathCache, nverts: u32) ![]Vertex {
        if (nverts > cache.verts.items.len) {
            const cverts = (nverts + 0xff) & 0xffffff00; // Round up to prevent allocations when things change just slightly.
            try cache.verts.ensureTotalCapacity(cverts);
            cache.verts.items.len = nverts;
        }

        return cache.verts.items;
    }

    fn lastPath(cache: *PathCache) ?*Path {
        if (cache.paths.items.len > 0)
            return &cache.paths.items[cache.paths.items.len - 1];
        return null;
    }

    fn addPath(cache: *PathCache) void {
        const path = cache.paths.addOne() catch return;
        path.* = std.mem.zeroes(Path);
        path.first = @truncate(cache.points.items.len);
        path.winding = .ccw;
    }

    fn lastPoint(cache: *PathCache) ?*Point {
        if (cache.points.items.len > 0)
            return &cache.points.items[cache.points.items.len - 1];
        return null;
    }

    fn addPoint(cache: *PathCache, x: f32, y: f32, dist_tol: f32) void {
        const path = cache.lastPath() orelse return;
        if (path.count > 0) {
            if (cache.lastPoint()) |point| {
                if (ptEquals(point.x, point.y, x, y, dist_tol)) return;
            }
        }
        const point = cache.points.addOne() catch return;
        point.* = .{ .x = x, .y = y };
        path.count += 1;
    }

    fn pathWinding(cache: *PathCache, winding: nvg.Winding) void {
        if (cache.lastPath()) |path| {
            path.winding = winding;
        }
    }
};

fn sign(a: f32) f32 {
    return if (a >= 0) 1 else -1;
}

fn cross(dx0: f32, dy0: f32, dx1: f32, dy1: f32) f32 {
    return dx1 * dy0 - dx0 * dy1;
}

fn normalize(x: *f32, y: *f32) f32 {
    const d = std.math.sqrt((x.*) * (x.*) + (y.*) * (y.*));
    if (d > 1e-6) {
        const id = 1.0 / d;
        x.* *= id;
        y.* *= id;
    }
    return d;
}

fn transformPoint(dx: *f32, dy: *f32, t: [6]f32, sx: f32, sy: f32) void {
    dx.* = sx * t[0] + sy * t[2] + t[4];
    dy.* = sx * t[1] + sy * t[3] + t[5];
}

pub fn setPaintColor(paint: *Paint, color: Color) void {
    paint.* = std.mem.zeroes(Paint);
    nvg.transformIdentity(&paint.xform);
    paint.radius = 0;
    paint.feather = 1;
    paint.inner_color = color;
    paint.outer_color = color;
}

fn ptEquals(x1: f32, y1: f32, x2: f32, y2: f32, tol: f32) bool {
    const dx = x2 - x1;
    const dy = y2 - y1;
    return dx * dx + dy * dy < tol * tol;
}

fn distPtSeg(x: f32, y: f32, px: f32, py: f32, qx: f32, qy: f32) f32 {
    const pqx = qx - px;
    const pqy = qy - py;
    var dx = x - px;
    var dy = y - py;
    const d = pqx * pqx + pqy * pqy;
    var t = pqx * dx + pqy * dy;
    if (d > 0) t /= d;
    t = std.math.clamp(t, 0, 1);
    dx = px + t * pqx - x;
    dy = py + t * pqy - y;
    return dx * dx + dy * dy;
}

fn triarea2(ax: f32, ay: f32, bx: f32, by: f32, cx: f32, cy: f32) f32 {
    const abx = bx - ax;
    const aby = by - ay;
    const acx = cx - ax;
    const acy = cy - ay;
    return acx * aby - abx * acy;
}

fn polyArea(pts: []Point) f32 {
    var area: f32 = 0;
    var i: u32 = 2;
    while (i < pts.len) : (i += 1) {
        const p0 = pts[0];
        const p1 = pts[i - 1];
        const p2 = pts[i];
        area += triarea2(p0.x, p0.y, p1.x, p1.y, p2.x, p2.y);
    }
    return area * 0.5;
}

fn polyReverse(pts: []Point) void {
    std.mem.reverse(Point, pts);
}
