//! Compile src/test.yam first, then: zig test src/app_test.zig -O ReleaseSafe
//! YAM_UI_OBJECTS can point to an isolated compiled notebook instead.
const std = @import("std");
const Heap = @import("heap.zig");
const Vm = @import("vm.zig");
const Graphics = @import("graphics.zig");
const App = @import("app.zig");
const loader = @import("object_loader.zig");
const Size = Graphics.Size;

fn expect_triangles(vm: anytype, app: App, size: Size, mouse: Graphics.Position) !void {
    const checkpoint = vm.heap.checkpoint();
    defer vm.heap.restore(checkpoint);
    const raw = try app.render(vm, size);
    const commands = try Graphics.parse(vm.ally, raw);
    try std.testing.expectEqual(@as(usize, 4), commands.len);
    const corners = [_]Graphics.Position{
        .{ .x = 0, .y = 0 },                    .{ .x = size.width, .y = 0 },
        .{ .x = size.width, .y = size.height }, .{ .x = 0, .y = size.height },
    };
    const colors = [_]Graphics.Color{
        .{ .r = 48, .g = 96, .b = 160 },  .{ .r = 224, .g = 112, .b = 80 },
        .{ .r = 48, .g = 160, .b = 128 }, .{ .r = 224, .g = 176, .b = 64 },
    };
    for (commands, colors, 0..) |command, color, i| {
        const fill = command.fill_path;
        try std.testing.expectEqual(color, fill.color);
        try std.testing.expectEqual(@as(usize, 3), fill.path.len);
        try std.testing.expectEqual(corners[i], fill.path[0].move_to);
        try std.testing.expectEqual(corners[(i + 1) % 4], fill.path[1].line_to);
        try std.testing.expectEqual(mouse, fill.path[2].line_to);
    }
}

fn check_backend(comptime Backend: type, code: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const ally = arena.allocator();
    var heap = try Heap.init(ally, 32_000_000);
    const root = try loader.load(ally, &heap, code);
    var vm = if (Backend == Vm.TreeWalk) try Backend.init(&heap, ally, .{}) else try Backend.init(&heap, ally);
    var fuel: usize = Vm.max_fuel;
    const result = try vm.call(.{ .obj = root }, &.{}, &fuel);
    const notebook = switch (result) {
        .returned => |obj| obj,
        else => return error.NotebookFailed,
    };
    try std.testing.expectEqual(@as(usize, 2), notebook.size());
    var app = App.init(notebook);
    const state_start = heap.checkpoint();
    const initial_mouse = Graphics.Position{ .x = 250, .y = 150 };
    try expect_triangles(&vm, app, .{ .width = 500, .height = 300 }, initial_mouse);
    const events = [_]App.Event{
        .{ .window_resized = .{ .width = 1200, .height = 800 } },
        .{ .char_entered = .{ .codepoint = 0x1f331 } },
        .{ .mouse_clicked = .{ .x = 10, .y = 20 } },
        .{ .key_pressed = .{ .keycode = 32, .control = true, .shift = false, .alt = false } },
    };
    for (events) |event| {
        try app.handle(&vm, event);
        app.value = try vm.garbage_collect(state_start, app.value);
        try expect_triangles(&vm, app, .{ .width = 1200, .height = 800 }, initial_mouse);
    }
    const points = [_]Graphics.Position{
        .{ .x = 123.5, .y = 67.25 }, .{ .x = 0, .y = 0 },
        .{ .x = 1200, .y = 0 },      .{ .x = 1200, .y = 800 },
        .{ .x = 0, .y = 800 },       .{ .x = 721.25, .y = 350.5 },
    };
    for (points) |point| {
        try app.handle(&vm, .{ .mouse_moved = point });
        app.value = try vm.garbage_collect(state_start, app.value);
        try expect_triangles(&vm, app, .{ .width = 1200, .height = 800 }, point);
        try app.handle(&vm, .{ .window_resized = .{ .width = 1600, .height = 900 } });
        app.value = try vm.garbage_collect(state_start, app.value);
        try expect_triangles(&vm, app, .{ .width = 1600, .height = 900 }, point);
    }
}

test "crosshair follows the mouse, spans every edge, and survives resizing and collection on all backends" {
    const path = std.testing.environ.getPosix("YAM_UI_OBJECTS") orelse "src/test.yam.objects";
    const code = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, path, std.testing.allocator, .unlimited);
    defer std.testing.allocator.free(code);
    try check_backend(Vm.Default, code);
    try check_backend(Vm.ByteCode, code);
    try check_backend(Vm.TreeWalk, code);
}

test "events use the notebook numeric tags and positional fields" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var heap = try Heap.init(arena.allocator(), 1000);
    const char = try App.event_object(&heap, .{ .char_entered = .{ .codepoint = 0x1f331 } });
    try std.testing.expectEqual(@as(u64, 2), char.child(0).word(0));
    try std.testing.expectEqual(@as(u64, 0x1f331), char.child(1).child(0).word(0));
    const key = try App.event_object(&heap, .{ .key_pressed = .{
        .keycode = 32,
        .control = true,
        .shift = false,
        .alt = true,
    } });
    try std.testing.expectEqual(@as(u64, 1), key.child(0).word(0));
    const fields = key.child(1);
    try std.testing.expectEqual(@as(u64, 32), fields.child(0).word(0));
    try std.testing.expectEqual(@as(u64, 1), fields.child(1).child(0).word(0));
    try std.testing.expectEqual(@as(u64, 0), fields.child(2).child(0).word(0));
    try std.testing.expectEqual(@as(u64, 1), fields.child(3).child(0).word(0));
    const moved = try App.event_object(&heap, .{ .mouse_moved = .{ .x = 123.5, .y = 67.25 } });
    try std.testing.expectEqual(@as(u64, 4), moved.child(0).word(0));
    try std.testing.expectEqual(@as(f64, 123.5), Graphics.float(moved.child(1).child(0)));
    try std.testing.expectEqual(@as(f64, 67.25), Graphics.float(moved.child(1).child(1)));
}

test "all drawing variants decode directly from Yam objects" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const ally = arena.allocator();
    var heap = try Heap.init(ally, 1000);
    // Raw Float bits for 0.0, 1.0 and 2.0; tags match DrawCommand in test.yam.
    const raw = try loader.load(ally, &heap,
        \\zero 0
        \\one 4607182418800017408
        \\two 4611686018427387904
        \\nothing (0 ())
        \\path (((0 (zero zero)) (1 (two one))))
        \\fill (1 (path (48 96 160)))
        \\transform (2 ((one zero zero one two one) fill))
        \\export (3 (nothing transform fill))
    );
    const commands = try Graphics.parse(ally, raw);
    try std.testing.expectEqual(@as(usize, 2), commands.len);
    const transformed = commands[0].transform;
    try std.testing.expectEqual(@as(f64, 2), transformed.by.e);
    try std.testing.expectEqual(@as(f64, 1), transformed.by.f);
    try std.testing.expectEqual(@as(usize, 1), transformed.children.len);
    try std.testing.expectEqual(@as(u8, 160), transformed.children[0].fill_path.color.b);
    try std.testing.expectEqual(Graphics.Position{ .x = 2, .y = 1 }, commands[1].fill_path.path[1].line_to);
}
