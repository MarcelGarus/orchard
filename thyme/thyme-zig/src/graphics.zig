const std = @import("std");
const rl = @import("raylib");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const object_mod = @import("object.zig");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;

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
        std.debug.print("{}\n", .{heap.get(current).words.len});
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

pub fn init() void {
    rl.setConfigFlags(.{ .window_resizable = true });
    rl.initWindow(600, 400, "Thyme");
    rl.setTargetFPS(60);
}

pub fn deinit() void {
    rl.closeWindow();
}

pub fn should_close() bool {
    return rl.windowShouldClose();
}

pub const Size = struct { width: usize, height: usize };
pub fn get_size() Size {
    return .{ .width = @intCast(rl.getScreenWidth()), .height = @intCast(rl.getScreenHeight()) };
}

pub fn render(ally: Ally, instructions: []const DrawingInstruction) !void {
    _ = ally;

    const width: usize = @intCast(rl.getScreenWidth());
    const height: usize = @intCast(rl.getScreenHeight());

    var image = rl.Image.genColor(@intCast(width), @intCast(height), rl.Color.white);
    defer image.unload();
    try render_all(&image, instructions);

    rl.beginDrawing();
    var rl_texture = try rl.Texture.fromImage(image);
    rl.drawTexturePro(
        rl_texture,
        .{ .x = 0, .y = 0, .width = @floatFromInt(width), .height = @floatFromInt(height) },
        .{ .x = 0, .y = 0, .width = @floatFromInt(width), .height = @floatFromInt(height) },
        .{ .x = 0.0, .y = 0.0 },
        0.0,
        .white,
    );
    defer rl_texture.unload();
    rl.endDrawing();
}

fn render_all(image: *rl.Image, instructions: []const DrawingInstruction) !void {
    for (instructions) |instruction| try render_single(image, instruction);
}
fn render_single(image: *rl.Image, instruction: DrawingInstruction) !void {
    switch (instruction) {
        .draw_rectangle => |draw_rect| {
            image.drawRectangle(
                @intCast(draw_rect.x),
                @intCast(draw_rect.y),
                @intCast(draw_rect.width),
                @intCast(draw_rect.height),
                .{
                    .a = 255,
                    .r = draw_rect.color.r,
                    .g = draw_rect.color.g,
                    .b = draw_rect.color.b,
                },
            );
        },
    }
}
