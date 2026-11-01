const std = @import("std");
const Allocator = std.mem.Allocator;
pub const Point = struct { x: f64, y: f64 };
pub const Polygon = []const Point;
pub const Region = []const Polygon;
pub const Error = error{ OutOfMemory, InvalidClipPath };
const epsilon = 1e-9;

fn cross(a: Point, b: Point, c: Point) f64 {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

fn signed_area(points: Polygon) f64 {
    if (points.len < 3) return 0;
    var sum: f64 = 0;
    var previous = points[points.len - 1];
    for (points) |point| {
        sum += previous.x * point.y - point.x * previous.y;
        previous = point;
    }
    return sum / 2;
}

fn equal(a: Point, b: Point) bool {
    return @abs(a.x - b.x) <= epsilon and @abs(a.y - b.y) <= epsilon;
}

fn triangle_contains(a: Point, b: Point, c: Point, p: Point) bool {
    return cross(a, b, p) >= -epsilon and cross(b, c, p) >= -epsilon and cross(c, a, p) >= -epsilon;
}

fn triangulate(ally: Allocator, input: Polygon, triangles: *std.ArrayList(Polygon)) Error!void {
    var points: std.ArrayList(Point) = .empty;
    for (input) |point| {
        if (points.items.len == 0 or !equal(points.items[points.items.len - 1], point))
            try points.append(ally, point);
    }
    if (points.items.len > 1 and equal(points.items[0], points.items[points.items.len - 1]))
        _ = points.pop();
    if (points.items.len < 3 or @abs(signed_area(points.items)) <= epsilon) return;
    if (signed_area(points.items) < 0) std.mem.reverse(Point, points.items);
    while (points.items.len > 3) {
        const count = points.items.len;
        var removed = false;
        for (0..count) |i| {
            const before = points.items[(i + count - 1) % count];
            const point = points.items[i];
            const after = points.items[(i + 1) % count];
            const turn = cross(before, point, after);
            if (@abs(turn) <= epsilon) {
                _ = points.orderedRemove(i);
                removed = true;
                break;
            }
            if (turn < 0) continue;
            var occupied = false;
            for (points.items, 0..) |other, j| {
                if (j == i or j == (i + count - 1) % count or j == (i + 1) % count) continue;
                if (triangle_contains(before, point, after, other)) {
                    occupied = true;
                    break;
                }
            }
            if (occupied) continue;
            try triangles.append(ally, try ally.dupe(Point, &.{ before, point, after }));
            _ = points.orderedRemove(i);
            removed = true;
            break;
        }
        if (!removed) return error.InvalidClipPath;
    }
    if (points.items.len == 3 and @abs(signed_area(points.items)) > epsilon)
        try triangles.append(ally, try ally.dupe(Point, points.items));
}

/// Converts move_to/line_to contours to convex pieces, applying the current
/// NanoVG transform. Each contour is an implicitly closed solid, like fill_path.
/// Allocations belong to the caller's frame arena.
pub fn from_path(ally: Allocator, path: anytype, transform: [6]f32) Error!Region {
    var region: std.ArrayList(Polygon) = .empty;
    var contour: std.ArrayList(Point) = .empty;
    for (path) |segment| {
        const point = switch (segment) {
            .move_to => |p| blk: {
                try triangulate(ally, contour.items, &region);
                contour.clearRetainingCapacity();
                break :blk p;
            },
            .line_to => |p| p,
        };
        try contour.append(ally, .{
            .x = @as(f64, transform[0]) * point.x + @as(f64, transform[2]) * point.y + transform[4],
            .y = @as(f64, transform[1]) * point.x + @as(f64, transform[3]) * point.y + transform[5],
        });
    }
    try triangulate(ally, contour.items, &region);
    return region.items;
}

fn intersection_point(a: Point, b: Point, start_distance: f64, end_distance: f64) Point {
    const t = start_distance / (start_distance - end_distance);
    return .{ .x = a.x + t * (b.x - a.x), .y = a.y + t * (b.y - a.y) };
}

fn intersect_convex(ally: Allocator, a: Polygon, b: Polygon) Error!Polygon {
    var result = a;
    var previous_edge = b[b.len - 1];
    for (b) |edge| {
        if (result.len == 0) break;
        var output: std.ArrayList(Point) = .empty;
        var previous = result[result.len - 1];
        var previous_distance = cross(previous_edge, edge, previous);
        for (result) |point| {
            const distance = cross(previous_edge, edge, point);
            const inside = distance >= 0;
            const previous_inside = previous_distance >= 0;
            if (inside != previous_inside)
                try output.append(ally, intersection_point(previous, point, previous_distance, distance));
            if (inside) try output.append(ally, point);
            previous = point;
            previous_distance = distance;
        }
        result = output.items;
        previous_edge = edge;
    }
    return result;
}

pub fn intersect(ally: Allocator, a: Region, b: Region) Error!Region {
    var result: std.ArrayList(Polygon) = .empty;
    for (a) |left| {
        for (b) |right| {
            const polygon = try intersect_convex(ally, left, right);
            if (@abs(signed_area(polygon)) > epsilon) try result.append(ally, polygon);
        }
    }
    return result.items;
}

fn area(region: Region) f64 {
    var result: f64 = 0;
    for (region) |polygon| result += @abs(signed_area(polygon));
    return result;
}
