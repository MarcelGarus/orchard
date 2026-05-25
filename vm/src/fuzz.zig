const std = @import("std");
const Heap = @import("heap.zig");
const Vm = @import("vm.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Smith = std.testing.Smith;

test "tree-walking vs byte-code" {
    try std.testing.fuzz({}, tree_walking_vs_byte_code, .{});
}
test "tree-walking vs byte-code (often)" {
    for (0..100000) |i| {
        var bytes: [4096]u8 = undefined;
        var prng: std.Random.DefaultPrng = .init(i);
        var off: usize = 0;
        while (off < bytes.len) : (off += 8) {
            const v = prng.random().intRangeLessThan(u64, 0, 64);
            std.mem.writeInt(u64, bytes[off..][0..8], v, .little);
        }
        var smith: Smith = .{ .in = &bytes };
        tree_walking_vs_byte_code({}, &smith) catch |err| {
            std.debug.print("seed {d}: {s}\n", .{ i, @errorName(err) });
            return err;
        };
    }
}
fn tree_walking_vs_byte_code(_: void, smith: *Smith) anyerror!void {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();
    const ally = arena.allocator();

    var heap_tw = try Heap.init(ally, 100000);
    var heap_bc = try Heap.init(ally, 100000);

    const fun_tw = generate_function(ally, smith, &heap_tw) catch return;
    const fun_bc = Vm.Fun{ .obj = try heap_tw.copy_to_other_heap(ally, &heap_bc, fun_tw.obj) };

    var vm_tw: Vm.TreeWalkingInterpreter = try .init(&heap_tw, ally);
    vm_tw.log_ub = false;
    vm_tw.max_expressions = 100000;
    var vm_bc: Vm.ByteCodeInterpreter = try .init(&heap_bc, ally);

    var fuel_tw: usize = 10000;
    const result_tw = vm_tw.call(fun_tw, &.{}, &fuel_tw) catch return;
    std.debug.print("Fun: {f}\n", .{fun_tw.obj});

    var fuel_bc: usize = 10000;
    const result_bc = vm_bc.call(fun_bc, &.{}, &fuel_bc) catch return;

    if (!compatible_results(result_tw, result_bc)) {
        std.debug.print("Tree-walking and byte-code interpreter behave differently:\n", .{});
        std.debug.print("Function: {f}\n", .{fun_tw.obj});
        std.debug.print("Tree-walking: {s}", .{@tagName(result_tw)});
        switch (result_tw) {
            .returned => |o| std.debug.print(" {f}", .{o}),
            .crashed => |o| std.debug.print(" {f}", .{o}),
            else => {},
        }
        std.debug.print("\nByte-code: {s}", .{@tagName(result_bc)});
        switch (result_bc) {
            .returned => |o| std.debug.print(" {f}", .{o}),
            .crashed => |o| std.debug.print(" {f}", .{o}),
            else => {},
        }
        std.debug.print("\n", .{});
        return error.VmMismatch;
    }
}

inline fn h(comptime src: std.builtin.SourceLocation) u32 {
    const key = src.file ++ ":" ++ src.fn_name;
    const base = std.hash.Wyhash.hash(0, key);
    return @truncate(base ^ src.line ^ (@as(u64, src.column) << 16));
}

fn generate_object(ally: std.mem.Allocator, smith: *Smith, heap: *Heap, depth: usize) !Obj {
    const MAX_OBJECT_SIZE = 10;
    const is_inner = if (depth > 5) false else smith.boolWeightedWithHash(1, 1, h(@src()));
    const size: usize = smith.valueRangeAtMostWithHash(u32, 0, MAX_OBJECT_SIZE, h(@src()));
    if (is_inner) {
        var children: [MAX_OBJECT_SIZE]Obj = undefined;
        for (0..size) |i|
            children[i] = try generate_object(ally, smith, heap, depth + 1);
        return try heap.new_inner(children[0..size]);
    } else {
        var words: [MAX_OBJECT_SIZE]Word = undefined;
        for (0..size) |i| words[i] = smith.valueWithHash(u64, h(@src()));
        return try heap.new_leaf(words[0..size]);
    }
}

fn generate_function(ally: std.mem.Allocator, smith: *Smith, heap: *Heap) !Vm.Fun {
    const args_obj = try heap.new_inner(&.{});
    var names = std.ArrayList([]const u8).empty;
    const body = try generate_expr(ally, smith, heap, &names, 0);
    return Vm.Fun{ .obj = try heap.new_inner(&.{ args_obj, body }) };
}
fn expr_obj(heap: *Heap, tag: []const u8, children: []const Obj) !Obj {
    const tag_obj = try heap.new_symbol(tag);
    var b = try heap.build_inner();
    try b.emit(tag_obj);
    for (children) |c| try b.emit(c);
    return b.finish();
}
fn generate_expr(
    ally: std.mem.Allocator,
    smith: *Smith,
    heap: *Heap,
    names: *std.ArrayList([]const u8),
    depth: usize,
) anyerror!Obj {
    return switch (smith.indexWithHash(if (depth > 5) 3 else 35, h(@src()))) {
        // expressions without child expressions
        0 => expr_obj(heap, "word", &.{
            try heap.new_leaf(&.{smith.valueWithHash(u64, h(@src()))}),
        }),
        1 => expr_obj(heap, "object", &.{
            try generate_object(ally, smith, heap, 5),
        }),
        2 => if (names.items.len == 0)
            generate_expr(ally, smith, heap, names, depth)
        else
            expr_obj(heap, "name", &.{
                try heap.new_symbol(names.items[smith.indexWithHash(names.items.len, h(@src()))]),
            }),
        // expressions with child expressions
        3 => expr_obj(heap, "add", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        4 => expr_obj(heap, "subtract", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        5 => expr_obj(heap, "multiply", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        6 => expr_obj(heap, "divide", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        7 => expr_obj(heap, "modulo", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        8 => expr_obj(heap, "shift-left", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        9 => expr_obj(heap, "shift-right", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        10 => expr_obj(heap, "and", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        11 => expr_obj(heap, "or", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        12 => expr_obj(heap, "xor", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        13 => expr_obj(heap, "compare", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        14 => expr_obj(heap, "f-add", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        15 => expr_obj(heap, "f-subtract", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        16 => expr_obj(heap, "f-multiply", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        17 => expr_obj(heap, "f-divide", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        18 => expr_obj(heap, "f-compare", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        19 => expr_obj(heap, "int-to-float", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        20 => expr_obj(heap, "float-to-int", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        21 => expr_obj(heap, "f-is-finite", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        22 => expr_obj(heap, "points", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        23 => expr_obj(heap, "size", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        24 => {
            const MAX_CHILDREN = 3;
            const count: usize = smith.valueRangeAtMostWithHash(u32, 0, MAX_CHILDREN, h(@src()));
            var children: [MAX_CHILDREN]Obj = undefined;
            for (0..count) |i|
                children[i] = try generate_expr(ally, smith, heap, names, depth + 1);
            const inner_obj = try heap.new_inner(children[0..count]);
            return expr_obj(heap, "new-leaf", &.{inner_obj});
        },
        25 => {
            const MAX_CHILDREN = 3;
            const count: usize = smith.valueRangeAtMostWithHash(u32, 0, MAX_CHILDREN, h(@src()));
            var children: [MAX_CHILDREN]Obj = undefined;
            for (0..count) |i| children[i] = try generate_expr(ally, smith, heap, names, depth + 1);
            const inner_obj = try generate_expr(ally, smith, heap, names, depth + 1);
            return expr_obj(heap, "new-inner", &.{inner_obj});
        },
        26 => expr_obj(heap, "flatten-to-leaf", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        27 => expr_obj(heap, "flatten-to-inner", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        28 => expr_obj(heap, "gc", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        29 => expr_obj(heap, "sandbox", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        30 => {
            const name = try std.fmt.allocPrint(ally, "var{d}", .{names.items.len});
            const def = try generate_expr(ally, smith, heap, names, depth + 1);
            try names.append(ally, name);
            defer _ = names.pop();
            const body = try generate_expr(ally, smith, heap, names, depth + 1);
            return expr_obj(heap, "let", &.{ try heap.new_symbol(name), def, body });
        },
        31 => expr_obj(heap, "if", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        32 => expr_obj(heap, "also", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        33 => expr_obj(heap, "use-fuel", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        34 => expr_obj(heap, "crash", &.{
            try generate_expr(ally, smith, heap, names, depth + 1),
        }),
        else => unreachable,
    };
}

fn compatible_results(a: Vm.Result, b: Vm.Result) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .returned => |ao| Heap.is_same(ao, b.returned),
        .crashed => |ao| Heap.is_same(ao, b.crashed),
        .out_of_fuel, .out_of_memory => true,
    };
}
