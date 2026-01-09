const std = @import("std");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;

pub fn empty_obj(heap: *Heap) !Address {
    var b = try heap.object_builder();
    return b.finish();
}

pub fn new_int(heap: *Heap, value: i64) !Address {
    const int_symbol = try new_symbol(heap, "int");
    const type_ = obj: {
        var b = try heap.object_builder();
        try b.emit_pointer(int_symbol);
        break :obj b.finish();
    };
    const literal_obj = obj: {
        var b = try heap.object_builder();
        try b.emit_literal(@bitCast(value));
        break :obj b.finish();
    };
    const int_obj = obj: {
        var b = try heap.object_builder();
        try b.emit_pointer(type_);
        try b.emit_pointer(literal_obj);
        break :obj b.finish();
    };
    return int_obj;
}
pub fn get_int(heap: Heap, int: Address) i64 {
    const literal_obj = heap.load(int, 1);
    return @bitCast(heap.load(literal_obj, 0));
}

pub fn new_symbol(heap: *Heap, val: []const u8) !Address {
    var builder = try heap.object_builder();
    const num_words = (val.len + 7) / 8;
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(val.len - i * 8, 8)) |j|
            w |= @as(Word, val[i * 8 + j]) << @intCast(j * 8);
        try builder.emit_literal(w);
    }
    return builder.finish();
}
pub fn get_symbol(heap: Heap, symbol: Address) []const u8 {
    var chars: []const u8 = @ptrCast(heap.get(symbol).words);
    while (chars.len > 0 and chars[chars.len - 1] == 0) chars.len -= 1;
    return chars;
}

pub const Lambda = struct { num_params: usize, instructions: Address, closure: Address, ir: ?Address };
pub fn new_lambda(heap: *Heap, lambda: Lambda) !Address {
    const lambda_symbol = try new_symbol(heap, "lambda");
    const num_params_obj = obj: {
        var b = try heap.object_builder();
        try b.emit_literal(@bitCast(lambda.num_params));
        break :obj b.finish();
    };
    const type_ = obj: {
        var b = try heap.object_builder();
        try b.emit_pointer(lambda_symbol);
        try b.emit_pointer(num_params_obj);
        break :obj b.finish();
    };
    const lambda_obj = obj: {
        var b = try heap.object_builder();
        try b.emit_pointer(type_);
        try b.emit_pointer(lambda.instructions);
        try b.emit_pointer(lambda.closure);
        if (lambda.ir) |ir| try b.emit_pointer(ir);
        break :obj b.finish();
    };
    return lambda_obj;
}
pub fn get_lambda(heap: *Heap, lambda: Address) Lambda {
    return .{
        .num_params = heap.load(heap.load(heap.load(lambda, 0), 1), 0),
        .instructions = heap.load(lambda, 1),
        .closure = heap.load(lambda, 2),
        .ir = if (heap.get(lambda).words.len == 4) heap.load(lambda, 3) else null,
    };
}

pub fn format(heap: Heap, value: Address, writer: *std.io.Writer, indentation: usize) !void {
    const type_ = heap.load(value, 0);
    const kind = heap.load(type_, 0);
    const kind_str = get_symbol(heap, kind);
    if (std.mem.eql(u8, kind_str, "int")) {
        try writer.print("{d}", .{heap.load(heap.load(value, 1), 0)});
    } else if (std.mem.eql(u8, kind_str, "string")) {
        try writer.print("\"{s}\"", .{get_symbol(heap, heap.load(value, 1))});
    } else if (std.mem.eql(u8, kind_str, "struct")) {
        try writer.print("(&", .{});
        for (heap.get(type_).words[1..], heap.get(value).words[1..]) |field_name, field| {
            try writer.print("\n", .{});
            for (0..indentation + 1) |_| try writer.writeAll("  ");
            try writer.print("{s} ", .{get_symbol(heap, field_name)});
            try writer.print("\n", .{});
            for (0..indentation + 2) |_| try writer.writeAll("  ");
            try format(heap, field, writer, indentation + 2);
        }
        try writer.print(")", .{});
    } else if (std.mem.eql(u8, kind_str, "enum")) {
        try writer.print("(| {s}", .{get_symbol(heap, heap.load(type_, 1))});
        const payload = heap.load(value, 1);
        try writer.print("\n", .{});
        for (0..indentation + 1) |_| try writer.writeAll("  ");
        try format(heap, payload, writer, indentation + 1);
        try writer.print(")", .{});
    } else if (std.mem.eql(u8, kind_str, "array")) {
        try writer.print("(array", .{});
        for (heap.get(value).words[1..]) |item| {
            try writer.print("\n", .{});
            for (0..indentation + 1) |_| try writer.writeAll("  ");
            try format(heap, item, writer, indentation + 1);
        }
        try writer.print(")", .{});
    } else if (std.mem.eql(u8, kind_str, "lambda")) {
        try writer.print("lambda", .{});
    } else @panic("unknown type");
}
