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

// pub fn format_value(heap: Heap, value: Address, writer: *std.io.Writer, indentation: usize) !void {
//     for (0..indentation) |_| try writer.writeAll("  ");
//     const obj = heap.get(value);
//     const type_ = heap.get(obj.words[0]);
//     const kind = type_.get(0);
//     if (std.mem.eql(u8, kind, "int")) {
//         try writer.print("{d}", .{obj.words[1]});
//     } else if (std.mem.eql(u8, kind, "string")) {
//         try writer.print("{s}", .{get_symbol(heap, obj.words[1])});
//     } else if (std.mem.eql(u8, kind, "struct")) {
//         try writer.print("(&", .{});
//         if (obj.words.len == 1) { // Empty struct (only contains type_object_address)
//             try writer.print(")", .{});
//             return;
//         }
//         try writer.print("\n", .{});
//         // Iterate through fields (field name, field value)
//         // Field names are in the type object (starting from index 1)
//         // Field values are in the object itself (starting from index 1)
//         for (1..obj.words.len) |i| {
//             const field_name_address = heap.load(type_object_address, i);
//             for (0..indentation + 1) |_| { // Inlined indent
//                 try writer.writeAll("  ");
//             }
//             try writer.print("{s} ", .{get_symbol(heap, field_name_address)});
//             try format_value(heap, heap.load(value, i), writer, indentation + 1); // Recursively format value with increased indentation
//             try writer.print("\n", .{});
//         }
//         for (0..indentation) |_| { // Inlined indent
//             try writer.writeAll("  ");
//         }
//         try writer.print(")", .{});
//         return;
//     } else if (std.mem.eql(u8, kind, "enum")) {
//         try writer.print("(| {s}", .{get_symbol(heap, heap.load(type_object_address, 1))});
//         if (obj.words.len > 1) { // Check if there's a payload
//             const payload_address = heap.load(value, 1); // Payload is at index 1
//             if (is_nil(heap, payload_address)) {
//                 // If payload is nil (empty struct), don't print it.
//                 // This matches the `(| empty)` style rather than `(| empty nil)`
//             } else {
//                 try writer.print("\n", .{});
//                 try format_value(heap, payload_address, writer, indentation + 1); // Indent payload
//                 try writer.print("\n", .{});
//                 for (0..indentation) |_| { // Inlined indent
//                     try writer.writeAll("  ");
//                 }
//             }
//         }
//         try writer.print(")", .{});
//         return;
//     } else if (std.mem.eql(u8, kind, "lambda")) {
//         try writer.print("lambda", .{});
//     } else @panic("unknown type");
// }
