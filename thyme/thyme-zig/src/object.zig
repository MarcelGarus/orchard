// The heap is agnostic about the layout of objects – every object just has a
// tag and either literal words or pointers to other objects. Thyme code expects
// objects with more semantic meaning. It uses the following object layout:
//
// Int         = object with tag 0, then 1 word with the signed int value (i64)
// Symbol      = object with tag 1, then UTF-8 bytes, padded with 0 to word size
// Composite   = object with tag 2, then pointers to other objects
// Nil         = composite with no pointers
// Instruction = composite where the first pointer pointers to a symbol that
//               tells us which kind of instruction this is (push_word, add,
//               etc.). The other pointers are payloads, i.e. the push_word
//               instructions contains a pointer to an int.
// Linked List = composite that is either:
//               - nil, to represent an empty list, or
//               - a composite with two pointers:
//                 - head pointing to an item
//                 - tail pointing to another linked list
// Fun         = object with tag 3 and three pointers:
//               - pointer to int that is actually a pointer to the IR in Zig (TODO)
//                 OR pointer to nil if there is no IR
//               - pointer to a linked list of instructions
//               - pointer to an int, the number of arguments
// Lambda      = object with tag 4 and two pointers:
//               - pointer to a fun
//               - pointer to a closure (composite)

const std = @import("std");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;

pub const Tag = enum(u8) {
    int = 0,
    symbol = 1,
    composite = 2,
    fun = 3,
    lambda = 4,
};

pub fn new_int(heap: *Heap, val: i64) !Address {
    var builder = try heap.object_builder(@intFromEnum(Tag.int));
    try builder.emit_literal(@bitCast(val));
    return try builder.finish();
}
pub fn assert_int(heap: *Heap, obj: Address) !void {
    if (heap.get(obj).tag != @intFromEnum(Tag.int)) return error.NotInt;
}
pub fn get_int(heap: Heap, int: Address) i64 {
    return @bitCast(heap.load(int, 0));
}

pub fn new_symbol(heap: *Heap, val: []const u8) !Address {
    var builder = try heap.object_builder(@intFromEnum(Tag.symbol));
    const num_words = (val.len + 7) / 8;
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(val.len - i * 8, 8)) |j|
            w |= @as(Word, val[i * 8 + j]) << @intCast(j * 8);
        try builder.emit_literal(w);
    }
    return try builder.finish();
}
pub fn assert_symbol(heap: *Heap, obj: Address) !void {
    if (heap.get(obj).tag != @intFromEnum(Tag.symbol)) return error.NotSymbol;
}
pub fn get_symbol(heap: Heap, symbol: Address) []const u8 {
    var chars: []const u8 = @ptrCast(heap.get(symbol).words);
    while (chars.len > 0 and chars[chars.len - 1] == 0) {
        std.debug.print("chars: {any}\n", .{chars});
        chars.len -= 1;
    }
    return chars;
}
// pub fn format_symbol(heap: Heap, symbol: Address, writer: *Writer) !void {
//     for (heap.get(symbol).literals) |word| {
//         for (0..8) |i| {
//             const c: u8 = @intCast((word >> @intCast(8 * i)) & 0xff);
//             if (c == 0) break;
//             if (c >= 32 and c <= 150)
//                 try writer.print("{c}", .{c})
//             else
//                 try writer.print("?", .{});
//         }
//     }
// }

pub fn new_nil(heap: *Heap) !Address {
    var builder = try heap.object_builder(@intFromEnum(Tag.composite));
    return try builder.finish();
}

pub const Fun = struct { num_params: Address, ir_: Address, instructions_: Address };
pub fn new_fun(heap: *Heap, num_params_: Address, ir_: Address, instructions_: Address) !Address {
    var builder = try heap.object_builder(@intFromEnum(Tag.fun));
    try builder.emit_pointer(ir_);
    try builder.emit_pointer(instructions_);
    try builder.emit_pointer(num_params_);
    return try builder.finish();
}
pub fn assert_fun(heap: *Heap, obj: Address) !void {
    if (heap.get(obj).tag != @intFromEnum(Tag.lambda)) return error.NotFun;
}
pub fn get_fun(heap: *Heap, fun: Address) Fun {
    return .{
        .ir_ = heap.load(fun, 0),
        .instructions_ = heap.load(fun, 1),
        .num_params = heap.load(fun, 2),
    };
}

pub const Lambda = struct { fun: Address, closure: Address };
pub fn new_lambda(heap: *Heap, lambda: Lambda) !Address {
    var builder = try heap.object_builder(@intFromEnum(Tag.lambda));
    try builder.emit_pointer(lambda.fun);
    try builder.emit_pointer(lambda.closure);
    return try builder.finish();
}
pub fn assert_lambda(heap: *Heap, obj: Address) !void {
    if (heap.get(obj).tag != @intFromEnum(Tag.lambda)) return error.NotLambda;
}
pub fn get_lambda(heap: *Heap, lambda: Address) Lambda {
    return .{
        .fun = heap.load(lambda, 0),
        .closure = heap.load(lambda, 1),
    };
}
