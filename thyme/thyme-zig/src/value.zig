const std = @import("std");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Obj = Heap.Obj;

pub fn new_empty(heap: *Heap) !Obj {
    var b = try heap.build_leaf();
    return b.finish();
}

pub fn new_symbol(heap: *Heap, value: []const u8) !Obj {
    var b = try heap.build_leaf();
    const num_words = (value.len + 7) / 8;
    for (0..num_words) |i| {
        var w: Word = 0;
        for (0..@min(value.len - i * 8, 8)) |j|
            w |= @as(Word, value[i * 8 + j]) << @intCast(j * 8);
        try b.emit(w);
    }
    return b.finish();
}

pub fn get_symbol(obj: Obj) []const u8 {
    var chars: []const u8 = @ptrCast(obj.words());
    while (chars.len > 0 and chars[chars.len - 1] == 0) chars.len -= 1;
    return chars;
}

pub const Int = struct {
    obj: Obj,

    pub fn from(obj: Obj) Int {
        return Value.from(obj).kind().int;
    }
    pub fn as_value(int: Int) Value {
        return Value.from(int.obj);
    }
    pub fn new(heap: *Heap, value: i64) !Int {
        const int_symbol = try new_symbol(heap, "int");
        const type_ = try heap.new_inner(&.{int_symbol});
        const literal_obj = try heap.new_leaf(&.{@bitCast(value)});
        const int_obj = try heap.new_inner(&.{ type_, literal_obj });
        return .{ .obj = int_obj };
    }
    pub fn get(self: Int) i64 {
        return @bitCast(self.obj.child(1).word(0));
    }
};

pub const String = struct {
    obj: Obj,

    pub fn from(obj: Obj) String {
        return Value.from(obj).kind().string;
    }
    pub fn new(heap: *Heap, string: []const u8) !String {
        const string_symbol = try new_symbol(heap, "string");
        const ty = try heap.new_inner(&.{string_symbol});
        const symbol = try new_symbol(heap, string);
        return .{ .obj = try heap.new_inner(&.{ ty, symbol }) };
    }
    pub fn get(self: String) []const u8 {
        return get_symbol(self.obj.child(1));
    }
};

pub const Struct = struct {
    obj: Obj,

    pub fn from(obj: Obj) Struct {
        return Value.from(obj).kind().struct_;
    }
};

pub const Enum = struct {
    obj: Obj,

    pub fn from(obj: Obj) Enum {
        return Value.from(obj).kind().enum_;
    }
    pub fn variant_obj(self: Enum) Obj {
        return self.obj.child(0).child(1);
    }
    pub fn variant(self: Enum) []const u8 {
        return get_symbol(self.variant_obj());
    }
    pub fn payload(self: Enum) Value {
        return .{ .obj = self.obj.child(1) };
    }
};

pub const Array = struct {
    obj: Obj,

    pub fn from(obj: Obj) Array {
        return Value.from(obj).kind().array;
    }
    pub fn items(self: Array) []const Value {
        return @ptrCast(self.obj.children()[1..]);
    }
};

pub const Lambda = struct {
    obj: Obj,

    pub fn from(obj: Obj) Lambda {
        return Value.from(obj).kind().lambda;
    }
    pub fn new(heap: *Heap, instructions: Obj, num_params: usize, closure: Obj, ir: ?Obj) !Lambda {
        const lambda_symbol = try new_symbol(heap, "lambda");
        const num_params_obj = try heap.new_leaf(&.{@bitCast(num_params)});
        const ty = try heap.new_inner(&.{ lambda_symbol, num_params_obj });
        const lambda_obj = obj: {
            var b = try heap.build_inner();
            try b.emit(ty);
            try b.emit(instructions);
            try b.emit(closure);
            if (ir) |ir_| try b.emit(ir_);
            break :obj b.finish();
        };
        return .{ .obj = lambda_obj };
    }
    // pub fn get_lambda(heap: *Heap, lambda: Address) Lambda {
    //     return .{
    //         .num_params = heap.load(heap.load(heap.load(lambda, 0), 1), 0),
    //         .instructions = heap.load(lambda, 1),
    //         .closure = heap.load(lambda, 2),
    //         .ir = if (heap.get(lambda).words.len == 4) heap.load(lambda, 3) else null,
    //     };
    // }
};

pub const Value = struct {
    obj: Obj,

    pub fn from(obj: Obj) Value {
        return .{ .obj = obj };
    }

    pub const Kind = union(enum) {
        int: Int,
        string: String,
        struct_: Struct,
        enum_: Enum,
        array: Array,
        lambda: Lambda,
    };
    pub fn kind(self: Value) Kind {
        const obj = self.obj;
        const kind_str = get_symbol(obj.child(0).child(0));
        if (std.mem.eql(u8, kind_str, "int")) return .{ .int = Int{ .obj = obj } };
        if (std.mem.eql(u8, kind_str, "string")) return .{ .string = String{ .obj = obj } };
        if (std.mem.eql(u8, kind_str, "struct")) return .{ .struct_ = Struct{ .obj = obj } };
        if (std.mem.eql(u8, kind_str, "enum")) return .{ .enum_ = Enum{ .obj = obj } };
        if (std.mem.eql(u8, kind_str, "array")) return .{ .array = Array{ .obj = obj } };
        if (std.mem.eql(u8, kind_str, "lambda")) return .{ .lambda = Lambda{ .obj = obj } };
        std.debug.print("kind '{s}'\n", .{kind_str});
        @panic("unknown type ");
    }

    pub fn format(self: Value, writer: *std.io.Writer) !void {
        try self.format_indented(writer, 0);
    }
    pub fn format_singleline(self: Value, writer: *std.io.Writer) !void {
        const ty = self.obj.child(0);
        switch (self.kind()) {
            .int => |int| try writer.print("{d}", .{int.get()}),
            .string => |str| try writer.print("\"{s}\"", .{str.get()}),
            .struct_ => {
                try writer.print("(&", .{});
                for (ty.children()[1..], self.obj.children()[1..]) |field_name, field| {
                    try writer.print(" {s} ", .{get_symbol(field_name)});
                    try (Value{ .obj = field }).format_singleline(writer);
                }
                try writer.print(")", .{});
            },
            .enum_ => |en| {
                try writer.print("(| {s} ", .{en.variant()});
                try en.payload().format_singleline(writer);
                try writer.print(")", .{});
            },
            .array => |a| {
                try writer.print("([]", .{});
                for (a.items()) |item| {
                    try writer.print(" ", .{});
                    try item.format_singleline(writer);
                }
                try writer.print(")", .{});
            },
            .lambda => try writer.print("lambda", .{}),
        }
    }
    fn singleline_len(self: Value) !usize {
        var len_tracker = std.Io.Writer.Discarding.init(&[_]u8{});
        try self.format_singleline(&len_tracker.writer);
        return len_tracker.count;
    }
    const WIDTH_LIMIT = 80;
    pub fn format_indented(self: Value, writer: *std.io.Writer, indentation: usize) !void {
        if (2 * indentation + try self.singleline_len() <= WIDTH_LIMIT) {
            try self.format_singleline(writer);
            return;
        }

        const ty = self.obj.child(0);
        switch (self.kind()) {
            .int => |int| try writer.print("{d}", .{int.get()}),
            .string => |str| try writer.print("\"{s}\"", .{str.get()}),
            .struct_ => {
                try writer.print("(&", .{});
                for (ty.children()[1..], self.obj.children()[1..]) |field_name, field_value| {
                    const name = get_symbol(field_name);
                    const value = Value{ .obj = field_value };
                    try writer.print("\n", .{});
                    for (0..indentation + 1) |_| try writer.writeAll("  ");
                    try writer.print("{s} ", .{name});
                    if (2 * (indentation + 1) + name.len + 1 + try value.singleline_len() <= WIDTH_LIMIT) {
                        try value.format_singleline(writer);
                    } else {
                        try writer.print("\n", .{});
                        for (0..indentation + 2) |_| try writer.writeAll("  ");
                        try value.format_indented(writer, indentation + 2);
                    }
                }
                try writer.print(")", .{});
            },
            .enum_ => |en| {
                try writer.print("(| {s}", .{en.variant()});
                try writer.print("\n", .{});
                for (0..indentation + 1) |_| try writer.writeAll("  ");
                try en.payload().format_indented(writer, indentation + 1);
                try writer.print(")", .{});
            },
            .array => |a| {
                try writer.print("([]", .{});
                for (a.items()) |item| {
                    try writer.print("\n", .{});
                    for (0..indentation + 1) |_| try writer.writeAll("  ");
                    try item.format_indented(writer, indentation + 1);
                }
                try writer.print(")", .{});
            },
            .lambda => try writer.print("lambda", .{}),
        }
    }
};
