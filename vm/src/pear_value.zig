const std = @import("std");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Obj = Heap.Obj;
const Vm = @import("vm.zig");
const Ir = @import("ir.zig");

obj: Obj,

const Value = @This();

pub fn from(obj: Obj) Value {
    return .{ .obj = obj };
}

// pub fn new_empty(heap: *Heap) !Obj {
//     var b = try heap.build_leaf();
//     return b.finish();
// }

pub const Kind = union(enum) { int, string, struct_, enum_, array, function };
pub fn kind(self: Value) Kind {
    const obj = self.obj;
    const kind_str = Heap.get_symbol(obj.child(0).child(0));
    if (std.mem.eql(u8, kind_str, "int")) return .int;
    if (std.mem.eql(u8, kind_str, "string")) return .string;
    if (std.mem.eql(u8, kind_str, "struct")) return .struct_;
    if (std.mem.eql(u8, kind_str, "enum")) return .enum_;
    if (std.mem.eql(u8, kind_str, "array")) return .array;
    if (std.mem.eql(u8, kind_str, "function")) return .function;
    std.debug.print("kind '{s}'\n", .{kind_str});
    @panic("unknown type ");
}

// Int stuff.

pub fn new_int(heap: *Heap, value: i64) !Value {
    const int_symbol = try heap.new_symbol("int");
    const type_ = try heap.new_inner(&.{int_symbol});
    const literal_obj = try heap.new_leaf(&.{@bitCast(value)});
    const int_obj = try heap.new_inner(&.{ type_, literal_obj });
    return .{ .obj = int_obj };
}

pub fn get_int(self: Value) i64 {
    std.debug.assert(self.kind() == .int);
    return @bitCast(self.obj.child(1).word(0));
}

// String stuff.

pub fn new_string(heap: *Heap, string: []const u8) !Value {
    const string_symbol = try heap.new_symbol("string");
    const ty = try heap.new_inner(&.{string_symbol});
    const symbol = try heap.new_symbol(string);
    return .{ .obj = try heap.new_inner(&.{ ty, symbol }) };
}

pub fn get_string(self: Value) []const u8 {
    std.debug.assert(self.kind() == .string);
    return Heap.get_symbol(self.obj.child(1));
}

// Struct stuff.

pub fn get_field(self: Value, name: []const u8) Value {
    std.debug.assert(self.kind() == .struct_);
    const ty = self.obj.child(0).children();
    for (ty[1..], 1..) |field, i| {
        if (std.mem.eql(u8, Heap.get_symbol(field), name)) {
            return Value.from(self.obj.child(i));
        }
    }
    @panic("field");
}

// Enum stuff.

pub fn new_enum(heap: *Heap, variant: []const u8) !Value {
    _ = heap;
    _ = variant;
    @panic("todo: new_enum");
}

pub fn get_variant(self: Value) []const u8 {
    std.debug.assert(self.kind() == .enum_);
    return Heap.get_symbol(self.obj.child(0).child(1));
}
pub fn get_payload(self: Value) Value {
    std.debug.assert(self.kind() == .enum_);
    return .{ .obj = self.obj.child(1) };
}

// Array stuff.

pub fn get_items(self: Value) []const Value {
    std.debug.assert(self.kind() == .array);
    return @ptrCast(self.obj.children()[1..]);
}

// Function stuff.

pub fn new(heap: *Heap, args: Obj, pear_ir: ?Obj, fun: Obj, captured: Obj) !Value {
    const function_symbol = try heap.new_symbol("function");
    const ty = obj: {
        var b = try heap.build_inner();
        try b.emit(function_symbol);
        try b.emit(args);
        try b.emit(fun);
        if (pear_ir) |ir_| try b.emit(ir_);
        break :obj b.finish();
    };
    return try heap.new_inner(&.{ ty, captured });
}
pub fn get_args(self: Value) Obj {
    return self.obj.child(0).child(1);
}
pub fn get_ir(self: Value) ?Obj {
    const type_ = self.obj.child(0);
    return if (type_.size() == 4) type_.child(3) else null;
}
pub fn get_fun(self: Value) Ir.Fun {
    return Ir.Fun{ .obj = self.obj.child(0).child(2) };
}
pub fn get_captured(self: Value) Obj {
    return self.obj.child(1);
}

pub fn call(function: Value, vm: *Vm, args: []const Value) !Value {
    std.debug.assert(function.kind() == .function);
    std.debug.assert(function.get_args().size() == args.len);
    const fun = function.get_fun();
    const closure = function.get_captured();
    const all_args = try vm.impl.ally.alloc(Word, args.len + 1);
    for (args, 0..) |arg, i| all_args[i] = arg.obj.address;
    all_args[args.len] = closure.address;
    const result = try vm.call(fun, all_args);
    return .{ .obj = .{ .address = result } };
}

pub fn format(self: Value, writer: *std.io.Writer) !void {
    try self.format_indented(writer, 0);
}
pub fn format_singleline(self: Value, writer: *std.io.Writer) error{WriteFailed}!void {
    const ty = self.obj.child(0);
    switch (self.kind()) {
        .int => try writer.print("{d}", .{self.get_int()}),
        .string => try writer.print("\"{s}\"", .{self.get_string()}),
        .struct_ => {
            try writer.print("(&", .{});
            for (ty.children()[1..], self.obj.children()[1..]) |field_name, field| {
                try writer.print(" {s} ", .{Heap.get_symbol(field_name)});
                try (Value{ .obj = field }).format_singleline(writer);
            }
            try writer.print(")", .{});
        },
        .enum_ => {
            try writer.print("(| {s} ", .{self.get_variant()});
            try self.get_payload().format_singleline(writer);
            try writer.print(")", .{});
        },
        .array => {
            try writer.print("([]", .{});
            for (self.get_items()) |item| {
                try writer.print(" ", .{});
                try item.format_singleline(writer);
            }
            try writer.print(")", .{});
        },
        .function => {
            try writer.print("(\\ (", .{});
            for (self.get_args().children(), 0..) |arg, i| {
                if (i > 0) try writer.print(" ", .{});
                try writer.print("{s}", .{Heap.get_symbol(arg)});
            }
            try writer.print(") [", .{});
            try self.get_captured().format(writer);
            // try Value.from(self.get_captured()).format_singleline(writer);
            try writer.print("] ", .{});
            if (self.get_ir()) |ir| {
                try Value.from(ir).format_singleline_code(writer);
            } else {
                try writer.print("...", .{});
            }
            try writer.print(")", .{});
        },
    }
}
pub fn format_singleline_code(self: Value, writer: *std.io.Writer) !void {
    const variant = self.get_variant();
    const payload = self.get_payload();
    if (std.mem.eql(u8, variant, "value")) {
        try payload.format_singleline(writer);
        return;
    }
    if (std.mem.eql(u8, variant, "crash")) {
        try writer.print("(@crash ", .{});
        try payload.format_singleline_code(writer);
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "let")) {
        try writer.print("(: {s} ", .{payload.get_field("name").get_string()});
        try payload.get_field("definition").format_singleline_code(writer);
        try writer.print(" ", .{});
        try payload.get_field("body").format_singleline_code(writer);
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "name")) {
        try writer.print("{s}", .{payload.get_string()});
        return;
    }
    if (std.mem.eql(u8, variant, "add")) {
        try writer.print("(@add ", .{});
        try payload.get_field("left").format_singleline_code(writer);
        try writer.print(" ", .{});
        try payload.get_field("right").format_singleline_code(writer);
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "subtract")) {
        try writer.print("(@subtract ", .{});
        try payload.get_field("left").format_singleline_code(writer);
        try writer.print(" ", .{});
        try payload.get_field("right").format_singleline_code(writer);
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "multiply")) {
        try writer.print("(@multiply ", .{});
        try payload.get_field("left").format_singleline_code(writer);
        try writer.print(" ", .{});
        try payload.get_field("right").format_singleline_code(writer);
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "string")) {
        try payload.format_singleline(writer);
        return;
    }
    if (std.mem.eql(u8, variant, "struct")) {
        try writer.print("(&", .{});
        for (payload.get_items()) |field| {
            try writer.print(" {s} ", .{field.get_field("name").get_string()});
            try field.get_field("value").format_singleline_code(writer);
        }
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "field")) {
        const of = payload.get_field("of");
        const name = payload.get_field("name");
        if (name.kind() == .enum_ and std.mem.eql(u8, name.get_variant(), "value") and name.get_payload().kind() == .string) {
            try writer.print("(.{s} ", .{name.get_payload().get_string()});
            try of.format_singleline_code(writer);
            try writer.print(")", .{});
        } else {
            try writer.print("(@field ", .{});
            try of.format_singleline_code(writer);
            try writer.print(" ", .{});
            try name.format_singleline_code(writer);
            try writer.print(")", .{});
        }
        return;
    }
    if (std.mem.eql(u8, variant, "enum")) {
        const actual_variant = payload.get_field("variant").get_string();
        const actual_payload = payload.get_field("payload");
        try writer.print("(| {s} ", .{actual_variant});
        try actual_payload.format_singleline_code(writer);
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "switch")) {
        try writer.print("(% ", .{});
        try payload.get_field("condition").format_singleline_code(writer);
        for (payload.get_field("cases").get_items()) |case| {
            const case_variant = case.get_field("variant").get_string();
            const binding_val = case.get_field("payload");
            const binding = bind: {
                const bind = binding_val.get_variant();
                if (std.mem.eql(u8, bind, "none")) break :bind null;
                if (std.mem.eql(u8, bind, "some")) break :bind binding_val.get_payload().get_string();
                @panic("invalid IR");
            };
            const body = case.get_field("body");
            if (binding) |name| {
                try writer.print(" ({s} {s}) ", .{ case_variant, name });
            } else {
                try writer.print(" {s} ", .{case_variant});
            }
            try body.format_singleline_code(writer);
        }
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "make_function")) {
        try writer.print("(@function ", .{});
        try payload.get_field("arguments").format_singleline_code(writer);
        try writer.print(" ", .{});
        try payload.get_field("captured").format_singleline_code(writer);
        try writer.print(" ", .{});
        try payload.get_field("body").format_singleline_code(writer);
        try writer.print(")", .{});
        return;
    }
    if (std.mem.eql(u8, variant, "call")) {
        try writer.print("(", .{});
        try payload.get_field("function").format_singleline_code(writer);
        for (payload.get_field("arguments").get_items()) |arg| {
            try writer.print(" ", .{});
            try arg.format_singleline_code(writer);
        }
        try writer.print(")", .{});
        return;
    }
    try writer.print("(@ir ", .{});
    try self.format_singleline(writer);
    try writer.print(")", .{});
}
fn singleline_len(self: Value) !usize {
    var len_tracker = std.Io.Writer.Discarding.init(&[_]u8{});
    try self.format_singleline(&len_tracker.writer);
    return len_tracker.count;
}
const WIDTH_LIMIT = 120;
pub fn format_indented(self: Value, writer: *std.io.Writer, indentation: usize) !void {
    if (indentation + try self.singleline_len() <= WIDTH_LIMIT) {
        try self.format_singleline(writer);
        return;
    }

    const ty = self.obj.child(0);
    switch (self.kind()) {
        .int => try writer.print("{d}", .{self.get_int()}),
        .string => try writer.print("\"{s}\"", .{self.get_string()}),
        .struct_ => {
            try writer.print("(&", .{});
            for (ty.children()[1..], self.obj.children()[1..]) |field_name, field_value| {
                const name = Heap.get_symbol(field_name);
                const value = Value{ .obj = field_value };
                try writer.print("\n", .{});
                for (0..indentation + 1) |_| try writer.writeAll(" ");
                try writer.print("{s} ", .{name});
                if (2 * (indentation + 1) + name.len + 1 + try value.singleline_len() <= WIDTH_LIMIT) {
                    try value.format_singleline(writer);
                } else {
                    try writer.print("\n", .{});
                    for (0..indentation + 2) |_| try writer.writeAll(" ");
                    try value.format_indented(writer, indentation + 2);
                }
            }
            try writer.print(")", .{});
        },
        .enum_ => {
            try writer.print("(| {s}", .{self.get_variant()});
            try writer.print("\n", .{});
            for (0..indentation + 1) |_| try writer.writeAll(" ");
            try self.get_payload().format_indented(writer, indentation + 1);
            try writer.print(")", .{});
        },
        .array => {
            try writer.print("([]", .{});
            for (self.get_items()) |item| {
                try writer.print("\n", .{});
                for (0..indentation + 1) |_| try writer.writeAll(" ");
                try item.format_indented(writer, indentation + 1);
            }
            try writer.print(")", .{});
        },
        .function => {
            try writer.print("(\\ (", .{});
            for (self.get_args().children(), 0..) |arg, i| {
                if (i > 0) try writer.print(" ", .{});
                try writer.print("{s}", .{Heap.get_symbol(arg)});
            }
            try writer.print(") ", .{});
            if (self.get_ir()) |ir| {
                try Value.from(ir).format_singleline_code(writer);
            } else {
                try writer.print("...", .{});
            }
            try writer.print(")", .{});
        },
    }
}
