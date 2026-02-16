const std = @import("std");
const Heap = @import("heap.zig");
const Address = Heap.Address;
const Word = Heap.Word;
const Obj = Heap.Obj;
const Vm = @import("vm.zig");

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
    pub fn as_value(string: String) Value {
        return Value.from(string.obj);
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
    pub fn get_field(self: Struct, name: []const u8) Value {
        const ty = self.obj.child(0).children();
        for (ty[1..], 1..) |field, i| {
            if (std.mem.eql(u8, get_symbol(field), name)) {
                return Value.from(self.obj.child(i));
            }
        }
        @panic("field");
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
    pub fn new(heap: *Heap, instructions: Obj, captured: Obj, args: Obj, ir: ?Obj) !Lambda {
        const lambda_symbol = try new_symbol(heap, "lambda");
        const ty = try heap.new_inner(&.{lambda_symbol});
        const lambda_obj = obj: {
            var b = try heap.build_inner();
            try b.emit(ty);
            try b.emit(instructions);
            try b.emit(captured);
            try b.emit(args);
            if (ir) |ir_| try b.emit(ir_);
            break :obj b.finish();
        };
        return .{ .obj = lambda_obj };
    }
    pub fn get_captured(self: Lambda) Obj {
        return self.obj.child(2);
    }
    pub fn get_args(self: Lambda) Obj {
        return self.obj.child(3);
    }
    pub fn get_ir(self: Lambda) ?Obj {
        if (self.obj.children().len < 5) return null;
        return self.obj.child(4);
    }

    pub fn call(lambda: Lambda, vm: *Vm, args: []const Value) !Value {
        const instructions = lambda.obj.child(1);
        const closure = lambda.obj.child(2);
        const num_params = lambda.obj.child(3).children().len;
        if (num_params != args.len) @panic("called function with wrong number of params");
        for (args) |arg| try vm.impl.push(arg.obj.address);
        try vm.impl.push(closure.address);
        try vm.impl.run(instructions);
        return .{ .obj = .{ .address = vm.impl.pop() } };
    }
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
    pub fn format_singleline(self: Value, writer: *std.io.Writer) error{WriteFailed}!void {
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
            .lambda => |lambda| {
                try writer.print("(\\ (", .{});
                for (lambda.get_args().children(), 0..) |arg, i| {
                    if (i > 0) try writer.print(" ", .{});
                    try writer.print("{s}", .{get_symbol(arg)});
                }
                try writer.print(") ", .{});
                if (lambda.get_ir()) |ir| {
                    try Value.from(ir).format_singleline_code(writer);
                } else {
                    try writer.print("...", .{});
                }
                try writer.print(")", .{});
            },
        }
    }
    pub fn format_singleline_code(self: Value, writer: *std.io.Writer) !void {
        const enum_ = self.kind().enum_;
        const variant = enum_.variant();
        const payload = enum_.payload();
        if (std.mem.eql(u8, variant, "object")) {
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
            const args = payload.kind().struct_;
            try writer.print("(: {s} ", .{args.get_field("name").kind().string.get()});
            try args.get_field("def").format_singleline_code(writer);
            try writer.print(" ", .{});
            try args.get_field("expr").format_singleline_code(writer);
            try writer.print(")", .{});
            return;
        }
        if (std.mem.eql(u8, variant, "name")) {
            const name = payload.kind().string.get();
            try writer.print("{s}", .{name});
            return;
        }
        if (std.mem.eql(u8, variant, "add")) {
            const args = payload.kind().struct_;
            try writer.print("(@add ", .{});
            try args.get_field("left").format_singleline_code(writer);
            try writer.print(" ", .{});
            try args.get_field("right").format_singleline_code(writer);
            try writer.print(")", .{});
            return;
        }
        if (std.mem.eql(u8, variant, "subtract")) {
            const args = payload.kind().struct_;
            try writer.print("(@subtract ", .{});
            try args.get_field("left").format_singleline_code(writer);
            try writer.print(" ", .{});
            try args.get_field("right").format_singleline_code(writer);
            try writer.print(")", .{});
            return;
        }
        if (std.mem.eql(u8, variant, "multiply")) {
            const args = payload.kind().struct_;
            try writer.print("(@multiply ", .{});
            try args.get_field("left").format_singleline_code(writer);
            try writer.print(" ", .{});
            try args.get_field("right").format_singleline_code(writer);
            try writer.print(")", .{});
            return;
        }
        if (std.mem.eql(u8, variant, "string")) {
            try payload.format_singleline(writer);
            return;
        }
        if (std.mem.eql(u8, variant, "struct")) {
            const struct_payload = payload.kind().array.items();
            try writer.print("(&", .{});
            for (struct_payload) |field| {
                const field_struct = field.kind().struct_;
                try writer.print(" {s} ", .{field_struct.get_field("name").kind().string.get()});
                try field_struct.get_field("value").format_singleline_code(writer);
            }
            try writer.print(")", .{});
            return;
        }
        if (std.mem.eql(u8, variant, "field")) {
            const field = payload.kind().struct_;
            try writer.print("(. ", .{});
            try field.get_field("of").format_singleline_code(writer);
            try writer.print(" {s})", .{field.get_field("name").kind().string.get()});
            return;
        }
        if (std.mem.eql(u8, variant, "enum")) {
            const enum_payload = payload.kind().struct_;
            const actual_variant = enum_payload.get_field("variant").kind().string.get();
            const actual_payload = enum_payload.get_field("payload");
            try writer.print("(| {s} ", .{actual_variant});
            try actual_payload.format_singleline_code(writer);
            try writer.print(")", .{});
            return;
        }
        if (std.mem.eql(u8, variant, "switch")) {
            const args = payload.kind().struct_;
            try writer.print("(% ", .{});
            try args.get_field("condition").format_singleline_code(writer);
            const cases = args.get_field("cases").kind().array.items();
            for (cases) |case| {
                const case_struct = case.kind().struct_;
                const case_variant = case_struct.get_field("variant").kind().string.get();
                const binding = bind: {
                    const en = case_struct.get_field("binding").kind().enum_;
                    const bind = en.variant();
                    if (std.mem.eql(u8, bind, "none")) break :bind null;
                    if (std.mem.eql(u8, bind, "some")) break :bind en.payload().kind().string.get();
                    @panic("invalid IR");
                };
                const body = case_struct.get_field("body");
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
        if (std.mem.eql(u8, variant, "function")) {
            const fun = payload.kind().struct_;
            try writer.print("(\\ (", .{});
            const args = fun.get_field("args").kind().array.items();
            for (args, 0..) |arg, i| {
                if (i > 0) try writer.print(" ", .{});
                try writer.print("{s}", .{arg.kind().string.get()});
            }
            try writer.print(") ", .{});
            try fun.get_field("body").format_singleline_code(writer);
            try writer.print(")", .{});
            return;
        }
        if (std.mem.eql(u8, variant, "call")) {
            const call = payload.kind().struct_;
            try writer.print("(", .{});
            try call.get_field("callee").format_singleline_code(writer);
            const args = call.get_field("args").kind().array.items();
            for (args) |arg| {
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
            .lambda => |lambda| {
                try writer.print("(\\ (", .{});
                for (lambda.get_args().children(), 0..) |arg, i| {
                    if (i > 0) try writer.print(" ", .{});
                    try writer.print("{s}", .{get_symbol(arg)});
                }
                try writer.print(") ", .{});
                if (lambda.get_ir()) |ir| {
                    try Value.from(ir).format_singleline_code(writer);
                } else {
                    try writer.print("...", .{});
                }
                try writer.print(")", .{});
            },
        }
    }
};
