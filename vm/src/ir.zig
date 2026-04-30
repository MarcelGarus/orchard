const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const new_symbol = Heap.new_symbol;
const get_symbol = Heap.get_symbol;

pub const Fun = struct {
    obj: Obj,

    pub fn args(fun: Fun) []const Obj {
        return fun.obj.child(0).children();
    }
    pub fn body(fun: Fun) Expr {
        return Expr{ .obj = fun.obj.child(1) };
    }

    pub fn format(fun: Fun, writer: *Writer) !void {
        try writer.print("fun\n", .{});
        try writer.print("{f}", .{fun.obj});
        // for (fun.args()) |arg| try writer.print(" {s}", .{get_symbol(arg)});
        // try writer.print("\n", .{});
        // (Expr{ .obj = fun.obj.child(1) }).format_indented(writer, 1) catch return error.WriteFailed;
    }
};

const Name = []const u8;

pub const Expr = struct {
    obj: Obj,

    pub const Kind = union(enum) {
        word: Word,
        object: Obj,
        name: Name,
        let: Let,
        also: Also,
        add: LeftRight,
        subtract: LeftRight,
        multiply: LeftRight,
        divide: LeftRight,
        modulo: LeftRight,
        shift_left: LeftRight,
        shift_right: LeftRight,
        and_: LeftRight,
        or_: LeftRight,
        xor: LeftRight,
        compare: LeftRight,
        if_: If,
        new_leaf: []const Expr,
        new_inner: []const Expr,
        flatten_to_leaf: Expr,
        flatten_to_inner: Expr,
        points: Expr,
        size: Expr,
        load: Load,
        call: Call,
        rec: []const Expr,
        collect_garbage: Expr,
        crash: Expr,
        unreachable_,

        pub const Let = struct { name: Name, def: Expr, expr: Expr };
        pub const LeftRight = struct { left: Expr, right: Expr };
        pub const If = struct { condition: Expr, then: Expr, else_: Expr };
        pub const Load = struct { object: Expr, index: Expr };
        pub const Also = struct { ignored: Expr, value: Expr };
        pub const Call = struct { fun: Expr, args: []const Expr };
    };

    pub fn kind(self: Expr) !Kind {
        const tag = get_symbol(self.obj.child(0));
        inline for (@typeInfo(Kind).@"union".fields) |field| {
            const expected_tag = comptime kebab_case: {
                var buf: [field.name.len]u8 = undefined;
                var len: usize = 0;
                for (field.name, 0..) |c, i| {
                    if (c == '_' and i == field.name.len - 1) continue;
                    buf[len] = if (c == '_') '-' else c;
                    len += 1;
                }
                break :kebab_case buf[0..len] ++ "";
            };
            if (std.mem.eql(u8, tag, expected_tag)) {
                const payload: field.type = switch (field.type) {
                    void => {},
                    Word => self.obj.child(1).word(0),
                    Obj => self.obj.child(1),
                    Name => get_symbol(self.obj.child(1)),
                    Expr => .{ .obj = self.obj.child(1) },
                    Kind.Let => .{ .name = get_symbol(self.obj.child(1)), .def = .{ .obj = self.obj.child(2) }, .expr = .{ .obj = self.obj.child(3) } },
                    Kind.Also => .{ .ignored = .{ .obj = self.obj.child(1) }, .value = .{ .obj = self.obj.child(2) } },
                    Kind.LeftRight => .{ .left = .{ .obj = self.obj.child(1) }, .right = .{ .obj = self.obj.child(2) } },
                    Kind.If => .{ .condition = .{ .obj = self.obj.child(1) }, .then = .{ .obj = self.obj.child(2) }, .else_ = .{ .obj = self.obj.child(3) } },
                    Kind.Load => .{ .object = .{ .obj = self.obj.child(1) }, .index = .{ .obj = self.obj.child(2) } },
                    Kind.Call => .{ .fun = .{ .obj = self.obj.child(1) }, .args = @ptrCast(self.obj.child(2).children()) },
                    []const Expr => @ptrCast(self.obj.child(1).children()),
                    else => @compileError("Unsupported payload type: " ++ @typeName(field.type)),
                };
                return @unionInit(Kind, field.name, payload);
            }
        }

        return error.BadIr;
    }

    pub fn format(expr: Expr, writer: *Writer) !void {
        expr.format_indented(writer, 0) catch return error.WriteFailed;
    }
    pub fn format_indented(expr: Expr, writer: *Writer, indentation: usize) !void {
        for (0..indentation) |_| try writer.print("  ", .{});
        switch (try expr.kind()) {
            .word => |word| try writer.print("word {}\n", .{word}),
            .object => |obj| try writer.print("object {x}\n", .{obj.address}),
            .name => |name| try writer.print("name {s}\n", .{name}),
            .let => |let| {
                try writer.print("let {s}\n", .{let.name});
                try let.def.format_indented(writer, indentation + 1);
                try let.expr.format_indented(writer, indentation + 1);
            },
            .add => |args| {
                try writer.print("add\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .subtract => |args| {
                try writer.print("subtract\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .multiply => |args| {
                try writer.print("multiply\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .divide => |args| {
                try writer.print("divide\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .modulo => |args| {
                try writer.print("modulo\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .shift_left => |args| {
                try writer.print("shift left\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .shift_right => |args| {
                try writer.print("shift right\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .and_ => |args| {
                try writer.print("and\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .or_ => |args| {
                try writer.print("or\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .xor => |args| {
                try writer.print("xor\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .compare => |args| {
                try writer.print("compare\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .if_ => |if_| {
                try writer.print("if\n", .{});
                try if_.condition.format_indented(writer, indentation + 1);
                try if_.then.format_indented(writer, indentation + 1);
                try if_.else_.format_indented(writer, indentation + 1);
            },
            .new_leaf => |children| {
                try writer.print("new leaf\n", .{});
                for (children) |child|
                    try child.format_indented(writer, indentation + 1);
            },
            .new_inner => |children| {
                try writer.print("new inner\n", .{});
                for (children) |child|
                    try child.format_indented(writer, indentation + 1);
            },
            .flatten_to_leaf => |arg| {
                try writer.print("flatten to leaf\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .flatten_to_inner => |arg| {
                try writer.print("flatten to inner\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .points => |arg| {
                try writer.print("points\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .size => |arg| {
                try writer.print("size\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .load => |load| {
                try writer.print("load\n", .{});
                try load.object.format_indented(writer, indentation + 1);
                try load.index.format_indented(writer, indentation + 1);
            },
            .collect_garbage => |arg| {
                try writer.print("gc\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .also => |also| {
                try writer.print("also\n", .{});
                try also.ignored.format_indented(writer, indentation + 1);
                try also.value.format_indented(writer, indentation + 1);
            },
            .call => |call| {
                try writer.print("call\n", .{});
                try call.fun.format_indented(writer, indentation + 1);
                for (call.args) |arg|
                    try arg.format_indented(writer, indentation + 1);
            },
            .rec => |args| {
                try writer.print("rec\n", .{});
                for (args) |arg|
                    try arg.format_indented(writer, indentation + 1);
            },
            .crash => |error_| {
                try writer.print("crash\n", .{});
                try error_.format_indented(writer, indentation + 1);
            },
            .unreachable_ => try writer.print("unreachable\n", .{}),
        }
    }
};
