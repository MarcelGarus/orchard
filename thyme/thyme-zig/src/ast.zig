const std = @import("std");
const ArrayList = std.ArrayList;
const Writer = std.io.Writer;

expr: Expr,

pub const Str = []const u8;
pub const Expr = union(enum) {
    name: Str,
    body: []Expr,
    int: i64,
    string: Str,
    struct_: []Field,
    member: Member,
    enum_: Enum,
    switch_: Switch,
    lambda: Lambda,
    call: Call,
    var_: Var,
};
pub const Field = struct { name: Str, value: Expr };
pub const Member = struct { of: *Expr, name: Str };
pub const Enum = struct { variant: Str, payload: *Expr };
pub const Switch = struct { condition: *Expr, cases: []Case };
pub const Case = struct { variant: Str, payload: ?Str, body: Expr };
pub const Lambda = struct { params: []Str, body: *Expr };
pub const Call = struct { callee: *Expr, args: []Expr };
pub const Var = struct { name: Str, value: *Expr };

pub fn format(ast: @This(), writer: *Writer) !void {
    format_expr(ast.expr, writer, 0);
}
pub fn format_expr(expr: Expr, writer: *Writer, indentation: usize) void {
    switch (expr) {
        .name => |name| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("{s}\n", .{name});
        },
        .body => |body| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("{{\n", .{});
            for (body) |child| format_expr(child, writer, indentation + 1);
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("}}\n", .{});
        },
        .int => |int| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("{}\n", .{int});
        },
        .string => |string| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("\"{s}\"\n", .{string});
        },
        .struct_ => |struct_| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("&\n", .{});
            for (struct_) |field| {
                for (0..indentation + 1) |_| try writer.print("  ", .{});
                try writer.print("{s}:\n", .{field.name});
                format_expr(field.value, writer, indentation + 2);
            }
        },
        .member => |member| {
            format_expr(member.of.*, writer, indentation);
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print(".{s}\n", .{member.name});
        },
        .enum_ => |enum_| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print(":{s}:\n", .{enum_.variant});
            format_expr(enum_.payload.*, writer, indentation + 2);
        },
        .switch_ => |switch_| {
            format_expr(switch_.condition.*, writer, indentation);
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("%\n", .{});
            for (switch_.cases) |case| {
                for (0..indentation + 1) |_| try writer.print("  ", .{});
                try writer.print("case {s}", .{case.variant});
                if (case.payload) |payload| try writer.print("({s})", .{payload});
                try writer.print("\n", .{});
                format_expr(case.body, writer, indentation + 2);
            }
        },
        .lambda => |lambda| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("|", .{});
            for (0.., lambda.params) |i, param| {
                if (i > 0) try writer.print(" ", .{});
                try writer.print("{s}", .{param});
            }
            try writer.print("|\n", .{});
            format_expr(lambda.body.*, writer, indentation + 1);
        },
        .call => |call| {
            format_expr(call.callee.*, writer, indentation);
            for (0..indentation + 1) |_| try writer.print("  ", .{});
            try writer.print("(\n", .{});
            for (call.args) |arg|
                format_expr(arg, writer, indentation + 2);
            for (0..indentation + 1) |_| try writer.print("  ", .{});
            try writer.print(")\n", .{});
        },
        .var_ => |var_| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("{s} =\n", .{var_.name});
            format_expr(var_.value.*, writer, indentation + 1);
        },
    }
}
