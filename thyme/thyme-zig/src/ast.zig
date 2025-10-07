const std = @import("std");
const ArrayList = std.ArrayList;

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

pub fn dump(expr: Expr, indentation: usize) void {
    switch (expr) {
        .name => |name| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("{s}\n", .{name});
        },
        .body => |body| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("{{\n", .{});
            for (body) |child| dump(child, indentation + 1);
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("}}\n", .{});
        },
        .int => |int| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("{}\n", .{int});
        },
        .string => |string| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("\"{s}\"\n", .{string});
        },
        .struct_ => |struct_| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("&\n", .{});
            for (struct_) |field| {
                for (0..indentation + 1) |_| std.debug.print("  ", .{});
                std.debug.print("{s}:\n", .{field.name});
                dump(field.value, indentation + 2);
            }
        },
        .member => |member| {
            dump(member.of.*, indentation);
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print(".{s}\n", .{member.name});
        },
        .enum_ => |enum_| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print(":{s}:\n", .{enum_.variant});
            dump(enum_.payload.*, indentation + 2);
        },
        .switch_ => |switch_| {
            dump(switch_.condition.*, indentation);
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("%\n", .{});
            for (switch_.cases) |case| {
                for (0..indentation + 1) |_| std.debug.print("  ", .{});
                std.debug.print("case {s}", .{case.variant});
                if (case.payload) |payload| std.debug.print("({s})", .{payload});
                std.debug.print("\n", .{});
                dump(case.body, indentation + 2);
            }
        },
        .lambda => |lambda| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("|", .{});
            for (0.., lambda.params) |i, param| {
                if (i > 0) std.debug.print(" ", .{});
                std.debug.print("{s}", .{param});
            }
            std.debug.print("|\n", .{});
            dump(lambda.body.*, indentation + 1);
        },
        .call => |call| {
            dump(call.callee.*, indentation);
            for (0..indentation + 1) |_| std.debug.print("  ", .{});
            std.debug.print("(\n", .{});
            for (call.args) |arg| {
                dump(arg, indentation + 2);
            }
            for (0..indentation + 1) |_| std.debug.print("  ", .{});
            std.debug.print(")\n", .{});
        },
        .var_ => |var_| {
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("{s} =\n", .{var_.name});
            dump(var_.value.*, indentation + 1);
        },
    }
}
