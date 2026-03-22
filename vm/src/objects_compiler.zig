const std = @import("std");
const Writer = std.io.Writer;
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringMap = std.StringHashMap;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Val = @import("pear_value.zig");
const new_symbol = Heap.new_symbol;
const get_symbol = Heap.get_symbol;
const Vm = @import("vm.zig");

const Str = []const u8;

fn box(ally: Ally, value: anytype) !*@TypeOf(value) {
    const ptr = try ally.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

const Ast = struct {
    defs: []const Def,

    pub const Def = struct { name: Str, expr: Expr };
    pub const Expr = union(enum) { name: Str, int: i64, string: Str, group: []const Expr };

    pub fn format(ast: Ast, writer: *Writer) !void {
        for (ast.defs) |def| {
            try writer.print("{s} ", .{def.name});
            try format_expr(def.expr, writer);
            try writer.print("\n", .{});
        }
    }
    pub fn format_expr(expr: Expr, writer: *Writer) !void {
        switch (expr) {
            .name => |name| try writer.print("{s}", .{name}),
            .int => |int| try writer.print("{}", .{int}),
            .string => |string| try writer.print("\"{s}\"", .{string}),
            .group => |children| {
                try writer.print("(", .{});
                for (children, 0..) |child, i| {
                    if (i > 0) try writer.print(" ", .{});
                    try format_expr(child, writer);
                }
                try writer.print(")", .{});
            },
        }
    }
};

pub fn str_to_ast(ally: Ally, input: Str) !Ast {
    var parser = Parser{ .ally = ally, .input = input, .cursor = 0 };
    var defs = std.ArrayList(Ast.Def).empty;
    while (true) {
        parser.consume_whitespace();
        if (parser.is_at_end()) break;
        const name = parser.parse_name() orelse {
            std.debug.print("Error at {}: Expected name.\n", .{parser.cursor});
            @panic("bad input");
        };
        const expr = parser.parse_expr() catch |e| {
            std.debug.print("Error at {}: {}\n", .{ parser.cursor, e });
            @panic("bad input");
        } orelse return error.ExpectedExpr;
        try defs.append(ally, .{ .name = name, .expr = expr });
    }
    return .{ .defs = defs.items };
}
const Parser = struct {
    ally: Ally,
    input: Str,
    cursor: usize,

    fn current(parser: Parser) u8 {
        return if (parser.is_at_end()) 0 else parser.input[parser.cursor];
    }
    fn advance(parser: *Parser) void {
        parser.cursor += 1;
    }
    fn is_at_end(parser: Parser) bool {
        return parser.cursor >= parser.input.len;
    }
    fn consume_whitespace(parser: *Parser) void {
        while (true) : (parser.advance()) {
            if (parser.current() == ' ') continue;
            if (parser.current() == '\n') continue;
            break;
        }
    }
    fn consume_char(parser: *Parser) u8 {
        const char = parser.current();
        parser.advance();
        return char;
    }
    fn consume(parser: *Parser, prefix: Str) bool {
        parser.consume_whitespace();
        const start = parser.cursor;
        for (prefix) |char| {
            if (parser.current() != char) {
                parser.cursor = start;
                return false;
            }
            parser.advance();
        }
        return true;
    }

    fn parse_name(parser: *Parser) ?Str {
        parser.consume_whitespace();
        const start = parser.cursor;
        while (true) {
            const char = parser.current();
            const is_letter = char != ' ' and char != '\n' and char != '(' and char != ')';
            if (is_letter) parser.advance() else break;
        }
        const end = parser.cursor;
        if (start == end) return null;
        return parser.input[start..end];
    }

    fn parse_int(parser: *Parser) ?i64 {
        parser.consume_whitespace();
        const start = parser.cursor;
        var num: i64 = 0;
        while (true) {
            const char = parser.current();
            const digit = std.mem.indexOfScalar(u8, "0123456789", char) orelse break;
            num = num * 10 + @as(i64, @intCast(digit));
            parser.advance();
        }
        const end = parser.cursor;
        if (start == end) return null;
        return num;
    }

    fn parse_string(parser: *Parser) !?Str {
        if (!parser.consume("\"")) return null;
        var b = ArrayList(u8).empty;
        while (true) {
            if (parser.is_at_end()) return error.StringDoesNotEnd;
            if (parser.current() == '\"') {
                parser.advance();
                break;
            }
            if (try parser.parse_escaped()) |escaped|
                try b.appendSlice(parser.ally, escaped)
            else
                try b.append(parser.ally, parser.consume_char());
        }
        return b.items;
    }
    fn parse_escaped(parser: *Parser) !?Str {
        if (parser.current() != '\\') return null;
        parser.advance();
        if (parser.is_at_end()) return error.NoEscapeSequence;
        const char = parser.consume_char();
        if (char == '\\') return "\\";
        if (char == '"') return "\"";
        if (char == 'n') return "\n";
        return error.UnknownEscapeSequence;
    }

    fn parse_group(parser: *Parser) !?[]const Ast.Expr {
        if (!parser.consume("(")) return null;
        var parts = ArrayList(Ast.Expr).empty;
        while (try parser.parse_expr()) |expr| try parts.append(parser.ally, expr);
        if (!parser.consume(")")) return error.ExpectedClosingBracket;
        return parts.items;
    }

    fn parse_expr(parser: *Parser) error{
        OutOfMemory,
        StringDoesNotEnd,
        NoEscapeSequence,
        UnknownEscapeSequence,
        ExpectedExpr,
        ExpectedClosingBracket,
    }!?Ast.Expr {
        if (parser.parse_int()) |int| return .{ .int = int };
        if (try parser.parse_string()) |string| return .{ .string = string };
        if (parser.parse_name()) |name| return .{ .name = name };
        if (try parser.parse_group()) |exprs| return .{ .group = exprs };
        return null;
    }
};

fn ast_to_object(ally: Ally, heap: *Heap, expr: Ast.Expr, defs: StringMap(Obj)) error{ OutOfMemory, Bad }!Obj {
    return obj: switch (expr) {
        .name => |name| defs.get(name) orelse {
            std.debug.print("Name {s} not in scope.", .{name});
            return error.Bad;
        },
        .int => |int| try heap.new_leaf(&.{@bitCast(int)}),
        .string => |string| try new_symbol(heap, string),
        .group => |children| {
            const objs = try ally.alloc(Obj, children.len);
            for (children, 0..) |child, i| objs[i] = try ast_to_object(ally, heap, child, defs);
            break :obj try heap.new_inner(objs);
        },
    };
}
pub fn create(ally: Ally, heap: *Heap, code: Str) !Obj {
    const ast = try str_to_ast(ally, code);
    var defs = StringMap(Obj).init(ally);
    for (ast.defs) |def| {
        // std.debug.print("Running {s}\n", .{def.name});
        const obj = try ast_to_object(ally, heap, def.expr, defs);
        try defs.put(def.name, obj);
    }
    return defs.get("export").?;
}
