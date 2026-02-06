// This file contains the Olive compiler, turning source code into heap objects.

const std = @import("std");
const Writer = std.io.Writer;
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringMap = std.StringHashMap;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Instruction = @import("instruction.zig").Instruction;
const Val = @import("pear_value.zig");
const new_empty = Val.new_empty;
const new_symbol = Val.new_symbol;
const get_symbol = Val.get_symbol;
const Vm = @import("vm.zig");

const Str = []const u8;

fn box(ally: Ally, value: anytype) !*@TypeOf(value) {
    const ptr = try ally.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

// The abstract syntax tree represents the syntax of the program, omitting stuff
// that's irrelevant for the semantics such as comments and whitespace.

const ProtoAst = union(enum) { name: Str, int: i64, string: Str, group: []const ProtoAst };

const Ast = struct {
    defs: []const Def,

    pub const Def = struct { name: Str, value: Expr };
    pub const Expr = union(enum) {
        name: Str,
        let: Let,
        int: i64,
        string: Str,
        new: New,
        if_: If,
        function: Function,
        call: Call,
    };
    pub const Let = struct { name: Str, def: *const Expr, expr: *const Expr };
    pub const New = struct { children: []const Expr };
    pub const If = struct { condition: *const Expr, then: *const Expr, else_: *const Expr };
    pub const Function = struct { params: []Str, body: *const Expr };
    pub const Call = struct { callee: *const Expr, args: []const Expr };

    pub fn format(ast: Ast, writer: *Writer) !void {
        for (ast.defs) |def| {
            try writer.print("{s} ", .{def.name});
            try format_expr(def.value, writer);
            try writer.print("\n", .{});
        }
    }
    pub fn format_expr(expr: Expr, writer: *Writer) !void {
        switch (expr) {
            .name => |name| try writer.print("{s}", .{name}),
            .let => |let| {
                try writer.print("(: {s}\n", .{let.name});
                try format_expr(let.def.*, writer);
                try writer.print(" ", .{});
                try format_expr(let.expr.*, writer);
                try writer.print(")", .{});
            },
            .int => |int| try writer.print("{}", .{int}),
            .string => |string| try writer.print("\"{s}\"", .{string}),
            .new => |new| {
                try writer.print("(#", .{});
                for (new.children) |child| {
                    try writer.print(" ", .{});
                    try format_expr(child, writer);
                }
                try writer.print(")", .{});
            },
            .if_ => |if_| {
                try writer.print("(if ", .{});
                try format_expr(if_.condition.*, writer);
                try writer.print(" ", .{});
                try format_expr(if_.then.*, writer);
                try writer.print(" ", .{});
                try format_expr(if_.else_.*, writer);
                try writer.print(")", .{});
            },
            .function => |function| {
                try writer.print("(\\ (", .{});
                for (0.., function.params) |i, param| {
                    if (i > 0) try writer.print(" ", .{});
                    try writer.print("{s}", .{param});
                }
                try writer.print(") ", .{});
                try format_expr(function.body.*, writer);
                try writer.print(")", .{});
            },
            .call => |call| {
                try writer.print("(", .{});
                try format_expr(call.callee.*, writer);
                for (call.args) |arg| {
                    try writer.print(" ", .{});
                    try format_expr(arg, writer);
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
        const raw_value = parser.parse_expr() catch |e| {
            std.debug.print("Error at {}: {}\n", .{ parser.cursor, e });
            @panic("bad input");
        } orelse return error.ExpectedExpr;
        const value = add_semantics(ally, raw_value) catch |e| {
            std.debug.print("Error at {}: {}\n", .{ parser.cursor, e });
            @panic("bad input");
        };
        try defs.append(ally, .{ .name = name, .value = value });
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

    fn parse_group(parser: *Parser) !?[]const ProtoAst {
        if (!parser.consume("(")) return null;
        var parts = ArrayList(ProtoAst).empty;
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
    }!?ProtoAst {
        if (parser.parse_int()) |int| return .{ .int = int };
        if (try parser.parse_string()) |string| return .{ .string = string };
        if (parser.parse_name()) |name| return .{ .name = name };
        if (try parser.parse_group()) |exprs| return .{ .group = exprs };
        return null;
    }
};
fn add_semantics(ally: Ally, ast: ProtoAst) !Ast.Expr {
    switch (ast) {
        .name => |n| return .{ .name = n },
        .int => |i| return .{ .int = i },
        .string => |s| return .{ .string = s },
        .group => |group| {
            if (group.len == 0) return error.ExpectedExpr;
            switch (group[0]) {
                .name => |name| {
                    if (std.mem.eql(u8, name, ":")) {
                        if (group.len % 2 == 1) return error.BadLet;
                        var expr = try add_semantics(ally, group[group.len - 1]);
                        for (0..group.len / 2 - 1) |i| {
                            const let_name = switch (group[group.len - 2 * i - 3]) {
                                .name => |n| n,
                                else => return error.BadLet,
                            };
                            const value = try add_semantics(ally, group[group.len - 2 * i - 2]);
                            const new = Ast.Expr{ .let = .{
                                .name = let_name,
                                .def = try box(ally, value),
                                .expr = try box(ally, expr),
                            } };
                            expr = new;
                        }
                        return expr;
                    }
                    if (std.mem.eql(u8, name, "#")) {
                        var children = try ally.alloc(Ast.Expr, group.len - 1);
                        for (group[1..], 0..) |item, i| children[i] = try add_semantics(ally, item);
                        return .{ .new = .{ .children = children } };
                    }
                    if (std.mem.eql(u8, name, "if")) {
                        if (group.len != 4) return error.BadIf;
                        const condition = try add_semantics(ally, group[1]);
                        const then = try add_semantics(ally, group[2]);
                        const else_ = try add_semantics(ally, group[3]);
                        return .{ .if_ = .{
                            .condition = try box(ally, condition),
                            .then = try box(ally, then),
                            .else_ = try box(ally, else_),
                        } };
                    }
                    if (std.mem.eql(u8, name, "\\")) {
                        if (group.len != 3) return error.BadFunction;
                        var params = ArrayList(Str).empty;
                        for (switch (group[1]) {
                            .group => |g| g,
                            else => return error.BadFunction,
                        }) |param| {
                            switch (param) {
                                .name => |n| try params.append(ally, n),
                                else => return error.BadFunction,
                            }
                        }
                        return .{ .function = .{
                            .params = params.items,
                            .body = try box(ally, try add_semantics(ally, group[2])),
                        } };
                    }
                },
                else => {},
            }
            const callee = try add_semantics(ally, group[0]);
            var args = ArrayList(Ast.Expr).empty;
            for (group[1..]) |arg| try args.append(ally, try add_semantics(ally, arg));
            return .{ .call = .{ .callee = try box(ally, callee), .args = args.items } };
        },
    }
}

pub fn function_to_object(ally: Ally, heap: *Heap, params: []const Str, body: Ast.Expr, defs: *StringMap(Obj)) !Obj {
    var vars = ArrayList(Var).empty;
    for (params, 0..) |param, i| try vars.append(ally, .{ .name = param, .offset = i });
    var instructions = ArrayList(Instruction).empty;
    try ast_to_instructions(ally, heap, body, &instructions, defs, &vars, params.len);
    return try Instruction.new_instructions(ally, heap, instructions.items);
}
const Var = struct { name: Str, offset: usize };
fn ast_to_instructions(
    ally: Ally,
    heap: *Heap,
    expr: Ast.Expr,
    instructions: *ArrayList(Instruction),
    defs: *StringMap(Obj),
    vars: *ArrayList(Var),
    stack_size: usize,
) error{ OutOfMemory, BadProgram }!void {
    switch (expr) {
        .name => |name| {
            for (0..vars.items.len) |i| {
                const entry = &vars.items[vars.items.len - 1 - i];
                if (std.mem.eql(u8, entry.name, name)) {
                    try instructions.append(ally, .{ .stack = stack_size - entry.offset - 1 });
                    return;
                }
            }
            const object = defs.get(name) orelse {
                std.debug.print("Name {s} not in scope.", .{name});
                return error.BadProgram;
            };
            try instructions.append(ally, .{ .address = object });
        },
        .let => |let| {
            try ast_to_instructions(ally, heap, let.def.*, instructions, defs, vars, stack_size);
            try vars.append(ally, .{ .name = let.name, .offset = stack_size });
            try ast_to_instructions(ally, heap, let.expr.*, instructions, defs, vars, stack_size + 1);
            try instructions.append(ally, .{ .popover = 1 });
            _ = vars.pop() orelse unreachable;
        },
        .int => |int| try instructions.append(ally, .{ .word = @bitCast(int) }),
        .string => |string| {
            const obj = try new_symbol(heap, string);
            try instructions.append(ally, .{ .address = obj });
        },
        .new => |new| {
            for (new.children, 0..) |child, i| try ast_to_instructions(ally, heap, child, instructions, defs, vars, stack_size + i);
            try instructions.append(ally, .{ .new = .{ .has_pointers = true, .num_words = new.children.len } });
        },
        .if_ => |if_| {
            try ast_to_instructions(ally, heap, if_.condition.*, instructions, defs, vars, stack_size);
            var then_instructions = ArrayList(Instruction).empty;
            try ast_to_instructions(ally, heap, if_.then.*, &then_instructions, defs, vars, stack_size);
            var else_instructions = ArrayList(Instruction).empty;
            try ast_to_instructions(ally, heap, if_.else_.*, &else_instructions, defs, vars, stack_size);
            try instructions.append(ally, .{ .@"if" = .{
                .then = then_instructions.items,
                .else_ = else_instructions.items,
            } });
        },
        .function => |function| {
            const obj = try function_to_object(ally, heap, function.params, function.body.*, defs);
            try instructions.append(ally, .{ .address = obj });
        },
        .call => |call| {
            for (call.args, 0..) |arg, i| try ast_to_instructions(ally, heap, arg, instructions, defs, vars, stack_size + i);
            try ast_to_instructions(ally, heap, call.callee.*, instructions, defs, vars, stack_size + call.args.len);
            try instructions.append(ally, .eval);
        },
    }
}

pub fn eval(ally: Ally, vm: *Vm, code: Str) !StringMap(Obj) {
    const ast = try str_to_ast(ally, code);
    std.debug.print("AST:\n{f}", .{ast});
    var defs = StringMap(Obj).init(ally);
    for (ast.defs) |def| {
        const instructions = try function_to_object(ally, vm.get_heap(), &.{}, def.value, &defs);
        try vm.run(instructions);
        const result = Obj{ .address = vm.pop() };
        try defs.put(def.name, result);
    }
    std.debug.print("Defs: {any}", .{defs});
    @panic("todo");
}
