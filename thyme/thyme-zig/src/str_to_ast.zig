// Parsing a string containing source code into an AST.

const std = @import("std");
const ArrayList = std.ArrayList;
const Ally = std.mem.Allocator;

const ast = @import("ast.zig");
const Expr = ast.Expr;
const Field = ast.Field;
const Case = ast.Case;

const Str = []const u8;

pub fn parse(ally: Ally, input: Str) !Expr {
    var parser = Parser{ .ally = ally, .input = input, .cursor = 0 };
    const exprs = parser.parse_exprs() catch |e| {
        std.debug.print("Error at {}: {}\n", .{ parser.cursor, e });
        @panic("bad");
    };
    parser.consume_whitespace();
    if (parser.cursor < parser.input.len) {
        std.debug.print("Unparsed input at {}.\n", .{parser.cursor});
        @panic("bad");
    }
    return .{ .body = exprs };
}

const Parser = struct {
    ally: Ally,
    input: Str,
    cursor: usize,

    fn current(parser: Parser) u8 {
        return if (parser.is_at_end())
            0
        else
            parser.input[parser.cursor];
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
            if (parser.current() == '#') {
                while (!parser.is_at_end() and parser.current() != '\n') parser.advance();
                continue;
            }
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
            const is_letter = std.mem.containsAtLeastScalar(
                u8,
                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_+-*/<>!=",
                1,
                char,
            );
            const is_digit = std.mem.containsAtLeastScalar(
                u8,
                "0123456789",
                1,
                char,
            );
            if (is_letter or (parser.cursor > start and is_digit))
                parser.advance()
            else
                break;
        }
        const end = parser.cursor;
        if (start == end) return null;
        const name = parser.input[start..end];
        if (std.mem.eql(u8, name, "->") or std.mem.eql(u8, name, "=")) {
            parser.cursor = start;
            return null;
        }
        return name;
    }

    fn parse_int(parser: *Parser) ?i64 {
        parser.consume_whitespace();
        const start = parser.cursor;
        var num: i64 = 0;
        while (true) {
            if (parser.current() == '_') {
                parser.advance();
                continue;
            }
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
            if (parser.is_at_end())
                return error.StringDoesNotEnd;
            if (parser.consume("\"")) break;
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

    fn parse_struct(parser: *Parser) !?Expr {
        if (!parser.consume("[")) return null;
        var fields = ArrayList(Field).empty;
        while (parser.parse_name()) |name| {
            const value: Expr =
                if (parser.consume(":"))
                    try parser.parse_expr() orelse return error.ExpectedValue
                else
                    .nil;

            try fields.append(parser.ally, .{ .name = name, .value = value });
        }
        if (!parser.consume("]")) return error.ExpectedClosingBracket;

        return .{ .struct_ = fields.items };
    }

    fn parse_lambda(parser: *Parser) !?Expr {
        if (!parser.consume("|")) return null;
        var params = ArrayList(Str).empty;
        while (true) {
            try params.append(parser.ally, parser.parse_name() orelse break);
            if (!parser.consume(",")) break;
        }
        if (!parser.consume("|")) return error.ExpectedClosingPipe;
        const body = try parser.parse_expr() orelse return error.ExpectedLambdaBody;
        const boxed_body = try parser.ally.create(Expr);
        boxed_body.* = body;
        return .{ .lambda = .{ .params = params.items, .body = boxed_body } };
    }

    fn parse_expr(parser: *Parser) error{
        OutOfMemory,
        StringDoesNotEnd,
        NoEscapeSequence,
        UnknownEscapeSequence,
        ExpectedArrow,
        ExpectedFieldName,
        ExpectedValue,
        ExpectedVariant,
        ExpectedPayload,
        ExpectedLambdaBody,
        ExpectedOpeningBrace,
        ExpectedClosingPipe,
        ExpectedClosingBrace,
        ExpectedClosingParen,
        ExpectedClosingBracket,
        ExpectedCondition,
        ExpectedCaseVariant,
        ExpectedCasePayload,
        ExpectedCaseBody,
        ExpectedVarValue,
        VarNeedsName,
    }!?Expr {
        var expr: Expr =
            if (parser.parse_int()) |int|
                .{ .int = int }
            else if (try parser.parse_string()) |string|
                .{ .string = string }
            else if (try parser.parse_struct()) |struct_|
                struct_
            else if (try parser.parse_lambda()) |lambda|
                lambda
            else if (try parser.parse_block()) |expr|
                expr
            else if (parser.parse_name()) |name|
                .{ .name = name }
            else
                return null;

        while (true) {
            if (parser.consume(".")) {
                const name = parser.parse_name() orelse return error.ExpectedFieldName;
                const boxed_of = try parser.ally.create(Expr);
                boxed_of.* = expr;
                expr = .{ .member = .{ .of = boxed_of, .name = name } };
            } else if (parser.consume("(")) {
                var args = ArrayList(Expr).empty;
                while (try parser.parse_expr()) |arg| {
                    try args.append(parser.ally, arg);
                    _ = parser.consume(",");
                }
                if (!parser.consume(")")) return error.ExpectedClosingParen;
                const boxed_callee = try parser.ally.create(Expr);
                boxed_callee.* = expr;
                expr = .{ .call = .{ .callee = boxed_callee, .args = args.items } };
            } else if (parser.consume("=")) {
                const name = switch (expr) {
                    .name => |n| n,
                    else => return error.VarNeedsName,
                };
                const right = try parser.parse_expr() orelse return error.ExpectedVarValue;
                const boxed_right = try parser.ally.create(Expr);
                boxed_right.* = right;
                expr = .{ .var_ = .{ .name = name, .value = boxed_right } };
            } else if (parser.consume("%")) {
                if (!parser.consume("{")) return error.ExpectedOpeningBrace;
                var cases = ArrayList(Case).empty;
                while (parser.parse_name()) |variant| {
                    const payload = payload: {
                        if (parser.consume(":")) {
                            const name = parser.parse_name() orelse return error.ExpectedCasePayload;
                            break :payload name;
                        }
                        break :payload null;
                    };
                    if (!parser.consume("->")) return error.ExpectedArrow;
                    const body = try parser.parse_expr() orelse return error.ExpectedCaseBody;
                    try cases.append(parser.ally, .{
                        .variant = variant,
                        .payload = payload,
                        .body = body,
                    });
                }
                if (!parser.consume("}")) return error.ExpectedClosingBrace;

                const boxed_condition = try parser.ally.create(Expr);
                boxed_condition.* = expr;
                return .{ .switch_ = .{ .condition = boxed_condition, .cases = cases.items } };
            } else break;
        }

        return expr;
    }

    fn parse_block(parser: *Parser) !?Expr {
        if (!parser.consume("{")) return null;
        const exprs = try parser.parse_exprs();
        if (!parser.consume("}")) return error.ExpectedClosingBrace;
        return .{ .body = exprs };
    }

    fn parse_exprs(parser: *Parser) ![]Expr {
        var exprs = ArrayList(Expr).empty;
        while (true) try exprs.append(parser.ally, try parser.parse_expr() orelse break);
        return exprs.items;
    }
};
