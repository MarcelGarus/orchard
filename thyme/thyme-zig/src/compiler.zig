// Parsing a string containing source code into an AST.

const std = @import("std");
const Writer = std.io.Writer;
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Instruction = @import("instruction.zig").Instruction;
const Object = @import("object.zig");
const Vm = @import("vm.zig");

const Str = []const u8;

const Ast = struct {
    expr: Expr,

    pub const Expr = union(enum) {
        nil,
        name: Str,
        body: []Expr,
        int: i64,
        string: Str,
        struct_: []Field,
        member: Member,
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

    pub fn format(expr: Expr, writer: *Writer) !void {
        try format_expr(expr, writer, 0);
    }
    pub fn format_expr(expr: Expr, writer: *Writer, indentation: usize) !void {
        switch (expr) {
            .nil => {
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("nil\n", .{});
            },
            .name => |name| {
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("{s}\n", .{name});
            },
            .body => |body| {
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("{{\n", .{});
                for (body) |child| try format_expr(child, writer, indentation + 1);
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
                    try format_expr(field.value, writer, indentation + 2);
                }
            },
            .member => |member| {
                try format_expr(member.of.*, writer, indentation);
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print(".{s}\n", .{member.name});
            },
            .switch_ => |switch_| {
                try format_expr(switch_.condition.*, writer, indentation);
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("%\n", .{});
                for (switch_.cases) |case| {
                    for (0..indentation + 1) |_| try writer.print("  ", .{});
                    try writer.print("case {s}", .{case.variant});
                    if (case.payload) |payload| try writer.print("({s})", .{payload});
                    try writer.print("\n", .{});
                    try format_expr(case.body, writer, indentation + 2);
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
                try format_expr(lambda.body.*, writer, indentation + 1);
            },
            .call => |call| {
                try format_expr(call.callee.*, writer, indentation);
                for (0..indentation + 1) |_| try writer.print("  ", .{});
                try writer.print("(\n", .{});
                for (call.args) |arg|
                    try format_expr(arg, writer, indentation + 2);
                for (0..indentation + 1) |_| try writer.print("  ", .{});
                try writer.print(")\n", .{});
            },
            .var_ => |var_| {
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("{s} =\n", .{var_.name});
                try format_expr(var_.value.*, writer, indentation + 1);
            },
        }
    }
};

pub fn str_to_ast(ally: Ally, input: Str) !Ast.Expr {
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

    fn parse_struct(parser: *Parser) !?Ast.Expr {
        if (!parser.consume("[")) return null;
        var fields = ArrayList(Ast.Field).empty;
        while (parser.parse_name()) |name| {
            const value: Ast.Expr =
                if (parser.consume(":"))
                    try parser.parse_expr() orelse return error.ExpectedValue
                else
                    .nil;

            try fields.append(parser.ally, .{ .name = name, .value = value });
        }
        if (!parser.consume("]")) return error.ExpectedClosingBracket;

        return .{ .struct_ = fields.items };
    }

    fn parse_lambda(parser: *Parser) !?Ast.Expr {
        if (!parser.consume("|")) return null;
        var params = ArrayList(Str).empty;
        while (true) {
            try params.append(parser.ally, parser.parse_name() orelse break);
            if (!parser.consume(",")) break;
        }
        if (!parser.consume("|")) return error.ExpectedClosingPipe;
        const body = try parser.parse_expr() orelse return error.ExpectedLambdaBody;
        const boxed_body = try parser.ally.create(Ast.Expr);
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
    }!?Ast.Expr {
        var expr: Ast.Expr =
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
            if (parser.consume(":")) {
                const name = parser.parse_name() orelse return error.ExpectedFieldName;
                const boxed_of = try parser.ally.create(Ast.Expr);
                boxed_of.* = expr;
                expr = .{ .member = .{ .of = boxed_of, .name = name } };
            } else if (parser.consume("(")) {
                var args = ArrayList(Ast.Expr).empty;
                while (try parser.parse_expr()) |arg| {
                    try args.append(parser.ally, arg);
                    _ = parser.consume(",");
                }
                if (!parser.consume(")")) return error.ExpectedClosingParen;
                const boxed_callee = try parser.ally.create(Ast.Expr);
                boxed_callee.* = expr;
                expr = .{ .call = .{ .callee = boxed_callee, .args = args.items } };
            } else if (parser.consume("= ") or parser.consume("=\n")) {
                const name = switch (expr) {
                    .name => |n| n,
                    else => return error.VarNeedsName,
                };
                const right = try parser.parse_expr() orelse return error.ExpectedVarValue;
                const boxed_right = try parser.ally.create(Ast.Expr);
                boxed_right.* = right;
                expr = .{ .var_ = .{ .name = name, .value = boxed_right } };
            } else if (parser.consume("%")) {
                if (!parser.consume("{")) return error.ExpectedOpeningBrace;
                var cases = ArrayList(Ast.Case).empty;
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

                const boxed_condition = try parser.ally.create(Ast.Expr);
                boxed_condition.* = expr;
                return .{ .switch_ = .{ .condition = boxed_condition, .cases = cases.items } };
            } else break;
        }

        return expr;
    }

    fn parse_block(parser: *Parser) !?Ast.Expr {
        if (!parser.consume("{")) return null;
        const exprs = try parser.parse_exprs();
        if (!parser.consume("}")) return error.ExpectedClosingBrace;
        return .{ .body = exprs };
    }

    fn parse_exprs(parser: *Parser) ![]Ast.Expr {
        var exprs = ArrayList(Ast.Expr).empty;
        while (true) try exprs.append(parser.ally, try parser.parse_expr() orelse break);
        return exprs.items;
    }
};

pub const Ir = struct {
    nodes: []Node,
    params: []Id,
    body: Body,

    pub const Id = struct {
        index: usize,

        pub fn format(id: Id, writer: *Writer) !void {
            try writer.print("%{}", .{id.index});
        }
    };
    pub const Body = struct { ids: []const Id, returns: Id };
    pub const Node = union(enum) {
        param,
        word: Word,
        object: Object,
        new: New,
        tag: Id,
        num_pointers: Id,
        num_literals: Id,
        load: Load,
        add: Args,
        subtract: Args,
        multiply: Args,
        divide: Args,
        modulo: Args,
        compare: Args, // equal: 0, greater: 1, less: 2
        call: Call,
        if_not_zero: If,
        crash: Id,
    };
    pub const New = struct { tag: u8, pointers: []const Id, literals: []const Id };
    pub const Load = struct { base: Id, offset: Id };
    pub const Args = struct { left: Id, right: Id };
    pub const Call = struct { fun: Id, args: []const Id };
    pub const If = struct { condition: Id, then: Body, else_: Body };

    pub fn get(ir: Ir, id: Id) Node {
        return ir.nodes[id.index];
    }

    pub fn format(ir: Ir, writer: *Writer) !void {
        try ir.format_indented(writer, 0);
    }
    fn format_indented(ir: Ir, writer: *Writer, indentation: usize) !void {
        try writer.print("code", .{});
        for (ir.params) |param| try writer.print(" {f}", .{param});
        try writer.print("\n", .{});
        try format_body(ir.body, ir, writer, indentation + 1);
    }
    fn format_body(body: Body, ir: Ir, writer: *Writer, indentation: usize) !void {
        for (body.ids) |id| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("{f} = ", .{id});
            try format_node(ir.get(id), ir, writer, indentation);
        }
        for (0..indentation) |_| try writer.print("  ", .{});
        try writer.print("{f}\n", .{body.returns});
    }
    fn format_node(node: Node, ir: Ir, writer: *Writer, indentation: usize) error{WriteFailed}!void {
        switch (node) {
            .param => try writer.print("param\n", .{}),
            .word => |word| try writer.print("word {}\n", .{word}),
            .object => |object| try writer.print("object *{x}\n", .{object.address.address}),
            .new => |new| {
                try writer.print("new [{x}]", .{new.tag});
                for (new.pointers) |pointer| try writer.print(" {f}", .{pointer});
                for (new.literals) |literal| try writer.print(" {f}", .{literal});
                try writer.print("\n", .{});
            },
            .tag => |address| try writer.print("tag {f}\n", .{address}),
            .num_pointers => |address| try writer.print("num_pointers {f}\n", .{address}),
            .num_literals => |address| try writer.print("num_literals {f}\n", .{address}),
            .load => |load| try writer.print("load {f} {f}\n", .{ load.base, load.offset }),
            .add => |args| try writer.print("add {f} {f}\n", .{ args.left, args.right }),
            .subtract => |args| try writer.print("subtract {f} {f}\n", .{ args.left, args.right }),
            .multiply => |args| try writer.print("multiply {f} {f}\n", .{ args.left, args.right }),
            .divide => |args| try writer.print("divide {f} {f}\n", .{ args.left, args.right }),
            .modulo => |args| try writer.print("modulo {f} {f}\n", .{ args.left, args.right }),
            .compare => |args| try writer.print("compare {f} {f}\n", .{ args.left, args.right }),
            .call => |call| {
                try writer.print("call {f} with", .{call.fun});
                for (call.args) |arg| try writer.print(" {f}", .{arg});
                try writer.print("\n", .{});
            },
            .if_not_zero => |if_| {
                try writer.print("if {f} != 0 then\n", .{if_.condition});
                try format_body(if_.then, ir, writer, indentation + 1);
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("else\n", .{});
                try format_body(if_.else_, ir, writer, indentation + 1);
            },
            .crash => |message| try writer.print("crash {f}\n", .{message}),
        }
    }

    pub const Builder = struct {
        ally: Ally,
        nodes: ArrayList(Node),
        params: ArrayList(Id),

        pub fn init(ally: Ally) Builder {
            return .{
                .ally = ally,
                .nodes = .empty,
                .params = .empty,
            };
        }

        pub fn body(builder: *Builder) BodyBuilder {
            return .{
                .parent = builder,
                .ids = .empty,
            };
        }

        fn create(builder: *Builder, node: Node) !Id {
            const id = Id{ .index = builder.nodes.items.len };
            try builder.nodes.append(builder.ally, node);
            return id;
        }

        pub fn param(builder: *Builder) !Id {
            const id = try builder.create(.param);
            try builder.params.append(builder.ally, id);
            return id;
        }

        pub fn finish(builder: Builder, bod: Body) Ir {
            return .{
                .nodes = builder.nodes.items,
                .params = builder.params.items,
                .body = bod,
            };
        }
    };
    pub const BodyBuilder = struct {
        parent: *Builder,
        ids: ArrayList(Id),

        fn create_and_push(body: *BodyBuilder, node: Node) !Id {
            const id = try body.parent.create(node);
            try body.ids.append(body.parent.ally, id);
            return id;
        }

        pub fn child_body(body: BodyBuilder) BodyBuilder {
            return .{
                .parent = body.parent,
                .ids = .empty,
            };
        }

        pub fn get(body: BodyBuilder, id: Id) Node {
            return body.parent.nodes[id.index];
        }

        pub fn finish(body: BodyBuilder, returns: Id) Body {
            return .{ .ids = body.ids.items, .returns = returns };
        }

        pub fn word(body: *BodyBuilder, word_: Word) !Id {
            return body.create_and_push(.{ .word = word_ });
        }
        pub fn object(body: *BodyBuilder, obj: Object) !Id {
            return body.create_and_push(.{ .object = obj });
        }
        pub fn new(body: *BodyBuilder, tag_: u8, pointers: []const Id, literals: []const Id) !Id {
            return try body.create_and_push(.{ .new = .{
                .tag = tag_,
                .pointers = pointers,
                .literals = literals,
            } });
        }
        pub fn tag(body: *BodyBuilder, address: Id) !Id {
            return body.create_and_push(.{ .tag = address });
        }
        pub fn num_pointers(body: *BodyBuilder, address: Id) !Id {
            return body.create_and_push(.{ .num_pointers = address });
        }
        pub fn num_literals(body: *BodyBuilder, address: Id) !Id {
            return body.create_and_push(.{ .num_literals = address });
        }
        pub fn load(body: *BodyBuilder, address: Id, offset: Id) !Id {
            return body.create_and_push(.{ .load = .{ .base = address, .offset = offset } });
        }
        pub fn add(body: *BodyBuilder, left: Id, right: Id) !Id {
            return body.create_and_push(.{ .add = .{ .left = left, .right = right } });
        }
        pub fn subtract(body: *BodyBuilder, left: Id, right: Id) !Id {
            return body.create_and_push(.{ .subtract = .{ .left = left, .right = right } });
        }
        pub fn multiply(body: *BodyBuilder, left: Id, right: Id) !Id {
            return body.create_and_push(.{ .multiply = .{ .left = left, .right = right } });
        }
        pub fn divide(body: *BodyBuilder, left: Id, right: Id) !Id {
            return body.create_and_push(.{ .divide = .{ .left = left, .right = right } });
        }
        pub fn modulo(body: *BodyBuilder, left: Id, right: Id) !Id {
            return body.create_and_push(.{ .modulo = .{ .left = left, .right = right } });
        }
        pub fn compare(body: *BodyBuilder, left: Id, right: Id) !Id {
            return body.create_and_push(.{ .compare = .{ .left = left, .right = right } });
        }
        pub fn call(body: *BodyBuilder, fun: Id, args: []const Id) !Id {
            return body.create_and_push(.{ .call = .{ .fun = fun, .args = args } });
        }
        pub fn if_not_zero(body: *BodyBuilder, condition: Id, then: Body, else_: Body) !Id {
            return body.create_and_push(.{ .if_not_zero = .{ .condition = condition, .then = then, .else_ = else_ } });
        }
        pub fn crash(body: *BodyBuilder, message: Id) !Id {
            return body.create_and_push(.{ .crash = message });
        }

        // Higher-level convenience functions

        fn assert_has_tag(body: *BodyBuilder, obj: Id, tag_: u8, message: Object) !void {
            _ = try body.if_not_zero(
                try body.subtract(try body.tag(obj), try body.word(tag_)),
                then: {
                    var child = body.child_body();
                    const msg = try child.object(message);
                    break :then try child.finish_with_crash(msg);
                },
                else_: {
                    var child = body.child_body();
                    break :else_ try child.finish_with_zero();
                },
            );
        }

        pub fn new_int(body: *BodyBuilder, value: Id) !Id {
            const words = try body.parent.ally.alloc(Id, 1);
            words[0] = value;
            return try body.new(Object.TAG_INT, &[_]Id{}, words);
        }
        pub fn assert_is_int(body: *BodyBuilder, obj: Id, message: Object) !void {
            try body.assert_has_tag(obj, Object.TAG_INT, message);
        }
        pub fn get_int_value(body: *BodyBuilder, int: Id) !Id {
            const zero = try body.word(0);
            return try body.load(int, zero);
        }

        pub fn get_symbol_len_in_words(body: *BodyBuilder, symbol: Id) !Id {
            return try body.num_literals(symbol);
        }

        pub fn new_struct(body: *BodyBuilder, keys_and_values: []Id) !Id {
            return try body.new(Object.TAG_STRUCT, keys_and_values, &[_]Id{});
        }
        pub fn assert_is_struct(body: *BodyBuilder, obj: Id, message: Object) !void {
            try body.assert_has_tag(obj, Object.TAG_STRUCT, message);
        }

        pub fn new_closure(body: *BodyBuilder, captured: []Id) !Id {
            return try body.new(Object.TAG_CLOSURE, captured, &[_]Id{});
        }
        pub fn get_closure_var(body: *BodyBuilder, closure: Id, index: i64) !Id {
            return try body.load(closure, body.word(index));
        }

        pub fn new_lambda(body: *BodyBuilder, fun: Id, closure: Id) !Id {
            const pointers = try body.parent.ally.alloc(Id, 2);
            pointers[0] = fun;
            pointers[1] = closure;
            return try body.new(Object.TAG_LAMBDA, pointers, &[_]Id{});
        }
        pub fn assert_is_lambda(body: *BodyBuilder, obj: Id, message: Object) !void {
            try body.assert_has_tag(obj, Object.TAG_LAMBDA, message);
        }
        pub fn get_lambda_fun(body: *BodyBuilder, lambda: Id) !Id {
            const zero = try body.word(0);
            return try body.load(lambda, zero);
        }
        pub fn get_lambda_closure(body: *BodyBuilder, lambda: Id) !Id {
            const one = try body.word(1);
            return try body.load(lambda, one);
        }

        pub fn finish_with_zero(body: *BodyBuilder) !Body {
            const zero = try body.word(0);
            return body.finish(zero);
        }
        pub fn finish_with_crash(body: *BodyBuilder, message: Id) !Body {
            const never = try body.crash(message);
            return body.finish(never);
        }
    };
};

pub fn ast_to_ir(ally: Ally, heap: *Heap, env: anytype, ast: Ast.Expr) !Ir {
    return try ast_to_ir_mod.compile(ally, env, ast, heap);
}

const ast_to_ir_mod = struct {
    const Node = Ir.Node;
    const Id = Ir.Id;
    const Body = Ir.Body;
    const Builder = Ir.Builder;
    const BodyBuilder = Ir.BodyBuilder;

    const Bindings = struct {
        bindings: ArrayList(Binding),

        fn init() Bindings {
            return .{ .bindings = .empty };
        }
        fn bind(bindings: *Bindings, ally: Ally, name: Str, id: Id) !void {
            try bindings.bindings.append(ally, .{ .name = name, .id = id });
        }
        fn get(bindings: Bindings, name: Str) !Id {
            for (bindings.bindings.items) |binding|
                if (std.mem.eql(u8, binding.name, name)) return binding.id;
            std.debug.print("name {s}\n", .{name});
            @panic("name not in scope");
        }
    };
    const Binding = struct { name: Str, id: Id };

    pub fn compile(ally: Ally, env: anytype, expr: Ast.Expr, heap: *Heap) !Ir {
        const common = common: {
            const nil = try Object.new_nil(heap);
            const compare_symbols_rec_fun = try ir_to_fun(ally, heap, ir: {
                var builder = Ir.Builder.init(ally);
                const a = try builder.param(); // a symbol object
                const b = try builder.param(); // a symbol object
                const cursor = try builder.param(); // a literal word
                const rec = try builder.param(); // a reference to this function
                var body = builder.body();
                const len = try body.get_symbol_len_in_words(a);
                const diff = try body.subtract(len, cursor);
                const res = try body.if_not_zero(
                    diff,
                    not_done: {
                        var inner = body.child_body();
                        const a_word = try inner.load(a, cursor);
                        const b_word = try inner.load(b, cursor);
                        const word_diff = try inner.subtract(a_word, b_word);
                        const res = try inner.if_not_zero(
                            word_diff,
                            word_differs: {
                                var innerer = body.child_body();
                                const zero = try innerer.word(0);
                                break :word_differs innerer.finish(zero);
                            },
                            word_same: {
                                var innerer = body.child_body();
                                const one = try innerer.word(1);
                                const next_cursor = try innerer.add(cursor, one);
                                var args = try ally.alloc(Id, 4);
                                args[0] = a;
                                args[1] = b;
                                args[2] = next_cursor;
                                args[3] = rec;
                                const res = try innerer.call(rec, args);
                                break :word_same innerer.finish(res);
                            },
                        );
                        break :not_done inner.finish(res);
                    },
                    done_comparing: {
                        var inner = body.child_body();
                        const one = try inner.word(1);
                        break :done_comparing inner.finish(one);
                    },
                );
                const body_ = body.finish(res);
                break :ir builder.finish(body_);
            });
            const compare_symbols_fun = try ir_to_fun(ally, heap, ir: {
                var builder = Ir.Builder.init(ally);
                const a = try builder.param(); // a symbol object
                const b = try builder.param(); // a symbol object
                var body = builder.body();
                const rec = try body.object(compare_symbols_rec_fun);
                var args = try ally.alloc(Id, 4);
                args[0] = a;
                args[1] = b;
                args[2] = try body.word(0);
                args[3] = rec;
                const res = try body.call(rec, args);
                const body_ = body.finish(res);
                break :ir builder.finish(body_);
            });
            const member_rec_fun = try ir_to_fun(ally, heap, ir: {
                var builder = Ir.Builder.init(ally);
                const struct_ = try builder.param(); // a struct object
                const name = try builder.param(); // a symbol object
                const cursor = try builder.param(); // a literal word
                const rec = try builder.param(); // a reference to this function
                var body = builder.body();
                const num_fields_times_two = try body.num_pointers(struct_);
                const diff = try body.subtract(num_fields_times_two, cursor);
                const res = try body.if_not_zero(
                    diff,
                    not_done: {
                        var inner = body.child_body();
                        const field_name = try inner.load(struct_, cursor);
                        const compare_symbols = try inner.object(compare_symbols_fun);
                        const compare_args = try ally.alloc(Id, 2);
                        compare_args[0] = name;
                        compare_args[1] = field_name;
                        const matches = try inner.call(compare_symbols, compare_args);
                        const res = try inner.if_not_zero(
                            matches,
                            match: {
                                var innerer = body.child_body();
                                const one = try innerer.word(1);
                                const value_index = try innerer.add(cursor, one);
                                const field_value = try innerer.load(struct_, value_index);
                                break :match innerer.finish(field_value);
                            },
                            no_match: {
                                var innerer = body.child_body();
                                const two = try innerer.word(2);
                                const next_cursor = try innerer.add(cursor, two);
                                var rec_args = try ally.alloc(Id, 4);
                                rec_args[0] = struct_;
                                rec_args[1] = name;
                                rec_args[2] = next_cursor;
                                rec_args[3] = rec;
                                const res = try innerer.call(rec, rec_args);
                                break :no_match innerer.finish(res);
                            },
                        );
                        break :not_done inner.finish(res);
                    },
                    field_not_found: {
                        var inner = body.child_body();
                        const message = try inner.object(try Object.new_symbol(heap, "field not found"));
                        break :field_not_found try inner.finish_with_crash(message);
                    },
                );
                const body_ = body.finish(res);
                break :ir builder.finish(body_);
            });
            const member_fun = try ir_to_fun(ally, heap, ir: {
                var builder = Ir.Builder.init(ally);
                const struct_ = try builder.param(); // a struct object
                const name = try builder.param(); // a symbol object
                var body = builder.body();
                const rec = try body.object(member_rec_fun);
                var args = try ally.alloc(Id, 4);
                args[0] = struct_;
                args[1] = name;
                args[2] = try body.word(0);
                args[3] = rec;
                const res = try body.call(rec, args);
                const body_ = body.finish(res);
                break :ir builder.finish(body_);
            });
            break :common Common{
                .nil = nil,
                .compare_symbols_fun = compare_symbols_fun,
                .member_fun = member_fun,
                .member_of_non_struct_symbol = try Object.new_symbol(heap, "member of non-struct"),
                .switch_on_non_struct_symbol = try Object.new_symbol(heap, "switch on non-struct"),
                .call_non_lambda_symbol = try Object.new_symbol(heap, "call non-lambda"),
            };
        };

        var builder = Builder.init(ally);
        var body = builder.body();
        var bindings = Bindings.init();
        inline for (@typeInfo(@TypeOf(env)).@"struct".fields) |field| {
            const ref = try body.object(@field(env, field.name));
            try bindings.bind(ally, field.name, ref);
        }
        const result = try compile_expr(ally, expr, &body, &bindings, heap, common);
        const body_result = body.finish(result);
        return builder.finish(body_result);
    }

    const Common = struct {
        nil: Object,
        compare_symbols_fun: Object,
        member_fun: Object,
        member_of_non_struct_symbol: Object,
        switch_on_non_struct_symbol: Object,
        call_non_lambda_symbol: Object,
    };

    fn compile_expr(
        ally: Ally,
        expr: Ast.Expr,
        body: *Ir.BodyBuilder,
        bindings: *Bindings,
        heap: *Heap,
        common: Common,
    ) error{OutOfMemory}!Id {
        switch (expr) {
            .nil => return try body.object(common.nil),
            .name => |name| return bindings.get(name),
            .body => |bod| return compile_body(ally, bod, body, bindings, heap, common),
            .int => |int| return body.object(try Object.new_int(heap, int)),
            .string => @panic("string"),
            .struct_ => |struct_| {
                var fields = try ally.alloc(Id, struct_.len * 2);
                for (0.., struct_) |i, field| {
                    fields[2 * i] = try body.object(try Object.new_symbol(heap, field.name));
                    fields[2 * i + 1] = try compile_expr(ally, field.value, body, bindings, heap, common);
                }
                return try body.new_struct(fields);
            },
            .member => |member| {
                const of = try compile_expr(ally, member.of.*, body, bindings, heap, common);
                try body.assert_is_struct(of, common.member_of_non_struct_symbol);
                const member_fun = try body.object(common.member_fun);
                const name = try body.object(try Object.new_symbol(heap, member.name));
                const args = try ally.alloc(Id, 2);
                args[0] = of;
                args[1] = name;
                return try body.call(member_fun, args);
            },
            .switch_ => |switch_| {
                const condition = try compile_expr(ally, switch_.condition.*, body, bindings, heap, common);
                try body.assert_is_struct(condition, common.switch_on_non_struct_symbol);
                const num_pointers = try body.num_pointers(condition);
                _ = try body.if_not_zero(
                    num_pointers,
                    then: {
                        var child = body.child_body();
                        break :then try child.finish_with_zero();
                    },
                    else_: {
                        var child = body.child_body();
                        const message = try Object.new_symbol(heap, "no fields");
                        const message_ref = try child.object(message);
                        break :else_ try child.finish_with_crash(message_ref);
                    },
                );

                const zero = try body.word(0);
                const one = try body.word(1);
                const variant = try body.load(condition, zero);
                const payload = try body.load(condition, one);

                var res = child: {
                    var child = body.child_body();
                    const message = try Object.new_symbol(heap, "no switch case matched");
                    const message_ref = try child.object(message);
                    break :child try child.finish_with_crash(message_ref);
                };
                for (0..switch_.cases.len) |i| {
                    const case = switch_.cases[switch_.cases.len - 1 - i];
                    const candidate = try Object.new_symbol(heap, case.variant);
                    var inner = body.child_body();
                    const candidate_ref = try inner.object(candidate);
                    const symbol_compare_ref = try inner.object(common.compare_symbols_fun);
                    const args = try ally.alloc(Id, 2);
                    args[0] = variant;
                    args[1] = candidate_ref;
                    const matches = try inner.call(symbol_compare_ref, args);
                    const check = try inner.if_not_zero(
                        matches,
                        then: {
                            const snapshot = bindings.bindings.items.len;
                            if (case.payload) |payload_name|
                                try bindings.bind(ally, payload_name, payload);
                            var innerer = inner.child_body();
                            const then = try compile_expr(
                                ally,
                                case.body,
                                &innerer,
                                bindings,
                                heap,
                                common,
                            );
                            bindings.bindings.items.len = snapshot;
                            break :then innerer.finish(then);
                        },
                        res,
                    );
                    res = inner.finish(check);
                }
                for (res.ids) |id| try body.ids.append(ally, id);
                return res.returns;
            },
            .lambda => |lambda| {
                const captured = try get_captured(ally, lambda);
                const closure = closure: {
                    var captured_values = ArrayList(Id).empty;
                    for (captured.items) |name|
                        try captured_values.append(ally, try bindings.get(name));
                    break :closure try body.new_closure(captured_values.items);
                };
                const lambda_ir = ir: {
                    var lambda_bindings = Bindings.init();
                    var lambda_builder = Builder.init(ally);
                    for (lambda.params) |param|
                        try lambda_bindings.bind(ally, param, try lambda_builder.param());
                    const closure_param = try lambda_builder.param();
                    var lambda_body = lambda_builder.body();
                    for (0.., captured.items) |i, name| {
                        const offset = try lambda_body.word(@intCast(i));
                        const value = try lambda_body.load(closure_param, offset);
                        try lambda_bindings.bind(ally, name, value);
                    }
                    const result = try compile_expr(
                        ally,
                        lambda.body.*,
                        &lambda_body,
                        &lambda_bindings,
                        heap,
                        common,
                    );
                    const body_result = lambda_body.finish(result);
                    break :ir lambda_builder.finish(body_result);
                };
                const lambda_fun = try body.object(try ir_to_fun(ally, heap, lambda_ir));
                return body.new_lambda(lambda_fun, closure);
            },
            .call => |call| {
                const callee = try compile_expr(ally, call.callee.*, body, bindings, heap, common);
                var args = ArrayList(Id).empty;
                for (call.args) |arg|
                    try args.append(ally, try compile_expr(ally, arg, body, bindings, heap, common));
                try body.assert_is_lambda(callee, common.call_non_lambda_symbol);
                const fun = try body.get_lambda_fun(callee);
                const closure = try body.get_lambda_closure(callee);
                try args.append(ally, closure);

                const num_args = try body.word(args.items.len);
                const two = try body.word(2);
                const num_params = try body.load(fun, two);
                const diff = try body.subtract(num_args, num_params);

                return try body.if_not_zero(
                    diff,
                    mismatched: {
                        var inner = body.child_body();
                        const message = try inner.object(try Object.new_symbol(heap, "wrong number of arguments"));
                        const result = try inner.crash(message);
                        break :mismatched inner.finish(result);
                    },
                    correct: {
                        var inner = body.child_body();
                        const result = try inner.call(fun, args.items);
                        break :correct inner.finish(result);
                    },
                );
            },
            .var_ => |var_| {
                const id = try compile_expr(ally, var_.value.*, body, bindings, heap, common);
                try bindings.bind(ally, var_.name, id);
                return body.object(common.nil);
            },
        }
    }

    fn compile_body(
        ally: Ally,
        exprs: []Ast.Expr,
        body: *BodyBuilder,
        bindings: *Bindings,
        heap: *Heap,
        common: Common,
    ) !Id {
        var last: ?Id = null;
        for (exprs) |expr|
            last = try compile_expr(ally, expr, body, bindings, heap, common);
        return last orelse body.object(common.nil);
    }

    fn get_captured(ally: Ally, lambda: Ast.Lambda) !ArrayList(Str) {
        var ignore = ArrayList(Str).empty;
        var captured = ArrayList(Str).empty;
        for (lambda.params) |param| try ignore.append(ally, param);
        try collect_captured(ally, lambda.body.*, &ignore, &captured);
        return captured;
    }
    fn collect_captured_in_new_scope(
        ally: Ally,
        expr: Ast.Expr,
        ignore: *ArrayList(Str),
        out: *ArrayList(Str),
    ) error{OutOfMemory}!void {
        const num_ignored = ignore.items.len;
        try collect_captured(ally, expr, ignore, out);
        ignore.items.len = num_ignored;
    }
    fn collect_captured(
        ally: Ally,
        expr: Ast.Expr,
        ignore: *ArrayList(Str),
        out: *ArrayList(Str),
    ) error{OutOfMemory}!void {
        switch (expr) {
            .nil => {},
            .name => |name| {
                for (ignore.items) |ig|
                    if (std.mem.eql(u8, ig, name))
                        return;
                for (out.items) |o|
                    if (std.mem.eql(u8, o, name))
                        return;
                try out.append(ally, name);
            },
            .body => |bod| {
                for (bod) |child|
                    try collect_captured(ally, child, ignore, out);
            },
            .int => {},
            .string => {},
            .struct_ => |struct_| {
                for (struct_) |field| {
                    try collect_captured_in_new_scope(ally, field.value, ignore, out);
                }
            },
            .member => |member| try collect_captured_in_new_scope(ally, member.of.*, ignore, out),
            .switch_ => |switch_| {
                try collect_captured_in_new_scope(ally, switch_.condition.*, ignore, out);
                for (switch_.cases) |case| {
                    const num_ignored = ignore.items.len;
                    if (case.payload) |payload|
                        try ignore.append(ally, payload);
                    try collect_captured(ally, case.body, ignore, out);
                    ignore.items.len = num_ignored;
                }
            },
            .lambda => |lambda| {
                const num_ignored = ignore.items.len;
                for (lambda.params) |param| try ignore.append(ally, param);
                try collect_captured(ally, lambda.body.*, ignore, out);
                ignore.items.len = num_ignored;
            },
            .call => |call| {
                try collect_captured_in_new_scope(ally, call.callee.*, ignore, out);
                for (call.args) |arg| {
                    try collect_captured_in_new_scope(ally, arg, ignore, out);
                }
            },
            .var_ => |var_| {
                const num_ignored = ignore.items.len;
                try collect_captured_in_new_scope(ally, var_.value.*, ignore, out);
                ignore.items.len = num_ignored;
                try ignore.append(ally, var_.name);
            },
        }
    }
};

pub fn ir_to_instructions(ally: Ally, ir: Ir) ![]const Instruction {
    return try ir_to_instructions_mod.compile(ally, ir);
}

const ir_to_instructions_mod = struct {
    const Id = Ir.Id;

    const Builder = struct {
        ally: Ally,
        instructions: ArrayList(Instruction),
        stack: *ArrayList(Id),

        pub fn init(ally: Ally, initial_stack: *ArrayList(Id)) Builder {
            return .{
                .ally = ally,
                .instructions = .empty,
                .stack = initial_stack,
            };
        }

        pub fn push_word(builder: *Builder, word: Word, id: Id) !void {
            try builder.instructions.append(builder.ally, .{ .push_word = word });
            try builder.stack.append(builder.ally, id);
        }
        pub fn push_address(builder: *Builder, object: Object, id: Id) !void {
            try builder.instructions.append(builder.ally, .{ .push_address = object });
            try builder.stack.append(builder.ally, id);
        }
        pub fn push_from_stack(builder: *Builder, id: Id) !void {
            const offset = for (0.., builder.stack.items) |i, stack_id| {
                if (stack_id.index == id.index)
                    break builder.stack.items.len - 1 - i;
            } else {
                std.debug.print("stack: {any} id: {any}\n", .{ builder.stack, id });
                @panic("id not on stack");
            };
            try builder.instructions.append(builder.ally, .{ .push_from_stack = offset });
            try builder.stack.append(builder.ally, id);
        }
        pub fn pop(builder: *Builder, amount: usize) !void {
            try builder.instructions.append(builder.ally, .{ .pop = amount });
            builder.stack.items.len -= amount;
        }
        pub fn pop_below_top(builder: *Builder, amount: usize) !void {
            try builder.instructions.append(builder.ally, .{ .pop_below_top = amount });
            const top = builder.stack.pop() orelse @panic("stack empty");
            builder.stack.items.len -= amount;
            try builder.stack.append(builder.ally, top);
        }
        pub fn add(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .add);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn subtract(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .subtract);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn multiply(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .multiply);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn divide(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .divide);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn modulo(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .modulo);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn shift_left(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .shift_left);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn shift_right(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .shift_right);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn compare(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .compare);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn if_not_zero(builder: *Builder, then: []const Instruction, else_: []const Instruction, id: Id) !void {
            try builder.instructions.append(builder.ally, .{ .if_not_zero = .{
                .then = then,
                .else_ = else_,
            } });
            _ = builder.stack.pop(); // condition
            try builder.stack.append(builder.ally, id);
        }
        pub fn new(builder: *Builder, tag_: u8, num_pointers_: usize, num_literals_: usize, id: Id) !void {
            try builder.instructions.append(builder.ally, .{ .new = .{
                .tag = tag_,
                .num_pointers = num_pointers_,
                .num_literals = num_literals_,
            } });
            builder.stack.items.len -= num_pointers_ + num_literals_;
            try builder.stack.append(builder.ally, id);
        }
        pub fn tag(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .tag);
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn num_pointers(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .num_pointers);
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn num_literals(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .num_literals);
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn load(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .load);
            _ = builder.stack.pop();
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
        pub fn eval(builder: *Builder, num_consumed: usize, id: Id) !void {
            try builder.instructions.append(builder.ally, .eval);
            _ = builder.stack.pop(); // list of instructions
            builder.stack.items.len -= num_consumed;
            try builder.stack.append(builder.ally, id);
        }
        pub fn crash(builder: *Builder, id: Id) !void {
            try builder.instructions.append(builder.ally, .crash);
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, id);
        }
    };

    pub fn compile(ally: Ally, ir: Ir) ![]const Instruction {
        var stack = ArrayList(Id).empty;
        for (ir.params) |param| try stack.append(ally, param);
        var builder = Builder.init(ally, &stack);
        for (ir.body.ids) |id|
            try compile_node(ally, id, ir, &builder);
        try builder.push_from_stack(ir.body.returns);
        try builder.pop_below_top(ir.body.ids.len + ir.params.len);
        return builder.instructions.items;
    }

    fn compile_body(ally: Ally, body: Ir.Body, ir: Ir, stack: *ArrayList(Id)) ![]const Instruction {
        const stack_size = stack.items.len;
        var builder = Builder.init(ally, stack);
        for (body.ids) |id| {
            try compile_node(ally, id, ir, &builder);
        }
        try builder.push_from_stack(body.returns);
        try builder.pop_below_top(body.ids.len);
        if (stack.items.len != stack_size + 1) @panic("bad compile");
        return builder.instructions.items;
    }

    fn compile_node(ally: Ally, id: Id, ir: Ir, builder: *Builder) error{OutOfMemory}!void {
        switch (ir.get(id)) {
            .param => unreachable,
            .word => |word| try builder.push_word(word, id),
            .object => |object| try builder.push_address(object, id),
            .new => |new| {
                for (new.pointers) |ref| try builder.push_from_stack(ref);
                for (new.literals) |ref| try builder.push_from_stack(ref);
                try builder.new(new.tag, new.pointers.len, new.literals.len, id);
            },
            .tag => |obj| {
                try builder.push_from_stack(obj);
                try builder.tag(id);
            },
            .num_pointers => |obj| {
                try builder.push_from_stack(obj);
                try builder.num_pointers(id);
            },
            .num_literals => |obj| {
                try builder.push_from_stack(obj);
                try builder.num_literals(id);
            },
            .load => |load| {
                try builder.push_from_stack(load.base);
                try builder.push_from_stack(load.offset);
                try builder.load(id);
            },
            .add => |args| {
                try builder.push_from_stack(args.left);
                try builder.push_from_stack(args.right);
                try builder.add(id);
            },
            .subtract => |args| {
                try builder.push_from_stack(args.left);
                try builder.push_from_stack(args.right);
                try builder.subtract(id);
            },
            .multiply => |args| {
                try builder.push_from_stack(args.left);
                try builder.push_from_stack(args.right);
                try builder.multiply(id);
            },
            .divide => |args| {
                try builder.push_from_stack(args.left);
                try builder.push_from_stack(args.right);
                try builder.divide(id);
            },
            .modulo => |args| {
                try builder.push_from_stack(args.left);
                try builder.push_from_stack(args.right);
                try builder.modulo(id);
            },
            .compare => |args| {
                try builder.push_from_stack(args.left);
                try builder.push_from_stack(args.right);
                try builder.compare(id);
            },
            .call => |call| {
                for (call.args) |arg| try builder.push_from_stack(arg);
                try builder.push_from_stack(call.fun);
                try builder.push_word(1, id);
                try builder.load(id);
                try builder.eval(call.args.len, id);
            },
            .if_not_zero => |if_| {
                try builder.push_from_stack(if_.condition);
                const condition = builder.stack.pop() orelse @panic("stack empty");
                const then = try compile_body(ally, if_.then, ir, builder.stack);
                _ = builder.stack.pop();
                const else_ = try compile_body(ally, if_.else_, ir, builder.stack);
                _ = builder.stack.pop();
                try builder.stack.append(builder.ally, condition);
                try builder.if_not_zero(then, else_, id);
            },
            .crash => |message| {
                try builder.push_from_stack(message);
                try builder.crash(id);
            },
        }
    }
};

pub fn ir_to_fun(ally: Ally, heap: *Heap, ir: Ir) !Object {
    const instructions = try ir_to_instructions(ally, ir);
    const instructions_obj = try Instruction.new_instructions(heap, instructions);
    const nil = try Object.new_nil(heap);
    return try Object.new_fun(heap, ir.params.len, nil, instructions_obj);
}

pub fn ir_to_lambda(ally: Ally, heap: *Heap, ir: Ir) !Object {
    const fun = try ir_to_fun(ally, heap, ir);
    const nil = try Object.new_nil(heap);
    return try Object.new_lambda(heap, fun, nil);
}

pub fn code_to_fun(ally: Ally, heap: *Heap, env: anytype, code: Str) !Object {
    const ast = try str_to_ast(ally, code);
    const ir = try ast_to_ir(ally, heap, env, ast);
    return try ir_to_fun(ally, heap, ir);
}
