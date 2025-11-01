// Parsing a string containing source code into an AST.

const std = @import("std");
const Writer = std.io.Writer;
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;

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
    pub fn format_indented(ir: Ir, writer: *Writer, indentation: usize) !void {
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
            .object => |object| try writer.print("object *{x}\n", .{object.address}),
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

pub fn optimize_ir(ally: Ally, heap: *Heap, ir: Ir) !Ir {
    var builder = Ir.Builder.init(ally);
    const mapping = Map(Ir.Id, Ir.Id).init();
    const body = try optimize_ir_mod.compile_body(
        ally,
        ir,
        ir.body,
        builder,
        heap,
        mapping,
    );
    return builder.finish(body);
}

const optimize_ir_mod = struct {
    const Id = Ir.Id;

    pub fn compile_expr(
        ally: Ally,
        old: Ir,
        old_id: Id,
        body: Ir.BodyBuilder,
        heap: *Heap,
        mapping: *Map(Id, Id),
    ) !Id {
        switch (old.get(old_id)) {
            .param => unreachable,
            .word => |word| return body.word(word),
            .object => |object| return body.object(object),
            .new => |new| {
                const pointers = try ally.alloc(Id, new.pointers.len);
                const literals = try ally.alloc(Id, new.literals.len);
                for (0.., new.pointers) |i, pointer| pointers[i] = mapping.get(pointer);
                for (0.., new.literals) |i, literal| literals[i] = mapping.get(literal);
                return body.new(new.tag, &pointers, &literals);
            },
            .tag => |address| return body.tag(mapping.get(address)),
            .num_pointers => |address| return body.num_pointers(mapping.get(address)),
            .num_literals => |address| return body.num_literals(mapping.get(address)),
            .load => |load| return body.load(mapping.get(load.address), mapping.get(load.offset)),
            .add => |args| return body.add(mapping.get(args.left), mapping.get(args.right)),
            .subtract => |args| return body.subtract(mapping.get(args.left), mapping.get(args.right)),
            .multiply => |args| return body.multiply(mapping.get(args.left), mapping.get(args.right)),
            .divide => |args| return body.divide(mapping.get(args.left), mapping.get(args.right)),
            .modulo => |args| return body.modulo(mapping.get(args.left), mapping.get(args.right)),
            .compare => |args| return body.compare(mapping.get(args.left), mapping.get(args.right)),
            .call => |call| {
                const args = try ally.alloc(Id, call.args.len);
                for (0.., call.args) |i, arg| args[i] = mapping.get(arg);
                return body.call(mapping.get(call.fun), args);
            },
            .if_not_zero => |if_| return body.if_not_zero(
                mapping.get(if_.condition),
                try compile_body(ally, old, if_.then, body.parent, heap, mapping),
                try compile_body(ally, old, if_.else_, body.parent, heap, mapping),
            ),
            .crash => |message| return body.crash(mapping.get(message)),
        }
    }

    fn compile_body(
        ally: Ally,
        old: Ir,
        old_body: Ir.Body,
        builder: *Ir.Builder,
        heap: *Heap,
        mapping: *Map(Id, Id),
    ) !Ir.Body {
        var body = builder.body();
        for (old_body.ids) |id|
            try mapping.put(id, try compile_expr(ally, old, id, heap));
        return body.finish(mapping.get(old_body.returns));
    }
};

pub const Waffle = struct {
    // The operation gives the children semantic meaning.
    op: Op,
    children: []const Waffle,

    const Op = union(enum) {
        param, // -
        let: Id, // def, value
        ref: Id, // -
        word: Word, // -
        object: Object, // -
        new: New, // pointers, literals
        tag, // object
        num_pointers, // object
        num_literals, // object
        load, // base, offset
        add, // left, right
        subtract, // left, right
        multiply, // left, right
        divide, // left, right
        modulo, // left, right
        compare, // left, right
        call, // args, fun
        if_not_zero, // condition, then, else
        crash, // message
    };

    pub const Id = struct { id: usize };
    pub const New = struct { tag: u8, num_pointers: usize, num_literals: usize };

    pub fn format(waffle: Waffle, writer: *Writer) !void {
        try waffle.format_indented(writer, 0);
    }
    pub fn format_indented(expr: Waffle, writer: *Writer, indentation: usize) error{WriteFailed}!void {
        for (0..indentation) |_| try writer.print("  ", .{});
        switch (expr.op) {
            .param => try writer.print("param\n", .{}),
            .let => |id| try writer.print("let %{}\n", .{id.id}),
            .ref => |id| try writer.print("ref %{}\n", .{id.id}),
            .word => |word| try writer.print("word {}\n", .{word}),
            .object => |object| try writer.print("object *{x}\n", .{object.address}),
            .new => |new| try writer.print("new [{x}]\n", .{new.tag}),
            .tag => try writer.print("tag\n", .{}),
            .num_pointers => try writer.print("num pointers\n", .{}),
            .num_literals => try writer.print("num literals\n", .{}),
            .load => try writer.print("load\n", .{}),
            .add => try writer.print("add\n", .{}),
            .subtract => try writer.print("subtract\n", .{}),
            .multiply => try writer.print("multiply\n", .{}),
            .divide => try writer.print("divide\n", .{}),
            .modulo => try writer.print("modulo\n", .{}),
            .compare => try writer.print("compare\n", .{}),
            .call => try writer.print("call\n", .{}),
            .if_not_zero => try writer.print("if not zero\n", .{}),
            .crash => try writer.print("crash\n", .{}),
        }
        for (expr.children) |child|
            try child.format_indented(writer, indentation + 1);
    }
};

pub fn ir_to_waffle(ally: Ally, ir: Ir) !Waffle {
    var compiler = IrToWaffle{
        .ally = ally,
        .ir = ir,
        .mapping = Map(Ir.Id, Waffle.Id).init(ally),
    };
    return try compiler.compile_fun(ir.params);
}
const IrToWaffle = struct {
    ally: Ally,
    ir: Ir,
    mapping: Map(Ir.Id, Waffle.Id),

    fn new_id(self: *IrToWaffle, ir_id: Ir.Id) !Waffle.Id {
        const id = Waffle.Id{ .id = self.mapping.unmanaged.count() };
        try self.mapping.put(ir_id, id);
        return id;
    }
    fn ref(self: IrToWaffle, ir_id: Ir.Id) Waffle {
        return .{
            .op = .{ .ref = self.mapping.get(ir_id).? },
            .children = &[_]Waffle{},
        };
    }

    fn compile_fun(self: *IrToWaffle, params: []const Ir.Id) !Waffle {
        if (params.len == 0) {
            return try self.compile_body(self.ir.body.ids, self.ir.body.returns);
        }
        const param = try self.new_id(params[0]);
        const children = try self.ally.alloc(Waffle, 2);
        children[0] = .{ .op = .param, .children = &[_]Waffle{} };
        children[1] = try self.compile_fun(params[1..]);
        return .{ .op = .{ .let = param }, .children = children };
    }
    fn compile_body(self: *IrToWaffle, ids: []const Ir.Id, returns: Ir.Id) !Waffle {
        if (ids.len == 0) return self.ref(returns);
        const id = try self.new_id(ids[0]);
        const children = try self.ally.alloc(Waffle, 2);
        children[0] = try self.compile_node(self.ir.get(ids[0]));
        children[1] = try self.compile_body(ids[1..], returns);
        return .{ .op = .{ .let = id }, .children = children };
    }
    pub fn compile_node(self: *IrToWaffle, node: Ir.Node) error{OutOfMemory}!Waffle {
        switch (node) {
            .param => unreachable,
            .word => |word| return .{
                .op = .{ .word = word },
                .children = &[_]Waffle{},
            },
            .object => |object| return .{
                .op = .{ .object = object },
                .children = &[_]Waffle{},
            },
            .new => |new| {
                const children = try self.ally.alloc(Waffle, new.pointers.len + new.literals.len);
                for (0.., new.pointers) |i, pointer| children[i] = self.ref(pointer);
                for (new.pointers.len.., new.literals) |i, literal| children[i] = self.ref(literal);
                return .{
                    .op = .{ .new = .{
                        .tag = new.tag,
                        .num_pointers = new.pointers.len,
                        .num_literals = new.literals.len,
                    } },
                    .children = children,
                };
            },
            .tag => |obj| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(obj);
                return .{ .op = .tag, .children = children };
            },
            .num_pointers => |obj| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(obj);
                return .{ .op = .num_pointers, .children = children };
            },
            .num_literals => |obj| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(obj);
                return .{ .op = .num_literals, .children = children };
            },
            .load => |load| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(load.base);
                children[1] = self.ref(load.offset);
                return .{ .op = .load, .children = children };
            },
            .add => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .add, .children = children };
            },
            .subtract => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .subtract, .children = children };
            },
            .multiply => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .multiply, .children = children };
            },
            .divide => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .divide, .children = children };
            },
            .modulo => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .modulo, .children = children };
            },
            .compare => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .compare, .children = children };
            },
            .call => |call| {
                const children = try self.ally.alloc(Waffle, call.args.len + 1);
                for (0.., call.args) |i, arg| children[i] = self.ref(arg);
                children[call.args.len] = self.ref(call.fun);
                return .{ .op = .call, .children = children };
            },
            .if_not_zero => |if_| {
                const children = try self.ally.alloc(Waffle, 3);
                children[0] = self.ref(if_.condition);
                children[1] = try self.compile_body(if_.then.ids, if_.then.returns);
                children[2] = try self.compile_body(if_.else_.ids, if_.else_.returns);
                return .{ .op = .if_not_zero, .children = children };
            },
            .crash => |message| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(message);
                return .{ .op = .crash, .children = children };
            },
        }
    }
};

pub fn optimize_waffle(ally: Ally, waffle: Waffle) !Waffle {
    const children = try ally.alloc(Waffle, waffle.children.len);
    for (0.., waffle.children) |i, child|
        children[i] = try optimize_waffle(ally, child);

    switch (waffle.op) {
        .let => |id| {
            const def = children[0];
            const value = children[1];

            const is_cheap = switch (def.op) {
                .word => true,
                .object => true,
                else => false,
            };
            if (is_cheap) return try inline_waffle(ally, value, id, def);

            // const num_references = count_references(value, id);

            // if (num_references == 0) {
            //     return .{ .op = .also, .children = children };
            // }
            // if (num_references == 1) {
            //     const used_before_impure = used: {
            //         find_usage_before_impure(value, id) catch |e| {
            //             switch (e) {
            //                 error.Used => break :used true,
            //                 error.NotUsed => break :used false,
            //             }
            //         };
            //         break :used false;
            //     };
            //     if (used_before_impure) {
            //         return try inline_waffle(ally, value, id, def);
            //     }
            // }
        },
        else => {},
    }

    return .{ .op = waffle.op, .children = children };
}
fn inline_waffle(ally: Ally, expr: Waffle, id: Waffle.Id, def: Waffle) !Waffle {
    switch (expr.op) {
        .ref => |ref| if (ref.id == id.id) return def,
        else => {},
    }

    const children = try ally.alloc(Waffle, expr.children.len);
    for (0.., expr.children) |i, child|
        children[i] = try inline_waffle(ally, child, id, def);
    return .{ .op = expr.op, .children = children };
}
fn count_references(expr: Waffle, id: Waffle.Id) usize {
    switch (expr.op) {
        .ref => |ref| return if (ref.id == id.id) 1 else 0,
        else => {
            var sum: usize = 0;
            for (expr.children) |child| sum += count_references(child, id);
            return sum;
        },
    }
}
fn find_usage_before_impure(expr: Waffle, id: Waffle.Id) error{ Used, NotUsed }!void {
    switch (expr.op) {
        .ref => |ref| if (ref.id == id.id) return error.Used,
        .call => {
            for (expr.children) |child| try find_usage_before_impure(child, id);
            return error.NotUsed;
        },
        .if_not_zero => try find_usage_before_impure(expr.children[0], id),
        .crash => {
            for (expr.children) |child| try find_usage_before_impure(child, id);
            return error.NotUsed;
        },
        else => {
            for (expr.children) |child| try find_usage_before_impure(child, id);
        },
    }
}

pub fn waffle_to_instructions(ally: Ally, waffle: Waffle) ![]const Instruction {
    var mapping = Map(Waffle.Id, usize).init(ally);
    var compiler = WaffleToInstructions{
        .ally = ally,
        .instructions = ArrayList(Instruction).empty,
        .stack_size = 0,
        .mapping = &mapping,
    };
    try compiler.compile(waffle);
    return compiler.instructions.items;
}
const WaffleToInstructions = struct {
    ally: Ally,
    instructions: ArrayList(Instruction),
    stack_size: usize,
    mapping: *Map(Waffle.Id, usize),

    fn emit(self: *WaffleToInstructions, instruction: Instruction) !void {
        try self.instructions.append(self.ally, instruction);
    }

    fn compile(self: *WaffleToInstructions, waffle: Waffle) error{OutOfMemory}!void {
        const stack_size_before = self.stack_size;
        switch (waffle.op) {
            .param => self.stack_size += 1,
            .let => |id| {
                const stack_size = self.stack_size;
                try self.mapping.put(id, stack_size);
                try self.compile(waffle.children[0]);
                try self.compile(waffle.children[1]);
                try self.emit(.{ .pop_below_top = 1 });
                self.stack_size -= 1;
            },
            .ref => |id| {
                const offset = self.stack_size - self.mapping.get(id).? - 1;
                try self.emit(.{ .push_from_stack = offset });
                self.stack_size += 1;
            },
            .word => |word| {
                try self.emit(.{ .push_word = word });
                self.stack_size += 1;
            },
            .object => |object| {
                try self.emit(.{ .push_address = object });
                self.stack_size += 1;
            },
            .new => |new| {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.{ .new = .{
                    .tag = new.tag,
                    .num_pointers = new.num_pointers,
                    .num_literals = new.num_literals,
                } });
                self.stack_size -= new.num_pointers + new.num_literals;
                self.stack_size += 1;
            },
            .tag => {
                try self.compile(waffle.children[0]);
                try self.emit(.tag);
                self.stack_size -= 1;
                self.stack_size += 1;
            },
            .num_pointers => {
                try self.compile(waffle.children[0]);
                try self.emit(.num_pointers);
                self.stack_size -= 1;
                self.stack_size += 1;
            },
            .num_literals => {
                try self.compile(waffle.children[0]);
                try self.emit(.num_literals);
                self.stack_size -= 1;
                self.stack_size += 1;
            },
            .load => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.load);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .add => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.add);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .subtract => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.subtract);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .multiply => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.multiply);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .divide => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.divide);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .modulo => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.modulo);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .compare => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.compare);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .call => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.{ .push_word = 1 });
                try self.emit(.load);
                try self.emit(.eval);
                self.stack_size -= waffle.children.len;
                self.stack_size += 1;
            },
            .if_not_zero => {
                try self.compile(waffle.children[0]);
                var then_compiler = WaffleToInstructions{
                    .ally = self.ally,
                    .instructions = ArrayList(Instruction).empty,
                    .stack_size = self.stack_size - 1,
                    .mapping = self.mapping,
                };
                try then_compiler.compile(waffle.children[1]);
                var else_compiler = WaffleToInstructions{
                    .ally = self.ally,
                    .instructions = ArrayList(Instruction).empty,
                    .stack_size = self.stack_size - 1,
                    .mapping = self.mapping,
                };
                try else_compiler.compile(waffle.children[2]);
                try self.emit(.{ .if_not_zero = .{
                    .then = then_compiler.instructions.items,
                    .else_ = else_compiler.instructions.items,
                } });
                self.stack_size -= 1;
                self.stack_size += 1;
            },
            .crash => {
                try self.compile(waffle.children[0]);
                try self.emit(.crash);
            },
        }
        const stack_size_after = self.stack_size;
        if (stack_size_before + 1 != stack_size_after) {
            std.debug.print("stack size before: {} after: {}\n", .{ stack_size_before, stack_size_after });
            std.debug.print("when compiling:\n{f}", .{waffle});
            @panic("bad compile");
        }
    }
};

pub fn instructions_to_fun(heap: *Heap, num_params_: usize, instructions: []const Instruction) !Object {
    const instructions_obj = try Instruction.new_instructions(heap, instructions);
    return try Object.new_fun(heap, num_params_, try Object.new_nil(heap), instructions_obj);
}

pub fn waffle_to_fun(heap: *Heap, num_params: usize, waffle: Waffle) !Object {
    const instructions = try waffle_to_instructions(waffle);
    return try instructions_to_fun(heap, num_params, instructions);
}

pub fn ir_to_fun(ally: Ally, heap: *Heap, ir: Ir) !Object {
    const waffle = try ir_to_waffle(ally, ir);
    const optimized_waffle = try optimize_waffle(ally, waffle);
    std.debug.print("optimized waffle:\n{f}", .{optimized_waffle});
    const instructions = try waffle_to_instructions(ally, optimized_waffle);
    const instructions_obj = try Instruction.new_instructions(heap, instructions);
    const boxed_ir = try ally.create(Ir);
    boxed_ir.* = ir;
    const ir_ptr = try Object.new_int(heap, @bitCast(@intFromPtr(boxed_ir)));
    return try Object.new_fun(heap, ir.params.len, ir_ptr, instructions_obj);
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
