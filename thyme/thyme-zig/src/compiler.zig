// This file contains the Thyme compiler, turning source code into heap objects.

const std = @import("std");
const Writer = std.io.Writer;
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const Instruction = @import("instruction.zig").Instruction;
const object_mod = @import("object.zig");
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
    expr: Expr,

    pub const Expr = union(enum) {
        name: Str,
        let: Let,
        int: i64,
        string: Str,
        struct_: Struct,
        enum_: Enum,
        member: Member,
        switch_: Switch,
        lambda: Lambda,
        call: Call,
    };
    pub const Let = struct { name: Str, value: *const Expr, expr: *const Expr };
    pub const Struct = struct { fields: []const Field };
    pub const Field = struct { name: Str, value: Expr };
    pub const Enum = struct { variant: []const u8, payload: *const Expr };
    pub const Member = struct { of: *const Expr, name: Str };
    pub const Switch = struct { condition: *const Expr, cases: []const Case };
    pub const Case = struct { variant: []const u8, payload_name: ?Str, body: *const Expr };
    pub const Lambda = struct { params: []Str, body: *const Expr };
    pub const Call = struct { callee: *const Expr, args: []const Expr };

    pub fn format(expr: Expr, writer: *Writer) !void {
        try format_expr(expr, writer, 0);
    }
    pub fn format_expr(expr: Expr, writer: *Writer, indentation: usize) !void {
        for (0..indentation) |_| try writer.print("  ", .{});
        switch (expr) {
            .name => |name| try writer.print("{s}", .{name}),
            .int => |int| try writer.print("{}", .{int}),
            .string => |string| try writer.print("\"{s}\"", .{string}),
            .let => |let| {
                try writer.print("(: {s}\n", .{let.name});
                try format_expr(let.value.*, writer, indentation + 1);
                try writer.print("\n", .{});
                try format_expr(let.expr.*, writer, indentation + 1);
                try writer.print(")", .{});
            },
            .struct_ => |struct_| {
                try writer.print("(&\n", .{});
                for (struct_.fields) |field| {
                    for (0..indentation + 1) |_| try writer.print("  ", .{});
                    try writer.print("{s}\n", .{field.name});
                    try format_expr(field.value, writer, indentation + 1);
                    try writer.print("\n", .{});
                }
            },
            .member => |member| {
                try writer.print("(.\n", .{});
                try format_expr(member.of.*, writer, indentation);
                try writer.print("\n", .{});
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("{s})\n", .{member.name});
            },
            .enum_ => |enum_| {
                try writer.print("(| {s}\n", .{enum_.variant});
                try format_expr(enum_.payload.*, writer, indentation + 2);
                try writer.print(")", .{});
            },
            .switch_ => |switch_| {
                try writer.print("(%\n", .{});
                try format_expr(switch_.condition.*, writer, indentation + 1);
                for (switch_.cases) |case| {
                    for (0..indentation) |_| try writer.print("  ", .{});
                    try writer.print("{s}", .{case.variant});
                    if (case.payload_name) |payload_name| {
                        try writer.print(" {s}", .{payload_name});
                    }
                    try writer.print("\n", .{});
                    try format_expr(case.body.*, writer, indentation + 1);
                }
                try writer.print(")", .{});
            },
            .lambda => |lambda| {
                try writer.print("(\\ (", .{});
                for (0.., lambda.params) |i, param| {
                    if (i > 0) try writer.print(" ", .{});
                    try writer.print("{s}", .{param});
                }
                try writer.print(")\n", .{});
                try format_expr(lambda.body.*, writer, indentation + 1);
                try writer.print(")", .{});
            },
            .call => |call| {
                try writer.print("(\n", .{});
                try format_expr(call.callee.*, writer, indentation);
                for (call.args) |arg| {
                    try writer.print("\n", .{});
                    try format_expr(arg, writer, indentation + 2);
                }
                for (0..indentation + 1) |_| try writer.print("  ", .{});
                try writer.print(")", .{});
            },
        }
    }
};

pub fn str_to_ast(ally: Ally, input: Str) !Ast.Expr {
    var parser = Parser{ .ally = ally, .input = input, .cursor = 0 };
    const expr = parser.parse_expr() catch |e| {
        std.debug.print("Error at {}: {}\n", .{ parser.cursor, e });
        @panic("bad input");
    } orelse return error.ExpectedExpr;
    parser.consume_whitespace();
    if (parser.cursor < parser.input.len) std.debug.print("Unparsed input at {}.\n", .{parser.cursor});
    return try add_semantics(ally, expr);
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
                            if (std.mem.eql(u8, let_name, "#")) continue;
                            const value = try add_semantics(ally, group[group.len - 2 * i - 2]);
                            const new = Ast.Expr{ .let = .{
                                .name = let_name,
                                .value = try box(ally, value),
                                .expr = try box(ally, expr),
                            } };
                            expr = new;
                        }
                        return expr;
                    }
                    if (std.mem.eql(u8, name, "&")) {
                        if (group.len % 2 != 1) return error.BadStruct;
                        var fields = try ally.alloc(Ast.Field, group.len / 2);
                        for (0..group.len / 2) |i|
                            fields[i] = .{
                                .name = switch (group[2 * i + 1]) {
                                    .name => |n| n,
                                    else => return error.BadStruct,
                                },
                                .value = try add_semantics(ally, group[2 * i + 2]),
                            };
                        return .{ .struct_ = .{ .fields = fields } };
                    }
                    if (std.mem.eql(u8, name, ".")) {
                        if (group.len != 3) return error.BadMember;
                        const of = try add_semantics(ally, group[1]);
                        const member = switch (group[2]) {
                            .name => |n| n,
                            else => return error.BadMember,
                        };
                        return .{ .member = .{ .of = try box(ally, of), .name = member } };
                    }
                    if (std.mem.eql(u8, name, "|")) {
                        if (group.len < 2 or group.len > 3) return error.BadEnum;
                        const variant = switch (group[1]) {
                            .name => |n| n,
                            else => return error.BadEnum,
                        };
                        const payload = if (group.len == 3)
                            try add_semantics(ally, group[2])
                        else
                            Ast.Expr{ .struct_ = .{ .fields = &.{} } }; // Directly create an empty Ast.Struct
                        return .{ .enum_ = .{ .variant = variant, .payload = try box(ally, payload) } };
                    }
                    if (std.mem.eql(u8, name, "%")) {
                        if (group.len % 2 == 1) return error.BadSwitch;
                        const condition = try add_semantics(ally, group[1]);
                        var cases = ArrayList(Ast.Case).empty;
                        for (0..group.len / 2 - 1) |i| {
                            var case_variant: Str = undefined;
                            var case_payload_name: ?Str = null;
                            switch (group[2 * i + 2]) {
                                .name => |n| case_variant = n,
                                .group => |g| {
                                    if (g.len != 2) return error.BadSwitch;
                                    case_variant = switch (g[0]) {
                                        .name => |n| n,
                                        else => return error.BadSwitch,
                                    };
                                    case_payload_name = switch (g[1]) {
                                        .name => |n| n,
                                        else => return error.BadSwitch,
                                    };
                                },
                                else => return error.BadSwitch,
                            }
                            try cases.append(ally, .{
                                .variant = case_variant,
                                .payload_name = case_payload_name,
                                .body = try box(ally, try add_semantics(ally, group[2 * i + 3])),
                            });
                        }
                        return .{ .switch_ = .{ .condition = try box(ally, condition), .cases = cases.items } };
                    }
                    if (std.mem.eql(u8, name, "\\")) {
                        if (group.len != 3) return error.BadLambda;
                        var params = ArrayList(Str).empty;
                        for (switch (group[1]) {
                            .group => |g| g,
                            else => return error.BadLambda,
                        }) |param| {
                            switch (param) {
                                .name => |n| try params.append(ally, n),
                                else => return error.BadLambda,
                            }
                        }
                        return .{ .lambda = .{
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

// The Intermediate Representation is a semantic representation of code.
// Variable names have been removed, nested expressions been flattened, and
// everything is just a graph.

pub const Ir = struct {
    nodes: []Node,
    params: []Id,
    body: Body,

    pub const Id = struct {
        index: usize, // index into the nodes list

        pub fn format(id: Id, writer: *Writer) !void {
            try writer.print("%{}", .{id.index});
        }
    };
    pub const Body = struct { ids: []const Id, returns: Id };
    pub const Node = union(enum) {
        param, // doesn't occur in the function body
        word: Word,
        object: Address,
        new: New, // creates a new object, results in address
        flatten_to_pointers_object: Id,
        flatten_to_literals_object: Id,
        has_pointers: Id, // takes address, returns 1 or 0
        num_words: Id, // takes address, returns number of words
        load: Load, // loads a field of an object
        add: Args,
        subtract: Args,
        multiply: Args,
        divide: Args,
        modulo: Args,
        shift_left: Args,
        shift_right: Args,
        compare: Args, // equal: 0, greater: 1, less: 2
        call: Call,
        if_not_zero: If,
        crash: Id,
    };
    pub const New = struct { has_pointers: bool, words: []const Id };
    pub const Load = struct { base: Id, offset: Id };
    pub const Args = struct { left: Id, right: Id };
    pub const Call = struct { instructions: Id, args: []const Id };
    pub const If = struct { condition: Id, then: Body, else_: Body };

    pub fn get(ir: Ir, id: Id) Node {
        return ir.nodes[id.index];
    }

    pub fn format(ir: Ir, heap: Heap, writer: *Writer) !void {
        try ir.format_indented(heap, writer, 0);
    }
    pub fn format_indented(ir: Ir, heap: Heap, writer: *Writer, indentation: usize) !void {
        try writer.print("code", .{});
        for (ir.params) |param| try writer.print(" {f}", .{param});
        try writer.print("\n", .{});
        try format_body(ir.body, ir, heap, writer, indentation + 1);
    }
    fn format_body(body: Body, ir: Ir, heap: Heap, writer: *Writer, indentation: usize) !void {
        for (body.ids) |id| {
            for (0..indentation) |_| try writer.print("  ", .{});
            try writer.print("{f} = ", .{id});
            try format_node(ir.get(id), ir, heap, writer, indentation);
        }
        for (0..indentation) |_| try writer.print("  ", .{});
        try writer.print("{f}\n", .{body.returns});
    }
    fn format_node(node: Node, ir: Ir, heap: Heap, writer: *Writer, indentation: usize) error{WriteFailed}!void {
        switch (node) {
            .param => try writer.print("param\n", .{}),
            .word => |word| try writer.print("word {}\n", .{word}),
            .object => |object| {
                try writer.print("object ", .{});
                try heap.format_indented(object, writer, indentation);
                try writer.print("\n", .{});
            },
            .new => |new| {
                try writer.print("new", .{});
                for (new.words) |word| try writer.print(" {f}", .{word});
                try writer.print("\n", .{});
            },
            .has_pointers => |address| try writer.print("has_pointers {f}\n", .{address}),
            .num_words => |address| try writer.print("num_words {f}\n", .{address}),
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
                try format_body(if_.then, ir, heap, writer, indentation + 1);
                for (0..indentation) |_| try writer.print("  ", .{});
                try writer.print("else\n", .{});
                try format_body(if_.else_, ir, heap, writer, indentation + 1);
            },
            .crash => |message| try writer.print("crash {f}\n", .{message}),
        }
    }

    pub const Builder = struct {
        ally: Ally,
        nodes: ArrayList(Node),
        params: ArrayList(Id),

        pub fn init(ally: Ally) Builder {
            return .{ .ally = ally, .nodes = .empty, .params = .empty };
        }

        pub fn body(builder: *Builder) BodyBuilder {
            return .{ .parent = builder, .ids = .empty };
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
            return .{ .nodes = builder.nodes.items, .params = builder.params.items, .body = bod };
        }
    };
    pub const BodyBuilder = struct {
        parent: *Builder,
        ids: ArrayList(Id),

        fn create_and_push(b: *BodyBuilder, node: Node) !Id {
            const id = try b.parent.create(node);
            try b.ids.append(b.parent.ally, id);
            return id;
        }

        pub fn child_body(b: BodyBuilder) BodyBuilder {
            return .{ .parent = b.parent, .ids = .empty };
        }

        pub fn get(b: BodyBuilder, id: Id) Node {
            return b.parent.nodes.items[id.index];
        }

        pub fn finish(b: BodyBuilder, returns: Id) Body {
            return .{ .ids = b.ids.items, .returns = returns };
        }

        pub fn word(b: *BodyBuilder, word_: Word) !Id {
            return b.create_and_push(.{ .word = word_ });
        }
        pub fn object(b: *BodyBuilder, obj: Address) !Id {
            return b.create_and_push(.{ .object = obj });
        }
        pub fn new(b: *BodyBuilder, has_pointers_: bool, words: []const Id) !Id {
            return try b.create_and_push(.{ .new = .{ .has_pointers = has_pointers_, .words = words } });
        }
        pub fn flatten_to_pointers_object(b: *BodyBuilder, list: Id) !Id {
            return try b.create_and_push(.{ .flatten_to_pointers_object = list });
        }
        pub fn flatten_to_literals_object(b: *BodyBuilder, list: Id) !Id {
            return try b.create_and_push(.{ .flatten_to_literals_object = list });
        }
        pub fn has_pointers(b: *BodyBuilder, address: Id) !Id {
            return b.create_and_push(.{ .has_pointers = address });
        }
        pub fn num_words(b: *BodyBuilder, address: Id) !Id {
            return b.create_and_push(.{ .num_words = address });
        }
        pub fn load(b: *BodyBuilder, address: Id, offset: Id) !Id {
            return b.create_and_push(.{ .load = .{ .base = address, .offset = offset } });
        }
        pub fn add(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .add = .{ .left = left, .right = right } });
        }
        pub fn subtract(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .subtract = .{ .left = left, .right = right } });
        }
        pub fn multiply(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .multiply = .{ .left = left, .right = right } });
        }
        pub fn divide(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .divide = .{ .left = left, .right = right } });
        }
        pub fn modulo(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .modulo = .{ .left = left, .right = right } });
        }
        pub fn shift_left(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .shift_left = .{ .left = left, .right = right } });
        }
        pub fn shift_right(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .shift_right = .{ .left = left, .right = right } });
        }
        pub fn compare(b: *BodyBuilder, left: Id, right: Id) !Id {
            return b.create_and_push(.{ .compare = .{ .left = left, .right = right } });
        }
        pub fn call(b: *BodyBuilder, instructions: Id, args: []const Id) !Id {
            return b.create_and_push(.{ .call = .{ .instructions = instructions, .args = args } });
        }
        pub fn if_not_zero(b: *BodyBuilder, condition: Id, then: Body, else_: Body) !Id {
            return b.create_and_push(.{ .if_not_zero = .{ .condition = condition, .then = then, .else_ = else_ } });
        }
        pub fn crash(b: *BodyBuilder, message: Id) !Id {
            return b.create_and_push(.{ .crash = message });
        }

        // Higher-level convenience functions

        pub fn loadi(b: *BodyBuilder, obj: Id, index: usize) !Id {
            const i = try b.word(index);
            return try b.load(obj, i);
        }
        pub fn type_of(b: *BodyBuilder, obj: Id) !Id {
            return try b.loadi(obj, 0);
        }
        pub fn kind_of(b: *BodyBuilder, type_: Id) !Id {
            return try b.loadi(type_, 0);
        }

        pub fn if_eq(b: *BodyBuilder, left: Id, right: Id, then: Body, else_: Body) !Id {
            return try b.if_not_zero(try b.subtract(left, right), else_, then);
        }
        pub fn if_eq_symbol(b: *BodyBuilder, heap: *Heap, symbol: Id, expected: []const u8, then: Body, else_: Body) !Id {
            const comptime_symbol = try object_mod.new_symbol(heap, expected);
            const words = heap.get(comptime_symbol).words;
            const matches = try b.if_eq(try b.num_words(symbol), try b.word(words.len), good: {
                var bb = b.child_body();
                const result = try bb.if_eq_symbol_rec(symbol, 0, words);
                break :good bb.finish(result);
            }, bad: {
                var bb = b.child_body();
                break :bad try bb.finish_with_zero();
            });
            return try b.if_not_zero(matches, then, else_);
        }
        fn if_eq_symbol_rec(b: *BodyBuilder, symbol: Id, i: usize, expected: []const u64) !Id {
            if (i == expected.len) {
                return try b.word(1);
            } else {
                return try b.if_eq(
                    try b.load(symbol, try b.word(i)),
                    try b.word(expected[i]),
                    the_same: {
                        var bb = b.child_body();
                        const result = try bb.if_eq_symbol_rec(symbol, i + 1, expected);
                        break :the_same bb.finish(result);
                    },
                    not_the_same: {
                        var bb = b.child_body();
                        break :not_the_same try bb.finish_with_zero();
                    },
                );
            }
        }

        pub fn crash_with_symbol(b: *BodyBuilder, heap: *Heap, message: []const u8) !Id {
            const message_obj = try object_mod.new_symbol(heap, message);
            return b.crash(try b.object(message_obj));
        }

        pub fn new_int(b: *BodyBuilder, common: CommonObjects, value: Id) !Id {
            const value_obj = obj: {
                const words = try b.parent.ally.alloc(Id, 1);
                words[0] = value;
                break :obj try b.new(false, words);
            };
            const int_obj = {
                const words = try b.parent.ally.alloc(Id, 2);
                words[0] = try b.object(common.int_type);
                words[1] = value_obj;
                return try b.new(true, words);
            };
            return int_obj;
        }
        pub fn assert_is_int(body: *BodyBuilder, value: Id, heap: *Heap, message: []const u8) !Id {
            const kind = try body.kind_of(try body.type_of(value));
            return try body.if_eq_symbol(heap, kind, "int", good: {
                var bb = body.child_body();
                const result = try bb.word(0);
                break :good bb.finish(result);
            }, bad: {
                var bb = body.child_body();
                const result = try bb.crash_with_symbol(heap, message);
                break :bad bb.finish(result);
            });
        }
        pub fn get_int_value(body: *BodyBuilder, int: Id) !Id {
            return try body.load(try body.load(int, try body.word(1)), try body.word(0));
        }

        pub fn assert_is_string(body: *BodyBuilder, value: Id, heap: *Heap, message: []const u8) !Id {
            const kind = try body.kind_of(try body.type_of(value));
            return try body.if_eq_symbol(heap, kind, "string", good: {
                var bb = body.child_body();
                const result = try bb.word(0);
                break :good bb.finish(result);
            }, bad: {
                var bb = body.child_body();
                const result = try bb.crash_with_symbol(heap, message);
                break :bad bb.finish(result);
            });
        }
        pub fn get_string_value(body: *BodyBuilder, string: Id) !Id {
            const one = try body.word(1);
            return try body.load(string, one);
        }

        pub fn assert_is_struct(body: *BodyBuilder, value: Id, heap: *Heap, message: []const u8) !Id {
            const kind = try body.kind_of(try body.type_of(value));
            return try body.if_eq_symbol(heap, kind, "struct", good: {
                var bb = body.child_body();
                const result = try bb.word(0);
                break :good bb.finish(result);
            }, bad: {
                var bb = body.child_body();
                const result = try bb.crash_with_symbol(heap, message);
                break :bad bb.finish(result);
            });
        }

        pub fn assert_is_enum(body: *BodyBuilder, value: Id, heap: *Heap, message: []const u8) !Id {
            const kind = try body.kind_of(try body.type_of(value));
            return try body.if_eq_symbol(heap, kind, "enum", good: {
                var bb = body.child_body();
                break :good try bb.finish_with_zero();
            }, bad: {
                var bb = body.child_body();
                break :bad try bb.finish_with_symbol_crash(heap, message);
            });
        }

        pub fn assert_is_array(body: *BodyBuilder, value: Id, heap: *Heap, message: []const u8) !Id {
            const kind = try body.kind_of(try body.type_of(value));
            return try body.if_eq_symbol(heap, kind, "array", good: {
                var bb = body.child_body();
                break :good try bb.finish_with_zero();
            }, bad: {
                var bb = body.child_body();
                break :bad try bb.finish_with_symbol_crash(heap, message);
            });
        }

        pub fn new_closure(body: *BodyBuilder, captured: []Id) !Id {
            return try body.new(true, captured);
        }
        pub fn get_closure_var(body: *BodyBuilder, closure: Id, index: i64) !Id {
            return try body.load(closure, body.word(index));
        }

        pub fn finish_with_zero(b: *BodyBuilder) !Body {
            const zero = try b.word(0);
            return b.finish(zero);
        }
        pub fn finish_with_one(b: *BodyBuilder) !Body {
            const zero = try b.word(1);
            return b.finish(zero);
        }
        pub fn finish_with_crash(b: *BodyBuilder, message: Id) !Body {
            const never = try b.crash(message);
            return b.finish(never);
        }
        pub fn finish_with_symbol_crash(b: *BodyBuilder, heap: *Heap, message: []const u8) !Body {
            const never = try b.crash_with_symbol(heap, message);
            return b.finish(never);
        }
    };
};

pub const CommonObjects = struct {
    empty_obj: Address,
    int_symbol: Address,
    string_symbol: Address,
    struct_symbol: Address,
    enum_symbol: Address,
    lambda_symbol: Address,
    int_type: Address,
    string_type: Address,
    array_type: Address,
    empty_struct: Address,
    true_: Address,
    false_: Address,
    equal: Address,
    greater: Address,
    less: Address,
    compare_symbols_fun: Address,
    lookup_field_fun: Address,

    pub fn create(ally: Ally, heap: *Heap) !CommonObjects {
        const Builder = Ir.Builder;
        const empty_obj = try object_mod.empty_obj(heap);
        const int_symbol = try object_mod.new_symbol(heap, "int");
        const string_symbol = try object_mod.new_symbol(heap, "string");
        const struct_symbol = try object_mod.new_symbol(heap, "struct");
        const enum_symbol = try object_mod.new_symbol(heap, "enum");
        const array_symbol = try object_mod.new_symbol(heap, "array");
        const lambda_symbol = try object_mod.new_symbol(heap, "lambda");
        const int_type = try heap.new_pointers(&[_]Word{int_symbol});
        const string_type = try heap.new_pointers(&[_]Word{string_symbol});
        const array_type = try heap.new_pointers(&[_]Word{array_symbol});
        const empty_struct = try heap.new_pointers(&[_]Word{
            try heap.new(.{ .has_pointers = true, .words = &[_]Word{struct_symbol} }),
        });
        const true_ = try heap.new_pointers(&.{
            try heap.new_pointers(&.{ enum_symbol, try object_mod.new_symbol(heap, "true") }),
            empty_struct,
        });
        const false_ = try heap.new_pointers(&.{
            try heap.new_pointers(&.{ enum_symbol, try object_mod.new_symbol(heap, "false") }),
            empty_struct,
        });
        const equal = try heap.new_pointers(&.{
            try heap.new_pointers(&.{ enum_symbol, try object_mod.new_symbol(heap, "equal") }),
            empty_struct,
        });
        const greater = try heap.new_pointers(&.{
            try heap.new_pointers(&.{ enum_symbol, try object_mod.new_symbol(heap, "greater") }),
            empty_struct,
        });
        const less = try heap.new_pointers(&.{
            try heap.new_pointers(&.{ enum_symbol, try object_mod.new_symbol(heap, "less") }),
            empty_struct,
        });
        // TODO: introduce loops to the IR
        const compare_symbols_fun_rec = try ir_to_instructions(ally, heap, fun: {
            var builder = Builder.init(ally);
            const rec = try builder.param();
            const left = try builder.param();
            const right = try builder.param();
            const index = try builder.param();
            const num_words = try builder.param();
            var b = builder.body();
            const result = try b.if_eq(
                index,
                num_words,
                done: {
                    var bb = builder.body();
                    break :done try bb.finish_with_one();
                },
                not_done: {
                    var bb = builder.body();
                    const left_word = try bb.load(left, index);
                    const right_word = try bb.load(right, index);
                    const result = try bb.if_eq(left_word, right_word, inner_then: {
                        var bbb = builder.body();
                        const next_index = try bbb.add(index, try bbb.word(1));
                        const result = try bbb.call(rec, try box(ally, .{ rec, left, right, next_index, num_words }));
                        break :inner_then bbb.finish(result);
                    }, inner_else: {
                        var bbb = builder.body();
                        break :inner_else try bbb.finish_with_zero();
                    });
                    break :not_done bb.finish(result);
                },
            );
            const body = b.finish(result);
            break :fun builder.finish(body);
        });
        const compare_symbols_fun = try ir_to_instructions(ally, heap, fun: {
            var builder = Builder.init(ally);
            const left = try builder.param();
            const right = try builder.param();
            var b = builder.body();
            const result = try b.if_eq(try b.num_words(left), try b.num_words(right), same_len: {
                var bb = builder.body();
                const num_words = try bb.num_words(left);
                const rec = try bb.object(compare_symbols_fun_rec);
                const zero = try bb.word(0);
                const result = try bb.call(rec, try box(ally, .{ rec, left, right, zero, num_words }));
                break :same_len bb.finish(result);
            }, different_lens: {
                var bb = builder.body();
                break :different_lens try bb.finish_with_zero();
            });
            const body = b.finish(result);
            break :fun builder.finish(body);
        });
        const lookup_field_fun_rec = try ir_to_instructions(ally, heap, fun: {
            var builder = Builder.init(ally);
            const rec = try builder.param();
            const type_obj = try builder.param();
            const symbol = try builder.param();
            const index = try builder.param();
            var b = builder.body();
            const result = try b.if_eq(index, try b.num_words(type_obj), no_match: {
                var bb = builder.body();
                const result = try bb.crash(try bb.object(try object_mod.new_symbol(heap, "no such field")));
                break :no_match bb.finish(result);
            }, check: {
                var bb = builder.body();
                const field_name = try bb.load(type_obj, index);
                const match = try bb.call(
                    try bb.object(compare_symbols_fun),
                    try box(ally, .{ field_name, symbol }),
                );
                const result = try bb.if_not_zero(match, match: {
                    var bbb = builder.body();
                    break :match bbb.finish(index);
                }, no_match: {
                    var bbb = builder.body();
                    const next_index = try bbb.add(index, try bbb.word(1));
                    const result = try bbb.call(rec, try box(ally, .{ rec, type_obj, symbol, next_index }));
                    break :no_match bbb.finish(result);
                });
                break :check bb.finish(result);
            });
            const body = b.finish(result);
            break :fun builder.finish(body);
        });
        const lookup_field_fun = try ir_to_instructions(ally, heap, fun: {
            var builder = Builder.init(ally);
            const type_obj = try builder.param();
            const symbol = try builder.param();
            var b = builder.body();
            const one = try b.word(1);
            const rec = try b.object(lookup_field_fun_rec);
            const result = try b.call(rec, try box(ally, .{ rec, type_obj, symbol, one }));
            const body = b.finish(result);
            break :fun builder.finish(body);
        });

        return .{
            .empty_obj = empty_obj,
            .int_symbol = int_symbol,
            .string_symbol = string_symbol,
            .struct_symbol = struct_symbol,
            .enum_symbol = enum_symbol,
            .lambda_symbol = lambda_symbol,
            .int_type = int_type,
            .string_type = string_type,
            .array_type = array_type,
            .empty_struct = empty_struct,
            .true_ = true_,
            .false_ = false_,
            .equal = equal,
            .greater = greater,
            .less = less,
            .compare_symbols_fun = compare_symbols_fun,
            .lookup_field_fun = lookup_field_fun,
        };
    }
};

pub fn ast_to_ir(ally: Ally, heap: *Heap, common: CommonObjects, env: anytype, ast: Ast.Expr) !Ir {
    return try AstToIr.compile(ally, common, env, ast, heap);
}

const AstToIr = struct {
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
            for (0..bindings.bindings.items.len) |i| {
                const binding = bindings.bindings.items[bindings.bindings.items.len - 1 - i];
                if (std.mem.eql(u8, binding.name, name)) return binding.id;
            }
            std.debug.print("name {s}\n", .{name});
            @panic("name not in scope");
        }
    };
    const Binding = struct { name: Str, id: Id };

    ally: Ally,
    heap: *Heap,
    common: CommonObjects,

    pub fn compile(ally: Ally, common: CommonObjects, env: anytype, expr: Ast.Expr, heap: *Heap) !Ir {
        var compiler = AstToIr{ .ally = ally, .heap = heap, .common = common };
        var bindings = Bindings.init();
        var builder = Builder.init(ally);
        _ = try builder.param();
        var b = builder.body();
        inline for (@typeInfo(@TypeOf(env)).@"struct".fields) |field| {
            const ref = try b.object(@field(env, field.name));
            try bindings.bind(ally, field.name, ref);
        }
        const result = try compiler.compile_expr(expr, &b, &bindings);
        const body_result = b.finish(result);
        return builder.finish(body_result);
    }

    fn compile_expr(self: *AstToIr, expr: Ast.Expr, b: *Ir.BodyBuilder, bindings: *Bindings) error{OutOfMemory}!Id {
        switch (expr) {
            .name => |name| return bindings.get(name),
            .int => |value| return try b.object(try object_mod.new_int(self.heap, value)),
            .string => |string| {
                const symbol_obj = try object_mod.new_symbol(self.heap, string);
                const string_obj = try self.heap.new_pointers(&[_]Word{ self.common.string_type, symbol_obj });
                return b.object(string_obj);
            },
            .let => |let| {
                const id = try self.compile_expr(let.value.*, b, bindings);
                const len = bindings.bindings.items.len;
                try bindings.bind(self.ally, let.name, id);
                const result = try self.compile_expr(let.expr.*, b, bindings);
                bindings.bindings.items.len = len;
                return result;
            },
            .struct_ => |struct_| {
                const field_names = try self.ally.alloc(Address, struct_.fields.len);
                for (0.., struct_.fields) |i, field|
                    field_names[i] = try object_mod.new_symbol(self.heap, field.name);
                const type_ = obj: {
                    var o = try self.heap.object_builder();
                    try o.emit_pointer(self.common.struct_symbol);
                    for (field_names) |field_name| try o.emit_pointer(field_name);
                    break :obj o.finish();
                };
                const fields = try self.ally.alloc(Id, 1 + struct_.fields.len);
                fields[0] = try b.object(type_);
                for (1.., struct_.fields) |i, field| fields[i] = try self.compile_expr(field.value, b, bindings);
                return b.new(true, fields);
            },
            .member => |member| {
                const of = try self.compile_expr(member.of.*, b, bindings);
                _ = try b.assert_is_struct(of, self.heap, "you can only get members of structs");
                const type_ = try b.type_of(of);
                const fun = try b.object(self.common.lookup_field_fun);
                const member_name = try b.object(try object_mod.new_symbol(self.heap, member.name));
                var args = try self.ally.alloc(Id, 2);
                args[0] = type_;
                args[1] = member_name;
                const index = try b.call(fun, args);
                return try b.load(of, index);
            },
            .enum_ => |enum_| {
                const variant_name = try object_mod.new_symbol(self.heap, enum_.variant);
                const type_ = obj: {
                    var o = try self.heap.object_builder();
                    try o.emit_pointer(self.common.enum_symbol);
                    try o.emit_pointer(variant_name);
                    break :obj o.finish();
                };
                const payload = try self.compile_expr(enum_.payload.*, b, bindings);
                const compiled = try self.ally.alloc(Id, 2);
                compiled[0] = try b.object(type_);
                compiled[1] = payload;
                return b.new(true, compiled);
            },
            .switch_ => |switch_| {
                const condition = try self.compile_expr(switch_.condition.*, b, bindings);
                _ = try b.assert_is_enum(condition, self.heap, "you can only switch on enums");
                const type_ = try b.type_of(condition);
                const variant = try b.loadi(type_, 1);
                const result = try self.handle_switch_cases(b, condition, variant, switch_.cases, bindings);
                return result;
            },
            .lambda => |lambda| {
                const num_args_obj = obj: {
                    var o = try self.heap.object_builder();
                    try o.emit_literal(@intCast(lambda.params.len));
                    break :obj o.finish();
                };
                const type_ = obj: {
                    var o = try self.heap.object_builder();
                    try o.emit_pointer(self.common.lambda_symbol);
                    try o.emit_pointer(num_args_obj);
                    break :obj o.finish();
                };
                const captured = try get_captured(self.ally, lambda);
                const ir = ir: {
                    var lambda_bindings = Bindings.init();
                    var lambda_builder = Builder.init(self.ally);
                    for (lambda.params) |param| try lambda_bindings.bind(self.ally, param, try lambda_builder.param());
                    const closure_param = try lambda_builder.param();
                    var lambda_body = lambda_builder.body();
                    for (0.., captured.items) |i, name| {
                        const offset = try lambda_body.word(@intCast(i));
                        const value = try lambda_body.load(closure_param, offset);
                        try lambda_bindings.bind(self.ally, name, value);
                    }
                    const result = try self.compile_expr(lambda.body.*, &lambda_body, &lambda_bindings);
                    const body_result = lambda_body.finish(result);
                    break :ir lambda_builder.finish(body_result);
                };
                const instructions = try b.object(try ir_to_instructions(self.ally, self.heap, ir));
                const closure = closure: {
                    var captured_values = ArrayList(Id).empty;
                    for (captured.items) |name| try captured_values.append(self.ally, try bindings.get(name));
                    break :closure try b.new_closure(captured_values.items);
                };
                const pointers = try b.parent.ally.alloc(Id, 3);
                pointers[0] = try b.object(type_);
                pointers[1] = instructions;
                pointers[2] = closure;
                return try b.new(true, pointers);
            },
            .call => |call| {
                const callee = try self.compile_expr(call.callee.*, b, bindings);
                const zero = try b.word(0);
                const one = try b.word(1);
                const two = try b.word(2);
                const type_ = try b.type_of(callee);
                const kind = try b.kind_of(type_);
                return try b.if_eq_symbol(self.heap, kind, "lambda", is_lambda: {
                    var bb = b.child_body();
                    const num_params = try bb.load(try bb.load(type_, one), zero);
                    const num_args = try bb.word(call.args.len);
                    const result = try bb.if_eq(num_params, num_args, args_fit: {
                        var bbb = bb.child_body();
                        const instructions = try bbb.load(callee, one);
                        const closure = try bbb.load(callee, two);
                        var args = ArrayList(Id).empty;
                        for (call.args) |arg| try args.append(self.ally, try self.compile_expr(arg, &bbb, bindings));
                        try args.append(self.ally, closure);
                        const result = try bbb.call(instructions, args.items);
                        break :args_fit bbb.finish(result);
                    }, args_dont_fit: {
                        var bbb = bb.child_body();
                        break :args_dont_fit try bbb.finish_with_symbol_crash(self.heap, "wrong number of arguments");
                    });
                    break :is_lambda bb.finish(result);
                }, not_lambda: {
                    var bb = b.child_body();
                    break :not_lambda try bb.finish_with_symbol_crash(self.heap, "you can only call lambdas");
                });
            },
        }
    }

    fn handle_switch_cases(
        self: *AstToIr,
        b: *BodyBuilder,
        condition: Id,
        variant: Id,
        cases: []const Ast.Case,
        bindings: *Bindings,
    ) !Id {
        if (cases.len == 0) {
            // TODO: name the variant in the error message
            return try b.crash_with_symbol(self.heap, "unknown variant");
        } else {
            const case = cases[0];
            return try b.if_eq_symbol(self.heap, variant, case.variant, found_it: {
                var bb = b.child_body();
                const len = bindings.bindings.items.len;
                if (case.payload_name) |payload_name| {
                    const payload_value = try bb.loadi(condition, 1);
                    try bindings.bind(self.ally, payload_name, payload_value);
                }
                const result = try self.compile_expr(case.body.*, &bb, bindings);
                bindings.bindings.items.len = len;
                break :found_it bb.finish(result);
            }, not_found_yet: {
                var bb = b.child_body();
                const result = try self.handle_switch_cases(&bb, condition, variant, cases[1..], bindings);
                break :not_found_yet bb.finish(result);
            });
        }
    }

    fn compile_body(self: *AstToIr, exprs: []Ast.Expr, b: *BodyBuilder, bindings: *Bindings) !Id {
        var last: ?Id = null;
        for (exprs) |expr| last = try self.compile_expr(expr, b, bindings);
        return last orelse b.object(self.nil);
    }

    fn get_captured(ally: Ally, lambda: Ast.Lambda) !ArrayList(Str) {
        var ignore = ArrayList(Str).empty;
        var captured = ArrayList(Str).empty;
        for (lambda.params) |param| try ignore.append(ally, param);
        try collect_captured(ally, lambda.body.*, &ignore, &captured);
        return captured;
    }
    fn collect_captured(
        ally: Ally,
        expr: Ast.Expr,
        ignore: *ArrayList(Str),
        out: *ArrayList(Str),
    ) error{OutOfMemory}!void {
        switch (expr) {
            .name => |name| {
                for (ignore.items) |ig| if (std.mem.eql(u8, ig, name)) return;
                for (out.items) |o| if (std.mem.eql(u8, o, name)) return;
                try out.append(ally, name);
            },
            .let => |let| {
                try collect_captured(ally, let.value.*, ignore, out);
                const len = ignore.items.len;
                try ignore.append(ally, let.name);
                try collect_captured(ally, let.expr.*, ignore, out);
                ignore.items.len = len;
            },
            .int => {},
            .string => {},
            .struct_ => |struct_| for (struct_.fields) |field| try collect_captured(ally, field.value, ignore, out),
            .enum_ => |enum_| try collect_captured(ally, enum_.payload.*, ignore, out),
            .member => |member| try collect_captured(ally, member.of.*, ignore, out),
            .switch_ => |switch_| {
                try collect_captured(ally, switch_.condition.*, ignore, out);
                for (switch_.cases) |case| {
                    const len = ignore.items.len;
                    if (case.payload_name) |payload_name| try ignore.append(ally, payload_name);
                    try collect_captured(ally, case.body.*, ignore, out);
                    ignore.items.len = len;
                }
            },
            .lambda => |lambda| {
                const len = ignore.items.len;
                for (lambda.params) |param| try ignore.append(ally, param);
                try collect_captured(ally, lambda.body.*, ignore, out);
                ignore.items.len = len;
            },
            .call => |call| {
                try collect_captured(ally, call.callee.*, ignore, out);
                for (call.args) |arg| try collect_captured(ally, arg, ignore, out);
            },
        }
    }
};

pub const Waffle = struct {
    // The operation gives the children semantic meaning.
    op: Op,
    children: []const Waffle,

    const Op = union(enum) {
        param, // -
        also, // ignored, value
        let: Id, // def, value
        ref: Id, // -
        word: Word, // -
        object: Address, // -
        new: New, // pointers, literals
        flatten_to_pointers_object, // object
        flatten_to_literals_object, // object
        has_pointers, // object
        num_words, // object
        load, // base, offset
        add, // left, right
        subtract, // left, right
        multiply, // left, right
        divide, // left, right
        modulo, // left, right
        shift_left, // left, right
        shift_right, // left, right
        compare, // left, right
        call, // args, instructions
        if_not_zero, // condition, then, else
        crash, // message
    };

    pub const Id = struct { id: usize };
    pub const New = struct { has_pointers: bool };

    pub fn format(waffle: Waffle, writer: *Writer) !void {
        try waffle.format_indented(writer, 0);
    }
    pub fn format_indented(expr: Waffle, writer: *Writer, indentation: usize) error{WriteFailed}!void {
        for (0..indentation) |_| try writer.print("  ", .{});
        try writer.print("{s}", .{@tagName(expr.op)});
        switch (expr.op) {
            inline else => |payload| {
                switch (@TypeOf(payload)) {
                    void => {},
                    Id => try writer.print(" %{}", .{payload.id}),
                    Word => try writer.print(" {x}", .{payload}),
                    New => try writer.print("", .{}),
                    else => @compileError("bad payload " ++ @typeName(@TypeOf(payload))),
                }
            },
        }
        try writer.print("\n", .{});
        for (expr.children) |child| try child.format_indented(writer, indentation + 1);
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
        return .{ .op = .{ .ref = self.mapping.get(ir_id).? }, .children = &[_]Waffle{} };
    }

    fn compile_fun(self: *IrToWaffle, params: []const Ir.Id) !Waffle {
        if (params.len == 0) return try self.compile_body(self.ir.body.ids, self.ir.body.returns);
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
            .word => |word| return .{ .op = .{ .word = word }, .children = &[_]Waffle{} },
            .object => |object| return .{ .op = .{ .object = object }, .children = &[_]Waffle{} },
            .new => |new| {
                const children = try self.ally.alloc(Waffle, new.words.len);
                for (0.., new.words) |i, word| children[i] = self.ref(word);
                return .{
                    .op = .{ .new = .{ .has_pointers = new.has_pointers } },
                    .children = children,
                };
            },
            .flatten_to_pointers_object => |id| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(id);
                return .{ .op = .flatten_to_pointers_object, .children = children };
            },
            .flatten_to_literals_object => |id| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(id);
                return .{ .op = .flatten_to_literals_object, .children = children };
            },
            .has_pointers => |obj| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(obj);
                return .{ .op = .has_pointers, .children = children };
            },
            .num_words => |obj| {
                const children = try self.ally.alloc(Waffle, 1);
                children[0] = self.ref(obj);
                return .{ .op = .num_words, .children = children };
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
            .shift_left => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .shift_left, .children = children };
            },
            .shift_right => |args| {
                const children = try self.ally.alloc(Waffle, 2);
                children[0] = self.ref(args.left);
                children[1] = self.ref(args.right);
                return .{ .op = .shift_right, .children = children };
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
                children[call.args.len] = self.ref(call.instructions);
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
    for (0.., waffle.children) |i, child| children[i] = try optimize_waffle(ally, child);

    switch (waffle.op) {
        .let => |id| {
            const def = children[0];
            const value = children[1];

            const is_param =
                switch (def.op) {
                    .param => true,
                    else => false,
                };

            const is_cheap = switch (def.op) {
                .word => true,
                .object => true,
                else => false,
            };
            if (is_cheap) return try inline_waffle(ally, value, id, def);

            const num_references = count_references(value, id);

            if (num_references == 0 and !is_param) {
                return .{ .op = .also, .children = children };
            }
            // if (num_references == 1 and !is_param) {
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
        else => for (expr.children) |child| try find_usage_before_impure(child, id),
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
            .also => {
                try self.compile(waffle.children[0]);
                try self.emit(.{ .pop = 1 });
                self.stack_size -= 1;
                try self.compile(waffle.children[1]);
            },
            .let => |id| {
                const stack_size = self.stack_size;
                try self.mapping.put(id, stack_size);
                try self.compile(waffle.children[0]);
                try self.compile(waffle.children[1]);
                try self.emit(.{ .popover = 1 });
                self.stack_size -= 1;
            },
            .ref => |id| {
                const offset = self.stack_size - self.mapping.get(id).? - 1;
                try self.emit(.{ .stack = offset });
                self.stack_size += 1;
            },
            .word => |word| {
                try self.emit(.{ .word = word });
                self.stack_size += 1;
            },
            .object => |address| {
                try self.emit(.{ .address = .{ .address = address } });
                self.stack_size += 1;
            },
            .new => |new| {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.{ .new = .{ .has_pointers = new.has_pointers, .num_words = waffle.children.len } });
                self.stack_size -= waffle.children.len;
                self.stack_size += 1;
            },
            .flatten_to_pointers_object => {
                try self.compile(waffle.children[0]);
                try self.emit(.flatptro);
                self.stack_size -= 1;
                self.stack_size += 1;
            },
            .flatten_to_literals_object => {
                try self.compile(waffle.children[0]);
                try self.emit(.flatlito);
                self.stack_size -= 1;
                self.stack_size += 1;
            },
            .has_pointers => {
                try self.compile(waffle.children[0]);
                try self.emit(.points);
                self.stack_size -= 1;
                self.stack_size += 1;
            },
            .num_words => {
                try self.compile(waffle.children[0]);
                try self.emit(.size);
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
            .shift_left => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.shl);
                self.stack_size -= 2;
                self.stack_size += 1;
            },
            .shift_right => {
                for (waffle.children) |child| try self.compile(child);
                try self.emit(.shr);
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
                try self.emit(.{ .@"if" = .{
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

pub fn create_builtins(ally: Ally, heap: *Heap, common: CommonObjects) !Address {
    // "collect_garbage" accepts a lambda that takes zero arguments. It makes a
    // heap checkpoint, calls the lambda, and does a garbage collection, freeing
    // everything that the lambda allocated except the return value.
    const unchecked_collect_garbage = try Instruction.new_instructions(ally, heap, &[_]Instruction{
        // stack: (fun closure)
        .heapsize, // (fun closure checkpoint)
        .{ .stack = 1 }, // (fun closure checkpoint closure)
        .{ .stack = 3 }, // (fun closure checkpoint closure fun)
        .{ .word = 1 }, // (fun closure checkpoint closure fun 1)
        .load, // (fun closure checkpoint closure instructions)
        .eval, // (fun closure checkpoint result)
        .gc, // (fun closure result)
        .{ .popover = 1 }, // (result)
    });
    const collect_garbage = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const lambda = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        const zero = try b.word(0);
        const type_ = try b.type_of(lambda);
        const kind = try b.kind_of(type_);
        const result = try b.if_eq_symbol(heap, kind, "lambda", is_lambda: {
            var bb = b.child_body();
            const num_params = try bb.loadi(try bb.loadi(type_, 1), 0);
            const result = try bb.if_eq(num_params, zero, has_no_args: {
                var bbb = bb.child_body();
                const fun = try bbb.loadi(lambda, 1);
                const closure = try bbb.loadi(lambda, 2);
                const unchecked = try bbb.object(unchecked_collect_garbage);
                const args = try ally.alloc(Ir.Id, 2);
                args[0] = fun;
                args[1] = closure;
                const result = try bbb.call(unchecked, args);
                break :has_no_args bbb.finish(result);
            }, has_args: {
                var bbb = bb.child_body();
                break :has_args try bbb.finish_with_symbol_crash(heap, "expected lambda with zero arguments");
            });
            break :is_lambda bb.finish(result);
        }, not_lambda: {
            var bb = b.child_body();
            break :not_lambda try bb.finish_with_symbol_crash(heap, "expected lambda");
        });
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const crash = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const message = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        const result = try b.crash(message);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const int_add = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const left = try builder.param();
        const right = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_int(left, heap, "you can only add ints");
        _ = try b.assert_is_int(right, heap, "you can only add ints");
        const left_val = try b.get_int_value(left);
        const right_val = try b.get_int_value(right);
        const res_val = try b.add(left_val, right_val);
        const result = try b.new_int(common, res_val);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const int_subtract = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const left = try builder.param();
        const right = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_int(left, heap, "you can only subtract ints");
        _ = try b.assert_is_int(right, heap, "you can only subtract ints");
        const left_val = try b.get_int_value(left);
        const right_val = try b.get_int_value(right);
        const res_val = try b.subtract(left_val, right_val);
        const result = try b.new_int(common, res_val);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const int_multiply = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const left = try builder.param();
        const right = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_int(left, heap, "you can only multiply ints");
        _ = try b.assert_is_int(right, heap, "you can only multiply ints");
        const left_val = try b.get_int_value(left);
        const right_val = try b.get_int_value(right);
        const res_val = try b.multiply(left_val, right_val);
        const result = try b.new_int(common, res_val);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const int_divide = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const left = try builder.param();
        const right = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_int(left, heap, "you can only divide ints");
        _ = try b.assert_is_int(right, heap, "you can only divide ints");
        const left_val = try b.get_int_value(left);
        const right_val = try b.get_int_value(right);
        const res_val = try b.divide(left_val, right_val);
        const result = try b.new_int(common, res_val);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const int_modulo = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const left = try builder.param();
        const right = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_int(left, heap, "you can only modulo ints");
        _ = try b.assert_is_int(right, heap, "you can only modulo ints");
        const left_val = try b.get_int_value(left);
        const right_val = try b.get_int_value(right);
        const res_val = try b.modulo(left_val, right_val);
        const result = try b.new_int(common, res_val);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const int_compare = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const left = try builder.param();
        const right = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_int(left, heap, "you can only compare ints");
        _ = try b.assert_is_int(right, heap, "you can only compare ints");
        const left_val = try b.get_int_value(left);
        const right_val = try b.get_int_value(right);
        const compared = try b.compare(left_val, right_val);
        const result = try b.if_not_zero(compared, not_equal: {
            var bb = b.child_body();
            const variant = try bb.if_eq(compared, try b.word(1), greater: {
                var bbb = bb.child_body();
                const variant = try bbb.object(common.greater);
                break :greater bbb.finish(variant);
            }, less: {
                var bbb = bb.child_body();
                const variant = try bbb.object(common.less);
                break :less bbb.finish(variant);
            });
            break :not_equal bb.finish(variant);
        }, equal: {
            var bb = b.child_body();
            const variant = try bb.object(common.equal);
            break :equal bb.finish(variant);
        });
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const string_get = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const string = try builder.param();
        const index = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_string(string, heap, "you can only get a char from a string");
        _ = try b.assert_is_int(index, heap, "index should be an int");
        const string_val = try b.get_string_value(string);
        const index_val = try b.get_int_value(index);
        const eight = try b.word(8);
        const word_index = try b.divide(index_val, eight);
        const byte_index = try b.modulo(index_val, eight);
        // load(string, index / 8) >> (index % 8 * 8) % 256
        const byte = try b.modulo(
            try b.shift_right(try b.load(string_val, word_index), try b.multiply(byte_index, eight)),
            try b.word(256),
        );
        const result = try b.new_int(common, byte);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const string_from_chars_rec_rec = try ir_to_instructions(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const rec = try builder.param();
        const chars = try builder.param();
        const i = try builder.param();
        const j = try builder.param();
        var b = builder.body();
        const one = try b.word(1);
        const eight = try b.word(8);
        const result = try b.if_eq(j, eight, done: {
            var bb = b.child_body();
            break :done try bb.finish_with_zero();
        }, more_stuff: {
            var bb = b.child_body();
            const len = try bb.subtract(try bb.num_words(chars), one);
            const index = try bb.add(i, j);
            const result = try bb.if_eq(index, len, done: {
                var bbb = bb.child_body();
                break :done try bbb.finish_with_zero();
            }, not_done: {
                var bbb = bb.child_body();
                const the_int = try bbb.load(chars, try bbb.add(index, one));
                _ = try bbb.assert_is_int(the_int, heap, "bad string_from_chars");
                const char = try bbb.get_int_value(the_int);
                const shifted = try bbb.shift_left(char, try bbb.multiply(j, eight));
                const rest = rest: {
                    var args = try ally.alloc(Ir.Id, 4);
                    args[0] = rec;
                    args[1] = chars;
                    args[2] = i;
                    args[3] = try bbb.add(j, one);
                    break :rest try bbb.call(rec, args);
                };
                const result = try bbb.add(shifted, rest);
                break :not_done bbb.finish(result);
            });
            break :more_stuff bb.finish(result);
        });
        const body = b.finish(result);
        break :ir builder.finish(body);
    });
    const string_from_chars_rec = try ir_to_instructions(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const rec = try builder.param();
        const chars = try builder.param();
        const i = try builder.param();
        var b = builder.body();
        const one = try b.word(1);
        const two = try b.word(2);
        const eight = try b.word(8);
        const raw_len = try b.num_words(chars);
        const len = try b.subtract(raw_len, one);
        const i_compared_to_len = try b.compare(i, len);
        const result = try b.if_eq(i_compared_to_len, two, more_to_do: {
            var bb = b.child_body();
            const word = word: {
                const rec_rec = try bb.object(string_from_chars_rec_rec);
                var args = try ally.alloc(Ir.Id, 4);
                args[0] = rec_rec;
                args[1] = chars;
                args[2] = i;
                args[3] = try bb.word(0);
                break :word try bb.call(rec_rec, args);
            };
            const boxed_word = try bb.new(false, try box(ally, .{word}));
            const rest = rest: {
                var args = try ally.alloc(Ir.Id, 3);
                args[0] = rec;
                args[1] = chars;
                args[2] = try bb.add(i, eight);
                break :rest try bb.call(rec, args);
            };
            const result = node: {
                var words = try ally.alloc(Ir.Id, 2);
                words[0] = boxed_word;
                words[1] = rest;
                break :node try bb.new(true, words);
            };
            break :more_to_do bb.finish(result);
        }, done: {
            var bb = b.child_body();
            const result = try bb.new(true, &[_]Ir.Id{});
            break :done bb.finish(result);
        });
        const body = b.finish(result);
        break :ir builder.finish(body);
    });
    const string_from_chars = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const chars = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_array(chars, heap, "bad string_from_chars");
        const one = try b.word(1);
        const seven = try b.word(7);
        const eight = try b.word(8);
        const raw_len = try b.num_words(chars);
        const len = try b.subtract(raw_len, one);
        const num_words = try b.divide(try b.add(len, seven), eight);
        _ = num_words;
        const rec = try b.object(string_from_chars_rec);
        var args = try ally.alloc(Ir.Id, 3);
        args[0] = rec;
        args[1] = chars;
        args[2] = try b.word(0);
        const words = try b.call(rec, args);
        const bytes = try b.flatten_to_literals_object(words);
        const result = try b.new(true, try box(ally, .{ try b.object(common.string_type), bytes }));
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const string_equals = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const left = try builder.param();
        const right = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        _ = try b.assert_is_string(left, heap, "bad string_equals");
        _ = try b.assert_is_string(right, heap, "bad string_equals");
        const is_same = try b.call(try b.object(common.compare_symbols_fun), try box(ally, .{ try b.loadi(left, 1), try b.loadi(right, 1) }));
        const result = try b.if_not_zero(is_same, true_: {
            var bb = b.child_body();
            const result = try bb.object(common.true_);
            break :true_ bb.finish(result);
        }, false_: {
            var bb = b.child_body();
            const result = try bb.object(common.false_);
            break :false_ bb.finish(result);
        });
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    const array_from_linked_list_rec = try ir_to_instructions(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const rec = try builder.param();
        const list = try builder.param();
        var b = builder.body();
        const one = try b.word(1);
        _ = try b.assert_is_enum(list, heap, "array_from_linked_list expects a list");
        const type_ = try b.type_of(list);
        const variant = try b.load(type_, one);
        const result = try b.if_eq_symbol(heap, variant, "empty", empty: {
            var bb = b.child_body();
            const result = try bb.object(common.empty_obj);
            break :empty bb.finish(result);
        }, not_empty: {
            var bb = b.child_body();
            const result = try bb.if_eq_symbol(heap, variant, "more", more: {
                var bbb = b.child_body();
                const payload = try bbb.load(list, one);
                _ = try bbb.assert_is_struct(payload, heap, "bad array_from_linked_list");
                const payload_type = try bbb.type_of(payload);
                const lookup_field = try bbb.object(common.lookup_field_fun);
                const head = head: {
                    var args = try ally.alloc(Ir.Id, 2);
                    args[0] = payload_type;
                    args[1] = try bbb.object(try object_mod.new_symbol(heap, "head"));
                    const index = try bbb.call(lookup_field, args);
                    break :head try bbb.load(payload, index);
                };
                const tail = tail: {
                    var args = try ally.alloc(Ir.Id, 2);
                    args[0] = payload_type;
                    args[1] = try bbb.object(try object_mod.new_symbol(heap, "tail"));
                    const index = try bbb.call(lookup_field, args);
                    break :tail try bbb.load(payload, index);
                };
                const mapped_tail = mapped: {
                    var args = try ally.alloc(Ir.Id, 2);
                    args[0] = rec;
                    args[1] = tail;
                    break :mapped try bbb.call(rec, args);
                };
                const result = obj: {
                    var words = try ally.alloc(Ir.Id, 2);
                    words[0] = head;
                    words[1] = mapped_tail;
                    break :obj try bbb.new(true, words);
                };
                break :more bbb.finish(result);
            }, bad: {
                var bbb = bb.child_body();
                const result = try bbb.crash_with_symbol(heap, "bad array_from_linked_list");
                break :bad bbb.finish(result);
            });
            break :not_empty bb.finish(result);
        });
        const body = b.finish(result);
        break :ir builder.finish(body);
    });
    const array_from_linked_list = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const list = try builder.param();
        _ = try builder.param(); // closure
        var b = builder.body();
        const rec = try b.object(array_from_linked_list_rec);
        const mapped = mapped: {
            var args = try ally.alloc(Ir.Id, 2);
            args[0] = rec;
            args[1] = list;
            break :mapped try b.call(rec, args);
        };
        const extra_node = node: {
            var words = try ally.alloc(Ir.Id, 2);
            words[0] = try b.object(common.array_type);
            words[1] = mapped;
            break :node try b.new(true, words);
        };
        const result = try b.flatten_to_pointers_object(extra_node);
        const body = b.finish(result);
        break :ir builder.finish(body);
    });

    // TODO: fix
    const get_num_words = try ir_to_lambda(ally, heap, ir: {
        var builder = Ir.Builder.init(ally);
        const obj = try builder.param();
        _ = try builder.param(); // closure
        var body = builder.body();
        const num_words = try body.num_words(obj);
        const boxed = try body.new_int(common, num_words);
        const body_ = body.finish(boxed);
        break :ir builder.finish(body_);
    });
    _ = get_num_words;

    const builtins = .{
        .collect_garbage = collect_garbage,
        .crash = crash,
        .add = int_add,
        .subtract = int_subtract,
        .multiply = int_multiply,
        .divide = int_divide,
        .modulo = int_modulo,
        .compare = int_compare,
        .string_get = string_get,
        .string_from_chars = string_from_chars,
        .string_equals = string_equals,
        // .num_words = get_num_words,
        .array_from_linked_list = array_from_linked_list,
    };

    var symbols = [_]Address{undefined} ** @typeInfo(@TypeOf(builtins)).@"struct".fields.len;
    inline for (0.., @typeInfo(@TypeOf(builtins)).@"struct".fields) |i, field| {
        symbols[i] = try object_mod.new_symbol(heap, field.name);
    }
    const struct_symbol = try object_mod.new_symbol(heap, "struct");
    const builtins_type = build: {
        var b = try heap.object_builder();
        try b.emit_pointer(struct_symbol);
        for (symbols) |symbol| try b.emit_pointer(symbol);
        break :build b.finish();
    };
    const builtins_struct = build: {
        var b = try heap.object_builder();
        try b.emit_pointer(builtins_type);
        inline for (@typeInfo(@TypeOf(builtins)).@"struct".fields) |field|
            try b.emit_pointer(@field(builtins, field.name));
        break :build b.finish();
    };

    return builtins_struct;
}

pub fn instructions_to_fun(
    ally: Ally,
    heap: *Heap,
    num_params_: usize,
    instructions: []const Instruction,
) !Address {
    const instructions_obj = try Instruction.new_instructions(ally, heap, instructions);
    return object_mod.new_lambda(heap, .{
        .num_params = num_params_,
        .instructions = instructions_obj,
        .closure = try object_mod.empty_obj(heap),
        .ir = null,
    });
}

pub fn ir_to_instructions(ally: Ally, heap: *Heap, ir: Ir) error{OutOfMemory}!Address {
    //std.debug.print("IR:\n", .{});
    //{
    //    var buffer: [64]u8 = undefined;
    //    const bw = std.debug.lockStderrWriter(&buffer);
    //    defer std.debug.unlockStderrWriter();
    //    ir.format(heap.*, bw) catch unreachable;
    //}
    const waffle = try ir_to_waffle(ally, ir);
    const optimized_waffle = try optimize_waffle(ally, waffle);
    const instructions = try waffle_to_instructions(ally, optimized_waffle);
    const instructions_obj = try Instruction.new_instructions(ally, heap, instructions);
    return instructions_obj;
}

pub fn ir_to_lambda(ally: Ally, heap: *Heap, ir: Ir) error{OutOfMemory}!Address {
    const instructions = try ir_to_instructions(ally, heap, ir);
    return try object_mod.new_lambda(heap, .{
        .num_params = ir.params.len - 1,
        .instructions = instructions,
        .closure = try object_mod.empty_obj(heap),
        .ir = null,
    });

    // const lambda_symbol = try object_mod.new_symbol(heap, "lambda");
    // const num_params_obj = try object_mod.new_int(heap, @intCast(ir.params.len - 1));
    // const type_ = blk: {
    //     var b = try heap.object_builder();
    //     try b.emit_pointer(lambda_symbol);
    //     try b.emit_pointer(num_params_obj);
    //     break :blk b.finish();
    // };
    // const nil = try object_mod.new_nil(heap);
    // var builder = try heap.object_builder();
    // try builder.emit_pointer(type_);
    // try builder.emit_pointer(fun);
    // try builder.emit_pointer(nil);
    // return builder.finish();
}

pub fn code_to_lambda(ally: Ally, heap: *Heap, common: CommonObjects, env: anytype, code: Str) !Address {
    std.debug.print("parsing\n", .{});
    const ast = try str_to_ast(ally, code);
    const ir = try ast_to_ir(ally, heap, common, env, ast);
    return try ir_to_lambda(ally, heap, ir);
}
