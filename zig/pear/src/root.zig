// The Pear Interpreter

const std = @import("std");
const testing = std.testing;

pub const World = struct {
    ally: std.mem.Allocator,
    words: std.ArrayList([]const u8),
    struct_types: std.ArrayList(StructType),
    values: std.ArrayList(ValueData),
    word_nil: Value,
    word_car: Value,
    word_cdr: Value,

    const Self = @This();

    pub fn init(ally: std.mem.Allocator) Self {
        var world = World{
            .ally = ally,
            .words = std.ArrayList([]const u8).init(ally),
            .values = std.ArrayList(ValueData).init(ally),
        };
        world.nil = world.word("nil");
        world.car = world.word("car");
        world.cdr = world.word("cdr");
        return world;
    }

    fn create_value(self: *Self, data: ValueData) !Value {
        const index = self.values.items.len;
        try self.values.append(data);
        return .{ .index = index };
    }
    fn get_value(self: Self, id: Value) ValueData {
        return self.values.items[id.index];
    }
    fn format(writer: anytype) void {
        @panic("write value");
    }
    fn eql(a: Value, b: Value) bool {
        @panic("eql");
    }

    fn number(self: *Self, the_number: Number) !Value {
        return self.create_value(.{ .number = the_number });
    }
    fn word(self: *Self, the_word: []const u8) !Value {
        for (self.words.items, 0..) |w, i|
            if (std.mem.eql(u8, w, the_word)) return .{ .index = i };
        const index = self.words.items.len;
        try self.words.append(the_word);
        return self.create_value(.{ .word = index });
    }
    fn quoted(self: *Self, the_value: Value) !Value {
        return self.create_value(.{ .quoted = the_value });
    }
    fn struct_(self: *Self, fields: std.AutoHashMap(WordIndex, Value)) !Value {
        return self.create_value(.{ .struct_ = fields });
    }
    fn lambda(self: *Self, the_lambda: ValueData.Lambda) !Value {
        return self.create_value(.{ .lambda = the_lambda });
    }
    fn builtin(self: *Self, the_builtin: Builtin) !Value {
        return self.create_value(.{ .builtin = the_builtin });
    }
};

const WordIndex = usize; // index into the World.words
const Value = struct {
    index: usize, // index into the World.values

    fn eql(a: Value, b: Value) bool {
        return a.index == b.index;
    }
};

const Number = i64;

const StructType = struct {};
pub const Struct = struct {
    type_: usize, // index into Mem.struct_types
    fields: []Value,
};

pub const Lambda = struct {
    params: []WordIndex,
    env: Env,
    body: *Code,
};

// An evaluted value.
pub const ValueData = union(enum) {
    number: Number, // 2
    word: WordIndex, // foo
    quoted: Value, // :foo
    struct_: Struct, // (& x 2 y 3)
    lambda: Lambda, // (\ (a b) (+ a b))
    builtin: Builtin, // @add

    // pub fn format(
    //     self: Value,
    //     comptime fmt: []const u8,
    //     options: std.fmt.FormatOptions,
    //     writer: anytype,
    // ) !void {
    //     _ = fmt;
    //     _ = options;

    //     switch (self) {
    //         .number => |number| try writer.print("{}", .{number}),
    //         .word => |word| try writer.print("{s}", .{word}),
    //         .quoted => |quoted| try writer.print(":{}", .{quoted.*}),
    //         .struct_ => |struct_| {
    //             // if (linked_list_to_slice(world, self)) |slice| {
    //             //     writer.print("(");
    //             //     var first = true;
    //             //     for (slice) |child| {
    //             //         if (first) first = false else writer.print(" ", .{});
    //             //         writer.print("{}", .{child});
    //             //     }
    //             //     writer.print(")", .{});
    //             //     return;
    //             // }
    //             try writer.print("(&", .{});
    //             var iterator = struct_.iterator();
    //             while (iterator.next()) |field| {
    //                 try writer.print(" {s} {}", .{ field.key_ptr.*, field.value_ptr.* });
    //             }
    //             try writer.print(")", .{});
    //         },
    //         .lambda => |lambda| {
    //             try writer.print("(\\", .{});
    //             for (lambda.params) |param|
    //                 try writer.print(" {s}", .{param});
    //             try writer.print(" {})", .{lambda.body});
    //         },
    //         .builtin => |builtin| try writer.print("{any}", .{builtin}),
    //     }
    // }
};

const Builtin = enum { equal, crash, add, subtract, multiply, divide };

// Code that can be evaluated to a value using an environment.
const Code = union(enum) {
    value: Value, // :foo
    word: WordIndex, // foo
    let: Let, // (let a 2 b a (& x a y b))
    struct_: Struct, // (& x 2 y 3)
    lambda: Lambda, // (\ a b (+ a b))
    if_: If,
    apply: Apply, // (a b c)

    const Let = struct { name: WordIndex, value: *Code, body: *Code };
    const Struct = []Field;
    const Field = struct { name: WordIndex, value: Code };
    const Lambda = struct { params: []WordIndex, body: *Code };
    const If = struct { condition: *Code, then: *Code, else_: *Code };
    const Apply = struct { what: *Code, args: []Code };
};

// A mapping from variable names to variables.
const Env = ?*Bindings;
const Bindings = struct { parent: Env, name: WordIndex, value: Value };

fn lookup(env: Env, word: WordIndex) Value {
    if (env) |bindings| {
        return if (bindings.name.eql(word))
            bindings.value
        else
            lookup(bindings.parent, word);
    } else {
        std.debug.print("Looking up {any}", word);
        @panic("value not defined");
    }
}

// Linked Lists
//
// Linked lists are values that adhere to a special structure:
//
// - they are nil to indicate an empty list or
// - they are a struct with these fields:
//   - "car" containing the current item
//   - "cdr" containing the rest of the list

fn linked_list(world: *World, items: []Value) !Value {
    if (items.len == 0) {
        return try world.quoted(world.nil);
    } else {
        var fields = std.AutoHashMap(WordIndex, Value).init(world.ally);
        try fields.put(world.car, items[0]);
        try fields.put(world.cdr, try linked_list(world, items[1..]));
        return try world.struct_(fields);
    }
}

fn linked_list_to_slice(world: *World, value: Value) ![]Value {
    var out = std.ArrayList(Value).init(world.ally);
    try linked_list_to_slice_rec(world, value, &out);
    return out.items;
}
fn linked_list_to_slice_rec(world: *World, value: Value, out: *std.ArrayList(Value)) !void {
    switch (world.get_value(value)) {
        .quoted => |quoted| {
            switch (world.get_value(quoted)) {
                .word => |word| {
                    if (!word.eql(world.nil)) return error.NotLinkedList;
                },
                else => {},
            }
        },
        .struct_ => |struct_| {
            const car = struct_.get(try world.word("car")) orelse return error.NotLinkedList;
            const cdr = struct_.get(try world.word("cdr")) orelse return error.NotLinkedList;
            try out.append(car);
            try linked_list_to_slice_rec(world, cdr, out);
        },
        else => return error.NotLinkedList,
    }
}

// Parsing
//
// Turning a string into a value.

fn parse(world: *World, input: []u8) !Value {
    const Parser = struct {
        world: *World,
        input: []u8,
        cursor: usize,

        const Self = @This();
        fn is_at_end(self: Self) bool {
            return self.cursor == self.input.len;
        }
        fn current(self: Self) u8 {
            return self.input[self.cursor];
        }
        fn advance(self: *Self) void {
            self.cursor += 1;
        }
        fn is_whitespace(c: u8) bool {
            return c == ' ' or c == '\t' or c == '\n';
        }
        fn consume_whitespace(self: *Self) void {
            while (!self.is_at_end()) {
                if (self.current() == '#') {
                    while (!self.is_at_end() and self.current() != '\n') {
                        self.advance();
                    }
                }
                if (self.is_at_end()) break;
                if (!is_whitespace(self.current())) break;
                self.advance();
            }
        }
        fn parse(self: *Self) !?Value {
            self.consume_whitespace();
            if (self.is_at_end()) return null;
            if (self.current() == ')') return null;
            // List
            if (self.current() == '(') {
                self.advance();
                var children = std.ArrayList(Value).init(self.world.ally);
                while (true) try children.append(try self.parse() orelse break);
                if (self.is_at_end()) return error.ListWithoutClosingParen;
                if (self.current() != ')') return error.ExpectedClosingParen;
                self.advance();
                return try linked_list(self.world, children.items);
            }
            // Quote
            if (self.current() == ':') {
                self.advance();
                return try self.world.value(.{
                    .quoted = try self.parse() orelse return error.ExpectedQuotedValue,
                });
            }
            // Number
            if (self.current() >= '0' and self.current() <= '9') {
                var num: Number = 0;
                while (!self.is_at_end()) {
                    const digit = switch (self.current()) {
                        '0'...'9' => self.current() - '0',
                        else => break,
                    };
                    num = num * 10 + digit;
                    self.advance();
                }
                return try self.world.value(.{ .number = num });
            }
            // Word
            const start = self.cursor;
            while (true) {
                self.advance();
                if (self.is_at_end()) break;
                if (self.current() == '(') break;
                if (self.current() == ')') break;
                if (self.current() == '#') break;
                if (is_whitespace(self.current())) break;
            }
            const end = self.cursor;
            return if (start == end)
                null
            else
                try self.world.value(.{
                    .word = try self.world.word(self.input[start..end]),
                });
        }
    };
    var parser = Parser{ .world = world, .input = input, .cursor = 0 };
    return (try parser.parse()) orelse error.NoValue;
}

fn compile(world: *World, value: Value) error{
    NotLinkedList,
    LetWithAnEvenNumberOfArgs,
    ExpectedNameOfLet,
    StructWithUnevenNumberOfArgs,
    FieldNameMustBeWord,
    ParamMustBeWord,
    InvalidIf,
    CompilingLambda,
    CompilingBuiltin,
    OutOfWorldory,
}!Code {
    std.debug.print("Compiling {any}\n", .{value});
    switch (world.get_value(value)) {
        .number => return .{ .value = value },
        .word => |word| {
            if (word.eql(try world.word("@equal"))) {
                const builtin = try world.value(.{ .builtin = .equal });
                return .{ .value = builtin };
            }
            return .{ .word = word };
        },
        .quoted => |quoted| return .{ .value = quoted },
        .struct_ => {
            const items = try linked_list_to_slice(world, value);
            const what = items[0];
            const args = items[1..];

            switch (world.get_value(what)) {
                .word => |word| {
                    if (word.eql(try world.word("let"))) {
                        std.debug.print("let: {any}\n", .{args});
                        if (args.len % 2 == 0) return error.LetWithAnEvenNumberOfArgs;
                        const LetCompiler = struct {
                            fn do(inner_world: *World, let_args: []Value, body: Value) !Code {
                                if (let_args.len == 0) {
                                    return try compile(inner_world, body);
                                } else {
                                    const name = switch (inner_world.get_value(let_args[0])) {
                                        .word => |name| name,
                                        else => return error.ExpectedNameOfLet,
                                    };
                                    const val = try inner_world.ally.create(Code);
                                    const bod = try inner_world.ally.create(Code);
                                    val.* = try compile(inner_world, let_args[1]);
                                    bod.* = try @This().do(
                                        inner_world,
                                        let_args[2..],
                                        body,
                                    );
                                    return .{ .let = .{
                                        .name = name,
                                        .value = val,
                                        .body = bod,
                                    } };
                                }
                            }
                        };
                        return LetCompiler.do(
                            world,
                            args[0..(args.len - 1)],
                            args[args.len - 1],
                        );
                    }
                    if (word.eql(try world.word("&"))) {
                        var fields = std.ArrayList(Code.Field).init(world.ally);
                        if (args.len % 2 != 0) return error.StructWithUnevenNumberOfArgs;
                        for (0..(args.len / 2)) |i| {
                            const name = switch (world.get_value(args[i * 2])) {
                                .word => |w| w,
                                else => return error.FieldNameMustBeWord,
                            };
                            const val = try compile(world, args[i * 2 + 1]);
                            try fields.append(.{ .name = name, .value = val });
                        }
                        return .{ .struct_ = fields.items };
                    }
                    if (word.eql(try world.word("\\"))) {
                        var params = std.ArrayList(WordIndex).init(world.ally);
                        for (args[0..(args.len - 1)]) |param| {
                            switch (world.get_value(param)) {
                                .word => |w| try params.append(w),
                                else => return error.ParamMustBeWord,
                            }
                        }
                        const body = try world.ally.create(Code);
                        body.* = try compile(world, args[args.len - 1]);
                        return .{ .lambda = .{
                            .params = params.items,
                            .body = body,
                        } };
                    }
                    if (word.eql(try world.word("if"))) {
                        if (args.len != 3) return error.InvalidIf;
                        const condition = try world.ally.create(Code);
                        const then = try world.ally.create(Code);
                        const else_ = try world.ally.create(Code);
                        condition.* = try compile(world, args[0]);
                        then.* = try compile(world, args[1]);
                        else_.* = try compile(world, args[2]);
                        return .{ .if_ = .{
                            .condition = condition,
                            .then = then,
                            .else_ = else_,
                        } };
                    }
                },
                else => {},
            }

            const compiled_what = try world.ally.create(Code);
            compiled_what.* = try compile(world, what);
            var compiled_args = std.ArrayList(Code).init(world.ally);
            for (args) |arg| try compiled_args.append(try compile(world, arg));
            return .{ .apply = .{
                .what = compiled_what,
                .args = compiled_args.items,
            } };
        },
        .lambda => return error.CompilingLambda,
        .builtin => return error.CompilingBuiltin,
    }
}

fn eval(world: *World, code: Code, env: Env) !Value {
    std.debug.print("eval with env {*}\n", .{env});

    switch (code) {
        .value => |val| return val,
        .word => |word| return lookup(env, word),
        .let => |let| {
            const inner_env = try world.ally.create(Bindings);
            // std.debug.print("Let with env {*} (parent {*})\n", .{ inner_env, env });
            inner_env.* = Bindings{
                .parent = env,
                .name = let.name,
                .value = try eval(world, let.value.*, env),
            };
            return eval(world, let.body.*, inner_env);
        },
        .struct_ => |struct_| {
            var fields = std.AutoHashMap(WordIndex, Value).init(world.ally);
            for (struct_) |field|
                try fields.put(
                    field.name,
                    try eval(world, field.value, env),
                );
            return world.value(.{ .struct_ = fields });
        },
        .lambda => |lambda| {
            return world.value(.{ .lambda = .{
                .params = lambda.params,
                .env = env,
                .body = lambda.body,
            } });
        },
        .if_ => |if_| {
            switch (world.get_value(try eval(world, if_.condition.*, env))) {
                .word => |condition| {
                    return if (condition.eql(try world.word("true")))
                        try eval(world, if_.then.*, env)
                    else if (condition.eql(try world.word("false")))
                        try eval(world, if_.else_.*, env)
                    else
                        error.InvalidCondition;
                },
                else => return error.InvalidCondition,
            }
        },
        .apply => |apply| {
            const what = try eval(world, apply.what.*, env);
            var args = std.ArrayList(Value).init(world.ally);
            for (apply.args) |arg| try args.append(try eval(world, arg, env));

            switch (world.get_value(what)) {
                .struct_ => |struct_| {
                    if (args.items.len != 1) return error.InvalidStructAccess;
                    const field = switch (world.get_value(args.items[0])) {
                        .word => |word| word,
                        else => return error.InvalidStructAccess,
                    };
                    return struct_.get(field) orelse return error.FieldDoesNotExist;
                },
                .lambda => |lambda| {
                    if (lambda.params.len != args.items.len)
                        return error.MismatchedParams;
                    var inner_env = env;
                    for (lambda.params, args.items) |param, arg| {
                        const new = try world.ally.create(Bindings);
                        new.* = .{ .parent = inner_env, .name = param, .value = arg };
                        inner_env = new;
                    }
                    return eval(world, lambda.body.*, inner_env);
                },
                .builtin => |builtin| {
                    switch (builtin) {
                        .equal => {
                            @panic("equal");
                        },
                        .crash => {
                            @panic("crash");
                        },
                        .add => {
                            const ops = try two_ints(world, args.items);
                            return world.value(.{ .number = ops.left + ops.right });
                        },
                        .subtract => {
                            const ops = try two_ints(world, args.items);
                            return world.value(.{ .number = ops.left - ops.right });
                        },
                        .multiply => {
                            const ops = try two_ints(world, args.items);
                            return world.value(.{ .number = ops.left * ops.right });
                        },
                        .divide => {
                            const ops = try two_ints(world, args.items);
                            return world.value(.{ .number = @divTrunc(ops.left, ops.right) });
                        },
                    }
                },
                else => return error.InvalidCall,
            }
        },
    }
}
const Ops = struct { left: Number, right: Number };
fn two_ints(world: *World, values: []Value) !Ops {
    if (values.len != 2) return error.ExpectedTwoInts;
    const left = switch (world.get_value(values[0])) {
        .number => |n| n,
        else => return error.ExpectedTwoInts,
    };
    const right = switch (world.get_value(values[1])) {
        .number => |n| n,
        else => return error.ExpectedTwoInts,
    };
    return .{ .left = left, .right = right };
}

pub fn eval_file(world: *World, path: []const u8) !Value {
    const content = try std.fs.cwd().readFileAlloc(world.ally, path, 100000);
    const value = try parse(world, content);
    const code = try compile(world, value);
    const result = try eval(world, code, null);

    std.debug.print("Code value: {any}\n", .{value});
    // todo("eval")
    // code.fill(
    //   "builtins",
    //   Value.struct_(map(
    //     "add" -> Value.builtin(Builtin.add),
    //     "subtract" -> Value.builtin(Builtin.subtract),
    //     "multiply" -> Value.builtin(Builtin.multiply),
    //     "divide" -> Value.builtin(Builtin.divide),
    //   ))
    // ).eval()
    // _ = code;
    return result;
}
