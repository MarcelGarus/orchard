// The Pear Interpreter

const std = @import("std");
const testing = std.testing;

pub const Mem = struct {
    ally: std.mem.Allocator,
};

const Number = i64;
const Word = []const u8;

// An evaluted value.
pub const Value = union(enum) {
    number: Number, // 2
    word: Word, // foo
    quoted: *Value, // :foo
    struct_: Struct, // (& x 2 y 3)
    lambda: Lambda, // (\ (a b) (+ a b))
    builtin: Builtin, // @add

    pub const Struct = std.StringHashMap(Value);
    pub const Lambda = struct {
        params: []Word,
        env: Env,
        body: *Code,
    };

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .number => |number| try writer.print("{}", .{number}),
            .word => |word| try writer.print("{s}", .{word}),
            .quoted => |quoted| try writer.print(":{}", .{quoted.*}),
            .struct_ => |struct_| {
                // if (linked_list_to_slice(mem, self)) |slice| {
                //     writer.print("(");
                //     var first = true;
                //     for (slice) |child| {
                //         if (first) first = false else writer.print(" ", .{});
                //         writer.print("{}", .{child});
                //     }
                //     writer.print(")", .{});
                //     return;
                // }
                try writer.print("(&", .{});
                var iterator = struct_.iterator();
                while (iterator.next()) |field| {
                    try writer.print(" {s} {}", .{ field.key_ptr.*, field.value_ptr.* });
                }
                try writer.print(")", .{});
            },
            .lambda => |lambda| {
                try writer.print("(\\", .{});
                for (lambda.params) |param|
                    try writer.print(" {s}", .{param});
                try writer.print(" {})", .{lambda.body});
            },
            .builtin => |builtin| try writer.print("{any}", .{builtin}),
        }
    }
};

const Builtin = enum { equal, crash, add, subtract, multiply, divide };

// Code that can be evaluated to a value using an environment.
const Code = union(enum) {
    value: *Value, // :foo
    word: Word, // foo
    let: Let, // (let a 2 b a (& x a y b))
    struct_: Struct, // (& x 2 y 3)
    lambda: Lambda, // (\ a b (+ a b))
    if_: If,
    apply: Apply, // (a b c)

    const Let = struct { name: Word, value: *Code, body: *Code };
    const Struct = []Field;
    const Field = struct { name: Word, value: Code };
    const Lambda = struct { params: []Word, body: *Code };
    const If = struct { condition: *Code, then: *Code, else_: *Code };
    const Apply = struct { what: *Code, args: []Code };
};

// A mapping from variable names to variables.
const Env = ?*Bindings;
const Bindings = struct { parent: Env, name: Word, value: Value };

fn lookup(env: Env, word: Word) Value {
    std.debug.print("Looking up {s}\n", .{word});
    if (env) |bindings| {
        std.debug.print("Binding is for {*}\n", .{bindings.name});

        return if (std.mem.eql(u8, bindings.name, word))
            bindings.value
        else
            lookup(bindings.parent, word);
    } else {
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

fn linked_list(mem: *Mem, items: []Value) !Value {
    if (items.len == 0) {
        const nil = try mem.ally.create(Value);
        nil.* = .{ .word = "nil" };
        return .{ .quoted = nil };
    } else {
        var fields = std.StringHashMap(Value).init(mem.ally);
        try fields.put("car", items[0]);
        try fields.put("cdr", try linked_list(mem, items[1..]));
        return .{ .struct_ = fields };
    }
}

fn linked_list_to_slice(mem: *Mem, value: Value) ![]Value {
    var out = std.ArrayList(Value).init(mem.ally);
    try linked_list_to_slice_rec(value, &out);
    return out.items;
}
fn linked_list_to_slice_rec(value: Value, out: *std.ArrayList(Value)) !void {
    switch (value) {
        .quoted => |quoted| {
            switch (quoted.*) {
                .word => |word| {
                    if (!std.mem.eql(u8, word, "nil"))
                        return error.NotLinkedList;
                },
                else => {},
            }
        },
        .struct_ => |struct_| {
            const car = struct_.get("car") orelse return error.NotLinkedList;
            const cdr = struct_.get("cdr") orelse return error.NotLinkedList;
            try out.append(car);
            try linked_list_to_slice_rec(cdr, out);
        },
        else => return error.NotLinkedList,
    }
}

// Parsing
//
// Turning a string into a value.

fn parse(mem: *Mem, input: []u8) !Value {
    const Parser = struct {
        mem: *Mem,
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
                var children = std.ArrayList(Value).init(self.mem.ally);
                while (true) try children.append(try self.parse() orelse break);
                if (self.is_at_end()) return error.ListWithoutClosingParen;
                if (self.current() != ')') return error.ExpectedClosingParen;
                self.advance();
                return try linked_list(self.mem, children.items);
            }
            // Quote
            if (self.current() == ':') {
                self.advance();
                const quoted = try self.mem.ally.create(Value);
                quoted.* = try self.parse() orelse return error.ExpectedQuotedValue;
                return .{ .quoted = quoted };
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
                return .{ .number = num };
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
                .{ .word = self.input[start..end] };
        }
    };
    var parser = Parser{ .mem = mem, .input = input, .cursor = 0 };
    return (try parser.parse()) orelse error.NoValue;
}

fn compile(mem: *Mem, value: Value) error{
    NotLinkedList,
    LetWithAnEvenNumberOfArgs,
    ExpectedNameOfLet,
    StructWithUnevenNumberOfArgs,
    FieldNameMustBeWord,
    ParamMustBeWord,
    InvalidIf,
    CompilingLambda,
    CompilingBuiltin,
    OutOfMemory,
}!Code {
    std.debug.print("Compiling {}\n", .{value});
    switch (value) {
        .number => {
            const heaped = try mem.ally.create(Value);
            heaped.* = value;
            return .{ .value = heaped };
        },
        .word => |word| {
            if (std.mem.eql(u8, word, "@equal")) {
                const builtin = mem.ally.create(Value);
                builtin.* = .{ .builtin = .equal };
                return .{ .value = builtin };
            }
            return .{ .word = word };
        },
        .quoted => |quoted| return .{ .value = quoted },
        .struct_ => {
            const items = try linked_list_to_slice(mem, value);
            const what = items[0];
            const args = items[1..];

            switch (what) {
                .word => |word| {
                    if (std.mem.eql(u8, word, "let")) {
                        std.debug.print("let: {any}\n", .{args});
                        if (args.len % 2 == 0) return error.LetWithAnEvenNumberOfArgs;
                        const LetCompiler = struct {
                            fn do(inner_mem: *Mem, let_args: []Value, body: Value) !Code {
                                if (let_args.len == 0) {
                                    return try compile(inner_mem, body);
                                } else {
                                    const name = switch (let_args[0]) {
                                        .word => |name| name,
                                        else => return error.ExpectedNameOfLet,
                                    };
                                    const val = try inner_mem.ally.create(Code);
                                    const bod = try inner_mem.ally.create(Code);
                                    val.* = try compile(inner_mem, let_args[1]);
                                    bod.* = try @This().do(
                                        inner_mem,
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
                            mem,
                            args[0..(args.len - 1)],
                            args[args.len - 1],
                        );
                    }
                    if (std.mem.eql(u8, word, "&")) {
                        var fields = std.ArrayList(Code.Field).init(mem.ally);
                        if (args.len % 2 != 0) return error.StructWithUnevenNumberOfArgs;
                        for (0..(args.len / 2)) |i| {
                            const name = switch (args[i * 2]) {
                                .word => |w| w,
                                else => return error.FieldNameMustBeWord,
                            };
                            const val = try compile(mem, args[i * 2 + 1]);
                            try fields.append(.{ .name = name, .value = val });
                        }
                        return .{ .struct_ = fields.items };
                    }
                    if (std.mem.eql(u8, word, "\\")) {
                        var params = std.ArrayList(Word).init(mem.ally);
                        for (args[0..(args.len - 1)]) |param| {
                            switch (param) {
                                .word => |w| try params.append(w),
                                else => return error.ParamMustBeWord,
                            }
                        }
                        const body = try mem.ally.create(Code);
                        body.* = try compile(mem, args[args.len - 1]);
                        return .{ .lambda = .{
                            .params = params.items,
                            .body = body,
                        } };
                    }
                    if (std.mem.eql(u8, word, "if")) {
                        if (args.len != 3) return error.InvalidIf;
                        const condition = try mem.ally.create(Code);
                        const then = try mem.ally.create(Code);
                        const else_ = try mem.ally.create(Code);
                        condition.* = try compile(mem, args[0]);
                        then.* = try compile(mem, args[1]);
                        else_.* = try compile(mem, args[2]);
                        return .{ .if_ = .{
                            .condition = condition,
                            .then = then,
                            .else_ = else_,
                        } };
                    }
                },
                else => {},
            }

            const compiled_what = try mem.ally.create(Code);
            compiled_what.* = try compile(mem, what);
            var compiled_args = std.ArrayList(Code).init(mem.ally);
            for (args) |arg| try compiled_args.append(try compile(mem, arg));
            return .{ .apply = .{
                .what = compiled_what,
                .args = compiled_args.items,
            } };
        },
        .lambda => return error.CompilingLambda,
        .builtin => return error.CompilingBuiltin,
    }
}

fn eval(mem: *Mem, code: Code, env: Env) !Value {
    std.debug.print("eval with env {*}\n", .{env});

    switch (code) {
        .value => |val| return val.*,
        .word => |word| return lookup(env, word),
        .let => |let| {
            const inner_env = try mem.ally.create(Bindings);
            std.debug.print("Let with env {*} (parent {*})\n", .{ inner_env, env });
            inner_env.* = Bindings{
                .parent = env,
                .name = let.name,
                .value = try eval(mem, let.value.*, env),
            };
            return eval(mem, let.body.*, inner_env);
        },
        .struct_ => |struct_| {
            var fields = std.StringHashMap(Value).init(mem.ally);
            for (struct_) |field|
                try fields.put(
                    field.name,
                    try eval(mem, field.value, env),
                );
            return .{ .struct_ = fields };
        },
        .lambda => |lambda| {
            return .{ .lambda = .{
                .params = lambda.params,
                .env = env,
                .body = lambda.body,
            } };
        },
        .if_ => |if_| {
            switch (try eval(mem, if_.condition.*, env)) {
                .word => |condition| {
                    return if (std.mem.eql(u8, condition, "true"))
                        try eval(mem, if_.then.*, env)
                    else if (std.mem.eql(u8, condition, "false"))
                        try eval(mem, if_.else_.*, env)
                    else
                        error.InvalidCondition;
                },
                else => return error.InvalidCondition,
            }
        },
        .apply => |apply| {
            const what = try eval(mem, apply.what.*, env);
            var args = std.ArrayList(Value).init(mem.ally);
            for (apply.args) |arg| try args.append(try eval(mem, arg, env));

            switch (what) {
                .struct_ => |struct_| {
                    if (args.items.len != 1) return error.InvalidStructAccess;
                    const field = switch (args.items[0]) {
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
                        const new = try mem.ally.create(Bindings);
                        new.* = .{ .parent = inner_env, .name = param, .value = arg };
                        inner_env = new;
                    }
                    return eval(mem, lambda.body.*, inner_env);
                },
                .builtin => |builtin| {
                    switch (builtin) {
                        // .equal => {
                        //     const ops = try two_ints(args.items);
                        //     return .{ .number = ops.left + ops.right };
                        // },
                        .add => {
                            const ops = try two_ints(args.items);
                            return .{ .number = ops.left + ops.right };
                        },
                        .subtract => {
                            const ops = try two_ints(args.items);
                            return .{ .number = ops.left - ops.right };
                        },
                        .multiply => {
                            const ops = try two_ints(args.items);
                            return .{ .number = ops.left * ops.right };
                        },
                        .divide => {
                            const ops = try two_ints(args.items);
                            return .{ .number = @divTrunc(ops.left, ops.right) };
                        },
                    }
                },
                else => return error.InvalidCall,
            }
        },
    }
}
const Ops = struct { left: Number, right: Number };
fn two_ints(values: []Value) !Ops {
    if (values.len != 2) return error.ExpectedTwoInts;
    const left = switch (values[0]) {
        .number => |n| n,
        else => return error.ExpectedTwoInts,
    };
    const right = switch (values[1]) {
        .number => |n| n,
        else => return error.ExpectedTwoInts,
    };
    return .{ .left = left, .right = right };
}

pub fn eval_file(mem: *Mem, path: []const u8) !Value {
    const content = try std.fs.cwd().readFileAlloc(mem.ally, path, 100000);
    const value = try parse(mem, content);
    const code = try compile(mem, value);
    const result = try eval(mem, code, null);

    std.debug.print("Code value: {}\n", .{value});
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
