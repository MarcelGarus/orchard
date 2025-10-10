const std = @import("std");
const ArrayList = std.ArrayList;
const Ally = std.mem.Allocator;
const Writer = std.io.Writer;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const ir_to_instructions = @import("ir_to_instructions.zig");
const Object = @import("object.zig");

const Str = []const u8;

nodes: []Node,
params: []Id,
body: Body,

pub const Ir = @This();

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

    fn assert_has_tag(body: *BodyBuilder, obj: Id, tag_: u8) !void {
        _ = try body.if_not_zero(
            try body.subtract(try body.tag(obj), try body.word(tag_)),
            then: {
                var child = body.child_body();
                const zero = try child.word(0);
                break :then try child.finish_with_crash(zero);
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
    pub fn assert_is_int(body: *BodyBuilder, obj: Id) !void {
        try body.assert_has_tag(obj, Object.TAG_INT);
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
    pub fn assert_is_struct(body: *BodyBuilder, obj: Id) !void {
        try body.assert_has_tag(obj, Object.TAG_STRUCT);
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
    pub fn assert_is_lambda(body: *BodyBuilder, obj: Id) !void {
        try body.assert_has_tag(obj, Object.TAG_LAMBDA);
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
