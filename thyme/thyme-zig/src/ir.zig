const std = @import("std");
const ArrayList = std.ArrayList;
const Ally = std.mem.Allocator;

const Heap = @import("heap.zig");
const Object = @import("object.zig");

const Str = []const u8;

nodes: []Node,
params: []Id,
body: Body,

pub const Ir = @This();

pub const Id = struct { index: usize };
pub const Body = struct { ids: []const Id, returns: Id };
pub const Node = union(enum) {
    param,
    word: i64,
    object: Object,
    new: New,
    tag: Id,
    load: Load,
    add: Args,
    subtract: Args,
    multiply: Args,
    divide: Args,
    modulo: Args,
    compare: Args,
    call: Call,
    switch_: Switch,
    crash: Id,
};
pub const New = struct { tag: u8, pointers: []const Id, literals: []const Id };
pub const Load = struct { base: Id, offset: Id };
pub const Args = struct { left: Id, right: Id };
pub const Call = struct { lambda: Id, args: []const Id };
pub const Switch = struct { condition: Id, cases: []const Body };

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
        builder.params.push(id);
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

    pub fn word(body: *BodyBuilder, word_: i64) !Id {
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
    pub fn call(body: *BodyBuilder, lambda: Id, args: []Id) !Id {
        return body.create_and_push(.{ .call = .{ .lambda = lambda, .args = args } });
    }
    pub fn switch_(body: *BodyBuilder, condition: Id, cases: []Body) !Id {
        return body.create_and_push(.{ .switch_ = .{ .condition = condition, .cases = cases } });
    }
    pub fn crash(body: *BodyBuilder, message: Id) !Id {
        return body.create_and_push(.{ .crash = message });
    }

    // Higher-level convenience functions

    fn assert_has_tag(body: *BodyBuilder, obj: Id, tag_: u8) !void {
        body.switch_(
            body.compare(body.tag(obj), body.word(tag_)),
            &[_]Id{
                body.child_body().finish_with_zero(),
                body.child_body().finish_with_crash("assert failed"),
                body.child_body().finish_with_crash("assert failed"),
            },
        ).ignore();
    }

    pub fn new_int(body: *BodyBuilder, value: Id) !Id {
        return try body.new(Object.TAG_INT, &[_]Id{}, &[_]Id{value});
    }
    pub fn get_int_value(body: *BodyBuilder, int: Id) !Id {
        body.load(int, body.word(1));
    }

    pub fn new_struct(body: *BodyBuilder, keys_and_values: []Id) !Id {
        return try body.new(Object.TAG_STRUCT, keys_and_values, &[_]Id{});
    }

    pub fn new_enum(body: *BodyBuilder, variant: Object, payload: Id) !Id {
        return try body.new(
            Object.TAG_ENUM,
            &[_]Id{ try body.object(variant), payload },
            &[_]Id{},
        );
    }

    pub fn new_closure(body: *BodyBuilder, captured: []Id) !Id {
        body.new(Object.TAG_CLOSURE, captured, &[_]Id{});
    }
    pub fn get_closure_var(body: *BodyBuilder, closure: Id, index: i64) !Id {
        body.load(closure, body.word(index));
    }

    pub fn new_lambda(body: *BodyBuilder, num_params: usize, instructions: Id, closure: Id) !Id {
        return try body.new(
            Object.TAG_LAMBDA,
            &[_]Id{ body.word(instructions), closure },
            &[_]Id{body.word(num_params)},
        );
    }
    pub fn assert_is_lambda(body: *BodyBuilder, obj: Id) void {
        body.assert_equals(body.load(obj, body.word(0)), body.word(Object.TAG_LAMBDA));
    }
    fn get_lambda_instructions(body: *BodyBuilder, lambda: Id) !Id {
        body.load(lambda, body.word(1));
    }
    fn get_lambda_closure(body: *BodyBuilder, lambda: Id) !Id {
        body.load(lambda, body.word(1));
    }

    pub fn finish_with_zero(body: BodyBuilder) Body {
        body.finish(body.word(0));
    }
    pub fn finish_with_crash(body: BodyBuilder, message: Str) Body {
        body.finish(body.crash(body.new_string(message)));
    }
};

pub fn dump(ir: Ir, indentation: usize) void {
    std.debug.print("code", .{});
    for (ir.params) |param| std.debug.print(" %{}", .{param.index});
    dump_body(ir.body, ir, indentation + 1);
}
fn dump_body(body: Body, ir: Ir, indentation: usize) void {
    for (body.ids) |id| {
        std.debug.print("\n", .{});
        for (0..indentation) |_| std.debug.print("  ", .{});
        std.debug.print("%{} = ", .{id.index});
        dump_node(ir.get(id), ir, indentation);
    }
    std.debug.print("\n", .{});
    for (0..indentation) |_| std.debug.print("  ", .{});
    std.debug.print("%{}\n", .{body.returns.index});
}
fn dump_node(node: Node, ir: Ir, indentation: usize) void {
    switch (node) {
        .param => std.debug.print("param", .{}),
        .word => |word| std.debug.print("word {}", .{word}),
        .object => |object| std.debug.print("object *{x}", .{object.address.address}),
        .new => |new| {
            std.debug.print("new [{x}]", .{new.tag});
            for (new.pointers) |pointer| std.debug.print(" %{}", .{pointer.index});
            for (new.literals) |literal| std.debug.print(" %{}", .{literal.index});
        },
        .tag => |address| std.debug.print("tag %{}", .{address.index}),
        .load => |load| std.debug.print("load %{} %{}", .{ load.base.index, load.offset.index }),
        .add => |args| std.debug.print("add %{} %{}", .{ args.left.index, args.right.index }),
        .subtract => |args| std.debug.print("subtract %{} %{}", .{ args.left.index, args.right.index }),
        .multiply => |args| std.debug.print("multiply %{} %{}", .{ args.left.index, args.right.index }),
        .divide => |args| std.debug.print("divide %{} %{}", .{ args.left.index, args.right.index }),
        .modulo => |args| std.debug.print("modulo %{} %{}", .{ args.left.index, args.right.index }),
        .compare => |args| std.debug.print("compare %{} %{}", .{ args.left.index, args.right.index }),
        .call => |call| {
            std.debug.print("call %{} with", .{call.lambda.index});
            for (call.args) |arg| std.debug.print(" %{}", .{arg.index});
        },
        .switch_ => |switch_| {
            std.debug.print("switch %{}", .{switch_.condition.index});
            for (0.., switch_.cases) |i, case| {
                std.debug.print("\n", .{});
                for (0..indentation) |_| std.debug.print("  ", .{});
                std.debug.print("  case {}", .{i});
                dump_body(case, ir, indentation + 2);
            }
        },
        .crash => |message| std.debug.print("crash %{}", .{message}),
    }
}
