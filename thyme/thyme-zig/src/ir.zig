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
        return try body.new(Object.TAG_INT, &[_]Id{}, &[_]Id{value});
    }
    pub fn assert_is_int(body: *BodyBuilder, obj: Id) !void {
        try body.assert_has_tag(obj, Object.TAG_INT);
    }
    pub fn get_int_value(body: *BodyBuilder, int: Id) !Id {
        return try body.load(int, try body.word(1));
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
    pub fn assert_is_enum(body: *BodyBuilder, obj: Id) !void {
        try body.assert_has_tag(obj, Object.TAG_ENUM);
    }
    pub fn get_enum_variant(body: *BodyBuilder, enum_: Id) !Id {
        return try body.load(enum_, try body.word(0));
    }
    pub fn get_enum_payload(body: *BodyBuilder, enum_: Id) !Id {
        return try body.load(enum_, try body.word(1));
    }

    pub fn new_closure(body: *BodyBuilder, captured: []Id) !Id {
        return try body.new(Object.TAG_CLOSURE, captured, &[_]Id{});
    }
    pub fn get_closure_var(body: *BodyBuilder, closure: Id, index: i64) !Id {
        return try body.load(closure, body.word(index));
    }

    pub fn new_lambda(body: *BodyBuilder, num_params: usize, instructions: Id, closure: Id) !Id {
        return try body.new(
            Object.TAG_LAMBDA,
            &[_]Id{ body.word(instructions), closure },
            &[_]Id{body.word(num_params)},
        );
    }
    pub fn assert_is_lambda(body: *BodyBuilder, obj: Id) !void {
        try body.assert_has_tag(obj, Object.TAG_LAMBDA);
    }
    fn get_lambda_instructions(body: *BodyBuilder, lambda: Id) !Id {
        return try body.load(lambda, body.word(1));
    }
    fn get_lambda_closure(body: *BodyBuilder, lambda: Id) !Id {
        return try body.load(lambda, body.word(1));
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

pub fn dump(ir: Ir, indentation: usize) void {
    std.debug.print("code", .{});
    for (ir.params) |param| std.debug.print(" %{}", .{param.index});
    std.debug.print("\n", .{});
    dump_body(ir.body, ir, indentation + 1);
}
fn dump_body(body: Body, ir: Ir, indentation: usize) void {
    for (body.ids) |id| {
        for (0..indentation) |_| std.debug.print("  ", .{});
        std.debug.print("%{} = ", .{id.index});
        dump_node(ir.get(id), ir, indentation);
    }
    for (0..indentation) |_| std.debug.print("  ", .{});
    std.debug.print("%{}\n", .{body.returns.index});
}
fn dump_node(node: Node, ir: Ir, indentation: usize) void {
    switch (node) {
        .param => std.debug.print("param\n", .{}),
        .word => |word| std.debug.print("word {}\n", .{word}),
        .object => |object| std.debug.print("object *{x}\n", .{object.address.address}),
        .new => |new| {
            std.debug.print("new [{x}]", .{new.tag});
            for (new.pointers) |pointer| std.debug.print(" %{}", .{pointer.index});
            for (new.literals) |literal| std.debug.print(" %{}", .{literal.index});
            std.debug.print("\n", .{});
        },
        .tag => |address| std.debug.print("tag %{}\n", .{address.index}),
        .load => |load| std.debug.print("load %{} %{}\n", .{ load.base.index, load.offset.index }),
        .add => |args| std.debug.print("add %{} %{}\n", .{ args.left.index, args.right.index }),
        .subtract => |args| std.debug.print("subtract %{} %{}\n", .{ args.left.index, args.right.index }),
        .multiply => |args| std.debug.print("multiply %{} %{}\n", .{ args.left.index, args.right.index }),
        .divide => |args| std.debug.print("divide %{} %{}\n", .{ args.left.index, args.right.index }),
        .modulo => |args| std.debug.print("modulo %{} %{}\n", .{ args.left.index, args.right.index }),
        .compare => |args| std.debug.print("compare %{} %{}\n", .{ args.left.index, args.right.index }),
        .call => |call| {
            std.debug.print("call %{} with", .{call.fun.index});
            for (call.args) |arg| std.debug.print(" %{}", .{arg.index});
            std.debug.print("\n", .{});
        },
        .if_not_zero => |if_| {
            std.debug.print("if %{} != 0 then\n", .{if_.condition.index});
            dump_body(if_.then, ir, indentation + 1);
            for (0..indentation) |_| std.debug.print("  ", .{});
            std.debug.print("else\n", .{});
            dump_body(if_.else_, ir, indentation + 1);
        },
        .crash => |message| std.debug.print("crash %{}\n", .{message}),
    }
}

pub fn new_ir(heap: *Heap, ir: Ir) !Object {
    _ = heap;
    _ = ir;
    // return switch (ir) {
    //     .push_word => |word| try Object.new_enum(
    //         heap,
    //         "push_word",
    //         try Object.new_int(heap, @bitCast(word)),
    //     ),
    //     .push_address => |object| try Object.new_enum(heap, "push_address", object),
    //     .push_from_stack => |offset| try Object.new_enum(
    //         heap,
    //         "push_from_stack",
    //         try Object.new_int(heap, @intCast(offset)),
    //     ),
    //     .pop => |amount| try Object.new_enum(heap, "pop", try Object.new_int(heap, @intCast(amount))),
    //     .add => try Object.new_enum(heap, "add", try Object.new_nil(heap)),
    //     .subtract => try Object.new_enum(heap, "subtract", try Object.new_nil(heap)),
    //     .multiply => try Object.new_enum(heap, "multiply", try Object.new_nil(heap)),
    //     .divide => try Object.new_enum(heap, "divide", try Object.new_nil(heap)),
    //     .modulo => try Object.new_enum(heap, "modulo", try Object.new_nil(heap)),
    //     .shift_left => try Object.new_enum(heap, "shift_left", try Object.new_nil(heap)),
    //     .shift_right => try Object.new_enum(heap, "shift_right", try Object.new_nil(heap)),
    //     .if_not_zero => |if_| try Object.new_enum(
    //         heap,
    //         "if_not_zero",
    //         try Object.new_struct(heap, .{ .then = if_.then, .@"else" = if_.else_ }),
    //     ),
    //     .new => |new| try Object.new_enum(
    //         heap,
    //         "new",
    //         try Object.new_struct(heap, .{
    //             .tag = try Object.new_int(heap, @intCast(new.tag)),
    //             .num_pointers = try Object.new_int(heap, @intCast(new.num_pointers)),
    //             .num_literals = try Object.new_int(heap, @intCast(new.num_literals)),
    //         }),
    //     ),
    //     .tag => try Object.new_enum(heap, "tag", try Object.new_nil(heap)),
    //     .num_pointers => try Object.new_enum(heap, "num_pointers", try Object.new_nil(heap)),
    //     .num_literals => try Object.new_enum(heap, "num_literals", try Object.new_nil(heap)),
    //     .load => try Object.new_enum(heap, "load", try Object.new_nil(heap)),
    //     .eval => try Object.new_enum(heap, "eval", try Object.new_nil(heap)),
    // };
}
// pub fn new_instructions(heap: *Heap, instructions: []const Instruction) !Object {
//     if (instructions.len == 0) {
//         return try Object.new_nil(heap);
//     } else {
//         return try Object.new_struct(heap, .{
//             .head = try new_instruction(heap, instructions[0]),
//             .tail = try new_instructions(heap, instructions[1..]),
//         });
//     }
// }
