const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ast = @import("ast.zig");
const Heap = @import("heap.zig");
const Instruction = @import("instruction.zig").Instruction;
const new_instruction = Instruction.new_instruction;
const new_instructions = Instruction.new_instructions;
const Ir = @import("ir.zig");
const Builder = Ir.Builder;
const BodyBuilder = Ir.BodyBuilder;
const Id = Ir.Id;
const Object = @import("object.zig");
const Vm = @import("vm.zig");

const Str = []const u8;

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
        @panic("name not in scope");
    }
};
const Binding = struct { name: Str, id: Id };

pub fn compile(ally: Ally, expr: ast.Expr, heap: *Heap) !Ir {
    var builder = Builder.init(ally);
    var body = builder.body();
    var bindings = Bindings.init();
    const result = try compile_expr(ally, expr, &body, &bindings, heap);
    const body_result = body.finish(result);
    return builder.finish(body_result);
}

fn compile_expr(
    ally: Ally,
    expr: ast.Expr,
    body: *Ir.BodyBuilder,
    bindings: *Bindings,
    heap: *Heap,
) error{OutOfMemory}!Id {
    switch (expr) {
        .name => |name| return bindings.get(name),
        .body => |bod| return compile_body(ally, bod, body, bindings, heap),
        .int => |int| return body.object(try Object.new_int(heap, int)),
        .string => @panic("string"),
        .struct_ => |struct_| {
            var fields = try ally.alloc(Id, struct_.len * 2);
            for (0.., struct_) |i, field| {
                fields[2 * i] = try body.object(try Object.new_symbol(heap, field.name));
                fields[2 * i + 1] = try compile_expr(ally, field.value, body, bindings, heap);
            }
            return try body.new_struct(fields);
        },
        .member => @panic("member"),
        .enum_ => |enum_| {
            const variant = try Object.new_symbol(heap, enum_.variant);
            const payload = try compile_expr(ally, enum_.payload.*, body, bindings, heap);
            return body.new_enum(variant, payload);
        },
        .switch_ => |switch_| {
            const condition = try compile_expr(ally, switch_.condition.*, body, bindings, heap);
            try body.assert_is_enum(condition);
            const variant = try body.get_enum_variant(condition);
            const payload = try body.get_enum_payload(condition);

            const symbol_compare = compare: {
                const rec = try new_instructions(heap, &[_]Instruction{
                    // (rec a b cursor)
                    .{ .push_from_stack = 2 }, // (rec a b cursor a)
                    .num_literals, // (rec a b cursor len)
                    .{ .push_from_stack = 1 }, // (rec a b cursor len cursor)
                    .subtract, // (rec a b cursor more?)
                    .{
                        .if_not_zero = .{
                            .then = try new_instructions(heap, &[_]Instruction{
                                // Not done comparing yet. (rec a b cursor)
                                .{ .push_from_stack = 2 }, // (rec a b cursor a)
                                .{ .push_from_stack = 1 }, // (rec a b cursor a cursor)
                                .load, // (rec a b cursor a_word)
                                .{ .push_from_stack = 2 }, // (rec a b cursor a_word b)
                                .{ .push_from_stack = 2 }, // (rec a b cursor a_word b cursor)
                                .load, // (rec a b cursor a_word b_word)
                                .subtract, // (rec a b cursor diff?)
                                .{
                                    .if_not_zero = .{
                                        .then = try new_instructions(heap, &[_]Instruction{
                                            // This word is different. (rec a b cursor)
                                            .{ .pop = 4 }, // ()
                                            .{ .push_word = 0 }, // (0)
                                        }),
                                        .else_ = try new_instructions(heap, &[_]Instruction{
                                            // This word is the same. (rec a b cursor)
                                            .{ .push_word = 1 }, // (rec a b cursor 1)
                                            .add, // (rec a b cursor')
                                            .{ .push_from_stack = 3 }, // (rec a b cursor')
                                            .eval, // (0) or (1)
                                        }),
                                    },
                                },
                            }),
                            .else_ = try new_instructions(heap, &[_]Instruction{
                                // Done comparing. (rec a b cursor)
                                .{ .pop = 4 }, // ()
                                .{ .push_word = 1 }, // (1)
                            }),
                        },
                    },
                });
                const compare = try new_instructions(heap, &[_]Instruction{
                    // (a b closure)
                    .{ .push_address = rec }, // (a b closure rec)
                    .{ .push_from_stack = 3 }, // (a b closure rec a)
                    .{ .push_from_stack = 3 }, // (a b closure rec a b)
                    .{ .push_word = 0 }, // (a b closure rec a b cursor)
                    .{ .push_address = rec }, // (a b closure rec a b cursor rec)
                    .eval, // (a b closure 0) or (a b closure 1)
                    .{ .pop_below_top = 3 },
                });
                break :compare try Object.new_fun(
                    heap,
                    3,
                    try Object.new_nil(heap),
                    compare,
                );
            };
            var res = child: {
                var child = body.child_body();
                const message = try Object.new_symbol(heap, "no switch case matched");
                const message_ref = try body.object(message);
                break :child try child.finish_with_crash(message_ref);
            };
            for (switch_.cases) |case| {
                const candidate = try Object.new_symbol(heap, case.variant);
                var inner = body.child_body();
                const candidate_ref = try inner.object(candidate);
                const symbol_compare_ref = try inner.object(symbol_compare);
                const args = try ally.alloc(Id, 2);
                args[0] = variant;
                args[1] = candidate_ref;
                const matches = try inner.call(symbol_compare_ref, args);
                const matches_int = try inner.get_int_value(matches);
                const check = try inner.if_not_zero(
                    matches_int,
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
        .lambda => @panic("lambda"),
        .call => |_| @panic("call"),
        .var_ => |var_| {
            const id = try compile_expr(ally, var_.value.*, body, bindings, heap);
            try bindings.bind(ally, var_.name, id);
            return body.object(try Object.new_nil(heap));
        },
    }
}

fn compile_body(ally: Ally, exprs: []ast.Expr, body: *BodyBuilder, bindings: *Bindings, heap: *Heap) !Id {
    var last: ?Id = null;
    for (exprs) |expr|
        last = try compile_expr(ally, expr, body, bindings, heap);
    return last orelse body.object(try Object.new_nil(heap));
}
