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

pub fn compile(ally: Ally, env: anytype, expr: ast.Expr, heap: *Heap) !Ir {
    const common = common: {
        const nil = try Object.new_nil(heap);
        const compare_symbols_rec_fun = try Object.new_fun_from_ir(heap, ir: {
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
        const compare_symbols_fun = try Object.new_fun_from_ir(heap, ir: {
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
        break :common Common{
            .nil = nil,
            .compare_symbols_fun = compare_symbols_fun,
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

const Common = struct { nil: Object, compare_symbols_fun: Object };

fn compile_expr(
    ally: Ally,
    expr: ast.Expr,
    body: *Ir.BodyBuilder,
    bindings: *Bindings,
    heap: *Heap,
    common: Common,
) error{OutOfMemory}!Id {
    switch (expr) {
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
        .member => @panic("member"),
        .enum_ => |enum_| {
            const variant = try Object.new_symbol(heap, enum_.variant);
            const payload = try compile_expr(ally, enum_.payload.*, body, bindings, heap, common);
            return body.new_enum(variant, payload);
        },
        .switch_ => |switch_| {
            const condition = try compile_expr(ally, switch_.condition.*, body, bindings, heap, common);
            try body.assert_is_enum(condition);
            const variant = try body.get_enum_variant(condition);
            const payload = try body.get_enum_payload(condition);

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
                const symbol_compare_ref = try inner.object(common.compare_symbols_fun);
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
        .lambda => @panic("lambda"),
        .call => |_| @panic("call"),
        .var_ => |var_| {
            const id = try compile_expr(ally, var_.value.*, body, bindings, heap, common);
            try bindings.bind(ally, var_.name, id);
            return body.object(common.nil);
        },
    }
}

fn compile_body(
    ally: Ally,
    exprs: []ast.Expr,
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
