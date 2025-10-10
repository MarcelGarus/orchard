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
        std.debug.print("name {s}\n", .{name});
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
        .member => @panic("member"),
        .switch_ => |switch_| {
            const condition = try compile_expr(ally, switch_.condition.*, body, bindings, heap, common);
            try body.assert_is_struct(condition);
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
            const lambda_fun = try body.object(try Object.new_fun_from_ir(heap, lambda_ir));
            return body.new_lambda(lambda_fun, closure);
        },
        .call => |call| {
            const callee = try compile_expr(ally, call.callee.*, body, bindings, heap, common);
            var args = ArrayList(Id).empty;
            for (call.args) |arg|
                try args.append(ally, try compile_expr(ally, arg, body, bindings, heap, common));
            try body.assert_is_lambda(callee);
            const fun = try body.get_lambda_fun(callee);
            const closure = try body.get_lambda_closure(callee);
            try args.append(ally, closure);
            return try body.call(fun, args.items);
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

fn get_captured(ally: Ally, lambda: ast.Lambda) !ArrayList(Str) {
    var ignore = ArrayList(Str).empty;
    var captured = ArrayList(Str).empty;
    for (lambda.params) |param| try ignore.append(ally, param);
    try collect_captured(ally, lambda.body.*, &ignore, &captured);
    return captured;
}
fn collect_captured_in_new_scope(
    ally: Ally,
    expr: ast.Expr,
    ignore: *ArrayList(Str),
    out: *ArrayList(Str),
) error{OutOfMemory}!void {
    const num_ignored = ignore.items.len;
    try collect_captured(ally, expr, ignore, out);
    ignore.items.len = num_ignored;
}
fn collect_captured(
    ally: Ally,
    expr: ast.Expr,
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
