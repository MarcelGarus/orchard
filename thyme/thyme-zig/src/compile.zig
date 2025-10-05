const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ast = @import("ast.zig");
const Heap = @import("heap.zig");
const Ir = @import("ir.zig");
const Builder = Ir.Builder;
const BodyBuilder = Ir.BodyBuilder;
const Id = Ir.Id;
const Object = @import("object.zig");

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
    return builder.finish(body.finish(result));
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
            return body.new_enum(
                try Object.new_symbol(heap, enum_.variant),
                try compile_expr(ally, enum_.payload.*, body, bindings, heap),
            );
        },
        .switch_ => |switch_| {
            _ = switch_;
            @panic("switch");
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
