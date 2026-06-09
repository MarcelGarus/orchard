const std = @import("std");
const object_loader = @import("object_loader.zig");
const Heap = @import("heap.zig");
const Vm = @import("vm.zig");
const Graphics = @import("graphics.zig");
const Value = @import("value.zig");
const Obj = Heap.Obj;
const Ally = std.mem.Allocator;
const Io = std.Io;
const App = @import("app.zig");

const BootstrapStep = struct {
    starttime: Io.Timestamp,
    heap: *Heap,

    fn start(io: Io, comptime name: []const u8, heap: *Heap) !BootstrapStep {
        std.debug.print(name, .{});
        for (name.len..30) |_| std.debug.print(" ", .{});
        const now = Io.Timestamp.now(io, .real);
        return .{ .starttime = now, .heap = heap };
    }
    fn end(self: *BootstrapStep, io: Io) void {
        const now = Io.Timestamp.now(io, .real);
        std.debug.print("{} ms, ", .{now.toMilliseconds() - self.starttime.toMilliseconds()});
        self.heap.dump_stats();
    }
};

fn is_same(a: Obj, b: Obj) bool {
    if (a.address == b.address) return true;
    if (a.is_inner() != b.is_inner()) return false;
    if (a.size() != b.size()) return false;
    if (a.is_inner()) {
        for (a.children(), b.children()) |ac, bc| if (!is_same(ac, bc)) return false;
    } else {
        for (a.words(), b.words()) |aw, bw| if (aw != bw) return false;
    }
    return true;
}
pub fn main(init: std.process.Init) !void {
    // std.debug.print("Orchard!\n", .{});

    const ally = init.gpa;
    const io = init.io;

    // const stdout_buf = try ally.alloc(u8, 1000);
    // const stdin_buf = try ally.alloc(u8, 1000);
    // var stdout_w = Io.File.stdout().writer(io, stdout_buf);
    // var stdin_r = Io.File.stdin().reader(io, stdin_buf);
    // const stdout = &stdout_w.interface;
    // const stdin = &stdin_r.interface;

    var heap = try Heap.init(ally, 300_000_000);
    const start_of_heap = heap.checkpoint();
    var vm = try Vm.Default.init(&heap, ally);

    const objects_code = try Io.Dir.cwd().readFileAlloc(io, "src/bootstrap.objects", ally, Io.Limit.unlimited);
    const olive_code = try Io.Dir.cwd().readFileAlloc(io, "src/bootstrap.olive", ally, Io.Limit.unlimited);
    const pear_code = try Io.Dir.cwd().readFileAlloc(io, "src/bootstrap.pear", ally, Io.Limit.unlimited);

    const compile_olive = step: {
        var step = try BootstrapStep.start(io, "Loading the Olive compiler.", &heap);
        defer step.end(io);
        break :step Value.from(try object_loader.load(ally, &heap, objects_code));
    };
    const olive = step: {
        var step = try BootstrapStep.start(io, "Compiling Olive.", &heap);
        defer step.end(io);
        const result = try compile_olive.call(&vm, &.{
            try Value.new_string(&heap, olive_code),
        });
        break :step Value.from(try vm.garbage_collect(start_of_heap, result.obj));
    };
    const olive_self_hosted = step: {
        var step = try BootstrapStep.start(io, "Self-compiling Olive.", &heap);
        defer step.end(io);
        break :step try olive.field("compile_olive").call(&vm, &.{
            try Value.new_string(&heap, olive_code),
        });
    };
    const olive_self_hosted_2 = step: {
        var step = try BootstrapStep.start(io, "Self-compiling Olive.", &heap);
        defer step.end(io);
        break :step try olive_self_hosted.field("compile_olive").call(&vm, &.{
            try Value.new_string(&heap, olive_code),
        });
    };
    _ = {
        var step = try BootstrapStep.start(io, "Confirming self-hosting.", &heap);
        defer step.end(io);
        if (!is_same(olive_self_hosted.obj, olive_self_hosted_2.obj)) {
            @panic("Not the same.");
        }
    };
    const compile_pear = step: {
        var step = try BootstrapStep.start(io, "Keeping only Pear compiler.", &heap);
        defer step.end(io);
        break :step Value.from(try vm.garbage_collect(start_of_heap, olive_self_hosted_2.field("compile_pear").obj));
    };

    // var defs = std.StringHashMapUnmanaged(Value).empty;
    // while (true) {
    //     try stdout.print(">> ", .{});
    //     try stdout.flush();
    //     const input = std.mem.trim(u8, try stdin.takeDelimiterInclusive('\n'), " \n");
    //     if (std.mem.eql(u8, input, "quit")) break;
    //     if (std.mem.eql(u8, input, "names")) {
    //         var it = defs.iterator();
    //         while (it.next()) |def| {
    //             try stdout.print("- {s}\n", .{def.key_ptr.*});
    //         }
    //         continue;
    //     }
    //     var cursor: usize = 0;
    //     const name_tmp = object_loader.parse_name(input, &cursor) orelse {
    //         try stdout.print("Expected name.", .{});
    //         continue;
    //     };
    //     const name = try ally.alloc(u8, name_tmp.len);
    //     std.mem.copyForwards(u8, name, name_tmp);
    //     const after_name = std.mem.trim(u8, input[cursor..], " \n");
    //     if (after_name.len == 0) {
    //         if (defs.get(name)) |def| {
    //             try stdout.print("{f}\n", .{def});
    //         } else {
    //             try stdout.print("Unknown name {s}.\n", .{name});
    //         }
    //         continue;
    //     }
    //     const value_creator = compile_pear.call(&vm, &.{
    //         try Value.new_string(&heap, after_name),
    //     }) catch |e| {
    //         try stdout.print("Crashed: {any}\n", .{e});
    //         continue;
    //     };
    //     const value = try value_creator.call(&vm, &.{});
    //     // const object = object_loader.parse_obj(input, &cursor, &heap, ally, defs) catch |e| {
    //     //     try stdout.print("Error: {any}\n", .{e});
    //     //     continue;
    //     // } orelse {
    //     //     try stdout.print("Expected expression.\n", .{});
    //     //     continue;
    //     // };
    //     try defs.put(ally, name, value);
    //     try stdout.print("Created {s}:\n{f}\n", .{ name, value });
    // }

    const pear = step: {
        var step = try BootstrapStep.start(io, "Compiling Pear.", &heap);
        defer step.end(io);
        const result = try compile_pear.call(&vm, &.{try Value.new_string(&heap, pear_code)});
        break :step Value.from(try vm.garbage_collect(start_of_heap, result.obj));
    };
    const app = try pear.call(&vm, &.{});
    std.debug.print("app:\n{f}\n", .{app});

    try App.run(ally, io, &heap, &vm, app, "src/data.objects");
}
