//! Orchard runs .objects files headlessly by default; --ui opens the returned app.
const std = @import("std");
const Heap = @import("heap.zig");
const Vm = @import("vm.zig");
const object_loader = @import("object_loader.zig");
const Obj = Heap.Obj;

const Backend = enum { default, jit, @"byte-code", @"tree-walk" };

pub fn main(init: std.process.Init) !void {
    const all_args = try init.minimal.args.toSlice(init.arena.allocator());
    const args = all_args[1..];
    var ui = false;
    var backend: Backend = .default;
    var path: ?[]const u8 = null;
    var at: usize = 0;
    while (at < args.len) : (at += 1) {
        if (std.mem.eql(u8, args[at], "--ui")) {
            if (ui) return error.InvalidArguments;
            ui = true;
        } else if (std.mem.eql(u8, args[at], "--help")) {
            print_usage();
            return;
        } else if (std.mem.eql(u8, args[at], "--backend")) {
            at += 1;
            if (at == args.len) return error.MissingBackend;
            backend = std.meta.stringToEnum(Backend, args[at]) orelse return error.UnknownBackend;
        } else if (std.mem.startsWith(u8, args[at], "--")) {
            print_usage();
            return error.InvalidArguments;
        } else if (path == null) {
            path = args[at];
        } else return error.InvalidArguments;
    }
    const input = path orelse {
        print_usage();
        return error.MissingInput;
    };
    const ally = if (ui) init.gpa else init.arena.allocator();
    const code = try std.Io.Dir.cwd().readFileAlloc(init.io, input, ally, .unlimited);
    var heap = try Heap.init(ally, if (ui) 600_000_000 else 32_000_000);
    const root = try object_loader.load(ally, &heap, code);
    if (!root.is_inner() or root.size() != 2 or !root.child(0).is_inner() or root.child(0).size() != 0)
        return error.ExpectedParameterlessFunction;
    switch (backend) {
        .default => {
            var vm = try Vm.Default.init(&heap, ally);
            return run(init, &vm, root, ui);
        },
        .jit => {
            if (comptime Vm.Jit != void) {
                var vm = try Vm.Jit.init(&heap, ally);
                return run(init, &vm, root, ui);
            } else return error.JitUnavailable;
        },
        .@"byte-code" => {
            var vm = try Vm.ByteCode.init(&heap, ally);
            return run(init, &vm, root, ui);
        },
        .@"tree-walk" => {
            var vm = try Vm.TreeWalk.init(&heap, ally, .{});
            return run(init, &vm, root, ui);
        },
    }
}

fn print_usage() void {
    std.debug.print("Usage: orchard [--ui] [--backend default|jit|byte-code|tree-walk] file.objects\n", .{});
}

fn run(init: std.process.Init, vm: anytype, root: Obj, ui: bool) !void {
    const ally = vm.ally;
    var fuel: usize = Vm.max_fuel;
    const result = try vm.call(.{ .obj = root }, &.{}, &fuel);
    switch (result) {
        .returned => |value| {
            if (ui) return @import("app.zig").run(ally, vm, value);
            var buffer: [4096]u8 = undefined;
            var file_writer = std.Io.File.stdout().writer(init.io, &buffer);
            const writer = &file_writer.interface;
            try value.format_singleline(false, writer);
            try writer.writeByte('\n');
            try writer.flush();
        },
        .crashed => |message| {
            if (message.is_leaf()) std.debug.print("{s}\n", .{Heap.get_symbol(message)}) else std.debug.print("Program crashed: {f}\n", .{message});
            return error.ProgramCrashed;
        },
        .out_of_memory => return error.VmOutOfMemory,
        .out_of_fuel => return error.VmOutOfFuel,
    }
}
