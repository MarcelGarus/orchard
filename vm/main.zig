//! Open a Yam notebook with the compiler embedded by the build.
const std = @import("std");
const App = @import("app.zig");
const Heap = @import("heap.zig");
const Notebook = @import("notebook.zig");
const Vm = @import("vm.zig");

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 2) {
        std.debug.print("Usage: orchard file.yam\n", .{});
        return error.ExpectedNotebookPath;
    }

    const path = args[1];
    const ally = init.gpa;
    const source = try std.Io.Dir.cwd().readFileAlloc(init.io, path, ally, .unlimited);
    defer ally.free(source);
    var heap = try Heap.init(ally, 1_000_000_000);
    var vm = try Vm.Default.init(&heap, ally);
    const app = try Notebook.run_notebook(&vm, @embedFile("compiler_image"), source);
    try App.run(init.io, ally, &vm, app, path);
}
