//! Build and verify the compiler image on the host VM.
const std = @import("std");
const Heap = @import("heap.zig");
const Vm = @import("vm.zig");
const Notebook = @import("notebook.zig");
const loader = @import("object_loader.zig");

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 4) return error.ExpectedSeedSourceAndOutput;
    const ally = std.heap.smp_allocator;
    const dir = std.Io.Dir.cwd();
    const seed = try dir.readFileAlloc(init.io, args[1], ally, .unlimited);
    const source = try dir.readFileAlloc(init.io, args[2], ally, .unlimited);
    var heap = try Heap.init(ally, 256_000_000);
    var vm = if (comptime Vm.Default == Vm.Jit)
        try Vm.Default.init_sized(&heap, ally, 8_000_000, 8_000_000)
    else
        try Vm.Default.init(&heap, ally);

    const library = try build(&vm, seed, source);
    var image = std.Io.Writer.Allocating.init(ally);
    defer image.deinit();
    try Heap.export_objects(library, ally, &image.writer);

    // Exercise the same serialized library that Orchard will load.
    const reloaded = try loader.load(init.arena.allocator(), &heap, image.written());
    _ = try Notebook.lookup(reloaded, "compile_yam");
    const compile_olive = try Notebook.lookup(reloaded, "compile_olive");
    const input = try Heap.new_symbol(&heap, source);
    const rebuilt = try Vm.call_for_value(&vm, compile_olive, &.{input});
    // The object format preserves sharing, which can differ between passes.
    if (!Heap.equals(reloaded, rebuilt))
        return error.SelfHostedCompilerMismatch;

    try dir.writeFile(init.io, .{ .sub_path = args[3], .data = image.written() });
}

fn build(vm: anytype, image: []const u8, source: []const u8) !Heap.Obj {
    const text = try Heap.new_symbol(vm.heap, source);
    const checkpoint = vm.heap.checkpoint();
    const root = try loader.load(vm.ally, vm.heap, image);
    // The handwritten compiler takes a tagged string and a capture object.
    // Adapt that fixed entry layout only at this initial bootstrap boundary.
    const tag = try Heap.new_symbol(vm.heap, "string");
    const string_type = try vm.heap.new_inner(&.{tag});
    const string = try vm.heap.new_inner(&.{ string_type, text });
    var first = try Vm.call_for_value(vm, root.child(0).child(3), &.{ string, root.child(1) });
    first = try vm.garbage_collect(checkpoint, first);
    const compiler = try Notebook.lookup(first, "compile_olive");
    const optimized = try Vm.call_for_value(vm, compiler, &.{text});
    return vm.garbage_collect(checkpoint, optimized);
}
