//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const std = @import("std");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("pear_lib");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("Welcome to the Pear interpreter!\n", .{});

    var ally = std.heap.DebugAllocator(.{}){};

    var mem = lib.Mem{ .ally = ally.allocator() };

    const path = "../../pear/pear.pear"; // args.get_maybe(1) or exit(1, "You didn't specify a file.\n")
    const result = lib.eval_file(&mem, path);

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("\n{any}\n", .{result});

    try bw.flush(); // Don't forget to flush!
}
