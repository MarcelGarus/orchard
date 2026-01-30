// Builds the Thyme VM with graphics support.

// To future me reading this: Graphics development is a mess. The graphics stack
// and the abstractions differ based on hardware and OS. Because I spent the day
// diving into how graphics stuff works, here's a brief overview of my
// understanding:
//
// - You have GPU hardware, most likely from AMD or nvidia.
//
// - Your GPU manufacturer provides drivers that allow your CPU to talk to the
//   GPU and instruct it to do things. (Especially?) in case of nvidia, these
//   are proprietary binary blobs that communicate with the GPU.
//
// - Working with drivers directly is painful. They update frequently. I don't
//   need to use every cutting edge GPU feature (in 2025, stuff like raytracing
//   or AI frame interpolation). I just want my code to work with AMD and nvidia
//   GPUs and just draw stuff. Other people feel the same, so there are unified
//   graphic APIs:
//
//   OpenGL:  [https://www.opengl.org] has been the standard for a long time and
//            is very well-supported. Is quite high level: The API is "draw a
//            triangle" etc.
//   Vulkan:  [https://www.vulkan.org] Lower level, you can set up pipelines of
//            compute shaders etc. Used by Android.
//   DirectX: [https://www.microsoft.com/de-de/download/details.aspx?id=35] is
//            a Windows-specific API.
//   Metal:   [https://developer.apple.com/metal/] is an Apple-specific API.
//   WebGPU:  [https://developer.mozilla.org/en-US/docs/Web/API/WebGPU_API]
//            works outside the browser as well (just like WASM does).
//
// - Even working with a unified library is painful. They are unified, but they
//   try to be as low-level as possible (while still supporting multiple GPUs).
//   In particular, they don't directly support high-level concepts like filling
//   paths, drawing bezier curves, etc. Mapping those to lower-level triangles
//   is called tesselation. Again, there are several libraries that make this
//   easier:
//
//   Cairo:  [https://www.cairographics.org/] is quite old, uses the X render
//           extension (whatever that is) to use the GPU.
//   SDL:    [https://www.libsdl.org] bundles up things for game dev (graphics,
//           sound, keyboard inputs, joystick support). It uses OpenGL under the
//           hood.
//   Raylib: [https://www.raylib.com] bundles up things for game dev. It also
//           uses OpenGL. It doesn't support more advanced stuff like filling
//           paths (only drawing bezier curves as strokes).
//   Skia:   [https://skia.org] is used by Chrome. Originally, Flutter also used
//           this, although now they've dropped one layer lower and created a
//           custom thingy.
//   Vello:  [https://github.com/linebender/vello] is new, very prototypish,
//           written in Rust, and tries to do as much work as possible
//           (including tesselation) on the GPU using compute shaders. It uses
//           WebGPU as the backend.
//   NanoVG: [https://github.com/memononen/nanovg] Originally a tiny C library,
//           but there's a Zig port. Uses OpenGL as the backend.
//
// I just want something that allows me to build paths using lines and bezier
// curves and then fill those paths with a solid color. I don't need font
// support, image loading, etc. Essentially, all of the libraries are overkill.
// In the future, I might look into dropping one layer lower, similar to what
// Flutter did. Right now, I just want to get something working, so I chose
// NanoVG.

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // On https://github.com/fabioarnold/nanovg-zig, it says:
    // > For an example on how to use nanovg-zig in your project's build.zig,
    // > you can take a look at https://github.com/fabioarnold/MiniPixel/blob/main/build.zig.
    // The code here is heavily inspired by that.

    const nanovg = b.addModule("nanovg", .{
        .root_source_file = b.path("nanovg/nanovg.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = !target.result.cpu.arch.isWasm(),
    });
    nanovg.addIncludePath(b.path("nanovg"));
    nanovg.addIncludePath(b.path("lib/gl2/include"));
    nanovg.addCSourceFile(.{ .file = b.path("nanovg/fontstash.c"), .flags = &.{ "-DFONS_NO_STDIO", "-fno-stack-protector" } });
    nanovg.addCSourceFile(.{ .file = b.path("nanovg/stb_image.c"), .flags = &.{ "-DSTBI_NO_STDIO", "-fno-stack-protector" } });
    if (target.result.cpu.arch.isWasm()) {
        nanovg.addIncludePath(b.path("nanovg/web/libc"));
    }

    const thyme = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    //thyme.linkLibrary(raylib.artifact("raylib"));
    thyme.addImport("nanovg", nanovg);

    const exe = b.addExecutable(.{
        .name = "thyme",
        .root_module = thyme,
    });
    if (target.result.cpu.arch.isWasm()) {
        exe.rdynamic = true;
        exe.entry = .disabled;
    } else {
        exe.addIncludePath(b.path("lib/gl2/include"));
        exe.addCSourceFile(.{ .file = b.path("lib/gl2/src/glad.c"), .flags = &.{} });
        switch (target.result.os.tag) {
            .windows => {
                b.installBinFile("glfw3.dll", "glfw3.dll");
                exe.linkSystemLibrary("glfw3dll");
                exe.linkSystemLibrary("opengl32");
            },
            .macos => {
                exe.addIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });
                exe.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
                exe.linkSystemLibrary("glfw");
                exe.linkFramework("OpenGL");
            },
            .linux => {
                exe.linkSystemLibrary("glfw3");
                exe.linkSystemLibrary("GL");
                exe.linkSystemLibrary("X11");
            },
            else => {
                std.log.warn("Unsupported target: {}", .{target});
                exe.linkSystemLibrary("glfw3");
                exe.linkSystemLibrary("GL");
            },
        }
    }

    //exe.linkLibC();
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run Thyme");
    run_step.dependOn(&run_cmd.step);
}
