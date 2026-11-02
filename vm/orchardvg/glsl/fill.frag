// OrchardVG modified NanoVG shader. See ../LICENSE.txt.
#ifdef GL_ES
precision mediump float;
#endif

uniform vec4 frag[7];
uniform sampler2D tex;
uniform sampler2D colormap;
uniform sampler2D clipMask;
uniform int clipEnabled;
uniform vec2 viewSize;
varying vec2 fpos;
#define paintMat mat3(frag[0].xyz, frag[1].xyz, frag[2].xyz)
#define innerCol frag[3]
#define outerCol frag[4]
#define extent frag[5].xy
#define radius frag[5].z
#define feather frag[5].w
#define texType int(frag[6].x)
#define type int(frag[6].y)
#define blurDir frag[6].zw

float sdroundrect(vec2 pt, vec2 ext, float rad) {
    vec2 ext2 = ext - vec2(rad,rad);
    vec2 d = abs(pt) - ext2;
    return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;
}

void main(void) {
   vec4 result;
    if (type == 0) { // Gradient
        // Calculate gradient color using box gradient
        vec2 pt = (paintMat * vec3(fpos,1.0)).xy;
        float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5) / feather, 0.0, 1.0);
        vec4 color = mix(innerCol,outerCol,d);
        result = color;
    } else if (type == 1) { // Image
        // Calculate color from texture
        vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;
        vec4 color = texture2D(tex, pt);
        if (texType == 1) color = vec4(color.xyz*color.w,color.w);
        if (texType == 2) color = vec4(color.x);
        if (texType == 3) {
            color = texture2D(colormap, vec2(color.x, 0.5));
            color = vec4(color.xyz*color.w,color.w);
        }
        // Apply color tint and alpha.
        color *= innerCol;
        result = color;
    } else if (type == 2) { // Stencil fill
        result = vec4(1,1,1,1);
    } else if (type == 4) { // Blur
        vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;
        vec4 color = vec4(0);
        // 9-tap r=4 sigma=2
        color += texture2D(tex, pt - 4.0 * blurDir) * 0.02853226260337099;
        color += texture2D(tex, pt - 3.0 * blurDir) * 0.06723453549491201;
        color += texture2D(tex, pt - 2.0 * blurDir) * 0.1240093299792275;
        color += texture2D(tex, pt - 1.0 * blurDir) * 0.1790438646174162;
        color += texture2D(tex, pt + 0.0 * blurDir) * 0.2023600146101466;
        color += texture2D(tex, pt + 1.0 * blurDir) * 0.1790438646174162;
        color += texture2D(tex, pt + 2.0 * blurDir) * 0.1240093299792275;
        color += texture2D(tex, pt + 3.0 * blurDir) * 0.06723453549491201;
        color += texture2D(tex, pt + 4.0 * blurDir) * 0.02853226260337099;
        // 11-tap r=5 sigma=2.44
        // color += texture2D(tex, pt - 5.0 * blurDir) * 0.020985076793630084;
        // color += texture2D(tex, pt - 4.0 * blurDir) * 0.04422272171421008;
        // color += texture2D(tex, pt - 3.0 * blurDir) * 0.07896305119735297;
        // color += texture2D(tex, pt - 2.0 * blurDir) * 0.1194701593446622;
        // color += texture2D(tex, pt - 1.0 * blurDir) * 0.15316463896215712;
        // color += texture2D(tex, pt + 0.0 * blurDir) * 0.166388703975975;
        // color += texture2D(tex, pt + 1.0 * blurDir) * 0.15316463896215712;
        // color += texture2D(tex, pt + 2.0 * blurDir) * 0.1194701593446622;
        // color += texture2D(tex, pt + 3.0 * blurDir) * 0.07896305119735297;
        // color += texture2D(tex, pt + 4.0 * blurDir) * 0.04422272171421008;
        // color += texture2D(tex, pt + 5.0 * blurDir) * 0.020985076793630084;
        // Apply color tint and alpha.
        color *= innerCol;
        result = color;
    }
    // Clip masks are framebuffer-sized textures retained across child draws.
    // Stencil construction only writes temporary bits, so it needs no mask sample.
    if (clipEnabled != 0 && type != 2) {
        vec2 uv = vec2(fpos.x / viewSize.x, 1.0 - fpos.y / viewSize.y);
        result *= texture2D(clipMask, uv).a;
    }
    gl_FragColor = result;
}