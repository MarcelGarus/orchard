uniform vec2 viewSize;
attribute vec2 vertex;
varying vec2 fpos;
void main(void)
{
    fpos = vertex;
    gl_Position = vec4(2.0 * vertex.x / viewSize.x - 1.0, 1.0 - 2.0 * vertex.y / viewSize.y, 0, 1);
}