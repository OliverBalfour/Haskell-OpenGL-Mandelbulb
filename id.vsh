attribute vec3 coord;
uniform float time;
void main(void) { 
 gl_Position = vec4(coord, 1.0);
}