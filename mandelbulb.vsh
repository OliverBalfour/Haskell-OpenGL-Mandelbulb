attribute vec3 coord;
uniform float time;
uniform float aa;
uniform float ab;
void main(void) { 
  gl_Position = vec4(coord, 1.0);
}