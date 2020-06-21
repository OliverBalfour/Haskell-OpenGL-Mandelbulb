
#define MAX_STEPS 100
#define EPSILON 0.001
#define OUT_OF_RANGE 10.0
// 1.0 / tan (radians(45 degrees) / 2.0)
#define COT_HALF_FOV 2.414214
#define ITERATIONS 4

// TODO: uniform vec2 uFramebufferSize;

// SDF for a unit sphere
float sphereSDF (vec3 point) {
  vec3 centre = vec3(0.0, 0.0, 0.0);
  float radius = 1.0;
  return length(point - centre) - radius;
}

// SDF for a unit cube centred at the origin without rotation
float cubeSDF (vec3 point) {
  vec3 q = abs(point) - vec3(1.0, 1.0, 1.0);
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

// float mandelbulbSDF (vec3 c) {
//   vec3 w = c;
//   float m = dot(w,w);
//   for (int i = 0; i < ITERATIONS; i++) {
//     // extract polar coordinates
//     float wr = sqrt(dot(w,w));
//     float wo = acos(w.y/wr);
//     float wi = atan(w.x,w.z);
//     // scale and rotate the point
//     wr = pow(wr, 8.0);
//     wo = wo * 8.0;
//     wi = wi * 8.0;
//     // convert back to cartesian coordinates
//     w.x = wr * sin(wo)*sin(wi);
//     w.y = wr * cos(wo);
//     w.z = wr * sin(wo)*cos(wi);
//     if (dot(w,w) > 256.0) break;
//   }
// }
float mandelbulbSDF (vec3 p) {
  vec3 w = p;
  float m = dot(w,w);
  vec4 trap = vec4(abs(w),m);
  float dz = 1.0;
  for(int i=0; i<4; i++) {
    dz = 8.0*pow(sqrt(m),7.0)*dz + 1.0;
    float r = length(w);
    float b = 8.0*acos( w.y/r);
    float a = 8.0*atan( w.x, w.z );
    w = p + pow(r,8.0) * vec3( sin(b)*sin(a), cos(b), sin(b)*cos(a) );
    trap = min( trap, vec4(abs(w),m) );

    m = dot(w,w);
    if (m > 256.0) break;
  }

  return 0.25*log(m)*sqrt(m)/dz;
}


/**
 * Combine all SDFs in the scene into one signed distance function.
 */
float netSDF (vec3 point) {
  return min(sphereSDF(point), cubeSDF(point / 0.7));
}

/**
 * Use raymarching to determine the distance a ray of light needs to travel
 * to intercept the netSDF surface described by a signed distance function.
 * @param camera vector representing the camera location
 * @param unitRay unit vector in the direction of the light ray being raymarched
 * @return distance to the nearest point in the SDF in the ray direction
 *  or OUT_OF_RANGE if no collision
 */
float raymarchDistanceToSDF (vec3 camera, vec3 unitRay) {
  float k = 0.0; // ray scalar factor
  for (int i = 0; i < MAX_STEPS; i++) {
    float signedDistance = netSDF(camera + k * unitRay);
    if (signedDistance < EPSILON) {
      return k;
    }
    k += signedDistance;
    if (k > OUT_OF_RANGE) break;
  }
  return OUT_OF_RANGE;
}

/**
 * Compute the direction of a ray through the pixel from the viewer's eye
 * @param fragCoord the x, y coords of the pixel/fragment
 * @return unit vector in the direction of the fragment
 */
vec3 computeRayDirection (vec2 fragCoord) {
  // The angle between the viewer's eye and the middle of the screen and fragment is FOV / 2.0
  // Then cot 1/2 FOV = (-z) / y
  // TODO: vec2 xy = fragCoord - uFramebufferSize / 2.0;
  vec2 xy = fragCoord - vec2(640.0, 480.0) / 2.0;
  // TODO: float z = -framebufferSize.y * COT_HALF_FOV;
  float z = -480.0 * COT_HALF_FOV;
  return normalize(vec3(xy, z));
}

/**
 * Return a transform matrix that will transform a ray from view space
 * to world coordinates, given the eye point, the camera target, and an up vector.
 *
 * This assumes that the center of the camera is aligned with the negative z axis in
 * view space when calculating the ray marching direction. See rayDirection.
 */
mat4 viewMatrix(vec3 eye, vec3 center, vec3 up) {
  // Based on gluLookAt man page
  vec3 f = normalize(center - eye);
  vec3 s = normalize(cross(f, up));
  vec3 u = cross(s, f);
  return mat4(
    vec4(s, 0.0),
    vec4(u, 0.0),
    vec4(-f, 0.0),
    vec4(0.0, 0.0, 0.0, 1)
  );
}


void mainImage (out vec4 fragColor, in vec2 fragCoord) {
  vec3 camera = vec3(5.0,5.0,6.0);
  vec3 unitRay = computeRayDirection(fragCoord);
  mat4 viewToWorld = viewMatrix(camera, vec3(0.0, 0.0, 0.0), vec3(0.0, 1.0, 0.0));
  vec3 worldRay = (viewToWorld * vec4(unitRay, 0.0)).xyz;
  float signedDistance = raymarchDistanceToSDF(camera, worldRay);
  if (signedDistance > OUT_OF_RANGE - EPSILON) {
    fragColor = vec4(0.0, 0.0, 0.0, 0.0);
    return;
  }
  vec3 interceptingPoint = camera + signedDistance * worldRay;
  fragColor = vec4(1.0, 1.0, 0.0, 1.0);
}
