#version 330
// Underwater post-processing for the Submarine game (Tyro engine).
//
// Applies the "value" strength (0..1 = shader.value) and works on the
// whole canvas area given by "area". Layers, top to bottom of the math:
//   * a gentle horizontal sine wobble that grows with depth,
//   * animated caustic bands that only show under the waterline,
//   * a blue-green depth tint,
//   * faint CRT scanlines for the retro cabinet feel,
//   * a soft vignette.

uniform vec2  resolution;
uniform float time;
uniform float value;
uniform vec4  area;
uniform sampler2D texture0;

in vec2  fragTexCoord;
in vec4  fragColor;
out vec4 finalColor;

void main()
{
    vec2 uv = fragTexCoord;
    float t = time;

    // The sky band sits on top of the canvas; below ~12% of the canvas
    // height everything is water. Everything after the wobble is masked
    // by the water region so the sky and the HUD stay readable.
    float waterMask = smoothstep(0.055, 0.115, uv.y);

    // gentle water wobble
    float wob = sin(uv.y * 46.0 + t * 1.7) * 0.0022
              + sin(uv.y * 17.0 - t * 1.1) * 0.0026;
    uv.x += wob * value * waterMask;

    vec3 col = texture(texture0, uv).rgb;

    // animated caustic bands under the waterline
    float band = sin(uv.x * resolution.x * 0.045 + t * 2.0)
               + sin(uv.x * resolution.x * 0.023 - t * 1.3 + uv.y * 14.0);
    float cao = smoothstep(1.15, 1.9, band) * (1.0 - uv.y);
    col += vec3(0.16, 0.30, 0.26) * cao * 0.55 * value * waterMask;

    // shimmering sunbeams swipe in the top half of the water
    float swipe = sin(uv.x * 6.0 + t * 0.9 + sin(uv.y * 5.0) * 2.0);
    float beam = smoothstep(0.86, 1.0, swipe) * smoothstep(0.42, 0.10, uv.y);
    col += vec3(0.20, 0.26, 0.22) * beam * 0.45 * value * waterMask;

    // depth tint: deeper = colder and bluer
    float depth = smoothstep(0.15, 0.95, uv.y);
    col = mix(col, col * vec3(0.62, 0.85, 1.0), depth * 0.45 * value * waterMask);

    // subtle CRT scanlines
    float scan = 0.925 + 0.075 * sin(uv.y * resolution.y * 3.14159265);
    col *= mix(1.0, scan, 0.55);

    // soft corner vignette
    vec2 v = uv - 0.5;
    col *= 1.0 - 0.65 * dot(v, v);

    finalColor = vec4(col, 1.0) * fragColor;
}
