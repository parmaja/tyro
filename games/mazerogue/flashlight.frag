// Flashlight / torch darkness - a custom post-processing fragment shader.
//
// Load it with:   shader.load("flashlight.frag")
// It receives the same uniforms as the built-in effects:
//   value  float  darkness strength (0..1), from shader.value
//   area   vec4   light circle {x, y, 2r, 2r}, y measured from the bottom
// Everything outside the soft light circle sinks into a deep-blue night,
// except self-lit pixels (explosions, plasma bolts, muzzle flashes) which
// glow through the dark like real emitters.
//#version 330

uniform vec2 resolution;
uniform float time;
uniform float value;
uniform vec4 area;
uniform sampler2D texture0;
uniform vec4 colDiffuse;

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

void main()
{
    vec4 color = texture(texture0, fragTexCoord);

    vec2 fragPx = fragTexCoord * resolution;
    vec2 torch = area.xy + area.zw * 0.5;
    float radius = max(area.z, 1.0) * 0.5;

    float dist = distance(fragPx, torch);
    // Soft falloff: full light near the gunner, fading out at the rim
    float lit = smoothstep(radius, radius * 0.45, dist);

    // A burning lamp flickers: two slow waves beating against each other
    float flicker = 0.97 + 0.03 * (sin(time * 6.7) * 0.6 + sin(time * 2.3) * 0.4);

    float strength = clamp(value, 0.0, 1.0);
    float dark = clamp(strength * (1.0 - lit) / flicker, 0.0, 1.0);

    // Bright emissive cores cut through the night
    float lum = dot(color.rgb, vec3(0.299, 0.587, 0.114));
    dark *= 1.0 - smoothstep(0.80, 0.94, lum);

    // Deep-blue night tint instead of flat black
    vec3 night = vec3(0.012, 0.016, 0.040);
    vec3 rgb = mix(color.rgb, night, dark);

    finalColor = vec4(rgb, color.a);
}
