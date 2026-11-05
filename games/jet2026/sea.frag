// ocean_1942.glsl — top-down ocean post-shader for raylib 6.2
//
// Only water pixels change color, and the water RIDES with the world:
//   - slow depth mottling (large, gentle patches)
//   - long wave crests (elongated in screen Y for a top-down feel)
//   - SUBTLE sun glitter on crest ridges (toned down — no blown-out highlights)
//   - gentle horizon-free gradient so the sea reads as deep at top, shallower below
//
// Non-blue pixels (islands, palms, bullets, HUD text) are masked out via "seaness".
//
// The game passes its world scroll distance (pixels, positive = the scene
// moving down the screen) through shader.area.x. Adding that to the sample
// position slides the whole water field DOWN under the islands at the same
// speed, instead of leaving it pinned to the viewport.

#version 330

uniform vec2  resolution;
uniform float time;
uniform float value;
uniform vec4  area;
uniform sampler2D texture0;
uniform vec4  colDiffuse;

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

// --- helpers ---------------------------------------------------------------

float seaness(vec3 c)
{
    return smoothstep(0.06, 0.32, c.b - max(c.r, c.g));
}

float hash21(vec2 p)
{
    p = fract(p * vec2(123.34, 456.21));
    p += dot(p, p + 45.32);
    return fract(p.x * p.y);
}

float vnoise(vec2 p)
{
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);
    float a = hash21(i);
    float b = hash21(i + vec2(1.0, 0.0));
    float c = hash21(i + vec2(0.0, 1.0));
    float d = hash21(i + vec2(1.0, 1.0));
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

float fbm(vec2 p)
{
    float v = 0.0;
    v += 0.55 * vnoise(p);
    v += 0.30 * vnoise(p * 2.03 + 7.1);
    v += 0.15 * vnoise(p * 4.11 - 3.7);
    return v;
}

void main()
{
    float strength = clamp(value, 0.0, 1.0);
    float t = time;

    vec2 uv = fragTexCoord;
    // area.x is the game's world scroll. fragTexCoord.y grows upward, so
    // adding the scroll to q.y moves every pattern DOWN the screen with the
    // islands (which also travel down as the jet climbs).
    vec2 q  = uv * resolution + vec2(0.0, area.x);

    vec4 color = texture(texture0, uv);

    float sea = seaness(color.rgb) * strength;
    if (sea < 0.001) {
        finalColor = color;
        return;
    }

    // ---- 1. Depth gradient: darker toward the top, slightly brighter/cyan
    //         toward the bottom. Color cue only; nothing moves.
    float v = uv.y;
    float depthTint = mix(-0.06, +0.04, v);
    vec3 shallowTint = vec3(-0.02, 0.02, 0.05);
    vec3 deepTint    = vec3(0.00, -0.01, 0.02);
    color.rgb *= 1.0 + sea * depthTint;
    color.rgb += sea * mix(deepTint, shallowTint, v) * 0.4;

    // ---- 2. Slow depth mottling: large, low-frequency patches.
    vec2 mottP = q * 0.0035 + vec2(t * 0.010, -t * 0.008);
    float mott = fbm(mottP) - 0.5;
    color.rgb *= 1.0 + sea * mott * 0.22;

    // ---- 3. Long drifting crests, stretched in Y for a top-down read.
    // The scroll in q.y carries the crests down WITH the islands; time only
    // bends the shapes and adds a tiny swell of its own, so the water never
    // appears pinned to the screen.
    float crest = 0.0;
    crest += sin(q.y * 0.014 + fbm(q * 0.006 + vec2(t * 0.02, 0.0)) * 3.0);
    crest += 0.7 * sin(q.y * 0.028 + q.x * 0.010 + t * 0.12 + 1.7);
    crest += 0.4 * sin(q.y * 0.060 - q.x * 0.020 - t * 0.30 + 0.8);
    crest *= 0.55;

    // Sharpened ridge mask: only the bright tops of crests get extra light.
    float ridge = pow(clamp(0.5 + crest * 0.5, 0.0, 1.0), 3.0);
    color.rgb += vec3(0.10, 0.22, 0.30) * ridge * 0.55 * sea;

    // Darken troughs slightly for contrast.
    float trough = pow(clamp(0.5 - crest * 0.5, 0.0, 1.0), 3.0);
    color.rgb *= 1.0 - trough * 0.10 * sea;

    // ---- 4. Sun glitter — TONED DOWN ---------------------------------------
    // Goals: no blown-out whites, sparkles stay on ridge tops, sparse.
    //   * lower amplitude (0.85 -> 0.22)
    //   * wider/lower exponent (10 -> 6) => softer, less pinpoint
    //   * ridge gate squared => confined to crest tops
    //   * desaturated toward sea blue instead of near-white
    //   * secondary sparkle field made much rarer and much dimmer
    float g1 = sin(q.x * 0.140 + t * 2.1) * sin(q.y * 0.115 - t * 1.7);
    float g2 = sin(q.x * 0.070 - t * 1.3 + 1.7) * sin(q.y * 0.170 + t * 2.4 + 0.8);
    float g  = g1 + g2;
    float glint = pow(clamp(0.5 + g * 0.30, 0.0, 1.0), 6.0);
    glint *= ridge * ridge;                         // tightly gated to crest tops
    // Tinted toward the sea, not toward white:
    color.rgb += vec3(0.30, 0.55, 0.75) * glint * 0.22 * sea;

    // Secondary sparkle field — much sparser and dimmer than before.
    float sparkle = pow(vnoise(q * 0.35 + vec2(t * 0.9, -t * 1.1)), 32.0);
    sparkle *= ridge * ridge * sea;
    color.rgb += vec3(0.40, 0.65, 0.85) * sparkle * 0.18;

    // ---- 5. Final tint & clamp ---------------------------------------------
    color.rgb = clamp(color.rgb, 0.0, 1.0);
    color.a   = 1.0;
    finalColor = color * colDiffuse;
}