// jet2026 open ocean: a board post-shader that turns the flat blue into a
// living sea - sun glitter, long drifting crests and slow depth shading on
// the water's colors. Pixels never move; only their colors breathe.
//
// The sea IS the whole board here, but land (islands, palms), bullets,
// blasts and white text also live on it. They keep their exact pixels:
// every effect below is masked by "seaness", and only blue-dominant
// pixels (the sea and the skyblue wave/surf tones) count as water.
//
// Uniforms the engine feeds every board effect:
//   resolution  vec2    canvas size in pixels
//   time        float   elapsed seconds
//   value       float   shader.value, the wave strength (0..1)
//   area        vec4    shader.area (unused: the ocean covers everything)

#version 330

uniform vec2 resolution;
uniform float time;
uniform float value;
uniform vec4 area;
uniform sampler2D texture0;
uniform vec4 colDiffuse;

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

// Water is blue-dominant: the sea, the wave crests and the surf all are;
// beige, green, orange, lime and white are not.
float seaness(vec3 c)
{
    return smoothstep(0.06, 0.32, c.b - max(c.r, c.g));
}

void main()
{
    float strength = clamp(value, 0.0, 1.0);
    float t = time;
    vec2 uv = fragTexCoord;
    vec2 q = uv * resolution;                 // pixels: keeps wave sizes stable

    // No position wobble: every pixel keeps its exact spot, only the
    // colors of water pixels breathe (glitter, crests, depth shading).
    vec4 color = texture(texture0, uv);
    float sea = seaness(color.rgb) * strength;

    // ---- depth mottle: slow low-frequency shading, no flat fill ----
    float mott = 0.5 + 0.5 * sin(q.x * 0.008 + t * 0.3) * sin(q.y * 0.006 - t * 0.22);
    color.rgb *= 1.0 + sea * (mott - 0.5) * 0.14;

    // ---- sun glitter: sharpened crossed ripples, bright on ridges only ----
    float g = sin(q.x * 0.110 + t * 1.6) * sin(q.y * 0.090 - t * 1.2)
            + sin(q.x * 0.050 - t * 0.9 + 1.7) * sin(q.y * 0.130 + t * 1.4 + 0.8);
    float glint = pow(clamp(0.5 + g * 0.35, 0.0, 1.0), 8.0);
    color.rgb += vec3(0.45, 0.75, 1.00) * glint * 0.50 * sea;

    finalColor = color;
}
