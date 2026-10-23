// Full-screen water post-processing for the fish tank.
//
// Loaded by the script with:  shader.load("water.frag")
// It receives the same uniforms as the built-in effects:
//   resolution  vec2    canvas size in pixels
//   time        float   elapsed seconds
//   value       float   shader.value, the water strength (0..1)
//   area        vec4    shader.area (unused here: the water covers everything)
//
// The canvas (background, sand, plants and bubbles) is rippled and tinted;
// the fish sprites are drawn afterwards on top by the engine.

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

// A cheap procedural caustic web: three crossed sine layers, sharpened so the
// bright ridges look like light focused by a moving surface.
float caustic(vec2 uv, float t)
{
    float c = 0.0;
    c += sin(uv.x * 1.7 + t * 0.90) * sin(uv.y * 1.3 - t * 0.70);
    c += sin(uv.x * 2.9 - t * 0.60 + 1.3) * sin(uv.y * 2.3 + t * 0.80 + 0.7);
    c += sin(uv.x * 4.3 + t * 0.40 + 2.1) * sin(uv.y * 3.7 - t * 0.50 + 1.9);
    c = clamp(0.5 + c / 6.0, 0.0, 1.0);
    return pow(c, 4.0);
}

void main()
{
    float strength = clamp(value, 0.0, 1.0);
    vec2 uv = fragTexCoord;

    // Gentle refraction everywhere. The amplitudes are small so the scene
    // stays readable, but the whole tank feels like it is moving.
    float wobX = sin(uv.y * 18.0 + time * 1.4) * 0.0015
               + sin(uv.y * 7.0 - time * 0.8) * 0.0022;
    float wobY = cos(uv.x * 15.0 - time * 1.1) * 0.0012;
    vec2 warped = uv + vec2(wobX, wobY) * strength;

    vec4 color = texture(texture0, warped);

    // Depth tint: a touch of deep blue at the bottom, open cyan-green at the
    // top. fragTexCoord.y is 0 at the bottom of the canvas.
    vec3 shallow = vec3(0.62, 0.95, 1.00);
    vec3 deep    = vec3(0.06, 0.22, 0.46);
    vec3 tint = mix(deep, shallow, clamp(uv.y, 0.0, 1.0));
    color.rgb *= mix(vec3(1.0), tint, 0.36 * strength);

    // Moving caustics, strongest in the upper water where the light enters.
    float lightFall = smoothstep(0.0, 0.85, uv.y);
    float ca = caustic(uv * 9.0, time) * lightFall;
    color.rgb += vec3(0.20, 0.72, 0.82) * ca * 0.34 * strength;

    // Slow horizontal shimmer bands, like the surface seen from below.
    float band = 0.5 + 0.5 * sin(uv.y * 55.0 + time * 0.7
               + sin(uv.x * 4.0 + time * 0.3) * 1.5);
    color.rgb += vec3(0.10, 0.36, 0.42) * band * 0.038 * strength;

    // Darkened glass corners (tank vignette).
    float d = distance(uv, vec2(0.5));
    color.rgb *= 1.0 - smoothstep(0.55, 0.95, d) * 0.30 * strength;

    // A thin bright glass edge around the screen.
    float edge = min(min(uv.x, 1.0 - uv.x), min(uv.y, 1.0 - uv.y));
    color.rgb += vec3(0.55, 0.85, 0.95)
               * (1.0 - smoothstep(0.0, 0.012, edge)) * 0.14 * strength;

    finalColor = color;
}
