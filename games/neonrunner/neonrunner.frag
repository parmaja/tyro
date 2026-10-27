// Neon Cyber Runner - Cyber Arcade Post-Processing Fragment Shader
// Features: 4-tap neon bloom, CRT arcade scanlines, chromatic aberration, speed tunnel vignette.

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

void main()
{
    vec2 uv = fragTexCoord;
    float strength = clamp(value, 0.0, 1.0);

    // If shader strength is 0, pass through directly
    if (strength <= 0.01)
    {
        finalColor = texture(texture0, uv) * colDiffuse;
        return;
    }

    // Check active area bounding box if restricted
    if (area.z > 0.0 && area.w > 0.0)
    {
        vec2 pixelCoord = uv * resolution;
        if (pixelCoord.x < area.x || pixelCoord.x > area.x + area.z ||
            pixelCoord.y < area.y || pixelCoord.y > area.y + area.w)
        {
            finalColor = texture(texture0, uv) * colDiffuse;
            return;
        }
    }

    // Chromatic aberration (horizontal offset scaling with strength)
    float split = 0.0018 * strength;
    float r = texture(texture0, vec2(uv.x + split, uv.y)).r;
    float g = texture(texture0, uv).g;
    float b = texture(texture0, vec2(uv.x - split, uv.y)).b;
    float a = texture(texture0, uv).a;

    vec3 baseColor = vec3(r, g, b);

    // 4-tap neon bloom on luminous cyber highlights
    vec2 px = 2.0 / resolution;
    vec3 bloom = vec3(0.0);
    bloom += texture(texture0, uv + vec2(-px.x, 0.0)).rgb;
    bloom += texture(texture0, uv + vec2( px.x, 0.0)).rgb;
    bloom += texture(texture0, uv + vec2(0.0, -px.y)).rgb;
    bloom += texture(texture0, uv + vec2(0.0,  px.y)).rgb;
    bloom *= 0.25;

    // Isolate bright pixels (neon cyan, magenta, gold)
    float luma = dot(bloom, vec3(0.2126, 0.7152, 0.0722));
    if (luma > 0.40)
    {
        baseColor += bloom * (0.45 * strength);
    }

    // Arcade CRT scanlines (soft sine pattern across vertical lines)
    float scanline = sin(uv.y * resolution.y * 3.14159) * 0.5 + 0.5;
    float scanFactor = 1.0 - (scanline * 0.14 * strength);
    baseColor *= scanFactor;

    // Cyber speed tunnel vignette (edge darkening)
    vec2 uvNorm = uv * 2.0 - 1.0;
    float dist = length(uvNorm);
    float vignette = 1.0 - smoothstep(0.7, 1.4, dist) * (0.35 * strength);
    baseColor *= vignette;

    finalColor = vec4(baseColor, a) * colDiffuse;
}
