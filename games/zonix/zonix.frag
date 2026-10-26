// Zonix - Cyber Arcade CRT & Neon Glow Shader for Tyro
// Fragment shader for post-processing the board canvas.

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

    // If shader is disabled or strength is 0, pass through directly
    if (strength <= 0.01)
    {
        finalColor = texture(texture0, uv) * colDiffuse;
        return;
    }

    // Check if we are inside the active area (if defined)
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

    // Subtle chromatic aberration (horizontal split on high-contrast lines)
    float split = 0.0012 * strength;
    float r = texture(texture0, vec2(uv.x + split, uv.y)).r;
    float g = texture(texture0, uv).g;
    float b = texture(texture0, vec2(uv.x - split, uv.y)).b;
    float a = texture(texture0, uv).a;

    vec3 baseColor = vec3(r, g, b);

    // 4-tap neon bloom / glow on bright cyber elements
    vec2 px = 1.5 / resolution;
    vec3 bloom = vec3(0.0);
    bloom += texture(texture0, uv + vec2(-px.x, 0.0)).rgb;
    bloom += texture(texture0, uv + vec2( px.x, 0.0)).rgb;
    bloom += texture(texture0, uv + vec2(0.0, -px.y)).rgb;
    bloom += texture(texture0, uv + vec2(0.0,  px.y)).rgb;
    bloom *= 0.25;

    // Enhance bright neon highlights (cutoff below 0.3)
    vec3 bright = max(bloom - vec3(0.3), vec3(0.0)) * 0.7;
    baseColor += bright * strength;

    // Subtle arcade CRT scanlines
    float scanline = 0.90 + 0.10 * sin(uv.y * resolution.y * 1.5 + time * 2.0);
    baseColor *= mix(1.0, scanline, strength * 0.6);

    // Gentle edge vignette
    vec2 vpos = uv * (1.0 - uv.yx);
    float vig = vpos.x * vpos.y * 28.0;
    vig = clamp(pow(vig, 0.22), 0.0, 1.0);
    baseColor *= mix(1.0, vig, strength * 0.4);

    // Subtle color grading: slight cyan/blue cyber tint in darks
    baseColor.b += (1.0 - vig) * 0.04 * strength;

    finalColor = vec4(baseColor, a) * colDiffuse;
}
