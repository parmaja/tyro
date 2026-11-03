// Neon Breaker CRT filter - scanlines, a slow scan bar and a soft vignette.
// Load with:   shader.load("crt.frag")       shader.value = 0..1 strength
#version 330

uniform vec2 resolution;
uniform float time;
uniform float value;
uniform sampler2D texture0;
uniform vec4 colDiffuse;

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

void main()
{
    vec4 color = texture(texture0, fragTexCoord);
    float strength = clamp(value, 0.0, 1.0);

    // Alternate scanline rows dimmed a touch, plus a soft glow on the lit rows
    float row = fract(fragTexCoord.y * resolution.y * 0.5);
    float scan = 1.0 - 0.18 * strength * smoothstep(0.15, 1.0, row);

    // The bright band of an old CRT slowly climbing the glass
    float bar = fract(fragTexCoord.y + time * 0.03);
    scan *= 1.0 + 0.04 * strength * smoothstep(0.0, 0.03, bar) * (1.0 - smoothstep(0.03, 0.08, bar));

    // Vignette darkening the corners
    vec2 d = fragTexCoord - 0.5;
    float vig = 1.0 - 0.45 * strength * dot(d, d);

    finalColor = vec4(color.rgb * scan * vig, color.a);
}
