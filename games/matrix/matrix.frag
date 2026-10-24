// Matrix CRT & Phosphor Scanline Shader
// Post-processing fragment shader for Tyro

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
    vec4 col = texture(texture0, uv);

    float strength = clamp(value, 0.0, 1.0);

    // Subtle CRT scanlines
    float scanline = 0.85 + 0.15 * sin(uv.y * resolution.y * 2.0);

    // Slight vignette at corners
    vec2 vpos = uv * (1.0 - uv.yx);
    float vig = vpos.x * vpos.y * 25.0;
    vig = clamp(pow(vig, 0.25), 0.0, 1.0);

    // Green phosphor boost on matrix greens
    vec3 rgb = col.rgb;
    rgb.g *= (1.0 + 0.1 * strength);
    rgb *= mix(1.0, scanline * vig, strength * 0.7);

    finalColor = vec4(rgb, col.a);
}
