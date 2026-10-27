// Neon Cyber Runner - Player Turbo Aura & Shield Fragment Shader
// Adds an emissive speed afterglow / rainbow aura around the hoverbike when Turbo Boost or Shield is active.

#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float time;
uniform vec2 resolution;
uniform float value;

void main()
{
    vec2 uv = fragTexCoord;
    vec4 tex = texture(texture0, uv);
    vec4 col = tex * fragColor * colDiffuse;

    float strength = (value > 0.0) ? value : 1.0;

    // Vector and pixel distance from center of the sprite quad
    vec2 fromCenter = (uv - vec2(0.5)) * resolution;
    float dist = length(fromCenter);

    // Dynamic high-speed aura pulse
    float pulse = 0.85 + 0.25 * sin(time * 8.0 - dist * 0.15);

    // Radiating energy light field
    float maxR = min(resolution.x, resolution.y) * 0.5;
    float auraFactor = clamp(1.0 - (dist / maxR), 0.0, 1.0);
    float auraField = pow(auraFactor, 2.0) * pulse * strength * 0.6;

    // Cycling rainbow cyber aura tint based on time
    vec3 auraColor = vec3(
        0.5 + 0.5 * sin(time * 4.0),
        0.5 + 0.5 * sin(time * 4.0 + 2.094),
        0.5 + 0.5 * sin(time * 4.0 + 4.188)
    );

    if (col.a > 0.05)
    {
        // Solid body of the sprite: boost core brightness
        vec3 boostedCore = col.rgb * (1.0 + 0.40 * pulse * strength);
        finalColor = vec4(boostedCore, col.a);
    }
    else
    {
        // Outer glow aura around sprite perimeter
        finalColor = vec4(auraColor * auraField, auraField * 0.8);
    }
}
