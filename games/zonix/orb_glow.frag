// Zonix - Quantum Orb Dynamic Light & Glow Fragment Shader
// Generates radiant light in radius size x2 directly around the enemy orb on the GPU

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

    // Dynamic light frequency pulse
    float pulse = 0.82 + 0.28 * sin(time * 5.0 - dist * 0.12);

    // Orb physical radius is ~9.5 px; light radius extends to 2x size (~21 px)
    float orbR = 9.5;
    float lightR = orbR * 2.1;

    // Radial light falloff in radius size x2 around the orb
    float lightFactor = clamp((lightR - dist) / (lightR - orbR * 0.75), 0.0, 1.0);
    float lightField = pow(lightFactor, 1.5) * pulse * strength;

    // Light tint matching enemy type
    vec3 lightTint = vec3(0.0, 0.90, 1.0); // Default electric cyan

    // Adjust light color if tinted (e.g. Red Seeker or Gold Pulsar Boss)
    if (colDiffuse.r > colDiffuse.b * 1.3)
    {
        lightTint = vec3(1.0, 0.22, 0.28); // Crimson red
    }
    else if (colDiffuse.r > 0.7 && colDiffuse.g > 0.6)
    {
        lightTint = vec3(1.0, 0.82, 0.15); // Golden amber
    }

    if (col.a > 0.05)
    {
        // Inside the physical orb: energetic glowing core
        vec3 coreGlow = col.rgb * (1.0 + 0.60 * pulse * strength);
        finalColor = vec4(coreGlow, col.a);
    }
    else
    {
        // Surrounding the orb: pure GPU fragment shader light in radius size x2
        float alpha = lightField * 0.70;
        vec3 lightRgb = lightTint * lightField;
        finalColor = vec4(lightRgb, alpha);
    }
}
