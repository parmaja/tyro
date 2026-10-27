// Zonix - Quantum Orb Neon Glow Fragment Shader
// Applied directly to enemy sprites (Orb, Seeker Drone, Boss Core) for a luminous pulsating aura

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
    float pulse = 0.80 + 0.30 * sin(time * 5.0);

    // Radial neighborhood samples for outer energy aura
    vec2 px = 2.2 / resolution;
    vec4 aura = vec4(0.0);
    aura += texture(texture0, uv + vec2(-px.x, -px.y));
    aura += texture(texture0, uv + vec2( px.x, -px.y));
    aura += texture(texture0, uv + vec2(-px.x,  px.y));
    aura += texture(texture0, uv + vec2( px.x,  px.y));
    aura += texture(texture0, uv + vec2(-px.x * 1.6, 0.0));
    aura += texture(texture0, uv + vec2( px.x * 1.6, 0.0));
    aura += texture(texture0, uv + vec2(0.0, -px.y * 1.6));
    aura += texture(texture0, uv + vec2(0.0,  px.y * 1.6));
    aura /= 8.0;

    // Glowing core illumination
    vec3 brightCore = col.rgb * (1.0 + 0.65 * pulse * strength);

    // Cyan-magenta electric aura halo
    vec3 auraColor = vec3(0.0, 0.90, 1.0);
    float halo = aura.a * (1.0 - col.a) * 0.95 * pulse * strength;

    vec3 rgb = brightCore + auraColor * halo;
    float a = clamp(col.a + halo, 0.0, 1.0);

    finalColor = vec4(rgb, a);
}
