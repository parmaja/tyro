// Torus Trooper's neon bloom - why the wires burn like an arcade CRT.
// Load with:   shader.load("neon.frag")    shader.value = 0..1 gain (default 1)
// The built-in "glow" blurs a 5x5 box, which a 1-pixel wire survives only as
// a faint halo. This one walks two rings of taps around every pixel - a near
// ring at 3 px and a far, dimmer ring at 7.5 px - and lets only the bright
// samples leak their own color, squared, so the hot wires bloom and the dark
// tube stays dark.
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
    float gain = clamp(value, 0.0, 1.0);
    vec2 t = 1.0 / resolution;

    vec3 halo = vec3(0.0);
    for (int i = 0; i < 12; i++)
    {
        float a = 6.2831853 * float(i) / 12.0;
        vec2 dir = vec2(cos(a), sin(a));
        vec3 near = texture(texture0, fragTexCoord + dir * (t * 3.0)).rgb;
        vec3 far = texture(texture0, fragTexCoord + dir * (t * 7.5)).rgb;
        halo += near * dot(near, vec3(0.2126, 0.7152, 0.0722));
        halo += far * dot(far, vec3(0.2126, 0.7152, 0.0722)) * 0.45;
    }
    halo /= 12.0;

    // the faint breathing of the CRT's power supply
    float pulse = 0.85 + 0.15 * sin(time * 1.7);

    finalColor = vec4(color.rgb + halo * gain * pulse * 2.2, color.a);
}
