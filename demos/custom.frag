// Window scanlines - a custom post-processing fragment shader.
//
// Load it with:   shader.load("custom.frag")
// It receives the same uniforms as the built-in effects:
//   resolution  vec2    canvas size in pixels
//   time        float   elapsed seconds
//   value       float   shader.value (0..1)
//   area        vec4    shader.area {x, y, w, h}, y measured from the bottom
// You may use all, some or none of them.
//#version 330

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
    vec4 color = texture(texture0, fragTexCoord);
    float strength = clamp(value, 0.0, 1.0);
    float line = 0.5 + 0.5 * sin(fragTexCoord.y * resolution.y * 0.5 + time * 3.0);
    float scan = mix(1.0, line, strength);
    vec3 rgb = color.rgb * scan;
    // ripple the red channel a little when value is high
    rgb.r += sin(fragTexCoord.x * resolution.x * 0.3 + time * 2.0) * strength * 0.05;
    finalColor = vec4(rgb, color.a);
}