// Seaquest water post-processing.
//
// Loaded by the script with:  shader.load("seaquest.frag")
//
// How it fits the renderer:  canvas.front = true makes the board canvas a
// TRANSLUCENT overlay that the engine composites ABOVE the sprites and the
// parallax background. The script paints soft washes into that board (god ray
// shafts, a deep-water tint, a vignette, the low-oxygen red pulse) and this
// shader turns those flat washes into moving water: it refracts them, adds a
// procedural caustic web and animated light shafts, and grades the depth.
// Because everything the shader produces carries alpha, it lights the sprites
// underneath without ever hiding them - which is what water should do.
//
// The HUD lives in the top strip of the board, outside shader.area, so it is
// passed through untouched: no refraction, no caustics, no tint.
//
// Uniforms (the same ones every Tyro custom shader gets):
//   resolution  vec2    canvas size in pixels
//   time        float   elapsed seconds
//   value       float   shader.value, the water strength (0..1)
//   area        vec4    shader.area (x, y, w, h) in GL pixels, y from the
//                       BOTTOM - the engine already flips it for us

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

// A cheap procedural caustic web: crossed sine layers, sharpened so the bright
// ridges look like light focused by a moving surface.
float caustic(vec2 uv, float t)
{
    float c = 0.0;
    c += sin(uv.x * 1.7 + t * 0.90) * sin(uv.y * 1.3 - t * 0.70);
    c += sin(uv.x * 2.9 - t * 0.60 + 1.3) * sin(uv.y * 2.3 + t * 0.80 + 0.7);
    c += sin(uv.x * 4.3 + t * 0.40 + 2.1) * sin(uv.y * 3.7 - t * 0.50 + 1.9);
    c = clamp(0.5 + c / 6.0, 0.0, 1.0);
    return pow(c, 4.0);
}

// Slanted light shafts sinking from the surface, breathing as they drift.
float shafts(vec2 uv, float depth, float t)
{
    float s = 0.0;
    for (int i = 0; i < 3; i++)
    {
        float fi = float(i);
        float lean = 0.10 + fi * 0.045;                 //how far the beam tilts
        float x = uv.x + depth * lean;                  //follow the lean down
        float freq = 7.0 + fi * 3.0;
        float phase = t * (0.13 + fi * 0.05) + fi * 2.1;
        float band = 0.5 + 0.5 * sin(x * freq + phase);
        s += pow(band, 6.0 + fi * 2.0) / (1.0 + fi);
    }
    //beams fade out with depth and never fully switch off at the surface
    s *= exp(-depth * 2.4);
    return s;
}

void main()
{
    vec2 px = fragTexCoord * resolution;            //y grows UP here
    vec2 uv = fragTexCoord;

    //Is this pixel inside the water band? A soft edge keeps the waterline from
    //biting, and lets the shader blur the band into the sky by a few pixels.
    float dTop    = (area.y + area.height) - px.y;  //distance below the surface
    float dBottom = px.y - area.y;                  //distance above the floor
    float inWater = smoothstep(-6.0, 10.0, dTop) * smoothstep(-6.0, 14.0, dBottom);
    float depth   = clamp(dTop / max(area.height, 1.0), 0.0, 1.0); //0 at surface

    if (inWater <= 0.001)
    {
        finalColor = texture(texture0, uv) * fragColor * colDiffuse;
        return;
    }

    float strength = clamp(value, 0.0, 1.0);
    float t = time;

    //Refraction: two crossed wobbles, strongest mid-water, tiny near the edges
    //so the washes the script drew keep their shape.
    float wx = sin(uv.y * 26.0 + t * 1.5) * 0.0016
             + sin(uv.y * 9.0 - t * 0.9) * 0.0026;
    float wy = cos(uv.x * 19.0 - t * 1.2) * 0.0014;
    vec2 warped = uv + vec2(wx, wy) * strength * inWater;

    vec4 board = texture(texture0, warped) * fragColor * colDiffuse;

    //---- procedural water light, added with alpha so it lights the world ----
    //Caustics: strongest in the upper water where the light enters.
    float ca = caustic(uv * vec2(9.0, 7.0), t) * exp(-depth * 1.6);

    //Light shafts, raked across the water.
    float sh = shafts(uv, depth, t);

    //A slow swell of brightness rolling through the band.
    float swell = 0.5 + 0.5 * sin(uv.y * 22.0 + t * 0.6
               + sin(uv.x * 5.0 + t * 0.35) * 1.7);

    //Depth grading: bright teal near the surface, ink at the floor.
    vec3 shallow = vec3(0.46, 0.86, 0.92);
    vec3 deep    = vec3(0.10, 0.22, 0.46);
    vec3 tint = mix(shallow, deep, depth);

    vec3 light = vec3(0.0);
    light += tint * ca * 0.55;
    light += vec3(0.55, 0.85, 0.80) * sh * 0.34;
    light += tint * swell * 0.055;
    light *= strength;

    //How much of that light shows through where the script painted nothing:
    //the washes the script drew are already visible, the light fills the gaps.
    float wash = board.a;
    float lightAlpha = inWater * strength * (0.30 + 0.42 * (ca + sh))
                     * (1.0 - wash * 0.65);

    //Vignette: the corners of the tank go dark, more so with depth.
    vec2 cen = uv - vec2(0.5);
    float vig = smoothstep(0.42, 0.92, length(cen * vec2(1.15, 1.0)));
    light -= tint * vig * 0.14 * strength * inWater;

    //Depth grade darkens the washes themselves, so the deep end reads heavy.
    board.rgb = mix(board.rgb, board.rgb * tint * 1.35, 0.34 * depth * strength);

    vec3 rgb = board.rgb + light;
    float alpha = clamp(wash + lightAlpha * (1.0 - wash), 0.0, 1.0);

    finalColor = vec4(rgb, alpha);
}
