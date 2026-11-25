# Shader Effects

[<< Tyro docs index](README.md)

Post-processing screen shaders are controlled with the `shader` table. They are
built-in and need no files. Assign properties the usual way:

```lua
shader.effect = "water"          -- pick the effect
shader.value  = 0.5              -- 0..1 effect parameter
shader.area   = {x, y, w, h}     -- restrict the effect to a rectangle
```

| Property | Description |
|----------|-------------|
| `shader.effect` | `"water"`, `"glow"`, `"gray"`, `"sepia"`, `"invert"`, `"vignette"`, `"pixelate"` or `"none"` |
| `shader.value` | `0..1` effect parameter (see per-effect meaning below) |
| `shader.area` | Rectangle `{x, y, w, h}` in canvas pixels (`y` from the top) where the effect applies; outside it the canvas is unchanged |
| `shader.load(file)` | Load a custom fragment shader from a GLSL file and activate it (see below) |
| `shader.ok` | Read-only boolean — `true` when a shader is compiled and in effect right now (see below) |

| Effect | What it does | `value` meaning (default) |
|--------|--------------|---------------------------|
| `"water"` | Wavy water with foam across the lower region | water surface height (0.34) |
| `"glow"` | Soft glow around bright pixels (suns, bulbs, lasers) | height of the glowing region from the bottom (1.0 = full) |
| `"gray"` | Grayscale | gray strength (1.0) |
| `"sepia"` | Old-photo look | sepia strength (1.0) |
| `"invert"` | Inverted colors | inversion strength (1.0) |
| `"vignette"` | Darkened corners | vignette amount (0.5) |
| `"pixelate"` | Chunky pixel blocks | block size (0.06 ~ 1px) |

```lua
window.show(640, 480)
shader.effect = "glow"           -- turn it on
shader.value  = 0.5              -- glow on the bottom half

-- draw something bright, it will glow
canvas.color = colors.yellow
canvas.circle(320, 240, 60, true)
```

`shader.value` and `shader.area` survive effect switches, so once set they apply
to whichever effect is active. Toggle effects and tweak the parameters at any
time:

```lua
if keyboard.pressed("1") then shader.effect = "water" end
if keyboard.pressed("2") then shader.effect = "glow" end
if keyboard.pressed("0") then shader.effect = "none" end
if keyboard.pressed("q") then shader.value = shader.value - 0.1 end
if keyboard.pressed("e") then shader.value = shader.value + 0.1 end
if keyboard.pressed("z") then shader.area = {0, canvas.height/2, canvas.width, canvas.height/2} end
```

The `"water"` effect tints and distorts the lower part of the canvas, so draw a
shore or sea bottom there if you want a sea scene. See `demos/shader_demo.tyro`
for a full on-screen demo of every effect, `value` and `area`.

## Custom shaders

`shader.load("file.frag")` loads a **fragment** shader from a file and activates
it. The file is looked up next to the script, then in the current directory, then
in `assets/` under the workspace. It must be GLSL `#version 330` with the same
outputs as the built-in effects:

```glsl
#version 330
uniform vec2  resolution;                 // canvas size in pixels
uniform float time;                       // elapsed seconds
uniform float value;                      // shader.value (0..1)
uniform vec4  area;                       // shader.area {x, y, w, h} (y from the bottom)
uniform sampler2D texture0;               // the canvas
uniform vec4 colDiffuse;                  // vertex color
in vec2 fragTexCoord;
in vec4 fragColor;
out vec4 finalColor;
```

All uniforms are optional: use the ones you need and ignore the rest. `shader.value`
and `shader.area` still control `value`/`area`. Set `shader.effect = "none"` to go
back to the built-in effects. See `demos/custom.frag` for a working example:

```lua
shader.load("custom.frag")    -- activate the scanlines shader
shader.value = 0.8
shader.area  = {0, 0, canvas.width, canvas.height}
```

## Did the shader actually load?

Everything in Tyro is queued, so `shader.load` only *posts* the request; the
engine compiles the file a frame later. `shader.ok` is the readback: it is
`true` only while a shader is really compiled and in effect — `false` for
`"none"`, for a missing file, and for a `.frag` that fails to compile. Read it
a frame (or a few) after the load, e.g. with a small countdown:

```lua
local probe, sea_on = 0, false
shader.load("sea.frag")        -- the request is only queued here
probe = 3                      -- check the answer after 3 frames

function update(dt)
  if probe > 0 then
    probe = probe - 1
    if probe == 0 then
      sea_on = shader.ok       -- now we know for sure
      if sea_on then shader.value = 1.0 else draw_fallback_sea() end
    end
  end
end
```

`shader.effect` tells the same truth as a name: it reports `"none"` when the
last load failed, or the file name (custom shader) / effect name in effect.
Games like `jet2026` and `torustrooper` use `shader.ok` to fall back to
hand-drawn art or the built-in `"glow"` when their custom file is missing.

# Screen Shake

`shake` jolts the world, the way an accident or an error should feel. Every
frame the canvas and the sprites are moved by a random offset that fades out
until the time is up; the terminal and the other controls stay glued to the
window.

| Function | Description |
|----------|-------------|
| `world.shake(ms [, power])` | Shake the world for *ms* milliseconds; `power` is the maximum offset in pixels (10 by default) |
| `world.shaking` | Read-only boolean — `true` while a shake is still running |
| `shake(ms [, power])` | Deprecated global spelling of `world.shake` |
| `shake(0)` | Stop a running shake |

A new call restarts the shake, so a longer or harder one simply wins. See
`demos/shake_demo.tyro`.

```lua
window.show(640, 480)

-- an accident: a long, hard jolt
shake(800, 40)

function update(dt)
    if keyboard.pressed("space") then
        world.shake(400)   -- 400ms with the default power
    end
end

function draw()
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.color = colors.red
    canvas.circle(300, 240, 30, true)
end
```

