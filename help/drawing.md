# Canvas, Screen and World - Where Does My Drawing Go? 🎨

Tyro gives you **three places to draw**. They look similar - all three take the
same drawing commands - but each one ends up somewhere different on the screen.
Once you know where each one sits, you can put every picture exactly where you
want it.

Think of the window like a sandwich:

```
  canvas   ← a drawing sheet that the camera looks through (below sprites)
  world    ← the camera itself + the page behind everything
  screen   ← the glass in front of the window (always on top, never moves)
  controls ← buttons, terminal, output...
  models   ← 3D models (if you use any)
  particles← engine sparks on top of the models (see help/particles.md)
  sprites  ← your sprites and tiles
```

| Table | What it is | Good for |
| --- | --- | --- |
| `canvas` | A drawing sheet the same size as the window, **below the sprites**, seen through the camera | backgrounds, hand-drawn shapes, paint programs |
| `world` | The page behind everything **and the camera** - it owns the shake and the window title | background color, things that should shake with the world |
| `screen` | A **fixed buffer on top of all drawing**, past the camera and the shaders | HUD: score, health bar, crosshair, messages |

---

## The Drawing Order (Back to Front)

Every frame Tyro paints the window in this order:

1. **world** - the background is filled with the world back color
2. **canvas** - the sheet is composited (with its shader effect, if set)
3. **sprites** - all your sprites and tile maps
4. **3D models** - if any
5. **particles** - the engine's live `particles.emit()` sparks, if any
   (see `help/particles.md`)
6. **controls** - the terminal, output panel, buttons
7. **screen** - the top buffer, drawn **last**

So something you draw with `screen.text()` is always on top of sprites,
controls and everything else.

---

## canvas - the Drawing Sheet

`canvas` is a texture the same size as the window. Everything you draw with it
is remembered, so it works like a paint program: draw once and it stays until
you clear it.

```lua
window.show(640, 480)

-- paint the background once (it stays)
canvas.color = colors.skyblue
canvas.rectangle(0, 0, canvas.width, canvas.height, true)

function draw()
  -- a ball that leaves trails (no clear = old circles stay)
  canvas.color = colors.red
  canvas.circle(math.random(canvas.width), math.random(canvas.height), 10, true)
end
```

`canvas.clear()` wipes the sheet with the canvas back color;
`canvas.clear(colors.white)` wipes it with any color you name.
Use the color `blank` (also `transparent` / `none`) to wipe it fully
transparent so the sprites behind show through.

The canvas sits inside the camera, so when the world shakes, the canvas
shakes with it. And the `shader` table (`shader.effect = "water"`, `glow`,
`gray`, ...) is a post-processing pass over this layer - it even affects the
sprites drawn above it, because they all ride inside the camera.

---

## world - the Page and the Camera

`world` is the page behind everything, and at the same time the camera that
looks at the game. Its `backcolor` fills the whole window every frame, and it
owns the shake:

```lua
world.backcolor = 'black'
world.title = 'My Cool Game'

function update(dt)
  if keyboard.pressed('space') then
    world.shake(400, 20)   -- jolt the world for 400 ms, 20 px power
  end
end

function draw()
  -- drawn directly into the camera: this square shakes together
  -- with the canvas and the sprites
  world.color = colors.lime
  world.rectangle(100, 100, 60, 60, true)
end
```

World extras (things only the camera owns):

| API | What it does |
| --- | --- |
| `world.shake(ms [, power])` | jolt the camera (the same as the global `shake()`) |
| `world.shaking` | `true` while a shake is still running |
| `world.title` | the window title |
| `world.light(x, y, z)` | the sun ray the 3D shadows fall along |
| `world.ambient(v)` | how much sky light fills the shadows (`0`..`1`) |
| `world.ground(z)` | the depth plane the 3D shadows land on |
| `world.timescale` | slow motion: `0.35` runs the whole scene at a third speed (default `1`) |

### `world` drawing only works inside `draw()` ⚠️

Drawing **with** the world is a per-frame job: `world.text()`,
`world.rectangle()`, `world.circle()`... belong **inside the `draw()`
function**, just like the example above. The world page is repainted every
frame, so a drawing command that runs anywhere else (in `update(dt)`, or in
plain script code) is not part of the picture and can be missed or painted
over - it is not reliable.

Only the world's *properties and commands* belong outside `draw()`:

```lua
world.backcolor = 'black'      -- setup: fine outside draw()
world.title = 'My Game'        -- fine anywhere

function update(dt)
  if bomb.exploded then
    world.shake(300, 10)       -- not drawing - fine inside update()
  end
end

function draw()
  world.color = colors.lime    -- drawing: goes here
  world.rectangle(100, 100, 60, 60, true)
end
```

---

## screen - the Fixed Glass on Top ⭐

`screen` is the special one. It is a **fixed buffer that is drawn on top of all
drawing**, *after* the camera pass and the controls have finished. That means:

- **Camera moves never touch it.** When `world.shake()` jitters everything,
  your `screen` text stays perfectly still and readable.
- **Shaders never touch it.** A `water` or `invert` effect changes the layers
  inside the camera - your HUD on `screen` keeps its true colors.
- **It is always on top** - sprites, 3D models and controls all sit below it.

So `screen` is the right place for anything the player must always read:

```lua
window.show(800, 600)
score = 0

function draw()
  -- This HUD never wobbles, even while the world shakes.
  -- The screen buffer remembers last frame, so clear it first.
  screen.clear('blank')
  screen.color = colors.white
  screen.text(10, 10, 'SCORE ' .. score)
  screen.color = colors.red
  screen.rectangle(10, 30, health * 2, 8, true)
end
```

> Tip: like `canvas`, the `screen` buffer keeps whatever you drew last frame.
> For a HUD that changes every frame, start with `screen.clear('blank')`.

---

## What Moves, What Stays?

While a `world.shake()` is running, or when a shader effect is on:

| Layer | Shakes with the camera? | Affected by `shader.effect`? | On top of sprites? |
| --- | --- | --- | --- |
| `world` drawing | yes | no (it's behind) | no |
| `canvas` drawing | yes | yes (it carries the effect) | no (sprites cover it) |
| sprites | yes | yes | - |
| controls (terminal...) | no | no | above sprites |
| `screen` drawing | **no** | **no** | **above everything** |

---

## The Same Commands on All Three

`canvas`, `world` and `screen` all answer the same drawing API, so whatever
you learn on one works on the others:

| Command | What it does |
| --- | --- |
| `x.clear([color])` | wipe the layer (no color = its back color) |
| `x.color` / `x.alpha` | pen color and transparency |
| `x.backcolor` | the color `x.clear()` uses |
| `x.pen` | line thickness |
| `x.text(px, py, s)` | draw text |
| `x.rectangle(px, py, w, h, fill)` | draw a rectangle |
| `x.circle(px, py, r, fill)` | draw a circle |
| `x.triangle(x1, y1, x2, y2, x3, y3, fill)` | draw a triangle |
| `x.line(x1, y1, x2, y2)` | draw a line |
| `x.point(px, py)` | draw one pixel |
| `x.width`, `x.height` | the size of the layer (usually the window) |
| `x.gradient.rectH(...)`, `x.gradient.rectV(...)`, `x.gradient.circle(...)` | gradient fills |

*(replace `x` with `canvas`, `world` or `screen`)*

> Gotcha: the alpha lives inside the pen color. Setting `x.color` replaces the
> whole color including its transparency, so to draw semi-transparent set the
> **color first, then the alpha** - `x.color = colors.red` after `x.alpha = 100`
> would snap the transparency back to full.
> (`screen` is a buffer first filled and then blended onto the window, so a
> wash there is blended twice: pick a higher alpha than the final look suggests.)

---

## Rules of Thumb

- Draw the **background** with `world.backcolor` or `canvas`.
- Draw **game shapes** near your sprites with `world` or `canvas` - they move
  and shake together with the sprites, so everything feels like one world.
- **Drawing with `world` goes inside `draw()`**; its properties
  (`backcolor`, `title`) and `shake()` can be set anywhere.
- Draw the **HUD** with `screen` - it stays glued to the window like glass,
  above all drawing, immune to shaders and shakes.
