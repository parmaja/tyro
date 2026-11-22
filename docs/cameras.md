# Free Camera

[<< Tyro docs index](README.md)

The model pass runs on a top-down orthographic eye by default — the pixel-
aligned one 2D games want. Hand it a real perspective eye of your own with
`world.camera` and the same world turns into a 3D playground: the space stays
exactly as it was (x right, y into the screen, z up, the same 1000-unit reach
of the depth buffer), only the way you look through it changes.

| Function | Description |
|----------|-------------|
| `world.camera(x, y, z[, tx, ty, tz[, ux, uy, uz]])` | Put the free eye at `x,y,z` looking at `tx,ty,tz` (default the origin) with the given up vector (default world up, `0,0,1`). Any call switches the model pass to this perspective eye. |
| `world.camera()` | With no arguments it reads the shot back as nine numbers — and never changes the mode, so `local x,y,z = ...` after a `world.ortho()` still tells where the free eye was parked. |
| `world.ortho()` | Hand the model pass back to the default top-down eye: `x/y` are screen pixels again and the cast shadows return. |
| `world.fov([deg])` | The free eye's vertical field of view in degrees, clamped to `10..170` (default 60). Read with no argument. |
| `world.wire([on])` | Paint every model as its edges only — `true` for the bare vector look (a glowing grid pairs nicely with `shader.effect = "glow"` or your own `shader.load("...frag")`), `false` for solid faces. Read with no argument; every model keeps its own tint. |
| `world.lights([on])` | Dress the solid models in light: they shade with lambert lighting from `world.ambient` (the sky fill), `world.light` (the sun ray) and up to four point bulbs (`world.pointlight`). Off by default — the classic flat ink — and the wire pass always dresses flat, as a bare grid takes its mood from the bloom. Read with no argument. |
| `world.pointlight(i, x, y, z [, color [, range]])` | Hang one of four bulbs (`i` = 1..4) at a world point. The color takes the same forms as `canvas.color` (default `"white"`); `range` is how far the light reaches before fading out (default 300). A bulb with no range left is dark, and calling with just `(i)` unscrews it. Only shines while `world.lights` is on. |

Under the free eye a model's `x/y/z` are plain **world coordinates** — the
same numbers `world.camera` takes — instead of screen pixels, and the screen
shake and window margin no longer move the models (the camera owns the frame
now). Cast shadows stay with the ortho eye: the ground plane is a top-down
notion, and a free 3D scene reads its depth directly.

Two more things change for the eye's sake. The model pass paints **both
skins** of every solid — back-face culling is off and the depth test alone
decides what is in front — so the eye can fly through bodies; faces that
look away from it are simply lit as back faces. And models paint **over**
the world canvas, so a HUD that must never dive under the scenery belongs
on the screen layer: `screen.text`, `screen.rectangle` (with
`screen.clear()` each frame — that layer keeps last frame's ink).

A **see-through** model — one whose `model.alpha` is under 255 — is painted
after the solids, far end first, and keeps no depth of its own. That is what
makes glass behave like glass whatever order the scripts built the world in: it
always finds what it looks through already painted, and it never hides a body
from the next stroke. Two glasses still sort between themselves, and a solid in
front of the glass hides it as usual.

```lua
window.show(640, 480)
canvas.backcolor = colors.black

local ship = models.box("ship", 5, 11, 3)   -- long axis on its own +y
world.fov(72)

local t = 0
function update(dt)
  t = t + dt
  -- a ship circling the origin at height 20, always nose-first along the ring
  local x, y = 120 * math.cos(t), 120 * math.sin(t)
  ship:move(x, y, 20)
  ship:face(x - math.sin(t) * 10, y + math.cos(t) * 10, 20)
  -- an eye that rides behind it, looking down at where it is going
  world.camera(x * 0.7, y * 0.7, 60, x, y, 10)
end
function draw()
  canvas.clear()
end
```

`demos/torus_demo.tyro` is a whole free-camera game in one file: a generated
torus loop to ride inside, its tube closed by an inside-out liner (see
`model.scale`), boost hoops that stand across the tube and feed your speed,
a chase eye whose up vector rides the tube so the frame rolls as the loop
turns, and a planet parked in the hole to spin against.

`games/torustrooper/torustrooper.tyro` builds the full arcade ride on the
same parts: a time trial inside the tube where energy cells buy back
seconds, boost gates pay only for the middle of the ring, and blocks on
the wall cost both. The course is one fixed layout, so every lap teaches
the next — and every model in it (track, liner, ribs, gates, ship) is an
engine-generated shape. See `games/torustrooper/README.md`.

# 2D Camera

The world has a roving eye of its own: the `camera` table. `camera.x`/`camera.y`
is the world point parked on the window's top-left corner, and `camera.zoom`
magnifies the picture around it. The defaults — `(0, 0)` at zoom `1` — paint
the world exactly as ever, so a game that never touches the camera never
notices it exists. Everything in the world pass rides it: the board, the
parallax, the sprites, the 3D models and the sparks all move as one picture,
while the screen canvas, the controls and the console stay glued to the
window — the HUD they carry.

| API | Description |
|----------|-------------|
| `camera.x` / `camera.y` | The world point parked on the window origin (read/write, default `0`). A side-scroller keeps its hero near the middle by steering the eye: `camera.x = hero.x - canvas.width / 2`. |
| `camera.zoom` | The picture's scale, clamped to `0.05..32` (default `1` — the classic pixel-perfect mapping). |
| `camera.to_world(sx, sy)` | → `wx, wy`: a window pixel (`mouse.x`, `mouse.y`) back into the world — the same math the draw uses, so a click lands exactly where it looks. |
| `camera.to_screen(wx, wy)` | → `sx, sy`: a world point onto the window — the way to hang a HUD marker or a control on a world position. |

The zoom stretches positions and model sizes; the depth axis and the ground
plane the model shadows fall on stay as they are — an ortho picture magnifies,
it does not pull the world nearer. The free camera (`world.camera`) owns its
own eye and ignores these numbers, exactly like it ignores the margin and
the shake.

```lua
window.show(640, 480)
local hero = sprites.puff("hero", 32)

function update(dt)
  hero:move(hero.x + 80 * dt, hero.y)
  -- the eye chases the hero, easing so the ride stays smooth
  local k = math.min(1, 4 * dt)
  camera.x = camera.x + (hero.x - canvas.width / 2 - camera.x) * k
  camera.y = camera.y + (hero.y - canvas.height / 2 - camera.y) * k
end
```

