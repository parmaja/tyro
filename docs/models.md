# 3D Models

[<< Tyro docs index](README.md)

The `models` system holds 3D meshes that the engine draws every frame, in a
top-down orthographic pass that is pixel-aligned with the 2D canvas — a model
at `x, y` sits on exactly the same pixel as a sprite or a `canvas.rectangle`.
It is the easy way to put a real 3D object inside a 2D game. The recommended
file format is **GLB** (binary glTF 2.0), a single self-contained file with
mesh, texture and material. For a full 3D scene — a perspective eye of your
own inside the world — there is the Free Camera section further down.

## Creating & Loading

| Function | Description |
|----------|-------------|
| `models.new("name"?)` | Create a new model object. Optional name registers it for lookup. |
| `models.sphere("name"? [, radius])` | An engine-built sphere (radius `1` by default, so the bare call is 2 wide and centred) — ready-made puffs, bubbles and planets with no file at all. |
| `models.box("name"? [, w, h, d])` | An engine-built flat-shaded box, `w × h × d` units along x/y/z (unit by default) — the vector block of a 3D world. |
| `models.cylinder("name"? [, radius, height])` | An engine-built drum standing on the z axis (unit by default), smooth on the mantle with hard lids — posts, wheels, pipes. |
| `models.cone("name"? [, radius, height])` | An engine-built cone standing on the z axis with its apex up (unit by default) — roofs, spikes, rockets. |
| `models.torus("name"? [, radius, tube])` | An engine-built ring lying on the x/y plane (radius `1`, tube `0.25` by default): `radius` is centre-of-hole to centre-of-tube, `tube` the thickness of the ring itself — a race loop, a portal, a donut. |
| `models("name")` | Look up a model by name (returns the model object or `nil`). |
| `models.find("name")` | Same as above. |

A named model is also exposed as a global, so after `models.new("jet")` you
can write `jet.load(...)` directly. Model files are searched in the script
directory and the workspace, like sprites.

## Model Methods

| Method | Description |
|--------|-------------|
| `model:load("jet.glb")` | Load (or replace) the mesh. Loading happens on the engine's queue, like sprite textures; `model.loaded` tells when it is ready. |
| `model:show()` | Make the model visible (shown by default). |
| `model:hide()` | Hide the model from rendering. |
| `model:move(x, y[, z])` | Set the screen position (pixels) and optional depth in one call. |
| `model:face(tx, ty, tz[, ux, uy, uz])` | Point the model's own +y axis — its nose, wherever `model.nose` says it lives — at the target `tx,ty,tz`; with the optional up vector, its own +z leans toward it. Aim once and fly; `angle`/`roll`/`tilt` still add small trims on top. (Meaningful under the free camera; see below.) |
| `model:play([what])` | Start a skeletal animation by name or index (`model:play("Walk")`); with no argument it replays the last clip (or the file's first). Returns whether a clip was found. The engine poses the bones every frame while it plays. |
| `model:stop()` | Stop the skeletal animation, keeping the current pose. |
| `model:free()` | Unload the model now: its mesh, textures and draw entry are gone (aliased by `model:destroy()`). Like a freed sprite, the old table stops addressing anything. |

## Model Properties

| Property | Type | Description |
|----------|------|-------------|
| `model.x` | `number` | Screen X of the model's centre, in canvas pixels (read/write). |
| `model.y` | `number` | Screen Y of the model's centre, in canvas pixels (read/write). |
| `model.z` | `number` | Depth: smaller draws in front of bigger (read/write). |
| `model.angle` | `number` | Spin in degrees around the view axis, clockwise (read/write). |
| `model.roll` | `number` | Bank in degrees around the model's own axis — the nose-tail line of a jet — so it leans into a turn like the classic 1942 sprite effect (read/write). |
| `model.tilt` | `number` | Bank in degrees around the screen's horizontal axis — foreshortens the model as it leans (read/write). |
| `model.scale` | `number` | Uniform scale in model units; combine with the bounding box below to size in pixels (read/write). A negative scale mirrors the mesh — winding and light flip with it — which lets a solid wear an inside-out skin (the tube liner in `games/torustrooper` is a torus at `scale = -1`). |
| `model.visible` | `boolean` | Whether the model is drawn (read/write). |
| `model.shadow` | `boolean` | Cast a real shadow: the mesh's true silhouette projected along the sun onto the ground plane — see Light & Shadows below (read/write, default `true`). |
| `model.tint` | `number` | Colour multiplied over the mesh, the same integer format as `canvas.color` / `colors.*`; white is the default (read/write). |
| `model.alpha` | `number` | Opacity 0..255, folded into the tint's alpha like a sprite; a see-through model also casts a proportionally softer shadow (read/write). |
| `model.name` | `string` | The model's name (read/write). |
| `model.nose` | `string` | Which own axis `face()` and `roll` treat as the nose-tail line: `"y"` (the default — engine-built shapes and hand-made boxes fly on their +y), or `"-y"`, `"z"`, `"-z"`, `"x"`, `"-x"` when a loaded file draws with its nose elsewhere. The loader stands glTF files upright, so a jet modelled nose-up in Blender typically asks for `"-y"`; when in doubt, point the model at something and try. The fix rides along in the draw — the mesh itself is never touched (read/write). |
| `model.glow` | `string`/`boolean` | The material that shines instead of being lit. A name from the loaded file (e.g. `"Engine"`) keeps just that material at full bright, immune to `world.lights` — glowing intakes, a lamp, a screen. `true` (or `"*"`) shines the whole model, ideal for a thruster flame; `""`/`false` (the default) leaves it to the lamps. The names come from the file's own JSON (raylib drops them), so a name the file does not carry simply never matches. It only shows while lights are on — with none, every material is already flat-lit (read/write). |
| `model.bright` | `number` | How hard a shining material burns: `1` (the default) is the colour the file was painted with, `2` twice as hot, and any level climbs toward white-hot — the difference between a painted panel and a lamp you can look at. Clamped to `0..16` and snapped to 1/8 steps, so a script that pulses the shine (an engine spooling up) costs one shader, not one a frame. It speaks for a glowing material only: a body the lamps light keeps its shading, and — like `model.glow` — the level only shows while `world.lights` is on (read/write). |
| `model.width` | `number` | Bounding-box X size of the loaded mesh (read-only). |
| `model.height` | `number` | Bounding-box Y size (read-only). |
| `model.depth` | `number` | Bounding-box Z size (read-only). |
| `model.loaded` | `boolean` | Whether the mesh is ready (read-only). |
| `model.animCount` | `number` | How many skeletal animation clips the loaded file carries (read-only). |
| `model.anim` | `number`/`string` | The clip now posed — read its index (`-1` = the bind pose), or write a name/index to play it, like `model:play`. |
| `model.animPlaying` | `boolean` | Whether a clip is playing (read/write; `false` stops and holds the pose). |
| `model.animTime` | `number` | Position inside the current clip in seconds (read/write — write it to scrub a stopped walker frame by frame). |
| `model.animSpeed` | `number` | Playback speed in keyframes per second; the fresh `30` is the clip's authored pace, `60` doubles it, `0` holds the pose while playing. |
| `model.animName` | `string` | The current clip's name, or `""` before anything plays (read-only). |

Models draw above the 2D canvas and sprites, below the controls, and they
shake with the screen like everything else. All three rotations (`angle`,
`roll`, `tilt`) turn the model about its own bounding-box centre, and the
model always faces the viewer with its own +Z pointing up the screen — so a
nose-up jet model needs no rotation for a player flying up the screen, and
`angle = 180` for the enemies flying down it. With `shadow` on (the default)
every model drops its true silhouette on the ground plane below — see
Light & Shadows.

```lua
window.show(320, 240)

local jet = models.new("player")
jet:load("jet.glb")

-- size it by pixels: widest horizontal extent becomes 46 px
local size = math.max(jet.width, jet.depth)
jet.scale = 46 / size
jet.tint = colors.white

local t = 0
function update(dt)
  t = t + dt
  jet:move(160 + 90 * math.sin(t), 150, 0)
  jet.roll = 30 * math.sin(t * 2)     -- bank left/right into the turn
end
```

`games/jet2026/jet2026.tyro` is a 1942-style scrolling shooter built this
way: one GLB jet reused through a pool for the player and every enemy,
tinted and rotated per role. Its clouds are stacks of `sprites.puff`
sprites (one batch per cloud), their soft edges fusing into fluff, and the
sea is a live one: `sea.frag` rolls swells, sun glitter and drifting crests
over the board's blue pixels, masked so islands and bullets keep their
sharp shapes.

A GLB can also carry a **skeleton and baked animation clips** (the format
stores them alongside the mesh). `model:load` picks those clips up and
`model:play("Walk")` switches between them; the engine then poses the bones
every frame on its own, so a script only names the clip and gets on with
the game. `games/astro/astro.tyro` shows it: a little astronaut patrols a
lunar ridge, playing his `Walk` clip as he paces and an `Idle` clip while he
rests at each end, and you can steer him with the arrows.

## Light & Shadows

Cast shadows share one sun, and it lives on the `world` table. Every
function below reads when called with no arguments and sets (then returns)
when called with them.

| Function | Description |
|----------|-------------|
| `world.light([dx, dy, dz])` | The direction the sunlight travels (`dy` may not be 0). The default `(0.06, 1, -0.035)` sits the sun high behind the viewer, so shadows slide a little right and down. With `world.lights` on it is also the sun that shades the solids. |
| `world.ambient([v])` | How much sky light fills the shadows, `0..1` (clamped). `0` is a hard sun with the darkest shadows, `1` is all ambient light so nothing stays dark. Default `0.45`. With `world.lights` on it is also the fill light of the shaded solids. |
| `world.ground([z])` | The `model.z` plane shadows fall on — the "floor" (the sea, say), which must sit farther from the eye than the models flying over it. Default `300`. |

A shadow is the mesh projected along the light ray onto that plane, so it
is the model's true silhouette — wings, bank and all — and it slides
farther away the closer the model sits to the viewer. Models nearer the
eye than the ground plane paint over the shadows behind them, so a 3D
cloud between the viewer and the sea covers the shadow under it and never
catches one on top.

```lua
world.light(0.10, 1, -0.05)   -- shadows land to the right and below
world.ambient(0.45)           -- soft darkness, sky light fills the rest
world.ground(300)             -- the sea plane, far behind the jets
```

