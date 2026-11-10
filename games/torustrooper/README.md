# Torus Trooper (Tyro Game Engine)

A time trial inside a torus for kids, built for Tyro on the free
perspective camera and the engine's own generated shapes — one torus,
some boxes and spheres, one fixed loop to master, and a rider whose own
delta-wing jet (`Ship.glb`) replaces the box the moment it loads.

## Overview

Fly the inside of a giant ring. The clock is the whole game: it always
runs down, and only pickups buy it back. Energy cells give a breath of
time, boost gates give a lot — but a gate only pays when you fly through
its middle, and the walls wear blocks that punish leaving the racing
line. Every crash costs time and speed. The loop never changes, so lap
after lap you learn where each gate, cell and block lives — the same
memorizable course that made the arcade original (Kenta Cho's
*Torus Trooper*, 2001) a classic. Each lap lifts the speed ceiling and
drains the clock harder, until your luck runs out; then it is just you,
the loop, and the high score.

This is the showcase game for the free 3D camera: a real perspective
eye that rides the tube, rolls with it, and shakes on impact — through a
glass ring hung in a shining sky.

## How to Run

```
bin\tyro.exe games\torustrooper\torustrooper.tyro
```

## Controls

| Key / pad        | Action                              |
| ---------------- | ----------------------------------- |
| LEFT / RIGHT     | slide across the tube wall          |
| UP / DOWN        | throttle up / down (A / X on pad)   |
| left stick       | slide / throttle on the pad         |
| SPACE / START    | start the ride, retry after game over |
| P                | pause                               |
| M                | mute                                |
| W                | vector mode — glowing wires on/off  |
| ESC              | quit                                |

## Gameplay

- **The clock** starts at 35 seconds and always drains (a little harder
  every lap, capped). Zero ends the run. Every ride opens with a short
  **count-in**: the clock waits, the walls can't bite, and distance
  already pays — then it is all yours.
- **Energy cells** (lime spheres in arcs across the wall, never on the
  center line): +time, +a touch of speed, +score. Re-armed at every lap,
  so the track is the same course every time round.
- **Boost gates** (gold rings): +2.5 s and a hard push — only from the
  middle of the ring. Off-center, they are decoration. They flash lime
  when they pay.
- **Walls** (maroon blocks half-sunk in the wall): a crash — speed cut,
  4 seconds gone, camera shake, and a moment of mercy before the next
  hit counts.
- **Laps** pay a bonus and raise the multiplier; the distance flown
  scores all the time. Laps are also **clocked** — each crossing prints
  its time across the track, a quicker lap goes down in lime as
  NEW BEST, LAST and BEST ride the HUD panel, and the best lap is called
  on the game-over screen. The high score lives in `torustrooper.hi`,
  and a run that beats it gets a NEW HIGH SCORE call.

## Engine Showcase

- **Free camera** — `world.camera()` rides the tube behind and above the
  ship, with its up-vector rolling around the loop; `world.fov()` sets
  the wide 74° eye; the title screen flies a slow attract orbit around
  the whole ring. The crash shake jitters the camera position directly
  (`shake()` belongs to the pixel pass and does not move the 3D world).
- **Generated shapes** — `models.torus` for the track, liner, ribs
  and gates, `models.box` for the ship and walls, `models.sphere` for
  the planet, stars and cells. The whole game is one fixed layout from
  `torustrooper_data.layout(seed)`, so the course is the same every run.
- **The rider's own hull** — the ship's box is a spare hull: `model:load`
  swaps in the hand-made delta-wing `Ship.glb` the moment it lands, sized
  to the box's 20-unit nose-to-tail and aimed with `model.nose = "-y"` —
  the axis the file's nose lives on once the loader has stood the glTF
  upright. Its intakes wear `model.glow = "Engine"`, so the file's own
  engine material stays full-bright cyan no matter how the ring's lamps
  dim the hull. No file, no fuss: the engine shrugs, the box flies, and the
  game never notices the difference.
- **The liner** — the model pass lights faces that look away from the eye
  as back faces, so the inside of a plain torus is a dim shell no matter
  where you fly. The trick rides in the engine's negative scale: a second
  torus a hair narrower at `scale = -1` mirrors the mesh so its faces (and
  its light) point into the tube, and the depth test lets it win from
  inside while the plain track wins from outside.
- **The tube as glass** — both skins wear `model.alpha` (`WALL_ALPHA` on the
  liner, the thinner of the two, `TRACK_ALPHA` on the outer skin, which keeps
  more body for the title's orbit), so the starfield outside shines through the
  ceiling and the floor of the chute and the ride flies in a lit aquarium rather
  than a painted tunnel. The engine backs this up: a see-through body is painted
  after the solids, far end first, and keeps no depth of its own, so the sky
  shows through the glass whichever order the world was built in.
- **A painted sky** — the stars are not models at all: each one is ink on the
  world canvas, a soft radial gradient (`canvas.gradient.circle`) under a hard
  point, projected by hand from the free camera that `world.camera()` reads back.
  So the sky blooms instead of scattering discs, the field can be as thick as it
  likes without costing a boot frame (the ride boots at 72 bodies), and since the
  glow rides under the model pass the glass tube dims a star exactly as it dims
  the sky behind it. Every star beats its own twinkle, and the one number drives
  the halo, the bloom and the point together.
- **`model:face(target, up)`** — the ship's long axis aims down the
  road and its roof into the tube; the gates and tunnel ribs stand
  square to the road; all of it recomputed every frame as the loop
  turns.
- **The screen layer** — under the free eye the models paint over the
  world canvas, so the HUD, the panels and the title band are drawn on
  `screen.*` (remember `screen.clear()` — that layer keeps last frame's
  ink).
- **Procedural sound** — `sound.loopPlay("Engine", ...)` drones under
  the ride with `sound.loopPitch()` climbing with speed; cells, gates,
  countdown ticks, the crash and the time-up jingle are one-shot
  `music.sound` / `sound.noise` / `music.mml`, all behind a mute switch.
- **Lit in 3D** — the ride runs with `world.lights(true)`: the sun along
  the engine's default ray and the sky's ambient fill shade every solid
  (lambert), and a headlamp (`world.pointlight`) rides a nose-ahead off
  the floor, so the tube ahead is lit while the tube behind falls back
  to the dim sky fill — the ride reads its own direction.
- **Vector mode (`W`)** — the whole ride as bare wireframe (`world.wire`)
  with `neon.frag` on the board so every wire blooms like a hot CRT tube.
  The HUD rides the screen layer above the board, so it stays crisp while
  the track glows.

## Files

| File                          | What it is                                        |
| ----------------------------- | ------------------------------------------------- |
| `torustrooper.tyro`           | the game — camera, entities, HUD, sound, states   |
| `torustrooper_data.tyro`      | pure rules and tuning (single source of truth)    |
| `Ship.glb`                    | the rider's own jet (optional — the box flies without it) |
| `neon.frag`                   | the wireframe bloom for vector mode (`shader.load`) |
| `torustrooper.hi`             | the high score (generated, git-ignored)           |
| `../../tests/test_torustrooper_data.tyro` | headless checks of the rules module  |
| `../../tests/test_torustrooper_play.tyro` | headless ride: drives the game itself |

## Testing

Tyro resolves `require()` inside the script's own folder, so the tests
stage copies next to themselves first:

```
Copy-Item games\torustrooper\torustrooper_data.tyro tests\ -Force
Copy-Item games\torustrooper\torustrooper.tyro tests\torustrooper_play.tyro -Force
Copy-Item games\torustrooper\neon.frag tests\ -Force
Copy-Item games\torustrooper\Ship.glb tests\ -Force
bin\tyro.exe tests\test_torustrooper_data.tyro --console --exit:5
bin\tyro.exe tests\test_torustrooper_play.tyro --console --exit:10
```

(`Ship.glb` is optional here too — without it the tests still pass on the
spare box hull; with it the jet path gets its miles.)

With `TORUSTROOPER_TEST` set the game exports its internals instead of
playing (and stays silent), so the play test drives real frames: cells,
gates, crashes, mercy time, pause, the lap seam and the game-over
retry all run through the actual game code.


###

Ship mesh https://sketchfab.com/3d-models/spaceships-69d7d84573be4e358d92d63e9cee4e50