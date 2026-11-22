# Tyro Roadmap — Closing the Gap with Godot

Status review: 2026-11-22. Tyro already beats Godot on the **first ten minutes**
(open → F2 edit → F5 run, one window, one file). It loses on **sharing the
result, recovering from mistakes, and polish tools**. This roadmap fixes those,
in priority order. Each ticket is sized as one working session (see AGENTS.md:
split big tasks into subtasks).

## What Tyro already has (the moat — never lose it)

- Zero-setup loop: one window, F2 editor, F5 rerun, console + terminal
- Sprites, sprite editor (F3), sheets / atlas / Aseprite, `sprites.puff/blob`
- Tilemaps, Chipmunk physics, particles, parallax, 2D + free camera, shaders,
  screen shake, controls (buttons, lists), gamepad, network + JSON
- MML music + synth waveforms (Kick/Snare/Engine/Siren…) — Godot has nothing
  like this for retro fun
- Scene timers `after()` / `every()` / `cancel()`, `update(dt)` / `draw()`

---

## P1 — Share it (packaging & export) — the #1 gap

A kid's game currently only runs inside `tyro.exe` + `raylib.dll`. If they
cannot send it to a friend, the fun stops.

- [ ] `--export=<dir>`: bundle script + assets + a minimal engine runtime into
      one self-contained folder that runs with `tyro game.lua` — no paths to fix
- [ ] Zip the bundle in one step (`tyro --export=game.zip`)
- [ ] Icon + title per game (`--set-title`, window config file inside the bundle)
- [ ] Stretch: web/WASM build so a game becomes a shareable link (raylib has
      WASM support). A link beats Godot's exporter for kids — nobody installs
      anything.

## P2 — Forgiving errors (debugging for kids)

- [ ] Catch script errors with file + line and show a **red banner inside the
      window**, not only console text
- [ ] Highlight the offending line when the F2 editor opens
- [ ] Plain-language hints: `keyboard.presed` → "did you mean `pressed`?"
      (nearest-name suggestion for unknown members)
- [ ] Show the *timer/tween stack* name in callback errors ("in after() at
      game.lua:12")

## P3 — Juice API (tween / animation helpers) ✅ IN PROGRESS — this task

Games feel dead without tweening; today every game in `games/` hand-rolls it.

- [x] `tween(obj, prop, value, ms [, easing [, done]])` — glide any numeric
      property of a sprite/control/plain table, driven on the scene clock
      (rides `timescale`, sleeps in `pause()`)
- [x] `tween.from(obj, prop, startValue, endValue, ms [, easing [, done]])` —
      the punch pattern: set at once, glide back
- [x] `tween.kill(id)` / `tween.killall(obj)` / `tween.count()`
- [x] Easings: linear, sine, quad, cubic, expo, back, elastic, bounce — each
      with `in` / `out` / `inout`; aliases `smooth`, `snap`, `wobble`
- [x] Small demo game using only the juice API (`games/bubblepop`)
- [ ] Later: `tween.color` (ARGB glide) and `tween.path(points, ms)`

## P4 — Events, not polling (mini signal system)

- [ ] `on("hit", fn)` / `emit("hit", a, b)` — a tiny event bus riding the same
      queue as timers
- [ ] Physics collision events through it (`on("collision", fn)` with the two
      sprites)
- [ ] Chipmunk **joints** exposed (`joint(a, b, opts)`) — no joints means no
      Angry Birds / ragdoll
- [ ] Fire-and-forget input binds: `on("space", fn)` next to `keyboard.pressed`

## P5 — Save it (persistence API)

`savejson/loadjson` exist but a kid must invent the file name and shape.

- [ ] `save("score", 120)` / `load("score")` — key/value sugar writing one
      `save.json` per game folder
- [ ] `save("hiscores", {..})` — any JSON-able value
- [ ] Keep the per-game `score.hi` convention (`score.hi_save`) working

## P6 — Touch & a second platform

Kids live on tablets; Tyro is effectively Windows-only.

- [ ] `touch.x / touch.y / touch.down / touch.count / touch.pressed` over
      raylib's touch input (also maps mouse on desktop, so scripts stay uniform)
- [ ] Linux build parity (FPC already targets it; raylib.so packaging)
- [ ] Stretch: Android build (this is where the P1 export pipeline pays off)

## P7 — Real text (Unicode / RTL / TTF)

- [ ] TTF font loading path (raylib `LoadFont`) selectable per script:
      `canvas.font("arial.ttf", 24)`
- [ ] RTL text layout + Arabic shaping — Godot does it, no kid-engine does;
      a differentiator for the home market
- [ ] Keep pixel fonts as the default so retro scripts are untouched

## P8 — Tilemap painter (F6?)

Loading tilemaps works; authoring one means hand-writing data.

- [ ] In-engine tile painter over the existing `tiles.*` API: pick tile from a
      sheet image, click/drag to paint, save back to the tilemap data file
- [ ] Reuse the F3 sprite editor's input/cursor machinery

## P9 — Learning ladder & community (what makes an engine win)

- [ ] "Make 5 games" official tutorial course in `help/` (each lesson is one
      short script that grows per chapter)
- [ ] Split README.md into a real docs site (it is 75 KB today — great
      reference, terrible onboarding)
- [ ] Game gallery: `tyro --export` bundles should be droppable into one folder
      others can run

## Not chasing (deliberately)

- Godot-style scene/inspector editor — would kill the one-window identity
- Serious 3D (PBR, skinned animation, navmesh) — keep toy-3D as a bonus
- C# / multiple language backends — Lua + MML is the right beginner stack
