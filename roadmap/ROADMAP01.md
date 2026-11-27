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

## P4 — Events, not polling (mini signal system) — ✅ done 2026-11-22 (joints split out to P4b)

- [x] `on(name, fn)` / `emit(name, ...)` / `off(id)` — a tiny event bus on the
      same beat as timers/tweens; `emit` answers how many listened
- [x] Physics collision events through it: `on("collision", fn)` with
      `(a, b, "enter"/"leave")`, beside the classic `onCollide`/`pump()`
- [x] Fire-and-forget input binds: `on("space", fn)`, `on("mouse.left", fn)`
      — plus `on("update", fn)` / `on("draw", fn)` scene listeners
- [x] Demo: `demos/events_demo.tyro` (bus + tween juice); API test:
      `tests/event_test.tyro` (15 checks)

## P4b — Joints (split from P4) — ✅ done 2026-11-22

- [x] Chipmunk **joints** exposed: `collision.joint(a, b, opts)` — pin /
      slide / rotator / limit; `joint_free` / `joints` / `joints_clear`;
      auto-cleanup when a sprite leaves the physics world
- [x] Demo: `demos/joints_demo.tyro` (pendulum wave + mouse leash + motor);
      API test: `tests/joint_test.tyro` (27 checks)

## P4c — When a rope snaps taut (split from P4b) — ✅ done 2026-11-22

- [x] `on("joint_stretch", fn)` — every slide measures its anchors after
      the solve and reports the **edges**: `(a, b, "taut"/"slack")`; the
      engine drains the queue each beat even when the bus is deaf
- [x] Demo juice wired: `joints_demo.tyro` bloops and squashes when the
      mouse leash catches the ball; `tests/joint_test.tyro` grew the
      stretch checks (27 total)

## P5 — Save it (persistence API) — ✅ done 2026-11-22

`savejson/loadjson` exist but a kid must invent the file name and shape.

- [x] `save("score", 120)` / `load("score")` — key/value sugar writing one
      `save.json` per game folder; every call hits the disk at once, and a
      key is dropped by leaving its value out (or handing it `nil`)
- [x] `save("hiscores", {..})` — any JSON-able value (functions and sprites
      answer `nil` + a message, never an error)
- [x] Keep the per-game `score.hi` convention (`score.hi_save`) working —
      verified side by side in the tests; `savejson` rewriting the page is
      noticed, a hand-broken `save.json` starts an empty page
- [x] `tests/save_test.tyro` (27 checks) and [`help/save.md`](../help/save.md)

## P6 — Touch & a second platform

Kids live on tablets; Tyro is effectively Windows-only.

- [x] `touch.x / touch.y / touch.count / touch.down / touch.pressed /
      touch.released / touch.mouse` fields over raylib's touch input, plus
      `touch.point(i)` for extra fingers — the mouse stands in as one contact
      on desktop, so scripts stay uniform; `on("touch", fn)` fires `(x, y)`
      on the press beat (`tests/test_touch_frame.lpr` 33 checks,
      `tests/touch_test.tyro`, [`help/touch.md`](../help/touch.md))
- [ ] `touch.dragged` / simple gestures (tap vs swipe) over the raw frames
- [ ] Linux build parity (FPC already targets it; raylib.so packaging)
- [ ] Stretch: Android build (this is where the P1 export pipeline pays off)

## P7 — Real text (Unicode / RTL / TTF)

- [x] Font handles and a font book (P7a): `font.load(name[, size[, codepage]])`
      answers an id (cached — same spec, same id, atlas baked once),
      `font.use(id)` / `font.free(id)` / `font.current()`, `font.glyphs`
      reports the bake; codepages `'ansi'`, `'arabic'`/`'cp864'` (the glyph
      set minibidi's shaped forms live in), `'everything'`
      (`tests/font_test.tyro` 24 checks, `tests/test_font_bake.lpr`,
      [`help/fonts.md`](../help/fonts.md))
- [ ] `canvas.font("name.ttf", size)` sugar over `font.load` + `font.use`
- [ ] RTL text layout + Arabic shaping — Godot does it, no kid-engine does;
      a differentiator for the home market (minibidi + the `'arabic'`
      codepage are the halves that already work)
- [ ] Fix the script string pipeline: `L.ToString` folds UTF-8 through the
      system ANSI codepage, so non-ASCII text becomes `?`/best-fit before
      raylib ever sees it — Arabic drawing waits on this
- [ ] `'everything'` bakes raise EInvalidOp out of raylib's loader on big
      TTFs (old font survives, script continues) — needs its own session
- [ ] Keep pixel fonts as the default so retro scripts are untouched

## P8 — Tilemap painter (F6?)

Loading tilemaps works; authoring one means hand-writing data.

- [ ] In-engine tile painter over the existing `tiles.*` API: pick tile from a
      sheet image, click/drag to paint, save back to the tilemap data file
- [ ] Reuse the F3 sprite editor's input/cursor machinery

## P9 — Learning ladder & community (what makes an engine win)

- [ ] "Make 5 games" official tutorial course in `help/` (each lesson is one
      short script that grows per chapter)
- [x] Split README.md into a real docs site: the API reference now lives in
      `docs/` (16 topic pages with a start-here index at `docs/README.md`);
      the root README is a short landing page
- [ ] Game gallery: `tyro --export` bundles should be droppable into one folder
      others can run

## Not chasing (deliberately)

- Godot-style scene/inspector editor — would kill the one-window identity
- Serious 3D (PBR, skinned animation, navmesh) — keep toy-3D as a bonus
- C# / multiple language backends — Lua + MML is the right beginner stack
