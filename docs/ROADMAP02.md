# Tyro Roadmap 02 — Closing the Kid-Facing Gaps

Status review: 2026-11-25. Baseline commit `103a991` (reviewed tree with local
uncommitted edits in `TyroLua.pas`, `TyroModels.pas`, `TyroParticles.pas`,
`TyroSprites.pas`). Follow-up to [`REVIEW01.md`](REVIEW01.md) (2016-era product
review), [`report01.md`](report01.md) (crash/memory audit) and
[`ROADMAP.md`](ROADMAP.md) (Godot-gap roadmap). This document records what a
second, code-based audit found **still missing** for a kids/noobs engine, with
file-and-line evidence. Each P0 item is sized as one working session (AGENTS.md:
split big tasks into subtasks).

---

## What is already handled — the moat, never lose it

- Real Lua sandbox: `io`, `os`, `dofile`, `loadfile`, `package`, `debug` removed;
  `require` and `openfile` restricted to the workspace
  (`..\lua\LuaClasses.pas:491-541`, `TyroLua.pas:6323,6440`).
- Runaway top-level loops: count hook every 100 instructions
  (`..\lua\LuaClasses.pas:365-375,543-544`; `TyroLua.pas:3004-3011`).
- Workpath is the script's directory (`TyroEngines.pas:2980-2989`) with a
  documented asset search chain (`TyroClasses.pas:1771-1803`).
- F2 script editor: syntax highlight, 100-step undo/redo, error-line jump
  (`TyroEditors.pas:27,71-72,726-780,1295,1740`); F3 sprite editor with its own
  undo/redo (`TyroSpriteEditor.pas:34,1693-1744`).
- Clean F5 recovery: controls, sprites, images, tiles, models, particles, queue,
  timescale, camera, music and error state all reset (`TyroEngines.pas:3003-3062`).
- Modern callback loop (`update(dt)`/`draw()`), tweens, events, joints, touch,
  `save`/`load`, `--fuse` packaging. Zero legacy `cycle` in 81 scripts.
- Kid-level building blocks already present: `sprite.onCollide`,
  `on("collision")`, `controls.new("edit")`, `console.read()`, `score.hi_*`,
  `rng()`, `clamp/lerp/sign/dist/aabb`, one-shot sounds with a global mute,
  `images.new` + `sprite:fromimage` (draw a sprite without an image file).

---

## P0 — Broken promises (do these first)

### P0-1 — Documented / demoed APIs that do not exist — ✅ done 2026-11-26

- [x] Re-enabled the ten `controls.*` table registrations that were commented
      out in `629d0736`: `caption`, `text`, `checked`, `items`, `item`,
      `additem`, `clear`, `viewcount`, `itemindex`, `load`. They are thin
      wrappers around the same queued operations as the field spellings, so
      restoring them fixes the docs, the demo and the goldens at once.
- [x] `demos/controls_align.tyro:38,46,49,54` (`controls.text(...)`) now runs
      without a nil-call error (verified with `--exit:2`).
- [x] Four stale goldens are valid again: `tests/test_utf8.lua/.txt`,
      `tests/test_image.lua/.txt`, `tests/test_list.lua/.txt`,
      `tests/test_props.lua/.txt`.
- [x] Added `game.pause_down()` — `TTyroEngine.PauseDown` (keyboard `p` or pad
      `start`/`menu` held) plus `TLuaGame.PauseDown_func` and registration;
      updated `help/game.md` so the `*_down` list includes it.
- [x] Retired the wrong spellings instead of adding aliases: `music.noise` →
      `sound.noise` in `docs/getting-started.md`, `docs/examples.md`,
      `demos/README.md`; `window.shake` / `window.shaking` → `world.shake` /
      `world.shaking` in `docs/effects.md`, `docs/examples.md`,
      `demos/README.md`. No `TLuaMusic.Noise_func` or `TLuaWindow.Shake_func`
      exists in the code.
- [x] Added `tests/roadmap02_p0_api_test.tyro` (17 checks, including negative
      checks that the retired spellings stay absent) and its golden
      `roadmap02_p0_api_out.txt`; both pass with exit 0.
- [ ] Still to do: fold the new checks into a single documented test runner
      (see P2 hygiene) so the whole suite is repeatable, not just this file.

### P0-2 — Errors and asset failures are invisible in a GUI session

- [x] First slice (2026-11-26): `ReportScriptError` now mirrors the message,
      line number and "Press F2 to edit, F5 to run again" into the visible
      `Output` panel as well as the hidden console. A kid in a normal GUI run
      can see the failure without opening F8.
- [x] Second slice (2026-11-26): added `TTyroEngine.Notice`, which writes to
      the visible Output panel and the console. Typo hints ("did you mean")
      and missing-asset messages now use it; `OnAssetMissing` lets
      `TyroClasses` report a miss without depending on `TyroEngines`.
- [x] Missing images now draw a 32×32 magenta/black checker placeholder and
      cache it, so one absent PNG neither blanks the game nor logs every frame.
      Verified with `tests/roadmap02_p0_feedback_test.tyro` (unknown member +
      missing image) — exits 0.
- [x] Third slice (2026-11-26): `DrawErrorBanner` paints a red strip at the
      bottom of the window while the last failure is unfixed (cleared on rerun).
      Verified with `tests/roadmap02_p0_banner_test.tyro` + `--screenshot:1`.
- [x] Missing sprites get the same magenta/black checker (sprite-owned texture),
      missing sounds and shaders report through `OnAssetMissing`, and invalid
      shaders simply leave the game running without that effect.
- [x] **P0-2 complete.** The remaining banner/placeholder items in the older
      [`ROADMAP.md`](ROADMAP.md) P2 are now covered here.
- [ ] Show the console or a red in-window error card on script failure;
      route engine hints to the visible Output panel; keep "Press F2 to edit"
      actionable.
- [ ] Missing image: `WriteLn('Image not loaded: …')` plus an empty cached
      texture, no placeholder (`TyroClasses.pas:1901-1927`, `:1066-1068`).
      Missing sprite: console-only (`TyroSprites.pas:2520-2527`,
      `TyroLua.pas:9502-9504`). Add a friendly placeholder and a plain-language
      "put the file next to your script" message.
- [ ] No stack trace: `luaL_traceback` is declared (`LuaAPI.pas:547,868`) but
      never used. `ParseLuaErrorLine` takes the first `:digits:` in the message
      (`TyroEngines.pas:2590-2605`), so a string containing `:12:` can highlight
      the wrong line.
- [ ] Queued-work exceptions are logged only when a console exists
      (`TyroEngines.pas:1163-1174`).
- [ ] Repeated callback errors are de-duplicated into silence
      (`TyroEngines.pas:2620-2621`); show a "still failing ×N" count.

### P0-3 — Bare `--exit` hangs when the script errors — ✅ done 2026-11-26

- [x] Reproduced: `bin\tyro.exe tests\syntax_error_audit.tyro --exit` never
      exited (killed at 15 s); `--exit:2` exited 1 with a caret diagnostic.
      `docs/getting-started.md:51-52` promises exit status 1.
- [x] Fixed the race: `OnScriptFinished` now latches `FScriptFinished` before
      any early exit (including the error branch), `AutoLifecycle` tests that
      latch instead of `FScriptThread.Started and Completed`, and the latch is
      reset in `Start` and `RunScriptThread` for reruns.
- [x] Verified: failing script exits 1, `test_cli_exit_script.tyro` exits 0,
      engine-driven `demos/bounce_demo.tyro` still exits 0 with bare `--exit`.
- [x] Follow-up: the two flags are now one `TScriptState` set
      (`ssStarted`, `ssFailed`, `ssFinished`) so a rerun clears them together
      and a failure cannot be "failed but not finished" (enum members carry an
      `ss` prefix because Pascal is case-insensitive and `scriptFailed` would
      collide with the `ScriptFailed` property).

### P0-4 — One line of kid code can kill the engine, silently — ✅ done 2026-11-26

- [x] `sound.play(440, 2000000000)` used to pass an unbounded millisecond value
      → `Round(Duration * SampleRate)` into a 32-bit `Cardinal` → multi-GB
      `MemAlloc`; the resulting exception was swallowed by `EX-QUEUE` when no
      console was open.
- [x] All Lua-facing sound durations (`music.beep`, `music.sound`,
      `sound.beep`, `sound.noise`, `sound.play`) now go through
      `ClampSoundPeriod` (10 s max) with a plain hint. `TTyroRayWave.Generate`
      also caps duration at 10 s as a last line of defence for direct callers.
- [x] Verified with `tests/roadmap02_p0_sound_test.tyro`: four absurd durations
      clamp with hints and exit 0; `demos/sound.tyro` still exits 0.
- [ ] Still open: `font.load` size (`TyroLua.pas:7242-7251`), `file:lines()`
      whole-file reads (`TyroLua.pas:6260-6264`), and `require`'s unprotected
      `lua_call` (`TyroLua.pas:6509`, an `error()` in a module longjmps past
      managed Pascal locals).

### P0-5 — F5 / Stop cannot interrupt every loop — ✅ done 2026-11-26

- [x] Main-thread callbacks now run under a temporary 3-second count hook
      (`CallbackWatchHook` in `TyroLua.pas`). `while true` inside `update()` or
      `draw()` raises a Lua error, reports it in the Output panel and stops the
      run, so the window stays responsive. The original termination hook is
      chained and restored afterwards.
- [x] `sleep()` / `pause()` now sleep in 20 ms steps and watch
      `StopScriptRequested`, so F5 / `stop` ends a waiting script instead of
      blocking until the full duration expires.
- [x] Verified with `tests/roadmap02_p0_watchdog_test.tyro` (infinite `update`
      loop aborts with a "took too long" message, `--exit:5` ends the process),
      plus `demos/animated_demo.tyro` and `demos/bounce_demo.tyro` still exit 0.
- [ ] Known limits (follow-up, not P0): `http_get`/`http_post` and
      `console.read()` are single blocking C calls; F5 can only act after they
      return (the built-in 10 s connect / 15 s read bounds still apply).
      There is still no global break key; `Ctrl+C` is copy in the terminal.

---

## P1 — Workflow and learning gaps

### P1-1 — Docs drift

- [ ] `docs/input.md:30,37` promise that closing F2 or Ctrl+S saves **and
      reruns**; the rerun call is commented out (`TyroEngines.pas:2526`) and
      `EditorSave` only writes the file (`:2737-2760`). Decide: implement the
      rerun or fix the docs.
- [ ] `docs/console.md:26` teaches `dir`, which is not registered; the real
      commands `new` and `sprite` are undocumented (`TyroEngines.pas:3344,3346`).
- [ ] `docs/README.md:35` claims sliders; `controls.new` has none
      (`TyroLua.pas:8615-8617`).
- [ ] `docs/README.md:25` calls `game.*` "score, lives, game-over glue"; it is
      only button polling (`TyroLua.pas:2621-2644`).
- [ ] Undocumented implemented APIs: `gamepad.released/up/count/axes/vibrate/
      rumble`, the whole `joystick.*` alias table (`TyroLua.pas:2592-2617`),
      `radio.*`, `spectrum.*` (`:2554-2564`).

### P1-2 — Broken references

- [ ] `docs/atomic.md` linked from `docs/README.md:50`,
      `docs/getting-started.md:107`, `docs/timing.md:260` — file absent.
- [ ] `docs/network.md:30` → `games/weather/weather.tyro` (folder absent but
      still in `games/games.conf:10`).
- [ ] `docs/tilemaps.md:87` → `games/dungeon/dungeon.tyro` (same, `games.conf:4`).
- [ ] `demos/README.md:81-82` → `midi_demo.tyro`, `spectrum_demo.tyro` absent;
      no `midi.*` API is registered at all.
- [ ] `docs/console.md:25` → `terminal_demo.lua`; the real file is `.tyro`.

### P1-3 — Onboarding

- [ ] `new [game|draw|sprite]` templates exist (`TyroEngines.pas:2790-2897`)
      but are invisible in docs and have no UI picker (F1 shows shortcuts only).
- [ ] `docs/getting-started.md` opens with CLI switches and a million-iteration
      draw loop; no step-by-step first game. "Make 5 games" is still unchecked
      in [`ROADMAP.md`](ROADMAP.md) P9.
- [ ] Quick-start points to `demos/pong.tyro`, which uses `while true` +
      `sleep(16)` and frame-count-based speed (`pong.tyro:18,49-145`) instead of
      the `dt`-scaled loop taught in `docs/timing.md`. Nine demos use
      `while true do`.
- [ ] `demos/diag_sprite.tyro:12` uses `pcall` — against AGENTS.md.
- [ ] `games/waddle/waddle.tyro:26-28` claims the colon form is broken and
      teaches dot-only calls; the engine supports both (`TyroLua.pas:7320-7332,
      10108-10119`).

### P1-4 — Kid-level helpers still missing

- [ ] Scenes: no `scene`, `goto`, or menu/play/gameover switch. Games hand-roll
      it (`whackamole.tyro:523-527,736-766`, `bubblepop.tyro:46,128-155`).
- [ ] Pause/resume toggle (only blocking `pause(ms)` exists), Lua-callable
      `restart()`, difficulty helper, high-score *table* with sorting,
      game-state undo/rewind.
- [ ] Non-blocking "ask name" widget (`console.read()` blocks, `edit` needs
      manual wiring).

### P1-5 — Editor and classroom

- [ ] No autosave anywhere in `src/`; a crash loses the buffer.
- [ ] No step debugger: `docs/DEBUG.md` is a full DBGp design, but
      `src/TyroDebugger.pas` does not exist. Error jump (`GotoLine`) has no test.
- [ ] Sandbox caveats: no `--safe` classroom switch, network unrestricted
      (`http_get`/`http_post`/`radio` with no allow-list,
      `TyroEngines.pas:2646-2730`), scripts can overwrite `main.tyro` and
      `save.json`, containment is lexical only so a junction/symlink can escape
      (`TyroClasses.pas:1805-1830`).
- [ ] Console `save <name>` has no try/except (`TyroEngines.pas:3524`); a
      read-only folder throws to the top-level handler. Editor Ctrl+S already
      shows a friendly toast (`:2749-2758`) — copy that pattern.

---

## P2 — Test and packaging hygiene

- [ ] `tests/` is ~500 ad-hoc files with `_out.txt` goldens, no runner, no CI
      workflow, no README. Several goldens are stale (P0-1).
- [ ] Zero tests for kid-facing paths: error jump, `new` templates,
      `console.read()`, missing assets, the commented-out `controls.*` table
      functions, `game.pause_down`. Negative checks for the retired
      `music.noise` / `window.shake` spellings now live in
      `tests/roadmap02_p0_api_test.tyro`. No test checks that doc links or
      demo-index files exist.
- [ ] Version mismatch: CLI prints `Tyro version 0.1` (`TyroApp.pas:374`) while
      `version.conf` says `0.8`.
- [ ] Asset creation still depends on Python: 11 games ship `make_assets.py`
      despite AGENTS.md preferring Lua/Tyro asset creation. F3 cannot export
      PNG/atlas; no asset browser ([`ROADMAP.md`](ROADMAP.md) P8).

## Not chasing (same as ROADMAP.md)

- Godot-style scene/inspector editor — would kill the one-window identity.
- Serious 3D (PBR, skinned animation, navmesh) — keep toy-3D as a bonus.
- C# / multiple language backends — Lua + MML is the right beginner stack.

## Note on a stale roadmap item

[`ROADMAP.md`](ROADMAP.md) P7 says `L.ToString` folds UTF-8 through the system
ANSI codepage. Current `TLuaHelper.ToString` / `PushString`
(`..\lua\LuaClasses.pas:658-662,741-744`) explicitly preserve UTF-8. Re-verify
before scheduling that work.
