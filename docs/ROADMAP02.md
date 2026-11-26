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
- [x] One runner for all P0 checks:
      `powershell -ExecutionPolicy Bypass -File tests\run_roadmap02.ps1` from
      the repo root. It checks exit codes for 10 scripts plus the banner
      screenshot command; all 10 passed after the final rebuild. The runner
      lives in `tests/`, which is gitignored, so it stays untracked on disk.

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
- [x] Error card + visible hints: red banner, Output mirror and "F2 to edit,
      F5 to run again" are all in place (slices 1–3).
- [x] Missing image/sprite get a magenta/black checker; sounds and shaders
      report a plain-language "put the file next to your script" message.
- [x] `TLuaHelper.RunString` now compiles with a fixed `@tyro_script` chunk
      name (so `:12:` in a string cannot fool the line parser) and appends
      `luaL_traceback`; the banner shows a one-line summary.
- [x] Queued-work exceptions go through `Notice` (visible Output panel) instead
      of `WriteLn` gated on `IsConsole`.
- [x] Repeated identical callback errors now update the banner with
      "(still failing xN)" instead of disappearing into de-duplication.

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
- [x] `font.load` size is clamped to 1..256 with a hint; `file:lines()` now
      streams 64 KB chunks (100k-line / 1 MB-per-line caps) instead of reading
      the whole remainder; `require` uses `lua_pcall` and returns `nil, msg`
      when a module calls `error()`, instead of longjmpping past managed locals.
- [x] Verified with `tests/roadmap02_p0_robustness_test.tyro` (3 checks), the
      24-check `font_test.tyro`, and `test_openfile.tyro`.

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
- [x] `http_get` / `http_post` check `StopScriptRequested` before and after
      the socket call (bounded by the built-in 10 s connect / 15 s read
      timeouts), and `console.read()` waits in 100 ms slices so F5 can end a
      waiting script. No infinite block remains reachable from a kid script.
- [ ] Follow-up (not P0): a global break key for a script blocked inside a
      single native call; `Ctrl+C` is still copy in the terminal.

---

## P1 — Workflow and learning gaps

### P1-1 — Docs drift

- [x] Fixed `docs/input.md`: closing F2 copies the buffer to the in-memory
      source, Ctrl+S writes the file, and F5 reruns — matching the code.
- [x] `docs/console.md` now uses `list` instead of `dir` and documents the real
      `new` / `sprite` commands; `demos/README.md` lists them too.
- [x] Removed the phantom "sliders" claim from `docs/README.md:35`, and fixed
      the `game.*` description to button pairs + `score.*` high scores.
- [x] Documented the missing APIs in `docs/input.md` (gamepad released/up/
      count/axes/vibrate/rumble and the `joystick.*` alias) and `docs/network.md`
      (`radio.*` / `spectrum.*`).
- [ ] Follow-up (not P1-critical): an in-app API reference with examples; the
      `help` console command still lists commands only.

### P1-2 — Broken references

- [x] All three dead `atomic.md` links now point to `engine-loop.md`, which
      owns the `enter()`/`leave()` grouping section.
- [x] Removed the missing `games/weather` and `games/dungeon` references from
      `docs/network.md`, `docs/tilemaps.md` and `games/games.conf`.
- [x] Removed the nonexistent `midi_demo.tyro` / `spectrum_demo.tyro` rows from
      `demos/README.md`; no `midi.*` API exists.
- [x] `terminal_demo.lua` → `terminal_demo.tyro` in `docs/console.md`.
- [x] Verified: no broken relative links remain in `docs/` or `help/`.

### P1-3 — Onboarding

- [x] The hidden `new [game|draw|sprite]` command is now visible in the F1
      help panel and in the `help` console output ("type `new game` ...").
      No new key binding was added, so F2/F5 and the console flow stay intact.
- [x] `docs/getting-started.md` now leads with a ten-minute, in-window path
      (start → F8 `new game` → F2 edit → F5 run → F8 `save mygame`), followed
      by a copy-paste `update(dt)`/`draw()` script. CLI switches are kept as a
      reference section below.
- [x] Added `demos/first_game.tyro`: a complete tiny game (blobs, score,
      sound, keyboard) on the recommended loop, with a `test` argument for
      `--screenshot`. Both `demos/README.md` and `docs/examples.md` now start
      with it; pong is labelled as the older worker-loop style.
- [ ] Follow-up: the full "Make 5 games" course in [`ROADMAP.md`](ROADMAP.md)
      P9 and an in-app API reference with examples.
- [x] `demos/diag_sprite.tyro` no longer uses `pcall`; engine calls report
      their own failures, as AGENTS.md requires.
- [x] Corrected the stale comment in `games/waddle/waddle.tyro`: both dot and
      colon spellings work.

### P1-4 — Kid-level helpers still missing

- [x] `scene.add/go/current/list/remove/clear` (2026-11-26) gives games named
      menu/play/over states with enter/update/draw/leave callbacks. The current
      scene is stepped by the engine without needing global callbacks.
      Documented in `docs/scenes.md`; tested in `tests/roadmap02_p1_scene_test.tyro`.
- [x] `game.paused` / `game.pause()` / `game.resume()` / `game.toggle_pause()`
      freeze the scene but keep drawing; `game.restart()` and global
      `restart()` re-run from a clean window; `game.difficulty(1..10)` stores
      `game.level` and adjusts `world.timescale`. Tested in
      `tests/roadmap02_p1_pause_test.tyro` and `..._restart_test.tyro`.
- [x] Added `scoreboard.lua` (sorted top-N table) and `state.lua` (snapshots
      and undo/rewind). `ask.lua` was removed: the engine's
      `console.read(prompt, fn)` is now non-blocking, returning immediately
      and firing the callback on a later frame. Fixed the dangling waiting
      object that made the callback form crash at shutdown.
- [x] Documented in `docs/game.md` and `help/game.md`; verified with
      `tests/roadmap02_p1_helpers_test.tyro` and
      `tests/roadmap02_p3_console_read_test.tyro`.
- [x] **P1-4 complete.**

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

- [x] Subtask 1: `tests/README.md` and `tests/run_roadmap02.ps1` are committed
      via narrow `.gitignore` exceptions; ad-hoc goldens stay local. Subtask 2:
      the runner now checks 15 exit codes **plus** golden-file/stdout patterns,
      so silent early exits cannot pass.
- [x] Subtask 4: `.github/workflows/build.yml` checks out the sibling minilib
      and miniLua packages, installs Lazarus, builds `tyro.lpi` with FPC and
      runs the smoke suite when the runtime DLL URLs are set as repository
      variables. Documented in `docs/CI.md`.
- [ ] Remaining: most of the ~500 ad-hoc files stay unversioned by design;
      DLL URLs must be configured per repository before CI smoke runs.
- [x] Kid-facing paths now covered: missing assets, `controls.*` table forms,
      `game.pause_down`, negative checks for retired spellings, scene/pause/
      restart, scoreboard/state/ask helpers.
- [ ] Not yet automated: editor error jump, console `new` templates and
      `console.read()` (need GUI automation), plus a doc-link/demo-index check.
- [x] Version mismatch fixed: `TyroVersion`/`TyroVersionString` are `0.8`,
      the CLI prints the constant instead of a hard-coded `0.1`, and the Lua
      global `version` now reports 0.8 too.
- [x] Subtask 3: added `games/spacefighter/make_assets.lua`, which generates
      all 16 PNGs with the engine. Its Lua helper module was later removed:
      `images.sheet/rect/circle/line/box/poly/atlas/save` now live in TyroLua.
      Verified with the `probe` argument; `make_assets.py` is reference-only.
- [x] Batch 1 of migration: `games/birds/make_assets.lua` (7 PNGs) and
      `games/defender/make_assets.lua` (18 PNGs) replace their Pillow scripts
      with Tyro drawing. Both verified with the `probe` argument, which was
      cleaned up without touching shipped art.
- [x] Batch 2: `games/neonrunner/make_assets.lua` regenerates all six
      `.sprite` atlases (player, traffic, items, hazards, shield, boom) via a
      new `pixelart.atlas()` helper. Probe run produced six valid ZIPs with
      PNG + JSON inside and was cleaned up.
- [x] Batch 3 part 1: `games/seaquest/make_assets.lua` regenerates 8 atlases,
      4 static PNGs and 16 parallax backgrounds. Probe run produced 28 files
      (all 8 ZIPs with PNG + JSON) and was cleaned up.
- [x] Batch 3 part 2: `games/waddle/make_assets.lua` regenerates 7 atlases
      plus tiles, cursor and preview PNGs. Probe run produced 10 files and was
      cleaned up.
- [x] Batch 3 part 3: `games/bomberman/make_assets.lua` regenerates 8 atlases
      and the tileset. Probe run produced 9 files and was cleaned up.
- [x] Batch 3 part 4: `games/mazerogue/make_assets.lua` regenerates 10 atlases
      and 14 static PNGs/tiles. Probe run produced 24 files and was cleaned up.
- [x] Batch 4 part 1: `games/breakout/make_assets.lua` regenerates 2 atlases,
      2 PNGs and 3 WAV effects using `sound.generate` in the engine. Probe
      run produced 7 valid files (WAVs checked: RIFF/22050) and was cleaned up.
- [x] Batch 4 part 2: `games/submarine/make_assets.lua` regenerates 8 atlases,
      5 static PNGs and 10 WAV effects. Probe run produced 23 valid files and
      was cleaned up.
- [x] Batch 4 part 3: `games/zonix/make_assets.lua` regenerates 9 atlases
      and 10 WAV effects. Probe run produced 19 valid files (all WAVs RIFF)
      and was cleaned up.
- [x] **All 11 game asset generators now have Lua replacements.** The Python
      scripts remain in each game folder for reference only. F3 PNG/atlas
      export and an asset browser are still separate P8 work.
      export PNG/atlas and there is no asset browser
      ([`ROADMAP.md`](ROADMAP.md) P8).

## Extra items (chosen after P2)

- [x] `--export=<dir|game.zip>` wraps the existing fuse packaging, and
      `--set-title=<name>` writes `title=` into the generated `fuse.conf`
      (loaded by `TTyroEngine.LoadConfig`). Documented in `getting-started.md`
      and `help/fuse.md`. WASM is still deliberately skipped.
- [x] Callback errors now name the source: `timer #1:`, `tween #1:` and
      `on "name" #1:` (plus the Lua file/line). Verified by
      `tests/roadmap02_p2_error_labels_test.tyro`.
- [x] "Make 5 games" course: `help/course/README.md` plus five lessons that
      grow from drawing to sharing a game, linked from `docs/README.md`.
- [x] In-app help: `F9` toggles an API cheat sheet, `help api` prints the same
      short reference in the console. The F1 panel lists F9.
- [x] `canvas.font(file [, size [, codepage]])` sugar loads and switches in
      one call (registered on canvas/screen/world). The oversized-atlas guard
      already lives in `RayClasses.pas:421-425`.
- [x] RTL groundwork: UTF-8 now survives the Lua/Pascal boundary, and
      `assets/rtl.lua` provides UTF-8-safe `reverse()` plus `text(y, s [, x])`.
      Documented in `help/rtl.md`. Full bidi paragraph layout is still out.

## Not chasing (same as ROADMAP.md)

- Godot-style scene/inspector editor — would kill the one-window identity.
- Serious 3D (PBR, skinned animation, navmesh) — keep toy-3D as a bonus.
- C# / multiple language backends — Lua + MML is the right beginner stack.

## Note on a stale roadmap item

[`ROADMAP.md`](ROADMAP.md) P7 says `L.ToString` folds UTF-8 through the system
ANSI codepage. Current `TLuaHelper.ToString` / `PushString`
(`..\lua\LuaClasses.pas:658-662,741-744`) explicitly preserve UTF-8. Re-verify
before scheduling that work.
