# Jet 2026 (Tyro Game Engine)

A 1942-style vertical scrolling jet fighter for kids, built for Tyro using
RayLib, a real 3D model for every jet, canvas-drawn waves and Lua.

## Overview

Fly your jet up the screen over the open sea and blast the enemy squadron
streaming in from the top. The world scrolls while you climb, you weave
through formations, and a big bomber shows up every sixth wave. A homage to
the 1984 Capcom classic — but the stars of the show are real 3D models
(`jet.glb`) rendered by Tyro's new 3D model pass.

This is the showcase game for the `models` API: the player and every enemy
are the SAME mesh — enemies are simply tinted red/orange/maroon and flown
nose-down, the bomber is the same mesh at big scale. Waves, bullets, clouds,
islands, explosions and the HUD are canvas primitives.

## Key Features

- **3D jets on a 2D game**: one `jet.glb` reused for player and enemies via
  the `models` API (`models.new` / `model:load/move/show/hide`), tinted and
  rotated per role. The engine draws them in a top-down orthographic 3D pass
  that is pixel-aligned with the 2D canvas.
- **Model pooling**: 18 pre-allocated enemy model objects are acquired and
  released from a Lua-side pool — no model churn during play.
- **Squadrons**: V formations of grunts (sine drift), divers that swoop at
  you, gunners that hold the top line and shoot, and a heavy bomber with a
  health bar's worth of hits every 6th wave.
- **Power-ups**: grab the `P` crate to go from single to double to triple
  shot; maxed-out crates score instead.
- **Juice**: screen shake, gamepad vibration, engine hum loop
  (`sound.loopPlay`), bank-into-the-turn and pitch-on-climb jet animation,
  hi-score persistence (`jet2026.hi`).

## Controls

| Action | Keyboard | Gamepad |
|---|---|---|
| **Fly** | `Arrows` / `WASD` | Left stick / D-pad |
| **Fire** (hold for auto) | `SPACE` | `A` / `X` / `RB` / `RT` |
| **Play / Pause / Resume** | `SPACE` / `P` | `START` |
| **Quit (menu/pause)** | `ESC` | `BACK` |

## Architecture

- `jet2026.tyro`: The whole game — state machine (ready/play/pause/dying/
  over), wave scheduler, enemy AI, bullets, power-ups, canvas renderer, and
  a `JET2026_TEST` hook that exports its internals when the file is pulled
  in via `require()` from a test harness.
- `jet.glb`: The single 3D mesh (binary glTF) shared by all jets.
- `tests/test_jet2026.tyro`: Headless logic test (pool acquire/release,
  scoring, hit/respawn/game-over flow) — see Running.
- `tests/test_jet2026_play.tyro`: Screenshot driver that replays real
  frames through the game's own `simulate`/`draw` for visual QA.

## Running

```bash
tyro games/jet2026/jet2026.tyro
```

Tests (the sandbox keeps `require` inside the script folder, so stage a
copy of the game next to the harness first):

```bash
Copy-Item games\jet2026\jet2026.tyro tests\jet2026_test.tyro
bin\tyro.exe tests\test_jet2026.tyro --console --exit:4
bin\tyro.exe tests\test_jet2026_play.tyro --console --screenshot:8 --exit:9
```
