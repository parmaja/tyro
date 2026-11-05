# Jet 2026 (Tyro Game Engine)

A 1942-style vertical scrolling jet fighter for kids, built for Tyro using
RayLib, a real 3D model for every jet, world-painted waves and Lua.

## Overview

Fly your jet up the screen over the open sea and blast the enemy squadron
streaming in from the top. The world scrolls while you climb, you weave
through formations, and a big bomber shows up every sixth wave. A homage to
the 1984 Capcom classic — but the stars of the show are real 3D models
(`jet.glb` for the player, `enemy.glb` for the squadron) rendered by Tyro's
new 3D model pass.

This is the showcase game for the `models` API: the player flies `jet.glb`
and the whole squadron flies `enemy.glb` in its own paint, flown nose-down,
with the bomber the same enemy mesh at big scale. The
clouds are clusters of engine-built 3D spheres (`models.sphere`) at a depth
between the jets and the sea, so every jet and cloud drops its true silhouette
on the water BELOW the clouds. Waves, bullets, islands, explosions and the HUD
are painted straight onto the frame buffer with `world` primitives.

## Key Features

- **3D jets on a 2D game**: `jet.glb` for the player and `enemy.glb` for the
  squadron (in its own paint), driven through the `models` API (`models.new` /
  `model:load/move/show/hide`) — the enemy mesh is authored canopy-up, so it
  carries a 180 roll on top of the 180 nose-down angle to keep its top facing
  the eye. The engine draws them in a top-down orthographic 3D pass that is
  pixel-aligned with the 2D canvas.
- **Model pooling**: 18 pre-allocated enemy model objects are acquired and
  released from a Lua-side pool — no model churn during play.
- **Squadrons**: V formations of grunts (sine drift), divers that swoop at
  you, gunners that hold the top line and shoot, and a heavy bomber with a
  health bar's worth of hits every 6th wave.
- **Power-ups**: grab the `P` crate to go from single to double to triple
  shot; maxed-out crates score instead.
- **Juice**: screen shake, gamepad vibration, engine hum loop
  (`sound.loopPlay`), roll-into-the-turn jet banking (the model's `roll`
  property), cast shadows — every jet and cloud projects its real silhouette
  onto the sea along the sun ray (`model.shadow` + `world.light/ambient/
  ground`) — hi-score persistence (`jet2026.hi`).
- **3D clouds**: each cloud is a cluster of `models.sphere` puffs parked at
  `CLOUD_Z`, a depth nearer the eye than the shadow plane, so shadows always
  land on the sea behind them and a cloud never wears one on top.

## Controls

| Action | Keyboard | Gamepad |
|---|---|---|
| **Fly** | `Arrows` / `WASD` | Left stick / D-pad |
| **Fire** (hold for auto) | `SPACE` | `A` / `X` / `RB` / `RT` |
| **Throttle up / down** | `SHIFT` + `UP` / `DOWN` | — |
| **Play / Pause / Resume** | `SPACE` / `P` | `START` |
| **Quit (menu/pause)** | `ESC` | `BACK` |

## Architecture

- `jet2026.tyro`: The whole game — state machine (ready/play/pause/dying/
  over), wave scheduler, enemy AI, bullets, power-ups, world renderer, and
  a `JET2026_TEST` hook that exports its internals when the file is pulled
  in via `require()` from a test harness.
- `jet.glb`: The player's 3D mesh (binary glTF).
- `enemy.glb`: The squadron's 3D mesh (binary glTF) — fighters and bomber
  alike, flown in its own paint and scaled up for the bomber.
- `tests/test_jet2026.tyro`: Headless logic test (pool acquire/release,
  scoring, hit/respawn/game-over flow) — see Running.
- `tests/test_jet2026_play.tyro`: Screenshot driver that replays real
  frames through the game's own `simulate`/`draw` for visual QA.

## Running

```bash
tyro games/jet2026/jet2026.tyro
```

Tests (the sandbox keeps `require` inside the script folder, so stage a
copy of the game and its media next to the harness first):

```bash
Copy-Item games\jet2026\jet2026.tyro tests\jet2026_test.tyro
Copy-Item games\jet2026\jet.glb,games\jet2026\enemy.glb tests\
Copy-Item games\jet2026\sea.frag tests\
bin\tyro.exe tests\test_jet2026.tyro --console --exit:4
bin\tyro.exe tests\test_jet2026_play.tyro --console --screenshot:8 --exit:9
```
