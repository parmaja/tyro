# Seaquest

An homage to the 1976 Atari coin-op, built for the Tyro Game Engine.

Pilot the yellow sub through eight levels of reef, trench and abyss: rescue the
stranded divers, keep the tank full of air, and blast the sharks, squid, eels
and red Patrol Subs that come for you. Surface with a full boat of six and they
are handed over for the big score. The Kraken is waiting at the bottom.

Everything in the water is a sprite (animated `.sprite` atlases), the seabed
and water column are a two-layer parallax baked per level, and `seaquest.frag`
washes the whole scene in caustics, light shafts and depth grading.

## How to Run

```
bin\tyro.exe games\seaquest\seaquest.tyro
```

Regenerate the art (optional — needs Python 3 with Pillow):

```
python games\seaquest\make_seaquest_assets.py
```

The generated `sq_*` files are checked in beside the script, so the game runs
as-is without Python.

## Controls

| Key / Input | Action |
|-------------|--------|
| **ARROWS** / **WASD** | Swim |
| **SPACE** / **Z** | Fire a torpedo (one at a time, like the arcade) |
| **X** | Drop a rescued diver back into the water |
| **SPACE** | Start / restart (title, level and game-over screens) |
| **ESC** | Exit to desktop |

## How to Play

### Rescue and delivery

- The sub carries **six** divers — the boat holds six and only six pay.
- Press **X** to release a rescued diver (to free a slot for another one).
- **Surface** (swim up into the sky band) to hand the boat over:
  - a full boat of **six** pays `6 x diver value` and the divers go back into
    the water to be rescued all over again;
  - surfacing with **1-5** divers hands over one and loses it (our variant —
    see `seaquest_data.tyro`).
- Surfacing also **locks the sub until the tank is full**: bubbles trail off
    the tank while it fills, then the engine lets go.

### Oxygen

- A full tank lasts **26 seconds** at speed; each later delivery burns a touch
  faster.
- At zero you get the arcade's **8 second alarm window** — siren, red pulse,
  then the sub is lost.
- When the sub is lost the divers scatter, and a partial load costs one of them.

### Scoring

- **Diver**: 50 on the first delivery, **+50** per delivery, capped at 1000.
- **Enemy**: 20 on the first delivery, **+10** per delivery, capped at 90
  (squid and Patrol Sub pay x2, eel x3).
- **Extra life every 10,000** points; **4 lives** to start.
- The red **Patrol Sub** joins the hunt after the second delivery.

### Levels

Each level needs `quota` full-boat deliveries to move on:

| # | Name | Deliveries | Divers in the water | Boss |
|---|------|-----------:|--------------------:|------|
| 1 | Sunlit Reef | 3 | 3 | — |
| 2 | Kelp Forest | 4 | 4 | — |
| 3 | Rocky Canyon | 4 | 4 | — |
| 4 | Shipwreck Graveyard | 5 | 5 | — |
| 5 | Volcanic Vent | 5 | 5 | — |
| 6 | Midnight Zone | 6 | 6 | — |
| 7 | The Abyss | 6 | 6 | — |
| 8 | Kraken's Lair | 4 | 6 | Kraken |

Sharks are there from the first level, squid and eels arrive as you descend,
and the roster and speed keep escalating. Past level 8 the levels wrap around
while the speed keeps climbing, so a long game stays hard.

### The Kraken

Level 8 is a boss level: **14 torpedoes** to kill it, and you must clear the
delivery quota *and* sink it to move on.

## Engine Features Used

- **Sprites** — every creature is an animated `.sprite` atlas (a ZIP holding a
  PNG strip plus a TexturePacker JSON with per-frame `duration`), loaded into
  reusable pools and driven with `sprite:play(fps)` / `flipH`.
- **`sprite.tint`, `sprite.alpha`, `sprite.layer`** — new sprite properties
  added to the engine for this game: the hit flash tints a sprite white, the
  bubbles and washed-out decor fade with alpha, and layers keep explosions and
  the HUD art above the actors regardless of creation order.
- **`canvas.front`** — the board canvas becomes a translucent overlay *above*
  the sprites, so the HUD and the water-light washes light the scene without
  ever hiding it.
- **`shader.load` / `shader.value` / `shader.area`** — `seaquest.frag` does
  refraction, caustics, light shafts, depth grading and a vignette, and
  `shader.area` gates all of it to the water band so the HUD strip stays clean.
- **Parallax** — `parallax.new` / `add` / `addground` with a slow background
  layer and a full-speed seabed, one baked PNG pair per level.
- **Sound** — the engine's procedurally synthesized waveforms via
  `music.sound(...)` (Pew, Explode, Siren, Coin, Cash, Bubble) and a looped
  `sound.loopPlay("Engine", ...)` whose pitch and volume follow the throttle.
- **Music** — `music.mml(...)` jingles for a delivery, a level clear and game
  over, played by the engine's note synth.
- **`require("seaquest_data")`** — the rules live in a pure Lua module with no
  engine calls at all, so `tests/test_seaquest_data.tyro` can check the numbers
  without booting the engine.

## Files

| File | Purpose |
|------|---------|
| `seaquest.tyro` | The game |
| `seaquest_data.tyro` | Pure rules and tuning module (single source of truth) |
| `seaquest.frag` | Water post-processing fragment shader (GLSL) |
| `make_seaquest_assets.py` | Procedural asset generator (Pillow) |
| `sq_*.sprite` | Animated sprite atlases (sub, divers, enemies, boom) |
| `sq_bg*.png`, `sq_flr*.png` | Per-level parallax background and seabed |
| `sq_torpedo*.png`, `sq_bubble.png`, `sq_pearl.png` | Static images |
| `../../tests/test_seaquest_data.tyro` | 47 checks against the rules module |
| `../../tests/test_sprite_tint_alpha_layer.tyro` | 19 checks for the new sprite properties |

## Credits & Sources

- **All artwork** (`sq_*.sprite`, `sq_bg*.png`, `sq_flr*.png`,
  `sq_torpedo*.png`, `sq_bubble.png`, `sq_pearl.png`): drawn procedurally at
  native pixel resolution with Python and Pillow by
  `games/seaquest/make_seaquest_assets.py`. **No third-party art is used and
  nothing was downloaded** — the art, palettes and animation frames all come
  from that one script, so the game is fully self-contained and reproducible.
  Re-running it regenerates identical files (it seeds its random generator).

- **Shader (`seaquest.frag`)**: custom GLSL post-processing written for Tyro —
  refraction, animated caustics, light shafts, depth grading and vignette.

- **Sound effects**: no audio files. Every effect is one of the Tyro engine's
  built-in waveforms, synthesized at play time in `src/tyrolib/TyroSounds.pas`.

- **Music**: no audio files. Jingles are `music.mml(...)` note strings played
  by the engine's synthesizer.

- **Font**: Terminus (`bin/terminus-18n.bdf`), the Tyro engine's built-in
  screen font — part of the engine, not this game.

- **Game design**: an original homage to *Seaquest* (Atari, 1976). No Atari
  artwork, sound, code or level data is used; only the well-documented rules
  (six-diver deliveries, the growing score tables, the oxygen alarm) are
  followed, and anything that does not translate is documented as a variant in
  `seaquest_data.tyro`.
