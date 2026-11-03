# NEON BREAKER - Classic Brick-Busting Arcade Action

**Neon Breaker** is a glowing take on the 1976 arcade classic *Breakout*, built for
the **Tyro Game Engine**. Bounce a plasma ball off your energy paddle and shatter
every neon brick hovering above the star field.

Where the ball hits the paddle decides the bounce angle - the edges throw it wide,
the middle sends it straight up. Top rows are worth more points. Clear one of the
four arcade walls and the next layout loads, a little faster!

Brick debris bursts, a glowing ball trail, floating combo call-outs and a
two-voice chiptune loop keep the arcade feel alive.

---

## Gameplay & Rules

1. **Four walls**: Classic Wall, Gold Pyramid, Neon Checkers and Steel Fortress,
   on a 14 x 6 grid. Points by row: 70 / 60 / 50 / 40 / 30 / 20 top to bottom.
   - **Steel bricks** (`H` in the layouts) take two hits - they crack after the
     first - and pay 50 points when they finally fall.
2. **The Paddle**: mouse, keyboard or gamepad. The bounce angle follows the hit
   position (up to 60 degrees off vertical), so aim with the edges!
3. **Power-ups**: broken bricks sometimes drop a capsule - catch it on the paddle!
   | Capsule | Effect |
   |---------|--------|
   | WIDE (aqua) | Paddle grows to 144 px for 15 s |
   | STICKY (yellow) | The ball sticks to the paddle; launch it when you like (12 s) |
   | MULTI (green) | Two extra balls join the fun (up to 3 at once) |
   | LASER (orange) | The paddle auto-fires bolts for 12 s |
   Active effects and their seconds show under the HUD. Losing a ball clears
   all effects.
4. **Combos**: bricks broken in one rally (without touching the paddle) pitch the
   break sound higher; every 4th streak shows a combo call-out and a screen jolt.
5. **Lives**: you start with 3 balls. With several balls in play you only lose a
   life when the last one drops; at zero lives it is game over.
6. **Board clear**: the ball speeds up ~7% per cleared board (up to 560 px/s)
   and the next level layout is built.
7. **Hi-Score**: the best score is kept in `breakout.hi` next to the script,
   and a NEW BEST! call-out appears the moment you pass it.

## Controls

| Key / Input | Action |
|-------------|--------|
| Mouse / LEFT-RIGHT (A-D) / Left-Stick / D-Pad | Move the paddle |
| SPACE, UP, W, left-click or pad A | Launch the ball / start / restart |
| P | Pause / resume |
| M | Mute / unmute |
| ESC | Exit |

## Files

| File | Description |
|------|-------------|
| `breakout.tyro` | The game: attract mode, physics loop, power-ups, HUD and state screens |
| `breakout_data.tyro` | Pure config and math (grid, levels, collisions, bounce angles, hi-score) - safe to `require()` from tests |
| `make_assets.py` | Regenerates `img/` and `sfx/` (needs Pillow) and copies the data module to `tests/` |
| `img/bricks.png` | 8-tile brick sheet (50x22 tiles with the grid gap baked in): 6 colors, steel, cracked steel - drawn as a tilemap |
| `img/powerups.sprite` | 4-frame capsule badge atlas (22x22), one sprite per possible drop |
| `img/paddle.png`, `img/ball.png` | Paddle and ball sprites |
| `sfx/*.wav` | Synthesized blip, brick-crack and shatter sounds |

## Run

```
tyro games/breakout/breakout.tyro
```

## Test

```
tyro tests/test_breakout_data.tyro --exit:5 --console
```
