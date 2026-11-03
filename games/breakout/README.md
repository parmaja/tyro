# NEON BREAKER - Classic Brick-Busting Arcade Action

**Neon Breaker** is a glowing take on the 1976 arcade classic *Breakout*, built for
the **Tyro Game Engine**. Bounce a plasma ball off your energy paddle and shatter
every neon brick hovering above the star field.

Where the ball hits the paddle decides the bounce angle - the edges throw it wide,
the middle sends it straight up. Top rows are worth more points. Clear the whole
board and it rebuilds itself a little faster!

---

## Gameplay & Rules

1. **The Wall**: a 14 x 6 grid of neon bricks, six classic colors deep.
   - Points by row: 70 / 60 / 50 / 40 / 30 / 20 from top to bottom.
2. **The Paddle**: mouse, keyboard or gamepad. The bounce angle follows the hit
   position (up to 60 degrees off vertical), so aim with the edges!
3. **Lives**: you start with 3 balls. Drop one below the paddle and the count
   goes down; at zero it is game over.
4. **Board clear**: the ball speeds up ~7% per cleared board (up to 560 px/s)
   and the wall is rebuilt.
5. **Hi-Score**: the best score is kept in `breakout.hi` next to the script.

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
| `breakout.tyro` | The game: attract mode, physics loop, HUD and state screens |
| `breakout_data.tyro` | Pure config and math (grid, collisions, bounce angles, hi-score) - safe to `require()` from tests |
| `make_assets.py` | Regenerates `img/` and `sfx/` (needs Pillow) and copies the data module to `tests/` |
| `img/bricks.png` | 6-tile brick sheet (50x22 tiles with the grid gap baked in) drawn as a tilemap |
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
