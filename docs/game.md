# Game Helper (`game.*`)

[<< Tyro docs index](README.md)

The chores every game re-writes live in the engine: the arcade button pairs in
the `game` table, the best scores in `score`, and the math helpers as plain
globals. Full notes in [`help/game.md`](../help/game.md).

```lua
clamp(v, lo, hi)   lerp(a, b, t)   sign(v)
dist(x1, y1, x2, y2)
aabb(x1, y1, w1, h1, x2, y2, w2, h2)   -- do two boxes overlap?

game.pause_pressed()  game.start_pressed()  game.fire_pressed()
game.a_pressed()  game.b_pressed()  game.x_pressed()  game.y_pressed()
game.l_pressed()  game.r_pressed()  game.select_pressed()
game.fire_down() ...  -- hold versions: *_down for every pair above;
                      -- keyboard OR gamepad, the usual arcade buttons

local hi = score.hi_load("cave.hi", 0)         -- best score from a small
hi = score.hi_save("cave.hi", score)           -- text file next to the game

local r = rng(42)   -- repeatable random: r() 0..1, r(n) 1..n, r(a,b) a..b
```

## Ready-made helpers (`require`)

Three small Lua modules ship with Tyro and need no setup. They live in the
app's `assets` folder and are loaded with the workspace-safe `require`:

```lua
local board = require("assets/scoreboard")
local state = require("assets/state")
local ask   = require("assets/ask")
```

- `scoreboard.top(file [, limit])` → an array of `{ rank, name, score }`
  sorted highest first. `scoreboard.add(file, name, points [, limit])` inserts
  a score, keeps the top *limit* (10 by default) and writes the file next to
  your script. `scoreboard.best(file)` answers the highest score.
- `state.remember(t)` pushes a snapshot of a plain table's numbers, strings
  and booleans; `state.undo(t)` restores the last one. `state.snapshot(t)` /
  `state.restore(t, copy)` hand you a copy you can keep or tween.
  `state.count(t)` and `state.clear(t)` manage the stack.
- `ask(prompt, fn)` (or `ask.show(...)`) opens an edit box and an OK button
  while the game keeps running. It fires `fn(answer)` when OK is clicked —
  no `console.read()` freeze and no polling call in your `update()`.
  `ask.hide()` closes it and `ask.active()` reports its state.

None of these use `pcall`; asset and file failures are reported by the engine
the usual way. See [`docs/scenes.md`](scenes.md) for scene and pause helpers.

`sound.muted = true` is the one silence switch: while it is on, every
following play request from `music.*` and `sound.*` goes quiet
(`music.muted` is the same flag).

