# Game Helper (`game.*`)

[<< Tyro docs index](README.md)

The chores every game re-writes live in the engine: the arcade button pairs in
the `game` table, the best scores in `score`, and the math helpers as plain
globals. Full notes in [`help/game.md`](../help/game.md).

```lua
clamp(v, lo, hi)   lerp(a, b, t)   sign(v)
dist(x1, y1, x2, y2)
aabb(x1, y1, w1, h1, x2, y2, w2, h2)   -- do two boxes overlap?

input.pause_pressed()  input.start_pressed()  input.fire_pressed()
input.a_pressed()  input.b_pressed()  input.x_pressed()  input.y_pressed()
input.l_pressed()  input.r_pressed()  input.select_pressed()
input.fire_down() ...  -- hold versions: *_down for every pair above
input.up_pressed() / down_pressed() / left_pressed() / right_pressed()
                      -- one edge from keyboard, D-pad or the joystick
-- The same button spellings still exist under game.* as aliases.

local hi = score.hi_load("cave.hi", 0)         -- best score from a small
hi = score.hi_save("cave.hi", score)           -- text file next to the game

local r = rng(42)   -- repeatable random: r() 0..1, r(n) 1..n, r(a,b) a..b
```

## Helpers

The high-score table is built into the engine as `scores`:

```lua
scores.add("hiscores.txt", "Ana", 4200, 5)
for _, e in ipairs(scores.top("hiscores.txt", 5)) do
  print(e.rank .. ". " .. e.name .. " - " .. e.score)
end
local best = scores.best("hiscores.txt")
```

`scores.top(file [, limit])` returns an array of `{ rank, name, score }`
sorted highest first. `scores.add(file, name, points [, limit])` inserts a
score, keeps the top *limit* (10 by default) and writes the tab-separated file
next to your script. `scores.best(file)` answers the highest score.

Table snapshots and undo are also built in, as the `state` table:

- `state.remember(t)` pushes a snapshot of a plain table's numbers, strings
  and booleans; `state.undo(t)` restores the last one. `state.snapshot(t)` /
  `state.restore(t, copy)` hand you a copy you can keep or tween.
  `state.count(t)` and `state.clear(t)` manage the stack.

No helper module is required for any of this — everything is in the engine.

For a question in the game, use the engine's non-blocking console read:

```lua
console.read("What is your name?", function(answer)
  print("Hello " .. answer)
end)
-- The script continues immediately; the callback fires on a later frame.
```

None of these use `pcall`; asset and file failures are reported by the engine
the usual way. See [`docs/scenes.md`](scenes.md) for scene and pause helpers.

`sound.muted = true` is the one silence switch: while it is on, every
following play request from `music.*` and `sound.*` goes quiet
(`music.muted` is the same flag).

