# game - The Helpers Every Game Re-writes

Every game re-types the same few lines: keep a number inside a range, load
last week's best score, answer P with the keyboard *and* the gamepad, and go
quiet when the player says MUTE. These live in the engine now - no files to
require, nothing to copy between projects. The math helpers are plain globals,
the best scores live in a `score` table and the button pairs in `game`.

```lua
clamp(v, lo, hi)               -- keep v inside lo..hi
lerp(a, b, t)                  -- the point t (0..1) of the way from a to b
sign(v)                        -- -1, 0 or 1
dist(x1, y1, x2, y2)           -- straight-line distance between two points
aabb(x1, y1, w1, h1,           -- do two boxes overlap? (x, y is each
     x2, y2, w2, h2)           --   box's top-left corner)
```

```lua
-- best scores: one small text file per game, kept in the game's own folder
local hi = score.hi_load("cave.hi", 0)
hi = score.hi_save("cave.hi", score)   -- writes only when score beats hi,
                                       -- and answers the best either way
```

```lua
-- repeatable random, like the engine's rand() but private to you
local r = rng(42)           -- two runs with the same seed make the same map
r()      -> 0 .. <1         -- a raw fraction
r(6)     -> 1 .. 6          -- one die
r(2, 5)  -> 2 .. 5          -- inside a range
```

## Button pairs (keyboard OR gamepad)

The usual arcade mappings in one call each; a game with its own scheme keeps
its own check, these are only the shared defaults.

| call | keyboard | gamepad (any pad) |
|---|---|---|
| `game.pause_pressed()` | p | start, menu |
| `game.start_pressed()` | space, enter | start, menu, a, x, fire |
| `game.fire_pressed()` | space, x | a, x, b, y, rb, rt, fire, trigger |
| `game.a_pressed()` | space | a |
| `game.b_pressed()` | x | b |
| `game.x_pressed()` | c | x |
| `game.y_pressed()` | v | y |
| `game.l_pressed()` | z | lb, lt |
| `game.r_pressed()` | b | rb, rt |
| `game.select_pressed()` | tab | select, back |
| `*_down()` | hold version of the `*_pressed` call | |

The `*_pressed` calls are true once, on the frame the button is pressed - use
them for menus and shots. The `*_down` calls (`pause_down`, `fire_down`, `a_down`,
`b_down`, `x_down`, `y_down`, `l_down`, `r_down`, `start_down`, `select_down`) are true
every frame the button stays held - good for auto-fire and throttles.

```lua
function update(dt)
  if game.pause_pressed() then paused = not paused end
  if game.fire_pressed() then shoot() end
  if game.l_pressed() then drop_bomb() end
end
```

## Ready-made helpers

The built-in `scores` table gives a sorted high-score table (`top`, `add`,
`best`); `require("assets/state")` still gives table snapshots and undo
(`remember`, `undo`, `snapshot`, `restore`). For input, `console.read(prompt,
fn)` is built into the engine and **does not freeze the game**: it returns
immediately and calls `fn(answer)` on a later frame. Details in
[`docs/game.md`](../docs/game.md).

## Scenes, pause and restart

`game.paused` (read/write) freezes `update()`, timers, tweens and physics while
`draw()` keeps running, so a game can paint its own PAUSED overlay.
`game.pause()`, `game.resume()` and `game.toggle_pause()` are the verb forms.
`game.restart()` — or the shorter global `restart()` — re-runs the script from
a clean window. `game.difficulty(1..10)` stores `game.level` and nudges
`world.timescale`; see [`docs/scenes.md`](../docs/scenes.md) for scene switching.

## sound.muted - the one silence switch

`sound.muted = true` (a MUTE key, an options menu) and every following play
request goes quiet: `music.mml`, `music.play`, `music.sound`, `music.beep`,
`sound.play`, `sound.noise` and `sound.loopPlay` all drop while it is on -
music uses the switch exactly like sound does. `music.muted` reads and writes
the same one flag, so either name works from either menu.

The switch gates the *start* of playback: a sound that is already playing
finishes its way out, and un-muting lets the next requests through.

```lua
function update(dt)
  if keyboard.pressed("m") then
    sound.muted = not sound.muted
  end
end
```

## Where it lives

The functions are methods of `TTyroEngine` (src/TyroEngines.pas); TyroLua only
binds them - the math helpers and `rng` as globals, the best scores under
`score`, the button pairs under the `game` table - so games, demos and the
editor all see the same behaviour. The mute flag is the `SoundMuted` global in
TyroSounds.pas, checked at every playback entry point.
