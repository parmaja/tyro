# game - The Helpers Every Game Re-writes

Every game re-types the same few lines: keep a number inside a range, load
last week's best score, answer P with the keyboard *and* the gamepad, and go
quiet when the player says MUTE. These live in the engine now, in one small
table called `game` - no files to require, nothing to copy between projects.

```lua
game.clamp(v, lo, hi)          -- keep v inside lo..hi
game.lerp(a, b, t)             -- the point t (0..1) of the way from a to b
game.sign(v)                   -- -1, 0 or 1
game.dist(x1, y1, x2, y2)      -- straight-line distance between two points
game.aabb(x1, y1, w1, h1,      -- do two boxes overlap? (x, y is each
        x2, y2, w2, h2)        --   box's top-left corner)
```

```lua
-- best scores: one small text file per game, kept in the game's own folder
local hi = game.hi_load("cave.hi", 0)
hi = game.hi_save("cave.hi", score)   -- writes only when score beats hi,
                                      -- and answers the best either way
```

```lua
-- repeatable random, like the engine's rand() but private to you
local r = game.rng(42)      -- two runs with the same seed make the same map
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
| `game.fire_down()` | hold version of `fire_pressed` | |

The `*_pressed` calls are true once, on the frame the button is pressed - use
them for menus and shots. `game.fire_down()` is true every frame the button
stays held - good for auto-fire.

```lua
function update(dt)
  if game.pause_pressed() then paused = not paused end
  if game.fire_pressed() then shoot() end
end
```

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
binds them to the `game` table, so games, demos and the editor all see the
same behaviour. The mute flag is the `SoundMuted` global in TyroSounds.pas,
checked at every playback entry point.
