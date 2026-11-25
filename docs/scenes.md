# Scenes, Pause and Restart

[<< Tyro docs index](README.md)

Every game has a title screen, a play state and a game-over card. Instead of
writing that state machine again, give each state a name and let Tyro step the
one you are in.

```lua
scene.add("menu", {
  enter = function()   print("menu!") end,
  update = function(dt)
    if game.start_pressed() then scene.go("play") end
  end,
  draw = function()
    screen.text_center(120, "PRESS SPACE")
  end,
})

scene.add("play", {
  enter = function() score = 0 end,
  update = function(dt) -- move the player, check collisions ...
  end,
  draw = function() -- draw the world ...
  end,
  leave = function() music.stop() end,
})

scene.go("menu")            -- runs menu.enter()
scene.current()             -- -> "menu"
scene.list()                -- -> { "menu", "play" }
scene.remove("menu")
scene.clear()
```

`scene.go(name [, ...])` calls `leave()` on the old scene (if any), switches
the current scene and calls `enter()` with any extra arguments. The current
scene's `update(dt)` and `draw()` are called **after** the global `update(dt)`
and `draw()` on each frame, so globals still work exactly as before. A script
that only uses scenes still drives the frame loop — you do not need to define
global callbacks just to keep the engine stepping.

Callbacks may be omitted. Errors inside a scene callback are reported the same
way as errors in `update()`/`draw()` (visible banner + Output panel).

## Pause, resume and restart

```lua
game.paused = true          -- freeze the scene; draw() still runs
game.toggle_pause()         -- flip the flag, answers the new value
game.resume()               -- same as game.paused = false
if game.paused then ... end -- read it any time

game.restart()              -- or the shorter global restart()
game.difficulty(2)          -- level 1..10, answers the level
game.level                  -- current level (property)
```

`game.paused` freezes `update()` and everything riding it — timers, tweens,
physics and sprite animation — but `draw()` keeps running, so you can paint
your own "PAUSED" overlay. `restart()` is handled at a safe point between
frames; the next run starts from a clean window with level 1 and no pause.

`game.difficulty(n)` stores the level (1..10) and sets `world.timescale` to
`1 + (level - 1) * 0.08`, so higher levels run a little faster. Read
`game.level` to show it in your HUD.

## Where it goes next

- [Timing & the game loop](timing.md) — `update(dt)` / `draw()`, timers, tweens.
- [The game helper](game.md) — button pairs, high scores, random helpers.
- [`demos/first_game.tyro`](../demos/first_game.tyro) — a complete small game
  you can copy and turn into several scenes.
