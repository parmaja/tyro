# Events - Signals Between Friends 📡

Games are conversations: the physics world tells you two sprites met, the
mouse tells you it was clicked, your own code tells the HUD the score moved.
Instead of threading those calls by hand, Tyro has a tiny **event bus**:
anyone can `on(name, fn)` to listen, anyone can `emit(name, ...)` to speak.
Nobody needs to know who is listening - and `emit` answers `0` when nobody is.

```lua
on("boss_hit", function(dmg)
  score = score + dmg
end)

emit("boss_hit", 25)     -- every listener runs, right now, in hook order
```

The bus shares the engine's beat with `after()` and `tween()`: listeners run
inside the frame loop, may move sprites, draw, queue sounds and call
`exit()` - and an error in a listener is reported like any script error
(with the event's name) and marks the run failed, so fix it and press F5.

---

## The Commands

| API | What it does |
| --- | --- |
| `on(name, fn)` | hook `fn` to `name`; returns a handler id. Names ignore case |
| `off(id)` | unhook one handler (`true`/`false`); never runs again, even mid-emit |
| `emit(name, ...)` | run every listener of `name` with the extra values; answers how many ran |

An `emit` in progress is a snapshot: a listener armed by another listener
starts answering from the **next** emit, and one unhooked by an earlier
listener is skipped even mid-dispatch. A broken listener reports its error
and the next one runs anyway.

## Names the Engine Emits Itself

The bus's trick is that some names the **engine** fires - these bindings are
fire-and-forget, no `update()` needed:

| Name | When your fn runs | Your fn gets |
| --- | --- | --- |
| a key - `on("space", fn)`, `on("w", fn)`... | the frame that key was pressed (every name `keyboard.pressed` knows) | nothing |
| `on("mouse.left", fn)` (`"mouse.right"`, `"mouse.middle"`) | the frame that button was pressed | nothing |
| `on("update", fn)` | every frame, before the global `update()` | the `dt` |
| `on("draw", fn)` | every paint, after the global `draw()` | nothing |
| `on("collision", fn)` | every physics contact event | `(a, b, "enter"/"leave")` |

```lua
on("space", function()
  jump()                      -- the fire-and-forget input bind
end)

on("collision", function(a, b, state)
  if state == "enter" and (a == player or b == player) then
    hurt()
  end
end)
```

Bindings ride **alongside** the classic style, not instead of it: a press
still answers `keyboard.pressed("space")` in `update()`, the per-sprite
`onCollide` fields still fire, and a manual `collision.pump()` in a script
still works - it just drains first, so when the bus is also listening the
events arrive one beat earlier through it. Use `on` where the old style needs
three lines of poll-and-branch; use the old style when it fits.

Two small cautions: hook the keys the editor keeps as shortcuts
(**F1..F8**) only on purpose - they reach both - and a listener that
fails marks the run failed by design: the bus never swallows an error
quietly. `on()`/`off()`/`emit()` keep their ids in the same family as the
timers and tweens, but the number spaces are independent - a tween id never
means a handler, and vice versa.

A script with **only** listeners gets the engine's frame loop, like the
timer-only and tween-only ones: a splash screen can be one `on("space", ...)`
and nothing else.

## The Whole Story in Twelve Lines

```lua
window.show(640, 480)
local hud = { lift = 0 }
local score = 0

on("scored", function(pts)          -- 1. someone listens
  score = score + pts
  tween.from(hud, "lift", 12, 0, 220, "quadout")
end)

on("mouse.left", function()         -- 2. the engine speaks on clicks
  emit("scored", 10)                -- 3. ...and your own code speaks too
end)

function draw()
  screen.clear("blank")
  screen.text(8, 452 - hud.lift, "SCORE " .. score)
end
```

For the bus meeting physics, tween juice and `off()` in one toy, open
[`demos/events_demo.tyro`](../demos/events_demo.tyro); for a full game view,
`games/bubblepop/` pairs it with the tween API.
