# Timing

[<< Tyro docs index](README.md)

For named menu/play/gameover states and the `game.paused` toggle, see
[Scenes](scenes.md).

| Function | Returns | Description |
|----------|---------|-------------|
| `sleep(ms)` | — | Pause the script thread for *ms* milliseconds |
| `pause(ms)` | — | Like `sleep(ms)`, but also **freeze the scene** while the script waits: physics, sprite animation/`on_update` and controls stop advancing, so the frame the script queued stays on screen. Audio (music/radio) and input keep running. Use it for a still hold; use `sleep()` when the scene should keep moving |
| `enter()` | — | **Hold** the processing queue: the drawing commands queued after this call are not executed until `leave()` balances it |
| `leave()` | — | Release the hold taken by `enter()`: the whole group queued in between is then drawn at one drawing frame |
| `frametime()` | `number` (seconds) | Time elapsed since the last frame |
| `time()` | `number` (seconds) | Elapsed time since the window was created |
| `millis()` | `number` (ms) | High-resolution monotonic clock in milliseconds (the same timer the frame budget uses); fine enough to time a drawing body |
| `rand(min, max)` | `integer` | Random integer in [min, max] |
| `after(ms, fn)` | `number` (id) | One shot: run `fn` once, `ms` of scene time later (see [Scene timers](#scene-timers-afterfn--everyfn)) |
| `every(ms, fn)` | `number` (id) | Repeat: run `fn` every `ms` of scene time until `cancel(id)` or the run ends |
| `cancel(id)` | `boolean` | Stop a waiting `after`/`every`; `true` when one was stopped, `false` for an id already answered or unknown |

## Game loop: `update(dt)` / `draw()`

Define `update` and/or `draw` as globals and let the top-level chunk **return**.
The engine then drives the frame for you:

```lua
window.show(640, 480)

local t = 0

function update(dt)          -- once per frame, before drawing; dt in seconds
    t = t + dt
    if keyboard.pressed("space") then exit() end
end

function draw()              -- once per frame, after update()
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.color = colors.green
    canvas.circle(320 + 100 * math.sin(t), 240, 20, true)
end                          -- chunk returns: the engine drives from here
```

Both callbacks are optional. Once the top-level chunk returns **normally**, the
engine looks for them; if either exists it keeps the script (and its locals,
which stay live inside the callbacks as upvalues) and calls `update(dt)` then
`draw()` once per frame on the main thread — no `sleep()` pacing needed.
`exit()` inside `update` ends the run. The callbacks must be **globals**:
`local function update(dt)` is not found by the engine.

If the chunk never returns (for example it keeps a `while true do` loop), or
defines neither callback, the script stays on its worker thread and can still
draw and pace itself with `sleep()`:

```lua
window.show()
start = time()
while true do
    ft = frametime()
    canvas.text(10, 10, "frame: " .. ft)
    sleep(16)   -- ~60 FPS pacing
end
print("uptime: " .. (time() - start))
```

Because only the engine callbacks touch the state after the hand-off — the
loader thread stops before the engine takes over — the two models are
interchangeable and never race. See [docs/engine-loop.md](engine-loop.md)
for the details.

A script that armed an `after()`/`every()` timer counts as a scene too: a
pending timer keeps the engine driving the frame even with no `update()` or
`draw()` defined, so `after(3000, exit)` alone is a whole (three second) run.

## Scene timers: `after(fn)` / `every(fn)`

Callbacks on the engine's own beat — no `while` loop, no `sleep()` pacing:

```lua
window.show(640, 480)

local bomb = sprites.puff("bomb", 32)
bomb:move(320, 240)

after(2000, function()           -- one shot: two seconds of scene time
  bomb.visible = false
  particles.emit(320, 240, 40)
end)

local ticks = 0
local id = every(500, function() -- the countdown ticks, four a second
  ticks = ticks + 1
  if ticks == 4 then cancel(id) end
end)
```

The waits ride the **scene clock**, not the wall clock: `world.timescale`
stretches them with the rest of the scene, `pause(ms)` sleeps them, and while
the top-level chunk is still running there is no engine beat yet — a timer
armed there counts from the moment the engine takes the frame loop over. The
callbacks run at the head of the engine's beat — before `update()` — and they
may draw, move sprites and call `exit()` like any engine code, so a callback's
marks show in the very frame that answers it. An `every()` that misses its
moment while the scene lags
costs one tick, never a burst; its floor is one millisecond. A timer is a
timer on one scene — an answered `after` and everything of a finished run are
gone with it, and a stale `cancel(id)` simply answers `false`.

Use `pause(ms)` instead of `sleep(ms)` for a still hold: it also freezes the
scene (physics, sprites, controls) for the duration while music/radio keep
playing.

```lua
canvas.text(10, 10, "Paused...")
pause(1000)                       -- the text stays on screen, the scene is still
canvas.text(10, 30, "Back!")
```

## Tweens: `tween()` — animation without the math

A tween glides one numeric property from where it stands to a value, over
milliseconds, with an easing curve — the engine writes every frame in
between. Sprites, controls and plain tables all work as the subject (a HUD
counter that jumps on a score is a plain-table tween), and the glides ride
the **scene clock** exactly like `after()`: stretched by `world.timescale`,
asleep during `pause()`.

```lua
window.show(640, 480)
local cat = sprites.blob("cat", 96)
cat.x, cat.y, cat.tint = 80, 240, rgb(140, 220, 160)

-- the punch pattern: write 0 at once, spring up to 1
tween.from(cat, "scale", 0, 1, 320, "backout")
-- glide to a new spot, overshoot and settle; a done callback runs on landing
tween(cat, "x", 460, 600, "outback", function(sp)
  music.sound("boing", 500, 80)
end)

local hud = { lift = 0 }          -- your own tables are fine subjects too
tween.from(hud, "lift", 12, 0, 200, "quadout")
```

`tween` is one callable table: `tween(obj, prop, to, ms [, easing [, done]])`
plus `tween.to` / `tween.from` / `tween.kill(id)` / `tween.killall(obj)` /
`tween.count()`. Easings are `sine` `quad` `cubic` `expo` `back` `elastic`
`bounce` each with `in` / `out` / `inout` (either word order works), the
aliases `smooth` / `snap` / `wobble`, and `linear`; leaving the name out
gives a plain decelerating glide, and every curve lands on the target value
exactly. A bad argument answers `nil` + a message instead of an error. A
script with *only* tweens still gets the engine frame loop, like the timer
ones. Full notes in [`help/tween.md`](../help/tween.md), and every moving thing
in [`games/bubblepop/`](../games/bubblepop/) is one of these calls.

## Events: `on()` / `emit()` — signals between friends

The mini event bus shares the frame beat with timers and tweens: `on(name, fn)`
hooks a listener, `emit(name, ...)` runs them all (answering **how many** ran —
`0` when nobody listens), and `off(id)` unhooks. Names are case-free. The
engine emits some names itself, so those bindings are fire-and-forget: a key
name (`on("space", fn)`) or `on("mouse.left", fn)` fires on the press beat;
`on("update", fn)` runs every frame with the `dt`; `on("draw", fn)` runs every
paint; and `on("collision", fn)` hears the physics queue as `(a, b,
"enter"/"leave")` — beside the classic per-sprite `onCollide` fields and a
manual `collision.pump()`, which all keep working. A listener's error is
reported with the event's name and marks the run failed like any script error;
an `emit` in progress is a snapshot (see [`help/events.md`](../help/events.md)).

```lua
on("space", function()          -- no update() poll needed
  jump()
end)

on("collision", function(a, b, state)
  if state == "enter" then
    pop(a)
    emit("scored", 10)          -- your own event: emit answers who listened
  end
end)
```

A runnable walk-through of the whole bus — engine names, custom events and
`off()` — lives in [`demos/events_demo.tyro`](../demos/events_demo.tyro).

## Joints: `collision.joint()` — ropes, rods and motors

Connect two sprites and let the solver keep the promise: a **pin** is a
rigid rod (`collision.joint(anchor, ball)` — a pendulum), a **slide** is a
leash that never stretches past `max`, a **rotator** is a spring motor
turning `b` toward `angle` beside `a`, and a **limit** is a gate that only
lets `b` turn between `min` and `max` degrees. Both sprites are switched to
`collide = true` on the way in; `kind` does the rest of the design (static
= bolt, kinematic = hand, dynamic = swing). `joint_free(id)` cuts one,
`joints()` counts the book, `joints_clear()` empties it — and a joint also
leaves the moment one of its sprites loses its body (freed, `collide` off,
or a shape/kind rebuild). When a slide catches what it holds, the event bus
carries the snap: `on("joint_stretch", fn)` hears `(a, b, "taut"/"slack")` —
edges only, ready for a rope sound or a squash tween. Anchors (`ax, ay /
bx, by`) attach anywhere on the
sprites and rotate with them. Full notes in
[`help/joints.md`](../help/joints.md);
[`demos/joints_demo.tyro`](../demos/joints_demo.tyro) swings a pendulum wave,
a mouse-leashed ball and a gearless motor at once.

```lua
hook = sprites.blob("hook", 32)    -- the bolt in the ceiling
hook.kind = "static"
hook.x, hook.y = 320, 80

ball = sprites.blob("ball", 44)
ball.x, ball.y = 420, 80           -- 100 px to the side
collision.joint(hook, ball)        -- ...and it swings
```

## Remembering between runs: `save()` / `load()`

One name, one value, no file to invent: `save("best", 4200)` keeps it in
`save.json` next to the script, and `load("best")` finds it back — `nil` on
the first run, the value after that. Numbers, text, booleans and whole tables
all fit; leaving the value out deletes the key, and every call hits the disk
at once, so a closed window never loses a score. `savejson`/`loadjson` stay
the do-it-yourself pair for your own file names, and the `score.hi_save`
champion files keep working beside them. Notes in
[`help/save.md`](../help/save.md).

```lua
best = load("best") or 0
if score > best then
  best = score
  save("best", best)          -- written now, not on exit
end
canvas.text(8, 8, "best: " .. best)
```

## Grouping drawing with enter() / leave()

`enter()` locks the processing queue: the drawing commands the script queues
after the call keep piling up but are not executed by the main loop. `leave()`
releases the lock, and the whole group is then drawn at one drawing frame —
instead of a large batch being split over several frames. Commands queued
outside the group behave as usual. Use it to group a batch inside `update` /
`draw`:

```lua
window.show()

function draw()
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)

    enter()                             -- hold the queue
    for i = 0, 99 do                    -- this batch is drawn as one
        canvas.circle(rand(0, canvas.width), rand(0, canvas.height), 4, true)
    end
    leave()                             -- draw the whole group now

    canvas.text(10, 10, "frame: " .. time())
end
```

`enter()`/`leave()` nest (they count up and down), and a script that stops or
errors between them is released automatically so the next run never inherits a
frozen queue. See [the engine loop](engine-loop.md) for the commit-watermark
details.

## Measuring the queue (queue.*)

The engine exposes read-only counters over the drawing queue to help diagnose
the "black box, then the text" flicker. A script on the worker thread keeps
queueing draw commands while the main loop drains them once per frame; when a
drain pass stops at the per-frame time budget with commands still waiting, the
frame is drawn with only part of the batch.

| Field | Description |
|-------|-------------|
| `queue.depth` | Commands waiting in the queue right now |
| `queue.holds` | Open `enter()` calls (the hold depth) |

The table also answers the old diagnostic names - `frames`, `drainms`,
`lastgap`, `maxgap`, `limit`, `waits`, `blocked` - which currently report 0.

The everyday use is waiting for queued commands to run, so a drawing getter
answers what the matching setter was set to:

```lua
canvas.rectangle(10, 10, 64, 40, true)
while queue.depth > 0 do
  sleep(1)      -- let the main loop execute the queued commands
end
```

The same view, plus the current `fps`, can be watched without a script by
pressing `CTRL`+`F10`: the engine draws a compact panel in the window's
top-left corner each frame. Press it again to hide the panel.

Wrapping a body in `enter()`/`leave()` holds the queue, so the main loop
cannot drain mid-body and no partial frame can appear.

