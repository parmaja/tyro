# The engine-driven loop: `update(dt)` / `draw()`

Tyro has one recommended game loop: define `update` and/or `draw` as **globals**
and let the top-level chunk of the script return. The engine then calls them
once per drawing frame.

```lua
window.show()
local x = 0

function update(dt)          -- called once per frame, dt in seconds
    x = x + 120 * dt
end

function draw()              -- called once per frame, after update
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.color = colors.white
    canvas.circle(x, 100, 20, true)
end                          -- top-level returns: the engine drives from here
```

`update(dt)` runs first, then `draw()`, then the queued drawing commands are
drained by the main loop. Both callbacks are optional; a script may define only
`update` (logic without drawing) or only `draw` (a static picture that reacts to
input).

## How it decides

- The main chunk runs on the worker thread (`TTyroScriptThread`) as usual. When
  it **returns normally**, the worker queues `TTyroEngine.OnScriptFinished` on
  the main thread (`TThread.Queue`) and stops touching Lua.
- The engine inspects the globals: if `update` and/or `draw` exist it keeps the
  script (and its `lua_State`) alive, sets `Handoff`, and calls the callbacks on
  the **main thread** once per frame. Otherwise it releases the worker, which
  closes the state.
- A top-level chunk that never returns keeps its worker thread. It can still
  draw and pace itself with `sleep(ms)`; in that mode there are no callbacks.
- A Lua error in a callback is caught (`pcall`) and reported; the session keeps
  running and the callbacks are called again next frame.
- `exit()` (or the engine stopping) ends an engine-driven run; the worker and
  its state are then released.
- The callbacks must be **globals**. `local function update(dt)` is not found by
  the engine, because only global names are inspected.

Because only these callbacks touch the state after the hand-off — the loader
thread stops before the engine takes over — the callbacks never race with the
worker.

## Drawing in a callback

Drawing works exactly as it does anywhere else in a script: `canvas.*` queues
commands that the main loop executes inside `BeginDraw`/`EndDraw`. The same is
true for sprites, controls, sound, and the other facades.

```lua
window.show(640, 480)
local t = 0

function update(dt)
    t = t + dt
    if keyboard.pressed("escape") then exit() end
end

function draw()
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.color = colors.green
    canvas.circle(320 + 100 * math.sin(t), 240, 20, true)
end
```

Locals declared before the callbacks stay live as upvalues. Use them for game
state instead of globals when the state is private to the script.

## Grouping a body with `enter()` / `leave()`

The queue is drained once per drawing frame while the script is still building
the next body. If a frame boundary lands in the middle of a body, only the
commands queued so far are presented — for example a black box without its
text, which then appears one frame later.

`enter()` holds the queue and `leave()` releases it, so everything queued in
between is drawn as one group in a single drawing frame:

```lua
window.show()

function draw()
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)

    enter()                             -- hold the queue
    for i = 0, 99 do                    -- this batch is drawn as one
        canvas.circle(rand(0, canvas.width), rand(0, canvas.height), 4, true)
    end
    leave()                             -- draw the whole group now

    canvas.text(10, 10, "frame: " .. time())
end
```

`enter()` / `leave()` nest (they count up and down). A script that stops or
errors between them is released automatically, so a dead run never freezes the
queue.

## Slow drawing

Grouping is about *when* a body is committed, so it also holds when a single
scripted draw command is slow. `canvas.test(ms)` is a diagnostic command that
emulates exactly that: it draws a marker rectangle, then spins for about `ms`
milliseconds **on the main thread** — the same place every scripted draw command
runs.

```lua
window.show(320, 240)

function update(dt)
    if keyboard.pressed("escape") then exit() end
end

function draw()
    canvas.color = colors.black
    canvas.rectangle(0, 0, 320, 60, true)   -- the box
    canvas.color = colors.green
    canvas.test(80)                         -- a draw call slower than a frame
    canvas.text(8, 8, "text arrives with the box")  -- not one frame later
end
```