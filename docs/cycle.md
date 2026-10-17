# `cycle` — the frame gate

`cycle` is Tyro's game-loop tick. It is a boolean global that blocks the
script until the current frame has been drawn and presented, so

```lua
while cycle do
    move(player, 1, 0)
end
```

runs its body **once per drawn frame** instead of spinning as fast as the CPU
allows. It is Tyro's replacement for `while true do`.

## For the script author

Read `cycle` as a question:

> "Has the screen finished drawing the previous frame yet? — then yes, go on."

| It returns | When |
|------------|------|
| `true`  | The next frame was presented; the body may run again |
| `false` | The script was stopped while waiting — end the loop |

Rules:

- It is a **gate, not a value**: never cached, every read waits again. Write
  `while cycle do`, not `while cycle() do` — `cycle` is not a function, and
  calling the boolean it returns raises *"attempt to call a boolean value"*.
- The wait is the **end** of a frame (`EndDrawing` has completed), not the
  beginning of one. The body that queued the frame's commands runs again
  *after* those commands were presented.
- **No window → no wait.** With a hidden, missing or headless window it never
  blocks and behaves like `while true do`, so scripts and tests do not hang.
- Waiting is interruptible: the script can still be stopped, and it notices
  within 50 ms, exiting the loop cleanly.

```lua
window.show()
local x = 0
while cycle do           -- one iteration per frame, paced by the display
    x = x + 1
    canvas.text(8, 8, "frame " .. x)
end                      -- only ends if the script is stopped (or cycle is false)
```

### Other yield points

`cycle` is one of four places that commit the current body and let the main
loop draw it before the script blocks:

| Call | Releases the current body before |
|------|----------------------------------|
| `cycle` | waiting for the next frame (the commit point) |
| `sleep(ms)` | sleeping |
| `pause(ms)` | freezing the scene (like `sleep`, but the engine stops simulating) |
| `console.read()` | waiting for the user to press Enter |

See `docs/atomic.md` for what "commit" does to the drawing queue.

## The engine-driven loop: `update(dt)` / `draw()`

`while cycle do` is one of **two** game loops. The other is engine-driven: a
script that defines `update` and/or `draw` as globals and lets its top-level
chunk **return** hands the frame to the engine.

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
    canvas.circle(x, 100, 20, false)
end                          -- top-level returns: the engine drives from here
```

How it decides, and why it is safe with Lua's single-threaded VM:

- The main chunk runs on the worker (`TTyroScriptThread`) as usual. When it
  **returns normally**, the worker queues `TTyroEngine.OnScriptFinished` on the
  main thread (`TThread.Queue`). The worker stops touching Lua at that point.
- The engine inspects the globals: if `update` and/or `draw` exist it keeps the
  script (and its `lua_State`) alive, sets `Handoff`, and calls the callbacks on
  the **main thread** once per frame. Otherwise it releases the worker, which
  closes the state.
- A script that never returns (the `while cycle do` case) never reaches this
  point, so the old loop keeps working unchanged.
- A Lua error in a callback is caught (`pcall`) and reported; the session keeps
  running and the callbacks are called again next frame.
- `exit()` (or the engine stopping) ends an engine-driven run; the worker and
  its state are then released.
- `cycle` never blocks in this mode (it returns `false`), because the main
  thread is the one that would have to present the frame.

Both loops talk to the same drawing queue, so `canvas.*` works the same way in
either. `update(dt)` receives the frame time in seconds.

## How the whole body is guaranteed to be drawn in one frame

Everything a script queues goes into a command queue that the **main loop**
drains once per frame, while the script is already building the next body.
Three mechanisms make "the whole `while cycle` body lands in exactly one
drawing cycle" true — none of them relies on timing.

**1. Every queued command is counted.**
Each accepted command bumps a monotonic counter (`FQueueEnqueued`,
`NoteEnqueue` in `TyroEngines.pas`). One body runs on one thread, so by the
time the body ends, all of its commands are counted.

**2. Reading `cycle` snapshots that count — the commit watermark.**
A `cycle` read does two things (`WaitToNextFrame` in `TyroEngines.pas`):

1. **Release** the hold taken for the body that just finished
   (`ReleaseAutoQueue`) — this *commits* it and arms a flush, so the next
   drain pass is allowed to ignore the frame time budget.
2. **Wait** for `EndDrawing`, then **re-arm** the hold for the body about to
   run (`HoldAutoQueue`), which calls `MarkQueueCommit`:
   `FQueueCommitUpTo := FQueueEnqueued` — literally *"everything queued up to
   now"*, i.e. exactly the committed body.

**3. The drain executes exactly that count, atomically.**
While a hold is open, `ProcessQueue` computes

```
ToDrain := FQueueCommitUpTo - FQueueConsumed
```

and stops at `Consumed >= ToDrain`, between one `Board.BeginDraw`/`EndDraw`
pair, ignoring the frame budget because the flush flag is set. So:

- `ToDrain <= 0` → draw nothing (the body still being built is not partial-drawn),
- otherwise → drain precisely the committed body, never a command more or less.

### Why it cannot race

- The script re-arms the hold immediately after waking, usually *before* the
  main loop's next `ProcessQueue` pass. Without the watermark the gate would
  look closed and nothing would be drawn — the backlog would grow and text
  would appear seconds late. The watermark lets that pass still see *"N
  commands are owed"* and drain exactly those N.
- The queue only shrinks on the main thread, and `FQueueConsumed` is
  published after every executed command (a command that raises is caught,
  logged and still counted), so the drain can never run short of `ToDrain`.
- `queuelimit()` throttling is **skipped while a hold is open**: otherwise a
  script could block waiting for a drain that cannot happen until it releases
  the hold — a deadlock.
- Self-healing: if a script stops or errors while holding the queue
  (including the automatic `cycle` hold), `ProcessQueue` clears the stuck
  hold, so a dead run can never freeze drawing.

### The timeline of one iteration

```
body runs, queues drawing commands
   │  (script reaches the next read of cycle)
   ▼
release hold → commit watermark covers the whole body → flush armed
   │  (script blocks on the frame event)
   ▼
ProcessQueue drains exactly the committed body in one BeginDraw/EndDraw
   ▼
EndDrawing presents the frame → the waiting script is woken
   ▼
hold re-armed for the next body → run it
```

## Verifying it from a script

`queue.consumed` reports how many commands have been executed; compare it
across a `cycle` read:

```lua
while cycle do
    local before = queue.consumed
    draw_body()
    local queued = queue.consumed - before   -- 0 until the next cycle commit
end
```

`queue.group` shows whether automatic grouping is on (the default),
`queue.holds` the open hold depth. See the diagnostics table in
`docs/atomic.md`.

## Slow drawing (a draw call slower than a frame)

Grouping is about *when* a body is committed, so it also holds when a single
scripted draw command is slow. `canvas.test(ms)` is a diagnostic command that
emulates exactly that: it draws a marker rectangle, then spins for about `ms`
milliseconds **on the main thread** — the same place every scripted draw
command runs. Use it to reproduce "slow drawing":

```lua
window.show(320, 240)
while cycle do
    canvas.color = colors.black
    canvas.rectangle(0, 0, 320, 60, true)   -- the box
    canvas.color = colors.green
    canvas.test(80)                         -- a draw call slower than a frame
    canvas.text(8, 8, "text arrives with the box")  -- not one frame later
end
```

`tests/test_slow_draw.tyro` runs a body of `box -> canvas.test(80) -> text` in
three phases and counts, per body, the **drain passes** it needed (`1` = the
whole body drawn in a single drawing cycle):

| Phase | body split | drain passes per body | `queue.deferred` |
|-------|-----------|------------------------|------------------|
| `auto-group` (default) | 0 / 10 | 1 | 0 |
| `group-off` (`autogroup(false)`) | 9 / 10 | 2 | 9 |
| `enter-leave` | 0 / 10 | 1 | 0 |

In the `auto-group` phase the drain pass lasts about 80 ms — far past one
frame — yet `passes = 1`: the box, the marker and the text are committed
together. With `autogroup(false)` the frame budget stops the pass *after* the
slow command and the text comes a frame later (the black-box-then-text
flicker), which is what `queue.deferred` counts.

So a slow draw does **not** split a `cycle` body; it only makes that one frame
long. If a body still flickers, look for a **yield inside it** (`sleep()`,
`pause()`, `console.read()`, or a second `cycle` read), because a yield is a
commit point and draws the part queued before it — or for `autogroup(false)`.

## Related

- `docs/atomic.md` — the hold/group machinery (`autogroup`, `enter()`/`leave()`),
  yield points and diagnostics.
- `docs/queue-backpressure.md` — `queuelimit()` and bounded queues.
