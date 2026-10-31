# Atomic drawing (`enter` / `leave`)

A drawing body can flicker: an opaque black box appears first and its text shows
up one frame later. This document explains why, and how `enter()` / `leave()`
prevents it.

## The problem

Drawing commands are queued by the script and drained by the main loop (on the
main thread) once per drawing frame, while the script is still building the
body. If a frame boundary lands in the middle of a body, the engine draws only
the commands queued so far — the black box without its text — presents that
frame, and the text arrives a frame later.

This is a producer/consumer race, not a frame-budget problem: a body usually
queues in well under a frame, so the natural split is rare, but it is real
whenever a body takes a while to build (string work, a wide loop, I/O).

## The fix: hold the queue

`enter()` **holds** the queue: the main loop keeps receiving commands but does
not execute them until `leave()` balances the hold. The whole group queued in
between is then committed and drawn in a single drawing frame.

```lua
window.show()

function draw()
    canvas.color = colors.black
    canvas.rectangle(0, 0, 640, 70, true)       -- the box ...

    enter()                                     -- hold the queue
    canvas.color = colors.green
    canvas.text(8, 8, "Station : " .. station)  -- ... and its text
    leave()                                     -- draw the group now
end
```

How the commit works:

- `enter()` arms the hold and snapshots the enqueue counter (the **commit
  watermark**). From then on the main loop drains only the commands committed
  *before* the hold, never the body still being built.
- `leave()` lowers the hold. When the last hold is released it arms a one-shot
  **flush**, so the next drain pass executes the whole group in one
  `BeginDraw`/`EndDraw` pair, ignoring the per-frame time budget.
- Nested `enter()` calls count up and need the same number of `leave()` calls.
  Only the outermost hold moves the watermark.
- A script that stops or errors between `enter()` and `leave()` is released
  automatically, so a dead run can never freeze the queue.

## Using it

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

Use it to group a batch inside a `draw()` callback, a menu build, or any body
whose parts must appear together. Commands queued outside the group behave as
usual.

## Yield points

A yield draws what is already queued, even inside an open group, then the script
continues. These calls yield:

| Call | Yields before |
|------|---------------|
| `sleep(ms)` | sleeping |
| `pause(ms)` | freezing the scene (like `sleep`, but the engine stops simulating) |
| `console.read()` | waiting for the user to press Enter |

`pause(ms)` behaves like `sleep(ms)` for the script thread, but it also marks the
engine as paused for the duration: the main loop skips `Update`, so physics,
sprite animation, per-sprite `on_update`/`on_collide` and controls stop advancing
and the last drawn frame stays on screen. `Update`'s audio work
(`UpdateMelodies`/`UpdateWaveforms`) and `RayUpdates.Update` (music/radio
streaming) keep running, as does window input, so the window stays responsive and
sound does not stutter. `tests/test_pause_probe.tyro` checks the difference: a
sprite script that moves 1px per `on_update` does not move across `pause()`, but
does across `sleep()`.

## Diagnostics

Read-only counters, plus a high-resolution clock. They are diagnostics only:
debug builds (the `diagnose` conditional) keep them, a release build compiles
them out and reports 0.

| Field | Description |
|-------|-------------|
| `queue.consumed` | Drawing commands executed since the engine started |
| `queue.passes` | `ProcessQueue` passes that executed at least one command |
| `queue.deferred` | Passes stopped at the frame time budget with more waiting (each drew a partial batch) |
| `queue.depth` | Commands waiting in the queue right now |
| `queue.max` | Largest backlog seen at the start of a pass (high-water mark) |
| `queue.holds` | Open `enter()` calls (the hold depth) |
| `queue.frames` | Drawing frames presented since the engine started |
| `queue.drainms` | Duration of the most recent drain pass, in milliseconds |
| `queue.lastgap` | Milliseconds between the last two queued commands (no yield in between) |
| `queue.maxgap` | Largest such gap of the run: the script stalled between two queue calls |
| `millis()` | Monotonic high-resolution clock in milliseconds |

Because the script builds a body while the main loop drains the queue on another
thread, the script cannot sample at the instant a frame falls mid-body. It can
catch it right after the body returns:

```lua
function draw()
    local before = queue.consumed       -- queue is empty here
    enter()                             -- or queue the body bare
    local n = draw_body()
    leave()
    local mid = queue.consumed - before -- drained while the body was built
    -- 0 < mid < n  ->  a frame showed only part of the body (the flicker)
end
```

`millis()` times how long a body takes to queue the same way.

### Catching a stall between two commands

Grouping commits a whole body in one drawing frame, but it cannot fix the script
taking a long time *between* two commands — e.g. `radio.station`/`radio.title`,
whose getters take locks the network thread may hold, blocking the script for a
second. The queue is held during that second, so nothing is drawn and the frame
freezes, which looks like "a black box first, then its text".

Every queued command is stamped with its enqueue time, so the gap is directly
visible: a big `queue.maxgap` means the script stalled between two queue calls.
If `radio.station` blocks between the box and the text:

```lua
canvas.rectangle(0, 0, 640, 70, true)
-- radio.station blocks here; the queue is held, so nothing is drawn yet
canvas.text(8, 8, "Station : " .. radio.station)
print("stall between box and text: " .. queue.maxgap .. " ms")
```

`queue.maxgap` is the largest gap of the run, `queue.lastgap` only the most
recent pair; use the former to catch a stall in the middle of a body. Gaps are
reset at every yield (`sleep()`, `pause()`, `console.read()`) and at the start of
each engine-driven frame, so frame waits are never counted. A small `maxgap` with
a black-only header means the metadata itself is empty (buffering/reconnect), not
a rendering race.

`tests/test_gap.tyro` exercises the counter:

| Scenario | `queue.maxgap` |
|----------|----------------|
| straight header + text bodies | ~0 ms |
| `sleep(150)` between the box and the text (a yield) | ~0 ms |
| 150 ms busy-wait, no yield (a blocking getter in disguise) | ~150 ms |

To be told when a stall happens, watch the gap from the script. The first gap
that clears the limit is printed to the console; a new report needs a gap a full
limit above the last one reported, so drift never spams:

```lua
local gap_warned = 0
local function gapwatch(aLimitMS)
  local g = queue.maxgap
  if g > aLimitMS and g >= gap_warned + aLimitMS then
    gap_warned = g
    println("GAP: " .. g .. " ms between two queued commands"
      .. " (limit " .. aLimitMS .. " ms, lastgap " .. queue.lastgap .. " ms)")
  end
  return g
end

function draw()
    -- ...draw the body...
    gapwatch(250)   -- >250 ms stall? the console says so
end
```

`tests/test_gap_report.tyro` checks the watch logic (alert counts: 0 for plain
bodies, 0 for `sleep()`, 1 for a 150 ms stall, one more for a 400 ms stall).

## Verifying it

`tests/test_queue_lag.tyro` measures a body that busy-spins across frame
boundaries:

| Phase | Mid-body splits / 10 |
|-------|----------------------|
| no grouping | 9 |
| `enter`/`leave` | 0 |

## Notes

- Grouping makes each body atomic; it does not raise the frame budget. A body
  that queues a very large number of commands still lands in one frame, so keep
  bodies reasonably sized.
- `enter()` is optional: the price of not using it is the occasional partial
  frame, never a lost command.
- Scripts running on the main thread (console lines) never block and are not
  grouped.
