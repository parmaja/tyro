# Atomic drawing (`autogroup`)

A drawing body inside `while cycle do ... end` used to flicker: an opaque black
box appeared first and its text showed up one frame later. This document
explains why, and how the engine now prevents it by default.

## The problem

Drawing commands are queued by the script thread and drained by the main loop
(on the main thread) once per frame, while the script is still building the
body. If a frame boundary lands in the middle of a body, the engine draws only
the commands queued so far — the black box without its text — presents that
frame, and the text arrives a frame later.

This is a producer/consumer race, not a frame-budget problem: a body usually
queues in well under a frame, so the natural split is rare, but it is real
whenever a body takes a while to build (string work, a wide loop, I/O).

## The fix

The engine groups a body: the queue is **held** from the moment a `cycle` body
starts until the next yield point, so the whole body is committed in a single
drawing cycle.

- The hold is released at the next yield point: the next `cycle`, `sleep()`, or
  `console.read()`. Releasing before a blocking call is what keeps a prompt
  drawn earlier visible while the script waits.
- It is taken again after the yield, and when the script starts.
- It is cleared when a script stops or errors, so a dead run can never freeze
  the queue.
- `enter()` / `leave()` use the same gate, so the two nest cleanly.

The gate in `ProcessQueue` is `explicit hold OR automatic hold`. While either is
open, no command is executed.

## Default behavior

Nothing to call — each `while cycle` body is atomic:

```lua
window.show()
while cycle do
    canvas.color = colors.black
    canvas.rectangle(0, 0, 640, 70, true)       -- the box ...
    canvas.color = colors.green
    canvas.text(8, 8, "Station : " .. station)  -- ... and its text together
end
```

Even a body that takes several frames to build lands in one drawing cycle.

## `enter()` / `leave()`

Hold the queue by hand. Anything queued between the two calls is drawn as one
group:

```lua
window.show()
while cycle do
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)

    enter()                             -- hold the queue
    for i = 0, 99 do                    -- this batch is drawn as one
        canvas.circle(rand(0, canvas.width), rand(0, canvas.height), 4, true)
    end
    leave()                             -- draw the whole group now

    canvas.text(10, 10, "frame: " .. time())
end
```

`enter()` / `leave()` nest (they count up and down). Use them to group part of
an iteration, or a body outside a cycle loop. A script that stops or errors
between them is released automatically.

## `atomic(f)`

The same pair wrapped around a function, which reads better when grouping is the
point of a helper:

```lua
local function header(text)
    atomic(function()
        canvas.color = colors.black
        canvas.rectangle(0, 0, 640, 70, true)
        canvas.color = colors.green
        canvas.text(8, 8, text)
    end)
end
```

If `f` raises, the hold is still released before the error is reported.

## `autogroup(on)`

Turns the automatic per-`cycle` grouping on (`true`, the default) or off:

```lua
autogroup(false)   -- raw streaming: a frame may drain part of a body again
```

Use it only when you deliberately want progressive drawing spread across frames.
`queue.group` reports the current setting.

## Yield points

These release the hold so what is already queued is shown, then re-arm it:

| Call | Releases the hold before |
|------|--------------------------|
| `cycle` | waiting for the next frame (the commit point) |
| `sleep(ms)` | sleeping |
| `console.read()` | waiting for the user to press Enter |

## Diagnostics

Read-only counters, plus a high-resolution clock:

| Field | Description |
|-------|-------------|
| `queue.consumed` | Drawing commands executed since the engine started |
| `queue.passes` | `ProcessQueue` passes that executed at least one command |
| `queue.deferred` | Passes stopped at the frame time budget with more waiting (each drew a partial batch) |
| `queue.depth` | Commands waiting in the queue right now |
| `queue.max` | Largest backlog seen at the start of a pass (high-water mark) |
| `queue.holds` | Open `enter()` calls (the hold depth) |
| `queue.frames` | Drawing cycles presented since the engine started |
| `queue.group` | `true` while automatic per-`cycle` grouping is enabled |
| `queue.drainms` | Duration of the most recent drain pass, in milliseconds |
| `queue.lastgap` | Milliseconds between the last two queued commands (no yield in between) |
| `queue.maxgap` | Largest such gap of the run: the script stalled between two queue calls |
| `millis()` | Monotonic high-resolution clock in milliseconds |

Because the script builds a body while the main loop drains the queue on another
thread, the script cannot sample at the instant a frame falls mid-body. It can
catch it right after the body returns, before the next `cycle`:

```lua
while cycle do
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

Grouping commits a whole body in one drawing cycle, but it cannot fix the script
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
reset at every yield (`cycle`, `sleep()`, `console.read()`), so frame waits are
never counted. A small `maxgap` with a black-only header means the metadata
itself is empty (buffering/reconnect), not a rendering race.

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

while cycle do
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
| `auto-group` (default) | 0 |
| `group-off` (`autogroup(false)`) | 9 |
| `enter-leave` | 0 |

## Notes

- Grouping makes each iteration atomic; it does not raise the frame budget. A
  body that queues a very large number of commands still lands in one frame, so
  keep bodies reasonably sized.
- A `cycle` read is the commit point: it draws the body queued since the
  previous read, then starts a new group.
- Scripts running on the main thread (console lines) never block and are not
  grouped.
- `autogroup(false)` is an escape hatch; the default is safe without any extra
  code.
