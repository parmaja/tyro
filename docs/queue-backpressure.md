# Bounded queue / back-pressure

A script normally yields once per frame (`while cycle do`), so the main loop
drains the queue as fast as the worker fills it. A script that does **not**
yield — a tight loop, or `autogroup(false)` streaming — is an unthrottled
producer: `Engine.Queue` grows, memory and lag grow with it, and more frames are
drawn from a partial batch. `queuelimit(n)` caps the queue so the producer waits
for the consumer instead.

Related: [atomic.md](atomic.md) (grouping), `README.md` (threading model).

## The API

| Call | Meaning |
|------|---------|
| `queuelimit(n)` | Cap the queue at `n` commands (`n = 0` restores the default unlimited behavior) |

Read back through the `queue` table (read-only):

| Field | Description |
|-------|-------------|
| `queue.limit` | Current cap (0 = unlimited) |
| `queue.waits` | Times a producer had to wait for room |
| `queue.blocked` | Producers waiting for room right now |

```lua
queuelimit(2048)          -- at most ~2048 commands buffered
autogroup(false)          -- stream without holding
while cycle do
    for i = 1, 5000 do
        canvas.point(rand(0, canvas.width), rand(0, canvas.height))
        -- after ~2048 queued, point() blocks until the main loop drains some
    end
end
```

The default is unlimited (`0`), so existing scripts are unaffected until they
opt in.

## Why the queue, not `Synchronize`

Drawing must happen **inside** `BeginDrawing`/`EndDrawing` with the canvas render
texture bound, which is exactly the window `ProcessQueue` drains in. A
`Synchronize` callback runs wherever the main thread pumps `CheckSynchronize`
(outside that window), and it blocks the worker per call. The queue keeps the
worker producing, preserves FIFO order and grouping, stays cancellable, and
lets the consumer cap its work per frame (the frame budget). The request/response
calls (`console.read`, `sprite.load`, control creation) still use `Synchronize`,
which is the right split: **queue = streaming output, `Synchronize` = RPC**.

## Why a cap

`TTyroScript.AddQueueObject` (`src/tyrolib/TyroScripts.pas`) admits every command
unless the script is stopped. `ProcessQueue` (`src/TyroEngines.pas`) drains once
per frame, capped by the per-frame time budget. A producer that does not yield
therefore outruns the drain:

| Symptom | Counter |
|---------|---------|
| Memory grows (objects pile up) | `queue.depth`, `queue.max` |
| Latency grows (commands drawn seconds later) | `queue.depth` vs. `queue.consumed` |
| More partial-frame drains ("black box, then text") | `queue.deferred` |
| Worker spins instead of blocking | `Thread.Yield` in `AddQueueObject` |

A cap makes the **producer** pay when the consumer is behind.

## When it throttles

`ThrottleQueue` returns at once when any of these hold:

1. `QueueLimit = 0` (unlimited; the default).
2. A group is open (`FQueueHold`/`FQueueAuto`): `ProcessQueue` executes nothing
   while held, so waiting would deadlock the script before `leave()`.
3. The window is hidden or not ready (headless): there is no drawing cycle to
   drain the queue.
4. `Running` is false (engine stopping).

Otherwise `AddQueueObject` blocks after admitting the command until the backlog
drops below the cap. It waits on an auto-reset `FQueueSpaceEvent` that
`ProcessQueue` sets after every pass (and that `Stop`/`Clear`/`SetQueueLimit`
set), with a 50 ms poll timeout — the same pattern as `WaitToNextFrame`, so a
missed signal costs latency, never a hang.

The wait happens **after** the global `Lock` is released: `ProcessQueue` needs
that same lock to extract, so waiting under it would stall the consumer.

## Implementation

`TTyroEngine` (`src/TyroEngines.pas`):

```pascal
procedure TTyroEngine.ThrottleQueue;
var
  Limit: Integer;
begin
  Limit := TInterlocked.Add(FQueueLimit, 0);
  if Limit <= 0 then
    Exit;
  if (TInterlocked.Add(FQueueHold, 0) > 0) or
     (TInterlocked.Add(FQueueAuto, 0) > 0) then
    Exit;
  if (FFrameEvent = nil) or IsWindowHidden or (not RayLib.IsWindowReady) then
    Exit;

  TInterlocked.Increment(FQueueBlocked);
  try
    while Running do
    begin
      Lock.Enter;
      try
        if Queue.Count < Limit then
          Break;
      finally
        Lock.Leave;
      end;
      TInterlocked.Increment(FQueueWaits);
      if FQueueSpaceEvent = nil then
        Break;
      FQueueSpaceEvent.WaitFor(50);
      Limit := TInterlocked.Add(FQueueLimit, 0);
      if Limit <= 0 then
        Break;
    end;
  finally
    TInterlocked.Decrement(FQueueBlocked);
  end;
end;
```

`AddQueueObject` calls it after a successful add, outside the lock:

```pascal
  if Admitted then
    Engine.ThrottleQueue;   // blocks only when over the limit
  if Thread <> nil then
    Thread.Yield;
```

`ProcessQueue` releases the waiters after each pass:

```pascal
    finally
      CanvasLock.Leave;
      if FQueueSpaceEvent <> nil then
        FQueueSpaceEvent.SetEvent;
    end;
```

`TQueueStats` gained `Limit`, `Waits`, `Blocked`, surfaced in `TLuaQueue.Getter`
as `queue.limit`, `queue.waits`, `queue.blocked`; `queuelimit_func` and the
`queuelimit` global are registered next to `autogroup`.

## Edge cases / deadlock safety

| Situation | Behavior | Why |
|-----------|----------|-----|
| `enter()` / `leave()` / `atomic()` / `cycle` body | **No throttle** | `ProcessQueue` executes nothing while held; blocking would deadlock before `leave()` |
| `while cycle do` body | Rarely throttles | `cycle` is already one body per frame (natural back-pressure) |
| Headless / hidden window | **No throttle** | No drawing cycle consumes the queue |
| REPL / main-thread script | **No throttle** | `Thread = nil`; blocking the main thread on its own consumer deadlocks |
| Script stopped while waiting | Wait loop exits (`Running` false) within the poll tick | Mirrors `WaitToNextFrame` polling `Active` |
| Engine shutdown | `Stop`/`Clear` set the space event; waiters wake | Same primitive, no new shutdown path |
| A single held group bigger than the cap | Allowed (grouping wins) | Atomicity cannot be preserved under a hard cap; `queue.max` still reports it |

## Verifying it

`tests/test_queue_limit.tyro` streams 4000 commands with `autogroup(false)` and
no yield, first unlimited then capped at 128. Expected
(`tests/test_queue_limit_result.txt`):

| Phase | `queue.limit` | max depth seen | `queue.waits` |
|-------|---------------|----------------|---------------|
| unlimited | 0 | 4000 (all queued) | 0 |
| limited | 128 | 127 (bounded) | > 0 |

`VERDICT limit_readback=true bounded=true waited=true`.

## Notes

- The cap is a safety net for streaming scripts; it does not replace grouping
  (`atomic.md`) and does not raise the frame budget.
- It is set from Lua with `queuelimit(n)`. A config/CLI default could be added
  later; the property is `Engine.QueueLimit`.
- It does not make a slow command faster (`queue.maxgap` still reports stalls)
  and does not change draw throughput (all GPU work is still serialized on the
  main thread).
