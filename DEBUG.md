# Lua Debugger (DBGp client) for Tyro

Add an Xdebug/DBGp debugger to Tyro so its embedded Lua scripts can be debugged from
miniEdit (the user's editor). Tyro plays the role of the DBGp **client / debuggee**:
it connects to miniEdit's DBGp **listener** (port 9000) and answers commands.

## Design decisions (asked & answered)

- Editor: miniEdit (`D:\lab\pascal\miniEdit`), not VS Code / PhpStorm.
- Pause behavior: **freeze the whole engine** (main raylib loop stops while Lua is
  paused at a breakpoint), not just the Lua script thread.
- Scope: full basics —
  - continue (`run`), `step_into`, `step_over`, `step_out`
  - line breakpoints: `breakpoint_set` / `breakpoint_remove`
  - `stack_get`, `context_get` (locals + globals)
  - `property_get` / `property_set` (and `property_value` used by miniEdit watches)
  - `eval`
- Transport: minilib socket package (`MiniSockets.lpk`).

## Roles / wire protocol

miniEdit's listener (`dbgpServers.pas`) is the protocol contract. All messages are
NULL (`#0`) terminated. Every command from the editor carries `-i <transaction_id>`;
the engine reply `<response ...>` MUST echo the same `transaction_id`.

Editor command flow on connect (`TdbgpConnection.Prepare`):
1. (engine sends `init` packet first — editor reads it without sending anything)
2. `feature_set -n show_hidden -v 1`
3. `feature_set -n max_depth -v <N>`
4. `feature_set -n max_children -v 100`
5. `breakpoint_set -t exception -X Error -s enabled`
6. `breakpoint_set -t exception -X Warning -s enabled`
7. existing user line breakpoints:
   - add:   `breakpoint_set -t line -n <line> -f <fileuri>`
   - remove: `breakpoint_remove -d <id>`
8. `run` (or `step_into` when BreakOnFirstLine)
9. after each stop, editor sends controls:
   - `run` | `step_into` | `step_over` | `step_out`
   - watches: `property_value -n "<name>" -m 1024`
   - eval:    `eval -- echo <expr>`  (expr is everything after the leading `echo `)
   - `stack_get`
   - `detach`, `stop`

Response shape (XML, `xmlns="urn:debugger_protocol_v1"`), root = `<response>` with
attributes `command`, `transaction_id`, optional `status`, `id` (breakpoint_set).

miniEdit response attribute uses:
- `response.id`           -> breakpoint id assigned by engine
- `response.status`       -> `stopping` disconnects the editor; `stoped` is ignored
- `stack_get`             -> `<stack where level type="file" filename fileuri lineno/>`
  children; `filename` passed through `URIToFileName`, `lineno` as int
- `property_value`/`eval` -> scalar: base64 CDATA in `<response>` (attributes
  `type`, `encoding="base64"`, `size`); composite: `<property name fullname type
  size encoding="base64">` children under the response + `children`/`numchildren`
- `CheckError` fails unless `transaction_id` matches.

miniEdit `URIToFileName` decodes `file:///` URI; engine must likewise parse the
`-f file:///...` file argument (`FileNameToURI`/`URIToFileName` exist in minilib
`mnUtils` — reuse them).

## Tyro-side architecture

New unit: `src/TyroDebugger.pas` (DBGp client over `TmnClientSocket` + Lua hooks).

Components:
- `TTyroDebugger` — owns the socket connection, breakpoint list, step state,
  command dispatch; globals/accessors gated by existing `IsDebug` flag.
- Lua hook: add a line hook (`LUA_MASKLINE`, count e.g. 1) alongside the existing
  `LUA_MASKCOUNT` 100 hook in `LuaClasses.pas` (`HookCount` currently raises
  `luaL_error` on termination). Debug hook inspects current line, checks
  breakpoints / step state, and when stopped blocks reading socket + replying until
  resume command arrives.

Execution states (mirror `TLuaStatus`):
- running: Lua executes; hook returns immediately (only breakpoint check)
- stopped: Lua blocked in hook; engine answers commands (`stack_get`,
  `context_get`, `property_get/set`, `eval`, breakpoint changes); main thread frozen
- step kinds: step_into / step_over / step_out tracked in hook
- finished: after script ends, engine sends `status="stopping"` then closes

Command loop inside the paused hook:
- `feature_get`, `feature_set` -> immediate reply, stay stopped
- `breakpoint_set` / `remove` -> update list, reply with `id`, stay stopped
- `property_value`, `context_get`, `property_get/set`, `eval`, `stack_get` ->
  build reply, stay stopped
- `run` / `step_*` -> set new state, break out of loop (resume Lua)
- `stop` / `detach` -> disable hook, terminate run, reply, close

Before any script execution when `IsDebug`: connect to `127.0.0.1:9000`, send init:
```
<?xml version="1.0" encoding="iso-8859-1"?>
<init xmlns="urn:debugger_protocol_v1" fileuri="file:///..." language="Lua"
      appid="tyro" idekey="..." engine_version="Tyro">...</init>
```
(`idekey` read by editor; `fileuri` = URI of the running script.)

### Lua integration points (already present)

- `LuaAPI.pas`: `lua_sethook`, `lua_gethook`, hook consts (`LUA_MASKLINE`,
  `LUA_MASKCOUNT`), `lua_getstack`, `lua_getinfo`, `lua_getlocal`/`setlocal`,
  `lua_getupvalue`/`setupvalue`, `lua_settop`/`gettop`, `lua_Debug` record,
  `lua_Hook` callback type.
- `LuaClasses.pas`: existing `HookCount` (`lua_sethook(..., LUA_MASKCOUNT, 100)`),
  threadvar `LuaStatus` (`luaNone, luaReady, luaRunning, luaTerminated`),
  `LuaSetTerminated`, `TLuaHelper.GetStack/GetInfo`, `RunString`. Lua 5.3 embedded.
- Scripts run on `TTyroScriptThread` (lower priority) or on main thread when
  `RunInMain` (`TyroEngines.pas` `TTyroMain.Init`, lines ~360-389). `TTyroScript`
  lifecycle: `Start -> BeforeRun -> Run -> AfterRun`.

### Freezing the engine (whole engine pauses)

- The Lua thread blocks in the hook at a stop; the main loop must also stop
  advancing. Add a `TDebuggerHalt` flag / `Sleep` in `TTyroMain` draw/update loop:
  while debugger is in `stopped` state, keep processing the window loop but skip
  game update/draw (still draw a "paused" overlay), `Sleep(15)`.
- Script-thread -> main-loop communication via a plain `Boolean` (interlocked) —
  hook sets `Stopped` on entry, clears on resume; main loop polls each frame.
- Avoid deadlock: while stopped, do NOT call `TTyroScript.RunQueueObject`
  (Synchronize) from the hook; the socket work is done directly in the hook thread.
- `IsDebug` global already exists in `TyroEngines.pas` (line 32) and is loaded from
  config key `'debug'` (line 277) — gate the whole debugger on it.

### Steps (implementation order)

1. Add `MiniSockets.lpk` to `tyro.lpi` (requires unit `mnClients`, `mnUtils`).
2. New `TyroDebugger.pas`: connection (connect to 127.0.0.1:9000), `init` packet,
  read-command-loop skeleton, NULL-terminated read/write helpers (mirror
  `Stream.ReadUntil(#0, ...)` used by miniEdit).
3. Reply builders: `feature_set`, `breakpoint_set/remove`, `stack_get`,
  `context_get`, `property_get/set`, `property_value`, `eval`, statuses.
   Use minilib `mnXMLUtils`/`mnXMLNodes` or manual string assembly (proto is
   simple enough to hand-build with proper escaping).
4. Lua line hook: install alongside `HookCount` when debugger active; inspect
   `lua_Debug` (`currentline`, `short_src`), match breakpoints by file+line;
   implement step_into/over/out and the blocked command loop.
   Note: `TLuaScript`/`TLuaObject` flow is main-thread `RunString`/callback based;
    hook runs on whichever thread executes Lua.
5. Wire breakpoints table; parse `-f file:///...` and `-n <line>` from editor.
6. Engine freeze: `TTyroMain` polls debugger `Stopped` each frame; skip update
   while stopped; draw debugger overlay; ensure quit path (`WindowShouldClose`)
   still works (stop script + detach socket).
7. Status reporting: on script end send `status="stopping"`; on `detach`/`stop`
   close connection and terminate gracefully (respect `LuaSetTerminated`).
8. Verify with miniEdit: set breakpoint in a Lua script, run Tyro with `debug`
   enabled, step/continue, watches, eval, variable view. Also test
   `miniEdit\source\test\DebugServer` stub for a minimal wire check.

## Key files

- `D:\lab\pascal\tyro\src\LuaAPI.pas`        — Lua debug APIs, `lua_Debug`, `lua_Hook`
- `D:\lab\pascal\tyro\src\LuaClasses.pas`    — `TLua.Init` hook install, `LuaStatus`, `LuaSetTerminated`
- `D:\lab\pascal\tyro\src\TyroEngines.pas`   — `IsDebug` (line 32/277), main loop, script creation (360-389)
- `D:\lab\pascal\tyro\src\TyroScripts.pas`   — `TTyroScriptThread`, `TTyroScript.Start/Run`, `RunQueueObject`
- `D:\lab\pascal\tyro\src\tyro.lpi`          — add `MiniSockets.lpk`; uses `raylib;tyrolib` unit paths
- `D:\lab\pascal\minilib\socket\source\mnClients.pas`    — `TmnClientSocket`
- `D:\lab\pascal\minilib\socket\source\mnSockets.pas`, `mnConnections.pas` — socket framework
- `D:\lab\pascal\minilib\socket\source\mnUtils.pas`      — `FileNameToURI` / `URIToFileName`
- `D:\lab\pascal\minilib\socket\package\MiniSockets.lpk` — package to add to Tyro
- `D:\lab\pascal\miniEdit\source\lib\dbgpServers.pas`    — editor-side DBGp server = protocol contract
- `D:\lab\pascal\miniEdit\source\test\DebugServer\`      — stub debuggee (`mnDBGServers.pas`, `MainForm.pas`, readme)