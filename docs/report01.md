# Tyro Engine — Code Audit Report 01

**Date:** 23 Sep 2026
**Scope:** Full review of the Tyro RayLib/Pascal game-engine codebase (src/)
**Method:** Direct review (TyroEngines, TyroScripts, TyroLua, TyroClasses, tyro.lpr, TyroRadio getters) +
three parallel read-only explorer passes (controls/terminal; sounds/melodies/spectrum/radio/physics/aseprites; input/lua-classes/editors/lua-api/sprites).
**Baseline commit:** `9cdc739` (reviewed tree)
**Fix commits:** `8c9d35b` (quick-win) · `ca8d2c9` (Batch A) · `130a81b` (Batch B) · `6111d06` (Batch C)
**Build check:** `lazbuild --build-all --build-mode=Debug tyro.lpi` — after `6111d06`: 25705 lines compiled, tyro.exe linked,
19 warnings / 77 hints / 54 notes (baseline 19/77/58; no new diagnostics from any fix batch — two unused locals
and notes retired by the fixes).
**Smoke tests:** launch test (8 s run) plus `basic_drawing.ls --exit` and `console_demo.ls --exit` both exit 0.
**Line numbers** refer to baseline `9cdc739` unless noted.

---

## 1. Priority map

| ID | Finding | Location (baseline) | Severity | Status |
|----|---------|--------------------|----------|--------|
| C1 | REPL echo: RunChunk reads index `1`, pops 2/iter → wrong echo + returns removed | TyroLua.pas 1219–1226 | High (functional) | ✅ FIXED |
| C2 | Radio getters: use-after-free TOCTOU on `FClient` vs `FreeAndNil` in Stop/DoError/StreamEnded | TyroRadio.pas 211–254 | **Critical (crash/UAF)** | ✅ FIXED (ca8d2c9) |
| C3 | Waveform div/mod-by-zero: `SampleRate div round(Frequency)`, `Index mod Round(WaveSamples)`, `Delta := 100/0` | TyroSounds.pas 157–160, 217–220, 490–508 | Critical (crash) | ✅ FIXED |
| C4 | MML: unvalidated tempo `t0`; `q` via note-number math can overflow Int64 | Melodies.pas 355–356, 605–618 | High (crash/overflow) | ✅ FIXED (ca8d2c9) |
| C5 | Aseprites: unbounded palette `SetLength`, unbounded layer-chunk reads, sheet-texture overflow, partial-failure count contract | Aseprites.pas 900–926, 1007–1013, 1536–1542, 1575–1588 | Critical (overflow/OOB reads) | ✅ FIXED (ca8d2c9) |
| C6 | Single-frame .aseprite never loads; partial-failure leaks sprite | TyroSprites.pas 1205–1262 | Medium (functional + leak) | ✅ FIXED (130a81b) |
| C7 | `lua_getextraspace` writes `L-8` backward pointer (custom-allocator context) | LuaAPI.pas 739–744 | High (memory corruption when allocator used) | ✅ FIXED (ca8d2c9) |
| C8 | `AQueueObject.LineNo := ar.currentline` outside `DEBUG_LUA` ifdef → reads uninitialized stack frame | TyroLua.pas 1263–1275 | High (UB in non-debug) | ✅ FIXED |
| F1 | Console can never be focused (`csFocus` missing in terminal Style; `SetFocused` ignores value; ProcessInput early-exit) → kills console typing, `console.read`, F2/F7/F8 | TyroTerminal.pas 253; TyroControls.pas 1153–1157; TyroEngines.pas 1032–1033 | **High (functional dead path)** | ✅ FIXED |
| F2 | `Update` for console & editor is commented out (caret, cursor, scroll dead) | TyroEngines.pas 935–955 | Medium (UX) | ✅ FIXED (130a81b) |
| F3 | `DoPaintBorder` is a red-frame stub (no real border rendering) | TyroControls.pas 1842–1885 | Low (cosmetic) | ✅ FIXED (6111d06) |
| F4 | Top-level `SetWindowRect`/resize is clobbered by `Realign` (`alClient` resize) | TyroControls.pas 513–519, 1108–1119 | High (functional) | ✅ FIXED (130a81b) |
| F5 | `console.show(w, h)` interpreted as pixels, not chars | TyroEngines.pas 1157–1166 | Low (API mismatch) | ✅ FIXED (130a81b) |
| F6 | `DrawLineTo` applied origin twice | TyroClasses.pas 677–680 | Medium (rendering) | ✅ FIXED |
| F7 | `RunString` leaks `LUA_MULTRET` results on the persistent stack | LuaClasses.pas 716–732 | Medium (leak) | ✅ FIXED (130a81b) |

## 2. Fixed in `8c9d35b`

1. **F1 — console focus dead path.** `TTyroTerminal.Style` now includes `csFocus`; `TTyroControl.SetFocused`
   honors its value (True steals focus, False releases only when owned); the global F2/F7/F8/ESC shortcuts run
   before the `FocusedControl = nil` early-exit; `ShowConsole` and `StartConsoleReadEx` (`console.read` from a
   script) now show + focus the console; `HideConsole` releases focus. Console typing, `console.read`, F2, F7 and
   F8 are functional again.
2. **C1 — REPL echo.** `RunChunk` converts each return value at index `i` (was constant `1`) and pops only the
   `luaL_tolstring` copy (was popping 2, deleting a real return value each iteration); the `n` converted values
   are then discarded from the persistent Lua stack (no leak).
3. **C8 — DEBUG_LUA.** `AQueueObject.LineNo` assignment moved inside the `{$ifdef DEBUG_LUA}` guard and the
   `GetStack` success branch.
4. **C3 — waveform guards.** Both `FADE` blocks clamp `round(Frequency) <= 0` and `WaveSamples <= 0` before the
   `div`/`Delta` math; `Sin_Waveform` requires `Round(WaveSamples) > 0` before `Index mod Round(WaveSamples)`.
5. **F6 — DrawLineTo.** Origin-relative coordinates passed through unchanged (DrawLine already adds origin to
   both endpoints).
6. **Cleanup.** Removed duplicated `Console.Visible := False; Console.Focused := True;` pair in the constructor.

## 3. Fixed in `ca8d2c9`

1. **C2 — radio use-after-free race.** `TRadioPlayer` now serializes every `FClient` access behind a
   `TCriticalSection` (`SyncObjs`): `Play` installs the client under the lock, `Stop`/`DoError`/`StreamEnded`
   `Close`+`FreeAndNil` under the lock, and all getters (`Title`/`Station`/`Genre`/`Bitrate`/`Buffered`) read
   under the lock. `Error` and `URL` (refcounted strings written on the main thread, read from script threads)
   now go through locked getters as well, closing the string tear/race. The script-thread-getter vs
   main-thread-lifecycle TOCTOU is gone.
2. **C5 — Aseprite bounds.** Palette chunk `NewSize` (a u32 from the file) is capped at 4096 entries before
   `SetLength`; layer chunks are capped at 1024 so `FLayers` growth is bounded; the raw file size is capped at
   512 MB before it is read in; the sheet-texture loader computes `Width * Frames * Height` in `Int64`, rejects
   > 4096 frames / > 64 M pixels, and nil-checks `MemAlloc` before `FillChar` — oversized/truncated/malicious
   files now fail gracefully (`AFrameCount = 0`, default texture) instead of overflowing or crashing. (The
   partial-failure count contract of `AsepriteLoadFrameTextures` is retained: callers already key off
   `ATextures[i].id`. Raw/compressed cel reads were already bounded by `cMaxDim` + chunk-end checks.)
3. **C4 — MML overflow.** `PlayNote` was refactored: the shared tail is `QueueSound`; the numeric-note path and
   the named-note path both verify the index is within `-255..255` before `Power()`/`floor()` (beyond that the
   result overflows the `Integer` frequency). The `q` command now plays its value as an explicit frequency via
   `PlayNoteFrequency` (1..32000 Hz, rejects ≤ 0) instead of routing it through the note-index math — `q 440`
   is a 440 Hz note rather than a 110 GHz overflow. `t` (tempo) is validated to 1..2000, so `BaseTempo / Tempo`
   can no longer divide by zero (`t0`).
4. **C7 — `lua_getextraspace`.** Rewritten with explicit byte arithmetic: `PByte(L) - LUA_EXTRASPACE`. The old
   `L - LUA_EXTRASPACE` on the `Plua_State` pointer only happened to work because `lua_State` is declared as an
   empty record (`SizeOf = 1`); it would have read/written far before the state block — corrupting
   `global_State` — if that record ever gains a real layout. Behavior is unchanged today; the hazard is removed.

## 4. Fixed in `130a81b`

1. **F7 — `RunString` stack leak.** `TLuaHelper.RunString` captures the persistent-stack depth before
   `luaL_loadstring`/`lua_pcall(LUA_MULTRET)` and `lua_settop` back to it afterwards — every `run`/`dofile`
   used to leave all return values on the Lua stack, growing unboundedly across runs.
2. **F4 — resize clobbered by `Realign`.** `Realign` no longer rewrites `FWindowRect := FBoundsRect` for
   top-level/non-aligned controls, so `ResizeWindow → SetWindowRect` survives (the `else` used to overwrite the
   freshly set rect with stale bounds every resize). The `FBoundsRect → FWindowRect` sync for `alNone` controls
   moved into `SetBoundsRect` (write-time), preserving dragged-window behavior.
3. **F6 — `console.show(w, h)` in characters.** The 4-arg `ShowConsole` installs `CharWidth`/`CharHeight` from
   the current font first, then scales `w`/`h` cells to pixels (`80x25` defaults now mean 80×25 chars, not
   pixels).
4. **F5 — single-frame .aseprite + failure leak.** A 1-frame `.aseprite` is no longer rejected: `TLoadSpriteObject`
   hands its single frame to the static-texture path (slot nilled so rollback frees skip it). Also: the texture is
   unloaded when `SetTexture` refuses it (store never took ownership — old leak), and leftover frames are freed on
   the failure path.
5. **F2 — console/editor `Update` re-enabled.** `TTyroMain.Update` calls `Console.Update` and `Editor.Update`
   again (caret blink, scrollbars, mouse selection) with the original try/except logging preserved.

## 5. Fixed in `6111d06`

1. **`ProcessQueue` exception-safety.** Each queued object runs inside `try/except`: a failing object no longer
   leaks itself, aborts the rest of the queue, or leaves the canvas half-drawn (`Board.EndDraw` always runs);
   failures are logged as `EX-QUEUE` (IsConsole only).
2. **`FControlCapture` dangle.** A control hidden (`F8` console, `F2` editor) or destroyed mid-drag drops the
   capture and falls back to normal hit-testing instead of being dereferenced next frame.
3. **Lua registration stack/guard.** `lua_register_table_{index,method,value}` now share a
   `lua_get_or_create_table` helper: only an existing *table* is reused (the old `= 0` guard treated any
   non-table as present and could `setmetatable` a non-table), and the `nil` that `lua_getglobal` pushes for a
   missing global is never left on the stack (1 slot leaked per created table).
4. **`LuaAlloc` contract.** Explicit C realloc semantics: `nsize = 0` frees and returns nil, otherwise
   ReallocMem; nil signals failure to Lua.
5. **`TLua.Init` re-init.** A record that already owns a Lua state is closed before `Self := Default(TLua)`
   (the Default would otherwise drop the pointer and leak the state + extraspace status).
6. **`luaL_setfuncs`/`luaL_newlib` open-array wrappers.** A `name=nil` sentinel copy is built before
   delegating to the C function (open Pascal arrays have no sentinel; the old `@lr` passed the descriptor and
   could over-read past the last entry); `luaL_newlibtable` prealloc hint counts all entries.
7. **`TTyroWindow.SetCanvas` leak.** The replaced canvas is freed (matches `TTyroControl.SetCanvas`).
8. **Sprite `UpdateAnims` clamp.** Catch-up is capped at one frame per tick, so a stall (pause, resize, lag)
   cannot fast-forward a whole animation; non-looping sprites no longer skip to the end in one tick.
9. **Sprite `SetTexture` self-UAF.** Re-installing the sprite's own texture id is a no-op instead of freeing
   the source first.
10. **F3 — real `DoPaintBorder`.** Replaces the red-frame stub with a solid dark-gray frame in the control's own
    buffer, brightening the hovered/dragged side for `brdSizable`.
11. **Cleanup.** `Witch` → `Which` scroll-parameter typo (TyroControls/TyroTerminal/TyroEditors); margin config
    read default is `cMainMargin` so an absent key can't zero the margin.

## 6. Open — Threading / design notes (lower priority)

| Note | Location | Why it matters |
|------|----------|----------------|
| Hidden-window break | TyroEngines.pas 401–402 | Early-exit path skips input/draw when window hidden — keep in mind for console-only runs. |
| Radio getter thread reads | TyroLua.pas 1660–1713 | Serialized behind the `TCriticalSection` added in ca8d2c9 (same root cause as C2). |
| Physics unbounded collision queue | TyroPhysics.pas 372 | No cap on colliding-pair queue (locking itself is correct). |
| Terminal password non-ASCII | TyroTerminal.pas 662, 1196, 1215 | Password chars stored as UTF-8 through single-byte window. |
| `StartRead` doesn't set focus | TyroTerminal.pas 597–607 | Mitigated by F1 fix (callers manage focus) — consider folding in. |
| Unit cycles | — | Circular uses between units make refactors harder. |
| Dead code | TyroClasses.pas 872–881 area (report ref; see note) | The only `try/finally` in TyroClasses.pas is legitimate stream cleanup — flagged item could not be reproduced; re-check if a specific block is meant. |
| `TCreateControlObject` cleanup | codex01.md | "Main-thread-safe cleanup of partially created or failed `TCreateControlObject` instances remains unresolved." |

## 7. Suggested next steps

1. ~~**Batch A — memory-safety/crash hardeners:** C2 (radio UAF) → C5 (Aseprites bounds) → C4 (MML overflow) → C7 (extraspace pointer).~~ **DONE — `ca8d2c9`** (clean build, no new warnings, launch smoke test OK).
2. ~~**Batch B — functional gaps:** F8 (RunString leak) → F4 (resize) → F6 (`console.show` chars) → F5 (single-frame aseprite) → F2 (re-enable updates).~~ **DONE — `130a81b`** (same verification).
3. ~~**Batch C — robustness/design:** `ProcessQueue` exception-safety, `FControlCapture` dangle, `luaL_setfuncs`, LuaClasses guards, sprite/update clamps, `SetCanvas` leak, F3 `DoPaintBorder`, cleanup (Witch typo, dead code, margin default).~~ **DONE — `6111d06`** (same verification; both `--exit` demo runs pass).
4. **Remaining lower-priority items:** Physics collision-queue cap, terminal password non-ASCII handling, `StartRead` focus folding, unit-cycle refactor, `TCreateControlObject` failed-instance cleanup (codex01.md).

Rebuild + smoke-test after each batch (`lazbuild --build-all --build-mode=Debug tyro.lpi` in `src/`).