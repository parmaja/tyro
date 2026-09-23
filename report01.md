# Tyro Engine — Code Audit Report 01

**Date:** 23 Sep 2026
**Scope:** Full review of the Tyro RayLib/Pascal game-engine codebase (src/)
**Method:** Direct review (TyroEngines, TyroScripts, TyroLua, TyroClasses, tyro.lpr, TyroRadio getters) +
three parallel read-only explorer passes (controls/terminal; sounds/melodies/spectrum/radio/physics/aseprites; input/lua-classes/editors/lua-api/sprites).
**Baseline commit:** `9cdc739` (reviewed tree)
**Fix commit:** `8c9d35b` (quick-win batch, see "Fixed" below)
**Build check:** `lazbuild --build-all --build-mode=Debug tyro.lpi` — 25464 lines compiled, tyro.exe linked,
19 warnings / 77 hints / 58 notes (all pre-existing; no new diagnostics from the fix batch).
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
| C6 | Single-frame .aseprite never loads; partial-failure leaks sprite | TyroSprites.pas 1205–1262 | Medium (functional + leak) | 🔴 OPEN |
| C7 | `lua_getextraspace` writes `L-8` backward pointer (custom-allocator context) | LuaAPI.pas 739–744 | High (memory corruption when allocator used) | ✅ FIXED (ca8d2c9) |
| C8 | `AQueueObject.LineNo := ar.currentline` outside `DEBUG_LUA` ifdef → reads uninitialized stack frame | TyroLua.pas 1263–1275 | High (UB in non-debug) | ✅ FIXED |
| F1 | Console can never be focused (`csFocus` missing in terminal Style; `SetFocused` ignores value; ProcessInput early-exit) → kills console typing, `console.read`, F2/F7/F8 | TyroTerminal.pas 253; TyroControls.pas 1153–1157; TyroEngines.pas 1032–1033 | **High (functional dead path)** | ✅ FIXED |
| F2 | `Update` for console & editor is commented out (caret, cursor, scroll dead) | TyroEngines.pas 935–955 | Medium (UX) | 🔴 OPEN |
| F3 | `DoPaintBorder` is a red-frame stub (no real border rendering) | TyroControls.pas 1842–1885 | Low (cosmetic) | 🔴 OPEN |
| F4 | Top-level `SetWindowRect`/resize is clobbered by `Realign` (`alClient` resize) | TyroControls.pas 513–519, 1108–1119 | High (functional) | 🔴 OPEN |
| F5 | `console.show(w, h)` interpreted as pixels, not chars | TyroEngines.pas 1157–1166 | Low (API mismatch) | 🔴 OPEN |
| F6 | `DrawLineTo` applied origin twice | TyroClasses.pas 677–680 | Medium (rendering) | ✅ FIXED |
| F7 | `RunString` leaks `LUA_MULTRET` results on the persistent stack | LuaClasses.pas 716–732 | Medium (leak) | 🔴 OPEN |

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

## 4. Open — Functional

- **F7 → F2 — `Update` / caret dead** (TyroEngines.pas 935–955): console/editor `Update` calls commented out;
  caret never blinks (`FCaretVisible` stays initial), cursor/selection/scroll handling is dormant. Note: input
  itself now works after the F1 fix; only the interactive polish is missing.
- **F4 — resize clobbered by `Realign`** (TyroControls.pas 513–519, 1108–1119): a window-level `SetWindowRect`
  is followed by `Realign`, which re-applies `alClient` and overwrites the requested size.
- **F5 → F6 — `console.show(w, h)` is pixels, not chars** (TyroEngines.pas 1157–1166): `BoundsRect` set from raw
  w/h; should multiply by `CharWidth`/`CharHeight`.
- **F5 — single-frame .aseprite never loads** (TyroSprites.pas 1205–1262): a 1-frame file hits a
  `< 2 frames` rejection / partial-load path; also leaks texture data on failure.
- **F3 — `DoPaintBorder` red-frame stub** (TyroControls.pas 1842–1885): no real border rendering.
- **F7 → F8 — `RunString` stack leak** (LuaClasses.pas 716–732): `LUA_MULTRET` results left on the persistent
  Lua state stack after each run; grows with every script run.

## 5. Open — Threading / design notes (lower priority)

| Note | Location | Why it matters |
|------|----------|----------------|
| `ProcessQueue` not exception-safe | TyroEngines.pas 581–623 | An exception in one queued object aborts the rest of the queue. |
| Margin default overwrite | TyroEngines.pas 723–726 | `margin = 0` config read overwrites a prior default. |
| `FControlCapture` dangle | TyroEngines.pas 994–1005 | Capture pointer not cleared if the control is hidden/freed mid-drag. |
| Hidden-window break | TyroEngines.pas 401–402 | Early-exit path skips input/draw when window hidden — keep in mind for console-only runs. |
| Radio getter thread reads | TyroLua.pas 1660–1713 | Serialized behind the `TCriticalSection` added in ca8d2c9 (same root cause as C2). |
| `luaL_setfuncs` open-array hazard | LuaAPI.pas 967–994 | Passing open arrays when a sentinel is expected. |
| `lua_register_table_index` mis-guard | LuaClasses.pas 205–228 | Guard tests the wrong condition. |
| `LuaAlloc` semantics | LuaClasses.pas 312–320 | Allocator realloc/nil handling differs from C contract. |
| `TLua.Init` `Default(TLua)` hazard | LuaClasses.pas 454–466 | `Default()` before init can zero active state. |
| `TTyroWindow.SetCanvas` leak | TyroControls.pas 1943–1947 | Old canvas not freed on replace. |
| Sprite `UpdateAnims` uncapped catch-up | TyroSprites.pas 523–574 | Large frame deltas advance many frames in one tick — clamp. |
| `SetTexture` self-texture UAF | TyroSprites.pas 608–626 | Re-assigning own texture can free the source. |
| Physics unbounded collision queue | TyroPhysics.pas 372 | No cap on colliding-pair queue (locking itself is correct). |
| Terminal password non-ASCII | TyroTerminal.pas 662, 1196, 1215 | Password chars stored as UTF-8 through single-byte window. |
| `StartRead` doesn't set focus | TyroTerminal.pas 597–607 | Mitigated by F1 fix (callers manage focus) — consider folding in. |
| Witch typo | — | Misspelled identifier/comment (cosmetic). |
| Unit cycles | — | Circular uses between units make refactors harder. |
| Dead code | TyroClasses.pas 872–881 area (report ref; see note) | The only `try/finally` in TyroClasses.pas is legitimate stream cleanup — flagged item could not be reproduced; re-check if a specific block is meant. |
| `TCreateControlObject` cleanup | codex01.md | "Main-thread-safe cleanup of partially created or failed `TCreateControlObject` instances remains unresolved." |

## 6. Suggested next steps

1. ~~**Batch A — memory-safety/crash hardeners:** C2 (radio UAF) → C5 (Aseprites bounds) → C4 (MML overflow) → C7 (extraspace pointer).~~ **DONE — `ca8d2c9`** (this commit; verified clean build, no new warnings, launch smoke test OK).
2. **Batch B — functional gaps:** F8 (RunString leak) → F4 (resize) → F6 (`console.show` chars) → F5 (single-frame aseprite) → F2 (re-enable updates).
3. **Batch C — robustness/design:** `ProcessQueue` exception-safety, `FControlCapture` dangle, `luaL_setfuncs`, LuaClasses guards, sprite/update clamps, `SetCanvas` leak, cleanup (Witch typo, dead code, margin default).

Rebuild + smoke-test after each batch (`lazbuild --build-all --build-mode=Debug tyro.lpi` in `src/`).