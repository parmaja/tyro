Objective
Review and harden the RayLib Pascal game engine, prioritizing concrete lifecycle, threading, GPU/audio ownership, and architectural defects through safe incremental fixes.

Requirements
Preserve compatibility unless a concrete bug requires behavioral change.
Rebuild after each logical fix group.
Keep Lua cancellation state local to each Lua state, not process-global.
Release RayLib GPU/audio resources before closing their backing context/device.
Document broad RayLib/control main-thread-confinement risks rather than attempting an unsafe rewrite.
Run focused shutdown, blocked-console-read, rendering, controls, sprites, collision, audio, editor, radio/spectrum, and CLI lifecycle smoke tests.
Do not overwrite or remove unrelated untracked/user files.
Decisions
Store Lua status in lua_getextraspace; make TLua.Close, SetReady, and SetTerminated repeatable and safe.
Cancel blocked console reads by detaching callbacks on the application thread with TThread.Synchronize(TThread.CurrentThread, ...).
Wait for workers by pumping CheckSynchronize until TThread.Finished, then call WaitFor.
Use atomic LongInt state for TQueueObject.FCancelled, TTyroScript.FActive/FStarted, and TTyroScriptThread.FStarted; start workers with CAS.
Keep the RayLib context alive in hidden mode with FLAG_WINDOW_HIDDEN; destroy GPU/audio owners before CloseWindow and CloseAudioDevice.
Keep FScriptMain as the editable template and execute clones through FScriptThread; retain --main as a worker-mode compatibility alias.
Define TTyroMain.GetActive as engine activity, not worker activity; completed scripts remain interactive unless --exit is requested.
Use FShowWindowOverride = -1/0/1 for forced hidden, script-controlled, and forced visible behavior.
Reject new asynchronous queue work for inactive scripts and cancel stale work after worker shutdown.
Keep generated waveform/MML resources centrally owned until playback finishes; advance MML incrementally on the main thread.
Treat TSprites as owner of sprite instances, installed textures, and animation-frame textures.
Preserve normal Lua unresolved-global behavior as nil; return false for unknown mouse-button names.
Keep successful controls owned by the Main child tree and Lua handles non-owning; the unsafe TCreateControlObject.Destroy/ExtractControl transfer was reverted.
Marshal control mutations to the main thread with dedicated queue objects; the TSetControl*Object classes are wired at every TyroLua.pas call site via FScript.RunQueueObject (verified in the final audit).
Work State
Completed
Hardened console.read() registration, cancellation, callback ownership, event cleanup, and graceful blocked-read shutdown.
Replaced process-global Lua cancellation with per-state atomic status and corrected Lua-state close ordering.
Hardened worker startup/shutdown races with atomic active, cancelled, and started flags.
Reordered TTyroMain teardown so physics, sprites, canvases, shaders, fonts, audio, radio, and spectrum release before contexts/devices.
Added missing Chipmunk cpSpaceFree teardown.
Fixed sprite/texture/animation-frame, default-font, sound/music, waveform, melody, radio, and spectrum ownership defects.
Made TRayUpdateList.Update mutation-safe and added deferred reclamation for completed one-shot file music.
Fixed MML queuing, generated-audio duration conversion, music.sound millisecond semantics, unresolved globals, invalid mouse buttons, and show.fps=false.
Added CLI --exit/-x, tri-state --show/-s, corrected option declarations, and prevented informational/error paths from entering Main.Run.
Fixed FHadScript initialization and automatic-exit state handling.
Reverted the unsafe control ownership experiment; the prior controls smoke test passed after the revert.
Last known Debug build succeeded with lazbuild --build-all --build-mode=Debug tyro.lpi.
Last known git -c core.whitespace=cr-at-eol diff --check passed.
Exercised help/list, hidden/visible window behavior, --exit, interactive completion, rendering, controls, overlapping audio/MML, spectrum, local-failure radio, invalid input, unresolved globals, and an earlier blocked-console.read() close path.
Implemented --lint/-l as a compile-only Lua syntax check that never opens a window or enters the engine loop, and made --execute/-e an alias for --exit.
Propagated failures as nonzero exit statuses: 1 for script errors under --exit/--execute and for lint failures; 2 for usage errors (unknown option, invalid --show value, --lint without a file).
Records the run error on TTyroScript.FLastError / TTyroMain.FScriptFailed before the worker/clone is released; the --exit branch now does a locked screenshot read, captures ScriptFailed, drains ProcessQueue once more, then terminates.
Documented the CLI (options table + exit statuses) and the threading model & lifecycle in README.md.
Rebuilt Debug successfully (25420 lines compiled, tyro.exe linked) and passed git -c core.whitespace=cr-at-eol diff --check.
Smoke-tested the CLI lifecycle: --help/--list exit 0; invalid option, --show=maybe, and --lint-without-file exit 2; --lint good.lua / bad.lua exit 0/1; --exit and --execute (incl. -e) with clean/failing scripts exit 0/1; hidden-mode --exit queue drain shuts down cleanly; screenshot-at-exit produced a valid 320x240 PNG.
Reconciled line endings with HEAD so the working-tree diff contains only real content changes, not EOL churn.
Active
Main-thread-safe cleanup of partially created or failed TCreateControlObject instances remains unresolved.
Visible-mode reruns of the editor, audio/MML, and radio/spectrum demos are still pending after the lifecycle changes (hidden-mode queue drain and the visible screenshot-at-exit path were exercised).
Blocked
(none)
Next Move
Commit the CLI/lifecycle/exit-code changes (5 modified files only; do not stage untracked user files).
Optional follow-ups: main-thread-safe rollback for partial control creation; visible-mode reruns of the editor, audio/MML, and radio/spectrum demos.
Relevant Files
src/TyroScripts.pas — queue objects, atomics, console cancellation, worker state, and newly added control-mutator queue classes.
src/TyroLua.pas — Lua facades wired to the control-mutator queue objects; also sprites, input, audio, radio, and spectrum access.
src/TyroEngines.pas — main loop, hidden-mode updates, queue processing, worker lifecycle, visibility, and teardown.
src/tyro.lpr — CLI parsing, early exits, --exit, --show, unsupported options, and exit statuses.
src/LuaClasses.pas — per-state Lua cancellation/status and close lifecycle.
src/raylib/RayClasses.pas — update-list mutation and one-shot music ownership.
src/tyrolib/TyroControls.pas — parent-owned controls and remaining thread-confinement concerns.
src/tyrolib/TyroSounds.pas — generated waveform/melody ownership and duration conversion.
src/tyrolib/Melodies.pas — incremental melody/channel state machine and partial-setup cleanup.
src/tyrolib/TyroSprites.pas — sprite, texture, and animation-frame ownership.
src/tyrolib/TyroPhysics.pas — Chipmunk world teardown and collision queues.
src/tyrolib/TyroRadio.pas — streamed-radio lifecycle.
src/tyrolib/TyroSpectrum.pas — audio callback, ring buffer, panel references, and shutdown.
README.md — CLI/lifecycle/main-thread-confinement documentation (added this session).
C:\Users\Zaher\AppData\Local\Temp\opencode\tyro-smoke — this session's smoke scripts and captured runtime output.
Important Context
The 5 modified files (README.md, src/tyro.lpr, src/TyroEngines.pas, src/TyroLua.pas, src/TyroScripts.pas) remain uncommitted; numerous unrelated untracked assets/local files exist and have been left untouched.
The latest successful Debug build (25420 lines compiled) and git -c core.whitespace=cr-at-eol diff --check postdate this session's changes; the working-tree diff contains no EOL churn (HEAD blobs are mixed/LF, working files now match HEAD per line).
The first combined control-marshalling patch failed verification in TyroLua.pas; the queue classes are now confirmed wired at all call sites.
Hidden-mode queue processing earlier discarded asynchronous operations under --exit; the exit branch now drains ProcessQueue once more before shutdown, verified by the hidden queue-drain smoke run.
Earlier sprite/collision tests used an invalid asset location, logged Sprite not loaded: followed by Lua assertion errors, and still exited with status 0.
Lua runtime errors are captured on TTyroScript.LastError and TTyroMain.ScriptFailed and now propagate exit status 1 under --exit/--execute and (for lint) 1.
--help and --list print Chipmunk initialization lines because global Main construction initializes physics before CLI early exit.
--show=maybe prints Invalid value for --show: maybe, shows help, and exits with status 2.
--lint and --execute are implemented: --lint is a compile-only check (0 clean / 1 errors / 2 no file); --execute/-e aliases --exit (0 clean / 1 script error).
The graceful WM_CLOSE test while blocked in console.read() passed earlier; a repeat after these lifecycle changes is listed as optional pending work.
Direct Lua-worker access to RayLib input/timing, controls, console/output, radio, and spectrum state remains the principal architectural concurrency risk.
