# Threading model & lifecycle

[<< Tyro docs index](README.md)

Script source runs on a **worker thread**, never on the render/main thread.
Every RayLib call and every control/console/audio mutation a script makes is
packaged as a queue object (`TQueueObject`) and dispatched back to the main
thread, where the main loop drains the queue (`Main.Queue`) once per frame.

- **Main-thread confinement.** Window, canvas, sprites, shaders, fonts, music/
  sound (the audio device), radio, spectrum and the control tree are all
  owned by the main thread and must only be touched there. The Lua facades hide this:
  they enqueue `TCreateControlObject`/`TSetControl*Object`/draw/music objects
  that execute on the main thread.
- **Worker lifecycle.** `Start` publishes an atomic `started` flag, `Stop`
  (also triggered by `--exit`/window close) cancels a pending queue wait,
  signals blocked `console.read()`, then pumps `CheckSynchronize` until the
  thread has actually finished before `WaitFor` and disposal.
- **Frame hand-off (`update`/`draw`).** When the main chunk returns *normally*
  and its globals define `update` and/or `draw`, the finished worker's
  `lua_State` is kept alive and those callbacks are driven once per frame on the
  main thread; only the main thread touches the state from then on. Otherwise —
  no callbacks, or a chunk that never returns — the worker closes the state as
  usual. A later `Stop`/new run releases a handed-off state (see
  [docs/engine-loop.md](engine-loop.md)).
- **Queue admission.** A stopped/inactive script rejects new asynchronous work
  (the request is freed immediately instead). Right after the worker exits the
  engine cancels and clears any stale queue entries so they cannot leak into a
  later interactive run; under `--exit` a final drain runs first so work the
  worker accepted before completing is still honored.
- **Per-state Lua cancellation.** Termination status lives in each Lua state's
  extra space, so cancelling one script can never affect another state.
- **Loaded script vs. worker.** `load`/`F4` fill a template script that owns the
  source; every run (`run`, `F5`, closing the editor) stops the current worker,
  clones that template and starts a new worker on the clone. The worker owns and
  frees its clone, so the template — and the editor buffer saved into it —
  survives a stop and is never written by a running script.
- **Clean slate per run.** Before the clone starts, the run frees the controls the
  finished run left in the window and the sprites it left in the sprite store
  (physics bodies first, since they are keyed by sprite handle), then wipes both
  canvases. The engine controls (console, output, editor, file picker) are kept,
  and keyboard focus and the mouse capture are dropped while the control they point
  at is still alive. Canvas clearing happens inside `BeginDraw`/`EndDraw`, which is
  the only window in which a texture canvas is bound to its render texture.
- **Resource release order.** GPU/audio owners (sprites, canvases, shaders,
  fonts, musicians, generated waveforms, radio, spectrum) are destroyed while
  the window/OpenGL context and audio device are still alive; the context and
  device are closed last, after every owner has been released.

The worker thread reads current input/timing values (`keyboard.down`, `mouse.x`,
`frametime`, ...) and submits drawing/mutation requests through the queue; it
must not hold references into the control tree or RayLib objects across
calls. Scripts blocking on `console.read()` or `sleep()` are cancelled by
`Stop`, so a shut-down never waits on user input.

