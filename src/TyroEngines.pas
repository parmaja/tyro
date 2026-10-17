unit TyroEngines;
{$ifdef FPC}
{$MODE DELPHI} {$H+}
{$endif}
 {**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *}

{
  +---------------------------+-------------------------------------------------+----------------------------------------------------------+
  | Mechanism                 | What It Does to the CPU                         | Effect on Background Threads                             |
  +---------------------------+-------------------------------------------------+----------------------------------------------------------+
  | VSync Block               | Spins or polls the GPU driver until             | Starves threads.                                         |
  |                           | the monitor refreshes.                          | The CPU core stays at 100% utilization.                  |
  +---------------------------+-------------------------------------------------+----------------------------------------------------------+
  | std::this_thread::yield() | Gives up the current CPU time slice             | Good balance. Immediate relief                           |
  |                           | to any thread waiting in line.                  | for other threads with minimal lag.                      |
  +---------------------------+-------------------------------------------------+----------------------------------------------------------+
  | sleep(1) / SetTargetFPS() | Puts the thread into a low-power "Wait" state   | Great for threads. Frees up the CPU completely, but can  |
  |                           | for a set time.                                 | cause minor frame stutter if the sleep overshoots.       |
  +---------------------------+-------------------------------------------------+----------------------------------------------------------+
}

interface

uses
  Classes, SysUtils, SyncObjs, StrUtils,
  mnLogs, mnUtils, mnClasses, mnConfigs,
  RayLib, RayClasses, TyroScripts, TyroSounds,
  TyroClasses, TyroControls, TyroTerminal,
  TyroSprites, TyroTiles, TyroPhysics, TyroEditors, TyroParallax;

//Diagnostics (queue/frame counters and the like) are compiled only when
//'diagnose' is defined, which tracks the debug switch. Debug builds keep them;
//release builds skip the counters and their per-command/per-frame overhead.
//They never affect drawing, and the Lua 'queue' table reports zero for those
//compiled out. See TQueueStats and the FQueue* declarations below.
{$ifopt d+}
{$define diagnose}
{$endif}

const
  TyroVersion: Double = 0.1;
  TyroVersionString = '0.1';

  sPromptChar: UTF8string = '>';
  sPromptDOT: UTF8string = '*';

  cMainMargin = 0;
  cDefaultWindowWidth = 640;
  cDefaultWindowHeight = 480;
  //Default screen shake amplitude in pixels (used when Shake gets no power)
  cShakePower = 10;

var
  // Debug switch: when True, DBG messages are written to the console.
  // Off by default (globals are zero-initialized) to keep the output clean.
  IsDebug: Boolean = False;

type

  { TTyroConsoleLog }

  TTyroConsoleLog = class(TInterfacedPersistent, ILog)
  private
    procedure LogWrite(LogLevel: TLogLevel; S: string);
  public
  end;

  TProcedureObject = procedure(Params: TStrings) of object;

  TConsoleCommand = class(TmnNamedObject)
  public
    Alts: TArray<string>;
    Proc: TProcedureObject;
    Note: string;
    SyncIt: Boolean;
    procedure Execute;
  end;

  TExecuteCommand = class(TObject)
  public
    Command: TConsoleCommand;
    Params: TStrings;
    procedure Execute;
  end;

  { TConsoleCommands }

  TConsoleCommands = class(TmnNamedObjectList<TConsoleCommand>)
  public
    function Add(Sync: Boolean; Name: UTF8String; Alts: TArray<string>; Proc: TProcedureObject; ANote: string = ''): TConsoleCommand; overload;
    function Add(Name: UTF8String; Alts: TArray<string>; Proc: TProcedureObject; ANote: string = ''): TConsoleCommand; overload;
    function Find(const Name: string): TConsoleCommand; overload;
    function Execute(Name: UTF8String; Params: TStrings = nil): Boolean; overload;
  end;

  { TTyroFileList }

  TFilePickEvent = procedure(Sender: TObject; const AFileName: string) of object;

  //The F4 script picker: a list of loadable files (default *.tyro and *.lua)
  //shown centered over the main window. Keyboard: Up/Down/PageUp/PageDown/Home/End
  //select (from TTyroListBox), Enter picks, Escape cancels.
  //Clicking a row selects it; clicking the selected row again picks it.
  TTyroFileList = class(TTyroListBox)
  private
    FDirectory: string;
    FOnPick: TFilePickEvent;
    FOnDismiss: TNotifyEvent;
  protected
  public
    destructor Destroy; override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    //Rebuild the item list from ADirectory using AMasks (one FindFirst each, so
    //several endings can be listed together); returns the number of files found,
    //sorted by name and without duplicates. The file names stored here are later
    //resolved against this directory by SelectedFile.
    function Refresh(const ADirectory: string; const AMasks: array of string): Integer;
    //Fully qualified name of the selected item, '' when nothing is selected.
    function SelectedFile: string;
    property Directory: string read FDirectory;
    //Fired when an item is confirmed (Enter, or a click on the selected row).
    property OnPick: TFilePickEvent read FOnPick write FOnPick;
    //Fired when the user cancels the picker (Escape).
    property OnDismiss: TNotifyEvent read FOnDismiss write FOnDismiss;
  end;

  { TTyroEngine }

  TTyroEngineOption = (moMainWindow, moShowTerminal, moShowFPS);
  TTyroEngineOptions = set of TTyroEngineOption;

  {
    Queue/frame diagnostics for the "black box, then the text" partial-frame
    flicker. A script on the worker thread keeps queueing draw commands while
    the main loop drains them once per frame; when a ProcessQueue pass stops at
    the per-frame time budget with commands still waiting, the frame is drawn
    with only part of the batch the script built, which shows as a black
    rectangle that gets its text a frame later. Read from Lua through the
    'queue' table (queue.consumed, queue.deferred, ...).

    Most of the counters below are filled only when the 'diagnose' conditional
    is defined (debug builds, see the top of the unit): a release build reports
    0 for them. Consumed, Depth, Holds, Group and Limit are always live.
  }
  TQueueStats = record
    //* Commands executed on the main thread since the engine started.
    Consumed: Integer;
    //* ProcessQueue passes that executed at least one command.
    Passes: Integer;
    //* Passes that stopped at the time budget while commands were still
    //* queued: each one drew a partial batch, i.e. a flicker frame.
    Deferred: Integer;
    //* Largest Queue.Count seen at the start of a pass (backlog high-water).
    MaxDepth: Integer;
    //* Commands waiting in the queue right now.
    Depth: Integer;
    //* Open enter() calls (the queue hold depth).
    Holds: Integer;
    //* Drawing cycles presented since the engine started.
    Frames: Integer;
    //* Duration of the most recent drain pass, in milliseconds.
    DrainMs: Double;
    //* True while automatic per-cycle grouping is enabled (the default).
    Group: Boolean;
    //* Milliseconds the script spent between the last two queued commands, and
    //* the largest such gap seen. A big value is the script stalling between
    //* two queue calls (a blocking getter, file I/O, ...) without yielding, so
    //* e.g. a rectangle and the text queued right after it land in the same
    //* group but seconds apart. Reset at every yield (cycle/sleep/read), so
    //* frame waits are not counted.
    LastGap: Integer;
    MaxGap: Integer;
    //* Admission limit on the queue (0 = unlimited). See ThrottleQueue.
    Limit: Integer;
    //* Times a producer had to wait for room at the limit.
    Waits: Integer;
    //* Producers waiting for room right now.
    Blocked: Integer;
  end;

  TConsoleReadEvent = procedure(AConsole: TTyroTerminal; AInput: UTF8String) of object;

  { TTyroMain }

  TTyroEngine = class(TObject)
  private
    FOpaque: Boolean;
    FTitle: string;
    FMargin: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FSizable: Boolean;
    FFPS: Integer;
    FOptions: TTyroEngineOptions;
    FBackColor: TColor;
    //* When True the script's Board canvas is composited ABOVE the sprites
    //* instead of below them, so a script can use the canvas for a HUD/overlay
    //* (see the Lua 'canvas.front'). The board must be cleared to a transparent
    //* colour for the sprites to show through it.
    FBoardOnTop: Boolean;
    //* Signaled by the main loop after every drawing cycle (EndDrawing). The
    //* Lua 'cycle' gate waits on it so "while cycle do" runs at most once per
    //* drawn frame. Auto-reset: an unwaited signal is simply lost.
    FFrameEvent: TEvent;
    //* The control which captured the mouse (e.g. dragging a sizable border).
    //* It keeps receiving MouseMove/MouseUp until the left button is released.
    FControlCapture: TTyroControl;
    function GetActive: Boolean;

    procedure Help_Command(Params: TStrings);
    procedure Dir_Command(Params: TStrings);
    procedure Clear_Command(Params: TStrings);
    procedure Exit_Command(Params: TStrings);
    procedure Load_Command(Params: TStrings);
    procedure Save_Command(Params: TStrings);
    procedure State_Command(Params: TStrings);
    procedure Run_Command(Params: TStrings);
    procedure Stop_Command(Params: TStrings);
    procedure Edit_Command(Params: TStrings);

    procedure EditorClosed(Sender: TObject);
    procedure EditorSave(Sender: TObject);

    procedure SaveEditorSource;
    procedure LoadScriptThread;
    procedure RunScriptThread;
    procedure StopScriptRun;
    procedure StopScriptThread;
    procedure Clear;
    procedure ClearCanvas(ACanvas: TTyroCanvas);
    procedure ClearScriptSprites;
    procedure ClearScriptTiles;
    procedure ShowWindowAfterScript;
    //* Service the command line lifecycle (--exit / --screenshot) once per
    //* frame. Returns True when this call terminated the session, so the main
    //* loop can stop after the current frame.
    function AutoLifecycle: Boolean;

    procedure ShowFileList;
    procedure HideFileList;
    procedure ToggleFileList;
    procedure RefreshFileList;
    procedure FileListPicked(Sender: TObject; const AFileName: string);
    procedure FileListDismissed(Sender: TObject);
  protected
    IsTerminated: Boolean;
    FCanvasLock: TmnCriticalSection;
    Camera2D: TCamera2D;
    procedure Terminate;

  protected
    FPrepared: Boolean; //InitWindow is used
    FQueue: TQueueObjects;
    //* The current worker. It owns the script clone it executes and frees it
    //* with the thread, hence a run is always a fresh clone + a fresh thread.
    FScriptThread: TTyroScriptThread;
    //Command line script, should run in main thread only
    FScriptREPL: TTyroScript;
    //F4 script picker: lists the *.tyro and *.lua files of the current directory;
    //picking
    FFileList: TTyroFileList;
    FReadCallback: TConsoleReadEvent;
    FWaitingQueueObject: TQueueObject; //the queue object a script thread is blocked waiting on
    FQueuedScreenshot: String; //filename requested by Lua screenshot(); captured after the next present
    FPresentedFrame: Boolean;
    FScriptFailed: Boolean;
    //* Command line lifecycle (see AutoLifecycle): --exit ends the session
    //* after FExitAfterSeconds, a bare --exit once the main script thread
    //* finished, and --screenshot captures the screen after
    //* FScreenshotAfterSeconds. Zero/False keeps the interactive behaviour.
    FExitAfterSeconds: Integer;
    FExitAfterScript: Boolean;
    FScreenshotAfterSeconds: Integer;
    FScreenshotFile: string;
    FScreenshotDone: Boolean;
    //* GetTime() (seconds) when the current run started, so the deadlines above
    //* are measured from the run and not from process start.
    FRunStart: Double;
    //Cached width of the '999 FPS' label (moShowFPS), measured once.
    FFpsLabelWidth: Integer;
    //* Set once the engine has shown the window by itself after a headless
    //script finished without asking for one, so it happens a single time.
    FWindowAutoShown: Boolean;
    //* True while the queue/frame statistics panel is drawn in the top-left
    //* corner. Toggled with Ctrl+F10 or through the ShowQueueStats property.
    //* Diagnostics-facing: the counters it reports are only populated in debug
    //* builds, so a release build shows 0 for most of them.
    FShowQueueStats: Boolean;
    //* Lua enter()/leave() queue grouping. FQueueHold counts the open enter()
    //* calls: while it is above zero ProcessQueue stops executing commands, so
    //* a whole group of draw calls is drawn at one drawing cycle instead of
    //* being spread over frames. FQueueFlush is the one-shot flag leave() sets
    //* to let the next ProcessQueue pass drain the held group without the
    //* per-frame time budget. Both are touched from the script thread, so all
    //* access goes through TInterlocked.
    FQueueHold: LongInt;
    FQueueFlush: LongInt;
    //* Commit watermark. FQueueEnqueued counts every command ever added to the
    //* queue (monotonic); FQueueCommitUpTo snapshots it when a hold is armed,
    //* so "commands committed before this hold" = FQueueCommitUpTo commands.
    //* ProcessQueue drains while fewer than that many commands have been drawn
    //* in total (FQueueConsumed), which lets a released group land in one cycle
    //* even though the script re-arms the hold before the next pass. Counters
    //* make this safe against a pass still draining when the next hold is armed.
    //* These three are load-bearing, not diagnostics: they run in release too.
    FQueueEnqueued: LongInt;
    FQueueCommitUpTo: LongInt;
    FQueueConsumed: LongInt;
    //* Automatic per-'cycle' grouping (see AutoGroup). FQueueAuto is the
    //* implicit hold taken when a cycle body starts and released at the next
    //* yield point, so the whole body is committed in one drawing cycle just
    //* like an explicit enter()/leave(). FAutoGroup enables the behaviour.
    //* Touched from the script thread, so access goes through TInterlocked.
    FQueueAuto: LongInt;
    FAutoGroup: Boolean;
    //* Count of active Lua pause() calls. While > 0 the main loop skips the
    //* scene simulation (physics, sprite animation/scripts, controls) so a
    //* paused script keeps showing a still frame; audio and input keep running.
    //* Raised/lowered on the script thread, read on the main thread, so access
    //* goes through TInterlocked.
    FScriptPause: LongInt;
    FRunning: LongInt;
    //* Admission limit on the processing queue: while FQueueLimit > 0 a
    //* producer (AddQueueObject) blocks in ThrottleQueue until the backlog
    //* falls below it, so a script that queues faster than the main loop
    //* drains cannot grow the queue without bound. 0 = unlimited (default).
    //* Set from the script thread (Lua queuelimit), so access goes through
    //* TInterlocked.
    FQueueLimit: Integer;
    //* Auto-reset event set by ProcessQueue after a drain pass (and by
    //* Stop/Clear/SetQueueLimit) so a throttled producer can wake and re-check
    //* the depth instead of sleeping a whole frame.
    FQueueSpaceEvent: TEvent;
    {$ifdef diagnose}
    //* Queue/frame diagnostics backing QueueStats (see TQueueStats). Compiled
    //* only when 'diagnose' is defined (debug builds, see the top of the unit):
    //* they are pure counters with no effect on drawing, so release builds skip
    //* the per-command and per-frame interlocked work that maintains them. The
    //* Lua 'queue' table reports zero for whichever ones are compiled out.
    //* Written on the main thread during ProcessQueue and the drawing cycle,
    //* read on the script thread, so reads go through TInterlocked.
    FQueuePasses: LongInt;
    FQueueDeferred: LongInt;
    FQueueMaxDepth: LongInt;
    FQueueDrainMs: Double;
    FFrameCount: LongInt;
    //* Time (ms) of the last queued command on the script thread, plus the
    //* resulting gaps. A gap is how long the script spent between two
    //* consecutive queue calls without yielding; a 0 stamp means there is no
    //* baseline yet (run start or just after a yield). Written on the script
    //* thread, read on the main thread, so access goes through TInterlocked.
    FQueueGapStamp: Int64;
    FQueueGapLast: LongInt;
    FQueueGapMax: LongInt;
    //* Times a producer had to wait for room at the limit, and how many are
    //* waiting right now (reported through QueueStats only).
    FQueueWaits: LongInt;
    FQueueBlocked: LongInt;
    {$endif}
    //* Screen shake state (see Shake). FShakeTime counts the seconds left,
    //* FShakeDuration the full length of the current shake so the amplitude can
    //* fade out, and FShakeX/FShakeY hold this frame's random offset in pixels.
    FShakeTime, FShakeDuration: Double;
    FShakePower: Integer;
    FShakeX, FShakeY: Integer;
    function GetRunning: Boolean;
    procedure SetRunning(AValue: Boolean);
    function GetShaking: Boolean;
  protected
    Commands: TConsoleCommands;
    //* Free the controls a finished run left in the window. Every run calls this
    //* before it starts the next one; it is protected so a test can drive the
    //* same step without starting a worker.
    procedure SetTitle(AValue: string);
    procedure ClearScriptControls;
    procedure ConsoleInput(AConsole: TTyroTerminal; AInput: UTF8String);
    procedure ExecuteCommand(ACommand: string);
    //If the word typed is not a builtin command it is treated as a one line
    //Lua script run on FScriptREPL's Lua state, so variables assigned in the
    //console survive across lines (and are shared with the main script).
    function RunLuaLine(const ALine: string): Boolean;
    procedure RegisterCommands;
  public
    constructor Create;
    destructor Destroy; override;

    //* TextureMode create texture with canvas
    procedure PrepareWindow(AWidth, AHeight: Integer); overload;
    procedure ShowWindow(AWidth, AHeight: Integer); overload;
    procedure ShowWindow; overload;
    procedure SetFPS(FPS: Integer); virtual;
    procedure HideWindow; virtual;

    //* Resize the window (and canvas) to the given size; canvas is inset by margin + border
    procedure ResizeWindow(AWidth, AHeight: Integer); virtual;

    //* Before Show window
    procedure Load; virtual;
    procedure Run(AOptions: TTyroEngineOptions = [moMainWindow, moShowFPS]);
    procedure Unload; virtual;
    //* After window initialized and other resource, load your resources here
    procedure Start; virtual;
    procedure Update;
    procedure Draw; virtual;

    //* Advance the screen shake by ADeltaTime seconds and pick a new random
    //* offset for this frame (FShakeX/FShakeY). Returns True while the shake is
    //* still running, so the drawing cycle can offset the world camera.
    function UpdateShake(ADeltaTime: Double): Boolean;

    //When application exit, unload your resources
    procedure ProcessQueue;

    //* Snapshot of the queue/frame diagnostics (see TQueueStats). Safe to call
    //* from any thread; backs the Lua 'queue' table.
    function QueueStats: TQueueStats;

    //* Block the calling (script) thread while the queue is at/over its
    //* admission limit, so a fast producer cannot grow it without bound.
    //* Returns at once when the limit is 0, a group is held (enter()/cycle),
    //* the window is hidden/headless, or the engine is stopping. Called by
    //* TTyroScript.AddQueueObject after a command is accepted; the caller must
    //* NOT hold the global Lock here (ProcessQueue needs it to drain).
    procedure ThrottleQueue;
    //* Current admission limit (0 = unlimited).
    function GetQueueLimit: Integer;
    //* Set the admission limit; 0 disables throttling. Raising or clearing it
    //* releases producers already waiting. Safe to call from any thread.
    procedure SetQueueLimit(ALimit: Integer);

    //* Handle files dropped onto the window (from Explorer). Processes .tyro/.lua
    //* to open and run them, and .bdf to load a custom font.
    procedure HandleDroppedFiles;

    function Terminated: Boolean; virtual;
    procedure ProcessInput; virtual;
    //* Draw the queue/frame statistics panel (see FShowQueueStats) in the
    //* window's top-left corner. A no-op when the panel is hidden. Called from
    //* the drawing cycle, after the world and the controls, so it stays on top.
    procedure DrawQueueStats;

    procedure LoadConfig;
    procedure Stop; //and wait
    procedure Shutdown; virtual;

    //* Block the calling script (thread) until the next drawing cycle
    //* (EndDrawing) has completed. Backs the Lua global 'cycle' so scripts can
    //* write "while cycle do" instead of "while true do". Returns False when
    //* AScript was stopped while waiting, termination is polled here because
    //* the Lua debug hook cannot fire while a C function blocks the thread.
    function WaitToNextFrame(AScript: TTyroScript): Boolean;

    //* Register the queue object a script thread is about to block on, so Stop
    //* can signal its event and unblock the thread (e.g. console read()).
    //* Returns False when the engine is stopping: the caller must not block.
    function RegisterWaiting(AQueueObject: TQueueObject): Boolean;
    //* Clear the registration made by RegisterWaiting.
    procedure UnregisterWaiting(AQueueObject: TQueueObject);
    //* Signal the event of the registered waiting queue object (used by Stop).
    procedure CancelWaiting;

    procedure StartConsoleRead;
    procedure StartConsoleReadEx(ACallback: TConsoleReadEvent);
    procedure CancelConsoleRead(AReader: TReadConsoleObject);
    //* Request a screenshot of the next presented frame. Safe to call from any
    //* thread (e.g. the Lua script thread); the file is written right after
    //* EndDrawing on the main thread.
    procedure QueueScreenshot(const AFileName: String);

    //* Shake the world (canvas + sprites) for ATimeMS milliseconds, the jolt of
    //* an accident or an error: every frame the world camera is moved by a
    //* random offset that fades out until the time is up. APower is the maximum
    //* offset in pixels (0 = cShakePower). A new call restarts the shake, so a
    //* longer/harder one simply wins. Safe to call from any thread (e.g. the
    //* Lua script thread); ATimeMS <= 0 stops the shake.
    procedure Shake(ATimeMS: Integer; APower: Integer = 0);
    //* Stop a running shake at once.
    procedure StopShake;
    //* True while a shake is still running.
    property Shaking: Boolean read GetShaking;

    //* Lua enter(): hold the processing queue. Commands keep being queued by
    //* the script, but ProcessQueue stops executing them until ReleaseQueue
    //* balances the hold, so the group is drawn at one drawing cycle. Nested
    //* enter() calls count up; safe to call from any thread.
    procedure HoldQueue;
    //* Snapshot the enqueue watermark for the hold that is opening now.
    //* ProcessQueue draws commands queued before the hold even while it stays
    //* open, so a released group is never stranded behind a hold the script
    //* re-armed before the next pass. Called from the script thread.
    procedure MarkQueueCommit;
    //* Lua leave(): release the hold taken by HoldQueue. When the last hold is
    //* released the next ProcessQueue pass drains the whole held group in one
    //* go (no per-frame time budget). Safe to call from any thread; an
    //* unbalanced leave() (more leaves than enters) is ignored.
    procedure ReleaseQueue;
    //* Release the implicit hold (if any) taken for the current 'cycle' body
    //* and arm the one-shot flush, so the main loop commits what the script has
    //* queued so far. Called at every yield point (cycle, sleep, console read).
    //* Returns True when a hold was actually released, so the caller can take it
    //* again after the yield. Safe to call from any thread.
    function ReleaseAutoQueue: Boolean;
    //* Take the implicit hold that groups the commands of one 'cycle' body.
    //* The next ReleaseAutoQueue commits them in a single drawing cycle. Safe
    //* to call from any thread.
    procedure HoldAutoQueue;
    //* Lua pause(): mark the engine as paused so the main loop freezes the
    //* scene (physics, sprite animation/on_update, controls) while it waits.
    //* The caller makes the matching EndScriptPause call in a finally. Audio
    //* and input are untouched, so music keeps playing and the window stays
    //* responsive. Safe to call from any thread; nested pauses count up.
    procedure BeginScriptPause;
    procedure EndScriptPause;
    //* True while at least one script pause() is active.
    function GetScriptPaused: Boolean;
    //* Record the moment a command was queued (called by TTyroScript.
    //* AddQueueObject on the script thread). Consecutive calls with no yield in
    //* between measure how long the script itself took to produce the next
    //* command, which is what TQueueStats.LastGap/MaxGap report: a blocking
    //* getter between a rectangle() and the text() that follows it shows up
    //* here. Safe to call from any thread.
    procedure NoteEnqueue;
    //* Drop the enqueue baseline so the next NoteEnqueue starts a fresh
    //* measurement. Called at every yield point (see ReleaseAutoQueue). Safe to
    //* call from any thread.
    procedure ResetEnqueueGap;
    //* Enable/disable automatic grouping of each 'while cycle' body. On by
    //* default; turn it off for progressive drawing that streams across frames.
    property AutoGroup: Boolean read FAutoGroup write FAutoGroup;
    //* True while a Lua pause() has frozen the scene (see BeginScriptPause).
    property ScriptPaused: Boolean read GetScriptPaused;
    property Opaque: Boolean read FOpaque write FOpaque;
    //* Draw the Board canvas above the sprites (for a canvas-based HUD) instead
    //* of below them. Backs the Lua 'canvas.front'.
    property BoardOnTop: Boolean read FBoardOnTop write FBoardOnTop;
  public
    ScriptFile: string;//Full file name , that to run in ScriptThread
    ScriptSource: string; //Shout pass to Thread to run it

    //Main for UI controls
    Main: TTyroWindow;
    //Board where Lua drawing on it
    Board: TTyroCanvas;

    Console: TTyroTerminal;
    Output: TTyroOutput;
    Editor: TyroEditor;

    Sprites: TSprites;
    Tiles: TTiles;
    Physics: TPhysics;
    //* Scrolling parallax backgrounds the script created (see TyroParallax).
    Parallaxes: TParallaxes;
    //property Board: TTyroImage read FBoard;
    property Running: Boolean read GetRunning write SetRunning;
    property Active: Boolean read GetActive;
    //* Show/hide the queue/frame statistics panel (also Ctrl+F10). See
    //* FShowQueueStats: the diagnostic counters read 0 outside debug builds.
    property ShowQueueStats: Boolean read FShowQueueStats write FShowQueueStats;

    procedure ShowConsole(AX, AY, AWidth, AHeight: Integer); overload;
    procedure ShowConsole; overload;
    procedure HideConsole;
    procedure ToggleConsole;
    procedure ToggleOutput;
    procedure ShowEditor;
    procedure HideEditor;
    procedure ToggleEditor;

    procedure LogWrite(Msg: string);

    //* Fetch a URL over HTTP/HTTPS and return the response body as text.
    //* The request runs on the calling thread (the script worker), so a script
    //* that calls it blocks its own thread while the render loop keeps drawing.
    //* Returns False and fills AError when the request fails. Backs the Lua
    //* global http_get(url).
    function HttpGet(const AUrl: string; out ABody: string; out AError: string): Boolean;

    property CanvasLock: TmnCriticalSection read FCanvasLock;
    property Options: TTyroEngineOptions read FOptions write FOptions;
    property BackColor: TColor read FBackColor write FBackColor;
    property FPS: Integer read FFPS write SetFPS;
    property Queue: TQueueObjects read FQueue;
    //* Admission limit of the queue (0 = unlimited). See ThrottleQueue.
    property QueueLimit: Integer read GetQueueLimit write SetQueueLimit;
    //* True when the last run-and-exit script finished with a Lua error. Reset
    //* at the start of every run; only meaningful for the --exit/--execute CLI
    //* lifecycle where the main loop inspects it before releasing the worker.
    property ScriptFailed: Boolean read FScriptFailed;

    //* --exit:N: end the session after N seconds of running (0 = never).
    property ExitAfterSeconds: Integer read FExitAfterSeconds write FExitAfterSeconds;
    //* Bare --exit: end the session once the main script thread has finished.
    property ExitAfterScript: Boolean read FExitAfterScript write FExitAfterScript;
    //* --screenshot:N: capture the screen N seconds into the run (0 = never).
    property ScreenshotAfterSeconds: Integer read FScreenshotAfterSeconds write FScreenshotAfterSeconds;
    //* File the timed screenshot is written to; empty picks screenshot.png in
    //* the working path.
    property ScreenshotFile: String read FScreenshotFile write FScreenshotFile;

    property Title: string read FTitle write SetTitle;
    property Margin: Integer read FMargin write FMargin;
    property Width: Integer read FWidth  write FWidth;
    property Height: Integer read FHeight write FHeight;

    property Sizable: Boolean read FSizable write FSizable;
  end;
{
  function IntToFPColor(I: Integer): TFPColor;
  function FPColorToInt(C: TFPColor): Integer;
  function RayColorOf(Color: TFPColor): TRGBAColor;
}
var
  Engine : TTyroEngine = nil;

implementation

uses
  TyroRadio, TyroSpectrum, mnHttpClient, mnModules;

{$IFDEF MMLDEBUG}
var
  FDebugFrameCount: Integer = 0;
{$ENDIF}

{
function IntToFPColor(I: Integer): TFPColor;
begin
  Result.Alpha := I and $ff;
  Result.Alpha := Result.Alpha + (Result.Alpha shl 8);

  I := I shr 8;
  Result.Blue := I and $ff;
  Result.Blue := Result.Blue + (Result.Blue shl 8);
  I := I shr 8;
  Result.Green := I and $ff;
  Result.Green := Result.Green + (Result.Green shl 8);
  I := I shr 8;
  Result.Red := I and $ff;
  Result.Red := Result.Red + (Result.Red shl 8);
end;

function FPColorToInt(C: TFPColor): Integer;
begin
  Result := hi(C.Red);
  Result := Result shl 8;
  Result := Result or hi(C.Green);
  Result := Result shl 8;
  Result := Result or hi(C.Blue);
  Result := Result shl 8;
  Result := Result or hi(C.Alpha);
end;

function RayColorOf(Color: TFPColor): TRGBAColor;
begin
  Result.Alpha := hi(Color.Alpha);
  Result.Red := hi(Color.Red);
  Result.Green := hi(Color.Green);
  Result.Blue := hi(Color.Blue);
end;
}

{ TTyroEngine }

{ Convert a Unicode codepoint to a UTF-8 encoded short string (TUTF8Char) }
function CodePointToUTF8(Ch: Integer): TUTF8Char;
begin
  if Ch < $80 then
    Result := TUTF8Char(Chr(Ch))
  else if Ch < $800 then
    Result := TUTF8Char(Chr($C0 or (Ch shr 6)) + Chr($80 or (Ch and $3F)))
  else if Ch < $10000 then
    Result := TUTF8Char(Chr($E0 or (Ch shr 12)) + Chr($80 or ((Ch shr 6) and $3F)) + Chr($80 or (Ch and $3F)))
  else
    Result := TUTF8Char('');
end;

function TTyroEngine.Terminated: Boolean;
begin
  Result := IsTerminated;
end;

procedure TTyroEngine.SetFPS(FPS: Integer);
begin
  FFPS := FPS;
  SetTargetFPS(FPS);
end;

procedure TTyroEngine.ShowWindow;
begin
  ShowWindow(cDefaultWindowWidth, cDefaultWindowHeight);
end;

procedure TTyroEngine.HideWindow;
begin
  if RayLib.IsWindowReady then
    RayLib.SetWindowState([FLAG_WINDOW_HIDDEN]);
end;

procedure TTyroEngine.Run(AOptions: TTyroEngineOptions);
begin
  PrepareWindow(cDefaultWindowWidth, cDefaultWindowHeight);

  LoadConfig;
  //ShowWindow(ScreenWidth, ScreenHeight); //with option to show window /w
  Running := True;
  Load;
  Res.Load;
  LoadScriptThread;
  Log.WriteLn('WorkPath: ' + Res.WorkPath);
  Options := Options + AOptions;
  Start;
  //The --exit/--screenshot deadlines are measured from now, when the script
  //starts, not from process start.
  FRunStart := GetTime();
  FScreenshotDone := False;
  if moMainWindow in AOptions then
    ShowWindow;
  {if Res.Config.Sections.ReadBool('show', 'console', False) then
  begin
    ShowConsole(0, 0, 0, 0);
    StartConsoleRead;
  end;

  if Res.Config.Sections.ReadBool('show', 'log', False) then
    Output.Show;}
  repeat
    if WindowShouldClose() then
    begin
      Shutdown;
      Terminate;
    end
    else
    begin
      // Queue work and media updates are independent of presentation. Hidden
      // mode still owns a live graphics/audio context and must service accepted
      // script commands before --exit can shut it down.
      ProcessQueue;

      Update;
      RayUpdates.Update;
      //A script file passed on the command line runs headless, so it can finish
      //without ever showing a window. Nothing would be on screen then, so show
      //the window after all and let the session continue.
      ShowWindowAfterScript;

      if not IsWindowHidden and IsWindowReady then
      begin
        if IsWindowHidden then
          break;

        if RayLib.IsWindowResized() then
          ResizeWindow(RayLib.GetScreenWidth(), RayLib.GetScreenHeight());

        RayLib.BeginDrawing;
        if Opaque then
          RayLib.ClearBackground(BackColor);

        try
          Camera2D.Target := Vector2Of(0, 0);
          Camera2D.Offset := Vector2Of(Margin, Margin);
          Camera2D.Zoom := 1;
          Camera2D.Rotation := 0;

          //A running shake (accident/error feedback) jitters the world camera,
          //so the canvas and the sprites move together while the terminal and
          //the other controls stay glued to the window.
          if UpdateShake(RayLib.GetFrameTime()) then
            Camera2D.Offset := Vector2Of(Margin + FShakeX, Margin + FShakeY);

          //Board.BeginDraw;
          BeginMode2D(Camera2D);
          Draw;
          EndMode2D;
          //Board.EndDraw;
//          Main.Canvas.PostDraw;
          Main.Paint;

          if moShowFPS in Options then
          begin
            //The label width never changes ('999 FPS', default font), so pay
            //for MeasureText once instead of every frame.
            if FFpsLabelWidth = 0 then
              FFpsLabelWidth := RayLib.MeasureText('999 FPS', 20) + 5;
            RayLib.DrawFPS(RayLib.GetScreenWidth - FFpsLabelWidth, 5);
          end;

          //Queue/frame statistics panel (Ctrl+F10), on top of the controls.
          if FShowQueueStats then
            DrawQueueStats;
        finally
          RayLib.EndDrawing;
          FPresentedFrame := True;
          {$ifdef diagnose}
          TInterlocked.Increment(FFrameCount);
          {$endif}
          //One drawing cycle completed: wake any script thread blocked on the
          //Lua 'cycle' gate so "while cycle do" runs at most once per frame.
          if FFrameEvent <> nil then
            FFrameEvent.SetEvent;
          //A screenshot requested by Lua screenshot() is captured right after
          //the frame has been presented, so it sees the finished image.
          if FQueuedScreenshot <> '' then
          begin
            Lock.Enter;
            try
              if FQueuedScreenshot <> '' then
              begin
                RayLib.TakeScreenshot(PUTF8Char(UTF8String(FQueuedScreenshot)));
                FQueuedScreenshot := '';
              end;
            finally
              Lock.Leave;
            end;
          end;
        end;
        HandleDroppedFiles;
        ProcessInput;
      end;
      //Command line lifecycle runs after the frame has been processed, so a
      //finishing script's last commands still land before --exit ends the
      //session; a screenshot armed here is captured at the next present.
      AutoLifecycle;
    end;
    CheckSynchronize;
    {$ifdef FPC}
    {$else}
    //YieldProcessor;
    {$endif}
    //Yield;
    //ThreadSwitch;
    //Sleep(1); //Why do i need it here, for Radio thread starting?!!!
  until Terminated;

  Unload;
end;

function TTyroEngine.GetActive: Boolean;
begin
  Result := Running;
end;

function TTyroEngine.GetRunning: Boolean;
begin
  Result := TInterlocked.Add(FRunning, 0) <> 0;
end;

procedure TTyroEngine.SetRunning(AValue: Boolean);
begin
  if AValue then
    TInterlocked.Exchange(FRunning, 1)
  else
    TInterlocked.Exchange(FRunning, 0);
end;

procedure TTyroEngine.CancelWaiting;
begin
  Lock.Enter;
  try
    // Keep the lock while cancelling so the waiter cannot unregister and
    // destroy the object before this call completes.
    if FWaitingQueueObject <> nil then
      FWaitingQueueObject.Cancel;
    FWaitingQueueObject := nil;
  finally
    Lock.Leave;
  end;
end;

procedure TTyroEngine.UnregisterWaiting(AQueueObject: TQueueObject);
begin
  if AQueueObject = nil then
    Exit;
  Lock.Enter;
  try
    if FWaitingQueueObject = AQueueObject then
      FWaitingQueueObject := nil;
  finally
    Lock.Leave;
  end;
end;

function TTyroEngine.RegisterWaiting(AQueueObject: TQueueObject): Boolean;
begin
  Lock.Enter;
  try
    //When the engine is stopping do not register: the caller must not block.
    if Running then
      FWaitingQueueObject := AQueueObject
    else
      FWaitingQueueObject := nil;
    Result := FWaitingQueueObject = AQueueObject;
  finally
    Lock.Leave;
  end;
end;

function TTyroEngine.WaitToNextFrame(AScript: TTyroScript): Boolean;
begin
  Result := True;
  //Reading 'cycle' is the commit point of the previous body: release the
  //implicit hold so the main loop draws everything queued since the last read
  //(also when no window is up, so the hold can never stay stuck).
  ReleaseAutoQueue;
  //No window -> no drawing cycles -> never block the script (behaves like
  //"while true do" so headless scripts do not hang).
  if (FFrameEvent = nil) or (IsWindowHidden) or (not RayLib.IsWindowReady) then
    Exit;
  while True do
  begin
    //While our C function blocks, the Lua count hook cannot run, so Stop()
    //waiting for the script thread would deadlock: poll the script's Active
    //flag (set False by Stop on the main thread) and let the loop end cleanly.
    if not AScript.Active then
    begin
      Result := False; //script was stopped while waiting: exit the cycle loop
      Exit;
    end;
    if FFrameEvent.WaitFor(50) = wrSignaled then
      Break; //the next drawing frame was presented: run one more iteration
  end;
  //Start holding the body that is about to run, so it lands in one cycle.
  HoldAutoQueue;
end;

function TTyroEngine.ReleaseAutoQueue: Boolean;
begin
  //Release unconditionally: if autogroup was just turned off with a body still
  //held, the hold must not stay stuck.
  Result := TInterlocked.Exchange(FQueueAuto, 0) <> 0;
  //Only a released hold needs the flush: it lets the next ProcessQueue pass
  //drain the whole body in one go instead of stopping at the time budget.
  if Result then
    TInterlocked.Exchange(FQueueFlush, 1);
  //The script is about to block/yield: stop measuring enqueue gaps, so the
  //wait itself is not reported as a stall and the next body starts clean.
  ResetEnqueueGap;
end;

procedure TTyroEngine.HoldAutoQueue;
begin
  if FAutoGroup then
    //0 -> 1 closes the gate for the body about to run: snapshot the commands
    //already queued (the previous, released body) so ProcessQueue can still
    //draw them even though this hold stays open for the new body.
    if TInterlocked.Exchange(FQueueAuto, 1) = 0 then
      if TInterlocked.Add(FQueueHold, 0) = 0 then
        MarkQueueCommit;
end;

procedure TTyroEngine.BeginScriptPause;
begin
  TInterlocked.Increment(FScriptPause);
end;

procedure TTyroEngine.EndScriptPause;
begin
  TInterlocked.Decrement(FScriptPause);
end;

function TTyroEngine.GetScriptPaused: Boolean;
begin
  Result := TInterlocked.Add(FScriptPause, 0) > 0;
end;

procedure TTyroEngine.NoteEnqueue;
{$ifdef diagnose}
var
  NowMs, Prev, Diff: Int64;
  Gap: LongInt;
{$endif}
begin
  //Monotonic count of accepted commands, the basis of the commit watermark
  //(FQueueCommitUpTo). Load-bearing, so it always runs.
  TInterlocked.Increment(FQueueEnqueued);
  {$ifdef diagnose}
  //Diagnose-only: time the gap between commands so a blocking getter shows up
  //as queue.lastgap/maxgap. Skipped entirely in release builds.
  NowMs := Round(GetTime() * 1000);
  //Exchange gives the previous stamp and installs the new one in one step; a
  //0/negative previous stamp means there is no baseline yet.
  Prev := TInterlocked.Exchange(FQueueGapStamp, NowMs);
  if Prev <= 0 then
    Exit;
  Diff := NowMs - Prev;
  if Diff < 0 then
    Diff := 0; //clock wrap: ignore
  if Diff > High(LongInt) then
    Gap := High(LongInt)
  else
    Gap := LongInt(Diff);
  TInterlocked.Exchange(FQueueGapLast, Gap);
  if Gap > TInterlocked.Add(FQueueGapMax, 0) then
    TInterlocked.Exchange(FQueueGapMax, Gap);
  {$endif}
end;

procedure TTyroEngine.ResetEnqueueGap;
begin
  {$ifdef diagnose}
  TInterlocked.Exchange(FQueueGapStamp, 0);
  {$endif}
end;

procedure TTyroEngine.QueueScreenshot(const AFileName: String);
begin
  Lock.Enter;
  try
    FQueuedScreenshot := AFileName;
  finally
    Lock.Leave;
  end;
end;

procedure TTyroEngine.Shake(ATimeMS: Integer; APower: Integer);
begin
  if ATimeMS <= 0 then
  begin
    StopShake;
    Exit;
  end;
  Lock.Enter;
  try
    //Called from any thread (script worker), so the state is published under
    //the engine lock and only read by the drawing cycle.
    FShakeTime := ATimeMS / 1000;
    FShakeDuration := FShakeTime;
    if APower > 0 then
      FShakePower := APower
    else
      FShakePower := cShakePower;
  finally
    Lock.Leave;
  end;
end;

procedure TTyroEngine.StopShake;
begin
  Lock.Enter;
  try
    FShakeTime := 0;
    FShakeDuration := 0;
    FShakeX := 0;
    FShakeY := 0;
  finally
    Lock.Leave;
  end;
end;

function TTyroEngine.GetShaking: Boolean;
begin
  Lock.Enter;
  try
    Result := FShakeTime > 0;
  finally
    Lock.Leave;
  end;
end;

procedure TTyroEngine.MarkQueueCommit;
begin
  //Watermark = total commands enqueued so far. ProcessQueue drains while
  //FQueueConsumed stays below it, so it draws exactly the commands that were
  //committed before this hold and never touches the body being built.
  TInterlocked.Exchange(FQueueCommitUpTo, TInterlocked.Add(FQueueEnqueued, 0));
end;

procedure TTyroEngine.HoldQueue;
begin
  //Only the transition from an open gate to a closed one marks a commit
  //boundary: nested enter() calls (e.g. one inside a 'cycle' body) must not
  //move the watermark past the outer hold's commands.
  if TInterlocked.Increment(FQueueHold) = 1 then
    if TInterlocked.Add(FQueueAuto, 0) = 0 then
      MarkQueueCommit;
end;

procedure TTyroEngine.ReleaseQueue;
begin
  if TInterlocked.Decrement(FQueueHold) <= 0 then
  begin
    //Clamp an unbalanced leave() back to zero ...
    TInterlocked.Exchange(FQueueHold, 0);
    //... and arm the one-shot flush: the next ProcessQueue pass drains the
    //whole held group in a single drawing cycle, without stopping at the
    //per-frame time budget the streaming drain uses.
    TInterlocked.Exchange(FQueueFlush, 1);
  end;
end;

procedure TTyroEngine.SetTitle(AValue: string);
begin
  AValue := 'Tyro - ' + AValue;
  RayLib.SetWindowTitle(PUTF8Char(UTF8String(AValue)));
end;

function TTyroEngine.UpdateShake(ADeltaTime: Double): Boolean;
var
  a: Integer;
begin
  Lock.Enter;
  try
    Result := False;
    if FShakeTime <= 0 then
    begin
      FShakeX := 0;
      FShakeY := 0;
      Exit;
    end;
    FShakeTime := FShakeTime - ADeltaTime;
    if FShakeTime <= 0 then
    begin
      //Time is up: leave the world exactly where it started.
      FShakeTime := 0;
      FShakeDuration := 0;
      FShakeX := 0;
      FShakeY := 0;
      Exit;
    end;
    Result := True;
    //Amplitude fades out with the remaining time, so the shake settles instead
    //of stopping dead. Random(a * 2) - a gives an offset in [-a, a).
    a := Round(FShakePower * FShakeTime / FShakeDuration);
    FShakeX := Random(a * 2) - a;
    FShakeY := Random(a * 2) - a;
  finally
    Lock.Leave;
  end;
end;

procedure TTyroEngine.ProcessQueue;
var
  p: TQueueObject;
  fpd: Double;
  ft, ft2: Double;
  flush: Boolean;
  Consumed: Integer;
  Depth: Integer;
  ToDrain: Integer; //>=0: drain exactly this many (committed) commands
  Hold0, Auto0: LongInt;
  {$ifdef diagnose}
  BudgetHit: Boolean;
  {$endif}
begin
  if Board <> nil then
  begin
    //A script that called enter() holds the queue: commands keep piling up,
    //but nothing is drawn until leave() releases the hold. Automatic per-cycle
    //grouping uses the same gate through FQueueAuto. A holder that has already
    //gone (a script that errored or was stopped between enter and leave) is
    //released here so it cannot lock drawing forever; the console REPL has no
    //worker thread, so it is not covered by that self-healing.
    Hold0 := TInterlocked.Add(FQueueHold, 0);
    Auto0 := TInterlocked.Add(FQueueAuto, 0);
    if (Hold0 > 0) or (Auto0 > 0) then
    begin
      if (FScriptThread = nil) or (not FScriptThread.Active) then
      begin
        TInterlocked.Exchange(FQueueHold, 0);
        TInterlocked.Exchange(FQueueAuto, 0);
        Hold0 := 0;
        Auto0 := 0;
      end;
    end;

    if (Hold0 > 0) or (Auto0 > 0) then
    begin
      //A body is being built: draw only the group that was committed before
      //the hold was armed. The script re-arms the hold as soon as the frame
      //event wakes it, which happens before this pass runs, so without the
      //commit watermark the gate is almost always seen closed and nothing is
      //drawn; the backlog then grows without bound and text appears seconds
      //late. Bound the pass by (commit watermark - already drawn).
      ToDrain := TInterlocked.Add(FQueueCommitUpTo, 0) - TInterlocked.Add(FQueueConsumed, 0);
      if ToDrain <= 0 then
        Exit;
    end
    else
    begin
      //Gate open: everything queued is committed, drain it normally.
      ToDrain := -1;
    end;

    //leave() asked for it: this pass ignores the frame time budget, so the
    //whole group queued between enter() and leave() lands in one cycle.
    flush := TInterlocked.Exchange(FQueueFlush, 0) <> 0;
    CanvasLock.Enter;
    try
      //Nothing queued: skip the whole texture-mode switch. Binding and
      //unbinding the render texture (BeginTextureMode/EndTextureMode) twice
      //per frame just to draw nothing costs GL state churn on every cycle.
      Lock.Enter;
      try
        Depth := Queue.Count;
        if Depth > 0 then
          p := Queue.Extract(Queue[0])
        else
          p := nil;
      finally
        Lock.Leave;
      end;

      if p = nil then
        Exit;

      {$ifdef diagnose}
      //Remember the backlog this pass starts with. A high-water mark that keeps
      //growing means the script thread queues commands faster than the main
      //loop drains them (see TQueueStats).
      if Depth > FQueueMaxDepth then
        FQueueMaxDepth := Depth;
      {$endif}

      ft := GetTime();
      fpd := (1 / FPS);
      Consumed := 0;
      {$ifdef diagnose}
      BudgetHit := False;
      {$endif}
      Board.BeginDraw;
      while True do
      begin
        try
          p.Execute;
        except
          on E: Exception do
          begin
            if IsConsole then
              WriteLn('EX-QUEUE: ' + E.ClassName + ': ' + E.Message);
            //A failing queue object must not leak, abort the rest of the queue,
            //or leave the canvas half-drawn (EndDraw below still runs); log and
            //move on to the next queued object.
          end;
        end;
        FreeAndNil(p);
        Inc(Consumed);
        if ToDrain >= 0 then
        begin
          //Committed group: drain exactly that many commands, in one drawing
          //cycle, regardless of the frame time budget.
          if Consumed >= ToDrain then
            Break;
        end
        else
        begin
          ft2 := GetTime() - ft;
          //Stop the pass at the frame time budget unless a flush was armed, and
          //also as soon as a fresh enter() is seen, so a command queued after
          //the hold began is never executed by this already-running pass.
          if (not flush) and (ft2 >= fpd) then
          begin
            //Budget exhausted while more commands may be waiting: the frame is
            //about to be drawn with a partial batch.
            {$ifdef diagnose}
            BudgetHit := True;
            {$endif}
            break;
          end;
          if TInterlocked.Add(FQueueHold, 0) > 0 then
            break;
        end;

        Lock.Enter;
        try
          if Queue.Count > 0 then
            p := Queue.Extract(Queue[0])
          else
            p := nil;
        finally
          Lock.Leave;
        end;
        if p = nil then
          Break;

      end;
      Board.EndDraw;
      //FQueueConsumed is load-bearing (the commit watermark compares against
      //it), so it is always published. Only the main thread writes it; the Lua
      //side reads through TInterlocked (QueueStats).
      TInterlocked.Add(FQueueConsumed, Consumed);
      {$ifdef diagnose}
      if Consumed > 0 then
        TInterlocked.Increment(FQueuePasses);
      FQueueDrainMs := (GetTime() - ft) * 1000;
      Lock.Enter;
      try
        Depth := Queue.Count;
      finally
        Lock.Leave;
      end;
      if BudgetHit and (Depth > 0) then
        TInterlocked.Increment(FQueueDeferred);
      {$endif}
    finally
      CanvasLock.Leave;
      //A drain pass may have made room: wake any producer throttled by
      //FQueueLimit so it can queue the next command without waiting a frame.
      if FQueueSpaceEvent <> nil then
        FQueueSpaceEvent.SetEvent;
    end;
  end;
end;

function TTyroEngine.QueueStats: TQueueStats;
begin
  //Zero first: the diagnose-only counters below stay 0 when compiled out, so
  //the Lua 'queue' table keeps its shape and reports 0 for them.
  Result := Default(TQueueStats);
  Result.Consumed := TInterlocked.Add(FQueueConsumed, 0);
  Result.Holds := TInterlocked.Add(FQueueHold, 0);
  Result.Group := FAutoGroup;
  Result.Limit := TInterlocked.Add(FQueueLimit, 0);
  {$ifdef diagnose}
  Result.Passes := TInterlocked.Add(FQueuePasses, 0);
  Result.Deferred := TInterlocked.Add(FQueueDeferred, 0);
  Result.MaxDepth := TInterlocked.Add(FQueueMaxDepth, 0);
  Result.Frames := TInterlocked.Add(FFrameCount, 0);
  Result.DrainMs := FQueueDrainMs;
  Result.LastGap := TInterlocked.Add(FQueueGapLast, 0);
  Result.MaxGap := TInterlocked.Add(FQueueGapMax, 0);
  Result.Waits := TInterlocked.Add(FQueueWaits, 0);
  Result.Blocked := TInterlocked.Add(FQueueBlocked, 0);
  {$endif}
  Lock.Enter;
  try
    Result.Depth := Queue.Count;
  finally
    Lock.Leave;
  end;
end;

procedure TTyroEngine.DrawQueueStats;
const
  cPad = 8;
  cFont = 16;
  cLine = 17;
var
  Stats: TQueueStats;
  Lines: TArray<string>;
  i: Integer;
  w, maxW, x, y, h: Integer;
begin
  Stats := QueueStats;
  //Two label/value columns per row to keep the panel short enough for a small
  //window; the header row names the panel and its shortcut.
  Lines := [
    Format('%-8s %-6d   %-8s %-6d', ['Consumed', Stats.Consumed, 'Passes', Stats.Passes]),
    Format('%-8s %-6d   %-8s %-6d', ['Deferred', Stats.Deferred, 'Depth', Stats.Depth]),
    Format('%-8s %-6d   %-8s %-6d', ['Max', Stats.MaxDepth, 'Holds', Stats.Holds]),
    Format('%-8s %-6d   %-8s %-6s', ['Frames', Stats.Frames, 'Group', BoolToStr(Stats.Group, True)]),
    Format('%-8s %-6d   %-8s %-6d', ['Limit', Stats.Limit, 'Waits', Stats.Waits]),
    Format('%-8s %-6d   %-8s %-6.2f', ['Blocked', Stats.Blocked, 'Drain ms', Stats.DrainMs]),
    Format('%-8s %-6d   %-8s %-6d', ['LastGap', Stats.LastGap, 'Maxgap', Stats.MaxGap]),
    Format('%-8s %-6d', ['FPS', RayLib.GetFPS])
  ];
  //Size the panel to its widest line, measured with the default font.
  x := cPad;
  y := cPad;
  h := cPad * 2 + Length(Lines) * cLine;
  maxW := 0;
  for i := 0 to High(Lines) do
  begin
    w := Length(Lines[i]) * Res.Font.Width;
    if w > maxW then
      maxW := w;
  end;
  //Translucent backdrop so the text stays readable over any drawing.
  RayLib.DrawRectangle(x - cPad, y - cPad, maxW + cPad * 2, h, clBlack.ReplaceAlpha(200));
  RayLib.DrawRectangleLines(x - cPad, y - cPad, maxW + cPad * 2, h, clGray);

  for i := 0 to High(Lines) do
    RayLib.DrawTextEx(Res.Font.Data, PUTF8Char(UTF8String(Lines[i])), Vector2Of(x, y + i * cLine), Res.Font.Height, 0, clWhite);
end;

function TTyroEngine.GetQueueLimit: Integer;
begin
  Result := TInterlocked.Add(FQueueLimit, 0);
end;

procedure TTyroEngine.SetQueueLimit(ALimit: Integer);
begin
  if ALimit < 0 then
    ALimit := 0;
  TInterlocked.Exchange(FQueueLimit, ALimit);
  //Raising or clearing the limit must release any producer already waiting.
  if FQueueSpaceEvent <> nil then
    FQueueSpaceEvent.SetEvent;
end;

procedure TTyroEngine.ThrottleQueue;
var
  Limit: Integer;
begin
  Limit := TInterlocked.Add(FQueueLimit, 0);
  if Limit <= 0 then
    Exit;
  //A held group (enter()/leave() or the automatic 'cycle' hold) is
  //built before it is drawn: ProcessQueue executes nothing while a hold is
  //open, so waiting here would deadlock the script before it could leave().
  if (TInterlocked.Add(FQueueHold, 0) > 0) or
     (TInterlocked.Add(FQueueAuto, 0) > 0) then
    Exit;
  //No consumer: a hidden/headless window has no drawing cycle, so behave as
  //unlimited instead of hanging the script forever.
  if (FFrameEvent = nil) or IsWindowHidden or (not RayLib.IsWindowReady) then
    Exit;

  {$ifdef diagnose}
  TInterlocked.Increment(FQueueBlocked);
  {$endif}
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
      {$ifdef diagnose}
      TInterlocked.Increment(FQueueWaits);
      {$endif}
      if FQueueSpaceEvent = nil then
        Break;
      //Poll with a short timeout, like WaitToNextFrame: a missed signal costs
      //latency, never a hang, and Stop() ends the loop.
      FQueueSpaceEvent.WaitFor(50);
      //queuelimit() may have raised or cleared the cap while we waited.
      Limit := TInterlocked.Add(FQueueLimit, 0);
      if Limit <= 0 then
        Break;
    end;
  finally
    {$ifdef diagnose}
    TInterlocked.Decrement(FQueueBlocked);
    {$endif}
  end;
end;

procedure TTyroEngine.Start;
begin
  FScriptFailed := False;
  FWindowAutoShown := False;
  RunScriptThread;
end;

{function TTyroEngine.CloneScript(AScript: TTyroScript): TTyroScript;
begin
  Result := nil;
  if AScript = nil then
    Exit;
  Result := TTyroScriptClass(AScript.ClassType).Create;
  Result.Path := AScript.Path;
  Result.FileName := AScript.FileName;
  Result.Source.Assign(AScript.Source);
end;}

procedure TTyroEngine.StopScriptThread;
begin
  if FScriptThread = nil then
    Exit;
  FScriptThread.Terminate;
  CancelWaiting;
  //A suspended thread must enter ThreadProc after Terminate; ThreadProc then
  //The worker can be blocked in TThread.Synchronize. Keep servicing callbacks
  //until it has actually exited; Script.Active changes too early for this.
  while not FScriptThread.Completed do
    CheckSynchronize(10);
  FScriptThread.WaitFor;
  FreeAndNil(FScriptThread);
end;

procedure TTyroEngine.LoadConfig;
var
  aColor: string;
begin
  with Res do
  begin
    FPS := Config.ReadInteger('fps', cFramePerSeconds);
    IsDebug := Config.ReadBool('debug', IsDebug);
    aColor := Config.ReadString('backcolor', '');
    if aColor <> '' then
      BackColor := StrToColor(aColor);

    if Config.Sections.ReadBool('show', 'fps', True) then
      Options := Options + [moShowFPS]
    else
      Options := Options - [moShowFPS];
    if Config.Sections.ReadBool('show', 'window', True) then
      Options := Options + [moShowFPS]
    else
      Options := Options - [moShowFPS];
  end;
end;

procedure TTyroEngine.Shutdown;
begin
  Stop;
end;

constructor TTyroEngine.Create;
begin
  inherited;
  RayLibrary.Load;
  FControlCapture := nil;
  FOptions := [];
  FOpaque := True;
  //Each 'while cycle' body is drawn atomically by default (see AutoGroup).
  FAutoGroup := True;
  Res := TTyroResources.Create;
  FCanvasLock := TmnCriticalSection.Create;
  //Auto-reset, initially clear: the Lua 'cycle' gate waits on it, the main
  //loop signals it after every EndDrawing.
  FFrameEvent := TEvent.Create(nil, False, False, '');
  //Auto-reset, set after every ProcessQueue pass: a producer throttled by
  //FQueueLimit waits on it. Unlimited by default (Lua queuelimit turns it on).
  FQueueSpaceEvent := TEvent.Create(nil, False, False, '');
  FQueueLimit := 0;
  FWidth := ScreenWidth;
  FHeight := ScreenHeight;
  FBackColor := clCornflowerBlue;

  //Configured margin only; absent key must not clobber the default with 0.
  Margin := Res.Config.Sections['window'].ReadInteger('margin', cMainMargin);
  //SetTraceLog(LOG_DEBUG or LOG_INFO or LOG_WARNING);
  SetTraceLogLevel([LOG_ERROR, LOG_FATAL]);
  FQueue := TQueueObjects.Create(True);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  Main := TTyroWindow.Create(nil);
  Main.Name := 'Main';
  Main.Border := brdNone;
  Main.Padding := 0;
  Main.Margin := Margin;
  Main.BoundsRect := Rect(0, 0, Width, Height);
  Main.Show;

  {with TTyroLabel.Create(Main) do
  begin
    Important := True;
    Name := 'Label';
    Caption := 'Label';
    Border:= brdThick;
    BackColor := clBlue;
    BoundsRect := Rect(0, cDefaultWindowHeight-50, 50, cDefaultWindowHeight);
    Show;
  end;}

  Output := TTyroOutput.Create(Main);
  Output.Important := True;
  Output.Name := 'Output';
  Output.BoundsRect := Rect(0, 0, Width, Height);
  Output.Align := alClient;
  Output.Show;

  Console := TTyroTerminal.Create(Main);
  Console.BoundsRect := Rect(0, 0, Width, Height div 2);
  Console.Important := True;
  Console.Border:= brdThick;
  Console.Visible := False;
  Console.OnInput := ConsoleInput;
  Console.Margin:= 5;
  Console.Padding:= 5;
  Console.Align := alBottom;
  Console.Name := 'Console';

  Editor := TyroEditor.Create(Main);
  Editor.Name := 'Editor';
  Editor.Important := True;
  Editor.BoundsRect := Rect(0, 0, 200, 200);
  Editor.Visible := False;
  Editor.OnClose := EditorClosed;
  Editor.Border:= brdThick;
  Editor.Margin:= 5;
  Editor.Padding := 5;
  Editor.Align := alClient;
  Editor.OnSave := EditorSave;

  //F4 script picker; BoundsRect is recentered every time it is shown so it
  //follows window resizes. Hidden until the user presses F4.
  FFileList := TTyroFileList.Create(Main);
  FFileList.Name := 'FileList';
  FFileList.Important := True;
  FFileList.PlaceHolder := 'No Files';
  FFileList.BoundsRect := Rect(0, 0, 360, 280);
  FFileList.Visible := False;
  FFileList.OnPick := FileListPicked;
  FFileList.OnDismiss := FileListDismissed;

  Sprites := TSprites.Create;
  Tiles := TTiles.Create;
  Physics := TPhysics.Create(Sprites);
  Parallaxes := TParallaxes.Create;
  Commands := TConsoleCommands.Create();
  RegisterCommands;
end;

destructor TTyroEngine.Destroy;
begin
  Stop;
  FreeAndNil(FScriptREPL);

  //Detach callbacks and release audio objects while their backing device and
  //the raylib update list are still alive.
  if RadioPlayer <> nil then
    RadioPlayer.Stop;
  if Spectrum <> nil then
    Spectrum.Shutdown;
  ShutdownMelodies;
  ShutdownWaveforms;
  if RayLibSound <> nil then
    RayLibSound.Shutdown;

  //Every object below owns raylib GPU Res. Destroy all of them while
  //the window/OpenGL context is still alive.
  FreeAndNil(Physics);
  FreeAndNil(Sprites);
  FreeAndNil(Tiles);
  //Parallaxes own layer textures: unload them while the GL context is alive.
  FreeAndNil(Parallaxes);
  FreeAndNil(Board);
  FreeAndNil(Res);
  FreeAndNil(FQueue);
  FreeAndNil(Commands);
  FreeAndNil(Main);
  inherited;
  if RayLib.IsWindowReady then
    RayLib.CloseWindow;
  FreeAndNil(FCanvasLock);
  FreeAndNil(FQueueSpaceEvent);
  FreeAndNil(FFrameEvent);
end;

procedure TTyroEngine.PrepareWindow(AWidth, AHeight: Integer);
begin
  if AWidth = 0 then
    raise exception.Create('Screen width can not be 0');
  if AHeight = 0 then
    raise exception.Create('Screen height can not be 0');

  //Do not use FLAG_VSYNC_HINT it block the other threads, or use YieldProcess, TODO
  RayLib.SetConfigFlags([FLAG_WINDOW_HIDDEN, FLAG_WINDOW_RESIZABLE]);
  RayLib.InitWindow(AWidth, AHeight, PUTF8Char(UTF8String(Title)));
  FWidth := AWidth;
  FHeight := AHeight;
  //Board is a render-texture: script draw commands run inside Board.BeginDraw
  //during ProcessQueue and are composited by Board.PostDraw inside the camera.
  Board := TTyroTextureCanvas.Create(AWidth - Margin * 2, AHeight - Margin * 2, True);

  Main.Margin := Margin;
  Main.BoundsRect := Rect(0, 0, AWidth, AHeight);
  //The script draws through Engine.Main.Canvas during ProcessQueue, which runs
  //before the first Paint: make sure the canvas exists (and is sized) now.
  FPrepared := True;
end;

procedure TTyroEngine.Load;
begin
end;

procedure TTyroEngine.Unload;
begin
end;

procedure TTyroEngine.Draw;
var
  W, H: Integer;
begin
  if Board = nil then
    Exit;

  //The parallax backgrounds are the farthest layer, so they go down first and
  //fill the whole drawable area (the same area Board covers).
  W := Board.Width;
  H := Board.Height;
  if Parallaxes <> nil then
    Parallaxes.Draw(W, H);

  //Canvas layer: normally at the bottom (its opaque background must not cover
  //the sprites); when BoardOnTop is set the script uses the canvas as a HUD and
  //it is composited after the sprites instead.
  if not FBoardOnTop then
    Board.PostDraw;
  try
    Sprites.DrawAll;
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EX-DRAW: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
  // scripted on_draw() overlays run last so they stay above the texture
  // and the legacy Board layer
  Sprites.DrawScripts;
  if FBoardOnTop then
    Board.PostDraw;
end;

procedure TTyroEngine.Update;
var
  Scripted: array[0..1023] of TCollisionEvent;
  Enough: Integer;
  I: Integer;
  ev: TCollisionEvent;
  AState: string;
begin
  {$IFDEF MMLDEBUG}
  Inc(FDebugFrameCount);
  if FDebugFrameCount mod 120 = 0 then
    WriteLn(Format('FPSLOG tick=%d frames=%d fps=%d ft=%.3fms',
      [TThread.GetTickCount64, FDebugFrameCount, RayLib.GetFPS, RayLib.GetFrameTime * 1000]));
  {$ENDIF}
  try
    //Audio is never frozen: a pause() must not stall music/radio/waveform
    //playback, which is advanced here and by RayUpdates.Update.
    UpdateMelodies;
    UpdateWaveforms;
    //Lua pause(): freeze the scene. Physics, sprite animation, per-sprite
    //on_update/on_collide and control work are all skipped while a pause is
    //active, so the last drawn frame stays as it is. Only the scene stops
    //changing; the window keeps presenting frames and audio keeps streaming.
    if ScriptPaused then
      Exit;
    if Physics <> nil then
      Physics.Step(RayLib.GetFrameTime());
    // Advance every playing sprite animation (frame textures swap here)
    if Sprites <> nil then
      Sprites.UpdateAnims(RayLib.GetFrameTime());
  except
    on E: Exception do
    begin
      if IsConsole then
      begin
        WriteLn('EX-STEP: ' + E.ClassName + ': ' + E.Message + ' @' + IntToHex(NativeUInt(ExceptAddr), 16));
      end;
      raise;
    end;
  end;
  // Fire per-sprite on_collide handlers (main thread) for events touching scripted sprites
  try
    if Physics <> nil then
    begin
      Physics.SplitScriptedEvents;
      Physics.PollScripted(Scripted, Enough);
      for I := 0 to Enough - 1 do
      begin
        ev := Scripted[I];
        if ev.State = csBegin then
          AState := 'enter'
        else
          AState := 'leave';
        if Sprites.HasScript(ev.HandleA) then
          Sprites.GetScript(ev.HandleA).OnCollide(ev.HandleB, AState);
        if Sprites.HasScript(ev.HandleB) then
          Sprites.GetScript(ev.HandleB).OnCollide(ev.HandleA, AState);
      end;
    end;
    if Sprites <> nil then
      Sprites.UpdateScripts;
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EX-SCRIPT: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
  inherited;

  // Per-frame work of every control (key auto-repeat, caret blink, mouse)
  try
    Main.UpdateControls;
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EX-CONTROL: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
end;


procedure TTyroEngine.ProcessInput;
var
  Shift: TShiftState;
  Key: TKeyboardKey;
  ch: Integer;
  aChar: TUTF8Char;
  aFocused: TTyroControl;
  mp: TVector2;
  mx, my, x, y: Integer;
  i: Integer;
  aControl: TTyroControl;
begin
  // Build shift state from RayLib key queries
  Shift := [];
  if RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT) then
    Shift := Shift + [ssShift];
  if RayLib.IsKeyDown(KEY_LEFT_CONTROL) or RayLib.IsKeyDown(KEY_RIGHT_CONTROL) then
    Shift := Shift + [ssCtrl];
  if RayLib.IsKeyDown(KEY_LEFT_ALT) or RayLib.IsKeyDown(KEY_RIGHT_ALT) then
    Shift := Shift + [ssAlt];
  if RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    Shift := Shift + [ssLeft];
  if RayLib.IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
    Shift := Shift + [ssRight];

  //* Mouse routing: move is sent to the control under the cursor every frame;
  //* once a left press lands inside a control the pointer is captured to it
  //* (keeps tracking the cursor while dragging a sizable border) until release.
  RayLib.SetMouseCursor(MOUSE_CURSOR_DEFAULT);
  mp := RayLib.GetMousePosition;

  mx := Round(mp.X);
  my := Round(mp.Y);

  if FControlCapture <> nil then
  begin
    //The captured control can be hidden (F8 console, F2 editor) or destroyed
    //while the button is still down; a stale pointer must never be dereferenced.
    if (FControlCapture.Parent = nil) or (not FControlCapture.Visible) then
    begin
      //Drop the capture and fall through to normal mouse hit-testing below.
      FControlCapture := nil;
    end
    else
    begin
      x := mx - FControlCapture.WindowRect.Left;
      y := my - FControlCapture.WindowRect.Top;
      FControlCapture.MouseMove(Shift, x, y);
      if not (ssLeft in Shift) then
      begin
        x := mx - FControlCapture.WindowRect.Left;
        y := my - FControlCapture.WindowRect.Top;
        FControlCapture.MouseUp(mbLeft, Shift, x, y);
        FControlCapture := nil;
      end;
    end;
  end
  else
  begin
    for i := Main.Controls.Count - 1 downto 0 do
    begin
      if Main.Controls[i] is TTyroControl then
      begin
        aControl := TTyroControl(Main.Controls[i]).MouseTargetAt(mx, my);
        if aControl <> nil then
        begin
          x := mx - aControl.WindowRect.Left;
          y := my - aControl.WindowRect.Top;
          aControl.MouseMove(Shift, x, y);
          if RayLib.IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
          begin
            aControl.MouseDown(mbLeft, Shift, x, y);
            FControlCapture := aControl;
          end;
          Break;
        end;
      end;
    end;
  end;

  //* Global UI shortcuts work whether or not a control is focused.
  // F5 reruns the loaded script on a new script thread
  if RayLib.IsKeyPressed(KEY_F5) then
    RunScriptThread;
  // F2 toggles the script editor
  if RayLib.IsKeyPressed(KEY_F2) then
    ToggleEditor;
  // F7 toggles the output control
  if RayLib.IsKeyPressed(KEY_F7) then
    ToggleOutput;
  // F8 toggles the console
  if RayLib.IsKeyPressed(KEY_F8) then
    ToggleConsole;
  // F4 toggles the script picker (*.tyro / *.lua): pick a file, then type "run"
  // to execute it in the script thread.
  if RayLib.IsKeyPressed(KEY_F4) then
    ToggleFileList;
  // Ctrl+F10 toggles the queue/frame statistics panel in the top-left corner.
  if (ssCtrl in Shift) and RayLib.IsKeyPressed(KEY_F10) then
    FShowQueueStats := not FShowQueueStats;

  if Main.FocusedControl = nil then
    Exit;

  // Process key codes (function keys, arrows, etc.)
  Key := RayLib.GetKeyPressed;
  while Key <> KEY_NULL do
  begin
    case Key of
      KEY_LEFT_SHIFT, KEY_RIGHT_SHIFT, KEY_LEFT_CONTROL, KEY_RIGHT_CONTROL,
      KEY_LEFT_ALT, KEY_RIGHT_ALT:
        begin
          // Shift keys themselves - skip to avoid sending as regular key
        end;
    else
      if Assigned(Main.FocusedControl) then
        Main.FocusedControl.KeyDown(Key, Shift);
    end;
    Key := RayLib.GetKeyPressed;
  end;

  // Process character input (printable text, respecting modifiers for shortcuts)
  ch := RayLib.GetCharPressed;
  while ch > 0 do
  begin
    if ch >= 32 then
    begin
      // If Ctrl is held, treat as key shortcut (e.g. Ctrl+V) not text
      if not (ssCtrl in Shift) then
      begin
        aFocused := Main.FocusedControl;
        if Assigned(aFocused) then
        begin
          aChar := CodePointToUTF8(ch);
          aFocused.KeyPress(aChar);
        end;
      end;
    end;
    ch := RayLib.GetCharPressed;
  end;
end;

procedure TTyroEngine.ShowWindow(AWidth, AHeight: Integer);
begin
  if AWidth = 0 then
    raise exception.Create('Screen width can not be 0');
  if AHeight = 0 then
    raise exception.Create('Screen height can not be 0');

  if Sizable then
    SetConfigFlags([FLAG_WINDOW_RESIZABLE]);

  RayLib.SetWindowSize(AWidth, AHeight);
  ResizeWindow(AWidth, AHeight);

  ClearWindowState([FLAG_WINDOW_HIDDEN]);
  ShowCursor();
end;

procedure TTyroEngine.ResizeWindow(AWidth, AHeight: Integer);
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  FWidth := AWidth;
  FHeight := AHeight;

  Main.Margin := Margin;
  Main.BoundsRect := Rect(0, 0, AWidth , AHeight);
    //Keep the script canvas in step with the window so a resize does not leave
    //script draw commands hitting a canvas sized for the old window.
  Board.Resize(AWidth - Margin * 2, AHeight - Margin * 2);
end;

procedure TTyroEngine.Stop;
begin
  Running := False;
  //Signal the event of any queue object the script thread is blocked waiting
  //on (e.g. console read()), so the blocked Wait returns and the thread can
  //finish instead of hanging the WaitFor below.
  CancelWaiting;
  StopScriptThread;
  // StopScriptThread can race with a final asynchronous enqueue. Drain only
  // after it has fully exited so no stale command survives into the next run.
  Lock.Enter;
  try
    FQueue.CancelAll;
    FQueue.Clear;
  finally
    Lock.Leave;
  end;
  //A run stopped between enter() and leave() must not leave the hold behind:
  //the next run would find its drawing frozen.
  TInterlocked.Exchange(FQueueHold, 0);
  TInterlocked.Exchange(FQueueAuto, 0);
  TInterlocked.Exchange(FQueueFlush, 0);
  //Keep the commit watermark consistent: with the queue dropped, all commands
  //ever enqueued are also considered drawn, or the leftover difference would
  //make a later hold drain its whole body before it is committed.
  TInterlocked.Exchange(FQueueCommitUpTo, 0);
  TInterlocked.Exchange(FQueueEnqueued, 0);
  TInterlocked.Exchange(FQueueConsumed, 0);
  ResetEnqueueGap;
  //Wake a producer throttled by FQueueLimit so Stop does not wait a poll tick.
  if FQueueSpaceEvent <> nil then
    FQueueSpaceEvent.SetEvent;
end;

procedure TTyroEngine.Terminate;
begin
  Stop;
  HideWindow;
  IsTerminated := True;
end;

//Service the command line lifecycle (--exit / --screenshot) once per frame.
//Returns True when this call terminated the session, so the main loop can stop.
//A screenshot armed on the same frame defers the exit by one frame, so the
//capture still gets presented before the window goes away.
function TTyroEngine.AutoLifecycle: Boolean;
var
  Elapsed: Double;
  Queued: Boolean;
begin
  Result := False;
  Queued := False;
  Elapsed := GetTime() - FRunStart;

  if (FScreenshotAfterSeconds > 0) and (not FScreenshotDone) and (Elapsed >= FScreenshotAfterSeconds) then
  begin
    FScreenshotDone := True;
    if FScreenshotFile = '' then
    begin
      FScreenshotFile := 'screenshot.png';
      if Res.WorkPath <> '' then
        //raylib's TakeScreenshot cannot open an absolute path on Windows, so
        //keep the file relative to the current directory (the working path).
        FScreenshotFile := ExtractRelativePath(IncludeTrailingPathDelimiter(GetCurrentDir), IncludeTrailingPathDelimiter(CorrectPath(Res.WorkPath))) + FScreenshotFile;
    end;
    QueueScreenshot(FScreenshotFile);
    Queued := True;
  end;
  if Queued then
    Exit;

  if (FExitAfterSeconds > 0) and (Elapsed >= FExitAfterSeconds) then
  begin
    Terminate;
    Exit(True);
  end;

  //Bare --exit: Started excludes a thread that was never launched and Completed
  //is published by the worker just before it exits, so this only fires for a
  //script that really ran and finished.
  if FExitAfterScript and (FScriptThread <> nil) and (FScriptThread.Started) and (FScriptThread.Completed) then
  begin
    Terminate;
    Exit(True);
  end;
end;

procedure TTyroEngine.ShowConsole(AX, AY, AWidth, AHeight: Integer);
begin
  Console.BoundsRect := Rect(AX, AY, AX + AWidth, AY + AHeight);
  ShowConsole;
end;

procedure TTyroEngine.ShowConsole;
begin
  Console.CharWidth := Res.Font.Width;
  Console.CharHeight := Res.Font.Height;
  Console.Show;
  Console.BringToFront;
  //Route typed input to the console now that it is visible.
  Console.Focused := True;
  Writeln(Console.WindowRect.Height);
  StartConsoleRead;
end;

procedure TTyroEngine.HideConsole;
begin
  Console.StopRead;
  Console.Hide;
  //Release keyboard focus so input doesn't feed an invisible console.
  Console.Focused := False;
end;

procedure TTyroEngine.ToggleConsole;
begin
  if Console.Visible then
    HideConsole
  else
    ShowConsole;
end;

procedure TTyroEngine.RefreshFileList;
begin
  //List the scripts of the current directory first (same source as the console
  //"list" and "load" commands); fall back to the workspace so F4 still finds
  //demos when the engine was launched without a script from an empty folder.
  FFileList.Refresh(Res.WorkPath, ['*.tyro', '*.lua']);
end;

procedure TTyroEngine.ShowFileList;
var
  LW, LH, W, H: Integer;
begin
  RefreshFileList;
  //Center the picker over the main window, keeping a small margin around it.
  LW := 420;
  LH := 300;
  W := Width;
  H := Height;
  if LW > W - 40 then
    LW := W - 40;
  if LH > H - 40 then
    LH := H - 40;
  FFileList.BoundsRect := Rect((W - LW) div 2, (H - LH) div 2,
                              (W + LW) div 2, (H + LH) div 2);
  FFileList.Show;
  FFileList.Important := True;
  FFileList.BringToFront;
  FFileList.SetFocus;
end;

procedure TTyroEngine.HideFileList;
begin
  FFileList.Hide;
  //Return keyboard focus (and a read prompt) to the console when it is shown.
end;

procedure TTyroEngine.ToggleFileList;
begin
  if FFileList.Visible then
    HideFileList
  else
    ShowFileList;
end;

procedure TTyroEngine.FileListPicked(Sender: TObject; const AFileName: string);
begin
  if (AFileName = '') or not SysUtils.FileExists(AFileName) then
  begin
    Log.WriteLn('Script not found: ' + AFileName);
    Exit;
  end;
  if ScriptTypes.FindByExtension(ExtractFileExt(AFileName)) = nil then
  begin
    Log.WriteLn('Unknown script type for: ' + ExtractFileName(AFileName));
    Exit;
  end;
  //Replace the current template with the picked file: LoadScriptThread stops the
  //worker and swaps FScriptMain, leaving it stopped so the user types "run" to
  //start it (or F2 to edit it first, which reruns it).
  ScriptFile := AFileName;
  try
    LoadScriptThread;
  except
    on E: Exception do
    begin
      Log.WriteLn('Unable to load ' + ExtractFileName(AFileName) + ': ' + E.Message);
      Exit;
    end;
  end;
  HideFileList;
  Log.WriteLn('Loaded: ' + ExtractFileName(AFileName) + '. Type "run" to execute it.');
  if not Console.Visible then
    ShowConsole;
end;

procedure TTyroEngine.FileListDismissed(Sender: TObject);
begin
  HideFileList;
end;

procedure TTyroEngine.ToggleOutput;
begin
  Output.Visible := not Output.Visible;
  if Output.Visible then
    Output.BringToFront;
end;

procedure TTyroEngine.ShowEditor;
begin
  //Edit the template, not the running script: the worker executes a clone, so
  //the source stays safe to change. Stop the run so it is not executing while
  //the source is being edited.
  StopScriptRun;
  Editor.FileName := ScriptFile;
  Editor.LoadSource(ScriptSource);
  Editor.BoundsRect := Rect(0, 0, Width, Height);
  Editor.Margin := 10;
  Editor.BackColor := clBlack;
  Editor.Show;
  Editor.Focused := True;
end;

procedure TTyroEngine.HideEditor;
begin
  if Editor.Visible then
  begin
    //Copy the buffer back first: it is the script source from here on, and it
    //is what the rerun below has to execute.
    SaveEditorSource;
    Editor.Hide;
    //Run the edited source, exactly like F5 does.
    //RunScriptThread;
  end;
end;

procedure TTyroEngine.ToggleEditor;
begin
  if Editor.Visible then
    HideEditor
  else
    ShowEditor;
end;

procedure TTyroEngine.LogWrite(Msg: string);
begin
  Console.Write(Msg);
  //Output.Writeln(Trim(Msg));
end;

function TTyroEngine.HttpGet(const AUrl: string; out ABody: string; out AError: string): Boolean;
var
  Client: TmnHttpClient;
begin
  //One client per call: TmnHttpClient owns a socket, so sharing one instance
  //between the script thread and the main thread would need locking. A fresh
  //instance keeps this call thread-safe and lets the caller block freely.
  Result := False;
  ABody := '';
  AError := '';
  Client := TmnHttpClient.Create;
  try
    try
      //Ask for an uncompressed answer: the client advertises "gzip, deflate"
      //by default but only inflates gzip responses, so a server that picks
      //deflate (open-meteo does) would hand back bytes the JSON parser
      //rejects. ovNo drops the Accept-Encoding header entirely.
      Client.Request.Use.AcceptCompressing := ovNo;
      Client.ConnectTimeout := 10000;
      Client.ReadTimeout := 15000;
      Client.GetString(UTF8String(AUrl), ABody);
      Result := True;
    except
      on E: Exception do
      begin
        ABody := '';
        AError := E.Message;
      end;
    end;
  finally
    Client.Free;
  end;
end;

procedure TTyroEngine.EditorClosed(Sender: TObject);
begin
  HideEditor;
end;

procedure TTyroEngine.EditorSave(Sender: TObject);
begin
  //Straight into the script rather than through SaveEditorSource, which only
  //copies while the editor is showing: what is being written has to be what the
  //editor holds, not what the script held before the editor was opened.
  Editor.SaveSource(ScriptSource);
  if ScriptFile = '' then
  begin
    //Text typed at the console has no file behind it, so there is nowhere to
    //write it. The buffer still reached the script, which is what a run uses.
    Log.WriteLn('The script has no file name, so it was not saved to disk');
    Exit;
  end;
  try
    SaveFileUTF8String(ScriptFile, ScriptSource);
    Log.WriteLn('Saved ' + ScriptFile);
  except
    on E: Exception do
      Log.WriteLn('Could not save ' + ScriptFile + ': ' + E.Message);
  end;
end;

procedure TTyroEngine.Edit_Command(Params: TStrings);
begin
  ShowEditor;
end;

procedure TTyroEngine.ConsoleInput(AConsole: TTyroTerminal; AInput: UTF8String);
begin
  // The terminal echoes the submitted command line itself, so only execute it
  if Assigned(FReadCallback) then
  begin
    FReadCallback(AConsole, AInput);
    Exit;
  end;

  ExecuteCommand(AInput);
  // Re-arm the console for the next line of input
  if Console.Visible then
    StartConsoleRead;
end;

procedure TTyroEngine.ExecuteCommand(ACommand: string);
var
  Params: TStringList;
  OriginalLine: string;
begin
  ACommand := Trim(ACommand);
  if ACommand <> '' then
  begin
    OriginalLine := ACommand;
    Params := TStringList.Create;
    ParseArguments(ACommand, Params, ['-', '/']);
    if (Params.Count>0) then
    try
      ACommand := Params[0];
      Params.Delete(0);
//          Write(#8);
      if not Commands.Execute(ACommand, Params) then
      begin
        if not RunLuaLine(OriginalLine) then
        begin
          Console.Writeln('Unknown command: ' + ACommand);
          Console.Writeln('Type "help" for available commands.');
          Console.Writeln('');
        end;
      end;
    finally
      FreeAndNil(Params);
    end;
  end;
end;

//Load ScriptFile into the editable template. The worker is not created here:
//RunScriptThread clones the template for every run.
procedure TTyroEngine.LoadScriptThread;
var
  aScriptType: TScriptType;
begin
  if ScriptFile = '' then
    exit;

  if EndsDelimiter(ScriptFile) and SysUtils.DirectoryExists(ScriptFile) then
  begin
    Res.WorkPath := ScriptFile;
    ScriptFile := '';
  end
  else
  begin
    Title := ScriptFile;
    aScriptType := ScriptTypes.FindByExtension(ExtractFileExt(ScriptFile));
    if (aScriptType <> nil) and SysUtils.FileExists(ScriptFile) then
    begin
      Log.WriteLn('File: ' + ScriptFile);
      if LeftStr(ScriptFile, 1) = '.' then
        ScriptFile := ExpandFileName(Res.WorkPath + ScriptFile);
      Res.WorkPath := ExtractFilePath(ScriptFile);
      Title := ScriptFile;

      ScriptSource := mnUtils.LoadFileString(ScriptFile);
    end
    else
      Log.WriteLn('Type of file not found: ' + ScriptFile);
  end;
end;

//(Re)start the loaded script: stop whatever is running, then hand a fresh clone
//of the template to a new worker. A thread can only be started once and it owns
//the script it runs, so a rerun is always a new thread (F5, "run", and hiding the
//editor all come through here).
procedure TTyroEngine.RunScriptThread;
var
  aScriptType: TScriptType;
  aScript: TTyroScript;
begin
  //With the editor open its buffer is the current source: take it over first so
  //a rerun never executes a stale template.
//  SaveEditorSource;
  StopScriptRun;
  //The finished run left the window full of its own controls, a sprite store full
  //of its own sprites and canvases full of its own pixels. Drop all three, or the
  //new script inherits them.
  ClearScriptControls;
  ClearScriptSprites;
  ClearScriptTiles;
  Clear;
  //A song the finished run left playing belongs to that run, not to the next one.
  aScriptType := ScriptTypes.FindByExtension(ExtractFileExt(ScriptFile));
  if (aScriptType <> nil) then
  begin
    aScript := aScriptType.ScriptClass.Create(ScriptSource);
    {$ifdef diagnose}
    //Fresh enqueue-gap measurement for this run (queue.maxgap/lastgap).
    TInterlocked.Exchange(FQueueGapStamp, 0);
    TInterlocked.Exchange(FQueueGapLast, 0);
    TInterlocked.Exchange(FQueueGapMax, 0);
    {$endif}
    //Group the script's initial drawing too, so even a body it builds before
    //its first cycle/sleep/read is committed atomically; the hold is released
    //at the first yield or when the run stops.
    HoldAutoQueue;
    FScriptThread := TTyroScriptThread.Create(aScript);
    FScriptThread.Start;
  end
  else
    Log.WriteLn('Type of file not found: ' + ScriptFile);
end;

//Free the controls the previous run created, so a rerun does not draw the old UI
//under the new one. Only the controls the engine owns (console, output, editor,
//file picker) are kept: they are created once and hold engine state, not script
//state, while everything else parented to the window came from the script.
procedure TTyroEngine.ClearScriptControls;
var
  i: Integer;
begin
  //Keyboard focus and the mouse capture can point at a control that is about to
  //disappear. SetFocusedControl calls FocusChanged on the control it replaces, so
  //the focus has to go while the old control is still alive. An engine control is
  //staying, and so keeps both.
  if (Main.FocusedControl <> nil) and not Main.FocusedControl.Important then
    Main.FocusedControl := nil;
  if (FControlCapture <> nil) and not FControlCapture.Important then
    FControlCapture := nil;
  //A freed control removes itself from Controls (TTyroControl.Destroy clears its
  //parent), so walking backwards keeps the remaining indexes valid.
  for i := Main.Controls.Count - 1 downto 0 do
    if not Main.Controls[i].Important then
      Main.Controls[i].Free;
end;

//Free the sprites the previous run created, so a rerun does not keep animating and
//colliding with them. Physics goes first: its bodies are keyed by the sprite
//handles that are about to disappear. The store hands out fresh handles, so the
//new run starts from an empty one.
procedure TTyroEngine.ClearScriptSprites;
begin
  if Physics <> nil then
    Physics.RemoveAll;
  if Sprites <> nil then
    Sprites.Clear;
end;

//Free the tilemaps the previous run created and unload their tileset textures.
//The store hands out fresh handles, so the new run starts from an empty one.
procedure TTyroEngine.ClearScriptTiles;
begin
  if Tiles <> nil then
    Tiles.Clear;
end;

//Wipe the pixels the previous run left on the two canvases it draws through:
//Board, the texture the script's drawing queue fills, and Canvas, the window
//canvas it is blitted to. Both keep their content between frames, so without this
//the new script starts on top of the old one.
procedure TTyroEngine.Clear;
begin
  //Same lock the queue processing takes while it draws into Board.
  CanvasLock.Enter;
  try
    ClearCanvas(Board);
    ClearCanvas(Main.Canvas);
    //Drop the finished run's parallax backgrounds and their textures too.
    if Parallaxes <> nil then
      Parallaxes.Clear;
  finally
    CanvasLock.Leave;
  end;
end;

//Clear one canvas in place. A texture canvas is only bound to its render texture
//between BeginDraw and EndDraw, so the clear has to happen inside that pair:
//outside it ClearBackground would wipe the window backbuffer and leave the canvas
//content on screen.
procedure TTyroEngine.ClearCanvas(ACanvas: TTyroCanvas);
begin
  if ACanvas = nil then
    Exit;
  ACanvas.BeginDraw;
  try
    ACanvas.Clear;
  finally
    ACanvas.EndDraw;
  end;
end;

//Copy the editor buffer back into the script it edits. The worker runs a clone,
//so this is the only place where the edited source reaches the template.
procedure TTyroEngine.SaveEditorSource;
begin
  Editor.SaveSource(ScriptSource);
end;

//Stop the running worker and drop the main-thread work it may still have queued,
//so a following run starts from a clean slate.
procedure TTyroEngine.StopScriptRun;
begin
  CancelWaiting;
  StopScriptThread;
  Lock.Enter;
  try
    FQueue.CancelAll;
    FQueue.Clear;
  finally
    Lock.Leave;
  end;
  //A rerun must not inherit a hold the stopped run never balanced with leave().
  TInterlocked.Exchange(FQueueHold, 0);
  TInterlocked.Exchange(FQueueAuto, 0);
  TInterlocked.Exchange(FQueueFlush, 0);
  //Keep the commit watermark consistent: with the queue dropped, all commands
  //ever enqueued are also considered drawn, or the leftover difference would
  //make a later hold drain its whole body before it is committed.
  TInterlocked.Exchange(FQueueCommitUpTo, 0);
  TInterlocked.Exchange(FQueueEnqueued, 0);
  TInterlocked.Exchange(FQueueConsumed, 0);
  ResetEnqueueGap;
  //Wake a producer throttled by FQueueLimit so a rerun starts without a stall.
  if FQueueSpaceEvent <> nil then
    FQueueSpaceEvent.SetEvent;
end;

//Called from the main loop while the window is still hidden. A script file given
//on the command line runs without moMainWindow, so it is free to finish without
//showing anything; if it does, nothing is on screen and the process just sits
//there. Show the window after all so the session stays usable.
procedure TTyroEngine.HandleDroppedFiles;
var
  DropList: TFilePathList;
  i: Integer;
  FilePath: string;
  Ext: string;
begin
  if not RayLib.IsWindowReady then
    Exit;
  if not RayLib.IsFileDropped then
    Exit;
  DropList := RayLib.LoadDroppedFiles;
  if DropList.count <= 0 then
  begin
    RayLib.UnloadDroppedFiles(DropList);
    Exit;
  end;
  try
    for i := 0 to DropList.count - 1 do
    begin
      // TFilePathList.paths is PPUTF8Char (array of PUTF8Char)
      FilePath := UTF8String(DropList.paths[i]);
      if FilePath = '' then
        Continue;
      Ext := LowerCase(ExtractFileExt(FilePath));
      if (Ext = '.tyro') or (Ext = '.lua') then
      begin
        // Stop any running script and prepare to run the dropped script
        StopScriptRun;
        ScriptFile := FilePath;
        try
          LoadScriptThread;
          RunScriptThread;
          // Ensure window is shown when running a dropped script
          if IsWindowHidden then
            ShowWindow;
        except
          on E: Exception do
          begin
            if Console <> nil then
              Console.Writeln('Unable to open dropped script: ' + E.Message);
            Log.WriteLn('Unable to open dropped script: ' + FilePath + ' - ' + E.Message);
          end;
        end;
        Break; // Only handle one script drop at a time
      end
      else if (Ext = '.bdf') then
      begin
        // Try to load font from BDF file
        try
          Res.Font.LoadFromFile(FilePath, 0);
          // Update UI components with new font if available
          if (Console <> nil) then
          begin
            Console.CharWidth := Res.Font.Width;
            Console.CharHeight := Res.Font.Height;
          end;
          Log.WriteLn('Loaded font: ' + FilePath);
        except
          on E: Exception do
          begin
            if Console <> nil then
              Console.Writeln('Unable to load font: ' + FilePath + ' - ' + E.Message);
            Log.WriteLn('Unable to load font: ' + FilePath + ' - ' + E.Message);
          end;
        end;
        Break;
      end;
    end;
  finally
    RayLib.UnloadDroppedFiles(DropList);
  end;
end;

procedure TTyroEngine.ShowWindowAfterScript;
begin
  //Bare --exit ends the session as soon as the script is done, so showing the
  //window here would only flash it for the one remaining frame.
  if FWindowAutoShown or (not IsWindowHidden) or IsTerminated or FExitAfterScript then
    Exit;
  //Only a real script run can end on its own: Started excludes a thread that was
  //never launched and Completed is published by the worker just before it exits.
  if (FScriptThread = nil) or (not FScriptThread.Started) or (not FScriptThread.Completed) then
    Exit;
  FWindowAutoShown := True;
  Log.WriteLn('Script finished without showing a window, showing it now');
  ShowWindow;
end;

//Treat an unknown console line as a one line Lua script run on the main
//script's Lua state (FScriptREPL). A fresh Lua state is created on demand so
//variables assigned in the console (e.g. x = 42) persist between lines.
function TTyroEngine.RunLuaLine(const ALine: string): Boolean;
var
  aScriptType: TScriptType;
  Output: string;
begin
  Result := False;
  Output := '';
  if FScriptREPL = nil then
  begin
    aScriptType := ScriptTypes.FindByExtension('.tyro');
    if aScriptType = nil then
      aScriptType := ScriptTypes.FindByExtension('.lua');
    if aScriptType = nil then
    begin
      Console.Writeln('No Lua environment available. Use "load <script>" first.');
      Exit(True);
    end;
    FScriptREPL := aScriptType.ScriptClass.Create('');
  end;

  if FScriptREPL.RunLine(ALine, Output) then
    Result := True
  else if Output <> '' then
  begin
    Console.Writeln(Output);
    Console.Writeln('');
    Result := True; //handled: it was the Lua line that failed
  end;
end;

procedure TTyroEngine.RegisterCommands;
var
  aCommand: TConsoleCommand;
begin
  Commands.Add('help', ['?'], Help_Command, 'Show help');
  Commands.Add('list', ['ls'], Dir_Command, 'Show current directory');
  Commands.Add('clear', ['cls'], Clear_Command, 'List files in current directory');
  Commands.Add('exit', ['quit', 'q'], Exit_Command, 'Hide console and stop');
  Commands.Add('stop', [], Stop_Command, 'Stop current script');
  Commands.Add('load', [], Load_Command, 'Load script name from current directory (F4 to pick)');
  Commands.Add('save', [], Save_Command, 'Save script name to current directory');
  Commands.Add('state', [], State_Command, 'State of current directory');
  Commands.Add('run', [], Run_Command, 'Run current loaded script');
  Commands.Add('edit', [], Edit_Command, 'Edit the current loaded script (F2)');
  Console.CommandNames.Clear;
  for aCommand in Commands do
  begin
    Console.CommandNames.Add(aCommand.Name);
    if aCommand.Alts <> nil then
      Console.CommandNames.AddStrings(aCommand.Alts);
  end;
end;

procedure TTyroEngine.StartConsoleRead;
begin
  // Clear any script callback so built-in commands are executed
  FReadCallback := nil;
  Console.OnInput := ConsoleInput;
  Console.StartRead(sPromptChar);
end;

procedure TTyroEngine.StartConsoleReadEx(ACallback: TConsoleReadEvent);
begin
  //console.read() from a script: make sure the console is visible and focused
  //so the user can actually type a reply. (Do not call StartConsoleRead here;
  //it would clear the callback we are installing.)
  if not Console.Visible then
  begin
    Console.CharWidth := Res.Font.Width;
    Console.CharHeight := Res.Font.Height;
    Console.Show;
  end;
  Console.Focused := True;
  FReadCallback := ACallback;
  Console.OnInput := ACallback;
  Console.StartRead(sPromptChar);
end;

procedure TTyroEngine.CancelConsoleRead(AReader: TReadConsoleObject);
begin
  // Only detach this reader when it still owns the terminal callback. This
  // avoids an old reader cancelling a newer read request.
  if (AReader <> nil) and (TMethod(FReadCallback).Data = AReader) then
  begin
    Console.StopRead;
    StartConsoleRead;
  end;
end;

procedure TTyroEngine.Help_Command(Params: TStrings);
var
  Command: TConsoleCommand;
begin
  Console.Writeln('Available commands:');
  for Command in Commands do
    if Command.Note <> '' then
      Console.Writeln(' '+sPromptDOT+' '+ Command.Name + ' : '+ Command.Note)
    else
      Console.Writeln(' '+sPromptDOT+' '+ Command.Name);
  Console.Writeln('');
end;

procedure TTyroEngine.Dir_Command(Params: TStrings);
var
  DirPath: string;
  sr: TSearchRec;
  aFile: string;
begin
  Console.Writeln('Directory: ' + Res.WorkPath);
  DirPath := ExcludeTrailingPathDelimiter(Res.WorkPath);
  if Params.Count > 0 then
    aFile := Params[0]
  else
      aFile := '*.*';
  if FindFirst(DirPath + PathDelim + aFile, faAnyFile, sr) = 0 then
  begin
    try
      repeat
        Console.Writeln('  ' + sr.Name);
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end
  else
  begin
    Console.Writeln('  (empty)');
  end;
  Console.Writeln('');
end;

procedure TTyroEngine.Clear_Command(Params: TStrings);
begin
  Console.Clear;
end;

procedure TTyroEngine.Exit_Command(Params: TStrings);
begin
  HideConsole;
  Terminate;
end;

procedure TTyroEngine.Run_Command(Params: TStrings);
begin
  RunScriptThread;
end;

procedure TTyroEngine.Stop_Command(Params: TStrings);
begin
  StopScriptRun;
end;

procedure TTyroEngine.Load_Command(Params: TStrings);
var
  aFile: string;
begin
  if (Params.Count = 0) then
  begin
    Console.Writeln('Usage: load <script_name>');
    Exit;
  end;

  aFile := Params[0];
  aFile := IncludePathDelimiter(Res.WorkPath) + aFile;

  if (ExtractFileExt(aFile) = '') and not SysUtils.FileExists(aFile) then
    aFile := aFile + '.tyro';

  if SysUtils.FileExists(aFile) then
  begin
    ScriptFile := aFile;
    try
      LoadScriptThread;
    except
      on E: Exception do
        Console.Writeln('Unable to load ' + aFile + ': ' + E.Message);
    end;
  end
  else
  begin
    Console.Writeln('Script not found: ' + aFile);
    Exit;
  end;
end;

procedure TTyroEngine.Save_Command(Params: TStrings);
var
  aFile, aFileName: string;
  aConfirm: Boolean;
begin
  if (Params.Count = 0) then
  begin
    Console.Writeln('Usage: save <script_name>, [yes]');
    Exit;
  end;

  aFile := Params[0];

  if (ExtractFileExt(aFile) = '') then
    aFile := aFile + '.tyro';

  aFileName := IncludePathDelimiter(Res.WorkPath) + aFile;

  if Params.Count > 1 then
    aConfirm := IsStrInArray(Params[1], ['1', 'yes', 'true', 'ok'])
  else
    aConfirm := False;
  //"load demo" loads demo.tyro: a name without an extension that does not exist
  //on its own is taken as a script. An explicit extension is never second-guessed.

  if SysUtils.FileExists(aFileName) and not aConfirm then
    Console.Writeln('Unable to save, file is exits, add yes after file name ' + aFile)
  else
  begin
    ScriptFile := aFileName;
    SaveFileUTF8String(aFileName, ScriptSource);
    Console.Writeln('File saved to ' + aFile);

    LoadScriptThread;
  end;
end;

procedure TTyroEngine.State_Command(Params: TStrings);
begin
  if not Active then
    Exit;
  if (FScriptThread <> nil) and FScriptThread.Active then
    Console.Writeln(ScriptFile + ' is running')
  else
    Console.Writeln(ScriptFile + ' is loaded');
end;

{ TTyroFileList }

function TTyroFileList.Refresh(const ADirectory: string; const AMasks: array of string): Integer;
var
  sr: TSearchRec;
  DirPath: string;
  Temp: TStringList;
  m, i: Integer;
begin
  Clear;
  FDirectory := ExcludeTrailingPathDelimiter(ADirectory);
  Temp := TStringList.Create;
  try
    Temp.Sorted := True;
    Temp.Duplicates := dupIgnore;
    DirPath := FDirectory;
    if DirPath <> '' then
    begin
      for m := 0 to High(AMasks) do
        if AMasks[m] <> '' then
          if FindFirst(DirPath + PathDelim + AMasks[m], faAnyFile, sr) = 0 then
          begin
            try
              repeat
                if (sr.Attr and faDirectory) = 0 then
                  Temp.Add(sr.Name);
              until FindNext(sr) <> 0;
            finally
              FindClose(sr);
            end;
          end;
    end;
    for i := 0 to Temp.Count - 1 do
      AddItem(Temp[i]);
  finally
    Temp.Free;
  end;
  Result := Items.Count;
end;

function TTyroFileList.SelectedFile: string;
begin
  Result := '';
  if (ItemIndex >= 0) and (ItemIndex < Items.Count) and (FDirectory <> '') then
    Result := IncludePathDelimiter(FDirectory) + Items[ItemIndex];
end;

destructor TTyroFileList.Destroy;
begin
  inherited;
end;

procedure TTyroFileList.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
var
  aFile: string;
begin
  //Up/Down/PageUp/PageDown/Home/End and the clipboard keys come from
  //TTyroListBox; this adds what only the picker needs.
  inherited;
  case Key of
    KEY_ENTER:
    begin
      if ItemIndex >= 0 then
      begin
        aFile := SelectedFile;
        if aFile <> '' then
        begin
          Key := KEY_NULL;
          if Assigned(FOnPick) then
            FOnPick(Self, aFile);
        end;
      end;
    end;
    KEY_ESCAPE:
    begin
      Key := KEY_NULL;
      if Assigned(FOnDismiss) then
        FOnDismiss(Self);
    end;
  else
    begin
      //Other keys are not handled by the picker.
    end;
  end;
end;

procedure TTyroFileList.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer);
var
  i, prevIndex: Integer;
begin
  //A click selects the row; clicking the selected row again picks it. Take the
  //selection before the inherited handler runs, because that handler moves
  //ItemIndex to the row under the mouse: comparing after it would make every
  //click, including the first one on a fresh row, look like a second click.
  prevIndex := ItemIndex;
  inherited;
  if (Button = mbLeft) and (HitScrollBar(x, y) = []) then
  begin
    i := ItemIndexAt(y - OuterSize);
    if (i >= 0) and (i = prevIndex) and Assigned(FOnPick) then
      FOnPick(Self, SelectedFile);
  end;
end;

{ TTyroConsoleLog }

procedure TTyroConsoleLog.LogWrite(LogLevel: TLogLevel; S: string);
begin
  Engine.LogWrite(S);
end;

{ TConsoleCommand }

procedure TConsoleCommand.Execute;
begin
end;

{ TExecuteCommand }

procedure TExecuteCommand.Execute;
begin
  Command.Proc(Params);
  Free;
end;

{ TConsoleCommands }

function TConsoleCommands.Add(Sync: Boolean; Name: UTF8String; Alts: TArray<string>; Proc: TProcedureObject; ANote: string): TConsoleCommand;
begin
  Result := TConsoleCommand.Create;
  Result.Name := Name;
  Result.Alts := Alts;
  Result.Proc := proc;
  Result.Note := ANote;
  Result.SyncIt := Sync;
  inherited Add(Result);
end;

function TConsoleCommands.Add(Name: UTF8String; Alts: TArray<string>; Proc: TProcedureObject; ANote: string): TConsoleCommand;
begin
  Result := Add(False, Name, Alts, Proc, ANote);
end;

function TConsoleCommands.Find(const Name: string): TConsoleCommand;
var
  i: integer;
begin
    if Name <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Name, Items[i].Name) or IsStrInArray(Name, Items[i].Alts, True) then
        exit(Items[i]);
    end;
  Result := nil;
end;

function TConsoleCommands.Execute(Name: UTF8String; Params: TStrings): Boolean;
var
  Command: TConsoleCommand;
  Exec :TExecuteCommand;
begin
  Command := Find(Name);
  Result := Command <> nil;
  if Result then
  begin
    Exec := TExecuteCommand.Create;
    Exec.Command := Command;
    Exec.Params := Params;
    Exec.Execute
  end;
end;

initialization
finalization
  //cReportLag := 20;
  FreeAndNil(Engine);
end.
