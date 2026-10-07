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

interface

uses
  Classes, SysUtils, SyncObjs, StrUtils,
  mnLogs, mnUtils, mnClasses, mnConfigs,
  RayLib, RayClasses, TyroScripts, TyroSounds,
  TyroClasses, TyroControls, TyroTerminal,
  TyroSprites, TyroPhysics, TyroEditors;

const
  TyroVersion: Double = 0.1;
  TyroVersionString = '0.1';

  sPromptChar: UTF8string = '>';
  sPromptDOT: UTF8string = '*';

  cMainPadding = 16;
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

  TTyroEngineOption = (moOpaque, moMainWindow, moShowTerminal, moShowFPS);
  TTyroEngineOptions = set of TTyroEngineOption;

  TConsoleReadEvent = procedure(AConsole: TTyroTerminal; AInput: UTF8String) of object;

  { TTyroMain }

  TTyroEngine = class(TObject)
  private
    FTitle: string;
    FMargin: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FSizable: Boolean;
    FFPS: Integer;
    FOptions: TTyroEngineOptions;
    FBackColor: TColor;
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
    procedure ClearRunCanvases;
    procedure WipeCanvas(ACanvas: TTyroCanvas);
    procedure ClearScriptSprites;
    function EngineControl(AControl: TTyroLayout): Boolean;
    procedure ShowWindowAfterScript;

    procedure ShowFileList;
    procedure HideFileList;
    procedure ToggleFileList;
    procedure RefreshFileList;
    procedure FileListPicked(Sender: TObject; const AFileName: string);
    procedure FileListDismissed(Sender: TObject);
  protected
    IsTerminated: Boolean;
    FCanvasLock: TCriticalSection;
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
    //* Set once the engine has shown the window by itself after a headless
    //script finished without asking for one, so it happens a single time.
    FWindowAutoShown: Boolean;
    FRunning: LongInt;
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

    function Terminated: Boolean; virtual;
    procedure ProcessInput; virtual;

    procedure LoadConfig;
    procedure Stop; //and wait
    procedure Shutdown; virtual;

    //* Block the calling script (thread) until the next drawing cycle
    //* (EndDrawing) has completed. Backs the Lua global 'cycle' so scripts can
    //* write "while cycle do" instead of "while true do". Returns False when
    //* AScript was stopped while waiting (termination is polled here because
    //* the Lua debug hook cannot fire while a C function blocks the thread).
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

  public
    ScriptFile: string;//Full file name , that to run in ScriptThread
    ScriptSource: string; //Shout pass to Thread to run it
    //Board is a canvas for ScriptThread draw on it
    Console: TTyroTerminal;
    Output: TTyroOutput;
    Editor: TyroEditor;
    Main: TTyroWindow;
    Board: TTyroCanvas;
    Sprites: TSprites;
    Physics: TPhysics;
    //property Board: TTyroImage read FBoard;
    property Running: Boolean read GetRunning write SetRunning;
    property Active: Boolean read GetActive;

    procedure ShowConsole(AX, AY, AWidth, AHeight: Integer); overload;
    procedure ShowConsole; overload;
    procedure HideConsole;
    procedure ToggleConsole;
    procedure ToggleOutput;
    procedure ShowEditor;
    procedure HideEditor;
    procedure ToggleEditor;

    procedure LogWrite(Msg: string);

    property CanvasLock: TCriticalSection read FCanvasLock;
    property Options: TTyroEngineOptions read FOptions write FOptions;
    property BackColor: TColor read FBackColor write FBackColor;
    property FPS: Integer read FFPS write SetFPS;
    property Queue: TQueueObjects read FQueue;
    //* True when the last run-and-exit script finished with a Lua error. Reset
    //* at the start of every run; only meaningful for the --exit/--execute CLI
    //* lifecycle where the main loop inspects it before releasing the worker.
    property ScriptFailed: Boolean read FScriptFailed;

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
  TyroRadio, TyroSpectrum,
  TyroMidi;

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
var
  tw: Integer;
begin
  PrepareWindow(cDefaultWindowWidth, cDefaultWindowHeight);

  LoadConfig;
  //ShowWindow(ScreenWidth, ScreenHeight); //with option to show window /w
  Running := True;
  Load;
  Res.Load;
  if EndsDelimiter(ScriptFile) and SysUtils.DirectoryExists(ScriptFile) then
  begin
    Res.WorkPath := ScriptFile;
    ScriptFile := '';
  end;
  LoadScriptThread;
  Options := Options + AOptions;
  Start;
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
    CheckSynchronize;
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

        RayLib.BeginDrawing();
        if moOpaque in Options then
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

//          Main.Canvas.BeginDraw;
          BeginMode2D(Camera2D);
          Draw;
          EndMode2D;
//          Main.Canvas.EndDraw;
//          Main.Canvas.PostDraw;
          Main.Paint;

          if moShowFPS in Options then
          begin
            tw := RayLib.MeasureText('999 FPS', 20) + 5;
            RayLib.DrawFPS(RayLib.GetScreenWidth - tw, 5);
          end;
        finally
          RayLib.EndDrawing();
          FPresentedFrame := True;
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
        ProcessInput;
      end;
    end;
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
      Exit; //the next drawing frame was presented: run one more iteration
  end;
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
begin
  if Board <> nil then
  begin
    CanvasLock.Enter;
    try
      ft := GetTime();
      fpd := (1 / FPS);
      Board.BeginDraw;
    while True do
    begin
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
      p.Free;
      ft2 := GetTime() - ft;
      if ft2 >= fpd then
      begin
        break;
      end;
    end;
    Board.EndDraw;
    finally
      CanvasLock.Leave;
    end;
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
  FOptions := [moOpaque];
  Res := TTyroResources.Create;
  FCanvasLock := TCriticalSection.Create;
  //Auto-reset, initially clear: the Lua 'cycle' gate waits on it, the main
  //loop signals it after every EndDrawing.
  FFrameEvent := TEvent.Create(nil, False, False, '');
  FWidth := ScreenWidth;
  FHeight := ScreenHeight;
  FBackColor := clCornflowerBlue;

  //Configured margin only; absent key must not clobber the default with 0.
  Margin := Res.Config.Sections['window'].ReadInteger('margin', cMainPadding);
  //SetTraceLog(LOG_DEBUG or LOG_INFO or LOG_WARNING);
  SetTraceLogLevel([LOG_ERROR, LOG_FATAL]);
  FQueue := TQueueObjects.Create(True);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  Main := TTyroWindow.Create(nil);

  Console := TTyroTerminal.Create(Main);
  Console.BoundsRect := Rect(0, 0 , 200, 200);
  Console.Important := True;
  Console.Border:= brdSizable;
  Console.BackColor := clNearBlack;
  Console.Color := clLightGray;
  Console.HighlightColor := clBlue;
  Console.SelectionColor := clWhite;
  Console.Visible := True;
  Console.OnInput := ConsoleInput;
  Console.Margin:= 5;
  Console.Padding:= 5;
  //Console.Align:= alBottom;
  Console.Name := 'Console';

  Output := TTyroOutput.Create(Main);
  Output.Important := True;
  Output.Name := 'Output';
  Output.BoundsRect := Rect(0, 0, 480, 240);

  Editor := TyroEditor.Create(Main);
  Editor.Name := 'Editor';
  Editor.Important := True;
  Editor.BoundsRect := Rect(0, 0, 200, 200);
  Editor.Visible := False;
  Editor.OnClose := EditorClosed;
  Editor.Border:= brdSizable;
  Editor.Margin:= 5;
  Editor.Padding:= 5;
  Editor.Align := alClient;
  Editor.OnSave := EditorSave;

  //F4 script picker; BoundsRect is recentered every time it is shown so it
  //follows window resizes. Hidden until the user presses F4.
  FFileList := TTyroFileList.Create(Main);
  FFileList.Name := 'FileList';
  FFileList.PlaceHolder := 'No Files';
  FFileList.BoundsRect := Rect(0, 0, 360, 280);
  FFileList.Visible := False;
  FFileList.OnPick := FileListPicked;
  FFileList.OnDismiss := FileListDismissed;

  Sprites := TSprites.Create;
  Physics := TPhysics.Create(Sprites);
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
  if MidiPlayer <> nil then
    MidiPlayer.Stop;
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
  FreeAndNil(Board);
  FreeAndNil(Res);
  FreeAndNil(FQueue);
  FreeAndNil(Commands);
  FreeAndNil(Main);
  inherited;
  if RayLib.IsWindowReady then
    RayLib.CloseWindow;
  FreeAndNil(FCanvasLock);
  FreeAndNil(FFrameEvent);
end;

procedure TTyroEngine.PrepareWindow(AWidth, AHeight: Integer);
begin
  if AWidth = 0 then
    raise exception.Create('Screen width can not be 0');
  if AHeight = 0 then
    raise exception.Create('Screen height can not be 0');

  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  //SetConfigFlags([FLAG_WINDOW_HIDDEN, FLAG_WINDOW_RESIZABLE]);
  SetConfigFlags([FLAG_WINDOW_HIDDEN, FLAG_WINDOW_RESIZABLE]);
  RayLib.InitWindow(AWidth, AHeight, PUTF8Char(Title));
  FWidth := AWidth;
  FHeight := AHeight;
  //MainPrepareCanvas;
  Board := TTyroTextureCanvas.Create(AWidth - 2 * Margin, AHeight - 2 * Margin, True);
  FPrepared := True;
end;

procedure TTyroEngine.Load;
begin
end;

procedure TTyroEngine.Unload;
begin
end;

procedure TTyroEngine.Draw;
begin
  if Board <> nil then
  begin
    // Canvas layer sits at the bottom: blit the legacy Board first so its
    // opaque background does not cover the sprites drawn on top of it.
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
  end;
  TThread.Yield
end;

procedure TTyroEngine.Update;
var
  Scripted: array[0..1023] of TCollisionEvent;
  Enough: Integer;
  I: Integer;
  ev: TCollisionEvent;
  AState: string;
begin
  try
    if Physics <> nil then
      Physics.Step(RayLib.GetFrameTime());
    UpdateMelodies;
    UpdateWaveforms;
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
  TThread.Yield;
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
  // Handle ESC to hide console when it's active and focused
  if (Console.Visible) and Console.Focused and RayLib.IsKeyPressed(KEY_ESCAPE) then
    HideConsole;

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
  FWidth := AWidth;
  FHeight := AHeight;

  ClearWindowState([FLAG_WINDOW_HIDDEN]);
  ShowCursor();
end;

procedure TTyroEngine.ResizeWindow(AWidth, AHeight: Integer);
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  if Main <> nil then
    Main.BoundsRect := Rect(0, 0 ,Width, Height);
  if Board <> nil then
    Board.Resize(AWidth - 2 * Margin, AHeight - 2 * Margin);
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
end;

procedure TTyroEngine.Terminate;
begin
  Stop;
  HideWindow;
  IsTerminated := True;
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
  ClearRunCanvases;
  //A song the finished run left playing belongs to that run, not to the next one.
  if MidiPlayer <> nil then
    MidiPlayer.Stop;
  aScriptType := ScriptTypes.FindByExtension(ExtractFileExt(ScriptFile));
  if (aScriptType <> nil) then
  begin
    aScript := aScriptType.ScriptClass.Create(ScriptSource);
    FScriptThread := TTyroScriptThread.Create(aScript);
    FScriptThread.Start;
  end
  else
    Log.WriteLn('Type of file not found: ' + ScriptFile);
end;

//True for the controls the engine creates itself and that therefore outlive a run
function TTyroEngine.EngineControl(AControl: TTyroLayout): Boolean;
begin
  Result := (AControl = Console) or (AControl = Output) or
    (AControl = Editor) or (AControl = FFileList);
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
  if (Main.FocusedControl <> nil) and not EngineControl(Main.FocusedControl) then
    Main.FocusedControl := nil;
  if (FControlCapture <> nil) and not EngineControl(FControlCapture) then
    FControlCapture := nil;
  //A freed control removes itself from Controls (TTyroControl.Destroy clears its
  //parent), so walking backwards keeps the remaining indexes valid.
  for i := Main.Controls.Count - 1 downto 0 do
    if not EngineControl(Main.Controls[i]) then
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

//Wipe the pixels the previous run left on the two canvases it draws through:
//Board, the texture the script's drawing queue fills, and Canvas, the window
//canvas it is blitted to. Both keep their content between frames, so without this
//the new script starts on top of the old one.
procedure TTyroEngine.ClearRunCanvases;
begin
  //Same lock the queue processing takes while it draws into Board.
  CanvasLock.Enter;
  try
    WipeCanvas(Board);
    WipeCanvas(Main.Canvas);
  finally
    CanvasLock.Leave;
  end;
end;

//Clear one canvas in place. A texture canvas is only bound to its render texture
//between BeginDraw and EndDraw, so the clear has to happen inside that pair:
//outside it ClearBackground would wipe the window backbuffer and leave the canvas
//content on screen.
procedure TTyroEngine.WipeCanvas(ACanvas: TTyroCanvas);
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
end;

//Called from the main loop while the window is still hidden. A script file given
//on the command line runs without moMainWindow, so it is free to finish without
//showing anything; if it does, nothing is on screen and the process just sits
//there. Show the window after all so the session stays usable.
procedure TTyroEngine.ShowWindowAfterScript;
begin
  if FWindowAutoShown or (not IsWindowHidden) or IsTerminated then
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
  i: Integer;
begin
  inherited;
  //A click selects the row; clicking the selected row again picks it.
  if (Button = mbLeft) and (HitScrollBar(x, y) = []) then
  begin
    i := ItemIndexAt(y - (Margin + BorderSize));
    if (i >= 0) and (i = ItemIndex) and Assigned(FOnPick) then
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
  FreeAndNil(Engine);
end.
