unit TyroEngines;
{$MODE DELPHI} {$H+}
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
  Classes, SysUtils, SyncObjs,
  mnLogs, mnUtils, mnConfigs,
  RayLib, RayClasses, TyroScripts, TyroSounds,
  TyroClasses, TyroControls, TyroTerminal,
  TyroSprites, TyroPhysics,
  TyroEditors,
  mnClasses;

const
  TyroVersion: Double = 0.1;
  TyroVersionString = '0.1';

  sPromptChar: UTF8string = '>';
  sPromptDOT: UTF8string = '*';

  cMainMargin = 16;
  cDefaultWindowWidth = 640;
  cDefaultWindowHeight = 480;

var
  // Debug switch: when True, DBG messages are written to the console.
  // Off by default (globals are zero-initialized) to keep the output clean.
  IsDebug: Boolean = False;

type

  TProcedureObject = procedure(Params: TStrings) of object;

  TConsoleCommand = class(TmnNamedObject)
  public
    Alts: TStringArray;
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
    function Add(Sync: Boolean; Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string = ''): TConsoleCommand; overload;
    function Add(Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string = ''): TConsoleCommand; overload;
    function Find(const Name: string): TConsoleCommand; overload;
    function Execute(Name: UTF8String; Params: TStrings = nil): Boolean; overload;
  end;

  { TTyroFileList }

  TFilePickEvent = procedure(Sender: TObject; const AFileName: string) of object;

  //The F4 script picker: a list of loadable files (default *.ls) shown centered
  //over the main window. Keyboard: Up/Down select, Enter picks, Escape cancels.
  //Clicking a row selects it; clicking the selected row again picks it.
  TTyroFileList = class(TTyroListBox)
  private
    FDirectory: string;
    FOnPick: TFilePickEvent;
    FOnDismiss: TNotifyEvent;
  protected
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
  public
    //Rebuild the item list from ADirectory using AMask; returns the number of
    //files found. The file names stored here are later resolved against this
    //directory by SelectedFile.
    function Refresh(const ADirectory: string; const AMask: string): Integer;
    //Fully qualified name of the selected item, '' when nothing is selected.
    function SelectedFile: string;
    property Directory: string read FDirectory;
    //Fired when an item is confirmed (Enter, or a click on the selected row).
    property OnPick: TFilePickEvent read FOnPick write FOnPick;
    //Fired when the user cancels the picker (Escape).
    property OnDismiss: TNotifyEvent read FOnDismiss write FOnDismiss;
  end;

  { TTyroMain }

  TTyroMainOption = (moOpaque, moMainWindow, moTerminal, moShowFPS);
  TTyroMainOptions = set of TTyroMainOption;

  TConsoleReadEvent = procedure(AConsole: TTyroTerminal; AInput: string) of object;

  TTyroMain = class(TTyroWindow)
  private
    FFPS: Integer;
    FOptions: TTyroMainOptions;
    FBackColor: TColor;
    //* Signaled by the main loop after every drawing cycle (EndDrawing). The
    //* Lua 'cycle' gate waits on it so "while cycle do" runs at most once per
    //* drawn frame. Auto-reset: an unwaited signal is simply lost.
    FFrameEvent: TEvent;
    //* The control which captured the mouse (e.g. dragging a sizable border).
    //* It keeps receiving MouseMove/MouseUp until the left button is released.
    FControlCapture: TTyroControl;
    function GetCanvasWidth: Integer;
    function GetCanvasHeight: Integer;
    function GetActive: Boolean;

    procedure Help_Command(Params: TStrings);
    procedure Dir_Command(Params: TStrings);
    procedure Clear_Command(Params: TStrings);
    procedure Exit_Command(Params: TStrings);
    procedure Load_Command(Params: TStrings);
    procedure State_Command(Params: TStrings);
    procedure Run_Command(Params: TStrings);
    procedure Stop_Command(Params: TStrings);
    procedure Edit_Command(Params: TStrings);

    procedure EditorClosed(Sender: TObject);
    procedure EditorSave(Sender: TObject);

    procedure LoadScriptThread;
    procedure RunScriptThread;
    procedure StopScriptThread;

    procedure ShowFileList;
    procedure HideFileList;
    procedure ToggleFileList;
    procedure RefreshFileList;
    procedure FileListPicked(Sender: TObject; const AFileName: string);
    procedure FileListDismissed(Sender: TObject);
  protected
    FTextureMode: Boolean;
    IsTerminated: Boolean;
    FCanvasLock: TCriticalSection;
    Camera2D: TCamera2D;
    function CreateCanvas: TTyroCanvas; override;
    procedure Terminate;

  protected
    FPrepared: Boolean; //InitWindow is used
    FQueue: TQueueObjects;
    FScriptThread: TTyroScriptThread;
    //Command line script, should run in main thread only
    FScriptREPL: TTyroScript;
    //F4 script picker: lists the *.ls files of the current directory; picking
    FFileList: TTyroFileList;
    FScriptTypes: TScriptTypes;
    FReadCallback: TConsoleReadEvent;
    FWaitingQueueObject: TQueueObject; //the queue object a script thread is blocked waiting on
    FQueuedScreenshot: String; //filename requested by Lua screenshot(); captured after the next present
    FPresentedFrame: Boolean;
    FScriptFailed: Boolean;
    FRunning: LongInt;
    function GetRunning: Boolean;
    procedure SetRunning(AValue: Boolean);
  protected
    Commands: TConsoleCommands;
    procedure SizeChanged; override;
    procedure ConsoleInput(AConsole: TTyroTerminal; AInput: string);
    procedure ExecuteCommand(ACommand: string);
    //If the word typed is not a builtin command it is treated as a one line
    //Lua script run on FScriptREPL's Lua state, so variables assigned in the
    //console survive across lines (and are shared with the main script).
    function RunLuaLine(const ALine: string): Boolean;
    procedure RegisterCommands;
  public
    constructor Create(AParent: TTyroLayout); override;
    constructor Create; reintroduce; overload;
    destructor Destroy; override;

    //* TextureMode create texture with canvas
    procedure PrepareWindow(AWidth, AHeight: Integer; ATextureMode: Boolean = False); overload;
    procedure ShowWindow(AWidth, AHeight: Integer); overload;
    procedure ShowWindow; overload;
    procedure SetFPS(FPS: Integer); virtual;
    procedure HideWindow; virtual;

    //* Resize the window (and canvas) to the given size; canvas is inset by margin + border
    procedure ResizeWindow(AWidth, AHeight: Integer); virtual;

    //* Before Show window
    procedure Init; virtual;
    procedure Run(AOptions: TTyroMainOptions);
    //* After window initialized and other resource, load your resources here
    procedure Start; virtual;
    procedure Update; override;
    procedure PrepareDraw; virtual;
    procedure Draw; virtual;

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

  public
    RunFile: string;//that to run in ScriptThread
    //Board is a canvas for ScriptThread draw on it
    Console: TTyroTerminal;
    Output: TTyroOutput;
    Editor: TyroEditor;
    Board: TTyroCanvas;
    Sprites: TSprites;
    Physics: TPhysics;
    //property Board: TTyroImage read FBoard;
    property Running: Boolean read GetRunning write SetRunning;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtentions: TStringArray; AScriptClass: TTyroScriptClass);

    procedure ShowConsole(AX, AY, AWidth, AHeight: Integer); overload;
    procedure ShowConsole; overload;
    procedure HideConsole;
    procedure ToggleConsole;
    procedure ToggleOutput;
    procedure ShowEditor;
    procedure HideEditor;
    procedure ToggleEditor;

    property CanvasLock: TCriticalSection read FCanvasLock;
    property Options: TTyroMainOptions read FOptions write FOptions;
    property BackColor: TColor read FBackColor write FBackColor;
    property FPS: Integer read FFPS write SetFPS;
    property Queue: TQueueObjects read FQueue;
    property ScriptTypes: TScriptTypes read FScriptTypes;
    //* True when the last run-and-exit script finished with a Lua error. Reset
    //* at the start of every run; only meaningful for the --exit/--execute CLI
    //* lifecycle where the main loop inspects it before releasing the worker.
    property ScriptFailed: Boolean read FScriptFailed;
  end;
{
  function IntToFPColor(I: Integer): TFPColor;
  function FPColorToInt(C: TFPColor): Integer;
  function RayColorOf(Color: TFPColor): TRGBAColor;
}
var
  Main : TTyroMain = nil;

implementation

uses
  TyroRadio, TyroSpectrum;

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

{ TTyroMain }

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

constructor TTyroMain.Create;
begin
  Create(nil);
end;

function TTyroMain.CreateCanvas: TTyroCanvas;
begin
  Result := TTyroTextureCanvas.Create(GetCanvasWidth, GetCanvasHeight, FTextureMode);
end;

function TTyroMain.GetCanvasWidth: Integer;
begin
  Result := Width - 2 * (BorderSize + Margin);
  if Result < 1 then
    Result := 1;
end;

function TTyroMain.GetCanvasHeight: Integer;
begin
  Result := Height - 2 * (BorderSize + Margin);
  if Result < 1 then
    Result := 1;
end;

function TTyroMain.Terminated: Boolean;
begin
  Result := IsTerminated;
end;

procedure TTyroMain.SetFPS(FPS: Integer);
begin
  FFPS := FPS;
  SetTargetFPS(FPS);
end;

procedure TTyroMain.ShowWindow;
begin
  ShowWindow(cDefaultWindowWidth, cDefaultWindowHeight);
end;

procedure TTyroMain.HideWindow;
begin
  if Visible then
  begin
    Hide;
    // Keep the graphics context alive until destruction. Canvases, shaders,
    // textures and fonts must be unloaded before CloseWindow.
    if RayLib.IsWindowReady then
      RayLib.SetWindowState([FLAG_WINDOW_HIDDEN]);
  end;
end;

procedure TTyroMain.Run(AOptions: TTyroMainOptions);
var
  tw: Integer;
begin
  PrepareWindow(cDefaultWindowWidth, cDefaultWindowHeight);
  Init;

  if FPS = 0 then
    SetFPS(cFramePerSeconds)
  else
    SetFPS(FPS);

  Resources.Load;

  Options := Options + AOptions;

  Start;
  if moMainWindow in AOptions then
    ShowWindow;
  {if Resources.Config.Sections.ReadBool('show', 'console', False) then
  begin
    ShowConsole(0, 0, 0, 0);
    StartConsoleRead;
  end;

  if Resources.Config.Sections.ReadBool('show', 'log', False) then
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

      if Visible and IsWindowReady then
      begin
        if IsWindowHidden then
          break;

        if RayLib.IsWindowResized() then
          ResizeWindow(RayLib.GetScreenWidth(), RayLib.GetScreenHeight());
        PrepareDraw;
        RayLib.BeginDrawing();
        if moOpaque in Options then
          RayLib.ClearBackground(BackColor);

        try
          Camera2D.Target := Vector2Of(0, 0);
          Camera2D.Offset := Vector2Of(Margin, Margin);
          Camera2D.Zoom := 1;
          Camera2D.Rotation := 0;

          Canvas.BeginDraw;
          BeginMode2D(Camera2D);
          Draw;
          EndMode2D();
          Canvas.EndDraw;
          Canvas.PostDraw;

          Paint;

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
                RayLib.TakeScreenshot(PUTF8Char(FQueuedScreenshot));
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
end;

function TTyroMain.GetActive: Boolean;
begin
  Result := Running;
end;

function TTyroMain.GetRunning: Boolean;
begin
  Result := InterlockedExchangeAdd(FRunning, 0) <> 0;
end;

procedure TTyroMain.SetRunning(AValue: Boolean);
begin
  if AValue then
    InterlockedExchange(FRunning, 1)
  else
    InterlockedExchange(FRunning, 0);
end;

procedure TTyroMain.SizeChanged;
begin
  inherited;
end;

procedure TTyroMain.CancelWaiting;
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

procedure TTyroMain.UnregisterWaiting(AQueueObject: TQueueObject);
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

function TTyroMain.RegisterWaiting(AQueueObject: TQueueObject): Boolean;
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

function TTyroMain.WaitToNextFrame(AScript: TTyroScript): Boolean;
begin
  Result := True;
  //No window -> no drawing cycles -> never block the script (behaves like
  //"while true do" so headless scripts do not hang).
  if (FFrameEvent = nil) or (not Visible) or (not RayLib.IsWindowReady) then
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

procedure TTyroMain.QueueScreenshot(const AFileName: String);
begin
  Lock.Enter;
  try
    FQueuedScreenshot := AFileName;
  finally
    Lock.Leave;
  end;
end;

procedure TTyroMain.ProcessQueue;
var
  p: TQueueObject;
  c: Integer;
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
      c := 0;
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
      Inc(c);
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

procedure TTyroMain.Start;
begin
  FScriptFailed := False;
  RunScriptThread;
end;

{function TTyroMain.CloneScript(AScript: TTyroScript): TTyroScript;
begin
  Result := nil;
  if AScript = nil then
    Exit;
  Result := TTyroScriptClass(AScript.ClassType).Create;
  Result.Path := AScript.Path;
  Result.FileName := AScript.FileName;
  Result.Source.Assign(AScript.Source);
end;}

procedure TTyroMain.StopScriptThread;
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

procedure TTyroMain.LoadConfig;
var
  aColor: string;
begin
  with Resources do
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
  end;
end;

procedure TTyroMain.Shutdown;
begin
  Stop;
end;

procedure TTyroMain.PrepareDraw;
begin
end;

constructor TTyroMain.Create(AParent: TTyroLayout);
begin
  inherited;
  RayLibrary.Load;
  FControlCapture := nil;
  FOptions := [moOpaque];
  Name := 'Main';
  Resources := TTyroResources.Create;
  FCanvasLock := TCriticalSection.Create;
  //Auto-reset, initially clear: the Lua 'cycle' gate waits on it, the main
  //loop signals it after every EndDrawing.
  FFrameEvent := TEvent.Create(nil, False, False, '');
  BoundsRect := Rect(0, 0, ScreenWidth, ScreenHeight);
  FBackColor := clCornflowerBlue;

  //Configured margin only; absent key must not clobber the default with 0.
  Margin := Resources.Config.Sections['window'].ReadInteger('margin', cMainMargin);
  //SetTraceLog(LOG_DEBUG or LOG_INFO or LOG_WARNING);
  SetTraceLogLevel([LOG_ERROR, LOG_FATAL]);
  FQueue := TQueueObjects.Create(True);
  FScriptTypes := TScriptTypes.Create(true);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  Console := TTyroTerminal.Create(Self);
  Console.BoundsRect := Rect(Margin, Margin , 100, 200);
  Console.Border:= brdSizable;
  Console.BackColor := clNearBlack;
  Console.TextColor := clLightGray;
  Console.HighlightColor := clBlue;
  Console.SelectionColor := clWhite;
  Console.Visible := False;
  Console.OnInput := ConsoleInput;
  Console.Margin:= 5;
  Console.Align:= alBottom;
  Console.Name := 'Console';

  Output := TTyroOutput.Create(Self);
  Output.Name := 'Output';
  Output.BoundsRect := Rect(Margin, Margin, 480, 240);
  Output.Visible := False;

  Editor := TyroEditor.Create(Self);
  Editor.Name := 'Editor';
  Editor.BoundsRect := Rect(0, 0, 200, 200);
  Editor.Visible := False;
  Editor.OnClose := EditorClosed;
  Editor.Border:= brdSizable;
  Editor.Margin:= 5;
  Editor.Align := alClient;
  Editor.OnSave := EditorSave;

  //F4 script picker; BoundsRect is recentered every time it is shown so it
  //follows window resizes. Hidden until the user presses F4.
  FFileList := TTyroFileList.Create(Self);
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

destructor TTyroMain.Destroy;
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

  //Every object below owns raylib GPU resources. Destroy all of them while
  //the window/OpenGL context is still alive.
  FreeAndNil(Physics);
  FreeAndNil(Sprites);
  FreeAndNil(Board);
  FreeAndNil(Resources);
  FreeAndNil(FQueue);
  FreeAndNil(FScriptTypes);
  FreeAndNil(Commands);
  inherited;
  if RayLib.IsWindowReady then
    RayLib.CloseWindow;
  FreeAndNil(FCanvasLock);
  FreeAndNil(FFrameEvent);
end;

procedure TTyroMain.PrepareWindow(AWidth, AHeight: Integer; ATextureMode: Boolean);
begin
  if AWidth = 0 then
    raise exception.Create('Screen width can not be 0');
  if AHeight = 0 then
    raise exception.Create('Screen height can not be 0');

  FTextureMode := ATextureMode;
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  //SetConfigFlags([FLAG_WINDOW_HIDDEN, FLAG_WINDOW_RESIZABLE]);
  SetConfigFlags([FLAG_WINDOW_HIDDEN, FLAG_WINDOW_RESIZABLE]);
  RayLib.InitWindow(AWidth, AHeight, PUTF8Char(Title));
  BoundsRect := Rect(0, 0, AWidth, AHeight);
  PrepareCanvas;
  Board := TTyroTextureCanvas.Create(AWidth - 2 * (BorderSize + Margin), AHeight - 2 * (BorderSize + Margin), True);
  FPrepared := True;
end;

procedure TTyroMain.Init;
begin
  LoadConfig;
  //ShowWindow(ScreenWidth, ScreenHeight); //with option to show window /w
  LoadScriptThread;
  Running := True;
end;

procedure TTyroMain.Draw;
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
  ThreadSwitch; //Yield
end;

procedure TTyroMain.Update;
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
    UpdateControls;
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EX-CONTROL: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
  ThreadSwitch; //Yield
end;


procedure TTyroMain.ProcessInput;
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
    for i := Controls.Count - 1 downto 0 do
    begin
      if Controls[i] is TTyroControl then
      begin
        aControl := TTyroControl(Controls[i]).MouseTargetAt(mx, my);
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
  // F2 toggles the script editor
  if RayLib.IsKeyPressed(KEY_F2) then
    ToggleEditor;
  // F7 toggles the output control
  if RayLib.IsKeyPressed(KEY_F7) then
    ToggleOutput;
  // F8 toggles the console
  if RayLib.IsKeyPressed(KEY_F8) then
    ToggleConsole;
  // F4 toggles the *.ls script picker: pick a file, then type "run" to execute
  // it in the script thread.
  if RayLib.IsKeyPressed(KEY_F4) then
    ToggleFileList;
  // Handle ESC to hide console when it's active and focused
  if (Console.Visible) and Console.Focused and RayLib.IsKeyPressed(KEY_ESCAPE) then
    HideConsole;

  if FocusedControl = nil then
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
      if Assigned(FocusedControl) then
        FocusedControl.KeyDown(Key, Shift);
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
        aFocused := FocusedControl;
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

procedure TTyroMain.RegisterLanguage(ATitle: string; AExtentions: TStringArray; AScriptClass: TTyroScriptClass);
var
  Item: TScriptType;
begin
  Item := TScriptType.Create;
  Item.Title := ATitle;
  Item.Extentions := AExtentions;
  Item.ScriptClass := AScriptClass;
  FScriptTypes.Add(Item);
end;

procedure TTyroMain.ShowWindow(AWidth, AHeight: Integer);
var
  pos: TVector2;
begin
  if AWidth = 0 then
    raise exception.Create('Screen width can not be 0');
  if AHeight = 0 then
    raise exception.Create('Screen height can not be 0');

  if Border = brdSizable then
    SetConfigFlags([FLAG_WINDOW_RESIZABLE]);

  RayLib.SetWindowSize(AWidth, AHeight);
  BoundsRect := Rect(0, 0, AWidth, AHeight);

  Show;
  ClearWindowState([FLAG_WINDOW_HIDDEN]);
  ShowCursor();
end;

procedure TTyroMain.ResizeWindow(AWidth, AHeight: Integer);
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;
  if (WindowRect.Width = AWidth) and (WindowRect.Height = AHeight)
     and (Canvas <> nil) and (Canvas.Width = GetCanvasWidth) and (Canvas.Height = GetCanvasHeight) then
    Exit;
  //The window is the layout root, so its BoundsRect and WindowRect stay in
  //sync. Its Resize call then propagates the new dimensions to all children.
  BoundsRect := Rect(0, 0, AWidth, AHeight);
  if Canvas <> nil then
    Canvas.Resize(GetCanvasWidth, GetCanvasHeight);
  if Board <> nil then
    Board.Resize(AWidth - 2 * (BorderSize + Margin), AHeight - 2 * (BorderSize + Margin));
end;

procedure TTyroMain.Stop;
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

procedure TTyroMain.Terminate;
begin
  Stop;
  HideWindow;
  IsTerminated := True;
end;

procedure TTyroMain.ShowConsole(AX, AY, AWidth, AHeight: Integer);
begin
  Console.BoundsRect := Rect(AX, AY, AX + AWidth, AY + AHeight);
  ShowConsole;
end;

procedure TTyroMain.ShowConsole;
begin
  Console.CharWidth := Resources.Font.Width;
  Console.CharHeight := Resources.Font.Height;
  Console.Show;
  Console.BringToFront;
  //Route typed input to the console now that it is visible.
  Console.Focused := True;

  StartConsoleRead;
end;

procedure TTyroMain.HideConsole;
begin
  Console.StopRead;
  Console.Hide;
  //Release keyboard focus so input doesn't feed an invisible console.
  Console.Focused := False;
end;

procedure TTyroMain.ToggleConsole;
begin
  if Console.Visible then
    HideConsole
  else
    ShowConsole;
end;

procedure TTyroMain.RefreshFileList;
begin
  //List the scripts of the current directory first (same source as the console
  //"list" and "load" commands); fall back to the workspace so F4 still finds
  //demos when the engine was launched without a script from an empty folder.
  FFileList.Refresh(Resources.WorkSpace, '*.ls');
end;

procedure TTyroMain.ShowFileList;
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
  FFileList.BringToFront;
  FFileList.SetFocus;
end;

procedure TTyroMain.HideFileList;
begin
  FFileList.Hide;
  //Return keyboard focus (and a read prompt) to the console when it is shown.
end;

procedure TTyroMain.ToggleFileList;
begin
  if FFileList.Visible then
    HideFileList
  else
    ShowFileList;
end;

procedure TTyroMain.FileListPicked(Sender: TObject; const AFileName: string);
var
  aScriptType: TScriptType;
  aScript: TTyroScript;
begin
  if (AFileName = '') or not SysUtils.FileExists(AFileName) then
  begin
    Console.Writeln('Script not found: ' + AFileName);
    Exit;
  end;
  aScriptType := ScriptTypes.FindByExtension(ExtractFileExt(AFileName));
  if aScriptType = nil then
  begin
    Console.Writeln('Unknown script type for: ' + ExtractFileName(AFileName));
    Exit;
  end;
  aScript := aScriptType.ScriptClass.Create;
  try
    aScript.LoadFile(AFileName);
  except
    on E: Exception do
    begin
      Console.Writeln('Unable to load ' + ExtractFileName(AFileName) + ': ' + E.Message);
      aScript.Free;
      Exit;
    end;
  end;
  //Replace the current template: stop the worker, swap the script, and leave
  //it stopped so the user types "run" to start it (or F2 to edit it first).
  StopScriptThread;
  Resources.WorkSpace := ExtractFilePath(AFileName);
  HideFileList;
  Console.Writeln('Loaded: ' + ExtractFileName(AFileName) + '. Type "run" to execute it.');
  if not Console.Visible then
    ShowConsole;
end;

procedure TTyroMain.FileListDismissed(Sender: TObject);
begin
  HideFileList;
end;

procedure TTyroMain.ToggleOutput;
begin
  Output.Visible := not Output.Visible;
  if Output.Visible then
    Output.BringToFront;
end;

procedure TTyroMain.ShowEditor;
begin
  //Stop the worker before editing its source template.
  StopScriptThread;
  if FScriptThread <> nil then
  begin
    Editor.FileName := FScriptThread.Script.FileName;
    Editor.LoadSource(FScriptThread.Script.Source);
  end;
  Editor.BoundsRect := Rect(0, 0, Width, Height);
  Editor.Margin := 10;
  Editor.BackColor := clBlack;
  Editor.Show;
  Editor.Focused := True;
end;

procedure TTyroMain.HideEditor;
begin
  if Editor.Visible then
  begin
    if FScriptThread <> nil then
    begin
      Editor.SaveSource(FScriptThread.Script.Source);
      //Rerun;
    end;
    Editor.Hide;
  end;
end;

procedure TTyroMain.ToggleEditor;
begin
  if Editor.Visible then
    HideEditor
  else
    ShowEditor;
end;

procedure TTyroMain.EditorClosed(Sender: TObject);
begin
  HideEditor;
end;

procedure TTyroMain.EditorSave(Sender: TObject);
begin
  if FScriptThread <> nil then
  begin
    Editor.SaveSource(FScriptThread.Script.Source);
  end;
end;

procedure TTyroMain.Edit_Command(Params: TStrings);
begin
  ShowEditor;
end;

procedure TTyroMain.ConsoleInput(AConsole: TTyroTerminal; AInput: string);
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

procedure TTyroMain.ExecuteCommand(ACommand: string);
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

procedure TTyroMain.LoadScriptThread;
var
  aScriptType: TScriptType;
  aScript: TTyroScript;
begin
  if RunFile = '' then
    exit;

  if FScriptThread <> nil then
  begin
    Log.WriteLn('Already running a file: ' + RunFile);
    exit;
  end;

  aScriptType := ScriptTypes.FindByExtension(ExtractFileExt(RunFile));
  if aScriptType <> nil then
  begin
    aScript := aScriptType.ScriptClass.Create;
    if SysUtils.FileExists(RunFile) then
    begin
      Log.WriteLn('File: ' + RunFile);
      if LeftStr(RunFile, 1) = '.' then
        RunFile := ExpandFileName(Resources.WorkSpace + RunFile);
      aScript.LoadFile(RunFile);
      Resources.WorkSpace := ExtractFilePath(RunFile);
      FScriptThread := TTyroScriptThread.Create(aScript);
      exit;
    end;
  end;
  Log.WriteLn('Type of file not found: ' + RunFile);
end;

procedure TTyroMain.RunScriptThread;
begin
  if FScriptThread <> nil then
    FScriptThread.Start;
end;

//Treat an unknown console line as a one line Lua script run on the main
//script's Lua state (FScriptREPL). A fresh Lua state is created on demand so
//variables assigned in the console (e.g. x = 42) persist between lines.
function TTyroMain.RunLuaLine(const ALine: string): Boolean;
var
  aScriptType: TScriptType;
  Output: string;
begin
  Result := False;
  Output := '';
  if FScriptREPL = nil then
  begin
    aScriptType := ScriptTypes.FindByExtension('.ls');
    if aScriptType = nil then
      aScriptType := ScriptTypes.FindByExtension('.lua');
    if aScriptType = nil then
    begin
      Console.Writeln('No Lua environment available. Use "load <script>" first.');
      Exit(True);
    end;
    FScriptREPL := aScriptType.ScriptClass.Create;
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

procedure TTyroMain.RegisterCommands;
var
  aCommand: TConsoleCommand;
begin
  Commands.Add('help', ['?'], Help_Command, 'Show help');
  Commands.Add('list', ['ls'], Dir_Command, 'Show current directory');
  Commands.Add('clear', ['cls'], Clear_Command, 'List files in current directory');
  Commands.Add('exit', ['quit', 'q'], Exit_Command, 'Hide console and stop');
  Commands.Add('stop', [], Stop_Command, 'Stop current script');
  Commands.Add('load', [], Load_Command, 'Load script name from current directory (F4 to pick)');
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

procedure TTyroMain.StartConsoleRead;
begin
  // Clear any script callback so built-in commands are executed
  FReadCallback := nil;
  Console.OnInput := ConsoleInput;
  Console.StartRead(sPromptChar);
end;

procedure TTyroMain.StartConsoleReadEx(ACallback: TConsoleReadEvent);
begin
  //console.read() from a script: make sure the console is visible and focused
  //so the user can actually type a reply. (Do not call StartConsoleRead here;
  //it would clear the callback we are installing.)
  if not Console.Visible then
  begin
    Console.CharWidth := Resources.Font.Width;
    Console.CharHeight := Resources.Font.Height;
    Console.Show;
  end;
  Console.Focused := True;
  FReadCallback := ACallback;
  Console.OnInput := ACallback;
  Console.StartRead(sPromptChar);
end;

procedure TTyroMain.CancelConsoleRead(AReader: TReadConsoleObject);
begin
  // Only detach this reader when it still owns the terminal callback. This
  // avoids an old reader cancelling a newer read request.
  if (AReader <> nil) and (TMethod(FReadCallback).Data = AReader) then
  begin
    Console.StopRead;
    StartConsoleRead;
  end;
end;

procedure TTyroMain.Help_Command(Params: TStrings);
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

procedure TTyroMain.Dir_Command(Params: TStrings);
var
  DirPath: string;
  sr: TSearchRec;
  aFile: string;
begin
  Console.Writeln('Directory: ' + Resources.WorkSpace);
  DirPath := ExcludeTrailingPathDelimiter(Resources.WorkSpace);
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

procedure TTyroMain.Clear_Command(Params: TStrings);
begin
  Console.Clear;
end;

procedure TTyroMain.Exit_Command(Params: TStrings);
begin
  HideConsole;
  Terminate;
end;

procedure TTyroMain.Run_Command(Params: TStrings);
begin
  RunScriptThread;
end;

procedure TTyroMain.Stop_Command(Params: TStrings);
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

procedure TTyroMain.Load_Command(Params: TStrings);
var
  aFile, aFileName: string;
  aScriptType: TScriptType;
  aScript: TTyroScript;
begin
  if (Params.Count = 0) then
  begin
    Console.Writeln('Usage: load <script_name>');
    Exit;
  end;

  aFile := Params[0];
  aFileName := IncludePathDelimiter(Resources.WorkSpace) + aFile;

  if SysUtils.FileExists(aFileName) then
  begin
    StopScriptThread;
    RunFile := aFileName;
    LoadScriptThread;
  end
  else
  begin
    Console.Writeln('Script not found: ' + aFile);
    Exit;
  end;
end;

procedure TTyroMain.State_Command(Params: TStrings);
begin
  if Active and (FScriptThread <> nil) then
    Console.Writeln(FScriptThread.Script.FileName + ' is running');
end;

{ TTyroFileList }

function TTyroFileList.Refresh(const ADirectory: string; const AMask: string): Integer;
var
  sr: TSearchRec;
  DirPath: string;
  Temp: TStringList;
  i: Integer;
begin
  Clear;
  FDirectory := ExcludeTrailingPathDelimiter(ADirectory);
  Temp := TStringList.Create;
  try
    DirPath := FDirectory;
    if DirPath <> '' then
    begin
      if FindFirst(DirPath + PathDelim + AMask, faAnyFile, sr) = 0 then
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
    Temp.Sort;
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
  inherited;
  case Key of
    KEY_UP:
    begin
      if ItemIndex > 0 then
        ItemIndex := ItemIndex - 1
      else if ItemIndex < 0 then
        ItemIndex := 0;
      Key := KEY_NULL;
    end;
    KEY_DOWN:
    begin
      if ItemIndex < Items.Count - 1 then
        ItemIndex := ItemIndex + 1;
      Key := KEY_NULL;
    end;
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

function TConsoleCommands.Add(Sync: Boolean; Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string): TConsoleCommand;
begin
  Result := TConsoleCommand.Create;
  Result.Name := Name;
  Result.Alts := Alts;
  Result.Proc := proc;
  Result.Note := ANote;
  Result.SyncIt := Sync;
  inherited Add(Result);
end;

function TConsoleCommands.Add(Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string): TConsoleCommand;
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
      if SameText(Name, Items[i].Name) or IsStrInArray(Name, Items[i].Alts) then
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
  Main := TTyroMain.Create;
finalization
  FreeAndNil(Main);
end.


