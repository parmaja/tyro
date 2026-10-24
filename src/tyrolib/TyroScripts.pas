unit TyroScripts;

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, SyncObjs, Types,
  mnUtils, mnClasses, mnLogs,
  RayLib, RayClasses,
  Melodies, TyroSounds,
  TyroClasses, TyroControls, TyroTerminal, TyroSpectrum;

type
  { TQueueObject }

  TQueueObject = class abstract(TObject)
  private
    FEvent: TEvent;
    FCancelled: LongInt;
  protected
    function EventNeeded: TEvent; virtual;
    procedure DoExecute; virtual; abstract;
  public
    LineNo: Integer;
    destructor Destroy; override;
    procedure Execute;
    //Sync to main thread
    procedure Run(Thread: TThread = nil);
    procedure SetEvent;
    function Wait(Timeout: Cardinal = INFINITE): Boolean;
    procedure Cancel; virtual;
    function GetCancelled: Boolean;
    property Cancelled: Boolean read GetCancelled;
  end;

  { TQueueObjects }

  TQueueObjects = class(TmnObjectList<TQueueObject>)
  public
    procedure CancelAll;
  end;

  { TDrawObject }

  TDrawObject = class abstract(TQueueObject)
  private
    FCanvas: TTyroCanvas;
  protected
    procedure Created; virtual;
    procedure DoExecute; override;
  public
    constructor Create(ACanvas: TTyroCanvas);
    property Canvas: TTyroCanvas read FCanvas;
  end;

  { TWindowObject }

  TWindowObject = class(TQueueObject)
  public
    fW, fH: Integer;
    constructor Create(W, H: Integer);
    procedure DoExecute; override;
  end;

  { TShowConsoleObject }

  TShowConsoleObject = class(TQueueObject)
  public
    fX, fY: Integer;
    fW, fH: Integer;
    constructor Create(AX, AY: Integer); overload;
    constructor Create(AX, AY, AW, AH: Integer); overload;
    procedure DoExecute; override;
  end;

  { TShowOutputObject }

  TShowOutputObject = class(TQueueObject)
  public
    fX, fY: Integer;
    fW, fH: Integer;
    constructor Create(AX, AY: Integer); overload;
    constructor Create(AX, AY, AW, AH: Integer); overload;
    procedure DoExecute; override;
  end;

  { THideOutputObject }

  THideOutputObject = class(TQueueObject)
  public
    procedure DoExecute; override;
  end;

  { TReadConsoleObject }

  TReadConsoleObject = class(TQueueObject)
  public
    Prompt: string;
    ResultString: string;
    constructor Create(APrompt: string);
    destructor Destroy; override;
    procedure DoExecute; override;
    procedure Cancel; override;
    procedure HandleConsoleInput(AInput: UTF8String);
  end;

   { TCreateControlObject }

   { Creates a control by class name on the main thread (it is painted by the
     main drawing cycle every frame) and returns the created control. Dispatch
     happens on the main thread so controls always belong to the control tree.
     Supported classes: 'button', 'panel', 'label', 'checkbox', 'edit',
     'spectrum', 'listbox', 'image'. An 'image' reads the caption argument as
     the file name of the texture it shows (see TLoadControlImageObject). }
  TCreateControlObject = class(TQueueObject)
  private
    FClassName: string;
    FCaption: utf8string;
    FX, FY, FW, FH: Integer;
    FName: string;
    FControl: TTyroControl;
    FTransferred: Boolean;
    FExecutionAttempted: Boolean;
    procedure FreeUntransferredControl;
  public
    constructor Create(const AClassName: string; const ACaption: utf8string; AX, AY, AW, AH: Integer; const AName: string);
    destructor Destroy; override;
    function TakeControl: TTyroControl;
    procedure DoExecute; override;
    property Control: TTyroControl read FControl;
   end;

   { TSetControlBoundsObject }

   { Moves/resizes an existing control on the main thread (the control tree is
     aligned/painted by the main drawing cycle). }
   TSetControlBoundsObject = class(TQueueObject)
   private
     FControl: TTyroControl;
     FBoundsRect: TRect;
   public
     constructor Create(AControl: TTyroControl; ABoundsRect: TRect);
     procedure DoExecute; override;
   end;

   { TSetControlFocusObject }

   { Moves the input focus to an existing control on the main thread. }
  TSetControlFocusObject = class(TQueueObject)
   private
     FControl: TTyroControl;
   public
     constructor Create(AControl: TTyroControl);
     procedure DoExecute; override;
   end;

  { TDrawSetColorObject }

  TDrawSetColorObject = class(TDrawObject)
  public
    fColor: TColor;
    constructor Create(ACanvas: TTyroCanvas; Color: TColor);
    procedure DoExecute; override;
  end;

  { TSetControlTextObject }

  TSetControlTextObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FText: string;
  public
    constructor Create(AControl: TTyroControl; const AText: string);
    procedure DoExecute; override;
  end;

  { TSetControlCheckedObject }

  TSetControlCheckedObject = class(TQueueObject)
  private
    FControl: TTyroCheckBox;
    FChecked: Boolean;
  public
    constructor Create(AControl: TTyroCheckBox; AChecked: Boolean);
    procedure DoExecute; override;
  end;

  TSetControlVisibleObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FVisible: Boolean;
  public
    constructor Create(AControl: TTyroControl; AVisible: Boolean);
    procedure DoExecute; override;
  end;

  TSetControlBorderObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FBorder: TBorder;
  public
    constructor Create(AControl: TTyroControl; ABorder: TBorder);
    procedure DoExecute; override;
  end;

  TSetControlBackColorObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FColor: TColor;
  public
    constructor Create(AControl: TTyroControl; AColor: TColor);
    procedure DoExecute; override;
  end;

  TSetControlNameObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FName: string;
  public
    constructor Create(AControl: TTyroControl; const AName: string);
    procedure DoExecute; override;
  end;

  TSetControlAlignObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FAlign: TAlign;
  public
    constructor Create(AControl: TTyroControl; AAlign: TAlign);
    procedure DoExecute; override;
  end;

  TSetControlParentObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FParent: TTyroLayout;
  public
    constructor Create(AControl: TTyroControl; AParent: TTyroLayout);
    procedure DoExecute; override;
  end;

  { ListBox helpers: the list box is painted by the main cycle, so every
    mutation is marshalled to the main thread like the other control setters. }

  TSetControlItemsObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FItems: TStringList; //owned copy
  public
    constructor Create(AControl: TTyroControl; AItems: TStringList);
    destructor Destroy; override;
    procedure DoExecute; override;
  end;

  TSetControlItemObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FIndex: Integer;
    FText: utf8string;
  public
    constructor Create(AControl: TTyroControl; AIndex: Integer; const AText: utf8string);
    procedure DoExecute; override;
  end;

  TAddControlItemObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FText: utf8string;
  public
    constructor Create(AControl: TTyroControl; const AText: utf8string);
    procedure DoExecute; override;
  end;

  TClearControlItemsObject = class(TQueueObject)
  private
    FControl: TTyroControl;
  public
    constructor Create(AControl: TTyroControl);
    procedure DoExecute; override;
  end;

  TSetControlViewCountObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FViewCount: Integer;
  public
    constructor Create(AControl: TTyroControl; AViewCount: Integer);
    procedure DoExecute; override;
  end;

  { TSetControlItemHeightObject }

  TSetControlItemHeightObject = class(TQueueObject)
  private
    FControl: TTyroControl;
  public
    constructor Create(AControl: TTyroControl);
    procedure DoExecute; override;
  end;

  TSetControlItemIndexObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FItemIndex: Integer;
  public
    constructor Create(AControl: TTyroControl; AItemIndex: Integer);
    procedure DoExecute; override;
  end;

  TSetControlPlaceHolderObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FPlaceHolder: string;
  public
    constructor Create(AControl: TTyroControl; const APlaceHolder: string);
    procedure DoExecute; override;
  end;

  { TLoadControlImageObject }

  { Loads a texture into a TTyroImage on the main thread, because the upload
    needs the same GL context the control is painted with. An empty file name
    releases the texture the control holds, so a failed load never keeps a
    texture alive. The file name is resolved by TTyroImage.LoadFromFile. }
  TLoadControlImageObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FFileName: string;
  public
    constructor Create(AControl: TTyroControl; const AFileName: string);
    procedure DoExecute; override;
  end;

  { TDrawSetAlphaObject }

  TDrawSetAlphaObject = class(TDrawObject)
  public
    fAlpha: Byte;
    constructor Create(ACanvas: TTyroCanvas; Alpha: Byte);
    procedure DoExecute; override;
  end;

  { TDrawSetBackColorObject }

  TDrawSetBackColorObject = class(TDrawObject)
  public
    fColor: TColor;
    constructor Create(ACanvas: TTyroCanvas; Color: TColor);
    procedure DoExecute; override;
  end;

  { TDrawCircleObject }

  TDrawCircleObject = class(TDrawObject)
  public
    fX, fY, fR: Integer;
    fFill: Boolean;
    constructor Create(ACanvas: TTyroCanvas; X, Y, R: Integer; Fill: Boolean);
    procedure DoExecute; override;
  end;

  { TDrawTriangleObject }

  TDrawTriangleObject = class(TDrawObject)
  public
    fX1, fY1, fX2, fY2, fX3, fY3: Integer;
    fFill: Boolean;
    constructor Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2, X3, Y3: Integer; Fill: Boolean);
    procedure DoExecute; override;
  end;

  { TDrawSetPenObject }

  TDrawSetPenObject = class(TDrawObject)
  public
    fPen: Integer;
    constructor Create(ACanvas: TTyroCanvas; APen: Integer);
    procedure DoExecute; override;
  end;

  { TDrawRectangleObject }

  TDrawRectangleObject = class(TDrawObject)
  public
    fX, fY, fW, fH: Integer;
    fFill: Boolean;
    constructor Create(ACanvas: TTyroCanvas; X, Y, W, H: Integer; Fill: Boolean);
    procedure DoExecute; override;
  end;

  { TDrawLineObject }

  TDrawLineObject = class(TDrawObject)
  public
    fX1, fY1, fX2, fY2: Integer;
    //0 keeps the canvas pen size; a positive value is a per-call thickness that
    //DoExecute applies and restores, so canvas.line(x1,y1,x2,y2,w) works.
    fPen: Integer;
    constructor Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2: Integer); overload;
    constructor Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2: Integer; APen: Integer); overload;
    procedure DoExecute; override;
  end;

  { TDrawLineToObject }

  TDrawLineToObject = class(TDrawObject)
  public
    fX, fY: Integer;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer);
    procedure DoExecute; override;
  end;

  { TDrawPointObject }

  TDrawPointObject = class(TDrawObject)
  public
    fX, fY: Integer;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer);
    procedure DoExecute; override;
  end;

  { TSlowDrawObject }

  //  Backs canvas.test(ms [, x, y, w, h]): a drawing command whose execution on
  //  the main thread spins for about ms milliseconds before drawing a marker
  //  rectangle. It emulates a single draw call slower than a frame, so a script
  //  can check that the per-'cycle' grouping still commits the whole body in one
  //  drawing cycle instead of splitting it. Diagnostic helper, always compiled.
  TSlowDrawObject = class(TDrawObject)
  public
    fMillis: Integer;
    fX, fY, fW, fH: Integer;
    fFill: Boolean;
    constructor Create(ACanvas: TTyroCanvas; AMillis, X, Y, W, H: Integer; Fill: Boolean);
    procedure DoExecute; override;
  end;

  { TDrawTextObject }

  TDrawTextObject = class(TDrawObject)
  public
    fX, fY: Integer;
    fText: String;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer; Text: String);
    procedure DoExecute; override;
  end;

  { TPrintObject }

  TPrintObject = class(TQueueObject)
  public
    FText: String;
    FNewLine: Boolean;
    constructor Create(Text: String; NewLine: Boolean);
    procedure DoExecute; override;
  end;

  { TOutputPrintObject - writes a copy of the script output (print/println)
    to the engine's Output control. }

  TOutputPrintObject = class(TQueueObject)
  public
    FText: String;
    FNewLine: Boolean;
    constructor Create(const Text: String; NewLine: Boolean);
    procedure DoExecute; override;
  end;


  { TBeepObject }

  TBeepObject = class(TQueueObject)
  public
    Period: Integer;
    constructor Create(APeriod: Integer);
    procedure DoExecute; override;
  end;

  { TPlaySoundObject }

  TPlaySoundObject = class(TQueueObject)
  public
    Freq, Period: Integer;
    WaveName: string;
    constructor Create(AFreq, APeriod: Integer; AWaveName: string = '');
    procedure DoExecute; override;
  end;

  { TPlayNoiseObject }

  TPlayNoiseObject = class(TQueueObject)
  public
    Freq, Period: Integer;
    constructor Create(AFreq, APeriod: Integer);
    procedure DoExecute; override;
  end;

  { TPlayMusicFileObject }

  TPlayMusicFileObject = class(TQueueObject)
  public
    FileName: string;
    constructor Create(AFileName: string);
    procedure DoExecute; override;
  end;

  { TPlaySoundFileObject }

  { sound.play("file.wav" [, volume]) - one-shot SFX from an audio file. }
  TPlaySoundFileObject = class(TQueueObject)
  public
    FileName: string;
    Volume: Single;
    constructor Create(const AFileName: string; AVolume: Single = 1.0);
    procedure DoExecute; override;
  end;

  { TLoadSoundFileObject }

  { sound.load("file.wav") - preload a file into the permanent sound cache. }
  TLoadSoundFileObject = class(TQueueObject)
  public
    FileName: string;
    constructor Create(const AFileName: string);
    procedure DoExecute; override;
  end;

  { TUnloadSoundFileObject }

  { sound.unload("file.wav") - drop a preloaded file from the sound cache. }
  TUnloadSoundFileObject = class(TQueueObject)
  public
    FileName: string;
    constructor Create(const AFileName: string);
    procedure DoExecute; override;
  end;

   { TPlayMMLObject }

   TPlayMMLObject = class(TQueueObject)
   public
     Song: TmmlSong;
     constructor Create(ASong: TmmlSong);
     procedure DoExecute; override;
   end;

   { TStartLoopSoundObject }

   TStartLoopSoundObject = class(TQueueObject)
   public
     WaveName: string;
     Frequency: Single;
     Volume: Single;
     constructor Create(const AWaveName: string; AFrequency, AVolume: Single);
     procedure DoExecute; override;
   end;

   { TStopLoopSoundObject }

   TStopLoopSoundObject = class(TQueueObject)
   public
     procedure DoExecute; override;
   end;

   { TSetLoopPitchObject }

   TSetLoopPitchObject = class(TQueueObject)
   public
     Frequency: Single;
     constructor Create(AFrequency: Single);
     procedure DoExecute; override;
   end;

   { TSetLoopVolumeObject }

   TSetLoopVolumeObject = class(TQueueObject)
   public
     Volume: Single;
     constructor Create(AVolume: Single);
     procedure DoExecute; override;
   end;

   { TLoadFontObject }

   TLoadFontObject = class(TQueueObject)
   public
     FileName: string;
     FontSize: Integer;
     constructor Create(AFileName: string; AFontSize: Integer = cFontSize);
     procedure DoExecute; override;
   end;

   { TSetEffectObject }

   TSetEffectObject = class(TQueueObject)
   public
     Canvas: TTyroCanvas;
     EffectName: string;
     constructor Create(ACanvas: TTyroCanvas; const AEffectName: string);
     procedure DoExecute; override;
   end;

   { TSetEffectValueObject }

   TSetEffectValueObject = class(TQueueObject)
   public
     Canvas: TTyroCanvas;
     Value: Single;
     constructor Create(ACanvas: TTyroCanvas; AValue: Single);
     procedure DoExecute; override;
   end;

   { TSetEffectAreaObject }

   TSetEffectAreaObject = class(TQueueObject)
   public
     Canvas: TTyroCanvas;
     Area: TRectangle;
     constructor Create(ACanvas: TTyroCanvas; const AArea: TRectangle);
     procedure DoExecute; override;
   end;

   { TLoadShaderObject }

   TLoadShaderObject = class(TQueueObject)
   public
     Canvas: TTyroCanvas;
     FileName: string;
     constructor Create(ACanvas: TTyroCanvas; const AFileName: string);
     procedure DoExecute; override;
   end;

  { TClearObject }

  TClearObject = class(TDrawObject)
  public
    fColor: TColor;
    fHasColor: Boolean;
    constructor Create(ACanvas: TTyroCanvas); overload;
    constructor Create(ACanvas: TTyroCanvas; AColor: TColor); overload;
    procedure DoExecute; override;
  end;

  IConsole = interface
    procedure Print(s: string);
    procedure Show;
  end;

  TTyroScriptThread = class;

  { TTyroScript }

  TTyroScript = class abstract(TObject)
  private
    FActive: LongInt;
    FStopRequested: LongInt;
    FHandoff: LongInt;
    FFileName: string;
    FStarted: LongInt;
    function GetActive: Boolean;
 function GetStarted: Boolean;
    function GetStopped: Boolean;
    function GetHandoff: Boolean;
    procedure SetHandoff(AValue: Boolean);
    procedure ExecuteQueueObject; //this for sync do not call it
    procedure ExecuteQueueObjectNoFree; //this for sync do not call it
  protected
    QueueObject: TQueueObject;
    Thread: TTyroScriptThread;
    FSource: TStringList;
    //* Error message of the most recent main-chunk execution, or '' when it
    //* finished without error. Written on the worker thread, published before
    //* the thread marks itself Completed, read once by the main thread during
    //* the --exit lifecycle. Not used by the interactive REPL (RunLine).
    FLastError: string;
    FKeepAlive: Boolean;

    procedure RunQueueObject(AQueueObject: TQueueObject);
    procedure RunQueueObjectNoFree(AQueueObject: TQueueObject);
    procedure AddQueueObject(AQueueObject: TQueueObject); virtual;

    procedure BeforeRun; virtual;
    procedure Init; virtual;
    procedure Run; virtual; abstract;
    procedure AfterRun; virtual;
  public
    constructor Create(ASource: string); virtual;
    destructor Destroy; override;
    procedure Stop; virtual;
    procedure Start; virtual;
    procedure LoadFile(AFileName: string); overload;
    procedure LoadString(ASource: string); overload;

    //Execute a single line of source on the persistent script state so globals
    //(e.g. x = 5) survive across lines; used by the console REPL. AOutput is
    //the error message when Result = False.
    function RunLine(const ALine: string; out AOutput: string): Boolean; virtual;

    //True when the finished script exposes engine-driven callbacks (update/
    //draw) and should be kept alive on the main thread instead of disposing its
    //state. Called on the main thread after the worker returned normally.
    function HasEngineCallbacks: Boolean; virtual;
    //Called once per frame on the main thread when HasEngineCallbacks is True.
    //Implementations queue drawing commands like any other script code.
    procedure EngineUpdate(ADeltaTime: Double); virtual;
    procedure EngineDraw; virtual;

    property FileName: string read FFileName write FFileName; //TODO FFileName
    property Active: Boolean read GetActive;
    property Started: Boolean read GetStarted; //started true even after stopped
    //* Stop() was asked for (Lua exit(), engine Stop). Distinct from Active,
    //* which is already False once a script returns normally: the engine uses
    //* this to tell an engine-driven script that ended its own run from one that
    //* simply finished its top-level chunk.
    property Stopped: Boolean read GetStopped;
    //* The engine took the script's state over for main-thread update()/draw()
    //* callbacks. Active is already False by then (the top-level chunk returned),
    //* so queue admission (AddQueueObject) and the drawing-cycle gate consult
    //* this flag to keep treating the script as current.
    property Handoff: Boolean read GetHandoff write SetHandoff;
    property LastError: string read FLastError; //'' when the last run was clean
    property Source: TStringList read FSource; //the loaded script lines
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
  end;

  { TTyroScriptThread }

 TTyroScriptThread = class(TThread)
 private
   FStarted: LongInt;
   FCompleted: LongInt;
   function GetActive: Boolean;
   function GetStarted: Boolean;
   function GetCompleted: Boolean;
  protected
    FScript: TTyroScript;
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create(AScript: TTyroScript); virtual;
    destructor Destroy; override;
    procedure Start; reintroduce;
    property Started: Boolean read GetStarted;
    property Completed: Boolean read GetCompleted;
    property Active: Boolean read GetActive;
    property Script: TTyroScript read FScript;
  end;

  TTyroScriptClass = class of TTyroScript;

  { TScriptType }

  TScriptType = class(TObject)
  public
    Title: string;
    Extentions: TArray<string>;
    ScriptClass: TTyroScriptClass;
    function CollectExtentions: string;
  end;

  { TScriptTypes }

  TScriptTypes = class(TmnObjectList<TScriptType>)
  public
    procedure RegisterLanguage(ATitle: string; AExtentions: TArray<string>; AScriptClass: TTyroScriptClass);
    function FindByExtension(Extension: string): TScriptType;
  end;

var
  ScriptTypes: TScriptTypes = nil;

implementation

uses
  TyroEngines;

{ TTyroScriptThread }

function TTyroScriptThread.GetActive: Boolean;
begin
  Result := (Script <> nil) and Script.Active;
end;

procedure TTyroScriptThread.TerminatedSet;
begin
  if Script <> nil then
    Script.Stop;
  inherited;
end;

procedure TTyroScriptThread.Execute;
var
  aScript: TTyroScript;
  aEngine: TTyroEngine;
begin
  try
    Script.Start;
  finally
    // Script finished normally (or was stopped). If it finished without being
    // terminated and should be kept alive for callbacks, notify engine.
    // Capture the targets in locals: the queued method runs later on the main
    // thread and must not read this thread's fields, which may already be freed
    // by then. The engine re-validates the script against its current worker.
    aScript := Script;
    aEngine := Engine;
    if (aScript <> nil) and not Terminated and aScript.KeepAlive and (aEngine <> nil) then
      TThread.Queue(TThread.CurrentThread, procedure begin aEngine.OnScriptFinished(aScript); end);
    // TThread.Finished is a plain Boolean written by the worker. Publish
    // completion atomically for lifecycle decisions made by the main thread.
    TInterlocked.Exchange(FCompleted, 1);
  end;
end;

procedure TTyroScriptThread.Start;
begin
  if TInterlocked.CompareExchange(FStarted, 1, 0) <> 0 then
    Exit;
  // Record the resume request synchronously. Setting this in Execute leaves a
  // race where Stop sees an apparently unstarted thread that is already live.
  try
    inherited Start;
  except
    TInterlocked.Exchange(FStarted, 0);
    raise;
  end;
end;

function TTyroScriptThread.GetStarted: Boolean;
begin
  Result := TInterlocked.Add(FStarted, 0) <> 0;
end;

function TTyroScriptThread.GetCompleted: Boolean;
begin
  Result := TInterlocked.Add(FCompleted, 0) <> 0;
end;

constructor TTyroScriptThread.Create(AScript: TTyroScript);
begin
  inherited Create(True);
  FScript := AScript;
  Script.Thread := Self;
  FreeOnTerminate := False;
  //tpNormal: tpLower made the worker's wake-ups after sleep() late and
  //irregular, so script-driven motion juddered. The loop blocks on events
  //almost all of the time, so a normal priority costs the renderer nothing.
  Priority := tpNormal;
end;

destructor TTyroScriptThread.Destroy;
begin
  FreeAndNil(Script);
  inherited;
end;

{ TScriptType }

function TScriptType.CollectExtentions: string;
begin
  Result := CollectStrings(Extentions, ',');
end;

{ TShowConsoleObject }

constructor TShowConsoleObject.Create(AX, AY: Integer);
begin
  inherited Create;
  fX := AX;
  fY := AY;
  fW := 0;
  fH := 0;
end;

constructor TShowConsoleObject.Create(AX, AY, AW, AH: Integer);
begin
  inherited Create;
  fX := AX;
  fY := AY;
  fW := AW;
  fH := AH;
end;

procedure TShowConsoleObject.DoExecute;
begin
  Engine.ShowConsole(fX, fY, fW, fH);
end;

{ TShowOutputObject }

constructor TShowOutputObject.Create(AX, AY: Integer);
begin
  inherited Create;
  fX := AX;
  fY := AY;
  fW := 0;
  fH := 0;
end;

constructor TShowOutputObject.Create(AX, AY, AW, AH: Integer);
begin
  inherited Create;
  fX := AX;
  fY := AY;
  fW := AW;
  fH := AH;
end;

procedure TShowOutputObject.DoExecute;
begin
  Engine.Output.Visible := True;
  Engine.Output.BringToFront;
  if (fW > 0) and (fH > 0) then
    Engine.Output.BoundsRect := Rect(fX, fY, fX + fW, fY + fH)
  else if (fX <> 0) or (fY <> 0) then
    Engine.Output.BoundsRect := Rect(fX, fY, fX + Engine.Output.Width, fY + Engine.Output.Height);
end;

{ THideOutputObject }

procedure THideOutputObject.DoExecute;
begin
  Engine.Output.Hide;
end;

{ TReadConsoleObject }

constructor TReadConsoleObject.Create(APrompt: string);
begin
  inherited Create;
  Prompt := APrompt;
  ResultString := '';
  EventNeeded;
end;

destructor TReadConsoleObject.Destroy;
begin
  Cancel;
  inherited;
end;

procedure TReadConsoleObject.DoExecute;
begin
  // Register before exposing the callback. Otherwise shutdown can happen
  // after Synchronize returns but before the script thread enters Wait.
  if Engine.RegisterWaiting(Self) then
    Engine.StartConsoleReadEx(Prompt, HandleConsoleInput)
  else
    SetEvent;
end;

procedure TReadConsoleObject.HandleConsoleInput(AInput: UTF8String);
begin
  ResultString := AInput;
  // Re-arm for built-in command mode
  Engine.StartConsoleRead;
  SetEvent;
end;

procedure TReadConsoleObject.Cancel;
begin
  inherited;
  // The terminal stores this object's method pointer. Restore the normal
  // command callback before destruction. The reader normally dies on the
  // script worker, so marshal detachment to the application thread.
  if Engine <> nil then
  begin
    if TThread.Current.ThreadID = MainThreadID then
      Engine.CancelConsoleRead(Self)
    else
 TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Engine <> nil then
            Engine.CancelConsoleRead(Self);
        end);
  end;
end;
{ TBeepObject }

constructor TBeepObject.Create(APeriod: Integer);
begin
  inherited Create;
  Period := APeriod;
  if Period = 0 then
    Period := 500;
end;

procedure TBeepObject.DoExecute;
begin
  PlayWaveform(@Sin_Waveform, 1000, Period);
end;

{ TPlayMMLObject }

constructor TPlayMMLObject.Create(ASong: TmmlSong);
begin
  Song := ASong;
end;

procedure TPlayMMLObject.DoExecute;
begin
  PlayMML(Song);
end;

{ TLoadFontObject }

constructor TLoadFontObject.Create(AFileName: string; AFontSize: Integer);
begin
  inherited Create;
  FileName := AFileName;
  FontSize := AFontSize;
end;

procedure TLoadFontObject.DoExecute;
begin
  Res.Font.LoadFromFile(FileName, FontSize);
end;

{ TSetEffectObject }

constructor TSetEffectObject.Create(ACanvas: TTyroCanvas; const AEffectName: string);
begin
  inherited Create;
  Canvas := ACanvas;
  EffectName := AEffectName;
end;

procedure TSetEffectObject.DoExecute;
begin
  if Canvas <> nil then
    Canvas.SetEffect(EffectName);
end;

{ TSetEffectValueObject }

constructor TSetEffectValueObject.Create(ACanvas: TTyroCanvas; AValue: Single);
begin
  inherited Create;
  Canvas := ACanvas;
  Value := AValue;
end;

procedure TSetEffectValueObject.DoExecute;
begin
  if Canvas <> nil then
    Canvas.SetEffectValue(Value);
end;

{ TSetEffectAreaObject }

constructor TSetEffectAreaObject.Create(ACanvas: TTyroCanvas; const AArea: TRectangle);
begin
  inherited Create;
  Canvas := ACanvas;
  Area := AArea;
end;

procedure TSetEffectAreaObject.DoExecute;
begin
  if Canvas <> nil then
    Canvas.SetEffectArea(Area);
end;

{ TLoadShaderObject }

constructor TLoadShaderObject.Create(ACanvas: TTyroCanvas; const AFileName: string);
begin
  inherited Create;
  Canvas := ACanvas;
  FileName := AFileName;
end;

procedure TLoadShaderObject.DoExecute;
begin
  if Canvas <> nil then
    Canvas.LoadCustomEffect(FileName);
end;

{ TPlaySoundObject }

constructor TPlaySoundObject.Create(AFreq, APeriod: Integer; AWaveName: string);
begin
  inherited Create;
  Freq := AFreq;
  Period := APeriod;
  WaveName := AWaveName ;
end;

procedure TPlaySoundObject.DoExecute;
begin
  PlayWaveform(WaveName, Freq, Period);
end;

{ TPlayNoiseObject }

constructor TPlayNoiseObject.Create(AFreq, APeriod: Integer);
begin
  inherited Create;
  Freq := AFreq;
  Period := APeriod;
end;

procedure TPlayNoiseObject.DoExecute;
begin
  if Freq <= 0 then
    PlayWaveform(@WhiteNoise_Waveform, 400, Period)
  else
    PlayWaveform(@Noise_Waveform, Freq, Period);
end;

{ TPlayMusicFileObject }

constructor TPlayMusicFileObject.Create(AFileName: string);
begin
  inherited Create;
  FileName := AFileName;
end;

procedure TPlayMusicFileObject.DoExecute;
begin
  RayLibSound.PlayMusicFile(FileName);
end;

{ TPlaySoundFileObject }

constructor TPlaySoundFileObject.Create(const AFileName: string; AVolume: Single);
begin
  inherited Create;
  FileName := AFileName;
  Volume := AVolume;
end;

procedure TPlaySoundFileObject.DoExecute;
begin
  PlaySoundFile(FileName, Volume);
end;

{ TLoadSoundFileObject }

constructor TLoadSoundFileObject.Create(const AFileName: string);
begin
  inherited Create;
  FileName := AFileName;
end;

procedure TLoadSoundFileObject.DoExecute;
begin
  LoadSoundFile(FileName);
end;

{ TUnloadSoundFileObject }

constructor TUnloadSoundFileObject.Create(const AFileName: string);
begin
  inherited Create;
  FileName := AFileName;
end;

procedure TUnloadSoundFileObject.DoExecute;
begin
  UnloadSoundFile(FileName);
end;

{ TStartLoopSoundObject }

constructor TStartLoopSoundObject.Create(const AWaveName: string; AFrequency, AVolume: Single);
begin
  inherited Create;
  WaveName := AWaveName;
  Frequency := AFrequency;
  Volume := AVolume;
end;

procedure TStartLoopSoundObject.DoExecute;
begin
  StartLoop(WaveName, Frequency, Volume);
end;

{ TStopLoopSoundObject }

procedure TStopLoopSoundObject.DoExecute;
begin
  StopLoop;
end;

{ TSetLoopPitchObject }

constructor TSetLoopPitchObject.Create(AFrequency: Single);
begin
  inherited Create;
  Frequency := AFrequency;
end;

procedure TSetLoopPitchObject.DoExecute;
begin
  SetLoopPitch(Frequency);
end;

{ TSetLoopVolumeObject }

constructor TSetLoopVolumeObject.Create(AVolume: Single);
begin
  inherited Create;
  Volume := AVolume;
end;

procedure TSetLoopVolumeObject.DoExecute;
begin
  SetLoopVolume(Volume);
end;

{ TDrawSetAlphaObject }

constructor TDrawSetAlphaObject.Create(ACanvas: TTyroCanvas; Alpha: Byte);
begin
  inherited Create(ACanvas);
  fAlpha := Alpha;
end;

procedure TDrawSetAlphaObject.DoExecute;
begin
  Canvas.PenAlpha := fAlpha;
end;

{ TDrawSetBackColorObject }

constructor TDrawSetBackColorObject.Create(ACanvas: TTyroCanvas; Color: TColor);
begin
  inherited Create(ACanvas);
  fColor := Color;
end;

procedure TDrawSetBackColorObject.DoExecute;
begin
  Canvas.BackColor := fColor;
end;

{ TQueueObject }

destructor TQueueObject.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

function TQueueObject.EventNeeded: TEvent;
begin
  if FEvent = nil then
    FEvent := TEvent.Create(nil, True, False, '');
  Result := FEvent;
end;

function TQueueObject.Wait(Timeout: Cardinal): Boolean;
begin
  if FEvent = nil then
    Exit(True);

  if Engine = nil then
    Exit(False);
  Result := FEvent.WaitFor(Timeout) = wrSignaled;
  Engine.UnregisterWaiting(Self);
end;

procedure TQueueObject.Cancel;
begin
  TInterlocked.Exchange(FCancelled, 1);
  SetEvent;
end;

function TQueueObject.GetCancelled: Boolean;
begin
  Result := TInterlocked.Add(FCancelled, 0) <> 0;
end;

procedure TQueueObject.Execute;
begin
  if Cancelled then
  begin
    SetEvent;
    Exit;
  end;

  try
    DoExecute;
  finally
    //SetEvent;
  end;
end;

procedure TQueueObject.Run(Thread: TThread);
begin
  if (Thread <> nil) and (Thread.ThreadID <> MainThreadID) then
    TThread.Synchronize(Thread, Execute)
  else
    Execute;
end;

procedure TQueueObject.SetEvent;
begin
  if FEvent <> nil then
    FEvent.SetEvent;
end;

{ TQueueObjects }

procedure TQueueObjects.CancelAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Cancel;
end;

{ TWindowObject }

constructor TWindowObject.Create(W, H: Integer);
begin
  inherited Create;
  FW := W;
  FH := H;
end;

procedure TWindowObject.DoExecute;
begin
  Engine.ShowWindow(FW, FH);
end;

{ TCreateControlObject }

constructor TCreateControlObject.Create(const AClassName: string; const ACaption: utf8string; AX, AY, AW, AH: Integer; const AName: string);
begin
  inherited Create;
  FClassName := AClassName;
  FCaption := ACaption;
  FX := AX;
  FY := AY;
  FW := AW;
  FH := AH;
  FName := AName;
end;

destructor TCreateControlObject.Destroy;
begin
  // Until TakeControl succeeds this helper owns rollback. The helper itself is
  // normally freed by the worker after Synchronize returns, so marshal an
  // abandoned parented control back to the application thread.
  if FExecutionAttempted and (FControl <> nil) and not FTransferred then
  begin
    if TThread.Current.ThreadID = MainThreadID then
      FreeUntransferredControl
    else
      TThread.Synchronize(TThread.CurrentThread, FreeUntransferredControl);
  end;
  inherited;
end;

procedure TCreateControlObject.FreeUntransferredControl;
begin
  FreeAndNil(FControl);
end;

function TCreateControlObject.TakeControl: TTyroControl;
begin
  Result := FControl;
  FTransferred := Result <> nil;
end;

procedure TCreateControlObject.DoExecute;
var
  LName: string;
  NewControl: TTyroControl;
begin
  FExecutionAttempted := True;
  NewControl := nil;
  LName := LowerCase(FClassName);
  if LName = 'button' then
    NewControl := TTyroButton.Create(Engine.Main)
  else if LName = 'panel' then
    NewControl := TTyroPanel.Create(Engine.Main)
  else if LName = 'label' then
    NewControl := TTyroLabel.Create(Engine.Main)
  else if LName = 'checkbox' then
    NewControl := TTyroCheckBox.Create(Engine.Main)
  else if LName = 'edit' then
    NewControl := TTyroEdit.Create(Engine.Main)
  else if LName = 'spectrum' then
    NewControl := TTyroSpectrum.Create(Engine.Main)
  else if LName = 'listbox' then
    NewControl := TTyroListBox.Create(Engine.Main)
  else if LName = 'image' then
    NewControl := TTyroImage.Create(Engine.Main);

  try
    if NewControl <> nil then
    begin
      NewControl.SetText(FCaption);
      if FName <> '' then
        NewControl.Name := FName;
      NewControl.BoundsRect := Rect(FX, FY, FX + FW, FY + FH);
      NewControl.Show;
      //An image has no text: the caption argument names the texture file
      //instead. It is loaded once the bounds are applied, so a texture larger
      //than the requested size grows the control (see TTyroImage.LoadFromFile).
      if (NewControl is TTyroImage) and (FCaption <> '') then
        TTyroImage(NewControl).LoadFromFile(FCaption);
      FControl := NewControl;
      NewControl := nil;
    end;
  finally
    // Configuration failure rolls back while still on the application thread.
    NewControl.Free;
  end;
end;

{ TSetControlBoundsObject }

constructor TSetControlBoundsObject.Create(AControl: TTyroControl; ABoundsRect: TRect);
begin
  inherited Create;
  FControl := AControl;
  FBoundsRect := ABoundsRect;
end;

procedure TSetControlBoundsObject.DoExecute;
begin
  FControl.BoundsRect := FBoundsRect;
end;

{ TSetControlFocusObject }

constructor TSetControlFocusObject.Create(AControl: TTyroControl);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TSetControlFocusObject.DoExecute;
begin
  FControl.Focused := True;
end;

{ TSetControlTextObject }

constructor TSetControlTextObject.Create(AControl: TTyroControl; const AText: string);
begin
  inherited Create;
  FControl := AControl;
  FText := AText;
end;

procedure TSetControlTextObject.DoExecute;
begin
  //a caption control (button/label/checkbox) keeps its text in Caption, an
  //edit in the shared Text, so both spellings end up in the right store
  if FControl is TTyroCaptionControl then
    TTyroCaptionControl(FControl).Caption := FText
  else
    FControl.Text := FText;
end;

{ TSetControlCheckedObject }

constructor TSetControlCheckedObject.Create(AControl: TTyroCheckBox;
  AChecked: Boolean);
begin
  inherited Create;
  FControl := AControl;
  FChecked := AChecked;
end;

procedure TSetControlCheckedObject.DoExecute;
begin
  FControl.Checked := FChecked;
end;

{ TSetControlVisibleObject }

constructor TSetControlVisibleObject.Create(AControl: TTyroControl; AVisible: Boolean);
begin
  inherited Create;
  FControl := AControl;
  FVisible := AVisible;
end;

procedure TSetControlVisibleObject.DoExecute;
begin
  FControl.Visible := FVisible;
end;

{ TSetControlBorderObject }

constructor TSetControlBorderObject.Create(AControl: TTyroControl;
  ABorder: TBorder);
begin
  inherited Create;
  FControl := AControl;
  FBorder := ABorder;
end;

procedure TSetControlBorderObject.DoExecute;
begin
  FControl.Border := FBorder;
end;

{ TSetControlBackColorObject }

constructor TSetControlBackColorObject.Create(AControl: TTyroControl;
  AColor: TColor);
begin
  inherited Create;
  FControl := AControl;
  FColor := AColor;
end;

procedure TSetControlBackColorObject.DoExecute;
begin
  FControl.BackColor := FColor;
end;

{ TSetControlNameObject }

constructor TSetControlNameObject.Create(AControl: TTyroControl;
  const AName: string);
begin
  inherited Create;
  FControl := AControl;
  FName := AName;
end;

procedure TSetControlNameObject.DoExecute;
begin
  FControl.Name := FName;
end;

{ TSetControlAlignObject }

constructor TSetControlAlignObject.Create(AControl: TTyroControl;
  AAlign: TAlign);
begin
  inherited Create;
  FControl := AControl;
  FAlign := AAlign;
end;

procedure TSetControlAlignObject.DoExecute;
begin
  FControl.Align := FAlign;
end;

{ TSetControlParentObject }

constructor TSetControlParentObject.Create(AControl: TTyroControl;
  AParent: TTyroLayout);
begin
  inherited Create;
  FControl := AControl;
  FParent := AParent;
end;

procedure TSetControlParentObject.DoExecute;
begin
  FControl.Parent := FParent;
end;

{ TSetControlItemsObject }

constructor TSetControlItemsObject.Create(AControl: TTyroControl; AItems: TStringList);
begin
  inherited Create;
  FControl := AControl;
  FItems := TStringList.Create;
  FItems.Assign(AItems);
end;

destructor TSetControlItemsObject.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TSetControlItemsObject.DoExecute;
begin
  TTyroListBox(FControl).Items := FItems;
end;

{ TSetControlItemObject }

constructor TSetControlItemObject.Create(AControl: TTyroControl; AIndex: Integer; const AText: utf8string);
begin
  inherited Create;
  FControl := AControl;
  FIndex := AIndex;
  FText := AText;
end;

procedure TSetControlItemObject.DoExecute;
begin
  if (FIndex >= 0) and (FIndex < TTyroListBox(FControl).Items.Count) then
    TTyroListBox(FControl).Items[FIndex] := FText;
end;

{ TAddControlItemObject }

constructor TAddControlItemObject.Create(AControl: TTyroControl; const AText: utf8string);
begin
  inherited Create;
  FControl := AControl;
  FText := AText;
end;

procedure TAddControlItemObject.DoExecute;
begin
  TTyroListBox(FControl).AddItem(FText);
end;

{ TClearControlItemsObject }

constructor TClearControlItemsObject.Create(AControl: TTyroControl);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TClearControlItemsObject.DoExecute;
begin
  TTyroListBox(FControl).Clear;
end;

{ TSetControlViewCountObject }

constructor TSetControlViewCountObject.Create(AControl: TTyroControl; AViewCount: Integer);
begin
  inherited Create;
  FControl := AControl;
  FViewCount := AViewCount;
end;

procedure TSetControlViewCountObject.DoExecute;
begin
  TTyroListBox(FControl).ViewCount := FViewCount;
end;

{ TSetControlItemHeightObject }

constructor TSetControlItemHeightObject.Create(AControl: TTyroControl);
begin
  inherited Create;
  FControl := AControl;
end;

procedure TSetControlItemHeightObject.DoExecute;
begin
  //TTyroListBox(FControl)
end;

{ TSetControlItemIndexObject }

constructor TSetControlItemIndexObject.Create(AControl: TTyroControl; AItemIndex: Integer);
begin
  inherited Create;
  FControl := AControl;
  FItemIndex := AItemIndex;
end;

procedure TSetControlItemIndexObject.DoExecute;
begin
  TTyroListBox(FControl).ItemIndex := FItemIndex;
end;

{ TSetControlPlaceHolderObject }

constructor TSetControlPlaceHolderObject.Create(AControl: TTyroControl; const APlaceHolder: string);
begin
  inherited Create;
  FControl := AControl;
  FPlaceHolder := APlaceHolder;
end;

procedure TSetControlPlaceHolderObject.DoExecute;
begin
  if FControl is TTyroEdit then
    TTyroEdit(FControl).PlaceHolder := FPlaceHolder
  else if FControl is TTyroListBox then
    TTyroListBox(FControl).PlaceHolder := FPlaceHolder;
end;

{ TLoadControlImageObject }

constructor TLoadControlImageObject.Create(AControl: TTyroControl; const AFileName: string);
begin
  inherited Create;
  FControl := AControl;
  FFileName := AFileName;
end;

procedure TLoadControlImageObject.DoExecute;
begin
  if FControl is TTyroImage then
    TTyroImage(FControl).LoadFromFile(FFileName);
end;

{ TDrawSetColorObject }

constructor TDrawSetColorObject.Create(ACanvas: TTyroCanvas; Color: TColor);
begin
  inherited Create(ACanvas);
  fColor := Color;
end;

procedure TDrawSetColorObject.DoExecute;
begin
  Canvas.Color := fColor;
end;

{ TScriptTypes }

function TScriptTypes.FindByExtension(Extension: string): TScriptType;
var
  itm: TScriptType;
  s: string;
begin
  //Return the first type that claims the extension: a plain 'break' here would
  //only leave the inner loop, so a later duplicate would overwrite the match.
  Result := nil;
  for itm in Self do
    for s in itm.Extentions do
      if SameText(s, Extension) then
        Exit(itm);
end;

procedure TScriptTypes.RegisterLanguage(ATitle: string; AExtentions: TArray<string>; AScriptClass: TTyroScriptClass);
var
  Item: TScriptType;
begin
  Item := TScriptType.Create;
  Item.Title := ATitle;
  Item.Extentions := AExtentions;
  Item.ScriptClass := AScriptClass;
  Add(Item);
end;

{ TDrawPointObject }

constructor TDrawPointObject.Create(ACanvas: TTyroCanvas; X, Y: Integer);
begin
  inherited Create(ACanvas);
  fX := X;
  fY := Y;
end;

procedure TDrawPointObject.DoExecute;
begin
  Canvas.DrawPixel(fX, fY, Canvas.Color);
end;

{ TSlowDrawObject }

constructor TSlowDrawObject.Create(ACanvas: TTyroCanvas; AMillis, X, Y, W, H: Integer; Fill: Boolean);
begin
  inherited Create(ACanvas);
  fMillis := AMillis;
  fX := X;
  fY := Y;
  fW := W;
  fH := H;
  fFill := Fill;
end;

procedure TSlowDrawObject.DoExecute;
var
  t0: Double;
begin
  //A real (expensive) draw: the marker first, then burn about fMillis on this
  //main thread, the same place every scripted draw command runs. The frame that
  //drains the group therefore takes longer than one frame, which is exactly the
  //case a split would show up in.
  if fFill then
    Canvas.FillRectangle(fX, fY, fW, fH, Canvas.Color)
  else
    Canvas.DrawRectangle(fX, fY, fW, fH, Canvas.Color);
  if fMillis > 0 then
  begin
    t0 := GetTime();
    while (GetTime() - t0) * 1000 < fMillis do
      ; //busy spin: deliberate, deterministic, no scheduler round-trip
  end;
end;

{ TDrawLineToObject }

constructor TDrawLineToObject.Create(ACanvas: TTyroCanvas; X, Y: Integer);
begin
  inherited Create(ACanvas);
  fX := X;
  fY := Y;
end;

procedure TDrawLineToObject.DoExecute;
begin
  Canvas.DrawLineTo(fX, fY, Canvas.Color);
end;

{ TDrawLIneObject }

constructor TDrawLineObject.Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2: Integer);
begin
  inherited Create(ACanvas);
  fX1 := X1;
  fY1 := Y1;
  fX2 := X2;
  fY2 := Y2;
  fPen := 0;
end;

constructor TDrawLineObject.Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2, APen: Integer);
begin
  Create(ACanvas, X1, Y1, X2, Y2);
  fPen := APen;
end;

procedure TDrawLineObject.DoExecute;
var
  OldPen: Integer;
begin
  if fPen > 0 then
  begin
    OldPen := Canvas.Pen;
    Canvas.Pen := fPen;
    Canvas.DrawLine(fX1, fY1, fX2, fY2, Canvas.Color);
    Canvas.Pen := OldPen;
  end
  else
    Canvas.DrawLine(fX1, fY1, fX2, fY2, Canvas.Color);
end;

{ TDrawTriangleObject }

constructor TDrawTriangleObject.Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2, X3, Y3: Integer; Fill: Boolean);
begin
  inherited Create(ACanvas);
  fX1 := X1;
  fY1 := Y1;
  fX2 := X2;
  fY2 := Y2;
  fX3 := X3;
  fY3 := Y3;
  fFill := Fill;
end;

procedure TDrawTriangleObject.DoExecute;
begin
  if fFill then
    Canvas.FillTriangle(fX1, fY1, fX2, fY2, fX3, fY3, Canvas.Color)
  else
    Canvas.DrawTriangle(fX1, fY1, fX2, fY2, fX3, fY3, Canvas.Color);
end;

{ TDrawSetPenObject }

constructor TDrawSetPenObject.Create(ACanvas: TTyroCanvas; APen: Integer);
begin
  inherited Create(ACanvas);
  fPen := APen;
end;

procedure TDrawSetPenObject.DoExecute;
begin
  if fPen < 1 then
    fPen := 1;
  Canvas.Pen := fPen;
end;

{ TClearObject }

constructor TClearObject.Create(ACanvas: TTyroCanvas);
begin
  inherited Create(ACanvas);
  fHasColor := False;
end;

constructor TClearObject.Create(ACanvas: TTyroCanvas; AColor: TColor);
begin
  inherited Create(ACanvas);
  fColor := AColor;
  fHasColor := True;
end;

procedure TClearObject.DoExecute;
begin
  if fHasColor then
    Canvas.ClearBackground(fColor)
  else
    Canvas.ClearBackground(Canvas.BackColor);
end;

{ TDrawRectangleObject }

constructor TDrawRectangleObject.Create(ACanvas: TTyroCanvas; X, Y, W, H: Integer; Fill: Boolean);
begin
  inherited Create(ACanvas);
  fX := X;
  fY := Y;
  fW := W;
  fH := H;
  fFill := Fill;
end;

procedure TDrawRectangleObject.DoExecute;
begin
  if fFill then
    Canvas.FillRectangle(fX, fY, fW, fH, Canvas.Color)
  else
    Canvas.DrawRectangle(fX, fY, fW, fH, Canvas.Color);
end;

{ TPrintObject }

constructor TPrintObject.Create(Text: String; NewLine: Boolean);
begin
  inherited Create;
  fText := Text;
  fNewLine := NewLine;
end;

procedure TPrintObject.DoExecute;
begin
  if FNewLine then
    Log.WriteLn(FText)
  else
    Log.Write(FText);
end;

{ TOutputPrintObject }

constructor TOutputPrintObject.Create(const Text: String; NewLine: Boolean);
begin
  inherited Create;
  FText := Text;
  FNewLine := NewLine;
end;

procedure TOutputPrintObject.DoExecute;
begin
  if FNewLine then
    Engine.Output.Writeln(FText)
  else
    Engine.Output.Write(FText);
end;

{ TDrawTextObject }

constructor TDrawTextObject.Create(ACanvas: TTyroCanvas; X, Y: Integer; Text: String);
begin
  inherited Create(ACanvas);
  fX := X;
  fY := Y;
  fText := Text;
end;

procedure TDrawTextObject.DoExecute;
begin
  Canvas.DrawText(fX, fY, fText, Canvas.Color);
end;

{ TDrawObject }

procedure TDrawObject.Created;
begin
end;

constructor TDrawObject.Create(ACanvas: TTyroCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

procedure TDrawObject.DoExecute;
begin
  if (Canvas = nil) then
    Log.WriteLn('You need to init window to use this command, ' + ClassName + ' line: ' + IntToStr(LineNo))
  else
    inherited;
end;

{ TDrawCircleObject }

constructor TDrawCircleObject.Create(ACanvas: TTyroCanvas; X, Y, R: Integer; Fill: Boolean);
begin
  inherited Create(ACanvas);
  fX := X;
  fY := Y;
  fR := R;
  fFill := Fill;
end;

procedure TDrawCircleObject.DoExecute;
begin
  if fFill then
    Canvas.FillCircle(fX, fY, fR, Canvas.Color)
  else
    Canvas.DrawCircle(fX, fY, fR, Canvas.Color);
end;

{ TTyroScript }

function TTyroScript.GetActive: Boolean;
begin
  Result := TInterlocked.Add(FActive, 0) <> 0;
end;

function TTyroScript.GetStarted: Boolean;
begin
  Result := TInterlocked.Add(FStarted, 0) <> 0;
end;

function TTyroScript.GetStopped: Boolean;
begin
  Result := TInterlocked.Add(FStopRequested, 0) <> 0;
end;

function TTyroScript.GetHandoff: Boolean;
begin
  Result := TInterlocked.Add(FHandoff, 0) <> 0;
end;

procedure TTyroScript.SetHandoff(AValue: Boolean);
begin
  if AValue then
    TInterlocked.Exchange(FHandoff, 1)
  else
    TInterlocked.Exchange(FHandoff, 0);
end;

procedure TTyroScript.ExecuteQueueObject;
begin
  QueueObject.Execute;
  FreeAndNil(QueueObject);
end;

procedure TTyroScript.RunQueueObject(AQueueObject: TQueueObject);
begin
  QueueObject := AQueueObject;
  if Thread <> nil then
    Thread.Synchronize(ExecuteQueueObject)
  else
    ExecuteQueueObject;
end;

procedure TTyroScript.ExecuteQueueObjectNoFree;
begin
  QueueObject.Execute;
  QueueObject := nil;
end;

procedure TTyroScript.RunQueueObjectNoFree(AQueueObject: TQueueObject);
begin
  QueueObject := AQueueObject;
  if Thread <> nil then
    Thread.Synchronize(ExecuteQueueObjectNoFree)
  else
    ExecuteQueueObjectNoFree;
end;

procedure TTyroScript.BeforeRun;
begin
end;

procedure TTyroScript.Init;
begin
end;

procedure TTyroScript.AfterRun;
begin
end;

procedure TTyroScript.AddQueueObject(AQueueObject: TQueueObject);
var
  Admitted: Boolean;
begin
  if AQueueObject = nil then
    Exit;
  Admitted := False;
  Lock.Enter;
  try
    // A stopped script must not leave Engine-thread work behind for a later
    // interactive run. Ownership remains here when the request is rejected.
    // An engine-driven script (Handoff) is admitted even though Active is
    // already False: the engine keeps calling its update()/draw() on the main
    // thread, and those callbacks keep queueing drawing commands.
    if (GetActive or GetHandoff) and (Engine <> nil) and Engine.Running then
    begin
      //Stamp the enqueue time (before the queue lock is contended) so the
      //engine can report how long the script stalled between two commands
      //(queue.lastgap/maxgap); see TTyroEngine.NoteEnqueue.
      Engine.NoteEnqueue;
      Engine.Queue.Add(AQueueObject);
      AQueueObject := nil;
      Admitted := True;
    end;
  finally
    Lock.Leave;
    AQueueObject.Free;
  end;
  if Admitted then
    //Back-pressure: if the engine has an admission limit (Lua queuelimit) and
    //the backlog is at it, wait here for the main loop to drain some. Called
    //AFTER the global Lock is released: ProcessQueue needs that same lock to
    //extract, so waiting under it would stall the very consumer we await.
    Engine.ThrottleQueue;
  if Thread <> nil then
    Thread.Yield;
end;

constructor TTyroScript.Create(ASource: string);
begin
  inherited Create;
  TInterlocked.Exchange(FActive, 1);
  //A script that returns normally may hand its state to the engine for
  //update()/draw() callbacks. KeepAlive only enables the notification; the
  //engine disposes the state when no callbacks are present (see
  //TTyroScript.HasEngineCallbacks).
  FKeepAlive := True;
  FSource := TStringList.Create;
  FSource.Text := ASource;
  Init;
end;

destructor TTyroScript.Destroy;
begin
  FreeAndNil(FSource);
  inherited Destroy;
end;

procedure TTyroScript.Stop;
begin
  TInterlocked.Exchange(FStopRequested, 1);
  //A stop ends the run for good: drop the handoff so a late callback frame (or
  //a queued command) is not admitted after exit()/Stop.
  TInterlocked.Exchange(FHandoff, 0);
  TInterlocked.Exchange(FActive, 0);
end;

procedure TTyroScript.Start;
begin
  TInterlocked.Exchange(FStarted, 1);
  TInterlocked.Exchange(FStopRequested, 0);
  TInterlocked.Exchange(FHandoff, 0);
  TInterlocked.Exchange(FActive, 1);
  try
    BeforeRun;
    Run;
    AfterRun;
  finally
    TInterlocked.Exchange(FActive, 0);
  end;
end;

procedure TTyroScript.LoadFile(AFileName: string);
begin
  Source.LoadFromFile(AFileName);
  //Must not be named FileName: that parameter would shadow the property of
  //the same name and the field would keep its initial empty value.
  FileName := AFileName;
end;

procedure TTyroScript.LoadString(ASource: string);
begin
  Source.Text := ASource;
end;

function TTyroScript.RunLine(const ALine: string; out AOutput: string): Boolean;
begin
  Result := False;
  AOutput := '';
end;

function TTyroScript.HasEngineCallbacks: Boolean;
begin
  Result := False;
end;

procedure TTyroScript.EngineUpdate(ADeltaTime: Double);
begin
end;

procedure TTyroScript.EngineDraw;
begin
end;

initialization
  ScriptTypes := TScriptTypes.Create;
finalization
  FreeAndNil(ScriptTypes);
end.

