unit TyroScripts;

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, SyncObjs,
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
    procedure HandleConsoleInput(AConsole: TTyroTerminal; AInput: string);
  end;

   { TCreateControlObject }

   { Creates a control by class name on the main thread (it is painted by the
     main drawing cycle every frame) and returns the created control. Dispatch
     happens on the main thread so controls always belong to the control tree.
     Supported classes: 'button', 'panel', 'label', 'checkbox', 'edit',
     'spectrum', 'listbox'. }
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

  TSetControlTextObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FText: utf8string;
  public
    constructor Create(AControl: TTyroControl; const AText: utf8string);
    procedure DoExecute; override;
  end;

  TSetControlCheckedObject = class(TQueueObject)
  private
    FControl: TTyroControl;
    FChecked: Boolean;
  public
    constructor Create(AControl: TTyroControl; AChecked: Boolean);
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

  { TDrawSetAlphaObject }

  TDrawSetAlphaObject = class(TDrawObject)
  public
    fAlpha: Byte;
    constructor Create(ACanvas: TTyroCanvas; Alpha: Byte);
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
    constructor Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2: Integer);
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

  { TDrawTextObject }

  TDrawTextObject = class(TDrawObject)
  public
    fX, fY: Integer;
    fText: String;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer; Text: String);
    procedure DoExecute; override;
  end;

  { TPrintObject }

  TPrintObject = class(TDrawObject)
  public
    FText: String;
    FNewLine: Boolean;
    constructor Create(ACanvas: TTyroCanvas; Text: String; NewLine: Boolean);
    procedure DoExecute; override;
  end;

  { TOutputPrintObject - writes a copy of the script output (print/println)
    to the engine's Output control. }

  TOutputPrintObject = class(TDrawObject)
  public
    FText: String;
    FNewLine: Boolean;
    constructor Create(ACanvas: TTyroCanvas; Text: String; NewLine: Boolean);
    procedure DoExecute; override;
  end;


  { TBeepObject }

  TBeepObject = class(TQueueObject)
  public
    constructor Create;
    procedure DoExecute; override;
  end;

  { TPlaySoundObject }

  TPlaySoundObject = class(TQueueObject)
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

   { TPlayMMLObject }

   TPlayMMLObject = class(TQueueObject)
   public
     Song: TmmlSong;
     constructor Create(ASong: TmmlSong);
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
    constructor Create(ACanvas: TTyroCanvas);
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
    FStarted: LongInt;
    FPath: string;
    FFileName: string;
    function GetActive: Boolean;
 function GetStarted: Boolean;
    procedure ExecuteQueueObject; //this for sync do not call it
    procedure ExecuteQueueObjectNoFree; //this for sync do not call it
  protected
    QueueObject: TQueueObject;
    Thread: TTyroScriptThread;
    ScriptText: TStringList;
    //* Error message of the most recent main-chunk execution, or '' when it
    //* finished without error. Written on the worker thread, published before
    //* the thread marks itself Completed, read once by the main thread during
    //* the --exit lifecycle. Not used by the interactive REPL (RunLine).
    FLastError: string;

    procedure RunQueueObject(AQueueObject: TQueueObject);
    procedure RunQueueObjectNoFree(AQueueObject: TQueueObject);
    procedure AddQueueObject(AQueueObject: TQueueObject); virtual;

    procedure BeforeRun; virtual;
    procedure Run; virtual; abstract;
    procedure AfterRun; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Stop; virtual;
    procedure Start; virtual;
    procedure LoadFile(AFileName: string); overload;

    //Execute a single line of source on the persistent script state so globals
    //(e.g. x = 5) survive across lines; used by the console REPL. AOutput is
    //the error message when Result = False.
    function RunLine(const ALine: string; out AOutput: string): Boolean; virtual;
    property Path: string read FPath write FPath;
    property FileName: string read FFileName write FFileName;
    property Active: Boolean read GetActive;
    property Started: Boolean read GetStarted; //started true even after stopped
    property LastError: string read FLastError; //'' when the last run was clean
    property Source: TStringList read ScriptText; //the loaded script lines
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
  public
    procedure Execute; override;
    procedure Start; reintroduce;
    constructor Create(AScript: TTyroScript); virtual;
    destructor Destroy; override;
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
    function FindByExtension(Extension: string): TScriptType;
  end;

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
begin
  try
    Script.Start;
  finally
    // TThread.Finished is a plain Boolean written by the worker. Publish
    // completion atomically for lifecycle decisions made by the main thread.
    InterlockedExchange(FCompleted, 1);
  end;
end;

procedure TTyroScriptThread.Start;
begin
  if InterlockedCompareExchange(FStarted, 1, 0) <> 0 then
    Exit;
  // Record the resume request synchronously. Setting this in Execute leaves a
  // race where Stop sees an apparently unstarted thread that is already live.
  try
    inherited Start;
  except
    InterlockedExchange(FStarted, 0);
    raise;
  end;
end;

function TTyroScriptThread.GetStarted: Boolean;
begin
  Result := InterlockedExchangeAdd(FStarted, 0) <> 0;
end;

function TTyroScriptThread.GetCompleted: Boolean;
begin
  Result := InterlockedExchangeAdd(FCompleted, 0) <> 0;
end;

constructor TTyroScriptThread.Create(AScript: TTyroScript);
begin
  inherited Create(True);
  FScript := AScript;
  Script.Thread := Self;
  FreeOnTerminate := False;
  Priority := tpLower; //hmmm
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
  Main.ShowConsole(fX, fY, fW, fH);
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
  Main.Output.Visible := True;
  Main.Output.BringToFront;
  if (fW > 0) and (fH > 0) then
    Main.Output.BoundsRect := Rect(fX, fY, fX + fW, fY + fH)
  else if (fX <> 0) or (fY <> 0) then
    Main.Output.BoundsRect := Rect(fX, fY, fX + Main.Output.Width, fY + Main.Output.Height);
end;

{ THideOutputObject }

procedure THideOutputObject.DoExecute;
begin
  Main.Output.Visible := False;
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
  if Cancelled then
  begin
    SetEvent;
    Exit;
  end;
  // Register before exposing the callback. Otherwise shutdown can happen
  // after Synchronize returns but before the script thread enters Wait.
  if Main.RegisterWaiting(Self) then
    Main.StartConsoleReadEx(HandleConsoleInput)
  else
    SetEvent;
end;

procedure TReadConsoleObject.HandleConsoleInput(AConsole: TTyroTerminal; AInput: string);
begin
  ResultString := AInput;
  // Re-arm for built-in command mode
  Main.StartConsoleRead;
  SetEvent;
end;

procedure TReadConsoleObject.Cancel;
begin
  inherited;
  // The terminal stores this object's method pointer. Restore the normal
  // command callback before destruction. The reader normally dies on the
  // script worker, so marshal detachment to the application thread.
  if Main <> nil then
  begin
    if GetCurrentThreadID = MainThreadID then
      Main.CancelConsoleRead(Self)
    else
 TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Main <> nil then
            Main.CancelConsoleRead(Self);
        end);
  end;
end;
{ TBeepObject }

constructor TBeepObject.Create;
begin
  inherited Create;
end;

procedure TBeepObject.DoExecute;
begin
  PlayWaveform(440, 1000);
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
  Resources.Font.LoadFromFile(FileName, FontSize);
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

constructor TPlaySoundObject.Create(AFreq, APeriod: Integer);
begin
  inherited Create;
  Freq := AFreq;
  Period := APeriod;
end;

procedure TPlaySoundObject.DoExecute;
begin
  PlayWaveform(Freq, Period);
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

  if Main = nil then
    Exit(False);
  Result := FEvent.WaitFor(Timeout) = wrSignaled;
  Main.UnregisterWaiting(Self);
end;

procedure TQueueObject.Cancel;
begin
  InterlockedExchange(FCancelled, 1);
  SetEvent;
end;

function TQueueObject.GetCancelled: Boolean;
begin
  Result := InterlockedExchangeAdd(FCancelled, 0) <> 0;
end;

procedure TQueueObject.Execute;
begin
  if Cancelled then
    Exit;
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
  Main.ShowWindow(FW, FH);
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
    if GetCurrentThreadID = MainThreadID then
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
    NewControl := TTyroButton.Create(Main)
  else if LName = 'panel' then
    NewControl := TTyroPanel.Create(Main)
  else if LName = 'label' then
    NewControl := TTyroLabel.Create(Main)
  else if LName = 'checkbox' then
    NewControl := TTyroCheckBox.Create(Main)
  else if LName = 'edit' then
    NewControl := TTyroEdit.Create(Main)
  else if LName = 'spectrum' then
    NewControl := TTyroSpectrum.Create(Main)
  else if LName = 'listbox' then
    NewControl := TTyroListBox.Create(Main);
  try
    if NewControl <> nil then
    begin
      NewControl.SetText(FCaption);
      if FName <> '' then
        NewControl.Name := FName;
      NewControl.BoundsRect := Rect(FX, FY, FX + FW, FY + FH);
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

constructor TSetControlTextObject.Create(AControl: TTyroControl;
  const AText: utf8string);
begin
  inherited Create;
  FControl := AControl;
  FText := AText;
end;

procedure TSetControlTextObject.DoExecute;
begin
  FControl.SetText(FText);
end;

{ TSetControlCheckedObject }

constructor TSetControlCheckedObject.Create(AControl: TTyroControl;
  AChecked: Boolean);
begin
  inherited Create;
  FControl := AControl;
  FChecked := AChecked;
end;

procedure TSetControlCheckedObject.DoExecute;
begin
  FControl.SetChecked(FChecked);
end;

{ TSetControlVisibleObject }

constructor TSetControlVisibleObject.Create(AControl: TTyroControl;
  AVisible: Boolean);
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

{ TDrawSetColorObject }

constructor TDrawSetColorObject.Create(ACanvas: TTyroCanvas; Color: TColor);
begin
  inherited Create(ACanvas);
  fColor := Color;
end;

procedure TDrawSetColorObject.DoExecute;
begin
  Canvas.PenColor := fColor;
end;

{ TScriptTypes }

function TScriptTypes.FindByExtension(Extension: string): TScriptType;
var
  itm: TScriptType;
  s: string;
begin
  Result := nil;
  for itm in Self do
  begin
    for s in itm.Extentions do
      if SameText(s, Extension) then
      begin
        Result := itm;
        break;
      end;
  end;
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
  Canvas.DrawPixel(fX, fY, Canvas.PenColor);
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
  Canvas.DrawLineTo(fX, fY, Canvas.PenColor);
end;

{ TDrawLIneObject }

constructor TDrawLineObject.Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2: Integer);
begin
  inherited Create(ACanvas);
  fX1 := X1;
  fY1 := Y1;
  fX2 := X2;
  fY2 := Y2;
end;

procedure TDrawLineObject.DoExecute;
begin
  Canvas.DrawLine(fX1, fY1, fX2, fY2, Canvas.PenColor);
end;

{ TClearObject }

constructor TClearObject.Create(ACanvas: TTyroCanvas);
begin
  inherited Create(ACanvas);
end;

procedure TClearObject.DoExecute;
begin
  ClearBackground(Canvas.BackColor);
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
  Canvas.DrawRectangle(fX, fY, fW, fH, Canvas.PenColor, fFill)
end;

{ TPrintObject }

constructor TPrintObject.Create(ACanvas: TTyroCanvas; Text: String; NewLine: Boolean);
begin
  inherited Create(ACanvas);
  fText := Text;
  fNewLine := NewLine;
end;

procedure TPrintObject.DoExecute;
begin
  if FNewLine then
    Main.Console.Writeln(FText)
  else
    Main.Console.Write(FText);
end;

{ TOutputPrintObject }

constructor TOutputPrintObject.Create(ACanvas: TTyroCanvas; Text: String; NewLine: Boolean);
begin
  inherited Create(ACanvas);
  FText := Text;
  FNewLine := NewLine;
end;

procedure TOutputPrintObject.DoExecute;
begin
  if FNewLine then
    Main.Output.Writeln(FText)
  else
    Main.Output.Write(FText);
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
  Canvas.DrawText(fX, fY, fText, Canvas.PenColor);
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
    inherited Execute;
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
  Canvas.DrawCircle(fX, fY, fR, Canvas.PenColor, fFill);
end;

{ TTyroScript }

function TTyroScript.GetActive: Boolean;
begin
  Result := InterlockedExchangeAdd(FActive, 0) <> 0;
end;

function TTyroScript.GetStarted: Boolean;
begin
  Result := InterlockedExchangeAdd(FStarted, 0) <> 0;
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

procedure TTyroScript.AfterRun;
begin
end;

procedure TTyroScript.AddQueueObject(AQueueObject: TQueueObject);
begin
  if AQueueObject = nil then
    Exit;
  Lock.Enter;
  try
    // A stopped script must not leave main-thread work behind for a later
    // interactive run. Ownership remains here when the request is rejected.
    if GetActive and (Main <> nil) and Main.Running then
    begin
      Main.Queue.Add(AQueueObject);
      AQueueObject := nil;
    end;
  finally
    Lock.Leave;
    AQueueObject.Free;
  end;
  if Thread <> nil then
    Thread.Yield;
end;

constructor TTyroScript.Create;
begin
  inherited Create;
  InterlockedExchange(FActive, 1);
  ScriptText := TStringList.Create;
end;

destructor TTyroScript.Destroy;
begin
  FreeAndNil(ScriptText);
  inherited Destroy;
end;

procedure TTyroScript.Stop;
begin
  InterlockedExchange(FActive, 0);
end;

procedure TTyroScript.Start;
begin
  InterlockedExchange(FStarted, 1);
  InterlockedExchange(FActive, 1);
  try
    BeforeRun;
    Run;
    AfterRun;
  finally
    InterlockedExchange(FActive, 0);
  end;
end;

procedure TTyroScript.LoadFile(AFileName: string);
begin
  ScriptText.LoadFromFile(AFileName);
  Path := ExtractFilePath(AFileName);
  //Must not be named FileName: that parameter would shadow the property of
  //the same name and the field would keep its initial empty value.
  FileName := ExtractFileName(AFileName);
end;

function TTyroScript.RunLine(const ALine: string; out AOutput: string): Boolean;
begin
  Result := False;
  AOutput := '';
end;

end.

