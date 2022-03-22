unit TyroScripts;

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  mnUtils, mnClasses, mnLogs, mnRTTIUtils,
  RayLib, RayClasses,
  Melodies, TyroSounds,
  TyroClasses;

type
  { TQueueObject }

  TQueueObject = class abstract(TObject)
  private
  protected
    procedure DoExecute; virtual; abstract;
  public
    LineNo: Integer;
    procedure Execute; virtual;
  end;

  { TQueueObjects }

  TQueueObjects = class(TmnObjectList<TQueueObject>)
  public
  end;

  { TDrawObject }

  TDrawObject = class abstract(TQueueObject)
  private
    FCanvas: TTyroCanvas;
  protected
    procedure Created; virtual;
  public
    constructor Create(ACanvas: TTyroCanvas);
    procedure Execute; override;
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
    fW, fH: Integer;
    constructor Create(W, H: Integer);
    procedure DoExecute; override;
  end;

  { TDrawSetColorObject }

  TDrawSetColorObject = class(TDrawObject)
  public
    fColor: TColor;
    constructor Create(ACanvas: TTyroCanvas; Color: TColor);
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
    constructor Create(ACanvas: TTyroCanvas; Text: String);
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
    FActive: Boolean;
    FStarted: Boolean;
    FAssetsFolder: string;
    function GetActive: Boolean;
    procedure ExecuteQueueObject; //this for sync do not call it
  protected
    QueueObject: TQueueObject;
    ScriptThread: TTyroScriptThread;
    ScriptText: TStringList;

    procedure RunQueueObject(AQueueObject: TQueueObject);
    procedure AddQueueObject(AQueueObject: TQueueObject); virtual;

    procedure BeforeRun; virtual;
    procedure Run; virtual; abstract;
    procedure AfterRun; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Stop; virtual;
    procedure Start; virtual;
    procedure LoadFile(FileName: string); overload;
    property AssetsFolder: string read FAssetsFolder write FAssetsFolder;
    property Active: Boolean read GetActive;
    property Started: Boolean read FStarted; //started true even after stopped
  end;

  { TTyroScriptThread }

  TTyroScriptThread = class(TThread)
  private
    FStarted: Boolean;
    function GetActive: Boolean;
  protected
    Script: TTyroScript;
    procedure TerminatedSet; override;
  public
    procedure Execute; override;
    constructor Create(AScript: TTyroScript); virtual;
    destructor Destroy; override;
    procedure Stop;
    property Started: Boolean read FStarted;
    property Active: Boolean read GetActive;
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
  Script.Stop;
  inherited;
end;

procedure TTyroScriptThread.Execute;
begin
  FStarted := True;
  Script.Start;
end;

constructor TTyroScriptThread.Create(AScript: TTyroScript);
begin
  inherited Create(True);
  Script := AScript;
  Script.ScriptThread := Self;
  FreeOnTerminate := False;
  Priority := tpLower; //hmmm
end;

destructor TTyroScriptThread.Destroy;
begin
  FreeAndNil(Script);
  inherited Destroy;
end;

procedure TTyroScriptThread.Stop;
begin
  Terminate;
end;

{ TScriptType }

function TScriptType.CollectExtentions: string;
begin
  Result := CollectStrings(Extentions, ',');
end;

{ TShowConsoleObject }

constructor TShowConsoleObject.Create(W, H: Integer);
begin
  inherited Create;
  FW := W;
  FH := H;
end;

procedure TShowConsoleObject.DoExecute;
begin
  Main.ShowConsole(FW, FH);
end;
{ TBeepObject }

constructor TBeepObject.Create;
begin
  inherited Create;
end;

procedure TBeepObject.DoExecute;
begin
  PlayWaveform(440, 1);
end;

{ TPlayMMLObject }

constructor TPlayMMLObject.Create(ASong: TmmlSong);
begin
  Song := ASong;
end;

procedure TPlayMMLObject.DoExecute;
var
  Melody: TRayMelody;
begin
  Melody := TRayMelody.Create;
  Melody.Play(Song);
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

procedure TQueueObject.Execute;
begin
  DoExecute;
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

constructor TPrintObject.Create(ACanvas: TTyroCanvas; Text: String);
begin
  inherited Create(ACanvas);
  fText := Text;
end;

procedure TPrintObject.DoExecute;
begin
  Main.Console.Writeln(fText);
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
  Canvas.DrawText(fX, fY, fText);
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

procedure TDrawObject.Execute;
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
  Result := FActive;
end;

procedure TTyroScript.ExecuteQueueObject;
begin
  QueueObject.Execute;
  FreeAndNil(QueueObject);
end;

procedure TTyroScript.RunQueueObject(AQueueObject: TQueueObject);
begin
  QueueObject := AQueueObject;
  if ScriptThread <> nil then
    ScriptThread.Synchronize(ExecuteQueueObject)
  else
    ExecuteQueueObject;
end;

procedure TTyroScript.BeforeRun;
begin
end;

procedure TTyroScript.AfterRun;
begin
end;

procedure TTyroScript.AddQueueObject(AQueueObject: TQueueObject);
begin
  Lock.Enter;
  try
    Main.Queue.Add(AQueueObject);
  finally
    Lock.Leave;
  end;
  if ScriptThread <> nil then
    ScriptThread.Yield;
end;

constructor TTyroScript.Create;
begin
  inherited Create;
  FActive := True;
  ScriptText := TStringList.Create;
end;

destructor TTyroScript.Destroy;
begin
  FreeAndNil(ScriptText);
  inherited Destroy;
end;

procedure TTyroScript.Stop;
begin
  FActive := False;
end;

procedure TTyroScript.Start;
begin
  FStarted := True;
  FActive := True;
  BeforeRun;
  Run;
  AfterRun;
  FActive := False;
end;

procedure TTyroScript.LoadFile(FileName: string);
begin
  ScriptText.LoadFromFile(FileName);
end;

end.

