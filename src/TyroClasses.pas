unit TyroClasses;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fgl, mnLogs,
  FPImage, FPCanvas,
  Melodies, RayClasses, TyroSounds, RayLib;

type
  TMyFPMemoryImage = class(TFPMemoryImage)
  public
    property Data: PFPIntegerArray read FData;
  end;

  TQueueObject = class;
  TTyroCanvas = class;

  { TTyroScript }

  TTyroScript = class abstract(TThread)
  private
    FActive: Boolean;
    FAssetsFolder: string;
    function GetActive: Boolean;
  protected
    ScriptText: TStringList;
    procedure BeforeRun; virtual;
    procedure Run; virtual; abstract;
    procedure AfterRun; virtual;
    procedure AddQueueObject(AQueueObject: TQueueObject); virtual;
    procedure TerminatedSet; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; override;
    procedure LoadFile(FileName: string); overload;
    property AssetsFolder: string read FAssetsFolder write FAssetsFolder;
    property Active: Boolean read GetActive;
  end;

  TTyroScriptClass = class of TTyroScript;

  TScriptType = class(TObject)
  public
    Title: string;
    Extention: string;
    ScriptClass: TTyroScriptClass;
  end;

  { TScriptTypes }

  TScriptTypes = class(specialize TFPGObjectList<TScriptType>)
  public
    function FindByExtension(Extension: string): TScriptType;
  end;

  { TTyroImage }

  TTyroImage = class(TObject)
  private
    FImage: TImage;
  protected
    property Image: TImage read FImage;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function LoadTexture: TTexture2D;
    procedure Circle(X, Y, R: Integer; Color: TColor);
  end;

  { TTyroCanvas }

  TTyroCanvas = class(TObject)
  private
    FOriginX, FOriginY: Integer;
    FLastX, FLastY: Integer;
    FPenColor: TColor;
    FBackColor: TColor;
    FWidth, FHeight: Integer;
    FTexture: TRenderTexture2D;
    FAlpha: Byte;
    Font: TRayFont;
    function GetPenAlpha: Byte;
    procedure SetPenAlpha(AValue: Byte);
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure SetOrigin(X, Y: Integer);
    procedure ResetOrigin;
    procedure BeginDraw;
    procedure EndDraw;
    procedure DrawCircle(X, Y, R: Integer; Color: TColor; Fill: Boolean = false);
    procedure DrawText(X, Y: Integer; S: string);
    procedure DrawPixel(X, Y: Integer; Color: TColor);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: TColor);
    procedure DrawLineTo(X2, Y2: Integer; Color: TColor);
    procedure DrawRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor; Fill: Boolean);
    procedure DrawRectangle(ARectangle: TRect; Color: TColor; Fill: Boolean);
    procedure DrawRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean);
    procedure DrawRect(ARectangle: TRect; Color: TColor; Fill: Boolean);
    procedure Clear;
    property PenAlpha: Byte read GetPenAlpha write SetPenAlpha;
    property PenColor: TColor read FPenColor write FPenColor;
    property Alpha: Byte read FAlpha write FAlpha;
    property BackColor: TColor read FBackColor write FBackColor;
    property Texture: TRenderTexture2D read FTexture;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

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

  TQueueObjects = class(specialize TFPGObjectList<TQueueObject>)
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

  function IntToFPColor(I: Integer): TFPColor;
  function FPColorToInt(C: TFPColor): Integer;

  function IntToColor(I: integer): TColor;
  function ColorToInt(C: TColor): integer;

const
  ScreenCharWidth = 40;
  ScreenCharHeight = 30;
  ScreenFontSize = 16;
  ScreenWidth: Integer = ScreenCharWidth * ScreenFontSize;
  ScreenHeight: Integer = ScreenCharHeight * ScreenFontSize;
  cFramePerSeconds = 60;

var
  Lock: TCriticalSection = nil;

function RayColorOf(Color: TFPColor): TRGBAColor;

implementation

uses
  TyroEngines, minibidi;

function RayColorOf(Color: TFPColor): TRGBAColor;
begin
  Result.Alpha := hi(Color.Alpha);
  Result.Red := hi(Color.Red);
  Result.Green := hi(Color.Green);
  Result.Blue := hi(Color.Blue);
end;

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

function IntToColor(I: Integer): TColor;
begin
  Result.RGBA.Red := I and $ff;
  I := I shr 8;
  Result.RGBA.Green := I and $ff;
  I := I shr 8;
  Result.RGBA.Blue := I and $ff;
  I := I shr 8;
  Result.RGBA.Alpha := I and $ff;
end;

function ColorToInt(C: TColor): Integer;
begin
  Result := C.RGBA.Alpha;
  Result := Result shl 8;
  Result := Result or C.RGBA.Blue;
  Result := Result shl 8;
  Result := Result or C.RGBA.Green;
  Result := Result shl 8;
  Result := Result or C.RGBA.Red;
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

{ TTyroImage }

constructor TTyroImage.Create(AWidth, AHeight: Integer);
begin
  //FImage := GetTextureData(FTexture.texture);
  FImage := GenImageColor(AWidth, AHeight, clRed);
end;

destructor TTyroImage.Destroy;
begin
  UnloadImage(FImage);
  inherited Destroy;
end;

function TTyroImage.LoadTexture: TTexture2D;
begin
  Result := LoadTextureFromImage(FImage);
end;

procedure TTyroImage.Circle(X, Y, R: Integer; Color: TColor);
begin
  ImageDrawCircle(FImage, X, Y, R, Color);
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
begin
  Result := nil;
  for itm in Self do
  begin
    if SameText(itm.Extention, Extension) then
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

{ TTyroCanvas }

procedure TTyroCanvas.SetHeight(AValue: Integer);
begin
  if FHeight =AValue then Exit;
  FHeight :=AValue;
end;

function TTyroCanvas.GetPenAlpha: Byte;
begin
  Result := FPenColor.RGBA.Alpha;
end;

procedure TTyroCanvas.SetPenAlpha(AValue: Byte);
begin
  FPenColor.RGBA.Alpha := AValue;
end;

procedure TTyroCanvas.SetWidth(AValue: Integer);
begin
  if FWidth =AValue then Exit;
  FWidth :=AValue;
end;

constructor TTyroCanvas.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  Font := TRayFont.Create;
  FAlpha := 255;
  FWidth := AWidth;
  FHeight := AHeight;
  FPenColor := clBlack;
  //FBackgroundColor := TColor.CreateRGBA($0892D0FF);
  //FBackgroundColor := TColor.CreateRGBA($B0C4DEFF); //Light Steel Blue
  FBackColor := TColor.CreateRGBA($77B5FEFF); //French Sky Blue
  //Font := GetFontDefault;

  //Font := LoadFont(PChar(Main.WorkSpace + 'alpha_beta.png'));
  //Font := LoadFont(PChar(Main.WorkSpace + 'Terminess-Bold.ttf'));
  //Font := LoadFont(PChar(Main.WorkSpace + 'dejavu.fnt'));
  //Font := LoadFont(PChar(Main.WorkSpace + 'DejaVuSansMono-Bold.ttf'));
  //Font := LoadFont(PChar(Main.WorkSpace + 'terminus.ttf'));
  //Font := LoadFont(PChar(Main.WorkSpace + 'fonts/Chroma.png'));
  //Font := LoadFont(PChar(Main.WorkSpace + 'fonts/alpha_beta.png'));
  //Font := LoadFont(PChar(Main.WorkSpace + 'fonts/ChiKareGo2.ttf'));
  //Font := LoadFontEx(PChar(Main.WorkSpace + 'fonts/terminus.ttf'), 12, nil, 255);
  //Font := LoadFontEx(PChar(Main.WorkSpace + 'fonts/AnonymousPro-Regular.ttf'), 12, nil, 255);
  //Font := LoadFont(PChar('computer_pixel.fon.ttf'));

  //Font := LoadFontEx(PChar(Main.WorkSpace + 'fonts/tahoma.ttf'), ScreenFontSize, nil, $FFFF); //Good for arabic but it take huge memory

  Font.LoadFromFile(Main.WorkSpace + 'fonts/font.png');
  Font.Height := 16;
  //Font := GetFontDefault();
  SetTextureFilter(Font.Data.texture, FILTER_POINT);

  FTexture := LoadRenderTexture(Width, Height);

  BeginTextureMode(FTexture);
  ClearBackground(BackColor);
  DrawText(10, 10, 'Ready!');
  EndTextureMode();
end;

destructor TTyroCanvas.Destroy;
begin
  FreeAndNil(Font);
  UnloadRenderTexture(FTexture);
  Finalize(FTexture);
  FreeAndNil(Font);
  inherited;
end;

procedure TTyroCanvas.SetOrigin(X, Y: Integer);
begin
  FOriginX := X;
  FOriginY := Y;
end;

procedure TTyroCanvas.ResetOrigin;
begin
  FOriginX := 0;
  FOriginY := 0;
end;

procedure TTyroCanvas.BeginDraw;
begin
  BeginTextureMode(FTexture);
end;

procedure TTyroCanvas.EndDraw;
begin
  EndTextureMode;
end;

procedure TTyroCanvas.DrawCircle(X, Y, R: Integer; Color: TColor; Fill: Boolean = false);
begin
  if Fill then
    RayLib.DrawCircle(X + FOriginX, Y + FOriginY, R, Color.SetAlpha(Alpha))
  else
    RayLib.DrawCircleLines(X + FOriginX, Y + FOriginY, R, Color.SetAlpha(Alpha));
  FLastX := X;
  FLastY := Y;
end;

procedure TTyroCanvas.DrawRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor; Fill: Boolean);
begin
  if Fill then
    RayLib.DrawRectangle(X + FOriginX, Y + FOriginY, AWidth, AHeight, Color.SetAlpha(Alpha))
  else
    RayLib.DrawRectangleLines(X + FOriginX, Y + FOriginY, AWidth, AHeight, Color.SetAlpha(Alpha));
  FLastX := X + AWidth;
  FLastY := Y + AHeight;
end;

procedure TTyroCanvas.DrawRectangle(ARectangle: TRect; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(ARectangle.Left, ARectangle.Top, ARectangle.Right - ARectangle.Left, ARectangle.Bottom - ARectangle.Top, Color, Fill);
end;

procedure TTyroCanvas.DrawRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(ALeft, ATop, ARight - ALeft, ABottom - ATop, Color, Fill);
end;

procedure TTyroCanvas.DrawRect(ARectangle: TRect; Color: TColor; Fill: Boolean);
begin
  DrawRect(ARectangle.Left, ARectangle.Top, ARectangle.Right, ARectangle.Bottom, Color, Fill);
end;

procedure TTyroCanvas.DrawText(X, Y: Integer; S: string);
begin
  RayLib.DrawTextEx(Font.Data, PChar(S), Vector2Of(x + FOriginX, y + FOriginY), Font.Height, 0, PenColor);
end;

procedure TTyroCanvas.DrawPixel(X, Y: Integer; Color: TColor);
begin
  RayLib.DrawPixel(X + FOriginX, Y + FOriginY, Color);
  FLastX := X;
  FLastY := Y;
end;

procedure TTyroCanvas.DrawLine(X1, Y1, X2, Y2: Integer; Color: TColor);
begin
  RayLib.DrawLine(X1 + FOriginX, Y1 + FOriginY, X2 + FOriginX, Y2 + FOriginY, Color);
  FLastX := X2;
  FLastY := Y2;
end;

procedure TTyroCanvas.DrawLineTo(X2, Y2: Integer; Color: TColor);
begin
  DrawLine(FLastX + FOriginX, FLastY + FOriginY, X2 + FOriginX, Y2 + FOriginY, Color);
end;

procedure TTyroCanvas.Clear;
begin
  ClearBackground(BackColor);
end;

{ TTyroScript }

function TTyroScript.GetActive: Boolean;
begin
  Result := FActive;
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
  Yield;
end;

constructor TTyroScript.Create;
begin
  inherited Create(True);
  FActive := True;
  FreeOnTerminate := False;
  Priority := tpLower; //hmmm
  ScriptText := TStringList.Create;
end;

destructor TTyroScript.Destroy;
begin
  FreeAndNil(ScriptText);
  inherited Destroy;
end;

procedure TTyroScript.TerminatedSet;
begin
  inherited TerminatedSet;
  FActive := False;
end;

procedure TTyroScript.Execute;
begin
  BeforeRun;
  Run;
  AfterRun;
  FActive := False;
end;

procedure TTyroScript.LoadFile(FileName: string);
begin
  ScriptText.LoadFromFile(FileName);
end;

initialization
  Lock := TCriticalSection.Create;
  Main := TTyroMain.Create;
finalization
  FreeAndNil(Main);
  FreeAndNil(Lock);
end.

