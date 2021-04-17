unit TyroClasses;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fgl,
  FPImage, FPCanvas,
  Melodies, RayClasses, TyroSounds, RayLib3;

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
    //Canvas: TTyroCanvas;
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
    destructor Destroy; virtual;
    procedure BeginDraw;
    procedure EndDraw;
    function LoadTexture: TTexture2D;
    procedure Circle(X, Y, R: Integer; Color: TColor);
  end;

  { TTyroCanvas }

  TTyroCanvas = class(TObject)
  private
    FTextColor: TColor;
    FBackColor: TColor;
    FFontSize: Integer;
    FTexture: TRenderTexture2D;
    //FBoard: TRenderTexture2D;
    //FBoard: TImage;
    Font: RayLib3.TFont;
    function GetAlpha: Byte;
    function GetTextColor: TColor;
    procedure SetAlpha(AValue: Byte);
    procedure SetBackColor(AValue: TColor);
    procedure SetTextColor(AValue: TColor);
  public
    Width, Height: Integer;
    LastX, LastY: Integer;
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; virtual;
    procedure BeginDraw;
    procedure EndDraw;
    //property Board: TImage read FBoard;
    procedure DrawCircle(X, Y, R: Integer; Color: TColor; Fill: Boolean = false);
    //procedure PrintTest;
    procedure DrawText(X, Y: Integer; S: string);
    procedure DrawPixel(X, Y: Integer; Color: TColor);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: TColor);
    procedure DrawRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor; Fill: Boolean);
    procedure DrawRect(Left: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean);
    procedure Print(S: string);
    procedure Clear;
    property TextColor: TColor read GetTextColor write SetTextColor;
    property PenColor: TColor read GetTextColor write SetTextColor;//temp trick
    property Alpha: Byte read GetAlpha write SetAlpha;
    property BackColor: TColor read FBackColor write SetBackColor;
    property FontSize: Integer read FFontSize write FFontSize;
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
    constructor Create(ACanvas: TTyroCanvas); virtual;
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
    procedure DoExecute; virtual;
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
    procedure DoExecute; virtual;
  end;

  { TPlayMusicFileObject }

  TPlayMusicFileObject = class(TQueueObject)
  public
    FileName: string;
    constructor Create(AFileName: string);
    procedure DoExecute; virtual;
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

  { TTyroMain }

  TTyroMain = class(TObject)
  private
    //FBoard: TTyroImage;
    function GetActive: Boolean;
  protected
    FQueue: TQueueObjects;
    FScript: TTyroScript;
    FScriptTypes: TScriptTypes;
    FCanvasLock: TCriticalSection;
    FCanvas: TTyroCanvas;
  protected
    DefaultBackground: raylib3.TColor;

    property Queue: TQueueObjects read FQueue;
    property ScriptTypes: TScriptTypes read FScriptTypes;
    procedure ShowWindow(W, H: Integer);
    procedure HideWindow;
  public
    Running: Boolean;
    WindowVisible: Boolean;
    Title: string;
    FileName: string;//that to run in script
    WorkSpace: string;
    constructor Create;
    destructor Destroy; virtual;
    procedure Start;
    procedure Stop;
    procedure ProcessQueue;
    procedure Run;
    procedure Loop;
    property Canvas: TTyroCanvas read FCanvas;
    //property Board: TTyroImage read FBoard;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtention: string; AScriptClass: TTyroScriptClass);

    property CanvasLock: TCriticalSection read FCanvasLock;
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
  Main : TTyroMain = nil;

//Contros
type
  TUTF8Char = String[7]; // UTF-8 character is at most 6 bytes plus a #0

  TScrollbarType = (sbtHorizontal, sbtVertical);
  TScrollbarTypes = set of TScrollbarType;

  TScrollCode = (
    scrollTOP,
    scrollBOTTOM,
    scrollLINEDOWN,
    scrollLINEUP,
    scrollPAGEDOWN,
    scrollPAGEUP,
    scrollTHUMBPOSITION,
    scrollTHUMBTRACK,
    scrollENDSCROLL
  );


  { TTyroControl }

  TTyroControl = class(TObject)
  private
  protected
    IsCreated: Boolean;
    Focused: Boolean;
    function ClientWidth: Integer;
    function ClientHeight: Integer;

    procedure ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
    procedure SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
    procedure SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);

    procedure SetBounds(Left, Top, Width, Height: Integer); virtual;
    procedure Invalidate; virtual;
    procedure Paint(ACanvas: TTyroCanvas); virtual;
    procedure Resize; virtual;
    procedure KeyPress(var Key: TUTF8Char); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: word; Shift: TShiftState); virtual;
    procedure CreateWnd; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseMove(Shift: TShiftState; x, y: integer); virtual;
  public
    constructor Create(AParent: TTyroControl); virtual;
    destructor Destroy; override;
  end;

procedure Log(S: string);
function RayColorOf(Color: TFPColor): TRGBAColor;

implementation

uses
  minibidi;

procedure Log(S: string);
begin
  if IsConsole then
    WriteLn(S);
end;

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

{ TTyroControl }

function TTyroControl.ClientWidth: Integer;
begin

end;

function TTyroControl.ClientHeight: Integer;
begin

end;

procedure TTyroControl.ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
begin

end;

procedure TTyroControl.SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
begin

end;

procedure TTyroControl.SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);
begin

end;

procedure TTyroControl.SetBounds(Left, Top, Width, Height: Integer);
begin

end;

procedure TTyroControl.Invalidate;
begin

end;

procedure TTyroControl.Paint(ACanvas: TTyroCanvas);
begin

end;

procedure TTyroControl.Resize;
begin

end;

procedure TTyroControl.KeyPress(var Key: TUTF8Char);
begin

end;

procedure TTyroControl.KeyDown(var Key: word; Shift: TShiftState);
begin

end;

procedure TTyroControl.KeyUp(var Key: word; Shift: TShiftState);
begin

end;

procedure TTyroControl.CreateWnd;
begin

end;

procedure TTyroControl.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin

end;

procedure TTyroControl.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin

end;

procedure TTyroControl.MouseMove(Shift: TShiftState; x, y: integer);
begin

end;

constructor TTyroControl.Create(AParent: TTyroControl);
begin

end;

destructor TTyroControl.Destroy;
begin
  inherited Destroy;
end;

{ TTyroImage }

constructor TTyroImage.Create(AWidth, AHeight: Integer);
begin
  //FImage := GetTextureData(FTexture.texture);
  FImage := GenImageColor(AWidth, AHeight, RED);
end;

destructor TTyroImage.Destroy;
begin
  UnloadImage(FImage);
  inherited Destroy;
end;

procedure TTyroImage.BeginDraw;
begin
  //BeginTextureMode(FImage);
end;

procedure TTyroImage.EndDraw;
begin
  //EndTextureMode();
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
  Canvas.Alpha := fAlpha;
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
  Canvas.TextColor := fColor;
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
  DrawPixel(fX, fY, Canvas.TextColor);
  Canvas.LastX := fX;
  Canvas.LastY := fY;
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
  DrawLine(Canvas.LastX, Canvas.LastY, fX, fY, Canvas.TextColor);
  Canvas.LastX := fX;
  Canvas.LastY := fY;
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
  DrawLine(fX1, fY1, fX2, fY2, Canvas.TextColor);
  Canvas.LastX := fX2;
  Canvas.LastY := fY2;
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
  if fFill then
    DrawRectangle(fX, fY, fW, fH, Canvas.TextColor)
  else
    DrawRectangleLines(fX, fY, fW, fH, Canvas.TextColor);
end;

{ TPrintObject }

constructor TPrintObject.Create(ACanvas: TTyroCanvas; Text: String);
begin
  inherited Create(ACanvas);
  fText := Text;
end;

procedure TPrintObject.DoExecute;
begin
  Canvas.Print(fText);
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
{var
  s: unicodestring;
  b: utf8string;
  i:integer;}
begin
  {i:=ord(fText[1]);
  s := UTF8Decode(fText);
  s := BidiString(s);
  b := UTF8Encode(s);
  DrawTextEx(Canvas.Font, PUTF8Char(b), Vector2Of(fX, fY), Canvas.FontSize, 2, Canvas.Color);}
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
    Log('You need to init window to use this command, ' + ClassName + ' line: ' + IntToStr(LineNo))
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
  if fFill then
    DrawCircle(fX, fY, fR, Canvas.TextColor)
  else
    DrawCircleLines(fX, fY, fR, Canvas.TextColor);
end;

{ TTyroCanvas }

constructor TTyroCanvas.Create(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
  FTextColor := BLACK;
  //FBackgroundColor := TColor.CreateRGBA($0892D0FF);
  //FBackgroundColor := TColor.CreateRGBA($B0C4DEFF); //Light Steel Blue
  FBackColor := TColor.CreateRGBA($77B5FEFF); //French Sky Blue
  //Font := GetFontDefault;

  //FPImage := TMyFPMemoryImage.Create(100,100);
  //FPImage.LoadFromFile(Main.WorkSpace + 'richard-say.png');
  //FPImage.UsePalette:=false;

  //FPCanvas := TFPImageCanvas.Create(FPImage);
  //FPCanvas.Pen.FPColor := colRed;
  //FPCanvas.Rectangle(10, 10, 10 , 10);
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

  Font := LoadFont(PChar(Main.WorkSpace + 'fonts/font.png'));
  SetTextureFilter(Font.texture, FILTER_POINT);

  FFontSize := Font.baseSize * 2;

  FTexture := LoadRenderTexture(Width, Height);
  //FBoard := GetTextureData(FTexture.texture);

  BeginTextureMode(FTexture);
  ClearBackground(BackColor);
  DrawText(10, 10, 'Ready!');
  EndTextureMode();
end;

destructor TTyroCanvas.Destroy;
begin
  UnloadFont(Font);
  //UnloadImage(FBoard);
  UnloadRenderTexture(FTexture);
  Finalize(FTexture);
  inherited;
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
//  BeginTextureMode(FBoard);
  if Fill then
    DrawCircle(X, Y, R, Color)
  else
    DrawCircleLines(X, Y, R, Color);
//  EndTextureMode();
end;

procedure TTyroCanvas.DrawRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor; Fill: Boolean);
begin
  if Fill then
    Raylib3.DrawRectangle(X, Y, Width, Height, Color)
  else
    Raylib3.DrawRectangleLines(X, Y, Width, Height, Color);
end;

procedure TTyroCanvas.DrawRect(Left: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean);
begin

end;

procedure TTyroCanvas.DrawText(X, Y: Integer; S: string);
begin
  RayLib3.DrawTextEx(Font, PChar(S), Vector2Of(x, y), FontSize, 0, TextColor);
end;

procedure TTyroCanvas.DrawPixel(X, Y: Integer; Color: TColor);
begin
  RayLib3.DrawPixel(X, Y, Color);
end;

procedure TTyroCanvas.DrawLine(X1, Y1, X2, Y2: Integer; Color: TColor);
begin

end;

procedure TTyroCanvas.Print(S: string);
begin
  //TODO
end;

procedure TTyroCanvas.Clear;
begin
  ClearBackground(BackColor);
end;

procedure TTyroCanvas.SetBackColor(AValue: TColor);
begin
  FBackColor := AValue;
end;

function TTyroCanvas.GetAlpha: Byte;
begin
  Result := FTextColor.RGBA.Alpha;
end;

function TTyroCanvas.GetTextColor: TColor;
begin
  Result := FTextColor;
end;

procedure TTyroCanvas.SetAlpha(AValue: Byte);
begin
  FTextColor.RGBA.Alpha := AValue;
end;

procedure TTyroCanvas.SetTextColor(AValue: TColor);
begin
  FTextColor.RGBA.Red := AValue.RGBA.Red;
  FTextColor.RGBA.Green := AValue.RGBA.Green;
  FTextColor.RGBA.Blue := AValue.RGBA.Blue;
  //No alpha
end;

{ TTyroMain }

function TTyroMain.GetActive: Boolean;
begin
  Result := Running or ((FScript <> nil) and FScript.Active);
end;

procedure TTyroMain.ShowWindow(W, H: Integer);
begin
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(W, H, PChar(Title));
  SetTargetFPS(cFramePerSeconds);
  ShowCursor();
  WindowVisible := True;
  FCanvas := TTyroCanvas.Create(W, H);
  //FBoard := TTyroImage.Create(W, H);
end;

procedure TTyroMain.HideWindow;
begin
  if WindowVisible then
    CloseWindow;
end;

procedure TTyroMain.ProcessQueue;
var
  p: TQueueObject;
  c: Integer;
  fpd: Double;
  ft, ft2: Double;
begin
  if Canvas <> nil then
  begin
    CanvasLock.Enter;
    try
      ft := GetTime();
      fpd := (1 / cFramePerSeconds);
      Canvas.BeginDraw;
      c := 0;
      while Queue.Count > 0 do
      begin
        Lock.Enter;
        try
          p := Queue.Extract(Queue[0]);
        finally
          Lock.Leave;
        end;
        p.Execute;
        p.Free;
        Inc(c);
        ft2 := GetTime() - ft;
        if ft2 >= fpd then
        begin
          break;
        end;
      end;
      Canvas.EndDraw;
    finally
      CanvasLock.Leave;
    end;
  end;
end;

constructor TTyroMain.Create;
begin
  inherited Create;
  FCanvasLock := TCriticalSection.Create;
  RayLib.Load;
  //SetTraceLog(LOG_DEBUG or LOG_INFO or LOG_WARNING);
  SetTraceLogLevel([LOG_ERROR, LOG_FATAL]);
  FQueue := TQueueObjects.Create(True);
  FScriptTypes := TScriptTypes.Create(true);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  DefaultBackground := TColor.Create(220, 230, 240, 0);
end;

destructor TTyroMain.Destroy;
begin
  //Stop;
  HideWindow;
  FreeAndNil(FCanvas);
  //FreeAndNil(FBoard);
  FreeAndNil(FQueue);
  FreeAndNil(FScriptTypes);
  FreeAndNil(FCanvasLock);
  inherited Destroy;
end;

procedure TTyroMain.Start;
var
  ScriptType: TScriptType;
begin
  Running := True;
//ShowWindow(ScreenWidth, ScreenHeight); with option to show window /w
  if FileName <> '' then
  begin
    ScriptType := ScriptTypes.FindByExtension(ExtractFileExt(FileName));
    if ScriptType <> nil then
    begin
      FScript := ScriptType.ScriptClass.Create;
      if SysUtils.FileExists(FileName) then
      begin
        if LeftStr(FileName, 1) = '.' then
          FileName := ExpandFileName(WorkSpace + FileName);
        FScript.AssetsFolder := ExtractFilePath(FileName);
        FScript.LoadFile(FileName);
      end;
    end
    else
      Log('Type of file not found: ' + FileName);
  end;
end;

procedure TTyroMain.Run;
var
  t: TTexture2D;
  //im: TImage;
begin
  if (FScript <> nil) and FScript.Suspended then
    FScript.Start;

  if WindowVisible then
  begin
    if WindowShouldClose() then
    begin
      Running := False;
      if (FScript <> nil) then
        FScript.Terminate;
    end;

    ProcessQueue;

    CanvasLock.Enter;
    try
      BeginDrawing;
      ClearBackground(DefaultBackground);

      {if Board <> nil then
      begin
        t := Board.LoadTexture;
        DrawTextureRec(t, TRectangle.Create(0, 0, t.width, t.height), TVector2.Create(0, 0), WHITE);
        UnloadTexture(t);
      end;}

      with Canvas.FTexture do
        DrawTextureRec(texture, TRectangle.Create(0, 0, texture.width, -texture.height), Vector2Of(0, 0), WHITE);

      ThreadSwitch; //Yield
    finally
      CanvasLock.Leave;
    end;
    BeginDrawing;
    Loop;
    ThreadSwitch; //Yield
    EndDrawing;
    RayUpdates.Update;
  end;
end;

procedure TTyroMain.Loop;
begin
end;

procedure TTyroMain.RegisterLanguage(ATitle: string; AExtention: string; AScriptClass: TTyroScriptClass);
var
  Item: TScriptType;
begin
  Item := TScriptType.Create;
  Item.Title := ATitle;
  Item.Extention := AExtention;
  Item.ScriptClass := AScriptClass;
  FScriptTypes.Add(Item);
end;

procedure TTyroMain.Stop;
begin
  Running := False;
  if FScript <> nil then
  begin
    FScript.Terminate;
    FScript.WaitFor;
    FreeAndNil(FScript);
  end;
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
  //Canvas := TTyroCanvas.Create;
end;

destructor TTyroScript.Destroy;
begin
  FreeAndNil(ScriptText);
  //FreeAndNil(Canvas);
  inherited Destroy;
end;

procedure TTyroScript.TerminatedSet;
begin
  inherited TerminatedSet;
  FActive := False;
end;

procedure TTyroScript.Execute;
begin
  //Board := LoadImage(PChar(Main.WorkSpace + 'richard-say.png'));
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

