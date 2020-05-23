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
  FPImage, FPCanvas, FPImgCanv, pngcomn, FPReadPNG,
  RayLibClasses, RayLib3;

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
    function GetActive: Boolean;
  protected
    //Canvas: TTyroCanvas;
    ScriptText: TStringList;
    procedure BeforeRun; virtual;
    procedure Run; virtual; abstract;
    procedure AfterRun; virtual;
    procedure AddQueueObject(AQueueObject: TQueueObject); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; override;
    procedure LoadFile(FileName: string); overload;
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

  { TTyroCanvas }

  TTyroCanvas = class(TObject)
  private
    FColor: TColor;
    FBackgroundColor: TColor;
    FFontSize: Integer;
    //FPImage: TMyFPMemoryImage;
    //FPCanvas: TFPImageCanvas;
    //FBoard: TRenderTexture2D;
    FTexture: TRenderTexture2D;
    //FBoard: raylib.TImage;
    Font: raylib3.TFont;
    function GetAlpha: Byte;
    function GetColor: TColor;
    procedure SetAlpha(AValue: Byte);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetColor(AValue: TColor);
  public
    Width, Height: Integer;
    LastX, LastY: Integer;
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    //property Board: raylib.TImage read FBoard;
    procedure Circle(X, Y, R: Integer; Fill: Boolean = false);
    procedure Rectangle(X, Y, W, H: Integer; Fill: Boolean = false);
    //procedure PrintTest;
    procedure DrawText(X, Y: Integer; S: string);
    procedure Print(S: string);
    procedure Clear;
    property Color: TColor read GetColor write SetColor;
    property Alpha: Byte read GetAlpha write SetAlpha;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property FontSize: Integer read FFontSize write FFontSize;
  end;

  { TQueueObject }

  TQueueObject = class abstract(TObject)
  private
    procedure DoExecute; virtual; abstract;
  protected
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

  { TClearObject }

  TClearObject = class(TDrawObject)
  public
    constructor Create(ACanvas: TTyroCanvas);
    procedure DoExecute; override;
  end;

  { TTyroMain }

  TTyroMain = class(TObject)
  private
    function GetActive: Boolean;
  protected
    FQueue: TQueueObjects;
    FScript: TTyroScript;
    FScriptTypes: TScriptTypes;
    //FLock: TCriticalSection;
    FCanvas: TTyroCanvas;
  protected
    DefaultBackground: raylib3.TColor;

    //property Lock: TCriticalSection read FLock;
    property Queue: TQueueObjects read FQueue;
    property ScriptTypes: TScriptTypes read FScriptTypes;
    procedure ShowWindow(W, H: Integer);
    procedure HideWindow;
  public
    WindowVisible: Boolean;
    Title: string;
    FileName: string;//that to run in script
    WorkSpace: string;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure ProcessQueue;
    procedure Run;
    procedure Loop;
    property Canvas: TTyroCanvas read FCanvas;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtention: string; AScriptClass: TTyroScriptClass);
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

procedure Log(S: string);
function RayColorOf(Color: TFPColor): raylib3.TColor;

implementation

procedure Log(S: string);
begin
  if IsConsole then
    WriteLn(S);
end;

function RayColorOf(Color: TFPColor): raylib3.TColor;
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

function IntToColor(I: integer): TColor;
begin
  Result.Red := I and $ff;
  I := I shr 8;
  Result.Green := I and $ff;
  I := I shr 8;
  Result.Blue := I and $ff;
  I := I shr 8;
  Result.Alpha := I and $ff;
end;

function ColorToInt(C: TColor): Integer;
begin
  Result := C.Alpha;
  Result := Result shl 8;
  Result := Result or C.Blue;
  Result := Result shl 8;
  Result := Result or C.Green;
  Result := Result shl 8;
  Result := Result or C.Red;
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
  Canvas.Color := fColor;
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
  RayLib.DrawPixel(fX, fY, Canvas.Color);
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
  RayLib.DrawLine(Canvas.LastX, Canvas.LastY, fX, fY, Canvas.Color);
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
  RayLib.DrawLine(fX1, fY1, fX2, fY2, Canvas.Color);
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
  RayLib.ClearBackground(Canvas.BackgroundColor);
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
    RayLib.DrawRectangle(fX, fY, fW, fH, Canvas.Color)
  else
    RayLib.DrawRectangleLines(fX, fY, fW, fH, Canvas.Color);
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
begin
  RayLib.DrawTextEx(Canvas.Font, PChar(fText), Vector2Of(fX, fY), Canvas.FontSize, 2, Canvas.Color);
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
    RayLib.DrawCircle(fX, fY, fR, Canvas.Color)
  else
    RayLib.DrawCircleLines(fX, fY, fR, Canvas.Color);
end;

{ TTyroCanvas }

constructor TTyroCanvas.Create(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
  FColor := BLACK;
  //FBackgroundColor := TColor.Create(0, 110, 160, 255);
  FBackgroundColor := TColor.Create($00FF00FF);
  //Font := GetFontDefault;

  //FPImage := TMyFPMemoryImage.Create(100,100);
  //FPImage.LoadFromFile(Main.WorkSpace + 'richard-say.png');
  //FPImage.UsePalette:=false;

  //FPCanvas := TFPImageCanvas.Create(FPImage);
  //FPCanvas.Pen.FPColor := colRed;
  //FPCanvas.Rectangle(10, 10, 10 , 10);
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'alpha_beta.png'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'Terminess-Bold.ttf'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'dejavu.fnt'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'DejaVuSansMono-Bold.ttf'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'terminus.ttf'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/Chroma.png'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/alpha_beta.png'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/ChiKareGo2.ttf'));
  //Font := raylib.LoadFontEx(PChar(Main.WorkSpace + 'fonts/terminus.ttf'), 12, nil, 255);
  //Font := raylib.LoadFont(PChar('computer_pixel.fon.ttf'));

  Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/font.png'));
  RayLib.SetTextureFilter(Font.texture, FILTER_POINT);

  FFontSize := Font.baseSize * 2;

  FTexture := RayLib.LoadRenderTexture(Width, Height);
  //FBoard := GetTextureData(FTexture.texture);

  RayLib.BeginTextureMode(FTexture);
  RayLib.ClearBackground(BackgroundColor);
  DrawText(10, 10, 'Ready!');
  RayLib.EndTextureMode();
end;

destructor TTyroCanvas.Destroy;
begin
  RayLib.UnloadFont(Font);
  //UnloadImage(FBoard);
  RayLib.UnloadRenderTexture(FTexture);
  Finalize(FTexture);
  inherited;
end;

procedure TTyroCanvas.Circle(X, Y, R: Integer; Fill: Boolean);
begin
//  BeginTextureMode(FBoard);
  if Fill then
    RayLib.DrawCircle(X, Y, R, Color)
  else
    RayLib.DrawCircleLines(X, Y, R, Color);
//  EndTextureMode();
end;

procedure TTyroCanvas.Rectangle(X, Y, W, H: Integer; Fill: Boolean);
begin
  if Fill then
    RayLib.DrawRectangle(X, Y, W, H, Color)
  else
    RayLib.DrawRectangleLines(X, Y, W, H, Color);
end;

procedure TTyroCanvas.DrawText(X, Y: Integer; S: string);
begin
  RayLib.DrawTextEx(Font, PChar(S), Vector2Of(x, y), FontSize, 0, Color);
  //ImageDrawTextEx(@FTexture, Vector2Create(x, y), Font, PChar(S), FontSize, 2, RayColorOf(Color));
end;

procedure TTyroCanvas.Print(S: string);
begin
  //TODO
end;

procedure TTyroCanvas.Clear;
begin
  RayLib.ClearBackground(BackgroundColor);
end;

procedure TTyroCanvas.SetBackgroundColor(AValue: TColor);
begin
  FBackgroundColor := AValue;
end;

function TTyroCanvas.GetAlpha: Byte;
begin
  Result := FColor.Alpha;
end;

function TTyroCanvas.GetColor: TColor;
begin
  Result := FColor;
end;

procedure TTyroCanvas.SetAlpha(AValue: Byte);
begin
  FColor.Alpha := AValue;
end;

procedure TTyroCanvas.SetColor(AValue: TColor);
begin
  FColor.Red := AValue.Red;
  FColor.Green := AValue.Green;
  FColor.Blue := AValue.Blue;
end;

{ TTyroMain }

function TTyroMain.GetActive: Boolean;
begin
  if WindowVisible then
    Result := not RayLib.WindowShouldClose()
  else
    Result := (FScript <> nil) and FScript.Active;
end;

procedure TTyroMain.ShowWindow(W, H: Integer);
begin
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  RayLib.InitWindow(W, H, PChar(Title));
  RayLib.SetTargetFPS(cFramePerSeconds);
  RayLib.ShowCursor();
  WindowVisible := True;
  FCanvas := TTyroCanvas.Create(W, H);
end;

procedure TTyroMain.HideWindow;
begin
  if WindowVisible then
    RayLib.CloseWindow;
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
    ft := RayLib.GetTime();
    fpd := (1 / cFramePerSeconds);
    RayLib.BeginTextureMode(Canvas.FTexture);
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
      ft2 := RayLib.GetTime() - ft;
      if ft2 >= fpd then
      begin
        break;
      end;
    end;
    RayLib.EndTextureMode;
  end;
end;

constructor TTyroMain.Create;
begin
  inherited Create;
  RayLib.Load;
  //SetTraceLog(LOG_DEBUG or LOG_INFO or LOG_WARNING);
  RayLib.SetTraceLogLevel([LOG_ERROR, LOG_FATAL]);
  //FLock := TCriticalSection.Create;
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
  FreeAndNil(FCanvas);
  HideWindow;
  FreeAndNil(FQueue);
  //FreeAndNil(FLock);
  FreeAndNil(FScriptTypes);
  inherited Destroy;
end;

procedure TTyroMain.Start;
var
  ScriptType: TScriptType;
begin
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
        FScript.LoadFile(FileName);
      end;
    end
    else
      Log('Type of file not found: ' + FileName);
  end;
end;

procedure TTyroMain.Run;
//var
  //t: TTexture2D;
  //im: TImage;
begin
  if (FScript <> nil) and FScript.Suspended then
    FScript.Start;

  if WindowVisible then
  begin
    ProcessQueue;

    RayLib.BeginDrawing;
    RayLib.ClearBackground(DefaultBackground);

    try
      {im := LoadImageEx(PColor(FScript.FPImage.Data), FScript.FPImage.Width, FScript.FPImage.Height);
      t := LoadTextureFromImage(im);
      DrawTextureRec(t, RectangleCreate(0, 0, t.width, t.height), Vector2Create(0, 0), WHITE);}

      //t := LoadTextureFromImage(FScript.Canvas.FTexture);
      //DrawTextureRec(t, RectangleCreate(0, 0, t.width, t.height), Vector2Create(0, 0), WHITE);

      with Canvas.FTexture do
        RayLib.DrawTextureRec(texture, TRectangle.Create(0, 0, texture.width, -texture.height), Vector2Of(0, 0), WHITE);

    finally
    end;

    Loop;
    RayLib.EndDrawing;
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

