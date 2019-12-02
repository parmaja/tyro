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
  raylib;

type
  TMyFPMemoryImage = class(TFPMemoryImage)
  public
    property Data: PFPIntegerArray read FData;
  end;

  TPoolObject = class;
  TTyroCanvas = class;

  { TTyroScript }

  TTyroScript = class abstract(TThread)
  private
  protected
    //Canvas: TTyroCanvas;
    ScriptText: TStringList;
    procedure BeforeRun; virtual;
    procedure Run; virtual; abstract;
    procedure AfterRun; virtual;
    procedure AddPoolObject(APoolObject: TPoolObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; override;
    procedure LoadFile(FileName: string); overload;
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
    FBackgroundColor: TFPColor;
    FFontSize: Integer;
    FColor: TFPColor;
    //FPImage: TMyFPMemoryImage;
    //FPCanvas: TFPImageCanvas;
    //FBoard: TRenderTexture2D;
    FTexture: TRenderTexture2D;
    //FBoard: raylib.TImage;
    Font: raylib.TFont;
    procedure SetBackgroundColor(AValue: TFPColor);
  public
    LastX, LastY: Integer;
    constructor Create;
    destructor Destroy; override;
    //property Board: raylib.TImage read FBoard;
    procedure Circle(X, Y, R: Integer; Fill: Boolean = false);
    procedure Rectangle(X, Y, W, H: Integer; Fill: Boolean = false);
    //procedure PrintTest;
    procedure DrawText(X, Y: Integer; S: string);
    procedure Print(S: string);
    procedure Clear;
    property Color: TFPColor read FColor write FColor;
    property FontSize: Integer read FFontSize write FFontSize;
    property BackgroundColor: TFPColor read FBackgroundColor write SetBackgroundColor;
  end;

  { TPoolObject }

  TPoolObject = class abstract(TObject)
  private
    FNeedSynchronize: Boolean;
  public
    procedure Execute; virtual; abstract;
    property NeedSynchronize: Boolean read FNeedSynchronize; //this will call Synchronize
  end;

  { TPoolObjects }

  TPoolObjects = class(specialize TFPGObjectList<TPoolObject>)
  public
  end;

  { TDrawObject }

  TDrawObject = class abstract(TPoolObject)
  private
    FCanvas: TTyroCanvas;
  protected
    procedure Created; virtual;
  public
    constructor Create(ACanvas: TTyroCanvas);
    property Canvas: TTyroCanvas read FCanvas;
  end;

  { TDrawCircleObject }

  TDrawCircleObject = class(TDrawObject)
  public
    fX, fY, fR: Integer;
    fFill: Boolean;
    constructor Create(ACanvas: TTyroCanvas; X, Y, R: Integer; Fill: Boolean);
    procedure Execute; override;
  end;

  { TDrawRectangleObject }

  TDrawRectangleObject = class(TDrawObject)
  public
    fX, fY, fW, fH: Integer;
    fFill: Boolean;
    constructor Create(ACanvas: TTyroCanvas; X, Y, W, H: Integer; Fill: Boolean);
    procedure Execute; override;
  end;

  { TDrawLineObject }

  TDrawLineObject = class(TDrawObject)
  public
    fX1, fY1, fX2, fY2: Integer;
    constructor Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2: Integer);
    procedure Execute; override;
  end;

  { TDrawLineToObject }

  TDrawLineToObject = class(TDrawObject)
  public
    fX, fY: Integer;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer);
    procedure Execute; override;
  end;

  { TDrawPointObject }

  TDrawPointObject = class(TDrawObject)
  public
    fX, fY: Integer;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer);
    procedure Execute; override;
  end;

  { TDrawTextObject }

  TDrawTextObject = class(TDrawObject)
  public
    fX, fY: Integer;
    fText: String;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer; Text: String);
    procedure Execute; override;
  end;

  { TPrintObject }

  TPrintObject = class(TDrawObject)
  public
    FText: String;
    constructor Create(ACanvas: TTyroCanvas; Text: String);
    procedure Execute; override;
  end;

  { TClearObject }

  TClearObject = class(TDrawObject)
  public
    constructor Create(ACanvas: TTyroCanvas);
    procedure Execute; override;
  end;

  { TTyroMain }

  TTyroMain = class(TObject)
  private
    function GetActive: Boolean;
  protected
    FPool: TPoolObjects;
    FScript: TTyroScript;
    FScriptTypes: TScriptTypes;
    //FLock: TCriticalSection;
    FCanvas: TTyroCanvas;
  protected
    DefaultBackground: raylib.TColor;

    //property Lock: TCriticalSection read FLock;
    property Pool: TPoolObjects read FPool;
    property ScriptTypes: TScriptTypes read FScriptTypes;
  public
    Title: string;
    FileName: string;//that to run in script
    WorkSpace: string;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure ProcessPool;
    procedure Run;
    procedure Loop;
    property Canvas: TTyroCanvas read FCanvas;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtention: string; AScriptClass: TTyroScriptClass);
  end;

function IntToFPColor(I: Integer): TFPColor;
function FPColorToInt(C: TFPColor): Integer;

var
  ScreenWidth: Integer = 640; //Temporary here, i will move it to Tyro object
  ScreenHeight: Integer = 480;

var
  Lock: TCriticalSection = nil;
  Main : TTyroMain = nil;

procedure Log(S: string);

implementation

procedure Log(S: string);
begin
  if IsConsole then
    WriteLn(S);
end;

function RayColorOf(Color: TFPColor): raylib.TColor;
begin
  Result.a := hi(Color.Alpha);
  Result.r := hi(Color.Red);
  Result.g := hi(Color.Green);
  Result.b := hi(Color.Blue);
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

procedure TDrawPointObject.Execute;
begin
  DrawPixel(fX, fY, RayColorOf(Canvas.Color));
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

procedure TDrawLineToObject.Execute;
begin
  DrawLine(Canvas.LastX, Canvas.LastY, fX, fY, RayColorOf(Canvas.Color));
  Canvas.LastX := fX;
  Canvas.LastY := fY;
end;

{ TDrawLIneObject }

constructor TDrawLIneObject.Create(ACanvas: TTyroCanvas; X1, Y1, X2, Y2: Integer);
begin
  inherited Create(ACanvas);
  fX1 := X1;
  fY1 := Y1;
  fX2 := X2;
  fY2 := Y2;
end;

procedure TDrawLineObject.Execute;
begin
  DrawLine(fX1, fY1, fX2, fY2, RayColorOf(Canvas.Color));
  Canvas.LastX := fX2;
  Canvas.LastY := fY2;
end;

{ TClearObject }

constructor TClearObject.Create(ACanvas: TTyroCanvas);
begin
  inherited Create(ACanvas);
end;

procedure TClearObject.Execute;
begin
  ClearBackground(RayColorOf(Canvas.BackgroundColor));
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

procedure TDrawRectangleObject.Execute;
begin
  if fFill then
    DrawRectangle(fX, fY, fW, fH, RayColorOf(Canvas.Color))
  else
    DrawRectangleLines(fX, fY, fW, fH, RayColorOf(Canvas.Color));
end;

{ TPrintObject }

constructor TPrintObject.Create(ACanvas: TTyroCanvas; Text: String);
begin
  inherited Create(ACanvas);
  fText := Text;
end;

procedure TPrintObject.Execute;
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

procedure TDrawTextObject.Execute;
begin
  raylib.DrawTextEx(Canvas.Font, PChar(fText), Vector2Create(fX, fY), Canvas.FontSize, 2, RayColorOf(Canvas.Color));
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

{ TDrawCircleObject }

constructor TDrawCircleObject.Create(ACanvas: TTyroCanvas; X, Y, R: Integer; Fill: Boolean);
begin
  inherited Create(ACanvas);
  fX := X;
  fY := Y;
  fR := R;
  fFill := Fill;
end;

procedure TDrawCircleObject.Execute;
begin
  if fFill then
    DrawCircle(fX, fY, fR, RayColorOf(Canvas.Color))
  else
    DrawCircleLines(fX, fY, fR, RayColorOf(Canvas.Color));
end;

{ TTyroCanvas }

constructor TTyroCanvas.Create;
begin
  FColor := colBlack;
  FBackgroundColor := FPColor(0, 110, 160, 0);
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
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'terminus.fon'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/Chroma.png'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/alpha_beta.png'));
  //Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/ChiKareGo2.ttf'));
  Font := raylib.LoadFont(PChar(Main.WorkSpace + 'fonts/font.png'));
  SetTextureFilter(Font.texture, FILTER_POINT);
  //Font := raylib.LoadFont(PChar('computer_pixel.fon.ttf'));
  FFontSize := 18;

  FTexture := LoadRenderTexture(ScreenWidth, ScreenHeight);
  //FBoard := GetTextureData(FTexture.texture);

  BeginTextureMode(FTexture);
  ClearBackground(RayColorOf(BackgroundColor));
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

procedure TTyroCanvas.Circle(X, Y, R: Integer; Fill: Boolean);
begin
//  BeginTextureMode(FBoard);
  if Fill then
    DrawCircle(X, Y, R, RayColorOf(Color))
  else
    DrawCircleLines(X, Y, R, RayColorOf(Color));
//  EndTextureMode();
end;

procedure TTyroCanvas.Rectangle(X, Y, W, H: Integer; Fill: Boolean);
begin
  if Fill then
    DrawRectangle(X, Y, W, H, RayColorOf(Color))
  else
    DrawRectangleLines(X, Y, W, H, RayColorOf(Color));
end;

procedure TTyroCanvas.DrawText(X, Y: Integer; S: string);
begin
  DrawTextEx(Font, PChar(S), Vector2Create(x, y), FontSize, 2, RayColorOf(Color));
  //ImageDrawTextEx(@FTexture, Vector2Create(x, y), Font, PChar(S), FontSize, 2, RayColorOf(Color));
end;

procedure TTyroCanvas.Print(S: string);
begin
  //TODO
end;

procedure TTyroCanvas.Clear;
begin
  ClearBackground(RayColorOf(BackgroundColor));
end;

procedure TTyroCanvas.SetBackgroundColor(AValue: TFPColor);
begin
  if FBackgroundColor =AValue then
    Exit;
  FBackgroundColor :=AValue;
end;

{ TTyroMain }

function TTyroMain.GetActive: Boolean;
begin
  Result := not WindowShouldClose();
end;

procedure TTyroMain.ProcessPool;
var
  p: TPoolObject;
  c: Integer;
begin
  {BeginTextureMode(Canvas.Board);
  c := 0;
  while Pool.Count > 0 do
  begin
    Lock.Enter;
    try
      p := Pool.Extract(Pool[0]);
    finally
      Lock.Leave;
    end;
    p.Execute;
    p.Free;
    Inc(c);
    if c > 100 then
      break;
    //WriteLn('run: ' + p.ClassName);
  end;
  EndTextureMode;}
end;

constructor TTyroMain.Create;
begin
  inherited Create;
  //FLock := TCriticalSection.Create;
  FPool := TPoolObjects.Create(True);
  FScriptTypes := TScriptTypes.Create(true);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  DefaultBackground := ColorCreate(220, 230, 240, 0);
end;

destructor TTyroMain.Destroy;
begin
  //Stop;
  FreeAndNil(FCanvas);
  CloseWindow;
  FreeAndNil(FPool);
  //FreeAndNil(FLock);
  FreeAndNil(FScriptTypes);
  inherited Destroy;
end;

procedure TTyroMain.Start;
var
  ScriptType: TScriptType;
begin
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(ScreenWidth, ScreenHeight, PChar(Title));
  ShowCursor();
  SetTargetFPS(60);
  FCanvas := TTyroCanvas.Create;

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

  ProcessPool;

  BeginDrawing;
  ClearBackground(DefaultBackground);

  try
    {im := LoadImageEx(PColor(FScript.FPImage.Data), FScript.FPImage.Width, FScript.FPImage.Height);
    t := LoadTextureFromImage(im);
    DrawTextureRec(t, RectangleCreate(0, 0, t.width, t.height), Vector2Create(0, 0), WHITE);}

    //t := LoadTextureFromImage(FScript.Canvas.FTexture);
    //DrawTextureRec(t, RectangleCreate(0, 0, t.width, t.height), Vector2Create(0, 0), WHITE);

    with Canvas.FTexture do
      DrawTextureRec(texture, RectangleCreate(0, 0, texture.width, -texture.height), Vector2Create(0, 0), WHITE);

  finally
  end;

  Loop;
  EndDrawing;
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

procedure TTyroScript.BeforeRun;
begin
end;

procedure TTyroScript.AfterRun;
begin
end;

procedure TTyroScript.AddPoolObject(APoolObject: TPoolObject);
var
  NeedSynchronize: Boolean;
begin
  Lock.Enter;
  try
    NeedSynchronize := APoolObject.NeedSynchronize;
    Main.Pool.Add(APoolObject);
  finally
    Lock.Leave;
  end;
  Yield;
  if NeedSynchronize then
    Synchronize(@Main.ProcessPool) //work but slow as sleep(1)
  else
    Queue(@Main.ProcessPool);
end;

constructor TTyroScript.Create;
begin
  inherited Create(True);
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

