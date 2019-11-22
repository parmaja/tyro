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

//TODO to stop lua https://stackoverflow.com/questions/6913999/forcing-a-lua-script-to-exit
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

  { TTyroScript }

  TTyroScript = class abstract(TThread)
  private
  protected
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
    Ext: string;
    ScriptClass: TTyroScriptClass;
  end;

  TScriptTypes = class(specialize TFPGObjectList<TScriptType>)
  end;

  { TTyroCanvas }

  TTyroCanvas = class(TObject)
  private
    FBackgroundColor: TFPColor;
    FFontSize: Integer;
    FColor: TFPColor;
    FPImage: TMyFPMemoryImage;
    FPCanvas: TFPImageCanvas;
    FBoard: TRenderTexture2D;
    Font: raylib.TFont;
    procedure SetBackgroundColor(AValue: TFPColor);
  public
    constructor Create;
    destructor Destroy; override;
    property Board: TRenderTexture2D read FBoard;
    procedure Circle(X, Y, R: Integer; Fill: Boolean);
    //procedure PrintTest;
    procedure DrawText(S: string; X, Y: Integer);
    procedure Clear;
    property Color: TFPColor read FColor write FColor;
    property FontSize: Integer read FFontSize write FFontSize;
    property BackgroundColor: TFPColor read FBackgroundColor write SetBackgroundColor;
  end;

  TPoolObject = class abstract(TObject)
  public
    procedure Execute; virtual; abstract;
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

  { TCircleObject }

  TCircleObject = class(TDrawObject)
  public
    fX, fY, fR: Integer;
    fFill: Boolean;
    constructor Create(ACanvas: TTyroCanvas; X, Y, R: Integer; Fill: Boolean);
    procedure Execute; override;
  end;

  { TTextObject }

  TTextObject = class(TDrawObject)
  public
    fX, fY: Integer;
    fText: String;
    constructor Create(ACanvas: TTyroCanvas; X, Y: Integer; Text: String);
    procedure Execute; override;
  end;

  TPoolObjects = class(specialize TFPGObjectList<TPoolObject>)
  public
  end;

  { TTyroMain }

  TTyroMain = class(TObject)
  private
    function GetActive: Boolean;
  protected
    FPool: TPoolObjects;
    FScript: TTyroScript;
    FLock: TCriticalSection;
    FCanvas: TTyroCanvas;
  protected
    DefaultBackground: raylib.TColor;

    procedure RunPool;
    property Lock: TCriticalSection read FLock;
    property Pool: TPoolObjects read FPool;
  public
    Title: string;
    FileName: string;//that to run in script
    WorkSpace: string;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure Start;
    procedure Stop;
    procedure Run;
    procedure Loop;
    property Canvas: TTyroCanvas read FCanvas;
    property Active: Boolean read GetActive;
  end;

var
  ScreenWidth: Integer = 640; //Temporary here, i will move it to Tyro object
  ScreenHeight: Integer = 480;

var
  Main : TTyroMain = nil;

implementation

uses
  TyroLua;

function RayColorOf(Color: TFPColor): raylib.TColor;
begin
  Result.a := Lo(Color.Alpha);
  Result.r := Lo(Color.Red);
  Result.g := Lo(Color.Green);
  Result.b := Lo(Color.Blue);
end;

{ TTextObject }

constructor TTextObject.Create(ACanvas: TTyroCanvas; X, Y: Integer; Text: String);
begin
  fX := X;
  fY := Y;
  fText := Text;
end;

procedure TTextObject.Execute;
begin
  BeginTextureMode(Canvas.Board);
  raylib.DrawText(PChar(fText), fX, fY, Canvas.FontSize, RayColorOf(Canvas.Color));
  EndTextureMode();
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

{ TCircleObject }

constructor TCircleObject.Create(ACanvas: TTyroCanvas; X, Y, R: Integer; Fill: Boolean);
begin
  inherited Create(ACanvas);
  fX := X;
  fY := Y;
  fR := R;
  fFill := Fill;
end;

procedure TCircleObject.Execute;
begin
  BeginTextureMode(Canvas.Board);
  if fFill then
    DrawCircle(fX, fY, fR, RayColorOf(Canvas.Color))
  else
    DrawCircleLines(fX, fY, fR, RayColorOf(Canvas.Color));
  EndTextureMode();
end;

{ TTyroCanvas }

constructor TTyroCanvas.Create;
begin
  FFontSize := 8;
  FColor := colBlack;
  FBackgroundColor := colWhite;
  FPImage := TMyFPMemoryImage.Create(100,100);
  FPImage.LoadFromFile(Main.WorkSpace + 'richard-say.png');
  FPImage.UsePalette:=false;
  FPCanvas := TFPImageCanvas.Create(FPImage);
  //FPCanvas.Pen.FPColor := colRed;
  //FPCanvas.Rectangle(10, 10, 10 , 10);
  //  Font := raylib.LoadFontEx(PChar('terminus.ttf'), 32, nil, 250);
  FBoard := LoadRenderTexture(ScreenWidth, ScreenHeight);
  BeginTextureMode(FBoard);
  ClearBackground(RayColorOf(colWhite));
  raylib.DrawText('Ready!', 3, 3, 8, BLACK);
  EndTextureMode();
end;

destructor TTyroCanvas.Destroy;
begin
  UnloadRenderTexture(FBoard);
  Finalize(FBoard);
  inherited Destroy;
end;

procedure TTyroCanvas.Circle(X, Y, R: Integer; Fill: Boolean);
begin
  // Clear render texture before entering the game loop
  Main.Lock.Enter;
  try
    BeginTextureMode(Board);
    if Fill then
      DrawCircle(X, Y, R, RayColorOf(Color))
    else
      DrawCircleLines(X, Y, R, RayColorOf(Color));
    EndTextureMode();
  finally
    Main.Lock.Leave;
  end;
end;

{procedure TTyroCanvas.PrintTest;
begin
  BeginTextureMode(Board);
  raylib.DrawText(PChar('Test'), 100, 100, FontSize, RayColorOf(Color));
  EndTextureMode();
end;}

procedure TTyroCanvas.DrawText(S: string; X, Y: Integer);
begin
  // Clear render texture before entering the game loop
  Main.Lock.Enter;
  try
    BeginTextureMode(Board);
    raylib.DrawText(PChar(S), X, Y, FontSize, RayColorOf(Color));
    EndTextureMode();
  finally
    Main.Lock.Leave;
  end;
end;

procedure TTyroCanvas.Clear;
begin
  Main.Lock.Enter;
  try
    BeginTextureMode(Board);
    ClearBackground(RayColorOf(BackgroundColor));
    EndTextureMode();
  finally
    Main.Lock.Leave;
  end;
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

procedure TTyroMain.RunPool;
var
  p: TPoolObject;
begin
  BeginTextureMode(Canvas.Board);
  while Pool.Count > 0 do
  begin
    Lock.Enter;
    try
      p := Pool.Extract(Pool[0]);
    finally
      Lock.Leave;
    end;
    p.Execute;
  end;
  EndTextureMode;
end;

constructor TTyroMain.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FPool := TPoolObjects.Create(True);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
end;

destructor TTyroMain.Destroy;
begin
  //Stop;
  FreeAndNil(FCanvas);
  CloseWindow;
  FreeAndNil(FPool);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TTyroMain.Init;
begin
  DefaultBackground := ColorCreate(0, 110, 160, 0);

  InitWindow(ScreenWidth, ScreenHeight, PChar(Title));
  SetTargetFPS(60);
  FCanvas := TTyroCanvas.Create;
end;

procedure TTyroMain.Start;
begin
  if FileName <> '' then
  begin
    FScript := TLuaScript.Create;
    if SysUtils.FileExists(FileName) then
      FScript.LoadFile(FileName);
  end;
end;

procedure TTyroMain.Run;
//var
{  t: TTexture2D;
  im: TImage;}
begin
  if (FScript <> nil) and FScript.Suspended then
    FScript.Start;

  //RunPool;

  BeginDrawing;
  ClearBackground(DefaultBackground);

  try
    {im := LoadImageEx(PColor(FScript.FPImage.Data), FScript.FPImage.Width, FScript.FPImage.Height);
    t := LoadTextureFromImage(im);
    DrawTextureRec(t, RectangleCreate(0, 0, t.width, t.height), Vector2Create(0, 0), WHITE);}

    with Canvas.FBoard do
      DrawTextureRec(texture, RectangleCreate(0, 0, texture.width, -texture.height), Vector2Create(0, 0), WHITE);
  finally
    Lock.Leave;
  end;
  Loop;
  EndDrawing;
end;

procedure TTyroMain.Loop;
begin
  //DrawTextEx(Font, PChar('Test'), Vector2Create(100, 100), Font.baseSize, -2, Black);
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
begin
  Main.Lock.Enter;
  try
    Main.Pool.Add(APoolObject);
  finally
    Main.Lock.Leave;
  end;
  Synchronize(@Main.RunPool);
end;

constructor TTyroScript.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  ScriptText := TStringList.Create;
end;

destructor TTyroScript.Destroy;
begin
  FreeAndNil(ScriptText);
  inherited Destroy;
end;

procedure TTyroScript.Execute;
begin
  BeforeRun;
  Run;
  AfterRun;
end;

procedure TTyroScript.LoadFile(FileName: string);
begin
  ScriptText.LoadFromFile(FileName);
end;

end.

