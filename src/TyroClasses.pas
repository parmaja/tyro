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
    //FPImage: TMyFPMemoryImage;
    //FPCanvas: TFPImageCanvas;
    FBoard: TRenderTexture2D;
    Font: raylib.TFont;
    procedure SetBackgroundColor(AValue: TFPColor);
  public
    constructor Create;
    destructor Destroy; override;
    property Board: TRenderTexture2D read FBoard;
    procedure Circle(X, Y, R: Integer; Fill: Boolean);
    procedure Rectangle(X, Y, W, H: Integer; Fill: Boolean);
    //procedure PrintTest;
    procedure DrawText(X, Y: Integer; S: string);
    procedure Print(S: string);
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
    fText: String;
    constructor Create(ACanvas: TTyroCanvas; Text: String);
    procedure Execute; override;
  end;

  { TClearObject }

  TClearObject = class(TDrawObject)
  public
    constructor Create(ACanvas: TTyroCanvas);
    procedure Execute; override;
  end;

  { TPoolObjects }

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

function IntToFPColor(I: Integer): TFPColor;
function FPColorToInt(C: TFPColor): Integer;

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

  //Font := raylib.LoadFont(PChar('computer_pixel.fon.ttf'));
  Font := GetFontDefault;
  FFontSize := Font.baseSize * 2;
  FBoard := LoadRenderTexture(ScreenWidth, ScreenHeight);
  BeginTextureMode(FBoard);
  ClearBackground(RayColorOf(BackgroundColor));
  DrawText(3, 3, 'Ready!');
  EndTextureMode();
end;

destructor TTyroCanvas.Destroy;
begin
  UnloadFont(Font);
  UnloadRenderTexture(FBoard);
  Finalize(FBoard);
  inherited;
end;

procedure TTyroCanvas.Circle(X, Y, R: Integer; Fill: Boolean);
begin
  if Fill then
    DrawCircle(X, Y, R, RayColorOf(Color))
  else
    DrawCircleLines(X, Y, R, RayColorOf(Color));
end;

procedure TTyroCanvas.Rectangle(X, Y, W, H: Integer; Fill: Boolean);
begin
  if Fill then
    DrawRectangle(X, Y, W, H, RayColorOf(Color))
  else
    DrawRectangleLines(X, Y, W, H, RayColorOf(Color));
end;

{procedure TTyroCanvas.PrintTest;
begin
  BeginTextureMode(Board);
  raylib.DrawText(PChar('Test'), 100, 100, FontSize, RayColorOf(Color));
  EndTextureMode();
end;}

procedure TTyroCanvas.DrawText(X, Y: Integer; S: string);
begin
  //raylib.DrawText(PChar(S), X, Y, FontSize, RayColorOf(Color));
  raylib.DrawTextEx(Font, PChar(S), Vector2Create(X, Y), FontSize, 2, RayColorOf(Color));
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
    WriteLn('run: ' + p.ClassName);
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
  DefaultBackground := ColorCreate(220, 230, 240, 0);
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
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(ScreenWidth, ScreenHeight, PChar(Title));
  ShowCursor();
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

  RunPool;

  BeginDrawing;
  ClearBackground(DefaultBackground);

  try
    {im := LoadImageEx(PColor(FScript.FPImage.Data), FScript.FPImage.Width, FScript.FPImage.Height);
    t := LoadTextureFromImage(im);
    DrawTextureRec(t, RectangleCreate(0, 0, t.width, t.height), Vector2Create(0, 0), WHITE);}

    //WriteLn('Copy Board');
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
  Sleep(1); //idk why?!!!!
  Yield;
end;

constructor TTyroScript.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Priority := tpLower; //hmmm
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

