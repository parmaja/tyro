unit TyroClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, fgl,
  raylib;

type

  TTyroCanvas = class(TObject)

  end;


  { TTyroScript }

  TTyroScript = class abstract(TObject)
  private
    FBackgroundColor: TFPColor;
    FFontSize: Integer;
    procedure SetBackgroundColor(AValue: TFPColor);
  protected
    FColor: TFPColor;
    FCanvas: TRenderTexture2D;
    ScriptText: TStringList;

    Font: raylib.TFont;
    procedure BeforeRun;
    procedure Run; virtual; abstract;
    procedure AfterRun;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute;
    procedure ExecuteFile(FileName: string); overload;

    procedure DrawText(S: string; X, Y: Integer);
    procedure Circle(X, Y, R: Integer; Fill: Boolean = False);
    procedure Clear;
    property Color: TFPColor read FColor write FColor;
    property FontSize: Integer read FFontSize write FFontSize;
    property BackgroundColor: TFPColor read FBackgroundColor write SetBackgroundColor;
    property Canvas: TRenderTexture2D read FCanvas;
  end;

  TTyroScriptClass = class of TTyroScript;

  TScriptType = class(TObject)
  public
    Ext: string;
    ScriptClass: TTyroScriptClass;
  end;

  TScriptTypes = class(specialize TFPGObjectList<TScriptType>)
  end;

  { TTyroMain }

  TTyroMain = class(TObject)
  protected
    FScript: TTyroScript;
  public
    Title: string;
    FileName: string;//that to run
    WorkSpace: string;
    constructor Create;
    procedure Run;
    procedure Loop;
    destructor Destroy; override;
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

{ TTyroMain }

constructor TTyroMain.Create;
begin
  inherited Create;
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  FScript := TLuaScript.Create;
end;

procedure TTyroMain.Run;
begin
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(ScreenWidth, ScreenHeight, PChar(Title));
  SetTargetFPS(60);

  FScript.Clear;

  if FileName <> '' then
  begin
    if SysUtils.FileExists(FileName) then
      FScript.ExecuteFile(FileName);
  end;

  while not WindowShouldClose() do
  begin
    BeginDrawing;
    Loop;
    EndDrawing;
  end;
end;

procedure TTyroMain.Loop;
begin
  //DrawTextEx(Font, PChar('Test'), Vector2Create(100, 100), Font.baseSize, -2, Black);
  DrawTextureRec(FScript.Canvas.texture, RectangleCreate(0, 0, FScript.Canvas.texture.width, -FScript.Canvas.texture.height), Vector2Create(0, 0), WHITE);
end;

destructor TTyroMain.Destroy;
begin
  FreeAndNil(FScript);
  CloseWindow;
  inherited Destroy;
end;

{ TTyroScript }

procedure TTyroScript.Circle(X, Y, R: Integer; Fill: Boolean);
begin
  if Fill then
    DrawCircle(X, Y, R, RayColorOf(Color))
  else
    DrawCircleLines(X, Y, R, RayColorOf(Color));
end;

procedure TTyroScript.SetBackgroundColor(AValue: TFPColor);
begin
  if FBackgroundColor =AValue then
    Exit;
  FBackgroundColor :=AValue;
end;

procedure TTyroScript.BeforeRun;
begin
  FCanvas := LoadRenderTexture(ScreenWidth, ScreenHeight);
  // Clear render texture before entering the game loop
  BeginTextureMode(Canvas);
  DrawText('Welcome To Tyro', 10, 10);
  Clear;
end;

procedure TTyroScript.AfterRun;
begin
  EndTextureMode();
end;

constructor TTyroScript.Create;
begin
  inherited Create;
  ScriptText := TStringList.Create;
  FFontSize := 8;
  FColor := colBlack;
  FBackgroundColor := colWhite;

//  Font := raylib.LoadFontEx(PChar('terminus.ttf'), 32, nil, 250);
end;

procedure TTyroScript.Clear;
begin
  ClearBackground(RayColorOf(BackgroundColor));
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

procedure TTyroScript.ExecuteFile(FileName: string);
begin
  ScriptText.LoadFromFile(FileName);
  Execute;
end;

procedure TTyroScript.DrawText(S: string; X, Y: Integer);
begin
  raylib.DrawText(PChar(S), X, Y, FontSize, RayColorOf(Color));
end;


end.

