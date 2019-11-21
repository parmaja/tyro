unit TyroClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage,
  raylib;

type

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
    procedure Circle(X, Y, R: Integer);
    procedure Clear;
    property Color: TFPColor read FColor write FColor;
    property FontSize: Integer read FFontSize write FFontSize;
    property BackgroundColor: TFPColor read FBackgroundColor write SetBackgroundColor;
    property Canvas: TRenderTexture2D read FCanvas;
  end;

var
  ScreenWidth: Integer = 640; //Temporary here, i will move it to Tyro object
  ScreenHeight: Integer = 480;

implementation

function RayColorOf(Color: TFPColor): raylib.TColor;
begin
  Result.a := Lo(Color.Alpha);
  Result.r := Lo(Color.Red);
  Result.g := Lo(Color.Green);
  Result.b := Lo(Color.Blue);
end;

{ TTyroScript }

procedure TTyroScript.Circle(X, Y, R: Integer);
begin
  DrawCircle(X, Y, R, RayColorOf(Color));
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

