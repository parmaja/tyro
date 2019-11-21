program tyro;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}


{$mode objfpc}
{$H+}

//ref https://github.com/raysan5/raylib/blob/master/examples/textures/textures_mouse_painting.c

uses
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, sardScripts,
  mnFields, mnUtils, mnParams,
  raylib,
  TyroClasses, TyroLua;

type

  { TTyro }

  TTyro = class(TPersistent)
  private
    FScript: TTyroScript;
    FArguments: TmnFields;
    FTitle: string;
    procedure CollectArguments;
  protected
    FileName: string;//that to run
    WorkSpace: string;
    procedure Run;
    procedure Loop; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Arguments: TmnFields read FArguments;
    property Title: string read FTitle write FTitle;
  end;

{ TTyro }

procedure TTyro.CollectArguments;
var
  i, P: Integer;
  aParam, aName, aValue: string;
  aLastField: TmnField;
begin
  aLastField := nil;
  for i := 1 to ParamCount do
  begin
    aParam := ParamStr(i);
    if CharInArray(LeftStr(aParam, 1), ['-', '/']) then
    begin
      aName := Copy(aParam, 1, Length(aParam) -1);
      P := Pos(aName, '=');
      if p > 0 then
      begin
        aValue := Copy(aName, P + 1, MaxInt);
        aName := Copy(aName, 1, P - 1);
      end
      else
        aValue := '';
      Arguments.Add(aName, DequoteStr(aValue));
    end
    else
      Arguments.Add('', aParam);
  end;

  if Arguments.Exists[''] then
    FileName := Arguments[''];
  WorkSpace := IncludePathSeparator(ExtractFilePath(ParamStr(0)));
end;

procedure TTyro.Run;
begin
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(ScreenWidth, ScreenHeight, PChar(Title));

  if FileName <> '' then
  begin
    if SysUtils.FileExists(FileName) then
      FScript.ExecuteFile(FileName);
  end;

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    BeginDrawing;
    FScript.Clear;
    Loop;
    EndDrawing;
  end;
end;

constructor TTyro.Create;
var
  i: Integer;
begin
  inherited Create;
  FArguments := TmnFields.Create;
  CollectArguments;

  FScript := TLuaScript.Create;
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
end;

destructor TTyro.Destroy;
begin
  CloseWindow;
  FreeAndNil(FScript);
  FreeAndNil(FArguments);
  inherited Destroy;
end;

procedure TTyro.Loop;
begin
  //DrawTextEx(Font, PChar('Test'), Vector2Create(100, 100), Font.baseSize, -2, Black);
  DrawTextureRec(FScript.Canvas.texture, RectangleCreate(0, 0, FScript.Canvas.texture.width, -FScript.Canvas.texture.height), Vector2Create(0, 0), WHITE);
end;

var
  Application: TTyro;

{$R *.res}

begin
  Application := TTyro.Create;
  Application.Title :='Tyro';
  Application.Run;
  Application.Free;
end.


