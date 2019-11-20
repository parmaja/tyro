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
  SysUtils, Classes, lua53, l4l_object, sardScripts,
  raylib, mnFields,
  mnUtils, mnParams;

const
  cScreenWidth = 640;
  cScreenHeight = 480;

type

  { TLuaScript }

  TLuaScript = class(TLuaObject)
  private
  protected
    Canvas: TRenderTexture2D;
  public
    constructor Create(L: Plua_State); override;
    destructor Destroy; override;
    procedure BeforeRun;
    procedure AfterRun;
    procedure Run(S: string); overload;
    procedure RunFile(FileName: string); overload;
  published
  end;

  { TTyro }

  TTyro = class(TPersistent)
  private
    Font: TFont;
    FArguments: TmnFields;
    FTitle: string;
    procedure CollectArguments;
  protected
    FileName: string;//that to run
    WorkSpace: string;
    Camera: TCamera2D;
    procedure Run;
    procedure Loop; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Arguments: TmnFields read FArguments;
    property Title: string read FTitle write FTitle;
  end;

var
  Lua: TLuaScript = nil;

function LuaAlloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t) : Pointer; cdecl;
begin
  try
    Result:= ptr;
    ReallocMem(Result, nSize);
  except
    Result:= nil;
  end;
end;

function print_func(L : Plua_State) : Integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := lua_gettop(L);
  s := lua_tostring(L, i);
  Result := 0;
end;

function log_func(L : Plua_State) : Integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := lua_gettop(L);
  for i := 1 to c do
  begin
    s := lua_tostring(L, i);
    if IsConsole then
      WriteLn(s);
  end;
  Result := 0;
end;

{ TLuaScript }

constructor TLuaScript.Create(L: Plua_State);
begin
  inherited Create(L);
  lua_register(L, 'print', @print_func);
  lua_register(L, 'log', @log_func);
end;

destructor TLuaScript.Destroy;
begin
  lua_close(LS);
  inherited Destroy;
end;

procedure TLuaScript.BeforeRun;
begin
  Canvas := LoadRenderTexture(cScreenWidth, cScreenHeight);
  // Clear render texture before entering the game loop
  BeginTextureMode(Canvas);
  ClearBackground(WHITE);
end;

procedure TLuaScript.AfterRun;
begin
  EndTextureMode();
end;

procedure TLuaScript.Run(S: string);
begin
  BeforeRun;
  DrawText('Test', 10, 10, 20, BLACK);

  if luaL_loadbuffer(LS, PChar(s), Length(s), 'R') <> 0 then
    Raise Exception.Create('');
  if lua_pcall(LS, 0, 0, 0) <> 0 then
    Raise Exception.Create('');

  AfterRun;
end;

procedure TLuaScript.RunFile(FileName: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.LoadFromFile(FileName);
  try
    Run(s.Text);
  finally
    s.Free;
  end;
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
  InitWindow(cScreenWidth, cScreenHeight, PChar(Title));

  if FileName <> '' then
  begin
    if SysUtils.FileExists(FileName) then
      Lua.RunFile(FileName);
  end;

  Font := LoadFontEx(PChar('terminus.ttf'), 32, nil, 250);

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);
    Loop;
    EndDrawing();
  end;
end;

constructor TTyro.Create;
var
  L: Plua_State;
  i: Integer;
begin
  inherited Create;
  FArguments := TmnFields.Create;
  CollectArguments;

  L := lua_newstate(@LuaAlloc, nil);
  Lua := TLuaScript.Create(L);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
end;

destructor TTyro.Destroy;
begin
  CloseWindow;
  FreeAndNil(Lua);
  FreeAndNil(FArguments);
  inherited Destroy;
end;

procedure TTyro.Loop;
begin
  //DrawTextEx(Font, PChar('Test'), Vector2Create(100, 100), Font.baseSize, -2, Black);
  DrawTextureRec(Lua.Canvas.texture, RectangleCreate(0, 0, Lua.Canvas.texture.width, -Lua.Canvas.texture.height), Vector2Create(0, 0), WHITE);
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


