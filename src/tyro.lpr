program tyro;

{$mode objfpc}
{$H+}

uses
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, CustApp,
  lua53, l4l_object, sardScripts,
  raylib,
  mnUtils, mnFields, mnParams;

const
  cScreenWidth = 640;
  cScreenHeight = 480;

type

  { TLuaTyroObject }

  TLuaTyroObject = class(TLuaObject)
  private
  protected
  public
    constructor Create(L: Plua_State); override;
    destructor Destroy; override;
    procedure Run(S: string); overload;
    procedure RunFile(FileName: string); overload;
  published
  end;

  { TTyro }

  TTyro = class(TCustomApplication)
  protected
    Camera: TCamera2D;
    procedure DoRun; override;
  public
    Arguments: TmnFields;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loop;
  end;

var
  Lua: TLuaTyroObject = nil;

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
    WriteLn(s);
  end;
  Result := 0;
end;

{ TLuaTyroObject }

constructor TLuaTyroObject.Create(L: Plua_State);
begin
  inherited Create(L);
  lua_register(L, 'print', @print_func);
  lua_register(L, 'log', @log_func);
end;

destructor TLuaTyroObject.Destroy;
begin
  lua_close(LS);
  inherited Destroy;
end;

procedure TLuaTyroObject.Run(S: string);
begin
  if luaL_loadbuffer(LS, PChar(s), Length(s), 'R') <> 0 then
    Raise Exception.Create('');
  if lua_pcall(LS, 0, 0, 0) <> 0 then
    Raise Exception.Create('');
end;

procedure TLuaTyroObject.RunFile(FileName: string);
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

procedure TTyro.DoRun;
var
  aFileName: string;
  A: TStringArray;
begin
  A := GetNonOptions('' , []);
  if Length(A) > 0 then
  begin
    aFileName := A[0];
    if SysUtils.FileExists(aFileName) then
      Lua.RunFile(aFileName);
  end;

  BeginDrawing();
  ClearBackground(RAYWHITE);
  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);
    Loop;
    EndDrawing();
  end;
  Terminate;
end;

procedure ArgumentsCallbackProc(Sender: Pointer; Index:Integer; Name, Value: string; var Resume: Boolean);
begin
  (TObject(Sender) as TmnFields).Add(Name, Value);
end;

constructor TTyro.Create(TheOwner: TComponent);
var
  L: Plua_State;
begin
  inherited Create(TheOwner);
  ParseArgumentsCallback(String(CmdLine), @ArgumentsCallbackProc, Arguments);

  L := lua_newstate(@LuaAlloc, nil);
  Lua := TLuaTyroObject.Create(L);
  StopOnException :=True;
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  InitWindow(cScreenWidth, cScreenHeight, 'Tyro');
end;

destructor TTyro.Destroy;
begin
  CloseWindow;
  FreeAndNil(Lua);
  inherited Destroy;
end;

procedure TTyro.Loop;
begin
  DrawText('Test', 190, 200, 20, BLACK);
end;

var
  Application: TTyro;
begin
  Application :=TTyro.Create(nil);
  Application.Title :='Tyro';
  Application.Run;
  Application.Free;
end.


