unit TyroLua;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  lua53, l4l_object,
  TyroClasses;

type

  { TLuaScript }

  TLuaScript = class(TTyroScript)
  private
  protected
    LuaStatus: Plua_State;
    procedure Run; override;
    function Circle_func(L: Plua_State): Integer; cdecl;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

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
begin
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

type
  lua_CMethod = function(L: Plua_State): Integer of object cdecl; // Lua Function

function lua_callback(L: Plua_State): Integer; cdecl;
var
  Method: TMethod;
begin
  Method.data := lua_topointer(L, lua_upvalueindex(1));
  Method.code := lua_topointer(L, lua_upvalueindex(2));
  Result := lua_CMethod(Method)(L);
end;

procedure lua_register_method(L: Plua_State; name: String; method: lua_CMethod);
var
  f: pointer;
begin
  lua_pushstring(L, PChar(name));
  lua_pushlightuserdata(L, TMethod(method).Data);
  lua_pushlightuserdata(L, TMethod(method).Code);
  lua_pushcclosure(L, @lua_callback, 2);
  lua_setglobal(L, PChar(name));
end;

procedure lua_register_function(L: Plua_State; name: String; func: lua_CFunction);
begin
  lua_register(L, PChar(name), func);
end;

constructor TLuaScript.Create;
begin
  inherited;
  LuaStatus := lua_newstate(@LuaAlloc, nil);
  lua_register(LuaStatus, 'print', @print_func);
  lua_register(LuaStatus, 'log', @log_func);
  lua_register_method(LuaStatus, 'Circle', Circle_func);
end;

destructor TLuaScript.Destroy;
begin
  lua_close(LuaStatus);
  inherited;
end;

procedure TLuaScript.Run;
var
  r: integer;
begin
  r := luaL_loadstring(LuaStatus, PChar(ScriptText.Text));
  if r = 0 then
     r := lua_pcall(LuaStatus, 0, LUA_MULTRET, 0);
end;

function TLuaScript.Circle_func(L : Plua_State) : Integer; cdecl;
var
  x, y, r: integer;
begin
  x := lua_tointeger(L, 1);
  y := lua_tointeger(L, 2);
  r := lua_tointeger(L, 3);
  Circle(x, y, r);
  Result := 0;
end;


end.

