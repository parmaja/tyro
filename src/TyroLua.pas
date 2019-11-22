unit TyroLua;
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
  Classes, SysUtils,
  lua53, l4l_object,
  TyroClasses;

type

  { TLuaScript }

  TLuaScript = class(TTyroScript)
  private
  protected
    LuaStatus: Plua_State;
    procedure DoError(S: string);
    procedure Run; override;
    function Print_func(L: Plua_State): Integer; cdecl;
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
begin
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
  lual_openlibs(LuaStatus);
  lua_register(LuaStatus, 'log', @log_func);
  lua_register_method(LuaStatus, 'circle', @Circle_func);
  lua_register_method(LuaStatus, 'print', @Print_func);
end;

destructor TLuaScript.Destroy;
begin
  lua_close(LuaStatus);
  inherited;
end;

procedure TLuaScript.DoError(S: string);
begin
  if IsConsole then
    WriteLn(S);
end;

procedure TLuaScript.Run;
var
  r: integer;
  Msg: string;
begin
  WriteLn('Run Script');
  //Sleep(1000);
  r := luaL_loadstring(LuaStatus, PChar(ScriptText.Text));
  if r = 0 then
  begin
    r := lua_pcall(LuaStatus, 0, LUA_MULTRET, 0);
  end;
  if (r <> LUA_OK)  then
  begin
    Msg := lua_tostring(LuaStatus, -1);
    DoError(Msg);
    lua_pop(LuaStatus, 1);  //* remove message
  end;
end;

function TLuaScript.Print_func(L: Plua_State): Integer; cdecl;
var
//  c: integer;
  x, y: Integer;
  s: string;
  f: Boolean;
begin
  f := false;
//  c := lua_gettop(L);
  s := lua_tostring(L, 1);
  x := lua_tointeger(L, 2);
  y := lua_tointeger(L, 3);
  AddPoolObject(TTextObject.Create(Main.Canvas, x, y, s));
  Result := 0;
end;

function TLuaScript.Circle_func(L : Plua_State) : Integer; cdecl;
var
  c: integer;
  x, y, r: integer;
  f: Boolean;
begin
  f := false;
  c := lua_gettop(L);
  x := lua_tointeger(L, 1);
  y := lua_tointeger(L, 2);
  r := lua_tointeger(L, 3);
  if c >= 4 then
    f := lua_toboolean(L, 4);
  AddPoolObject(TCircleObject.Create(Main.Canvas, x, y, r, f));
  Result := 0;
end;


end.

