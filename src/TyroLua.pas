unit TyroLua;

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
    procedure Run; override;
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

function circle_func(L : Plua_State) : Integer; cdecl;
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

type
  TLuaProc = function(L: Plua_State): Integer of object; // Lua Function

function lua_callback(L: Plua_State): Integer; cdecl;
var
  Method: TMethod;
begin
  Method.data := lua_topointer(L, lua_upvalueindex(1));
  Method.code := lua_topointer(L, lua_upvalueindex(2));
  Result := TLuaProc(Method)(L);
end;

procedure lua_register_method(L: Plua_State; data, code: Pointer);
begin
  lua_pushlightuserdata(L, data);
  lua_pushlightuserdata(L, code);
  lua_pushcclosure(L, @lua_callback, 2);
end;

procedure lua_register_function(L: Plua_State; data, Code: pointer; name: String);
begin
  lua_register_method(L, data, code);
  lua_setglobal(L, pchar(name));
end;

constructor TLuaScript.Create;
begin
  inherited;
  LuaStatus := lua_newstate(@LuaAlloc, nil);
  lua_register(LuaStatus, 'print', @print_func);
  lua_register(LuaStatus, 'log', @log_func);
end;

destructor TLuaScript.Destroy;
begin
  lua_close(LuaStatus);
  inherited;
end;

procedure TLuaScript.Run;
begin
  if luaL_loadbuffer(LuaStatus, PChar(ScriptText.Text), Length(ScriptText.Text), '') <> 0 then
    Raise Exception.Create('');
  if lua_pcall(LuaStatus, 0, 0, 0) <> 0 then
    Raise Exception.Create('');
end;

end.

