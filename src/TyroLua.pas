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
  lua53, FPImage,
  TyroClasses;

type

  { TLuaObject }

  TLuaObject = class abstract(TObject)
  protected
    function __setter(L: PLua_State): integer; cdecl; virtual; abstract;
    function __getter(L: PLua_State): integer; cdecl; virtual; abstract;
  end;

  { TLuaCanvas }

  TLuaCanvas = class(TLuaObject)
  protected
    function __setter(L: PLua_State): integer; cdecl; override;
    function __getter(L: PLua_State): integer; cdecl; override;
  public
  end;

  { TLuaScript }

  TLuaScript = class(TTyroScript)
  private
  protected
    LuaState: Plua_State;
    LuaCanvas: TLuaCanvas;
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

function lua_method_callback(L: Plua_State): Integer; cdecl;
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
  lua_pushcclosure(L, @lua_method_callback, 2);
  lua_setglobal(L, PChar(name));
end;

procedure lua_register_function(L: Plua_State; name: String; func: lua_CFunction);
begin
  lua_register(L, PChar(name), func);
end;

procedure lua_push_method(L: Plua_State; name: String; method: lua_CMethod);
begin
  lua_pushlightuserdata(L, TMethod(method).Data);
  lua_pushlightuserdata(L, TMethod(method).Code);
  lua_pushcclosure(L, @lua_method_callback, 2);
  lua_setfield(L, -2, pchar(name));
end;

procedure lua_register_table(L : Plua_State; table: string; obj: TLuaObject);
begin
  //table
  lua_newtable(L);

  //metatable
  lua_newtable(L);

  lua_push_method(L, '__index', @obj.__getter);
  lua_push_method(L, '__newindex', @obj.__setter);

  lua_setmetatable(L, -2);
  //end metatable

  lua_setglobal(L, pchar(table)); //set table name
  //end table
end;

procedure lua_register_table_method(L : Plua_State; table: string; obj: TObject; name: string; method: lua_CMethod);
begin
  //table
  lua_getglobal(L, pchar(table)); //get table by name
  if lua_getmetatable(L, -1) = 0 then
    lua_newtable(L);
  lua_push_method(L, pchar(Name), method);
  //lua_setmetatable(L, -2);
  //end metatable
end;


{ TLuaCanvas }

function TLuaCanvas.__setter(L: PLua_State): integer; cdecl;
var
  i: integer;
  field: string;
begin
  Result:= 0;
  field := lua_tostring(L, 2);
  case field of
    'color':
    begin
      i := Round(lua_tonumber(L, 1));
      Main.Canvas.Color := IntToFPColor(i);//thread unsafe
      Result:= 1;
    end;
    'backcolor':
    begin
      i := Round(lua_tonumber(L, 1));
      Main.Canvas.BackgroundColor := IntToFPColor(i);//thread unsafe
      Result:= 1;
    end;
  end;
end;

function TLuaCanvas.__getter(L: PLua_State): integer; cdecl;
var
  i: integer;
  field: string;
begin
  Result:= 0;
  field := lua_tostring(L, 2);
  case field of
    'color':
    begin
      i := FPColorToInt(Main.Canvas.Color);
      lua_pushnumber(L, i);
      Result := 1;
    end;
    'backcolor':
    begin
      i := FPColorToInt(Main.Canvas.BackgroundColor);
      lua_pushnumber(L, i);
      Result := 1;
    end;
  end;
end;

constructor TLuaScript.Create;
begin
  inherited;
  LuaState := lua_newstate(@LuaAlloc, nil);
  lual_openlibs(LuaState);


  lua_register(LuaState, 'log', @log_func);
  lua_register_method(LuaState, 'circle', @Circle_func);
  lua_register_method(LuaState, 'text', @Print_func);

  LuaCanvas := TLuaCanvas.Create;
  lua_register_table(LuaState, 'canvas', LuaCanvas);
  lua_register_table_method(LuaState, 'canvas', self, 'circle', @Circle_func);
  //lua_register_table(LuaState, 'color', LuaCanvas);
end;

destructor TLuaScript.Destroy;
begin
  lua_close(LuaState);
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
  r := luaL_loadstring(LuaState, PChar(ScriptText.Text));
  if r = 0 then
  begin
    r := lua_pcall(LuaState, 0, LUA_MULTRET, 0);
  end;
  if (r <> LUA_OK)  then
  begin
    Msg := lua_tostring(LuaState, -1);
    DoError(Msg);
    lua_pop(LuaState, 1);  //* remove message
  end;
end;

function TLuaScript.Print_func(L: Plua_State): Integer; cdecl;
var
//  c: integer;
  x, y: Integer;
  s: string;
begin
//  c := lua_gettop(L);
  s := lua_tostring(L, 1);
  x := lua_tointeger(L, 2);
  y := lua_tointeger(L, 3);
  AddPoolObject(TDrawTextObject.Create(Main.Canvas, s, x, y));
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
  AddPoolObject(TDrawCircleObject.Create(Main.Canvas, x, y, r, f));
  Result := 0;
end;


end.

