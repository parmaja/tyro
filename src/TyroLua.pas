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

  { TLuaColors }

  TLuaColors = class(TLuaObject)
  protected
    type
      TLuaColor = record
        Name: string;
        Color: TFPColor;
      end;
    var
      Colors: array of TLuaColor;
    function __setter(L: PLua_State): integer; cdecl; override;
    function __getter(L: PLua_State): integer; cdecl; override;
  public
    procedure AddColor(Name: string; AColor: TFPColor);
    constructor Create;
  end;

  { TLuaScript }

  TLuaScript = class(TTyroScript)
  private
  protected
    LuaState: Plua_State;
    LuaCanvas: TLuaCanvas;
    LuaColors: TLuaColors;
    procedure DoError(S: string);
    procedure Run; override;
    //canvas functions
    function Clear_func(L: Plua_State): Integer; cdecl;
    function DrawText_func(L: Plua_State): Integer; cdecl;
    function DrawCircle_func(L: Plua_State): Integer; cdecl;
    function DrawRectangle_func(L: Plua_State): Integer; cdecl;
    function DrawLine_func(L: Plua_State): Integer; cdecl;
    function DrawPoint_func(L: Plua_State): Integer; cdecl;
    //global functions
    function Print_func(L: Plua_State): Integer; cdecl;
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

//global functions
function sleep_func(L : Plua_State) : Integer; cdecl;
var
  n: int64;
begin
  n := round(lua_tonumber(L, 1));
  sleep(n);
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

  lua_setmetatable(L, -2);
  //end metatable

  lua_setglobal(L, pchar(table)); //set table name
  //end table
end;

procedure lua_register_table_index(L : Plua_State; table: string; obj: TLuaObject);
var
  new: Boolean;
begin
  //table
  new := lua_getglobal(L, pchar(table)) = 0; //get table by name
  if new then
    lua_newtable(L);

  //metatable
  lua_newtable(L);

  lua_push_method(L, '__index', @obj.__getter);
  lua_push_method(L, '__newindex', @obj.__setter);

  lua_setmetatable(L, -2);
  //end metatable

  if new then
    lua_setglobal(L, pchar(table)); //set table name
  //end table
end;

procedure lua_register_table_method(L : Plua_State; table: string; obj: TObject; name: string; method: lua_CMethod);
var
  new: Boolean;
begin
  new := lua_getglobal(L, pchar(table)) = 0; //get table by name
  if new then
    lua_newtable(L);
  lua_push_method(L, pchar(Name), method);

  if new then
    lua_setglobal(L, pchar(table));
end;

procedure lua_register_table_value(L : Plua_State; table, name: string; value: integer);
var
  new: Boolean;
begin
  //table
  new := lua_getglobal(L, pchar(table)) = 0; //get table by name
  if new then
    lua_newtable(L);

  lua_pushinteger(L, value);
  lua_setfield(L, -2, pchar(name));

  if new then
    lua_setglobal(L, pchar(table));
  //end metatable
end;

procedure lua_register_integer(L : Plua_State; name: string; value: integer);
begin
  lua_pushinteger(L, value);
  lua_setfield(L, -2, pchar(name));
end;

procedure lua_register_fpcolor(L : Plua_State; name: string; value: TFPColor);
begin
  lua_pushinteger(L, FPColorToInt(colRed));
  lua_setfield(L, -2, pchar(name));
end;

{ TLuaColors }

function TLuaColors.__setter(L: PLua_State): integer; cdecl;
begin
  Result := 0;
end;

function TLuaColors.__getter(L: PLua_State): integer; cdecl;
var
  c: integer;
  index: integer;
  field: string;
begin
  Result := 0;
  if lua_isnumber(L, 2) then
  begin
    index := lua_tointeger(L, 2);
    if index < Length(Colors) then
    begin
      c := FPColorToInt(Colors[index].Color);
      lua_pushinteger(L, c);
      Result := 1;
    end;
  end
  else
  begin
    field := lua_tostring(L, 2);
    case field of
      'count':
      begin
        lua_pushinteger(L, Length(Colors));
        Result := 1;
      end;
    end;
  end;
end;

procedure TLuaColors.AddColor(Name: string; AColor: TFPColor);
var
  aItem: TLuaColor;
begin
  aItem.Name := Name;
  aItem.Color := AColor;
  SetLength(Colors, Length(Colors) + 1);
  Colors[Length(Colors) -1] := aItem;
end;

constructor TLuaColors.Create;
begin
  AddColor('white', colWhite);
  AddColor('silver', colSilver);
  AddColor('gray' , colGray);
  AddColor('black', colBlack);
  AddColor('red'  , colRed);
  AddColor('maroon', colMaroon);
  AddColor('yellow', colYellow);
  AddColor('olive', colOlive);
  AddColor('lime' , colLime);
  AddColor('green', colGreen);
  AddColor('aqua' , colAqua);
  AddColor('teal' , colTeal);
  AddColor('blue' , colBlue);
  AddColor('navy' , colNavy);
  AddColor('fuchsia', colFuchsia);
  AddColor('purple', colPurple);
end;

{ TLuaCanvas }

function TLuaCanvas.__setter(L: PLua_State): integer; cdecl;
var
  i: integer;
  field: string;
begin
  Result := 0;
  field := lua_tostring(L, 2);
  case field of
    'color':
    begin
      i := Round(lua_tonumber(L, -1));
      Main.Canvas.Color := IntToFPColor(i);//thread unsafe
      Result:= 1;
    end;
    'backcolor':
    begin
      i := Round(lua_tonumber(L, -1));
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
  Result := 0;
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

procedure HookCount(L: Plua_State; ar: Plua_Debug); cdecl;
begin
  if not Main.Active then
    luaL_error(L, PChar('Terminated!'));
end;

constructor TLuaScript.Create;
var
  i: Integer;
begin
  inherited;
  LuaState := lua_newstate(@LuaAlloc, nil);
  lual_openlibs(LuaState);
  lua_sethook(LuaState, @HookCount, LUA_MASKCOUNT, 100);

  lua_register(LuaState, 'log', @log_func);
  lua_register(LuaState, 'sleep', @sleep_func);
  lua_register_method(LuaState, 'print', @DrawText_func);

//  lua_register_integer(LuaState, 'width', ScreenWidth));
//  lua_register_integer(LuaState, 'heigh', ScreenHeight));

  LuaCanvas := TLuaCanvas.Create;
  LuaColors := TLuaColors.Create;

  //lua_register_table(LuaState, 'draw', LuaCanvas);
  lua_register_table_method(LuaState, 'canvas', self, 'clear', @Clear_func);
  lua_register_table_method(LuaState, 'canvas', self, 'text', @DrawText_func);
  lua_register_table_method(LuaState, 'canvas', self, 'circle', @DrawCircle_func);
  lua_register_table_method(LuaState, 'canvas', self, 'rectangle', @DrawRectangle_func);
  lua_register_table_method(LuaState, 'canvas', self, 'line', @DrawLine_func);
  lua_register_table_method(LuaState, 'canvas', self, 'point', @DrawPoint_func);

  lua_register_table_value(LuaState, 'canvas', 'width', ScreenWidth);
  lua_register_table_value(LuaState, 'canvas', 'height', ScreenHeight);

  lua_register_table_index(LuaState, 'canvas', LuaCanvas); //Should be last one

  lua_newtable(LuaState);
  for i := 0 to Length(LuaColors.Colors) -1 do
    lua_register_fpcolor(LuaState, LuaColors.Colors[i].Name, LuaColors.Colors[i].Color);
  lua_setglobal(LuaState, 'colors');
  lua_register_table_index(LuaState, 'colors', LuaColors); //Should be last one

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

function TLuaScript.Clear_func(L: Plua_State): Integer; cdecl;
begin
  AddPoolObject(TClearObject.Create(Main.Canvas));
  Result := 0;
end;

function TLuaScript.DrawText_func(L: Plua_State): Integer; cdecl;
var
//  c: integer;
  x, y: Integer;
  s: string;
begin
//  c := lua_gettop(L);
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  s := lua_tostring(L, 3);
  AddPoolObject(TDrawTextObject.Create(Main.Canvas, x, y, s));
  Result := 0;
end;

function TLuaScript.DrawCircle_func(L : Plua_State) : Integer; cdecl;
var
  c: integer;
  x, y, r: integer;
  f: Boolean;
begin
  f := false;
  c := lua_gettop(L);
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  r := round(lua_tonumber(L, 3));
  if c >= 4 then
    f := lua_toboolean(L, 4);
  AddPoolObject(TDrawCircleObject.Create(Main.Canvas, x, y, r, f));
  Result := 0;
end;

function TLuaScript.DrawRectangle_func(L: Plua_State): Integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
  f: Boolean;
begin
  f := false;
  c := lua_gettop(L);
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  w := round(lua_tonumber(L, 3));
  h := round(lua_tonumber(L, 4));
  if c >= 4 then
    f := lua_toboolean(L, 5);
  AddPoolObject(TDrawRectangleObject.Create(Main.Canvas, x, y, w, h, f));
  Result := 0;
end;

function TLuaScript.DrawLine_func(L: Plua_State): Integer; cdecl;
var
  c: integer;
  x1, y1, x2, y2: integer;
begin
  c := lua_gettop(L);
  x1 := round(lua_tonumber(L, 1));
  y1 := round(lua_tonumber(L, 2));
  if c = 4 then
  begin
    x2 := round(lua_tonumber(L, 3));
    y2 := round(lua_tonumber(L, 4));
    AddPoolObject(TDrawLineObject.Create(Main.Canvas, x1, y1, x2, y2));
  end
  else
    AddPoolObject(TDrawLineToObject.Create(Main.Canvas, x1, y1));
  Result := 0;
end;

function TLuaScript.DrawPoint_func(L: Plua_State): Integer; cdecl;
var
  x, y: integer;
begin
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  AddPoolObject(TDrawPointObject.Create(Main.Canvas, x, y));
  Result := 0;
end;

function TLuaScript.Print_func(L: Plua_State): Integer; cdecl;
var
//  c: integer;
  x, y: Integer;
  s: string;
begin
//  c := lua_gettop(L);
  s := lua_tostring(L, 1);
  x := round(lua_tonumber(L, 2));
  y := round(lua_tonumber(L, 3));
  AddPoolObject(TDrawTextObject.Create(Main.Canvas, x, y, s));
  Result := 0;
end;

end.

