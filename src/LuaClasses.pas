unit LuaClasses;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

{$ifdef fpc}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, LuaAPI;

type

  { TLuaObject }

  TLuaObject = class abstract(TObject)
  private
  protected
    function __setter(L: PLua_State): integer; cdecl; virtual; abstract;
    function __getter(L: PLua_State): integer; cdecl; virtual; abstract;
  public
  end;

  TLua = record
  private
    FVersion: Double;
  public
    State: Plua_State;
    procedure Init;
    property Version: Double read FVersion;
  end;

  { TLuaParam }

  TLuaParam = record
  private
    Index: Integer;
    State: Plua_State;
  public
    function AsInteger: Integer;
    function AsNumber: Double;
  end;

  TLuaMethod = function(L: Plua_State): integer of object cdecl;
  TLuaFunction = lua_CFunction;

  { TLuaHelper }

  TLuaHelper = record helper for Plua_State
  private
    function GetParams(Index: Integer): TLuaParam;
    function GetParamsCount: Integer;
  public
    property ParamsCount: Integer read GetParamsCount;
    property Params[Index: Integer]: TLuaParam read GetParams; default;
    procedure Register(const Name: string; Method: TLuaMethod); overload;
    procedure Register(const Name: string; LuaFunction: TLuaFunction); overload;

    procedure RegisterGlobal(const Name: string; Value: string); overload;
    procedure RegisterGlobal(const Name: string; Value: Integer); overload;
    procedure RegisterGlobal(const Name: string; Value: Double); overload;

    procedure Register(const Name: string; Value: string); overload;
    procedure Register(const Name: string; Value: Integer); overload;
    procedure Register(const Name: string; Value: Double); overload;

    procedure Register(const Table, Name: string; AObject:TObject; Method: TLuaMethod); overload;
    //Must be last one after object fields
    procedure Register(const Table: string; AObject: TLuaObject); overload;

    procedure RegisterTable(Table: string);

    procedure Register(const Table, Name: string; Value: string); overload;


  end;

procedure LuaSetTerminated;

implementation

function lua_method_callback(L: Plua_State): integer; cdecl;
var
  Method: TMethod;
begin
  Method.Data := lua_topointer(L, lua_upvalueindex(1));
  Method.Code := lua_topointer(L, lua_upvalueindex(2));
  if Method.Data = nil then
    raise Exception.Create('Lua: cannot execute object method!');
  Result := TLuaMethod(Method)(L);
end;

procedure lua_register_method(L: Plua_State; Name: string; method: TLuaMethod);
begin
  lua_pushlightuserdata(L, TMethod(method).Data);
  lua_pushlightuserdata(L, TMethod(method).Code);
  lua_pushcclosure(L, @lua_method_callback, 2);
  lua_setglobal(L, PChar(Name));
end;

procedure lua_register_function(L: Plua_State; Name: string; func: lua_CFunction);
begin
  lua_register(L, PChar(Name), func);
end;

procedure lua_push_method(L: Plua_State; Name: string; method: TLuaMethod);
begin
  lua_pushlightuserdata(L, TMethod(method).Data);
  lua_pushlightuserdata(L, TMethod(method).Code);
  lua_pushcclosure(L, @lua_method_callback, 2);
  lua_setfield(L, -2, PChar(Name));
end;

procedure lua_register_table(L: Plua_State; table: string);
begin
  //table
  lua_newtable(L);
  //metatable
  lua_newtable(L); //Yes again
  lua_setmetatable(L, -2);
  //end metatable
  lua_setglobal(L, PChar(table)); //set table name
  //end table
end;

procedure lua_register_table_index(L: Plua_State; table: string; obj: TLuaObject);
var
  new: boolean;
begin
  //table
  new := lua_getglobal(L, PChar(table)) = 0; //get table by name
  if new then
    lua_newtable(L);

  //metatable
  lua_newtable(L);

  lua_push_method(L, '__index', obj.__getter);
  lua_push_method(L, '__newindex', obj.__setter);

  lua_setmetatable(L, -2);
  //end metatable

  if new then
    lua_setglobal(L, PChar(table)) //set table name
  else
    lua_pop(L, 1); //pop table from stack
  //end table
end;

procedure lua_register_table_method(L: Plua_State; table: string; Name: string; obj: TObject; method: TLuaMethod);
var
  new: boolean;
begin
  new := lua_getglobal(L, PChar(table)) = 0; //get table by name
  if new then
    lua_newtable(L);
  lua_push_method(L, PChar(Name), method);

  if new then
    lua_setglobal(L, PChar(table))
  else
    lua_pop(L, 1); //pop table from stack
end;

procedure lua_register_table_value(L: Plua_State; table, Name: string; Value: integer);
var
  new: boolean;
begin
  //table
  new := lua_getglobal(L, PChar(table)) = 0; //get table by name
  if new then
    lua_newtable(L);

  lua_pushinteger(L, Value);
  lua_setfield(L, -2, PChar(Name));

  if new then
    lua_setglobal(L, PChar(table))
  else
    lua_pop(L, 1); //pop table from stack
  //end metatable
end;

procedure lua_register_string(L: Plua_State; Name: string; Value: string);
begin
  lua_pushstring(L, Value);
  lua_setfield(L, -2, PChar(Name));
end;

procedure lua_register_integer(L: Plua_State; Name: string; Value: integer);
begin
  lua_pushinteger(L, Value);
  lua_setfield(L, -2, PChar(Name));
end;

procedure lua_register_number(L: Plua_State; Name: string; Value: Double);
begin
  lua_pushnumber(L, Value);
  lua_setfield(L, -2, PChar(Name));
end;

procedure lua_register_global_integer(L: Plua_State; Name: string; Value: integer);
begin
  lua_pushinteger(L, Value);
  lua_setglobal(L, PChar(Name));
end;

procedure lua_register_global_number(L: Plua_State; Name: string; Value: double);
begin
  lua_pushnumber(L, Value);
  lua_setglobal(L, PChar(Name));
end;

procedure lua_register_global_string(L: Plua_State; Name: string; Value: string);
begin
  lua_pushstring(L, Value);
  lua_setglobal(L, PChar(Name));
end;

function LuaAlloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t): Pointer; cdecl;
begin
  try
    Result := ptr;
    ReallocMem(Result, nSize);
  except
    Result := nil;
  end;
end;

type
  TLuaStatus = (luaNone, luaReady, luaRunning, luaTerminated);

threadvar
  LuaStatus: TLuaStatus;

procedure LuaSetTerminated;
begin
  LuaStatus := luaTerminated;
end;

procedure HookCount(L: Plua_State; ar: Plua_Debug); cdecl;
begin
  if LuaStatus >= luaTerminated then
    luaL_error(L, PChar('Terminated by user!'));
end;

{ TLua }

procedure TLua.Init;
begin
  Self := Default(TLua);
  State := lua_newstate(@LuaAlloc, nil, 0);
  FVersion := lua_version(State);
  //All libraries
  luaL_openselectedlibs(State, -1, 0);
  LuaStatus := luaReady;
  lua_sethook(State, @HookCount, LUA_MASKCOUNT, 100);
end;

{ TLuaParam }

function TLuaParam.AsInteger: Integer;
begin
  Result := lua_tointeger(State, Index);
end;

function TLuaParam.AsNumber: Double;
begin
  Result := lua_tonumber(State, Index);
end;

{ TLuaHelper }

function TLuaHelper.GetParams(Index: Integer): TLuaParam;
begin
  Result := Default(TLuaParam);
  Result.State := Self;
  Result.Index := Index;
end;

function TLuaHelper.GetParamsCount: Integer;
begin
  Result := lua_gettop(Self);
end;

procedure TLuaHelper.Register(const Name: string; Method: TLuaMethod);
begin
  lua_register_method(Self, Name, Method);
end;

procedure TLuaHelper.Register(const Name: string; LuaFunction: TLuaFunction);
begin
  lua_register(Self, PUTF8Char(Name), LuaFunction);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Value: string);
begin
  lua_register_global_string(Self, Name, Value);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Value: Integer);
begin
  lua_register_global_integer(Self, Name, Value);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Value: Double);
begin
  lua_register_global_number(Self, Name, Value);
end;

procedure TLuaHelper.Register(const Name: string; Value: string);
begin
  lua_register_string(Self, Name, Value);
end;

procedure TLuaHelper.Register(const Name: string; Value: Integer);
begin
  lua_register_integer(Self, Name, Value);
end;

procedure TLuaHelper.Register(const Name: string; Value: Double);
begin
  lua_register_number(Self, Name, Value);
end;

procedure TLuaHelper.Register(const Table, Name: string; AObject: TObject; Method: TLuaMethod);
begin
  lua_register_table_method(Self, Table, Name, AObject, Method);
end;

procedure TLuaHelper.Register(const Table: string; AObject: TLuaObject);
begin
  lua_register_table_index(Self, Table, AObject); //Should be last one for window
end;

procedure TLuaHelper.RegisterTable(Table: string);
begin
  lua_register_table(Self, Table);
end;

procedure TLuaHelper.Register(const Table, Name: string; Value: string);
begin
  //TODO
end;

end.

