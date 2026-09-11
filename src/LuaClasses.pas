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
{$else}
{$RTTI EXPLICIT METHODS([vcPublic, vcProtected, vcPublished])}
{$endif}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, LuaAPI, Rtti, mnLogs;

type

  { TLuaObject }

  TLuaObject = class abstract(TObject)
  private
  protected
    function Setter(L: PLua_State): integer; virtual;
    function Getter(L: PLua_State): integer; virtual;
    procedure EnumMethods;
    procedure RegisterMethod(const AName: string; AParams: TStringList); virtual;
  public
    function __setter(L: PLua_State): integer; cdecl;
    function __getter(L: PLua_State): integer; cdecl;
  public
    //Name: string;
    procedure Register; virtual;
    constructor Create;
  published
  end;

  TLua = record
  private
    FVersion: Double;
  public
    State: Plua_State;
    procedure Init;
    procedure Close;
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
    function AsString: string;
  end;

  TLuaMethod = function(L: Plua_State): integer of object cdecl;
  TLuaFunction = lua_CFunction;

  { TLuaHelper }

  TLuaHelper = record helper for Plua_State
  private
    function GetParams(Index: Integer): TLuaParam;
    function GetCount: Integer;
  public
    property Count: Integer read GetCount;
    property Params[Index: Integer]: TLuaParam read GetParams; default;
    procedure RegisterGlobal(const Name: string; LuaFunction: TLuaFunction); overload;
    procedure RegisterGlobal(const Name: string; Method: TLuaMethod); overload;
    procedure RegisterGlobal(const Name: string; Value: string); overload;
    procedure RegisterGlobal(const Name: string; Value: Integer); overload;
    procedure RegisterGlobal(const Name: string; Value: Double); overload;

    procedure Register(const Name: string; Value: string); overload;
    procedure Register(const Name: string; Value: Integer); overload;
    procedure Register(const Name: string; Value: Double); overload;
{
    RegisterMethod_1 gives the C callback access to the actual Lua table, not just the underlying C/Delphi object.
    RegisterMethod_2 only knows the C object. Snippet 1 knows both.

    Here is why a callback needs the Lua table:

    The benefit is that RegisterMethod_1 gives the C callback access to the actual Lua table, not just the underlying C/Delphi object.
    RegisterMethod_2 only knows the C object. RegisterMethod_1 knows both.

    * Lua-side state: To read or write fields stored directly in the Lua table (e.g., custom properties not exposed to C).
    * Wrapper identity: If multiple Lua tables wrap the same underlying C object, the callback knows exactly which Lua table to interact with.
    * C-to-Lua callbacks: If the method registers an event (like "on animation finish"), the C code needs the Lua table reference to call back into Lua later.
    * Garbage Collection: Holding the table as an upvalue prevents the Lua table from being garbage-collected while the closure exists.

    Summary: Use RegisterMethod_2 for pure C-side logic. Use RegisterMethod_1 when the method needs to interact with the Lua-side wrapper.
}
    //RegisterMethod_1
    procedure Register(const Name: string; Method: TLuaMethod; TableStackIdx: Integer = -1); overload;
    //
    //RegisterMethod_2
    procedure Register(const Table, Name: string; AObject:TObject; Method: TLuaMethod); overload;
    procedure Register(const Name: string; LuaFunction: TLuaFunction); overload;
    //Must be last one after object fields
    procedure Register(const Table: string; AObject: TLuaObject); overload;

    procedure RegisterTable(Table: string);

    function RunString(Script: string; out Output: string): Boolean;

    procedure BeginTable;
    procedure EndTable(Table: string; AObject: TLuaObject = nil);
  end;

procedure LuaSetTerminated;

implementation

function lua_table_method_callback(L: Plua_State): integer; cdecl;
var
  Method: TMethod;
begin
  Method.Data := lua_topointer(L, lua_upvalueindex(1));
  Method.Code := lua_topointer(L, lua_upvalueindex(2));
  if Method.Data = nil then
    raise Exception.Create('Lua: cannot execute object method!');
  Result := TLuaMethod(Method)(L);
end;

procedure lua_register_global_method(L: Plua_State; Name: string; Method: TLuaMethod);
begin
  lua_pushlightuserdata(L, TMethod(method).Data);
  lua_pushlightuserdata(L, TMethod(method).Code);
  lua_pushcclosure(L, @lua_table_method_callback, 2);
  lua_setglobal(L, PChar(Name));
end;

procedure lua_push_method(L: Plua_State; Name: string; method: TLuaMethod);
begin
  lua_pushlightuserdata(L, TMethod(method).Data);
  lua_pushlightuserdata(L, TMethod(method).Code);
  lua_pushcclosure(L, @lua_table_method_callback, 2);
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

{ TLuaObject }

function TLuaObject.__setter(L: PLua_State): integer; cdecl;
begin
  Result := Setter(L);
end;

function TLuaObject.__getter(L: PLua_State): integer; cdecl;
begin
  Result := Getter(L);
end;

function TLuaObject.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaObject.Getter(L: PLua_State): integer;
begin
  Result := 0;
end;

procedure TLuaObject.EnumMethods;
var
  aContext: TRttiContext;
  aMethods: TArray<TRttiMethod>;
  procedure EnumParams(aMethod: TRttiMethod);
  var
    //aType: TRttiType;
    aParams: TStringList;
    aMethodParameter: TRttiParameter;
    aMethodParameters: TArray<TRttiParameter>;
  begin
    aParams := TStringList.Create;
    try
      aParams.NameValueSeparator := ':';
      aMethodParameters := aMethod.GetParameters;
      for aMethodParameter in aMethodParameters do
      begin
        aParams.AddPair(aMethodParameter.Name, aMethodParameter.ParamType.Name);
      end;
      RegisterMethod(aMethod.Name, aParams);
    finally
      aParams.Free;
    end;
  end;
var
  aType: TRttiType;
  aMethod: TRttiMethod;
begin
  aContext := TRttiContext.Create;
  try
    aType := aContext.GetType(ClassType);
    if aType <> nil then
    begin
      aMethods := aType.GetMethods;
      for aMethod in aMethods do
        EnumParams(aMethod);
    end;
  finally
    aContext.Free;
  end;
end;

procedure TLuaObject.RegisterMethod(const AName: string; AParams: TStringList);
begin

end;

procedure TLuaObject.Register;
begin
end;

constructor TLuaObject.Create;
begin
  inherited Create;
  EnumMethods;
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

procedure TLua.Close;
begin
  lua_close(State);
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

function TLuaParam.AsString: string;
begin
  Result := lua_tostring(State, Index);
end;

{ TLuaHelper }

function TLuaHelper.GetParams(Index: Integer): TLuaParam;
begin
  Result := Default(TLuaParam);
  Result.State := Self;
  Result.Index := Index;
end;

function TLuaHelper.GetCount: Integer;
begin
  Result := lua_gettop(Self);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Method: TLuaMethod);
begin
  lua_register_global_method(Self, Name, Method);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; LuaFunction: TLuaFunction);
begin
  lua_reg_global_function(Self, PUTF8Char(Name), LuaFunction);
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

function lua_method_callback(L: Plua_State): integer; cdecl;
var
  Method: TMethod;
begin
  Method.Data := lua_topointer(L, lua_upvalueindex(1));
  Method.code := lua_topointer(L, lua_upvalueindex(2));
  lua_pushvalue(L, lua_upvalueindex(3));
  lua_insert(L, 1);
  if Method.Data = nil then
    raise Exception.Create('Lua: cannot execute object method!');
  Result := TLuaMethod(Method)(L);
end;

procedure TLuaHelper.Register(const Name: string; Method: TLuaMethod; TableStackIdx: Integer);
begin
  lua_pushlightuserdata(Self, TMethod(method).Data);
  lua_pushlightuserdata(Self, TMethod(method).Code);
  lua_pushvalue(Self, TableStackIdx - 2);
  lua_pushcclosure(Self, @lua_method_callback, 3);
  lua_setfield(Self, -2, PUTF8Char(Name));
end;

procedure TLuaHelper.Register(const Table, Name: string; AObject: TObject;
  Method: TLuaMethod);
begin
  lua_register_table_method(Self, Table, Name, AObject, Method);
end;

procedure TLuaHelper.Register(const Name: string; LuaFunction: TLuaFunction);
begin
  lua_reg_function(Self, PUTF8Char(Name), LuaFunction);
end;

procedure TLuaHelper.Register(const Table: string; AObject: TLuaObject);
begin
  lua_register_table_index(Self, Table, AObject); //Should be last one for window
end;

procedure TLuaHelper.RegisterTable(Table: string);
begin
  lua_register_table(Self, Table);
end;

function TLuaHelper.RunString(Script: string; out Output: string): Boolean;
var
  r: integer;
  Msg: string;
begin
  r := luaL_loadstring(Self, PChar(Script));
  if r = 0 then
    r := lua_pcall(Self, 0, LUA_MULTRET, 0);
  Result := r = LUA_OK;
  if not Result then
  begin
    Output := lua_tostring(Self, -1);    ;
    lua_pop(Self, 1);  //* remove message
  end
  else
    Output := '';
end;

procedure TLuaHelper.BeginTable;
begin
  lua_newtable(Self);
end;

procedure TLuaHelper.EndTable(Table: string; AObject: TLuaObject);
begin
  lua_setglobal(Self, PUTF8Char(Table));
  if AObject <> nil then
    Register(Table, AObject); //Should be last one
end;

end.

