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
{$MINENUMSIZE 4}

interface

uses
  Classes, SysUtils, Rtti, SyncObjs,
  LuaAPI,
  mnLogs;

type
  { Re-exported Lua types so callers can use LuaClasses without depending on LuaAPI directly }
  //Plua_State = LuaAPI.Plua_State;
  lua_CFunction = LuaAPI.lua_CFunction;
  lua_Debug = LuaAPI.lua_Debug;

  TLuaStatus = (luaNone, luaReady, luaRunning, luaTerminated);

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
    function GetStatus: TLuaStatus;
    procedure SetStatus(AStatus: TLuaStatus);
  public
    State: Plua_State;
    procedure Init(SafeMode: Boolean = True; HookCount: Integer = 1000);
    procedure Close;
    procedure SetReady;
    procedure SetTerminated;
    property Version: Double read FVersion;
    property Status: TLuaStatus read GetStatus write SetStatus;
  end;

  { TLuaParam }

  TLuaMethod = function(L: Plua_State): integer of object cdecl;
  TLuaFunction = lua_CFunction;

  { TLuaHelper }

  { Every string in TLuaHelper is declared as plain `string` so the same code
    compiles for both FPC and Delphi: `string` is AnsiString under FPC and
    UnicodeString under Delphi. The UTF8String conversion happens once, at
    each LuaApi call site, instead of leaking UTF8String through the public
    API and forcing every caller to convert by hand. }
  TLuaHelper = record helper for lua_State
  private
    function GetArgsCount: Integer;
  public
    property ArgsCount: Integer read GetArgsCount;

    function FunctionExists(const FunctionName: string): Boolean;

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
    RegisterMethod_2 only knows the C object.

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
    procedure Register(const Name: string; Method: TLuaMethod); overload;

    procedure RegisterMethod(const Name: string; Method: TLuaMethod); overload;
    //
    //RegisterMethod_2
    procedure Register(const Table, Name: string; AObject:TObject; Method: TLuaMethod; AToMeta: Boolean = False); overload;
    procedure Register(const Name: string; LuaFunction: TLuaFunction); overload;
    //Must be last one after object fields
    procedure Register(const Table: string; AObject: TLuaObject); overload;
    //Set a method into the table that is just below the top of the stack (no self table injected); used for metamethods
    procedure RegisterMeta(const Name: string; Method: TLuaMethod); overload;

    //Low-level stack access used by the C callbacks; keeps the callers independent of LuaAPI
    function ToString(Index: Integer): string; overload;
    function ToString(Index: Integer; const Name: string): string; overload;
    function PopString: string;

    function ToNumber(Index: Integer): Double;

    function ToBoolean(Index: Integer): Boolean;

    function ToInteger(Index: Integer): lua_Integer; overload;
    function ToInteger(Index: Integer; const Name: string): Integer; overload;
    function PopInteger: Integer; overload;

    function IsInteger(Index: Integer): Boolean;
    function IsNumber(Index: Integer): Boolean;
    function IsNil(Index: Integer): Boolean;
    function IsString(Index: Integer): Boolean;
    function IsTable(Index: Integer): Boolean;

    procedure PushBoolean(Value: Boolean);
    procedure PushInteger(Value: lua_Integer);
    procedure PushNumber(Value: Double);
    procedure PushString(const Value: string);
    procedure PushNil;

    procedure PushValue(Index: Integer);

    procedure Pop(Count: Integer = 1);

    procedure NewTable;
    procedure BeginTable;
    procedure SetMetaTable(Index: Integer);
    procedure EndTableGlobal(Table: string; AObject: TLuaObject = nil);

    procedure GetField(Index: Integer; const Name: string);
    procedure SetField(Index: Integer; const Name: string);


    procedure Remove(Index: Integer);

    function GetStack(Level: Integer; var AInfo: lua_Debug): Boolean;
    function GetInfo(const What: string; var AInfo: lua_Debug): Boolean;

    procedure RegisterTable(Table: string);
    //Run
    function LoadString(const Script: string): Integer;

    function Run(Ref: Integer; out Output: string): Boolean; overload;

    function RunString(Script: string; out Output: string): Boolean;

  end;

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
  lua_setglobal(L, PUTF8Char(UTF8String(Name)));
end;

procedure lua_push_method(L: Plua_State; Name: string; method: TLuaMethod);
begin
  lua_pushlightuserdata(L, TMethod(method).Data);
  lua_pushlightuserdata(L, TMethod(method).Code);
  lua_pushcclosure(L, @lua_table_method_callback, 2);
  lua_setfield(L, -2, PUTF8Char(UTF8String(Name)));
end;

//Fetch the global table by name, pushing it. When the global is absent or holds
//a non-table value (nil/string/number), the old value is discarded and a fresh
//table is pushed instead. Returns True when the table was just created.
//The stack is balanced on exit (exactly the table, nothing else), so callers
//cannot leak the nil that lua_getglobal pushes for a missing global.
function lua_get_or_create_table(L: Plua_State; const table: string): Boolean;
begin
  lua_getglobal(L, PUTF8Char(UTF8String(table)));
  if lua_type(L, -1) = LUA_TTABLE then
    Result := False
  else
  begin
    lua_pop(L, 1); //discard the non-table value (or nil)
    lua_newtable(L);
    Result := True;
  end;
end;

procedure lua_register_table(L: Plua_State; table: string);
begin
  //table
  lua_newtable(L);
  //metatable
  lua_newtable(L); //Yes again
  lua_setmetatable(L, -2);
  //end metatable
  lua_setglobal(L, PUTF8Char(UTF8String(table))); //set table name
  //end table
end;

procedure lua_register_table_index(L: Plua_State; table: string; obj: TLuaObject);
begin
  //table
  lua_get_or_create_table(L, table); //push the table

  //metatable
  lua_newtable(L);

  lua_push_method(L, '__index', obj.__getter);
  lua_push_method(L, '__newindex', obj.__setter);

  lua_setmetatable(L, -2);
  //end metatable

  lua_setglobal(L, PUTF8Char(UTF8String(table))); //set table name; pops the table
end;

procedure lua_register_table_method(L: Plua_State; table: string; Name: string; obj: TObject; method: TLuaMethod; AToMeta: Boolean);
begin
  lua_get_or_create_table(L, table); //push the table
  if AToMeta then
  begin
    if lua_getmetatable(L, -1) = 0 then
    begin
      lua_pop(L, 1);
      lua_newtable(L);
      lua_setmetatable(L, -2);
      lua_getmetatable(L, -1);
    end;
  end;
  lua_push_method(L, Name, method);

  if AToMeta then
    lua_pop(L, 2) //pop table and metatable from stack
  else
    lua_setglobal(L, PUTF8Char(UTF8String(table))); //set table name; pops the table
end;

procedure lua_register_table_value(L: Plua_State; table, Name: string; Value: integer);
begin
  lua_get_or_create_table(L, table); //push the table
  lua_pushinteger(L, Value);
  lua_setfield(L, -2, PUTF8Char(UTF8String(Name)));
  lua_setglobal(L, PUTF8Char(UTF8String(table))); //set table name; pops the table
end;

procedure lua_register_string(L: Plua_State; Name: string; Value: string);
begin
  lua_pushstring(L, PUTF8Char(UTF8String(Value)));
  lua_setfield(L, -2, PUTF8Char(UTF8String(Name)));
end;

procedure lua_register_integer(L: Plua_State; Name: string; Value: integer);
begin
  lua_pushinteger(L, Value);
  lua_setfield(L, -2, PUTF8Char(UTF8String(Name)));
end;

procedure lua_register_number(L: Plua_State; Name: string; Value: Double);
begin
  lua_pushnumber(L, Value);
  lua_setfield(L, -2, PUTF8Char(UTF8String(Name)));
end;

procedure lua_register_global_integer(L: Plua_State; Name: string; Value: integer);
begin
  lua_pushinteger(L, Value);
  lua_setglobal(L, PUTF8Char(UTF8String(Name)));
end;

procedure lua_register_global_number(L: Plua_State; Name: string; Value: double);
begin
  lua_pushnumber(L, Value);
  lua_setglobal(L, PUTF8Char(UTF8String(Name)));
end;

procedure lua_register_global_string(L: Plua_State; Name: string; Value: string);
begin
  lua_pushstring(L, PUTF8Char(UTF8String(Value)));
  lua_setglobal(L, PUTF8Char(UTF8String(Name)));
end;

function LuaAlloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t): Pointer; cdecl;
begin
  //C realloc contract used by Lua:
  //  ptr=nil, nsize>0  -> allocate               (ReallocMem(nil, n) allocates)
  //  ptr<>nil, nsize>0 -> grow/shrink in place   (may move)
  //  nsize=0           -> free and return nil    (Lua never keeps a result on free)
  //Any failure signals Lua by returning nil.
  Result := nil;
  try
    if nsize = 0 then
    begin
      if ptr <> nil then
        FreeMem(ptr);
      Exit;
    end;
    Result := ptr;
    ReallocMem(Result, nsize);
  except
    Result := nil;
  end;
end;

type
  TLuaExtraSpace = class(TObject)
  public
    Status: TLuaStatus;
  end;
  PLuaExtraSpace = ^TLuaExtraSpace;

function LuaStatusPointer(L: Plua_State): TLuaExtraSpace; inline;
begin
  Result := TLuaExtraSpace(PPointer(lua_getextraspace(L))^);
end;

procedure HookCallback(L: Plua_State; ar: Plua_Debug); cdecl;
var
  AExtraSpace: TLuaExtraSpace;
begin
  AExtraSpace := LuaStatusPointer(L);
  if (AExtraSpace <> nil) and
     (TInterlocked.Add(Integer(AExtraSpace.Status), 0) = Ord(luaTerminated)) then
    luaL_error(L, PUTF8Char('Terminated by user!'));
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
//  EnumMethods; not yet
end;

{ TLua }

function TLua.GetStatus: TLuaStatus;
var
  AExtraSpace: TLuaExtraSpace;
begin
  if State = nil then
    Exit(luaNone);
  AExtraSpace := LuaStatusPointer(State);
  if AExtraSpace = nil then
    Exit(luaNone);
  Result := TLuaStatus(TInterlocked.Add(Integer(AExtraSpace.Status), 0));
end;

procedure TLua.SetStatus(AStatus: TLuaStatus);
var
  AExtraSpace: TLuaExtraSpace;
begin
  if State = nil then
    Exit;
  AExtraSpace := LuaStatusPointer(State);
  if AExtraSpace <> nil then
    TInterlocked.Exchange(Integer(AExtraSpace.Status), Ord(AStatus));
end;

procedure TLua.SetReady;
begin
  SetStatus(luaReady);
end;

procedure TLua.SetTerminated;
begin
  SetStatus(luaTerminated);
end;

procedure TLua.Init(SafeMode: Boolean; HookCount: Integer);
var
  AExtraSpace: TLuaExtraSpace;
begin
  //Both in-tree call sites keep the record in a zeroed class field, but a
  //record that already owns a Lua state must release it first: Self :=
  //Default(TLua) below would otherwise just drop the pointer (leaking the
  //state, its extraspace status and every allocate).
  if State <> nil then
    Close;
  Self := Default(TLua);
  AExtraSpace := TLuaExtraSpace.Create;
  AExtraSpace.Status := luaNone;
  State := lua_newstate(@LuaAlloc, nil, 0);
  if State = nil then
  begin
    AExtraSpace.Free;
    raise Exception.Create('Unable to create Lua state');
  end;
  PPointer(lua_getextraspace(State))^ := AExtraSpace;
  FVersion := lua_version(State);
  //All libraries
  luaL_openselectedlibs(State, -1, 0);
  //Remove direct file system access through io and os: openfile() is the only
  //way a script gets at a file, and it refuses anything outside the workspace
  lua_pushnil(State);
  if SafeMode then
  begin
    lua_setglobal(State, PUTF8Char('io'));
    lua_pushnil(State);
    lua_setglobal(State, PUTF8Char('os'));
    //Close the loaders that would walk around that guard: dofile and loadfile run
    //any Lua file on disk and require can pull in a native module. require itself
    //is put back by the script class with a loader that only reads inside the
    //workspace and the app folder; see TLuaScript.Require_func.
    lua_pushnil(State);
    lua_setglobal(State, PUTF8Char('dofile'));
    lua_pushnil(State);
    lua_setglobal(State, PUTF8Char('loadfile'));
    lua_pushnil(State);
    lua_setglobal(State, PUTF8Char('require'));
    //lua_pushnil(State);
    //lua_setglobal(State, PUTF8Char('package'));
    //lua_pushnil(State);
    //debug goes too, since it reaches the registry and the call stack.
    //lua_setglobal(State, PUTF8Char('debug'));
  end;
  SetReady;
  if HookCount > 0 then
    lua_sethook(State, @HookCallback, LUA_MASKCOUNT, HookCount);
end;

procedure TLua.Close;
var
  AExtraSpace: TLuaExtraSpace;
begin
  if State = nil then
    Exit;
  AExtraSpace := LuaStatusPointer(State);
  lua_close(State);
  State := nil;
  if AExtraSpace <> nil then
    AExtraSpace.Free;
end;

{ TLuaHelper }

function TLuaHelper.GetArgsCount: Integer;
begin
  Result := lua_gettop(@Self);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Method: TLuaMethod);
begin
  lua_register_global_method(@Self, Name, Method);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; LuaFunction: TLuaFunction);
begin
  lua_reg_global_function(@Self, PUTF8Char(UTF8String(Name)), LuaFunction);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Value: string);
begin
  lua_register_global_string(@Self, Name, Value);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Value: Integer);
begin
  lua_register_global_integer(@Self, Name, Value);
end;

procedure TLuaHelper.RegisterGlobal(const Name: string; Value: Double);
begin
  lua_register_global_number(@Self, Name, Value);
end;

procedure TLuaHelper.Register(const Name: string; Value: string);
begin
  lua_register_string(@Self, Name, Value);
end;

procedure TLuaHelper.Register(const Name: string; Value: Integer);
begin
  lua_register_integer(@Self, Name, Value);
end;

procedure TLuaHelper.Register(const Name: string; Value: Double);
begin
  lua_register_number(@Self, Name, Value);
end;

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

function lua_method_callback_index(L: Plua_State): integer; cdecl;
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

procedure TLuaHelper.Register(const Name: string; Method: TLuaMethod);
begin
  lua_pushlightuserdata(@Self, TMethod(method).Data);
  lua_pushlightuserdata(@Self, TMethod(method).Code);
  lua_pushvalue(@Self, -3);
  lua_pushcclosure(@Self, @lua_method_callback_index, 3);
  lua_setfield(@Self, -2, PUTF8Char(UTF8String(Name)));
end;

procedure TLuaHelper.RegisterMethod(const Name: string; Method: TLuaMethod);
begin
  lua_pushlightuserdata(@Self, TMethod(method).Data);
  lua_pushlightuserdata(@Self, TMethod(method).Code);
  lua_pushcclosure(@Self, @lua_method_callback, 2);
  lua_setfield(@Self, -2, PUTF8Char(UTF8String(Name)));
end;

procedure TLuaHelper.Register(const Table, Name: string; AObject: TObject; Method: TLuaMethod; AToMeta: Boolean);
begin
  lua_register_table_method(@Self, Table, Name, AObject, Method, AToMeta);
end;

procedure TLuaHelper.RegisterMeta(const Name: string; Method: TLuaMethod);
begin
  lua_push_method(@Self, Name, Method);
end;

function TLuaHelper.ToString(Index: Integer): string;
begin
  //lua_tostring returns the PChar straight from Lua, so it is already UTF-8
  Result := string(UTF8String(lua_tostring(@Self, Index)));
end;

function TLuaHelper.ToInteger(Index: Integer): lua_Integer;
begin
  Result := lua_tointeger(@Self, Index);
end;

function TLuaHelper.ToNumber(Index: Integer): Double;
begin
  Result := lua_tonumber(@Self, Index);
end;

function TLuaHelper.ToString(Index: Integer; const Name: string): string;
begin
  GetField(Index, Name);
  Result := PopString;
end;

function TLuaHelper.ToBoolean(Index: Integer): Boolean;
begin
  Result := lua_toboolean(@Self, Index);
end;

function TLuaHelper.ToInteger(Index: Integer; const Name: string): Integer;
begin
  GetField(Index, Name);
  Result := PopInteger;
end;

function TLuaHelper.IsInteger(Index: Integer): Boolean;
begin
  Result := lua_isinteger(@Self, Index);
end;

function TLuaHelper.IsNil(Index: Integer): Boolean;
begin
  Result := lua_isnil(@Self, Index);
end;

function TLuaHelper.IsNumber(Index: Integer): Boolean;
begin
  Result := lua_isnumber(@Self, Index);
end;

function TLuaHelper.IsString(Index: Integer): Boolean;
begin
  Result := lua_isstring(@Self, Index);
end;

function TLuaHelper.IsTable(Index: Integer): Boolean;
begin
  Result := lua_istable(@Self, Index);
end;

procedure TLuaHelper.PushBoolean(Value: Boolean);
begin
  lua_pushboolean(@Self, Value);
end;

procedure TLuaHelper.PushInteger(Value: lua_Integer);
begin
  lua_pushinteger(@Self, Value);
end;

procedure TLuaHelper.PushNumber(Value: Double);
begin
  lua_pushnumber(@Self, Value);
end;

procedure TLuaHelper.PushString(const Value: string);
begin
  lua_pushstring(@Self, PUTF8Char(UTF8String(Value)));
end;

procedure TLuaHelper.PushNil;
begin
  lua_pushnil(@Self);
end;

procedure TLuaHelper.PushValue(Index: Integer);
begin
  lua_pushvalue(@Self, Index);
end;

procedure TLuaHelper.Pop(Count: Integer);
begin
  lua_pop(@Self, Count);
end;

function TLuaHelper.PopInteger: Integer;
begin
  Result := ToInteger(-1);
  Pop(1);
end;

function TLuaHelper.PopString: string;
begin
  Result := ToString(-1);
  Pop(1);
end;

procedure TLuaHelper.NewTable;
begin
  lua_newtable(@Self);
end;

procedure TLuaHelper.GetField(Index: Integer; const Name: string);
begin
  lua_getfield(@Self, Index, PUTF8Char(UTF8String(Name)));
end;

procedure TLuaHelper.SetField(Index: Integer; const Name: string);
begin
  lua_setfield(@Self, Index, PUTF8Char(UTF8String(Name)));
end;

procedure TLuaHelper.SetMetaTable(Index: Integer);
begin
  lua_setmetatable(@Self, Index);
end;

procedure TLuaHelper.Remove(Index: Integer);
begin
  lua_remove(@Self, Index);
end;

function TLuaHelper.GetStack(Level: Integer; var AInfo: lua_Debug): Boolean;
begin
  Result := lua_getstack(@Self, Level, AInfo) > 0;
end;

function TLuaHelper.GetInfo(const What: string; var AInfo: lua_Debug): Boolean;
begin
  Result := lua_getinfo(@Self, PUTF8Char(UTF8String(What)), AInfo) > 0;
end;

procedure TLuaHelper.Register(const Name: string; LuaFunction: TLuaFunction);
begin
  lua_reg_function(@Self, PUTF8Char(UTF8String(Name)), LuaFunction);
end;

procedure TLuaHelper.Register(const Table: string; AObject: TLuaObject);
begin
  lua_register_table_index(@Self, Table, AObject); //Should be last one for window
end;

procedure TLuaHelper.RegisterTable(Table: string);
begin
  lua_register_table(@Self, Table);
end;

function TLuaHelper.LoadString(const Script: string): Integer;  // returns registry ref
begin
  if luaL_loadstring(@Self, PUTF8Char(UTF8String(Script))) <> LUA_OK then
  begin
    lua_pop(@Self, 1);
    Result := LUA_NOREF;
  end
  else
    Result := luaL_ref(@Self, LUA_REGISTRYINDEX);
end;

function TLuaHelper.Run(Ref: Integer; out Output: string): Boolean;
begin
  lua_rawgeti(@Self, LUA_REGISTRYINDEX, Ref);  // push compiled chunk
  Result := lua_pcall(@Self, 0, LUA_MULTRET, 0) = LUA_OK;
  if Result then
    Output := ''
  else
  begin
    if lua_isstring(@Self, -1) then
      //lua_tostring returns the PChar straight from Lua, so it is already UTF-8
      Output := string(UTF8String(lua_tostring(@Self, -1)))
    else
      Output := '(unknown error)';
    lua_pop(@Self, 1);
  end;
end;

function TLuaHelper.RunString(Script: string; out Output: string): Boolean;
var
  Ref: Integer;
begin
  Ref := LoadString(Script);
  if Ref <> LUA_NOREF then
    Result := Run(Ref, Output)
  else
    Result := False;
end;

function TLuaHelper.FunctionExists(const FunctionName: string): Boolean;
begin
  lua_getglobal(@Self, PUTF8Char(UTF8String(FunctionName)));
  Result := lua_isfunction(@Self, -1);
  lua_pop(@Self, 1);
end;

procedure TLuaHelper.BeginTable;
begin
  lua_newtable(@Self);
end;

procedure TLuaHelper.EndTableGlobal(Table: string; AObject: TLuaObject);
begin
  lua_setglobal(@Self, PUTF8Char(UTF8String(Table)));
  if AObject <> nil then
    Register(Table, AObject); //Should be last one
end;

end.

