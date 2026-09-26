unit TyroLua;
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

{$mode objfpc}{$H+}{$M+}
{$WARN 5024 off : Parameter "$1" not used}
{$define DEBUG_LUA}

interface

uses
  Classes, SysUtils,
  LuaClasses, LuaAPI, FPImage,
  RayLib, RayClasses, //remove it
  mnUtils,
  TyroScripts, TyroSounds, TyroClasses, Melodies, TyroSprites, TyroPhysics,
  TyroControls, TyroEngines, TyroInput,
  TyroRadio, TyroSpectrum;

type
  TLuaScript = class;

  { TTyroLuaObject }

  TTyroLuaObject = class abstract(TLuaObject)
  private
    FScript: TLuaScript;
  protected
    procedure Created; virtual;
  protected
  public
    constructor Create(AScript: TLuaScript); virtual;
    procedure Register; override;
    property Script: TLuaScript read FScript;
  end;

  { TLuaCanvas }

  TLuaCanvas = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Clear_func(L: Plua_State): integer; cdecl;
    function Text_func(L: Plua_State): integer; cdecl;
    function Circle_func(L: Plua_State): integer; cdecl;
    function Rectangle_func(L: Plua_State): integer; cdecl;
    function Line_func(L: Plua_State): integer; cdecl;
    function Point_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
  end;

  { TLuaShader }

  TLuaShader = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Load_func(L: PLua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
  end;

  { TLuaConsole }

  TLuaConsole = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Print_func(L: Plua_State): integer; cdecl;
    function PrintLn_func(L: Plua_State): integer; cdecl;
    function PrintOut_func(L: Plua_State): integer; cdecl;
    function PrintLnOut_func(L: Plua_State): integer; cdecl;
    function Show_func(L: Plua_State): integer; cdecl;
    function Read_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
  end;

  { TLuaOutput }

  TLuaOutput = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    function Clear_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
  end;

  { TLuaWindow }

  TLuaWindow = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    constructor Create(AScript: TLuaScript); override;
  published
    function Window_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaFont }

  TLuaFont = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Load_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
  end;

  { TLuaColors }

  TLuaColors = class(TTyroLuaObject)
  protected
  type
    TLuaColor = record
      Name: string;
      Color: TColor;
    end;

  var
    Colors: array of TLuaColor;
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;

    procedure Created; override;
  public
    procedure AddColor(AName: string; AColor: TColor);
  end;

  { TLuaMusic }

  TLuaMusic = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Beep_func(L: Plua_State): integer; cdecl;
    function Sound_func(L: Plua_State): integer; cdecl;
    function Play_func(L: Plua_State): integer; cdecl;
    function MML_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
  end;

  { TLuaRadio }

  TLuaRadio = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Play_func(L: Plua_State): integer; cdecl;
    function Pause_func(L: Plua_State): integer; cdecl;
    function Resume_func(L: Plua_State): integer; cdecl;
    function Stop_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaSpectrum }

  TLuaSpectrum = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
  end;

  { TRadioAction }

  TRadioAction = (raPlay, raPause, raResume, raStop);

  { Routes radio.play/pause/resume/stop into the main window thread (the same
    one that drives RayUpdates.Update and the raylib audio device). }
  TRadioPlayObject = class(TQueueObject)
  private
    FAction: TRadioAction;
    FURL: string;
  public
    constructor Create(AAction: TRadioAction); overload;
    constructor Create(AAction: TRadioAction; const AURL: string); overload;
    procedure DoExecute; override;
  end;

  { TShowSpectrumObject }

  { Shows the spectrum analyzer panel on the main window thread (the panel is
    part of the control tree painted by the main drawing cycle). }
  TShowSpectrumObject = class(TQueueObject)
  private
    FX, FY, FW, FH: Integer;
  public
    constructor Create(AX, AY, AW, AH: Integer);
    procedure DoExecute; override;
  end;

  { THideSpectrumObject }

  THideSpectrumObject = class(TQueueObject)
  public
    procedure DoExecute; override;
  end;

  { TLuaSprite }

  TLuaSprite = class(TTyroLuaObject)
  private
  protected
    function Getter(L: Plua_State): integer; override;
    function Setter(L: Plua_State): integer; override;
  public
    function RegisterSprite(AHandle: Integer): Integer;
    function Load_func(L: Plua_State): integer; cdecl;
    function LoadScript_func(L: Plua_State): integer; cdecl;
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    function Move_func(L: Plua_State): integer; cdecl;
    function Width_func(L: Plua_State): integer; cdecl;
    function Height_func(L: Plua_State): integer; cdecl;
    function Play_func(L: Plua_State): integer; cdecl;
    function Stop_func(L: Plua_State): integer; cdecl;
    function FrameCount_func(L: Plua_State): integer; cdecl;
  end;

  TLuaSprites = class(TTyroLuaObject)
  protected
  public
    //sprites
    function New_func(L: Plua_State): integer; cdecl;
    function Find_func(L: Plua_State): integer; cdecl;
    function Call_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaControls }

  { Generic control creation and inspection. controls.new('button', ...)
    creates a control by class name ('button', 'panel', 'label', 'checkbox',
    'edit', 'spectrum'); the returned handle addresses a control owned by the
    main window. The same object backs both the 'controls' table and the
    legacy 'buttons' alias table, so old scripts keep working. }
  TLuaControls = class(TTyroLuaObject)
  private
    FItems: TList; //of TTyroControl (owned by the main window, not by us)
    function GetControl(AHandle: Integer): TTyroControl;
    function FindByName(const AName: string): Integer;
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function New_func(L: Plua_State): integer; cdecl;
    function Caption_func(L: Plua_State): integer; cdecl;
    function Text_func(L: Plua_State): integer; cdecl;
    function Checked_func(L: Plua_State): integer; cdecl;
    function Position_func(L: Plua_State): integer; cdecl;
    function Move_func(L: Plua_State): integer; cdecl;
    function Width_func(L: Plua_State): integer; cdecl;
    function Height_func(L: Plua_State): integer; cdecl;
    function Visible_func(L: Plua_State): integer; cdecl;
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    function Hover_func(L: Plua_State): integer; cdecl;
    function Down_func(L: Plua_State): integer; cdecl;
    function Clicked_func(L: Plua_State): integer; cdecl;
    function Focused_func(L: Plua_State): integer; cdecl;
    function Focus_func(L: Plua_State): integer; cdecl;
    function Border_func(L: Plua_State): integer; cdecl;
    function BackColor_func(L: Plua_State): integer; cdecl;
    function Name_func(L: Plua_State): integer; cdecl;
    function Align_func(L: Plua_State): integer; cdecl;
    function Parent_func(L: Plua_State): integer; cdecl;
    function Items_func(L: Plua_State): integer; cdecl;
    function Item_func(L: Plua_State): integer; cdecl;
    function AddItem_func(L: Plua_State): integer; cdecl;
    function Clear_func(L: Plua_State): integer; cdecl;
    function ViewCount_func(L: Plua_State): integer; cdecl;
    function ItemIndex_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
    destructor Destroy; override;
  end;

{ TLuaSpriteScript }

  // One Lua state per sprite (a TLua recording). Routed through handler
  // globals that the main thread polls: on_update(), on_draw(), on_collide(other, state).
  // When attached, the file is compiled and Run()ed once, so its top-level code
  // runs immediately; the handlers themselves are plain functions we look up each tick.
  TLuaSpriteScript = class(TInterfacedObject, ISpriteScript)
  private
    FScript: TLuaScript;
    FHandle: Integer;
    FLua: TLua;
    FFileName: string;
    FSprite: TLuaSprite; // proxy object whose getter/setter back the self/other tables
    procedure DoError(S: string; const AHandler: string);
    procedure BuildSelf; // registers the self global table (properties read/write main sprite)
    procedure BuildGlobals; // global helpers shared from the main script state (time, rand, println)
    procedure BuildColors; // registers the colors global table
    procedure BuildDraw; // registers the draw global table
    procedure CallHandler(const AName: string; AArgs: Integer); // pcall a global handler (args already on stack), ignore if not a function
  public
    constructor Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
    destructor Destroy; override;
    function Load: Boolean; // load file and run top-level code once
    procedure Update; // dispatch on_update()
    procedure Draw;   // dispatch on_draw()
    procedure OnCollide(AOtherHandle: Integer; const AState: string); // dispatch on_collide(other, state)
    // draw.* helpers for on_draw; safe only while a frame is active (main thread)
    function Circle_func(L: Plua_State): integer; cdecl;
    function Rectangle_func(L: Plua_State): integer; cdecl;
    function Line_func(L: Plua_State): integer; cdecl;
    function Text_func(L: Plua_State): integer; cdecl;
    property FileName: string read FFileName;
  end;

  { Loads a per-sprite script on the main thread (engine canvas queue), then
    attaches the ISpriteScript to the sprite. }
  TLoadSpriteScriptObject = class(TQueueObject)
  private
    FScript: TLuaScript;
    FHandle: Integer;
    FFileName: string;
  public
    constructor Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
    procedure DoExecute; override;
  end;

{ TLuaCollision }

  TLuaCollision = class(TTyroLuaObject)
  private
    procedure FireEvent(L: Plua_State; AHandle, AOtherHandle: Integer; const AState: string);
  protected
    function Getter(L: Plua_State): integer; override;
    function Setter(L: Plua_State): integer; override;
  public
    // collision.pump() drains the queue and fires sprite.onCollide(other, state)
    function Pump_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaScript }

  TLuaScript = class(TTyroScript)
  private
  protected
    Lua: TLua;

    Canvas: TLuaCanvas;
    Console: TLuaConsole;
    Window: TLuaWindow;
    Colors: TLuaColors;
    Font: TLuaFont;
    Music: TLuaMusic;
    Radio: TLuaRadio;
    Sprite: TLuaSprite;
    Sprites: TLuaSprites;
    Controls: TLuaControls;
    Output: TLuaOutput;
    Collision: TLuaCollision;
    Shader: TLuaShader;
    SpectrumLua: TLuaSpectrum;
    procedure DoError(S: string);
    procedure Run; override;
    procedure Stop; override;
    function RunLine(const ALine: string; out AOutput: string): Boolean; override;
  protected

    //input & timing
    function IsKeyPressed_func(L: Plua_State): integer; cdecl;
    function IsKeyDown_func(L: Plua_State): integer; cdecl;
    function MouseX_func(L: Plua_State): integer; cdecl;
    function MouseY_func(L: Plua_State): integer; cdecl;
    function IsMouseButtonPressed_func(L: Plua_State): integer; cdecl;
    function FrameTime_func(L: Plua_State): integer; cdecl;
    function TotalTime_func(L: Plua_State): integer; cdecl;
    function RandomValue_func(L: Plua_State): integer; cdecl;
    function Screenshot_func(L: Plua_State): integer; cdecl;

   public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddQueueObject(AQueueObject: TQueueObject); override;
    //Global environment hooks: unresolved globals resolve to sprites/controls by name
    function __global_getter(L: Plua_State): integer; cdecl;
    function __global_setter(L: Plua_State): integer; cdecl;
  end;

const
  // Integer key base in the Lua registry for "sprite handle -> sprite table"
  cSpriteRegistryBase = $00700000; //HUH

implementation

//global functions
function sleep_func(L: Plua_State): integer; cdecl;
var
  n: int64;
begin
  n := round(L.Params[1].AsInteger);
  sleep(n);
  Result := 0;
end;

function log_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.Count;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.Params[i].AsString;
    if IsConsole then
      WriteLn(L.Params[i].AsString);
  end;
  //mirror the log line to the Output control too
  if (Main <> nil) and (Main.Output <> nil) then
    Main.Output.Writeln(s);
  Result := 0;
end;

// Callback trampoline for the global-environment hooks: binds a TLuaScript
// method to a Lua C-closure (mirrors the internal one in LuaClasses).
function global_meta_callback(L: Plua_State): integer; cdecl;
var
  Method: TMethod;
begin
  Method.Data := lua_topointer(L, lua_upvalueindex(1));
  Method.Code := lua_topointer(L, lua_upvalueindex(2));
  if Method.Data = nil then
    raise Exception.Create('Lua: cannot execute global hook!');
  Result := TLuaMethod(Method)(L);
end;

// Global environment __index: called when a global name is not stored raw in
// the globals table. Raw globals win; then the name is resolved against the
// sprite list (by name) and the control list (buttons by caption); the final
// fallback is an empty table. lua_rawget/rawset keep this recursion-free.
function TLuaScript.__global_getter(L: Plua_State): integer; cdecl;
var
  aName: string;
  AHandle: integer;
begin
  Result := 1;
  //1) Existing globals win; raw read never re-enters __index
  L.PushValue(2);
  lua_rawget(L, 1);
  if not lua_isnil(L, -1) then
    Exit;
  L.Pop(1); //[globals, key]

  //2) Only string names can refer to a sprite/control
  if lua_type(L, 2) <> LUA_TSTRING then
  begin
    L.PushNil;
    Exit;
  end;

  aName := L.ToString(2);

  //3) 'cycle': the drawing-cycle gate. Reading it blocks the script thread
  //until the next raylib frame (EndDrawing) completes, so "while cycle do"
  //runs the loop body at most once per drawn frame instead of queuing many
  //drawing commands into the same cycle. Never cached: every read waits again.
  if aName = 'cycle' then
  begin
    if (Thread <> nil) and not Main.WaitToNextFrame(Self) then
      L.PushBoolean(False) //script was stopped while waiting -> end the loop
    else
      L.PushBoolean(True);
    Exit;
  end;

  //4) Sprites: reuse/create the sprite proxy bound to the found handle
  AHandle := Main.Sprites.FindByName(aName);
  if AHandle > cSpriteInvalid then
  begin
    lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle); //[globals, key, sprite?]
    if not lua_istable(L, -1) then
    begin
      L.Pop(1);
      Sprite.RegisterSprite(AHandle); //[globals, key, sprite]
    end;
    //cache it raw into the globals table and return it
    L.PushValue(-1); //[globals, key, sprite, sprite]
    L.PushValue(2); //[globals, key, sprite, sprite, key]
    lua_rawset(L, 1); //globals[key] = sprite -> [globals, key, sprite]
    L.Remove(1); //[key, sprite]
    L.Remove(1); //[sprite]
    Exit;
  end;

  //5) Controls: named controls (Name field set at creation) resolve by name
  AHandle := Controls.FindByName(aName);
  if AHandle > 0 then
  begin
    L.PushInteger(AHandle);
    Exit;
  end;

  //6) Preserve normal Lua semantics for an unresolved global.
  L.PushNil;
end;

// Global environment __newindex: assign the value raw into the globals table.
// lua_rawset never re-enters __newindex (no recursion).
function TLuaScript.__global_setter(L: Plua_State): integer; cdecl;
begin
  lua_rawset(L, 1);
  Result := 0;
end;

{ TLuaConsole }

function TLuaConsole.Setter(L: PLua_State): integer;
var
  i: integer;
  field: string;
  color: string;
begin
  Result := 0;
  field := L.ToString(2);
  if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'height' then
      Main.Console.Height := i
    else if field = 'width' then
      Main.Console.Width := i
    else if field = 'margin' then
      Main.Console.Margin := i
  end
  else if L.IsString(-1) then
  begin
    if field = 'align' then
    begin
      //* TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
      if L.ToString(-1) = 'none' then
        Main.Console.Align := TAlign(0)
      else if L.ToString(-1) = 'left' then
        Main.Console.Align := TAlign(1)
      else if L.ToString(-1) = 'top' then
        Main.Console.Align := TAlign(2)
      else if L.ToString(-1) = 'right' then
        Main.Console.Align := TAlign(3)
      else if L.ToString(-1) = 'bottom' then
        Main.Console.Align := TAlign(4)
      else if L.ToString(-1) = 'client' then
        Main.Console.Align := TAlign(5);
    end;
  end;
end;

function TLuaConsole.Getter(L: PLua_State): integer;
var
  i: integer;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  case field of
    'active':
    begin
      L.PushBoolean(Main.Console.Visible);
      Result := 1;
    end;
    'align':
    begin
      //* TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
      i := Ord(Main.Console.Align);
      case i of
        0: L.PushString('none');
        1: L.PushString('left');
        2: L.PushString('top');
        3: L.PushString('right');
        4: L.PushString('bottom');
        5: L.PushString('client');
        else
          L.PushString('none');
      end;
      Result := 1;
    end;
    'height':
    begin
      L.PushInteger(Main.Console.Height);
      Result := 1;
    end;
    'width':
    begin
      L.PushInteger(Main.Console.Width);
      Result := 1;
    end;
    'margin':
    begin
      L.PushInteger(Main.Console.Margin);
      Result := 1;
    end;
  end;
end;

constructor TLuaConsole.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
end;

{ TLuaWindow }

function TLuaWindow.Setter(L: PLua_State): integer;
var
  i: integer;
  field: string;
  color: string;
begin
  Result := 0;
  field := L.ToString(2);
  if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'margin' then
      Main.Margin := i
    else if field = 'backcolor' then
      Main.BackColor := IntToColor(i);
  end
  else if L.IsString(-1) then
  begin
    color := L.ToString(-1);
    if field = 'backcolor' then
      Main.BackColor := StrToColor(color);
  end;
end;

function TLuaWindow.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  case field of
    'margin':
    begin
      L.PushInteger(Main.Margin);
      Result := 1;
    end;
    'backcolor':
    begin
      L.PushInteger(ColorToInt(Main.BackColor));
      Result := 1;
    end;
    'width':
    begin
      L.PushInteger(Main.Width);
      Result := 1;
    end;
    'height':
    begin
      L.PushInteger(Main.Height);
      Result := 1;
    end;
  end;
end;

constructor TLuaWindow.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
end;

{ TLuaFont }

function TLuaFont.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaFont.Getter(L: PLua_State): integer;
begin
  Result := 0;
end;

constructor TLuaFont.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
end;

{ TLuaMusic }

function TLuaMusic.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaMusic.Getter(L: PLua_State): integer;
begin
  Result := 0;
end;

constructor TLuaMusic.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
end;

{ TTyroLuaObject }

procedure TTyroLuaObject.Created;
begin
end;

constructor TTyroLuaObject.Create(AScript: TLuaScript);
begin
  inherited Create;
  FScript := AScript;
  Created;
end;

procedure TTyroLuaObject.Register;
begin
end;

{ TLuaColors }

function TLuaColors.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaColors.Getter(L: PLua_State): integer;
var
  c: integer;
  index: integer;
  field: string;
begin
  Result := 0;
  if L.IsNumber(2) then
  begin
    index := round(L.ToInteger(2));
    if index < Length(Colors) then
    begin
      c := ColorToInt(Colors[index].Color);
      L.PushInteger(c);
      Result := 1;
    end;
  end
  else
  begin
    field := L.ToString(2);
    case field of
      'count':
      begin
        L.PushInteger(Length(Colors));
        Result := 1;
      end;
    end;
  end;
end;

procedure TLuaColors.AddColor(AName: string; AColor: TColor);
var
  aItem: TLuaColor;
begin
  aItem.Name := aName;
  aItem.Color := AColor;
  SetLength(Colors, Length(Colors) + 1);
  Colors[Length(Colors) - 1] := aItem;
end;

procedure TLuaColors.Created;
begin
  AddColor('white', clWhite);
  AddColor('silver', clLightgray);
  AddColor('gray', clGray);
  AddColor('grey', clGray);
  AddColor('lightgray', clLightgray);
  AddColor('darkgray', clDarkGray);
  AddColor('black', clBlack);
  AddColor('red', clRed);
  AddColor('maroon', clMaroon);
  AddColor('yellow', clYellow);
  AddColor('gold', clGold);
  AddColor('orange', clOrange);
  AddColor('pink', clPink);
  AddColor('olive', clDarkgreen);
  AddColor('lime', clLime);
  AddColor('green', clGreen);
  AddColor('darkgreen', clDarkgreen);
  AddColor('aqua', clSkyBlue);
  AddColor('skyblue', clSkyBlue);
  AddColor('teal', clBrown);
  AddColor('blue', clBlue);
  AddColor('darkblue', clDarkblue);
  AddColor('navy', clViolet);
  AddColor('purple', clPurple);
  AddColor('violet', clViolet);
  AddColor('darkpurple', clDarkpurple);
  AddColor('fuchsia', clMagenta);
  AddColor('magenta', clMagenta);
  AddColor('brown', clBrown);
  AddColor('darkbrown', clDarkbrown);
  AddColor('beige', clBeige);
  AddColor('raywhite', clRayWhite);
end;

{ TLuaCanvas }

function TLuaCanvas.Setter(L: PLua_State): integer;
var
  i: integer;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if L.IsInteger(-1) then
    case field of
      'color':
      begin
        i := L.ToInteger(-1);
        FScript.AddQueueObject(TDrawSetColorObject.Create(Main.Canvas, IntToColor(i)));
        Result := 1;
      end;
      'alpha':
      begin
        i := L.ToInteger(-1);
        FScript.AddQueueObject(TDrawSetAlphaObject.Create(Main.Canvas, i));
        Result := 1;
      end;
      'backcolor':
      begin
        i := L.ToInteger(-1);
        //Main.Canvas.BackgroundColor := RayColorOf(IntToColor(i));//thread unsafe
        Result := 1;
      end;
    end;
end;

function TLuaCanvas.Getter(L: PLua_State): integer;
var
  i: integer;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  case field of
    'color':
    begin
      i := ColorToInt(Main.Canvas.PenColor);
      L.PushInteger(i);
      Result := 1;
    end;
    'backcolor':
    begin
      i := ColorToInt(Main.Canvas.BackColor);
      L.PushInteger(i);
      Result := 1;
    end;
    'width':
    begin
      L.PushInteger(Main.Canvas.Width);
      Result := 1;
    end;
    'height':
    begin
      L.PushInteger(Main.Canvas.Height);
      Result := 1;
    end;
  end;
end;

constructor TLuaCanvas.Create(AScript: TLuaScript);
begin
  inherited;
end;

{ TLuaShader }

constructor TLuaShader.Create(AScript: TLuaScript);
begin
  inherited;
end;

function TLuaShader.Setter(L: PLua_State): integer;
var
  field: string;
  area: TRectangle;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'effect' then
  begin
    if L.IsString(-1) then
      FScript.AddQueueObject(TSetEffectObject.Create(Main.Board, L.ToString(-1)));
  end
  else if field = 'value' then
  begin
    if L.IsNumber(-1) then
      FScript.AddQueueObject(TSetEffectValueObject.Create(Main.Board, L.ToNumber(-1)));
  end
  else if field = 'area' then
  begin
    if lua_istable(L, -1) then
    begin
      area := Default(TRectangle);
      lua_rawgeti(L, -1, 1);
      if L.IsNumber(-1) then
        area.x := L.ToNumber(-1);
      lua_pop(L, 1);
      lua_rawgeti(L, -1, 2);
      if L.IsNumber(-1) then
        area.y := L.ToNumber(-1);
      lua_pop(L, 1);
      lua_rawgeti(L, -1, 3);
      if L.IsNumber(-1) then
        area.width := L.ToNumber(-1);
      lua_pop(L, 1);
      lua_rawgeti(L, -1, 4);
      if L.IsNumber(-1) then
        area.height := L.ToNumber(-1);
      lua_pop(L, 1);
      FScript.AddQueueObject(TSetEffectAreaObject.Create(Main.Board, area));
    end;
  end;
end;

function TLuaShader.Getter(L: PLua_State): integer;
var
  field: string;
  area: TRectangle;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'effect' then
  begin
    if Main.Board <> nil then
      L.PushString(Main.Board.GetEffectName)
    else
      L.PushString('none');
    Result := 1;
  end
  else if field = 'value' then
  begin
    if Main.Board <> nil then
      L.PushNumber(Main.Board.GetEffectValue)
    else
      L.PushNumber(1.0);
    Result := 1;
  end
  else if field = 'area' then
  begin
    if Main.Board <> nil then
      area := Main.Board.GetEffectArea
    else
    begin
      area := Default(TRectangle);
      area.width := Main.Canvas.Width;
      area.height := Main.Canvas.Height;
    end;
    lua_createtable(L, 4, 0);
    lua_pushnumber(L, area.x);
    lua_rawseti(L, -2, 1);
    lua_pushnumber(L, area.y);
    lua_rawseti(L, -2, 2);
    lua_pushnumber(L, area.width);
    lua_rawseti(L, -2, 3);
    lua_pushnumber(L, area.height);
    lua_rawseti(L, -2, 4);
    Result := 1;
  end;
end;

function TLuaShader.Load_func(L: PLua_State): integer; cdecl;
begin
  //Load a custom fragment shader from a file
  if L.IsString(1) then
    FScript.AddQueueObject(TLoadShaderObject.Create(Main.Board, L.ToString(1)));
  Result := 0;
end;

constructor TLuaScript.Create;
var
  i: integer;
begin
  inherited;
  Lua.Init;
  Lua.State.RegisterGlobal('version', TyroVersion);
  Lua.State.RegisterGlobal('log', @log_func);
  Lua.State.RegisterGlobal('sleep', @sleep_func);

  Canvas := TLuaCanvas.Create(Self);
  Window := TLuaWindow.Create(Self);
  Console := TLuaConsole.Create(Self);
  Colors := TLuaColors.Create(Self);
  Font := TLuaFont.Create(Self);
  Music := TLuaMusic.Create(Self);
  Radio := TLuaRadio.Create(Self);
  SpectrumLua := TLuaSpectrum.Create(Self);
  Sprite := TLuaSprite.Create(Self);
  Sprites := TLuaSprites.Create(Self);
  Controls := TLuaControls.Create(Self);
  Output := TLuaOutput.Create(Self);
  Collision := TLuaCollision.Create(Self);
  Shader := TLuaShader.Create(Self);

  //window
  Lua.State.Register('window', 'show', Window, @Window.Window_func);
  Lua.State.Register('window', Window); //Should be last one for window

  //global functions
  Lua.State.RegisterGlobal('print', @Console.PrintOut_func);
  Lua.State.RegisterGlobal('println', @Console.PrintLnOut_func);

  //console
  Lua.State.Register('console', 'print', Console, @Console.Print_func);
  Lua.State.Register('console', 'println', Console, @Console.PrintLn_func);
  Lua.State.Register('console', 'show', Console, @Console.Show_func);
  Lua.State.Register('console', 'read', Console, @Console.Read_func);
  Lua.State.Register('console', Console); //Should be last one

  //canvas
  Lua.State.Register('canvas', 'clear', Canvas, @Canvas.Clear_func);
  Lua.State.Register('canvas', 'text', Canvas, @Canvas.Text_func);
  Lua.State.Register('canvas', 'circle', Canvas, @Canvas.Circle_func);
  Lua.State.Register('canvas', 'rectangle', Canvas, @Canvas.Rectangle_func);
  Lua.State.Register('canvas', 'line', Canvas, @Canvas.Line_func);
  Lua.State.Register('canvas', 'point', Canvas, @Canvas.Point_func);
  Lua.State.Register('canvas', Canvas); //Should be last one

  //shader (property-style: shader.effect, shader.value, shader.area)
  Lua.State.Register('shader', 'load', Shader, @Shader.Load_func);
  Lua.State.Register('shader', Shader); //Should be last one

  //font
  Lua.State.Register('font', 'load', Font, @Font.Load_func);
  Lua.State.Register('font', Font); //Should be last one

  //music
  Lua.State.Register('music', 'beep', Music, @Music.Beep_func);
  Lua.State.Register('music', 'sound', Music, @Music.Sound_func);
  Lua.State.Register('music', 'play', Music, @Music.Play_func);
  Lua.State.Register('music', 'mml', Music, @Music.MML_func);

  //radio (radio.play(url), radio.pause(), radio.resume(), radio.stop()
  // + getters: radio.title, radio.station, radio.state, radio.playing ...)
  Lua.State.Register('radio', 'play', Radio, @Radio.Play_func);
  Lua.State.Register('radio', 'pause', Radio, @Radio.Pause_func);
  Lua.State.Register('radio', 'resume', Radio, @Radio.Resume_func);
  Lua.State.Register('radio', 'stop', Radio, @Radio.Stop_func);
  Lua.State.Register('radio', Radio); //Should be last one

  //spectrum (spectrum.show(x, y, w, h), spectrum.hide()
  // + getters/setters: spectrum.bars, spectrum.active, spectrum.visible)
  Lua.State.Register('spectrum', 'show', SpectrumLua, @SpectrumLua.Show_func);
  Lua.State.Register('spectrum', 'hide', SpectrumLua, @SpectrumLua.Hide_func);
  Lua.State.Register('spectrum', SpectrumLua); //Should be last one

  //input & timing (global functions)
  Lua.State.RegisterGlobal('iskeypressed', @IsKeyPressed_func);
  Lua.State.RegisterGlobal('iskeydown', @IsKeyDown_func);
  Lua.State.RegisterGlobal('mousex', @MouseX_func);
  Lua.State.RegisterGlobal('mousey', @MouseY_func);
  Lua.State.RegisterGlobal('ismousepressed', @IsMouseButtonPressed_func);
  Lua.State.RegisterGlobal('frametime', @FrameTime_func);
  Lua.State.RegisterGlobal('time', @TotalTime_func);
  Lua.State.RegisterGlobal('rand', @RandomValue_func);
  Lua.State.RegisterGlobal('screenshot', @Screenshot_func);

  // Sprite system: Sprites.new creates a sprite, Sprites("name") finds by name
  Lua.State.RegisterTable('Sprites');
  Lua.State.Register('Sprites', 'new', Self, @Sprites.New_func);
  Lua.State.Register('Sprites', 'find', Self, @Sprites.Find_func);
  // Set __call in the Sprites metatable so Sprites("name") works; Lua passes the table as arg 1
  Lua.State.Register('Sprites', '__call', Self, @Sprites.Call_func, True);

  //controls (generic control table; 'buttons' is a legacy alias to the same
  //object so old scripts keep working)
  Lua.State.RegisterTable('controls');
  Lua.State.Register('controls', 'new', Controls, @Controls.New_func);
  Lua.State.Register('controls', 'caption', Controls, @Controls.Caption_func);
  Lua.State.Register('controls', 'text', Controls, @Controls.Text_func);
  Lua.State.Register('controls', 'checked', Controls, @Controls.Checked_func);
  Lua.State.Register('controls', 'position', Controls, @Controls.Position_func);
  Lua.State.Register('controls', 'move', Controls, @Controls.Move_func);
  Lua.State.Register('controls', 'width', Controls, @Controls.Width_func);
  Lua.State.Register('controls', 'height', Controls, @Controls.Height_func);
  Lua.State.Register('controls', 'visible', Controls, @Controls.Visible_func);
  Lua.State.Register('controls', 'show', Controls, @Controls.Show_func);
  Lua.State.Register('controls', 'hide', Controls, @Controls.Hide_func);
  Lua.State.Register('controls', 'hover', Controls, @Controls.Hover_func);
  Lua.State.Register('controls', 'down', Controls, @Controls.Down_func);
  Lua.State.Register('controls', 'clicked', Controls, @Controls.Clicked_func);
  Lua.State.Register('controls', 'focused', Controls, @Controls.Focused_func);
  Lua.State.Register('controls', 'focus', Controls, @Controls.Focus_func);
  Lua.State.Register('controls', 'border', Controls, @Controls.Border_func);
  Lua.State.Register('controls', 'backcolor', Controls, @Controls.BackColor_func);
  Lua.State.Register('controls', 'name', Controls, @Controls.Name_func);
  Lua.State.Register('controls', 'align', Controls, @Controls.Align_func);
  Lua.State.Register('controls', 'parent', Controls, @Controls.Parent_func);
  Lua.State.Register('controls', 'items', Controls, @Controls.Items_func);
  Lua.State.Register('controls', 'item', Controls, @Controls.Item_func);
  Lua.State.Register('controls', 'additem', Controls, @Controls.AddItem_func);
  Lua.State.Register('controls', 'clear', Controls, @Controls.Clear_func);
  Lua.State.Register('controls', 'viewcount', Controls, @Controls.ViewCount_func);
  Lua.State.Register('controls', 'itemindex', Controls, @Controls.ItemIndex_func);
  Lua.State.Register('controls', Controls); //should be last one
  Lua.State.RegisterTable('buttons');
  Lua.State.Register('buttons', 'new', Controls, @Controls.New_func);
  Lua.State.Register('buttons', 'caption', Controls, @Controls.Caption_func);
  Lua.State.Register('buttons', 'border', Controls, @Controls.Border_func);
  Lua.State.Register('buttons', 'hover', Controls, @Controls.Hover_func);
  Lua.State.Register('buttons', 'down', Controls, @Controls.Down_func);
  Lua.State.Register('buttons', 'clicked', Controls, @Controls.Clicked_func);
  Lua.State.Register('buttons', Controls); //should be last one

  //output (catches print/println/log)
  Lua.State.RegisterTable('output');
  Lua.State.Register('output', 'show', Output, @Output.Show_func);
  Lua.State.Register('output', 'hide', Output, @Output.Hide_func);
  Lua.State.Register('output', 'clear', Output, @Output.Clear_func);
  Lua.State.Register('output', Output); //should be last one

  // Collision system: collision.pump() drains events and fires sprite.onCollide(other, state)
  Lua.State.Register('collision', 'pump', Collision, @Collision.Pump_func);
  Lua.State.Register('collision', Collision); // should be last — wires getter/setter metamethods

  Lua.State.BeginTable;
  for i := 0 to Length(Colors.Colors) - 1 do
    Lua.State.Register(Colors.Colors[i].Name, ColorToInt(Colors.Colors[i].Color));
  Lua.State.EndTable('colors', Colors);

  //Attach a metatable to the global environment so an unresolved global name
  //resolves to a sprite or control by name (e.g. richard.move(...) when
  //"richard" is a sprite). The globals table is fetched via LUA_RIDX_GLOBALS
  //for Lua 5.5 compatibility; __index/__newindex use raw access (no recursion).
  lua_rawgeti(Lua.State, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS); //[globals]
  Lua.State.NewTable; //[globals, meta]
  lua_pushlightuserdata(Lua.State, TMethod(@__global_getter).Data);
  lua_pushlightuserdata(Lua.State, TMethod(@__global_getter).Code);
  lua_pushcclosure(Lua.State, @global_meta_callback, 2); //[globals, meta, getter]
  lua_setfield(Lua.State, -2, '__index'); //[globals, meta]
  lua_pushlightuserdata(Lua.State, TMethod(@__global_setter).Data);
  lua_pushlightuserdata(Lua.State, TMethod(@__global_setter).Code);
  lua_pushcclosure(Lua.State, @global_meta_callback, 2); //[globals, meta, setter]
  lua_setfield(Lua.State, -2, '__newindex'); //[globals, meta]
  lua_setmetatable(Lua.State, -2); //globals.metatable = meta -> [globals]
  lua_pop(Lua.State, 1); //[]
end;

destructor TLuaScript.Destroy;
begin
  // Lua holds light-userdata/method pointers to these facade objects, so close
  // the state before releasing them.
  Lua.Close;
  FreeAndNil(Shader);
  FreeAndNil(Collision);
  FreeAndNil(Output);
  FreeAndNil(Controls);
  FreeAndNil(Sprites);
  FreeAndNil(Sprite);
  FreeAndNil(SpectrumLua);
  FreeAndNil(Radio);
  FreeAndNil(Music);
  FreeAndNil(Font);
  FreeAndNil(Colors);
  FreeAndNil(Console);
  FreeAndNil(Window);
  FreeAndNil(Canvas);
  inherited;
end;

procedure TLuaScript.DoError(S: string);
begin
  if IsConsole then
    WriteLn(S);
end;

procedure TLuaScript.Run;
var
  Msg: string;
begin
  // A stopped script object may be run again from the interactive console.
  Lua.SetReady;
  //Publish the failure state for the CLI --exit lifecycle: the worker thread
  //reads this after the run and the main loop surfaces it as the exit code.
  FLastError := '';
  //WriteLn('Run Script');
  //Sleep(1000);
  if not Lua.State.RunString(ScriptText.Text, Msg) then
  begin
    FLastError := Msg;
    DoError(Msg);
  end;
end;

procedure TLuaScript.Stop;
begin
  //abort the running Lua bytecode: HookCount calls luaL_error when
  //this state's status is terminated, and RunString returns on the next hook tick
  Lua.SetTerminated;
  inherited;
end;

// Runs a single console line on the persistent Lua state (same globals as the
// main script), so assignments like "x = 5" stay alive for later lines. Any
// return values are echoed to the terminal (REPL style); syntax/runtime errors
// are reported through AOutput.
function TLuaScript.RunLine(const ALine: string; out AOutput: string): Boolean;
var
  n: Integer;
  p: PUTF8Char;
  S, Msg, AChunk: string;

  //pcall the chunk already loaded on the stack and echo any return values
  //(REPL style); runtime errors are reported through AOutput
  procedure RunChunk;
  var
    i: Integer;
  begin
    if lua_pcall(Lua.State, 0, LUA_MULTRET, 0) = LUA_OK then
    begin
      Result := True;
      n := lua_gettop(Lua.State);
      S := '';
      for i := 1 to n do
      begin
        if i > 1 then
          S := S + #9;
        //luaL_tolstring pushes a string representation of the value at index i
        //onto the top of the stack; pop that copy and concatenate it. The
        //original return values remain below and are discarded after the loop.
        p := luaL_tolstring(Lua.State, i, nil);
        S := S + PUTF8Char(p);
        lua_pop(Lua.State, 1);
      end;
      if S <> '' then
        Main.Console.Writeln(S);
      //Discard the converted return values from the persistent Lua stack.
      lua_pop(Lua.State, n);
    end
    else
    begin
      AOutput := lua_tostring(Lua.State, -1);
      lua_pop(Lua.State, 1);
    end;
  end;

begin
  Result := False;
  AOutput := '';
  if Lua.State = nil then
    Exit;
  //A previous Stop() left this state terminated. A console line starts a new
  //execution in the same persistent Lua state.
  Lua.SetReady;
  AChunk := ALine;
  if luaL_loadstring(Lua.State, PUTF8Char(AChunk)) = 0 then
    RunChunk
  else
  begin
    Msg := lua_tostring(Lua.State, -1);
    lua_pop(Lua.State, 1);
    //bare expression ("x", "1 + 1"): retry as "return ..." so it prints a value
    if luaL_loadstring(Lua.State, PUTF8Char('return ' + AChunk)) = 0 then
      RunChunk
    else
    begin
      lua_pop(Lua.State, 1);
      AOutput := Msg; //genuine syntax error: report the first one
    end;
  end;
end;

procedure TLuaScript.AddQueueObject(AQueueObject: TQueueObject);
{$ifdef DEBUG_LUA}
var
  ar: lua_Debug;
{$endif}
begin
  {$ifdef DEBUG_LUA}
  //Debug-only: stamp the queued command with the offending Lua source line.
  if Lua.State.GetStack(1, ar) then
  begin
    Lua.State.GetInfo('nSl', ar);
    AQueueObject.LineNo := ar.currentline;
  end;
  {$endif}
  inherited;
end;

function TLuaCanvas.Clear_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TClearObject.Create(Main.Canvas));
  Result := 0;
end;

function TLuaWindow.Window_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  w, h: integer;
begin
  c := L.Count;
  w := ScreenWidth;
  h := ScreenHeight;
  if c > 0 then
    w := round(L.Params[1].AsNumber);
  if c > 1 then
    h := round(L.Params[2].AsNumber);
  FScript.RunQueueObject(TWindowObject.Create(w, h));
  Result := 0;
end;

function TLuaConsole.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
begin
  c := L.Count;
  x := 0;
  y := 0;
  w := 0;
  h := 0;
  if c >= 2 then
  begin
    x := round(L.ToNumber(1));
    y := round(L.ToNumber(2));
  end;
  if c >= 4 then
  begin
    w := round(L.ToNumber(3));
    h := round(L.ToNumber(4));
  end;
  if (w > 0) and (h > 0) then
    FScript.RunQueueObject(TShowConsoleObject.Create(x, y, w, h))
  else
    FScript.RunQueueObject(TShowConsoleObject.Create(x, y));
  Result := 0;
end;

function TLuaCanvas.Text_func(L: Plua_State): integer; cdecl;
var
  x, y: integer;
  s: string;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  s := L.ToString(3);
  FScript.AddQueueObject(TDrawTextObject.Create(Main.Canvas, x, y, s));
  Result := 0;
end;

function TLuaCanvas.Circle_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, r: integer;
  f: boolean;
begin
  f := False;
  c := L.Count;
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  r := round(L.ToNumber(3));
  if c >= 4 then
    f := L.ToBoolean(4);
  FScript.AddQueueObject(TDrawCircleObject.Create(Main.Canvas, x, y, r, f));
  Result := 0;
end;

function TLuaCanvas.Rectangle_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
  f: boolean;
begin
  f := False;
  c := L.Count;
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  w := round(L.ToNumber(3));
  h := round(L.ToNumber(4));
  if c >= 4 then
    f := L.ToBoolean(5);
  FScript.AddQueueObject(TDrawRectangleObject.Create(Main.Canvas, x, y, w, h, f));
  Result := 0;
end;

function TLuaCanvas.Line_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x1, y1, x2, y2: integer;
begin
  c := L.Count;
  x1 := round(L.ToNumber(1));
  y1 := round(L.ToNumber(2));
  if c = 4 then
  begin
    x2 := round(L.ToNumber(3));
    y2 := round(L.ToNumber(4));
    FScript.AddQueueObject(TDrawLineObject.Create(Main.Canvas, x1, y1, x2, y2));
  end
  else
    FScript.AddQueueObject(TDrawLineToObject.Create(Main.Canvas, x1, y1));
  Result := 0;
end;

function TLuaCanvas.Point_func(L: Plua_State): integer; cdecl;
var
  x, y: integer;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  FScript.AddQueueObject(TDrawPointObject.Create(Main.Canvas, x, y));
  Result := 0;
end;

function TLuaConsole.Print_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.Count;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(Main.Canvas, s, False));
  Result := 0;
end;

function TLuaConsole.PrintLn_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.Count;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.Params[i].AsString;
  end;
  FScript.AddQueueObject(TPrintObject.Create(Main.Canvas, s, True));
  Result := 0;
end;

function TLuaConsole.PrintOut_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.Count;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(Main.Canvas, s, False));
  FScript.AddQueueObject(TOutputPrintObject.Create(Main.Canvas, s, False));
  Result := 0;
end;

function TLuaConsole.PrintLnOut_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.Count;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.Params[i].AsString;
  end;
  FScript.AddQueueObject(TPrintObject.Create(Main.Canvas, s, True));
  FScript.AddQueueObject(TOutputPrintObject.Create(Main.Canvas, s, True));
  Result := 0;
end;

{ TLuaOutput }

function TLuaOutput.Setter(L: PLua_State): integer;
var
  i: integer;
  field: string;
  r: TRect;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'visible' then
    Main.Output.Visible := L.ToBoolean(-1)
  else if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'height' then
      Main.Output.Height := i
    else if field = 'width' then
      Main.Output.Width := i
    else if field = 'left' then
    begin
      r := Main.Output.BoundsRect;
      Main.Output.BoundsRect := Rect(i, r.Top, i + r.Width, r.Bottom);
    end
    else if field = 'top' then
    begin
      r := Main.Output.BoundsRect;
      Main.Output.BoundsRect := Rect(r.Left, i, r.Right, i + r.Height);
    end
    else if field = 'margin' then
      Main.Output.Margin := i
    else if field = 'maxlines' then
      Main.Output.MaxLines := i;
  end
  else if L.IsString(-1) then
  begin
    if field = 'textColor' then
      Main.Output.TextColor := StrToColor(L.ToString(-1))
    else if field = 'backColor' then
      Main.Output.BackColor := StrToColor(L.ToString(-1));
  end;
end;

function TLuaOutput.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  case field of
    'visible':
    begin
      L.PushBoolean(Main.Output.Visible);
      Result := 1;
    end;
    'height':
    begin
      L.PushInteger(Main.Output.Height);
      Result := 1;
    end;
    'width':
    begin
      L.PushInteger(Main.Output.Width);
      Result := 1;
    end;
    'left':
    begin
      L.PushInteger(Main.Output.BoundsRect.Left);
      Result := 1;
    end;
    'top':
    begin
      L.PushInteger(Main.Output.BoundsRect.Top);
      Result := 1;
    end;
    'lines':
    begin
      L.PushInteger(Main.Output.LineCount);
      Result := 1;
    end;
    'maxlines':
    begin
      L.PushInteger(Main.Output.MaxLines);
      Result := 1;
    end;
    'margin':
    begin
      L.PushInteger(Main.Output.Margin);
      Result := 1;
    end;
  end;
end;

constructor TLuaOutput.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
end;

// output.show() | output.show(x, y) | output.show(x, y, w, h) -> show and place
function TLuaOutput.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
begin
  c := L.Count;
  x := 0;
  y := 0;
  w := 0;
  h := 0;
  if c >= 2 then
  begin
    x := round(L.ToNumber(1));
    y := round(L.ToNumber(2));
  end;
  if c >= 4 then
  begin
    w := round(L.ToNumber(3));
    h := round(L.ToNumber(4));
  end;
  if (w > 0) and (h > 0) then
    FScript.RunQueueObject(TShowOutputObject.Create(x, y, w, h))
  else
    FScript.RunQueueObject(TShowOutputObject.Create(x, y));
  Result := 0;
end;

function TLuaOutput.Hide_func(L: Plua_State): integer; cdecl;
begin
  FScript.RunQueueObject(THideOutputObject.Create);
  Result := 0;
end;

function TLuaOutput.Clear_func(L: Plua_State): integer; cdecl;
begin
  Main.Output.Clear;
  Result := 0;
end;

{ TLuaMusic }

function TLuaMusic.Beep_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TBeepObject.Create);
  Result := 0;
end;

function TLuaMusic.Sound_func(L: Plua_State): integer; cdecl;
var
  Freq, Period: integer;
begin
  Freq := round(L.ToNumber(1));
  Period := round(L.ToNumber(2));
  FScript.AddQueueObject(TPlaySoundObject.Create(Freq, Period));
  Result := 0;
end;

function TLuaMusic.Play_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := L.ToString(1);
  s := Resources.GuessFileName(s, Script.Path);
  FScript.AddQueueObject(TPlayMusicFileObject.Create(s));
  Result := 0;
end;

function TLuaMusic.MML_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
  Song: TmmlSong;
begin
  Song := nil;
  c := L.Count;
  SetLength(Song, c);
  for i := 0 to c - 1 do
  begin
    s := L.ToString(i + 1);
    Song[i] := s;
  end;
  //Audio objects and raylib audio calls stay on the main thread. Playback is
  //advanced incrementally by TTyroMain.Update, so this does not block drawing.
  FScript.AddQueueObject(TPlayMMLObject.Create(Song));
  Result := 0;
end;

{ TLuaRadio }

function TLuaRadio.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  case field of
    'title':
    begin
      L.PushString(RadioPlayer.Title);
      Result := 1;
    end;
    'station':
    begin
      L.PushString(RadioPlayer.Station);
      Result := 1;
    end;
    'genre':
    begin
      L.PushString(RadioPlayer.Genre);
      Result := 1;
    end;
    'bitrate':
    begin
      L.PushString(RadioPlayer.Bitrate);
      Result := 1;
    end;
    'url':
    begin
      L.PushString(RadioPlayer.URL);
      Result := 1;
    end;
    'state':
    begin
      L.PushString(RadioPlayer.StateString);
      Result := 1;
    end;
    'error':
    begin
      L.PushString(RadioPlayer.Error);
      Result := 1;
    end;
    'playing':
    begin
      L.PushBoolean(RadioPlayer.Playing);
      Result := 1;
    end;
    'buffered':
    begin
      L.PushInteger(RadioPlayer.Buffered);
      Result := 1;
    end;
  end;
end;

function TLuaRadio.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaRadio.Play_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raPlay, L.ToString(1)));
  Result := 0;
end;

function TLuaRadio.Pause_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raPause));
  Result := 0;
end;

function TLuaRadio.Resume_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raResume));
  Result := 0;
end;

function TLuaRadio.Stop_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raStop));
  Result := 0;
end;

{ TRadioPlayObject }

constructor TRadioPlayObject.Create(AAction: TRadioAction);
begin
  inherited Create;
  FAction := AAction;
end;

constructor TRadioPlayObject.Create(AAction: TRadioAction; const AURL: string);
begin
  Create(AAction);
  FURL := AURL;
end;

procedure TRadioPlayObject.DoExecute;
begin
  if RadioPlayer = nil then
    Exit;
  case FAction of
    raPlay: RadioPlayer.Play(FURL);
    raPause: RadioPlayer.Pause;
    raResume: RadioPlayer.Resume;
    raStop: RadioPlayer.Stop;
  end;
end;

{ TLuaSpectrum }

constructor TLuaSpectrum.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
end;

function TLuaSpectrum.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  case field of
    'active':
    begin
      L.PushBoolean(Spectrum.Active);
      Result := 1;
    end;
    'bars':
    begin
      L.PushInteger(Spectrum.Bars);
      Result := 1;
    end;
    'visible':
    begin
      L.PushBoolean(Spectrum.Visible);
      Result := 1;
    end;
  end;
end;

function TLuaSpectrum.Setter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if (field = 'bars') and L.IsInteger(-1) then
    Spectrum.RequestedBars := L.ToInteger(-1);
end;

function TLuaSpectrum.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
begin
  c := L.Count;
  x := 120;
  y := 80;
  w := 400;
  h := 200;
  if c >= 2 then
  begin
    x := round(L.ToNumber(1));
    y := round(L.ToNumber(2));
  end;
  if c >= 4 then
  begin
    w := round(L.ToNumber(3));
    h := round(L.ToNumber(4));
  end;
  FScript.RunQueueObject(TShowSpectrumObject.Create(x, y, w, h));
  Result := 0;
end;

function TLuaSpectrum.Hide_func(L: Plua_State): integer; cdecl;
begin
  FScript.RunQueueObject(THideSpectrumObject.Create);
  Result := 0;
end;

{ TShowSpectrumObject }

constructor TShowSpectrumObject.Create(AX, AY, AW, AH: Integer);
begin
  inherited Create;
  FX := AX;
  FY := AY;
  FW := AW;
  FH := AH;
end;

procedure TShowSpectrumObject.DoExecute;
begin
  Spectrum.Show(FX, FY, FW, FH);
end;

{ THideSpectrumObject }

procedure THideSpectrumObject.DoExecute;
begin
  Spectrum.Hide;
end;

function TLuaScript.IsKeyPressed_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := L.ToString(1);
  L.PushBoolean(TyroInput.IsKeyPressed(s));
  Result := 1;
end;

function TLuaScript.IsKeyDown_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := L.ToString(1);
  L.PushBoolean(TyroInput.IsKeyDown(s));
  Result := 1;
end;

function TLuaScript.MouseX_func(L: Plua_State): integer; cdecl;
begin
  L.PushInteger(TyroInput.MouseX);
  Result := 1;
end;

function TLuaScript.MouseY_func(L: Plua_State): integer; cdecl;
begin
  L.PushInteger(TyroInput.MouseY);
  Result := 1;
end;

function TLuaScript.IsMouseButtonPressed_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := L.ToString(1);
  L.PushBoolean(TyroInput.IsMouseButtonPressed(s));
  Result := 1;
end;

function TLuaScript.FrameTime_func(L: Plua_State): integer; cdecl;
begin
  L.PushNumber(TyroInput.FrameTime);
  Result := 1;
end;

function TLuaScript.TotalTime_func(L: Plua_State): integer; cdecl;
begin
  L.PushNumber(TyroInput.TotalTime);
  Result := 1;
end;

function TLuaScript.RandomValue_func(L: Plua_State): integer; cdecl;
var
  minv, maxv: integer;
begin
  minv := round(L.ToNumber(1));
  maxv := round(L.ToNumber(2));
  L.PushInteger(TyroInput.RandomValue(minv, maxv));
  Result := 1;
end;

function TLuaScript.Screenshot_func(L: Plua_State): integer; cdecl;
begin
  if L.Count > 0 then
    // TakeScreenshot must run on the main thread after the frame is
    // presented, so queue the request and let the engine capture it.
    Main.QueueScreenshot(L.ToString(1));
  Result := 0;
end;

function TLuaConsole.Read_func(L: Plua_State): integer; cdecl;
var
  s: string;
  c: integer;
  Reader: TReadConsoleObject;
begin
  s := '> ';
  c := L.Count;
  if c > 0 then
    s := L.ToString(1);

  Reader := TReadConsoleObject.Create(s);
  try
    // Run the DoExecute on the main thread via Synchronize.
    // The object is NOT freed by the engine; we free it here.
    Reader.Run(Script.Thread);
    //TThread.Synchronize(ScriptThread, procedure begin sleep(1000) end);
    // Wait for user to press Enter (signaled from main thread callback)
    if Reader.Wait then
      // Push the result string to Lua
      L.PushString(Reader.ResultString);
    Result := 1;
  finally
    Reader.Free;
  end;
end;

function TLuaFont.Load_func(L: Plua_State): integer; cdecl;
var
  aFile: string;
  aSize: integer;
begin
  aFile := L.ToString(1);
  // Load font from current directory (ScriptPath or WorkSpace)
  aFile := Resources.GuessFileName(aFile, Script.Path);
  if L.IsNumber(2) then
    aSize := L.ToInteger(2) //LoadFontEx
  else
    aSize := 0; //LoadFont
   FScript.AddQueueObject(TLoadFontObject.Create(aFile, aSize));
   Result := 0;
end;

{ Sprites }

function TLuaSprite.RegisterSprite(AHandle: integer): integer;
var
  base: integer;
begin
  with Script do
  begin
    Lua.State.BeginTable; //[sprite]
    base := Lua.State.Count; //index of the sprite table

    //keep a duplicate, the -2/-3 addressing in Register() hits the sprite table
    Lua.State.PushValue(-1); //[sprite, sprite]

    Lua.State.PushInteger(AHandle);
    Lua.State.SetField(-2, '__handle'); //sprite.__handle = AHandle

    //methods receive the sprite table injected as argument 1
    Lua.State.Register('load', @Load_func);
    Lua.State.Register('loadscript', @LoadScript_func);
    Lua.State.Register('show', @Show_func);
    Lua.State.Register('hide', @Hide_func);
    Lua.State.Register('move', @Move_func);
    Lua.State.Register('width', @Width_func);
    Lua.State.Register('height', @Height_func);
    Lua.State.Register('play', @Play_func);
    Lua.State.Register('stop', @Stop_func);
    Lua.State.Register('pause', @Stop_func);
    Lua.State.Register('framecount', @FrameCount_func);

    //metatable with property getter/setter (no table injection, Lua passes the table as arg 1)
    Lua.State.NewTable; //[sprite, sprite, meta]
    Lua.State.RegisterMeta('__index', @__getter);
    Lua.State.RegisterMeta('__newindex', @__setter);
    Lua.State.SetMetaTable(-2); //sprite.metatable = meta

    // remember this sprite table so collision.pump() can find it by handle
    if AHandle > cSpriteInvalid then
    begin
      Lua.State.PushValue(-1); // duplicate the sprite table
      lua_rawseti(Lua.State, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle);
    end;

    Lua.State.Remove(base); //drop the first reference, keep one on the stack
    Result := 1;
  end;
end;

// Sprites.new("name"?) -> creates a new sprite object, returns it on Lua stack
function TLuaSprites.New_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
  CreateObj: TCreateSpriteObject;
begin
  if L.Count >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  // Create the sprite object immediately on the main thread so script-only
  // ("texture-less") sprites get a real handle; load() swaps the texture in place.
  CreateObj := TCreateSpriteObject.Create(aName);
  try
    CreateObj.Run(Script.Thread); //Will run in Synchronize
    handle := CreateObj.HandleResult;
  finally
    CreateObj.Free;
  end;
  if handle > cSpriteInvalid then
    Script.Sprite.RegisterSprite(handle)
  else
    Script.Sprite.RegisterSprite(-1);
  // Store optional name
  if aName <> '' then
  begin
    L.PushString(aName);
    L.SetField(-2, '__name');
  end;
  Result := 1;
end;

// Sprites.find("name") -> returns a sprite object for the named sprite, or nil
function TLuaSprites.Find_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(1);
  handle := Main.Sprites.FindByName(aName);
  if handle > cSpriteInvalid then
    Script.Sprite.RegisterSprite(handle)
  else
    L.PushNil;
  Result := 1;
end;

// Sprites("name") -> __call metamethod, same as Sprites.find
function TLuaSprites.Call_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(2); // first arg after table self
  handle := Main.Sprites.FindByName(aName);
  if handle > cSpriteInvalid then
    Script.Sprite.RegisterSprite(handle)
  else
    L.PushNil;
  Result := 1;
end;

function GetSpriteHandle(L: Plua_State; idx: integer): integer;
begin
  L.GetField(idx, '__handle');
  Result := L.ToInteger(-1);
  L.Pop(1);
end;

{ TLuaControls }

constructor TLuaControls.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
  FItems := TList.Create;
end;

destructor TLuaControls.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TLuaControls.GetControl(AHandle: Integer): TTyroControl;
begin
  Result := nil;
  if (AHandle >= 1) and (AHandle <= FItems.Count) then
    Result := TTyroControl(FItems[AHandle - 1]);
end;

function TLuaControls.FindByName(const AName: string): Integer;
var
  i: Integer;
  c: TTyroControl;
begin
  Result := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    c := TTyroControl(FItems[i]);
    if (c <> nil) and (c.Name = AName) then
    begin
      Result := i + 1;
      Exit;
    end;
  end;
end;

function TLuaControls.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaControls.Getter(L: PLua_State): integer;
begin
  Result := 0;
  if L.ToString(2) = 'count' then
  begin
    L.PushInteger(FItems.Count);
    Result := 1;
  end;
end;

//controls.new(class, captionOrText, x?, y?, w?, h?, name?) -> handle
//(created on the main thread, self-drawn by the main cycle; the returned
//handle is 1-based over the controls created by this script)
//legacy call buttons.new(caption, x?, y?, w?, h?, borderSize?) still works
function TLuaControls.New_func(L: Plua_State): integer; cdecl;
var
  clsName, caption, aName: string;
  c: Integer;
  x, y, w, h: Integer;
  popStart: Integer; //position of the first optional numeric argument
  CreateObj: TCreateControlObject;
begin
  Result := 1;
  c := L.Count;
  caption := L.ToString(1);
  clsName := LowerCase(caption);
  x := 0;
  y := 0;
  aName := '';
  if (clsName = 'button') or (clsName = 'panel') or (clsName = 'label') or
     (clsName = 'checkbox') or (clsName = 'edit') or (clsName = 'spectrum') or (clsName = 'listbox') then
  begin
    //new style: controls.new(class, captionOrText, x?, y?, w?, h?, name?)
    clsName := caption;
    caption := L.ToString(2);
    popStart := 3;
    case clsName of
      'button': begin w := 100; h := 32; end;
      'panel': begin w := 100; h := 100; end;
      'label': begin w := 120; h := 24; end;
      'checkbox': begin w := 120; h := 24; end;
      'edit': begin w := 140; h := 28; end;
      'spectrum': begin w := 500; h := 200; end;
      'listbox': begin w := 160; h := 120; end;
    end;
    if c >= 7 then
      aName := L.ToString(7);
  end
  else
  begin
    //legacy: buttons.new(caption, x?, y?, w?, h?, borderSize?)
    clsName := 'button';
    popStart := 2;
    w := 100;
    h := 32;
  end;
  if c >= popStart then x := round(L.ToNumber(popStart));
  if c >= popStart + 1 then y := round(L.ToNumber(popStart + 1));
  if c >= popStart + 2 then w := round(L.ToNumber(popStart + 2));
  if c >= popStart + 3 then h := round(L.ToNumber(popStart + 3));

  CreateObj := TCreateControlObject.Create(clsName, caption, x, y, w, h, aName);
  try
    CreateObj.Run(Script.Thread); //Will run in Synchronize
    if CreateObj.Control <> nil then
    begin
      FItems.Add(CreateObj.TakeControl);
      L.PushInteger(FItems.Count); //handle of the created control
    end
    else
      L.PushNil;
  finally
    CreateObj.Free;
  end;
end;

//controls.caption(handle [, text]) -> get/set the caption (button/label/checkbox)
function TLuaControls.Caption_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.Count >= 2 then
 begin
  FScript.RunQueueObject(TSetControlTextObject.Create(ctrl, L.ToString(2)));
    Result := 0;
  end
  else
  begin
    L.PushString(ctrl.GetText);
    Result := 1;
  end;
end;

//controls.text(handle [, s]) -> get/set the text (caption for buttons/labels,
//edited text for edits)
function TLuaControls.Text_func(L: Plua_State): integer; cdecl;
begin
  Result := Caption_func(L);
end;

//controls.checked(handle [, value]) -> get/set the checked state (checkbox)
function TLuaControls.Checked_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.Count >= 2 then
 begin
  FScript.RunQueueObject(TSetControlCheckedObject.Create(ctrl,
    L.ToBoolean(2)));
    Result := 0;
  end
  else
  begin
    L.PushBoolean(ctrl.GetChecked);
    Result := 1;
  end;
end;

//controls.position(handle) -> x, y ; controls.position(handle, x, y) -> move
function TLuaControls.Position_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  r: TRect;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.Count >= 3 then
  begin
    r := ctrl.BoundsRect;
    r := Rect(round(L.ToNumber(2)), round(L.ToNumber(3)),
              round(L.ToNumber(2)) + r.Width, round(L.ToNumber(3)) + r.Height);
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ctrl.BoundsRect.Left);
    L.PushInteger(ctrl.BoundsRect.Top);
    Result := 2;
  end;
end;

//controls.move(handle, x, y) -> move the control (keeps its size)
function TLuaControls.Move_func(L: Plua_State): integer; cdecl;
begin
  Result := Position_func(L);
end;

//controls.width(handle [, w]) -> get/set the width
function TLuaControls.Width_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  r: TRect;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.Count >= 2 then
  begin
    r := ctrl.BoundsRect;
    r.Right := r.Left + round(L.ToNumber(2));
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ctrl.Width);
    Result := 1;
  end;
end;

//controls.height(handle [, h]) -> get/set the height
function TLuaControls.Height_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  r: TRect;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.Count >= 2 then
  begin
    r := ctrl.BoundsRect;
    r.Bottom := r.Top + round(L.ToNumber(2));
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ctrl.Height);
    Result := 1;
  end;
end;

//controls.visible(handle [, value]) -> get/set visibility
function TLuaControls.Visible_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.Count >= 2 then
 begin
  FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl,
    L.ToBoolean(2)));
    Result := 0;
  end
  else
  begin
    L.PushBoolean(ctrl.Visible);
    Result := 1;
  end;
end;

//controls.show(handle) -> make the control visible
function TLuaControls.Show_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
 ctrl := GetControl(round(L.ToNumber(1)));
 if ctrl <> nil then
  FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl, True));
  Result := 0;
end;

//controls.hide(handle) -> make the control invisible
function TLuaControls.Hide_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
 ctrl := GetControl(round(L.ToNumber(1)));
 if ctrl <> nil then
  FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl, False));
  Result := 0;
end;

//controls.hover(handle) -> is the mouse over the control
function TLuaControls.Hover_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Hover);
  Result := 1;
end;

//controls.down(handle) -> is the control pressed (mouse down over it)
function TLuaControls.Down_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Down);
  Result := 1;
end;

//controls.clicked(handle) -> true once after the control is released (click)
function TLuaControls.Clicked_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Clicked);
  Result := 1;
end;

//controls.focused(handle) -> is the control focused (owns keyboard input)
function TLuaControls.Focused_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Focused);
  Result := 1;
end;

//controls.focus(handle) -> move the keyboard focus to the control (on the
//main thread, like every change of the input state)
function TLuaControls.Focus_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl <> nil then
    FScript.RunQueueObject(TSetControlFocusObject.Create(ctrl));
  Result := 0;
end;

//controls.border(handle [, style]) -> get/set the border style
//0=none, 1=thin, 2=thick, 3=sizable
function TLuaControls.Border_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  v: Integer;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.Count >= 2 then
  begin
  v := round(L.ToNumber(2));
  case v of
   1: FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, brdThin));
   2: FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, brdThick));
   3: FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, brdSizable));
  else
   FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, brdNone));
  end;
    Result := 0;
  end
  else
  begin
    case ctrl.Border of
      brdThin: L.PushInteger(1);
      brdThick: L.PushInteger(2);
      brdSizable: L.PushInteger(3);
    else
      L.PushInteger(0);
    end;
    Result := 1;
  end;
end;

//controls.backcolor(handle [, color]) -> get/set the back color as an int
//(use colors.name or an #rrggbb int from the colors table)
function TLuaControls.BackColor_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.Count >= 2 then
 begin
  FScript.RunQueueObject(TSetControlBackColorObject.Create(ctrl,
    IntToColor(round(L.ToNumber(2)))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ColorToInt(ctrl.BackColor));
    Result := 1;
  end;
end;

//controls.name(handle [, name]) -> get/set the control name (named controls
//resolve as Lua globals, e.g. controls.new('button', 'OK', 0, 0, ..., 'ok')
//makes the global 'ok' a handle)
function TLuaControls.Name_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.Count >= 2 then
 begin
  FScript.RunQueueObject(TSetControlNameObject.Create(ctrl, L.ToString(2)));
    Result := 0;
  end
  else
  begin
    L.PushString(ctrl.Name);
    Result := 1;
  end;
end;

//controls.align(handle [, value]) -> get/set the docking edge of a control.
//Values: 'none', 'left', 'top', 'right', 'bottom', 'client' (or 0..5).
function TLuaControls.Align_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  s: string;
  n: Integer;
  aAlign: TAlign;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if L.Count >= 2 then
  begin
    s := LowerCase(Trim(L.ToString(2)));
    aAlign := alNone;
    if TryStrToInt(s, n) and (n >= Ord(alNone)) and (n <= Ord(alClient)) then
      aAlign := TAlign(n)
    else if s = 'none' then
      aAlign := alNone
    else if s = 'left' then
      aAlign := alLeft
    else if s = 'top' then
      aAlign := alTop
    else if s = 'right' then
      aAlign := alRight
    else if s = 'bottom' then
      aAlign := alBottom
    else if s = 'client' then
      aAlign := alClient
    else
    begin
      L.PushNil;
      Result := 1;
      Exit;
    end;
    FScript.RunQueueObject(TSetControlAlignObject.Create(ctrl, aAlign));
    Result := 0;
  end
  else
  begin
    case ctrl.Align of
      alLeft: L.PushString('left');
      alTop: L.PushString('top');
      alRight: L.PushString('right');
      alBottom: L.PushString('bottom');
      alClient: L.PushString('client');
    else
      L.PushString('none');
    end;
    Result := 1;
  end;
end;

//controls.parent(handle [, parentHandle]) -> get/set the container. The getter
//returns 0 when the control belongs to the main window, and the setter moves
//it back there for a nil or zero handle.
function TLuaControls.Parent_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  parentCtrl: TTyroControl;
  newParent: TTyroLayout;
  i: Integer;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if L.Count >= 2 then
  begin
    if lua_isnil(L, 2) or (round(L.ToNumber(2)) <= 0) then
      newParent := Main
    else
    begin
      parentCtrl := GetControl(round(L.ToNumber(2)));
      if parentCtrl = nil then
      begin
        L.PushNil;
        Result := 1;
        Exit;
      end;
      newParent := parentCtrl;
    end;
    FScript.RunQueueObject(TSetControlParentObject.Create(ctrl, newParent));
    Result := 0;
  end
  else
  begin
    //Return 0 for the main window so the result can be passed straight back
    //to the setter.
    L.PushInteger(0);
    Result := 1;
    for i := 0 to FItems.Count - 1 do
    begin
      parentCtrl := TTyroControl(FItems[i]);
      if (parentCtrl <> nil) and (parentCtrl = ctrl.Parent) then
      begin
        L.PushInteger(i + 1);
        Break;
      end;
    end;
  end;
end;

//controls.items(handle [, item1, item2, ...]) -> replace all items; with no
//extra arguments returns the number of items
function TLuaControls.Items_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  lb: TTyroListBox;
  i: Integer;
  Items: TStringList;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  lb := TTyroListBox(ctrl);
  if L.Count >= 2 then
  begin
    Items := TStringList.Create;
    try
      for i := 1 to L.Count - 1 do
        Items.Add(L.ToString(i + 1));
      FScript.RunQueueObject(TSetControlItemsObject.Create(lb, Items));
    finally
      Items.Free;
    end;
    Result := 0;
  end
  else
  begin
    L.PushInteger(lb.Items.Count);
    Result := 1;
  end;
end;

//controls.item(handle, index [, text]) -> get/set a single item
function TLuaControls.Item_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  lb: TTyroListBox;
  idx: Integer;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  lb := TTyroListBox(ctrl);
  idx := round(L.ToNumber(2));
  if L.Count >= 3 then
  begin
    FScript.RunQueueObject(TSetControlItemObject.Create(lb, idx, L.ToString(3)));
    Result := 0;
  end
  else
  begin
    if (idx < 0) or (idx >= lb.Items.Count) then
      L.PushNil
    else
      L.PushString(lb.Items[idx]);
    Result := 1;
  end;
end;

//controls.additem(handle, text) -> append an item
function TLuaControls.AddItem_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl <> nil) and (ctrl is TTyroListBox) then
    FScript.RunQueueObject(TAddControlItemObject.Create(TTyroListBox(ctrl), L.ToString(2)));
  Result := 0;
end;

//controls.clear(handle) -> remove all items
function TLuaControls.Clear_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl <> nil) and (ctrl is TTyroListBox) then
    FScript.RunQueueObject(TClearControlItemsObject.Create(TTyroListBox(ctrl)));
  Result := 0;
end;

//controls.viewcount(handle [, n]) -> get/set the visible rows (0 = boundsrect size)
function TLuaControls.ViewCount_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.Count >= 2 then
  begin
    FScript.RunQueueObject(TSetControlViewCountObject.Create(TTyroListBox(ctrl), round(L.ToNumber(2))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(TTyroListBox(ctrl).ViewCount);
    Result := 1;
  end;
end;

//controls.itemindex(handle [, n]) -> get/set the selected row (-1 = none)
function TLuaControls.ItemIndex_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.Count >= 2 then
  begin
    FScript.RunQueueObject(TSetControlItemIndexObject.Create(TTyroListBox(ctrl), round(L.ToNumber(2))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(TTyroListBox(ctrl).ItemIndex);
    Result := 1;
  end;
end;
function TLuaSprite.Load_func(L: Plua_State): integer; cdecl;
var
  aFile, aName, aScriptFile: string;
  handle: integer;
  LoadObj: TLoadSpriteObject;
  ScriptObj: TLoadSpriteScriptObject;
begin
  aFile := Resources.GuessFileName(L.ToString(2));
  if L.Count >= 3 then
    aScriptFile := L.ToString(3);
  L.GetField(1, '__name');
  if L.IsString(-1) then
    aName := L.ToString(-1)
  else
    aName := ExtractFileName(aFile);
  L.Pop(1);
  // reuse the handle created by Sprites.new() if this sprite already exists
  handle := GetSpriteHandle(L, 1);
  if handle <= cSpriteInvalid then
    handle := cSpriteInvalid;
  LoadObj := TLoadSpriteObject.Create(aFile, aName, handle);
  try
    LoadObj.Run(Script.Thread);
    handle := LoadObj.HandleResult;
    if handle > cSpriteInvalid then
    begin
      L.PushInteger(handle);
      L.SetField(1, '__handle');
      // re-index the sprite table under its real handle (it was keyed as unloaded)
      L.PushValue(1);
      lua_rawseti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + handle);
      // optional per-sprite script; compiled on the main thread by the engine queue
      if aScriptFile <> '' then
      begin
        ScriptObj := TLoadSpriteScriptObject.Create(Script, handle, aScriptFile);
        FScript.AddQueueObject(ScriptObj);
      end;
    end
    else
      Script.DoError('Sprite not loaded: ' + aFile);
  finally
    LoadObj.Free;
  end;
  Result := 0;
end;

// sprite:loadscript("file.ls") -> compile & attach a per-sprite script (main thread)
function TLuaSprite.LoadScript_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
  aScriptFile: string;
  ScriptObj: TLoadSpriteScriptObject;
begin
  handle := GetSpriteHandle(L, 1);
  aScriptFile := L.ToString(2);
  if (handle > cSpriteInvalid) and (aScriptFile <> '') then
  begin
    ScriptObj := TLoadSpriteScriptObject.Create(Script, handle, aScriptFile);
    FScript.AddQueueObject(ScriptObj);
  end;
  Result := 0;
end;

function TLuaSprite.Show_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Main.Sprites.SetVisible(handle, True);
  Result := 0;
end;

function TLuaSprite.Hide_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Main.Sprites.SetVisible(handle, False);
  Result := 0;
end;

function TLuaSprite.Move_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
  x, y: single;
begin
  x := L.ToNumber(2);
  y := L.ToNumber(3);
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Main.Sprites.SetPosition(handle, x, y);
  Result := 0;
end;

function TLuaSprite.Width_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Main.Sprites.GetWidth(handle));
  Result := 1;
end;

function TLuaSprite.Height_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Main.Sprites.GetHeight(handle));
  Result := 1;
end;

// sprite:play([fps]) -> restart and play the animation. fps overrides the
// per-frame durations stored in the .aseprite file; 0 (or omitted) keeps them.
function TLuaSprite.Play_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
  begin
    if L.Count >= 2 then
      Main.Sprites.SetAnimSpeed(handle, L.ToNumber(2));
    Main.Sprites.SetAnimFrame(handle, 0);
    Main.Sprites.SetPlaying(handle, True);
  end;
  Result := 0;
end;

// sprite:stop() / sprite:pause() -> freeze the animation on the current frame
function TLuaSprite.Stop_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Main.Sprites.SetPlaying(handle, False);
  Result := 0;
end;

// sprite:framecount() -> number of frames in the loaded .aseprite animation
function TLuaSprite.FrameCount_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Main.Sprites.GetFrameCount(handle));
  Result := 1;
end;

// Sprite __index: read properties x, y, angle, scale, visible + physics keys; unknown keys raw-read from the sprite table
function TLuaSprite.Getter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key
  handle := GetSpriteHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle > cSpriteInvalid then
  begin
    if field = 'x' then
    begin
      L.PushNumber(Main.Sprites.GetX(handle));
      Result := 1;
    end
    else if field = 'y' then
    begin
      L.PushNumber(Main.Sprites.GetY(handle));
      Result := 1;
    end
    else if field = 'angle' then
    begin
      L.PushNumber(Main.Sprites.GetAngle(handle));
      Result := 1;
    end
    else if field = 'scale' then
    begin
      L.PushNumber(Main.Sprites.GetScale(handle));
      Result := 1;
    end
    else if field = 'visible' then
    begin
      L.PushBoolean(Main.Sprites.GetVisible(handle));
      Result := 1;
    end
    else if field = 'collide' then
    begin
      L.PushBoolean(Main.Sprites.GetCollide(handle));
      Result := 1;
    end
    else if field = 'kind' then
    begin
      case Main.Sprites.GetKind(handle) of
        skKinematic: L.PushString('kinematic');
        skStatic: L.PushString('static');
      else
        L.PushString('dynamic');
      end;
      Result := 1;
    end
    else if field = 'mass' then
    begin
      L.PushNumber(Main.Sprites.GetMass(handle));
      Result := 1;
    end
    else if field = 'friction' then
    begin
      L.PushNumber(Main.Sprites.GetFriction(handle));
      Result := 1;
    end
    else if field = 'bouncy' then
    begin
      L.PushNumber(Main.Sprites.GetBouncy(handle));
      Result := 1;
    end
    else if field = 'radius' then
    begin
      L.PushNumber(Main.Sprites.GetRadius(handle));
      Result := 1;
    end
    else if field = 'frames' then
    begin
      L.PushInteger(Main.Sprites.GetFrameCount(handle));
      Result := 1;
    end
    else if field = 'frame' then
    begin
      L.PushInteger(Main.Sprites.GetAnimFrame(handle));
      Result := 1;
    end
    else if field = 'playing' then
    begin
      L.PushBoolean(Main.Sprites.GetPlaying(handle));
      Result := 1;
    end
    else if field = 'speed' then
    begin
      L.PushNumber(Main.Sprites.GetAnimSpeed(handle));
      Result := 1;
    end
    else if field = 'looping' then
    begin
      L.PushBoolean(Main.Sprites.GetLooping(handle));
      Result := 1;
    end;
  end;
  if Result = 0 then
  begin
    // unknown key (e.g. onCollide) or unloaded sprite: read raw from the sprite table
    L.PushValue(2);
    lua_rawget(L, 1);
    Result := 1;
  end;
end;

// Sprite __newindex: write properties x, y, angle, scale, visible + physics keys; unknown keys raw-stored
function TLuaSprite.Setter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
  curX, curY: single;
  k: TSpriteKind;
begin
  // arg 1 is the table, arg 2 is the key, arg 3 is the value
  handle := GetSpriteHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle <= cSpriteInvalid then
  begin
    // unloaded sprite: store unknown keys (like onCollide) raw so they survive load()
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
    Exit;
  end;
  if (field = 'x') or (field = 'y') then
  begin
    curX := Main.Sprites.GetX(handle);
    curY := Main.Sprites.GetY(handle);
    if field = 'x' then
      curX := L.ToNumber(3)
    else
      curY := L.ToNumber(3);
    Main.Sprites.SetPosition(handle, curX, curY);
  end
  else if field = 'angle' then
  begin
    Main.Sprites.SetAngle(handle, L.ToNumber(3));
  end
  else if field = 'scale' then
  begin
    Main.Sprites.SetScale(handle, L.ToNumber(3));
  end
  else if field = 'visible' then
  begin
    Main.Sprites.SetVisible(handle, L.ToBoolean(3));
  end
  else if field = 'collide' then
  begin
    Main.Sprites.SetCollide(handle, L.ToBoolean(3));
  end
  else if field = 'kind' then
  begin
    if L.ToString(3) = 'kinematic' then
      k := skKinematic
    else if L.ToString(3) = 'static' then
      k := skStatic
    else
      k := skDynamic;
    Main.Sprites.SetKind(handle, k);
  end
  else if field = 'mass' then
  begin
    Main.Sprites.SetMass(handle, L.ToNumber(3));
  end
  else if field = 'friction' then
  begin
    Main.Sprites.SetFriction(handle, L.ToNumber(3));
  end
  else if field = 'bouncy' then
  begin
    Main.Sprites.SetBouncy(handle, L.ToNumber(3));
  end
  else if field = 'radius' then
  begin
    Main.Sprites.SetRadius(handle, L.ToNumber(3));
  end
  else if field = 'frame' then
  begin
    Main.Sprites.SetAnimFrame(handle, L.ToInteger(3));
  end
  else if field = 'playing' then
  begin
    Main.Sprites.SetPlaying(handle, L.ToBoolean(3));
  end
  else if field = 'speed' then
  begin
    Main.Sprites.SetAnimSpeed(handle, L.ToNumber(3));
  end
  else if field = 'looping' then
  begin
    Main.Sprites.SetLooping(handle, L.ToBoolean(3));
  end
  else
  begin
    // unknown key (e.g. onCollide = function): store it raw in the sprite table
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
  end;
end;

{ TLuaSpriteScript }

// Callback trampoline: Lua C-closure bound to an object method. Mirrors the
// internal one in LuaClasses, used to back the self/other sprite tables.
function sprite_script_method_callback(L: Plua_State): integer; cdecl;
var
  Method: TMethod;
begin
  Method.Data := lua_topointer(L, lua_upvalueindex(1));
  Method.Code := lua_topointer(L, lua_upvalueindex(2));
  if Method.Data = nil then
    raise Exception.Create('Lua: cannot execute sprite script method!');
  Result := TLuaMethod(Method)(L);
end;

// Builds a sprite table (like Sprites.new returns) bound to AHandle and leaves
// it on the Lua stack. Properties route through the proxy TLuaSprite getter/
// setter, so self.x / self.kind / ... work and unknown keys are raw-stored.
procedure PushSpriteTable(L: Plua_State; AHandle: Integer; ASprite: TLuaSprite);
begin
  L.NewTable; //[t]
  L.PushInteger(AHandle);
  L.SetField(-2, '__handle'); //t.__handle = AHandle -> [t]
  L.PushString(Main.Sprites.GetName(AHandle));
  L.SetField(-2, '__name'); //t.__name = sprite name -> [t]
  L.NewTable; //[t, meta]
  lua_pushlightuserdata(L, TMethod(@ASprite.__getter).Data);
  lua_pushlightuserdata(L, TMethod(@ASprite.__getter).Code);
  lua_pushcclosure(L, @sprite_script_method_callback, 2);
  lua_setfield(L, -2, '__index'); //[t, meta]
  lua_pushlightuserdata(L, TMethod(@ASprite.__setter).Data);
  lua_pushlightuserdata(L, TMethod(@ASprite.__setter).Code);
  lua_pushcclosure(L, @sprite_script_method_callback, 2);
  lua_setfield(L, -2, '__newindex'); //[t, meta]
  lua_setmetatable(L, -2); //t.metatable = meta -> [t]
end;

// draw.* color for sprite scripts: the colors table stores AARRGGBB as a Lua
// integer. Read it back via the Lua number (int64-safe) and mask bytes
// explicitly so large values never get mangled by 32-bit arithmetic.
function SpriteColorValue(L: Plua_State; Idx: Integer): TColor;
var
  v: UInt32;
begin
  v := UInt32(Trunc(L.ToNumber(Idx)));
  Result.RGBA.Red := Byte(v and $FF);
  Result.RGBA.Green := Byte((v shr 8) and $FF);
  Result.RGBA.Blue := Byte((v shr 16) and $FF);
  Result.RGBA.Alpha := Byte((v shr 24) and $FF);
end;

constructor TLuaSpriteScript.Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
begin
  inherited Create;
  FScript := AScript;
  FHandle := AHandle;
  FFileName := AFileName;
  FSprite := TLuaSprite.Create(AScript); // proxy; only its property getter/setter are used
  FLua.Init;
  BuildSelf;
  BuildGlobals;
  BuildColors;
  BuildDraw;
end;

destructor TLuaSpriteScript.Destroy;
begin
  FLua.Close;
  FSprite.Free;
  inherited;
end;

procedure TLuaSpriteScript.DoError(S: string; const AHandler: string);
begin
  FScript.DoError('[' + FFileName + '] ' + AHandler + ': ' + S);
end;

function TLuaSpriteScript.Load: Boolean;
var
  Msg: string;
begin
  Result := False;
  if FFileName = '' then
    Exit;
  FFileName := Resources.GuessFileName(FFileName);
  if luaL_dofile(FLua.State, PUTF8Char(FFileName)) <> 0 then
  begin
    Msg := FLua.State.ToString(-1);
    FLua.State.Pop(1);
    DoError(Msg, 'load');
  end
  else
    Result := True;
end;

procedure TLuaSpriteScript.BuildSelf;
begin
  PushSpriteTable(FLua.State, FHandle, FSprite); //[self]
  lua_setglobal(FLua.State, 'self'); //[]
end;

procedure TLuaSpriteScript.BuildGlobals;
begin
  // Reuse the main script's globals so per-sprite states can call the same
  // helpers (the bound FScript outlives this state).
  FLua.State.RegisterGlobal('time', @FScript.TotalTime_func);
  FLua.State.RegisterGlobal('rand', @FScript.RandomValue_func);
  FLua.State.RegisterGlobal('println', @FScript.Console.PrintLn_func);
end;

procedure TLuaSpriteScript.BuildColors;
var
  i: integer;
begin
  FLua.State.BeginTable;
  for i := 0 to Length(FScript.Colors.Colors) - 1 do
    FLua.State.Register(FScript.Colors.Colors[i].Name, ColorToInt(FScript.Colors.Colors[i].Color));
  FLua.State.EndTable('colors', FScript.Colors);
end;

procedure TLuaSpriteScript.BuildDraw;
begin
  FLua.State.RegisterTable('draw');
  FLua.State.Register('draw', 'circle', Self, @Circle_func);
  FLua.State.Register('draw', 'rectangle', Self, @Rectangle_func);
  FLua.State.Register('draw', 'line', Self, @Line_func);
  FLua.State.Register('draw', 'text', Self, @Text_func);
end;

// Look up the global handler AName and pcall it. The caller has pushed the
// arguments (AArgs of them) below the handler position; an absent/ non-
// function handler just drains the stack and is ignored.
procedure TLuaSpriteScript.CallHandler(const AName: string; AArgs: Integer);
var
  Msg: string;
begin
  lua_getglobal(FLua.State, PUTF8Char(AName)); //[args..., func]
  if not lua_isfunction(FLua.State, -1) then
  begin
    FLua.State.Pop(1 + AArgs); // discard the function and the caller args
    Exit;
  end;
  if AArgs > 0 then
    lua_insert(FLua.State, 1); //[func, args...]
  if lua_pcall(FLua.State, AArgs, 0, 0) <> 0 then
  begin
    Msg := FLua.State.ToString(-1);
    FLua.State.Pop(1);
    DoError(Msg, AName);
  end;
end;

procedure TLuaSpriteScript.Update;
begin
  CallHandler('on_update', 0);
end;

procedure TLuaSpriteScript.Draw;
begin
  CallHandler('on_draw', 0);
end;

procedure TLuaSpriteScript.OnCollide(AOtherHandle: Integer; const AState: string);
begin
  PushSpriteTable(FLua.State, AOtherHandle, FSprite); //[other]
  FLua.State.PushString(AState); //[other, state]
  CallHandler('on_collide', 2);
end;

// draw.circle(x, y, radius, color, fill?) - sprite/world coordinates
function TLuaSpriteScript.Circle_func(L: Plua_State): integer; cdecl;
var
  x, y, r: single;
  f: boolean;
begin
  x := L.ToNumber(1);
  y := L.ToNumber(2);
  r := L.ToNumber(3);
  if L.Count >= 5 then
    f := L.ToBoolean(5)
  else
    f := True;
  if f then
    RayLib.DrawCircle(round(x), round(y), r, SpriteColorValue(L, 4))
  else
    RayLib.DrawCircleLines(round(x), round(y), r, SpriteColorValue(L, 4));
  Result := 0;
end;

// draw.rectangle(x, y, w, h, color, fill?) - sprite/world coordinates
function TLuaSpriteScript.Rectangle_func(L: Plua_State): integer; cdecl;
var
  x, y, w, h: integer;
  f: boolean;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  w := round(L.ToNumber(3));
  h := round(L.ToNumber(4));
  if L.Count >= 6 then
    f := L.ToBoolean(6)
  else
    f := True;
  if f then
    RayLib.DrawRectangle(x, y, w, h, SpriteColorValue(L, 5))
  else
    RayLib.DrawRectangleLinesEx(RectangleOf(x, y, w, h), 1, SpriteColorValue(L, 5));
  Result := 0;
end;

// draw.line(x1, y1, x2, y2, color)
function TLuaSpriteScript.Line_func(L: Plua_State): integer; cdecl;
begin
  RayLib.DrawLineEx(Vector2Of(L.ToNumber(1), L.ToNumber(2)), Vector2Of(L.ToNumber(3), L.ToNumber(4)), 1, SpriteColorValue(L, 5));
  Result := 0;
end;

// draw.text(x, y, text, color)
function TLuaSpriteScript.Text_func(L: Plua_State): integer; cdecl;
begin
  RayLib.DrawTextEx(Resources.Font.Data, PUTF8Char(L.ToString(3)), Vector2Of(L.ToNumber(1), L.ToNumber(2)), Resources.Font.Height, 0, SpriteColorValue(L, 4));
  Result := 0;
end;

{ TLoadSpriteScriptObject }

constructor TLoadSpriteScriptObject.Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
begin
  inherited Create;
  FScript := AScript;
  FHandle := AHandle;
  FFileName := AFileName;
end;

procedure TLoadSpriteScriptObject.DoExecute;
var
  Scr: TLuaSpriteScript;
begin
  if FHandle <= cSpriteInvalid then
    Exit;
  Scr := TLuaSpriteScript.Create(FScript, FHandle, FFileName);
  try
    if Scr.Load then
      Main.Sprites.SetScript(FHandle, Scr)
    else
      Scr.Free;
  except
    on E: Exception do
    begin
      Scr.Free;
      FScript.DoError('Sprite script failed: ' + FFileName + ': ' + E.ClassName + ': ' + E.Message);
    end;
  end;
end;
{ TLuaCollision }

function TLuaCollision.Getter(L: Plua_State): integer;
begin
  Result := 0;
  if Main.Physics = nil then
    Exit;
  if L.ToString(2) = 'gravityx' then
  begin
    L.PushNumber(Main.Physics.GetGravityX);
    Result := 1;
  end
  else if L.ToString(2) = 'gravityy' then
  begin
    L.PushNumber(Main.Physics.GetGravityY);
    Result := 1;
  end
  else if L.ToString(2) = 'bodies' then
  begin
    L.PushInteger(Main.Physics.BodyCount);
    Result := 1;
  end;
end;

function TLuaCollision.Setter(L: Plua_State): integer;
var
  x, y: single;
begin
  Result := 0;
  if Main.Physics = nil then
    Exit;
  x := Main.Physics.GetGravityX;
  y := Main.Physics.GetGravityY;
  if L.ToString(2) = 'gravityx' then
    x := L.ToNumber(3)
  else if L.ToString(2) = 'gravityy' then
    y := L.ToNumber(3);
  Main.Physics.SetGravity(x, y);
end;

// Fire the onCollide handler of the sprite AHandle, passing the other sprite and the contact state
procedure TLuaCollision.FireEvent(L: Plua_State; AHandle, AOtherHandle: Integer; const AState: string);
begin
  // get the target sprite table from the registry
  lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle);
  if not lua_istable(L, -1) then
  begin
    L.Pop(1);
    Exit;
  end;
  // read its onCollide field raw (bypasses __index)
  L.PushString('onCollide');
  lua_rawget(L, -2);
  if not lua_isfunction(L, -1) then
  begin
    L.Pop(2);
    Exit;
  end;
  // push the other sprite (or nil) and the state string as arguments
  lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AOtherHandle);
  if not lua_istable(L, -1) then
  begin
    L.Pop(1);
    L.PushNil;
  end;
  L.PushString(AState);
  if lua_pcall(L, 2, 0, 0) <> 0 then
  begin
    Script.DoError('onCollide: ' + L.ToString(-1));
    L.Pop(1);
  end;
  L.Pop(1); // remove the target sprite table
end;

// collision.pump() -> number of events dispatched; fires onCollide handlers for both sides
function TLuaCollision.Pump_func(L: Plua_State): integer; cdecl;
var
  Evs: array[0..1023] of TCollisionEvent;
  ACount: Integer;
  I: Integer;
  ev: TCollisionEvent;
begin
  Result := 0;
  if Main.Physics = nil then
    Exit;
  ACount := 0;
  Main.Physics.Poll(Evs, ACount);
  for I := 0 to ACount - 1 do
  begin
    ev := Evs[I];
    if ev.State = csBegin then
    begin
      FireEvent(L, ev.HandleA, ev.HandleB, 'enter');
      FireEvent(L, ev.HandleB, ev.HandleA, 'enter');
    end
    else
    begin
      FireEvent(L, ev.HandleA, ev.HandleB, 'leave');
      FireEvent(L, ev.HandleB, ev.HandleA, 'leave');
    end;
    Inc(Result);
  end;
end;

initialization
  Main.RegisterLanguage('Lua', ['.ls', '.lua', '.pluto'], TLuaScript);
end.
