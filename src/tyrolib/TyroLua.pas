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

{$ifdef FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$endif}
{$H+}{$M+}
{$define DEBUG_LUA}

interface

uses
  Classes, SysUtils, Types,
  LuaAPI, LuaClasses,
  RayLib, RayClasses, //remove it
  mnUtils, mnLogs,
  TyroScripts, TyroSounds, TyroClasses, Melodies, TyroSprites, TyroPhysics,
  TyroControls, TyroEngines, TyroInput,
  TyroRadio, TyroSpectrum,
  TyroMidi;

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
    function Shake_func(L: Plua_State): integer; cdecl;
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

  { TLuaMidi }

  TLuaMidi = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Play_func(L: PLua_State): integer; cdecl;
    function Pause_func(L: PLua_State): integer; cdecl;
    function Resume_func(L: PLua_State): integer; cdecl;
    function Stop_func(L: PLua_State): integer; cdecl;
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

  { TMidiAction }

  TMidiAction = (maPlay, maPause, maResume, maStop);

  { Routes midi.play/pause/resume/stop into the main window thread (the same
    one that drives RayUpdates.Update and the raylib audio device). }

  TMidiPlayObject = class(TQueueObject)
  private
    FAction: TMidiAction;
    FFileName: string;
  public
    constructor Create(AAction: TMidiAction); overload;
    constructor Create(AAction: TMidiAction; const AFileName: string); overload;
    procedure DoExecute; override;
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
    'edit', 'spectrum', 'listbox', 'image'); the returned handle addresses a
    control owned by the main window. The same object backs both the
    'controls' table and the legacy 'buttons' alias table, so old scripts keep
    working. }
  TLuaControls = class(TTyroLuaObject)
  private
    FItems: TList; //of TTyroControl (owned by the main window, not by us)
    function GetControl(AHandle: Integer): TTyroControl;
    function FindByName(const AName: string): Integer;
    function GetControlHandle(AControl: TTyroControl): Integer;
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function RegisterControl(AHandle: Integer): Integer;
    function New_func(L: Plua_State): integer; cdecl;
    function Caption_func(L: Plua_State): integer; cdecl;
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
    //text: btn.text = "Hi" / controls.text(handle [, "Hi"]); a caption control
    //keeps it in Caption, an edit in the shared Text
    function Text_func(L: Plua_State): integer; cdecl;
    //image control: img.load("logo.png") / controls.load(handle, "logo.png"),
    //the new state answers the call and reads back through the loaded field
    function LoadImage_func(L: Plua_State): integer; cdecl;
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
    procedure CallHandler(const AName: utf8string; AArgs: Integer); // pcall a global handler (args already on stack), ignore if not a function
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

  { TLuaFile }

  { A file handle handed to the script by openfile(). The object owns the
    stream; the Lua table registered for it keeps closures pointing at this
    instance, so the owning script frees every handle after closing its state.
    Binary modes ('b' in the mode string) read and write raw bytes, the
    remaining modes add the newline translation io does. }
  TLuaFile = class(TTyroLuaObject)
  private
    FFileName: string;
    FMode: string;
    //All reads and writes go through this one stream; text mode only adds
    //newline translation (see ReadTextFormat / Write_func)
    FStream: TFileStream;
    FIsText: Boolean;
    FIsOpen: Boolean;
    FError: string;
    //A pending f:lines() iterator; TStringList holds the lines it still owes
    FLines: TStringList;
    //How many of those lines LinesNext_func has handed out already. The position
    //lives here rather than in the loop control variable because Pluto's generic
    //for does not pass the value the iterator returned back to the next call.
    FLinesPos: Integer;
    function ModeFlag(const AFlag: Char): Boolean;
    //Reads one token ('n') or one line ('l'/'L') and pushes exactly one value
    function ReadTextFormat(L: PLua_State; const AFormat: Char): Boolean;
    //Index of the first value the script passed; see the implementation
    function FirstArgIndex(L: PLua_State): Integer;
  public
    //Opens the (already validated) path; returns False and sets Error on failure
    function Open(const AFileName, AMode: string): Boolean;
    function Read_func(L: Plua_State): integer; cdecl;
    function Write_func(L: Plua_State): integer; cdecl;
    function Close_func(L: Plua_State): integer; cdecl;
    function Flush_func(L: Plua_State): integer; cdecl;
    function Lines_func(L: Plua_State): integer; cdecl;
    constructor Create(AScript: TLuaScript); override;
    destructor Destroy; override;
    //Iterates the lines that Lines_func has already queued, so a plain
    //"for line in f:lines()" loop works. Returns nil once the list is drained.
    function LinesNext_func(L: Plua_State): integer; cdecl;
    property IsOpen: Boolean read FIsOpen;
    property Error: string read FError;
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
    Midi: TLuaMidi;
    Sprite: TLuaSprite;
    Sprites: TLuaSprites;
    Controls: TLuaControls;
    Output: TLuaOutput;
    Collision: TLuaCollision;
    Shader: TLuaShader;
    SpectrumLua: TLuaSpectrum;
    //Owns every TLuaFile handed out by openfile(); freed after Lua.Close,
    //because the Lua closures hold pointers into those objects
    Files: TList;
    procedure DoError(S: string);
    procedure Run; override;
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
    function Exit_func(L: Plua_State): integer; cdecl;
    function Clock_func(L: Plua_State): integer; cdecl;
    function Time_func(L: Plua_State): integer; cdecl;

    //file access

    function OpenFile_func(L: Plua_State): integer; cdecl;
    function Require_func(L: Plua_State): integer; cdecl;

   public
    procedure Init; override;
    destructor Destroy; override;
    procedure Stop; override;
    function RunLine(const ALine: string; out AOutput: string): Boolean; override;

    procedure AddQueueObject(AQueueObject: TQueueObject); override;
    //Global environment hooks: unresolved globals resolve to sprites/controls by name
    function __global_getter(L: Plua_State): integer; cdecl;
    function __global_setter(L: Plua_State): integer; cdecl;

  end;

const
  // Integer key base in the Lua registry for "sprite handle -> sprite table"
  cSpriteRegistryBase = $00700000; //HUH
  // Integer key base in the Lua registry for "control handle -> control table"
  cControlRegistryBase = $00800000;
  // Key in the Lua registry for the table that caches what require already ran
  cRequireCache = 'tyro.require.cache';

implementation

//global functions
function sleep_func(L: Plua_State): integer; cdecl;
var
  n: int64;
begin
  //Round, not truncate: sleep(16.7) for a 60 FPS frame must not collapse
  //to 16 ms and drift against the display cadence.
  n := Round(L.ToNumber(1));
  sleep(n);
  Result := 0;
end;

// True when FileName already carries a drive ("C:\x", "C:x") or a UNC root
// ("\\host\x"), and so has to be used as it stands. Pasting such a name onto a
// base folder only builds a name that cannot exist, and saying "outside the
// workspace" is the honest answer instead.
function IsRootedName(const FileName: string): Boolean;
begin
  Result := (Length(FileName) > 1) and ((FileName[2] = ':') or
    ((FileName[1] = '\') and (FileName[2] = '\')));
end;

// Resolves a script-supplied file name to an absolute path and returns it only
// when it lands inside the workspace, or '' when no candidate did. The bases are
// tried in the order the rest of Tyro does: the name as given (against the
// process directory), then the script folder (ABasePath), then the workspace
// root. Every candidate is expanded before the containment test, so a name built
// from "..", a drive-relative form or a UNC path cannot slip past it.
function ResolveAndValidatePath(const FileName, ABasePath: string): string;
var
  Candidates: array[0..2] of string;
  Candidate: string;
  i: Integer;
begin
  Result := '';
  if (FileName = '') or (Res = nil) then
    Exit;
  if IsRootedName(FileName) then
  begin
    Candidate := ExpandFileName(FileName);
    if Res.IsPathInsideWorkspace(Candidate) then
      Result := Candidate;
    Exit;
  end;
  Candidates[0] := FileName;
  Candidates[1] := IncludePathDelimiter(ABasePath) + FileName;
  Candidates[2] := IncludePathDelimiter(Res.WorkPath) + FileName;
  for i := Low(Candidates) to High(Candidates) do
    if Candidates[i] <> '' then
      // ExpandFileName collapses "..", relative and UNC forms, so the
      // containment test can trust a plain prefix comparison.
      if Res.IsPathInsideWorkspace(ExpandFileName(Candidates[i])) then
        Exit(ExpandFileName(Candidates[i]));
end;

function log_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
    Log.WriteLn(L.ToString(i));
  end;
  Result := 0;
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
    if (Thread <> nil) and not Engine.WaitToNextFrame(Self) then
      L.PushBoolean(False) //script was stopped while waiting -> end the loop
    else
      L.PushBoolean(True);
    Exit;
  end;

  //4) Sprites: reuse/create the sprite proxy bound to the found handle
  AHandle := Engine.Sprites.FindByName(aName);
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
    lua_rawgeti(L, LUA_REGISTRYINDEX, cControlRegistryBase + AHandle);
    if not lua_istable(L, -1) then
    begin
      L.Pop(1);
      Controls.RegisterControl(AHandle);
    end;
    L.PushValue(-1);
    L.PushValue(2);
    lua_rawset(L, 1);
    L.Remove(1); L.Remove(1);
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
begin
  Result := 0;
  field := L.ToString(2);
  if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'height' then
      Engine.Console.Height := i
    else if field = 'width' then
      Engine.Console.Width := i
    else if field = 'margin' then
      Engine.Console.Margin := i
  end
  else if L.IsString(-1) then
  begin
    if field = 'align' then
    begin
      //* TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
      if L.ToString(-1) = 'none' then
        Engine.Console.Align := TAlign(0)
      else if L.ToString(-1) = 'left' then
        Engine.Console.Align := TAlign(1)
      else if L.ToString(-1) = 'top' then
        Engine.Console.Align := TAlign(2)
      else if L.ToString(-1) = 'right' then
        Engine.Console.Align := TAlign(3)
      else if L.ToString(-1) = 'bottom' then
        Engine.Console.Align := TAlign(4)
      else if L.ToString(-1) = 'client' then
        Engine.Console.Align := TAlign(5);
    end;
  end;
end;

function TLuaConsole.Getter(L: PLua_State): integer;
var
  i: Integer;
  field: UTF8String;
begin
  Result := 0;
  field := lua_tostring(L, 2);
  if field = 'active' then
  begin
    lua_pushboolean(L, Engine.Console.Visible);
    Result := 1;
  end
  else if field = 'align' then
  begin
    // TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
    i := Ord(Engine.Console.Align);
    case i of
      0: lua_pushstring(L, 'none');
      1: lua_pushstring(L, 'left');
      2: lua_pushstring(L, 'top');
      3: lua_pushstring(L, 'right');
      4: lua_pushstring(L, 'bottom');
      5: lua_pushstring(L, 'client');
    else
      lua_pushstring(L, 'none');
    end;
    Result := 1;
  end
  else if field = 'height' then
  begin
    lua_pushinteger(L, Engine.Console.Height);
    Result := 1;
  end
  else if field = 'width' then
  begin
    lua_pushinteger(L, Engine.Console.Width);
    Result := 1;
  end
  else if field = 'margin' then
  begin
    lua_pushinteger(L, Engine.Console.Margin);
    Result := 1;
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
      Engine.Margin := i
    else if field = 'backcolor' then
      Engine.BackColor := IntToColor(i);
  end
  else if L.IsString(-1) then
  begin
    color := L.ToString(-1);
    if field = 'backcolor' then
      Engine.BackColor := StrToColor(color);
  end;
end;

function TLuaWindow.Getter(L: PLua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);

  if field = 'margin' then
  begin
    L.PushInteger(Engine.Margin);
    Result := 1;
  end
  else if field = 'backcolor' then
  begin
    L.PushInteger(ColorToInt(Engine.BackColor));
    Result := 1;
  end
  else if field = 'width' then
  begin
    L.PushInteger(Engine.Width);
    Result := 1;
  end
  else if field = 'height' then
  begin
    L.PushInteger(Engine.Height);
    Result := 1;
  end
  else if field = 'shaking' then
  begin
    L.PushBoolean(Engine.Shaking);
    Result := 1;
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
    //colors[0] is the first entry (the same order as the raw colors table)
    index := L.ToInteger(2);
    if (index >= 0) and (index < Length(Colors)) then
    begin
      c := ColorToInt(Colors[index].Color);
      L.PushInteger(c);
      Result := 1;
    end;
  end
  else
  begin
    field := L.ToString(2);
    if field = 'count' then
    begin
      L.PushInteger(Length(Colors));
      Result := 1;
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

function TLuaCanvas.Setter(L: PLua_State): Integer;
var
  i: Integer;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);

  if L.IsInteger(-1) then
  begin
    if field = 'color' then
    begin
      i := L.ToInteger(-1);
      FScript.AddQueueObject(TDrawSetColorObject.Create(Engine.Main.Canvas, IntToColor(i)));
      Result := 1;
    end
    else if field = 'alpha' then
    begin
      i := L.ToInteger(-1);
      FScript.AddQueueObject(TDrawSetAlphaObject.Create(Engine.Main.Canvas, i));
      Result := 1;
    end
    else if field = 'backcolor' then
    begin
      i := L.ToInteger(-1);
      //Thread-safe through the queue: the color is applied on the Engine
      //thread, so canvas.clear() clears with what the script asked for.
      FScript.AddQueueObject(TDrawSetBackColorObject.Create(Engine.Main.Canvas, IntToColor(i)));
      Result := 1;
    end;
  end;
end;

function TLuaCanvas.Getter(L: PLua_State): Integer;
var
  i: Integer;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);

  if field = 'color' then
  begin
    i := ColorToInt(Engine.Main.Canvas.PenColor);
    L.PushInteger(i);
    Result := 1;
  end
  else if field = 'backcolor' then
  begin
    i := ColorToInt(Engine.Main.Canvas.BackColor);
    L.PushInteger(i);
    Result := 1;
  end
  else if field = 'width' then
  begin
    L.PushInteger(Engine.Main.Canvas.Width);
    Result := 1;
  end
  else if field = 'height' then
  begin
    L.PushInteger(Engine.Main.Canvas.Height);
    Result := 1;
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
      FScript.AddQueueObject(TSetEffectObject.Create(Engine.Board, L.ToString(-1)));
  end
  else if field = 'value' then
  begin
    if L.IsNumber(-1) then
      FScript.AddQueueObject(TSetEffectValueObject.Create(Engine.Board, L.ToNumber(-1)));
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
      FScript.AddQueueObject(TSetEffectAreaObject.Create(Engine.Board, area));
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
    if Engine.Board <> nil then
      L.PushString(Engine.Board.GetEffectName)
    else
      L.PushString('none');
    Result := 1;
  end
  else if field = 'value' then
  begin
    if Engine.Board <> nil then
      L.PushNumber(Engine.Board.GetEffectValue)
    else
      L.PushNumber(1.0);
    Result := 1;
  end
  else if field = 'area' then
  begin
    if Engine.Board <> nil then
      area := Engine.Board.GetEffectArea
    else
    begin
      area := Default(TRectangle);
      area.width := Engine.Main.Canvas.Width;
      area.height := Engine.Main.Canvas.Height;
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
    FScript.AddQueueObject(TLoadShaderObject.Create(Engine.Board, L.ToString(1)));
  Result := 0;
end;

procedure TLuaScript.Init;
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
  Midi := TLuaMidi.Create(Self);
  SpectrumLua := TLuaSpectrum.Create(Self);
  Sprite := TLuaSprite.Create(Self);
  Sprites := TLuaSprites.Create(Self);
  Controls := TLuaControls.Create(Self);
  Output := TLuaOutput.Create(Self);
  Collision := TLuaCollision.Create(Self);
  Shader := TLuaShader.Create(Self);
  Files := TList.Create; //of TLuaFile, freed in Destroy

  //window
  Lua.State.Register('window', 'show', Window, Window.Window_func);
  //Screen shake: window.shake(ms [, power]) and the global shake(ms [, power])
  Lua.State.Register('window', 'shake', Window, Window.Shake_func);
  Lua.State.RegisterGlobal('shake', Window.Shake_func);
  Lua.State.Register('window', Window); //Should be last one for window

  //global functions
  Lua.State.RegisterGlobal('print', Console.PrintOut_func);
  Lua.State.RegisterGlobal('println', Console.PrintLnOut_func);

  //console
  Lua.State.Register('console', 'print', Console, Console.Print_func);
  Lua.State.Register('console', 'println', Console, Console.PrintLn_func);
  Lua.State.Register('console', 'show', Console, Console.Show_func);
  Lua.State.Register('console', 'read', Console, Console.Read_func);
  Lua.State.Register('console', Console); //Should be last one

  //canvas
  Lua.State.Register('canvas', 'clear', Canvas, Canvas.Clear_func);
  Lua.State.Register('canvas', 'text', Canvas, Canvas.Text_func);
  Lua.State.Register('canvas', 'circle', Canvas, Canvas.Circle_func);
  Lua.State.Register('canvas', 'rectangle', Canvas, Canvas.Rectangle_func);
  Lua.State.Register('canvas', 'line', Canvas, Canvas.Line_func);
  Lua.State.Register('canvas', 'point', Canvas, Canvas.Point_func);
  Lua.State.Register('canvas', Canvas); //Should be last one

  //shader (property-style: shader.effect, shader.value, shader.area)
  Lua.State.Register('shader', 'load', Shader, Shader.Load_func);
  Lua.State.Register('shader', Shader); //Should be last one

  //font
  Lua.State.Register('font', 'load', Font, Font.Load_func);
  Lua.State.Register('font', Font); //Should be last one

  //music
  Lua.State.Register('music', 'beep', Music, Music.Beep_func);
  Lua.State.Register('music', 'sound', Music, Music.Sound_func);
  Lua.State.Register('music', 'play', Music, Music.Play_func);
  Lua.State.Register('music', 'mml', Music, Music.MML_func);

  //radio (radio.play(url), radio.pause(), radio.resume(), radio.stop()
  // + getters: radio.title, radio.station, radio.state, radio.playing ...)
  Lua.State.Register('radio', 'play', Radio, Radio.Play_func);
  Lua.State.Register('radio', 'pause', Radio, Radio.Pause_func);
  Lua.State.Register('radio', 'resume', Radio, Radio.Resume_func);
  Lua.State.Register('radio', 'stop', Radio, Radio.Stop_func);
  Lua.State.Register('radio', Radio); //Should be last one

  //midi (midi.play(file), midi.pause(), midi.resume(), midi.stop()
  // + getters: midi.name, midi.state, midi.playing, midi.position,
  // midi.length, midi.tracks, midi.tempo, midi.error)
  Lua.State.Register('midi', 'play', Midi, Midi.Play_func);
  Lua.State.Register('midi', 'pause', Midi, Midi.Pause_func);
  Lua.State.Register('midi', 'resume', Midi, Midi.Resume_func);
  Lua.State.Register('midi', 'stop', Midi, Midi.Stop_func);
  Lua.State.Register('midi', Midi); //Should be last one

  //spectrum (spectrum.show(x, y, w, h), spectrum.hide()
  // + getters/setters: spectrum.bars, spectrum.active, spectrum.visible)
  Lua.State.Register('spectrum', 'show', SpectrumLua, SpectrumLua.Show_func);
  Lua.State.Register('spectrum', 'hide', SpectrumLua, SpectrumLua.Hide_func);
  Lua.State.Register('spectrum', SpectrumLua); //Should be last one

  //input & timing (global functions)
  Lua.State.RegisterGlobal('iskeypressed', IsKeyPressed_func);
  Lua.State.RegisterGlobal('iskeydown', IsKeyDown_func);
  Lua.State.RegisterGlobal('mousex', MouseX_func);
  Lua.State.RegisterGlobal('mousey', MouseY_func);
  Lua.State.RegisterGlobal('ismousepressed', IsMouseButtonPressed_func);
  Lua.State.RegisterGlobal('frametime', FrameTime_func);
  Lua.State.RegisterGlobal('time', TotalTime_func);
  Lua.State.RegisterGlobal('rand', RandomValue_func);
  Lua.State.RegisterGlobal('screenshot', Screenshot_func);
  Lua.State.RegisterGlobal('exit', Exit_func);
  Lua.State.RegisterGlobal('clock', Clock_func);
  Lua.State.RegisterGlobal('time', Time_func);

  //file access: openfile(name [, mode]) -> file handle, or nil + error
  //This is the only way a script reaches the file system; the io/os libraries
  //are removed in TLua.Init, so nothing here can be bypassed from Lua.
  Lua.State.RegisterGlobal('openfile', OpenFile_func);

  //require(name) is the stock loader's replacement: the Lua one is removed in
  //TLua.Init along with dofile/loadfile/package, and this one reads only from
  //the workspace and the app folder. The table that remembers what already ran
  //lives in the registry, since package.loaded is no longer there to hold it.
  Lua.State.RegisterGlobal('require', Require_func);
  lua_newtable(Lua.State); //[cache]
  lua_setfield(Lua.State, LUA_REGISTRYINDEX, PUTF8Char(cRequireCache));

  // Sprite system: Sprites.new creates a sprite, Sprites("name") finds by name
  Lua.State.RegisterTable('Sprites');
  Lua.State.Register('Sprites', 'new', Self, Sprites.New_func);
  Lua.State.Register('Sprites', 'find', Self, Sprites.Find_func);
  // Set __call in the Sprites metatable so Sprites("name") works; Lua passes the table as arg 1
  Lua.State.Register('Sprites', '__call', Self, Sprites.Call_func, True);

  //controls (generic control table; 'buttons' is a legacy alias to the same
  //object so old scripts keep working). Every function here takes the control as
  //its first argument and each of them also has a field spelling on the control
  //table itself (btn.width = 200 for controls.width(btn, 200)), except for the
  //actions - show, hide, move, position, focus, additem, item, clear, load -
  //which stay methods on the control table and take a colon call.
  Lua.State.RegisterTable('controls');
  Lua.State.Register('controls', 'new', Controls, Controls.New_func);
  Lua.State.Register('controls', 'position', Controls, Controls.Position_func);
  Lua.State.Register('controls', 'move', Controls, Controls.Move_func);
  Lua.State.Register('controls', 'width', Controls, Controls.Width_func);
  Lua.State.Register('controls', 'height', Controls, Controls.Height_func);
  Lua.State.Register('controls', 'visible', Controls, Controls.Visible_func);
  Lua.State.Register('controls', 'show', Controls, Controls.Show_func);
  Lua.State.Register('controls', 'hide', Controls, Controls.Hide_func);
  Lua.State.Register('controls', 'hover', Controls, Controls.Hover_func);
  Lua.State.Register('controls', 'down', Controls, Controls.Down_func);
  Lua.State.Register('controls', 'clicked', Controls, Controls.Clicked_func);
  Lua.State.Register('controls', 'focused', Controls, Controls.Focused_func);
  Lua.State.Register('controls', 'focus', Controls, Controls.Focus_func);
  Lua.State.Register('controls', 'border', Controls, Controls.Border_func);
  Lua.State.Register('controls', 'backcolor', Controls, Controls.BackColor_func);
  Lua.State.Register('controls', 'name', Controls, Controls.Name_func);
  //caption/text: both spellings work on a caption control, and text also
  //covers an edit; btn.caption and btn.text are the field spellings of both
  Lua.State.Register('controls', 'caption', Controls, Controls.Caption_func);
  Lua.State.Register('controls', 'text', Controls, Controls.Text_func);
  Lua.State.Register('controls', 'checked', Controls, Controls.Checked_func);
  Lua.State.Register('controls', 'align', Controls, Controls.Align_func);
  Lua.State.Register('controls', 'parent', Controls, Controls.Parent_func);
  //listbox: items/viewcount/itemindex are fields on the control table
  //(lst.items), the three actions additem/item/clear are also methods there
  //(lst:additem("a")); the table form takes the handle first
  //(controls.additem(lst, "a"))
  Lua.State.Register('controls', 'items', Controls, Controls.Items_func);
  Lua.State.Register('controls', 'item', Controls, Controls.Item_func);
  Lua.State.Register('controls', 'additem', Controls, Controls.AddItem_func);
  Lua.State.Register('controls', 'clear', Controls, Controls.Clear_func);
  Lua.State.Register('controls', 'viewcount', Controls, Controls.ViewCount_func);
  Lua.State.Register('controls', 'itemindex', Controls, Controls.ItemIndex_func);
  //image control: load the texture an image shows (img.load(...) is the same)
  Lua.State.Register('controls', 'load', Controls, Controls.LoadImage_func);
  Lua.State.Register('controls', Controls); //should be last one

  //output (catches print/println/log)
  Lua.State.RegisterTable('output');
  Lua.State.Register('output', 'show', Output, Output.Show_func);
  Lua.State.Register('output', 'hide', Output, Output.Hide_func);
  Lua.State.Register('output', 'clear', Output, Output.Clear_func);
  Lua.State.Register('output', Output); //should be last one
  // Collision system: collision.pump() drains events and fires sprite.onCollide(other, state)
  Lua.State.Register('collision', 'pump', Collision, Collision.Pump_func);
  Lua.State.Register('collision', Collision); // should be last — wires getter/setter metamethods

  Lua.State.BeginTable;
  for i := 0 to Length(Colors.Colors) - 1 do
    Lua.State.Register(Colors.Colors[i].Name, ColorToInt(Colors.Colors[i].Color));
  Lua.State.EndTableGlobal('colors', Colors);

  //Attach a metatable to the global environment so an unresolved global name
  //resolves to a sprite or control by name (e.g. richard.move(...) when
  //"richard" is a sprite). The globals table is fetched with
  //lua_pushglobaltable (LUA_RIDX_GLOBALS) for Lua 5.5 compatibility;
  //__index/__newindex use raw access (no recursion).
  //
  //Metamethods MUST be registered with RegisterMeta: the plain
  //Register(Name, Method) injects the table as an extra first argument, which
  //is what "obj:method()" needs, but it shifts the metamethod arguments to
  //(table, table, key[, value]) and __newindex would then leave two values on
  //the Lua stack (corrupting it).
  lua_pushglobaltable(Lua.State); //[globals]
  Lua.State.NewTable; //[globals, meta]
  Lua.State.RegisterMeta('__index', __global_getter);
  Lua.State.RegisterMeta('__newindex', __global_setter);
  Lua.State.SetMetaTable(-2); //globals.metatable = meta -> [globals]
  Lua.State.Pop(1); //[]
end;

destructor TLuaScript.Destroy;
var
  aFile: TLuaFile;
begin
  // Lua holds light-userdata/method pointers to these facade objects, so close
  // the state before releasing them.
  Lua.Close;
  //Any file still open is closed and released here; the closures that referenced
  //it died with the state, so nothing can call back into it afterwards.
  if Files <> nil then
  begin
    while Files.Count > 0 do
    begin
      aFile := TLuaFile(Files[Files.Count - 1]);
      Files.Delete(Files.Count - 1);
      aFile.Free;
    end;
    FreeAndNil(Files);
  end;
  FreeAndNil(Shader);
  FreeAndNil(Collision);
  FreeAndNil(Output);
  FreeAndNil(Controls);
  FreeAndNil(Sprites);
  FreeAndNil(Sprite);
  FreeAndNil(SpectrumLua);
  FreeAndNil(Midi);
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
  //reads this after the run and the Engine loop surfaces it as the exit code.
  FLastError := '';
  //WriteLn('Run Script');
  //Sleep(1000);
  if not Lua.State.RunString(Source.Text, Msg) then
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
// Engine script), so assignments like "x = 5" stay alive for later lines. Any
// return values are echoed to the terminal (REPL style); syntax/runtime errors
// are reported through AOutput.
function TLuaScript.RunLine(const ALine: string; out AOutput: string): Boolean;
var
  n: Integer;
  p: PUTF8Char;
  S, Msg, AChunk: UTF8String;

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
        Log.WriteLn(S);
      //Discard the converted return values from the persistent Lua stack.
      lua_pop(Lua.State, n);
    end
    else
    begin
      AOutput := Lua.State.ToString(-1);
      Lua.State.Pop(1);
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
  FScript.AddQueueObject(TClearObject.Create(Engine.Main.Canvas));
  Result := 0;
end;

function TLuaWindow.Window_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  w, h: integer;
begin
  c := L.ArgsCount;
  w := ScreenWidth;
  h := ScreenHeight;
  if c > 0 then
    w := round(L.ToNumber(1));
  if c > 1 then
    h := round(L.ToNumber(2));
  FScript.RunQueueObject(TWindowObject.Create(w, h));
  Result := 0;
end;

// window.shake(ms [, power]) / shake(ms [, power])
// Jolt the world like an accident or an error. Works as a plain function too,
// so the first argument is always the time in milliseconds.
function TLuaWindow.Shake_func(L: Plua_State): integer; cdecl;
var
  ms, power: Integer;
begin
  ms := 0;
  power := 0;
  if L.ArgsCount > 0 then
    ms := Round(L.ToNumber(1));
  if L.ArgsCount > 1 then
    power := Round(L.ToNumber(2));
  if Engine <> nil then
    //Thread safe: the engine takes the request from the script thread and
    //jitters the world camera on the next frames.
    Engine.Shake(ms, power);
  Result := 0;
end;

function TLuaConsole.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
begin
  c := L.ArgsCount;
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
  FScript.AddQueueObject(TDrawTextObject.Create(Engine.Main.Canvas, x, y, s));
  Result := 0;
end;

function TLuaCanvas.Circle_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, r: integer;
  f: boolean;
begin
  f := False;
  c := L.ArgsCount;
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  r := round(L.ToNumber(3));
  if c >= 4 then
    f := L.ToBoolean(4);
  FScript.AddQueueObject(TDrawCircleObject.Create(Engine.Main.Canvas, x, y, r, f));
  Result := 0;
end;

function TLuaCanvas.Rectangle_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
  f: boolean;
begin
  f := False;
  c := L.ArgsCount;
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  w := round(L.ToNumber(3));
  h := round(L.ToNumber(4));
  if c >= 4 then
    f := L.ToBoolean(5);
  FScript.AddQueueObject(TDrawRectangleObject.Create(Engine.Main.Canvas, x, y, w, h, f));
  Result := 0;
end;

function TLuaCanvas.Line_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x1, y1, x2, y2: integer;
begin
  c := L.ArgsCount;
  x1 := round(L.ToNumber(1));
  y1 := round(L.ToNumber(2));
  if c = 4 then
  begin
    x2 := round(L.ToNumber(3));
    y2 := round(L.ToNumber(4));
    FScript.AddQueueObject(TDrawLineObject.Create(Engine.Main.Canvas, x1, y1, x2, y2));
  end
  else
    FScript.AddQueueObject(TDrawLineToObject.Create(Engine.Main.Canvas, x1, y1));
  Result := 0;
end;

function TLuaCanvas.Point_func(L: Plua_State): integer; cdecl;
var
  x, y: integer;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  FScript.AddQueueObject(TDrawPointObject.Create(Engine.Main.Canvas, x, y));
  Result := 0;
end;

function TLuaConsole.Print_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(Engine.Main.Canvas, s, False));
  Result := 0;
end;

function TLuaConsole.PrintLn_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(Engine.Main.Canvas, s, True));
  Result := 0;
end;

function TLuaConsole.PrintOut_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(Engine.Main.Canvas, s, False));
  FScript.AddQueueObject(TOutputPrintObject.Create(Engine.Main.Canvas, s, False));
  Result := 0;
end;

function TLuaConsole.PrintLnOut_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(Engine.Main.Canvas, s, True));
  FScript.AddQueueObject(TOutputPrintObject.Create(Engine.Main.Canvas, s, True));
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
    Engine.Output.Visible := L.ToBoolean(-1)
  else if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'height' then
      Engine.Output.Height := i
    else if field = 'width' then
      Engine.Output.Width := i
    else if (field = 'left') or (field = 'x') then
    begin
      r := Engine.Output.BoundsRect;
      Engine.Output.BoundsRect := Rect(i, r.Top, i + r.Width, r.Bottom);
    end
    else if (field = 'top') or (field = 'y') then
    begin
      r := Engine.Output.BoundsRect;
      Engine.Output.BoundsRect := Rect(r.Left, i, r.Right, i + r.Height);
    end
    else if field = 'margin' then
      Engine.Output.Margin := i
    else if field = 'maxlines' then
      Engine.Output.MaxLines := i
    else if field = 'border' then
      //* 0=none, 1=thin, 2=thick, 3=sizable
      case i of
        1: Engine.Output.Border := brdThin;
        2: Engine.Output.Border := brdThick;
        3: Engine.Output.Border := brdSizable;
      else
        Engine.Output.Border := brdNone;
      end;
  end
  else if L.IsString(-1) then
  begin
    if field = 'color' then
      Engine.Output.Color := StrToColor(L.ToString(-1))
    else if field = 'backColor' then
      Engine.Output.BackColor := StrToColor(L.ToString(-1));
  end;
end;

function TLuaOutput.Getter(L: Plua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'visible' then
  begin
    lua_pushboolean(L, Engine.Output.Visible);
    Result := 1;
  end
  else if field = 'height' then
  begin
    lua_pushinteger(L, Engine.Output.Height);
    Result := 1;
  end
  else if field = 'width' then
  begin
    lua_pushinteger(L, Engine.Output.Width);
    Result := 1;
  end
  else if (field = 'left') or (field = 'x') then
  begin
    lua_pushinteger(L, Engine.Output.BoundsRect.Left);
    Result := 1;
  end
  else if (field = 'top') or (field = 'y') then
  begin
    lua_pushinteger(L, Engine.Output.BoundsRect.Top);
    Result := 1;
  end
  else if field = 'border' then
  begin
    case Engine.Output.Border of
      brdThin: lua_pushinteger(L, 1);
      brdThick: lua_pushinteger(L, 2);
      brdSizable: lua_pushinteger(L, 3);
    else
      lua_pushinteger(L, 0);
    end;
    Result := 1;
  end
  else if field = 'lines' then
  begin
    lua_pushinteger(L, Engine.Output.LineCount);
    Result := 1;
  end
  else if field = 'maxlines' then
  begin
    lua_pushinteger(L, Engine.Output.MaxLines);
    Result := 1;
  end
  else if field = 'margin' then
  begin
    lua_pushinteger(L, Engine.Output.Margin);
    Result := 1;
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
  c := L.ArgsCount;
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
  Engine.Output.Clear;
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
  s := Res.GuessFileName(s, Res.WorkPath);
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
  c := L.ArgsCount;
  SetLength(Song, c);
  for i := 0 to c - 1 do
  begin
    s := L.ToString(i + 1);
    Song[i] := s;
  end;
  //Audio objects and raylib audio calls stay on the Engine thread. Playback is
  //advanced incrementally by TTyroMain.Update, so this does not block drawing.
  FScript.AddQueueObject(TPlayMMLObject.Create(Song));
  Result := 0;
end;

{ TLuaRadio }

function TLuaRadio.Getter(L: Plua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'title' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Title)));
    Result := 1;
  end
  else if field = 'station' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Station)));
    Result := 1;
  end
  else if field = 'genre' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Genre)));
    Result := 1;
  end
  else if field = 'bitrate' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Bitrate)));
    Result := 1;
  end
  else if field = 'url' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.URL)));
    Result := 1;
  end
  else if field = 'state' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.StateString)));
    Result := 1;
  end
  else if field = 'error' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Error)));
    Result := 1;
  end
  else if field = 'playing' then
  begin
    lua_pushboolean(L, RadioPlayer.Playing);
    Result := 1;
  end
  else if field = 'buffered' then
  begin
    lua_pushinteger(L, RadioPlayer.Buffered);
    Result := 1;
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

{ TLuaMidi }

function TLuaMidi.Getter(L: Plua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if MidiPlayer = nil then
    Exit;
  if field = 'name' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(MidiPlayer.Name)));
    Result := 1;
  end
  else if field = 'file' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(MidiPlayer.FileName)));
    Result := 1;
  end
  else if field = 'state' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(MidiPlayer.StateString)));
    Result := 1;
  end
  else if field = 'error' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(MidiPlayer.Error)));
    Result := 1;
  end
  else if field = 'playing' then
  begin
    lua_pushboolean(L, MidiPlayer.Playing);
    Result := 1;
  end
  else if field = 'position' then
  begin
    lua_pushnumber(L, MidiPlayer.Position);
    Result := 1;
  end
  else if field = 'length' then
  begin
    lua_pushnumber(L, MidiPlayer.Length);
    Result := 1;
  end
  else if field = 'tracks' then
  begin
    lua_pushinteger(L, MidiPlayer.Tracks);
    Result := 1;
  end
  else if field = 'tempo' then
  begin
    lua_pushinteger(L, MidiPlayer.Tempo);
    Result := 1;
  end;
end;

//Playback is the only thing a script can ask for: a MIDI player renders a file
//it was given, it does not take live note commands the way music.beep does.
function TLuaMidi.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaMidi.Play_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := L.ToString(1);
  s := Res.GuessFileName(s, Res.WorkPath);
  FScript.AddQueueObject(TMidiPlayObject.Create(maPlay, s));
  Result := 0;
end;

function TLuaMidi.Pause_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TMidiPlayObject.Create(maPause));
  Result := 0;
end;

function TLuaMidi.Resume_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TMidiPlayObject.Create(maResume));
  Result := 0;
end;

function TLuaMidi.Stop_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TMidiPlayObject.Create(maStop));
  Result := 0;
end;

{ TMidiPlayObject }

constructor TMidiPlayObject.Create(AAction: TMidiAction);
begin
  inherited Create;
  FAction := AAction;
end;

constructor TMidiPlayObject.Create(AAction: TMidiAction; const AFileName: string);
begin
  Create(AAction);
  FFileName := AFileName;
end;

procedure TMidiPlayObject.DoExecute;
begin
  if MidiPlayer = nil then
    Exit;
  case FAction of
    maPlay: MidiPlayer.Play(FFileName);
    maPause: MidiPlayer.Pause;
    maResume: MidiPlayer.Resume;
    maStop: MidiPlayer.Stop;
  end;
end;

{ TLuaSpectrum }

constructor TLuaSpectrum.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
end;

function TLuaSpectrum.Getter(L: Plua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'active' then
  begin
    lua_pushboolean(L, Spectrum.Active);
    Result := 1;
  end
  else if field = 'bars' then
  begin
    lua_pushinteger(L, Spectrum.Bars);
    Result := 1;
  end
  else if field = 'visible' then
  begin
    lua_pushboolean(L, Spectrum.Visible);
    Result := 1;
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
  c := L.ArgsCount;
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

function TLuaScript.Exit_func(L: Plua_State): integer; cdecl;
begin
  //Stop the script as soon as the current call finishes
  Stop;
  Result := 0;
end;

function TLuaScript.Clock_func(L: Plua_State): integer; cdecl;
begin
  L.PushNumber(GetTickCount / 1000.0);
  Result := 1;
end;

function TLuaScript.Time_func(L: Plua_State): integer; cdecl;
var
  t: TDateTime;
  secs: Int64;
begin
  t := Now;
  secs := Trunc(t * 86400);
  L.PushInteger(secs);
  Result := 1;
end;

function TLuaScript.Screenshot_func(L: Plua_State): integer; cdecl;
begin
  if L.ArgsCount > 0 then
    // TakeScreenshot must run on the Engine thread after the frame is
    // presented, so queue the request and let the engine capture it.
    Engine.QueueScreenshot(L.ToString(1));
  Result := 0;
end;

{ TLuaFile }

// Pushes AData onto the Lua stack with its exact byte count, so binary content
// that holds #0 bytes survives the trip. PushString goes through a
// null-terminated PChar and would cut the string at the first #0.
procedure PushBytes(L: PLua_State; const AData: AnsiString);
begin
  if Length(AData) > 0 then
    lua_pushlstring(L, PUTF8Char(PAnsiChar(@AData[1])), Length(AData))
  else
    lua_pushstring(L, PUTF8Char(''));
end;

// Drops the ACount values Lua passed in as arguments, so a function that built
// its own table on top of them leaves exactly that table as the result. lua_pop
// would take from the top and eat the new table instead, so the arguments are
// removed one by one from the bottom, where lua_remove shifts them down.
procedure PopArgs(L: PLua_State; ACount: Integer);
begin
  while ACount > 0 do
  begin
    L.Remove(1);
    Dec(ACount);
  end;
end;

function TLuaFile.ModeFlag(const AFlag: Char): Boolean;
begin
  Result := Pos(AFlag, LowerCase(FMode)) > 0;
end;

//Index of the first value the script really passed. Register() injects the
//handle table as argument 1, so "f.write('x')" has its value at index 2 while
//the natural colon spelling "f:write('x')" repeats the table at index 2 and
//starts at 3. Comparing argument 2 with the injected table tells the two apart,
//so both spellings read the same values.
function TLuaFile.FirstArgIndex(L: PLua_State): Integer;
begin
  if lua_rawequal(L, 1, 2) then
    Result := 3
  else
    Result := 2;
end;

constructor TLuaFile.Create(AScript: TLuaScript);
begin
  inherited Create(AScript);
  FIsOpen := False;
end;

destructor TLuaFile.Destroy;
begin
  FreeAndNil(FLines);
  if FStream <> nil then
  begin
    FreeAndNil(FStream); //also flushes whatever the script left buffered
    FIsOpen := False;
  end;
  inherited Destroy;
end;

//Opens AFileName with the mode letters io.open understands: 'r' (default),
//'w' (truncate), 'a' (append), 'x' (create, never overwrite), an optional '+'
//(also readable) and an optional 'b'. Everything runs through TFileStream; 'b'
//only switches off the newline translation that the text modes apply. AFileName
//has already passed ResolveAndValidatePath, so it is known to be inside the
//workspace.
function TLuaFile.Open(const AFileName, AMode: string): Boolean;
var
  Flags: Integer;
begin
  Result := False;
  FError := '';
  FFileName := AFileName;
  FMode := AMode;
  if AFileName = '' then
  begin
    FError := 'no file name';
    Exit;
  end;
  FIsText := not ModeFlag('b');

  if ModeFlag('x') then
  begin
    //exclusive create: a file that is already there is never touched
    if SysUtils.FileExists(AFileName) then
    begin
      FError := 'file already exists';
      Exit;
    end;
    if ModeFlag('+') then
      Flags := fmOpenReadWrite or fmCreate or fmShareDenyWrite
    else
      Flags := fmCreate or fmShareDenyWrite; //create or truncate, still readable
  end
  else if ModeFlag('w') then
    Flags := fmCreate or fmShareDenyWrite //create or truncate, still readable
  else if ModeFlag('a') and ModeFlag('+') then
    Flags := fmOpenReadWrite or fmShareDenyWrite
  else if ModeFlag('a') then
  begin
    //append: update an existing file, create it when it is missing
    if SysUtils.FileExists(AFileName) then
      Flags := fmOpenReadWrite or fmShareDenyWrite
    else
      Flags := fmCreate or fmShareDenyWrite
  end
  else if ModeFlag('+') then
    //read and write, without truncating
    Flags := fmOpenReadWrite or fmShareDenyWrite
  else
    Flags := fmOpenRead or fmShareDenyWrite;

  try
    FStream := TFileStream.Create(AFileName, Flags);
  except
    on E: Exception do
    begin
      FStream := nil;
      FError := E.Message;
      Exit;
    end;
  end;
  if ModeFlag('a') then
  begin
    try
      FStream.Seek(0, soEnd); //append always writes at the end
    except
      on E: Exception do
      begin
        FError := E.Message;
        FreeAndNil(FStream);
        Exit;
      end;
    end;
  end;
  FIsOpen := True;
  Result := True;
end;

//Reads one token ("n") or one line ("l" without the newline, "L" with it) and
//pushes exactly one value, following the io library's text-mode conventions: at
//the end of the stream it pushes nil, and a number it cannot parse pushes nil
//plus a message (and returns False so the caller returns two values).
function TLuaFile.ReadTextFormat(L: PLua_State; const AFormat: Char): Boolean;
var
  Buf: AnsiString;
  Num: Double;
  Whole: Int64;
  c: Char;
  StartPos: Int64;
begin
  Result := True;
  Buf := '';
  StartPos := FStream.Position;

  if AFormat = 'n' then
  begin
    //"n" is a token read, not a line read: drop the blanks first, then take
    //everything up to the next one
    while FStream.Position < FStream.Size do
    begin
      c := #0;
      FStream.ReadBuffer(c, 1);
      if c > ' ' then
      begin
        Buf := c;
        Break;
      end;
    end;
    if Buf = '' then
    begin
      L.PushNil; //nothing but blanks left, like io
      Exit;
    end;
    while FStream.Position < FStream.Size do
    begin
      c := #0;
      FStream.ReadBuffer(c, 1);
      if c <= ' ' then
        Break;
      Buf := Buf + c;
    end;
  end
  else
  begin
    //"l"/"L" are line reads: stop at the newline, or at the end of the file
    while FStream.Position < FStream.Size do
    begin
      c := #0;
      FStream.ReadBuffer(c, 1);
      if c = #10 then
        Break;
      Buf := Buf + c;
    end;
    if FStream.Position = StartPos then
    begin
      L.PushNil; //end of the file, like io
      Exit;
    end;
    //a trailing #13 belongs to the CRLF pair, never to the returned text
    if (Buf <> '') and (Buf[Length(Buf)] = #13) then
      Delete(Buf, Length(Buf), 1);
    if AFormat = 'L' then
      Buf := Buf + #10;
  end;

  if AFormat = 'n' then
  begin
    //A whole number comes back as an integer, the way io.read("n") hands it over;
    //the spellings TryStrToInt64 rejects (a fraction, an exponent) go on as a float
    if TryStrToInt64(Buf, Whole) then
      L.PushInteger(Whole)
    else if TryStrToFloat(Buf, Num) then
      L.PushNumber(Num)
    else
    begin
      L.PushNil;
      L.PushString('malformed number near ''' + Buf + '''');
      Result := False;
    end;
  end
  else
    PushBytes(L, Buf);
end;

//file:read([what [, ...]]) with the io.read formats: "a"/"*a" for everything
//left, "l"/"*l" for one line, "L"/"*L" for a line keeping its newline, "n" for
//the next number (io style) and a plain number for that many bytes. Formats
//only apply to text streams; on a binary stream they read raw bytes.
function TLuaFile.Read_func(L: PLua_State): integer; cdecl;
var
  Format: AnsiString;
  Fmt: Char;
  Count: Int64;
  Buf: AnsiString;
  idx: Integer;
begin
  if not FIsOpen then
  begin
    L.PushNil;
    L.PushString('file is closed');
    Exit(2);
  end;
  idx := FirstArgIndex(L);
  if L.ArgsCount < idx then
    Format := 'l'
  else
    Format := AnsiString(L.ToString(idx));
  if (Format <> '') and (Format[1] = '*') then
    Delete(Format, 1, 1);
  if Format = '' then
    Format := 'l';
  //The letter keeps the case the script wrote, because that is what tells "l"
  //from "L"; the other letters are accepted in either case.
  Fmt := Format[1];

  if FIsText and (Fmt in ['l', 'L', 'n']) then
  begin
    //the text formats push one value, or nil plus a message on a bad number
    if ReadTextFormat(L, Fmt) then
      Exit(1)
    else
      Exit(2);
  end;

  if (Fmt = 'a') or (Fmt = 'A') then
  begin
    Buf := '';
    Count := FStream.Size - FStream.Position;
    SetLength(Buf, Count);
    if Count > 0 then
      FStream.ReadBuffer(Buf[1], Count);
    PushBytes(L, Buf);
    Exit(1);
  end;

  if not TryStrToInt64(Format, Count) then
    Count := 0;
  if Count <= 0 then
  begin
    L.PushNil;
    L.PushString('invalid format');
    Exit(2);
  end;
  if Count > FStream.Size - FStream.Position then
    Count := FStream.Size - FStream.Position;
  Buf := '';
  SetLength(Buf, Count);
  if Count > 0 then
    FStream.ReadBuffer(Buf[1], Count);
  PushBytes(L, Buf);
  Result := 1;
end;

//file:write(...) -> the file handle, so calls chain: f:write(..):close().
//A leading number is the byte count of the string that follows it, matching
//io.write. The values start at FirstArgIndex, so "f:write(x)" and
//"f.write(f, x)" behave the same.
function TLuaFile.Write_func(L: PLua_State): integer; cdecl;
var
  i, first: Integer;
  Buf, OutBuf: AnsiString;
begin
  if not FIsOpen then
  begin
    L.PushNil;
    L.PushString('file is closed');
    Exit(2);
  end;
  Buf := '';
  first := FirstArgIndex(L);
  i := first;
  while i <= L.ArgsCount do
  begin
    //"f:write(#s, s)" writes only the first n bytes of s. Only a real number
    //is a byte count, like the stock io.write: L.IsNumber also accepts a
    //string that converts, so f:write(tostring(7) .. "\n") would be read as
    //"write 7 bytes" and silently write nothing at all
    if (i = first) and (lua_type(L, i) = LUA_TNUMBER) and (L.ToInteger(i) < L.ArgsCount) then
    begin
      Inc(i); //now on the string that follows the count
      Buf := Buf + Copy(AnsiString(L.ToString(i)), 1, Integer(L.ToInteger(first)));
      Inc(i);
    end
    else
    begin
      Buf := Buf + AnsiString(L.ToString(i));
      Inc(i);
    end;
  end;

  if FIsText and (Buf <> '') then
  begin
    //a text stream writes CRLF: put a #13 in front of every #10 that does not
    //already have one, so a script writing "\r\n" does not get "\r\r\n"
    OutBuf := '';
    for i := 1 to Length(Buf) do
    begin
      if (Buf[i] = #10) and ((i = 1) or (Buf[i - 1] <> #13)) then
        OutBuf := OutBuf + #13;
      OutBuf := OutBuf + Buf[i];
    end;
    Buf := OutBuf;
  end;

  if Length(Buf) > 0 then
    //Writing past the end extends the file by itself, and growing it through
    //TStream.Size would also move the file pointer, which is not what a write
    //in the middle of the stream should do
    FStream.WriteBuffer(Buf[1], Length(Buf));
  L.PushValue(1); //the handle table, for chaining
  Result := 1;
end;

function TLuaFile.Flush_func(L: PLua_State): integer; cdecl;
begin
  if FIsOpen then
    FStream.Flush;
  L.PushValue(1); //the handle table, for chaining
  Result := 1;
end;

function TLuaFile.Close_func(L: PLua_State): integer; cdecl;
begin
  if FIsOpen then
  begin
    FreeAndNil(FStream); //also flushes the buffered tail
    FIsOpen := False;
  end;
  //true, not the handle: closing is the end of the road, and a script that
  //chains on it should not end up writing to a handle that is already gone
  L.PushBoolean(True);
  Result := 1;
end;

//file:lines() -> an iterator over the lines that are left, so "for line in
//f:lines()" works. What remains is read once into FLines and handed out one line
//per step; a second call replaces the pending iterator, like the io library.
function TLuaFile.Lines_func(L: PLua_State): integer; cdecl;
var
  Buf: AnsiString;
  LineStr: AnsiString;
  Start, I, Len, idx: Integer;
  base: Integer;
begin
  if not FIsOpen then
  begin
    L.PushNil;
    L.PushString('file is closed');
    Exit(2);
  end;
  if FLines = nil then
    FLines := TStringList.Create
  else
    FLines.Clear;
  FLinesPos := 0;

  //Take what is left in one go and cut it into lines by hand; TextString would
  //stop at a #0 byte and lose the rest of a binary file.
  Buf := '';
  Len := FStream.Size - FStream.Position;
  SetLength(Buf, Len);
  if Len > 0 then
    FStream.ReadBuffer(Buf[1], Len);

  Start := 1;
  for I := 1 to Len do
    if Buf[I] = #10 then
    begin
      LineStr := Copy(Buf, Start, I - Start - 1);
      if (LineStr <> '') and (LineStr[Length(LineStr)] = #13) then
        Delete(LineStr, Length(LineStr), 1);
      FLines.Add(LineStr);
      Start := I + 1;
    end;
  //a last line without a trailing newline still counts
  if Start <= Len then
  begin
    LineStr := Copy(Buf, Start, Len - Start + 1);
    if (LineStr <> '') and (LineStr[Length(LineStr)] = #13) then
      Delete(LineStr, Length(LineStr), 1);
    FLines.Add(LineStr);
  end;

  //Return the same three values the io library does: the step function, the
  //state handed back on every step, and the control variable. Lua takes the top
  //Result values as they stand, so they go on the stack function first and
  //control last.
  base := L.ArgsCount; //the arguments are still on the stack
  L.BeginTable; //[.., iterator]
  L.Register('linesnext', LinesNext_func);
  idx := base + 1; //where the iterator table sits
  L.PushValue(idx); //[.., iterator, iterator] a copy to read the field from
  L.GetField(L.ArgsCount, 'linesnext'); //[.., iterator, iterator, step]
  L.Remove(L.ArgsCount - 1); //[.., iterator, step] drop the copy
  lua_insert(L, idx); //[.., step, iterator] the step function has to come first
  L.PushInteger(0); //[.., step, iterator, 0] the control variable
  PopArgs(L, base); //[step, iterator, 0]
  Result := 3;
end;

//One step of the f:lines() iterator: it hands out the next queued line and the
//count of lines done so far, and yields nil once the queue is drained. The count
//is only returned for the callers that drive the iterator themselves; Pluto's
//generic for hands the previous line back instead of it, which is why the
//position is kept in FLinesPos and not taken from an argument.
function TLuaFile.LinesNext_func(L: PLua_State): integer; cdecl;
begin
  if (FLines = nil) or (FLinesPos < 0) or (FLinesPos >= FLines.Count) then
  begin
    L.PushNil;
    Exit(1);
  end;
  L.PushString(FLines[FLinesPos]);
  Inc(FLinesPos);
  L.PushInteger(FLinesPos);
  Result := 2;
end;

//openfile(name [, mode]) -> a file handle, or nil plus an error message.
//This is the only file access a script has: TLua.Init removes the io and os
//libraries from the globals, so nothing here can be bypassed from Lua code.
function TLuaScript.OpenFile_func(L: Plua_State): integer; cdecl;
var
  aName, aMode, Resolved: string;
  aFile: TLuaFile;
  base: Integer;
begin
  if L.ArgsCount < 1 then
  begin
    L.PushNil;
    L.PushString('openfile: missing file name');
    Exit(2);
  end;
  aName := L.ToString(1);
  aMode := 'r';
  if (L.ArgsCount >= 2) and (L.ToString(2) <> '') then
    aMode := L.ToString(2);

  //Expand the name first (relative, "." / ".." and drive-relative forms) and
  //keep it only when it lands inside the workspace. The check runs on the
  //expanded path, so a name that walks out of the workspace is refused before
  //any handle exists.
  Resolved := ResolveAndValidatePath(aName, Res.WorkPath);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('openfile: ''' + aName + ''' resolves outside the workspace');
    Exit(2);
  end;

  aFile := TLuaFile.Create(Self);
  if not aFile.Open(Resolved, aMode) then
  begin
    L.PushNil;
    L.PushString('openfile: cannot open ''' + aName + ''' (' + aFile.Error + ')');
    aFile.Free;
    Exit(2);
  end;
  //the script owns the handle for as long as its state lives; Destroy frees them
  Files.Add(aFile);

  base := L.ArgsCount; //the arguments are still on the stack
  Lua.State.BeginTable; //[file]
  //Register injects the table as argument 1, so every method below reads its own
  //arguments from index 2, the way the rest of the Tyro bindings do.
  Lua.State.Register('read', aFile.Read_func);
  Lua.State.Register('write', aFile.Write_func);
  Lua.State.Register('flush', aFile.Flush_func);
  Lua.State.Register('close', aFile.Close_func);
  Lua.State.Register('lines', aFile.Lines_func);
  PopArgs(L, base); //leave the handle table as the only value
  Result := 1;
end;

//Turns a module name into the file it stands for, without the ending: "foo.bar"
//is "foo\bar", the way the stock require reads it. A name that already carries an
//ending keeps its folder part and loses the ".tyro" or ".lua", so both spellings
//find the same file. Nothing is opened here, the name only becomes a path; the
//containment check decides whether the path may be used at all.
function ModuleStem(const AName: string): string;
var
  s: string;
  i: Integer;
  c: Char;
begin
  s := AName;
  if LowerCase(ExtractFileExt(s)) = '.tyro' then
    s := Copy(s, 1, Length(s) - 5)
  else if LowerCase(ExtractFileExt(s)) = '.lua' then
    s := Copy(s, 1, Length(s) - 4);
  //dots that are part of ".." must stay; other dots become the path separator
  Result := '';
  i := 1;
  while i <= Length(s) do
  begin
    c := s[i];
    if (c = '.') and (i + 1 <= Length(s)) and (s[i + 1] = '.') then
    begin
      //double dot: keep both as dots (and let ExpandFileName collapse them)
      Result := Result + '..';
      Inc(i, 2);
      Continue;
    end;
    if c = '.' then
    begin
      Result := Result + PathDelim;
      Inc(i);
      Continue;
    end;
    //treat slashes and backslashes the same as path delimiters too
    if (c = '/') or (c = '\') then
    begin
      Result := Result + PathDelim;
      Inc(i);
      Continue;
    end;
    Result := Result + c;
    Inc(i);
  end;
end;

//Expands <root>\<name> and returns it only when the file is really there and
//lands inside root, so a name built from "..", a drive letter or a UNC root
//cannot walk out of the folder it is checked against. The existence test is
//what lets require walk a list of endings: without it the first ending would
//"win" for every name and the rest of the list would never be reached.
function ResolveInRoot(const FileName, ARoot: string): string;
var
  Candidate: string;
begin
  Result := '';
  if (FileName = '') or (ARoot = '') or (Res = nil) then
    Exit;
  if IsRootedName(FileName) then
    Candidate := ExpandFileName(FileName)
  else
    Candidate := ExpandFileName(IncludePathDelimiter(ARoot) + FileName);
  if Res.IsPathInside(Candidate, ARoot) and SysUtils.FileExists(Candidate) then
    Result := Candidate;
end;

//require("name") -> runs a Lua file from the workspace or the app folder and
//returns whatever it returned, the same contract the stock require has. The
//stock one is gone (see TLua.Init) because it can load a native module from
//anywhere on disk: this loader expands the name first and refuses anything that
//does not land inside the workspace or the app folder. A module that returned a
//value is remembered in the registry under the file it came from, so the second
//require hands that value straight back and "m", "m.tyro" and "m.lua" share one
//entry.
function TLuaScript.Require_func(L: Plua_State): integer; cdecl;
var
  aName, Stem, Resolved, aMsg: string;
  Roots: array[0..1] of string;
  Extensions: array[0..1] of string;
  i, j: Integer;
begin
  if L.ArgsCount < 1 then
  begin
    L.PushNil;
    L.PushString('require: missing module name');
    Exit(2);
  end;
  aName := L.ToString(1);
  Stem := ModuleStem(aName);
  Resolved := '';

  //the workspace comes first, then the app folder; inside each, a .tyro file
  //comes before one of Tyro's own .lua scripts
  Extensions[0] := '.tyro';
  Extensions[1] := '.lua';
  Roots[0] := Res.WorkPath;
  Roots[1] := Res.AppPath;
  for i := 0 to 1 do
    for j := 0 to 1 do
      if Resolved = '' then
        Resolved := ResolveInRoot(Stem + Extensions[j], Roots[i]);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('require: ''' + aName + ''' is not a .tyro or .lua file inside the workspace or the app folder');
    Exit(2);
  end;

  //already run? the cached value is the answer, as it is with the stock loader
  lua_getfield(L, LUA_REGISTRYINDEX, PUTF8Char(cRequireCache)); //[cache]
  lua_pushstring(L, PUTF8Char(UTF8String(Resolved))); //[cache, path]
  lua_rawget(L, -2); //[cache, value]
  if not lua_isnil(L, -1) then
  begin
    Result := 1;
    Exit;
  end;
  lua_pop(L, 2); //[]

  if luaL_loadfile(L, PUTF8Char(UTF8String(Resolved))) <> LUA_OK then
  begin
    //the loader left its message on the stack; hand it back the way the stock
    //require does, nil plus a message
    aMsg := 'require: ' + aName + ': ' + L.ToString(-1);
    L.Pop(1);
    L.PushNil;
    L.PushString(aMsg);
    Exit(2);
  end;
  //the stock require calls the chunk with the module name as its argument
  lua_pushstring(L, PUTF8Char(UTF8String(aName))); //[chunk, name]
  lua_call(L, 1, 1); //[value]

  if lua_isnil(L, -1) then
    //a module that returns nothing runs again on the next require, as before
    L.Pop(1)
  else
  begin
    lua_getfield(L, LUA_REGISTRYINDEX, PUTF8Char(cRequireCache)); //[value, cache]
    lua_pushstring(L, PUTF8Char(UTF8String(Resolved))); //[value, cache, path]
    lua_pushvalue(L, -3); //[value, cache, path, value]
    lua_rawset(L, -3); //cache[path] = value -> [value, cache]
    L.Pop(1); //[value]
  end;
  Result := 1;
end;

function TLuaConsole.Read_func(L: Plua_State): integer; cdecl;
var
  s: string;
  c: integer;
  Reader: TReadConsoleObject;
begin
  s := '> ';
  c := L.ArgsCount;
  if c > 0 then
    s := L.ToString(1);

  Reader := TReadConsoleObject.Create(s);
  try
    // Run the DoExecute on the Engine thread via Synchronize.
    // The object is NOT freed by the engine; we free it here.
    Reader.Run(Script.Thread);
    //TThread.Synchronize(ScriptThread, procedure begin sleep(1000) end);
    // Wait for user to press Enter (signaled from Engine thread callback)
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
  aFile := Res.GuessFileName(aFile, Res.WorkPath);
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
    base := Lua.State.ArgsCount; //index of the sprite table

    //keep a duplicate, the -2/-3 addressing in Register() hits the sprite table
    Lua.State.PushValue(-1); //[sprite, sprite]

    Lua.State.PushInteger(AHandle);
    Lua.State.SetField(-2, '__handle'); //sprite.__handle = AHandle

    //methods receive the sprite table injected as argument 1
    Lua.State.Register('load', Load_func);
    Lua.State.Register('loadscript', LoadScript_func);
    Lua.State.Register('show', Show_func);
    Lua.State.Register('hide', Hide_func);
    Lua.State.Register('move', Move_func);
    Lua.State.Register('width', Width_func);
    Lua.State.Register('height', Height_func);
    Lua.State.Register('play', Play_func);
    Lua.State.Register('stop', Stop_func);
    Lua.State.Register('pause', Stop_func);
    Lua.State.Register('framecount', FrameCount_func);

    //metatable with property getter/setter (no table injection, Lua passes the table as arg 1)
    Lua.State.NewTable; //[sprite, sprite, meta]
    Lua.State.RegisterMeta('__index', __getter);
    Lua.State.RegisterMeta('__newindex', __setter);
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
  if L.ArgsCount >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  // Create the sprite object immediately on the Engine thread so script-only
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
  handle := Engine.Sprites.FindByName(aName);
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
  handle := Engine.Sprites.FindByName(aName);
  if handle > cSpriteInvalid then
    Script.Sprite.RegisterSprite(handle)
  else
    L.PushNil;
  Result := 1;
end;

//__handle must be read raw: the 'controls' and 'sprites' tables carry no
//__handle of their own, so lua_getfield would re-enter the Getter sitting on
//their __index and recurse until Lua reports "C stack overflow". That is what
//answers a lookup of a name the table does not hold, so the read has to stay
//recursion-free.
//The key goes on top of an unchanged idx: lua_rawget pops the key and pushes
//the raw value in its place, leaving the frame exactly as it was found (a
//leaked value would inflate L.ArgsCount and turn every getter into a setter).
//0 means "not a sprite/control table".
function GetSpriteHandle(L: Plua_State; idx: integer): integer;
begin
  L.PushString('__handle');
  lua_rawget(L, idx);
  Result := L.PopInteger;
end;

function GetControlTableHandle(L: Plua_State; idx: integer): integer;
begin
  L.PushString('__handle');
  lua_rawget(L, idx);
  Result := L.PopInteger;
end;

//a caption control (button/label/checkbox) keeps its text in Caption, every
//other control (an edit) in the shared Text. TSetControlTextObject writes to
//the same place, so a read here always answers what a write stored.
function GetControlText(AControl: TTyroControl): utf8string;
begin
  if AControl is TTyroCaptionControl then
    Result := TTyroCaptionControl(AControl).Caption
  else
    Result := AControl.Text;
end;

//Maps a border style value onto the enum: 0=none, 1=thin, 2=thick, 3=sizable.
//Anything out of range means "no border", the same answer controls.border
//gives a setter for a style it does not know.
function BorderOfValue(AValue: Integer): TBorder;
begin
  case AValue of
    1: Result := brdThin;
    2: Result := brdThick;
    3: Result := brdSizable;
  else
    Result := brdNone;
  end;
end;

//Maps a Lua value onto the docking edge: 'none', 'left', 'top', 'right',
//'bottom' or 'client', or the enum value 0..5. False when the value names no
//edge, so the caller can answer nil instead of silently docking nowhere.
function TryAlignOfValue(const AValue: string; out AAlign: TAlign): Boolean;
var
  s: string;
  n: Integer;
begin
  s := LowerCase(Trim(AValue));
  if TryStrToInt(s, n) and (n >= Ord(alNone)) and (n <= Ord(alClient)) then
    AAlign := TAlign(n)
  else if s = 'none' then
    AAlign := alNone
  else if s = 'left' then
    AAlign := alLeft
  else if s = 'top' then
    AAlign := alTop
  else if s = 'right' then
    AAlign := alRight
  else if s = 'bottom' then
    AAlign := alBottom
  else if s = 'client' then
    AAlign := alClient
  else
    Exit(False);
  Result := True;
end;

//The name of a docking edge, as controls.align and the align field read it.
function AlignName(AAlign: TAlign): string;
begin
  case AAlign of
    alLeft: Result := 'left';
    alTop: Result := 'top';
    alRight: Result := 'right';
    alBottom: Result := 'bottom';
    alClient: Result := 'client';
  else
    Result := 'none';
  end;
end;

//The border style as the number the border field and controls.border use.
function BorderStyleValue(ABorder: TBorder): Integer;
begin
  case ABorder of
    brdThin: Result := 1;
    brdThick: Result := 2;
    brdSizable: Result := 3;
  else
    Result := 0;
  end;
end;

//Resolves the container of a control: the handle a script can pass straight
//back into controls.parent, so 0 answers "the Engine window". Shared by the
//parent field and controls.parent.
function ParentHandleOf(AControls: TList; AControl: TTyroControl): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to AControls.Count - 1 do
  begin
    if (TTyroControl(AControls[i]) <> nil) and (TTyroControl(AControls[i]) = AControl.Parent) then
    begin
      Result := i + 1;
      Break;
    end;
  end;
end;

//Where the first value a script passed sits in the arguments of a control
//function. The control is always argument 1, either as a table or as a handle.
//A method registered on the control table gets that table pushed in front of the
//script's arguments, so lst.additem("a") arrives as (table, "a") - but Lua also
//passes the receiver to a colon call, so lst:additem("a") arrives as
//(table, table, "a"). Both spellings mean the same call, so a method reads its
//values from this position and not from a fixed one.
function FirstArg(L: PLua_State): Integer;
begin
  Result := 2;
  if (L.ArgsCount >= 2) and L.IsTable(1) and L.IsTable(2) and
    lua_rawequal(L, 1, 2) then
    Result := 3;
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
    if (c <> nil) and (SameText(c.Name, AName)) then
    begin
      Result := i + 1;
      Exit;
    end;
  end;
end;

function TLuaControls.GetControlHandle(AControl: TTyroControl): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    if TTyroControl(FItems[i]) = AControl then
    begin
      Result := i + 1;
      Exit;
    end;
  end;
end;

function TLuaControls.Setter(L: PLua_State): integer;
var
  ctrl: TTyroControl;
  parentCtrl: TTyroControl;
  field: string;
  aAlign: TAlign;
  r: TRect;
  v: Integer;
  Items: TStringList;
begin
  Result := 0;
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    Exit;
  field := LowerCase(L.ToString(2));

  if (field = 'x') or (field = 'left') or (field = 'top') or (field = 'y') or (field = 'width') or (field = 'height') then
  begin
    //geometry: a write changes one number of the rect and keeps the rest, so
    //x/y move the control and width/height resize it
    r := ctrl.BoundsRect;
    v := round(L.ToNumber(3));
    if (field = 'x') or (field = 'left') then
      r := Rect(v, r.Top, v + r.Width, r.Bottom)
    else if (field = 'y') or (field = 'top') then
      r := Rect(r.Left, v, r.Right, v + r.Height)
    else if field = 'width' then
      r := Rect(r.Left, r.Top, r.Left + v, r.Bottom)
    else
      r := Rect(r.Left, r.Top, r.Right, r.Top + v);
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Exit;
  end
  else if field = 'visible' then
  begin
    FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl, L.ToBoolean(3)));
    Exit;
  end
  else if field = 'border' then
  begin
    FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, BorderOfValue(round(L.ToNumber(3)))));
    Exit;
  end
  else if field = 'backcolor' then
  begin
    if L.IsNumber(3) then
      FScript.RunQueueObject(TSetControlBackColorObject.Create(ctrl, IntToColor(round(L.ToNumber(3)))));
    Exit;
  end
  else if field = 'name' then
  begin
    FScript.RunQueueObject(TSetControlNameObject.Create(ctrl, L.ToString(3)));
    Exit;
  end
  else if field = 'align' then
  begin
    if TryAlignOfValue(L.ToString(3), aAlign) then
      FScript.RunQueueObject(TSetControlAlignObject.Create(ctrl, aAlign));
    Exit;
  end
  else if field = 'parent' then
  begin
    //a nil or zero handle moves the control back to the Engine window
    if lua_isnil(L, 3) or (L.IsNumber(3) and (round(L.ToNumber(3)) <= 0)) then
      FScript.RunQueueObject(TSetControlParentObject.Create(ctrl, Engine.Main))
    else
    begin
      if L.IsTable(3) then
        parentCtrl := GetControl(GetControlTableHandle(L, 3))
      else
        parentCtrl := GetControl(round(L.ToNumber(3)));
      if parentCtrl <> nil then
        FScript.RunQueueObject(TSetControlParentObject.Create(ctrl, parentCtrl));
    end;
    Exit;
  end
  else if (field = 'text') or (field = 'caption') then
  begin
    //btn.text = "Hi" is the property form of controls.text(handle, "Hi")
    FScript.RunQueueObject(TSetControlTextObject.Create(ctrl, L.ToString(3)));
    Exit;
  end
  else if field = 'checked' then
  begin
    //chk.checked = true, the property form of controls.checked(handle, v)
    if ctrl is TTyroCheckBox then
      FScript.RunQueueObject(TSetControlCheckedObject.Create(TTyroCheckBox(ctrl), L.ToBoolean(3)));
    Exit;
  end
  else if field = 'file' then
  begin
    //img.file = "logo.png" is the property form of img.load(...)
    if ctrl is TTyroImage then
      FScript.RunQueueObject(TLoadControlImageObject.Create(ctrl, L.ToString(3)));
    Exit;
  end
  else if ctrl is TTyroListBox then
  begin
    //listbox fields; the ones that need a value only act on a listbox
    if field = 'items' then
    begin
      //lst.items = {"a", "b"} replaces the list, a single string replaces it
      //with one item and nil (or false) empties it
      Items := TStringList.Create;
      try
        if L.IsTable(3) then
        begin
          lua_pushnil(L);
          while lua_next(L, 3) <> 0 do
          begin
            //the key stays below the value, so the value is on top
            Items.Add(L.ToString(-1));
            lua_pop(L, 1);
          end;
        end
        else if not lua_isnil(L, 3) and not lua_isboolean(L, 3) then
          Items.Add(L.ToString(3));
        FScript.RunQueueObject(TSetControlItemsObject.Create(TTyroListBox(ctrl), Items));
      finally
        Items.Free;
      end;
      Exit;
    end
    else if field = 'itemindex' then
    begin
      FScript.RunQueueObject(TSetControlItemIndexObject.Create(TTyroListBox(ctrl), round(L.ToNumber(3))));
      Exit;
    end
    else if field = 'viewcount' then
    begin
      FScript.RunQueueObject(TSetControlViewCountObject.Create(TTyroListBox(ctrl), round(L.ToNumber(3))));
      Exit;
    end;
  end;
  // fallback: store raw
  L.PushValue(2); L.PushValue(3); lua_rawset(L, 1);
end;

//A control handle is a table of properties: every name below answers the
//current state of the control, and the Setter accepts the same names to change
//it. A control table also carries the few action methods (show, hide, move,
//position, focus, additem, item, clear, load) that have no property meaning.
function TLuaControls.Getter(L: PLua_State): integer;
var
  ctrl: TTyroControl;
  field: string;
begin
  Result := 0;
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  field := LowerCase(L.ToString(2));
  if field = 'count' then
  begin
    L.PushInteger(FItems.Count);
    Result := 1;
    Exit;
  end;
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if (field = 'x') or (field = 'left') then
  begin
    L.PushInteger(ctrl.BoundsRect.Left);
    Result := 1;
    Exit;
  end
  else if (field = 'y') or (field = 'top') then
  begin
    L.PushInteger(ctrl.BoundsRect.Top);
    Result := 1;
    Exit;
  end
  else if field = 'width' then
  begin
    L.PushInteger(ctrl.Width);
    Result := 1;
    Exit;
  end
  else if field = 'height' then
  begin
    L.PushInteger(ctrl.Height);
    Result := 1;
    Exit;
  end
  else if field = 'visible' then
  begin
    L.PushBoolean(ctrl.Visible);
    Result := 1;
    Exit;
  end
  else if field = 'hover' then
  begin
    L.PushBoolean(ctrl.Hover);
    Result := 1;
    Exit;
  end
  else if field = 'down' then
  begin
    L.PushBoolean(ctrl.Down);
    Result := 1;
    Exit;
  end
  else if field = 'clicked' then
  begin
    L.PushBoolean(ctrl.Clicked);
    Result := 1;
    Exit;
  end
  else if field = 'focused' then
  begin
    L.PushBoolean(ctrl.Focused);
    Result := 1;
    Exit;
  end
  else if field = 'name' then
  begin
    L.PushString(ctrl.Name);
    Result := 1;
    Exit;
  end
  else if field = 'parent' then
  begin
    //0 is the Engine window, a value controls.parent takes back unchanged
    L.PushInteger(ParentHandleOf(FItems, ctrl));
    Result := 1;
    Exit;
  end
  else if field = 'border' then
  begin
    L.PushInteger(BorderStyleValue(ctrl.Border));
    Result := 1;
    Exit;
  end
  else if field = 'backcolor' then
  begin
    L.PushInteger(ColorToInt(ctrl.BackColor));
    Result := 1;
    Exit;
  end
  else if field = 'align' then
  begin
    L.PushString(AlignName(ctrl.Align));
    Result := 1;
    Exit;
  end
  else if (field = 'text') or (field = 'caption') then
  begin
    //the caption of a button/label/checkbox, the edited text of an edit
    L.PushString(GetControlText(ctrl));
    Result := 1;
    Exit;
  end
  else if field = 'checked' then
  begin
    L.PushBoolean((ctrl is TTyroCheckBox) and TTyroCheckBox(ctrl).Checked);
    Result := 1;
    Exit;
  end
  else if field = 'loaded' then
  begin
    //true once an image control holds a texture
    L.PushBoolean((ctrl is TTyroImage) and TTyroImage(ctrl).Loaded);
    Result := 1;
    Exit;
  end
  else if ctrl is TTyroListBox then
  begin
    //listbox fields; the ones that need a listbox answer nil on anything else
    if field = 'items' then
      L.PushInteger(TTyroListBox(ctrl).Items.Count)
    else if field = 'itemindex' then
      L.PushInteger(TTyroListBox(ctrl).ItemIndex)
    else if field = 'viewcount' then
      L.PushInteger(TTyroListBox(ctrl).ViewCount)
    else
    begin
      L.PushValue(2);
      lua_rawget(L, 1);
      Result := 1;
      Exit;
    end;
    Result := 1;
    Exit;
  end;
  // fallback
  L.PushValue(2); lua_rawget(L, 1); Result := 1;
end;

function TLuaControls.RegisterControl(AHandle: Integer): Integer;
var
  ctrl: TTyroControl;
  base: Integer;
begin
  with Script do
  begin
    Lua.State.BeginTable;
    base := Lua.State.ArgsCount;

    Lua.State.PushValue(-1);
    Lua.State.PushInteger(AHandle);
    Lua.State.SetField(-2, '__handle');

    ctrl := GetControl(AHandle);
    if ctrl <> nil then
    begin
      Lua.State.PushString(ctrl.Name);
      Lua.State.SetField(-2, '__name');
    end;

    // A control table is a property bag first: the Getter answers every field
    // (x, width, text, checked, itemindex, ...) and the Setter takes it back, so
    // a field must NOT be registered here as a method - a raw field wins over
    // __index and over __newindex, which would make the property both unreadable
    // and silently dead. Only the actions that have no property spelling are
    // registered, and they take their control from the self argument, so they
    // are called with the colon form (btn:show()). controls.<name>(handle, ...)
    // takes a handle or a table and stays the plain spelling.
    if ctrl is TTyroListBox then
    begin
      Lua.State.Register('clear', Clear_func);
      Lua.State.Register('additem', AddItem_func);
      Lua.State.Register('item', Item_func);
    end
    else if ctrl is TTyroImage then
    begin
      //loaded is read as a field (img.loaded), not a call, see Controls.Getter
      Lua.State.Register('load', LoadImage_func);
    end;

    if ctrl is TTyroControl then
    begin
      Lua.State.Register('focus', Focus_func);
    end;

    if ctrl is TTyroLayout then
    begin
      Lua.State.Register('show', Show_func);
      Lua.State.Register('hide', Hide_func);
      Lua.State.Register('move', Move_func);
      Lua.State.Register('position', Position_func);
    end;

    Lua.State.NewTable;
    Lua.State.RegisterMeta('__index', Controls.__getter);
    Lua.State.RegisterMeta('__newindex', Controls.__setter);
    Lua.State.SetMetaTable(-2);

    if AHandle >= 1 then
    begin
      Lua.State.PushValue(-1);
      lua_rawseti(Lua.State, LUA_REGISTRYINDEX, cControlRegistryBase + AHandle);
    end;

    Lua.State.Remove(base);
    Result := 1;
  end;
end;

//controls.new(class, captionOrText, x?, y?, w?, h?, name?) -> handle
//(created on the Engine thread, self-drawn by the Engine cycle; the returned
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
  c := L.ArgsCount;
  caption := L.ToString(1);
  clsName := LowerCase(caption);
  x := 0;
  y := 0;
  aName := '';
  w := 500; h := 200;
  if (clsName = 'button') or (clsName = 'panel') or (clsName = 'label') or
     (clsName = 'checkbox') or (clsName = 'edit') or (clsName = 'spectrum') or
     (clsName = 'listbox') or (clsName = 'image') then
  begin
    //new style: controls.new(class, captionOrText, x?, y?, w?, h?, name?)
    clsName := caption;
    caption := L.ToString(2);
    popStart := 3;
    if clsName = 'button' then
    begin
      w := 100; h := 32;
    end
    else if clsName = 'panel' then
    begin
      w := 100; h := 100;
    end
    else if clsName = 'label' then
    begin
      w := 120; h := 24;
    end
    else if clsName = 'checkbox' then
    begin
      w := 120; h := 24;
    end
    else if clsName = 'edit' then
    begin
      w := 140; h := 28;
    end
    else if clsName = 'spectrum' then
    begin
      w := 500; h := 200;
    end
    else if clsName = 'listbox' then
    begin
      w := 160; h := 120;
    end
    else if clsName = 'image' then
    begin
      //same default as TTyroImage itself; a loaded texture that is larger
      //grows the control, a smaller one keeps these bounds
      w := 64; h := 64;
    end;
    if c >= 7 then
      aName := L.ToString(7);
  end
  else
  begin
    //legacy: buttons.new(caption, x?, y?, w?, h?, borderSize?)
    clsName := 'button';
    popStart := 2;
    w := 100; h := 32;
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
      RegisterControl(FItems.Count); //push control table
      Result := 1;
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if ctrl is TTyroCaptionControl then
  begin
    if L.ArgsCount >= 2 then
    begin
      FScript.RunQueueObject(TSetControlTextObject.Create(ctrl, L.ToString(2)));
      Result := 0;
    end
    else
    begin
      L.PushString((ctrl as TTyroCaptionControl).Caption);
      Result := 1;
    end;
  end;
end;

//controls.text(handle [, text]) -> get/set the control text: the caption of a
//button, label or checkbox, the edited text of an edit. Reading it answers the
//same string as controls.caption does for a caption control.
function TLuaControls.Text_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    FScript.RunQueueObject(TSetControlTextObject.Create(ctrl, L.ToString(2)));
    Result := 0;
  end
  else
  begin
    L.PushString(GetControlText(ctrl));
    Result := 1;
  end;
end;

function TLuaControls.Checked_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if ctrl is TTyroCheckBox then
  begin
    if L.ArgsCount >= 2 then
    begin
      FScript.RunQueueObject(TSetControlCheckedObject.Create(ctrl as TTyroCheckBox, L.ToBoolean(2)));
      Result := 0;
    end
    else
    begin
      L.PushBoolean((ctrl as TTyroCheckBox).Checked);
      Result := 1;
    end;
  end;
end;

//controls.position(handle) -> x, y ; controls.position(handle, x, y) -> move
function TLuaControls.Position_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  r: TRect;
  i: Integer;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  i := FirstArg(L);
  if (i + 1) <= L.ArgsCount then
  begin
    r := ctrl.BoundsRect;
    r := Rect(round(L.ToNumber(i)), round(L.ToNumber(i + 1)),
              round(L.ToNumber(i)) + r.Width, round(L.ToNumber(i + 1)) + r.Height);
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.ArgsCount >= 2 then
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

//controls.show(handle) / btn:show() -> make the control visible
function TLuaControls.Show_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  //btn:show() hands over its control table as the self argument,
  //controls.show(handle) hands over a handle
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl <> nil then
    FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl, True));
  Result := 0;
end;

//controls.hide(handle) / btn:hide() -> make the control invisible
function TLuaControls.Hide_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Focused);
  Result := 1;
end;

//controls.focus(handle) -> move the keyboard focus to the control (on the
//Engine thread, like every change of the input state)
function TLuaControls.Focus_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
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
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, BorderOfValue(round(L.ToNumber(2)))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(BorderStyleValue(ctrl.Border));
    Result := 1;
  end;
end;

//controls.backcolor(handle [, color]) -> get/set the back color as an int
//(use colors.name or an #rrggbb int from the colors table)
function TLuaControls.BackColor_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.ArgsCount >= 2 then
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.ArgsCount >= 2 then
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
  aAlign: TAlign;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if L.ArgsCount >= 2 then
  begin
    if TryAlignOfValue(L.ToString(2), aAlign) then
    begin
      FScript.RunQueueObject(TSetControlAlignObject.Create(ctrl, aAlign));
      Result := 0;
    end
    else
    begin
      L.PushNil;
      Result := 1;
    end;
  end
  else
  begin
    L.PushString(AlignName(ctrl.Align));
    Result := 1;
  end;
end;

//controls.parent(handle [, parentHandle]) -> get/set the container. The getter
//returns 0 when the control belongs to the Engine window, and the setter moves
//it back there for a nil or zero handle.
function TLuaControls.Parent_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  parentCtrl: TTyroControl;
  newParent: TTyroLayout;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if L.ArgsCount >= 2 then
  begin
    if lua_isnil(L, 2) or (L.IsNumber(2) and (round(L.ToNumber(2)) <= 0)) then
      newParent := Engine.Main
  else
  begin
    if L.IsTable(2) then
    parentCtrl := GetControl(GetControlTableHandle(L, 2))
  else
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
    //Return 0 for the Engine window so the result can be passed straight back
    //to the setter.
    L.PushInteger(ParentHandleOf(FItems, ctrl));
    Result := 1;
  end;
end;

//controls.items(handle [, item1, item2, ...]) -> replace all items; the items
//may also come as one table of strings (controls.items(h, {"a", "b"})), the
//spelling the items field uses. With no extra argument returns the item count.
function TLuaControls.Items_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  lb: TTyroListBox;
  i: Integer;
  Items: TStringList;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  lb := TTyroListBox(ctrl);
  if L.ArgsCount >= 2 then
  begin
    Items := TStringList.Create;
    try
      //a table of items as one argument, else one item per argument
      if L.IsTable(2) then
      begin
        lua_pushnil(L);
        while lua_next(L, 2) <> 0 do
        begin
          //the key stays below the value, so the value is on top
          Items.Add(L.ToString(-1));
          lua_pop(L, 1);
        end;
      end
      else
        for i := 2 to L.ArgsCount do
          Items.Add(L.ToString(i));
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
  idx, i: Integer;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  lb := TTyroListBox(ctrl);
  i := FirstArg(L);
  if i > L.ArgsCount then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  idx := round(L.ToNumber(i));
  if (i + 1) <= L.ArgsCount then
  begin
    FScript.RunQueueObject(TSetControlItemObject.Create(lb, idx, L.ToString(i + 1)));
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
  i: Integer;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  i := FirstArg(L);
  if (ctrl <> nil) and (ctrl is TTyroListBox) and (i <= L.ArgsCount) then
    FScript.RunQueueObject(TAddControlItemObject.Create(TTyroListBox(ctrl), L.ToString(i)));
  Result := 0;
end;

//controls.clear(handle) -> remove all items
function TLuaControls.Clear_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl <> nil) and (ctrl is TTyroListBox) then
    FScript.RunQueueObject(TClearControlItemsObject.Create(TTyroListBox(ctrl)));
  Result := 0;
end;

//img.load("logo.png") / img:load("logo.png") / controls.load(handle, "logo.png")
//Loads the texture an image control shows, replacing the one it holds. An
//empty (or missing) file name releases it. The upload runs on the Engine thread
//(it needs the GL context the control is painted with) and a texture bigger
//than the control grows it. Answers with the new state, so a file that could
//not be read reports false just like img.loaded does.
function TLuaControls.LoadImage_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  aFile: string;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  //A method registered on the control table is handed that table as argument 1,
  //and lst:load("a.png") passes it a second time, so the file name sits one slot
  //lower than for lst.load("a.png"). The last argument is the file in every
  //accepted spelling.
  if L.ArgsCount >= 2 then
    aFile := L.ToString(L.ArgsCount)
  else
    aFile := '';
  if (ctrl <> nil) and (ctrl is TTyroImage) then
  begin
    FScript.RunQueueObject(TLoadControlImageObject.Create(ctrl, aFile));
    //the upload already happened, so the flag is the answer of the call
    L.PushBoolean(TTyroImage(ctrl).Loaded);
  end
  else
    L.PushBoolean(False);
  Result := 1;
end;

//controls.viewcount(handle [, n]) -> get/set the visible rows (0 = boundsrect size)
function TLuaControls.ViewCount_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
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
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
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
  aFile := Res.GuessFileName(L.ToString(2));
  if L.ArgsCount >= 3 then
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
      // optional per-sprite script; compiled on the Engine thread by the engine queue
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

// sprite:loadscript("file.tyro") -> compile & attach a per-sprite script (Engine thread)
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
    Engine.Sprites.SetVisible(handle, True);
  Result := 0;
end;

function TLuaSprite.Hide_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Engine.Sprites.SetVisible(handle, False);
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
    Engine.Sprites.SetPosition(handle, x, y);
  Result := 0;
end;

function TLuaSprite.Width_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Engine.Sprites.GetWidth(handle));
  Result := 1;
end;

function TLuaSprite.Height_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Engine.Sprites.GetHeight(handle));
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
    if L.ArgsCount >= 2 then
      Engine.Sprites.SetAnimSpeed(handle, L.ToNumber(2));
    Engine.Sprites.SetAnimFrame(handle, 0);
    Engine.Sprites.SetPlaying(handle, True);
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
    Engine.Sprites.SetPlaying(handle, False);
  Result := 0;
end;

// sprite:framecount() -> number of frames in the loaded .aseprite animation
function TLuaSprite.FrameCount_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Engine.Sprites.GetFrameCount(handle));
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
      L.PushNumber(Engine.Sprites.GetX(handle));
      Result := 1;
    end
    else if field = 'y' then
    begin
      L.PushNumber(Engine.Sprites.GetY(handle));
      Result := 1;
    end
    else if field = 'angle' then
    begin
      L.PushNumber(Engine.Sprites.GetAngle(handle));
      Result := 1;
    end
    else if field = 'scale' then
    begin
      L.PushNumber(Engine.Sprites.GetScale(handle));
      Result := 1;
    end
    else if field = 'visible' then
    begin
      L.PushBoolean(Engine.Sprites.GetVisible(handle));
      Result := 1;
    end
    else if field = 'collide' then
    begin
      L.PushBoolean(Engine.Sprites.GetCollide(handle));
      Result := 1;
    end
    else if field = 'kind' then
    begin
      case Engine.Sprites.GetKind(handle) of
        skKinematic: L.PushString('kinematic');
        skStatic: L.PushString('static');
      else
        L.PushString('dynamic');
      end;
      Result := 1;
    end
    else if field = 'mass' then
    begin
      L.PushNumber(Engine.Sprites.GetMass(handle));
      Result := 1;
    end
    else if field = 'friction' then
    begin
      L.PushNumber(Engine.Sprites.GetFriction(handle));
      Result := 1;
    end
    else if field = 'bouncy' then
    begin
      L.PushNumber(Engine.Sprites.GetBouncy(handle));
      Result := 1;
    end
    else if field = 'radius' then
    begin
      L.PushNumber(Engine.Sprites.GetRadius(handle));
      Result := 1;
    end
    else if field = 'frames' then
    begin
      L.PushInteger(Engine.Sprites.GetFrameCount(handle));
      Result := 1;
    end
    else if field = 'frame' then
    begin
      L.PushInteger(Engine.Sprites.GetAnimFrame(handle));
      Result := 1;
    end
    else if field = 'playing' then
    begin
      L.PushBoolean(Engine.Sprites.GetPlaying(handle));
      Result := 1;
    end
    else if field = 'speed' then
    begin
      L.PushNumber(Engine.Sprites.GetAnimSpeed(handle));
      Result := 1;
    end
    else if field = 'looping' then
    begin
      L.PushBoolean(Engine.Sprites.GetLooping(handle));
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
    //One lock round-trip: the other axis is read inside the store under the
    //same lock instead of GetX + GetY + SetPosition (three of them).
    Engine.Sprites.SetPositionComponent(handle, field = 'x', L.ToNumber(3));
  end
  else if field = 'angle' then
  begin
    Engine.Sprites.SetAngle(handle, L.ToNumber(3));
  end
  else if field = 'scale' then
  begin
    Engine.Sprites.SetScale(handle, L.ToNumber(3));
  end
  else if field = 'visible' then
  begin
    Engine.Sprites.SetVisible(handle, L.ToBoolean(3));
  end
  else if field = 'collide' then
  begin
    Engine.Sprites.SetCollide(handle, L.ToBoolean(3));
  end
  else if field = 'kind' then
  begin
    if L.ToString(3) = 'kinematic' then
      k := skKinematic
    else if L.ToString(3) = 'static' then
      k := skStatic
    else
      k := skDynamic;
    Engine.Sprites.SetKind(handle, k);
  end
  else if field = 'mass' then
  begin
    Engine.Sprites.SetMass(handle, L.ToNumber(3));
  end
  else if field = 'friction' then
  begin
    Engine.Sprites.SetFriction(handle, L.ToNumber(3));
  end
  else if field = 'bouncy' then
  begin
    Engine.Sprites.SetBouncy(handle, L.ToNumber(3));
  end
  else if field = 'radius' then
  begin
    Engine.Sprites.SetRadius(handle, L.ToNumber(3));
  end
  else if field = 'frame' then
  begin
    Engine.Sprites.SetAnimFrame(handle, L.ToInteger(3));
  end
  else if field = 'playing' then
  begin
    Engine.Sprites.SetPlaying(handle, L.ToBoolean(3));
  end
  else if field = 'speed' then
  begin
    Engine.Sprites.SetAnimSpeed(handle, L.ToNumber(3));
  end
  else if field = 'looping' then
  begin
    Engine.Sprites.SetLooping(handle, L.ToBoolean(3));
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

// Builds a sprite table (like Sprites.new returns) bound to AHandle and leaves
// it on the Lua stack. Properties route through the proxy TLuaSprite getter/
// setter, so self.x / self.kind / ... work and unknown keys are raw-stored.
procedure PushSpriteTable(L: Plua_State; AHandle: Integer; ASprite: TLuaSprite);
begin
  L.NewTable; //[t]
  L.PushInteger(AHandle);
  L.SetField(-2, '__handle'); //t.__handle = AHandle -> [t]
  L.PushString(Engine.Sprites.GetName(AHandle));
  L.SetField(-2, '__name'); //t.__name = sprite name -> [t]
  L.NewTable; //[t, meta]

  //RegisterMeta, not Register: Lua already passes the table as argument 1 to
  //__index/__newindex, so the table must not be injected a second time.
  L.RegisterMeta('__index', ASprite.__getter);
  L.RegisterMeta('__newindex', ASprite.__setter);

  L.SetMetaTable(-2); //t.metatable = meta -> [t]
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
  FFileName := Res.GuessFileName(FFileName);
  Result := FLua.State.RunFile(FFileName, Msg);
  if not Result then
    DoError(Msg, 'load');
end;

procedure TLuaSpriteScript.BuildSelf;
begin
  PushSpriteTable(FLua.State, FHandle, FSprite); //[self]
  lua_setglobal(FLua.State, 'self'); //[]
end;

procedure TLuaSpriteScript.BuildGlobals;
begin
  // Reuse the Engine script's globals so per-sprite states can call the same
  // helpers (the bound FScript outlives this state).
  FLua.State.RegisterGlobal('time', FScript.TotalTime_func);
  FLua.State.RegisterGlobal('rand', FScript.RandomValue_func);
  FLua.State.RegisterGlobal('println', FScript.Console.PrintLn_func);
end;

procedure TLuaSpriteScript.BuildColors;
var
  i: integer;
begin
  FLua.State.BeginTable;
  for i := 0 to Length(FScript.Colors.Colors) - 1 do
    FLua.State.Register(FScript.Colors.Colors[i].Name, ColorToInt(FScript.Colors.Colors[i].Color));
  FLua.State.EndTableGlobal('colors', FScript.Colors);
end;

procedure TLuaSpriteScript.BuildDraw;
begin
  FLua.State.RegisterTable('draw');
  FLua.State.Register('draw', 'circle', Self, Circle_func);
  FLua.State.Register('draw', 'rectangle', Self, Rectangle_func);
  FLua.State.Register('draw', 'line', Self, Line_func);
  FLua.State.Register('draw', 'text', Self, Text_func);
end;

// Look up the global handler AName and pcall it. The caller has pushed the
// arguments (AArgs of them) below the handler position; an absent/ non-
// function handler just drains the stack and is ignored.
procedure TLuaSpriteScript.CallHandler(const AName: utf8string; AArgs: Integer);
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
  if L.ArgsCount >= 5 then
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
  if L.ArgsCount >= 6 then
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
  RayLib.DrawTextEx(Res.Font.Data, PUTF8Char(UTF8String(L.ToString(3))), Vector2Of(L.ToNumber(1), L.ToNumber(2)), Res.Font.Height, 0, SpriteColorValue(L, 4));
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
      Engine.Sprites.SetScript(FHandle, Scr)
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
  if Engine.Physics = nil then
    Exit;
  if L.ToString(2) = 'gravityx' then
  begin
    L.PushNumber(Engine.Physics.GetGravityX);
    Result := 1;
  end
  else if L.ToString(2) = 'gravityy' then
  begin
    L.PushNumber(Engine.Physics.GetGravityY);
    Result := 1;
  end
  else if L.ToString(2) = 'bodies' then
  begin
    L.PushInteger(Engine.Physics.BodyCount);
    Result := 1;
  end;
end;

function TLuaCollision.Setter(L: Plua_State): integer;
var
  x, y: single;
begin
  Result := 0;
  if Engine.Physics = nil then
    Exit;
  x := Engine.Physics.GetGravityX;
  y := Engine.Physics.GetGravityY;
  if L.ToString(2) = 'gravityx' then
    x := L.ToNumber(3)
  else if L.ToString(2) = 'gravityy' then
    y := L.ToNumber(3);
  Engine.Physics.SetGravity(x, y);
end;

// Fire the onCollide handler of the sprite AHandle, passing the other sprite and the contact state
procedure TLuaCollision.FireEvent(L: Plua_State; AHandle, AOtherHandle: Integer; const AState: string);
begin
  // get the target sprite table from the registry
  lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle);
  if not L.IsTable(-1) then
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
  if Engine.Physics = nil then
    Exit;
  ACount := 0;
  Engine.Physics.Poll(Evs, ACount);
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
  ScriptTypes.RegisterLanguage('Lua', ['', '.tyro', '.lua', '.pluto'], TLuaScript);
end.
