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
  TyroControls, TyroEngines, TyroInput;

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

  { TLuaConsole }

  TLuaConsole = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Print_func(L: Plua_State): integer; cdecl;
    function PrintLn_func(L: Plua_State): integer; cdecl;
    function Show_func(L: Plua_State): integer; cdecl;
    function Read_func(L: Plua_State): integer; cdecl;
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
  end;

  TLuaSprites = class(TTyroLuaObject)
  protected
  public
    //sprites
    function New_func(L: Plua_State): integer; cdecl;
    function Find_func(L: Plua_State): integer; cdecl;
    function Call_func(L: Plua_State): integer; cdecl;
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
    Sprite: TLuaSprite;
    Sprites: TLuaSprites;
    Collision: TLuaCollision;
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

   public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddQueueObject(AQueueObject: TQueueObject); override;
  end;

const
  // Integer key base in the Lua registry for "sprite handle -> sprite table"
  cSpriteRegistryBase = $00700000;

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
  for i := 1 to c do
  begin
    s := L.Params[i].AsString;
    if IsConsole then
      WriteLn(s);
  end;
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
    else if field = 'border' then
      Main.Console.BorderSize := i
    else if field = 'margin' then
      Main.Console.MarginSize := i
    else if field = 'borderColor' then
      Main.Console.BorderColor := IntToColor(i);
  end
  else if L.IsString(-1) then
  begin
    if field = 'borderColor' then
    begin
      color := L.ToString(-1);
      Main.Console.BorderColor := StrToColor(color);
    end
    else if field = 'align' then
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
    'border':
    begin
      L.PushInteger(Main.Console.BorderSize);
      Result := 1;
    end;
    'borderColor':
    begin
      L.PushInteger(ColorToInt(Main.Console.BorderColor));
      Result := 1;
    end;
    'margin':
    begin
      L.PushInteger(Main.Console.MarginSize);
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
begin
  Result := 0;
  field := L.ToString(2);
  if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'margin' then
      Main.MarginSize := i
    else if field = 'border' then
      Main.BorderSize := i
    else if field = 'borderColor' then
      Main.BorderColor := IntToColor(i);
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
      L.PushInteger(Main.MarginSize);
      Result := 1;
    end;
    'border':
    begin
      L.PushInteger(Main.BorderSize);
      Result := 1;
    end;
    'borderColor':
    begin
      L.PushInteger(ColorToInt(Main.BorderColor));
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
  AddColor('black', clBlack);
  AddColor('red', clRed);
  AddColor('maroon', clMaroon);
  AddColor('yellow', clYellow);
  AddColor('olive', clDarkgreen);
  AddColor('lime', clLime);
  AddColor('green', clGreen);
  AddColor('aqua', clSkyBlue);
  AddColor('teal', clBrown);
  AddColor('blue', clBlue);
  AddColor('navy', clViolet);
  AddColor('fuchsia', clMagenta);
  AddColor('purple', clPurple);
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
  end;
end;

constructor TLuaCanvas.Create(AScript: TLuaScript);
begin
  inherited;
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
  Sprite := TLuaSprite.Create(Self);
  Sprites := TLuaSprites.Create(Self);
  Collision := TLuaCollision.Create(Self);

  //window
  Lua.State.Register('window', 'show', Window, @Window.Window_func);
  Lua.State.Register('window', Window); //Should be last one for window

  //global functions
  Lua.State.RegisterGlobal('print', @Console.Print_func);
  Lua.State.RegisterGlobal('println', @Console.PrintLn_func);

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

  //font
  Lua.State.Register('font', 'load', Font, @Font.Load_func);
  Lua.State.Register('font', Font); //Should be last one

  //music
  Lua.State.Register('music', 'beep', Music, @Music.Beep_func);
  Lua.State.Register('music', 'sound', Music, @Music.Sound_func);
  Lua.State.Register('music', 'play', Music, @Music.Play_func);
  Lua.State.Register('music', 'mml', Music, @Music.MML_func);

  //input & timing (global functions)
  Lua.State.RegisterGlobal('iskeypressed', @IsKeyPressed_func);
  Lua.State.RegisterGlobal('iskeydown', @IsKeyDown_func);
  Lua.State.RegisterGlobal('mousex', @MouseX_func);
  Lua.State.RegisterGlobal('mousey', @MouseY_func);
  Lua.State.RegisterGlobal('ismousepressed', @IsMouseButtonPressed_func);
  Lua.State.RegisterGlobal('frametime', @FrameTime_func);
  Lua.State.RegisterGlobal('time', @TotalTime_func);
  Lua.State.RegisterGlobal('rand', @RandomValue_func);

  // Sprite system: Sprites.new creates a sprite, Sprites("name") finds by name
  Lua.State.RegisterTable('Sprites');
  Lua.State.Register('Sprites', 'new', Self, @Sprites.New_func);
  Lua.State.Register('Sprites', 'find', Self, @Sprites.Find_func);
  // Set __call in the Sprites metatable so Sprites("name") works; Lua passes the table as arg 1
  Lua.State.Register('Sprites', '__call', Self, @Sprites.Call_func, True);

  // Collision system: collision.pump() drains events and fires sprite.onCollide(other, state)
  Lua.State.Register('collision', 'pump', Collision, @Collision.Pump_func);
  Lua.State.Register('collision', Collision); // should be last — wires getter/setter metamethods

  Lua.State.BeginTable;
  for i := 0 to Length(Colors.Colors) - 1 do
    Lua.State.Register(Colors.Colors[i].Name, ColorToInt(Colors.Colors[i].Color));
  Lua.State.EndTable('colors', Colors);
end;

destructor TLuaScript.Destroy;
begin
  Lua.Close;
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
  //WriteLn('Run Script');
  //Sleep(1000);
  if not Lua.State.RunString(ScriptText.Text, Msg) then
    DoError(Msg);
end;

procedure TLuaScript.AddQueueObject(AQueueObject: TQueueObject);
{$ifdef DEBUG_LUA}
var
  ar: lua_Debug;
{$endif}
begin
  {$ifdef DEBUG_LUA}
  if Lua.State.GetStack(1, ar) then
    Lua.State.GetInfo('nSl', ar);
  {$endif}
  AQueueObject.LineNo := ar.currentline;
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
  if ExtractFileDir(s) = '' then
    s := Resources.CurrentDirectory + s;
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
  //FScript.AddQueueObject(TPlayMMLObject.Create(Song));
  with TPlayMMLObject.Create(Song) do
    //using current lua thread to not block current thread, or maybe use a thread
  begin
    Execute;
    Free;
  end;
  Result := 0;
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
    Reader.Run;
    //TThread.Synchronize(ScriptThread, procedure begin sleep(1000) end);
    // Wait for user to press Enter (signaled from main thread callback)
    Reader.Wait;
    // Push the result string to Lua
    L.PushString(Reader.ResultString);
    Result := 1;
  finally
    Reader.Free;
  end;
end;

function TLuaFont.Load_func(L: Plua_State): integer; cdecl;
var
  aFile, s: string;
  aSize: integer;
begin
  aFile := L.ToString(1);
  // Load font from current directory (ScriptPath or WorkSpace)
  if ExtractFileDir(aFile) = '' then
  begin
    if not SysUtils.FileExists(aFile) then
      s := IncludePathDelimiter(FScript.Path) + aFile;
    if not SysUtils.FileExists(s) then
      s := IncludePathDelimiter(Resources.CurrentDirectory) + aFile;
    if not SysUtils.FileExists(s) then
      s := IncludePathDelimiter(Resources.WorkSpace) + 'assets' + PathDelim + aFile;
  end;
  if L.IsNumber(2) then
    aSize := L.ToInteger(2) //LoadFontEx
  else
    aSize := 0; //LoadFont
   FScript.AddQueueObject(TLoadFontObject.Create(s, aSize));
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
begin
  if L.Count >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  // Create sprite object with handle -1 (no texture yet, load() will populate it)
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
  LoadObj := TLoadSpriteObject.Create(aFile, aName);
  try
    LoadObj.Run(Script.Thread);
    LoadObj.Wait;
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

// draw.* color argument: an integer paletted color or a color name
function ColorValue(L: Plua_State; Idx: Integer): TColor;
begin
  if L.IsString(Idx) then
    Result := StrToColor(L.ToString(Idx))
  else
    Result := IntToColor(L.ToInteger(Idx));
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
    RayLib.DrawCircle(round(x), round(y), r, ColorValue(L, 4))
  else
    RayLib.DrawCircleLines(round(x), round(y), r, ColorValue(L, 4));
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
    RayLib.DrawRectangle(x, y, w, h, ColorValue(L, 5))
  else
    RayLib.DrawRectangleLinesEx(RectangleOf(x, y, w, h), 1, ColorValue(L, 5));
  Result := 0;
end;

// draw.line(x1, y1, x2, y2, color)
function TLuaSpriteScript.Line_func(L: Plua_State): integer; cdecl;
begin
  RayLib.DrawLineEx(Vector2Of(L.ToNumber(1), L.ToNumber(2)), Vector2Of(L.ToNumber(3), L.ToNumber(4)), 1, ColorValue(L, 5));
  Result := 0;
end;

// draw.text(x, y, text, color)
function TLuaSpriteScript.Text_func(L: Plua_State): integer; cdecl;
begin
  RayLib.DrawTextEx(Resources.Font.Data, PUTF8Char(L.ToString(3)), Vector2Of(L.ToNumber(1), L.ToNumber(2)), Resources.Font.Height, 0, ColorValue(L, 4));
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
