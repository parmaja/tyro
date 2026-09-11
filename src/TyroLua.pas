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
  LuaAPI, LuaClasses, FPImage,
  RayLib, RayClasses, //remove it
  mnUtils,
  TyroScripts, TyroSounds, TyroClasses, Melodies, TyroSprites,
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
  field := lua_tostring(L, 2);
  if lua_isinteger(L, -1) or lua_isnumber(L, -1) then
  begin
    i := lua_tointeger(L, -1);
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
  else if lua_isstring(L, -1) then
  begin
    if field = 'borderColor' then
    begin
      color := StrPas(lua_tostring(L, -1));
      Main.Console.BorderColor := StrToColor(color);
    end
    else if field = 'align' then
    begin
      //* TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
      if lua_tostring(L, -1) = 'none' then
        Main.Console.Align := TAlign(0)
      else if lua_tostring(L, -1) = 'left' then
        Main.Console.Align := TAlign(1)
      else if lua_tostring(L, -1) = 'top' then
        Main.Console.Align := TAlign(2)
      else if lua_tostring(L, -1) = 'right' then
        Main.Console.Align := TAlign(3)
      else if lua_tostring(L, -1) = 'bottom' then
        Main.Console.Align := TAlign(4)
      else if lua_tostring(L, -1) = 'client' then
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
  field := lua_tostring(L, 2);
  case field of
    'active':
    begin
      lua_pushboolean(L, Main.Console.Visible);
      Result := 1;
    end;
    'align':
    begin
      //* TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
      i := Ord(Main.Console.Align);
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
    end;
    'height':
    begin
      lua_pushinteger(L, Main.Console.Height);
      Result := 1;
    end;
    'width':
    begin
      lua_pushinteger(L, Main.Console.Width);
      Result := 1;
    end;
    'border':
    begin
      lua_pushinteger(L, Main.Console.BorderSize);
      Result := 1;
    end;
    'borderColor':
    begin
      lua_pushinteger(L, ColorToInt(Main.Console.BorderColor));
      Result := 1;
    end;
    'margin':
    begin
      lua_pushinteger(L, Main.Console.MarginSize);
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
  field := lua_tostring(L, 2);
  if lua_isinteger(L, -1) or lua_isnumber(L, -1) then
  begin
    i := lua_tointeger(L, -1);
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
  field := lua_tostring(L, 2);
  case field of
    'margin':
    begin
      lua_pushinteger(L, Main.MarginSize);
      Result := 1;
    end;
    'border':
    begin
      lua_pushinteger(L, Main.BorderSize);
      Result := 1;
    end;
    'borderColor':
    begin
      lua_pushinteger(L, ColorToInt(Main.BorderColor));
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
  if lua_isnumber(L, 2) then
  begin
    index := round(lua_tointeger(L, 2));
    if index < Length(Colors) then
    begin
      c := ColorToInt(Colors[index].Color);
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
  field := lua_tostring(L, 2);
  if lua_isinteger(L, -1) then
    case field of
      'color':
      begin
        i := lua_tointeger(L, -1);
        FScript.AddQueueObject(TDrawSetColorObject.Create(Main.Canvas, IntToColor(i)));
        Result := 1;
      end;
      'alpha':
      begin
        i := lua_tointeger(L, -1);
        FScript.AddQueueObject(TDrawSetAlphaObject.Create(Main.Canvas, i));
        Result := 1;
      end;
      'backcolor':
      begin
        i := lua_tointeger(L, -1);
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
  field := lua_tostring(L, 2);
  case field of
    'color':
    begin
      i := ColorToInt(Main.Canvas.PenColor);
      lua_pushinteger(L, i);
      Result := 1;
    end;
    'backcolor':
    begin
      i := ColorToInt(Main.Canvas.BackColor);
      lua_pushinteger(L, i);
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
  if lua_getstack(Lua.State, 1, ar) > 0 then
    lua_getinfo(Lua.State, 'nSl', ar);
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
  c := lua_gettop(L);
  x := 0;
  y := 0;
  w := 0;
  h := 0;
  if c >= 2 then
  begin
    x := round(lua_tonumber(L, 1));
    y := round(lua_tonumber(L, 2));
  end;
  if c >= 4 then
  begin
    w := round(lua_tonumber(L, 3));
    h := round(lua_tonumber(L, 4));
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
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  s := lua_tostring(L, 3);
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
  c := lua_gettop(L);
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  r := round(lua_tonumber(L, 3));
  if c >= 4 then
    f := lua_toboolean(L, 4);
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
  c := lua_gettop(L);
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  w := round(lua_tonumber(L, 3));
  h := round(lua_tonumber(L, 4));
  if c >= 4 then
    f := lua_toboolean(L, 5);
  FScript.AddQueueObject(TDrawRectangleObject.Create(Main.Canvas, x, y, w, h, f));
  Result := 0;
end;

function TLuaCanvas.Line_func(L: Plua_State): integer; cdecl;
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
  x := round(lua_tonumber(L, 1));
  y := round(lua_tonumber(L, 2));
  FScript.AddQueueObject(TDrawPointObject.Create(Main.Canvas, x, y));
  Result := 0;
end;

function TLuaConsole.Print_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := lua_gettop(L);
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + lua_tostring(L, i);
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
  Freq := round(lua_tonumber(L, 1));
  Period := round(lua_tonumber(L, 2));
  FScript.AddQueueObject(TPlaySoundObject.Create(Freq, Period));
  Result := 0;
end;

function TLuaMusic.Play_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 1);
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
  c := lua_gettop(L);
  SetLength(Song, c);
  for i := 0 to c - 1 do
  begin
    s := lua_tostring(L, i + 1);
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
  s := lua_tostring(L, 1);
  lua_pushboolean(L, TyroInput.IsKeyPressed(s));
  Result := 1;
end;

function TLuaScript.IsKeyDown_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 1);
  lua_pushboolean(L, TyroInput.IsKeyDown(s));
  Result := 1;
end;

function TLuaScript.MouseX_func(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TyroInput.MouseX);
  Result := 1;
end;

function TLuaScript.MouseY_func(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TyroInput.MouseY);
  Result := 1;
end;

function TLuaScript.IsMouseButtonPressed_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 1);
  lua_pushboolean(L, TyroInput.IsMouseButtonPressed(s));
  Result := 1;
end;

function TLuaScript.FrameTime_func(L: Plua_State): integer; cdecl;
begin
  lua_pushnumber(L, TyroInput.FrameTime);
  Result := 1;
end;

function TLuaScript.TotalTime_func(L: Plua_State): integer; cdecl;
begin
  lua_pushnumber(L, TyroInput.TotalTime);
  Result := 1;
end;

function TLuaScript.RandomValue_func(L: Plua_State): integer; cdecl;
var
  minv, maxv: integer;
begin
  minv := round(lua_tonumber(L, 1));
  maxv := round(lua_tonumber(L, 2));
  lua_pushinteger(L, TyroInput.RandomValue(minv, maxv));
  Result := 1;
end;

function TLuaConsole.Read_func(L: Plua_State): integer; cdecl;
var
  s: string;
  c: integer;
  Reader: TReadConsoleObject;
begin
  s := '> ';
  c := lua_gettop(L);
  if c > 0 then
    s := lua_tostring(L, 1);

  Reader := TReadConsoleObject.Create(s);
  try
    // Run the DoExecute on the main thread via Synchronize.
    // The object is NOT freed by the engine; we free it here.
    Reader.Run;
    //TThread.Synchronize(ScriptThread, procedure begin sleep(1000) end);
    // Wait for user to press Enter (signaled from main thread callback)
    Reader.Wait;
    // Push the result string to Lua
    lua_pushstring(L, PChar(Reader.ResultString));
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
  aFile := lua_tostring(L, 1);
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
  if lua_isnumber(L, 2) then
    aSize := lua_tointeger(L, 2) //LoadFontEx
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
    base := lua_gettop(Lua.State); //index of the sprite table

    //keep a duplicate, the -2/-3 addressing in Register() hits the sprite table
    lua_pushvalue(Lua.State, -1); //[sprite, sprite]

    lua_pushinteger(Lua.State, AHandle);
    lua_setfield(Lua.State, -2, '__handle'); //sprite.__handle = AHandle

    //methods receive the sprite table injected as argument 1
    Lua.State.Register('load', @Load_func);
    Lua.State.Register('show', @Show_func);
    Lua.State.Register('hide', @Hide_func);
    Lua.State.Register('move', @Move_func);
    Lua.State.Register('width', @Width_func);
    Lua.State.Register('height', @Height_func);

    //metatable with property getter/setter (no table injection, Lua passes the table as arg 1)
    lua_newtable(Lua.State); //[sprite, sprite, meta]
    Lua.State.RegisterMeta('__index', @__getter);
    Lua.State.RegisterMeta('__newindex', @__setter);
    lua_setmetatable(Lua.State, -2); //sprite.metatable = meta

    lua_remove(Lua.State, base); //drop the first reference, keep one on the stack
    Result := 1;
  end;
end;

// Sprites.new("name"?) -> creates a new sprite object, returns it on Lua stack
function TLuaSprites.New_func(L: Plua_State): integer; cdecl;
var
  aName: string;
begin
  if lua_gettop(L) >= 1 then
    aName := lua_tostring(L, 1)
  else
    aName := '';
  // Create sprite object with handle -1 (no texture yet, load() will populate it)
  Script.Sprite.RegisterSprite(-1);
  // Store optional name
  if aName <> '' then
  begin
    lua_pushstring(L, aName);
    lua_setfield(L, -2, '__name');
  end;
  Result := 1;
end;

// Sprites.find("name") -> returns a sprite object for the named sprite, or nil
function TLuaSprites.Find_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := lua_tostring(L, 1);
  handle := Main.Sprites.FindByName(aName);
  if handle > cSpriteInvalid then
    Script.Sprite.RegisterSprite(handle)
  else
    lua_pushnil(L);
  Result := 1;
end;

// Sprites("name") -> __call metamethod, same as Sprites.find
function TLuaSprites.Call_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := lua_tostring(L, 2); // first arg after table self
  handle := Main.Sprites.FindByName(aName);
  if handle > cSpriteInvalid then
    Script.Sprite.RegisterSprite(handle)
  else
    lua_pushnil(L);
  Result := 1;
end;

function GetSpriteHandle(L: Plua_State; idx: integer): integer;
begin
  lua_getfield(L, idx, '__handle');
  Result := lua_tointeger(L, -1);
  lua_pop(L, 1);
end;

function TLuaSprite.Load_func(L: Plua_State): integer; cdecl;
var
  aFile, aName: string;
  handle: integer;
  LoadObj: TLoadSpriteObject;
begin
  aFile := Resources.GuessFileName(lua_tostring(L, 2));
  lua_getfield(L, 1, '__name');
  if lua_isstring(L, -1) then
    aName := lua_tostring(L, -1)
  else
    aName := ExtractFileName(aFile);
  lua_pop(L, 1);
  LoadObj := TLoadSpriteObject.Create(aFile, aName);
  try
    LoadObj.Run(Script.Thread);
    LoadObj.Wait;
    handle := LoadObj.HandleResult;
    if handle > cSpriteInvalid then
    begin
      lua_pushinteger(L, handle);
      lua_setfield(L, 1, '__handle');
    end
    else
      Script.DoError('Sprite not loaded: ' + aFile);
  finally
    LoadObj.Free;
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
  x := lua_tonumber(L, 2);
  y := lua_tonumber(L, 3);
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
  lua_pushinteger(L, Main.Sprites.GetWidth(handle));
  Result := 1;
end;

function TLuaSprite.Height_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  lua_pushinteger(L, Main.Sprites.GetHeight(handle));
  Result := 1;
end;

// Sprite __index: read properties x, y, angle, scale, visible
function TLuaSprite.Getter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key
  handle := GetSpriteHandle(L, 1);
  field := lua_tostring(L, 2);
  Result := 0;
  if handle <= cSpriteInvalid then
    Exit;
  if field = 'x' then
  begin
    lua_pushnumber(L, Main.Sprites.GetX(handle));
    Result := 1;
  end
  else if field = 'y' then
  begin
    lua_pushnumber(L, Main.Sprites.GetY(handle));
    Result := 1;
  end
  else if field = 'angle' then
  begin
    lua_pushnumber(L, Main.Sprites.GetAngle(handle));
    Result := 1;
  end
  else if field = 'scale' then
  begin
    lua_pushnumber(L, Main.Sprites.GetScale(handle));
    Result := 1;
  end
  else if field = 'visible' then
  begin
    lua_pushboolean(L, Main.Sprites.GetVisible(handle));
    Result := 1;
  end;
end;

// Sprite __newindex: write properties x, y, angle, scale, visible
function TLuaSprite.Setter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
  curX, curY: single;
begin
  // arg 1 is the table, arg 2 is the key, arg 3 is the value
  handle := GetSpriteHandle(L, 1);
  field := lua_tostring(L, 2);
  Result := 0;
  if handle <= cSpriteInvalid then
    Exit;
  if (field = 'x') or (field = 'y') then
  begin
    curX := Main.Sprites.GetX(handle);
    curY := Main.Sprites.GetY(handle);
    if field = 'x' then
      curX := lua_tonumber(L, 3)
    else
      curY := lua_tonumber(L, 3);
    Main.Sprites.SetPosition(handle, curX, curY);
  end
  else if field = 'angle' then
  begin
    Main.Sprites.SetAngle(handle, lua_tonumber(L, 3));
  end
  else if field = 'scale' then
  begin
    Main.Sprites.SetScale(handle, lua_tonumber(L, 3));
  end
  else if field = 'visible' then
  begin
    Main.Sprites.SetVisible(handle, lua_toboolean(L, 3));
   end;
end;

initialization
  Main.RegisterLanguage('Lua', ['.ls', '.lua', '.pluto'], TLuaScript);
end.
