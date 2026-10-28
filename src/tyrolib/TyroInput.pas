unit TyroInput;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *  Thin wrapper over RayLib input/timing functions exposed to script engines.
 *  Key and mouse-button names are strings (e.g. "space", "a", "left") that
 *  map to RayLib TKeyboardKey / TMouseButton enums.
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils,
  RayLib;

{ Maps a human-readable key name to a RayLib TKeyboardKey.
  Supported names:
    Letters "a".."z", digits "0".."9"
    "space" "enter" "tab" "escape" "backspace" "delete" "insert"
    "up" "down" "left" "right"
    "f1".."f12"
    "shift" "ctrl" "alt"  (left variants)
  Returns KEY_NULL when the name is unrecognised. }
function KeyNameToEnum(const Name: string): TKeyboardKey;

{ Maps a human-readable mouse-button name to a RayLib TMouseButton.
  Returns False for unknown names rather than silently querying left. }
function TryMouseButtonNameToEnum(const Name: string; out Button: TMouseButton): Boolean;

{ Maps a human-readable gamepad-button name to a RayLib TGamepadButton.
  Supported names:
    "a", "b", "x", "y" (Xbox)
    "cross", "circle", "square", "triangle" (PlayStation)
    "lb", "rb", "lt", "rt" / "l1", "r1", "l2", "r2"
    "start", "select", "guide", "back", "menu", "options", "share", "home"
    "lstick", "rstick", "l3", "r3" (stick clicks)
    "dpad_up", "dpad_down", "dpad_left", "dpad_right" (and "up", "down", "left", "right")
    "fire", "trigger", "button1".."button12"
  Returns False for unknown names rather than silently querying a button. }
function TryGamepadButtonNameToEnum(const Name: string; out Button: TGamepadButton): Boolean;

{ Maps a human-readable gamepad-axis name to a RayLib TGamepadAxis.
  Supported names:
    "leftx", "lefty", "rightx", "righty"
    "lx", "ly", "rx", "ry", "x", "y"
    "ltrigger", "rtrigger", "lt", "rt"
  Returns False for unknown names. }
function TryGamepadAxisNameToEnum(const Name: string; out Axis: TGamepadAxis): Boolean;

{ --- Query functions (thread-safe atomic reads of RayLib internal state) --- }

function IsKeyPressed(const KeyName: string): Boolean;
function IsKeyDown(const KeyName: string): Boolean;
function MouseX: Integer;
function MouseY: Integer;
function IsMouseButtonPressed(const ButtonName: string): Boolean;

{ --- Gamepad / Joystick queries (Gamepad: player index 0..3, or -1 for any connected) --- }

{ True if specified gamepad is connected, or any gamepad when Gamepad = -1 }
function GamepadAvailable(Gamepad: Integer = -1): Boolean;
{ Controller name; when Gamepad = -1, returns name of first connected gamepad, '' when none }
function GamepadName(Gamepad: Integer = -1): string;
{ Total number of connected gamepads (0..4) }
function GamepadCount: Integer;
{ Number of axes on gamepad (or first connected if Gamepad = -1) }
function GamepadAxisCount(Gamepad: Integer = -1): Integer;
{ Button press edge. When Gamepad = -1, checks all connected gamepads }
function IsGamepadPressed(const ButtonName: string; Gamepad: Integer = -1): Boolean;
{ Button held down. When Gamepad = -1, checks all connected gamepads }
function IsGamepadDown(const ButtonName: string; Gamepad: Integer = -1): Boolean;
{ Button release edge. When Gamepad = -1, checks all connected gamepads }
function IsGamepadReleased(const ButtonName: string; Gamepad: Integer = -1): Boolean;
{ Button not down (up). When Gamepad = -1, returns true if not down on any connected gamepad }
function IsGamepadUp(const ButtonName: string; Gamepad: Integer = -1): Boolean;
{ Axis movement -1..1 (triggers 1..-1). When Gamepad = -1, checks active movement across connected gamepads }
function GamepadAxis(const AxisName: string; Gamepad: Integer = -1): Single;
{ Gamepad vibration (left motor: low freq, right motor: high freq; duration in seconds). Gamepad = -1 vibrates all }
procedure GamepadVibrate(LeftMotor, RightMotor, Duration: Single; Gamepad: Integer = -1);

{ Adds gamepad mappings in the SDL/GLFW format RayLib understands, one
  mapping per line (<GUID>,<name>,<controls>). GUIDs copied from SDL 2.0.5+
  or SDL3 databases (the form that carries a CRC of the device name) are
  converted to the form GLFW matches, so both work. Most controllers need no
  mapping, but generic ones do: without an entry every button and axis reads
  as released. Returns nonzero when the library accepted the text; raylib
  reports success, not how many lines matched a connected controller. }
function SetGamepadMappings(const Mappings: string): Integer;

{ --- Frame timing --- }

{ Seconds elapsed since the last frame was drawn }
function FrameTime: Single;
{ Seconds elapsed since InitWindow() was called }
function TotalTime: Double;

{ Random integer in [Min, Max] (inclusive) }
function RandomValue(Min, Max: Integer): Integer;

implementation

function KeyNameToEnum(const Name: string): TKeyboardKey;
var
  s: string;
  num: Integer;
begin
  Result := KEY_NULL;
  s := LowerCase(Trim(Name));
  if s = '' then
    Exit;

  // Single-character: letters and digits
  if Length(s) = 1 then
  begin
    case s[1] of
      'a'..'z':
        Result := TKeyboardKey(Ord(UpCase(s[1])));  // 'a' -> KEY_A (65)
      '0'..'9':
        Result := TKeyboardKey(Ord(s[1]));           // '0' -> KEY_ZERO (48)
    end;
    Exit;
  end;

  // Named keys
  if s = 'space' then
    Result := KEY_SPACE
  else if s = 'enter' then
    Result := KEY_ENTER
  else if s = 'tab' then
    Result := KEY_TAB
  else if s = 'escape' then
    Result := KEY_ESCAPE
  else if s = 'backspace' then
    Result := KEY_BACKSPACE
  else if s = 'delete' then
    Result := KEY_DELETE
  else if s = 'insert' then
    Result := KEY_INSERT
  else if s = 'up' then
    Result := KEY_UP
  else if s = 'down' then
    Result := KEY_DOWN
  else if s = 'left' then
    Result := KEY_LEFT
  else if s = 'right' then
    Result := KEY_RIGHT
  else if s = 'shift' then
    Result := KEY_LEFT_SHIFT
  else if s = 'ctrl' then
    Result := KEY_LEFT_CONTROL
  else if s = 'alt' then
    Result := KEY_LEFT_ALT
  else if (s[1] = 'f') and (Length(s) > 1) and
          TryStrToInt(Copy(s, 2, MaxInt), num) and (num >= 1) and (num <= 12) then
    Result := TKeyboardKey(Ord(KEY_F1) + (num - 1));
end;

function TryMouseButtonNameToEnum(const Name: string; out Button: TMouseButton): Boolean;
var
  n: string;
begin
  n := LowerCase(Trim(Name));
  Result := True;
  if n = 'left' then
    Button := MOUSE_BUTTON_LEFT
  else if n = 'right' then
    Button := MOUSE_BUTTON_RIGHT
  else if n = 'middle' then
    Button := MOUSE_BUTTON_MIDDLE
  else
    Result := False;
end;

function TryGamepadButtonNameToEnum(const Name: string; out Button: TGamepadButton): Boolean;
var
  n: string;
begin
  n := LowerCase(Trim(Name));
  Result := True;
  if (n = 'a') or (n = 'cross') or (n = 'button1') or (n = 'fire') or (n = 'trigger') then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_DOWN      // Xbox A / PS cross
  else if (n = 'b') or (n = 'circle') or (n = 'button2') then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_RIGHT     // Xbox B / PS circle
  else if (n = 'x') or (n = 'square') or (n = 'button3') then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_LEFT      // Xbox X / PS square
  else if (n = 'y') or (n = 'triangle') or (n = 'button4') then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_UP        // Xbox Y / PS triangle
  else if (n = 'lb') or (n = 'l1') or (n = 'button5') then
    Button := GAMEPAD_BUTTON_LEFT_TRIGGER_1
  else if (n = 'rb') or (n = 'r1') or (n = 'button6') then
    Button := GAMEPAD_BUTTON_RIGHT_TRIGGER_1
  else if (n = 'lt') or (n = 'l2') or (n = 'button7') then
    Button := GAMEPAD_BUTTON_LEFT_TRIGGER_2
  else if (n = 'rt') or (n = 'r2') or (n = 'button8') then
    Button := GAMEPAD_BUTTON_RIGHT_TRIGGER_2
  else if (n = 'select') or (n = 'back') or (n = 'share') or (n = 'button9') then
    Button := GAMEPAD_BUTTON_MIDDLE_LEFT
  else if (n = 'guide') or (n = 'home') then
    Button := GAMEPAD_BUTTON_MIDDLE
  else if (n = 'start') or (n = 'menu') or (n = 'options') or (n = 'button10') then
    Button := GAMEPAD_BUTTON_MIDDLE_RIGHT
  else if (n = 'lstick') or (n = 'l3') or (n = 'lthumb') or (n = 'button11') then
    Button := GAMEPAD_BUTTON_LEFT_THUMB
  else if (n = 'rstick') or (n = 'r3') or (n = 'rthumb') or (n = 'button12') then
    Button := GAMEPAD_BUTTON_RIGHT_THUMB
  else if (n = 'dpad_up') or (n = 'dpad-up') or (n = 'up') then
    Button := GAMEPAD_BUTTON_LEFT_FACE_UP
  else if (n = 'dpad_down') or (n = 'dpad-down') or (n = 'down') then
    Button := GAMEPAD_BUTTON_LEFT_FACE_DOWN
  else if (n = 'dpad_left') or (n = 'dpad-left') or (n = 'left') then
    Button := GAMEPAD_BUTTON_LEFT_FACE_LEFT
  else if (n = 'dpad_right') or (n = 'dpad-right') or (n = 'right') then
    Button := GAMEPAD_BUTTON_LEFT_FACE_RIGHT
  else
    Result := False;
end;

function TryGamepadAxisNameToEnum(const Name: string; out Axis: TGamepadAxis): Boolean;
var
  n: string;
begin
  n := LowerCase(Trim(Name));
  Result := True;
  if (n = 'leftx') or (n = 'left_x') or (n = 'lx') or (n = 'x') then
    Axis := GAMEPAD_AXIS_LEFT_X
  else if (n = 'lefty') or (n = 'left_y') or (n = 'ly') or (n = 'y') then
    Axis := GAMEPAD_AXIS_LEFT_Y
  else if (n = 'rightx') or (n = 'right_x') or (n = 'rx') then
    Axis := GAMEPAD_AXIS_RIGHT_X
  else if (n = 'righty') or (n = 'right_y') or (n = 'ry') then
    Axis := GAMEPAD_AXIS_RIGHT_Y
  else if (n = 'ltrigger') or (n = 'left_trigger') or (n = 'lt') or (n = 'l2') then
    Axis := GAMEPAD_AXIS_LEFT_TRIGGER
  else if (n = 'rtrigger') or (n = 'right_trigger') or (n = 'rt') or (n = 'r2') then
    Axis := GAMEPAD_AXIS_RIGHT_TRIGGER
  else
    Result := False;
end;

function IsKeyPressed(const KeyName: string): Boolean;
begin
  Result := RayLib.IsKeyPressed(KeyNameToEnum(KeyName));
end;

function IsKeyDown(const KeyName: string): Boolean;
begin
  Result := RayLib.IsKeyDown(KeyNameToEnum(KeyName));
end;

function MouseX: Integer;
begin
  Result := RayLib.GetMouseX;
end;

function MouseY: Integer;
begin
  Result := RayLib.GetMouseY;
end;

function IsMouseButtonPressed(const ButtonName: string): Boolean;
var
  Button: TMouseButton;
begin
  Result := TryMouseButtonNameToEnum(ButtonName, Button) and
    RayLib.IsMouseButtonPressed(Button);
end;

function GamepadAvailable(Gamepad: Integer): Boolean;
var
  i: Integer;
begin
  if Gamepad >= 0 then
    Result := RayLib.IsGamepadAvailable(Gamepad)
  else
  begin
    Result := False;
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) then
        Exit(True);
    end;
  end;
end;

function GamepadName(Gamepad: Integer): string;
var
  p: PUTF8Char;
  i: Integer;
begin
  Result := '';
  if Gamepad >= 0 then
  begin
    if RayLib.IsGamepadAvailable(Gamepad) then
    begin
      p := RayLib.GetGamepadName(Gamepad);
      if p <> nil then
        Result := string(p);
    end;
  end
  else
  begin
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) then
      begin
        p := RayLib.GetGamepadName(i);
        if p <> nil then
          Exit(string(p));
      end;
    end;
  end;
end;

function GamepadCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to 3 do
  begin
    if RayLib.IsGamepadAvailable(i) then
      Inc(Result);
  end;
end;

function GamepadAxisCount(Gamepad: Integer): Integer;
var
  i: Integer;
begin
  if Gamepad >= 0 then
  begin
    if RayLib.IsGamepadAvailable(Gamepad) then
      Result := RayLib.GetGamepadAxisCount(Gamepad)
    else
      Result := 0;
  end
  else
  begin
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) then
        Exit(RayLib.GetGamepadAxisCount(i));
    end;
    Result := 0;
  end;
end;

function IsGamepadPressed(const ButtonName: string; Gamepad: Integer): Boolean;
var
  Button: TGamepadButton;
  i: Integer;
begin
  if not TryGamepadButtonNameToEnum(ButtonName, Button) then
    Exit(False);
  if Gamepad >= 0 then
    Result := RayLib.IsGamepadAvailable(Gamepad) and RayLib.IsGamepadButtonPressed(Gamepad, Ord(Button))
  else
  begin
    Result := False;
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) and RayLib.IsGamepadButtonPressed(i, Ord(Button)) then
        Exit(True);
    end;
  end;
end;

function IsGamepadDown(const ButtonName: string; Gamepad: Integer): Boolean;
var
  Button: TGamepadButton;
  i: Integer;
begin
  if not TryGamepadButtonNameToEnum(ButtonName, Button) then
    Exit(False);
  if Gamepad >= 0 then
    Result := RayLib.IsGamepadAvailable(Gamepad) and RayLib.IsGamepadButtonDown(Gamepad, Ord(Button))
  else
  begin
    Result := False;
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) and RayLib.IsGamepadButtonDown(i, Ord(Button)) then
        Exit(True);
    end;
  end;
end;

function IsGamepadReleased(const ButtonName: string; Gamepad: Integer): Boolean;
var
  Button: TGamepadButton;
  i: Integer;
begin
  if not TryGamepadButtonNameToEnum(ButtonName, Button) then
    Exit(False);
  if Gamepad >= 0 then
    Result := RayLib.IsGamepadAvailable(Gamepad) and RayLib.IsGamepadButtonReleased(Gamepad, Ord(Button))
  else
  begin
    Result := False;
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) and RayLib.IsGamepadButtonReleased(i, Ord(Button)) then
        Exit(True);
    end;
  end;
end;

function IsGamepadUp(const ButtonName: string; Gamepad: Integer): Boolean;
var
  Button: TGamepadButton;
  i: Integer;
begin
  if not TryGamepadButtonNameToEnum(ButtonName, Button) then
    Exit(False);
  if Gamepad >= 0 then
    Result := RayLib.IsGamepadAvailable(Gamepad) and RayLib.IsGamepadButtonUp(Gamepad, Ord(Button))
  else
  begin
    Result := True;
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) and not RayLib.IsGamepadButtonUp(i, Ord(Button)) then
        Exit(False);
    end;
  end;
end;

function GamepadAxis(const AxisName: string; Gamepad: Integer): Single;
var
  Axis: TGamepadAxis;
  i: Integer;
  val, maxVal: Single;
begin
  if not TryGamepadAxisNameToEnum(AxisName, Axis) then
    Exit(0);
  if Gamepad >= 0 then
  begin
    if not RayLib.IsGamepadAvailable(Gamepad) then
      Exit(0);
    Result := RayLib.GetGamepadAxisMovement(Gamepad, Ord(Axis));
  end
  else
  begin
    maxVal := 0;
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) then
      begin
        val := RayLib.GetGamepadAxisMovement(i, Ord(Axis));
        if Abs(val) > Abs(maxVal) then
          maxVal := val;
      end;
    end;
    Result := maxVal;
  end;
end;

procedure GamepadVibrate(LeftMotor, RightMotor, Duration: Single; Gamepad: Integer);
var
  i: Integer;
begin
  if not Assigned(RayLib.SetGamepadVibration) then
    Exit;
  if Gamepad >= 0 then
  begin
    if RayLib.IsGamepadAvailable(Gamepad) then
      RayLib.SetGamepadVibration(Gamepad, LeftMotor, RightMotor, Duration);
  end
  else
  begin
    for i := 0 to 3 do
    begin
      if RayLib.IsGamepadAvailable(i) then
        RayLib.SetGamepadVibration(i, LeftMotor, RightMotor, Duration);
    end;
  end;
end;

//SDL 2.0.5+ and SDL3 put a CRC16 of the device name in bytes 2-3 of a Windows
//GUID; GLFW leaves those bytes zero and so do the mappings it matches. Zeroing
//the CRC lets a line copied from SDL's controller database work as it is.
function NormalizeGamepadMappings(const Mappings: string): string;
var
  Lines: TStringList;
  i: Integer;
  Line: string;

  function IsHexGuid(const S: string): Boolean;
  var
    j: Integer;
  begin
    Result := Length(S) = 32;
    if not Result then
      Exit;
    for j := 1 to Length(S) do
      case S[j] of
        '0'..'9', 'a'..'f', 'A'..'F': ;
      else
        Exit(False);
      end;
  end;

begin
  Result := Mappings;
  if Pos(',', Mappings) = 0 then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := Mappings;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      //Only mapping lines start with a 32 digit GUID; comments and empty
      //lines are handed to the library untouched.
      if (Length(Line) > 32) and (Line[33] = ',') and IsHexGuid(Copy(Line, 1, 32)) then
        if (Copy(Line, 1, 4) = '0300') and (Copy(Line, 5, 4) <> '0000') then
          Lines[i] := Copy(Line, 1, 4) + '0000' + Copy(Line, 9, MaxInt);
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function SetGamepadMappings(const Mappings: string): Integer;
var
  s: UTF8String;
begin
  Result := 0;
  if not Assigned(RayLib.SetGamepadMappings) then
    Exit;
  s := UTF8String(NormalizeGamepadMappings(Mappings));
  Result := RayLib.SetGamepadMappings(PUTF8Char(PAnsiChar(s)));
end;

function FrameTime: Single;
begin
  Result := RayLib.GetFrameTime;
end;

function TotalTime: Double;
begin
  Result := RayLib.GetTime;
end;

function RandomValue(Min, Max: Integer): Integer;
begin
  Result := RayLib.GetRandomValue(Min, Max);
end;

end.
