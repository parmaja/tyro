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
  Supported names (Xbox layout, PS names in brackets):
    "a" [cross] "b" [circle] "x" [square] "y" [triangle]
    "lb" "rb" "lt" "rt"
    "start" "select" "guide"
    "lstick" "rstick"          (stick clicks)
    "dpad_up" "dpad_down" "dpad_left" "dpad_right"
  Returns False for unknown names rather than silently querying a button. }
function TryGamepadButtonNameToEnum(const Name: string; out Button: TGamepadButton): Boolean;

{ Maps a human-readable gamepad-axis name to a RayLib TGamepadAxis.
  Supported names: "leftx" "lefty" "rightx" "righty" "ltrigger" "rtrigger"
  Returns False for unknown names. }
function TryGamepadAxisNameToEnum(const Name: string; out Axis: TGamepadAxis): Boolean;

{ --- Query functions (thread-safe atomic reads of RayLib internal state) --- }

function IsKeyPressed(const KeyName: string): Boolean;
function IsKeyDown(const KeyName: string): Boolean;
function MouseX: Integer;
function MouseY: Integer;
function IsMouseButtonPressed(const ButtonName: string): Boolean;

{ --- Gamepad queries (Gamepad is the player index, 0 by default) --- }

function GamepadAvailable(Gamepad: Integer = 0): Boolean;
{ Controller name, '' when the gamepad is not connected }
function GamepadName(Gamepad: Integer = 0): string;
function IsGamepadPressed(const ButtonName: string; Gamepad: Integer = 0): Boolean;
function IsGamepadDown(const ButtonName: string; Gamepad: Integer = 0): Boolean;
{ Axis movement -1..1 (triggers 1..-1); 0 for an unknown axis or gamepad }
function GamepadAxis(const AxisName: string; Gamepad: Integer = 0): Single;

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
  if n = 'a' then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_DOWN      // Xbox A / PS cross
  else if n = 'b' then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_RIGHT     // Xbox B / PS circle
  else if n = 'x' then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_LEFT      // Xbox X / PS square
  else if n = 'y' then
    Button := GAMEPAD_BUTTON_RIGHT_FACE_UP        // Xbox Y / PS triangle
  else if n = 'lb' then
    Button := GAMEPAD_BUTTON_LEFT_TRIGGER_1
  else if n = 'rb' then
    Button := GAMEPAD_BUTTON_RIGHT_TRIGGER_1
  else if n = 'lt' then
    Button := GAMEPAD_BUTTON_LEFT_TRIGGER_2
  else if n = 'rt' then
    Button := GAMEPAD_BUTTON_RIGHT_TRIGGER_2
  else if n = 'select' then
    Button := GAMEPAD_BUTTON_MIDDLE_LEFT
  else if n = 'guide' then
    Button := GAMEPAD_BUTTON_MIDDLE
  else if n = 'start' then
    Button := GAMEPAD_BUTTON_MIDDLE_RIGHT
  else if n = 'lstick' then
    Button := GAMEPAD_BUTTON_LEFT_THUMB
  else if n = 'rstick' then
    Button := GAMEPAD_BUTTON_RIGHT_THUMB
  else if n = 'dpad_up' then
    Button := GAMEPAD_BUTTON_LEFT_FACE_UP
  else if n = 'dpad_down' then
    Button := GAMEPAD_BUTTON_LEFT_FACE_DOWN
  else if n = 'dpad_left' then
    Button := GAMEPAD_BUTTON_LEFT_FACE_LEFT
  else if n = 'dpad_right' then
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
  if n = 'leftx' then
    Axis := GAMEPAD_AXIS_LEFT_X
  else if n = 'lefty' then
    Axis := GAMEPAD_AXIS_LEFT_Y
  else if n = 'rightx' then
    Axis := GAMEPAD_AXIS_RIGHT_X
  else if n = 'righty' then
    Axis := GAMEPAD_AXIS_RIGHT_Y
  else if n = 'ltrigger' then
    Axis := GAMEPAD_AXIS_LEFT_TRIGGER
  else if n = 'rtrigger' then
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
begin
  Result := RayLib.IsGamepadAvailable(Gamepad);
end;

function GamepadName(Gamepad: Integer): string;
var
  p: PUTF8Char;
begin
  if not RayLib.IsGamepadAvailable(Gamepad) then
    Exit('');
  p := RayLib.GetGamepadName(Gamepad);
  if p = nil then
    Result := ''
  else
    Result := string(p);
end;

function IsGamepadPressed(const ButtonName: string; Gamepad: Integer): Boolean;
var
  Button: TGamepadButton;
begin
  Result := TryGamepadButtonNameToEnum(ButtonName, Button) and
    RayLib.IsGamepadButtonPressed(Gamepad, Ord(Button));
end;

function IsGamepadDown(const ButtonName: string; Gamepad: Integer): Boolean;
var
  Button: TGamepadButton;
begin
  Result := TryGamepadButtonNameToEnum(ButtonName, Button) and
    RayLib.IsGamepadButtonDown(Gamepad, Ord(Button));
end;

function GamepadAxis(const AxisName: string; Gamepad: Integer): Single;
var
  Axis: TGamepadAxis;
begin
  if not TryGamepadAxisNameToEnum(AxisName, Axis) then
    Exit(0);
  Result := RayLib.GetGamepadAxisMovement(Gamepad, Ord(Axis));
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
