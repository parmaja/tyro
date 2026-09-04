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

{$mode delphi}{$H+}

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
  Supported: "left" "right" "middle" }
function MouseButtonNameToEnum(const Name: string): TMouseButton;

{ --- Query functions (thread-safe atomic reads of RayLib internal state) --- }

function IsKeyPressed(const KeyName: string): Boolean;
function IsKeyDown(const KeyName: string): Boolean;
function MouseX: Integer;
function MouseY: Integer;
function IsMouseButtonPressed(const ButtonName: string): Boolean;

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

function MouseButtonNameToEnum(const Name: string): TMouseButton;
var
  n: string;
begin
  n := LowerCase(Trim(Name));
  if n = 'left' then
    Result := MOUSE_BUTTON_LEFT
  else if n = 'right' then
    Result := MOUSE_BUTTON_RIGHT
  else if n = 'middle' then
    Result := MOUSE_BUTTON_MIDDLE
  else
    Result := MOUSE_BUTTON_LEFT;
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
begin
  Result := RayLib.IsMouseButtonPressed(MouseButtonNameToEnum(ButtonName));
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
