# Input

[<< Tyro docs index](README.md)

Keyboard and mouse input is exposed as the `keyboard`, `mouse` and `gamepad`
tables. Key and button names are strings that map to RayLib key/button enums —
they work identically in Lua and PascalScript.

## Unified input (`input`)

The `input` table mirrors the engine's `TTyroInput` and answers one press edge
from whichever device the player is using. Buttons live here; `game.*` only
holds non-input helpers such as `game.pause()` and `game.difficulty()`.

| API | Keyboard | Gamepad |
|-----|----------|---------|
| `input.pause_pressed()` / `input.pause_down()` | `p` | start, menu |
| `input.start_pressed()` / `input.start_down()` | space, enter | start, menu, a, x, fire |
| `input.fire_pressed()` / `input.fire_down()` | space, x | a, x, b, y, rb, rt, fire, trigger |
| `input.a_pressed()` / `input.a_down()` | space | a |
| `input.b_pressed()` / `input.b_down()` | x | b |
| `input.x_pressed()` / `input.x_down()` | c | x |
| `input.y_pressed()` / `input.y_down()` | v | y |
| `input.l_pressed()` / `input.l_down()` | z | lb, lt |
| `input.r_pressed()` / `input.r_down()` | b | rb, rt |
| `input.select_pressed()` / `input.select_down()` | tab | select, back |

The four direction helpers combine the arrow key or WASD, the gamepad D-pad,
and the left stick crossing its deadzone into one press edge:

```lua
if input.up_pressed() then y = y - 1 end
if input.down_pressed() then y = y + 1 end
if input.left_pressed() then x = x - 1 end
if input.right_pressed() then x = x + 1 end
```

## Keyboard

| API | Description | Examples |
|----------|-------------|----------|
| `keyboard.pressed(key)` | True once when the key transitions from released to pressed | `"space"`, `"w"`, `"f1"` |
| `keyboard.down(key)` | True every frame while the key is held | `"ctrl"`, `"up"`, `"a"` |
| `keyboard.released(key)` | True once on the frame the key is released | `"space"`, `"a"` |
| `keyboard.up(key)` | True every frame while the key is not held | `"a"`, `"shift"` |

Supported key names: letters `a`-`z`, digits `0`-`9`, `space`, `enter`, `tab`,
`escape`, `backspace`, `delete`, `insert`, `up`, `down`, `left`, `right`,
`f1`-`f12`, `shift`, `ctrl`, `alt`, `minus`, `equals` (the `+` key is the
shifted `=`, so ask for `plus` to catch it), `kp_plus`, `kp_minus`.

### Engine shortcuts

These work while the engine is running, whatever has the keyboard focus:

| Key | Description |
|-----|-------------|
| `F1` | Toggle the keyboard shortcuts help panel (lists every shortcut below) |
| `F2` | Toggle the script editor. Opening it stops the script; closing it (also with `ESC`) copies the edited buffer back to the in-memory source. Press `F5` to run it |
| `F3` | Toggle the sprite editor |
| `F4` | Toggle the script picker (`*.tyro` and `*.lua`) |
| `F5` | Rerun the loaded script on a new script thread |
| `F7` | Toggle the output panel |
| `F8` | Toggle the console |
| `CTRL`+`F10` | Toggle the queue/frame statistics panel in the top-left corner |
| `CTRL`+`S` | In the editor: write the source to the script file (then `F5` to run) |

The script that is edited and rerun is the one loaded with `load` or picked with
`F4`; every run gets a fresh copy of its source, so the editor never changes a
running script. A run starts from a clean window: the controls and the sprites the
previous run created are freed and both canvases (board and window) are cleared,
while the console, the output panel, the editor and the `F4` picker stay put.

```lua
window.show()
while true do
    if keyboard.down("w") then print("W is held") end
    if keyboard.pressed("space") then print("Space pressed!") end
    sleep(16)
end
```

## Mouse

| API | Description | Examples |
|----------|-------------|----------|
| `mouse.x` | Current mouse X position | — |
| `mouse.y` | Current mouse Y position | — |
| `mouse.wheel` | Wheel movement this frame, in scroll steps (usually `+1`/`-1`, `0` when nothing scrolled) | — |
| `mouse.pressed([button])` | True once when a mouse button is pressed (default `"left"`) | `"left"`, `"right"`, `"middle"` |
| `mouse.down([button])` | True every frame while the button is held — what drag needs | `"left"` |
| `mouse.released([button])` | True once on the frame a button is released | `"left"` |
| `mouse.up([button])` | True every frame while the button is not held | `"left"` |

```lua
window.show()
while true do
    if mouse.pressed("left") then
        canvas.circle(mouse.x, mouse.y, 5, true)
    end
     sleep(10)
 end
 ```

## Touch

| API | Description | Examples |
|----------|-------------|----------|
| `touch.x`, `touch.y` | Where the pen is: the first finger, or the cursor on a desktop | — |
| `touch.count` | Contacts on the glass right now (0..8) | — |
| `touch.down` / `touch.up` | True while something touches / while nothing does | — |
| `touch.pressed` / `touch.released` | True once on the frame a contact arrives / leaves | — |
| `touch.mouse` | True while the mouse is standing in for a finger | — |
| `touch.point(i)` | The i-th contact (from 1), answers `x, y` or `nil` | `for i = 1, touch.count do` |

On a desktop the mouse stands in as one contact while its left button is
held, so one script answers a tap and a click alike. `on("touch", fn)` fires
`(x, y)` on the press beat. Notes in [`help/touch.md`](../help/touch.md).

```lua
window.show()
function update(dt)
    if touch.pressed then
        canvas.circle(touch.x, touch.y, 8, true)   -- tap or left click
    end
end
```

## Gamepad

| API | Description | Examples |
|----------|-------------|----------|
| `gamepad.available([gamepad])` | True when a gamepad is connected (default pad 0) | — |
| `gamepad.name([gamepad])` | The pad's name, or empty when there is no pad | — |
| `gamepad.pressed(button [, gamepad])` | True once when a pad button transitions to pressed | `"a"`, `"start"`, `"dpad_up"` |
| `gamepad.down(button [, gamepad])` | True every frame while a pad button is held | `"a"`, `"lb"`, `"dpad_left"` |
| `gamepad.released(button [, gamepad])` | True once on the release edge | `"a"` |
| `gamepad.up(button [, gamepad])` | True while the button is not held | `"a"` |
| `gamepad.axis(axis [, gamepad])` | Axis value in -1..1 (0 without a pad) | `"leftx"`, `"lefty"`, `"rightx"`, `"righty"`, `"ltrigger"`, `"rtrigger"` |
| `gamepad.axes([gamepad])` | All axis values as a table | — |
| `gamepad.count()` | Number of connected pads | — |
| `gamepad.vibrate(left, right [, duration [, gamepad]])`, `gamepad.rumble(...)` | Rumble motors (0..1); duration defaults to 0.1 s | — |
| `gamepad.mappings(text)` | Add gamepad mappings in SDL/GLFW format; returns nonzero when accepted | — |

Every `gamepad.*` call also works as `joystick.*` — the two tables are aliases,
so flight-stick scripts can use whichever name reads better.

Supported button names: `a`, `b`, `x`, `y`, `lb`, `rb`, `lt`, `rt`, `start`,
`select`, `guide`, `lstick`, `rstick`, `dpad_up`, `dpad_down`, `dpad_left`,
`dpad_right`.

Xbox and PlayStation pads work out of the box. Generic or no-name pads often
have no built-in mapping, so every button and axis reads as released even
though Windows itself sees the controller. In that case drop a
`gamecontrollerdb.txt` (the SDL controller database) and/or a
`gamepadmappings.txt` next to `tyro.exe`. They hold SDL/GLFW mapping lines
(`<GUID>,<name>,<controls>`, one per controller) that are loaded at startup for
every script; the database is read first, then `gamepadmappings.txt`, so your
own lines override it for the same GUID. `gamepad.mappings(text)` does the same
from a script. A GUID copied from an SDL 2.0.5+ controller database (the form
that carries a `crc:` field) is converted automatically, so its line can be
pasted unchanged.

