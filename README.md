# Tyro

Runing embed programming language in simple graphical environment, for kids and newbies, using [raylib](https://www.raylib.com/) as small game engine to draw.
Currently supports Lua (`.tyro` / `.lua`).
It also should have editing tool inside that environment, console output and input, work in same graphical window.
playing sound using mmf code.

It is More simulating old computer, but with modern languages and graphic.

# Command Line

```
tyro [<script>] [--workpath=<dir>] [<options>]
```

Running a `.tyro`/`.lua` file executes it in a graphical environment. Without a
script argument Tyro starts an interactive console session.

| Option | Description |
|--------|-------------|
| `--help`, `-h` | Show the help page and exit |
| `--list` | List the supported programming languages and exit |
| `--console`, `-c` | Force a command prompt / console output |
| `--debug`, `-d` | Enable debug logging |
| `--lint`, `-l` | Syntax-check the script with Lua and exit without running it (nothing is executed, no window opens) |
| `--main`, `-m` | Legacy compatibility alias; scripts always run on a worker thread with main-thread dispatch |
| `--exit`, `-x` | Exit automatically after the script finishes |
| `--execute`, `-e` | Alias for `--exit` (run the script, then exit) |
| `--show=true/false`, `-s` | Force the window visible/hidden; default keeps script-controlled behavior |
| `--workpath=<path>` | Workspace directory (defaults to the executable location) |

### Exit statuses

| Status | Meaning |
|--------|---------|
| `0` | Success (also `--help`/`--list`, and interactive sessions closed normally) |
| `1` | Lua runtime error while running with `--exit`/`--execute`, or `--lint` found syntax errors |
| `2` | Invalid command line usage: unknown option, `--lint` without a script file, or an invalid `--show` value |

# Lua Example

```lua
    canvas.text(10, 30, 'Printing text test')
    i = 1000000
    canvas.color = colors.black
    canvas.line(0, 100, canvas.width, 100)

    while i > 0 do
        c = math.random(3, colors.count)
        canvas.color = colors[c]
        r = math.random(5, 20) --size of circle
        x = math.random(640)
        y = math.random(480)
        canvas.circle(x, y, r, true)
        r = math.random(5, 20) --size of circle
        x = math.random(640)
        y = math.random(480)
        canvas.rectangle(x, y, r, r, true)
        sleep(10)
        i = i - 1
    end
```

The canvas primitives are `canvas.clear()`, `canvas.text(x, y, s)`,
`canvas.rectangle(x, y, w, h [, fill])`, `canvas.circle(x, y, r [, fill])`,
`canvas.triangle(x1, y1, x2, y2, x3, y3 [, fill])`,
`canvas.line(x1, y1, x2, y2 [, thickness])` and `canvas.point(x, y)`. Set the
color with `canvas.color = colors.red` (and `canvas.backcolor` / `canvas.clear()`
for the background). `canvas.pensize` (alias `canvas.linewidth`) sets the
thickness of lines and of rectangle/triangle outlines; `canvas.line` also takes
a per-call thickness. `textwidth(s)` and `textheight()` measure the built-in
font, and `canvas.width` / `canvas.height` report the canvas size.

For sound, `music.beep()`, `music.sound(freq, ms)` or `music.sound(wave, freq,
ms)`, `music.noise(ms)` (white) / `music.noise(freq, ms)` (pitched) and
`music.mml(...)` play one-shot effects and tunes. For a sound that has to run
without gaps—an engine drone, a siren, a wind loop—use `sound.loopPlay(freq
[, volume])` or `sound.loopPlay(wave, freq [, volume])`. It keeps looping until
`sound.loopStop()`, and `sound.loopPitch(freq)` / `sound.loopVolume(0..1)` retune
it while it plays. Available `wave` names include `Sin`, `Square`, `Saw`, `Tri`,
`Organ`, `FM`, `Noise`, `Pink`, `Engine` and the drum waveforms `Kick`, `Snare`
and `HiHat`.

For a one-shot sound effect from an audio file, `sound.play("file.wav" [, volume])`
loads the file (raylib `LoadSound`, `.wav`) and plays it once. The file is resolved
like every other script asset (next to the script, the workspace, the app folder);
a missing file is ignored, not an error.

# Input

Keyboard and mouse input can be queried from script. Key and button names are
strings that map to RayLib key/button enums — they work identically in Lua and
PascalScript.

## Keyboard

| Function | Description | Examples |
|----------|-------------|----------|
| `iskeypressed(key)` | True once when the key transitions from released to pressed | `"space"`, `"w"`, `"f1"` |
| `iskeydown(key)` | True every frame while the key is held | `"ctrl"`, `"up"`, `"a"` |

Supported key names: letters `a`-`z`, digits `0`-`9`, `space`, `enter`, `tab`,
`escape`, `backspace`, `delete`, `insert`, `up`, `down`, `left`, `right`,
`f1`-`f12`, `shift`, `ctrl`, `alt`.

### Engine shortcuts

These work while the engine is running, whatever has the keyboard focus:

| Key | Description |
|-----|-------------|
| `F2` | Toggle the script editor. Opening it stops the script; closing it (also with `ESC`) saves the edited source back to the script and runs it |
| `F4` | Toggle the script picker (`*.tyro` and `*.lua`) |
| `F5` | Rerun the loaded script on a new script thread |
| `F7` | Toggle the output panel |
| `F8` | Toggle the console |
| `CTRL`+`F10` | Toggle the queue/frame statistics panel in the top-left corner |
| `CTRL`+`S` | In the editor: save the source back and rerun the script |

The script that is edited and rerun is the one loaded with `load` or picked with
`F4`; every run gets a fresh copy of its source, so the editor never changes a
running script. A run starts from a clean window: the controls and the sprites the
previous run created are freed and both canvases (board and window) are cleared,
while the console, the output panel, the editor and the `F4` picker stay put.

```lua
window.show()
while true do
    if iskeydown("w") then print("W is held") end
    if iskeypressed("space") then print("Space pressed!") end
    sleep(16)
end
```

## Mouse

| Function | Description | Examples |
|----------|-------------|----------|
| `mousex()` | Current mouse X position | — |
| `mousey()` | Current mouse Y position | — |
| `ismousepressed(button)` | True once when a mouse button is pressed | `"left"`, `"right"`, `"middle"` |

```lua
window.show()
while true do
    if ismousepressed("left") then
        canvas.circle(mousex(), mousey(), 5, true)
    end
     sleep(10)
 end
 ```

## Gamepad

| Function | Description | Examples |
|----------|-------------|----------|
| `gamepadavailable([gamepad])` | True when a gamepad is connected (default pad 0) | — |
| `gamepadname([gamepad])` | The pad's name, or empty when there is no pad | — |
| `isgamepadpressed(button [, gamepad])` | True once when a pad button transitions to pressed | `"a"`, `"start"`, `"dpad_up"` |
| `isgamepaddown(button [, gamepad])` | True every frame while a pad button is held | `"a"`, `"lb"`, `"dpad_left"` |
| `gamepadaxis(axis [, gamepad])` | Axis value in -1..1 (0 without a pad) | `"leftx"`, `"lefty"`, `"rightx"`, `"righty"`, `"ltrigger"`, `"rtrigger"` |

Supported button names: `a`, `b`, `x`, `y`, `lb`, `rb`, `lt`, `rt`, `start`,
`select`, `guide`, `lstick`, `rstick`, `dpad_up`, `dpad_down`, `dpad_left`,
`dpad_right`.

# Files

Scripts have no direct file system access: the Lua `io` and `os` libraries are
removed from the sandbox, so `io.open` and friends do not exist. `openfile` is
the only way in, and it hands out a handle only for a file that resolves inside
the workspace.

```lua
local f, err = openfile("scores.txt", "w")   -- relative names land next to the script
if not f then
    println("cannot write scores.txt: " .. err)
    return
end
f:write("hello\n")
f:close()
```

`openfile(name [, mode])` returns a handle, or `nil` plus a message — it never
raises an error. `mode` takes the `io.open` letters: `r` (default), `w`, `a`,
`x` and `+`, with `b` for binary. Without `b` the handle is a text handle and
translates `\n` to `\r\n` on the way out and back on the way in.

| Function | Description |
|----------|-------------|
| `openfile(name [, mode])` | Open a workspace file; returns the handle, or `nil` plus a message |
| `f:read([what])` | `"l"` a line, `"L"` a line keeping its newline, `"a"` what is left, `"n"` the next number, or a byte count. Returns `nil` at the end of the file. |
| `f:write(...)` | Write the arguments, returns `f` so calls chain. A leading number writes only that many bytes of the string after it. |
| `f:lines()` | Iterate the lines that are left: `for line in f:lines() do ... end` |
| `f:flush()` | Push anything buffered to the disk, returns `f` so calls chain |
| `f:close()` | Close the handle and return `true`; further reads and writes report the file as closed |

The name is expanded before it is checked, so `..`, a drive-relative form or a
UNC path cannot walk out of the workspace:

```lua
openfile("../secrets.txt")          --> nil, "resolves outside the workspace"
openfile("sub/../data.txt")         --> fine, it stays inside
```

See `tests/test_openfile.tyro` for a runnable check of all of it.

# Network

Scripts can talk to web APIs with `http_get` and read the answer with
`json_decode`. The request runs on the script thread, so the render loop keeps
drawing while the call is in flight; the script itself blocks until the answer
(or the failure) arrives.

| Function | Description |
|----------|-------------|
| `http_get(url)` | Fetch a URL over HTTP/HTTPS; returns the body, or `nil` plus a message |
| `json_decode(text)` | Parse a JSON document into the matching Lua value: object→table (keyed by member name), array→table (1..n), number, boolean, `null`→`nil`; returns `nil` plus the parser message on invalid input |

```lua
local body, err = http_get("https://ipapi.co/json/")   -- IP-based geolocation
if body then
  local geo = json_decode(body)
  println("you are in " .. geo.city .. ", " .. geo.country_code)
end
```

`json_decode` answers `nil` plus a message for a bare top-level number or
boolean (`"42"`, `"false"`) — the underlying parser reads objects, arrays and
strings. See `games/weather/weather.tyro` for a full app built on both
functions, and `tests/test_http_json.tyro` for a runnable check.

# Console

## Built-in Terminal Commands

When the graphical console is visible, you can type commands at the `> ` prompt.
The following commands are built in:

| Command | Description |
|---------|-------------|
| `dir`, `list`, `ls` | List files in the current directory |
| `clear`, `cls` | Clear the console output |
| `help`, `?` | Show available commands |
| `load <script>` | Load a script from the current directory (F4 to pick one) |
| `run` | Run the loaded script (like `F5`; reruns it when it is already running) |
| `stop` | Stop the running script |
| `state` | Show which script is loaded and whether it is running |
| `edit` | Open the script editor (like `F2`) |
| `exit`, `quit` | Hide the console and stop the engine |
| `ESC` | Hide the console (keybinding) |

Example:
```
tyro demos/terminal_demo.lua
# then type "dir" at the console prompt
```

Holding a key auto-repeats it at the prompt: typed characters, `BACKSPACE`,
`DELETE`, the arrow keys and `HOME`/`END` keep repeating after a short delay.
`ENTER`, `ESC` and `CTRL` shortcuts stay one-shot.

Text at the prompt can be selected with `SHIFT`+`LEFT`/`RIGHT`/`HOME`/`END` (a
plain arrow press jumps to the edge of the selection), with `CTRL`+`A`, or by
dragging with the mouse. Typing, `BACKSPACE`, `DELETE` or pasting replaces the
selection, and it is copied/pasted with `CTRL`+`C` / `CTRL`+`V` or with the
traditional console keys `CTRL`+`INSERT` / `SHIFT`+`INSERT`.

### Console API

| Function | Description |
|----------|-------------|
| `console.show([w, h])` | Show the console (optionally sized in characters) |
| `console.print(text)` | Print text to the console (no newline) |
| `console.println(text)` | Print text to the console (with newline) |
| `console.read([prompt])` | **Block** and read a line of input from the user |
| `console.active` | Read-only boolean — `true` while the console is visible |

### Interactive Reading (console.read)

`console.read()` displays a prompt on the console and blocks the script until
the user types a line and presses Enter. It returns the typed string.

```lua
console.show()
local name = console.read("Your name? ")
println("Hello, " .. name .. "!")
```

See `demos/terminal_demo.tyro` and `demos/console_read_demo.tyro` for examples.

# Sprites

The `Sprites` system manages textured images ("sprites") that the engine draws
every frame. Each sprite is created with `Sprites.new`, loaded with `load`, and
positioned with `move` or by setting `x`/`y` properties.

## Creating & Loading

| Function | Description |
|----------|-------------|
| `Sprites.new("name"?)` | Create a new sprite. Optional name registers it for lookup. |
| `Sprites("name")` | Look up a sprite by name (returns the sprite object or `nil`). |
| `Sprites.find("name")` | Same as above. |

## Sprite Methods

| Method | Description |
|--------|-------------|
| `sprite:load("image.png")` | Load a texture from file into this sprite. `.aseprite`/`.ase` files load every frame as an animation. `.sprite` files (a ZIP holding a PNG texture + a TexturePacker/Phaser/PixiJS JSON atlas) load every atlas frame as an animation frame. |
| `sprite:show()` | Make the sprite visible (shown by default). |
| `sprite:hide()` | Hide the sprite from rendering. |
| `sprite:move(x, y)` | Set the sprite's position. |
| `sprite:width()` | Return the texture width in pixels. |
| `sprite:height()` | Return the texture height in pixels. |
| `sprite:play([fps])` | Restart and play the animation. `fps` overrides the per-frame timings stored in the file (0 or omitted keeps them — needed because Aseprite files often have ~1 ms delays). |
| `sprite:stop()` | Freeze the animation on the current frame (aliased by `sprite:pause()`). |
| `sprite:framecount()` | Number of frames in the loaded `.aseprite`/`.sprite` animation. |

## Sprite Properties

| Property | Type | Description |
|----------|------|-------------|
| `sprite.x` | `number` | X position (read/write). |
| `sprite.y` | `number` | Y position (read/write). |
| `sprite.angle` | `number` | Rotation in degrees (read/write). |
| `sprite.scale` | `number` | Scale factor, 1.0 = original size (read/write). |
| `sprite.visible` | `boolean` | Whether the sprite is drawn (read/write). |
| `sprite.frames` | `number` | Number of animation frames (read-only). |
| `sprite.frame` | `number` | Current animation frame, 0-based (read/write). |
| `sprite.playing` | `boolean` | Whether the animation is running (read/write). |
| `sprite.speed` | `number` | Animation speed in frames-per-second; 0 = use the file timings (read/write). |
| `sprite.looping` | `boolean` | Whether the animation loops when it reaches the last frame (read/write). |
| `sprite.flipH` | `boolean` | Mirror the texture horizontally (read/write). |
| `sprite.flipV` | `boolean` | Mirror the texture vertically (read/write). |

Image files are searched in the script directory, the workspace `sprites/`
folder, and the current directory.

```lua
window.show(640, 480)
canvas.color(colors.black)
canvas.clear()

mysprite1 = Sprites.new("player")
mysprite1.load("richard-say.png")
mysprite1.show()
mysprite1.move(100, 200)

mysprite2 = Sprites.new()
mysprite2.load("richard-say.png")
mysprite2.move(300, 100)
mysprite2.angle = 45
mysprite2.scale = 0.5

-- Look up by name
local p = Sprites("player")
println("Sprite size: " .. p.width() .. "x" .. p.height())

local a = 0
while true do
    canvas.color(colors.black)
    canvas.clear()

    -- Engine draws all sprites automatically; just update properties
    mysprite1.move(mousex(), mousey())
    mysprite1.angle = a

    a = a + 1
    if a >= 360 then a = 0 end

    sleep(16)
end
```

# Tilemaps

The `tiles` system draws a rectangular grid of tiles from a single tileset
texture. A tilemap is created with `tiles.new`, loaded with `load`, filled with
`fill`/`set`, and drawn with `draw`. Both the dot form (`map.set(...)`) and the
colon form (`map:set(...)`) work.

Tile ids are **1-based**: id `1` is the top-left tile of the sheet, id `12` the
last tile of the first row of a 12-column sheet, and so on. Id `0` means an
empty cell, which is skipped when drawing (so a second, empty map can be used as
an overlay layer).

## Creating & Loading

| Function | Description |
|----------|-------------|
| `tiles.new("name"?)` | Create a new tilemap. The optional name registers it for lookup. |
| `tiles.find("name")` | Look up a tilemap by name (returns the map object or `nil`). |

A named tilemap is also exposed as a global, so after
`tiles.new("terrain")` you can write `terrain.set(...)` directly.

## Tilemap Methods

| Method | Description |
|--------|-------------|
| `map.load(file[, tilew, tileh, columns, spacing, margin])` | Load (or replace) the tileset texture. Defaults: `16, 16, 0, 0, 0`. Leave `columns` at `0` to derive it from the texture width. |
| `map.resize(cols, rows)` | Resize the grid, keeping the overlapping cells. |
| `map.fill(tile)` | Set every cell to `tile` (`0` clears the map). |
| `map.clear()` | Empty every cell. |
| `map.set(col, row, tile)` | Set one cell (out-of-range coordinates are ignored). |
| `map.get(col, row)` | Return the tile id of one cell (`0` when empty or out of range). |
| `map.data()` | Return a flat array of tile ids, row-major (`index = row * cols + col + 1`). |
| `map.solidtile(tile [, solid])` | Read, or set, whether a tile id is a wall. With `solid` it writes and returns nothing; without it returns a boolean. |
| `map.solid(col, row)` | `true` when the cell is a solid tile, or is outside the map. |
| `map.draw([x, y [, scale]])` | Draw the map (enqueues one native draw command). Position defaults to the map's own `x`/`y`, scale to `scale`. |

`draw` captures a snapshot of the grid at the moment it is called, so a later
`set`/`clear` does not affect a draw already queued this frame. Re-stamp an
entity layer every frame and call `draw` after the changes.

## Tilemap Properties

| Property | Type | Description |
|----------|------|-------------|
| `map.cols` | `number` | Grid width in tiles (read-only). |
| `map.rows` | `number` | Grid height in tiles (read-only). |
| `map.width` | `number` | Pixel width, `cols * tilewidth` (read-only). |
| `map.height` | `number` | Pixel height, `rows * tileheight` (read-only). |
| `map.tilewidth` | `number` | Tile width in the sheet (read-only). |
| `map.tileheight` | `number` | Tile height in the sheet (read-only). |
| `map.columns` | `number` | Tiles per row in the sheet (read-only). |
| `map.spacing` | `number` | Pixels between tiles in the sheet (read-only). |
| `map.margin` | `number` | Pixels from the sheet edge to the first tile (read-only). |
| `map.visible` | `boolean` | Whether the map is drawn (read/write). |
| `map.x` | `number` | Draw X position (read/write). |
| `map.y` | `number` | Draw Y position (read/write). |
| `map.scale` | `number` | Draw scale, 1.0 = original size (read/write). |
| `map.name` | `string` | The map's name (read/write). |

```lua
window.show(320, 240)

-- Kenney "Tiny Dungeon": 16x16 tiles, 1px spacing, 12 columns
local map = tiles.new("ground")
map.load("tiles.png", 16, 16, 0, 1, 0)  -- columns derived from the texture
map.resize(20, 15)
map.fill(49)                             -- 49 = a floor tile
map.solidtile(41, true)                  -- 41 = a wall tile

-- draw a wall border
for c = 0, map.cols - 1 do
  map.set(c, 0, 41)
  map.set(c, map.rows - 1, 41)
end

while true do
  canvas.color(colors.black)
  canvas.clear()
  map.draw(0, 0, 2)                      -- scale 2 => 32px tiles
end
```

See `games/dungeon/dungeon.tyro` for a small game built on this API (a static
terrain map plus a second map used as an entity overlay).

# Shader Effects

Post-processing screen shaders are controlled with the `shader` table. They are
built-in and need no files. Assign properties the usual way:

```lua
shader.effect = "water"          -- pick the effect
shader.value  = 0.5              -- 0..1 effect parameter
shader.area   = {x, y, w, h}     -- restrict the effect to a rectangle
```

| Property | Description |
|----------|-------------|
| `shader.effect` | `"water"`, `"glow"`, `"gray"`, `"sepia"`, `"invert"`, `"vignette"`, `"pixelate"` or `"none"` |
| `shader.value` | `0..1` effect parameter (see per-effect meaning below) |
| `shader.area` | Rectangle `{x, y, w, h}` in canvas pixels (`y` from the top) where the effect applies; outside it the canvas is unchanged |
| `shader.load(file)` | Load a custom fragment shader from a GLSL file and activate it (see below) |

| Effect | What it does | `value` meaning (default) |
|--------|--------------|---------------------------|
| `"water"` | Wavy water with foam across the lower region | water surface height (0.34) |
| `"glow"` | Soft glow around bright pixels (suns, bulbs, lasers) | height of the glowing region from the bottom (1.0 = full) |
| `"gray"` | Grayscale | gray strength (1.0) |
| `"sepia"` | Old-photo look | sepia strength (1.0) |
| `"invert"` | Inverted colors | inversion strength (1.0) |
| `"vignette"` | Darkened corners | vignette amount (0.5) |
| `"pixelate"` | Chunky pixel blocks | block size (0.06 ~ 1px) |

```lua
window.show(640, 480)
shader.effect = "glow"           -- turn it on
shader.value  = 0.5              -- glow on the bottom half

-- draw something bright, it will glow
canvas.color = colors.yellow
canvas.circle(320, 240, 60, true)
```

`shader.value` and `shader.area` survive effect switches, so once set they apply
to whichever effect is active. Toggle effects and tweak the parameters at any
time:

```lua
if iskeypressed("1") then shader.effect = "water" end
if iskeypressed("2") then shader.effect = "glow" end
if iskeypressed("0") then shader.effect = "none" end
if iskeypressed("q") then shader.value = shader.value - 0.1 end
if iskeypressed("e") then shader.value = shader.value + 0.1 end
if iskeypressed("z") then shader.area = {0, canvas.height/2, canvas.width, canvas.height/2} end
```

The `"water"` effect tints and distorts the lower part of the canvas, so draw a
shore or sea bottom there if you want a sea scene. See `demos/shader_demo.tyro`
for a full on-screen demo of every effect, `value` and `area`.

## Custom shaders

`shader.load("file.frag")` loads a **fragment** shader from a file and activates
it. The file is looked up next to the script, then in the current directory, then
in `assets/` under the workspace. It must be GLSL `#version 330` with the same
outputs as the built-in effects:

```glsl
#version 330
uniform vec2  resolution;                 // canvas size in pixels
uniform float time;                       // elapsed seconds
uniform float value;                      // shader.value (0..1)
uniform vec4  area;                       // shader.area {x, y, w, h} (y from the bottom)
uniform sampler2D texture0;               // the canvas
uniform vec4 colDiffuse;                  // vertex color
in vec2 fragTexCoord;
in vec4 fragColor;
out vec4 finalColor;
```

All uniforms are optional: use the ones you need and ignore the rest. `shader.value`
and `shader.area` still control `value`/`area`. Set `shader.effect = "none"` to go
back to the built-in effects. See `demos/custom.frag` for a working example:

```lua
shader.load("custom.frag")    -- activate the scanlines shader
shader.value = 0.8
shader.area  = {0, 0, canvas.width, canvas.height}
```

# Screen Shake

`shake` jolts the world, the way an accident or an error should feel. Every
frame the canvas and the sprites are moved by a random offset that fades out
until the time is up; the terminal and the other controls stay glued to the
window.

| Function | Description |
|----------|-------------|
| `shake(ms [, power])` | Shake the world for *ms* milliseconds; `power` is the maximum offset in pixels (10 by default) |
| `window.shake(ms [, power])` | The same function on the `window` table |
| `window.shaking` | Read-only boolean — `true` while a shake is still running |
| `shake(0)` | Stop a running shake |

A new call restarts the shake, so a longer or harder one simply wins. See
`demos/shake_demo.tyro`.

```lua
window.show(640, 480)

-- an accident: a long, hard jolt
shake(800, 40)

while cycle do
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.color = colors.red
    canvas.circle(300, 240, 30, true)

    if iskeypressed("space") then
        window.shake(400)  -- 400ms with the default power
    end
end
```

# Controls

Controls are created by class name and are owned by the control tree. Panels are
containers, so a control can be nested inside another control with
`controls.parent`.

```lua
panel = controls.new("panel", "", 0, 0, 240, 300)
controls.align(panel, "left")

top = controls.new("button", "Top", 0, 0, 180, 40)
controls.parent(top, panel)
controls.align(top, "top")

bottom = controls.new("button", "Bottom", 0, 0, 180, 40)
controls.parent(bottom, panel)
controls.align(bottom, "bottom")
```

A handle is also a property bag, so every row above that reads or writes a value
has a field spelling on the handle itself. The field is the value - never a
function - so these are interchangeable:

```lua
local b = controls.new("button", "Hi", 10, 10, 120, 32)

b.caption = "Bye"        -- controls.caption(b, "Bye")
b.width = 200            -- controls.width(b, 200)
b.align = "left"         -- controls.align(b, "left")
b.visible = false        -- controls.visible(b, false)
b.parent = panel         -- controls.parent(b, panel)

local w, h = b.width, b.height
```

| Field | Type | Notes |
|-------|------|-------|
| `x` / `left`, `y` / `top`, `width`, `height` | integer | Preferred geometry; writing one of them keeps the others |
| `visible`, `name`, `border`, `backcolor`, `align`, `parent` | | Same values as the functions above |
| `text` / `caption` | string | The caption of a button, label, or checkbox; the edited text of an edit |
| `hover`, `down`, `clicked`, `focused` | boolean | Read-only mouse and keyboard state |
| `checked` | boolean | A checkbox only, `false` on anything else |
| `items`, `itemindex`, `viewcount` | integer | A `listbox` only; `lst.items = {"a", "b"}` replaces the list, a single string replaces it with one item and `nil` empties it |
| `loaded` | boolean | An `image` only; `img.file = "logo.png"` loads the texture |

The names that name an action stay methods on the handle and take a colon call,
because a Lua key cannot be both a value and a function:
`lst:additem("a")`, `lst:clear()`, `lst:item(0)`, `b:show()`, `b:hide()`,
`b:move(x, y)`, `b:position(x, y)`, `b:focus()`, `img:load("logo.png")`. Each one
also takes the handle as its first argument on the table: `controls.additem(lst, "a")`.

| Function | Description |
|----------|-------------|
| `controls.new(class, caption, x, y, w, h, name?)` | Create a `button`, `panel`, `label`, `checkbox`, `edit`, `spectrum`, `listbox`, or `image`; returns a handle |
| `controls.align(handle [, value])` | Get/set `none`, `left`, `top`, `right`, `bottom`, or `client` |
| `controls.parent(handle [, parentHandle])` | Get/set the container; the getter returns `0` for the main window, and `nil`/`0` moves the control there |
| `controls.width/height(handle [, value])` | Get/set the preferred size |
| `controls.position/move(handle, x, y)` | Get/set the preferred position |
| `controls.text/caption(handle [, value])` | Get/set a caption, label, or edit value |
| `controls.visible/show/hide(handle)` | Show or hide a control; hidden aligned controls release their space |
| `controls.hover/down/clicked(handle)` | Read mouse state |
| `controls.checked(handle [, value])` | Get/set the checked state of a checkbox |
| `controls.border(handle [, style])` | `0` none, `1` thin, `2` thick, `3` sizable |
| `controls.backcolor(handle [, color])` | Set a color from the `colors` table |
| `controls.load(handle, file)` | Load the texture of an `image`; answers `true` when the file produced one |
| `controls.items(handle [, item...])` | Replace the items of a `listbox` (also one table of strings); with no argument it returns the count |
| `controls.item(handle, index [, text])` | Get/set a single item, `nil` outside the list |
| `controls.additem(handle, text)` / `controls.clear(handle)` | Append an item / remove them all |
| `controls.itemindex(handle [, index])` / `controls.viewcount(handle [, rows])` | Get/set the selected row (`-1` is none) / the visible rows (`0` is the control size) |

`BoundsRect` stores a control's preferred position and size. Docking changes only
its effective `WindowRect`, so `controls.width`, `controls.height`, and
`controls.position` keep reporting the preferred geometry. Changing a child's
size realigns its siblings, and resizing the main window propagates through
every container level. See `demos/controls_align.tyro` for a left-docked panel
with top- and bottom-docked buttons.

## Image controls

`image` is a control that shows a texture. Its caption argument is the file to
show, so the common case is one line:

```lua
logo = controls.new("image", "logo.png", 40, 40, 128, 128, "logo")
```

The texture is loaded on the main thread, and a texture larger than the requested
size grows the control. The file is looked up like every other script asset
(work path, then `assets`, then the app folder).

An existing control can load another file at any time, and reports whether it
holds a texture:

```lua
controls.load(logo, "other.png")  -- or logo.load("other.png"), or logo.file = "other.png"
if not logo.loaded then
    print("logo.png could not be read")
end
logo.load("")                     -- release the texture
```

`controls.load` answers `true`/`false` for the file it just read, and `false`
for a control that is not an `image`.

# Timing

| Function | Returns | Description |
|----------|---------|-------------|
| `sleep(ms)` | — | Pause the script thread for *ms* milliseconds |
| `pause(ms)` | — | Like `sleep(ms)`, but also **freeze the scene** while the script waits: physics, sprite animation/`on_update` and controls stop advancing, so the frame the script queued stays on screen. Audio (music/radio) and input keep running. Use it for a still hold; use `sleep()` when the scene should keep moving |
| `cycle` | `true` | **Block** until the next drawing frame completes. Use `while cycle do` (instead of `while true do`) to run the loop body at most once per drawn frame, so drawing commands cannot pile up inside a single raylib drawing cycle |
| `enter()` | — | **Hold** the processing queue: the drawing commands queued after this call are not executed until `leave()` balances it |
| `leave()` | — | Release the hold taken by `enter()`: the whole group queued in between is then drawn at one drawing cycle |
| `autogroup(on)` | — | Turn automatic per-`cycle` grouping on (`true`, default) or off (`false`) |
| `frametime()` | `number` (seconds) | Time elapsed since the last frame |
| `time()` | `number` (seconds) | Elapsed time since the window was created |
| `millis()` | `number` (ms) | High-resolution monotonic clock in milliseconds (the same timer the frame budget uses); fine enough to time a `while cycle do` body |
| `rand(min, max)` | `integer` | Random integer in [min, max] |

`cycle` is a frame gate: reading it pauses the script thread until the main
loop has presented the next drawing frame (`EndDrawing`). It never ends on its
own (`while cycle do` runs forever), and it only waits when a window is being
drawn — scripts running on the main thread (e.g. console lines) never block.

```lua
window.show()
while cycle do                      -- one iteration per drawing frame
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.circle(320, 240, 20, true)
end                                 -- no sleep() needed for ~60 FPS pacing
```

## Automatic grouping (default)

Each `while cycle do` iteration is drawn as one group automatically. From the
moment a body starts until the next yield point (the next `cycle`, `sleep()`,
`pause()`, or `console.read()`), the queue is held, so a body that takes several
frames to build still lands in a single drawing cycle. This prevents the "black
box, then the text" flicker with no extra code:

```lua
window.show()
while cycle do
    canvas.color = colors.black
    canvas.rectangle(0, 0, 640, 70, true)   -- the box ...
    canvas.color = colors.green
    canvas.text(8, 8, "Station : " .. station) -- ... and its text arrive together
end
```

The hold is released before `sleep()`/`pause()`/`console.read()` so a prompt or
progress drawn earlier still shows while the script waits. Call
`autogroup(false)` for progressive drawing that must stream across frames.

For sleep-based pacing you can still use the classic loop:

```lua
window.show()
start = time()
while true do
    ft = frametime()
    canvas.text(10, 10, "frame: " .. ft)
    sleep(16)   -- ~60 FPS pacing
end
print("uptime: " .. (time() - start))
```

Use `pause(ms)` instead of `sleep(ms)` for a still hold: it also freezes the
scene (physics, sprites, controls) for the duration while music/radio keep
playing.

```lua
canvas.text(10, 10, "Paused...")
pause(1000)                       -- the text stays on screen, the scene is still
canvas.text(10, 30, "Back!")
```

## Grouping drawing with enter() / leave()

`enter()` locks the processing queue: the drawing commands the script queues
after the call keep piling up but are not executed by the main loop. `leave()`
releases the lock, and the whole group is then drawn at one drawing cycle —
instead of a large batch being split over several frames. Commands queued
outside the group behave as usual. This is automatic for `while cycle` bodies
(see *Automatic grouping* above); use it explicitly to group part of an
iteration, or a body outside a cycle loop.

```lua
window.show()
while cycle do
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)

    enter()                             -- hold the queue
    for i = 0, 99 do                    -- this batch is drawn as one
        canvas.circle(rand(0, canvas.width), rand(0, canvas.height), 4, true)
    end
    leave()                             -- draw the whole group now

    canvas.text(10, 10, "frame: " .. time())
end
```

`enter()`/`leave()` nest (they count up and down), and a script that stops or
errors between them is released automatically so the next run never inherits a
frozen queue.

## Measuring the queue (queue.*)

The engine exposes read-only counters to help diagnose the "black box, then the
text" flicker, where a frame is presented with only part of a drawing body.

These counters are diagnostics only, and are compiled only in debug builds (the
`diagnose` conditional tracks the debug switch). A release build reports 0 for
the diagnostic-only ones; `queue.consumed`, `queue.depth`, `queue.holds`,
`queue.group` and `queue.limit` always stay live.

The same fields can be watched live, without a script, by pressing `CTRL`+`F10`:
the engine draws a compact panel in the window's top-left corner, showing every
counter below (plus the current `fps`) and refreshed each frame. Press it again
to hide the panel. Only the counters above the `diagnose` line are populated in
a release build.

| Field | Description |
|-------|-------------|
| `queue.consumed` | Drawing commands executed since the engine started (grows on every drain) |
| `queue.passes` | `ProcessQueue` passes that executed at least one command |
| `queue.deferred` | Passes stopped at the frame time budget while more commands were still queued (each one drew a partial batch) |
| `queue.depth` | Commands waiting in the queue right now |
| `queue.max` | Largest backlog seen at the start of a pass (a high-water mark) |
| `queue.holds` | Open `enter()` calls (the hold depth) |
| `queue.frames` | Drawing cycles presented since the engine started |
| `queue.group` | `true` while automatic per-`cycle` grouping is enabled |
| `queue.drainms` | Duration of the most recent drain pass, in milliseconds |
| `queue.lastgap` | Milliseconds between the last two queued commands (no yield in between) |
| `queue.maxgap` | Largest such gap of the run: the script stalled between two queue calls |

Because the script thread builds a body while the main loop drains the queue on
its own thread, a frame boundary can fall in the middle of a body. The script
cannot sample at that instant (it is not blocked on `cycle` then), but it can
catch it right after the body returns, before the next `cycle`:

```lua
while cycle do
    local before = queue.consumed      -- queue is empty here
    enter()                            -- or queue the body bare
    n = draw_body()
    leave()
    local mid = queue.consumed - before -- drained while the body was built
    -- 0 < mid < n  ->  a frame showed only part of the body (the flicker)
end
```

The same trick with `millis()` times how long the body takes to queue:

```lua
local t0 = millis()
n = draw_body()
print("body queued in " .. (millis() - t0) .. " ms")
```

Grouping makes the body land in one drawing cycle, but it cannot fix the script
itself taking a long time between two commands (a `radio.station` getter that
blocks on a lock the network thread holds, file I/O, ...). The engine stamps
every queued command with its enqueue time, so `queue.maxgap` reports the
largest time the script spent between two consecutive queue calls with no yield
in between — exactly the gap between a `rectangle()` and the `text()` that is
meant to cover it:

```lua
canvas.rectangle(0, 0, 640, 70, true)   -- opaque black box
-- radio.station blocks here for a second; the box alone cannot be shown yet,
-- but it will be the whole group is held, so the frame freezes instead
canvas.text(8, 8, "Station : " .. radio.station)
print("stall between box and text: " .. queue.maxgap .. " ms")
```

Wait times that *are* yields (`cycle`, `sleep()`, `console.read()`) reset the
baseline, so `maxgap` does not count frame waits: a small `maxgap` with a
black-only header means the metadata itself is empty (buffering/reconnect), not
a rendering race.

To be told when a stall happens, watch the gap from the script (used by
`demos/radio.tyro`). The first gap that clears the limit is printed to the
console; a new report needs a gap a full limit above the last one reported, so
drift never spams:

```lua
local gap_warned = 0
local function gapwatch(aLimitMS)
  local g = queue.maxgap
  if g > aLimitMS and g >= gap_warned + aLimitMS then
    gap_warned = g
    println("GAP: " .. g .. " ms between two queued commands"
      .. " (limit " .. aLimitMS .. " ms, lastgap " .. queue.lastgap .. " ms)")
  end
  return g
end

while cycle do
    -- ...draw the body...
    gapwatch(250)   -- >250 ms stall? the console says so
end
```

Wrapping the body in `enter()`/`leave()` holds the queue, so the main loop
cannot drain mid-body and no partial frame can appear.

# Examples

See [demos/README.md](demos/README.md) for the full demo index.
Run any demo with:

```
tyro demos/<name>.tyro
```

| File | Feature |
|------|---------|
| `demos/pong.tyro` | Complete Pong game — drawing, keyboard input, AI, physics, collision, sound, scoring |
| `demos/basic_drawing.tyro` | All drawing primitives: rectangle, circle, line, point, text, colors |
| `demos/animated_demo.tyro` | Animation loop with random colors and sleep timing |
| `demos/cycle_demo.tyro` | Per-frame loop using `while cycle do` — one drawing per frame |
| `demos/enter_leave_demo.tyro` | Grouped drawing with `enter()` / `leave()` — a batch of draw calls lands in one drawing cycle |
| `demos/sprites_demo.tyro` | Sprites system: load, show, hide, move, rotate, scale, named access |
| `demos/controls.tyro` | Generic buttons, labels, checkboxes, edits, and panels |
| `demos/controls_align.tyro` | Nested controls: a left-docked panel with top- and bottom-docked buttons |
| `demos/aseprites_demo.tyro` | Aseprite animations: load `.aseprite` files (all frames as textures), `play(fps)`, `stop`, `looping`, per-frame stepping — idle/walk/run showcase |
| `demos/sprite_atlas_demo.tyro` | Texture atlas `.sprite` files: load a ZIP'd PNG + JSON atlas (TexturePacker/Phaser/PixiJS), every atlas frame becomes an animation frame, `play`/`stop`, speed control |
| `demos/interactive_paint.tyro` | Mouse drawing with keyboard color switching (uses input APIs) |
| `demos/console_demo.tyro` | Console output: print, println, log |
| `demos/terminal_demo.tyro` | Built-in terminal commands: dir, list, clear, help, exit |
| `demos/console_read_demo.tyro` | Interactive console.read() — prompt the user for input from Lua |
| `demos/music_demo.tyro` | Sound effects (music.sound, music.noise) and MML melodies (music.mml) |
| `demos/test.tyro` | Circle animation with random colors |
| `demos/colors_bar.tyro` | Full color palette display |
| `demos/multiply.tyro` | Drawing + MML sound |
| `demos/text.tyro` | Multi-language text rendering |
| `demos/shader_demo.tyro` | Post-processing shaders: water, glow, gray, sepia, invert, vignette, pixelate |
| `demos/shake_demo.tyro` | Screen shake: `shake(ms, power)` / `window.shake(ms, power)` — the world jolts like an accident or an error |

# Threading model & lifecycle

Script source runs on a **worker thread**, never on the render/main thread.
Every RayLib call and every control/console/audio mutation a script makes is
packaged as a queue object (`TQueueObject`) and dispatched back to the main
thread, where the main loop drains the queue (`Main.Queue`) once per cycle.

- **Main-thread confinement.** Window, canvas, sprites, shaders, fonts, music/
  sound (the audio device), radio, spectrum and the control tree are all
  owned by the main thread and must only be touched there. The Lua facades hide this:
  they enqueue `TCreateControlObject`/`TSetControl*Object`/draw/music objects
  that execute on the main thread.
- **Worker lifecycle.** `Start` publishes an atomic `started` flag, `Stop`
  (also triggered by `--exit`/window close) cancels a pending queue wait,
  signals blocked `console.read()`, then pumps `CheckSynchronize` until the
  thread has actually finished before `WaitFor` and disposal.
- **Queue admission.** A stopped/inactive script rejects new asynchronous work
  (the request is freed immediately instead). Right after the worker exits the
  engine cancels and clears any stale queue entries so they cannot leak into a
  later interactive run; under `--exit` a final drain runs first so work the
  worker accepted before completing is still honored.
- **Per-state Lua cancellation.** Termination status lives in each Lua state's
  extra space, so cancelling one script can never affect another state.
- **Loaded script vs. worker.** `load`/`F4` fill a template script that owns the
  source; every run (`run`, `F5`, closing the editor) stops the current worker,
  clones that template and starts a new worker on the clone. The worker owns and
  frees its clone, so the template — and the editor buffer saved into it —
  survives a stop and is never written by a running script.
- **Clean slate per run.** Before the clone starts, the run frees the controls the
  finished run left in the window and the sprites it left in the sprite store
  (physics bodies first, since they are keyed by sprite handle), then wipes both
  canvases. The engine controls (console, output, editor, file picker) are kept,
  and keyboard focus and the mouse capture are dropped while the control they point
  at is still alive. Canvas clearing happens inside `BeginDraw`/`EndDraw`, which is
  the only window in which a texture canvas is bound to its render texture.
- **Resource release order.** GPU/audio owners (sprites, canvases, shaders,
  fonts, musicians, generated waveforms, radio, spectrum) are destroyed while
  the window/OpenGL context and audio device are still alive; the context and
  device are closed last, after every owner has been released.

The worker thread reads current input/timing values (`iskeydown`, `mousex`,
`frametime`, ...) and submits drawing/mutation requests through the queue; it
must not hold references into the control tree or RayLib objects across
calls. Scripts blocking on `console.read()` or `while cycle do` are cancelled
by `Stop`, so a shut-down never waits on user input.

# Issues

There is problem in raylib, in fact in OpenGL that cannot/not easy share texture between threads, we need another trick to pass drawing commands to main thread, but now i am sending objects to draw it in main thread, it is work fine until now

https://github.com/raysan5/raylib/issues/454

# Compile

Lazarus or Delphi

# Libraries

You need MiniLib package minilib.lpk
[minilib](https://github.com/parmaja/minilib)
You need only MiniLua package minilib.lpk
[miniLua](https://github.com/parmaja/miniLua)

# Dependencies

[raylib](https://www.raylib.com/) for raylib.dll/so put it in same of tyro exe folder

### Competition

[yabasic](http://www.yabasic.de)

## Fonts ##

Thanks for
https://pixelfonts.org/#116
https://github.com/IT-Studio-Rech/bdf-fonts
https://forums.adafruit.com/viewtopic.php?t=203655