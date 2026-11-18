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
| `--console`, `-c` | Force a command prompt / console output |
| `--debug`, `-d` | Enable debug logging |
| `--exit[:N]` | Exit after *N* seconds, or — with a bare `--exit` (no `:N`) — as soon as the script finishes |
| `--screenshot:N` | Save `screenshot.png` *N* seconds into the run; opens a window so the screen can be captured |
| `--workpath=<path>` | Workspace directory (defaults to the executable location) |

With an engine-driven script — one that defines `update`/`draw` (see
[Timing](#timing)) — the script "finishes" the moment its top-level chunk
returns, so a bare `--exit` ends the run right away, before the callbacks get
to run. Use `--exit:N` to let such a script play for *N* seconds.

## Script arguments

Every parameter after `tyro.exe` is handed to the running script as the global
`args` table — a 1-based array of strings, including the script name and the
engine switches:

```
bin\tyro.exe game.lua red 42 --console
```

```lua
-- args == { "game.lua", "red", "42", "--console" }
for i = 1, #args do
    print(i .. ": " .. args[i])
end
```

Extra words after the script (`red 42` above) do not confuse the loader: the
first parameter that names an existing file or folder is the one that runs.
Per-sprite scripts (`on_update`/`on_draw` files) see the same `args`.

### Exit statuses

| Status | Meaning |
|--------|---------|
| `0` | Success (also `--help`/`--list`, and interactive sessions closed normally) |
| `1` | Lua runtime error while running with `--exit` |

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
`canvas.image(x, y, file [, scale])`,
`canvas.triangle(x1, y1, x2, y2, x3, y3 [, fill])`,
`canvas.line(x1, y1, x2, y2 [, thickness])` and `canvas.point(x, y)`.
`canvas.gradient.rectV(x, y, w, h [, startColor, endColor])` fills a rectangle with
a top-to-bottom gradient, `canvas.gradient.rectH(...)` with a left-to-right one,
and `canvas.gradient.circle(x, y, r [, innerColor, outerColor])` fills a circle
with a radial gradient (inner colour at the centre, outer at the edge);
their colors take the same `colors.*` values as `canvas.color` and default to
the current color when omitted. Set the color with `canvas.color = colors.red`
(and `canvas.backcolor` / `canvas.clear()` for the background).
`canvas.pen` sets the
thickness of lines and of rectangle/triangle outlines; `canvas.line` also takes
a per-call thickness. `textwidth(s)` and `textheight()` measure the built-in
font, and `canvas.width` / `canvas.height` report the canvas size.

`canvas.test(ms)` is a **diagnostic** draw: it draws a marker rectangle and then
spends about *ms* milliseconds on the main thread, emulating a single draw call
slower than a frame. It exists to check that a body grouped with `enter()` /
`leave()` is still drawn as one group (see [docs/atomic.md](docs/atomic.md));
normal scripts do not need it.

For sound, `music.beep()`, `music.sound(freq, ms)` or `music.sound(wave, freq,
ms)`, `music.noise(ms)` (white) / `music.noise(freq, ms)` (pitched) and
`music.mml(...)` play one-shot effects and tunes. The `music.volume` property
(0..1) sets the level of the `music.mml` **melodies only** - one-shot sounds
and loops keep their own volume - and reading it returns the current level.
For a sound that has to run
without gaps—an engine drone, a siren, a wind loop—use `sound.loopPlay(freq
[, volume])` or `sound.loopPlay(wave, freq [, volume])`. It keeps looping until
`sound.loopStop()`, and `sound.loopPitch(freq)` / `sound.loopVolume(0..1)` retune
it while it plays. Available `wave` names include `Sin`, `Square`, `Saw`, `Tri`,
`Organ`, `FM`, `Noise`, `Pink`, `Engine`, the game effects `Fire`, `Boing`,
`Siren` and `Bubble`, and the drum waveforms `Kick`, `Snare` and `HiHat`.

For a one-shot sound effect from an audio file, `sound.play("file.wav" [, volume])`
plays the file (raylib `LoadSound`, `.wav`) once. The file is resolved like every
other script asset (next to the script, the workspace, the app folder); a missing
file is ignored, not an error. Use `sound.load("file.wav")` to decode a file ahead
of time and keep it cached (so the first `sound.play` is instant), and
`sound.unload("file.wav")` to drop it from the cache again. A preloaded file is
played as a cheap alias of the cached copy; a file that was never loaded is decoded
on the spot and released once it finishes, so `sound.play` on its own does not keep
anything cached.

# Input

Keyboard and mouse input is exposed as the `keyboard`, `mouse` and `gamepad`
tables. Key and button names are strings that map to RayLib key/button enums —
they work identically in Lua and PascalScript.

## Keyboard

| API | Description | Examples |
|----------|-------------|----------|
| `keyboard.pressed(key)` | True once when the key transitions from released to pressed | `"space"`, `"w"`, `"f1"` |
| `keyboard.down(key)` | True every frame while the key is held | `"ctrl"`, `"up"`, `"a"` |

Supported key names: letters `a`-`z`, digits `0`-`9`, `space`, `enter`, `tab`,
`escape`, `backspace`, `delete`, `insert`, `up`, `down`, `left`, `right`,
`f1`-`f12`, `shift`, `ctrl`, `alt`, `minus`, `equals` (the `+` key is the
shifted `=`, so ask for `plus` to catch it), `kp_plus`, `kp_minus`.

### Engine shortcuts

These work while the engine is running, whatever has the keyboard focus:

| Key | Description |
|-----|-------------|
| `F1` | Toggle the keyboard shortcuts help panel (lists every shortcut below) |
| `F2` | Toggle the script editor. Opening it stops the script; closing it (also with `ESC`) saves the edited source back to the script and runs it |
| `F3` | Toggle the sprite editor |
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
| `mouse.pressed([button])` | True once when a mouse button is pressed (default `"left"`) | `"left"`, `"right"`, `"middle"` |

```lua
window.show()
while true do
    if mouse.pressed("left") then
        canvas.circle(mouse.x, mouse.y, 5, true)
    end
     sleep(10)
 end
 ```

## Gamepad

| API | Description | Examples |
|----------|-------------|----------|
| `gamepad.available([gamepad])` | True when a gamepad is connected (default pad 0) | — |
| `gamepad.name([gamepad])` | The pad's name, or empty when there is no pad | — |
| `gamepad.pressed(button [, gamepad])` | True once when a pad button transitions to pressed | `"a"`, `"start"`, `"dpad_up"` |
| `gamepad.down(button [, gamepad])` | True every frame while a pad button is held | `"a"`, `"lb"`, `"dpad_left"` |
| `gamepad.axis(axis [, gamepad])` | Axis value in -1..1 (0 without a pad) | `"leftx"`, `"lefty"`, `"rightx"`, `"righty"`, `"ltrigger"`, `"rtrigger"` |
| `gamepad.mappings(text)` | Add gamepad mappings in SDL/GLFW format; returns nonzero when accepted | — |

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

# Game Helper (`game.*`)

The chores every game re-writes live in the engine: the arcade button pairs in
the `game` table, the best scores in `score`, and the math helpers as plain
globals. Full notes in [`help/game.md`](help/game.md).

```lua
clamp(v, lo, hi)   lerp(a, b, t)   sign(v)
dist(x1, y1, x2, y2)
aabb(x1, y1, w1, h1, x2, y2, w2, h2)   -- do two boxes overlap?

game.pause_pressed()  game.start_pressed()  game.fire_pressed()
game.fire_down()     -- keyboard OR gamepad, the usual arcade buttons

local hi = score.hi_load("cave.hi", 0)         -- best score from a small
hi = score.hi_save("cave.hi", score)           -- text file next to the game

local r = rng(42)   -- repeatable random: r() 0..1, r(n) 1..n, r(a,b) a..b
```

`sound.muted = true` is the one silence switch: while it is on, every
following play request from `music.*` and `sound.*` goes quiet
(`music.muted` is the same flag).

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
| `list`, `ls` | List files in the current directory |
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
| `Sprites.puff("name"? [, size] [, count])` | Create a sprite carrying an **engine-built soft puff texture** (no file, like `models.sphere`): a solid core with a feathered, lumpy rim — the arcade building block for clouds and smoke. `size` is the texture in pixels (default 128). `count` above 1 builds that many puffs in one hop and returns a table of them, named `name1..nameN`. Stack a few with jittered position/scale/alpha and their soft edges fuse into one fluffy mass. |
| `Sprites.blob("name"? [, size] [, count])` | The **crisp-rim twin** of `Sprites.puff`: the same lumpy, wobbled silhouette, but the alpha cuts off at the coast within a pixel instead of feathering — an organic shape with a sharp edge (islands, rocks, hard-edged decor that must not look fluffy). Same `size`/`count`/batch behaviour as `Sprites.puff`. |
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
| `sprite.x` | `number` | X position of the sprite's centre (the engine draws and rotates around the centre, like the physics body) (read/write). |
| `sprite.y` | `number` | Y position of the sprite's centre (read/write). |
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
| `sprite.tint` | `number` | Colour multiplied over the texture, the same `$AABBGGRR` integer format as `canvas.color` / `colors.*`; white (`colors.white`) is the default (read/write). |
| `sprite.alpha` | `number` | Opacity `0..255` folded into the tint's alpha: `0` hides the sprite without touching `visible`, in-between values fade it (read/write). |
| `sprite.layer` | `number` | Draw order: every frame the sprites are drawn sorted by layer, lower layers first (behind); the default is `0`, and sprites that share a layer keep a stable order by handle (read/write). |

The layer sort makes stacking deterministic, so distant decor can sit at
`layer = -10`, the actors at `0`, and effects that must cover the action (an
explosion, a muzzle flash) at `layer = 10` — no matter in which order the
sprites were created. `sprite.tint` and `sprite.alpha` are the cheap way to
flash a hit, fade a ghost or sink an object into the blue without swapping
textures.

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
    mysprite1.move(mouse.x, mouse.y)
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

# 3D Models

The `models` system holds 3D meshes that the engine draws every frame, in a
top-down orthographic pass that is pixel-aligned with the 2D canvas — a model
at `x, y` sits on exactly the same pixel as a sprite or a `canvas.rectangle`.
It is the easy way to put a real 3D object inside a 2D game. The recommended
file format is **GLB** (binary glTF 2.0), a single self-contained file with
mesh, texture and material. For a full 3D scene — a perspective eye of your
own inside the world — there is the Free Camera section further down.

## Creating & Loading

| Function | Description |
|----------|-------------|
| `models.new("name"?)` | Create a new model object. Optional name registers it for lookup. |
| `models.sphere("name"? [, radius])` | An engine-built sphere (radius `1` by default, so the bare call is 2 wide and centred) — ready-made puffs, bubbles and planets with no file at all. |
| `models.box("name"? [, w, h, d])` | An engine-built flat-shaded box, `w × h × d` units along x/y/z (unit by default) — the vector block of a 3D world. |
| `models.cylinder("name"? [, radius, height])` | An engine-built drum standing on the z axis (unit by default), smooth on the mantle with hard lids — posts, wheels, pipes. |
| `models.cone("name"? [, radius, height])` | An engine-built cone standing on the z axis with its apex up (unit by default) — roofs, spikes, rockets. |
| `models.torus("name"? [, radius, tube])` | An engine-built ring lying on the x/y plane (radius `1`, tube `0.25` by default): `radius` is centre-of-hole to centre-of-tube, `tube` the thickness of the ring itself — a race loop, a portal, a donut. |
| `models("name")` | Look up a model by name (returns the model object or `nil`). |
| `models.find("name")` | Same as above. |

A named model is also exposed as a global, so after `models.new("jet")` you
can write `jet.load(...)` directly. Model files are searched in the script
directory and the workspace, like sprites.

## Model Methods

| Method | Description |
|--------|-------------|
| `model:load("jet.glb")` | Load (or replace) the mesh. Loading happens on the engine's queue, like sprite textures; `model.loaded` tells when it is ready. |
| `model:show()` | Make the model visible (shown by default). |
| `model:hide()` | Hide the model from rendering. |
| `model:move(x, y[, z])` | Set the screen position (pixels) and optional depth in one call. |
| `model:face(tx, ty, tz[, ux, uy, uz])` | Point the model's own +y axis — its nose, wherever `model.nose` says it lives — at the target `tx,ty,tz`; with the optional up vector, its own +z leans toward it. Aim once and fly; `angle`/`roll`/`tilt` still add small trims on top. (Meaningful under the free camera; see below.) |
| `model:play([what])` | Start a skeletal animation by name or index (`model:play("Walk")`); with no argument it replays the last clip (or the file's first). Returns whether a clip was found. The engine poses the bones every frame while it plays. |
| `model:stop()` | Stop the skeletal animation, keeping the current pose. |

## Model Properties

| Property | Type | Description |
|----------|------|-------------|
| `model.x` | `number` | Screen X of the model's centre, in canvas pixels (read/write). |
| `model.y` | `number` | Screen Y of the model's centre, in canvas pixels (read/write). |
| `model.z` | `number` | Depth: smaller draws in front of bigger (read/write). |
| `model.angle` | `number` | Spin in degrees around the view axis, clockwise (read/write). |
| `model.roll` | `number` | Bank in degrees around the model's own axis — the nose-tail line of a jet — so it leans into a turn like the classic 1942 sprite effect (read/write). |
| `model.tilt` | `number` | Bank in degrees around the screen's horizontal axis — foreshortens the model as it leans (read/write). |
| `model.scale` | `number` | Uniform scale in model units; combine with the bounding box below to size in pixels (read/write). A negative scale mirrors the mesh — winding and light flip with it — which lets a solid wear an inside-out skin (the tube liner in `games/torustrooper` is a torus at `scale = -1`). |
| `model.visible` | `boolean` | Whether the model is drawn (read/write). |
| `model.shadow` | `boolean` | Cast a real shadow: the mesh's true silhouette projected along the sun onto the ground plane — see Light & Shadows below (read/write, default `true`). |
| `model.tint` | `number` | Colour multiplied over the mesh, the same integer format as `canvas.color` / `colors.*`; white is the default (read/write). |
| `model.alpha` | `number` | Opacity 0..255, folded into the tint's alpha like a sprite; a see-through model also casts a proportionally softer shadow (read/write). |
| `model.name` | `string` | The model's name (read/write). |
| `model.nose` | `string` | Which own axis `face()` and `roll` treat as the nose-tail line: `"y"` (the default — engine-built shapes and hand-made boxes fly on their +y), or `"-y"`, `"z"`, `"-z"`, `"x"`, `"-x"` when a loaded file draws with its nose elsewhere. The loader stands glTF files upright, so a jet modelled nose-up in Blender typically asks for `"-y"`; when in doubt, point the model at something and try. The fix rides along in the draw — the mesh itself is never touched (read/write). |
| `model.glow` | `string`/`boolean` | The material that shines instead of being lit. A name from the loaded file (e.g. `"Engine"`) keeps just that material at full bright, immune to `world.lights` — glowing intakes, a lamp, a screen. `true` (or `"*"`) shines the whole model, ideal for a thruster flame; `""`/`false` (the default) leaves it to the lamps. The names come from the file's own JSON (raylib drops them), so a name the file does not carry simply never matches. It only shows while lights are on — with none, every material is already flat-lit (read/write). |
| `model.bright` | `number` | How hard a shining material burns: `1` (the default) is the colour the file was painted with, `2` twice as hot, and any level climbs toward white-hot — the difference between a painted panel and a lamp you can look at. Clamped to `0..16` and snapped to 1/8 steps, so a script that pulses the shine (an engine spooling up) costs one shader, not one a frame. It speaks for a glowing material only: a body the lamps light keeps its shading, and — like `model.glow` — the level only shows while `world.lights` is on (read/write). |
| `model.width` | `number` | Bounding-box X size of the loaded mesh (read-only). |
| `model.height` | `number` | Bounding-box Y size (read-only). |
| `model.depth` | `number` | Bounding-box Z size (read-only). |
| `model.loaded` | `boolean` | Whether the mesh is ready (read-only). |
| `model.animCount` | `number` | How many skeletal animation clips the loaded file carries (read-only). |
| `model.anim` | `number`/`string` | The clip now posed — read its index (`-1` = the bind pose), or write a name/index to play it, like `model:play`. |
| `model.animPlaying` | `boolean` | Whether a clip is playing (read/write; `false` stops and holds the pose). |
| `model.animTime` | `number` | Position inside the current clip in seconds (read/write — write it to scrub a stopped walker frame by frame). |
| `model.animSpeed` | `number` | Playback speed in keyframes per second; the fresh `30` is the clip's authored pace, `60` doubles it, `0` holds the pose while playing. |
| `model.animName` | `string` | The current clip's name, or `""` before anything plays (read-only). |

Models draw above the 2D canvas and sprites, below the controls, and they
shake with the screen like everything else. All three rotations (`angle`,
`roll`, `tilt`) turn the model about its own bounding-box centre, and the
model always faces the viewer with its own +Z pointing up the screen — so a
nose-up jet model needs no rotation for a player flying up the screen, and
`angle = 180` for the enemies flying down it. With `shadow` on (the default)
every model drops its true silhouette on the ground plane below — see
Light & Shadows.

```lua
window.show(320, 240)

local jet = models.new("player")
jet:load("jet.glb")

-- size it by pixels: widest horizontal extent becomes 46 px
local size = math.max(jet.width, jet.depth)
jet.scale = 46 / size
jet.tint = colors.white

local t = 0
function update(dt)
  t = t + dt
  jet:move(160 + 90 * math.sin(t), 150, 0)
  jet.roll = 30 * math.sin(t * 2)     -- bank left/right into the turn
end
```

`games/jet2026/jet2026.tyro` is a 1942-style scrolling shooter built this
way: one GLB jet reused through a pool for the player and every enemy,
tinted and rotated per role. Its clouds are stacks of `Sprites.puff`
sprites (one batch per cloud), their soft edges fusing into fluff, and the
sea is a live one: `sea.frag` rolls swells, sun glitter and drifting crests
over the board's blue pixels, masked so islands and bullets keep their
sharp shapes.

A GLB can also carry a **skeleton and baked animation clips** (the format
stores them alongside the mesh). `model:load` picks those clips up and
`model:play("Walk")` switches between them; the engine then poses the bones
every frame on its own, so a script only names the clip and gets on with
the game. `games/astro/astro.tyro` shows it: a little astronaut patrols a
lunar ridge, playing his `Walk` clip as he paces and an `Idle` clip while he
rests at each end, and you can steer him with the arrows.

## Light & Shadows

Cast shadows share one sun, and it lives on the `world` table. Every
function below reads when called with no arguments and sets (then returns)
when called with them.

| Function | Description |
|----------|-------------|
| `world.light([dx, dy, dz])` | The direction the sunlight travels (`dy` may not be 0). The default `(0.06, 1, -0.035)` sits the sun high behind the viewer, so shadows slide a little right and down. With `world.lights` on it is also the sun that shades the solids. |
| `world.ambient([v])` | How much sky light fills the shadows, `0..1` (clamped). `0` is a hard sun with the darkest shadows, `1` is all ambient light so nothing stays dark. Default `0.45`. With `world.lights` on it is also the fill light of the shaded solids. |
| `world.ground([z])` | The `model.z` plane shadows fall on — the "floor" (the sea, say), which must sit farther from the eye than the models flying over it. Default `300`. |

A shadow is the mesh projected along the light ray onto that plane, so it
is the model's true silhouette — wings, bank and all — and it slides
farther away the closer the model sits to the viewer. Models nearer the
eye than the ground plane paint over the shadows behind them, so a 3D
cloud between the viewer and the sea covers the shadow under it and never
catches one on top.

```lua
world.light(0.10, 1, -0.05)   -- shadows land to the right and below
world.ambient(0.45)           -- soft darkness, sky light fills the rest
world.ground(300)             -- the sea plane, far behind the jets
```

# Free Camera

The model pass runs on a top-down orthographic eye by default — the pixel-
aligned one 2D games want. Hand it a real perspective eye of your own with
`world.camera` and the same world turns into a 3D playground: the space stays
exactly as it was (x right, y into the screen, z up, the same 1000-unit reach
of the depth buffer), only the way you look through it changes.

| Function | Description |
|----------|-------------|
| `world.camera(x, y, z[, tx, ty, tz[, ux, uy, uz]])` | Put the free eye at `x,y,z` looking at `tx,ty,tz` (default the origin) with the given up vector (default world up, `0,0,1`). Any call switches the model pass to this perspective eye. |
| `world.camera()` | With no arguments it reads the shot back as nine numbers — and never changes the mode, so `local x,y,z = ...` after a `world.ortho()` still tells where the free eye was parked. |
| `world.ortho()` | Hand the model pass back to the default top-down eye: `x/y` are screen pixels again and the cast shadows return. |
| `world.fov([deg])` | The free eye's vertical field of view in degrees, clamped to `10..170` (default 60). Read with no argument. |
| `world.wire([on])` | Paint every model as its edges only — `true` for the bare vector look (a glowing grid pairs nicely with `shader.effect = "glow"` or your own `shader.load("...frag")`), `false` for solid faces. Read with no argument; every model keeps its own tint. |
| `world.lights([on])` | Dress the solid models in light: they shade with lambert lighting from `world.ambient` (the sky fill), `world.light` (the sun ray) and up to four point bulbs (`world.pointlight`). Off by default — the classic flat ink — and the wire pass always dresses flat, as a bare grid takes its mood from the bloom. Read with no argument. |
| `world.pointlight(i, x, y, z [, color [, range]])` | Hang one of four bulbs (`i` = 1..4) at a world point. The color takes the same forms as `canvas.color` (default `"white"`); `range` is how far the light reaches before fading out (default 300). A bulb with no range left is dark, and calling with just `(i)` unscrews it. Only shines while `world.lights` is on. |

Under the free eye a model's `x/y/z` are plain **world coordinates** — the
same numbers `world.camera` takes — instead of screen pixels, and the screen
shake and window margin no longer move the models (the camera owns the frame
now). Cast shadows stay with the ortho eye: the ground plane is a top-down
notion, and a free 3D scene reads its depth directly.

Two more things change for the eye's sake. The model pass paints **both
skins** of every solid — back-face culling is off and the depth test alone
decides what is in front — so the eye can fly through bodies; faces that
look away from it are simply lit as back faces. And models paint **over**
the world canvas, so a HUD that must never dive under the scenery belongs
on the screen layer: `screen.text`, `screen.rectangle` (with
`screen.clear()` each frame — that layer keeps last frame's ink).

A **see-through** model — one whose `model.alpha` is under 255 — is painted
after the solids, far end first, and keeps no depth of its own. That is what
makes glass behave like glass whatever order the scripts built the world in: it
always finds what it looks through already painted, and it never hides a body
from the next stroke. Two glasses still sort between themselves, and a solid in
front of the glass hides it as usual.

```lua
window.show(640, 480)
canvas.backcolor = colors.black

local ship = models.box("ship", 5, 11, 3)   -- long axis on its own +y
world.fov(72)

local t = 0
function update(dt)
  t = t + dt
  -- a ship circling the origin at height 20, always nose-first along the ring
  local x, y = 120 * math.cos(t), 120 * math.sin(t)
  ship:move(x, y, 20)
  ship:face(x - math.sin(t) * 10, y + math.cos(t) * 10, 20)
  -- an eye that rides behind it, looking down at where it is going
  world.camera(x * 0.7, y * 0.7, 60, x, y, 10)
end
function draw()
  canvas.clear()
end
```

`demos/torus_demo.tyro` is a whole free-camera game in one file: a generated
torus loop to ride inside, its tube closed by an inside-out liner (see
`model.scale`), boost hoops that stand across the tube and feed your speed,
a chase eye whose up vector rides the tube so the frame rolls as the loop
turns, and a planet parked in the hole to spin against.

`games/torustrooper/torustrooper.tyro` builds the full arcade ride on the
same parts: a time trial inside the tube where energy cells buy back
seconds, boost gates pay only for the middle of the ring, and blocks on
the wall cost both. The course is one fixed layout, so every lap teaches
the next — and every model in it (track, liner, ribs, gates, ship) is an
engine-generated shape. See `games/torustrooper/README.md`.

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
| `shader.ok` | Read-only boolean — `true` when a shader is compiled and in effect right now (see below) |

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
if keyboard.pressed("1") then shader.effect = "water" end
if keyboard.pressed("2") then shader.effect = "glow" end
if keyboard.pressed("0") then shader.effect = "none" end
if keyboard.pressed("q") then shader.value = shader.value - 0.1 end
if keyboard.pressed("e") then shader.value = shader.value + 0.1 end
if keyboard.pressed("z") then shader.area = {0, canvas.height/2, canvas.width, canvas.height/2} end
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

## Did the shader actually load?

Everything in Tyro is queued, so `shader.load` only *posts* the request; the
engine compiles the file a frame later. `shader.ok` is the readback: it is
`true` only while a shader is really compiled and in effect — `false` for
`"none"`, for a missing file, and for a `.frag` that fails to compile. Read it
a frame (or a few) after the load, e.g. with a small countdown:

```lua
local probe, sea_on = 0, false
shader.load("sea.frag")        -- the request is only queued here
probe = 3                      -- check the answer after 3 frames

function update(dt)
  if probe > 0 then
    probe = probe - 1
    if probe == 0 then
      sea_on = shader.ok       -- now we know for sure
      if sea_on then shader.value = 1.0 else draw_fallback_sea() end
    end
  end
end
```

`shader.effect` tells the same truth as a name: it reports `"none"` when the
last load failed, or the file name (custom shader) / effect name in effect.
Games like `jet2026` and `torustrooper` use `shader.ok` to fall back to
hand-drawn art or the built-in `"glow"` when their custom file is missing.

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

function update(dt)
    if keyboard.pressed("space") then
        window.shake(400)  -- 400ms with the default power
    end
end

function draw()
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.color = colors.red
    canvas.circle(300, 240, 30, true)
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
| `controls.new(class, name, x, y, w, h)` | Create a `button`, `panel`, `label`, `checkbox`, `edit`, `spectrum`, `listbox`, or `image`; returns a handle. If `name` is a number, the name is omitted and coordinates follow. |
| `controls.align(handle [, value])` | Get/set `none`, `left`, `top`, `right`, `bottom`, or `client` |
| `controls.parent(handle [, parentHandle])` | Get/set the container; the getter returns `0` for the main window, and `nil`/`0` moves the control there |
| `controls.width/height(handle [, value])` | Get/set the preferred size |
| `controls.position/move(handle, x, y)` | Get/set the preferred position |
| `controls.visible/show/hide(handle)` | Show or hide a control; hidden aligned controls release their space |
| `controls.hover/down/clicked(handle)` | Read mouse state |
| `controls.border(handle [, style])` | `0` none, `1` thin, `2` thick, `3` sizable |
| `controls.backcolor(handle [, color])` | Set a color from the `colors` table |

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
logo = controls.new("image", "logo.png", 40, 40, 128, 128)
controls.name(logo, "logo")
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
| `enter()` | — | **Hold** the processing queue: the drawing commands queued after this call are not executed until `leave()` balances it |
| `leave()` | — | Release the hold taken by `enter()`: the whole group queued in between is then drawn at one drawing frame |
| `frametime()` | `number` (seconds) | Time elapsed since the last frame |
| `time()` | `number` (seconds) | Elapsed time since the window was created |
| `millis()` | `number` (ms) | High-resolution monotonic clock in milliseconds (the same timer the frame budget uses); fine enough to time a drawing body |
| `rand(min, max)` | `integer` | Random integer in [min, max] |

## Game loop: `update(dt)` / `draw()`

Define `update` and/or `draw` as globals and let the top-level chunk **return**.
The engine then drives the frame for you:

```lua
window.show(640, 480)

local t = 0

function update(dt)          -- once per frame, before drawing; dt in seconds
    t = t + dt
    if keyboard.pressed("space") then exit() end
end

function draw()              -- once per frame, after update()
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)
    canvas.color = colors.green
    canvas.circle(320 + 100 * math.sin(t), 240, 20, true)
end                          -- chunk returns: the engine drives from here
```

Both callbacks are optional. Once the top-level chunk returns **normally**, the
engine looks for them; if either exists it keeps the script (and its locals,
which stay live inside the callbacks as upvalues) and calls `update(dt)` then
`draw()` once per frame on the main thread — no `sleep()` pacing needed.
`exit()` inside `update` ends the run. The callbacks must be **globals**:
`local function update(dt)` is not found by the engine.

If the chunk never returns (for example it keeps a `while true do` loop), or
defines neither callback, the script stays on its worker thread and can still
draw and pace itself with `sleep()`:

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

Because only the engine callbacks touch the state after the hand-off — the
loader thread stops before the engine takes over — the two models are
interchangeable and never race. See [docs/engine-loop.md](docs/engine-loop.md)
for the details.

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
releases the lock, and the whole group is then drawn at one drawing frame —
instead of a large batch being split over several frames. Commands queued
outside the group behave as usual. Use it to group a batch inside `update` /
`draw`:

```lua
window.show()

function draw()
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
frozen queue. See [docs/atomic.md](docs/atomic.md) for the commit-watermark
details.

## Measuring the queue (queue.*)

The engine exposes read-only counters over the drawing queue to help diagnose
the "black box, then the text" flicker. A script on the worker thread keeps
queueing draw commands while the main loop drains them once per frame; when a
drain pass stops at the per-frame time budget with commands still waiting, the
frame is drawn with only part of the batch.

| Field | Description |
|-------|-------------|
| `queue.depth` | Commands waiting in the queue right now |
| `queue.holds` | Open `enter()` calls (the hold depth) |

The table also answers the old diagnostic names - `frames`, `drainms`,
`lastgap`, `maxgap`, `limit`, `waits`, `blocked` - which currently report 0.

The everyday use is waiting for queued commands to run, so a drawing getter
answers what the matching setter was set to:

```lua
canvas.rectangle(10, 10, 64, 40, true)
while queue.depth > 0 do
  sleep(1)      -- let the main loop execute the queued commands
end
```

The same view, plus the current `fps`, can be watched without a script by
pressing `CTRL`+`F10`: the engine draws a compact panel in the window's
top-left corner each frame. Press it again to hide the panel.

Wrapping a body in `enter()`/`leave()` holds the queue, so the main loop
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
| `demos/bounce_demo.tyro` | Engine-driven `update(dt)` / `draw()` loop — a ball moves once per frame |
| `demos/enter_leave_demo.tyro` | Grouped drawing with `enter()` / `leave()` — a batch of draw calls lands in one drawing frame |
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
| `demos/torus_demo.tyro` | Free camera: ride the inside of a generated torus loop, boost hoops feed your speed, a chase eye rolls with the tube — `world.camera` / `models.torus` |

# Threading model & lifecycle

Script source runs on a **worker thread**, never on the render/main thread.
Every RayLib call and every control/console/audio mutation a script makes is
packaged as a queue object (`TQueueObject`) and dispatched back to the main
thread, where the main loop drains the queue (`Main.Queue`) once per frame.

- **Main-thread confinement.** Window, canvas, sprites, shaders, fonts, music/
  sound (the audio device), radio, spectrum and the control tree are all
  owned by the main thread and must only be touched there. The Lua facades hide this:
  they enqueue `TCreateControlObject`/`TSetControl*Object`/draw/music objects
  that execute on the main thread.
- **Worker lifecycle.** `Start` publishes an atomic `started` flag, `Stop`
  (also triggered by `--exit`/window close) cancels a pending queue wait,
  signals blocked `console.read()`, then pumps `CheckSynchronize` until the
  thread has actually finished before `WaitFor` and disposal.
- **Frame hand-off (`update`/`draw`).** When the main chunk returns *normally*
  and its globals define `update` and/or `draw`, the finished worker's
  `lua_State` is kept alive and those callbacks are driven once per frame on the
  main thread; only the main thread touches the state from then on. Otherwise —
  no callbacks, or a chunk that never returns — the worker closes the state as
  usual. A later `Stop`/new run releases a handed-off state (see
  [docs/engine-loop.md](docs/engine-loop.md)).
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

The worker thread reads current input/timing values (`keyboard.down`, `mouse.x`,
`frametime`, ...) and submits drawing/mutation requests through the queue; it
must not hold references into the control tree or RayLib objects across
calls. Scripts blocking on `console.read()` or `sleep()` are cancelled by
`Stop`, so a shut-down never waits on user input.

# Issues

There is problem in raylib, in fact in OpenGL that cannot/not easy share texture between threads, we need used a trick to pass drawing commands to main thread

https://github.com/raysan5/raylib/issues/454

# Compile

Lazarus or Delphi

# Libraries

[minilib](https://github.com/parmaja/minilib) MiniLib package minilib.lpk

[miniLua](https://github.com/parmaja/miniLua) MiniLua package minilua.lpk

# Dependencies

[raylib](https://www.raylib.com/) for raylib.dll/so put it in same of tyro exe folder

[pluto](https://pluto-lang.org/)

### Competition

[love2d](https://love2d.org/)

[yabasic](http://www.yabasic.de)

## Fonts ##

Thanks for

https://pixelfonts.org/#116

https://github.com/IT-Studio-Rech/bdf-fonts

https://forums.adafruit.com/viewtopic.php?t=203655
