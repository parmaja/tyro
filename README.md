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
| `sprite:load("image.png")` | Load a texture from file into this sprite. `.aseprite`/`.ase` files load every frame as an animation. |
| `sprite:show()` | Make the sprite visible (shown by default). |
| `sprite:hide()` | Hide the sprite from rendering. |
| `sprite:move(x, y)` | Set the sprite's position. |
| `sprite:width()` | Return the texture width in pixels. |
| `sprite:height()` | Return the texture height in pixels. |
| `sprite:play([fps])` | Restart and play the animation. `fps` overrides the per-frame timings stored in the file (0 or omitted keeps them — needed because Aseprite files often have ~1 ms delays). |
| `sprite:stop()` | Freeze the animation on the current frame (aliased by `sprite:pause()`). |
| `sprite:framecount()` | Number of frames in the loaded `.aseprite` animation. |

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

| Function | Description |
|----------|-------------|
| `controls.new(class, caption, x, y, w, h, name?)` | Create a `button`, `panel`, `label`, `checkbox`, `edit`, `spectrum`, or `listbox`; returns a handle |
| `controls.align(handle [, value])` | Get/set `none`, `left`, `top`, `right`, `bottom`, or `client` |
| `controls.parent(handle [, parentHandle])` | Get/set the container; the getter returns `0` for the main window, and `nil`/`0` moves the control there |
| `controls.width/height(handle [, value])` | Get/set the preferred size |
| `controls.position/move(handle, x, y)` | Get/set the preferred position |
| `controls.text/caption(handle [, value])` | Get/set a caption, label, or edit value |
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

# Timing

| Function | Returns | Description |
|----------|---------|-------------|
| `sleep(ms)` | — | Pause the script thread for *ms* milliseconds |
| `cycle` | `true` | **Block** until the next drawing frame completes. Use `while cycle do` (instead of `while true do`) to run the loop body at most once per drawn frame, so drawing commands cannot pile up inside a single raylib drawing cycle |
| `frametime()` | `number` (seconds) | Time elapsed since the last frame |
| `time()` | `number` (seconds) | Elapsed time since the window was created |
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
| `demos/sprites_demo.tyro` | Sprites system: load, show, hide, move, rotate, scale, named access |
| `demos/controls.tyro` | Generic buttons, labels, checkboxes, edits, and panels |
| `demos/controls_align.tyro` | Nested controls: a left-docked panel with top- and bottom-docked buttons |
| `demos/aseprites_demo.tyro` | Aseprite animations: load `.aseprite` files (all frames as textures), `play(fps)`, `stop`, `looping`, per-frame stepping — idle/walk/run showcase |
| `demos/interactive_paint.tyro` | Mouse drawing with keyboard color switching (uses input APIs) |
| `demos/console_demo.tyro` | Console output: print, println, log |
| `demos/terminal_demo.tyro` | Built-in terminal commands: dir, list, clear, help, exit |
| `demos/console_read_demo.tyro` | Interactive console.read() — prompt the user for input from Lua |
| `demos/music_demo.tyro` | Sound effects (music.sound) and MML melodies (music.mml) |
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
  sound (the audio device), radio, spectrum and the control tree are all owned
  by the main thread and must only be touched there. The Lua facades hide this:
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

Use FreePascal 3.x or Lazarus with it

# Libraries

You need only MiniLib package minilib.lpk
[minilib](https://github.com/parmaja/minilib)

# Dependencies

[raylib](https://www.raylib.com/) for raylib.dll/so put it in same of tyro exe folder

[Lua](https://www.lua.org/) for lua dll 5.3 in same of tyro exe folder

# Ported

You do not need to use it, it is already in the source folder

[Lua4Lazarus](https://github.com/malcome/Lua4Lazarus)


# TODO

[Sard Objects](https://github.com/parmaja/p-sard)


### Competition

[yabasic](http://www.yabasic.de)

## Fonts ##

Thanks for
https://pixelfonts.org/#116
https://github.com/IT-Studio-Rech/bdf-fonts
https://forums.adafruit.com/viewtopic.php?t=203655