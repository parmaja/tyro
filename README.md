# Tyro

Runing embed programming language in simple graphical environment, for kids and newbies, using [raylib](https://www.raylib.com/) as small game engine to draw.
Currently supports Lua (`.lua` / `.ls`).
It also should have editing tool inside that environment, console output and input, work in same graphical window.
playing sound using mmf code.

It is More simulating old computer, but with modern languages and graphic.

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

# Console

## Built-in Terminal Commands

When the graphical console is visible, you can type commands at the `> ` prompt.
The following commands are built in:

| Command | Description |
|---------|-------------|
| `dir`, `list`, `ls` | List files in the current directory |
| `clear`, `cls` | Clear the console output |
| `help`, `?` | Show available commands |
| `exit`, `quit` | Hide the console and stop the engine |
| `ESC` | Hide the console (keybinding) |

Example:
```
tyro demos/terminal_demo.lua
# then type "dir" at the console prompt
```

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

See `demos/terminal_demo.lua` and `demos/console_read_demo.lua` for examples.

# Spirits (Sprites)

The `Spirits` system manages textured images ("spirits") that the engine draws
every frame. Each spirit is created with `Spirits.new`, loaded with `load`, and
positioned with `move` or by setting `x`/`y` properties.

## Creating & Loading

| Function | Description |
|----------|-------------|
| `Spirits.new("name"?)` | Create a new spirit. Optional name registers it for lookup. |
| `Spirits("name")` | Look up a spirit by name (returns the spirit object or `nil`). |
| `Spirits.find("name")` | Same as above. |

## Spirit Methods

| Method | Description |
|--------|-------------|
| `spirit:load("image.png")` | Load a texture from file into this spirit. |
| `spirit:show()` | Make the spirit visible (shown by default). |
| `spirit:hide()` | Hide the spirit from rendering. |
| `spirit:move(x, y)` | Set the spirit's position. |
| `spirit:width()` | Return the texture width in pixels. |
| `spirit:height()` | Return the texture height in pixels. |

## Spirit Properties

| Property | Type | Description |
|----------|------|-------------|
| `spirit.x` | `number` | X position (read/write). |
| `spirit.y` | `number` | Y position (read/write). |
| `spirit.angle` | `number` | Rotation in degrees (read/write). |
| `spirit.scale` | `number` | Scale factor, 1.0 = original size (read/write). |
| `spirit.visible` | `boolean` | Whether the spirit is drawn (read/write). |

Image files are searched in the script directory, the workspace `sprites/`
folder, and the current directory.

```lua
window.show(640, 480)
canvas.color(colors.black)
canvas.clear()

myspirit1 = Spirits.new("player")
myspirit1.load("richard-say.png")
myspirit1.show()
myspirit1.move(100, 200)

myspirit2 = Spirits.new()
myspirit2.load("richard-say.png")
myspirit2.move(300, 100)
myspirit2.angle = 45
myspirit2.scale = 0.5

-- Look up by name
local p = Spirits("player")
println("Sprite size: " .. p.width() .. "x" .. p.height())

local a = 0
while true do
    canvas.color(colors.black)
    canvas.clear()

    -- Engine draws all spirits automatically; just update properties
    myspirit1.move(mousex(), mousey())
    myspirit1.angle = a

    a = a + 1
    if a >= 360 then a = 0 end

    sleep(16)
end
```

# Timing

| Function | Returns | Description |
|----------|---------|-------------|
| `sleep(ms)` | — | Pause the script thread for *ms* milliseconds |
| `frametime()` | `number` (seconds) | Time elapsed since the last frame |
| `time()` | `number` (seconds) | Elapsed time since the window was created |
| `rand(min, max)` | `integer` | Random integer in [min, max] |

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
tyro demos/<name>.lua
```

| File | Feature |
|------|---------|
| `demos/pong.lua` | Complete Pong game — drawing, keyboard input, AI, physics, collision, sound, scoring |
| `demos/basic_drawing.lua` | All drawing primitives: rectangle, circle, line, point, text, colors |
| `demos/animated_demo.lua` | Animation loop with random colors and sleep timing |
| `demos/sprites_demo.lua` | Spirits system: load, show, hide, move, rotate, scale, named access |
| `demos/interactive_paint.lua` | Mouse drawing with keyboard color switching (uses input APIs) |
| `demos/console_demo.lua` | Console output: print, println, log |
| `demos/terminal_demo.lua` | Built-in terminal commands: dir, list, clear, help, exit |
| `demos/console_read_demo.lua` | Interactive console.read() — prompt the user for input from Lua |
| `demos/music_demo.lua` | Sound effects (music.sound) and MML melodies (music.mml) |
| `demos/test.ls` | Circle animation with random colors |
| `demos/colors_bar.lua` | Full color palette display |
| `demos/multiply.lua` | Drawing + MML sound |
| `demos/text.lua` | Multi-language text rendering |

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
