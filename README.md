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
window()
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
window()
while true do
    if ismousepressed("left") then
        canvas.circle(mousex(), mousey(), 5, true)
    end
    sleep(10)
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
window()
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
| `demos/interactive_paint.lua` | Mouse drawing with keyboard color switching (uses input APIs) |
| `demos/console_demo.lua` | Console output: print, println, log |
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
