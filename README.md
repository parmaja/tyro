# Tyro

Runing embed programming language in simple graphical environment, for kids and newbies, using (https://www.raylib.com/ "raylib") as small game engine to draw.
Now I am working on adding Lua, in the future I want to add more simple progamming lanuguages like Basic.
It also should have editing tool inside that environment, console output and input, work in same graphical window.
playing sound using mmf code.

It is More simulating old computer, but with modern languages and graphic.


# Issues

There is problem in raylib, in fact in OpenGL that cannot/not easy share texture between thread, we need another trick to pass drawing commands to main thread, but now i am sending objects to draw it in main thread, it is work fine until now

https://github.com/raysan5/raylib/issues/454

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

# Compile

Use FPC 3.x or Lazarus with it

# Libraries

(https://github.com/malcome/Lua4Lazarus "Lua4Lazarus")

[https://github.com/zaher/raylib-pas raylib-pas]

[https://www.raylib.com/ raylib] for raylib.dll/so

[https://www.lua.org/ Lua] for lua dll 5.3

[https://sourceforge.net/p/minilib/ minilib]

# TODO

[https://github.com/parmaja/p-sard Pascal Sard Objects]

