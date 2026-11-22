# Tyro

Runing embed programming language in simple graphical environment, for kids and newbies, using [raylib](https://www.raylib.com/) as small game engine to draw.
Currently supports Lua (`.tyro` / `.lua`).
It also should have editing tool inside that environment, console output and input, work in same graphical window.
playing sound using mmf code.

It is More simulating old computer, but with modern languages and graphic.

## Documentation

The full API and the onboarding path live in **[docs/](docs/README.md)** —
one page per topic, with a start-here ladder at
[docs/README.md](docs/README.md):

| | |
| --- | --- |
| [Getting started](docs/getting-started.md) | run your first script, command line, script arguments |
| [Input](docs/input.md) | keyboard, mouse, touch, gamepad |
| [Sprites](docs/sprites.md) | the heart of every Tyro game |
| [Tilemaps](docs/tilemaps.md) · [Physics](docs/physics.md) | grids, gravity, collisions |
| [3D models](docs/models.md) · [Cameras](docs/cameras.md) | models, lights, free and 2D cameras |
| [Shader effects](docs/effects.md) · [Controls](docs/controls.md) | shader presets, screen shake, in-window widgets |
| [Timing](docs/timing.md) | `update`/`draw`, timers, tweens, events, joints, save/load |
| [Game helper](docs/game.md) · [Files](docs/files.md) · [Network](docs/network.md) · [Console](docs/console.md) | the helper tables |
| [Examples](docs/examples.md) · [Threading](docs/threading.md) | the demo index, and how the engine runs |

Feature stories and design notes — why fonts bake, how tweens tick — are in
[help/](help/).

## Try it

```
tyro demos/pong.tyro
```

A `.tyro` file is a Lua script: `window.show(640, 480)`, then
`update(dt)` / `draw()` callbacks — [the first example is here](docs/getting-started.md#lua-example).
Without a script argument Tyro starts an interactive console session;
`tyro --help` lists the command line options.

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
