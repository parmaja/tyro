# Tyro Demos

This folder contains Lua script examples for the Tyro engine. Scripts can be
run with `tyro <script>`.

## Quick Start

```
tyro demos/pong.lua
```

## Demo Index

### Showcase

| File | Feature |
|------|---------|
| `pong.lua` | Complete Pong game — drawing, input, AI, physics, sound, scoring |

### Drawing

| File | Feature |
|------|---------|
| `basic_drawing.lua` | All drawing primitives: rectangle, circle, line, point, text, colors |
| `colors_bar.lua` | Iterate the full color palette |
| `animated_demo.lua` | Animation loop with random colors and sleep() timing |
| `test.ls` | Circle animation demo with random sizes and colors |

### Console

| File | Feature |
|------|---------|
| `print.ls` | console.show(), print(), println() |
| `console_demo.lua` | Console output: print, println, log |
| `terminal_demo.lua` | Built-in terminal commands: dir, list, clear, help, exit |
| `console_read_demo.lua` | Interactive console.read() — prompt the user for input from Lua |

### Sound & Music

| File | Feature |
|------|---------|
| `music_demo.lua` | music.sound() tones and music.mml() melodies |
| `multiply.lua` | Drawing + MML sound effects |

### Input

| File | Feature |
|------|---------|
| `interactive_paint.lua` | Mouse drawing + keyboard color switching (uses input APIs) |

### Text

| File | Feature |
|------|---------|
| `text.lua` | Multi-language text rendering, colors, alpha |

## Running

Place the Tyro executable and `raylib.dll` in the same directory as the script,
or pass the script path directly:

```
tyro demos/pong.lua
tyro demos/basic_drawing.lua
tyro demos/interactive_paint.lua
```

Use `--console` to also show the terminal output:

```
tyro --console demos/pong.lua
```

## Script API Reference

See the main [README.md](../README.md) for the full API documentation.
