# Tyro Demos

This folder contains Lua script examples for the Tyro engine. Scripts can be
run with `tyro <script>`.

## Quick Start

```
tyro demos/pong.tyro
```

## Demo Index

### Showcase

| File | Feature |
|------|---------|
| `pong.tyro` | Complete Pong game — drawing, input, AI, physics, sound, scoring |

### Drawing

| File | Feature |
|------|---------|
| `basic_drawing.tyro` | All drawing primitives: rectangle, circle, line, point, text, colors |
| `colors_bar.tyro` | Iterate the full color palette |
| `animated_demo.tyro` | Animation loop with random colors and sleep() timing |
| `cycle_demo.tyro` | Per-frame loop using `while cycle do` — one drawing per raylib frame |
| `test.tyro` | Circle animation demo with random sizes and colors |

### Animation

| File | Feature |
|------|---------|
| `aseprites_demo.tyro` | Load and play Aseprite animations from `demos/aseprites/` — `sprite:play(fps)`, `stop`, `looping`, `frames`/`frame` — with a controllable idle/walk/run hero |

### Console

| File | Feature |
|------|---------|
| `print.tyro` | console.show(), print(), println() |
| `console_demo.tyro` | Console output: print, println, log |
| `terminal_demo.tyro` | Built-in terminal commands: dir, list, clear, help, exit |
| `console_read_demo.tyro` | Interactive console.read() — prompt the user for input from Lua |

### Sound & Music

| File | Feature |
|------|---------|
| `music_demo.tyro` | music.sound() tones and music.mml() melodies |
| `multiply.tyro` | Drawing + MML sound effects |
| `spectrum_demo.tyro` | Internet radio + live stereo spectrum analyzer (`spectrum.show`, `spectrum.bars`) |

### Effects

| File | Feature |
|------|---------|
| `shader_demo.tyro` | Post-processing shaders: `shader.effect`, `shader.value`, `shader.area` and `shader.load("custom.frag")` |
| `shake_demo.tyro` | Screen shake: `shake(ms, power)` / `window.shake(ms, power)`, `window.shaking` — the world jolts and fades out, like an accident or an error |

### Input

| File | Feature |
|------|---------|
| `interactive_paint.tyro` | Mouse drawing + keyboard color switching (uses input APIs) |

### Controls

| File | Feature |
|------|---------|
| `controls.tyro` | Generic control table: `controls.new('button'/'label'/'checkbox'/'edit'/'panel'/'image', ...)` with text, checked, position/size, visible, hover/down/clicked, focus, border, backcolor, name |
| `controls_align.tyro` | Nested aligned controls: a panel docked left, buttons docked to its top, and one button docked to its bottom; resizing reflows the whole tree |

### Text

| File | Feature |
|------|---------|
| `text.tyro` | Multi-language text rendering, colors, alpha |

### Files

| File | Feature |
|------|---------|
| `../tests/test_openfile.tyro` | `openfile` smoke test: text and binary reads, numbers, append, `f:lines()`, chained writes and the names that are refused for leaving the workspace. Results land in `openfile_out.txt`. |
| `rawio_test.tyro` | Minimal `openfile` write with `flush()` in between |
| `diag_sprite.tyro` | `openfile` used as the log file for a sprite load diagnosis |

## Running

Place the Tyro executable and `raylib.dll` in the same directory as the script,
or pass the script path directly:

```
tyro demos/pong.tyro
tyro demos/basic_drawing.tyro
tyro demos/interactive_paint.tyro
```

Use `--console` to also show the terminal output:

```
tyro --console demos/pong.tyro
```

## Script API Reference

See the main [README.md](../README.md) for the full API documentation.
