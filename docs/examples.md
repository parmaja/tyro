# Examples

[<< Tyro docs index](README.md)

See [demos/README.md](../demos/README.md) for the full demo index.
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
| `demos/sprites_script.tyro` | Per-sprite scripts: `sprite:load(img, script)` / `sprite:loadscript()` — `on_update`, `on_draw`, `on_collide` |
| `demos/collision.tyro` | Rigid-body physics: dynamic/static bodies, gravity, `sprite.onCollide`, `collision.pump()` |
| `demos/controls.tyro` | Generic buttons, labels, checkboxes, edits, and panels |
| `demos/controls_align.tyro` | Nested controls: a left-docked panel with top- and bottom-docked buttons |
| `demos/aseprites_demo.tyro` | Aseprite animations: load `.aseprite` files (all frames as textures), `play(fps)`, `stop`, `looping`, per-frame stepping — idle/walk/run showcase |
| `demos/sprite_atlas_demo.tyro` | Texture atlas `.sprite` files: load a ZIP'd PNG + JSON atlas (TexturePacker/Phaser/PixiJS), every atlas frame becomes an animation frame, `play`/`stop`, speed control |
| `demos/interactive_paint.tyro` | Mouse drawing with keyboard color switching (uses input APIs) |
| `demos/console_demo.tyro` | Console output: print, println, log |
| `demos/terminal_demo.tyro` | Built-in terminal commands: dir, list, clear, help, exit |
| `demos/console_read_demo.tyro` | Interactive console.read() — prompt the user for input from Lua |
| `demos/music_demo.tyro` | Sound effects (music.sound, sound.noise) and MML melodies (music.mml) |
| `demos/colors_bar.tyro` | Full color palette display |
| `demos/multiply.tyro` | Drawing + MML sound |
| `demos/text.tyro` | Multi-language text rendering |
| `demos/shader_demo.tyro` | Post-processing shaders: water, glow, gray, sepia, invert, vignette, pixelate |
| `demos/shake_demo.tyro` | Screen shake: `world.shake(ms, power)` / `shake(ms, power)` — the world jolts like an accident or an error |
| `demos/torus_demo.tyro` | Free camera: ride the inside of a generated torus loop, boost hoops feed your speed, a chase eye rolls with the tube — `world.camera` / `models.torus` |

