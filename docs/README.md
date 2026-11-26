# Tyro Documentation

Tyro is a small game engine for kids and noobs: the [raylib](https://www.raylib.com/)
drawing/sound engine driven by [Pluto](https://pluto-lang.org/) Lua scripts,
wrapped in an editor and console, built with FPC/Lazarus. You write an `update(dt)`
and a `draw()` function; Tyro runs the window, the frame loop, the fonts, the
sounds, and the physics.

## Start here

0. **New here?** Follow [Make 5 Games](../help/course/README.md) — five short
   lessons that grow from drawing to sharing a game.
1. [Getting started](getting-started.md) — run your first script, the command
   line, script arguments.
2. [Input](input.md) — keyboard, mouse, touch, gamepad.
3. [Sprites](sprites.md) — pictures that move; the heart of every Tyro game.
4. [Timing & game loop](timing.md) — `update`/`draw`, timers, tweens, events,
   joints, saving between runs.
5. [Examples](examples.md) — the demo index in `demos/`.

## The API, by topic

| Page | Covers |
| --- | --- |
| [Getting started](getting-started.md) | command line, script arguments, first example |
| [Input](input.md) | `keyboard.*`, `mouse.*`, `touch.*`, `gamepad.*` |
| [Game helper](game.md) | `game.*` button pairs, `score.*` high scores, math helpers |
| [Scenes](scenes.md) | named menu/play/gameover states, pause/resume, restart, difficulty |
| [Files](files.md) | `openfile`, the workspace and its paths |
| [Network](network.md) | `http.*`, `json.*` |
| [Console](console.md) | the built-in terminal and its commands |
| [Sprites](sprites.md) | creating, methods, properties |
| [Tilemaps](tilemaps.md) | sheet-backed grids, `map.*` |
| [Physics](physics.md) | gravity, collisions, joints between sprites |
| [3D models](models.md) | loading models, lights and shadows |
| [Cameras](cameras.md) | the free 3D camera and the 2D camera |
| [Shader effects & screen shake](effects.md) | built-in shader presets, custom shaders, `shake.*` |
| [Controls](controls.md) | in-window buttons, labels, checkboxes, edits, lists, images |
| [Timing](timing.md) | `update`/`draw`, `after`/`every`, `tween`, `on`/`emit`, `collision.joint`, `save`/`load`, `enter`/`leave`, `queue.*` |
| [Examples](examples.md) | what to read in `demos/` |
| [Threading & lifecycle](threading.md) | the script thread, the queue, how objects die |

## Going deeper

The [`help/`](../help/) folder holds the story behind individual features —
why they work the way they do: [drawing](../help/drawing.md),
[events](../help/events.md), [fonts](../help/fonts.md),
[fuse](../help/fuse.md), [game](../help/game.md), [joints](../help/joints.md),
[MML music](../help/mml.md), [particles](../help/particles.md),
[save](../help/save.md), [touch](../help/touch.md), [tween](../help/tween.md).

Build and test automation: [CI](CI.md). Engine internals live beside this index:
[the engine loop](engine-loop.md)
(which also covers `enter()`/`leave()` grouping), the [roadmap](ROADMAP.md),
and the newer [ROADMAP02.md](ROADMAP02.md) with the kid-facing gaps closed in
November.
