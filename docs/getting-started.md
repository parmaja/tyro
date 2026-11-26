# Getting Started

[<< Tyro docs index](README.md)

## Your first 10 minutes — no command line needed

Tyro opens as **one window** with an editor, a console and the game screen in
the same place. You do not need a project folder, an install or a terminal.

1. **Start Tyro.** Double-click `tyro.exe`, or run `tyro` with no arguments.
2. **Make a starter script.** Press `F8` to open the console, type
   `new game` and press Enter. The script editor opens with a tiny game.
3. **Edit it.** Press `F2` (or keep the editor open) and change the line
   `local speed = 320` to `local speed = 500`.
4. **Run it.** Press `F5`. Use the arrow keys to move. Press `F5` again any
   time you change the code — every run starts from a clean window.
5. **Keep it.** Press `F8`, type `save mygame` and press Enter. The file goes
   next to the script; open it later with `load mygame` or `F4`.

Press `F1` in the window to see all shortcuts. The console remembers commands:
`list` shows files, `run` reruns, `stop` stops, `sprite` opens the sprite
editor. Type `help` for the full list.

### Copy-paste first game

This is the same pattern as [`demos/first_game.tyro`](../demos/first_game.tyro):
`window.show()` once, then `update(dt)` for movement and `draw()` for pictures.

```lua
window.show(640, 480)
world.backcolor = "navy"

local x = 320
local speed = 200

function update(dt)          -- dt is seconds since the last frame
    if keyboard.down("left")  then x = x - speed * dt end
    if keyboard.down("right") then x = x + speed * dt end
end

function draw()
    screen.clear("blank")
    screen.color = colors.gold
    screen.circle(x, 240, 24, true)
    screen.color = colors.white
    screen.text(10, 10, "Arrow keys to move - F2 to edit, F5 to run")
end
```

`update` and `draw` must be **globals** — do not write `local function update`.
See [the engine loop](engine-loop.md) for how the callbacks are called and for
`enter()` / `leave()` grouping.

## Command line

Once you are comfortable in the window, scripts can also be run directly:

```
tyro [<script>] [--workpath=<dir>] [<options>]
```

Running a `.tyro`/`.lua` file executes it in the graphical environment.
Without a script argument Tyro starts an interactive console session.

| Option | Description |
|--------|-------------|
| `--help`, `-h` | Show the help page and exit |
| `--console`, `-c` | Force a command prompt / console output |
| `--debug`, `-d` | Enable debug logging |
| `--exit[:N]` | Exit after *N* seconds, or — with a bare `--exit` (no `:N`) — as soon as the script finishes |
| `--screenshot:N` | Save `screenshot.png` *N* seconds into the run; opens a window so the screen can be captured |
| `--workpath=<path>` | Workspace directory (defaults to the executable location) |
| `--export=<dir\|game.zip>` | Pack the script named on the command line into a shareable folder or zip (runtime included) |
| `--set-title=<name>` | Window title stored in the exported game's `fuse.conf` |

With an engine-driven script — one that defines `update`/`draw` (see
[Timing](timing.md)) — the script "finishes" the moment its top-level chunk
returns, so a bare `--exit` ends the run right away, before the callbacks get
to run. Use `--exit:N` to let such a script play for *N* seconds.

### Script arguments

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

## Drawing cheat sheet

The canvas primitives are `canvas.clear()`, `canvas.text(x, y, s)`,
`canvas.rectangle(x, y, w, h [, fill])`, `canvas.circle(x, y, r [, fill])`,
`canvas.image(x, y, file [, scale])`,
`canvas.triangle(x1, y1, x2, y2, x3, y3 [, fill])`,
`canvas.line(x1, y1, x2, y2 [, thickness])`, `canvas.point(x, y)` and
`canvas.text_center(y, s)` (centres sideways on that sheet).
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
Grouping commands with `enter()` / `leave()` is covered in
[the engine loop](engine-loop.md).

`id = font.load("name.ttf" [, size [, codepage]])` loads a font and answers its
id — the engine's font book caches it, so the same file and size always answer
the same id and bake only once. `font.use(id)` switches the text-drawing font
to a cached one (`font.current()` answers the id in use), `font.free(id)` throws
one away, and `font.height` / `font.glyphs` describe the active font. The
codepage picks the glyph set: `'ansi'`, `'arabic'` (alias `'cp864'`, the shapes
Arabic needs), or `'everything'`; the default is raylib's small ASCII set.
Notes in [`help/fonts.md`](../help/fonts.md).

`canvas.test(ms)` is a **diagnostic** draw: it draws a marker rectangle and then
spends about *ms* milliseconds on the main thread, emulating a single draw call
slower than a frame. It exists to check that a body grouped with `enter()` /
`leave()` is still drawn as one group; normal scripts do not need it.

## Sound

`sound.generate("effect.wav", freq, ms [, wave [, volume]])` writes a small
16-bit PCM WAV file next to your script. `wave` is `sine` (default), `square`,
`saw`, `noise` or `sweep`; `volume` is 0..1. It is the engine way to make
sound effects — no Python, `wave` module or extra library needed. Asset
generators use it to build their `sfx/` folders.

For sound, `music.beep()`, `music.sound(freq, ms)` or `music.sound(wave, freq,
ms)`, `sound.noise(ms)` (white) / `sound.noise(freq, ms)` (pitched) and
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
