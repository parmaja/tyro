# Command Line

[<< Tyro docs index](README.md)

```
tyro [<script>] [--workpath=<dir>] [<options>]
```

Running a `.tyro`/`.lua` file executes it in a graphical environment. Without a
script argument Tyro starts an interactive console session.

| Option | Description |
|--------|-------------|
| `--help`, `-h` | Show the help page and exit |
| `--console`, `-c` | Force a command prompt / console output |
| `--debug`, `-d` | Enable debug logging |
| `--exit[:N]` | Exit after *N* seconds, or — with a bare `--exit` (no `:N`) — as soon as the script finishes |
| `--screenshot:N` | Save `screenshot.png` *N* seconds into the run; opens a window so the screen can be captured |
| `--workpath=<path>` | Workspace directory (defaults to the executable location) |

With an engine-driven script — one that defines `update`/`draw` (see
[Timing](timing.md)) — the script "finishes" the moment its top-level chunk
returns, so a bare `--exit` ends the run right away, before the callbacks get
to run. Use `--exit:N` to let such a script play for *N* seconds.

## Script arguments

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

The canvas primitives are `canvas.clear()`, `canvas.text(x, y, s)`,
`canvas.rectangle(x, y, w, h [, fill])`, `canvas.circle(x, y, r [, fill])`,
`canvas.image(x, y, file [, scale])`,
`canvas.triangle(x1, y1, x2, y2, x3, y3 [, fill])`,
`canvas.line(x1, y1, x2, y2 [, thickness])` and `canvas.point(x, y)`.
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
`leave()` is still drawn as one group (see [docs/atomic.md](atomic.md));
normal scripts do not need it.

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

