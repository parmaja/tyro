# Fonts: `font.*` — load, switch, come back

A game usually needs more than one typeface: the house font for the score,
a big bold one for the title, maybe an Arabic one for the story. Tyro keeps
**every font you ever loaded in a book**, and each one carries a small
number — its id. You hold the number, the engine holds the font.

```lua
title = font.load("BigTitle.ttf", 48)   -- answers an id, right away
score = font.load("score.ttf", 16)      -- another id, cached in the book

font.use(title)                         -- from now on text draws with it
canvas.text(10, 10, "GAME OVER")
font.use(score)                         -- switch back whenever you like
```

## The calls

| Call | What it does |
| --- | --- |
| `id = font.load(name [, size [, codepage]])` | loads a font and answers its id; the font becomes the active one |
| `font.use(id)` | makes a cached font the active one again — no loading, no waiting |
| `font.free(id)` | throws one cached font away (never the active one) |
| `font.current()` | the id of the font text draws with right now |
| `font.height` | height of the active font, in pixels |
| `font.glyphs` | how many glyphs the active font really baked |

The id number never gets reused, and the same file at the same size and
codepage always answers **the same id** — loading it twice costs nothing,
the atlas is baked once and the book hands you back the number:

```lua
a = font.load("game.ttf", 24)
b = font.load("game.ttf", 24)   -- same font again: a == b
```

A font that is in use cannot be freed — `font.free` answers `nil` and a
message until you `font.use(something_else)` first. This is a promise, not
a punishment: the screen is drawing with that font *right now*.

Like every drawing call, loading and switching run on the engine's queue:
write `font.use(title)` then `canvas.text(...)` in the same beat and they
happen in that order. The id itself is already real when `font.load`
answers, even though the atlas bakes a beat later.

## Codepages: only the letters you need

By default raylib bakes the 95 printable ASCII characters — enough for
English, small in memory. The third argument picks a different set of
glyphs, called a **codepage**:

| Codepage | What it carries |
| --- | --- |
| *(nothing)* | raylib's default: 95 printable ASCII |
| `'ansi'` (`'cp1252'`) | plus Latin-1: é, ñ, ¿ — most of Western Europe |
| `'arabic'` (`'cp864'`) | ASCII plus the Arabic letters and the shaped forms of the CP864 codepage |
| `'everything'` | every glyph the file holds — huge, experimental, may refuse big sizes |

The two spellings of a codepage are aliases: `'cp864'` and `'arabic'` name
one entry, one id. A name the engine does not know raises honestly (the old
font keeps drawing) instead of silently meaning ASCII.

`'arabic'` is the codepage `minibidi`'s shaped output lives in: the forms
it produces are exactly the ones this set bakes, so an Arabic game can carry
a few hundred glyphs instead of tens of thousands.

```lua
arabic = font.load("DejaVuSans.ttf", 24, "arabic")

function update(dt)
    -- by the time the first frame runs, the bake is done:
    print(font.glyphs)     -- a few hundred, not thousands
end
```

## A small show

```lua
house  = font.current()           -- the font the engine started with
title  = font.load("arcade.ttf", 40)

font.load("dejavu.ttf", 20, "ansi")  -- now this one is active
font.free(house)                     -- the engine's start font is idle: free to go
font.use(title)                      -- back to the arcade face
```

`font.load` never destroys the font it replaces — everything waits in the
book until `font.free` says otherwise. That is what makes the come-back
instant: `font.use` never re-reads a file.
