# Save: `save()` / `load()` — remembering between runs

A game that forgets is a toy. Tyro gives persistence the way a kid thinks
about it: one name, one value, no file to invent.

```lua
best = load("best") or 0        -- first run: nil, so 0
if score > best then
    best = score
    save("best", best)          -- written to disk at once
end
canvas.text(8, 8, "best: " .. best)
```

Behind the scenes there is exactly one file: **`save.json` in the game's own
folder** — the folder holding the `.tyro` script. You never type its name,
never parse it, never think about it. Open it in Notepad between runs and
you'll find plain JSON, one member per key:

```json
{"best":4200,"level":3,"sound":false}
```

## What fits in a key

Anything JSON can hold — the same values `json_encode` takes:

```lua
save("score", 120)                    -- numbers
save("hero", "zyya")                  -- text (accents and Arabic ride along)
save("sound", false)                  -- booleans
save("hiscores", {                    -- whole tables, arrays and all
    { name = "ana",  points = 300 },
    { name = "omar", points = 120 },
})
```

`save(key, value)` answers `true`, or `nil` plus a message when the value
cannot be JSON (a function, a sprite, a table that holds itself). That is the
only failure a `save` can bring — a bad name is refused the same way.

`load(key)` answers the value, or `nil` when the game never saved it. The
first run of a game is just a page of empty keys, never an error.

## Deleting a key

Leave the value out — or hand it `nil` — and the key is gone:

```lua
save("tutorial_seen")        -- forget it happened
```

## When it hits the disk

Every `save()` call writes the whole `save.json` right away, on the beat it
was called. Closing the window, pulling the plug, or a crash in the next
frame loses nothing. That is simple on purpose: a kid should never have to
learn the word "flush".

## Neighbours: `savejson` and `score.hi_*`

`savejson(value, "name")` / `loadjson("name")` remain the do-it-yourself
pair: any file name, any shape, any folder in the workspace. They share the
page honestly — if a script rewrites `save.json` with `savejson`, the next
`load()` reads the truth instead of a stale memory. And the old per-game
champion file still works beside it:

```lua
hi = score.hi_save("cave.hi", score)   -- only writes when it is a new best
```

Use `score.hi_*` for a shareable champion file you can mail to a friend,
`save()` for everything else.

## A few manners worth knowing

- **Keys are exact text.** Unlike event names, keys are case-sensitive:
  `save("Best", 1)` and `save("best", 1)` are two different keys — the JSON
  file keeps every name exactly as written.
- **A table comes back as a fresh table.** Mutating what `load` answered
  does not change the saved page until you `save` it again.
- **A broken `save.json` is a bad day, not an error.** If the file is hand-
  edited into nonsense, the game starts from empty keys, and the next
  `save()` repairs the file.
- **Don't store what the engine already keeps** (sprite positions, tweens).
  Save the *recipe*, not the cake.
- Like the rest of the API, `save`/`load` never raise: they answer
  `nil` + a message so your game can decide what forgetting means.
