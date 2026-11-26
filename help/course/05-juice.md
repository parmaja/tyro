# Lesson 5: Bubble pop

**Goal:** make it feel alive, keep progress, and share it.

Use `tween()` for motion, `particles.emit()` for bursts and `save()` for
progress. The complete, runnable version is
[`demos/first_game.tyro`](../../demos/first_game.tyro) and the showcase is
[`games/bubblepop`](../../games/bubblepop/README.md).

```lua
window.show(640, 480)
world.backcolor = rgb(16, 40, 84)

local bubble = sprites.blob("b", 96)
bubble.tint = rgb(120, 190, 255)
bubble.x, bubble.y = 320, 500

tween.from(bubble, "scale", 0, 1, 340, "backout")
tween(bubble, "y", -70, 3000, "linear")

function update(dt)
  if mouse.pressed("left") and
     dist(mouse.x, mouse.y, bubble.x, bubble.y) < 60 then
    particles.emit(bubble.x, bubble.y, 14, { color = colors.white })
    music.sound("Bubble", 660, 70)
    save("pops", (load("pops") or 0) + 1)
    bubble.y = 500
    tween.from(bubble, "scale", 0, 1, 340, "backout")
    tween(bubble, "y", -70, 3000, "linear")
  end
end

function draw()
  screen.clear("blank")
  screen.color = colors.white
  screen.text(10, 10, "Pops: " .. tostring(load("pops") or 0))
end
```

`save("key", value)` writes `save.json` next to your game;
`load("key")` reads it back. Read [timing](../../docs/timing.md),
[particles](../../help/particles.md) and [save](../../help/save.md).

## Share it

From the repo root:

```
bin\tyro.exe games\bubblepop\bubblepop.tyro --export=bubblepop.zip --set-title="Bubble Pop"
```

Send the zip to a friend. They unzip it and run `tyro.exe` — no install.

**You have made five games.** Next, open [the docs index](../../docs/README.md)
and pick a topic: tilemaps, physics, shaders, gamepad, network or 3D models.
