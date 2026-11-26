# Lesson 1: Dots and boxes

**Goal:** draw something that appears on screen the moment you press `F5`.

Press `F8`, type `new draw`, then `F2` and replace the whole editor buffer with:

```lua
window.show(640, 480)
world.backcolor = "navy"

function draw()
  screen.color = colors.gold
  screen.circle(320, 240, 60, true)
  screen.color = colors.white
  screen.text(10, 10, "Hello, Tyro!")
end
```

Press `F5`. `draw()` runs once per frame, so whatever it draws stays on screen.

Try changing `60` to `120`, and `colors.gold` to `colors.cyan`.
`screen` is the top layer that is always visible; `canvas` is an off-screen
sheet and `world` is the scrolling playfield. You can read about them in
[drawing](../../help/drawing.md).

**Checkpoint:** you can draw a shape and change its colour and size.

Next: [2 - Catch the dot](02-moving.md).
