# Lesson 2: Catch the dot

**Goal:** move with the arrow keys at the same speed on every computer.

Start from lesson 1 and add `update(dt)`:

```lua
window.show(640, 480)
world.backcolor = "navy"

local x, y = 320, 240
local speed = 220

function update(dt)
  if keyboard.down("left")  then x = x - speed * dt end
  if keyboard.down("right") then x = x + speed * dt end
  if keyboard.down("up")    then y = y - speed * dt end
  if keyboard.down("down")  then y = y + speed * dt end
  x = clamp(x, 20, 620)
  y = clamp(y, 20, 460)
end

function draw()
  screen.clear("blank")
  screen.color = colors.gold
  screen.circle(x, y, 18, true)
  screen.color = colors.white
  screen.text(10, 10, "Arrows to move")
end
```

`dt` is the seconds since the previous frame. Multiplying by `dt` means the
dot travels `220` pixels per **second**, whatever the frame rate.

Try changing `speed` to `400`. Read [input](../../docs/input.md) for gamepad
and touch, and [the engine loop](../../docs/engine-loop.md) for why callbacks
are globals.

**Checkpoint:** the dot moves smoothly and stops at the window edges.

Next: [3 - Catch the stars](03-sprites.md).
