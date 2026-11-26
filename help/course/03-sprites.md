# Lesson 3: Catch the stars

**Goal:** use sprites, a score and a collision check — the heart of most games.

Sprites need no image files: `sprites.blob()` bakes a soft shape for you.

```lua
window.show(640, 480)
world.backcolor = "navy"

local player = sprites.blob("hero", 48)
player.tint = colors.cyan
player.x, player.y = 320, 420

local stars = {}
for i = 1, 6 do
  local s = sprites.blob("star" .. i, 28)
  s.tint = colors.gold
  s.x = math.random(40, 600)
  s.y = -20
  stars[i] = s
end

local catches = 0

function update(dt)
  if keyboard.down("left")  then player.x = player.x - 260 * dt end
  if keyboard.down("right") then player.x = player.x + 260 * dt end

  for i = 1, #stars do
    local s = stars[i]
    s.y = s.y + 90 * dt
    if s.y > 500 then
      s.y = -20
      s.x = math.random(40, 600)
    end
    if dist(s.x, s.y, player.x, player.y) < 34 then
      catches = catches + 1
      s.y = -20
      music.sound("Coin", 880, 60)
    end
  end
end

function draw()
  screen.clear("blank")
  screen.color = colors.white
  screen.text(10, 10, "Caught: " .. catches)
end
```

`dist(x1,y1,x2,y2)` is one of the math helpers in [game](../../docs/game.md).
Read [sprites](../../docs/sprites.md) for images, sheets, animations and Aseprite.

**Checkpoint:** stars fall, you catch them, the score and a sound respond.

Next: [4 - Space catcher](04-scenes.md).
