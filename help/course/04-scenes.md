# Lesson 4: Space catcher

**Goal:** a title screen, a play state and a game-over card, without writing a
state machine. Use the [scene helper](../../docs/scenes.md).

```lua
window.show(640, 480)
world.backcolor = "navy"

local score = 0

scene.add("title", {
  enter = function() score = 0 end,
  update = function(dt)
    if game.start_pressed() then scene.go("play") end
  end,
  draw = function()
    screen.text_center(160, "SPACE CATCHER")
    screen.text_center(200, "Press SPACE to start")
  end,
})

scene.add("play", {
  update = function(dt)
    -- move the player, catch falling things, add to score ...
    if keyboard.pressed("p") then game.toggle_pause() end
    if game.paused then return end
    if score >= 10 then scene.go("over") end
  end,
  draw = function()
    screen.text(10, 10, "Score: " .. score)
    if game.paused then screen.text_center(220, "PAUSED") end
  end,
  leave = function() music.stop() end,
})

scene.add("over", {
  enter = function()
    score.hi_save("space.hi", score)
  end,
  draw = function()
    screen.text_center(200, "Game over - R to play again")
  end,
  update = function(dt)
    if keyboard.pressed("r") then scene.go("play") end
  end,
})

scene.go("title")
```

`game.paused` freezes the scene but keeps drawing. `restart()` starts the
whole script again from a clean window. Read [Scenes](../../docs/scenes.md).

**Checkpoint:** three states, pause works, and the best score is kept.

Next: [5 - Bubble pop](05-juice.md).
