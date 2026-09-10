--======================================================================
-- spirits_demo.lua  Sprite loading, drawing, moving & transforms
--======================================================================
-- Demonstrates the Spirits system:
--   Spirits.new("name")     - create a named spirit
--   spirit:load("image.png") - load a texture into the spirit
--   spirit:show()            - make the spirit visible
--   spirit:hide()            - make the sprite invisible
--   spirit:move(x, y)        - set position
--   spirit.x / spirit.y      - get/set position
--   spirit.angle             - get/set rotation angle
--   spirit.scale             - get/set scale
--   spirit.width() / spirit.height() - dimensions
--   Spirits("name")          - look up a spirit by name
--   Spirits.find("name")     - same as above, returns spirit or nil
--
-- The engine draws all visible spirits every frame automatically.
--======================================================================

window.show(640, 480)
canvas.color = colors.black
canvas.clear()

-- Create a spirit named "player"
local player = Spirits.new("player")
player.load("richard-say.png")
player.show()
player.move(100, 200)

-- Create a second spirit without a name
local ghost = Spirits.new()
ghost.load("richard-say.png")
ghost.show()
ghost.angle = 45
ghost.scale = 0.5
ghost.move(300, 100)

-- Access by name
local found = Spirits("player")
if found then
  print("Found spirit by name, width=" .. found.width() .. " height=" .. found.height())
end

local angle = 0

while true do
  -- Clear the screen each frame
  canvas.color(colors.black)
  canvas.clear()

  -- The engine draws all spirits automatically, so we just update properties
  player.move(mousex(), mousey())
  player.angle = angle

  ghost.x = ghost.x + 1
  if ghost.x > 640 then ghost.x = 0 end

  -- Draw some text
  canvas.color = colors.white
  canvas.text(10, 10, "Move mouse | SPACE toggles scale | R resets angle")

  -- Animate rotation
  angle = angle + 1
  if angle >= 360 then angle = 0 end

  if iskeypressed("space") then
    ghost.scale = ghost.scale + 0.2
    if ghost.scale > 3.0 then ghost.scale = 0.5 end
  end

  if iskeypressed("r") then
    angle = 0
    player.angle = 0
  end

  sleep(16)  -- ~60 FPS
end
