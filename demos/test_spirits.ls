window.show(640, 480)
canvas.color = colors.black
canvas.clear()

log("Spirits type = " .. type(Spirits))

local player = Spirits.new("player")
log("Created spirit: " .. tostring(player))

-- Test load
local ok, err = pcall(function()
  player.load("richard-say.png")
end)
log("load ok=" .. tostring(ok) .. " err=" .. tostring(err))

log("width=" .. tostring(player:width()))
log("height=" .. tostring(player:height()))

player.show()
player.move(100, 200)
log("After move, x=" .. tostring(player.x) .. " y=" .. tostring(player.y))
log("angle=" .. tostring(player.angle) .. " scale=" .. tostring(player.scale))
log("visible=" .. tostring(player.visible))

-- Test property setting
player.angle = 45
log("After set angle, angle=" .. tostring(player.angle))

player.scale = 2.0
log("After set scale, scale=" .. tostring(player.scale))

player.x = 50
log("After set x, x=" .. tostring(player.x))

player.y = 60
log("After set y, y=" .. tostring(player.y))

-- Test named lookup
local found = Spirits("player")
log("Found by name: " .. tostring(found))
if found then
  log("Found width=" .. tostring(found:width()))
end

-- Test hide/show
player.hide()
log("After hide, visible=" .. tostring(player.visible))
player.show()
log("After show, visible=" .. tostring(player.visible))

-- Test find
local found2 = Spirits.find("player")
log("Find by name: " .. tostring(found2))

-- Test drawing in a loop
log("Starting loop...")
local a = 0
while true do
  canvas.color = colors.black
  canvas.clear()

  player.move(mousex(), mousey())
  player.angle = a
  a = a + 1
  if a >= 360 then a = 0 end

  canvas.color = colors.white
  canvas.text(10, 10, "Spirit demo - move mouse")

  sleep(16)
  
  -- Exit after 2 seconds for testing
  if a == 1 then
    break
  end
end
log("Loop ended, test complete")
