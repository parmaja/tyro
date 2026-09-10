window.show(640, 480)
canvas.color = colors.black
canvas.clear()

log("Testing Spirits")

local player = Spirits.new("player")
log("Created spirit")

-- Test load with different paths
log("Path 1: demos/richard-say.png")
local ok, err = pcall(function()
  player.load("demos/richard-say.png")
end)
log("load ok=" .. tostring(ok) .. " err=" .. tostring(err))
log("width=" .. tostring(player:width()))
log("height=" .. tostring(player:height()))

running = false
