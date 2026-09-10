window.show(640, 480)
canvas.color = colors.black
canvas.clear()

log("Testing Spirits")

local player = Spirits.new("player")
log("Created spirit, handle check via width before load: " .. player:width())

-- Check file existence
local f = io.open("richard-say.png", "rb")
if f then
  log("File richard-say.png exists in current dir")
  f:close()
else
  log("File richard-say.png NOT found in current dir")
end

-- Check workspace path
log("Trying load...")
local ok, err = pcall(function()
  player.load("richard-say.png")
end)
log("load ok=" .. tostring(ok) .. " err=" .. tostring(err))

log("width after load=" .. tostring(player:width()))
log("height after load=" .. tostring(player:height()))

player.show()
log("visible after show=" .. tostring(player.visible))
player.move(100, 200)
log("x=" .. tostring(player.x) .. " y=" .. tostring(player.y))

running = false
