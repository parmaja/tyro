window.show(640, 480)
canvas.color = colors.black
canvas.clear()

local f = io.open("spirits_test_output.txt", "w")

f:write("Spirits type = " .. type(Spirits) .. "\n")
if type(Spirits) ~= "table" then
  f:write("Spirits is not a table!\n")
  f:close()
  running = false
  return
end

f:write("Spirits.new type = " .. type(Spirits.new) .. "\n")
f:write("Spirits.find type = " .. type(Spirits.find) .. "\n")

-- Try calling new
local player = Spirits.new("player")
f:write("Created spirit: " .. tostring(player) .. "\n")
f:write("player type = " .. type(player) .. "\n")

if player then
  f:write("player.load type = " .. type(player.load) .. "\n")
  f:write("player.move type = " .. type(player.move) .. "\n")
  f:write("player.show type = " .. type(player.show) .. "\n")
  
  -- Test load
  local ok, err = pcall(function()
    player.load("richard-say.png")
  end)
  f:write("load ok=" .. tostring(ok) .. " err=" .. tostring(err) .. "\n")
  f:write("width=" .. tostring(player:width()) .. " height=" .. tostring(player:height()) .. "\n")
  
  player.show()
  player.move(100, 200)
  f:write("After move, x=" .. player.x .. " y=" .. player.y .. "\n")
  f:write("angle=" .. player.angle .. " scale=" .. player.scale .. "\n")
  
  -- Test named lookup
  local found = Spirits("player")
  f:write("Found by name: " .. tostring(found) .. "\n")
  if found then
    found.angle = 45
    f:write("Found angle=" .. found.angle .. "\n")
  end
end

f:write("Test complete\n")
f:close()

running = false
