window.show(640, 480)
canvas.color = colors.black
canvas.clear()

local f = io.open("sprites_test_output.txt", "w")

f:write("Sprites type = " .. type(Sprites) .. "\n")
if type(Sprites) ~= "table" then
  f:write("Sprites is not a table!\n")
  f:close()
  running = false
  return
end

f:write("Sprites.new type = " .. type(Sprites.new) .. "\n")
f:write("Sprites.find type = " .. type(Sprites.find) .. "\n")

-- Try calling new
local player = Sprites.new("player")
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
  local found = Sprites("player")
  f:write("Found by name: " .. tostring(found) .. "\n")
  if found then
    found.angle = 45
    f:write("Found angle=" .. found.angle .. "\n")
  end
end

f:write("Test complete\n")
f:close()

running = false
