local f, ferr = openfile("diag.txt", "w")
if not f then
  log("cannot open diag.txt: " .. tostring(ferr))
  running = false
  return
end
f:write("step1 start\n")
f:flush()
window.show(640, 480)
f:write("step2 window shown\n")
f:flush()
local bg = Sprites.new("bg")
f:write("step3 sprite created " .. tostring(bg) .. "\n")
f:flush()
local ok, err = pcall(function() bg.load("richard-say.png") end)
f:write("step4 load ok=" .. tostring(ok) .. " err=" .. tostring(err) .. "\n")
f:flush()
f:write("step5 width=" .. tostring(bg:width()) .. " height=" .. tostring(bg:height()) .. " visible=" .. tostring(bg.visible) .. "\n")
f:flush()
bg.show()
bg.move(100, 100)
f:write("step6 done\n")
f:flush()
f:close()