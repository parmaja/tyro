window.show(640, 480)
canvas.color = colors.black
canvas.clear()

log("Spirits type = " .. type(Spirits))
log("test1")
if Spirits then
  log("Spirits.new type = " .. type(Spirits.new))
  log("Spirits.find type = " .. type(Spirits.find))
  log("test2")
  
  local player = Spirits.new("player")
  log("Created spirit: " .. tostring(player))
  log("player type = " .. type(player))
  
  if player then
    log("player.load type = " .. type(player.load))
    log("player.move type = " .. type(player.move))
    log("player.show type = " .. type(player.show))
    log("player.width type = " .. type(player.width))
  end
end
log("test done")
running = false
