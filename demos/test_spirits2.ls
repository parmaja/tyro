window.show(640, 480)
canvas.color = colors.black
canvas.clear()

-- Open console for output
console.show()

println("Test 1: table exists")
println("Spirits type = " .. type(Spirits))
if Spirits then
  println("Spirits.new type = " .. type(Spirits.new))
  println("Spirits.find type = " .. type(Spirits.find))
  
  local player = Spirits.new("player")
  println("Created spirit: " .. tostring(player))
  
  if player then
    println("player type = " .. type(player))
    println("player.load type = " .. type(player.load))
    println("player.move type = " .. type(player.move))
    println("player.show type = " .. type(player.show))
  end
end

-- Don't loop, just test registration
running = false
