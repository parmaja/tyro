--======================================================================
--  output.ls - Output control
--======================================================================
--  The output control catches everything sent through print(),
--  println() and log() and shows it on screen. print/println/log
--  still also go to the console as usual.
--
--  API:
--    output.show(x, y [, w, h])  show the output control and place it
--    output.hide()               hide it
--    output.clear()              clear its lines
--    output.x / y / width / height / border / margin / maxlines
--    output.visible              get/set
--    output.lines                number of lines currently held
--    output.textColor / backColor  get/set

-- open the console so the mirrored text is visible too
console.show()

window.show(640, 480)

output.show(40, 40)
output.x = 40
output.y = 40
output.width = 480
output.height = 200
output.border = 1
output.textColor = colors.black -- contrasts with the light window backcolor

print("output control ready")
println("print/println/log go here AND to the console")
log("log() lines are mirrored here too")

-- show/hide cycle
timer = 0
visible = true

while true do
  timer = timer + 1
  if timer == 120 then
    if visible then
      output.hide()
    else
      output.show(40, 40, 480, 200)
    end
    visible = not visible
    timer = 0
  end

  if iskeypressed(keys.space) then
    println("space pressed at time " .. time())
  end

  sleep(16)
end