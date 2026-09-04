--======================================================================
--  interactive_paint.lua - Mouse drawing with keyboard controls
--======================================================================
--  Demonstrates: mousex(), mousey(), ismousepressed(), iskeypressed(),
--  iskeydown() — the new input APIs
--
--  Controls:
--    Left mouse button  - draw circles at cursor
--    SPACE              - clear the screen
--    C                  - cycle drawing color
--======================================================================

window(640, 480)
canvas.color = colors.black
canvas.clear()

-- Color palette (cycle with "c" key)
palette = {colors.white, colors.red, colors.green, colors.blue,
           colors.yellow, colors.fuchsia, colors.aqua}
colorIndex = 0

print("Interactive Paint")
print("Left mouse = draw | SPACE = clear | C = next color")
print("Current color: " .. palette[colorIndex + 1])

while true do
  -- Draw with left mouse button
  if ismousepressed("left") then
    mx = mousex()
    my = mousey()
    canvas.color = palette[colorIndex + 1]
    canvas.circle(mx, my, 5, true)
  end

  -- Clear with SPACE (one press per key press)
  if iskeypressed("space") then
    canvas.color = colors.black
    canvas.clear()
    print("Screen cleared")
  end

  -- Cycle color with C
  if iskeypressed("c") then
    colorIndex = (colorIndex + 1) % #palette
    print("Color changed")
  end

  sleep(10)
end
