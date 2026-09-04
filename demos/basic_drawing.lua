--======================================================================
--  basic_drawing.lua - All basic drawing primitives
--======================================================================
--  Demonstrates: canvas.clear, canvas.rectangle, canvas.circle,
--  canvas.line, canvas.point, canvas.text, colors
--======================================================================

window(640, 480)
canvas.color = colors.black
canvas.clear()

canvas.color = colors.white
canvas.text(10, 10, "Basic Drawing Demo")

-- Filled rectangle
canvas.color = colors.blue
canvas.rectangle(50, 50, 100, 80, true)

-- Rectangle outline
canvas.color = colors.red
canvas.rectangle(200, 50, 100, 80, false)

-- Filled circle
canvas.color = colors.green
canvas.circle(150, 200, 40, true)

-- Circle outline
canvas.color = colors.yellow
canvas.circle(300, 200, 40, false)

-- Thick line (using rectangle)
canvas.color = colors.white
canvas.line(10, 400, 630, 400)

-- Single point
canvas.color = colors.fuchsia
canvas.point(320, 300)

-- Color bar using numeric indices
for i = 0, colors.count - 1 do
  canvas.color = colors[i]
  canvas.rectangle(10, 420 + i * 5, canvas.width - 20, 4, true)
end
