--======================================================================
--  animated_demo.lua - Animation loop with random colors
--======================================================================
--  Demonstrates: while-loop animation, random colors, sleep() timing
--======================================================================

window(640, 480)
canvas.color = colors.black
canvas.clear()
canvas.color = colors.white
canvas.text(10, 10, "Animated Demo - watch the circles appear!")

-- Animated circle that bounces left/right
x = 0
direction = 1

while x < canvas.width do
  -- pick a random color from the palette
  c = math.random(0, colors.count - 1)
  canvas.color = colors[c]

  r = math.random(5, 20)
  y = 100 + math.random(0, 50)
  canvas.circle(x, y, r, true)

  x = x + r
  sleep(20)
end

canvas.color = colors.white
canvas.text(10, 250, "Done!")
