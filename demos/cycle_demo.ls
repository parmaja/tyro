--======================================================================
--  cycle_demo.lua - one drawing per frame using the cycle gate
--======================================================================
--  Demonstrates: while cycle do ... end — the loop body runs at most once
--  per raylib drawing frame, so drawing commands cannot pile up inside a
--  single "while true do" + sleep() cycle.
--======================================================================

window.show(640, 480)

x = 50
y = 100
dx = 3
dy = 2
r = 12

while cycle do
  -- clear the previous frame
  canvas.color = colors.black
  canvas.rectangle(0, 0, canvas.width, canvas.height, true)

  -- title
  canvas.color = colors.white
  canvas.text(10, 10, "Cycle Demo - the ball is drawn once per frame")

  -- move the ball
  x = x + dx
  y = y + dy
  if x < r or x > canvas.width - r then dx = -dx end
  if y < r or y > canvas.height - r then dy = -dy end

  -- draw it (flushed at the next drawing cycle)
  canvas.color = colors.white
  canvas.circle(x, y, r, true)
end