--======================================================================
--  controls.ls - Button controls
--======================================================================
--  Demonstrate the built-in button control: buttons are created from
--  Lua and are drawn (self-drawn) every frame by the main drawing cycle.
--  The BorderSize drives how rounded the border is (bigger = rounder).
--
--  API:
--    buttons.new(caption, x, y, w?, h?, borderSize?) -> handle
--    buttons.caption(handle [, text])   get/set caption
--    buttons.border(handle [, size])    get/set border size
--    buttons.hover(handle)              mouse is over the button
--    buttons.down(handle)               button is pressed
--    buttons.clicked(handle)            true once after a click
--    buttons.count                      number of created buttons

window.show(640, 480)

-- border sizes 2, 6 and 10 -> three levels of rounded corners
b1 = buttons.new("Open", 40, 40, 100, 36, 2)
b2 = buttons.new("Save", 160, 40, 100, 36, 6)
b3 = buttons.new("Close", 280, 40, 100, 36, 10)

-- a counter button: click it to change its caption
counter = buttons.new("Click me!", 120, 130, 140, 40, 4)

clicks = 0

while true do
  if buttons.clicked(counter) then
    clicks = clicks + 1
    buttons.caption(counter, "Clicked " .. clicks)
  end

  -- status line for every button (cleared by a black box first)
  status = ""
  for i = 1, buttons.count do
    if status ~= "" then status = status .. "  |  " end
    if buttons.down(i) then
      status = status .. "#" .. i .. "=down"
    elseif buttons.hover(i) then
      status = status .. "#" .. i .. "=hover"
    else
      status = status .. "#" .. i .. "=idle"
    end
  end

  canvas.color = colors.black
  canvas.rectangle(40, 210, 420, 60, true)
  canvas.color = colors.white
  canvas.text(40, 220, "border: tiny (2)   middle (6)   big (10)")
  canvas.text(40, 240, status)
  canvas.text(40, 260, "buttons.count = " .. buttons.count)

  sleep(16)
end