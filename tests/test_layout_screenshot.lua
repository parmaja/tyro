-- Layout smoke test for the TTyroWindow / Main.BoundsRect fixes.
-- Fill the board area so the margin padding is visible, capture a screenshot
-- of the presented frame, then quit once the file exists on disk.
-- Run: bin\tyro.exe tests\test_layout_screenshot.lua

canvas.color(0x00, 0xFF, 0xFF) -- cyan board fill, contrasts with the margin
canvas.rectangle(0, 0, canvas.width, canvas.height, true)
screenshot("layout_full.png")

local waited = 0
while waited < 300 do
  local f = openfile("layout_full.png", "rb")
  if f then
    f:close()
    exit()
  end
  sleep(50)
  waited = waited + 1
end
log("screenshot was never captured")
exit()
