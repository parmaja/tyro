--Paints an image control in a shown window and captures the frame, so the
--rendered result of controls.new("image", ...) is checked, not only its state.
--The capture lands in the process working directory (screenshot takes a plain
--path), so run this from the folder you want it in and look at the png.

window.show(320, 240)

local pic = controls.new("image", "test_image_tile.png", 40, 40, 128, 128, "pic")

canvas.clear()

frames = 0
while cycle do
  frames = frames + 1
  if frames == 10 then
    screenshot("test_image_show.png")
  end
  if frames > 20 then
    break
  end
end

exit()