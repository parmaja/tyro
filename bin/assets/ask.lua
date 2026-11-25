-- ask.lua - a non-blocking input box. No console.read() freeze.
-- Usage:
--   local ask = require("assets/ask")
--   ask("What is your name?", function(name)
--       print("Hello " .. name)
--   end)
-- The game keeps running while the player types; the callback fires on OK.

local M = {}
local label, box, button
local callback = nil
local handler = nil
local active = false

local function hide()
  if handler then
    off(handler)
    handler = nil
  end
  if label then label.visible = false end
  if box then box.visible = false end
  if button then button.visible = false end
  active = false
end

function M.show(prompt, fn)
  if active then hide() end
  local w, h = screen.width, screen.height
  local left = math.floor(w / 2) - 140
  local top = math.floor(h / 2) - 44

  if not box then
    label = controls.new("label", "ask_label", left, top, 280, 24)
    box = controls.new("edit", "ask_box", left, top + 28, 280, 36)
    button = controls.new("button", "ask_button", left + 95, top + 72, 90, 32)
  else
    controls.position(label, left, top)
    controls.position(box, left, top + 28)
    controls.position(button, left + 95, top + 72)
  end

  controls.text(label, prompt or "")
  controls.text(box, "")
  controls.text(button, "OK")
  label.visible = true
  box.visible = true
  button.visible = true
  callback = fn
  active = true

  -- The engine fires on("update") every frame, so this widget needs no
  -- polling call in the game's own update().
  handler = on("update", function()
    if button and button.clicked then
      local answer = controls.text(box)
      hide()
      if callback then
        local done = callback
        callback = nil
        done(answer)
      end
    end
  end)
end

M.ask = M.show
M.hide = hide
M.active = function() return active end

return M
