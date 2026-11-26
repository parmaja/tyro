-- pixelart.lua - draw sprites with Tyro itself, no Python/Pillow needed.
-- Usage:
--   local px = require("assets/pixelart")
--   local ship = px.sheet(64, 48)
--   px.poly(ship, { 32,2, 60,44, 4,44 }, colors.cyan)
--   px.save(ship, "ship.png")
-- Every draw call is queued like other Tyro drawing; px.save writes the
-- finished pixels as a PNG inside the script's workspace.

local M = {}

function M.sheet(w, h, color)
  local s = images.new(w, h)
  if color then
    s.color = color
    s.rectangle(0, 0, w, h, true)
  end
  return s
end

-- Triangle fan, enough for any convex polygon in pixel art.
function M.poly(s, points, color, outline)
  s.color = color
  for i = 2, #points / 2 - 1 do
    s.triangle(points[1], points[2],
      points[i * 2 - 1], points[i * 2],
      points[i * 2 + 1], points[i * 2 + 2], true)
  end
  if outline then
    s.color = outline
    for i = 1, #points / 2 do
      local j = i % (#points / 2) + 1
      s.line(points[i * 2 - 1], points[i * 2],
        points[j * 2 - 1], points[j * 2])
    end
  end
end

function M.rect(s, x, y, w, h, color, fill)
  s.color = color
  s.rectangle(x, y, w, h, fill == nil or fill)
end

function M.circle(s, x, y, r, color, fill)
  s.color = color
  s.circle(x, y, r, fill == nil or fill)
end

-- A filled rectangle with a border, the shape most placeholder art uses.
function M.box(s, x, y, w, h, fill, border)
  s.color = fill
  s.rectangle(x, y, w, h, true)
  if border then
    s.color = border
    s.rectangle(x, y, w, h, false)
  end
end

function M.line(s, x1, y1, x2, y2, color)
  s.color = color
  s.line(x1, y1, x2, y2)
end

function M.save(s, name)
  return images.save(s, name)
end

-- An animation atlas: one shared sheet with `count` frames of tw x th,
-- saved as name.png plus name.sprite. drawFrame(sheet, index, offsetX)
-- paints frame `index` starting at offsetX; frames are named f0, f1, ...
function M.atlas(name, tw, th, count, duration, drawFrame)
  local sheet = images.new(tw * count, th)
  local frames = {}
  for i = 0, count - 1 do
    drawFrame(sheet, i, i * tw)
    frames[i + 1] = {
      image = sheet, name = "f" .. i,
      x = i * tw, y = 0, w = tw, h = th, duration = duration,
    }
  end
  return images.savesprite(name .. ".sprite", frames)
end

return M
