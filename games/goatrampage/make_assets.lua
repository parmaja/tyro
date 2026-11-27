--======================================================================
--  Goat Rampage - all art, painted by Tyro (no Python, no Pillow)
--======================================================================
--  Run from the repository root:
--    bin\tyro.exe games\goatrampage\make_assets.lua --workpath=games\goatrampage --exit
--
--  Everything lands in games/goatrampage/img/. The game only loads these
--  PNGs; the script itself never needs an external image editor.
--======================================================================

local P = "img/"

local WHITE = rgb(248, 246, 238)
local SHADE = rgb(214, 210, 198)
local HORN  = rgb(198, 186, 158)
local SNOUT = rgb(246, 176, 176)
local EYE   = rgb(42, 36, 42)
local HOOF  = rgb(104, 78, 58)
local PINK  = rgb(250, 136, 158)

-- rgb() takes integer channels: round every interpolated channel before
-- packing it, otherwise Pluto's integer conversion gives a black pixel.
local function mix(a, b, t)
  local ar, ag, ab = rgbcomponents(a)
  local br, bg, bb = rgbcomponents(b)
  local function ch(x, y) return math.floor(x + (y - x) * t + 0.5) end
  return rgb(ch(ar, br), ch(ag, bg), ch(ab, bb))
end

-- rgbcomponents() is an engine helper on some builds; a tiny fallback keeps
-- the generator portable if that helper is not exposed to scripts.
if type(rgbcomponents) ~= "function" then
  rgbcomponents = function(c)
    -- rgb() packs AARRGGBB in a signed 32-bit integer; Lua's % keeps the
    -- answer positive even for the negative values rgb() returns.
    local r = c % 256
    local g = math.floor(c / 256) % 256
    local b = math.floor(c / 65536) % 256
    return r, g, b
  end
end

local function sheet(w, h)
  return images.sheet(w, h)
end
local function rect(s, x, y, w, h, c)
  images.rect(s, x, y, w, h, c, true)
end
local function circ(s, x, y, r, c)
  images.circle(s, x, y, r, c, true)
end
local function line(s, x1, y1, x2, y2, c, pen)
  if pen then s.pen = pen end
  images.line(s, x1, y1, x2, y2, c)
  if pen then s.pen = 1 end
end
local function poly(s, pts, fill, border)
  images.poly(s, pts, fill, border)
end
local function save(s, name)
  images.save(s, P .. name)
end

-- ---- parallax layers --------------------------------------------------
-- Sky: a warm afternoon gradient with a goofy sun. Nearly static.
do
  local s = sheet(960, 540)
  local top, bottom = rgb(96, 174, 226), rgb(255, 220, 174)
  for y = 0, 538, 2 do
    rect(s, 0, y, 960, 2, mix(top, bottom, y / 538))
  end
  circ(s, 790, 106, 42, rgb(255, 240, 168))
  circ(s, 790, 106, 34, rgb(255, 248, 206))
  for i = 0, 11 do
    local a = i * math.pi / 6
    line(s, 790 + 50 * math.cos(a), 106 + 50 * math.sin(a),
            790 + 62 * math.cos(a), 106 + 62 * math.sin(a), rgb(255, 242, 176), 3)
  end
  save(s, "sky.png")
end

-- Clouds: three fluffy banks, repeated at the seam so tiling never pops.
do
  local s = sheet(960, 220)
  local function clouds(x)
    for _, p in ipairs({ { 0, 82, 30 }, { 32, 68, 34 }, { 70, 84, 28 },
                         { 104, 72, 24 }, { 148, 86, 26 }, { 178, 78, 22 } }) do
      circ(s, x + p[1], p[2], p[3], rgb(252, 252, 248))
      circ(s, x + p[1] - 6, p[2] + 8, p[3] * 0.8, rgb(232, 240, 244))
    end
  end
  clouds(-940); clouds(-20); clouds(420); clouds(860)
  save(s, "clouds.png")
end

-- Far hills with tiny houses: slowest moving "ground" layer.
do
  local s = sheet(960, 260)
  for x = -480, 1440, 480 do
    circ(s, x + 110, 282, 128, rgb(104, 164, 112))
    circ(s, x + 350, 290, 150, rgb(88, 150, 100))
    circ(s, x + 600, 286, 116, rgb(116, 176, 118))
    -- a few little houses on the ridge
    for _, h in ipairs({ { x + 168, 196 }, { x + 402, 202 }, { x + 682, 194 } }) do
      rect(s, h[1], h[2], 30, 26, rgb(246, 228, 196))
      poly(s, { h[1] - 4, h[2], h[1] + 15, h[2] - 18, h[1] + 34, h[2] }, rgb(200, 110, 96), nil)
      rect(s, h[1] + 11, h[2] + 12, 8, 14, rgb(120, 92, 76))
      rect(s, h[1] + 3, h[2] + 6, 6, 6, rgb(150, 200, 220))
    end
  end
  save(s, "hills.png")
end

-- Town facades: pastel shops and windows, glued to the floor line.
do
  local s = sheet(960, 200)
  local colors = { rgb(224, 176, 158), rgb(166, 196, 214), rgb(236, 208, 156),
                   rgb(188, 176, 214), rgb(172, 210, 186), rgb(228, 186, 196) }
  for x = -960, 1920, 160 do
    local i = ((x + 960) / 160) % #colors + 1
    local c = colors[i]
    rect(s, x + 4, 34, 152, 166, c)
    rect(s, x + 4, 34, 152, 6, rgb(120, 96, 92))
    for wy = 56, 150, 42 do
      for wx = x + 16, x + 130, 38 do
        rect(s, wx, wy, 22, 26, rgb(148, 200, 224))
        rect(s, wx + 10, wy, 2, 26, rgb(244, 244, 236))
        rect(s, wx, wy + 12, 22, 2, rgb(244, 244, 236))
      end
    end
    rect(s, x + 60, 158, 42, 42, rgb(112, 84, 72))
    circ(s, x + 94, 180, 2, rgb(246, 214, 110))
    -- awning stripes
    for k = 0, 6 do
      rect(s, x + 10 + k * 20, 34, 10, 12, k % 2 == 0 and rgb(236, 232, 220) or rgb(196, 96, 92))
    end
  end
  save(s, "town.png")
end

-- Shrubs and trees, nearest of the scrolling back layers.
do
  local s = sheet(960, 160)
  for x = -480, 1440, 240 do
    rect(s, x + 58, 92, 10, 68, rgb(124, 86, 58))
    circ(s, x + 63, 66, 46, rgb(80, 148, 82))
    circ(s, x + 40, 84, 32, rgb(96, 166, 94))
    circ(s, x + 90, 84, 34, rgb(68, 132, 76))
    circ(s, x + 180, 124, 34, rgb(92, 156, 86))
    circ(s, x + 212, 130, 26, rgb(112, 174, 98))
  end
  save(s, "trees.png")
end

-- ---- tile strip ------------------------------------------------------
-- Eight 16px tiles: hedge, grass, sidewalk, road, curb, brick, flowers,
-- crosswalk. Drawn at scale 2 by the game (32px cells).
do
  local s = sheet(128, 16)
  -- 1 hedge
  rect(s, 0, 0, 16, 16, rgb(76, 140, 76))
  circ(s, 5, 7, 6, rgb(104, 174, 96)); circ(s, 12, 9, 6, rgb(88, 158, 84))
  circ(s, 9, 3, 5, rgb(116, 186, 104))
  -- 2 grass
  rect(s, 16, 0, 16, 16, rgb(104, 182, 96))
  for i = 0, 4 do rect(s, 18 + i * 3, 2 + (i % 3) * 4, 2, 3, rgb(136, 206, 118)) end
  -- 3 sidewalk
  rect(s, 32, 0, 16, 16, rgb(208, 204, 192))
  line(s, 32, 15, 47, 15, rgb(176, 172, 162))
  line(s, 47, 0, 47, 15, rgb(188, 184, 174))
  -- 4 road
  rect(s, 48, 0, 16, 16, rgb(94, 98, 108))
  rect(s, 54, 6, 5, 3, rgb(218, 206, 140))
  -- 5 curb
  rect(s, 64, 0, 16, 5, rgb(232, 228, 216))
  rect(s, 64, 5, 16, 11, rgb(148, 146, 140))
  -- 6 brick
  rect(s, 80, 0, 16, 16, rgb(190, 120, 96))
  for y = 0, 12, 4 do line(s, 80, y, 95, y, rgb(150, 92, 72)) end
  line(s, 87, 0, 87, 4, rgb(150, 92, 72)); line(s, 83, 4, 83, 8, rgb(150, 92, 72))
  line(s, 91, 8, 91, 12, rgb(150, 92, 72)); line(s, 87, 12, 87, 16, rgb(150, 92, 72))
  -- 7 flowerbed
  rect(s, 96, 0, 16, 16, rgb(112, 166, 92))
  for _, f in ipairs({ { 100, 6, rgb(250, 136, 158) }, { 108, 10, rgb(250, 220, 110) },
                       { 102, 13, rgb(206, 160, 236) } }) do
    rect(s, f[1], f[2], 1, 4, rgb(66, 122, 66))
    circ(s, f[1], f[2], 2, f[3])
  end
  -- 8 crosswalk
  rect(s, 112, 0, 16, 16, rgb(94, 98, 108))
  rect(s, 113, 2, 5, 12, rgb(232, 232, 224)); rect(s, 121, 2, 5, 12, rgb(232, 232, 224))
  save(s, "tiles.png")
end

-- ---- the goat -------------------------------------------------------
-- Five frames: four running steps, one jump, one headbutt, one dizzy.
-- Each frame is its own PNG so the game can swap visible sprites without
-- re-uploading textures every step.
local function goat(x, leg0, leg1, leg2, leg3, headx, heady, mood, butt)
  local s = sheet(56, 48)
  -- shadow / far legs
  for i, dy in ipairs({ leg0, leg1, leg2, leg3 }) do
    local lx = 14 + (i - 1) * 7
    rect(s, lx, 36 + dy, 5, 10 - dy, i % 2 == 0 and SHADE or WHITE)
    rect(s, lx, 44 + dy, 6, 3, HOOF)
  end
  -- body and tail
  circ(s, 25, 27, 14, SHADE)
  circ(s, 25, 26, 13, WHITE)
  circ(s, 10, 22, 5, WHITE)
  circ(s, 9, 21, 3, SHADE)
  -- neck + collar + bell
  rect(s, 33, 18, 9, 14, WHITE)
  rect(s, 35, 28, 9, 3, rgb(196, 72, 82))
  circ(s, 39, 33, 4, rgb(246, 206, 96))
  circ(s, 39, 34, 2, rgb(220, 160, 60))
  -- head
  local hx, hy = 40 + headx, 16 + heady
  circ(s, hx, hy, 10, SHADE)
  circ(s, hx, hy - 1, 9, WHITE)
  -- ears
  poly(s, { hx - 8, hy - 3, hx - 18, hy - 9, hx - 11, hy + 2 }, SHADE, HORN)
  poly(s, { hx + 6, hy - 5, hx + 14, hy - 11, hx + 10, hy - 1 }, WHITE, HORN)
  -- horns
  poly(s, { hx - 6, hy - 8, hx - 8, hy - 17, hx - 1, hy - 10 }, HORN, rgb(158, 146, 122))
  poly(s, { hx + 3, hy - 9, hx + 5, hy - 18, hx + 9, hy - 8 }, HORN, rgb(158, 146, 122))
  -- snout
  circ(s, hx + 7, hy + 5, 6, SNOUT)
  circ(s, hx + 10, hy + 4, 2, rgb(206, 118, 124))
  -- face
  if mood == "dizzy" then
    line(s, hx - 2, hy - 5, hx + 3, hy, EYE, 2)
    line(s, hx + 3, hy - 5, hx - 2, hy, EYE, 2)
    line(s, hx + 7, hy - 3, hx + 12, hy + 2, EYE, 2)
    line(s, hx + 12, hy - 3, hx + 7, hy + 2, EYE, 2)
  elseif mood == "butt" then
    line(s, hx - 4, hy - 4, hx, hy - 1, EYE, 2)
    line(s, hx + 4, hy - 5, hx + 1, hy - 2, EYE, 2)
    rect(s, hx + 3, hy + 6, 7, 3, rgb(230, 108, 124)) -- tongue out
  else
    circ(s, hx - 2, hy - 3, 2, EYE)
    circ(s, hx + 6, hy - 4, 2, EYE)
    circ(s, hx - 2, hy - 4, 1, WHITE)
    circ(s, hx + 6, hy - 5, 1, WHITE)
    line(s, hx + 5, hy + 7, hx + 10, hy + 7, EYE)
  end
  if butt then
    -- cheeky speed marks in front of the headbutt
    line(s, hx + 17, hy - 6, hx + 24, hy - 8, rgb(255, 244, 176), 2)
    line(s, hx + 18, hy + 2, hx + 25, hy + 2, rgb(255, 244, 176), 2)
  end
  return s
end

save(goat(0,  0, -3,  1, -2,  0,  0, "happy", false), "goat_run0.png")
save(goat(0, -3,  1, -2,  0,  1, -1, "happy", false), "goat_run1.png")
save(goat(0,  1, -2,  0, -3,  0, -2, "happy", false), "goat_run2.png")
save(goat(0, -2,  0, -3,  1,  1,  0, "happy", false), "goat_run3.png")
save(goat(0, -4, -4, -3, -3, -1, -4, "happy", false), "goat_jump.png")
save(goat(0, -1, -1,  0,  0,  3,  1, "butt",  true),  "goat_butt.png")
save(goat(0,  0,  0,  0,  0,  0,  1, "dizzy", false), "goat_hurt.png")

-- ---- props ----------------------------------------------------------
do -- flower: the free snack
  local s = sheet(24, 28)
  rect(s, 11, 12, 2, 15, rgb(72, 138, 72))
  circ(s, 12, 8, 7, rgb(250, 136, 158))
  circ(s, 12, 8, 4, rgb(255, 214, 110))
  circ(s, 6, 10, 4, rgb(252, 168, 184)); circ(s, 18, 10, 4, rgb(252, 168, 184))
  save(s, "flower.png")
end
do -- pot: smash it
  local s = sheet(26, 30)
  rect(s, 5, 12, 16, 4, rgb(206, 112, 82))
  poly(s, { 7, 16, 19, 16, 17, 29, 9, 29 }, rgb(190, 92, 72), rgb(146, 66, 56))
  circ(s, 13, 8, 7, rgb(88, 158, 88))
  circ(s, 10, 6, 4, rgb(112, 184, 104))
  save(s, "pot.png")
end
do -- mailbox: the classic goat target
  local s = sheet(30, 46)
  rect(s, 13, 24, 4, 22, rgb(104, 96, 84))
  rect(s, 5, 8, 20, 20, rgb(88, 132, 184))
  circ(s, 15, 18, 10, rgb(88, 132, 184))
  rect(s, 8, 16, 14, 3, rgb(232, 232, 220))
  rect(s, 22, 12, 4, 12, rgb(196, 72, 72))
  circ(s, 24, 12, 3, rgb(196, 72, 72))
  save(s, "mailbox.png")
end
do -- trash bin
  local s = sheet(32, 42)
  poly(s, { 5, 12, 27, 12, 24, 40, 8, 40 }, rgb(92, 142, 126), rgb(62, 100, 90))
  rect(s, 3, 8, 26, 6, rgb(112, 166, 148))
  rect(s, 9, 16, 2, 22, rgb(128, 184, 164))
  rect(s, 18, 16, 2, 22, rgb(128, 184, 164))
  save(s, "bin.png")
end
do -- hydrant
  local s = sheet(28, 34)
  rect(s, 6, 12, 16, 20, rgb(206, 74, 68))
  circ(s, 14, 11, 9, rgb(226, 88, 78))
  circ(s, 14, 8, 5, rgb(240, 116, 100))
  rect(s, 4, 18, 20, 4, rgb(168, 52, 52))
  rect(s, 3, 29, 22, 4, rgb(168, 52, 52))
  save(s, "hydrant.png")
end
do -- bench
  local s = sheet(60, 32)
  rect(s, 4, 8, 52, 6, rgb(176, 116, 70))
  rect(s, 4, 16, 52, 6, rgb(196, 134, 82))
  rect(s, 8, 22, 5, 10, rgb(96, 96, 104))
  rect(s, 47, 22, 5, 10, rgb(96, 96, 104))
  save(s, "bench.png")
end
do -- garden gnome: worth double, and he hates it
  local s = sheet(26, 38)
  poly(s, { 13, 2, 22, 20, 4, 20 }, rgb(214, 76, 76), rgb(160, 52, 56))
  circ(s, 13, 22, 7, rgb(248, 220, 186))
  rect(s, 10, 24, 6, 8, rgb(236, 232, 220))
  circ(s, 13, 17, 5, rgb(250, 214, 178))
  circ(s, 11, 16, 1, EYE); circ(s, 15, 16, 1, EYE)
  rect(s, 8, 31, 10, 6, rgb(76, 132, 180))
  save(s, "gnome.png")
end
do -- parked car: the big, noisy prize
  local s = sheet(78, 42)
  rect(s, 4, 18, 70, 16, rgb(214, 96, 96))
  rect(s, 20, 8, 40, 14, rgb(232, 116, 110))
  rect(s, 24, 10, 15, 10, rgb(150, 206, 228))
  rect(s, 41, 10, 15, 10, rgb(150, 206, 228))
  rect(s, 2, 26, 74, 5, rgb(176, 70, 74))
  circ(s, 20, 34, 8, rgb(56, 56, 64)); circ(s, 20, 34, 3, rgb(186, 186, 194))
  circ(s, 58, 34, 8, rgb(56, 56, 64)); circ(s, 58, 34, 3, rgb(186, 186, 194))
  rect(s, 70, 22, 6, 4, rgb(250, 226, 140))
  save(s, "car.png")
end
do -- hay bale: the GOAT MODE pickup
  local s = sheet(36, 34)
  rect(s, 3, 6, 30, 26, rgb(226, 186, 96))
  rect(s, 3, 6, 30, 4, rgb(250, 222, 140))
  rect(s, 3, 28, 30, 4, rgb(196, 146, 70))
  line(s, 10, 6, 10, 32, rgb(176, 126, 62), 2)
  line(s, 25, 6, 25, 32, rgb(176, 126, 62), 2)
  rect(s, 15, 14, 6, 10, rgb(196, 146, 70))
  save(s, "hay.png")
end
do -- sign
  local s = sheet(30, 46)
  rect(s, 13, 24, 4, 22, rgb(112, 88, 66))
  rect(s, 2, 6, 26, 20, rgb(246, 224, 148), rgb(152, 112, 70))
  line(s, 7, 12, 23, 12, rgb(144, 96, 72), 2)
  line(s, 7, 18, 19, 18, rgb(144, 96, 72), 2)
  save(s, "sign.png")
end

-- ---- the angry dog --------------------------------------------------
local function dog(flying)
  local s = sheet(48, 32)
  local bounce = flying and -3 or 0
  circ(s, 16, 18 + bounce, 11, rgb(176, 126, 84))
  circ(s, 34, 15 + bounce, 8, rgb(196, 142, 96))
  poly(s, { 30, 9 + bounce, 28, 1 + bounce, 36, 8 + bounce }, rgb(150, 100, 70))
  poly(s, { 39, 9 + bounce, 44, 2 + bounce, 43, 13 + bounce }, rgb(150, 100, 70))
  circ(s, 37, 14 + bounce, 2, EYE)
  circ(s, 45, 19 + bounce, 3, EYE)
  rect(s, 40, 23 + bounce, 6, 2, rgb(236, 232, 220))
  circ(s, 6, 11 + bounce, 4, rgb(176, 126, 84))
  if flying then
    circ(s, 38, 4, 6, rgb(255, 244, 176)) -- dizzy halo
  else
    rect(s, 12, 26, 4, 6, rgb(150, 100, 70))
    rect(s, 22, 26, 4, 6, rgb(150, 100, 70))
  end
  return s
end
save(dog(false), "dog.png")
save(dog(true), "dog_fly.png")

print("Goat Rampage art generated in " .. P)
exit()
