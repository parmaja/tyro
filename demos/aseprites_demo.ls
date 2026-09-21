--======================================================================
-- aseprites_demo.ls  Load & animate Aseprite files (demos/aseprites)
--======================================================================
-- Demonstrates loading .aseprite animations with the Sprites system and
-- playing them at a chosen speed:
--
--   sprite.load("aseprites/<file>.aseprite")  -- loads EVERY frame
--   sprite:play([fps])                        -- restart & play (fps override)
--   sprite:stop() / sprite:pause()            -- freeze on current frame
--   sprite.frames                             -- number of frames
--   sprite.frame                              -- current frame (read/write)
--   sprite.playing                            -- animation running? (bool)
--   sprite.speed                              -- frames-per-second (0 = file)
--   sprite.looping                            -- loop when the last frame ends
--
-- The three files in demos/aseprites/ are 32x32 Aseprite animations:
--   16x32 Idle.aseprite  (20 frames)
--   16x32 Walk.aseprite  (20 frames)
--   16x32 Run.aseprite   (30 frames)
--
-- Controls:
--   Left / Right or A / D   walk
--   hold SHIFT (or R)       run while moving
--   ESC                     exit
--======================================================================

window.show(800, 400)
canvas.color = colors.black
canvas.clear()

-- three showcase sprites, one per animation, so all are visible at once
idleS = Sprites.new("idle")
idleS.load("aseprites/16x32 Idle.aseprite")
idleS.x = 110
idleS.y = 150
idleS:play(7)

walkS = Sprites.new("walk")
walkS.load("aseprites/16x32 Walk.aseprite")
walkS.x = 400
walkS.y = 150
walkS:play(10)

runS = Sprites.new("run")
runS.load("aseprites/16x32 Run.aseprite")
runS.x = 690
runS.y = 150
runS:play(14)

-- the controllable hero
hero = Sprites.new("hero")
hero.load("aseprites/16x32 Idle.aseprite")
hero.x = 384
hero.y = 336
hero:play(7)

-- scrolling ground marks, drawn each frame
local scroll = 0
local mode = "idle"          -- current hero animation: idle / walk / run
local lastFrame = hero.frame

println("Aseprite demo: idle=" .. idleS.frames .. "f walk=" .. walkS.frames .. "f run=" .. runS.frames .. "f")

while cycle do
  -- input ---------------------------------------------------------------
  local moving = iskeydown("left") or iskeydown("right")
              or iskeydown("a") or iskeydown("d")

  local newMode
  if not moving then
    newMode = "idle"
  elseif iskeydown("shift") or iskeydown("r") then
    newMode = "run"
  else
    newMode = "walk"
  end

  -- switch animation only when the state really changes
  if newMode ~= mode then
    mode = newMode
    if mode == "idle" then
      hero.load("aseprites/16x32 Idle.aseprite")
      hero:play(7)
    elseif mode == "walk" then
      hero.load("aseprites/16x32 Walk.aseprite")
      hero:play(10)
    else
      hero.load("aseprites/16x32 Run.aseprite")
      hero:play(14)
    end
  end

  if moving then
    scroll = scroll + (mode == "run" and 8 or 4)
  end

  local frame = hero.frame
  if frame ~= lastFrame then
    lastFrame = frame
  end

  -- draw the scene -------------------------------------------------------
  canvas.color = colors.black
  canvas.clear()

  -- sky bands (fake gradient)
  canvas.color = colors.darkblue
  canvas.rectangle(0, 0, canvas.width, 190, true)
  canvas.color = colors.blue
  canvas.rectangle(0, 190, canvas.width, 130, true)

  -- stars
  canvas.color = colors.white
  canvas.point(60, 40)
  canvas.point(140, 70)
  canvas.point(210, 30)
  canvas.point(330, 55)
  canvas.point(460, 35)
  canvas.point(560, 75)
  canvas.point(650, 45)
  canvas.point(740, 60)

  -- ground
  canvas.color = colors.green
  canvas.rectangle(0, 320, canvas.width, 60, true)
  canvas.color = colors.darkgreen
  canvas.rectangle(0, 320, canvas.width, 6, true)
  canvas.color = colors.darkbrown
  canvas.rectangle(0, 326, canvas.width, 54, true)

  -- scrolling dashes on the ground show walking / running speed
  canvas.color = colors.brown
  local spacing = 48
  local off = scroll % spacing
  local x = -off
  while x < canvas.width do
    canvas.rectangle(x + 8, 350, 26, 4, true)
    x = x + spacing
  end

  -- HUD text ------------------------------------------------------------
  canvas.color = colors.white
  canvas.text(170, 8, "Aseprite demo - loading animations from demos/aseprites/")
  canvas.text(170, 24, "Left/Right or A/D: walk   hold SHIFT (or R): run   ESC: exit")

  canvas.color = colors.yellow
  canvas.text(110 - 60, 176, "idle " .. idleS.frames .. "f")
  canvas.text(400 - 60, 176, "walk " .. walkS.frames .. "f")
  canvas.text(690 - 60, 176, "run " .. runS.frames .. "f")

  canvas.color = colors.white
  canvas.text(10, 374, "hero: " .. mode)
  canvas.text(170, 374, "frames: " .. hero.frames .. "   frame: " .. hero.frame)
  canvas.text(360, 374, "playing: " .. tostring(hero.playing))
  canvas.text(510, 374, "speed: " .. hero.speed .. " fps")

  if iskeypressed("escape") then
    break
  end
end