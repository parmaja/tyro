-- soundgen.lua - write small PCM WAV effects from Lua, no Python needed.
-- Usage:
--   local sg = require("assets/soundgen")
--   sg.wav("sfx/bounce.wav", 440, 90, "sine", 0.5)
-- Wave kinds: sine, square, saw, noise, sweep. Files are 16-bit mono 22050 Hz.

local M = {}
local RATE = 22050

local function u16(v)
  v = math.floor(v) % 65536
  return string.char(v % 256, math.floor(v / 256) % 256)
end

local function u32(v)
  local a = v % 256
  local b = math.floor(v / 256) % 256
  local c = math.floor(v / 65536) % 256
  local d = math.floor(v / 16777216) % 256
  return string.char(a, b, c, d)
end

local function s16(v)
  if v < -32768 then v = -32768 end
  if v > 32767 then v = 32767 end
  if v < 0 then v = v + 65536 end
  return u16(v)
end

function M.wav(name, freq, ms, kind, volume)
  local n = math.floor(RATE * ms / 1000)
  if n < 1 then return false end
  local f = openfile(name, "wb") -- binary: no CRLF translation
  if not f then return false end
  local data = n * 2
  f:write("RIFF" .. u32(36 + data) .. "WAVE" ..
    "fmt " .. u32(16) .. u16(1) .. u16(1) .. u32(RATE) ..
    u32(RATE * 2) .. u16(2) .. u16(16) .. "data" .. u32(data))
  for i = 0, n - 1 do
    local t = i / RATE
    local s
    if kind == "square" then
      s = math.sin(2 * math.pi * freq * t) >= 0 and 1 or -1
    elseif kind == "saw" then
      s = ((freq * t) % 1) * 2 - 1
    elseif kind == "noise" then
      s = math.random() * 2 - 1
    elseif kind == "sweep" then
      s = math.sin(2 * math.pi * freq * (1 + t * 3) * t)
    else
      s = math.sin(2 * math.pi * freq * t)
    end
    f:write(s16(s * (volume or 0.5) * 32767))
  end
  f:close()
  return true
end

return M
