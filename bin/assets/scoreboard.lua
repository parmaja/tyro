-- scoreboard.lua - a sorted high-score table kept next to the game.
-- Usage:
--   local board = require("assets/scoreboard")
--   board.add("hiscores.txt", "Ana", 4200, 5)
--   for _, e in ipairs(board.top("hiscores.txt", 5)) do
--     print(e.rank .. ". " .. e.name .. " - " .. e.score)
--   end
-- No pcall: openfile reports errors the engine way.

local M = {}

local function by_score(a, b)
  return a.score > b.score
end

function M.top(file, limit)
  limit = limit or 10
  local entries = {}
  local f = openfile(file, "r")
  if f then
    for line in f:lines() do
      local tab = line:find("\t", 1, true)
      if tab then
        local name = line:sub(1, tab - 1)
        local value = tonumber(line:sub(tab + 1))
        if name ~= "" and value then
          entries[#entries + 1] = { name = name, score = math.floor(value) }
        end
      end
    end
    f:close()
  end
  table.sort(entries, by_score)
  while #entries > limit do
    table.remove(entries)
  end
  for i = 1, #entries do
    entries[i].rank = i
  end
  return entries
end

function M.add(file, name, points, limit)
  limit = limit or 10
  local entries = M.top(file, limit)
  entries[#entries + 1] = { name = tostring(name), score = math.floor(points or 0) }
  table.sort(entries, by_score)
  while #entries > limit do
    table.remove(entries)
  end
  local f = openfile(file, "w")
  if f then
    for i = 1, #entries do
      f:write(entries[i].name .. "\t" .. entries[i].score .. "\n")
    end
    f:close()
  end
  for i = 1, #entries do
    entries[i].rank = i
  end
  return entries
end

function M.best(file)
  local entries = M.top(file, 1)
  return entries[1] and entries[1].score or 0
end

return M
