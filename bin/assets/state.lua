-- state.lua - tiny snapshots and undo for plain Lua tables.
-- Usage:
--   local state = require("assets/state")
--   state.remember(player)             -- push a snapshot
--   player.x, player.y = 10, 20
--   state.undo(player)                 -- put x/y back
--   local copy = state.snapshot(hud)   -- hand a copy to a tween
--   state.restore(hud, copy)
-- Only numbers, strings and booleans are copied (nested tables and sprites
-- are left alone), which is exactly what a score/position/lives snapshot needs.

local M = {}
local stacks = {} -- table -> stack of snapshots

local function snapshot(t)
  local copy = {}
  for k, v in pairs(t) do
    local kind = type(v)
    if kind == "number" or kind == "string" or kind == "boolean" then
      copy[k] = v
    end
  end
  return copy
end

function M.snapshot(t)
  if type(t) ~= "table" then return {} end
  return snapshot(t)
end

function M.restore(t, copy)
  if type(t) ~= "table" or type(copy) ~= "table" then return false end
  for k, v in pairs(copy) do
    t[k] = v
  end
  return true
end

function M.remember(t)
  if type(t) ~= "table" then return false end
  local stack = stacks[t]
  if not stack then
    stack = {}
    stacks[t] = stack
  end
  stack[#stack + 1] = snapshot(t)
  return true
end

function M.undo(t)
  local stack = stacks[t]
  if not stack or #stack == 0 then return false end
  local copy = table.remove(stack)
  return M.restore(t, copy)
end

function M.count(t)
  local stack = stacks[t]
  return stack and #stack or 0
end

function M.clear(t)
  stacks[t] = nil
end

return M
