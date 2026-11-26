-- rtl.lua - first step toward right-to-left text in Tyro.
-- font.load(name, size, "arabic") bakes the shaped glyph forms; this helper
-- reverses the order of the characters so a line reads right-to-left.
-- Full bidi paragraph layout is not implemented yet.

local M = {}

function M.reverse(s)
  local out = {}
  local i = 1
  while i <= #s do
    local c = s:byte(i)
    local n = 1
    if c >= 0xF0 then n = 4
    elseif c >= 0xE0 then n = 3
    elseif c >= 0xC0 then n = 2 end
    table.insert(out, 1, s:sub(i, i + n - 1))
    i = i + n
  end
  return table.concat(out)
end

function M.text(y, s, x)
  local line = M.reverse(s or "")
  if x then
    screen.text(x, y, line)
  else
    screen.text_center(y, line)
  end
end

return M
