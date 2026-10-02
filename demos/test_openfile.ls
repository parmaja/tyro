-- openfile() smoke test: everything it checks is written to openfile_out.txt
-- next to this script (assert() returns nothing in Tyro, so it is never used
-- to capture a value here).

local out, outErr = openfile("openfile_out.txt", "w")
if not out then
  log("openfile_out.txt: " .. tostring(outErr))
  running = false
  return
end
local function say(s) out:write(tostring(s) .. "\n") end
local function open(name, mode) -- open, report the failure, hand back the handle
  local h, err = openfile(name, mode)
  if not h then
    say(string.format("  FAILED to open %s (%s): %s", name, mode, tostring(err)))
  end
  return h
end

say("io = " .. tostring(io))
say("os = " .. tostring(os))
say("openfile = " .. type(openfile))

-- write, then read the file back
local f = open("openfile_test.txt", "w")
f:write("line one\n")
f:write("line two\n")
f:write("no newline at the end")
f:close()

f = open("openfile_test.txt", "r")
say("read l  = " .. string.format("%q", tostring(f:read("l"))))
say("read L  = " .. string.format("%q", tostring(f:read("L"))))
say("read a  = " .. string.format("%q", tostring(f:read("a"))))
say("at eof  = " .. tostring(f:read("l")))
say("closed  = " .. tostring(select('#', f:read("l"))) .. " value(s)")
f:close()

f = open("openfile_test.txt", "r")
for line in f:lines() do
  say("line: " .. string.format("%q", tostring(line)))
end
f:close()

-- numbers
local nums = open("openfile_nums.txt", "w")
nums:write("10 20.5 -3\n")
nums:close()
nums = open("openfile_nums.txt", "r")
say("num1 = " .. tostring(nums:read("n")))
say("num2 = " .. tostring(nums:read("n")))
say("num3 = " .. tostring(nums:read("n")))
say("num4 = " .. tostring(nums:read("n")))
nums:close()

-- append keeps the earlier content
local a = open("openfile_test.txt", "a")
a:write("\nappended\n")
a:close()
f = open("openfile_test.txt", "r")
say("after append = " .. string.format("%q", tostring(f:read("a"))))
f:close()

-- binary mode keeps #0 bytes
local png = open("richard-say.png", "rb")
if png then
  local head = png:read(8)
  local rest = png:read("a")
  say("png: first 8 = " .. tostring(#head) .. " bytes, rest = " .. tostring(#rest) .. " bytes")
  png:close()
end

-- write returns the handle so calls chain
open("openfile_chain.txt", "w"):write("chained\n"):close()
local c = open("openfile_chain.txt", "r")
say("chain = " .. tostring(c:read("l")))
c:close()

-- "x" creates, but never overwrites what is already there
local x1 = open("openfile_new.txt", "x")
x1:write("created once\n")
x1:close()
local x2, xerr = openfile("openfile_new.txt", "x")
say("openfile_new.txt a second time -> " .. tostring(x2) .. " / " .. tostring(xerr))
local x3 = open("openfile_new.txt", "r")
say("openfile_new.txt = " .. tostring(x3:read("l")))
x3:close()

-- the dot spelling works as well as the colon one
local d = open("openfile_dot.txt", "w")
d.write(d, "dotted\n")
d.close()
local d2 = open("openfile_dot.txt", "r")
say("dot spelling = " .. tostring(d2:read("l")))
d2:close()

-- a handle that was closed says so instead of falling over
local shut = open("openfile_chain.txt", "r")
shut:close()
say("after close -> " .. tostring(shut:read("l")))
say("closed  -> " .. tostring(shut:close()))

-- names that leave the workspace are refused
local bad = {
  "../outside.txt",
  "..\\outside.txt",
  "demos/../../outside.txt",
  "C:\\Windows\\win.ini",
  "\\\\?\\C:\\Windows\\win.ini",
  "",
}
for i = 1, #bad do
  local h, err = openfile(bad[i], "w")
  say(string.format("refused %-32s -> %s / %s", string.format("%q", bad[i]), tostring(h), tostring(err)))
end

-- a name inside the workspace but with .. still works when it stays inside
local inside = openfile("sub/../openfile_inside.txt", "w")
if inside then
  inside:write("stayed inside\n")
  inside:close()
  say("openfile_inside.txt written through a '..' that stays in the workspace")
end

-- errors are returned, never raised
local h, err = openfile("does_not_exist_here.txt", "r")
say("missing file -> " .. tostring(h) .. " / " .. tostring(err))
say("no arguments -> " .. tostring(select(2, openfile())))

out:close()
running = false
