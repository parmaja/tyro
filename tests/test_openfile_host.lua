-- Why `openfile` is nil when the script is run with plain lua.exe.
--
-- `openfile` is not a stock Lua function. It is a global that the Tyro host
-- injects into the script state, the same way it injects controls, canvas,
-- sleep, println and friends. Stock Lua has no such name, so the two
-- interpreters see opposite halves of the API:
--
--   plain lua.exe (c:\Programs\lua\bin\lua.exe)  ->  io, os present
--                                                      openfile, controls nil
--   bin\tyro.exe                                  ->  openfile, controls present
--
-- So "attempt to call a nil value (global 'openfile')" is not a Tyro bug: the
-- script was handed to the wrong host. These scripts have to be launched with
-- bin\tyro.exe, not with lua.exe.
--
-- Note: TLua.Init only strips io/os/dofile/loadfile when SafeMode is set, and
-- TLuaScript.Create calls Lua.Init with the SafeMode parameter left at its
-- default of False, so io is in fact still reachable from a Tyro script. The
-- README claims otherwise; this run prints what is really there.
--
-- Run it either way; it only reports what it sees, and it writes the same
-- report to test_openfile_host_report.txt when it is allowed a file:
--   bin\tyro.exe --execute tests\test_openfile_host.lua
--   lua tests\test_openfile_host.lua

local host = (type(openfile) == "function") and "tyro" or "lua"

-- Report to the screen, and to a file when this host has a way to write one.
local log
if host == "tyro" then
  log = openfile("test_openfile_host_report.txt", "w")
end

local function say(fmt, ...)
  local line
  if select("#", ...) > 0 then
    line = string.format(fmt, ...)
  else
    line = fmt
  end
  print(line)
  if log then
    log:write(line .. "\n")
  end
end

local function kind(label, value)
  say("%-12s %s", label, tostring(value))
end

say("--- who am I running under ---")
kind("_VERSION", _VERSION)
kind("arg[-1]", arg and arg[-1])
kind("arg[0]", arg and arg[0])
kind("arg[1]", arg and arg[1])
-- Tyro registers `version` as a global; stock Lua has no such name.
kind("version", version)
kind("host", host)

say("")
say("--- the file API on this host ---")
kind("openfile", type(openfile))
kind("io", type(io))
kind("os", type(os))
kind("dofile", type(dofile))
kind("loadfile", type(loadfile))
kind("require", type(require))
kind("controls", type(controls))

if host ~= "tyro" then
  say("")
  say("openfile is nil here, so this is NOT the Tyro host.")
  say("io/os/dofile/loadfile are all still present, which is the tell.")
  say("Re-run it with:")
  say("  bin\\tyro.exe --execute tests\\test_openfile_host.lua")
  say("")
  say("-- for contrast, what plain Lua offers instead --")
  local h, err = io.open("test_openfile_host.txt", "w")
  kind("io.open", tostring(h) and "handle" or tostring(err))
  if h then
    h:write("written by stock Lua, not by Tyro\n")
    h:close()
    say("io.open wrote the file just fine, which is exactly why this")
    say("script cannot be checked with lua.exe.")
  end
  return
end

say("")
say("openfile is here, so this IS the Tyro host.")
say("controls is a table and version is set, both injected by Tyro.")

local f, err = openfile("test_openfile_host.txt", "w")
if not f then
  say("openfile refused the write: %s", tostring(err))
else
  f:write("written through openfile() inside the Tyro sandbox\n")
  f:close()
  say("openfile wrote test_openfile_host.txt next to the script")
end

local back = openfile("test_openfile_host.txt", "r")
if back then
  say("read back: %s", tostring(back:read("l")))
  back:close()
end

-- The guard the README documents, shown from the inside.
say("")
say("--- workspace guard ---")
for _, name in ipairs({ "../outside.txt", "C:\\Windows\\win.ini" }) do
  local h, e = openfile(name, "w")
  say("  %-22s -> %s / %s", name, tostring(h), tostring(e))
end

say("")
say("done")
if log then
  log:close()
  say("report written to test_openfile_host_report.txt")
end
running = false
