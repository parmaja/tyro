--reading __handle must stay a raw read: the 'controls' table has no __handle of
--its own, so a normal field read re-enters the getter on its __index and
--recurses until Lua reports "C stack overflow". Every name the table does not
--hold has to answer nil instead, and the getters must keep answering values.
local f = openfile("test_luatable.txt", "w")

local l = controls.new("listbox", "", 0, 0, 200, 100, "lst")
controls.additem(lst, "a")

f:write("unknown-control-table=" .. tostring(lst.nosuchfield) .. "\n")
f:write("unknown-controls-table=" .. tostring(controls.nosuchfield) .. "\n")
f:write("unknown-plain-table=" .. tostring({}.nosuchfield) .. "\n")
f:write("count=" .. tostring(controls.count) .. "\n")
f:write("width=" .. tostring(controls.width(lst)) .. "\n")
f:write("items=" .. tostring(controls.items(lst)) .. "\n")
f:write("bytable=" .. tostring(controls.items(l)) .. "\n")
f:write("bynumber=" .. tostring(controls.items(1)) .. "\n")

--f:write must only read a real number as a byte count. A numeric string used to
--be eaten whole, so the two writes below would join into a single line
f:write("numstring=")
f:write(tostring(7) .. "\n")
f:write("bytcount=")
f:write(2, "abcdef\n")

f:close()
exit()