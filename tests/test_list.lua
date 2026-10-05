--listbox API: the controls table takes the handle, and the same functions are
--also registered on the control table itself (lst.additem("a"))
local f = openfile("test_list.txt", "w")

local l = controls.new("listbox", "", 0, 0, 200, 100, "lst")
f:write("created=" .. tostring(l ~= nil) .. "\n")

controls.additem(lst, "a")
controls.additem(lst, "b")
lst.additem("c")
f:write("count=" .. tostring(controls.items(lst)) .. "\n")
f:write("item0=" .. tostring(controls.item(lst, 0)) .. "\n")

controls.item(lst, 1, "z")
f:write("item1=" .. tostring(controls.item(lst, 1)) .. "\n")
f:write("outside=" .. tostring(controls.item(lst, 9)) .. "\n")

controls.items(lst, "p", "q")
f:write("replaced=" .. tostring(controls.items(lst)) .. "\n")

f:write("index=" .. tostring(controls.itemindex(lst)) .. "\n")
controls.itemindex(lst, 1)
f:write("indexset=" .. tostring(controls.itemindex(lst)) .. "\n")

f:write("viewcount=" .. tostring(controls.viewcount(lst)) .. "\n")
controls.viewcount(lst, 1)
f:write("viewcountset=" .. tostring(controls.viewcount(lst)) .. "\n")

--a plain handle works as well, and anything that is not a listbox answers nil
f:write("bynumber=" .. tostring(controls.items(1)) .. "\n")
local btn = controls.new("button", "hi", 0, 0, 100, 30, "btn")
f:write("notalist=" .. tostring(controls.items(btn)) .. "\n")
f:write("badhandle=" .. tostring(controls.items(999)) .. "\n")

controls.clear(lst)
f:write("cleared=" .. tostring(controls.items(lst)) .. "\n")

f:close()
exit()