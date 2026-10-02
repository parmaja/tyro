local f = io.open("D:\\test_edit.txt", "w")
f:write("start\n")
local e = controls.new("edit", "txt", 0,0,100,20, "e1")
f:write("ok1\n")
f:write(tostring(e1.placeholder) .. "\n")
e1.placeholder = "hint"
f:write(tostring(e1.placeholder) .. "\n")
f:write(tostring(controls.text(e1)) .. "\n")
f:close()