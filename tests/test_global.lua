local f = io.open("D:\\test_global.txt", "w")
f:write("start\n")
local b = controls.new("button", "Hi", 10,10,100,30, "btn2")
f:write("ok\n")
btn2.text = "World"
f:write(tostring(btn2.text) .. "\n")
f:close()