local f = openfile("test_verify3.txt", "w")
f:write("start\n")
local b = controls.new("button", "Hi", 10, 10, 120, 40)
controls.name(b, "btn1")
f:write("created\n")
btn1.text = "Hello"
f:write(tostring(controls.text(btn1)) .. "\n")
f:close()