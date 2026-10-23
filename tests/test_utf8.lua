--re-encoded on the way out
local f = openfile("test_utf8.txt", "w")

local b = controls.new("button", "hllo wrld", 0, 0, 120, 32)
controls.name(b, "btn")
local e = controls.new("edit", "", 0, 0, 140, 28)
controls.name(e, "edt")

f:write("caption-fn=" .. tostring(controls.caption(b)) .. "\n")
f:write("text-fn=" .. tostring(controls.text(b)) .. "\n")
f:write("text-field=" .. tostring(b.text) .. "\n")

controls.text(b, "??ber")
f:write("caption-after-set=" .. tostring(controls.caption(b)) .. "\n")

controls.text(e, "caf?Ñ")
f:write("edit-text=" .. tostring(controls.text(e)) .. "\n")
e.text = "na?»ve"
f:write("edit-field=" .. tostring(e.text) .. "\n")
f:write("edit-byfn=" .. tostring(controls.text(e)) .. "\n")

f:close()
exit()
