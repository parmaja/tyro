--non-ASCII text has to survive both spellings, so utf8string must not be
--re-encoded on the way out
local f = openfile("test_utf8.txt", "w")

local b = controls.new("button", "héllo wörld", 0, 0, 120, 32, "btn")
local e = controls.new("edit", "", 0, 0, 140, 28, "edt")

f:write("caption-fn=" .. tostring(controls.caption(b)) .. "\n")
f:write("text-fn=" .. tostring(controls.text(b)) .. "\n")
f:write("text-field=" .. tostring(b.text) .. "\n")

controls.text(b, "über")
f:write("caption-after-set=" .. tostring(controls.caption(b)) .. "\n")

controls.text(e, "café")
f:write("edit-text=" .. tostring(controls.text(e)) .. "\n")
e.text = "naïve"
f:write("edit-field=" .. tostring(e.text) .. "\n")
f:write("edit-byfn=" .. tostring(controls.text(e)) .. "\n")

f:close()
exit()