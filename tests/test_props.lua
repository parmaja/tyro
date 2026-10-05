--control handles are property bags: every field the getter knows can also be
--written, and controls.<name>(handle, ...) stays the function spelling
local f = openfile("test_props.txt", "w")

--geometry
local b = controls.new("button", "Hi", 10, 20, 120, 32, "btn")
f:write("x,y=" .. b.x .. "," .. b.y .. "\n")
f:write("w,h=" .. b.width .. "," .. b.height .. "\n")
b.width = 200
b.height = 40
b.x = 5
b.y = 6
f:write("set w,h=" .. b.width .. "," .. b.height .. "\n")
f:write("set x,y=" .. b.x .. "," .. b.y .. "\n")
f:write("by table form=" .. controls.width(b) .. "," .. controls.height(b) .. "\n")
f:write("by handle form=" .. controls.width(1) .. "," .. controls.height(1) .. "\n")

--a field is a value, never the method that used to shadow it
f:write("is function=" .. tostring(type(b.width) == "function") .. "\n")
f:write("text=" .. b.text .. " caption=" .. b.caption .. "\n")
b.caption = "Bye"
f:write("caption set=" .. b.text .. "/" .. controls.caption(b) .. "\n")

--state fields read back what the setter queued
b.name = "renamed"
f:write("name=" .. b.name .. "\n")
b.visible = false
f:write("visible=" .. tostring(b.visible) .. "\n")
b.border = 2
f:write("border=" .. b.border .. " by table=" .. controls.border(b) .. "\n")
b.align = "left"
f:write("align=" .. b.align .. "\n")
b.backcolor = colors.red
f:write("backcolor=" .. b.backcolor .. " by table=" .. controls.backcolor(b) .. "\n")

--an unknown name still lands on the control table itself
b.mine = 7
f:write("raw field=" .. tostring(b.mine) .. "\n")

--checkbox: checked is a property
local chk = controls.new("checkbox", "On", 10, 100, 90, 24, "chk")
f:write("checked=" .. tostring(chk.checked) .. "\n")
chk.checked = true
f:write("checked set=" .. tostring(chk.checked) .. "\n")
f:write("checked on button=" .. tostring(b.checked) .. "\n")

--listbox: items/itemindex/viewcount are properties, the actions stay methods
local l = controls.new("listbox", "", 10, 140, 200, 100, "lst")
l.items = {"a", "b", "c"}
f:write("items=" .. l.items .. " item1=" .. tostring(l.item(1)) .. "\n")
f:write("itemindex=" .. l.itemindex .. "\n")
l.itemindex = 1
f:write("itemindex set=" .. l.itemindex .. " by table=" .. controls.itemindex(l) .. "\n")
l.viewcount = 2
f:write("viewcount=" .. l.viewcount .. " by table=" .. controls.viewcount(l) .. "\n")
l:additem("d")
l.additem("e")
f:write("added=" .. l.items .. " last=" .. tostring(l.item(4)) .. "\n")
l.item(0, "z")
f:write("replaced=" .. tostring(l.item(0)) .. "\n")
l.items = "only"
f:write("single=" .. l.items .. "/" .. tostring(l.item(0)) .. "\n")
l:clear()
f:write("cleared=" .. l.items .. "\n")
f:write("items on button=" .. tostring(b.items) .. "\n")

--image: loaded reads, file writes
local img = controls.new("image", "test_image_tile.png", 300, 10, 64, 64, "img")
f:write("loaded=" .. tostring(img.loaded) .. "\n")
img.file = "test_image_tile.png"
f:write("loaded after file=" .. tostring(img.loaded) .. " w,h=" .. img.width .. "," .. img.height .. "\n")
f:write("loaded on button=" .. tostring(b.loaded) .. "\n")

--parent: a handle that can be passed straight back
local pan = controls.new("panel", "", 300, 100, 200, 200, "pan")
b.parent = pan
f:write("parent=" .. b.parent .. "\n")
f:write("parent by table=" .. controls.parent(b) .. "\n")
b.parent = 0
f:write("back to main=" .. b.parent .. "\n")

--the actions that have no property spelling, in both spellings
b:show()
f:write("show=" .. tostring(b.visible) .. "\n")
b.hide()
f:write("hide=" .. tostring(b.visible) .. "\n")
b:move(11, 12)
f:write("move=" .. b.x .. "," .. b.y .. "\n")
b.position(13, 14)
f:write("position=" .. b.x .. "," .. b.y .. "\n")
b:show()

--hover/down/clicked/focused are fields, and read false with no mouse on them
f:write("flags=" .. tostring(b.hover) .. tostring(b.down) ..
        tostring(b.clicked) .. tostring(b.focused) .. "\n")
f:write("focus=" .. tostring(controls.focused(b)) .. "\n")

f:close()
exit()