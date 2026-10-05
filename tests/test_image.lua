local f = openfile("test_image.txt", "w")
f:write("start\n")

--controls.new('image', file, ...) : the caption slot names the texture
local img = controls.new("image", "test_image_tile.png", 10, 10, 128, 128, "pic")
f:write("created=" .. tostring(img ~= nil) .. "\n")
f:write("loaded=" .. tostring(pic.loaded) .. "\n")

--img.load(file) replaces the texture and answers with the new state
f:write("dotret=" .. tostring(pic.load("test_image_tile.png")) .. "\n")
f:write("colonret=" .. tostring(pic:load("test_image_tile.png")) .. "\n")
f:write("loaded=" .. tostring(pic.loaded) .. "\n")

--property form
pic.file = "test_image_tile.png"
f:write("loaded=" .. tostring(pic.loaded) .. "\n")

--handle form through the controls table
f:write("ctlret=" .. tostring(controls.load(pic, "test_image_tile.png")) .. "\n")
f:write("loaded=" .. tostring(pic.loaded) .. "\n")
f:write("size=" .. tostring(controls.width(pic)) .. "x" .. tostring(controls.height(pic)) .. "\n")

--a missing (or empty) file name releases the texture
pic.load("no_such_file.png")
f:write("missing=" .. tostring(pic.loaded) .. "\n")
pic.load("")
f:write("empty=" .. tostring(pic.loaded) .. "\n")

--controls that are not images never load
local lbl = controls.new("label", "text", 0, 0, 100, 20, "lbl")
f:write("notimage=" .. tostring(controls.load(lbl, "test_image_tile.png")) .. "\n")
f:write("loaded=" .. tostring(lbl.loaded) .. "\n")
f:write("badhandle=" .. tostring(controls.load(0, "test_image_tile.png")) .. "\n")

f:close()
exit()