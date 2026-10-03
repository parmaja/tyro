-- Script under test for tests/test_rerun_editor.lpr: every run appends its tag to
-- rerun_target.log, so the test can see how often the engine (re)started this
-- script. openfile() is the sandbox file API and resolves next to this script.
--
-- The test replaces the tag below to build the "edited" buffer it types into the
-- in-game editor (F2) and then hides again, so keep the tag on its own line.

local tag = 'v1'

local f = openfile('rerun_target.log', 'a')
if f then
  f:write(tag .. '\n')
  f:close()
end

-- Leave a control in the window, focused, so the test can check that a rerun
-- takes both the control and the focus it holds away again. The name carries the
-- tag: the test edits the tag above, so a rerun has to produce a new name.
local ctrl = controls.new('button', tag, 10, 10, 120, 30, 'ctrl' .. tag)
controls.focus(ctrl)

-- Leave a sprite in the store too. A texture-less sprite needs no graphics
-- context, and the name carries the tag so the test can tell the runs apart.
Sprites.new('sp' .. tag)