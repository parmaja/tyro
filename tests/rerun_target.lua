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