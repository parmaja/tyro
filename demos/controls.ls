--======================================================================
--  controls.ls - Generic control table (buttons, labels, checkboxes, edits)
--======================================================================
--  Create controls from Lua by class name; they are owned by the main
--  window and self-drawn every frame by the main drawing cycle.
--
--  API:
--    controls.new(class, captionOrText, x?, y?, w?, h?, name?) -> handle
--      class: 'button' | 'panel' | 'label' | 'checkbox' | 'edit' | 'spectrum'
--    controls.text(handle [, s])   get/set text (caption or edited text)
--    controls.caption(handle [,s]) alias of text (legacy buttons.caption)
--    controls.checked(handle [,v]) get/set checked state (checkbox)
--    controls.position(handle [,x, y]) get/move position
--    controls.move(handle, x, y)   move (keeps size)
--    controls.width/h(andle [,v])  get/set size
--    controls.visible / controls.show(handle) / controls.hide(handle)
--    controls.hover/down/clicked(handle)  mouse state (button/checkbox)
--    controls.focused(handle) / controls.focus(handle)  keyboard focus
--    controls.border(handle [,style])  0=none 1=thin 2=thick 3=sizable
--    controls.backcolor(handle [,color])  color from the colors table
--    controls.name(handle [,name])  named controls resolve as Lua globals
--    controls.count                  number of created controls
--
--  Legacy alias: buttons.new("caption", x, y, w, h, borderSize) still works;
--  the 'buttons' table is the same object as 'controls'.

window.show(640, 480)

-- buttons
bOpen   = controls.new("button", "Open", 40, 40, 100, 36)
bSave   = controls.new("button", "Save", 160, 40, 100, 36)
bClose  = controls.new("button", "Close", 280, 40, 100, 36)
bCounter = controls.new("button", "Click me!", 120, 130, 160, 40)

-- named control: the global 'okbtn' resolves to its handle from now on
bOK     = controls.new("button", "OK", 400, 40, 100, 36, "okbtn")

-- label
lbl     = controls.new("label", "Label demo", 40, 210, 150, 24)
controls.backcolor(lbl, colors.blue)      -- 'blue' from the colors table

-- checkbox (toggles itself when clicked)
chk     = controls.new("checkbox", "Enable sound", 40, 250, 170, 26)

-- edit box: click it to focus, then type (caret, selection, arrows,
-- backspace/delete, home/end all work)
ed      = controls.new("edit", "type here", 40, 290, 240, 28)
controls.focus(ed)

-- panel: a simple opaque box showing the backcolor API ('silver' = lightgray)
panel   = controls.new("panel", "", 420, 110, 160, 120)
controls.backcolor(panel, colors.silver)

clicks = 0

function bstate(h)
  if controls.down(h) then return "down"
  elseif controls.hover(h) then return "hover"
  else return "idle" end
end

while true do
  -- counter button: click it to change its caption
  if controls.clicked(bCounter) then
    clicks = clicks + 1
    controls.text(bCounter, "Clicked " .. clicks)
  end

  -- Close hides itself; Open brings it back
  if controls.clicked(bClose) then controls.hide(bClose) end
  if controls.clicked(bOpen) then controls.show(bClose) end

  -- Save grows the counter button (width API, sized on the main thread)
  if controls.clicked(bSave) and controls.width(bCounter) < 280 then
    controls.width(bCounter, controls.width(bCounter) + 40)
  end

  -- the named global 'okbtn' equals the bOK handle
  sound = "off"
  if controls.checked(chk) then sound = "on" end

  fe = "no"
  if controls.focused(ed) then fe = "yes" end

  -- status box (drawn with the legacy canvas API)
  canvas.color = colors.black
  canvas.rectangle(20, 330, 480, 130, true)
  canvas.color = colors.white
  canvas.text(30, 340, bstate(bOpen) .. " | " .. bstate(bSave) .. " | " ..
                       bstate(bClose) .. " | " .. bstate(okbtn))
  canvas.text(30, 360, "counter=" .. clicks .. "  sound=" .. sound ..
                       "  controls.count=" .. controls.count)
  canvas.text(30, 380, "edit focused=" .. fe .. "  border=" .. controls.border(panel))
  canvas.text(30, 400, "edit text: " .. controls.text(ed))

  sleep(16)
end