--======================================================================
--  marks.ls - a script with no on_collide, to show it's optional
--======================================================================
--  Attach at runtime with:  sprite:loadscript("marks.ls")
--======================================================================

angle = 0

function on_update()
  angle = angle + 2
  if angle >= 360 then angle = 0 end
end

function on_draw()
  local a = angle * math.pi / 180
  draw.line(self.x - 40 * math.cos(a), self.y - 40 * math.sin(a),
            self.x + 40 * math.cos(a), self.y + 40 * math.sin(a), colors.aqua)
  draw.text(self.x - 16, self.y - 10, "marks", colors.aqua)
end