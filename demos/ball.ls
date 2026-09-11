--======================================================================
--  ball.ls - a sprite script attached to a sprite
--======================================================================
--  Attach with:  sprite.load("ball.png", "ball.ls")
--  The file's top-level code runs once when attached. The engine then
--  calls on_update()/on_draw()/on_collide() on the main thread.
--======================================================================

bounces = 0

function on_update()
  -- gentle self-steering so the ball keeps moving between bounces
  if time() % 4 > 2.5 then
    self.x = self.x + 0.4
  else
    self.x = self.x - 0.4
  end
end

function on_draw()
  -- overlay drawn after the texture blit, in sprite/world coordinates
  draw.circle(self.x, self.y, self.radius + 4, colors.yellow, false)
  draw.text(self.x - self.radius, self.y - 48, "bounces " .. bounces, colors.white)
end

function on_collide(other, state)
  if state == "enter" and other.__name == "ground" then
    bounces = bounces + 1
    println("ball bounced on the ground (#" .. bounces .. ")")
  end
end