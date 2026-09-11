--======================================================================
--  sprite scripts demo - per-sprite Lua "brains"
--======================================================================
--  Demonstrates:
--    sprite:load("image.png", "script.ls") - load texture + attach script
--    sprite:loadscript("script.ls")        - attach a script at runtime
--    on_update()              - per-frame logic, main thread
--    on_draw()                - drawn on top of the texture, world coords
--    on_collide(other, state) - physics contact ("enter" / "leave")
--    self.x / self.y / self.radius ...  - the sprite's own state
--    draw.* helpers           - circle, rectangle, line, text
--======================================================================

window.show(640, 480)
console.show()

collision.gravityy = 980

-- Static ground the ball bounces on
ground = Sprites.new("ground")
ground.load("ground.png")
ground.visible = false
ground.kind = "static"
ground.collide = true
ground.bouncy = 1.0
ground.x = 320
ground.y = 470

-- A bouncing ball driven entirely by ball.ls
ball = Sprites.new("ball")
ball.load("ball.png", "ball.ls")
ball.kind = "dynamic"
ball.collide = true
ball.radius = 20
ball.mass = 1
ball.bouncy = 0.75
ball.friction = 0.3
ball.x = 320
ball.y = 60

-- Runtime-attached script; no texture, just a rotating marker over the ground
marks = Sprites.new("marks")
marks.loadscript("marks.ls")
marks.x = 160
marks.y = 240

println("=== sprite scripts demo ===")

while true do
  -- physics runs inside the engine; scripted on_collide fires automatically.
  -- legacy sprites keep using collision.pump() as before.
  collision.pump()
  sleep(16)
end