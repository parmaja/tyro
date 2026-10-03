--======================================================================
--  Collision demo - Chipmunk2D physics for Tyro
--======================================================================
--  Demonstrates: rigid body physics (dynamic/static), gravity,
--  sprite.onCollide(other, state) triggers, and collision.pump().
--
--  Two balls fall and bounce off static walls/floor. The ball
--  sprites are invisible; we draw them with canvas primitives using
--  sprite.x/y (which Chipmunk updates every frame).
--======================================================================

window.show(640, 480)
console.show()

-- ---- Physics setup ----------------------------------------------------
collision.gravityy = 980          -- fall down (Y-down screen coords)

-- ---- Scene: static walls and floor ------------------------------------
ground = Sprites.new("ground")    -- floor: 640 x 32 static box
ground.load("ground.png")
ground.visible = false
ground.kind = "static"
ground.collide = true
ground.x = 320
ground.y = 470

lwall = Sprites.new("leftwall")   -- wall: 10 x 300 static box
lwall.load("wall.png")
lwall.visible = false
lwall.kind = "static"
lwall.collide = true
lwall.x = 5
lwall.y = 280

rwall = Sprites.new("rightwall")
rwall.load("wall.png")
rwall.visible = false
rwall.kind = "static"
rwall.collide = true
rwall.x = 635
rwall.y = 280

-- ---- Dynamic balls -----------------------------------------------------
ball1 = Sprites.new("ball1")      -- 64x64 texture, circle body r=20
ball1.load("ball.png")
ball1.visible = false
ball1.kind = "dynamic"
ball1.collide = true
ball1.radius = 20
ball1.mass = 1
ball1.bouncy = 0.7
ball1.friction = 0.3
ball1.x = 180
ball1.y = 80

ball2 = Sprites.new("ball2")      -- heavier + bouncier circle r=18
ball2.load("ball.png")
ball2.visible = false
ball2.kind = "dynamic"
ball2.collide = true
ball2.radius = 18
ball2.mass = 2
ball2.bouncy = 0.9
ball2.x = 460
ball2.y = 60

-- ---- Collision triggers -----------------------------------------------
hits1 = 0
ball1.onCollide = function(other, state)
  if state == "enter" and other.__name == "ground" then
    hits1 = hits1 + 1
  end
  println("ball1 x " .. other.__name .. " " .. state)
end

hits2 = 0
ball2.onCollide = function(other, state)
  if state == "enter" then
    hits2 = hits2 + 1
  end
  println("ball2 x " .. other.__name .. " " .. state)
end

println("=== Collision demo ===")
println("Bodies: " .. collision.bodies)

-- ---- Main loop --------------------------------------------------------
while true do
  -- drain physics events and fire onCollide handlers
  collision.pump()

  -- draw frame
  canvas.color = colors.black
  canvas.rectangle(0, 0, canvas.width, canvas.height, true)

  -- ground (640x32 centered)
  canvas.color = colors.white
  canvas.rectangle(ground.x - 320, ground.y - 16, 640, 32, true)

  -- walls (10x300 centered)
  canvas.color = colors.white
  canvas.rectangle(lwall.x - 5, lwall.y - 150, 10, 300, true)
  canvas.rectangle(rwall.x - 5, rwall.y - 150, 10, 300, true)

  -- balls
  canvas.color = colors.red
  canvas.circle(ball1.x, ball1.y, ball1.radius, true)
  canvas.color = colors.green
  canvas.circle(ball2.x, ball2.y, ball2.radius, true)

  -- status
  canvas.color = colors.white
  canvas.text(10, 10, "hits: " .. hits1 .. " / " .. hits2)

  sleep(16)   -- ~60 FPS
end