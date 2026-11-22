# Physics

[<< Tyro docs index](README.md)

Every sprite can join a real rigid-body world (Chipmunk2D, pixels and seconds,
screen-style y-down). Set `collide` and the sprite gets a body on the next
frame; the engine steps the world once per frame, so a dynamic body's `x/y`
and `vx/vy` are the simulation's own answer, written back to the sprite.
A sprite with no `radius` takes a box the size of its texture at its scale.

| Property | Type | Description |
|----------|------|-------------|
| `sprite.collide` | `boolean` | Put a body in the world for this sprite (read/write, default `false`). |
| `sprite.kind` | `string` | `'dynamic'` (gravity-driven, the default), `'static'` (an immovable wall) or `'kinematic'` (the script moves it, everything else reacts to it). |
| `sprite.mass` | `number` | Mass of a dynamic body (default `1`). |
| `sprite.friction` | `number` | Surface friction, `0..1` (default `0.5`). |
| `sprite.bouncy` | `number` | Bounciness, `0..1` (default `0`). |
| `sprite.radius` | `number` | A circle instead of a box, in pixels (`0` = the texture box). |
| `sprite.vx` / `sprite.vy` | `number` | Velocity in px/s (read/write): a write sets the body, a read reports its current motion. |
| `sprite.sensor` | `boolean` | A sensor reports touches but never pushes: bodies pass through it (default `false`). Triggers, one-way gates and check lines are sensors. |
| `sprite.group` | `number` | Shapes that share a non-zero group never touch each other (`0` = no group, the default). Two bullets of one fleet: give them all `group = 3` and they fly through each other, yet still hit the wall. |
| `sprite.category` | `number` | The bits this shape asks to be seen with (default: all bits). |
| `sprite.mask` | `number` | The bits this shape agrees to touch (default: all bits). Two shapes touch only when each one's `category` bit lives in the other's `mask` — the classic way to let a bullet hit enemies but not its shooter. |

| Function / property | Description |
|----------|-------------|
| `collision.gravityx` / `collision.gravityy` | The world's gravity in px/s² (read/write, default `0` / `980`). |
| `collision.bodies` | How many sprite bodies the space holds (read-only). Tilemap walls are not counted — they are part of the map, not a sprite. |
| `collision.pump()` | Drain the pending contact events and fire each sprite's `onCollide(other, state)` (`"enter"` / `"leave"`). A sprite with its own script gets `on_collide` fired by the engine instead, without any pumping. |

A tilemap joins the world through its own switch: `map.collide = true` turns
every **solid cell** (`map.solidtile(tile, true)`) into a static wall standing
where the map draws — the same `x`/`y`/`scale` the picture uses. Edit the grid
or move the map and the walls follow on the next frame. The walls are silent:
they push, but they never appear in an `on_collide` — which answers for the
sprite, so a platformer's hero lands on a tile and hears nothing but its own
`on_update` telling it that it can jump again.

```lua
window.show(320, 240)
collision.gravityy = 600

local floor = tiles.new("ground")
floor.load("tiles.png", 16, 16)
floor.resize(20, 3)
floor.fill(1)
floor.solidtile(1, true)      -- tile 1 is a wall tile
floor.y = 200
floor.collide = true          -- and now it is also physics

local ball = sprites.puff("ball", 24)
ball.radius = 12
ball.collide = true
ball.bouncy = 0.7
ball.x, ball.y = 80, 20

local gate = sprites.puff("gate", 24)
gate.kind = "static"
gate.collide = true
gate.radius = 40
gate.sensor = true            -- a trigger the ball falls straight through
gate.x, gate.y = 160, 100

function update(dt)
  collision.pump() -- fires ball.onCollide for the touches under the gate
end

function ball.onCollide(other, state)
  if state == "enter" and other == gate then
    println("through the gate")
  end
end
```

A sprite that owns a script file needs no pump: the engine fires its
`on_collide(other, state)` on the main thread by itself.

