# Joints — Ropes, Rods and Motors 🪢

Physics gets fun the moment things connect. A **joint** is a promise between
two sprites: *stay this far apart*, *never further than that*, *turn toward
this angle*. Chipmunk keeps the promises every frame — no update() math on
your side.

```lua
local hook = sprites.blob("hook", 32)
hook.kind = "static"              -- the anchor: bolts do not move
hook.x, hook.y = 320, 80

local ball = sprites.blob("ball", 44)
ball.x, ball.y = 420, 80          -- 100 px to the side
collision.joint(hook, ball)       -- a pin keeps them 100 px apart - a pendulum!
```

The joint speaks through the `collision` family because it lives in the
physics world:

| API | What it does |
| --- | --- |
| `collision.joint(a, b [, opts])` | join two sprites; answers a joint id (or `nil` + a message) |
| `collision.joint_free(id)` | cut one joint (`true`/`false`) |
| `collision.joints()` | how many joints the script has armed |
| `collision.joints_clear()` | cut them all; answers how many left |

Both sprites join the physics world on the way in — `joint()` sets
`collide = true` on them, because a rope tied to a non-physical sprite holds
nothing. Their `kind` is what it already is, and that is the whole design:
a **static** sprite is a bolt, a **kinematic** one is a hand you move by
script, and a **dynamic** one is the thing that swings, dangles and falls.

## The Kinds

| `kind` | The promise | Its numbers (all optional) |
| --- | --- | --- |
| `"pin"` (the default) | a rigid rod — the anchors keep their distance | `length` px (default: the distance right now) |
| `"slide"` | a leash — the anchors stay between `min` and `max` | `max` px (default: the distance right now), `min` px (default 0) |
| `"rotator"` | a motor — `b` turns toward `angle` beside `a` and holds there | `angle` degrees (default 0), `strength` (default 300), `damping` (default 30) |
| `"limit"` | a gate — `b` may only turn between `min` and `max` | `min`, `max` degrees (default -180 / 180) |

`"rope"`/`"leash"` and `"motor"` work as spellings of slide/rotator, and
`"hinge"` of limit. Every joint also takes:

- `ax, ay` / `bx, by` — the **point each sprite holds onto**, in pixels from
  its centre, rotating along with it (default the centre itself).
- `strength` — for pin/slide/limit the cap on how hard the joint may pull
  (default: as hard as it must); for a rotator it is the spring's own muscle.

A few hands-on recipes:

```lua
-- a chain: each link hangs on the one above
for i = 2, 6 do
  collision.joint(links[i - 1], links[i], { length = 34, ay = 17, by = -17 })
end

-- a grab: the box dangles from your mouse until you let go
collision.joint(mouse_sprite, box, { kind = "slide", max = 90 })

-- a self-closing door: the gate stops at the frame, the motor pulls it shut
collision.joint(frame, door, { kind = "limit", min = 0, max = 95 })
collision.joint(frame, door, { kind = "rotator", angle = 0, strength = 60 })
```

## When Joints Leave

A joint lives as long as both of its sprites stay in the physics world:

- free either sprite, or turn its `collide` off, and the joint is gone with
  the body it hung on;
- **rebuild the sprite** — change its `kind`, or its shape (radius, or the
  texture size behind the box) — and the body is rebuilt fresh, so the
  sprite's joints drop with the old body. Re-arm what you still want.
- `collision.joints_clear()` (or the run ending) takes the rest.

A joint armed before its sprites are physical waits a beat or two and lands
the moment both bodies exist — `joints()` counts those already armed, and
`joint_free()` can cancel one that has not landed yet. The rotator is a
*damped spring*, so a strong push (or a light `strength`) makes it springy
on purpose: that is the wobble of a hand-cranked thing, not a bug.

## When a Rope Snaps Taut

A slide joint feels the world and says so on the event bus: the moment it
reaches its `max` it reports `"taut"`, and the moment the gap shrinks back
inside it, `"slack"` — edges only, never a every-frame drip.

```lua
on("joint_stretch", function(a, b, state)
  if state == "taut" then
    music.sound("bubble", 260, 60)        -- the rope goes "boing"
    tween.from(pet, "scale", 1.3, 1, 200, "quadout")
  end
end)
```

The handler hears every slide in the game as `(spriteA, spriteB, state)`,
so it reads like `on("collision")` — check which pair it names before
answering. Pins, rotators and limits never report: they hold their promise
every beat, so they have nothing new to say. It is the same bus as
everything else ([`help/events.md`](events.md)), and a game nobody listens
in costs nothing — the engine drains the whispers unread.

To see all of it swinging — pendulum wave, mouse leash and a gearless
motor, with the leash blooping when it catches — run
[`demos/joints_demo.tyro`](../demos/joints_demo.tyro).
