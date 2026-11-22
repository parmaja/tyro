# Tweens - Juice Without the Math 🧃

A sprite that *teleports* to its new spot looks broken. A sprite that
**glides, overshoots and settles** looks alive. That glide is a *tween*: you
tell the engine the property, the value and the milliseconds - and it writes
every frame in between for you. The global `tween` table does it all.

```lua
function update(dt)
  if keyboard.pressed("space") then
    tween(player, "x", 400, 350, "outback")   -- glide, overshoot, settle
  end
end
```

No lists of animations, no easing formulas, no per-frame bookkeeping. Tweens
run on the **scene clock**: `world.timescale` stretches them with the rest of
the scene and `pause()` sleeps them - exactly like `after()` and `every()`.

---

## The Commands

| API | What it does |
| --- | --- |
| `tween(obj, prop, to, ms [, easing [, done]])` | glide one numeric property from where it stands to `to`; returns the tween id |
| `tween.to(...)` | the very same call, spelled out |
| `tween.from(obj, prop, from, to, ms [, easing [, done]])` | write `from` **at once**, then glide to `to` - the punch pattern |
| `tween.kill(id)` | stop one glide; the property stays where it was (`true`/`false`) |
| `tween.killall(obj)` | stop every glide on that exact subject; returns how many died |
| `tween.count()` | how many glides are moving right now |

`tween` is a callable table: `tween(...)` and `tween.to(...)` are the same
call. A bad argument never crashes - it answers `nil` plus a message:

```lua
print(tween(player, "x", "far", 300))  -- nil  tween: the target value must be a number
```

## What Can Be Tweaked

Anything numeric: a sprite's `x`, `y`, `angle`, `scale`, `alpha`, `radius`
(the same property names as usual - the tween writes them the very way your
script does); a control's `x` and `y`; or any field of **your own table** -
that's how the HUD gets juice:

```lua
local hud = { lift = 0 }                 -- a plain table is a fine subject
function pop()
  score = score + 1
  tween.from(hud, "lift", 12, 0, 200, "quadout")  -- the score jumps and settles
end
function draw()
  screen.text(8, 450 - hud.lift, "SCORE " .. score)
end
```

`from` is read at arming time through the subject's own eyes, so a sprite
answers with its live engine value. Use `tween.from` when the value is not
there yet, or when you want to *start big* (a punch, a flash, a hit blink).

## The Easing Names

One base + one direction, in either order, case and `_` ignored: `"backout"`,
`"outback"` and `"BACK_OUT"` all mean the same curve.

| Base | Feel |
| --- | --- |
| `sine` | soft, the gentle one |
| `quad` | plain accelerate / decelerate |
| `cubic` | stronger version of quad |
| `expo` | snappy, saves everything for the end |
| `back` | dips below / overshoots before landing |
| `elastic` | a rubber-band wobble as it settles |
| `bounce` | lands like a dropped ball |

Directions: `in` (starts slow), `out` (ends slow), `inout` (both). A bare
base means its `out` curve. Three friendly aliases: `smooth` (= sineinout),
`snap` (= backout, the button-punch favourite) and `wobble` (= elasticout).
`linear` is a steady glide, and a name you make up lands on the plain
`quadout` - so leaving the easing out altogether already looks decent.
Every curve lands **exact**: `to` is the value you get, to the digit.

## Ids, Deaths and Respawn Safety

A tween answers with an id like `after()`; it dies when it lands (its `done`
callback, if any, runs on that frame with the subject as argument), when you
`tween.kill(id)` it, or when the run ends. The one habit worth learning is
**kill before reuse**:

```lua
function launch(b)
  tween.killall(b)                 -- shed the old life's glides
  b.visible = true
  tween.from(b, "scale", 0, 1, 320, "backout")   -- pop into existence
  tween(b, "y", -70, 5000, "linear")             -- drift up
end
```

A sprite table, like any table, can carry as many parallel tweens as you
like - one on `x`, one on `alpha`, one on `angle` - they never fight unless
they aim at the same property.

## Only Tweens? Fine.

A script with no `update()` and no `draw()` but a live tween still gets the
engine's frame loop, so a splash card can be pure tweens:

```lua
local title = { y = -40 }
function draw()
  screen.text(200, title.y, "BUBBLE POP")
end
tween(title, "y", 190, 700, "backout")   -- the engine runs until it lands
```

## A Handful of Moves

```lua
tween(btn,   "scale", 1.1, 90, "in")                       -- button grow
tween.from(btn, "scale", 1.1, 1, 140, "snap")              -- ... and its release
tween.from(flash, "a", 90, 0, 400, "quadout")              -- red damage flash
tween(boss, "x", 120, 90, "linear", function()             -- jitter combo
          tween(boss, "x", 520, 90, "linear")
        end)
tween(money, "y", money.y - 30, 500, "out")                -- a rising +5 coin
tween(chest, "angle", -10, 70, "inout",                     -- a small shake
  function() tween(chest, "angle", 10, 140, "inout",
  function() tween(chest, "angle", 0, 70, "inout") end) end)
```

For the full picture in a real game, read `games/bubblepop/` - every moving
thing in it is one of these calls.
