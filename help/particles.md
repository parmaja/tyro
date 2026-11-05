# Particles - Explosions Without the Bookkeeping 💥

Something blew up and you want shards, sparks and smoke - without writing a
particle engine yourself. Tyro has one built in: the global `particles` table.
You **throw a burst** at a screen position and forget about it. The engine
animates every spark and draws it for you, on top of the sprites and the 3D
models, until the last spark fades.

```lua
function update(dt)
  if bomb.exploded then
    bomb.exploded = false
    particles.emit(bomb.x, bomb.y, 20, { color = 'orange', speed = 180 })
  end
end
```

That's the whole game loop: no lists of sparks, no per-frame drawing, no
cleanup. The sparks move, shrink, fade and vanish on their own.

---

## The Commands

| API | What it does |
| --- | --- |
| `particles.emit(x, y [, count] [, options])` | throw one burst at the screen pixel `x,y`; returns how many sparks were born |
| `particles.clear()` | drop every live spark (e.g. when a round ends) |
| `particles.count` | how many sparks are alive right now |

## The Options (all optional)

| Option | Default | What it does |
| --- | --- | --- |
| `color` | `'orange'` | the spark's ink (a `colors.` name or value); it fades out as the spark dies |
| `speed` | `140` | launch speed in px/s - each spark gets a jitter of its own |
| `life` | `0.5` | seconds a spark lives (jittered per spark) |
| `size` | `3` | spark radius in px; it shrinks toward a small core |
| `gravity` | `240` | px/s^2 toward the bottom edge - hot debris falls |
| `drag` | `1.5` | fraction of the speed shed per second (air friction) |
| `spread` | `360` | degrees the burst covers; `360` is a full ring |
| `direction` | `0` | the middle of the spread, degrees (`0` right, `-90` up) |
| `under` | `false` | `true` draws the burst BELOW the 3D models - for a flame a jet carries, so the model covers it |

The engine caps the sky at 2048 sparks; when it is full, a new burst is
simply dropped - so firing wildly can never slow the game down.

---

## Recipes

A classic explosion - fire, then embers, then a white flash:

```lua
particles.emit(x, y, 26, { color = 'orange', speed = 200, life = 0.7, size = 4, gravity = 160 })
particles.emit(x, y, 14, { color = 'yellow', speed = 130, life = 0.45, size = 3 })
particles.emit(x, y, 8,  { color = 'white',  speed = 90,  life = 0.25, size = 2 })
```

Dust kicked up behind a racing car (a narrow cone, aimed up):

```lua
particles.emit(car.x, car.y + 10, 3, { color = 'gray', speed = 60, life = 0.4,
                                       size = 4, spread = 40, direction = -90 })
```

A magic sparkle trail - no gravity, soft drag, tiny dots:

```lua
particles.emit(fairy.x, fairy.y, 2, { color = 'skyblue', speed = 20,
                                      life = 0.6, size = 2, gravity = -30 })
```

---

## Good to Know

- **Sparks ride the camera.** When the world `shake()`s, the sparks shake with
  it, exactly like the sprites and models.
- **A paused scene freezes the sparks** - they belong to the scene, like
  sprite animations do.
- **Emit anywhere.** `particles.emit()` is safe in `update()`, in a collision
  callback or in plain script code - it is not a drawing command, it is a
  request the engine takes over.
- The bursts are drawn **above the 3D models**, so an explosion always reads
  over the jets, cars or wizards that made it. A burst emitted with
  `under = true` takes the layer *below* the models instead - the right choice
  for an exhaust flame a jet carries: the plane covers the flame at its own
  skin, and only the burn behind the tail shows.
