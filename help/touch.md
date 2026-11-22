# Touch: `touch.*` — one script for the glass and the mouse

A tablet has fingers, a laptop has a cursor. Tyro refuses to make you write
the game twice: the `touch` table speaks for both. On a tablet it reports the
real fingers; on a desktop the **mouse stands in as one contact** while its
left button is held, so the same script answers a tap and a click.

```lua
function update(dt)
    if touch.pressed then
        pop(touch.x, touch.y)          -- a finger landed, or a left click
    end
    if touch.down then
        drag(touch.x, touch.y)         -- held: it keeps moving with you
    end
end
```

## The fields

Every field answers **the frame the engine just ran** — reading it twice in
one `update()` gives the same answer twice, like the keyboard's own flags.

| Field | What it means |
| --- | --- |
| `touch.x`, `touch.y` | where the pen is: the first finger, or the cursor on a desktop, or where the last finger left the glass |
| `touch.count` | how many contacts are on the glass right now (0..8) |
| `touch.down` | `count > 0` — something is touching |
| `touch.up` | nothing is touching |
| `touch.pressed` | a contact **arrived** this frame (tap, or left click) |
| `touch.released` | a contact **left** this frame (lift, or button release) |
| `touch.mouse` | `true` while the cursor is standing in for a finger |

## More than one finger

`touch.point(i)` walks the contacts, counted from 1 in the order the glass
felt them, and answers `x, y` — or `nil` past the last one:

```lua
for i = 1, touch.count do
    local x, y = touch.point(i)
    star(x, y)
end
```

`touch.x` / `touch.y` always follow the **first** finger, so a one-player
game never touches `point()` at all.

## The fire-and-forget way

`on("touch", fn)` runs your function on the press beat and hands it the
place, exactly like `on("space", ...)` hands you nothing but a moment:

```lua
on("touch", function(x, y)
    if inside(x, y, button_rect) then
        start_game()
    end
end)
```

## A few manners worth knowing

- **A real finger outranks the cursor.** The mouse only stands in while the
  driver reports nothing.
- **`pressed` is arrival, not holding.** Press and hold gives one `pressed`
  frame, then plain `down` frames — the same rule as `mouse.pressed`.
- **On a desktop `touch.mouse` is `true` whenever no finger is down**, even
  hovering; the stand-in only *becomes a contact* when the left button is
  held. Use it to show a hint ("tap the screen!") only on real tablets.
- Positions are window pixels, the same space `mouse.x` / `mouse.y` use, so
  `camera:to_world(touch.x, touch.y)` works for both.
