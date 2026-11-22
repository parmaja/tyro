# Bubble Pop 🫧

The juice demo: every moving thing in this game is a `tween()` glide — no
hand-rolled easing anywhere. Bubbles punch up from the sea floor, rise with
an entry overshoot, and you pop them before they escape. Small ones pay
best. Three escapes and the sea closes.

    bin\tyro.exe games/bubblepop/bubblepop.tyro

No image files: the bubbles are engine-baked `sprites.blob` textures, tinted
per spawn.

## What the tweens do here

| Moment | Call |
| --- | --- |
| entrance | `tween.from(b, "scale", 0, s, 340, "backout")` |
| the rise | `tween(b, "y", -70, ms, "linear")` |
| the pop | `tween.from(b, "scale", 1.25s, 1.75s, ...)` + alpha fade whose `done` hides the sprite |
| score | a plain table: `tween.from(hud, "lift", 12, 0, 200, "quadout")` |
| miss | full-screen red `tween.from(flash, "a", 90, 0, 420, "quadout")` + life-bubble `snap` |
| title / over card | `backout` slide-ins |

The side-to-side sway is deliberately *not* a tween — it's a sine in
`update()`, to show both styles side by side.

## Keys

- **Left mouse** pop
- **SPACE** start / restart
- **R** restart any time
- **ESC** exit

## Testing

    bin\tyro.exe games/bubblepop/bubblepop.tyro test  --exit:6 --console
    bin\tyro.exe games/bubblepop/bubblepop.tyro dbg   --exit:9 --screenshot:3 --console

`test` starts a round at once, pops a bubble through the real `pop()` and
prints the tween bookkeeping; `dbg` holds the splash open so a screenshot
can catch it. The API itself is covered by `tests/tween_test.tyro`.
