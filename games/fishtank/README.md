# Fish Tank

A calm aquarium for Tyro. Nine pixel fish swim freely over the whole screen,
each with its own speed, depth and wandering mind. Bubbles rise from two air
stones and from the fishes' mouths, wobble on the way up, and pop at the
surface with a generated `Bubble` waveform. A custom full-screen shader
(`water.frag`) refracts the scene and adds depth tint, moving caustics,
shimmer bands and a glass vignette.

## Run

```
tyro games/fishtank/fishtank.tyro
```

The `fish/` folder must stay next to `fishtank.tyro`; `water.frag` is loaded
from the script folder as well.

## Controls

| Input | Action |
|-------|--------|
| `B` or left click | Blow a burst of bubbles |
| `W` | Water shader on / off |
| `UP` / `DOWN` | Stronger / weaker water |
| `ESC` | Quit |

## Engine sound

The bubble sound is a real waveform generator in `src/tyrolib/TyroSounds.pas`
(`Bubble_Waveform`), registered as `"Bubble"`. It is a short rising "bloop":
the pitch glides up from 55% of the note in about 20 ms and a small high ping
gives the resonant tick. It is played with `music.sound("Bubble", pitch, ms)`.

# Credits

https://polar-harbor.itch.io/pixel-reef-fish
