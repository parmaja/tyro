# The Matrix: Digital Rain & Cyber Defense

An authentic Matrix digital rain experience and cyber-defense minigame built for the Tyro Game Engine.

Features cascading green digital code streams with glowing white lead characters, fading phosphor tails, dynamic character mutations, depth parallax, vintage CRT scanline/phosphor shader effects, and an interactive cyber-defense minigame accompanied by the official 1999 *The Matrix* trailer soundtrack.

## How to Run

```
bin\tyro.exe games\matrix\matrix.tyro
```

## Controls

| Key / Input | Action |
|-------------|--------|
| **Mouse Move** / **Arrow Keys** / **WASD** | Aim targeting reticle / crosshair |
| **Mouse Left Click** / **SPACE** / **ENTER** | Fire EMP laser beam at Sentinels / items |
| **B** | Trigger Bullet Time (slow-motion rain) |
| **S** | Toggle CRT phosphor scanline shader |
| **R** | Replay / restart trailer soundtrack |
| **M** | Mute / unmute soundtrack |
| **ESC** | Exit to desktop |

## Game Elements

- **Sentinels (`sentinel.png`)**: Squid hunter-killer drones descending through the matrix code. Neutralize them before they reach the bottom and damage Zion's system integrity.
- **Agents (`agent.png`)**: Heavily armored rogue Agent glitches that take multiple EMP hits.
- **Red Pill (`pill_red.png`)**: Trigger Matrix Overdrive and instant Bullet Time.
- **Blue Pill (`pill_blue.png`)**: Detonate a full-screen EMP shockwave resetting corrupted threats.
- **White Rabbit (`rabbit.png`)**: "Follow the White Rabbit" bonus target: awards high score points and restores +25% system integrity.
- **Hardline Phone (`phone.png`)**: Escape/checkpoint node that advances threat levels with high score rewards.

## Credits & Sources

- **Music / Audio (`matrix_trailer.mp3`)**:
  - The official theatrical trailer soundtrack from *The Matrix* (1999), Warner Bros. Pictures, directed by the Wachowskis.
  - Original score composed by Don Davis.
  - Source: Official Theatrical Trailer (YouTube: `https://www.youtube.com/watch?v=vKQi3bBA1y8`).

- **Font (`whitrabt.ttf`)**:
  - *White Rabbit* TrueType font by Matthew Welch (Square Gear, http://www.squaregear.net/fonts/).
  - Free for personal and commercial use.

- **Sprites (`make_matrix_assets.py`)**:
  - All sprites (`rabbit.png`, `sentinel.png`, `agent.png`, `pill_red.png`, `pill_blue.png`, `crosshair.png`, `phone.png`) were procedurally generated at native pixel resolution using Python and Pillow via `make_matrix_assets.py`.

- **Shader (`matrix.frag`)**:
  - Custom CRT raster scanline, vignette, and phosphor bloom GLSL post-processing fragment shader written for Tyro.
