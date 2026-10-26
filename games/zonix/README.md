# ZONIX - Cyber Arcade Territorial Conquest

**Zonix** is a fast-paced, high-voltage retro-arcade territorial conquest game built for the **Tyro Game Engine**, inspired by the 1982 coin-op and DOS classics *Xonix* and *Qix*.

Pilot the high-energy **Cutter Craft** across a glowing cybernetic grid. Carve paths through the dark void, claim territory away from bouncing quantum orbs, dodge fast perimeter sparks, and race against the trailing fuse spark to capture the required percentage of each sector!

---

## Gameplay & Rules

1. **The Grid & Safe Territory**:
   - The playfield is an 88x46 grid (880x460 pixels) inside a 960x540 display.
   - The outer perimeter and claimed areas are safe **Land**. Bouncing orbs inside the void cannot hit you while you are on Land.
   - However, **Border Sparks** patrol the perimeter of Land — do not let them touch your Cutter Craft!

2. **Cutting the Void**:
   - When you move off Land into the uncaptured **Sea**, your Cutter deploys cutting lasers and leaves behind an active energetic **Trail**.
   - If an Orb touches your trail while you are cutting: **instant explosion**!
   - You cannot reverse 180 degrees directly into your own trail.
   - If you hesitate too long in the void, a **Fuse Spark** ignites at the trail origin and races along the path toward you!

3. **Claiming Territory**:
   - When your Cutter returns to any Land cell, the trail closes.
   - An instant breadth-first flood-fill scans the void starting from all internal enemies. Any region containing **no enemies** is enclosed and claimed!
   - Newly claimed territory transforms into illuminated high-tech neon panels, awarding points and score bonuses for large slices (cuts >= 10% or >= 25% award massive point multipliers).
   - Once your captured territory reaches or exceeds the sector's **Target Percentage** (75% to 85%), the level is cleared!

4. **Power-Ups**:
   - Random cyber badges materialize in the uncaptured sea:
     - **FREEZE (Snowflake)**: Freezes all bouncing orbs for 5 seconds.
     - **SUPER SPEED (Lightning)**: Grants 1.5x cutting speed boost.
     - **SLOW MOTION (Hourglass)**: Halves enemy movement speed.
     - **BONUS SCORE (Diamond)**: Awards +2500 instant points.
     - **EXTRA LIFE (Cyber Heart)**: Restores +1 Cutter Craft life (up to 5).
     - **SMART BOMB (Nova)**: Detonates, clearing all border sparks and trailing fuses.
   - Power-ups can be collected by flying over them or by enclosing them within claimed land.

---

## 8 Cyber Sectors (Levels)

Each level features unique target thresholds, enemy compositions, speeds, and custom neon color palettes:

| Level | Sector Name | Target % | Orbs | Fast Orbs | Boss | Sparks | Theme Palette |
|:-----:|:------------|:--------:|:----:|:---------:|:----:|:------:|:--------------|
| **1** | CYBER GRID   | 75% | 1 | 0 | 0 | 1 | Cyber Blue & Electric Cyan |
| **2** | NEON CORE    | 75% | 2 | 0 | 0 | 1 | Neon Magenta & Deep Pink |
| **3** | QUANTUM LAB  | 78% | 2 | 1 | 0 | 2 | Emerald Matrix & Acid Green |
| **4** | DIGITAL VOID | 78% | 2 | 1 | 0 | 2 | Solar Amber & Cyber Yellow |
| **5** | PLASMA MATRIX| 80% | 3 | 1 | 0 | 2 | Laser Crimson & Deep Orange |
| **6** | IONIC STORM  | 80% | 3 | 2 | 0 | 3 | Cosmic Violet & Electric Purple |
| **7** | SUB-ATOMIC   | 82% | 3 | 2 | 1 | 3 | Quantum Teal & Deep Turquoise |
| **8** | ZONIX NEXUS  | 85% | 4 | 2 | 1 | 4 | Hyperion Gold & Lava Orange |

---

## Controls

| Action | Keyboard | Gamepad |
|:-------|:---------|:--------|
| **Steer / Cruise** | `Arrow Keys` or `W`, `A`, `S`, `D` | `Left Stick` / `D-Pad` |
| **Boost Speed** | `Space` or `Shift` | `A Button` |
| **Pause / Resume** | `P` | `Start` |
| **Mute / Unmute** | `M` | - |
| **Shader CRT Glow** | `Q` (decrease) / `E` (increase) | - |
| **Quit** | `ESC` | - |

---

## Engine Features & Architecture

- **Custom Shaders**:
  - `zonix.frag`: Fragment shader featuring 4-tap neon bloom, subtle arcade CRT scanlines, gentle edge chromatic aberration, and corner vignette. Strength can be adjusted live in-game with `Q` and `E`.
- **Wavetable Sound Synthesis**:
  - Leverages 4 new procedural sound generators added to `TyroSounds.pas`:
    - `Zap`: Electric arc discharge when sparks pop or damage occurs.
    - `Laser`: Frequency-modulated beam chirp when cutting grid cells.
    - `Chime`: Multi-partial crystalline harmonic chime for sector claims.
    - `Sweep`: Upward sci-fi sweep for power-ups and sector completion.
- **Sprite Atlases**:
  - Multi-frame `.sprite` atlases with TexturePacker JSON descriptors for animated entities:
    - `cutter.sprite`: 8 frames (4 cruise directions + 4 cutting laser directions).
    - `orb.sprite`: 6 frames of rotating quantum electron cores.
    - `fast_orb.sprite`: 4 frames of agile spiked plasma drones.
    - `boss_core.sprite`: 6 frames of heavy rotating armored pulsar core.
    - `spark.sprite`: 4 frames of electric perimeter arcs.
    - `fuse.sprite`: 4 frames of trailing plasma fire.
    - `powerups.sprite`: 6 frames of distinct badge icons.
    - `boom.sprite`: 8 frames of explosive shockwave particles.
    - `trail_spark.sprite`: 4 frames of laser trail markers.

---

## Asset Credits

All sprite textures, atlases, animations, and sound effects were custom-designed and procedurally generated from scratch using Python (Pillow + standard library `wave` and `math` in `make_assets.py`). Zero third-party or external copyrighted assets are used.

- **Author / Developer**: Belal / Antigravity AI
- **Engine**: Tyro Game Engine (RayLib + Lua + FPC)

---

## How to Run

From the repository root:

```bash
# Run the game:
bin\tyro.exe games\zonix\zonix.tyro

# Run automated smoke test:
bin\tyro.exe tests\test_zonix_data.tyro --exit:5 --console

# Run attract mode test:
type nul > games\zonix\attract.txt
bin\tyro.exe games\zonix\zonix.tyro --exit:6 --screenshot:4 --console
del games\zonix\attract.txt
```
