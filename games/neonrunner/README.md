# Neon Cyber Runner

A high-speed 2D cyber arcade lane-runner built for the **Tyro Game Engine** using Lua, Raylib, custom GLSL fragment shaders, procedural wavetable synthesis, and animated sprite atlases.

Pilot the experimental **Vector-9** hoverbike down the high-speed cyber arteries of Neo-Metropolis. Weave through autonomous cyber traffic, jump over EMP barricades and electro-spikes, harvest neon battery cells to fuel hypersonic Turbo Boost, and unleash EMP shockwaves to obliterate roadblocks.

---

## Features

- **Dedicated GPU Sprite Shader**:
  - `player_turbo.frag`: Dedicated GPU fragment shader attached to the hoverbike quad, providing an emissive cycling rainbow speed aura and plasma afterglow when Turbo Boost or Shield is active.
- **Animated Multi-Frame Sprite Atlases (`.sprite` & `.png`)**:
  - `player.sprite`: Vector-9 hoverbike with cruising, banking up/down, hover jump, hypersonic turbo boost, and glitch/damage frames.
  - `traffic.sprite`: 4 distinct autonomous vehicles (Crimson Speeder, Heavy Armored Hauler, Security Patrol Drone, Glitch Phantom Racer).
  - `items.sprite`: Battery Cells, Gold Data Cubes, Forcefield Shield Orbs, EMP Shockwave Cores, and Speed Boost Chevrons.
  - `hazards.sprite`: EMP Road Barricades, Vertical Laser Pylons, and Electro-Spike Strips.
  - `shield.sprite`: Rotating hexagonal electric forcefield bubble.
  - `boom.sprite`: 6-frame digital voxel cyber explosion shockwave.
- **Procedural Sound & Wavetable Synthesis**:
  - New wavetables added to `TyroSounds`:
    - `CyberBass`: Fat, resonant analog synth bass with a punchy downward filter sweep and subtle drive.
    - `TurboRev`: High-energy ion turbine / jet engine rev-up with phase-modulation and metallic harmonic sheen.
  - Built-in wavetables: `Laser`, `Zap`, `Sweep`, `Explode`, `Chime`, `Boing`, `Coin`.
  - Looping engine drone retuned dynamically with hoverbike velocity (`sound.loopPitch`).
  - Multi-voice Synthwave MML soundtrack (Lead + Bass arpeggiator) running in real time.
- **4 Progressive Cyber Zones**:
  1. *NEO-DOWNTOWN* (Cyan & Magenta skyline)
  2. *MEGATOWER GRID* (Electric Gold & Amber grid)
  3. *ORBIT SKYWAY* (Deep Purple & Emerald Aurora)
  4. *CYBER CORE* (Hyper Crimson & Plasma White)
- **Arcade Systems**:
  - High score tracking saved persistently to `neonrunner.hi`.
  - Combo multiplier system (x1 to x4).
  - Floating combat text popups, dynamic screen shake, and attract demo mode.

---

## Controls

| Key / Input | Action |
|---|---|
| **UP** / **DOWN** (or **W** / **S**) | Change lanes (Lanes 1 to 4) |
| **SPACE** / **Z** | Jump hoverbike (clears low obstacles & spikes) |
| **SHIFT** / **RIGHT** / **D** | Turbo Boost (hypersonic speed, ramming kinetic shield) |
| **X** | Trigger EMP Shockwave (destroys all on-screen obstacles) |
| **P** | Pause / Resume |
| **M** | Mute / Unmute audio |
| **ESC** | Exit to desktop |
| **Gamepad** | D-Pad / Left Stick: Steer; **A**: Jump; **B** / **RT**: Turbo Boost; **X**: EMP |

---

## How to Play

1. **Lane Weaving**: Dodge oncoming cyber cars, trucks, and drones by shifting across the 4 lanes.
2. **Jumping**: Low hazards like EMP Barricades and Electro-Spikes can be jumped over cleanly with `SPACE`. Vertical Laser Pylons cannot be jumped and must be dodged!
3. **Turbo Boost**: Holding `SHIFT` consumes Turbo Battery to reach blistering speeds (over 800 km/h). In Turbo mode, your bike projects a kinetic ram field that smashes through smaller traffic for bonus points!
4. **Energy & Powerups**:
   - **Battery Cells**: Restores Turbo Battery (+35 energy).
   - **Data Cubes**: +300 score and builds the combo multiplier (up to x4).
   - **Shield Orbs**: Equips a protective energy bubble that absorbs one fatal collision.
   - **EMP Cores**: Stores an emergency EMP blast (`X`) to clear the entire screen.
   - **Boost Pads**: Gives an immediate supersonic acceleration surge.
5. **Survival**: You have 3 lives. Colliding without an active shield or turbo ram consumes a life.

---

## How to Run

From the Tyro root repository folder:

```cmd
bin\tyro.exe games\neonrunner\neonrunner.tyro
```

To regenerate all pixel-art sprite atlases (optional — Python 3 with Pillow):

```cmd
python games\neonrunner\make_assets.py
```

---

## Credits & Architecture

- **Engine**: Tyro Game Engine (built with FreePascal/Delphi and Raylib).
- **Game Design & Code**: Built with Lua for the Tyro Game Engine.
- **Sprites & Artwork**: Procedurally synthesized pixel-art cyber assets generated via Pillow (`make_assets.py`). Zero third-party proprietary binary bloat.
- **Audio & SFX**: Procedural sound synthesis using Tyro's wavetable synthesizer (`CyberBass`, `TurboRev`, `Laser`, `Zap`, `Sweep`, `Explode`, `Chime`, `Boing`). MML synthwave soundtrack.
- **License**: MIT License (same as Tyro).
