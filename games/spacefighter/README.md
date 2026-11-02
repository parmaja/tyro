# Space Fighter 3D: Deep Space Combat

A 3D arcade space combat shooter for the Tyro Game Engine built with Lua and RayLib.

## Concept & Visual Style

Pilot an advanced deep space starfighter through uncharted star systems, nebulae, and asteroid belts. Engage hostile alien scout squadrons, interceptor wings, and heavy cruisers in full 3D perspective projection.

- **Deep Space Environment**: Multi-layer 3D starfield with glistening diamond star flares, drifting nebulae, and planetary backdrops.
- **Player Starfighter**: 3rd-person chase camera view with banking tilt, glowing particle thrusters, and 3D crosshair HUD.
- **3D Ballistics & Collisions**: Twin forward plasma cannons firing into the screen depth, spherical 3D hitboxes, expanding shockwaves, and tumbling debris.
- **Enemy AI**: Formations entering from deep space ($Z \approx 2000$), executing combat passes, firing laser volleys, and breaking off.
- **Boss Encounter**: Massive alien flagship dreadnought with destructible core hardpoints.

## Controls

| Input | Action |
|---|---|
| **W / S** or **Up / Down** | Pitch Down / Up |
| **A / D** or **Left / Right** | Roll & Strafe Left / Right |
| **SPACE** | Fire Twin Plasma Cannons |
| **SHIFT** | Afterburner Boost |
| **R** | Restart Mission |
| **ESC** | Exit Game |

## How to Run

```cmd
bin\tyro.exe games\spacefighter\spacefighter.tyro
```

## Architecture & Implementation Modules

1. **3D Projection System**: $X, Y, Z \to sx, sy, scale$ with depth sorting ($Z$-buffer).
2. **Procedural Sprite Pipeline**: Python asset generator (`make_assets.py`) for starfighter, enemies, asteroids, projectiles, and particle flares.
3. **Engine Callbacks**: Strict adherence to Tyro's `update(dt)` and `draw()` callbacks.
4. **Combat & Collision**: 3D distance checks and spatial collision response.
5. **Wave Progression**: Wave-based state machine culminating in a multi-stage boss.
