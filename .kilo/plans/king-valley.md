# King Valley + Engine Features

Build the **King Valley** pyramid-treasure game (using the 5 Kenney jingles already in `games/kingvalley/`) and add the four requested engine features: `sound.play(file)` SFX, gamepad input, `sprite.flipH/flipV`, and drum waveforms.

## Assets (downloaded from free sites)

| Asset | Source | License | Destination |
|---|---|---|---|
| Player: Egyptian male 4-dir walk sheet | `https://opengameart.org/sites/default/files/Egyptian%20RPG%2024x24px_0.png` (ZomBCool) | CC0 | `games/kingvalley/res/` |
| Mummy enemy: 4-dir walk/idle sheets | `https://opengameart.org/sites/default/files/mummy-1.2.zip` (AntumDeluge/Cabbit) | CC-BY 3.0/4.0 | `games/kingvalley/res/` |
| Tiles: 16x16 top-down desert set (sand, cacti, stones, vases, pyramids) | `https://opengameart.org/sites/default/files/cute_retro_rpg_-_grand_desert_demo.zip` (16Pixel) | CC-BY-SA 4.0 | `games/kingvalley/tiles.png` |
| SFX wavs (2–3 short CC0 .wav, e.g. pickup/hit) | OGA CC0 wav pack (verify at impl; fallback: synthesize with Python `wave`) | CC0 | `games/kingvalley/sfx/` |
| Music: 5 jingles | already in `games/kingvalley/*.mp3` (Kenney) | CC0 | stay |

- `games/kingvalley/CREDITS.txt` records every source + license (CC-BY attribution).
- `games/kingvalley/make_assets.py` (Python + PIL, precedent: `games/birds/make_ab_assets.py`): slices the player/mummy sheets into frames, packs them + vase/treasure frames into **`kingvalley.sprite`** (ZIP + packed PNG + Phaser/TexturePacker JSON — format verified against `src/tyrolib/SpriteAtlas.pas` before writing). Player/mummy right-facing frames reuse the left frames via the new `flipH` instead of storing both.
- The tileset sheet layout (columns, tile ids) is inspected after download; the `.tyro` tile table is written from what the script prints.

## Part 1 — Engine features

### 1a. Drum waveforms (`Kick`, `Snare`, `HiHat`) — `src/tyrolib/TyroSounds.pas`
- Add `Kick_Waveform` (sine pitch-sweep 120→45 Hz with `Exp(-t/0.05)` decay), `Snare_Waveform` (`HashNoise` burst + ~180 Hz body, `Exp(-t/0.08)`), `HiHat_Waveform` (short metallic hashed-noos noise, `Exp(-t/0.03)`) — modeled on `Explode_Waveform` (line ~1497, uses the `HashNoise(Index)` idiom so renders are deterministic).
- Forward-declare near line 229, register in the `initialization` block (line ~1572).
- **Verify:** `tests/test_waveforms.lpr` range-checks every registered waveform automatically.

### 1b. `sound.play("file.wav" [, volume])` — file-based SFX
- `src/raylib/RayClasses.pas`: `TRaySound` already frees any `Sound` with `FrameCount > 0` in its destructor, so a file-loaded `TSound` needs no new class there.
- `src/tyrolib/TyroSounds.pas`: add `TTyroFileSound = class(TTyroRayWave)` with `constructor CreateFromFile(const AFileName: string)` doing `inherited Create(False)` + `Sound := RayLib.LoadSound(PUTF8Char(AFileName))` (validates via `IsSoundValid`), plus `procedure PlaySoundFile(const AFileName: string; AVolume: Single)` that creates it, sets volume, plays, and adds to `PlayingWaveforms` (untyped `TList`, freed by the existing `UpdateWaveforms` poll — cast-safe because it subclasses `TTyroRayWave`).
- `src/tyrolib/TyroScripts.pas`: `TPlaySoundFileObject(TQueueObject)` mirroring `TPlayMusicFileObject` (line 486/1034).
- `src/tyrolib/TyroLua.pas`: `TLuaSound.Play_func` (line ~179/1504) — resolves the path with `Res.GuessFileName` like `music.play`, queues the object.
- **Verify:** `tests/test_sound_play.tyro` (+ result txt, pattern of `test_sound_api.tyro`) + runtime check via the game.

### 1c. Gamepad input — `src/tyrolib/TyroInput.pas` + `TyroLua.pas`
- `TyroInput.pas` (174 lines): add `GamepadAvailable(Gamepad)`, `GamepadName(Gamepad)`, `IsGamepadPressed(Gamepad; ButtonName)` / `IsGamepadDown(...)` via a `TryGamepadButtonNameToEnum` mapper (mirrors `TryMouseButtonNameToEnum`: `a,b,x,y`, `lb,rb,lt,rt`, `start,select,guide`, `lstick,rstick`, `dpad_up/down/left/right`), and `GamepadAxis(Gamepad; AxisName)` via `TryGamepadAxisNameToEnum` (`leftx,lefty,rightx,righty,ltrigger,rtrigger`). All call the already-linked `RayLib.IsGamepadButtonPressed/ButtonDown/GetGamepadAxisMovement` (RayLib.pas lines 1465–1485) — direct worker-thread reads, same pattern as `iskeypressed`.
- `TyroLua.pas`: declare near line 516, implement near line 2616, `RegisterGlobal` near line 1529: `isgamepadpressed(button [, gamepad=0])`, `isgamepaddown(button [, gamepad])`, `gamepadaxis(axis [, gamepad])`, `gamepadavailable([gamepad])`, `gamepadname([gamepad])`.
- **Verify:** `tests/test_gamepad.tyro` (+ result txt) — returns false/0 without a pad, real values with one.

### 1d. `sprite.flipH` / `sprite.flipV` — `src/tyrolib/TyroSprites.pas` + `TyroLua.pas`
- `TSprite`: add `FlipH, FlipV: boolean` (default `False` in `Create`); `TSprites`: `SetFlipH/SetFlipV/GetFlipH/GetFlipV` (lock pattern of `SetAngle`, line 721).
- `DrawAll` (line 1208) + `DrawSprite` (line 1258): flip via `DrawTexturePro` by negating `Dst.width`/`Dst.height` (raylib supports negative dst dims) — no new binding.
- `TyroLua.pas` `TLuaSprite.Getter/Setter` (lines 5123/5262): add `'flipH'`/`'flipV'` branches after `'scale'`.
- **Verify:** runtime via the game (player/mummy right-facing = left frames + `flipH=true`) + lint test.

**Build:** `d:\lab\programs\lazarus\lazbuild.exe src\tyro.lpi` (Debug) → `bin\tyro.exe`; `lazbuild tests\test_waveforms.lpi` → run `bin\test_waveforms.exe`. Verify the lazbuild path first (from `src/tyrolib/compile_image.cmd`).

## Part 2 — King Valley game (`games/kingvalley/kingvalley.tyro`)

**Design:** top-down desert treasure hunt. Each level is a valley map (16x16 desert tiles at scale 2, like `games/dungeon/dungeon.tyro`'s ASCII-grid levels): sand floor + cacti/stones/pyramid obstacles, golden vases as treasure, mummies patrolling. Collect **all treasure** → `03 All Treasure Collected.mp3` → `04 Next Pyramid.mp3` → next level (bigger, more mummies). A mummy catch costs a life (3) + `Explode` waveform + `shake`; 0 lives → `05 Game Over.mp3`. Title screen plays `01 Game Start.mp3` on start; `02 Main Theme.mp3` plays per level.

- **States:** TITLE → PLAY → LEVEL_DONE (2 s) → next level / GAME_OVER → restart. HUD (score, lives, level) via `canvas.text`.
- **Player:** sprite from `kingvalley.sprite`, 4-dir walk animation (`sprite.frame` per direction, `playing/speed`), right-facing via `flipH`; grid movement with `iskeydown` arrows/WASD **plus** `gamepadaxis("leftx"/"lefty")` + d-pad buttons (dogfoods 1c).
- **Mummies:** sprites from the atlas (left frames + `flipH`), move on the grid every ~300 ms (random walk; chase within 4 tiles).
- **Audio map:** jingles via `music.play` (existing), SFX via `sound.play("sfx/pickup.wav")` (new 1b) + `music.sound('Kick'/'HiHat'/'Explode')` (new 1a + existing).
- **Levels:** 3 hand-authored grids written after the tileset sheet is inspected (tile ids from `make_assets.py` output).

## Part 3 — Tests & docs

- `tests/test_sound_play.tyro` + `tests/test_gamepad.tyro` (+ result txts) per the `tests/` convention.
- `tyro --lint games/kingvalley/kingvalley.tyro` as the game's syntax gate.
- `tests/test_waveforms.lpr` auto-covers the drums.
- `README.md`: document `sound.play`, the gamepad globals, `sprite.flipH/flipV`, the new waveforms, and add `games/kingvalley/kingvalley.tyro` to the games index.
- No git commit unless asked (AGENTS.md).

## Execution order

1. Drums (1a) → build → run `test_waveforms.exe`.
2. `sound.play` (1b) → build → `test_sound_play.tyro`.
3. Gamepad (1c) → build → `test_gamepad.tyro`.
4. `flipH/flipV` (1d) → build.
5. Download assets → `make_assets.py` → `kingvalley.sprite` + `tiles.png` + `sfx/` + `CREDITS.txt`.
6. Write `kingvalley.tyro`, lint, run, iterate on feel.
7. README update.

## Caveats

- The desert sheet's exact layout (columns/spacing/tile ids) is unknown until downloaded; the game's tile table is written from the inspected PNG (the engine derives `columns` from texture width when passed 0).
- `SpriteAtlas.pas` JSON dialect is verified by reading it before writing the packer; the `demos/sprite_atlas_demo.tyro` + `tests/test_sprite_atlas` pair is the reference.
- If the OGA wav search yields no clean CC0 **.wav** (LoadSound reads wav only), the SFX wavs are synthesized with Python's stdlib `wave` module instead.
- `games/minerman/` assets stay untouched (future game).
