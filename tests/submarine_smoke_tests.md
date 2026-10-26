# Submarine game smoke tests

The game lives in `games/submarine/`. All commands run from the repo root
with `bin/tyro.exe`. Screenshots land as `screenshot.png` inside the
script's folder (the engine forces WorkPath to the script directory).

## 1. Asset / sound / shader smoke test

```
bin\tyro.exe tests\test_submarine_assets.tyro --console --exit:10 --screenshot:7
```

Expected: no Lua errors, `games/...` note — the script loads every
submarine atlas (`games/submarine/img/*.sprite`), plays the generated
WAVs and compiles `underwater.frag`. The screenshot should show the sub,
shark, jellyfish, mine, patrol sub, torpedo, a boom frame and the tinted
water background.

## 2. Title screen + no-runtime-error check

```
bin\tyro.exe games\submarine\submarine.tyro --console --exit:8 --screenshot:6
```

Expected: title screen with sub, sand, HUD (SCORE / HI / LEVEL / O2 bar /
DIVERS slots). Any runtime error aborts drawing — the picture becomes a
flat or black frame, which is the failure signal.

## 3. Gameplay / attract-mode test

Create the attract hook file, let the autopilot play, then inspect:

```
type nul > games\submarine\attract.txt
bin\tyro.exe games\submarine\submarine.tyro --console --exit:170 --screenshot:130
del games\submarine\attract.txt games\submarine\screenshot.png
```

Expected (screenshot at 130 s): the autopilot is two or three levels in —
carried divers trail the sub, jellies / mines / patrol appear from their
level gates, the four palettes cycle. The game must survive the whole
run with no restart loops of the same frame garbage (an occasional demo
game-over → title → demo restart is normal).

## Notes

- `--console` writes Lua errors to a dedicated console window; when the
  output pipeline looks silent, double-check the screenshot: a frame
  that stops mid-draw means a runtime error in update()/draw().
- Regenerate assets with `python games/submarine/make_assets.py` if the
  `img/` or `sfx/` folders were emptied (Pillow required).
