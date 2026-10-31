"""Regenerate the King Valley game assets from the downloaded packs in res/.

Run from the repo root:  python games/kingvalley/make_assets.py
Requires Pillow. Produces next to this script:
  player.sprite   Egyptian male walk frames (down/left/up rows; the game
                  mirrors the left frames via sprite.flipH for right-facing,
                  so the right row of the sheet is intentionally not packed)
  mummy.sprite    Mummy walk frames, rows N/E/S/W x 3 frames
  tiles.png       the 16x16 Grand Desert tileset (15-column sheet)
  sfx/pickup.wav  treasure pickup blip (synthesized, CC0)
  sfx/hit.wav     damage thud (synthesized, CC0)
  CREDITS.txt     source + license of every asset

A .sprite file is a ZIP holding one packed PNG plus a TexturePacker/Phaser
"JSON Hash" descriptor (see src/tyrolib/SpriteAtlas.pas).  All frames in one
.sprite form one animation, so each animated character gets its own file.
"""
import io
import json
import math
import os
import shutil
import struct
import wave
import zipfile

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
RES = os.path.join(HERE, "res")

PLAYER_SHEET = os.path.join(RES, "player-sheet.png")
MUMMY_SHEET = os.path.join(RES, "mummy", "PNG", "24x32", "mummy-01.png")
DESERT_SHEET = os.path.join(RES, "desert", "Cute Retro RPG - Grand Desert Demo",
                            "Tilesets.png")


def build_sprite(path, image_name, atlas, frames):
    """Write one .sprite: ZIP(atlas PNG + TexturePacker hash JSON).

    frames is a list of (name, x, y, w, h, duration_ms); the JSON key order
    defines the engine's frame indices 0..n-1.
    """
    buf = io.BytesIO()
    atlas.save(buf, "PNG")
    doc = {
        "frames": {},
        "meta": {
            "image": image_name,
            "format": "RGBA8888",
            "size": {"w": atlas.width, "h": atlas.height},
            "scale": "1",
        },
    }
    for name, x, y, w, h, dur in frames:
        doc["frames"][name] = {
            "frame": {"x": x, "y": y, "w": w, "h": h},
            "rotated": False,
            "trimmed": False,
            "sourceSize": {"w": w, "h": h},
            "spriteSourceSize": {"x": 0, "y": 0},
            "duration": dur,
        }
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr(image_name, buf.getvalue())
        z.writestr(os.path.splitext(image_name)[0] + ".json",
                   json.dumps(doc, separators=(",", ":")))


def player_sprite():
    """Pack the Egyptian male sheet (96x120, 4 cols x 5 rows of 24x24).

    Sheet rows (verified by alpha/palette analysis):
      row 0 walk down (front), row 1 walk right, row 2 walk up (back),
      row 3 walk left, row 4 extra poses.
    The game draws right-facing with the left frames + flipH (Part 1d), so
    only rows 0 (down), 3 (left) and 2 (up) are packed, in that order:
    frames 0-3 down, 4-7 left, 8-11 up.
    """
    sheet = Image.open(PLAYER_SHEET).convert("RGBA")
    rows = [("down", 0), ("left", 3), ("up", 2)]
    atlas = Image.new("RGBA", (96, 72), (0, 0, 0, 0))
    frames = []
    for i, (label, row) in enumerate(rows):
        for col in range(4):
            cell = sheet.crop((col * 24, row * 24, col * 24 + 24, row * 24 + 24))
            ax, ay = (i % 4) * 24, (i // 4) * 24
            atlas.paste(cell, (ax, ay))
            frames.append(("%s%d.png" % (label, col + 1), ax, ay, 24, 24, 120))
    build_sprite(os.path.join(HERE, "player.sprite"), "player.png", atlas, frames)


def mummy_sprite():
    """Pack mummy-01.png (72x128, 3 cols x 4 rows of 24x32, rows N/E/S/W)."""
    sheet = Image.open(MUMMY_SHEET).convert("RGBA")
    dirs = ["n", "e", "s", "w"]
    atlas = Image.new("RGBA", (72, 128), (0, 0, 0, 0))
    frames = []
    for row in range(4):
        for col in range(3):
            cell = sheet.crop((col * 24, row * 32, col * 24 + 24, row * 32 + 32))
            atlas.paste(cell, (col * 24, row * 32))
            frames.append(("%s%d.png" % (dirs[row], col + 1), col * 24, row * 32,
                           24, 32, 150))
    build_sprite(os.path.join(HERE, "mummy.sprite"), "mummy.png", atlas, frames)


def make_wav(path, samples):
    with wave.open(path, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(44100)
        w.writeframes(b"".join(struct.pack("<h", s) for s in samples))


def _lcg_noise(seed, count):
    """Deterministic pseudo-noise so the wavs are byte-reproducible."""
    x = seed
    for _ in range(count):
        x = (1103515245 * x + 12345) & 0x7FFFFFFF
        yield (x / 0x7FFFFFFF) * 2.0 - 1.0


def sfx_wavs():
    sr = 44100
    sfx = os.path.join(HERE, "sfx")
    if not os.path.isdir(sfx):
        os.mkdir(sfx)

    # pickup: two-tone rising blip with a fast decay (classic treasure ping)
    samples = []
    for freq, dur in ((660.0, 0.07), (990.0, 0.14)):
        n = int(sr * dur)
        for i in range(n):
            t = i / sr
            env = math.exp(-t / (dur * 0.5))
            samples.append(int(32767 * 0.45 * env * math.sin(2 * math.pi * freq * t)))
    make_wav(os.path.join(sfx, "pickup.wav"), samples)

    # hit: noise burst over a low decaying thud
    n = int(sr * 0.18)
    samples = []
    for i, noise in enumerate(_lcg_noise(12345, n)):
        t = i / sr
        s = noise * 0.55 * math.exp(-t / 0.05) + 0.65 * math.sin(2 * math.pi * 110 * t) * math.exp(-t / 0.08)
        samples.append(int(32767 * max(-1.0, min(1.0, s))))
    make_wav(os.path.join(sfx, "hit.wav"), samples)


CREDITS = """King Valley - asset credits
===========================

Player (Egyptian male walk frames, 24x24)
  "Egyptian RPG" by ZomBCool - OpenGameArt.org
  https://opengameart.org/content/egyptian-rpg
  License: CC0 1.0 Universal
  File: res/player-sheet.png (rows down/left/up packed into player.sprite;
  the right-facing row is replaced at runtime by sprite.flipH of the left
  frames, so it is not packed).

Mummy (enemy walk frames, 24x32, rows N/E/S/W x 3)
  "Mummy" v1.2 by Svetlana Kushnariova (Cabbit) & Jordan Irwin (AntumDeluge)
  https://opengameart.org/node/101157
  Licenses: CC-BY 3.0 / CC-BY 4.0 / OGA-BY 3.0 (attribution required)
  File: res/mummy/PNG/24x32/mummy-01.png packed into mummy.sprite.
  (mummy-02.png is the v1.1 recolor and is not used.)

Tiles (16x16 top-down desert set, 15-column sheet)
  "Cute Retro RPG - Grand Desert Demo" by 16Pixel
  https://16pixel.itch.io/cute-retro-rpg-grand-desert
  https://opengameart.org/content/16x16-desert-tilesets
  License: CC-BY-SA 4.0
  File: res/desert/.../Tilesets.png copied to tiles.png.

Music (5 jingles)
  Kenney Audio - https://kenney.nl/assets (Kenney Audio bundle)
  License: CC0 1.0 Universal
  Files: 01 Game Start.mp3 ... 05 Game Over.mp3 (kept in the game folder).

Sound effects
  sfx/pickup.wav and sfx/hit.wav synthesized with Python's stdlib wave
  module by make_assets.py (CC0, generated).
"""


def main():
    player_sprite()
    mummy_sprite()
    shutil.copyfile(DESERT_SHEET, os.path.join(HERE, "tiles.png"))
    sfx_wavs()
    with open(os.path.join(HERE, "CREDITS.txt"), "w", newline="\n") as f:
        f.write(CREDITS)
    print("assets written to", HERE)


if __name__ == "__main__":
    main()
