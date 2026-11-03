#!/usr/bin/env python3
"""
Bomberman - Asset Generator for Tyro Game Engine
Creates pixel-art sprites (.sprite atlases + .png) and the 32x32 tileset.
Zero external dependencies beyond Python standard library and Pillow.

Tiles (tiles.png, 4 cols x 1 row, 32x32):
  0 = grass floor      1 = solid wall (indestructible)
  2 = crate (destructible)             3 = void background
"""

import io
import json
import os
import random
import zipfile
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "img")
os.makedirs(IMG, exist_ok=True)

CLR_CLEAR = (0, 0, 0, 0)
CLR_WHITE = (255, 255, 255, 255)
CLR_BLACK = (12, 12, 16, 255)

# Bomberman palette
CLR_BOMB_WHITE = (244, 244, 248, 255)
CLR_BOMB_SHADE = (198, 202, 214, 255)
CLR_BOMB_BLUE = (48, 96, 210, 255)
CLR_BOMB_BLUE_D = (30, 62, 150, 255)
CLR_BOMB_BOOT = (222, 60, 48, 255)
CLR_BOMB_BOOT_D = (160, 36, 30, 255)
CLR_SKIN = (255, 214, 170, 255)

# Balom palette
CLR_BAL_BODY = (232, 76, 96, 255)
CLR_BAL_DARK = (168, 40, 66, 255)
CLR_BAL_LIGHT = (255, 140, 150, 255)
CLR_BAL_EYE = (255, 238, 120, 255)
CLR_BAL_FOOT = (250, 168, 60, 255)

# Bomb / fire palette
CLR_BOMB_BODY = (34, 34, 44, 255)
CLR_BOMB_HILT = (120, 90, 60, 255)
CLR_FIRE_0 = (255, 252, 214, 255)
CLR_FIRE_1 = (255, 196, 60, 255)
CLR_FIRE_2 = (255, 120, 30, 255)
CLR_FIRE_3 = (226, 60, 30, 255)

# Tile palette
CLR_GRASS = (72, 152, 74, 255)
CLR_GRASS_D = (58, 130, 62, 255)
CLR_GRASS_L = (90, 172, 88, 255)
CLR_STONE = (122, 132, 150, 255)
CLR_STONE_D = (78, 86, 104, 255)
CLR_STONE_L = (168, 178, 196, 255)
CLR_WOOD = (164, 108, 52, 255)
CLR_WOOD_D = (112, 68, 30, 255)
CLR_WOOD_L = (204, 148, 84, 255)
CLR_VOID = (18, 20, 30, 255)

def new_img(w, h):
    im = Image.new("RGBA", (w, h), CLR_CLEAR)
    return im, ImageDraw.Draw(im)

def save_sheet(frames, tw, th, name, duration=100):
    n = len(frames)
    atlas = Image.new("RGBA", (tw * n, th), CLR_CLEAR)
    for i, f in enumerate(frames):
        atlas.paste(f, (i * tw, 0))
    atlas_png_name = name + "_sheet.png"
    atlas.save(os.path.join(IMG, atlas_png_name))

    # Also save the single 1x frame as name.png so it can load instantly without atlas overhead
    frames[0].save(os.path.join(IMG, name + ".png"))

    desc = {"frames": {}, "meta": {"image": atlas_png_name, "size": {"w": tw * n, "h": th}}}
    for i in range(n):
        desc["frames"][f"f{i}"] = {
            "frame": {"x": i * tw, "y": 0, "w": tw, "h": th},
            "duration": duration,
        }
    path = os.path.join(IMG, name + ".sprite")
    buf = io.BytesIO()
    atlas.save(buf, format="PNG")
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr(atlas_png_name, buf.getvalue())
        z.writestr(name + ".json", json.dumps(desc))
    print(f"Generated {name}.sprite & .png ({n} frames, {tw}x{th})")

# -----------------------------------------------------------------------------
# 1. Tileset (tiles.png, 32x32 x 4)
# -----------------------------------------------------------------------------
def build_tiles():
    rng = random.Random(1985)  # deterministic grass noise
    atlas = Image.new("RGBA", (32 * 4, 32), CLR_CLEAR)
    d = ImageDraw.Draw(atlas)

    # Tile 0: grass floor, soft checker with a little noise
    for y in range(32):
        for x in range(32):
            base = CLR_GRASS
            if (x // 8 + y // 8) % 2 == 1:
                base = CLR_GRASS_D
            if rng.random() < 0.05:
                base = CLR_GRASS_L
            elif rng.random() < 0.05:
                base = CLR_GRASS_D
            atlas.putpixel((x, y), base)

    # Tile 1: solid stone wall, beveled brick block
    ox = 32
    d.rectangle((ox + 1, 1, ox + 30, 30), fill=CLR_STONE_D)
    d.rectangle((ox + 2, 2, ox + 29, 28), fill=CLR_STONE)
    d.rectangle((ox + 2, 2, ox + 29, 5), fill=CLR_STONE_L)      # top light
    d.rectangle((ox + 2, 2, ox + 4, 28), fill=CLR_STONE_L)      # left light
    d.rectangle((ox + 2, 25, ox + 29, 28), fill=CLR_STONE_D)    # bottom shade
    d.rectangle((ox + 26, 5, ox + 29, 28), fill=CLR_STONE_D)    # right shade
    # mortar seams for a chunky block look
    d.rectangle((ox + 2, 14, ox + 29, 15), fill=CLR_STONE_D)
    d.rectangle((ox + 15, 2, ox + 16, 14), fill=CLR_STONE_D)
    d.rectangle((ox + 8, 15, ox + 9, 28), fill=CLR_STONE_D)
    d.rectangle((ox + 22, 15, ox + 23, 28), fill=CLR_STONE_D)

    # Tile 2: destructible wooden crate
    ox = 64
    d.rectangle((ox + 2, 3, ox + 29, 30), fill=CLR_WOOD_D)
    d.rectangle((ox + 3, 4, ox + 28, 29), fill=CLR_WOOD)
    d.rectangle((ox + 3, 4, ox + 28, 6), fill=CLR_WOOD_L)       # top light
    d.rectangle((ox + 3, 26, ox + 28, 29), fill=CLR_WOOD_D)     # bottom shade
    # planks + cross braces
    d.rectangle((ox + 3, 12, ox + 28, 13), fill=CLR_WOOD_D)
    d.rectangle((ox + 3, 21, ox + 28, 22), fill=CLR_WOOD_D)
    d.line((ox + 5, 6, ox + 26, 27), fill=CLR_WOOD_D, width=2)
    d.line((ox + 26, 6, ox + 5, 27), fill=CLR_WOOD_D, width=2)
    d.line((ox + 5, 5, ox + 26, 26), fill=CLR_WOOD_L, width=1)
    # corner nails
    for px, py in ((ox + 5, 7), (ox + 26, 7), (ox + 5, 27), (ox + 26, 27)):
        d.point((px, py), fill=CLR_WHITE)

    # Tile 3: void background
    ox = 96
    d.rectangle((ox, 0, ox + 31, 31), fill=CLR_VOID)
    d.rectangle((ox, 0, ox + 31, 31), outline=(28, 32, 46, 255))

    atlas.save(os.path.join(IMG, "tiles.png"))
    print("Generated tiles.png (4 tiles, 32x32)")

# -----------------------------------------------------------------------------
# 2. Player: White Bomberman (bomber.sprite, 4-frame walk cycle)
# -----------------------------------------------------------------------------
def make_bomber_frame(step):
    im, d = new_img(32, 32)
    bob = [0, -1, 0, -1][step]
    foot_l = [-2, 0, 2, 0][step]
    foot_r = [2, 0, -2, 0][step]

    # Shadow
    d.ellipse((8, 27, 24, 31), fill=(10, 12, 18, 90))

    # Red boots
    d.rounded_rectangle((9 + foot_l, 25, 14 + foot_l, 30), 2, fill=CLR_BOMB_BOOT_D)
    d.rounded_rectangle((9 + foot_l, 24 + 0, 14 + foot_l, 28), 2, fill=CLR_BOMB_BOOT)
    d.rounded_rectangle((18 + foot_r, 25, 23 + foot_r, 30), 2, fill=CLR_BOMB_BOOT_D)
    d.rounded_rectangle((18 + foot_r, 24, 23 + foot_r, 28), 2, fill=CLR_BOMB_BOOT)

    # Blue suit body
    yb = 14 + bob
    d.rounded_rectangle((9, yb, 23, yb + 11), 4, fill=CLR_BOMB_BLUE_D)
    d.rounded_rectangle((10, yb, 22, yb + 10), 4, fill=CLR_BOMB_BLUE)
    d.rectangle((12, yb + 3, 20, yb + 4), fill=CLR_BOMB_BLUE_D)  # belt line
    # White gloves / arms
    d.ellipse((5, yb + 2, 10, yb + 7), fill=CLR_BOMB_SHADE)
    d.ellipse((6, yb + 2, 10, yb + 6), fill=CLR_BOMB_WHITE)
    d.ellipse((22, yb + 2, 27, yb + 7), fill=CLR_BOMB_SHADE)
    d.ellipse((22, yb + 2, 26, yb + 6), fill=CLR_BOMB_WHITE)

    # Round white helmet head
    yh = 2 + bob
    d.ellipse((7, yh, 25, yh + 13), fill=CLR_BOMB_SHADE)
    d.ellipse((8, yh, 24, yh + 12), fill=CLR_BOMB_WHITE)
    d.arc((9, yh + 1, 23, yh + 11), 200, 290, fill=CLR_WHITE, width=2)  # helmet shine
    # Antenna
    d.rectangle((15, yh - 3, 16, yh + 1), fill=CLR_BOMB_SHADE)
    d.ellipse((14, yh - 5, 18, yh - 1), fill=CLR_BOMB_BLUE)
    # Face
    d.ellipse((11, yh + 5, 13, yh + 8), fill=CLR_BLACK)
    d.ellipse((18, yh + 5, 20, yh + 8), fill=CLR_BLACK)
    d.point((12, yh + 6), fill=CLR_WHITE)
    d.point((19, yh + 6), fill=CLR_WHITE)
    # Rosy cheeks + smile
    d.rectangle((10, yh + 9, 12, yh + 10), fill=(255, 160, 150, 180))
    d.rectangle((19, yh + 9, 21, yh + 10), fill=(255, 160, 150, 180))
    d.arc((13, yh + 8, 19, yh + 12), 20, 160, fill=CLR_BLACK, width=1)
    return im

def build_bomber():
    frames = [make_bomber_frame(i) for i in range(4)]
    save_sheet(frames, 32, 32, "bomber", duration=130)

# -----------------------------------------------------------------------------
# 3. Enemy: Balom balloon monster (balom.sprite, 4-frame hover bob)
# -----------------------------------------------------------------------------
def make_balom_frame(step):
    im, d = new_img(32, 32)
    bob = [0, -2, -3, -1][step]
    squish = [0, 1, 1, 0][step]

    # Shadow
    d.ellipse((9, 28, 23, 31), fill=(10, 12, 18, 70))

    # Orange feet dangling under the balloon
    d.rectangle((11, 25 + bob, 13, 28 + bob), fill=CLR_BAL_FOOT)
    d.rectangle((18, 25 + bob, 20, 28 + bob), fill=CLR_BAL_FOOT)
    d.ellipse((10, 27 + bob, 14, 29 + bob), fill=CLR_BAL_FOOT)
    d.ellipse((17, 27 + bob, 21, 29 + bob), fill=CLR_BAL_FOOT)

    # Balloon body
    d.ellipse((5 - squish, 3 + bob, 27 + squish, 26 + bob), fill=CLR_BAL_DARK)
    d.ellipse((6 - squish, 4 + bob, 26 + squish, 25 + bob), fill=CLR_BAL_BODY)
    # Glossy highlight
    d.arc((8, 6 + bob, 22, 20 + bob), 200, 280, fill=CLR_BAL_LIGHT, width=2)

    # Four googly eyes (classic two rows)
    for ex, ey in ((10, 10), (17, 9), (9, 16), (17, 16)):
        d.ellipse((ex, ey + bob, ex + 5, ey + 5 + bob), fill=CLR_WHITE)
        d.ellipse((ex + 2, ey + 2 + bob, ex + 4, ey + 4 + bob), fill=CLR_BLACK)
    # Grumpy mouth
    d.arc((11, 20 + bob, 19, 24 + bob), 180, 340, fill=CLR_BAL_DARK, width=2)
    return im

def build_balom():
    frames = [make_balom_frame(i) for i in range(4)]
    save_sheet(frames, 32, 32, "balom", duration=160)

# -----------------------------------------------------------------------------
# 4. Bomb (bomb.sprite, 4-frame pulse with growing fuse spark)
# -----------------------------------------------------------------------------
def make_bomb_frame(step):
    im, d = new_img(32, 32)
    grow = [0, 0, 1, 1][step]  # final tick swells right before it blows
    cx, cy, r = 16, 20 - grow, 10 + grow

    # Shadow
    d.ellipse((8, 28, 24, 31), fill=(10, 12, 18, 80))
    # Fuse stump
    d.rectangle((14, 6 - grow, 17, 10 - grow), fill=CLR_BOMB_HILT)
    # Spark on the fuse
    if step == 0:
        d.point((15, 5 - grow), fill=CLR_FIRE_1)
    elif step == 1:
        d.ellipse((13, 3 - grow, 18, 7 - grow), fill=CLR_FIRE_1)
    elif step == 2:
        d.ellipse((12, 2 - grow, 19, 8 - grow), fill=CLR_FIRE_2)
        d.ellipse((14, 3 - grow, 17, 6 - grow), fill=CLR_FIRE_0)
    else:
        d.ellipse((11, 1 - grow, 20, 9 - grow), fill=CLR_FIRE_2)
        d.ellipse((13, 2 - grow, 18, 7 - grow), fill=CLR_FIRE_0)

    # Black round body
    d.ellipse((cx - r, cy - r, cx + r, cy + r), fill=(10, 10, 14, 255))
    d.ellipse((cx - r + 1, cy - r + 1, cx + r - 1, cy + r - 1), fill=CLR_BOMB_BODY)
    # Shine
    d.arc((cx - r + 2, cy - r + 2, cx + 2, cy + 2), 200, 270, fill=(120, 124, 150, 255), width=2)
    d.point((cx - 4, cy - 5), fill=CLR_WHITE)
    return im

def build_bomb():
    frames = [make_bomb_frame(i) for i in range(4)]
    save_sheet(frames, 32, 32, "bomb", duration=90)

# -----------------------------------------------------------------------------
# 5. Explosion flame pieces (flame_center / flame_arm / flame_tip, 4 frames)
# -----------------------------------------------------------------------------
FIRE_COLORS = [CLR_FIRE_0, CLR_FIRE_1, CLR_FIRE_2, CLR_FIRE_3]

def blob(d, cx, cy, r, color, halo):
    d.ellipse((cx - r - 2, cy - r - 2, cx + r + 2, cy + r + 2), fill=halo)
    d.ellipse((cx - r, cy - r, cx + r, cy + r), fill=color)

def make_flame_frame(step, kind):
    im, d = new_img(32, 32)
    c = FIRE_COLORS[step]
    halo = (255, 90, 20, 120)
    wob = [0, 1, 2, 1][step]
    if kind == "center":
        blob(d, 16, 16, 13 - wob // 2, c, halo)
        for ang_x, ang_y in ((26, 16), (6, 16), (16, 26), (16, 6)):
            blob(d, ang_x, ang_y, 7 + wob, c, halo)
    elif kind == "arm":
        blob(d, 16, 16, 14 - wob // 2, c, halo)
        blob(d, 16, 4 if step % 2 else 28, 5 + wob, c, halo)
    else:  # tip
        blob(d, 16, 16, 9 + wob, c, halo)
        blob(d, 16, 10 if step % 2 else 22, 4 + wob, c, halo)
    # Hot core
    d.ellipse((13, 13, 19, 19), fill=(255, 255, 235, 230) if step < 2 else CLR_FIRE_0)
    return im

def build_flames():
    for kind in ("center", "arm", "tip"):
        frames = [make_flame_frame(i, kind) for i in range(4)]
        save_sheet(frames, 32, 32, "flame_" + kind, duration=70)

# -----------------------------------------------------------------------------
# 6. Power-ups (powerup.sprite, 4 frames: flame / bomb / speed / shield)
# -----------------------------------------------------------------------------
def build_powerups():
    # Frame 0: FLAMEUP - orange flame badge
    im, d = new_img(32, 32)
    d.ellipse((3, 3, 29, 29), fill=(150, 50, 20, 255))
    d.ellipse((4, 4, 28, 28), fill=(90, 30, 14, 255))
    d.polygon([(16, 6), (23, 17), (20, 15), (22, 24), (16, 20), (11, 25), (13, 15), (10, 17)],
              fill=CLR_FIRE_2)
    d.polygon([(16, 11), (20, 18), (16, 16), (13, 19)], fill=CLR_FIRE_0)
    f0 = im
    # Frame 1: BOMBUP - extra bomb badge
    im, d = new_img(32, 32)
    d.ellipse((3, 3, 29, 29), fill=(40, 40, 60, 255))
    d.ellipse((4, 4, 28, 28), fill=(22, 22, 34, 255))
    d.rectangle((15, 5, 17, 10), fill=CLR_BOMB_HILT)
    d.point((16, 4), fill=CLR_FIRE_1)
    d.ellipse((8, 9, 24, 25), fill=(10, 10, 14, 255))
    d.ellipse((9, 10, 23, 24), fill=CLR_BOMB_BODY)
    d.point((12, 14), fill=CLR_WHITE)
    f1 = im
    # Frame 2: SPEEDUP - yellow boot chevrons
    im, d = new_img(32, 32)
    d.ellipse((3, 3, 29, 29), fill=(150, 120, 20, 255))
    d.ellipse((4, 4, 28, 28), fill=(80, 62, 12, 255))
    for i, xo in enumerate((4, 12, 20)):
        shade = CLR_FIRE_0 if i == 2 else CLR_FIRE_1
        d.polygon([(xo, 8), (xo + 7, 16), (xo, 24), (xo + 3, 16)], fill=shade)
    f2 = im
    # Frame 3: SHIELD - blue shield badge
    im, d = new_img(32, 32)
    d.ellipse((3, 3, 29, 29), fill=(20, 90, 150, 255))
    d.ellipse((4, 4, 28, 28), fill=(12, 52, 92, 255))
    d.polygon([(16, 6), (25, 11), (23, 21), (16, 27), (9, 21), (7, 11)], fill=(70, 160, 230, 255))
    d.polygon([(16, 9), (22, 12), (21, 20), (16, 24), (11, 20), (10, 12)], fill=(140, 210, 255, 255))
    d.rectangle((15, 12, 17, 19), fill=(230, 246, 255, 255))
    f3 = im

    save_sheet([f0, f1, f2, f3], 32, 32, "powerup", duration=0)

# -----------------------------------------------------------------------------
# 7. Exit door (door.sprite, 2 frames: closed / open)
# -----------------------------------------------------------------------------
def build_door():
    # Closed: brick archway with a shut metal door
    im, d = new_img(32, 32)
    d.rectangle((2, 2, 30, 31), fill=CLR_STONE_D)
    d.rectangle((3, 3, 29, 30), fill=CLR_STONE)
    d.rectangle((3, 3, 29, 5), fill=CLR_STONE_L)
    d.rectangle((8, 8, 24, 31), fill=(40, 30, 24, 255))
    d.rounded_rectangle((9, 9, 23, 31), 3, fill=(96, 70, 46, 255))
    d.rectangle((9, 14, 23, 15), fill=(60, 42, 28, 255))
    d.rectangle((9, 23, 23, 24), fill=(60, 42, 28, 255))
    d.ellipse((18, 17, 21, 20), fill=(240, 200, 90, 255))  # knob
    f0 = im
    # Open: dark passage with a glowing rim
    im, d = new_img(32, 32)
    d.rectangle((2, 2, 30, 31), fill=CLR_STONE_D)
    d.rectangle((3, 3, 29, 30), fill=CLR_STONE)
    d.rectangle((3, 3, 29, 5), fill=CLR_STONE_L)
    d.rectangle((8, 8, 24, 31), fill=(8, 8, 14, 255))
    d.rounded_rectangle((9, 9, 23, 31), 3, fill=(6, 6, 10, 255))
    d.arc((9, 9, 23, 31), 180, 360, fill=(120, 220, 255, 255), width=2)
    d.ellipse((13, 18, 19, 24), fill=(60, 140, 200, 90))
    f1 = im
    save_sheet([f0, f1], 32, 32, "door", duration=0)

if __name__ == "__main__":
    build_tiles()
    build_bomber()
    build_balom()
    build_bomb()
    build_flames()
    build_powerups()
    build_door()
    print("Done. All assets written to img/")
