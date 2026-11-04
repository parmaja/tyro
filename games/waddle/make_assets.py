#!/usr/bin/env python3
"""
Waddle - Asset Generator for Tyro Game Engine
Creates the pixel-art crowd, the terrain tileset, the props and the skill
badges. Zero external dependencies beyond Python standard library and Pillow.

Terrain tiles (img/tiles.png, 4 cols x 1 row, 32x32):
  id 1 = dirt with grass top   id 2 = plain dirt
  id 3 = rock (never diggable) id 4 = sandstone (bash only)

The waddle itself is one 24x24 atlas; the six skills are told apart by a
16x16 badge drawn next to the creature plus a per-skill tint, so one body
serves the whole crowd (see waddle.tyro SKILL_* tables).
"""

import io
import json
import os
import zipfile
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "img")
os.makedirs(IMG, exist_ok=True)

CLR_CLEAR = (0, 0, 0, 0)
CLR_WHITE = (255, 255, 255, 255)
CLR_BLACK = (16, 16, 24, 255)
CLR_OUTLINE = (26, 26, 44, 255)

# Waddle palette (a friendly blue blob; skills tint + badge tell them apart)
CLR_BODY = (72, 168, 226, 255)
CLR_BODY_D = (44, 118, 178, 255)
CLR_BODY_L = (146, 214, 246, 255)
CLR_FACE = (252, 236, 208, 255)
CLR_FACE_D = (226, 200, 164, 255)
CLR_SCARF = (242, 110, 86, 255)
CLR_SCARF_D = (186, 66, 52, 255)
CLR_BOOT = (58, 52, 74, 255)

# Terrain palette
CLR_GRASS = (104, 190, 92, 255)
CLR_GRASS_D = (72, 148, 68, 255)
CLR_GRASS_L = (148, 220, 124, 255)
CLR_DIRT = (146, 102, 66, 255)
CLR_DIRT_D = (108, 72, 46, 255)
CLR_DIRT_L = (184, 138, 96, 255)
CLR_ROCK = (124, 132, 152, 255)
CLR_ROCK_D = (80, 88, 108, 255)
CLR_ROCK_L = (172, 182, 200, 255)
CLR_SAND = (214, 184, 122, 255)
CLR_SAND_D = (172, 142, 88, 255)
CLR_SAND_L = (240, 220, 170, 255)

# Prop palette
CLR_WOOD = (172, 116, 60, 255)
CLR_WOOD_D = (118, 74, 36, 255)
CLR_WOOD_L = (214, 160, 96, 255)
CLR_BALLOON = (240, 96, 112, 255)
CLR_BALLOON_D = (178, 52, 72, 255)
CLR_BALLOON_L = (255, 168, 178, 255)
CLR_STONE = (152, 160, 178, 255)
CLR_STONE_D = (96, 104, 124, 255)
CLR_STONE_L = (198, 206, 224, 255)

# Effect palette
CLR_FIRE_0 = (255, 250, 216, 255)
CLR_FIRE_1 = (255, 200, 84, 255)
CLR_FIRE_2 = (255, 132, 46, 255)
CLR_MIST = (176, 226, 250, 255)


def new_img(w, h):
    im = Image.new("RGBA", (w, h), CLR_CLEAR)
    return im, ImageDraw.Draw(im)


def save_sheet(frames, tw, th, name, duration=100):
    """Write img/<name>_sheet.png + img/<name>.sprite (zip: PNG + JSON atlas)."""
    n = len(frames)
    atlas = Image.new("RGBA", (tw * n, th), CLR_CLEAR)
    for i, f in enumerate(frames):
        atlas.paste(f, (i * tw, 0))
    atlas_png_name = name + "_sheet.png"
    atlas.save(os.path.join(IMG, atlas_png_name))

    # First frame alone, so a static prop loads without atlas overhead
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
# 1. Terrain tileset (tiles.png, 32x32 x 4)
# -----------------------------------------------------------------------------
def build_tiles():
    from random import Random

    atlas = Image.new("RGBA", (32 * 4, 32), CLR_CLEAR)
    d = ImageDraw.Draw(atlas)

    def speckle(ox, base, dark, light, seed, chance=0.09):
        rng = Random(seed)
        for y in range(32):
            for x in range(32):
                c = base
                r = rng.random()
                if r < chance:
                    c = dark
                elif r < chance * 2:
                    c = light
                atlas.putpixel((ox + x, y), c)

    # Tile 1: dirt with a grass top edge
    speckle(0, CLR_DIRT, CLR_DIRT_D, CLR_DIRT_L, 11)
    d.rectangle((0, 0, 31, 6), fill=CLR_GRASS)
    d.rectangle((0, 0, 31, 2), fill=CLR_GRASS_L)
    d.rectangle((0, 5, 31, 6), fill=CLR_GRASS_D)
    for x in range(0, 32, 4):          # grass tufts biting into the dirt
        d.rectangle((x + 1, 7, x + 2, 8), fill=CLR_GRASS_D)
    d.rectangle((0, 0, 31, 0), fill=CLR_GRASS_L)

    # Tile 2: plain dirt
    speckle(32, CLR_DIRT, CLR_DIRT_D, CLR_DIRT_L, 22)
    d.rectangle((32, 0, 63, 31), outline=CLR_DIRT_D)

    # Tile 3: rock - hard, bevelled, never diggable
    speckle(64, CLR_ROCK, CLR_ROCK_D, CLR_ROCK_L, 33, 0.06)
    d.rectangle((64, 0, 95, 31), outline=CLR_ROCK_D)
    d.line((66, 4, 92, 9), fill=CLR_ROCK_L, width=2)
    d.line((68, 22, 90, 27), fill=CLR_ROCK_D, width=2)
    d.rectangle((64, 0, 95, 1), fill=CLR_ROCK_L)

    # Tile 4: sandstone - layered, only a basher breaks it
    speckle(96, CLR_SAND, CLR_SAND_D, CLR_SAND_L, 44, 0.07)
    for y in (6, 15, 24):
        d.rectangle((96, y, 127, y + 1), fill=CLR_SAND_D)
    d.rectangle((96, 0, 127, 0), fill=CLR_SAND_L)
    d.rectangle((96, 31, 127, 31), fill=CLR_SAND_D)

    atlas.save(os.path.join(IMG, "tiles.png"))
    print("Generated tiles.png (4 tiles, 32x32)")


# -----------------------------------------------------------------------------
# 2. The waddle (waddle.sprite, 24x24: 4 walk + fall + block)
# -----------------------------------------------------------------------------
def body(d, bob, squash=0):
    """Round blue blob body with boots, scarf and face. squashed = landing."""
    top = 3 + bob + squash
    bot = 18 + bob - squash
    d.ellipse((4 - squash, top, 20 + squash, bot), fill=CLR_BODY_D)
    d.ellipse((5 - squash, top + 1, 19 + squash, bot - 1), fill=CLR_BODY)
    d.arc((7 - squash, top + 2, 15, top + 9), 200, 285, fill=CLR_BODY_L, width=2)

    # Scarf around the neck - the crowd's colour accent
    d.rectangle((5, bot - 4, 19, bot - 1), fill=CLR_SCARF_D)
    d.rectangle((5, bot - 5, 19, bot - 3), fill=CLR_SCARF)
    d.polygon([(17, bot - 4), (22, bot - 1), (18, bot + 1)], fill=CLR_SCARF_D)

    # Face plate, eyes and shine
    d.ellipse((7, top + 6, 17, top + 14), fill=CLR_FACE_D)
    d.ellipse((8, top + 6, 17, top + 13), fill=CLR_FACE)
    d.ellipse((9, top + 7, 11, top + 11), fill=CLR_BLACK)
    d.ellipse((13, top + 7, 15, top + 11), fill=CLR_BLACK)
    d.point((10, top + 8), fill=CLR_WHITE)
    d.point((14, top + 8), fill=CLR_WHITE)
    return top, bot


def boots(d, bob, left, right):
    d.rounded_rectangle((5 + left, 19 + bob, 10 + left, 22 + bob), 2, fill=CLR_BOOT)
    d.rounded_rectangle((13 + right, 19 + bob, 18 + right, 22 + bob), 2, fill=CLR_BOOT)


def make_walk_frame(step):
    im, d = new_img(24, 24)
    bob = [0, -1, 0, -1][step]
    left = [-2, 0, 2, 0][step]
    right = [2, 0, -2, 0][step]
    d.ellipse((6, 21, 18, 23), fill=(12, 14, 22, 70))     # shadow
    boots(d, bob, left, right)
    body(d, bob)
    return im


def make_fall_frame(step):
    im, d = new_img(24, 24)
    flap = [0, 1, 0, -1][step]
    d.ellipse((6, 21, 18, 23), fill=(12, 14, 22, 50))
    # Feet splayed out, arms up: a startled waddle
    d.rounded_rectangle((3, 18 + flap, 8, 21 + flap), 2, fill=CLR_BOOT)
    d.rounded_rectangle((15, 18 - flap, 20, 21 - flap), 2, fill=CLR_BOOT)
    top, bot = body(d, 1, -1)                              # stretched downward
    # Wide "!" mouth
    d.ellipse((10, top + 11, 14, top + 14), fill=CLR_BLACK)
    d.ellipse((11, top + 12, 13, top + 13), fill=(90, 40, 40, 255))
    return im


def make_block_frame():
    im, d = new_img(24, 24)
    d.ellipse((4, 21, 20, 24), fill=(12, 14, 22, 80))
    boots(d, 0, -3, 3)
    body(d, 1, 1)                                          # squat and solid
    # Braced arms
    d.rectangle((1, 12, 5, 15), fill=CLR_BODY_D)
    d.rectangle((19, 12, 23, 15), fill=CLR_BODY_D)
    # Determined brows
    d.line((9, 9, 11, 10), fill=CLR_BLACK, width=1)
    d.line((15, 9, 13, 10), fill=CLR_BLACK, width=1)
    return im


def build_waddle():
    frames = [make_walk_frame(i) for i in range(4)]
    frames.append(make_fall_frame(0))
    frames.append(make_block_frame())
    save_sheet(frames, 24, 24, "waddle", duration=140)


# -----------------------------------------------------------------------------
# 3. Skill badges (badge.sprite, 16x16: none/dig/build/block/bash/bomb/floater)
# -----------------------------------------------------------------------------
def badge_base(d, rim, fill):
    d.rounded_rectangle((1, 1, 14, 14), 4, fill=rim)
    d.rounded_rectangle((2, 2, 13, 13), 3, fill=fill)


def build_badges():
    frames = []

    # 0: none - plain plate, the crowd colour
    im, d = new_img(16, 16)
    badge_base(d, (34, 42, 58, 255), (66, 78, 100, 255))
    frames.append(im)

    # 1: dig - spade
    im, d = new_img(16, 16)
    badge_base(d, (98, 62, 28, 255), (154, 102, 50, 255))
    d.rectangle((7, 3, 8, 8), fill=CLR_WOOD_L)
    d.polygon([(4, 8), (11, 8), (9, 13), (6, 13)], fill=CLR_STONE_L)
    d.rectangle((6, 8, 9, 9), fill=CLR_STONE)
    frames.append(im)

    # 2: build - three planks
    im, d = new_img(16, 16)
    badge_base(d, (92, 60, 26, 255), (142, 96, 46, 255))
    for i, y in enumerate((4, 7, 10)):
        d.rectangle((3 + i, y, 12 - i, y + 2), fill=CLR_WOOD_L)
        d.rectangle((3 + i, y + 2, 12 - i, y + 2), fill=CLR_WOOD_D)
    frames.append(im)

    # 3: block - shield
    im, d = new_img(16, 16)
    badge_base(d, (26, 74, 122, 255), (46, 118, 186, 255))
    d.polygon([(8, 3), (13, 5), (12, 11), (8, 14), (4, 11), (3, 5)], fill=(196, 226, 252, 255))
    d.polygon([(8, 5), (11, 6), (10, 10), (8, 12), (6, 10), (5, 6)], fill=(86, 168, 232, 255))
    frames.append(im)

    # 4: bash - cracked block
    im, d = new_img(16, 16)
    badge_base(d, (110, 76, 34, 255), (196, 158, 88, 255))
    d.rectangle((3, 3, 12, 12), fill=CLR_SAND)
    d.rectangle((3, 3, 12, 4), fill=CLR_SAND_L)
    d.line((8, 3, 6, 7), fill=CLR_SAND_D, width=1)
    d.line((6, 7, 9, 9), fill=CLR_SAND_D, width=1)
    d.line((9, 9, 7, 12), fill=CLR_SAND_D, width=1)
    d.line((11, 6, 13, 4), fill=CLR_FIRE_1, width=1)
    frames.append(im)

    # 5: bomb - lit stick of dynamite
    im, d = new_img(16, 16)
    badge_base(d, (118, 34, 34, 255), (196, 62, 56, 255))
    d.rectangle((6, 5, 9, 13), fill=(214, 74, 62, 255))
    d.rectangle((6, 7, 9, 8), fill=(250, 236, 208, 255))
    d.rectangle((7, 2, 8, 5), fill=(150, 118, 74, 255))
    d.ellipse((5, 0, 10, 4), fill=CLR_FIRE_2)
    d.ellipse((6, 1, 9, 3), fill=CLR_FIRE_0)
    frames.append(im)

    # 6: floater - balloon
    im, d = new_img(16, 16)
    badge_base(d, (74, 40, 96, 255), (130, 78, 160, 255))
    d.ellipse((4, 2, 12, 10), fill=CLR_BALLOON_D)
    d.ellipse((5, 2, 11, 9), fill=CLR_BALLOON)
    d.arc((6, 3, 10, 7), 190, 260, fill=CLR_BALLOON_L, width=1)
    d.polygon([(7, 9), (9, 9), (8, 11)], fill=CLR_BALLOON_D)
    d.line((8, 11, 8, 14), fill=CLR_FACE, width=1)
    frames.append(im)

    save_sheet(frames, 16, 16, "badge", duration=0)


# -----------------------------------------------------------------------------
# 4. Exit door (door.sprite, 32x32: closed / open)
# -----------------------------------------------------------------------------
def build_door():
    # Closed: stone arch with a shut wooden door
    im, d = new_img(32, 32)
    d.rectangle((1, 1, 30, 31), fill=CLR_STONE_D)
    d.rectangle((2, 2, 29, 31), fill=CLR_STONE)
    d.rectangle((2, 2, 29, 4), fill=CLR_STONE_L)
    d.rectangle((2, 2, 4, 31), fill=CLR_STONE_L)
    d.rectangle((8, 8, 23, 31), fill=CLR_WOOD_D)
    d.rounded_rectangle((9, 9, 22, 31), 3, fill=CLR_WOOD)
    d.rectangle((9, 9, 22, 11), fill=CLR_WOOD_L)
    d.rectangle((9, 16, 22, 17), fill=CLR_WOOD_D)
    d.rectangle((9, 24, 22, 25), fill=CLR_WOOD_D)
    d.ellipse((17, 19, 21, 23), fill=(246, 208, 96, 255))
    f_closed = im

    # Open: dark passage with a welcoming glow
    im, d = new_img(32, 32)
    d.rectangle((1, 1, 30, 31), fill=CLR_STONE_D)
    d.rectangle((2, 2, 29, 31), fill=CLR_STONE)
    d.rectangle((2, 2, 29, 4), fill=CLR_STONE_L)
    d.rectangle((2, 2, 4, 31), fill=CLR_STONE_L)
    d.rectangle((8, 8, 23, 31), fill=(14, 16, 26, 255))
    d.rounded_rectangle((9, 9, 22, 31), 3, fill=(8, 10, 18, 255))
    d.arc((9, 9, 22, 31), 180, 360, fill=(150, 236, 255, 255), width=2)
    d.ellipse((11, 20, 20, 31), fill=(96, 200, 250, 70))
    # A light arrow pointing in: the crowd knows where to go
    d.polygon([(16, 12), (20, 18), (17, 18), (17, 24), (15, 24),
               (15, 18), (12, 18)], fill=(255, 250, 210, 240))
    f_open = im

    save_sheet([f_closed, f_open], 32, 32, "door", duration=0)


# -----------------------------------------------------------------------------
# 5. Props: balloon (32x32, 2 bob frames) and boulder (32x32, single)
# -----------------------------------------------------------------------------
def build_balloon():
    frames = []
    for step in range(2):
        im, d = new_img(32, 32)
        bob = [0, -1][step]
        squish = [0, 1][step]
        d.ellipse((8, 28, 24, 31), fill=(12, 14, 22, 60))       # shadow
        d.ellipse((4 - squish, 2 + bob, 28 + squish, 24 + bob), fill=CLR_BALLOON_D)
        d.ellipse((5 - squish, 3 + bob, 27 + squish, 23 + bob), fill=CLR_BALLOON)
        d.arc((9, 7 + bob, 21, 18 + bob), 195, 265, fill=CLR_BALLOON_L, width=2)
        d.polygon([(14, 23 + bob), (18, 23 + bob), (16, 26 + bob)], fill=CLR_BALLOON_D)
        d.line((16, 26 + bob, 16, 30 + bob), fill=CLR_FACE_D, width=1)
        frames.append(im)
    save_sheet(frames, 32, 32, "balloon", duration=260)


def build_boulder():
    im, d = new_img(32, 32)
    cx, cy, r = 16, 17, 13
    d.ellipse((6, 28, 26, 31), fill=(12, 14, 22, 80))
    d.ellipse((cx - r, cy - r, cx + r, cy + r), fill=CLR_STONE_D)
    d.ellipse((cx - r + 1, cy - r + 1, cx + r - 1, cy + r - 1), fill=CLR_STONE)
    d.arc((cx - r + 3, cy - r + 3, cx + 1, cy + 1), 200, 280, fill=CLR_STONE_L, width=2)
    for px, py, rr in ((21, 12, 3), (11, 20, 2), (20, 22, 2)):
        d.ellipse((px - rr, py - rr, px + rr, py + rr), fill=CLR_STONE_D)
    d.point((cx - 5, cy - 6), fill=CLR_WHITE)
    save_sheet([im], 32, 32, "boulder", duration=0)


# -----------------------------------------------------------------------------
# 6. Effects: dust puff (3) and sparkle (3)
# -----------------------------------------------------------------------------
def build_dust():
    frames = []
    for step in range(3):
        im, d = new_img(24, 24)
        grow = step
        alpha = 210 - step * 60
        cloud = (198, 176, 150, alpha)
        for cx, cy, r in ((12, 15, 5), (6, 17, 4), (18, 17, 4)):
            rr = r + grow
            d.ellipse((cx - rr, cy - rr, cx + rr, cy + rr), fill=cloud)
        d.ellipse((12 - grow, 9 - grow, 12 + grow, 9 + grow),
                  fill=(226, 208, 186, max(alpha - 40, 40)))
        frames.append(im)
    save_sheet(frames, 24, 24, "dust", duration=90)


def build_sparkle():
    frames = []
    for step in range(3):
        im, d = new_img(24, 24)
        r = [9, 5, 2][step]
        c = [CLR_FIRE_1, CLR_FIRE_0, CLR_FIRE_1][step]
        d.polygon([(12, 12 - r), (14, 10), (12 + r, 12), (14, 14),
                   (12, 12 + r), (10, 14), (12 - r, 12), (10, 10)], fill=c)
        if step < 2:
            d.ellipse((10, 10, 14, 14), fill=CLR_WHITE)
        frames.append(im)
    save_sheet(frames, 24, 24, "sparkle", duration=80)


# -----------------------------------------------------------------------------
# 7. Targeting ring (cursor.png, 24x24 single frame)
# -----------------------------------------------------------------------------
def build_cursor():
    im, d = new_img(24, 24)
    d.ellipse((2, 2, 21, 21), outline=(255, 255, 255, 235), width=2)
    d.ellipse((4, 4, 19, 19), outline=(30, 34, 50, 160), width=1)
    for x1, y1, x2, y2 in ((12, 0, 12, 5), (12, 18, 12, 23), (0, 12, 5, 12), (18, 12, 23, 12)):
        d.line((x1, y1, x2, y2), fill=(255, 255, 255, 235), width=2)
    im.save(os.path.join(IMG, "cursor.png"))
    print("Generated cursor.png (24x24)")


# -----------------------------------------------------------------------------
# Contact sheet - scale x4 so the art can be checked at a glance
# -----------------------------------------------------------------------------
def contact_sheet():
    names = ["waddle", "badge", "door", "balloon", "boulder", "dust", "sparkle"]
    sheets = []
    for n in names:
        p = os.path.join(IMG, n + "_sheet.png")
        if os.path.exists(p):
            sheets.append((n, Image.open(p)))
    tiles = Image.open(os.path.join(IMG, "tiles.png"))
    cursor = Image.open(os.path.join(IMG, "cursor.png"))

    pad, label_h = 8, 14
    cells = [(n, im) for n, im in sheets] + [("tiles", tiles), ("cursor", cursor)]
    scale = 4
    widest = max(im.width for _, im in cells)
    height = sum(label_h + im.height * scale + pad for _, im in cells) + pad
    sheet = Image.new("RGBA", (widest * scale + pad * 3, height), (24, 26, 38, 255))
    d = ImageDraw.Draw(sheet)
    y = pad
    for n, im in cells:
        big = im.resize((im.width * scale, im.height * scale), Image.NEAREST)
        sheet.alpha_composite(big, (pad, y + label_h))
        d.text((pad + 2, y), n + "  " + str(im.width) + "x" + str(im.height), fill=(220, 228, 240, 255))
        y += label_h + big.height + pad
    sheet.save(os.path.join(IMG, "preview.png"))
    print("Generated preview.png (contact sheet)")


if __name__ == "__main__":
    build_tiles()
    build_waddle()
    build_badges()
    build_door()
    build_balloon()
    build_boulder()
    build_dust()
    build_sparkle()
    build_cursor()
    contact_sheet()
    print("Done. All assets written to img/")
