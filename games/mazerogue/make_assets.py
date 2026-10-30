#!/usr/bin/env python3
"""
Maze Rogue - Asset Generator for Tyro Game Engine
Creates pixel-art sprites (.sprite atlases + .png), tileset, and syncs rules data.
Zero external dependencies beyond Python standard library and Pillow.
"""

import io
import json
import math
import os
import shutil
import zipfile
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "img")
os.makedirs(IMG, exist_ok=True)

CLR_CLEAR = (0, 0, 0, 0)
CLR_WHITE = (255, 255, 255, 255)
CLR_BLACK = (10, 12, 18, 255)

# Gunner palette
CLR_ARMOR_DARK = (28, 38, 54, 255)
CLR_ARMOR_MID = (46, 62, 88, 255)
CLR_ARMOR_LIGHT = (76, 98, 132, 255)
CLR_VISOR = (0, 240, 255, 255)
CLR_GUN_BODY = (20, 22, 28, 255)
CLR_GUN_ACCENT = (255, 190, 40, 255)
CLR_BOOTS = (20, 25, 35, 255)

# Enemy palettes
CLR_BUG_BODY = (110, 30, 140, 255)
CLR_BUG_SHELL = (160, 45, 190, 255)
CLR_BUG_EYE = (40, 255, 80, 255)

CLR_DRONE_METAL = (70, 80, 95, 255)
CLR_DRONE_LIGHT = (140, 155, 180, 255)
CLR_DRONE_EYE = (255, 35, 60, 255)
CLR_THRUSTER = (0, 200, 255, 220)

CLR_BRUTE_BODY = (45, 52, 60, 255)
CLR_BRUTE_PLATE = (85, 95, 110, 255)
CLR_BRUTE_CORE = (255, 100, 20, 255)

CLR_KAMI_BODY = (210, 140, 20, 255)
CLR_KAMI_STRIPE = (25, 25, 30, 255)
CLR_KAMI_GLOW = (255, 40, 40, 255)

def new_img(w, h):
    im = Image.new("RGBA", (w, h), CLR_CLEAR)
    return im, ImageDraw.Draw(im)

def save_png(im, name):
    path = os.path.join(IMG, name + ".png")
    im.save(path)
    return path

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
# 1. Player Gunner (gunner.sprite, 4 frames walk cycle)
# -----------------------------------------------------------------------------
def make_gunner_frame(step):
    im, d = new_img(32, 32)
    # Leg stride offsets
    leg_l = [-2, 0, 2, 0][step]
    leg_r = [2, 0, -2, 0][step]
    bob = [0, -1, 0, -1][step]

    # Shadow
    d.ellipse((8, 26, 24, 31), fill=(10, 12, 18, 90))

    # Legs & Boots
    d.rectangle((10 + leg_l, 20 + bob, 13 + leg_l, 27), fill=CLR_ARMOR_DARK)
    d.rectangle((9 + leg_l, 26, 14 + leg_l, 29), fill=CLR_BOOTS)

    d.rectangle((18 + leg_r, 20 + bob, 21 + leg_r, 27), fill=CLR_ARMOR_DARK)
    d.rectangle((17 + leg_r, 26, 22 + leg_r, 29), fill=CLR_BOOTS)

    # Body / Torso armor
    y_body = 11 + bob
    d.rectangle((10, y_body, 22, y_body + 9), fill=CLR_ARMOR_MID)
    d.rectangle((12, y_body + 2, 20, y_body + 7), fill=CLR_ARMOR_LIGHT)
    # Belt / Ammo pouches
    d.rectangle((10, y_body + 8, 22, y_body + 10), fill=CLR_ARMOR_DARK)
    d.rectangle((11, y_body + 8, 14, y_body + 10), fill=CLR_GUN_ACCENT)
    d.rectangle((18, y_body + 8, 21, y_body + 10), fill=CLR_GUN_ACCENT)

    # Tactical Helmet & Visor
    y_head = 4 + bob
    d.rectangle((11, y_head, 21, y_head + 7), fill=CLR_ARMOR_MID)
    d.rectangle((12, y_head - 1, 20, y_head + 1), fill=CLR_ARMOR_LIGHT)
    # Glowing Cyan Visor
    d.rectangle((15, y_head + 2, 22, y_head + 4), fill=CLR_VISOR)
    d.point((17, y_head + 3), fill=CLR_WHITE)

    # Arms & Gun (facing rightwards)
    y_gun = 13 + bob
    # Gun barrel & receiver
    d.rectangle((16, y_gun, 28, y_gun + 4), fill=CLR_GUN_BODY)
    d.rectangle((26, y_gun + 1, 30, y_gun + 3), fill=(150, 160, 175, 255)) # muzzle tip
    d.rectangle((19, y_gun + 1, 23, y_gun + 2), fill=CLR_GUN_ACCENT) # heat vent
    # Hands holding gun
    d.rectangle((15, y_gun + 2, 18, y_gun + 5), fill=CLR_ARMOR_LIGHT)
    d.rectangle((22, y_gun + 3, 25, y_gun + 5), fill=CLR_ARMOR_LIGHT)
    return im

def build_gunner():
    frames = [make_gunner_frame(i) for i in range(4)]
    save_sheet(frames, 32, 32, "gunner", duration=120)

# -----------------------------------------------------------------------------
# 2. Enemies
# -----------------------------------------------------------------------------
def make_scuttler_frame(step):
    im, d = new_img(28, 28)
    bob = [0, 1, 0, -1][step]
    leg_spread = [2, 0, -2, 0][step]

    # Shadow
    d.ellipse((6, 21, 22, 26), fill=(10, 12, 18, 90))

    # Spidery Legs
    d.line((14, 14 + bob, 4 - leg_spread, 22), fill=CLR_BUG_SHELL, width=2)
    d.line((14, 14 + bob, 24 + leg_spread, 22), fill=CLR_BUG_SHELL, width=2)
    d.line((14, 14 + bob, 2 + leg_spread, 10), fill=CLR_BUG_SHELL, width=2)
    d.line((14, 14 + bob, 26 - leg_spread, 10), fill=CLR_BUG_SHELL, width=2)

    # Carapace Body
    d.ellipse((8, 8 + bob, 20, 20 + bob), fill=CLR_BUG_BODY)
    d.ellipse((10, 10 + bob, 18, 18 + bob), fill=CLR_BUG_SHELL)

    # Eyes & Mandibles
    d.rectangle((19, 11 + bob, 21, 13 + bob), fill=CLR_BUG_EYE)
    d.rectangle((19, 15 + bob, 21, 17 + bob), fill=CLR_BUG_EYE)
    d.polygon([(20, 13 + bob), (25, 11 + bob), (22, 14 + bob)], fill=(220, 220, 230, 255))
    d.polygon([(20, 15 + bob), (25, 17 + bob), (22, 14 + bob)], fill=(220, 220, 230, 255))
    return im

def make_drone_frame(step):
    im, d = new_img(28, 28)
    bob = [0, -1, -2, -1][step]
    eye_glow = [(255, 30, 50, 255), (255, 80, 100, 255), (255, 30, 50, 255), (200, 20, 40, 255)][step]

    # Shadow
    d.ellipse((6, 23, 22, 27), fill=(10, 12, 18, 70))

    # Thruster particle
    d.ellipse((12, 18 + bob, 16, 24 + bob), fill=CLR_THRUSTER)
    d.point((14, 21 + bob), fill=CLR_WHITE)

    # Hover Sphere Body
    d.ellipse((7, 5 + bob, 21, 19 + bob), fill=CLR_DRONE_METAL)
    d.ellipse((9, 7 + bob, 19, 17 + bob), fill=CLR_DRONE_LIGHT)

    # Armor plating ring
    d.line((5, 12 + bob, 23, 12 + bob), fill=CLR_DRONE_METAL, width=2)

    # Central Sensor Eye
    d.ellipse((11, 9 + bob, 17, 15 + bob), fill=(30, 10, 15, 255))
    d.ellipse((12, 10 + bob, 16, 14 + bob), fill=eye_glow)
    d.point((13, 11 + bob), fill=CLR_WHITE)

    # Side Blasters
    d.rectangle((4, 11 + bob, 7, 14 + bob), fill=(40, 45, 55, 255))
    d.rectangle((21, 11 + bob, 24, 14 + bob), fill=(40, 45, 55, 255))
    return im

def make_brute_frame(step):
    im, d = new_img(36, 36)
    bob = [0, 1, 0, -1][step]
    arm_sway = [-2, 0, 2, 0][step]

    # Shadow
    d.ellipse((7, 30, 29, 35), fill=(10, 12, 18, 110))

    # Heavy Legs
    d.rectangle((10, 24 + bob, 15, 32), fill=CLR_BRUTE_BODY)
    d.rectangle((21, 24 + bob, 26, 32), fill=CLR_BRUTE_BODY)
    d.rectangle((9, 31, 16, 34), fill=(30, 35, 40, 255))
    d.rectangle((20, 31, 27, 34), fill=(30, 35, 40, 255))

    # Massive Torso
    d.rectangle((8, 10 + bob, 28, 25 + bob), fill=CLR_BRUTE_BODY)
    d.rectangle((10, 12 + bob, 26, 23 + bob), fill=CLR_BRUTE_PLATE)

    # Core reactor vents
    d.rectangle((14, 14 + bob, 22, 20 + bob), fill=CLR_BRUTE_CORE)
    d.line((14, 17 + bob, 22, 17 + bob), fill=(255, 220, 100, 255))

    # Heavy Head
    d.rectangle((13, 5 + bob, 23, 11 + bob), fill=CLR_BRUTE_PLATE)
    d.line((14, 8 + bob, 22, 8 + bob), fill=(255, 60, 20, 255), width=2) # visor

    # Spiked Cyber Fist Arms
    d.rectangle((4, 12 + bob + arm_sway, 9, 25 + bob + arm_sway), fill=CLR_BRUTE_PLATE)
    d.rectangle((27, 12 + bob - arm_sway, 32, 25 + bob - arm_sway), fill=CLR_BRUTE_PLATE)
    d.polygon([(27, 24 + bob - arm_sway), (34, 27 + bob - arm_sway), (27, 29 + bob - arm_sway)], fill=(180, 190, 200, 255))
    return im

def make_kamikaze_frame(step):
    im, d = new_img(24, 24)
    bob = [0, 1, 0, -1][step]
    flash = [(255, 200, 30, 255), (255, 60, 40, 255), (255, 200, 30, 255), (255, 20, 20, 255)][step]

    # Shadow
    d.ellipse((5, 18, 19, 23), fill=(10, 12, 18, 80))

    # Little quick legs
    d.line((7, 14, 4, 20), fill=CLR_KAMI_STRIPE, width=2)
    d.line((17, 14, 20, 20), fill=CLR_KAMI_STRIPE, width=2)

    # Rounded Bomb Shell
    d.ellipse((5, 5 + bob, 19, 18 + bob), fill=flash)
    # Hazard stripes
    d.line((7, 8 + bob, 13, 16 + bob), fill=CLR_KAMI_STRIPE, width=2)
    d.line((11, 7 + bob, 17, 15 + bob), fill=CLR_KAMI_STRIPE, width=2)

    # Ticking Fuse / Antenna
    d.line((12, 5 + bob, 12, 2 + bob), fill=CLR_WHITE, width=1)
    d.point((12, 1 + bob), fill=(255, 255, 0, 255))
    return im

def build_enemies():
    save_sheet([make_scuttler_frame(i) for i in range(4)], 28, 28, "enemy_scuttler", duration=100)
    save_sheet([make_drone_frame(i) for i in range(4)], 28, 28, "enemy_drone", duration=120)
    save_sheet([make_brute_frame(i) for i in range(4)], 36, 36, "enemy_brute", duration=140)
    save_sheet([make_kamikaze_frame(i) for i in range(4)], 24, 24, "enemy_kamikaze", duration=80)

# -----------------------------------------------------------------------------
# 3. Weapons: Bullets, Grenade, Explosion, Muzzle Flash
# -----------------------------------------------------------------------------
def build_weapons():
    # Player plasma bullet (directional: nose pointing RIGHT at 0 deg, 20x12)
    im, d = new_img(20, 12)
    # Energy tail
    d.polygon([(1, 6), (7, 3), (7, 9)], fill=(0, 180, 255, 120))
    d.polygon([(4, 6), (10, 4), (10, 8)], fill=(0, 220, 255, 190))
    # Bullet projectile body (pointed nose facing right)
    d.rectangle((7, 3, 14, 8), fill=(0, 240, 255, 255))
    d.polygon([(14, 3), (19, 6), (14, 8)], fill=(120, 255, 255, 255))
    # White-hot interior core
    d.rectangle((9, 4, 15, 7), fill=CLR_WHITE)
    d.polygon([(15, 4), (18, 6), (15, 7)], fill=CLR_WHITE)
    save_png(im, "bullet")

    # Enemy plasma bolt (directional: nose pointing RIGHT at 0 deg, 18x12)
    im, d = new_img(18, 12)
    # Red/violet flame tail
    d.polygon([(1, 6), (6, 3), (6, 9)], fill=(255, 20, 60, 120))
    d.polygon([(3, 6), (9, 4), (9, 8)], fill=(255, 80, 110, 190))
    # Dart body
    d.rectangle((6, 3, 12, 8), fill=(255, 40, 70, 255))
    d.polygon([(12, 3), (17, 6), (12, 8)], fill=(255, 120, 150, 255))
    # Core
    d.rectangle((8, 4, 13, 7), fill=CLR_WHITE)
    d.polygon([(13, 4), (16, 6), (13, 7)], fill=CLR_WHITE)
    save_png(im, "bullet_enemy")

    # Muzzle flash (24x24)
    im, d = new_img(24, 24)
    cx, cy = 12, 12
    # Spikes
    for angle in [0, 45, 90, 135, 180, 225, 270, 315]:
        rad = math.radians(angle)
        ex = cx + int(math.cos(rad) * 10)
        ey = cy + int(math.sin(rad) * 10)
        d.line((cx, cy, ex, ey), fill=(255, 220, 50, 230), width=2)
    d.ellipse((8, 8, 16, 16), fill=(255, 240, 120, 255))
    d.ellipse((10, 10, 14, 14), fill=CLR_WHITE)
    save_png(im, "muzzle")

    # Grenade Shadow (16x16)
    im, d = new_img(16, 16)
    d.ellipse((3, 7, 13, 11), fill=(10, 12, 18, 110))
    save_png(im, "grenade_shadow")

    # Grenade rotating frames (16x16, 4 frames)
    g_frames = []
    for step in range(4):
        im, d = new_img(16, 16)
        rot = step * 90
        # Grenade body (olive green with grid texture)
        d.ellipse((4, 4, 12, 13), fill=(60, 85, 45, 255))
        d.rectangle((5, 6, 11, 11), fill=(75, 105, 55, 255))
        d.line((5, 8, 11, 8), fill=(40, 55, 30, 255))
        d.line((8, 5, 8, 12), fill=(40, 55, 30, 255))
        # Metal top cap & lever
        d.rectangle((6, 2, 10, 5), fill=(160, 170, 180, 255))
        # Blinking red LED fuse
        led = (255, 30, 30, 255) if step % 2 == 0 else (255, 220, 220, 255)
        d.point((8, 4), fill=led)
        if rot > 0:
            im = im.rotate(-rot)
        g_frames.append(im)
    save_sheet(g_frames, 16, 16, "grenade", duration=90)

    # Explosion animation (48x48, 8 frames)
    exp_frames = []
    for step in range(8):
        im, d = new_img(48, 48)
        progress = step / 7.0
        r_outer = 4 + int(progress * 20)
        alpha = int(255 * (1.0 - progress * 0.7))

        if step < 4:
            # Outer blast shockwave
            d.ellipse((24 - r_outer, 24 - r_outer, 24 + r_outer, 24 + r_outer),
                      outline=(255, 180, 40, int(alpha * 0.8)), width=2)
        # Fiery clouds
        num_puffs = 7
        for p in range(num_puffs):
            ang = p * (2 * math.pi / num_puffs) + step * 0.4
            dist = r_outer * 0.65
            px = 24 + int(math.cos(ang) * dist)
            py = 24 + int(math.sin(ang) * dist)
            pr = max(2, int((r_outer * 0.5) * (1.0 - progress * 0.5)))
            clr_fire = (255, max(30, int(200 - progress * 170)), 20, alpha)
            d.ellipse((px - pr, py - pr, px + pr, py + pr), fill=clr_fire)

        # Core blast
        if step < 5:
            r_core = max(2, int((10 - step * 1.8)))
            d.ellipse((24 - r_core, 24 - r_core, 24 + r_core, 24 + r_core), fill=CLR_WHITE)
            d.ellipse((24 - r_core - 2, 24 - r_core - 2, 24 + r_core + 2, 24 + r_core + 2),
                      outline=(255, 230, 100, 220), width=2)
        # Smoke particles in later frames
        if step >= 3:
            for s in range(5):
                sang = s * 1.3
                sdist = r_outer * 0.8
                sx = 24 + int(math.cos(sang) * sdist)
                sy = 24 + int(math.sin(sang) * sdist)
                d.ellipse((sx - 3, sy - 3, sx + 3, sy + 3), fill=(50, 45, 45, int(180 * (1.0 - progress))))
        exp_frames.append(im)
    save_sheet(exp_frames, 48, 48, "explosion", duration=60)

    # Scorch blast mark (32x32)
    im, d = new_img(32, 32)
    d.ellipse((3, 5, 29, 27), fill=(18, 16, 20, 160))
    d.ellipse((7, 8, 25, 24), fill=(10, 8, 12, 190))
    for i in range(12):
        rx = 16 + int(math.cos(i) * 11)
        ry = 16 + int(math.sin(i) * 9)
        d.point((rx, ry), fill=(35, 30, 25, 140))
    save_png(im, "scorch")

# -----------------------------------------------------------------------------
# 4. Interactive Props & Pickups
# -----------------------------------------------------------------------------
def build_props():
    # Breakable crate (28x28)
    im, d = new_img(28, 28)
    d.rectangle((2, 2, 26, 26), fill=(110, 75, 45, 255))
    d.rectangle((4, 4, 24, 24), fill=(140, 95, 55, 255))
    d.rectangle((3, 3, 25, 25), outline=(70, 45, 25, 255), width=2)
    # Cross braces
    d.line((4, 4, 24, 24), fill=(95, 60, 35, 255), width=2)
    d.line((4, 24, 24, 4), fill=(95, 60, 35, 255), width=2)
    # Corner rivets
    for rx, ry in [(5, 5), (23, 5), (5, 23), (23, 23)]:
        d.point((rx, ry), fill=(200, 190, 170, 255))
    save_png(im, "crate")

    # Explosive Barrel (26x30)
    im, d = new_img(26, 30)
    # Shadow
    d.ellipse((3, 24, 23, 29), fill=(10, 12, 18, 90))
    # Red drum body
    d.rectangle((4, 4, 22, 25), fill=(220, 35, 40, 255))
    d.rectangle((5, 5, 21, 24), fill=(245, 50, 55, 255))
    # Metal ribs
    d.line((4, 8, 22, 8), fill=(80, 20, 20, 255), width=2)
    d.line((4, 15, 22, 15), fill=(255, 210, 20, 255), width=3) # Yellow hazard band
    d.line((4, 21, 22, 21), fill=(80, 20, 20, 255), width=2)
    # Hazard skull / exclamation icon
    d.rectangle((12, 11, 14, 16), fill=(20, 20, 20, 255))
    d.point((13, 18), fill=(20, 20, 20, 255))
    # Top rim
    d.ellipse((4, 2, 22, 7), fill=(160, 25, 30, 255))
    d.ellipse((6, 3, 20, 6), fill=(90, 20, 25, 255))
    save_png(im, "barrel")

    # Medkit pickup (22x22)
    im, d = new_img(22, 22)
    d.rectangle((3, 5, 19, 18), fill=(235, 240, 245, 255))
    d.rectangle((4, 6, 18, 17), fill=CLR_WHITE)
    d.rectangle((2, 4, 20, 19), outline=(170, 180, 190, 255), width=1)
    # Red medical cross
    d.rectangle((9, 8, 13, 15), fill=(235, 40, 45, 255))
    d.rectangle((6, 10, 16, 13), fill=(235, 40, 45, 255))
    save_png(im, "pickup_health")

    # Grenade Ammo box pickup (22x22)
    im, d = new_img(22, 22)
    d.rectangle((3, 5, 19, 18), fill=(45, 70, 40, 255))
    d.rectangle((4, 6, 18, 17), fill=(60, 95, 50, 255))
    d.rectangle((2, 4, 20, 19), outline=(30, 50, 25, 255), width=1)
    # Grenade stencil
    d.ellipse((8, 8, 14, 15), fill=(30, 45, 25, 255))
    d.point((11, 7), fill=(255, 200, 40, 255))
    save_png(im, "pickup_grenade")

    # Shield Battery pickup (22x22)
    im, d = new_img(22, 22)
    d.ellipse((3, 3, 19, 19), fill=(0, 140, 255, 160))
    d.ellipse((5, 5, 17, 17), fill=(80, 200, 255, 220))
    d.ellipse((7, 7, 15, 15), fill=CLR_WHITE)
    d.line((11, 4, 11, 18), fill=(0, 120, 240, 255), width=2)
    save_png(im, "pickup_shield")

    # Spinning Gem / Credit (18x18, 4 frames)
    gem_frames = []
    for step in range(4):
        im, d = new_img(18, 18)
        width_ratio = [1.0, 0.6, 0.25, 0.6][step]
        gw = int(7 * width_ratio)
        cx, cy = 9, 9
        pts = [(cx, cy - 7), (cx + gw, cy), (cx, cy + 7), (cx - gw, cy)]
        d.polygon(pts, fill=(0, 230, 160, 255))
        d.line([(cx, cy - 7), (cx, cy + 7)], fill=CLR_WHITE, width=1)
        gem_frames.append(im)
    save_sheet(gem_frames, 18, 18, "gem", duration=120)

    # Stairs / Exit Portal (36x36, 4 frames)
    portal_frames = []
    for step in range(4):
        im, d = new_img(36, 36)
        # Stone archway base
        d.rectangle((4, 4, 32, 32), fill=(35, 40, 50, 255))
        d.rectangle((6, 6, 30, 30), fill=(20, 24, 32, 255))
        # Steps descending into depths
        d.rectangle((8, 8, 28, 13), fill=(45, 52, 65, 255))
        d.rectangle((10, 13, 26, 18), fill=(38, 44, 55, 255))
        d.rectangle((12, 18, 24, 23), fill=(28, 33, 42, 255))
        d.rectangle((14, 23, 22, 28), fill=(15, 18, 24, 255))
        # Pulsing runic glow
        glow_alpha = [140, 200, 255, 180][step]
        d.rectangle((12, 8, 24, 28), outline=(0, 240, 255, glow_alpha), width=1)
        d.point((18, 16), fill=(0, 255, 255, glow_alpha))
        portal_frames.append(im)
    save_sheet(portal_frames, 36, 36, "stairs", duration=150)

    # Sci-fi Crosshair (24x24)
    im, d = new_img(24, 24)
    cx, cy = 12, 12
    d.ellipse((4, 4, 20, 20), outline=(0, 240, 255, 180), width=1)
    d.line((cx - 8, cy, cx - 3, cy), fill=(0, 255, 255, 240), width=2)
    d.line((cx + 3, cy, cx + 8, cy), fill=(0, 255, 255, 240), width=2)
    d.line((cx, cy - 8, cx, cy - 3), fill=(0, 255, 255, 240), width=2)
    d.line((cx, cy + 3, cx, cy + 8), fill=(0, 255, 255, 240), width=2)
    d.point((cx, cy), fill=CLR_WHITE)
    save_png(im, "crosshair")

# -----------------------------------------------------------------------------
# 5. High-Definition Maze Tileset (tiles.png, 8 cols x 2 rows, 32x32 tiles)
# -----------------------------------------------------------------------------
def build_tileset():
    tw, th = 32, 32
    cols, rows = 8, 2
    sheet = Image.new("RGBA", (tw * cols, th * rows), CLR_CLEAR)

    def draw_tile(col, row, func):
        im, d = new_img(tw, th)
        func(d)
        sheet.paste(im, (col * tw, row * th))

    # Tile 1: Void / Pit (dark deep abyss)
    def t_void(d):
        d.rectangle((0, 0, 31, 31), fill=(8, 10, 14, 255))

    # Tile 2: Tech Floor A (metallic plate with subtle studs)
    def t_floor_a(d):
        d.rectangle((0, 0, 31, 31), fill=(34, 40, 52, 255))
        d.rectangle((1, 1, 30, 30), fill=(42, 50, 64, 255))
        d.rectangle((2, 2, 29, 29), outline=(50, 60, 76, 255), width=1)
        for x, y in [(5, 5), (26, 5), (5, 26), (26, 26)]:
            d.point((x, y), fill=(25, 30, 40, 255))

    # Tile 3: Tech Floor B (grated vent panel)
    def t_floor_b(d):
        d.rectangle((0, 0, 31, 31), fill=(28, 32, 42, 255))
        for y in range(4, 28, 4):
            d.line((4, y, 27, y), fill=(16, 20, 28, 255), width=2)
            d.line((4, y + 1, 27, y + 1), fill=(55, 65, 82, 255), width=1)

    # Tile 4: Tech Floor C (warning hazard strip)
    def t_floor_c(d):
        d.rectangle((0, 0, 31, 31), fill=(42, 50, 64, 255))
        # Hazard diagonal stripe
        for i in range(-16, 48, 8):
            d.line((i, 0, i + 16, 31), fill=(220, 180, 20, 160), width=3)
            d.line((i + 3, 0, i + 19, 31), fill=(20, 20, 25, 180), width=3)

    # Tile 5: Solid Wall Top (dark tech-stone block)
    def t_wall_top(d):
        d.rectangle((0, 0, 31, 31), fill=(58, 68, 85, 255))
        d.rectangle((1, 1, 30, 30), fill=(72, 84, 105, 255))
        d.rectangle((3, 3, 28, 28), outline=(90, 105, 130, 255), width=1)
        d.line((1, 1, 30, 1), fill=(115, 132, 160, 255), width=1) # top highlight

    # Tile 6: Wall Face / Front (with metallic panel detailing and shadow)
    def t_wall_face(d):
        # 3D wall face dropping down
        d.rectangle((0, 0, 31, 10), fill=(58, 68, 85, 255)) # top ledge
        d.rectangle((0, 10, 31, 31), fill=(38, 44, 56, 255)) # front drop
        d.rectangle((3, 13, 28, 28), fill=(48, 56, 70, 255)) # panel
        d.line((0, 10, 31, 10), fill=(20, 24, 32, 255), width=2) # shadow crease
        d.line((0, 31, 31, 31), fill=(15, 18, 24, 255), width=1) # ground contact shadow

    # Tile 7: Wall Corner Left
    def t_wall_corner_l(d):
        d.rectangle((0, 0, 31, 31), fill=(58, 68, 85, 255))
        d.rectangle((2, 2, 31, 31), fill=(72, 84, 105, 255))
        d.line((0, 0, 0, 31), fill=(30, 36, 46, 255), width=2)

    # Tile 8: Wall Corner Right
    def t_wall_corner_r(d):
        d.rectangle((0, 0, 31, 31), fill=(58, 68, 85, 255))
        d.rectangle((0, 2, 29, 31), fill=(72, 84, 105, 255))
        d.line((31, 0, 31, 31), fill=(30, 36, 46, 255), width=2)

    # Row 1 tiles
    def t_door_closed(d):
        d.rectangle((0, 0, 31, 31), fill=(50, 60, 75, 255))
        d.rectangle((4, 4, 27, 27), fill=(25, 30, 40, 255))
        d.rectangle((6, 6, 25, 25), fill=(180, 50, 40, 255))
        d.line((6, 16, 25, 16), fill=(255, 220, 30, 255), width=2)

    def t_pillar(d):
        d.rectangle((0, 0, 31, 31), fill=(34, 40, 52, 255))
        d.rectangle((6, 6, 25, 25), fill=(60, 70, 88, 255))
        d.rectangle((8, 8, 23, 23), fill=(80, 94, 118, 255))
        d.ellipse((10, 10, 21, 21), fill=(0, 240, 255, 180))

    # Row 0
    draw_tile(0, 0, t_void)
    draw_tile(1, 0, t_floor_a)
    draw_tile(2, 0, t_floor_b)
    draw_tile(3, 0, t_floor_c)
    draw_tile(4, 0, t_wall_top)
    draw_tile(5, 0, t_wall_face)
    draw_tile(6, 0, t_wall_corner_l)
    draw_tile(7, 0, t_wall_corner_r)

    # Row 1
    draw_tile(0, 1, t_door_closed)
    draw_tile(1, 1, t_pillar)
    draw_tile(2, 1, t_floor_a)
    draw_tile(3, 1, t_floor_b)
    draw_tile(4, 1, t_wall_top)
    draw_tile(5, 1, t_wall_face)
    draw_tile(6, 1, t_wall_corner_l)
    draw_tile(7, 1, t_wall_corner_r)

    tiles_path = os.path.join(HERE, "tiles.png")
    sheet.save(tiles_path)
    print(f"Generated tiles.png ({tw * cols}x{th * rows}, 32x32 tiles)")

def sync_data_module():
    src = os.path.join(HERE, "mazerogue_data.tyro")
    dst = os.path.join(HERE, "..", "..", "tests", "mazerogue_data.tyro")
    dst = os.path.abspath(dst)
    if os.path.exists(src):
        shutil.copyfile(src, dst)
        print(f"Synced mazerogue_data.tyro -> {dst}")

def main():
    print("Building Maze Rogue assets...")
    build_gunner()
    build_enemies()
    build_weapons()
    build_props()
    build_tileset()
    sync_data_module()
    print("All assets generated successfully!")

if __name__ == "__main__":
    main()
