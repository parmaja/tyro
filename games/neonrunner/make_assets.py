#!/usr/bin/env python3
"""
Neon Cyber Runner - Procedural Asset Generator for Tyro Game Engine
Creates high-definition pixel-art cyber sprites (.sprite atlases + .png).
Zero external dependencies beyond standard library and Pillow (PIL).
"""

import io
import json
import math
import os
import random
import zipfile

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "img")
os.makedirs(IMG, exist_ok=True)

# Cyberpunk Neon Color Palette
COLOR_TRANSPARENT = (0, 0, 0, 0)
COLOR_DARK_CHASSIS = (16, 20, 34, 255)
COLOR_CHASSIS_LIGHT = (38, 48, 76, 255)
COLOR_CHASSIS_METAL = (70, 85, 120, 255)

NEON_CYAN = (0, 240, 255, 255)
CYAN_GLOW = (0, 180, 230, 180)
NEON_MAGENTA = (255, 30, 150, 255)
MAGENTA_GLOW = (220, 20, 120, 180)
NEON_YELLOW = (255, 235, 40, 255)
NEON_ORANGE = (255, 120, 20, 255)
NEON_GREEN = (40, 255, 140, 255)
NEON_PURPLE = (180, 50, 255, 255)
NEON_RED = (255, 45, 60, 255)
COLOR_WHITE = (255, 255, 255, 255)
COLOR_GOLD = (255, 210, 30, 255)

def new_img(w, h):
    img = Image.new("RGBA", (w, h), COLOR_TRANSPARENT)
    return img, ImageDraw.Draw(img)

def save_sheet(frames, tw, th, name, duration=100):
    """Pack frames horizontally and save both PNG and .sprite zip atlas."""
    n = len(frames)
    atlas = Image.new("RGBA", (tw * n, th), COLOR_TRANSPARENT)
    for i, f in enumerate(frames):
        atlas.paste(f, (i * tw, 0))
    png_name = name + ".png"
    atlas.save(os.path.join(IMG, png_name))

    desc = {"frames": {}, "meta": {"image": png_name, "size": {"w": tw * n, "h": th}}}
    for i in range(n):
        desc["frames"][f"f{i}"] = {
            "frame": {"x": i * tw, "y": 0, "w": tw, "h": th},
            "duration": duration,
        }
    path = os.path.join(IMG, name + ".sprite")
    buf = io.BytesIO()
    atlas.save(buf, format="PNG")
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr(png_name, buf.getvalue())
        z.writestr(name + ".json", json.dumps(desc))
    print(f"Generated {name}.sprite ({n} frames, {tw}x{th})")

# -----------------------------------------------------------------------------
# 1. Player Hoverbike "Vector-9" (player.sprite)
# -----------------------------------------------------------------------------
def make_player_frame(mode="cruise", anim_step=0):
    w, h = 64, 32
    img, dr = new_img(w, h)
    cy = 16

    # Hover bobbing offset
    bob_y = 0
    tilt_angle = 0
    if mode == "lean_up":
        tilt_angle = -7
        bob_y = -1
    elif mode == "lean_down":
        tilt_angle = 7
        bob_y = 1
    elif mode == "jump":
        bob_y = -4
        tilt_angle = -12
    elif mode == "boost":
        bob_y = 0
        tilt_angle = 0
    elif mode == "glitch":
        tilt_angle = random.choice([-5, 5, 0])
        bob_y = random.choice([-1, 1])

    # Rear thruster exhaust fire
    flame_len = 14
    if mode == "boost":
        flame_len = 24
    flame_w = 6 if mode == "boost" else 4

    # Draw thruster flames
    flame_col_outer = NEON_MAGENTA if mode == "boost" else CYAN_GLOW
    flame_col_core = COLOR_WHITE if mode == "boost" else NEON_CYAN

    # Dual thrusters at x = 4..8, y = 13 and y = 19
    for ty in [13 + bob_y, 19 + bob_y]:
        fl = flame_len + (anim_step % 3) * 2
        dr.polygon([(10, ty - flame_w//2), (10 - fl, ty), (10, ty + flame_w//2)], fill=flame_col_outer)
        dr.polygon([(10, ty - 2), (10 - fl//2, ty), (10, ty + 2)], fill=flame_col_core)

    # Hoverbike main aerodynamic chassis
    # Main fuselage
    fuselage = [
        (8, 12 + bob_y),
        (22, 9 + bob_y),
        (44, 11 + bob_y),
        (56, 16 + bob_y),
        (44, 21 + bob_y),
        (22, 23 + bob_y),
        (8, 20 + bob_y),
    ]
    dr.polygon(fuselage, fill=COLOR_DARK_CHASSIS)

    # Upper body panel
    upper_panel = [
        (16, 12 + bob_y),
        (28, 10 + bob_y),
        (42, 12 + bob_y),
        (38, 16 + bob_y),
        (16, 16 + bob_y),
    ]
    dr.polygon(upper_panel, fill=COLOR_CHASSIS_LIGHT)

    # Cockpit canopy (sleek glowing cyan glass)
    canopy = [
        (26, 11 + bob_y),
        (36, 12 + bob_y),
        (46, 15 + bob_y),
        (38, 16 + bob_y),
        (26, 15 + bob_y),
    ]
    canopy_col = NEON_CYAN if mode != "glitch" else NEON_ORANGE
    dr.polygon(canopy, fill=canopy_col)
    dr.line([(28, 12 + bob_y), (42, 14 + bob_y)], fill=COLOR_WHITE, width=1)

    # Neon racing edge stripes
    stripe_col = NEON_MAGENTA if mode == "boost" else NEON_CYAN
    dr.line([(10, 13 + bob_y), (24, 10 + bob_y), (48, 13 + bob_y)], fill=stripe_col, width=1)
    dr.line([(10, 19 + bob_y), (24, 22 + bob_y), (48, 19 + bob_y)], fill=stripe_col, width=1)

    # Front nose cone headlight / sensor
    nose_col = COLOR_WHITE if mode != "glitch" else NEON_RED
    dr.line([(52, 15 + bob_y), (58, 16 + bob_y), (52, 17 + bob_y)], fill=nose_col, width=2)

    # Hover repulsor pads under the chassis (glowing discs)
    dr.ellipse([14, 22 + bob_y, 22, 25 + bob_y], fill=CYAN_GLOW)
    dr.ellipse([36, 21 + bob_y, 44, 24 + bob_y], fill=CYAN_GLOW)

    # Extra jump repulsor rings
    if mode == "jump":
        dr.ellipse([10, 24 + bob_y, 26, 28 + bob_y], outline=NEON_CYAN, width=1)
        dr.ellipse([32, 23 + bob_y, 48, 27 + bob_y], outline=NEON_CYAN, width=1)

    # Boost speed aura lines
    if mode == "boost":
        for ly in [8, 14, 20, 26]:
            dr.line([(0, ly), (8, ly)], fill=COLOR_WHITE, width=1)

    return img

def generate_player():
    frames = [
        make_player_frame("cruise", 0),     # 0: Standard cruise
        make_player_frame("lean_up", 1),    # 1: Banking up
        make_player_frame("lean_down", 2),  # 2: Banking down
        make_player_frame("jump", 0),       # 3: Airborne / jump
        make_player_frame("boost", 0),      # 4: Turbo boost
        make_player_frame("glitch", 0),     # 5: Hit / glitch
    ]
    save_sheet(frames, 64, 32, "player")

# -----------------------------------------------------------------------------
# 2. Traffic & Opponents (traffic.sprite)
# -----------------------------------------------------------------------------
def make_traffic_speeder():
    # Crimson Cyber Speeder Car (fast lane cruiser)
    w, h = 64, 32
    img, dr = new_img(w, h)
    # Fuselage
    dr.polygon([(6, 12), (20, 8), (48, 10), (58, 16), (48, 22), (20, 24), (6, 20)], fill=(80, 10, 20, 255))
    dr.polygon([(16, 10), (32, 9), (46, 12), (40, 16), (16, 16)], fill=(160, 25, 45, 255))
    # Red/Pink glowing neon lines
    dr.line([(8, 13), (22, 9), (50, 12)], fill=NEON_RED, width=1)
    dr.line([(8, 19), (22, 23), (50, 20)], fill=NEON_RED, width=1)
    # Cockpit
    dr.polygon([(26, 11), (38, 12), (44, 15), (28, 15)], fill=(255, 180, 60, 255))
    # Glowing dual headlights (facing left because traffic travels toward player or slower)
    dr.line([(6, 13), (4, 13)], fill=NEON_YELLOW, width=2)
    dr.line([(6, 19), (4, 19)], fill=NEON_YELLOW, width=2)
    # Taillights on right
    dr.line([(57, 15), (59, 16), (57, 17)], fill=NEON_RED, width=2)
    return img

def make_traffic_truck():
    # Heavy Cargo Hauler / Cyber Truck
    w, h = 64, 32
    img, dr = new_img(w, h)
    # Big boxy armored hull
    dr.rectangle([8, 6, 54, 26], fill=(24, 30, 48, 255))
    dr.rectangle([12, 8, 40, 24], fill=(36, 44, 70, 255))
    # Hazard yellow/black diagonal stripes on cargo container
    for x in range(14, 38, 6):
        dr.line([(x, 8), (x + 4, 24)], fill=NEON_YELLOW, width=2)
    # Front cab on left
    dr.rectangle([4, 10, 12, 22], fill=(60, 75, 110, 255))
    dr.rectangle([5, 12, 8, 20], fill=NEON_CYAN) # Windshield
    # Heavy hover coils underneath
    dr.ellipse([14, 25, 24, 28], fill=NEON_ORANGE)
    dr.ellipse([36, 25, 46, 28], fill=NEON_ORANGE)
    dr.line([(4, 11), (2, 11)], fill=COLOR_WHITE, width=2)
    dr.line([(4, 21), (2, 21)], fill=COLOR_WHITE, width=2)
    return img

def make_traffic_drone():
    # Security Patrol Drone (circular hovering disc with scanner eye)
    w, h = 64, 32
    img, dr = new_img(w, h)
    cx, cy = 32, 16
    # Outer metal hull
    dr.ellipse([cx - 16, cy - 10, cx + 16, cy + 10], fill=COLOR_DARK_CHASSIS)
    dr.ellipse([cx - 14, cy - 8, cx + 14, cy + 8], fill=COLOR_CHASSIS_METAL)
    # Glowing energy ring
    dr.ellipse([cx - 12, cy - 6, cx + 12, cy + 6], outline=NEON_CYAN, width=1)
    # Center surveillance red ocular scanner
    dr.ellipse([cx - 6, cy - 4, cx + 2, cy + 4], fill=NEON_RED)
    dr.ellipse([cx - 3, cy - 2, cx, cy + 2], fill=COLOR_WHITE)
    # Scanner beam pointing forward/left
    dr.polygon([(cx - 16, cy - 4), (cx - 28, cy - 8), (cx - 28, cy + 8), (cx - 16, cy + 4)], fill=(255, 40, 60, 60))
    # Quad hover blades / repulsor nodes
    dr.ellipse([cx - 14, cy - 12, cx - 8, cy - 9], fill=NEON_CYAN)
    dr.ellipse([cx + 8, cy - 12, cx + 14, cy - 9], fill=NEON_CYAN)
    dr.ellipse([cx - 14, cy + 9, cx - 8, cy + 12], fill=NEON_CYAN)
    dr.ellipse([cx + 8, cy + 9, cx + 14, cy + 12], fill=NEON_CYAN)
    return img

def make_traffic_glitch_racer():
    # Violet / Magenta Phantom Racer
    w, h = 64, 32
    img, dr = new_img(w, h)
    dr.polygon([(10, 14), (24, 7), (46, 9), (56, 16), (46, 23), (24, 25), (10, 18)], fill=(40, 10, 60, 255))
    dr.polygon([(18, 11), (30, 9), (42, 11), (38, 16), (18, 16)], fill=NEON_PURPLE)
    dr.line([(12, 14), (26, 8), (48, 11)], fill=NEON_MAGENTA, width=2)
    dr.line([(12, 18), (26, 24), (48, 21)], fill=NEON_MAGENTA, width=2)
    dr.polygon([(26, 12), (38, 13), (44, 15), (32, 15)], fill=NEON_CYAN)
    # Twin headlights
    dr.line([(8, 14), (4, 14)], fill=NEON_MAGENTA, width=2)
    dr.line([(8, 18), (4, 18)], fill=NEON_MAGENTA, width=2)
    return img

def generate_traffic():
    frames = [
        make_traffic_speeder(),     # 0: Cyber Speeder (Car)
        make_traffic_truck(),       # 1: Heavy Hauler (Truck)
        make_traffic_drone(),       # 2: Patrol Drone
        make_traffic_glitch_racer(),# 3: Phantom Racer
    ]
    save_sheet(frames, 64, 32, "traffic")

# -----------------------------------------------------------------------------
# 3. Collectibles & Powerups (items.sprite)
# -----------------------------------------------------------------------------
def make_item_battery():
    # Neon Battery Cell (Turbo Fuel)
    w, h = 32, 32
    img, dr = new_img(w, h)
    # Battery outer casing
    dr.rectangle([8, 8, 22, 24], fill=COLOR_DARK_CHASSIS, outline=NEON_GREEN, width=1)
    dr.rectangle([22, 12, 24, 20], fill=COLOR_CHASSIS_METAL) # Positive terminal
    # Internal glowing fluid
    dr.rectangle([10, 10, 20, 22], fill=NEON_GREEN)
    # Lightning bolt emblem inside
    dr.polygon([(16, 11), (13, 16), (17, 16), (14, 21), (19, 15), (15, 15)], fill=COLOR_WHITE)
    # Glow halo
    dr.ellipse([6, 6, 24, 26], outline=(40, 255, 140, 100), width=1)
    return img

def make_item_creds():
    # Cyber Data Cube / Gold Cred
    w, h = 32, 32
    img, dr = new_img(w, h)
    # Isometric glowing cube
    cx, cy = 16, 16
    # Top face
    dr.polygon([(cx, cy - 8), (cx + 8, cy - 4), (cx, cy), (cx - 8, cy - 4)], fill=COLOR_WHITE)
    # Left face
    dr.polygon([(cx - 8, cy - 4), (cx, cy), (cx, cy + 8), (cx - 8, cy + 4)], fill=COLOR_GOLD)
    # Right face
    dr.polygon([(cx + 8, cy - 4), (cx, cy), (cx, cy + 8), (cx + 8, cy + 4)], fill=NEON_ORANGE)
    # Wireframe neon highlight
    dr.polygon([(cx, cy - 9), (cx + 9, cy - 4), (cx, cy + 1), (cx - 9, cy - 4)], outline=NEON_YELLOW, width=1)
    dr.line([(cx, cy + 1), (cx, cy + 9)], fill=NEON_YELLOW, width=1)
    return img

def make_item_shield():
    # Energy Shield Orb Powerup
    w, h = 32, 32
    img, dr = new_img(w, h)
    cx, cy = 16, 16
    dr.ellipse([cx - 10, cy - 10, cx + 10, cy + 10], fill=(0, 120, 240, 120), outline=NEON_CYAN, width=2)
    # Inner atomic core
    dr.ellipse([cx - 4, cy - 4, cx + 4, cy + 4], fill=COLOR_WHITE)
    # Diagonal orbital ring
    dr.ellipse([cx - 11, cy - 5, cx + 11, cy + 5], outline=COLOR_WHITE, width=1)
    return img

def make_item_emp():
    # EMP Bomb Core
    w, h = 32, 32
    img, dr = new_img(w, h)
    cx, cy = 16, 16
    # Octagonal power node
    dr.polygon([
        (cx - 8, cy - 4), (cx - 4, cy - 8), (cx + 4, cy - 8), (cx + 8, cy - 4),
        (cx + 8, cy + 4), (cx + 4, cy + 8), (cx - 4, cy + 8), (cx - 8, cy + 4)
    ], fill=NEON_PURPLE, outline=NEON_MAGENTA, width=1)
    # Glowing core
    dr.ellipse([cx - 4, cy - 4, cx + 4, cy + 4], fill=COLOR_WHITE)
    # Electric sparks
    dr.line([(cx - 8, cy), (cx - 12, cy - 2)], fill=NEON_CYAN, width=1)
    dr.line([(cx + 8, cy), (cx + 12, cy + 2)], fill=NEON_CYAN, width=1)
    dr.line([(cx, cy - 8), (cx + 2, cy - 12)], fill=NEON_CYAN, width=1)
    dr.line([(cx, cy + 8), (cx - 2, cy + 12)], fill=NEON_CYAN, width=1)
    return img

def make_item_boost_pad():
    # Highway Turbo Boost Arrow Pad (road surface Chevron)
    w, h = 32, 32
    img, dr = new_img(w, h)
    # Twin Chevron forward arrows pointing right
    for ox in [6, 16]:
        dr.polygon([(ox, 8), (ox + 8, 16), (ox, 24), (ox + 4, 24), (ox + 12, 16), (ox + 4, 8)], fill=NEON_YELLOW)
        dr.line([(ox + 8, 16), (ox + 12, 16)], fill=COLOR_WHITE, width=1)
    # Outer glow
    dr.rectangle([4, 6, 28, 26], outline=(255, 230, 40, 80), width=1)
    return img

def generate_items():
    frames = [
        make_item_battery(),    # 0: Battery (Turbo Fuel)
        make_item_creds(),      # 1: Data Cube (Creds/Score)
        make_item_shield(),     # 2: Shield Orb
        make_item_emp(),        # 3: EMP Bomb
        make_item_boost_pad(),  # 4: Boost Pad Chevron
    ]
    save_sheet(frames, 32, 32, "items")

# -----------------------------------------------------------------------------
# 4. Hazards & Obstacles (hazards.sprite)
# -----------------------------------------------------------------------------
def make_hazard_barrier():
    # EMP Road Barricade (low barrier, can be jumped over or dodged)
    w, h = 32, 32
    img, dr = new_img(w, h)
    # Support pylons at base
    dr.rectangle([4, 18, 8, 28], fill=COLOR_DARK_CHASSIS)
    dr.rectangle([24, 18, 28, 28], fill=COLOR_DARK_CHASSIS)
    # Heavy crossbar with warning chevrons
    dr.rectangle([2, 10, 30, 20], fill=COLOR_DARK_CHASSIS, outline=NEON_RED, width=1)
    for x in range(4, 28, 6):
        dr.polygon([(x, 10), (x + 3, 10), (x + 1, 20), (x - 2, 20)], fill=NEON_YELLOW)
    # Top red flashing beacon lights
    dr.ellipse([4, 6, 8, 10], fill=NEON_RED)
    dr.ellipse([24, 6, 28, 10], fill=NEON_RED)
    # Electric arc over top
    dr.line([(8, 8), (14, 5), (18, 9), (24, 8)], fill=COLOR_WHITE, width=1)
    return img

def make_hazard_laser_pylon():
    # Vertical Laser Gate Pylon
    w, h = 32, 32
    img, dr = new_img(w, h)
    # Metal base post
    dr.rectangle([10, 4, 22, 28], fill=COLOR_DARK_CHASSIS, outline=COLOR_CHASSIS_METAL, width=1)
    # Central laser emitter crystal
    dr.rectangle([13, 8, 19, 24], fill=NEON_CYAN)
    dr.line([(16, 6), (16, 26)], fill=COLOR_WHITE, width=2)
    # High-voltage warning rings
    dr.ellipse([9, 2, 23, 7], fill=NEON_MAGENTA)
    dr.ellipse([9, 25, 23, 30], fill=NEON_MAGENTA)
    return img

def make_hazard_spikes():
    # Electro-Spikes on the road surface
    w, h = 32, 32
    img, dr = new_img(w, h)
    # Base road mount
    dr.rectangle([2, 22, 30, 28], fill=COLOR_DARK_CHASSIS, outline=NEON_RED, width=1)
    # 3 sharp triangular metal spikes
    for sx in [6, 14, 22]:
        dr.polygon([(sx, 22), (sx + 4, 8), (sx + 8, 22)], fill=COLOR_CHASSIS_METAL, outline=COLOR_WHITE, width=1)
        dr.line([(sx + 4, 8), (sx + 4, 22)], fill=NEON_CYAN, width=1)
    # Crackling spark between spike tips
    dr.line([(10, 8), (14, 12), (18, 8), (22, 11), (26, 8)], fill=COLOR_WHITE, width=1)
    return img

def generate_hazards():
    frames = [
        make_hazard_barrier(),      # 0: Barrier
        make_hazard_laser_pylon(),  # 1: Laser Pylon
        make_hazard_spikes(),       # 2: Electro Spikes
    ]
    save_sheet(frames, 32, 32, "hazards")

# -----------------------------------------------------------------------------
# 5. Energy Shield Bubble (shield.sprite)
# -----------------------------------------------------------------------------
def make_shield_frame(step=0):
    w, h = 72, 48
    img, dr = new_img(w, h)
    cx, cy = 36, 24
    rx, ry = 32, 20

    # Draw hexagonal / elliptical forcefield bubble
    dr.ellipse([cx - rx, cy - ry, cx + rx, cy + ry], fill=(0, 180, 255, 35), outline=NEON_CYAN, width=2)
    dr.ellipse([cx - rx + 3, cy - ry + 3, cx + rx - 3, cy + ry - 3], outline=(0, 240, 255, 90), width=1)

    # Rotating energy hex lattice points
    angle_offset = (step * math.pi / 2)
    for i in range(6):
        a = angle_offset + i * (math.pi / 3)
        px = cx + int((rx - 4) * math.cos(a))
        py = cy + int((ry - 3) * math.sin(a))
        dr.ellipse([px - 2, py - 2, px + 2, py + 2], fill=COLOR_WHITE)

    # Electric spark arc along circumference
    p1_a = angle_offset + 0.3
    p2_a = angle_offset + 1.2
    x1 = cx + int(rx * math.cos(p1_a))
    y1 = cy + int(ry * math.sin(p1_a))
    x2 = cx + int(rx * math.cos(p2_a))
    y2 = cy + int(ry * math.sin(p2_a))
    dr.line([(x1, y1), ((x1+x2)//2 + 2, (y1+y2)//2 - 2), (x2, y2)], fill=COLOR_WHITE, width=1)

    return img

def generate_shield():
    frames = [make_shield_frame(i) for i in range(4)]
    save_sheet(frames, 72, 48, "shield", duration=80)

# -----------------------------------------------------------------------------
# 6. Cyber Voxel Explosion (boom.sprite)
# -----------------------------------------------------------------------------
def make_boom_frame(frame_idx):
    w, h = 48, 48
    img, dr = new_img(w, h)
    cx, cy = 24, 24

    if frame_idx == 0:
        # Initial piercing white/cyan core flash
        dr.ellipse([cx - 8, cy - 8, cx + 8, cy + 8], fill=COLOR_WHITE)
        dr.ellipse([cx - 14, cy - 14, cx + 14, cy + 14], outline=NEON_CYAN, width=2)
    elif frame_idx == 1:
        # Expanding plasma fireball & shockwave
        dr.ellipse([cx - 16, cy - 16, cx + 16, cy + 16], fill=NEON_YELLOW)
        dr.ellipse([cx - 10, cy - 10, cx + 10, cy + 10], fill=COLOR_WHITE)
        dr.ellipse([cx - 20, cy - 20, cx + 20, cy + 20], outline=NEON_CYAN, width=2)
    elif frame_idx == 2:
        # Voxel shard scatter + ring
        dr.ellipse([cx - 22, cy - 22, cx + 22, cy + 22], outline=NEON_MAGENTA, width=2)
        # Flying square voxels
        rng = random.Random(42 + frame_idx)
        for _ in range(12):
            ang = rng.uniform(0, 2 * math.pi)
            dist = rng.uniform(10, 20)
            vx = int(cx + dist * math.cos(ang))
            vy = int(cy + dist * math.sin(ang))
            sz = rng.choice([2, 3])
            col = rng.choice([NEON_CYAN, NEON_YELLOW, NEON_MAGENTA, COLOR_WHITE])
            dr.rectangle([vx - sz, vy - sz, vx + sz, vy + sz], fill=col)
    elif frame_idx == 3:
        # Dispersing shards and widening shockwave
        dr.ellipse([cx - 23, cy - 23, cx + 23, cy + 23], outline=(255, 30, 150, 150), width=1)
        rng = random.Random(42 + frame_idx)
        for _ in range(16):
            ang = rng.uniform(0, 2 * math.pi)
            dist = rng.uniform(15, 23)
            vx = int(cx + dist * math.cos(ang))
            vy = int(cy + dist * math.sin(ang))
            col = rng.choice([NEON_CYAN, NEON_ORANGE, NEON_PURPLE])
            dr.rectangle([vx - 1, vy - 1, vx + 1, vy + 1], fill=col)
    elif frame_idx == 4:
        # Fading embers
        rng = random.Random(42 + frame_idx)
        for _ in range(10):
            ang = rng.uniform(0, 2 * math.pi)
            dist = rng.uniform(18, 23)
            vx = int(cx + dist * math.cos(ang))
            vy = int(cy + dist * math.sin(ang))
            dr.point((vx, vy), fill=(255, 120, 20, 180))
    elif frame_idx == 5:
        # Final residual spark
        rng = random.Random(42 + frame_idx)
        for _ in range(4):
            ang = rng.uniform(0, 2 * math.pi)
            dist = rng.uniform(20, 23)
            vx = int(cx + dist * math.cos(ang))
            vy = int(cy + dist * math.sin(ang))
            dr.point((vx, vy), fill=(100, 240, 255, 100))

    return img

def generate_boom():
    frames = [make_boom_frame(i) for i in range(6)]
    save_sheet(frames, 48, 48, "boom", duration=70)

def main():
    print("Generating Neon Cyber Runner sprite assets...")
    generate_player()
    generate_traffic()
    generate_items()
    generate_hazards()
    generate_shield()
    generate_boom()
    print("All Neon Cyber Runner sprites successfully generated!")

if __name__ == "__main__":
    main()
