"""Asset generator for Space Fighter 3D.
Generates all pixel/sprite assets needed for games/spacefighter/spacefighter.tyro.
Run: python games/spacefighter/make_assets.py
"""
import math
import os
import random
from PIL import Image, ImageDraw, ImageFilter

HERE = os.path.dirname(os.path.abspath(__file__))

def save(name, im):
    path = os.path.join(HERE, name)
    im.save(path)
    print(f"Generated {name} ({im.size[0]}x{im.size[1]})")

def new(w, h):
    return Image.new("RGBA", (w, h), (0, 0, 0, 0))

# ---------------------------------------------------------------------------
# Player Starfighter (Chase view from rear)
# ---------------------------------------------------------------------------
def make_player_ship(bank=0):
    """
    bank: -1 (hard left), 0 (level), +1 (hard right)
    """
    W, H = 64, 48
    im = new(W, H)
    d = ImageDraw.Draw(im)

    cx = W / 2
    cy = H / 2 + 4

    # Wingspan and tilt
    tilt = bank * 6.0
    wing_l = (cx - 28, cy - 2 - tilt)
    wing_r = (cx + 28, cy - 2 + tilt)
    nose = (cx, cy - 18)
    rear_l = (cx - 16, cy + 12 - tilt * 0.4)
    rear_r = (cx + 16, cy + 12 + tilt * 0.4)

    # Main Hull Triangle / Wedge
    hull_pts = [
        nose,
        (cx + 12, cy - 6),
        wing_r,
        (cx + 22, cy + 4 + tilt),
        rear_r,
        (cx, cy + 8),
        rear_l,
        (cx - 22, cy + 4 - tilt),
        wing_l,
        (cx - 12, cy - 6)
    ]
    d.polygon(hull_pts, fill=(50, 75, 120, 255), outline=(130, 180, 240, 255))

    # Upper hull armor plates (lighter titanium blue)
    inner_pts = [
        (cx, cy - 14),
        (cx + 8, cy - 4),
        (cx + 14, cy + 6 + tilt * 0.3),
        (cx, cy + 4),
        (cx - 14, cy + 6 - tilt * 0.3),
        (cx - 8, cy - 4)
    ]
    d.polygon(inner_pts, fill=(80, 115, 175, 255), outline=(180, 220, 255, 255))

    # Cockpit canopy (translucent cyan bubble viewed from behind/top)
    canopy_pts = [
        (cx, cy - 10),
        (cx + 4, cy - 3),
        (cx + 3, cy + 2),
        (cx, cy + 1),
        (cx - 3, cy + 2),
        (cx - 4, cy - 3)
    ]
    d.polygon(canopy_pts, fill=(100, 230, 255, 240), outline=(220, 255, 255, 255))
    # Specular glint on canopy
    d.point((int(cx - 1), int(cy - 4)), fill=(255, 255, 255, 255))

    # Dual Engines at the rear
    e_left = (int(cx - 10), int(cy + 6 - tilt * 0.3))
    e_right = (int(cx + 10), int(cy + 6 + tilt * 0.3))

    for ex, ey in [e_left, e_right]:
        d.rectangle((ex - 3, ey - 2, ex + 3, ey + 4), fill=(40, 45, 60, 255), outline=(90, 100, 130, 255))
        # Thruster glow port
        d.ellipse((ex - 2, ey + 1, ex + 2, ey + 4), fill=(0, 220, 255, 255))
        d.ellipse((ex - 1, ey + 2, ex + 1, ey + 4), fill=(220, 255, 255, 255))

    # Wingtip lights / guns
    d.point((int(wing_l[0] + 1), int(wing_l[1])), fill=(255, 80, 80, 255))
    d.point((int(wing_r[0] - 1), int(wing_r[1])), fill=(80, 255, 120, 255))

    return im

# ---------------------------------------------------------------------------
# Enemy Scout (Small, nimble red/purple flyer)
# ---------------------------------------------------------------------------
def make_enemy_scout():
    W, H = 48, 48
    im = new(W, H)
    d = ImageDraw.Draw(im)
    cx, cy = W / 2, H / 2

    # Menacing swept forward wings
    pts = [
        (cx, cy + 14),           # front nose pointing toward camera
        (cx + 8, cy + 4),
        (cx + 20, cy - 10),      # wingtip
        (cx + 14, cy - 14),
        (cx + 4, cy - 6),
        (cx, cy - 10),           # rear
        (cx - 4, cy - 6),
        (cx - 14, cy - 14),
        (cx - 20, cy - 10),
        (cx - 8, cy + 4)
    ]
    d.polygon(pts, fill=(160, 30, 45, 255), outline=(255, 100, 100, 255))

    # Dark core & crimson eye
    d.ellipse((cx - 5, cy - 2, cx + 5, cy + 6), fill=(40, 10, 15, 255))
    d.ellipse((cx - 3, cy, cx + 3, cy + 4), fill=(255, 40, 40, 255))
    d.point((int(cx), int(cy + 2)), fill=(255, 220, 220, 255))

    return im

# ---------------------------------------------------------------------------
# Enemy Interceptor (Medium aggressive fighter with twin energy pods)
# ---------------------------------------------------------------------------
def make_enemy_interceptor():
    W, H = 56, 56
    im = new(W, H)
    d = ImageDraw.Draw(im)
    cx, cy = W / 2, H / 2

    # Heavy arrowhead hull
    pts = [
        (cx, cy + 18),           # Forward beak
        (cx + 6, cy + 8),
        (cx + 22, cy + 2),       # Forward pod
        (cx + 24, cy - 14),      # Wingtip
        (cx + 14, cy - 8),
        (cx + 4, cy - 12),
        (cx - 4, cy - 12),
        (cx - 14, cy - 8),
        (cx - 24, cy - 14),
        (cx - 22, cy + 2),
        (cx - 6, cy + 8)
    ]
    d.polygon(pts, fill=(110, 40, 130, 255), outline=(220, 120, 255, 255))

    # Yellow/amber plasma cores on wingtips
    d.ellipse((cx - 22, cy - 2, cx - 16, cy + 4), fill=(255, 180, 0, 255))
    d.ellipse((cx + 16, cy - 2, cx + 22, cy + 4), fill=(255, 180, 0, 255))
    # Central cockpit slit
    d.rectangle((cx - 2, cy - 2, cx + 2, cy + 8), fill=(0, 255, 220, 255))

    return im

# ---------------------------------------------------------------------------
# Enemy Heavy Cruiser / Destroyer
# ---------------------------------------------------------------------------
def make_enemy_cruiser():
    W, H = 72, 72
    im = new(W, H)
    d = ImageDraw.Draw(im)
    cx, cy = W / 2, H / 2

    # Diamond dreadnought frame
    pts = [
        (cx, cy + 26),
        (cx + 20, cy + 10),
        (cx + 30, cy - 12),
        (cx + 16, cy - 24),
        (cx, cy - 18),
        (cx - 16, cy - 24),
        (cx - 30, cy - 12),
        (cx - 20, cy + 10)
    ]
    d.polygon(pts, fill=(60, 65, 80, 255), outline=(140, 150, 180, 255))

    # Heavy armored armor plates
    d.polygon([(cx - 12, cy - 10), (cx + 12, cy - 10), (cx + 8, cy + 12), (cx - 8, cy + 12)],
              fill=(90, 100, 120, 255), outline=(180, 190, 220, 255))

    # Glowing plasma battery pods
    for px, py in [(-18, 0), (18, 0), (0, 16)]:
        d.ellipse((cx + px - 4, cy + py - 4, cx + px + 4, cy + py + 4),
                  fill=(255, 60, 20, 255), outline=(255, 200, 150, 255))

    return im

# ---------------------------------------------------------------------------
# Boss Flagship: Sovereign Dreadnought
# ---------------------------------------------------------------------------
def make_boss():
    W, H = 112, 100
    im = new(W, H)
    d = ImageDraw.Draw(im)
    cx, cy = W / 2, H / 2

    # Gigantic menacing mothership hull
    main_hull = [
        (cx, cy + 38),
        (cx + 22, cy + 24),
        (cx + 48, cy + 12),
        (cx + 52, cy - 20),
        (cx + 32, cy - 36),
        (cx + 12, cy - 28),
        (cx, cy - 32),
        (cx - 12, cy - 28),
        (cx - 32, cy - 36),
        (cx - 52, cy - 20),
        (cx - 48, cy + 12),
        (cx - 22, cy + 24),
    ]
    d.polygon(main_hull, fill=(45, 30, 60, 255), outline=(180, 80, 220, 255))

    # Inner armor reinforcement
    d.polygon([
        (cx, cy + 22), (cx + 26, cy + 6), (cx + 28, cy - 14),
        (cx, cy - 18), (cx - 28, cy - 14), (cx - 26, cy + 6)
    ], fill=(70, 45, 95, 255), outline=(220, 140, 255, 255))

    # Super-laser core reactor in the center
    d.ellipse((cx - 10, cy - 6, cx + 10, cy + 14), fill=(255, 30, 80, 255), outline=(255, 230, 100, 255))
    d.ellipse((cx - 5, cy - 1, cx + 5, cy + 9), fill=(255, 255, 200, 255))

    # Wing weapon clusters
    d.ellipse((cx - 44, cy - 6, cx - 36, cy + 2), fill=(0, 240, 255, 255))
    d.ellipse((cx + 36, cy - 6, cx + 44, cy + 2), fill=(0, 240, 255, 255))

    return im

# ---------------------------------------------------------------------------
# Asteroids (3D tumbling space rocks)
# ---------------------------------------------------------------------------
def make_asteroid(seed=1, size=48):
    im = new(size, size)
    d = ImageDraw.Draw(im)
    cx, cy = size / 2, size / 2
    r_base = size * 0.42

    rng = random.Random(seed)
    n_pts = 12
    pts = []
    for i in range(n_pts):
        angle = (2 * math.pi * i) / n_pts
        r = r_base * rng.uniform(0.75, 1.15)
        px = cx + math.cos(angle) * r
        py = cy + math.sin(angle) * r
        pts.append((px, py))

    # Dark rock surface
    d.polygon(pts, fill=(90, 85, 80, 255), outline=(140, 135, 130, 255))

    # Craters and highlights
    for _ in range(3):
        cr_x = cx + rng.uniform(-r_base*0.4, r_base*0.4)
        cr_y = cy + rng.uniform(-r_base*0.4, r_base*0.4)
        cr_r = rng.uniform(3, 7)
        d.ellipse((cr_x - cr_r, cr_y - cr_r, cr_x + cr_r, cr_y + cr_r),
                  fill=(60, 55, 50, 255), outline=(120, 115, 110, 255))

    # Light highlight along upper-left edge
    d.arc((cx - r_base*0.8, cy - r_base*0.8, cx + r_base*0.4, cy + r_base*0.4),
          start=160, end=290, fill=(175, 170, 165, 255), width=2)

    return im

# ---------------------------------------------------------------------------
# Lasers & Projectiles
# ---------------------------------------------------------------------------
def make_laser_player():
    W, H = 16, 24
    im = new(W, H)
    d = ImageDraw.Draw(im)
    cx = W / 2
    # Dual elongated glowing plasma bolt
    d.ellipse((cx - 4, 2, cx + 4, H - 2), fill=(0, 160, 255, 180))
    d.ellipse((cx - 2, 4, cx + 2, H - 4), fill=(100, 230, 255, 255))
    d.line([(cx, 6), (cx, H - 6)], fill=(255, 255, 255, 255), width=2)
    return im

def make_laser_enemy():
    W, H = 16, 20
    im = new(W, H)
    d = ImageDraw.Draw(im)
    cx, cy = W / 2, H / 2
    # Crimson plasma orb
    d.ellipse((cx - 6, cy - 6, cx + 6, cy + 6), fill=(255, 40, 20, 160))
    d.ellipse((cx - 4, cy - 4, cx + 4, cy + 4), fill=(255, 120, 40, 240))
    d.ellipse((cx - 2, cy - 2, cx + 2, cy + 2), fill=(255, 255, 200, 255))
    return im

# ---------------------------------------------------------------------------
# Shiny Star Glint / Specular Diamond Flare
# ---------------------------------------------------------------------------
def make_spark():
    S = 32
    im = new(S, S)
    d = ImageDraw.Draw(im)
    c = S / 2
    # 4-pointed diamond flare with fading tips
    d.polygon([(c, 0), (c + 2, c - 2), (S, c), (c + 2, c + 2),
               (c, S), (c - 2, c + 2), (0, c), (c - 2, c - 2)],
              fill=(255, 255, 255, 240))
    d.ellipse((c - 3, c - 3, c + 3, c + 3), fill=(220, 240, 255, 255))
    d.ellipse((c - 1, c - 1, c + 1, c + 1), fill=(255, 255, 255, 255))
    return im

# ---------------------------------------------------------------------------
# Shield Hit Impact Flare
# ---------------------------------------------------------------------------
def make_shield():
    S = 48
    im = new(S, S)
    d = ImageDraw.Draw(im)
    c = S / 2
    d.ellipse((2, 2, S - 2, S - 2), outline=(0, 230, 255, 220), width=2)
    d.ellipse((6, 6, S - 6, S - 6), outline=(100, 255, 255, 150), width=1)
    # Hex grid ticks
    for angle_deg in range(0, 360, 60):
        rad = math.radians(angle_deg)
        x1 = c + math.cos(rad) * (c - 6)
        y1 = c + math.sin(rad) * (c - 6)
        x2 = c + math.cos(rad) * (c - 2)
        y2 = c + math.sin(rad) * (c - 2)
        d.line([(x1, y1), (x2, y2)], fill=(200, 255, 255, 255), width=2)
    return im

# ---------------------------------------------------------------------------
# Explosions (3 stages)
# ---------------------------------------------------------------------------
def make_explosion(stage):
    S = 48
    im = new(S, S)
    d = ImageDraw.Draw(im)
    c = S / 2
    rng = random.Random(stage * 77)

    if stage == 0:
        # Initial hot plasma burst
        d.ellipse((c - 14, c - 14, c + 14, c + 14), fill=(255, 255, 240, 255))
        d.ellipse((c - 18, c - 18, c + 18, c + 18), outline=(255, 200, 50, 220), width=2)
    elif stage == 1:
        # Expanding fireball with fire tendrils
        d.ellipse((c - 18, c - 18, c + 18, c + 18), fill=(255, 140, 20, 230))
        d.ellipse((c - 10, c - 10, c + 10, c + 10), fill=(255, 240, 100, 255))
        for _ in range(8):
            ang = rng.uniform(0, 2 * math.pi)
            dist = rng.uniform(14, 22)
            d.ellipse((c + math.cos(ang)*dist - 3, c + math.sin(ang)*dist - 3,
                       c + math.cos(ang)*dist + 3, c + math.sin(ang)*dist + 3),
                      fill=(255, 80, 20, 200))
    else:
        # Dissipating smoke & sparks
        d.ellipse((c - 20, c - 20, c + 20, c + 20), outline=(180, 70, 20, 140), width=2)
        for _ in range(12):
            ang = rng.uniform(0, 2 * math.pi)
            dist = rng.uniform(8, 23)
            d.point((int(c + math.cos(ang)*dist), int(c + math.sin(ang)*dist)),
                    fill=(255, 180, 50, 255))

    return im

def main():
    print("Generating Space Fighter 3D assets...")
    save("ship_level.png", make_player_ship(bank=0))
    save("ship_left.png", make_player_ship(bank=-1))
    save("ship_right.png", make_player_ship(bank=1))

    save("enemy_scout.png", make_enemy_scout())
    save("enemy_interceptor.png", make_enemy_interceptor())
    save("enemy_cruiser.png", make_enemy_cruiser())
    save("boss.png", make_boss())

    save("asteroid1.png", make_asteroid(seed=101, size=48))
    save("asteroid2.png", make_asteroid(seed=202, size=56))

    save("laser_player.png", make_laser_player())
    save("laser_enemy.png", make_laser_enemy())
    save("spark.png", make_spark())
    save("shield.png", make_shield())

    save("boom0.png", make_explosion(0))
    save("boom1.png", make_explosion(1))
    save("boom2.png", make_explosion(2))
    print("All assets generated successfully!")

if __name__ == "__main__":
    main()
