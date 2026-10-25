"""Regenerate the pixel art used by games/seaquest/seaquest.tyro.

Run from the repo root:  python games/seaquest/make_seaquest_assets.py
Requires Pillow. Produces the sq_* files next to this script.

Everything is drawn procedurally (no downloads), so the game is fully
self-contained and reproducible: the art, the palettes and the animation
frames all come from this one script. Animated things are packed into
.sprite atlases (a ZIP holding a PNG strip + a TexturePacker JSON); static
things stay plain PNGs.

The look is chunky Atari-era arcade: small sprites, hard pixel edges and a
limited palette per creature.
"""
import json
import math
import os
import random
import zipfile

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
random.seed(7)


def new(w, h):
    return Image.new("RGBA", (w, h), (0, 0, 0, 0))


def save(name, im):
    im.save(os.path.join(HERE, name))
    print("  wrote", name, im.size)


# ------------------------------------------------------- pixel-string art ----
# Characters are laid out by hand for the small creatures (precise, and it
# reads like a sprite sheet in a retro game source). Rows may be written as
# adjacent string literals; the loader pads/trims every row to the widest one.

def rows_to_image(rows, legend):
    width = max(len(r) for r in rows)
    im = new(width, len(rows))
    for y, row in enumerate(rows):
        for x in range(width):
            ch = row[x] if x < len(row) else "."
            if ch == ".":
                continue
            im.putpixel((x, y), legend[ch])
    return im


# legend shared by the metal/outline colours
M = {
    ".": (0, 0, 0, 0),
    "B": (24, 26, 44, 255),      # outline
    "Y": (244, 206, 66, 255),    # sub yellow
    "D": (188, 142, 40, 255),    # dark yellow
    "W": (255, 253, 236, 255),   # highlight
    "C": (128, 220, 255, 255),   # glass
    "c": (40, 122, 184, 255),    # dark glass
    "O": (230, 116, 44, 255),    # orange trim
    "S": (214, 222, 232, 255),   # steel
    "s": (122, 134, 150, 255),   # dark steel
    "G": (150, 162, 178, 255),   # shark grey
    "g": (94, 106, 124, 255),    # dark grey
    "V": (232, 240, 248, 255),   # belly white
    "E": (12, 12, 20, 255),      # eye
    "P": (208, 96, 150, 255),    # squid pink
    "p": (140, 46, 96, 255),     # dark pink
    "K": (255, 158, 60, 255),    # kraken orange
    "k": (168, 74, 20, 255),     # dark kraken
    "N": (238, 222, 190, 255),   # diver skin
    "A": (60, 96, 214, 255),     # diver suit blue
    "a": (30, 48, 120, 255),     # dark suit
    "T": (246, 196, 52, 255),    # air tank yellow
    "R": (214, 40, 40, 255),     # patrol red
    "r": (132, 24, 24, 255),     # dark red
}


# ------------------------------------------------------------ atlas packer --
def write_sprite(name, frames, durations, scale=1):
    """Pack equally sized frames into a horizontal strip and a .sprite ZIP."""
    fw, fh = frames[0].size
    strip = new(fw * len(frames), fh)
    for i, fr in enumerate(frames):
        strip.paste(fr, (i * fw, 0))
    png_name = name.replace(".sprite", ".png")

    entries = []
    for i in range(len(frames)):
        entries.append({
            "filename": f"{i:03d}",
            "frame": {"x": i * fw, "y": 0, "w": fw, "h": fh},
            "rotated": False,
            "trimmed": False,
            "spriteSourceSize": {"x": 0, "y": 0, "w": fw, "h": fh},
            "sourceSize": {"w": fw, "h": fh},
            "duration": durations[i % len(durations)],
        })
    descriptor = {
        "frames": entries,
        "meta": {"app": "make_seaquest_assets", "image": png_name,
                 "size": {"w": strip.width, "h": strip.height}},
    }

    import io
    buf = io.BytesIO()
    strip.save(buf, "PNG")
    with zipfile.ZipFile(os.path.join(HERE, name), "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("atlas.json", json.dumps(descriptor))
        z.writestr(png_name, buf.getvalue())
    print("  wrote", name, f"({len(frames)} frames of {fw}x{fh})")


# ----------------------------------------------------------------- player ---
# The yellow Seaquest sub, facing right. The hull is hand-laid pixel rows; the
# propeller is stamped on per frame so the three hull frames stay identical.

SUB_BODY = [
    ".............BBBB.............",
    "...........BBYYBB.............",
    "........BBBYYYYYBBB...........",
    "......BBBYYYYYYYYYBBB.........",
    "...BBBYYYYYYYYYYYYYYBBB.......",
    "..BBYYYYCCCYYYYYYYYYYYBB......",
    "..BBYYYYcccYYYYYYYYYYYYBBB....",
    "..BBYYYYCCCYYYYYYYYOOYYYBB....",
    "...BBBYYYYYYYYYYYYOOYYYBBB....",
    "......BBBYYYYYYYYYYYYBBB......",
    "...........BBYYYBBB...........",
    ".............BBBB.............",
]


def sub_frame(prop):
    im = rows_to_image(SUB_BODY, M)
    d = ImageDraw.Draw(im)
    # propeller (three poses: vertical, diagonal, horizontal)
    S, s = M["S"], M["s"]
    if prop == 0:
        for y in range(3, 9):
            im.putpixel((2, y), S if y != 6 else s)
            im.putpixel((3, y), s if y != 6 else S)
        d.rectangle([0, 5, 5, 6], fill=S)
    elif prop == 1:
        for x in (1, 2, 3, 4):
            for y in (3, 4):
                im.putpixel((x, y), S)
            for y in (7, 8):
                im.putpixel((x, y), s)
        for x in (0, 5):
            im.putpixel((x, 4), s)
            im.putpixel((x, 7), s)
        d.rectangle([1, 5, 4, 6], fill=S)
    else:
        d.rectangle([0, 3, 1, 8], fill=S)
        d.rectangle([4, 3, 5, 8], fill=s)
        d.rectangle([0, 5, 5, 6], fill=S)
    # exhaust bubbles behind the prop
    return im


def make_sub():
    write_sprite("sq_sub.sprite", [sub_frame(0), sub_frame(1), sub_frame(2)],
                 [70, 70, 70])


# ----------------------------------------------------------------- enemies ---
SHARK_A = [
    "..............BBBB............",
    "............BBGGGGBB..........",
    "......BBBBBBGGGGGGGBB.........",
    "....BBGGGGGGGGGGGGGGGBB.......",
    "...BGGGGGGGGGGGGGGGGGGGBB.....",
    "...BVGGGGGGGGGGGGGGGGGGGGB....",
    "....BBVGGGGGGGGGGGGGGGVGBB....",
    "......BBGGGGGGGGGGGGGVGB......",
    "........BBBGGGGGGGBBBB........",
    "...........BBBBBB.............",
]


def shark_frame(wag):
    rows = [r.ljust(30, ".") for r in SHARK_A]
    im = rows_to_image(rows, M)
    d = ImageDraw.Draw(im)
    # head detail: eye, mouth, gills
    im.putpixel((26, 4), M["E"])
    im.putpixel((26, 5), M["E"])
    d.line([(29, 6), (24, 6)], fill=(70, 40, 46, 255), width=1)
    for gx in (18, 19, 20):
        d.line([(gx, 3), (gx, 5)], fill=M["g"], width=1)
    # wagging tail (left end)
    tail = [(0, 1), (4, 4), (4, 5), (0, 8)] if wag == 0 else \
           [(0, 6), (4, 4), (4, 5), (0, 3)]
    d.polygon(tail, fill=M["G"], outline=M["B"])
    return im


def make_shark():
    write_sprite("sq_shark.sprite", [shark_frame(0), shark_frame(1)],
                 [230, 230])


SQUID_A = [
    "......BBBB......",
    ".....BBBBBB.....",
    "....BBPPPPBB....",
    "...BBPPPPPPBB...",
    "..BBPPWPPWPPBB..",
    "..BBPPPPPPPPBB..",
    "..BBPPPPPPPPBB..",
    "..BpPPPPPPPPpB..",
    "...BpBPPPPBpB...",
    "...B.pBBBBp.B...",
    "....B..B..B.....",
    "........B.......",
]


def squid_frame(phase):
    rows = [r.ljust(16, ".") for r in SQUID_A]
    im = rows_to_image(rows, M)
    d = ImageDraw.Draw(im)
    # two waving tentacles under the mantle
    for i, base_x in enumerate((3, 6, 9, 12)):
        sway = math.sin((phase + i) * 1.6) * 2
        d.line([(base_x, 9), (base_x + sway, 13), (base_x - sway, 15)],
               fill=M["p"], width=1)
    return im


def make_squid():
    write_sprite("sq_squid.sprite", [squid_frame(0), squid_frame(1)],
                 [180, 180])


EEL_A = [
    "..BBBB..............",
    ".BBGGGBB...BBBB.....",
    "BBGGGGGGBBBGGGGBB...",
    "BGGVGGGGGGGGGGGGBB..",
    "BGGGGGGGGGGGGGGGGBB.",
    ".BBGGGGGGGGGGGGGVGBB",
    "...BBGGGGGGGGGGGBBB.",
    ".....BBBBBBBBBBB....",
]


def eel_frame(phase):
    rows = [r.ljust(22, ".") for r in EEL_A]
    im = rows_to_image(rows, M)
    d = ImageDraw.Draw(im)
    # body wave: shear each column a little so it slithers
    src = im.load()
    waved = new(im.width, im.height)
    wav = waved.load()
    for x in range(im.width):
        off = int(round(math.sin((x / 5.0) + phase) * 1.5))
        for y in range(im.height):
            yy = y + off
            if 0 <= yy < im.height:
                wav[x, yy] = src[x, y]
    d = ImageDraw.Draw(waved)
    d.point((19, 3), fill=M["E"])
    d.point((19, 4), fill=M["E"])
    return waved


def make_eel():
    write_sprite("sq_eel.sprite", [eel_frame(0), eel_frame(1.6)],
                 [110, 110])


PATROL_A = [
    "..........BBBB................",
    "........BBrrrrBB..............",
    "......BBrrRRRRrrBB............",
    "...BBBrrRRRRRRRRrrBBB.........",
    "..BBrrRRCCCrrRRRRRRrrBB.......",
    "..BBrrRRcccrrRRRRRRRRrrBBB....",
    "..BBrrRRCCCrrRRRRSSRRRRrrBB...",
    "...BBBrrRRRRRRRRSSRRRRrrBBB...",
    "......BBrrRRRRRRrrrrBB........",
    "..........BBBBBBBB............",
]


def patrol_frame(prop):
    rows = [r.ljust(30, ".") for r in PATROL_A]
    im = rows_to_image(rows, M)
    d = ImageDraw.Draw(im)
    S, s = M["S"], M["s"]
    if prop == 0:
        for y in range(2, 8):
            im.putpixel((2, y), S)
            im.putpixel((3, y), s)
    else:
        d.rectangle([0, 3, 1, 6], fill=S)
        d.rectangle([4, 3, 5, 6], fill=s)
    return im


def make_patrol():
    write_sprite("sq_patrol.sprite", [patrol_frame(0), patrol_frame(1)],
                 [110, 110])


def kraken_frame(phase):
    im = new(76, 56)
    d = ImageDraw.Draw(im)
    # mantle
    d.ellipse([24, 2, 52, 26], fill=M["K"], outline=M["k"])
    d.ellipse([28, 6, 48, 22], fill=(255, 190, 110, 255))
    # eyes
    for ex in (31, 44):
        d.ellipse([ex, 12, ex + 5, 18], fill=(255, 250, 220, 255), outline=M["k"])
        d.rectangle([ex + 2, 14, ex + 3, 16], fill=(16, 10, 10, 255))
    # eight tentacles, tips swaying with the phase
    for i in range(8):
        bx = 20 + i * 5
        sway = math.sin(phase + i * 0.9)
        pts = [(bx, 24)]
        pts.append((bx + sway * 3, 34))
        pts.append((bx - sway * 5, 44))
        pts.append((bx + sway * 7, 52))
        d.line(pts, fill=M["K"], width=3)
        # suckers
        for t in (0.4, 0.7):
            x = bx + (pts[1][0] - bx) * t * sway
            y = 24 + 28 * t
            d.point((x, y), fill=M["k"])
    d.line([(24, 24), (52, 24)], fill=M["k"], width=1)
    return im


def make_kraken():
    write_sprite("sq_kraken.sprite",
                 [kraken_frame(p) for p in (0.0, 0.9, 1.8, 2.7)],
                 [200, 200, 200, 200])


# ------------------------------------------------------------------ diver ---
DIVER_A = [
    "....BBBB....",
    "...BBNNBB...",
    "..BBCCCCBB..",
    "..BcCCCCcB..",
    "...BBBBBB...",
    "..BBAAAABB..",
    ".BBATAATAABB.",
    ".BBATTTTAABB.",
    "..BBAAAABB..",
    "..BBB..BBB..",
    "..BBB..BBB..",
    ".BBB....BBB.",
    "SSS......SSS",
    "SS........SS",
]


def diver_frame(phase):
    rows = [r.ljust(13, ".") for r in DIVER_A]
    im = rows_to_image(rows, M)
    d = ImageDraw.Draw(im)
    # fluttering flippers
    fx = 1 if phase == 0 else -1
    d.line([(0, 13), (0 + fx, 13)], fill=M["S"], width=1)
    d.line([(11, 13), (11 - fx, 13)], fill=M["S"], width=1)
    # a regulator bubble drifting up from the mouth
    by = 6 if phase == 0 else 2
    d.ellipse([1, by, 3, by + 2], outline=(220, 246, 255, 220))
    return im


def make_diver():
    write_sprite("sq_diver.sprite", [diver_frame(0), diver_frame(1)], [260, 260])


# ----------------------------------------------------------------- effects --
def boom_frame(t):
    im = new(34, 34)
    d = ImageDraw.Draw(im)
    rnd = random.Random(100 + t)
    r = 2.5 + t * 2.0        # max radius ~10.5 + wobble, stays in the 34px frame
    cols = [(255, 250, 210), (255, 210, 90), (255, 140, 40), (190, 62, 28)]
    col = cols[min(t, 3)]
    cx = cy = 17
    # a hollow, ragged fireball: a bright ring plus scattered burning chunks,
    # so the last frames read as an expanding blast instead of a solid disc
    ring = int(r)
    for a in range(0, 360, 7):
        ang = math.radians(a + t * 11)
        wob = rnd.uniform(-2.5, 2.5) * (0.4 + t * 0.3)
        x = cx + math.cos(ang) * (ring + wob)
        y = cy + math.sin(ang) * (ring + wob)
        sz = rnd.uniform(1.5, 3.4)
        shade = cols[min(3, t + (1 if rnd.random() < 0.5 else 0))]
        d.ellipse([x - sz, y - sz, x + sz, y + sz], fill=shade + (255,))
    # flying embers that outrun the ring
    for _ in range(10 - t):
        ang = rnd.uniform(0, math.tau)
        rr = rnd.uniform(r * 1.05, min(r * 1.4 + 2, 15.5))
        x, y = cx + math.cos(ang) * rr, cy + math.sin(ang) * rr
        d.point((x, y), fill=col + (235,))
        d.point((x + 1, y), fill=col + (180,))
    # hot core, shrinking away as the blast opens out
    cr = max(0.0, 5.0 - t * 1.7)
    if cr > 0.5:
        d.ellipse([cx - cr, cy - cr, cx + cr, cy + cr],
                  fill=(255, 255, 238, 255))
    return im


def make_boom():
    write_sprite("sq_boom.sprite", [boom_frame(t) for t in range(5)],
                 [45, 45, 55, 65, 75])


def make_torpedo():
    im = new(16, 6)
    d = ImageDraw.Draw(im)
    d.polygon([(15, 3), (9, 0), (9, 6)], fill=M["O"])
    d.rectangle([3, 1, 9, 4], fill=(206, 214, 226, 255))
    d.rectangle([1, 2, 3, 3], fill=(120, 130, 146, 255))
    d.rectangle([0, 2, 1, 3], fill=(255, 190, 90, 255))
    save("sq_torpedo.png", im)


def make_torpedo_enemy():
    im = new(16, 6)
    d = ImageDraw.Draw(im)
    d.polygon([(0, 3), (6, 0), (6, 6)], fill=(255, 120, 120, 255))
    d.rectangle([6, 1, 12, 4], fill=(226, 130, 130, 255))
    d.rectangle([12, 2, 14, 3], fill=(150, 60, 60, 255))
    d.rectangle([14, 2, 15, 3], fill=(255, 200, 200, 255))
    save("sq_torpedo_e.png", im)


def make_bubble():
    im = new(10, 10)
    d = ImageDraw.Draw(im)
    d.ellipse([0, 0, 9, 9], outline=(200, 236, 255, 190))
    d.ellipse([2, 1, 5, 4], fill=(240, 252, 255, 220))
    save("sq_bubble.png", im)


def make_pearl():
    im = new(10, 10)
    d = ImageDraw.Draw(im)
    d.ellipse([0, 0, 9, 9], fill=(250, 246, 232, 255), outline=(180, 170, 150, 255))
    d.ellipse([2, 2, 4, 4], fill=(255, 255, 255, 255))
    save("sq_pearl.png", im)


# ------------------------------------------------------------ backgrounds ---
# 640px wide so the parallax tiles seamlessly; the sky band lives in the image
# too, which is what lets the sub poke above the waterline without the (front)
# HUD canvas covering it.

BW, BH = 640, 540
SKY = 62            # waterline, in pixels from the top


def lerp(a, b, t):
    return tuple(int(a[i] + (b[i] - a[i]) * t) for i in range(3))


def gradient(w, h, top, bottom, y0=0):
    im = Image.new("RGB", (w, h))
    d = ImageDraw.Draw(im)
    for y in range(h):
        t = y / max(1, h - 1)
        d.line([(0, y), (w, y)], fill=lerp(top, bottom, t))
    return im


def wrap_x(draw_fn, w):
    """Draw with a copy shifted by +-w so shapes crossing the edge tile."""
    for dx in (-w, 0, w):
        draw_fn(dx)


def make_level_bg(n, sky, wtop, wbot, hill, hill2, seed):
    rnd = random.Random(seed)
    im = Image.new("RGBA", (BW, BH))
    # sky band with a few slow clouds
    sky_band = gradient(BW, SKY, sky, lerp(sky, (255, 255, 255), 0.25))
    im.paste(sky_band, (0, 0))
    water = gradient(BW, BH - SKY, wtop, wbot).convert("RGBA")
    im.paste(water, (0, SKY))
    d = ImageDraw.Draw(im)

    # distant hill silhouettes (sine sums keep the seam invisible)
    for band, col, amp, base in ((0, hill2, 26, SKY + 60),
                                 (1, hill, 34, SKY + 110)):
        def draw_hill(dx, amp=amp, base=base, col=col):
            pts = [(x, base) for x in range(0, BW + 1, 4)]
            for i, (x, _) in enumerate(pts):
                xx = x + dx
                yy = base + math.sin(xx / 70.0 + band * 2) * amp \
                     + math.sin(xx / 31.0 + band) * amp * 0.4
                pts[i] = (x, yy)
            d.polygon(pts + [(BW, BH), (0, BH)], fill=col + (255,))
        wrap_x(draw_hill, BW)

    # light shafts coming through the surface (level 1-3 only, fades with depth)
    if n <= 3:
        for _ in range(5):
            sx = rnd.uniform(0, BW)
            wdt = rnd.uniform(26, 64)
            for y in range(SKY, BH, 2):
                t = y / BH
                grow = wdt * (1 + t * 1.6)
                a = int(46 * (1 - t))
                d.line([(sx - grow / 2, y), (sx + grow / 2, y)],
                       fill=(210, 244, 255, max(0, a)))

    # drifting motes / plankton
    for _ in range(70):
        mx, my = rnd.uniform(0, BW), rnd.uniform(SKY + 10, BH)
        a = rnd.randint(30, 110)
        d.point((mx, my), fill=(230, 250, 255, a))

    save(f"sq_bg{n}.png", im)


def make_level_floor(n, floor, rock, coral1, coral2, seed):
    rnd = random.Random(seed)
    im = new(BW, 150)
    d = ImageDraw.Draw(im)

    # seabed ridge
    pts = [(0, 150)]
    for x in range(0, BW + 1, 8):
        y = 46 + math.sin(x / 45.0 + seed) * 12 + math.sin(x / 17.0) * 6
        pts.append((x, y))
    pts.append((BW, 150))
    d.polygon(pts, fill=floor + (255,))

    # rocks on the ridge
    for _ in range(9):
        rx = rnd.uniform(0, BW)
        rw = rnd.uniform(20, 58)
        rh = rnd.uniform(14, 40)
        ry = 46 + math.sin(rx / 45.0 + seed) * 12 + math.sin(rx / 17.0) * 6
        d.ellipse([rx - rw / 2, ry - rh, rx + rw / 2, ry + rh * 0.5],
                  fill=rock + (255,))
        d.ellipse([rx - rw / 4, ry - rh, rx + rw / 6, ry - rh * 0.4],
                  fill=lerp(rock, (255, 255, 255), 0.18) + (255,))

    # coral fans and seaweed
    for _ in range(14):
        cx = rnd.uniform(0, BW)
        cy = 46 + math.sin(cx / 45.0 + seed) * 12 + math.sin(cx / 17.0) * 6
        col = coral1 if rnd.random() < 0.5 else coral2
        for b in range(6):
            ang = -math.pi / 2 + (b - 2.5) * 0.32
            ex = cx + math.cos(ang) * rnd.uniform(12, 26)
            ey = cy + math.sin(ang) * rnd.uniform(12, 26)
            d.line([(cx, cy), (ex, ey)], fill=col + (255,), width=2)
            d.ellipse([ex - 2, ey - 2, ex + 2, ey + 2], fill=col + (255,))
    for _ in range(16):
        gx = rnd.uniform(0, BW)
        gy = 46 + math.sin(gx / 45.0 + seed) * 12 + math.sin(gx / 17.0) * 6
        hgt = rnd.uniform(20, 46)
        sway = rnd.uniform(-10, 10)
        d.line([(gx, gy), (gx + sway, gy - hgt)], fill=(40, 110, 80, 255),
               width=2)

    # wrap-fix: copy the left 24px column strip onto the right edge if needed
    save(f"sq_flr{n}.png", im)


LEVELS = [
    # (name, sky, water top, water bottom, far hill, near hill, floor, rock, coral1, coral2)
    ("Sunlit Reef",        (128, 200, 240), (38, 138, 176), (12, 70, 118),
     (52, 118, 140), (28, 88, 112), (208, 178, 122), (128, 118, 100),
     (238, 116, 138), (246, 198, 88)),
    ("Kelp Forest",        (118, 186, 214), (30, 122, 132), (10, 62, 92),
     (40, 104, 106), (22, 74, 82), (176, 152, 104), (104, 96, 82),
     (96, 176, 92), (206, 172, 70)),
    ("Rocky Canyon",       (104, 166, 202), (26, 106, 146), (8, 52, 96),
     (46, 92, 122), (26, 62, 92), (150, 132, 106), (86, 78, 74),
     (206, 140, 92), (150, 190, 110)),
    ("Shipwreck Graveyard", (84, 136, 178), (20, 88, 128), (6, 40, 82),
     (34, 74, 104), (18, 48, 76), (126, 112, 96), (70, 64, 62),
     (168, 118, 84), (110, 150, 96)),
    ("Volcanic Vent",      (72, 110, 150), (22, 70, 108), (8, 28, 60),
     (56, 60, 84), (30, 32, 54), (92, 76, 70), (54, 44, 44),
     (238, 120, 50), (200, 70, 40)),
    ("Midnight Zone",      (40, 62, 110), (14, 42, 90), (4, 16, 52),
     (22, 40, 78), (12, 22, 56), (66, 60, 76), (38, 34, 48),
     (90, 200, 210), (140, 110, 220)),
    ("The Abyss",          (26, 34, 70), (10, 22, 60), (3, 6, 30),
     (16, 22, 54), (9, 12, 38), (44, 40, 60), (24, 22, 36),
     (70, 170, 190), (110, 80, 190)),
    ("Kraken's Lair",      (46, 30, 56), (26, 14, 44), (10, 4, 22),
     (34, 16, 44), (20, 9, 30), (58, 40, 56), (32, 22, 34),
     (190, 70, 90), (150, 50, 60)),
]


def make_backgrounds():
    for i, lv in enumerate(LEVELS, start=1):
        (name, sky, wtop, wbot, hill, hill2, floor, rock, c1, c2) = lv
        make_level_bg(i, sky, wtop, wbot, hill, hill2, i * 13 + 1)
        make_level_floor(i, floor, rock, c1, c2, i * 7 + 3)


def mirror_rules_module():
    """Copy the rules module into tests/ so the test can require() it.

    Tyro resolves require() inside the workspace, which is the folder of the
    running script - so a test in tests/ cannot reach games/seaquest/. The
    copy is generated (never hand-edited) from the one source next to the
    game, the same way the sprite art is generated; re-run this script after
    changing the rules and both copies stay identical.
    """
    import shutil
    src = os.path.join(HERE, "seaquest_data.tyro")
    dst = os.path.join(os.path.dirname(HERE), "..", "tests", "seaquest_data.tyro")
    dst = os.path.normpath(dst)
    shutil.copyfile(src, dst)
    print("  mirrored seaquest_data.tyro ->", dst)


if __name__ == "__main__":
    print("seaquest assets ->", HERE)
    make_sub()
    make_shark()
    make_squid()
    make_eel()
    make_patrol()
    make_kraken()
    make_diver()
    make_boom()
    make_torpedo()
    make_torpedo_enemy()
    make_bubble()
    make_pearl()
    make_backgrounds()
    mirror_rules_module()
    print("done.")
