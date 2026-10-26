#!/usr/bin/env python3
"""
Submarine - asset generator for the Tyro game engine.

Generates every image and sound the game needs into img/ and sfx/:

  img/*.png     single-frame sprites (torpedo, decor rocks, ...)
  img/*.sprite  multi-frame sprite atlases (a ZIP holding one PNG texture
                plus a TexturePacker-ish JSON descriptor, the format the
                Tyro SpriteAtlas reader understands)
  sfx/*.wav     22050 Hz 16-bit mono sound effects

The sea itself (water gradient, god rays, waves, sand) is not an image -
the game draws it with canvas primitives every frame so the runtime
water shader can distort it.

All the art is drawn from scratch with Pillow, all the sounds are
synthesized with the standard library - no third-party assets, so the
game folder ships with zero external credits.

Usage:  python make_assets.py
"""

import io
import json
import math
import os
import random
import struct
import wave
import zipfile

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "img")
SFX = os.path.join(HERE, "sfx")
os.makedirs(IMG, exist_ok=True)
os.makedirs(SFX, exist_ok=True)

random.seed(20271)

LINE = (24, 26, 44, 255)  # universal dark outline

# ---------------------------------------------------------------------------
# sprite art
# ---------------------------------------------------------------------------


def new_img(w, h):
    img = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    return img, ImageDraw.Draw(img)


def outline(dr, color=LINE):
    """Coarse 1px border on every opaque pixel (8-neighbour outline)."""
    w, h = dr._image.size
    src = dr._image.load()
    marks = []
    for y in range(h):
        for x in range(w):
            r, g, b, a = src[x, y]
            if a < 200:
                continue
            edge = False
            for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)):
                nx, ny = x + dx, y + dy
                if nx < 0 or ny < 0 or nx >= w or ny >= h:
                    edge = True
                    break
                if src[nx, ny][3] < 200:
                    edge = True
                    break
            if edge:
                marks.append((x, y))
    for x, y in marks:
        src[x, y] = color


def px(dr, x, y, color):
    dr.point((x, y), fill=color)


def rect(dr, x, y, w, h, color):
    dr.rectangle((x, y, x + w - 1, y + h - 1), fill=color)


def disc(dr, cx, cy, r, color):
    dr.ellipse((cx - r, cy - r, cx + r, cy + r), fill=color)


def ring(dr, cx, cy, r, color):
    dr.ellipse((cx - r, cy - r, cx + r, cy + r), outline=color)


def poly(dr, pts, color):
    dr.polygon(pts, fill=color)


def sheet(frames, tw, th, name):
    """Pack frames (RGBA images) horizontally and write img/name .sprite"""
    n = len(frames)
    atlas = Image.new("RGBA", (tw * n, th), (0, 0, 0, 0))
    for i, f in enumerate(frames):
        atlas.paste(f, (i * tw, 0))
    png_name = name + ".png"
    atlas.save(os.path.join(IMG, png_name))
    desc = {"frames": {}, "meta": {"image": png_name, "size": {"w": tw * n, "h": th}}}
    for i in range(n):
        desc["frames"]["f%d" % i] = {
            "frame": {"x": i * tw, "y": 0, "w": tw, "h": th},
            "duration": 120,
        }
    path = os.path.join(IMG, name + ".sprite")
    buf = io.BytesIO()
    atlas.save(buf, format="PNG")
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr(png_name, buf.getvalue())
        z.writestr(name + ".json", json.dumps(desc))
    print("ok", name + ".sprite", atlas.size)


def img_save(img, name):
    img.save(os.path.join(IMG, name + ".png"))
    print("ok", name + ".png", img.size)


# --- player submarine -------------------------------------------------------

def sub_frame(prop_phase, light_on):
    """48x20 sub facing right: yellow hull, dark details, spinning prop."""
    img, dr = new_img(48, 20)
    hull = (244, 200, 40, 255)
    hull_dark = (206, 150, 24, 255)
    steel = (176, 184, 196, 255)
    glass = (90, 220, 230, 255)
    # main hull
    rect(dr, 2, 6, 38, 10, hull)
    # nose cone
    poly(dr, [(40, 6), (46, 10), (46, 12), (40, 16)], hull)
    rect(dr, 2, 6, 38, 2, (255, 226, 92, 255))  # top highlight
    rect(dr, 2, 14, 38, 2, hull_dark)           # belly shade
    # conning tower
    rect(dr, 14, 2, 10, 5, hull)
    rect(dr, 16, 0, 6, 3, hull_dark)
    # periscope
    rect(dr, 18, -1 + 1, 2, 2, steel)
    rect(dr, 19, 0, 3, 1, steel)
    # windows (front one lights up in frame variants)
    rect(dr, 10, 9, 4, 4, glass)
    if light_on:
        rect(dr, 16, 9, 4, 4, (255, 250, 180, 255))
    else:
        rect(dr, 16, 9, 4, 4, glass)
    rect(dr, 26, 9, 4, 4, (40, 48, 72, 255))
    # fins
    poly(dr, [(6, 16), (2, 19), (8, 19)], hull_dark)
    poly(dr, [(22, 16), (18, 19), (24, 19)], hull_dark)
    # tail + propeller (3 phases)
    rect(dr, 0, 9, 3, 4, steel)
    cx = 1
    if prop_phase == 0:
        px(dr, cx, 7, steel); px(dr, cx, 15, steel)
    elif prop_phase == 1:
        px(dr, cx, 9, steel); px(dr, cx, 13, steel)
    else:
        px(dr, cx, 11, steel)
    outline(dr)
    return img


# --- shark ------------------------------------------------------------------

def shark_frame(tail):
    """44x20 shark facing right; tail slides up/down."""
    img, dr = new_img(44, 20)
    body = (110, 132, 168, 255)
    belly = (206, 216, 232, 255)
    fin = (86, 104, 140, 255)
    # body
    rect(dr, 4, 7, 28, 7, body)
    # nose
    poly(dr, [(32, 7), (40, 10), (32, 14)], body)
    rect(dr, 6, 13, 26, 2, belly)
    # dorsal fin
    poly(dr, [(14, 7), (19, 1), (23, 7)], fin)
    # tail (3 poses)
    if tail == 0:
        poly(dr, [(4, 10), (0, 4), (2, 10), (0, 16)], fin)
    elif tail == 1:
        poly(dr, [(4, 10), (1, 7), (3, 10), (1, 14)], fin)
    else:
        poly(dr, [(4, 10), (0, 10), (2, 9), (0, 12)], fin)
    # pectoral fin
    poly(dr, [(20, 14), (24, 18), (28, 14)], fin)
    # eye + gills + mouth
    rect(dr, 30, 9, 2, 2, LINE)
    for gy in (9, 11):
        px(dr, 25, gy, (60, 76, 108, 255))
    px(dr, 34, 12, LINE)
    px(dr, 35, 12, LINE)
    outline(dr)
    return img


# --- jellyfish --------------------------------------------------------------

def jelly_frame(squeezed):
    """18x24 jellyfish, translucent pink bell, waving arms."""
    img, dr = new_img(18, 24)
    bell = (238, 130, 200, 215)
    bell_hi = (250, 190, 226, 215)
    arm = (238, 130, 200, 170)
    top = 4 if squeezed else 6
    disc(dr, 9, top + 4, 7, bell)
    rect(dr, 2, top + 4, 14, 2, bell_hi)
    # arms: sine waving
    for i, bx in enumerate((5, 8, 11, 14)):
        sw = 2 if (i + squeezed) % 2 == 0 else -2
        for t in range(5):
            x = bx + (sw if t > 2 else 0)
            y = top + 8 + t * 2
            if y > 23:
                y = 23
            img.putpixel((max(0, min(17, x)), y), arm)
    # spots
    px(dr, 6, top + 2, (255, 240, 250, 235))
    px(dr, 12, top + 3, (255, 240, 250, 235))
    return img


# --- diver ------------------------------------------------------------------

def diver_frame(kick):
    """22x12 diver facing right: orange wetsuit, silver tank."""
    img, dr = new_img(22, 12)
    suit = (240, 122, 78, 255)
    suit_dark = (196, 84, 46, 255)
    skin = (232, 180, 140, 255)
    tank = (188, 196, 210, 255)
    mask = (70, 200, 220, 255)
    # horizontal body
    rect(dr, 2, 4, 14, 5, suit)
    rect(dr, 2, 4, 14, 1, suit_dark)
    # head + mask + snorkel
    disc(dr, 17, 5, 3, skin)
    rect(dr, 17, 4, 3, 2, mask)
    px(dr, 20, 2, tank)
    # tank on the back
    rect(dr, 9, 2, 6, 2, tank)
    # kick legs: 2 poses
    if kick == 0:
        rect(dr, 2, 6, 4, 2, suit_dark)
        px(dr, 1, 8, suit_dark)
    else:
        rect(dr, 2, 2, 4, 2, suit_dark)
        px(dr, 1, 4, suit_dark)
    outline(dr)
    return img


# --- enemy patrol sub -------------------------------------------------------

def patrol_frame(phase):
    """54x20 enemy sub facing left, dark red hull, cannon, prop."""
    img, dr = new_img(54, 20)
    hull = (196, 74, 74, 255)
    hull_dark = (146, 48, 48, 255)
    steel = (120, 126, 138, 255)
    glass = (60, 70, 90, 255)
    # hull
    rect(dr, 10, 5, 38, 10, hull)
    rect(dr, 10, 5, 38, 2, (226, 108, 100, 255))
    rect(dr, 10, 13, 38, 2, hull_dark)
    # nose (faces left)
    poly(dr, [(10, 5), (4, 10), (10, 15)], hull)
    # tower + periscope + cannon
    rect(dr, 28, 1, 10, 5, hull)
    rect(dr, 30, 0, 6, 2, hull_dark)
    px(dr, 33, 0, steel)
    px(dr, 34, -1 + 1, steel)
    rect(dr, 20, 7, 4, 4, glass)
    rect(dr, 36, 7, 4, 4, glass)
    # tail
    poly(dr, [(48, 6), (52, 3), (52, 7)], hull_dark)
    poly(dr, [(48, 14), (52, 13), (52, 17)], hull_dark)
    # prop spins
    rect(dr, 52, 9, 2, 4, steel)
    if phase == 0:
        px(dr, 53, 7, steel); px(dr, 53, 15, steel)
    else:
        px(dr, 53, 11, steel)
    outline(dr)
    return img


# --- mine -------------------------------------------------------------------

def mine_frame(blink):
    """18x18 floating mine, spikes, blinking red light."""
    img, dr = new_img(18, 18)
    iron = (92, 96, 106, 255)
    iron_hi = (126, 132, 142, 255)
    dr.ellipse((3, 3, 14, 14), fill=iron)
    dr.ellipse((5, 5, 11, 11), fill=iron_hi)
    # 8 spikes
    for (sx, sy) in ((9, 0), (9, 15), (0, 8), (16, 8), (3, 2), (13, 2), (3, 13), (13, 13)):
        dr.line((9, 8, sx, sy + 1), fill=iron, width=2)
    dr.ellipse((7, 7, 11, 11), fill=(60, 62, 70, 255))
    if blink:
        dr.ellipse((7, 7, 11, 11), fill=(255, 66, 66, 255))
        px(dr, 8, 8, (255, 210, 180, 255))
    else:
        dr.ellipse((7, 7, 11, 11), fill=(168, 44, 44, 255))
    outline(dr)
    return img


# --- torpedo ----------------------------------------------------------------

def make_torpedo():
    """16x5 torpedo facing right: white nose, dark body, bubble trail dots."""
    img, dr = new_img(16, 5)
    body = (150, 158, 172, 255)
    nose = (238, 238, 242, 255)
    rect(dr, 3, 1, 11, 3, body)
    rect(dr, 3, 1, 11, 1, (118, 124, 138, 255))
    disc(dr, 14, 2, 2, nose)
    px(dr, 13, 0, body); px(dr, 13, 4, body)
    px(dr, 1, 2, (120, 200, 220, 180))
    px(dr, 0, 2, (120, 200, 220, 120))
    outline(dr)
    img_save(img, "torpedo")


# --- explosion (6 frames) ---------------------------------------------------

def boom_frame(n):
    """32x32 radial burst; n 0..5."""
    img, dr = new_img(32, 32)
    stages = [
        # fill rings drawn from outside in: (radius, color) pairs
        [(13, (255, 232, 130, 255)), (10, (255, 150, 40, 255)), (5, (255, 240, 200, 255))],
        [(14, (255, 120, 40, 255)), (11, (255, 190, 60, 255)), (7, (255, 244, 190, 255))],
        [(15, (220, 90, 40, 255)), (12, (255, 150, 50, 255)), (8, (255, 230, 120, 255)), (4, (255, 250, 220, 255))],
        [(14, (150, 150, 160, 255)), (11, (230, 120, 60, 255)), (6, (255, 200, 90, 255))],
        [(13, (110, 110, 120, 200)), (9, (170, 100, 70, 180))],
        [(11, (90, 90, 100, 130))],
    ]
    random.seed(100 + n)
    cx = cy = 16
    for r, col in stages[n]:
        # flame spikes ring
        for ang in range(0, 360, 30):
            a = math.radians(ang + n * 12)
            x = cx + int(math.cos(a) * (r + 3))
            y = cy + int(math.sin(a) * (r + 3))
            disc(dr, x, y, 2, col)
        disc(dr, cx, cy, r, col)
    # debris dots on last stages
    if n >= 3:
        for _ in range(6 + n * 2):
            ang = random.random() * 2 * math.pi
            dist = 10 + random.random() * 6
            disc(dr, cx + int(math.cos(ang) * dist), cy + int(math.sin(ang) * dist), 1, (255, 180, 80, 220))
    return img


# --- seaweed (3 sway frames) ------------------------------------------------

def seaweed_frame(sway):
    """16x36 swaying kelp."""
    img, dr = new_img(16, 36)
    g1 = (44, 148, 66, 255)
    g2 = (92, 200, 96, 255)
    h = 34
    for strand, bx in enumerate((5, 9)):
        for t in range(h):
            f = t / h
            x = bx + int(math.sin(f * 2.2 + sway + strand) * (f * 4))
            col = g1 if strand == 0 else g2
            rect(dr, x, 36 - t - 1, 3, 1, col)
            # side leaves
            if t % 5 == 2:
                lx = x + (3 if (t // 5) % 2 == 0 else -3)
                rect(dr, lx, 36 - t - 2, 2, 3, g2)
    return img


# --- decor ------------------------------------------------------------------

def make_rock():
    random.seed(7)
    img, dr = new_img(30, 16)
    dr.ellipse((2, 4, 27, 15), fill=(96, 100, 116, 255))
    dr.ellipse((5, 2, 22, 14), fill=(120, 124, 140, 255))
    for _ in range(6):
        x, y = random.randint(6, 22), random.randint(4, 12)
        px(dr, x, y, (74, 78, 92, 255))
    outline(dr)
    img_save(img, "rock")


def make_coral():
    img, dr = new_img(24, 20)
    base = (232, 108, 130, 255)
    tips = (248, 170, 120, 255)
    for bx, up in ((6, 12), (11, 18), (16, 10)):
        dr.line((bx, 19, bx, 19 - up), fill=base, width=3)
        disc(dr, bx, 18 - up, 2, tips)
    rect(dr, 3, 17, 18, 3, base)
    outline(dr)
    img_save(img, "coral")


def make_star():
    img, dr = new_img(14, 13)
    col = (248, 176, 64, 255)
    disc(dr, 7, 7, 3, col)
    for ang in range(0, 360, 72):
        a = math.radians(ang - 90)
        x, y = 7 + int(math.cos(a) * 6), 7 + int(math.sin(a) * 6)
        poly(dr, [(7, 7), (x + 2, y), (x - 2, y)], col)
    px(dr, 6, 6, (255, 226, 170, 255))
    outline(dr)
    img_save(img, "star")


def make_chest():
    img, dr = new_img(22, 16)
    wood = (166, 108, 52, 255)
    wood_dark = (128, 80, 36, 255)
    gold = (240, 190, 60, 255)
    rect(dr, 2, 6, 18, 9, wood)
    rect(dr, 2, 6, 18, 2, wood_dark)
    rect(dr, 2, 10, 18, 3, gold)
    rect(dr, 9, 10, 4, 4, (170, 128, 30, 255))
    # lid
    dr.polygon([(2, 7), (19, 7), (19, 4), (2, 4)], fill=wood)
    dr.rectangle((2, 3, 19, 5), fill=wood)
    rect(dr, 2, 3, 18, 2, wood_dark)
    outline(dr)
    img_save(img, "chest")


# ---------------------------------------------------------------------------


def make_all_sprites():
    # multi-frame atlases
    sheet([sub_frame(ph, lo) for ph, lo in ((0, True), (1, False), (2, False), (1, True))],
          48, 20, "sub")
    sheet([shark_frame(t) for t in (0, 1, 2)], 44, 20, "shark")
    sheet([jelly_frame(s) for s in (0, 1, 0)], 18, 24, "jelly")
    sheet([diver_frame(k) for k in (0, 1, 0)], 22, 14, "diver")
    sheet([patrol_frame(p) for p in (0, 1)], 54, 20, "patrol")
    sheet([mine_frame(b) for b in (False, True)], 18, 18, "mine")
    sheet([boom_frame(n) for n in range(6)], 32, 32, "boom")
    sheet([seaweed_frame(s) for s in (0, 1, 2)], 16, 36, "seaweed")
    # single frames
    make_torpedo()
    make_rock()
    make_coral()
    make_star()
    make_chest()


# ---------------------------------------------------------------------------
# sound effects (pure stdlib synthesis)
# ---------------------------------------------------------------------------

RATE = 22050


def write_wav(name, samples):
    path = os.path.join(SFX, name + ".wav")
    with wave.open(path, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(RATE)
        data = b"".join(struct.pack("<h", max(-32767, min(32767, int(s * 32767))))
                        for s in samples)
        w.writeframes(data)
    print("ok", name + ".wav", len(samples) / RATE, "s")


def synth(seconds, f):
    n = int(RATE * seconds)
    out = []
    for i in range(n):
        t = i / RATE
        out.append(f(t, i / n))
    return out


def mix(*parts):
    n = max(len(p) for p in parts)
    out = [0.0] * n
    for p in parts:
        for i, v in enumerate(p):
            out[i] += v
    m = max(1e-6, max(abs(v) for v in out))
    if m > 0.98:
        out = [v * 0.98 / m for v in out]
    return out


def env_exp(t, dur, k=6):
    return math.exp(-k * t / dur)


def note(freq, dur, vol=0.6, wave_shape="square", glide=0.0):
    n = int(RATE * dur)
    out = []
    phase = 0.0
    for i in range(n):
        t = i / RATE
        fq = freq * (1.0 + glide * (i / n))
        phase += fq / RATE
        if wave_shape == "square":
            v = 1.0 if (phase % 1.0) < 0.5 else -1.0
        elif wave_shape == "tri":
            p = phase % 1.0
            v = 4 * p - 1 if p < 0.5 else 3 - 4 * p
        elif wave_shape == "saw":
            v = 2 * (phase % 1.0) - 1
        else:
            v = math.sin(2 * math.pi * phase)
        a = min(1.0, 24 * (i / n) * (1 - 1e-6) + 0.02)
        rel = env_exp(t, dur, 5)
        out.append(vol * v * a * rel)
    return out


def noise_burst(dur, vol=0.7, lp=0.4):
    n = int(RATE * dur)
    out, ysee = [], 0.0
    for i in range(n):
        t = i / RATE
        hi = random.uniform(-1, 1)
        ysee = ysee * (1 - lp) + hi * lp
        a = min(1.0, 40 * (i / n) + 0.01)
        out.append(vol * ysee * env_exp(t, dur, 7) * a)
    return out


def sfx_all():
    # shoot: bubble-ish thump + short hiss
    shoot = mix(
        synth(0.20, lambda t, f: 0.5 * math.sin(2 * math.pi * (600 - 2100 * t) * t) * env_exp(t, 0.20, 8)),
        noise_burst(0.10, 0.20, 0.2),
    )
    write_wav("shoot", shoot)

    # boom: big filtered noise + low thump
    boom = mix(
        noise_burst(0.55, 0.95, 0.14),
        synth(0.5, lambda t, f: 0.85 * math.sin(2 * math.pi * (110 - 60 * t) * t) * env_exp(t, 0.5, 5)),
    )
    write_wav("boom", boom)

    # pickup: bright 3-note arpeggio
    pick = mix(note(660, 0.10, 0.5), note(880, 0.10, 0.5), note(1320, 0.22, 0.5))
    # note() starts all at once; pad the later ones like an arpeggio
    pick = [note(660, 0.10, 0.5), note(880, 0.16, 0.5), note(1320, 0.26, 0.55)]
    total = max(len(p) for p in pick)
    out = [0.0] * total

    def _add(seq, at):
        for j, v in enumerate(seq):
            if at + j < total:
                out[at + j] += v
    _add(pick[0], 0)
    _add(pick[1], int(0.07 * RATE))
    _add(pick[2], int(0.14 * RATE))
    m = max(1e-6, max(abs(v) for v in out))
    if m > 0.95:
        out = [v * 0.95 / m for v in out]
    write_wav("pickup", out)

    # deliver: rising 4-note fanfare (triangle, add octave sparkle)
    fan = [523, 659, 784, 1047]
    seqs, at = [], 0.0
    for k, fq in enumerate(fan):
        seqs.append((note(fq, 0.16, 0.5, "tri") + note(fq * 2, 0.16, 0.25, "sin"), 0.11 if k else 0.0))
        at += 0.10
    seqs.append((note(1568, 0.34, 0.5, "tri"), 0.30))
    total = int(max(len(s) + a * RATE for s, a in seqs))
    out = [0.0] * total
    for s, a0 in seqs:
        n0 = int(a0 * RATE)
        for j, v in enumerate(s):
            if n0 + j < total:
                out[n0 + j] += v
    m = max(1e-6, max(abs(v) for v in out))
    if m > 0.95:
        out = [v * 0.95 / m for v in out]
    write_wav("deliver", out)

    # warn: two square beeps
    b1, b2 = note(392, 0.16, 0.55), note(392, 0.16, 0.55)
    total = len(b1) + len(b2) + int(0.14 * RATE)
    out = [0.0] * total
    for j, v in enumerate(b1):
        out[j] += v
    for j, v in enumerate(b2):
        out[int(0.30 * RATE) + j] += v
    write_wav("warn", out)

    # hit: falling saw + splat noise
    hit = mix(
        synth(0.5, lambda t, f: 0.6 * math.sin(2 * math.pi * (420 - 700 * t) * t) * env_exp(t, 0.5, 6)),
        noise_burst(0.28, 0.5, 0.3),
    )
    write_wav("hit", hit)

    # clear: 6-note level-clear fanfare
    seqs = [
        (note(523, 0.12, 0.5, "tri"), 0.00), (note(659, 0.12, 0.5, "tri"), 0.10),
        (note(784, 0.12, 0.5, "tri"), 0.20), (note(1047, 0.12, 0.5, "tri"), 0.30),
        (note(784, 0.12, 0.45, "tri"), 0.40), (note(1319, 0.44, 0.55, "tri"), 0.50),
    ]
    total = int(max(a * RATE + len(s) for s, a in seqs))
    out = [0.0] * total
    for s, a0 in seqs:
        n0 = int(a0 * RATE)
        for j, v in enumerate(s):
            if n0 + j < total:
                out[n0 + j] += v
    m = max(1e-6, max(abs(v) for v in out))
    if m > 0.95:
        out = [v * 0.95 / m for v in out]
    write_wav("clear", out)

    # over: sad descending phrase
    seqs = [
        (note(659, 0.2, 0.5, "tri"), 0.0), (note(587, 0.2, 0.5, "tri"), 0.20),
        (note(523, 0.2, 0.5, "tri"), 0.40), (note(392, 0.55, 0.55, "tri"), 0.62),
        (note(196, 0.55, 0.3, "sin"), 0.62),
    ]
    total = int(max(a * RATE + len(s) for s, a in seqs))
    out = [0.0] * total
    for s, a0 in seqs:
        n0 = int(a0 * RATE)
        for j, v in enumerate(s):
            if n0 + j < total:
                out[n0 + j] += v
    m = max(1e-6, max(abs(v) for v in out))
    if m > 0.95:
        out = [v * 0.95 / m for v in out]
    write_wav("over", out)

    # sonar: pinging sine with echo
    def ping(t, f=-1):
        if t < 0.16:
            v = math.sin(2 * math.pi * 1180 * t) * env_exp(t, 0.16, 7)
        else:
            v = 0.0
        e = t - 0.30
        if 0 <= e < 0.20:
            v += 0.45 * math.sin(2 * math.pi * 1180 * e) * env_exp(e, 0.20, 7)
        e2 = t - 0.58
        if 0 <= e2 < 0.24:
            v += 0.22 * math.sin(2 * math.pi * 1180 * e2) * env_exp(e2, 0.24, 7)
        return 0.55 * v
    write_wav("sonar", synth(0.95, ping))

    # start: two quick rising notes + sweep
    out = [0.0] * int(0.6 * RATE)
    for j, v in enumerate(note(523, 0.10, 0.5)):
        out[j] += v
    for j, v in enumerate(note(784, 0.10, 0.5)):
        out[int(0.10 * RATE) + j] += v
    for j, v in enumerate(note(1047, 0.28, 0.55)):
        out[int(0.20 * RATE) + j] += v
    m = max(1e-6, max(abs(v) for v in out))
    if m > 0.95:
        out = [v * 0.95 / m for v in out]
    write_wav("start", out)


if __name__ == "__main__":
    make_all_sprites()
    sfx_all()
    print("assets done ->", IMG, SFX)
