#!/usr/bin/env python3
"""
Neon Breaker - Asset Generator for Tyro Game Engine
Creates pixel-art sprites (.png + .sprite atlases) and sound effects (.wav).
Zero external dependencies beyond Python standard library and Pillow.
"""

import io
import json
import math
import os
import random
import shutil
import struct
import wave
import zipfile

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "img")
SFX = os.path.join(HERE, "sfx")
os.makedirs(IMG, exist_ok=True)
os.makedirs(SFX, exist_ok=True)

random.seed(777)

LINE_DARK = (10, 14, 28, 255)
CYAN = (0, 230, 255, 255)
WHITE = (255, 255, 255, 255)

# The six classic brick rows, top row first, plus steel and cracked steel.
ROW_COLORS = [
    (255, 70, 85, 255),   # 1 red
    (255, 140, 40, 255),  # 2 orange
    (255, 220, 60, 255),  # 3 yellow
    (60, 220, 110, 255),  # 4 green
    (60, 170, 255, 255),  # 5 blue
    (185, 90, 255, 255),  # 6 purple
    (150, 160, 175, 255), # 7 steel
]
STEEL_CRACK = (90, 100, 115, 255)


def new_img(w, h):
    img = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    return img, ImageDraw.Draw(img)


def shade(col, factor):
    return tuple(min(255, max(0, int(c * factor))) for c in col[:3]) + (col[3],)


def sheet(frames, tw, th, name, duration=200):
    """Pack frames horizontally and save both PNG and .sprite zip atlas."""
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
# 1. Bricks (bricks.png tilemap sheet + bricks.sprite) - 6 tiles, one per
#    classic row colour. Each tile is 50x22: the 48x20 brick with a 1px inset
#    and the 2px grid gap baked in, so a plain tiles.draw() lines up exactly
#    with the collision rects from breakout_data.
# -----------------------------------------------------------------------------
def build_bricks():
    w, h = 50, 22  # D.BRICK_W + D.BRICK_GAP, D.BRICK_H + D.BRICK_GAP
    frames = []
    for col in ROW_COLORS:
        img, dr = new_img(w, h)
        # Body with dark outline, inset 1 so neighbouring tiles never touch
        dr.rounded_rectangle([2, 2, w - 3, h - 3], radius=4, fill=col, outline=LINE_DARK)
        # Bevel: bright top edge, dark bottom edge
        dr.line([(7, 4), (w - 8, 4)], fill=shade(col, 1.45), width=1)
        dr.line([(7, h - 5), (w - 8, h - 5)], fill=shade(col, 0.45), width=1)
        # Gloss strip on the upper half
        dr.rounded_rectangle([6, 6, w - 7, 10], radius=2, fill=shade(col, 1.6)[:3] + (90,))
        # Tiny rivets at the ends for the arcade cabinet feel
        dr.rectangle([4, h // 2 - 1, 5, h // 2], fill=LINE_DARK)
        dr.rectangle([w - 6, h // 2 - 1, w - 5, h // 2], fill=LINE_DARK)
        frames.append(img)
    # Cracked steel: takes two hits, fissures after the first
    img, dr = new_img(w, h)
    col = ROW_COLORS[6]
    dr.rounded_rectangle([2, 2, w - 3, h - 3], radius=4, fill=STEEL_CRACK, outline=LINE_DARK)
    dr.line([(7, 4), (w - 8, 4)], fill=shade(col, 1.1), width=1)
    dr.rounded_rectangle([6, 6, w - 7, 10], radius=2, fill=shade(col, 1.2)[:3] + (60,))
    dr.line([(14, 4), (20, 10), (16, 13), (24, 18)], fill=LINE_DARK, width=1)
    dr.line([(34, 5), (30, 11), (36, 14), (32, 18)], fill=LINE_DARK, width=1)
    dr.rectangle([4, h // 2 - 1, 5, h // 2], fill=LINE_DARK)
    dr.rectangle([w - 6, h // 2 - 1, w - 5, h // 2], fill=LINE_DARK)
    frames.append(img)
    sheet(frames, w, h, "bricks", duration=400)


# -----------------------------------------------------------------------------
# 4. Power-up badges (powerups.sprite, 4 frames 22x22)
#    0 WIDE, 1 STICKY, 2 MULTI, 3 LASER  - order matches D.PU
# -----------------------------------------------------------------------------
PU_BADGES = [CYAN, (255, 230, 60, 255), (60, 255, 150, 255), (255, 140, 40, 255)]


def build_powerups():
    w, h = 22, 22
    frames = []
    cx, cy = 11, 11
    for idx, col in enumerate(PU_BADGES):
        img, dr = new_img(w, h)
        # Hexagon badge
        r = 10
        pts = [(cx + math.cos(a) * r, cy + math.sin(a) * r)
               for a in [i * math.pi / 3 for i in range(6)]]
        dr.polygon(pts, fill=(15, 20, 35, 245), outline=col)
        if idx == 0:      # WIDE: wide bar with arrows
            dr.rounded_rectangle([3, cy - 2, w - 4, cy + 2], radius=2, fill=col)
            dr.polygon([(3, cy), (6, cy - 3), (6, cy + 3)], fill=WHITE)
            dr.polygon([(w - 4, cy), (w - 7, cy - 3), (w - 7, cy + 3)], fill=WHITE)
        elif idx == 1:    # STICKY: caught ball with ring
            dr.ellipse([cx - 3, cy - 3, cx + 3, cy + 3], fill=col)
            dr.arc([cx - 6, cy - 6, cx + 6, cy + 6], 30, 300, fill=WHITE)
        elif idx == 2:    # MULTI: three little balls
            for bx, by in [(cx - 4, cy + 2), (cx + 4, cy + 2), (cx, cy - 4)]:
                dr.ellipse([bx - 2, by - 2, bx + 2, by + 2], fill=col, outline=WHITE)
        else:             # LASER: twin bolts
            dr.polygon([(cx - 4, cy - 6), (cx - 1, cy - 6), (cx - 3, cy),
                        (cx, cy), (cx - 3, cy + 6)], fill=col)
            dr.polygon([(cx + 4, cy - 6), (cx + 1, cy - 6), (cx + 3, cy),
                        (cx, cy), (cx + 3, cy + 6)], fill=WHITE)
        frames.append(img)
    sheet(frames, w, h, "powerups", duration=600)


# -----------------------------------------------------------------------------
# 5. Paddle (paddle.png, 96x16 = D.PADDLE_W / D.PADDLE_H)
# -----------------------------------------------------------------------------
def build_paddle():
    w, h = 96, 16
    img, dr = new_img(w, h)
    dr.rounded_rectangle([1, 1, w - 2, h - 2], radius=7, fill=(20, 60, 120, 255), outline=LINE_DARK)
    # Cyan energy body
    dr.rounded_rectangle([4, 3, w - 5, h - 4], radius=5, fill=shade(CYAN, 0.55))
    # Bright core rail
    dr.line([(10, h // 2), (w - 11, h // 2)], fill=CYAN, width=2)
    dr.line([(10, h // 2 - 1), (w - 11, h // 2 - 1)], fill=WHITE, width=1)
    # End caps
    dr.ellipse([5, h // 2 - 3, 11, h // 2 + 3], fill=WHITE)
    dr.ellipse([w - 12, h // 2 - 3, w - 6, h // 2 + 3], fill=WHITE)
    img.save(os.path.join(IMG, "paddle.png"))
    print("Generated paddle.png (96x16)")


# -----------------------------------------------------------------------------
# 3. Ball (ball.png, 16x16; core radius 7 fits D.BALL_R)
# -----------------------------------------------------------------------------
def build_ball():
    w, h = 16, 16
    img, dr = new_img(w, h)
    cx, cy = 8, 8
    # Soft outer glow ring
    dr.ellipse([cx - 7, cy - 7, cx + 7, cy + 7], fill=(120, 245, 255, 70))
    dr.ellipse([cx - 6, cy - 6, cx + 6, cy + 6], fill=(90, 220, 255, 130))
    # Hard plasma core
    dr.ellipse([cx - 5, cy - 5, cx + 5, cy + 5], fill=(190, 250, 255, 255), outline=CYAN)
    dr.ellipse([cx - 3, cy - 3, cx + 3, cy + 3], fill=WHITE)
    dr.point([(cx - 2, cy - 3), (cx - 3, cy - 2)], fill=WHITE)
    img.save(os.path.join(IMG, "ball.png"))
    print("Generated ball.png (16x16)")


# -----------------------------------------------------------------------------
# Sound Effects (22050 Hz 16-bit mono WAV)
# -----------------------------------------------------------------------------
def write_wav(filename, samples, sample_rate=22050):
    path = os.path.join(SFX, filename)
    with wave.open(path, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(sample_rate)
        raw = bytearray()
        for s in samples:
            val = max(-32767, min(32767, int(s * 32767)))
            raw.extend(struct.pack("<h", val))
        w.writeframes(raw)
    print(f"Generated sound {filename} ({len(samples)} samples, {len(samples)/sample_rate:.2f}s)")


def make_sounds():
    SR = 22050

    # 1. bounce.wav (short paddle/wall blip)
    duration = 0.06
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.exp(-t * 60.0)
        s = math.sin(2 * math.pi * 330 * t) * env * 0.7
        samples.append(s)
    write_wav("bounce.wav", samples, SR)

    # 2. brick.wav (crisp brick crack, pitch rises with the row)
    duration = 0.10
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.exp(-t * 32.0)
        freq = 520 + 90 * math.exp(-t * 25.0)
        phase = 2 * math.pi * freq * t
        s = (math.sin(phase) * 0.6 + ((phase % (2 * math.pi)) / math.pi - 1.0) * 0.3) * env * 0.8
        samples.append(s)
    write_wav("brick.wav", samples, SR)

    # 3. break.wav (brick shatter pop)
    duration = 0.16
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.exp(-t * 24.0)
        noise = (random.random() * 2 - 1)
        s = (math.sin(2 * math.pi * 180 * t) * 0.5 + noise * 0.5) * env * 0.8
        samples.append(s)
    write_wav("break.wav", samples, SR)


def main():
    print("Building Neon Breaker sprites...")
    build_bricks()
    build_powerups()
    build_paddle()
    build_ball()

    print("Building Neon Breaker sounds...")
    make_sounds()

    # Copy breakout_data.tyro to tests/ so tests/test_breakout_data.tyro can require it
    src_data = os.path.join(HERE, "breakout_data.tyro")
    dst_data = os.path.join(os.path.dirname(HERE), "..", "tests", "breakout_data.tyro")
    if os.path.exists(src_data):
        shutil.copyfile(src_data, dst_data)
        print("Copied breakout_data.tyro to tests/")

    print("Done! All Neon Breaker assets generated successfully.")


if __name__ == "__main__":
    main()
