#!/usr/bin/env python3
"""
Zonix - Asset Generator for Tyro Game Engine
Creates high-quality pixel-art sprites (.sprite atlases + .png) and sound effects (.wav).
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

random.seed(4242)

# Color palettes
LINE_DARK = (10, 14, 28, 255)
CYBER_CYAN = (0, 240, 255, 255)
CYBER_BLUE = (30, 110, 240, 255)
NEON_PINK = (255, 30, 140, 255)
NEON_PURPLE = (180, 50, 240, 255)
NEON_YELLOW = (255, 230, 40, 255)
NEON_GREEN = (40, 255, 120, 255)
NEON_RED = (255, 50, 60, 255)
NEON_ORANGE = (255, 130, 20, 255)
WHITE = (255, 255, 255, 255)

def new_img(w, h):
    img = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    return img, ImageDraw.Draw(img)

def sheet(frames, tw, th, name, duration=100):
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
# 1. Player Cutter Craft (cutter.sprite)
# -----------------------------------------------------------------------------
# 8 frames:
# 0: Right (cruise), 1: Down (cruise), 2: Left (cruise), 3: Up (cruise)
# 4: Right (cutting), 5: Down (cutting), 6: Left (cutting), 7: Up (cutting)
def make_cutter_frame(direction, cutting=False):
    w, h = 24, 24
    img, dr = new_img(w, h)
    cx, cy = 12, 12

    # Draw body facing right first, then rotate
    base_img, bdr = new_img(w, h)
    
    body_color = (40, 180, 255, 255) if not cutting else (255, 120, 40, 255)
    core_color = (200, 245, 255, 255) if not cutting else (255, 240, 150, 255)
    wing_color = (20, 90, 180, 255) if not cutting else (180, 50, 20, 255)
    glow_color = CYBER_CYAN if not cutting else NEON_YELLOW

    # Main fuselage (sleek wedge pointing right)
    bdr.polygon([(cx + 8, cy), (cx - 6, cy - 7), (cx - 3, cy), (cx - 6, cy + 7)], fill=body_color, outline=LINE_DARK)
    # Side wings
    bdr.polygon([(cx - 2, cy - 2), (cx - 8, cy - 9), (cx - 5, cy - 2)], fill=wing_color, outline=LINE_DARK)
    bdr.polygon([(cx - 2, cy + 2), (cx - 8, cy + 9), (cx - 5, cy + 2)], fill=wing_color, outline=LINE_DARK)
    # Cockpit / core crystal
    bdr.polygon([(cx + 4, cy), (cx - 1, cy - 3), (cx - 2, cy), (cx - 1, cy + 3)], fill=core_color)
    
    # Engine thruster flame
    flame_len = 5 if not cutting else 8
    flame_col = (100, 220, 255, 255) if not cutting else (255, 80, 30, 255)
    bdr.polygon([(cx - 4, cy - 2), (cx - 4 - flame_len, cy), (cx - 4, cy + 2)], fill=flame_col)
    bdr.polygon([(cx - 4, cy - 1), (cx - 3 - flame_len // 2, cy), (cx - 4, cy + 1)], fill=WHITE)

    # Cutting laser emitter on nose
    if cutting:
        bdr.ellipse([cx + 7, cy - 2, cx + 11, cy + 2], fill=WHITE, outline=NEON_YELLOW)
        # Laser sparks
        bdr.point([(cx + 11, cy - 3), (cx + 12, cy + 1), (cx + 10, cy + 3)], fill=NEON_YELLOW)

    # Rotate according to direction: 0: Right, 1: Down, 2: Left, 3: Up
    angles = {0: 0, 1: -90, 2: 180, 3: 90}
    rotated = base_img.rotate(angles[direction])
    return rotated

def build_cutter():
    frames = []
    for d in range(4):
        frames.append(make_cutter_frame(d, cutting=False))
    for d in range(4):
        frames.append(make_cutter_frame(d, cutting=True))
    sheet(frames, 24, 24, "cutter", duration=120)

# -----------------------------------------------------------------------------
# 2. Quantum Orb / Internal Bouncer (orb.sprite)
# -----------------------------------------------------------------------------
# 48x48 quad with 24x24 centered orb and transparent margin so fragment shader
# can generate dynamic radial light in radius size x2 around the orb.
def build_orb():
    w, h = 48, 48
    frames = []
    cx, cy = 24, 24
    n_frames = 6
    for i in range(n_frames):
        img, dr = new_img(w, h)
        angle = (i / n_frames) * 2 * math.pi
        pulse = 0.5 + 0.5 * math.sin(angle * 2)

        # Outer energy ring with rotation
        r_outer = 9
        dr.ellipse([cx - r_outer, cy - r_outer, cx + r_outer, cy + r_outer], outline=(0, 180, 240, 180), width=1)

        # Rotating electron / energy nodes
        for node in range(3):
            na = angle + node * (2 * math.pi / 3)
            nx = cx + int(math.cos(na) * 7.5)
            ny = cy + int(math.sin(na) * 7.5)
            dr.ellipse([nx - 2, ny - 2, nx + 2, ny + 2], fill=CYBER_CYAN, outline=LINE_DARK)

        # Inner pulsing core
        r_core = 5 + int(pulse * 1.5)
        dr.ellipse([cx - r_core, cy - r_core, cx + r_core, cy + r_core], fill=(20, 140, 255, 255), outline=LINE_DARK)
        dr.ellipse([cx - 2, cy - 2, cx + 2, cy + 2], fill=WHITE)
        frames.append(img)
    sheet(frames, 48, 48, "orb", duration=90)

# -----------------------------------------------------------------------------
# 3. Fast Red Seeker Drone (fast_orb.sprite)
# -----------------------------------------------------------------------------
def build_fast_orb():
    w, h = 48, 48
    frames = []
    cx, cy = 24, 24
    n_frames = 4
    for i in range(n_frames):
        img, dr = new_img(w, h)
        angle = (i / n_frames) * 2 * math.pi

        # 4 spiked tips rotating
        spikes = []
        for s in range(4):
            sa = angle + s * (math.pi / 2)
            tip_x = cx + math.cos(sa) * 10
            tip_y = cy + math.sin(sa) * 10
            base1_x = cx + math.cos(sa - 0.4) * 4
            base1_y = cy + math.sin(sa - 0.4) * 4
            base2_x = cx + math.cos(sa + 0.4) * 4
            base2_y = cy + math.sin(sa + 0.4) * 4
            dr.polygon([(tip_x, tip_y), (base1_x, base1_y), (base2_x, base2_y)], fill=NEON_RED, outline=LINE_DARK)

        # Danger core
        dr.ellipse([cx - 5, cy - 5, cx + 5, cy + 5], fill=NEON_ORANGE, outline=LINE_DARK)
        dr.ellipse([cx - 2, cy - 2, cx + 2, cy + 2], fill=NEON_YELLOW)
        frames.append(img)
    sheet(frames, 48, 48, "fast_orb", duration=70)

# -----------------------------------------------------------------------------
# 4. Boss Pulsar Core (boss_core.sprite)
# -----------------------------------------------------------------------------
def build_boss_core():
    w, h = 64, 64
    frames = []
    cx, cy = 32, 32
    n_frames = 6
    for i in range(n_frames):
        img, dr = new_img(w, h)
        angle = (i / n_frames) * 2 * math.pi
        pulse = 0.5 + 0.5 * math.sin(angle)

        # Heavy outer armored octagonal ring
        r_outer = 13
        points = []
        for pt in range(8):
            pa = angle + pt * (math.pi / 4)
            points.append((cx + math.cos(pa) * r_outer, cy + math.sin(pa) * r_outer))
        dr.polygon(points, fill=(70, 20, 110, 255), outline=NEON_PURPLE)

        # Inner spinning teeth
        for t_idx in range(4):
            ta = -angle * 1.5 + t_idx * (math.pi / 2)
            tx = cx + math.cos(ta) * 9
            ty = cy + math.sin(ta) * 9
            dr.ellipse([tx - 2, ty - 2, tx + 2, ty + 2], fill=NEON_YELLOW)

        # Core reactor
        r_core = 6 + int(pulse * 2)
        dr.ellipse([cx - r_core, cy - r_core, cx + r_core, cy + r_core], fill=NEON_PINK, outline=WHITE)
        dr.ellipse([cx - 2, cy - 2, cx + 2, cy + 2], fill=WHITE)
        frames.append(img)
    sheet(frames, 64, 64, "boss_core", duration=100)

# -----------------------------------------------------------------------------
# 5. Border Spark / Snarler (spark.sprite)
# -----------------------------------------------------------------------------
def build_spark():
    w, h = 20, 20
    frames = []
    cx, cy = 10, 10
    n_frames = 4
    for i in range(n_frames):
        img, dr = new_img(w, h)
        angle = (i / n_frames) * 2 * math.pi

        # 4 electrical arcs
        for a_idx in range(4):
            aa = angle + a_idx * (math.pi / 2)
            p1 = (cx, cy)
            p2 = (cx + math.cos(aa + 0.3) * 5, cy + math.sin(aa + 0.3) * 5)
            p3 = (cx + math.cos(aa - 0.2) * 8, cy + math.sin(aa - 0.2) * 8)
            dr.line([p1, p2, p3], fill=NEON_YELLOW, width=2)
            dr.point(p3, fill=WHITE)

        dr.ellipse([cx - 3, cy - 3, cx + 3, cy + 3], fill=WHITE, outline=NEON_ORANGE)
        frames.append(img)
    sheet(frames, 20, 20, "spark", duration=60)

# -----------------------------------------------------------------------------
# 6. Fuse Flame Spark (fuse.sprite)
# -----------------------------------------------------------------------------
def build_fuse():
    w, h = 16, 16
    frames = []
    cx, cy = 8, 8
    n_frames = 4
    for i in range(n_frames):
        img, dr = new_img(w, h)
        r = 5 + (i % 2)
        dr.ellipse([cx - r, cy - r, cx + r, cy + r], fill=NEON_ORANGE, outline=NEON_RED)
        dr.ellipse([cx - 2, cy - 2, cx + 2, cy + 2], fill=WHITE)
        # Spark rays
        for ray in range(4):
            ra = (i * 0.4) + ray * (math.pi / 2)
            dr.point((cx + int(math.cos(ra) * 6), cy + int(math.sin(ra) * 6)), fill=NEON_YELLOW)
        frames.append(img)
    sheet(frames, 16, 16, "fuse", duration=80)

# -----------------------------------------------------------------------------
# 7. Power-ups (powerups.sprite)
# -----------------------------------------------------------------------------
# 0: Freeze, 1: Speed, 2: Slow, 3: Score, 4: Life, 5: Bomb
def build_powerups():
    w, h = 24, 24
    frames = []
    cx, cy = 12, 12

    def power_bg(dr, border_col):
        # Hexagonal badge
        r = 10
        pts = [(cx + math.cos(a) * r, cy + math.sin(a) * r) for a in [i * math.pi / 3 for i in range(6)]]
        dr.polygon(pts, fill=(15, 20, 35, 240), outline=border_col)

    # 0: Freeze (Snowflake)
    f0, d0 = new_img(w, h)
    power_bg(d0, CYBER_CYAN)
    for a in [0, math.pi / 3, 2 * math.pi / 3]:
        d0.line([(cx - math.cos(a) * 6, cy - math.sin(a) * 6), (cx + math.cos(a) * 6, cy + math.sin(a) * 6)], fill=CYBER_CYAN, width=2)
    d0.ellipse([cx - 2, cy - 2, cx + 2, cy + 2], fill=WHITE)
    frames.append(f0)

    # 1: Speed (Lightning bolt)
    f1, d1 = new_img(w, h)
    power_bg(d1, NEON_YELLOW)
    bolt = [(cx + 2, cy - 7), (cx - 4, cy), (cx + 1, cy), (cx - 2, cy + 7), (cx + 4, cy - 1), (cx - 1, cy - 1)]
    d1.polygon(bolt, fill=NEON_YELLOW, outline=WHITE)
    frames.append(f1)

    # 2: Slow (Hourglass / Clock)
    f2, d2 = new_img(w, h)
    power_bg(d2, NEON_PURPLE)
    hg = [(cx - 5, cy - 6), (cx + 5, cy - 6), (cx, cy), (cx + 5, cy + 6), (cx - 5, cy + 6), (cx, cy)]
    d2.polygon(hg, fill=NEON_PURPLE, outline=WHITE)
    d2.point([(cx, cy - 2), (cx, cy + 2)], fill=WHITE)
    frames.append(f2)

    # 3: Score (Diamond Gem)
    f3, d3 = new_img(w, h)
    power_bg(d3, NEON_GREEN)
    gem = [(cx, cy - 6), (cx + 6, cy - 1), (cx, cy + 7), (cx - 6, cy - 1)]
    d3.polygon(gem, fill=NEON_GREEN, outline=WHITE)
    d3.polygon([(cx, cy - 3), (cx + 3, cy), (cx, cy + 3), (cx - 3, cy)], fill=WHITE)
    frames.append(f3)

    # 4: Life (Cyber Heart)
    f4, d4 = new_img(w, h)
    power_bg(d4, NEON_RED)
    # Heart shape
    d4.polygon([(cx, cy + 6), (cx - 6, cy - 1), (cx - 4, cy - 5), (cx, cy - 2), (cx + 4, cy - 5), (cx + 6, cy - 1)], fill=NEON_RED, outline=WHITE)
    frames.append(f4)

    # 5: Bomb (Nova / Blast)
    f5, d5 = new_img(w, h)
    power_bg(d5, NEON_ORANGE)
    d5.ellipse([cx - 5, cy - 5, cx + 5, cy + 5], fill=NEON_ORANGE, outline=WHITE)
    for sp in range(8):
        sa = sp * math.pi / 4
        d5.point((cx + int(math.cos(sa) * 7), cy + int(math.sin(sa) * 7)), fill=WHITE)
    frames.append(f5)

    sheet(frames, 24, 24, "powerups", duration=500)

# -----------------------------------------------------------------------------
# 8. Explosion (boom.sprite)
# -----------------------------------------------------------------------------
def build_boom():
    w, h = 32, 32
    frames = []
    cx, cy = 16, 16
    n_frames = 8
    for i in range(n_frames):
        img, dr = new_img(w, h)
        frac = (i + 1) / n_frames
        r_inner = int(10 * frac)
        r_outer = int(14 * frac)

        # Debris particles
        for p in range(12):
            pa = p * (2 * math.pi / 12) + (i * 0.2)
            dist = r_outer * (0.8 + 0.4 * ((p % 3) / 2))
            px = cx + math.cos(pa) * dist
            py = cy + math.sin(pa) * dist
            dr.point((px, py), fill=WHITE if i < 4 else NEON_ORANGE)

        # Expanding shockwave ring
        if i < 6:
            alpha = int(255 * (1 - frac))
            dr.ellipse([cx - r_inner, cy - r_inner, cx + r_inner, cy + r_inner], outline=(255, 200, 50, alpha), width=2)
        if i < 4:
            dr.ellipse([cx - 4 + i, cy - 4 + i, cx + 4 - i, cy + 4 - i], fill=WHITE)
        frames.append(img)
    sheet(frames, 32, 32, "boom", duration=50)

# -----------------------------------------------------------------------------
# 9. Trail Spark (trail_spark.sprite)
# -----------------------------------------------------------------------------
def build_trail_spark():
    w, h = 12, 12
    frames = []
    cx, cy = 6, 6
    n_frames = 4
    for i in range(n_frames):
        img, dr = new_img(w, h)
        r = 3 + (i % 2)
        dr.ellipse([cx - r, cy - r, cx + r, cy + r], fill=CYBER_CYAN)
        dr.ellipse([cx - 1, cy - 1, cx + 1, cy + 1], fill=WHITE)
        frames.append(img)
    sheet(frames, 12, 12, "trail_spark", duration=60)

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

    # 1. cut.wav (Laser cutting hum)
    duration = 0.12
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.sin(math.pi * (t / duration))
        # Sawtooth + sine laser buzz
        phase = 2 * math.pi * 320 * t
        s = (math.sin(phase) * 0.6 + ((phase % (2 * math.pi)) / math.pi - 1.0) * 0.4) * env * 0.7
        samples.append(s)
    write_wav("cut.wav", samples, SR)

    # 2. claim.wav (Area captured arpeggio fanfare)
    duration = 0.45
    n = int(SR * duration)
    samples = []
    notes = [523.25, 659.25, 783.99, 1046.50, 1318.51] # C5, E5, G5, C6, E6
    for i in range(n):
        t = i / SR
        note_idx = min(len(notes) - 1, int(t / (duration / len(notes))))
        freq = notes[note_idx]
        note_t = t - note_idx * (duration / len(notes))
        env = math.exp(-note_t * 18.0)
        s = (math.sin(2 * math.pi * freq * t) * 0.7 + math.sin(4 * math.pi * freq * t) * 0.3) * env * 0.8
        samples.append(s)
    write_wav("claim.wav", samples, SR)

    # 3. hit.wav (Electric zap hit)
    duration = 0.20
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.exp(-t * 22.0)
        freq = 800 * math.exp(-t * 15.0)
        noise = (random.random() * 2 - 1) * 0.4
        s = (math.sin(2 * math.pi * freq * t) * 0.6 + noise) * env * 0.9
        samples.append(s)
    write_wav("hit.wav", samples, SR)

    # 4. boom.wav (Deep explosion)
    duration = 0.50
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.exp(-t * 7.0)
        freq = 120 * math.exp(-t * 6.0)
        noise = (random.random() * 2 - 1) * 0.6
        s = (math.sin(2 * math.pi * freq * t) * 0.5 + noise) * env * 0.95
        samples.append(s)
    write_wav("boom.wav", samples, SR)

    # 5. freeze.wav (Crystal freeze sound)
    duration = 0.35
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = (1 - math.exp(-t * 500)) * math.exp(-t * 8.0)
        # High glittering harmonics
        s = (math.sin(2 * math.pi * 1760 * t) * 0.4 + 
             math.sin(2 * math.pi * 2349 * t) * 0.3 + 
             math.sin(2 * math.pi * 3135 * t) * 0.3) * env * 0.8
        samples.append(s)
    write_wav("freeze.wav", samples, SR)

    # 6. powerup.wav (Ascending powerup chime)
    duration = 0.28
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.exp(-t * 6.0)
        freq = 440 + 800 * (t / duration)
        s = (math.sin(2 * math.pi * freq * t) * 0.7 + math.sin(4 * math.pi * freq * t) * 0.3) * env * 0.8
        samples.append(s)
    write_wav("powerup.wav", samples, SR)

    # 7. alarm.wav (Fuse danger alarm)
    duration = 0.15
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.sin(math.pi * (t / duration))
        freq = 1200 if (i % 500 < 250) else 900
        s = math.sin(2 * math.pi * freq * t) * env * 0.75
        samples.append(s)
    write_wav("alarm.wav", samples, SR)

    # 8. clear.wav (Level complete victory fanfare)
    duration = 0.75
    n = int(SR * duration)
    samples = []
    c_notes = [523.25, 659.25, 783.99, 1046.50, 1318.51, 1567.98]
    for i in range(n):
        t = i / SR
        note_idx = min(len(c_notes) - 1, int(t / (duration / len(c_notes))))
        freq = c_notes[note_idx]
        note_t = t - note_idx * (duration / len(c_notes))
        env = math.exp(-note_t * 12.0)
        s = (math.sin(2 * math.pi * freq * t) * 0.65 + math.sin(4 * math.pi * freq * t) * 0.35) * env * 0.85
        samples.append(s)
    write_wav("clear.wav", samples, SR)

    # 9. over.wav (Game over sad melody)
    duration = 0.80
    n = int(SR * duration)
    samples = []
    sad_notes = [440.0, 415.3, 392.0, 349.2, 329.6]
    for i in range(n):
        t = i / SR
        note_idx = min(len(sad_notes) - 1, int(t / (duration / len(sad_notes))))
        freq = sad_notes[note_idx]
        note_t = t - note_idx * (duration / len(sad_notes))
        env = math.exp(-note_t * 8.0)
        s = (math.sin(2 * math.pi * freq * t) * 0.7 + math.sin(2 * math.pi * freq * 1.5 * t) * 0.2) * env * 0.8
        samples.append(s)
    write_wav("over.wav", samples, SR)

    # 10. start.wav (Game start chime)
    duration = 0.40
    n = int(SR * duration)
    samples = []
    for i in range(n):
        t = i / SR
        env = math.exp(-t * 7.0)
        freq = 330 * (1 + 2.0 * (t / duration))
        s = math.sin(2 * math.pi * freq * t) * env * 0.8
        samples.append(s)
    write_wav("start.wav", samples, SR)

def main():
    print("Building Zonix sprites...")
    build_cutter()
    build_orb()
    build_fast_orb()
    build_boss_core()
    build_spark()
    build_fuse()
    build_powerups()
    build_boom()
    build_trail_spark()

    print("Building Zonix sounds...")
    make_sounds()

    # Copy zonix_data.tyro to tests/ so tests/test_zonix_data.tyro can require it
    src_data = os.path.join(HERE, "zonix_data.tyro")
    dst_data = os.path.join(os.path.dirname(HERE), "..", "tests", "zonix_data.tyro")
    if os.path.exists(src_data):
        shutil.copyfile(src_data, dst_data)
        print("Copied zonix_data.tyro to tests/")

    print("Done! All Zonix assets generated successfully.")

if __name__ == "__main__":
    main()
