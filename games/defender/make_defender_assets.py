"""Regenerate the pixel art used by games/defender/defender.tyro.

Run from the repo root:  python games/defender/make_defender_assets.py
Requires Pillow. Produces the def_*.png files next to this script.

The art is drawn procedurally (no external downloads) so the game is fully
self-contained and reproducible. Everything is authored at native pixel size;
the sprites are small so the game keeps the chunky arcade look.
"""
import math
import os
import random

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))


def save(name, im):
    im.save(os.path.join(HERE, name))
    print("  wrote", name)


def new(w, h):
    return Image.new("RGBA", (w, h), (0, 0, 0, 0))


def poly(im, pts, fill, outline=None):
    d = ImageDraw.Draw(im)
    d.polygon(pts, fill=fill)
    if outline is not None:
        d.line(list(pts) + [pts[0]], fill=outline, width=1)


def ellipse(im, box, fill, outline=None):
    ImageDraw.Draw(im).ellipse(box, fill=fill, outline=outline, width=1)


def rect(im, box, fill):
    ImageDraw.Draw(im).rectangle(box, fill=fill)


def px(im, x, y, c):
    im.putpixel((x, y), c)


# ---------------------------------------------------------------- sprites ----

def ship():
    # Sleek wedge pointed right, cockpit bump, engine glow at the back.
    im = new(34, 14)
    poly(im, [(33, 7), (10, 1), (2, 4), (0, 7), (2, 10), (10, 13)],
         (210, 225, 240, 255), (90, 110, 140, 255))
    poly(im, [(12, 5), (24, 6), (24, 8), (12, 9)], (60, 130, 220, 255))
    rect(im, (0, 6, 5, 8), (255, 200, 90, 255))
    rect(im, (2, 5, 6, 5), (255, 240, 180, 255))
    rect(im, (2, 9, 6, 9), (255, 240, 180, 255))
    return im


def lander():
    # Green hostile saucer: dome + hull + two landing legs.
    im = new(22, 18)
    ellipse(im, (5, 0, 16, 9), (120, 200, 120, 255), (40, 110, 50, 255))
    poly(im, [(0, 9), (21, 9), (17, 14), (4, 14)], (70, 160, 80, 255), (30, 90, 40, 255))
    rect(im, (3, 15, 4, 17), (40, 110, 50, 255))
    rect(im, (17, 15, 18, 17), (40, 110, 50, 255))
    px(im, 8, 4, (230, 255, 230, 255))
    px(im, 13, 4, (230, 255, 230, 255))
    return im


def mutant():
    # An abducted human mutated into an angry red flier.
    im = new(22, 18)
    ellipse(im, (6, 2, 15, 11), (220, 70, 70, 255), (130, 20, 20, 255))
    poly(im, [(0, 4), (6, 7), (6, 10), (0, 13)], (170, 40, 40, 255), (110, 20, 20, 255))
    poly(im, [(21, 4), (15, 7), (15, 10), (21, 13)], (170, 40, 40, 255), (110, 20, 20, 255))
    px(im, 8, 6, (255, 230, 120, 255))
    px(im, 13, 6, (255, 230, 120, 255))
    rect(im, (8, 13, 13, 15), (150, 100, 60, 255))  # dangling body
    return im


def bomber():
    # Purple heavy that drops mines.
    im = new(24, 16)
    poly(im, [(23, 8), (6, 1), (0, 5), (0, 11), (6, 15)], (170, 110, 210, 255),
         (90, 50, 130, 255))
    rect(im, (6, 6, 14, 10), (110, 60, 160, 255))
    rect(im, (0, 7, 4, 9), (255, 190, 90, 255))
    px(im, 17, 6, (240, 230, 255, 255))
    px(im, 17, 10, (240, 230, 255, 255))
    return im


def pod():
    # Yellow pod that releases swarmers when shot.
    im = new(18, 18)
    ellipse(im, (0, 0, 17, 17), (230, 200, 70, 255), (150, 120, 20, 255))
    for x, y in [(5, 5), (12, 5), (5, 12), (12, 12), (8, 8)]:
        px(im, x, y, (120, 90, 10, 255))
    return im


def swarmer():
    # Tiny fast escort released by pods.
    im = new(14, 10)
    poly(im, [(13, 5), (3, 0), (0, 3), (0, 7), (3, 10)], (180, 240, 140, 255),
         (80, 150, 60, 255))
    px(im, 7, 5, (255, 80, 80, 255))
    return im


def human():
    # The astronaut landers try to carry off.
    im = new(10, 18)
    ellipse(im, (2, 0, 7, 5), (230, 230, 230, 255), (140, 140, 150, 255))  # helmet
    rect(im, (2, 6, 7, 12), (235, 235, 235, 255))                          # suit
    rect(im, (2, 6, 7, 7), (80, 150, 230, 255))                            # chest
    rect(im, (1, 7, 1, 11), (200, 200, 200, 255))
    rect(im, (8, 7, 8, 11), (200, 200, 200, 255))
    rect(im, (2, 13, 3, 17), (150, 150, 160, 255))
    rect(im, (6, 13, 7, 17), (150, 150, 160, 255))
    px(im, 3, 2, (60, 60, 70, 255))
    px(im, 6, 2, (60, 60, 70, 255))
    return im


def bullet():
    # A crisp laser bolt.
    im = new(12, 4)
    rect(im, (0, 1, 11, 2), (120, 230, 255, 255))
    rect(im, (8, 0, 11, 3), (240, 255, 255, 255))
    return im


def bomb():
    im = new(8, 8)
    ellipse(im, (0, 0, 7, 7), (255, 120, 60, 255), (150, 50, 10, 255))
    px(im, 3, 3, (255, 230, 180, 255))
    return im


def boom_frames():
    # A four frame expanding fireball ring.
    frames = []
    for i in range(4):
        im = new(32, 32)
        r = 5 + i * 4
        c = 16
        box = (c - r, c - r, c + r, c + r)
        if i < 3:
            ellipse(im, box, None, (255, 240 - i * 40, 80, 255))
        else:
            ellipse(im, box, (255, 210, 120, 255), (255, 120, 40, 255))
        ellipse(im, (c - r // 2, c - r // 2, c + r // 2, c + r // 2),
                (255, 160, 60, 255))
        frames.append(im)
    return frames


# ----------------------------------------------------------- backgrounds ----

def star_field(w, h, count, seed, big=0):
    random.seed(seed)
    im = new(w, h)
    for _ in range(count):
        x = random.randint(1, w - 2)
        y = random.randint(1, h - 2)
        b = random.randint(120, 255)
        px(im, x, y, (b, b, b, 255))
        if big and random.random() < 0.15:
            px(im, x + 1, y, (b, b, b, 255))
            px(im, x, y + 1, (b, b, b, 255))
    return im


def nebula(w, h, seed):
    random.seed(seed)
    im = new(w, h)
    d = ImageDraw.Draw(im)
    for _ in range(26):
        cx = random.randint(0, w)
        cy = random.randint(0, h)
        r = random.randint(30, 90)
        col = random.choice([(60, 40, 110), (40, 70, 120), (90, 40, 90), (30, 90, 110)])
        for step in range(3):
            rr = r - step * 6
            if rr <= 0:
                break
            a = 40 + step * 34
            # Keep the cloud inside the tile so it repeats seamlessly.
            d.ellipse((cx - rr, cy - rr, cx + rr, cy + rr), fill=col + (a,))
    return im


def ridge(w, h, seed, base, amp, color, outline):
    """A horizontally seamless rocky ridge silhouette."""
    random.seed(seed)
    im = new(w, h)
    d = ImageDraw.Draw(im)
    phases = [(random.uniform(0, math.tau), random.randint(1, 4),
               random.uniform(0.4, 1.0)) for _ in range(4)]
    top = []
    for x in range(w):
        u = x / w * math.tau
        y = 0.0
        # Only integer harmonics of the tile width keep the left and right
        # edges at the same height, which is what makes the tile seamless.
        for ph, k, wgt in phases:
            y += math.sin(u * k + ph) * wgt
        y = y / sum(p[2] for p in phases)          # -> [-1, 1]
        yy = int(base - (y * 0.5 + 0.5) * amp)
        yy = max(1, min(h - 1, yy))
        top.append(yy)
    for x in range(w):
        d.line([(x, top[x]), (x, h - 1)], fill=color)
        d.point((x, top[x]), fill=outline)
    return im


def ground(w, h, seed):
    """A seamless rocky crust strip; the top 3px is the lit surface."""
    random.seed(seed)
    im = new(w, h)
    rect(im, (0, 0, w - 1, h - 1), (74, 58, 46, 255))
    for _ in range(w * 4):
        x = random.randint(0, w - 1)
        y = random.randint(3, h - 1)
        shade = random.randint(-18, 18)
        base = (74 + shade, 58 + shade, 46 + shade, 255)
        px(im, x, y, base)
    rect(im, (0, 0, w - 1, 2), (120, 96, 66, 255))
    rect(im, (0, 0, w - 1, 0), (156, 128, 88, 255))
    return im


def main():
    print("defender sprites:")
    save("def_ship.png", ship())
    save("def_lander.png", lander())
    save("def_mutant.png", mutant())
    save("def_bomber.png", bomber())
    save("def_pod.png", pod())
    save("def_swarmer.png", swarmer())
    save("def_human.png", human())
    save("def_bullet.png", bullet())
    save("def_bomb.png", bomb())
    for i, f in enumerate(boom_frames()):
        save("def_boom%d.png" % i, f)

    print("defender backgrounds:")
    save("def_stars.png", star_field(512, 512, 240, 1, big=1))
    save("def_nebula.png", nebula(512, 512, 2))
    save("def_mountains.png", ridge(512, 200, 3, base=170, amp=150,
                                    color=(46, 40, 70, 255),
                                    outline=(70, 62, 100, 255)))
    save("def_hills.png", ridge(512, 140, 4, base=110, amp=100,
                                color=(52, 68, 58, 255),
                                outline=(84, 104, 86, 255)))
    save("def_ground.png", ground(512, 56, 5))
    print("done ->", HERE)


if __name__ == "__main__":
    main()
