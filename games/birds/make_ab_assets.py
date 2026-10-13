"""Regenerate the Angry-Birds-style placeholder art used by slingshot.tyro.

Run from the repo root:  python games/birds/make_ab_assets.py
Requires Pillow. Produces ab_*.png next to this script.
"""
import os
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))


def save(name, im):
    im.save(os.path.join(HERE, name))


def boxed(w, h, fill, border, bw=3):
    im = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    d.rectangle([0, 0, w - 1, h - 1], fill=fill, outline=border, width=bw)
    return im


def disc(diam, fill, border, highlight=None):
    im = Image.new("RGBA", (diam, diam), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    d.ellipse([1, 1, diam - 2, diam - 2], fill=fill, outline=border, width=2)
    if highlight:
        d.ellipse([diam * 0.30, diam * 0.22, diam * 0.55, diam * 0.45], fill=highlight)
    return im


def ground():
    # The visible top band is the first 40px; everything below is buried fill
    # and the extra width extends the floor far past the play area so that
    # debris flung sideways still lands on it instead of falling off the edge.
    w, h = 3200, 240
    im = Image.new("RGBA", (w, h), (92, 64, 40, 255))  # dirt below
    d = ImageDraw.Draw(im)
    d.rectangle([0, 0, w - 1, 39], fill=(86, 150, 74, 255))       # grass
    d.rectangle([0, 0, w - 1, 2], fill=(60, 112, 52, 255))        # dark grass edge
    d.rectangle([0, 34, w - 1, 39], fill=(60, 112, 52, 255))      # dark grass edge
    return im


def main():
    save("ab_ground.png", ground())
    save("ab_plank.png", boxed(24, 120, (168, 116, 66, 255), (120, 78, 42, 255)))
    save("ab_beam.png", boxed(120, 24, (176, 124, 74, 255), (126, 84, 46, 255)))
    save("ab_beam_long.png", boxed(240, 24, (176, 124, 74, 255), (126, 84, 46, 255)))
    save("ab_block.png", boxed(40, 40, (150, 150, 158, 255), (110, 110, 120, 255)))
    save("ab_bird.png", disc(30, (214, 60, 60, 255), (150, 30, 30, 255)))
    save("ab_pig.png", disc(36, (70, 160, 70, 255), (40, 110, 40, 255),
                            highlight=(120, 200, 120, 255)))
    print("assets written to", HERE)


if __name__ == "__main__":
    main()
