"""Build a real .sprite (ZIP + PNG + JSON) for the atlas test.

Uses the standard library's zipfile (DEFLATE) so the archive is produced by an
external tool rather than raylib, which is what a real asset would look like.
Run from the repository root:

    python tests/make_real_sprite.py
"""
import json
import os
import struct
import zipfile


def png_size(path):
    with open(path, "rb") as f:
        head = f.read(24)
    # 8-byte signature, 4-byte length, 4-byte "IHDR", then width/height
    width, height = struct.unpack(">II", head[16:24])
    return width, height


def main():
    repo = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    png_path = os.path.join(repo, "bin", "assets", "Expire.png")
    out_path = os.path.join(repo, "tests", "real_atlas.sprite")

    w, h = png_size(png_path)
    fw, fh = w // 2, h // 2

    frames = [
        {
            "filename": f"tile{i}",
            "frame": {"x": (i % 2) * fw, "y": (i // 2) * fh, "w": fw, "h": fh},
            "rotated": False,
            "trimmed": False,
            "spriteSourceSize": {"x": 0, "y": 0, "w": fw, "h": fh},
            "sourceSize": {"w": fw, "h": fh},
            "duration": 80,
        }
        for i in range(4)
    ]
    descriptor = {
        "frames": frames,
        "meta": {"app": "make_real_sprite", "image": "Expire.png", "size": {"w": w, "h": h}},
    }

    with open(png_path, "rb") as f:
        png_bytes = f.read()

    with zipfile.ZipFile(out_path, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("atlas.json", json.dumps(descriptor))
        z.writestr("Expire.png", png_bytes)

    print(f"wrote {out_path} ({w}x{h}, 4 frames of {fw}x{fh})")


if __name__ == "__main__":
    main()
