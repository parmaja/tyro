#!/usr/bin/env python3
"""Fetch the sprite editor icons from Bootstrap Icons and rasterize them.

Bootstrap Icons (https://icons.getbootstrap.com) is MIT licensed, see
LICENSE in this folder. Every icon is downloaded as an SVG, recolored to
white and exported as a 32x32 PNG with a transparent background. The UI
tints them at draw time, so white is the neutral source color.

Requires Inkscape on PATH (or set INKSCAPE to the executable).

Usage:
    python assets/icons/fetch-icons.py
"""

import os
import shutil
import subprocess
import sys
import urllib.request

VERSION = "1.13.2"
BASE = "https://cdn.jsdelivr.net/npm/bootstrap-icons@" + VERSION + "/icons/"
SIZE = 32
HERE = os.path.dirname(os.path.abspath(__file__))

# output name -> Bootstrap Icons name
ICONS = {
    # sprite editor chrome
    "save": "floppy",
    "undo": "arrow-return-left",
    "redo": "arrow-return-right",
    "mirror-h": "symmetry-horizontal",
    "mirror-v": "symmetry-vertical",
    "rotate-cw": "arrow-clockwise",
    "rotate-ccw": "arrow-counterclockwise",
    "grid": "grid-3x3",
    "plus": "plus-lg",
    "minus": "dash-lg",
    "close": "x-lg",
    "play": "play-fill",
    "loop": "repeat",
    "action": "collection-play",
    "frame": "image",
    "props": "sliders2",
    "palette": "palette",
    "tools": "tools",
    # drawing tools
    "tool-select": "cursor",
    "tool-pixel": "pencil",
    "tool-pen": "brush",
    "tool-fill": "paint-bucket",
    "tool-pick": "eyedropper",
    "tool-circle": "circle",
    "tool-rect": "square",
    "tool-rotate": "arrow-repeat",
}


def find_inkscape():
    exe = os.environ.get("INKSCAPE")
    if exe and os.path.isfile(exe):
        return exe
    for name in ("inkscape", "inkscape.com", "inkscape.exe"):
        found = shutil.which(name)
        if found:
            return found
    for candidate in (
        r"C:\Program Files\Inkscape\bin\inkscape.com",
        r"C:\Program Files\Inkscape\bin\inkscape.exe",
        "/usr/bin/inkscape",
        "/usr/local/bin/inkscape",
    ):
        if os.path.isfile(candidate):
            return candidate
    sys.exit("Inkscape not found, set INKSCAPE to its executable")


def fetch(name):
    url = BASE + name + ".svg"
    with urllib.request.urlopen(url, timeout=60) as r:
        return r.read().decode("utf-8")


def main():
    inkscape = find_inkscape()
    for out_name, icon_name in ICONS.items():
        svg = fetch(icon_name)
        svg = svg.replace("currentColor", "#FFFFFF")
        tmp_svg = os.path.join(HERE, out_name + ".svg")
        with open(tmp_svg, "w", encoding="utf-8", newline="\n") as f:
            f.write(svg)
        png = os.path.join(HERE, out_name + ".png")
        subprocess.run(
            [
                inkscape,
                tmp_svg,
                "--export-type=png",
                "--export-filename=" + png,
                "--export-width=" + str(SIZE),
                "--export-height=" + str(SIZE),
                "--export-background-opacity=0",
            ],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        os.remove(tmp_svg)
        print("%-14s <- %-24s %d bytes" % (out_name, icon_name, os.path.getsize(png)))


if __name__ == "__main__":
    main()
