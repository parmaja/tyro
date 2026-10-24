"""Generate pixel art sprites for games/matrix/matrix.tyro.

Run from repo root: python games/matrix/make_matrix_assets.py
Produces rabbit.png, pill_red.png, pill_blue.png, crosshair.png, sentinel.png, phone.png, agent.png
"""
import os
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))

def save(name, im):
    path = os.path.join(HERE, name)
    im.save(path)
    print(f"  wrote {name} ({im.size[0]}x{im.size[1]})")

def new(w, h):
    return Image.new("RGBA", (w, h), (0, 0, 0, 0))

def rabbit():
    # 28x26 pixel art white rabbit
    im = new(28, 26)
    d = ImageDraw.Draw(im)
    WHITE = (245, 245, 255, 255)
    SHADOW = (185, 195, 215, 255)
    PINK = (255, 175, 195, 255)
    EYE = (40, 20, 25, 255)
    OUTLINE = (60, 80, 70, 200)

    # Body
    d.ellipse([(6, 11), (22, 23)], fill=WHITE, outline=OUTLINE)
    # Body shadow
    d.arc([(6, 11), (22, 23)], start=40, end=140, fill=SHADOW, width=2)
    # Fluffy tail
    d.ellipse([(2, 14), (8, 20)], fill=WHITE, outline=OUTLINE)
    # Head
    d.ellipse([(14, 7), (26, 18)], fill=WHITE, outline=OUTLINE)
    # Paws
    d.ellipse([(9, 21), (15, 25)], fill=WHITE, outline=OUTLINE)
    d.ellipse([(17, 21), (23, 25)], fill=WHITE, outline=OUTLINE)
    # Left Ear
    d.polygon([(16, 10), (13, 1), (17, 0), (20, 9)], fill=WHITE, outline=OUTLINE)
    d.polygon([(15, 8), (14, 2), (16, 2), (18, 7)], fill=PINK)
    # Right Ear
    d.polygon([(19, 9), (19, 0), (23, 0), (23, 8)], fill=WHITE, outline=OUTLINE)
    d.polygon([(20, 7), (20, 1), (22, 1), (22, 6)], fill=PINK)
    # Eye
    d.ellipse([(22, 10), (24, 12)], fill=EYE)
    d.point((23, 10), fill=(255, 255, 255, 255)) # glint
    # Nose
    d.point((26, 13), fill=PINK)
    return im

def pill(color_theme="red"):
    # 26x14 3D capsule pill
    im = new(26, 14)
    d = ImageDraw.Draw(im)

    if color_theme == "red":
        main_c = (220, 35, 45, 255)
        bright_c = (255, 100, 110, 255)
        dark_c = (140, 15, 25, 255)
        white_c = (245, 245, 250, 255)
        gray_c = (180, 185, 195, 255)
    else:
        main_c = (35, 120, 240, 255)
        bright_c = (90, 180, 255, 255)
        dark_c = (15, 60, 150, 255)
        white_c = (245, 245, 250, 255)
        gray_c = (180, 185, 195, 255)

    # Left half: colored dome
    d.pieslice([(2, 2), (14, 12)], 90, 270, fill=main_c)
    d.rectangle([(8, 2), (13, 12)], fill=main_c)
    # Right half: white dome
    d.rectangle([(13, 2), (18, 12)], fill=white_c)
    d.pieslice([(12, 2), (24, 12)], 270, 90, fill=white_c)

    # Center seam
    d.line([(13, 2), (13, 12)], fill=(40, 40, 50, 180), width=1)

    # Highlights along top
    d.line([(5, 4), (12, 4)], fill=bright_c, width=1)
    d.line([(14, 4), (21, 4)], fill=(255, 255, 255, 255), width=1)

    # Shading along bottom
    d.line([(5, 10), (12, 10)], fill=dark_c, width=1)
    d.line([(14, 10), (21, 10)], fill=gray_c, width=1)

    # Outer outline
    outline = (20, 25, 30, 220)
    d.arc([(2, 2), (14, 12)], 90, 270, fill=outline, width=1)
    d.arc([(12, 2), (24, 12)], 270, 90, fill=outline, width=1)
    d.line([(8, 2), (18, 2)], fill=outline, width=1)
    d.line([(8, 12), (18, 12)], fill=outline, width=1)
    return im

def crosshair():
    # 32x32 targeting reticle
    im = new(32, 32)
    d = ImageDraw.Draw(im)
    G_BRIGHT = (120, 255, 160, 255)
    G_DIM = (40, 180, 80, 200)

    # 4 corner brackets
    arm = 7
    # Top-left
    d.line([(3, 3), (3 + arm, 3)], fill=G_BRIGHT, width=2)
    d.line([(3, 3), (3, 3 + arm)], fill=G_BRIGHT, width=2)
    # Top-right
    d.line([(28, 3), (28 - arm, 3)], fill=G_BRIGHT, width=2)
    d.line([(28, 3), (28, 3 + arm)], fill=G_BRIGHT, width=2)
    # Bottom-left
    d.line([(3, 28), (3 + arm, 28)], fill=G_BRIGHT, width=2)
    d.line([(3, 28), (3, 28 - arm)], fill=G_BRIGHT, width=2)
    # Bottom-right
    d.line([(28, 28), (28 - arm, 28)], fill=G_BRIGHT, width=2)
    d.line([(28, 28), (28, 28 - arm)], fill=G_BRIGHT, width=2)

    # Outer circle dots
    d.ellipse([(8, 8), (23, 23)], outline=G_DIM, width=1)

    # Center cross tick lines
    d.line([(15, 6), (15, 11)], fill=G_BRIGHT, width=1)
    d.line([(15, 20), (15, 25)], fill=G_BRIGHT, width=1)
    d.line([(6, 15), (11, 15)], fill=G_BRIGHT, width=1)
    d.line([(20, 15), (25, 15)], fill=G_BRIGHT, width=1)

    # Center dot
    d.ellipse([(14, 14), (16, 16)], fill=(255, 255, 255, 255))
    return im

def sentinel():
    # 36x28 Sentinel drone with red eyes and tentacles
    im = new(36, 28)
    d = ImageDraw.Draw(im)
    HULL = (45, 52, 60, 255)
    HULL_LIGHT = (75, 88, 100, 255)
    HULL_DARK = (25, 30, 35, 255)
    RED_GLOW = (255, 40, 50, 255)
    RED_DIM = (180, 20, 30, 255)
    TENTACLE = (50, 60, 68, 240)

    # Tentacles trailing back/down
    t_points = [
        [(8, 15), (4, 18), (1, 25)],
        [(11, 17), (8, 22), (6, 27)],
        [(15, 18), (14, 23), (12, 27)],
        [(19, 18), (20, 23), (22, 27)],
        [(23, 17), (26, 22), (28, 27)],
        [(26, 15), (30, 18), (33, 25)],
    ]
    for pts in t_points:
        d.line(pts, fill=TENTACLE, width=2)
        d.point(pts[-1], fill=(80, 95, 110, 255))

    # Main carapace (armored cephalothorax)
    d.ellipse([(7, 4), (27, 18)], fill=HULL, outline=HULL_DARK, width=1)
    # Upper crest
    d.ellipse([(10, 2), (24, 12)], fill=HULL_LIGHT)
    d.line([(11, 3), (23, 3)], fill=(120, 140, 160, 255))

    # Sensor array / glowing red cluster eyes
    eyes = [(13, 12), (17, 13), (21, 12), (15, 9), (19, 9)]
    for ex, ey in eyes:
        d.ellipse([(ex-1, ey-1), (ex+1, ey+1)], fill=RED_GLOW, outline=RED_DIM)
    return im

def agent():
    # 24x30 Agent Smith silhouette
    im = new(24, 30)
    d = ImageDraw.Draw(im)
    SUIT = (25, 28, 32, 255)
    SHIRT = (220, 225, 230, 255)
    TIE = (15, 15, 18, 255)
    SKIN = (210, 170, 140, 255)
    HAIR = (40, 32, 28, 255)
    SHADES = (10, 15, 20, 255)
    GREEN_GLINT = (80, 240, 120, 255)

    # Hair
    d.rectangle([(7, 2), (16, 7)], fill=HAIR)
    # Head / jaw
    d.rectangle([(8, 6), (15, 14)], fill=SKIN)
    # Sunglasses
    d.rectangle([(7, 8), (11, 10)], fill=SHADES)
    d.rectangle([(12, 8), (16, 10)], fill=SHADES)
    d.point((9, 8), fill=GREEN_GLINT)
    d.point((14, 8), fill=GREEN_GLINT)
    # Earpiece wire
    d.line([(16, 9), (17, 12), (17, 16)], fill=(180, 180, 190, 200), width=1)

    # Suit shoulders & torso
    d.polygon([(2, 16), (21, 16), (19, 29), (4, 29)], fill=SUIT)
    # White shirt triangle
    d.polygon([(10, 16), (13, 16), (12, 22), (11, 22)], fill=SHIRT)
    # Black tie
    d.polygon([(11, 17), (12, 17), (13, 24), (11, 27), (10, 24)], fill=TIE)
    return im

def phone():
    # 24x26 green wireframe glowing phone
    im = new(24, 26)
    d = ImageDraw.Draw(im)
    G = (60, 255, 120, 255)
    G_DIM = (20, 140, 60, 200)

    # Receiver handset
    d.ellipse([(2, 3), (8, 9)], fill=G)
    d.ellipse([(15, 3), (21, 9)], fill=G)
    d.line([(5, 5), (11, 2), (18, 5)], fill=G, width=2)
    # Base cradle
    d.polygon([(4, 12), (19, 12), (21, 23), (2, 23)], fill=G_DIM, outline=G, width=1)
    # Rotary dial / keypad
    d.ellipse([(8, 14), (15, 21)], outline=G, width=1)
    d.point((11, 17), fill=(255, 255, 255, 255))
    return im

def main():
    print("Generating Matrix sprites in", HERE)
    save("rabbit.png", rabbit())
    save("pill_red.png", pill("red"))
    save("pill_blue.png", pill("blue"))
    save("crosshair.png", crosshair())
    save("sentinel.png", sentinel())
    save("agent.png", agent())
    save("phone.png", phone())
    print("Done!")

if __name__ == "__main__":
    main()
