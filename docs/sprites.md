# Sprites

[<< Tyro docs index](README.md)

The `sprites` system manages textured images ("sprites") that the engine draws
every frame. Each sprite is created with `sprites.new`, loaded with `load`, and
positioned with `move` or by setting `x`/`y` properties.

## Creating & Loading

| Function | Description |
|----------|-------------|
| `sprites.new("name"?)` | Create a new sprite. Optional name registers it for lookup. |
| `sprites.puff("name"? [, size] [, count])` | Create a sprite carrying an **engine-built soft puff texture** (no file, like `models.sphere`): a solid core with a feathered, lumpy rim — the arcade building block for clouds and smoke. `size` is the texture in pixels (default 128). `count` above 1 builds that many puffs in one hop and returns a table of them, named `name1..nameN`. Stack a few with jittered position/scale/alpha and their soft edges fuse into one fluffy mass. |
| `sprites.blob("name"? [, size] [, count])` | The **crisp-rim twin** of `sprites.puff`: the same lumpy, wobbled silhouette, but the alpha cuts off at the coast within a pixel instead of feathering — an organic shape with a sharp edge (islands, rocks, hard-edged decor that must not look fluffy). Same `size`/`count`/batch behaviour as `sprites.puff`. |
| `sprites("name")` | Look up a sprite by name (returns the sprite object or `nil`). |
| `sprites.find("name")` | Same as above. |

## Drawing your own art

`images.*` draws pixels into a memory sheet, then saves it as a PNG or a
`.sprite` animation atlas. This is how the shipped games generate their art —
no Python, Pillow or extra module needed.

```lua
local s = images.sheet(64, 48, colors.navy)  -- new w,h [, filled colour]
images.rect(s, 0, 0, 20, 20, colors.red, true)
images.circle(s, 40, 24, 12, colors.cyan, true)
images.line(s, 0, 40, 60, 40, colors.white)
images.box(s, 2, 2, 16, 16, colors.gold, colors.white) -- fill, border
images.poly(s, { 40,2, 60,20, 40,38, 20,20 }, colors.green, colors.white)
images.save(s, "ship.png")

-- One shared sheet, count frames, f0..fN-1, saved as PNG + .sprite:
images.atlas("hero", 32, 32, 6, 120, function(sheet, i, offsetX)
  images.box(sheet, offsetX + 1, 1, 30, 30, colors.cyan, colors.white)
end)
```

Colours may be a packed integer (`rgb(...)`) or a name (`"gold"`). A sprite
can use a generated sheet with `sprite:fromimage(sheet)`.

## Sprite Methods

| Method | Description |
|--------|-------------|
| `sprite:load("image.png")` | Load a texture from file into this sprite. `.aseprite`/`.ase` files load every frame as an animation. `.sprite` files (a ZIP holding a PNG texture + a TexturePacker/Phaser/PixiJS JSON atlas) load every atlas frame as an animation frame. |
| `sprite:show()` | Make the sprite visible (shown by default). |
| `sprite:hide()` | Hide the sprite from rendering. |
| `sprite:move(x, y)` | Set the sprite's position. |
| `sprite:width()` | Return the texture width in pixels. |
| `sprite:height()` | Return the texture height in pixels. |
| `sprite:play([fps])` | Restart and play the animation. `fps` overrides the per-frame timings stored in the file (0 or omitted keeps them — needed because Aseprite files often have ~1 ms delays). |
| `sprite:stop()` | Freeze the animation on the current frame (aliased by `sprite:pause()`). |
| `sprite:framecount()` | Number of frames in the loaded `.aseprite`/`.sprite` animation. |
| `sprite:free()` | Drop the sprite now: its textures, name, physics body and script hook are released and the sprite stops drawing (aliased by `sprite:destroy()`). The old table simply stops addressing anything — reads answer `0`/`false`, writes do nothing — so freed bullets never need pooling. |

## Sprite Properties

| Property | Type | Description |
|----------|------|-------------|
| `sprite.x` | `number` | X position of the sprite's centre (the engine draws and rotates around the centre, like the physics body) (read/write). |
| `sprite.y` | `number` | Y position of the sprite's centre (read/write). |
| `sprite.angle` | `number` | Rotation in degrees (read/write). |
| `sprite.scale` | `number` | Scale factor, 1.0 = original size (read/write). |
| `sprite.visible` | `boolean` | Whether the sprite is drawn (read/write). |
| `sprite.frames` | `number` | Number of animation frames (read-only). |
| `sprite.frame` | `number` | Current animation frame, 0-based (read/write). |
| `sprite.playing` | `boolean` | Whether the animation is running (read/write). |
| `sprite.speed` | `number` | Animation speed in frames-per-second; 0 = use the file timings (read/write). |
| `sprite.looping` | `boolean` | Whether the animation loops when it reaches the last frame (read/write). |
| `sprite.flipH` | `boolean` | Mirror the texture horizontally (read/write). |
| `sprite.flipV` | `boolean` | Mirror the texture vertically (read/write). |
| `sprite.tint` | `number` | Colour multiplied over the texture, the same `$AABBGGRR` integer format as `canvas.color` / `colors.*`; white (`colors.white`) is the default (read/write). |
| `sprite.alpha` | `number` | Opacity `0..255` folded into the tint's alpha: `0` hides the sprite without touching `visible`, in-between values fade it (read/write). |
| `sprite.layer` | `number` | Draw order: every frame the sprites are drawn sorted by layer, lower layers first (behind); the default is `0`, and sprites that share a layer keep a stable order by handle (read/write). |

The layer sort makes stacking deterministic, so distant decor can sit at
`layer = -10`, the actors at `0`, and effects that must cover the action (an
explosion, a muzzle flash) at `layer = 10` — no matter in which order the
sprites were created. `sprite.tint` and `sprite.alpha` are the cheap way to
flash a hit, fade a ghost or sink an object into the blue without swapping
textures.

Image files are searched in the script directory, the workspace `sprites/`
folder, and the current directory.

```lua
window.show(640, 480)
canvas.color(colors.black)
canvas.clear()

mysprite1 = sprites.new("player")
mysprite1.load("richard-say.png")
mysprite1.show()
mysprite1.move(100, 200)

mysprite2 = sprites.new()
mysprite2.load("richard-say.png")
mysprite2.move(300, 100)
mysprite2.angle = 45
mysprite2.scale = 0.5

-- Look up by name
local p = sprites("player")
println("Sprite size: " .. p.width() .. "x" .. p.height())

local a = 0
while true do
    canvas.color(colors.black)
    canvas.clear()

    -- Engine draws all sprites automatically; just update properties
    mysprite1.move(mouse.x, mouse.y)
    mysprite1.angle = a

    a = a + 1
    if a >= 360 then a = 0 end

    sleep(16)
end
```

