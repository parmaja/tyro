# Tilemaps

[<< Tyro docs index](README.md)

The `tiles` system draws a rectangular grid of tiles from a single tileset
texture. A tilemap is created with `tiles.new`, loaded with `load`, filled with
`fill`/`set`, and drawn with `draw`. Both the dot form (`map.set(...)`) and the
colon form (`map:set(...)`) work.

Tile ids are **1-based**: id `1` is the top-left tile of the sheet, id `12` the
last tile of the first row of a 12-column sheet, and so on. Id `0` means an
empty cell, which is skipped when drawing (so a second, empty map can be used as
an overlay layer).

## Creating & Loading

| Function | Description |
|----------|-------------|
| `tiles.new("name"?)` | Create a new tilemap. The optional name registers it for lookup. |
| `tiles.find("name")` | Look up a tilemap by name (returns the map object or `nil`). |

A named tilemap is also exposed as a global, so after
`tiles.new("terrain")` you can write `terrain.set(...)` directly.

## Tilemap Methods

| Method | Description |
|--------|-------------|
| `map.load(file[, tilew, tileh, columns, spacing, margin])` | Load (or replace) the tileset texture. Defaults: `16, 16, 0, 0, 0`. Leave `columns` at `0` to derive it from the texture width. |
| `map.resize(cols, rows)` | Resize the grid, keeping the overlapping cells. |
| `map.fill(tile)` | Set every cell to `tile` (`0` clears the map). |
| `map.clear()` | Empty every cell. |
| `map.set(col, row, tile)` | Set one cell (out-of-range coordinates are ignored). |
| `map.get(col, row)` | Return the tile id of one cell (`0` when empty or out of range). |
| `map.data()` | Return a flat array of tile ids, row-major (`index = row * cols + col + 1`). |
| `map.solidtile(tile [, solid])` | Read, or set, whether a tile id is a wall. With `solid` it writes and returns nothing; without it returns a boolean. |
| `map.solid(col, row)` | `true` when the cell is a solid tile, or is outside the map. |
| `map.draw([x, y [, scale]])` | Draw the map (enqueues one native draw command). Position defaults to the map's own `x`/`y`, scale to `scale`. |

`draw` captures a snapshot of the grid at the moment it is called, so a later
`set`/`clear` does not affect a draw already queued this frame. Re-stamp an
entity layer every frame and call `draw` after the changes.

## Tilemap Properties

| Property | Type | Description |
|----------|------|-------------|
| `map.cols` | `number` | Grid width in tiles (read-only). |
| `map.rows` | `number` | Grid height in tiles (read-only). |
| `map.width` | `number` | Pixel width, `cols * tilewidth` (read-only). |
| `map.height` | `number` | Pixel height, `rows * tileheight` (read-only). |
| `map.tilewidth` | `number` | Tile width in the sheet (read-only). |
| `map.tileheight` | `number` | Tile height in the sheet (read-only). |
| `map.columns` | `number` | Tiles per row in the sheet (read-only). |
| `map.spacing` | `number` | Pixels between tiles in the sheet (read-only). |
| `map.margin` | `number` | Pixels from the sheet edge to the first tile (read-only). |
| `map.visible` | `boolean` | Whether the map is drawn (read/write). |
| `map.x` | `number` | Draw X position (read/write). |
| `map.y` | `number` | Draw Y position (read/write). |
| `map.scale` | `number` | Draw scale, 1.0 = original size (read/write). |
| `map.name` | `string` | The map's name (read/write). |
| `map.collide` | `boolean` | When `true`, every solid cell also becomes a wall in the physics world (see [Physics](physics.md)). Default `false`. |

```lua
window.show(320, 240)

-- Kenney "Tiny Dungeon": 16x16 tiles, 1px spacing, 12 columns
local map = tiles.new("ground")
map.load("tiles.png", 16, 16, 0, 1, 0)  -- columns derived from the texture
map.resize(20, 15)
map.fill(49)                             -- 49 = a floor tile
map.solidtile(41, true)                  -- 41 = a wall tile

-- draw a wall border
for c = 0, map.cols - 1 do
  map.set(c, 0, 41)
  map.set(c, map.rows - 1, 41)
end

while true do
  canvas.color(colors.black)
  canvas.clear()
  map.draw(0, 0, 2)                      -- scale 2 => 32px tiles
end
```

See `games/dungeon/dungeon.tyro` for a small game built on this API (a static
terrain map plus a second map used as an entity overlay).

