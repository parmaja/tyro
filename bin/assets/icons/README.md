# Sprite editor icons

The sprite editor loads its toolbar, panel and tool icons from this folder
(`assets/icons/<name>.png`). `TTyroIcons` caches them and tints them at draw
time, so every PNG is white on a transparent background.

If a file is missing the editor falls back to its built in vector drawings,
so the editor keeps working without this folder.

## Source

[Bootstrap Icons](https://icons.getbootstrap.com) 1.13.2, MIT licensed.
See `LICENSE` in this folder.

Regenerate them with:

```
python assets/icons/fetch-icons.py
```

The script downloads the SVGs, recolors them white and rasterizes them to
32x32 PNG with Inkscape.

## Mapping

| File | Bootstrap Icons |
| --- | --- |
| `save.png` | `floppy` |
| `undo.png` | `arrow-return-left` |
| `redo.png` | `arrow-return-right` |
| `mirror-h.png` | `symmetry-horizontal` |
| `mirror-v.png` | `symmetry-vertical` |
| `rotate-cw.png` | `arrow-clockwise` |
| `rotate-ccw.png` | `arrow-counterclockwise` |
| `grid.png` | `grid-3x3` |
| `plus.png` | `plus-lg` |
| `minus.png` | `dash-lg` |
| `close.png` | `x-lg` |
| `play.png` | `play-fill` |
| `loop.png` | `repeat` |
| `action.png` | `collection-play` |
| `frame.png` | `image` |
| `props.png` | `sliders2` |
| `palette.png` | `palette` |
| `tools.png` | `tools` |
| `tool-select.png` | `cursor` |
| `tool-pixel.png` | `pencil` |
| `tool-pen.png` | `brush` |
| `tool-fill.png` | `paint-bucket` |
| `tool-pick.png` | `eyedropper` |
| `tool-circle.png` | `circle` |
| `tool-rect.png` | `square` |
| `tool-rotate.png` | `arrow-repeat` |
