# Right-to-left text (work in progress)

Tyro can already **draw** Arabic: Lua strings are UTF-8 end to end (the old
codepage folding was fixed), and `font.load(name, size, "arabic")` bakes the
shaped glyph forms that minibidi produces. What is still missing is **bidi
layout** — deciding where a mixed Latin/Arabic line starts and how numbers run.

Until that lands, use the small helper:

```lua
local rtl = require("assets/rtl")

font.load("assets/DejaVuSans.ttf", 24, "arabic")
canvas.font("assets/DejaVuSans.ttf", 24, "arabic") -- sugar: load + use

function draw()
  -- Reverse a line and paint it centred (or at x with a third argument).
  rtl.text(120, "مرحبا بالعالم")
  rtl.text(160, "Score: 12", 10)
end
```

`rtl.reverse(s)` reverses Unicode codepoints safely (it keeps multi-byte UTF-8
sequences intact). `rtl.text(y, s [, x])` draws the reversed line with the
active font.

This is **line reversal**, not full shaping or paragraph layout. If you only
write Arabic on its own line, it reads correctly. Mixed numbers and brackets
may still need manual ordering. See [fonts](fonts.md) and
[drawing](drawing.md) for the rest of the text API.
