# Controls

[<< Tyro docs index](README.md)

Controls are created by class name and are owned by the control tree. Panels are
containers, so a control can be nested inside another control with
`controls.parent`.

```lua
panel = controls.new("panel", "", 0, 0, 240, 300)
controls.align(panel, "left")

top = controls.new("button", "Top", 0, 0, 180, 40)
controls.parent(top, panel)
controls.align(top, "top")

bottom = controls.new("button", "Bottom", 0, 0, 180, 40)
controls.parent(bottom, panel)
controls.align(bottom, "bottom")
```

A handle is also a property bag, so every row above that reads or writes a value
has a field spelling on the handle itself. The field is the value - never a
function - so these are interchangeable:

```lua
local b = controls.new("button", "Hi", 10, 10, 120, 32)

b.text = "Bye"           -- b.caption works too
b.width = 200            -- controls.width(b, 200)
b.align = "left"         -- controls.align(b, "left")
b.visible = false        -- controls.visible(b, false)
b.parent = panel         -- controls.parent(b, panel)

local w, h = b.width, b.height
```

| Field | Type | Notes |
|-------|------|-------|
| `x` / `left`, `y` / `top`, `width`, `height` | integer | Preferred geometry; writing one of them keeps the others |
| `visible`, `name`, `border`, `backcolor`, `align`, `parent` | | Same values as the functions above |
| `text` / `caption` | string | The caption of a button, label, or checkbox; the edited text of an edit |
| `hover`, `down`, `clicked`, `focused` | boolean | Read-only mouse and keyboard state |
| `checked` | boolean | A checkbox only, `false` on anything else |
| `items`, `itemindex`, `viewcount` | integer | A `listbox` only; `lst.items = {"a", "b"}` replaces the list, a single string replaces it with one item and `nil` empties it |
| `loaded` | boolean | An `image` only; `img.file = "logo.png"` loads the texture |

The names that name an action stay methods on the handle and take a colon call,
because a Lua key cannot be both a value and a function:
`lst:additem("a")`, `lst:clear()`, `lst:item(0)`, `b:show()`, `b:hide()`,
`b:move(x, y)`, `b:position(x, y)`, `b:focus()`, `img:load("logo.png")`.
The `controls` table itself only exposes base-control methods that take a handle
(`controls.new`, `controls.align`, `controls.width`, …); property spellings such
as `controls.text(handle)` or `controls.additem(handle, "a")` do not exist.

| Function | Description |
|----------|-------------|
| `controls.new(class, name, x, y, w, h)` | Create a `button`, `panel`, `label`, `checkbox`, `edit`, `spectrum`, `listbox`, or `image`; returns a handle. If `name` is a number, the name is omitted and coordinates follow. |
| `controls.align(handle [, value])` | Get/set `none`, `left`, `top`, `right`, `bottom`, or `client` |
| `controls.parent(handle [, parentHandle])` | Get/set the container; the getter returns `0` for the main window, and `nil`/`0` moves the control there |
| `controls.width/height(handle [, value])` | Get/set the preferred size |
| `controls.position/move(handle, x, y)` | Get/set the preferred position |
| `controls.visible/show/hide(handle)` | Show or hide a control; hidden aligned controls release their space |
| `controls.hover/down/clicked(handle)` | Read mouse state |
| `controls.border(handle [, style])` | `0` none, `1` thin, `2` thick, `3` sizable |
| `controls.backcolor(handle [, color])` | Set a color from the `colors` table |

`BoundsRect` stores a control's preferred position and size. Docking changes only
its effective `WindowRect`, so `controls.width`, `controls.height`, and
`controls.position` keep reporting the preferred geometry. Changing a child's
size realigns its siblings, and resizing the main window propagates through
every container level. See `demos/controls_align.tyro` for a left-docked panel
with top- and bottom-docked buttons.

## Image controls

`image` is a control that shows a texture. Its caption argument is the file to
show, so the common case is one line:

```lua
logo = controls.new("image", "logo.png", 40, 40, 128, 128)
controls.name(logo, "logo")
```

The texture is loaded on the main thread, and a texture larger than the requested
size grows the control. The file is looked up like every other script asset
(work path, then `assets`, then the app folder).

An existing control can load another file at any time, and reports whether it
holds a texture:

```lua
logo.load("other.png")  -- or logo.file = "other.png"
if not logo.loaded then
    print("logo.png could not be read")
end
logo.load("")                     -- release the texture
```

`logo.load(...)` answers `true`/`false` for the file it just read, and `false`
for a control that is not an `image`.

