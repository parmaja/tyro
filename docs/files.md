# Files

[<< Tyro docs index](README.md)

Scripts have no direct file system access: the Lua `io` and `os` libraries are
removed from the sandbox, so `io.open` and friends do not exist. `openfile` is
the only way in, and it hands out a handle only for a file that resolves inside
the workspace.

```lua
local f, err = openfile("scores.txt", "w")   -- relative names land next to the script
if not f then
    println("cannot write scores.txt: " .. err)
    return
end
f:write("hello\n")
f:close()
```

`openfile(name [, mode])` returns a handle, or `nil` plus a message — it never
raises an error. `mode` takes the `io.open` letters: `r` (default), `w`, `a`,
`x` and `+`, with `b` for binary. Without `b` the handle is a text handle and
translates `\n` to `\r\n` on the way out and back on the way in.

| Function | Description |
|----------|-------------|
| `openfile(name [, mode])` | Open a workspace file; returns the handle, or `nil` plus a message |
| `f:read([what])` | `"l"` a line, `"L"` a line keeping its newline, `"a"` what is left, `"n"` the next number, or a byte count. Returns `nil` at the end of the file. |
| `f:write(...)` | Write the arguments, returns `f` so calls chain. A leading number writes only that many bytes of the string after it. |
| `f:lines()` | Iterate the lines that are left: `for line in f:lines() do ... end` |
| `f:close()` | Close the handle and return `true`; further reads and writes report the file as closed |

The name is expanded before it is checked, so `..`, a drive-relative form or a
UNC path cannot walk out of the workspace:

```lua
openfile("../secrets.txt")          --> nil, "resolves outside the workspace"
openfile("sub/../data.txt")         --> fine, it stays inside
```

For the common case of keeping a table between runs, `savejson` and `loadjson`
skip the handle entirely — the same name rules apply, and a table saved is a
table found again:

```lua
savejson({ score = 4200, level = 3 }, "progress.json")
local save = loadjson("progress.json")   -- nil + a message when it is not there
```

| Function | Description |
|----------|-------------|
| `savejson(value, name)` | Write the JSON text of a value (`json_encode`) into a workspace file; returns `true`, or `nil` plus a message |
| `loadjson(name)` | Read a workspace file and parse it (`json_decode`); returns the value, or `nil` plus a message |

And when a file name is one thought too many, `save("score", 120)` /
`load("score")` keep key/value pairs in one automatic `save.json` next to the
script — see [Remembering between runs](timing.md#remembering-between-runs-save--load)
below and [`help/save.md`](../help/save.md).

See `tests/test_openfile.tyro` for a runnable check of all of it.

