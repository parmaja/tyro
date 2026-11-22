# Console

[<< Tyro docs index](README.md)

## Built-in Terminal Commands

When the graphical console is visible, you can type commands at the `> ` prompt.
The following commands are built in:

| Command | Description |
|---------|-------------|
| `list`, `ls` | List files in the current directory |
| `clear`, `cls` | Clear the console output |
| `help`, `?` | Show available commands |
| `load <script>` | Load a script from the current directory (F4 to pick one) |
| `run` | Run the loaded script (like `F5`; reruns it when it is already running) |
| `stop` | Stop the running script |
| `state` | Show which script is loaded and whether it is running |
| `edit` | Open the script editor (like `F2`) |
| `exit`, `quit` | Hide the console and stop the engine |
| `ESC` | Hide the console (keybinding) |

Example:
```
tyro demos/terminal_demo.lua
# then type "dir" at the console prompt
```

Holding a key auto-repeats it at the prompt: typed characters, `BACKSPACE`,
`DELETE`, the arrow keys and `HOME`/`END` keep repeating after a short delay.
`ENTER`, `ESC` and `CTRL` shortcuts stay one-shot.

Text at the prompt can be selected with `SHIFT`+`LEFT`/`RIGHT`/`HOME`/`END` (a
plain arrow press jumps to the edge of the selection), with `CTRL`+`A`, or by
dragging with the mouse. Typing, `BACKSPACE`, `DELETE` or pasting replaces the
selection, and it is copied/pasted with `CTRL`+`C` / `CTRL`+`V` or with the
traditional console keys `CTRL`+`INSERT` / `SHIFT`+`INSERT`.

### Console API

| Function | Description |
|----------|-------------|
| `console.show([w, h])` | Show the console (optionally sized in characters) |
| `console.print(text)` | Print text to the console (no newline) |
| `console.println(text)` | Print text to the console (with newline) |
| `console.read([prompt])` | **Block** and read a line of input from the user |
| `console.active` | Read-only boolean — `true` while the console is visible |

### Interactive Reading (console.read)

`console.read()` displays a prompt on the console and blocks the script until
the user types a line and presses Enter. It returns the typed string.

```lua
console.show()
local name = console.read("Your name? ")
println("Hello, " .. name .. "!")
```

See `demos/terminal_demo.tyro` and `demos/console_read_demo.tyro` for examples.

