--======================================================================
--  terminal_demo.lua - Terminal-style console demo
--======================================================================
--  Demonstrates the built-in terminal commands available in the console.
--  When the console is visible, type commands at the prompt:
--    dir, list, ls  - List files in the current directory
--    clear, cls     - Clear the console
--    help, ?        - Show available commands
--    exit, quit     - Hide console and stop
--
--  Run with:  tyro demos/terminal_demo.lua
--  Then type "dir" or "list" at the prompt.
--======================================================================

-- The console starts hidden. Show it with a 60x20 character grid.
console.show(60, 20)

print("Welcome to Tyro Terminal!")
println("Type 'help' for available commands, or 'dir' to list files.")
println("Type 'exit' to quit.")
println("")

-- Keep the script alive so the console stays interactive.
-- The main loop handles input dispatch; this while-loop just keeps
-- the script thread running.
while console.active do
  -- Draw a simple animation on the canvas while console is visible
  canvas.clear()
  canvas.color = colors.blue
  canvas.rectangle(10, 10, 100, 50, true)
  canvas.color = colors.white
  canvas.text(20, 30, "Console Active")
  sleep(50)
end
