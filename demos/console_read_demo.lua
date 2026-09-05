--======================================================================
--  console_read_demo.lua - Interactive console.read() demo
--======================================================================
--  Demonstrates the console.read() function, which allows a Lua script
--  to prompt the user for input on the console. The script blocks until
--  the user types a line and presses Enter.
--
--  Run with:  tyro demos/console_read_demo.lua
--======================================================================

-- Draw something on the canvas while asking for input
canvas.clear()
canvas.color = colors.yellow
canvas.text(10, 10, "Lua Console Read Demo")
canvas.color = colors.white
canvas.text(10, 40, "Check the console (left side) for prompts")

-- Prompt the user for their name
local name = console.read("Your name? ")
println("Hello, " .. name .. "!")

-- Prompt for a number and do a calculation
local input = console.read("Enter a number: ")
local n = tonumber(input)
if n then
  println(name .. ", " .. n .. " squared is " .. (n * n))
else
  println("That doesn't look like a number!")
end

-- One more prompt
local color_name = console.read("What is your favorite color? ")
println("Nice! I like " .. color_name .. " too.")

println("")
println("Press ESC to hide the console and exit.")

-- Keep the script alive until console is hidden
while console.active do
  sleep(50)
end
