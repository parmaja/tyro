--======================================================================
--  console_demo.lua - Console output
--======================================================================
--  Demonstrates: console.show(), print(), println(), log()
--======================================================================

console.show()

print("Hello from Tyro!")
println("This is a console demo.")
log("log() writes to console too")

-- Print some computed values
x = 10
y = 20
println("x + y = " .. (x + y))
println("Version: " .. version)
