local f, ferr = openfile("rawio.txt", "w")
if not f then
  log("cannot open rawio.txt: " .. tostring(ferr))
  running = false
  return
end
f:write("raw-write-1\n")
f:flush()
f:write("line two\n")
f:flush()
f:close()
running = false