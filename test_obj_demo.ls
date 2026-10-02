window.show(640,480)
local b = controls.new("button", "Hi", 10,10,120,40, "btn1")
while true do
  btn1.text = "Test"
  if btn1.text == "Test" then
    print("ok")
    break
  end
  sleep(16)
end
exit()