local b = controls.new("button", "Hi", 10,10,120,40, "btn1")
btn1.text = "Test"
openfile("test_obj_quick.txt", "w"):write(btn1.text):close()