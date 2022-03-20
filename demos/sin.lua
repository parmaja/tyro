window()
log("test")
canvas.text(10, 10, "This is a Sin wave")
canvas.color = colors.white
m = canvas.height / 2
x = 0
console.show();
console.print("hello");
music.mml("abc");
canvas.clear()
canvas.point(x, m)
while x < canvas.width do
     y = math.sin(math.rad(x)) * 100
    canvas.line(x , m + y)
    sleep(10)
    x = x + 1
end
