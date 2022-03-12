window()
log('start' + version)
i = 100000
c = 0
print('Hello World');
print('Circle demo');
canvas.alpha = 100
while i > 0 do
    c = math.random(0, colors.count - 1)
    canvas.color = colors[c]
    canvas.alpha = 100
    r = math.random(5, 20) --size of circle
    x = math.random(canvas.width)
    y = math.random(canvas.height)
    canvas.circle(x, y, r, true)
    canvas.color = colors.black
    canvas.circle(x, y, r, false)
    sleep(1)
    i = i - 1
end
