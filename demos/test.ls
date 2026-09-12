window.show()
log('start ' .. version)
i = 100000
c = 0
log('Hello World');
log('Circle demo');
--canvas.alpha = 150
while i > 0 do
    c = math.random(0, colors.count - 1)
    canvas.color = colors[c]
    r = math.random(5, 20) --size of circle
    x = math.random(window.width)
    y = math.random(window.height)
    canvas.circle(x, y, r, true)
    canvas.color = colors.black
    canvas.circle(x, y, r, false)
    sleep(100)
    i = i - 1
end
