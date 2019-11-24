log('start')

log(canvas.width)

canvas.text(10, 30, 'Printing text test')
canvas.text(10, 100, '110 عربي')

canvas.text(10, 30, 'Printing text test')
i = 1000000
canvas.color = colors.black
canvas.line(0, 100, canvas.width, 100)

while i > 0 do
    c = math.random(3, colors.count)
    canvas.color = colors[c]
	r = math.random(5, 20) --size of circle
    x = math.random(640)
    y = math.random(480)
	canvas.circle(x, y, r, true)
	r = math.random(5, 20) --size of circle
    x = math.random(640)
    y = math.random(480)
	canvas.rectangle(x, y, r, r, true)
    sleep(10)
    i = i - 1
end


