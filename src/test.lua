log('start')

--log(colors.red)

--canvas.color := colors.red

canvas.text(20, 20, 'Printing text test')

canvas.text(100, 100, '110 عربي')

i = 1000000
while i > 0 do
	r = math.random(5, 20) --size of circle
    x = math.random(640)
    y = math.random(480)
	canvas.circle(x, y, r)
	r = math.random(5, 20) --size of circle
    x = math.random(640)
    y = math.random(480)
	canvas.rectangle(x, y, r, r)
    sleep(100)
    i = i - 1
end


