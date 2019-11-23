log('start')

--log(colors.red)

--canvas.color := colors.red

text('Printing text test', 20, 20)
text('110 عربي', 100, 100)
i = 10
while i > 0 do
	r = math.random(5, 20) --size of circle
    x = math.random(640)
    y = math.random(480)
	canvas.circle(x, y, r)
    i = i - 1;
end


