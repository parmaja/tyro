log('test')
i = 100000
while i > 0 do
	r = math.random(1, 10) --size of circle
    x = math.random(640)
    y = math.random(480)
	circle(x, y, r)
    i = i - 1;
end


