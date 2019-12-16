window()
x = 10
w = 50
i = 0
while i < colors.count do
    canvas.color = colors[i]
    h = 20
    y = i * h + 50
    canvas.rectangle(x, y, w, h, true)
    canvas.color = colors.black
    canvas.rectangle(x, y, w, h, false)
    sleep(1)
    i = i + 1
end


