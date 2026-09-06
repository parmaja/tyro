log(version);
font.load('terminus.ttf', 24)
--font.load('ter-u24n.fnt')
window.show(1024, 400);
console.show(50, 50, 600, 200);
console.print('Hello World');
console.read();
for i = 1,10 do
    console.print(i.."\r\n")
end
console.print(tostring(10+10));
