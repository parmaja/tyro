log(version);
--font.load('terminus.ttf', 24)
font.load('DejaVuSansMono.ttf', 20)
--font.load('ter-u24n.fnt')
--font.load('font2bitmap.png', 24)
--font.load('Inconsolata-16r.psf')
--font.load('10x20-CP864-0-20.fnt');
window.show(1024, 600);
console.show(50, 50, 600, 400);
console.print('Hello World');
x = console.read();
log(x)
--for i = 1,10 do
--    console.print(i.."\r\n")
--end
--console.print(tostring(10+10));
