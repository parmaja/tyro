log(version);
--font.load('terminus.ttf', 18)
--font.load('DejaVuSansMono.ttf', 18);
--font.load('spleen-12x24.bdf')
--font.load('c:/temp/1.png')
--font.load('ter-u24n.fnt')
--font.load('font2bitmap.png', 24)
--font.load('Inconsolata-16r.psf')
console.align = 'bottom'
window.show(1024, 600);
console.margin = 10
console.borderColor = red
--console.height = 20
console.show(50, 50, 600, 400);
console.print('Hello World');
x = console.read();
log(x)
--for i = 1,10 do
--    console.print(i.."\r\n")
--end
--console.print(tostring(10+10));
--function align()
--    global console
--    console.align = 'bottom'
--end