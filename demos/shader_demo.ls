--======================================================================
--  shader_demo.lua - Post-processing shader effects
--======================================================================
--  Demonstrates the shader API (property style):
--
--    shader.effect = "water"     - wavy water + foam at the bottom
--    shader.effect = "glow"      - soft glow around bright things
--    shader.effect = "gray"      - grayscale
--    shader.effect = "sepia"     - old photo look
--    shader.effect = "invert"    - inverted colors
--    shader.effect = "vignette"  - darkened corners
--    shader.effect = "pixelate"  - chunky blocks
--    shader.effect = "none"      - back to normal
--    shader.value  = 0..1        - effect parameter (water height, glow
--                                  region, gray/sepia/invert strength,
--                                  vignette amount, pixelate block size)
--    shader.area   = {x, y, w, h} - restrict the effect to a rectangle
--                                  (canvas pixels, y from the top)
--    shader.load("custom.frag")  - load a custom fragment shader from file
--
--  Keys:  1 water  2 glow  3 gray  4 sepia  5 invert  6 vignette  7 pixelate
--         8 custom (custom.frag)  0 none
--         q = value -0.1        e = value +0.1
--         z = area bottom half  x = area full canvas  c = small center area
--======================================================================

window.show(640, 480)

print("Effects demo")
print("1-8 = effects  0 = none  q/e = value  z/x/c = area")

effect = "water"
value = 0.5
areaName = "canvas"

shader.effect = effect
shader.value = value

t = 0
-- clear with a black rectangle (canvas.clear uses the back color)
canvas.color = colors.black
canvas.rectangle(0, 0, canvas.width, canvas.height, true)

-- a bright sun so the glow effect has something to shine on
canvas.color = colors.yellow
canvas.circle(320, 180, 50, true)
canvas.color = colors.white
canvas.circle(320, 180, 20, true)

-- a few twinkling stars
for i = 1, 10 do
    canvas.color = colors[i % 2 + 1]
    canvas.point(math.random(canvas.width), math.random(150))
end

-- a colored band across the middle (shows grayscale/sepia/invert off)
canvas.color = colors.olive
canvas.rectangle(0, 200, canvas.width, 10, true)
canvas.color = colors.green
canvas.rectangle(0, 210, canvas.width, 10, true)
canvas.color = colors.red
canvas.rectangle(0, 220, canvas.width, 10, true)

-- shore line and a blue water mass at the bottom (for the water effect)
canvas.color = colors.aqua
canvas.rectangle(0, canvas.height - 120, canvas.width, 120, true)

function set_area(name, x, y, w, h)
    areaName = name
    if name == "canvas" then
        shader.area = {0, 0, canvas.width, canvas.height}
    else
        shader.area = {x, y, w, h}
    end
end

function update_text()
    -- show the active effect, value and area
    local a = shader.area
    canvas.color = colors.white
    canvas.text(10, 10, "effect: " .. effect .. "  value: " .. value .. "  area: " .. areaName)
end

set_area("canvas", 0, 0, canvas.width, canvas.height)
update_text()

while true do
    if iskeypressed("1") then
        effect = "water"
        shader.effect = effect
        update_text()
    elseif iskeypressed("2") then
        effect = "glow"
        shader.effect = effect
        update_text()
    elseif iskeypressed("3") then
        effect = "gray"
        shader.effect = effect
        update_text()
    elseif iskeypressed("4") then
        effect = "sepia"
        shader.effect = effect
        update_text()
    elseif iskeypressed("5") then
        effect = "invert"
        shader.effect = effect
        update_text()
    elseif iskeypressed("6") then
        effect = "vignette"
        shader.effect = effect
        update_text()
    elseif iskeypressed("7") then
        effect = "pixelate"
        shader.effect = effect
        update_text()
    elseif iskeypressed("8") then
        effect = "custom.frag"
        shader.load("custom.frag")
        update_text()
    elseif iskeypressed("0") then
        effect = "none"
        shader.effect = effect
        update_text()
    elseif iskeypressed("q") then
        value = value - 0.1
        if value < 0.0 then value = 0.0 end
        shader.value = value
        update_text()
    elseif iskeypressed("e") then
        value = value + 0.1
        if value > 1.0 then value = 1.0 end
        shader.value = value
        update_text()
    elseif iskeypressed("z") then
        set_area("bottom", 0, canvas.height / 2, canvas.width, canvas.height / 2)
        update_text()
    elseif iskeypressed("x") then
        set_area("canvas", 0, 0, canvas.width, canvas.height)
        update_text()
    elseif iskeypressed("c") then
        set_area("center", 170, 90, 300, 300)
        update_text()
    end

    t = t + 1
    sleep(16)
end