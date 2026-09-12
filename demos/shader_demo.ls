--======================================================================
--  shader_demo.lua - Post-processing shader effects
--======================================================================
--  Demonstrates: canvas.effect() with built-in GLSL shaders
--
--    canvas.effect("water") - wavy water + foam at the bottom of the canvas
--    canvas.effect("glow")  - soft glow around bright things
--    canvas.effect("none")  - back to normal
--
--  Keys:  1 = water    2 = glow    0 = none
--======================================================================

window.show(640, 480)

print("Effects demo")
print("Press 1 = water  2 = glow  0 = none")

effect = "water"
canvas.effect(effect)

t = 0
while true do
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

    -- shore line and a blue water mass at the bottom (for the water effect)
    canvas.color = colors.olive
    canvas.rectangle(0, canvas.height - 160, canvas.width, 8, true)
    canvas.color = colors.aqua
    canvas.rectangle(0, canvas.height - 152, canvas.width, 152, true)

    -- show the active effect
    canvas.color = colors.white
    canvas.text(10, 10, "effect: " .. effect)

    if iskeypressed("1") then
        effect = "water"
        canvas.effect(effect)
    elseif iskeypressed("2") then
        effect = "glow"
        canvas.effect(effect)
    elseif iskeypressed("0") then
        effect = "none"
        canvas.effect(effect)
    end

    t = t + 1
    sleep(16)
end