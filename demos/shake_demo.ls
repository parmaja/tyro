--======================================================================
--  shake_demo.lua - shake the world, like an accident or an error
--======================================================================
--  Demonstrates the screen shake:
--
--    window.shake(ms [, power]) - jolt the world for ms milliseconds,
--                                  power = max offset in pixels
--                                  (0/omitted = default)
--    shake(ms [, power])        - the same function as a global
--    window.shaking             - true while the shake is still running
--
--  The canvas and the sprites move together while the terminal and the
--  other controls stay glued to the window.
--
--  Keys:  space = shake 400ms, default power
--         1 = small (200ms, 6px)   2 = crash (800ms, 40px)
--         3 = long (2000ms, 20px)  s = stop
--         q = quit
--======================================================================

window.show(640, 480)

local hits = 0

while cycle do
    canvas.color = colors.black
    canvas.rectangle(0, 0, canvas.width, canvas.height, true)

    -- a grid, so every jolt of the shake is easy to see
    canvas.color = colors.darkgray
    for gx = 0, canvas.width, 40 do
        canvas.line(gx, 0, gx, canvas.height)
    end
    for gy = 0, canvas.height, 40 do
        canvas.line(0, gy, canvas.width, gy)
    end

    canvas.color = colors.white
    canvas.text(10, 10, "space = shake   1 = small   2 = crash   3 = long   s = stop   q = quit")
    canvas.text(10, 30, "shaking: " .. tostring(window.shaking) .. "   shakes: " .. hits)

    -- the "world" that gets jolted
    canvas.color = colors.red
    canvas.rectangle(canvas.width / 2 - 40, canvas.height / 2 - 40, 80, 80, true)
    canvas.color = colors.yellow
    canvas.circle(canvas.width / 2, canvas.height / 2, 20, true)

    if iskeypressed("space") then
        window.shake(400)
        hits = hits + 1
    end
    if iskeypressed("1") then
        window.shake(200, 6)
        hits = hits + 1
    end
    if iskeypressed("2") then
        -- an accident: a long, hard jolt
        window.shake(800, 40)
        hits = hits + 1
    end
    if iskeypressed("3") then
        shake(2000, 20) -- the global form
        hits = hits + 1
    end
    if iskeypressed("s") then
        window.shake(0) -- 0 stops it
    end
    if iskeypressed("q") then
        break
    end
end
