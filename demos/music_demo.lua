--======================================================================
--  music_demo.lua - Sound and music effects
--======================================================================
--  Demonstrates: music.sound() for tone effects, music.mml() for
--  Music Macro Language melodies
--======================================================================

window(640, 480)
canvas.color = colors.white
canvas.text(10, 30, "Music & Sound Demo")
canvas.text(10, 50, "You should hear sound effects...")

-- Simple tone effects (frequency Hz, duration ms)
music.sound(440, 200)  -- A note
sleep(250)
music.sound(523, 200)  -- C note (higher)
sleep(250)
music.sound(587, 200)  -- D note
sleep(250)
music.sound(659, 200)  -- E note
sleep(300)

canvas.color = colors.green
canvas.text(10, 100, "Tone sequence done!")
canvas.text(10, 120, "Now playing MML melody...")

-- Music Macro Language - simple scale
music.mml("c", "d", "e", "f", "g", "a", "b", "c5")

sleep(2000)
canvas.text(10, 150, "That's all, folks!")
