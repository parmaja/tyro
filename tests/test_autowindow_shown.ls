-- Headless run that shows its own window at a distinctive size, then finishes.
-- The engine must not resize/re-show it after the script ends.
window.show(500, 400)
sleep(4000)
println("test_autowindow_shown: done")
