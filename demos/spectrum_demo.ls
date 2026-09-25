--======================================================================
--  spectrum_demo.ls - Radio + live stereo spectrum analyzer
--======================================================================
--  Play an internet radio station and analyze the live stream with a
--  Winamp-style stereo spectrum panel.
--
--  API:
--    spectrum.show([x, y, w, h])   show the analyzer panel (absolute coords)
--    spectrum.hide()
--    spectrum.bars = n             number of frequency bars (8..128)
--    spectrum.bars                 current number of bars
--    spectrum.active               true while a radio stream feeds the panel
--    spectrum.visible              panel is shown

window.show(640, 480)
spectrum.show(0, 82, 640, 260)
spectrum.bars = 32

--radio.play("http://countrymusic24.powerstream.de:9000")
radio.play("http://solid24.streamupsolutions.com:8026/stream")
--radio.play("https://streams.80s80s.de/techno/mp3-192")
--radio.play("https://stream04.pcradio.app/vangelis-med")
--radio.play("https://server.emancity.com:9992/stream")

while cycle do
  -- left/right arrow keys change the number of bars
  if iskeypressed("left") and spectrum.bars > 8 then
    spectrum.bars = spectrum.bars - 8
  end
  if iskeypressed("right") and spectrum.bars < 128 then
    spectrum.bars = spectrum.bars + 8
  end

  -- status header (cleared with a black box first)
  canvas.color = colors.black
  canvas.rectangle(0, 0, 640, 70, true)
  canvas.color = colors.green
  canvas.text(8, 8, "Station : " .. radio.station)
  canvas.text(8, 26, "Title   : " .. radio.title)
  canvas.text(8, 44, "State   : " .. radio.state .. "   " .. radio.bitrate)

  -- bottom hint
  canvas.color = colors.black
  canvas.rectangle(0, 360, 640, 120, true)
  canvas.color = colors.white
  if spectrum.active then
    canvas.text(8, 368, "bars = " .. spectrum.bars .. "   (left/right to change)")
  else
    canvas.text(8, 368, "waiting for the radio stream...")
  end
end