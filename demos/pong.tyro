--======================================================================
--  Pong - Classic arcade game demo for Tyro
--======================================================================
--  Demonstrates: drawing (circle, rectangle, line, text), keyboard input,
--  animation/game-loop, collision detection, scoring, sound effects,
--  and console output.
--
--  Controls:
--    W / S  - Move the left paddle up / down
--======================================================================

window.show(640, 480)
console.show()

-- ---- Configuration ----------------------------------------------------
paddle_w = 20
paddle_h = 120
paddle_speed = 6
ball_r = 10

-- ---- State ------------------------------------------------------------
left_x, left_y = 30, 180          -- player paddle (W/S)
right_x, right_y = 590, 180        -- AI paddle
left_score, right_score = 0, 0

ball_x, ball_y = 320, 240
ball_dx, ball_dy = 3, 2

serve = math.random(0, 1)
if serve == 0 then ball_dx = -ball_dx end

canvas.color = colors.black

-- ---- Helper: reset ball after a score ---------------------------------
function reset_ball(direction)
  ball_x, ball_y = 320, 240
  ball_dy = math.random(-2, 2)
  if direction == "left" then
    ball_dx = -3
  else
    ball_dx = 3
  end
end

print("=== Pong ===")
print("W/S to move paddle | Close window to quit")

-- ---- Main game loop ---------------------------------------------------
while true do
  -- Input: player paddle (W/S keys)
  if iskeydown("w") then
    left_y = left_y - paddle_speed
  end
  if iskeydown("s") then
    left_y = left_y + paddle_speed
  end

  -- Clamp player paddle to screen
  if left_y < 0 then left_y = 0 end
  if left_y + paddle_h > canvas.height then
    left_y = canvas.height - paddle_h
  end

  -- AI: simple tracking (moves toward the ball)
  ai_center = right_y + paddle_h / 2
  if ai_center < ball_y - 10 then
    right_y = right_y + paddle_speed * 0.7
  elseif ai_center > ball_y + 10 then
    right_y = right_y - paddle_speed * 0.7
  end

  -- Clamp AI paddle to screen
  if right_y < 0 then right_y = 0 end
  if right_y + paddle_h > canvas.height then
    right_y = canvas.height - paddle_h
  end

  -- Move ball
  ball_x = ball_x + ball_dx
  ball_y = ball_y + ball_dy

  -- Bounce off top / bottom walls
  if ball_y - ball_r < 0 then
    ball_y = ball_r
    ball_dy = -ball_dy
    music.sound(440, 60)
  elseif ball_y + ball_r > canvas.height then
    ball_y = canvas.height - ball_r
    ball_dy = -ball_dy
    music.sound(440, 60)
  end

  -- Bounce off left paddle
  if ball_dx < 0 and
     ball_x - ball_r < left_x + paddle_w and
     ball_y > left_y and ball_y < left_y + paddle_h then
    ball_x = left_x + paddle_w + ball_r
    ball_dx = -ball_dx * 1.07   -- speed up (progressive difficulty)
    ball_dy = ball_dy * 1.05
    music.sound(660, 60)
  end

  -- Bounce off right paddle
  if ball_dx > 0 and
     ball_x + ball_r > right_x and
     ball_y > right_y and ball_y < right_y + paddle_h then
    ball_x = right_x - ball_r
    ball_dx = -ball_dx * 1.07   -- speed up
    ball_dy = ball_dy * 1.05
    music.sound(660, 60)
  end

  -- Scoring
  if ball_x < 0 then
    right_score = right_score + 1
    print("Point for AI!  " .. right_score .. " - " .. left_score)
    reset_ball("right")
  elseif ball_x > canvas.width then
    left_score = left_score + 1
    print("Point for Player!  " .. left_score .. " - " .. right_score)
    reset_ball("left")
  end

  -- ---- Drawing --------------------------------------------------------
  -- Clear screen with a black background rectangle
  canvas.color = colors.black
  canvas.rectangle(0, 0, canvas.width, canvas.height, true)

  -- Center line
  canvas.color = colors.white
  canvas.line(320, 0, 320, canvas.height)

  -- Paddles (white fill, no outline)
  canvas.color = colors.white
  canvas.rectangle(left_x, left_y, paddle_w, paddle_h, true)
  canvas.rectangle(right_x, right_y, paddle_w, paddle_h, true)

  -- Ball
  canvas.circle(ball_x, ball_y, ball_r, true)

  -- Scores
  canvas.text(260, 10, tostring(left_score))
  canvas.text(370, 10, tostring(right_score))

  sleep(16)   -- ~60 FPS frame pacing
end
