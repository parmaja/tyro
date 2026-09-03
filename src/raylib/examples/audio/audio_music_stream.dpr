program audio_music_stream;
{*******************************************************************************************
*
*   raylib [audio] example - music stream
*
*   Example complexity rating: [★☆☆☆] 1/4
*
*   Example originally created with raylib 1.3, last time updated with raylib 4.2
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
*
*   Ported to Pascal: Zaher Dirkey (@zaher)
*
********************************************************************************************}

{$APPTYPE CONSOLE}
{$MINENUMSIZE 4} //All enum must be sized as Integer
{$Z4}
{$A8}

uses
  System.SysUtils, RayLib;

const
  ScreenWidth = 800;
  ScreenHeight = 450;

var
  Music: TMusic;
  TimePlayed: Single;
  Pause: Boolean;
  Pan: Single;
  Volume: Single;
begin
  InitLibrary;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - music stream');
  InitAudioDevice;              // Initialize audio device

  Music := LoadMusicStream('resources/country.mp3');

  PlayMusicStream(Music);

  Pause := False;            // Music playing paused

  Pan := 0.0;                // Default audio pan center [-1.0..1.0]
  SetMusicPan(Music, Pan);

  Volume := 0.8;             // Default audio volume [0.0..1.0]
  SetMusicVolume(Music, Volume);

  SetTargetFPS(30);               // Set our game to run at 30 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while (not WindowShouldClose()) do   // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    UpdateMusicStream(Music);   // Update music buffer with new stream data

    // Restart music playing (stop and play)
    if (IsKeyPressed(KEY_SPACE)) then
    begin
      StopMusicStream(Music);
      PlayMusicStream(Music);
    end;

    // Pause/Resume music playing
    if (IsKeyPressed(KEY_P)) then
    begin
      Pause := not Pause;
      if Pause then
        PauseMusicStream(Music)
      else
        ResumeMusicStream(Music);
    end;

    // Set audio pan
    if (IsKeyDown(KEY_LEFT)) then
    begin
      Pan := Pan - 0.05;
      if (Pan < -1.0) then Pan := -1.0;
      SetMusicPan(Music, Pan);
    end
    else if (IsKeyDown(KEY_RIGHT)) then
    begin
      Pan := Pan + 0.05;
      if (Pan > 1.0) then Pan := 1.0;
      SetMusicPan(Music, Pan);
    end;

    // Set audio volume
    if (IsKeyDown(KEY_DOWN)) then
    begin
      Volume := Volume - 0.05;
      if (Volume < 0.0) then Volume := 0.0;
      SetMusicVolume(Music, Volume);
    end
    else if (IsKeyDown(KEY_UP)) then
    begin
      Volume := Volume + 0.05;
      if (Volume > 1.0) then Volume := 1.0;
      SetMusicVolume(Music, Volume);
    end;

    // Get normalized time played for current music stream
    TimePlayed := GetMusicTimePlayed(Music) / GetMusicTimeLength(Music);

    if (TimePlayed > 1.0) then TimePlayed := 1.0;   // Make sure time played is no longer than music
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing;

      ClearBackground(clRayWhite);

      DrawText('MUSIC SHOULD BE PLAYING!', 255, 150, 20, clLightGray);

      DrawText('LEFT-RIGHT for PAN CONTROL', 320, 74, 10, clDarkBlue);
      DrawRectangle(300, 100, 200, 12, clLightGray);
      DrawRectangleLines(300, 100, 200, 12, clGray);
      DrawRectangle(Round(300 + (Pan + 1.0) / 2.0 * 200 - 5), 92, 10, 28, clDarkGray);

      DrawRectangle(200, 200, 400, 12, clLightGray);
      DrawRectangle(200, 200, Integer(Round(TimePlayed * 400.0)), 12, clMaroon);
      DrawRectangleLines(200, 200, 400, 12, clGray);

      DrawText('PRESS SPACE TO RESTART MUSIC', 215, 250, 20, clLightGray);
      DrawText('PRESS P TO PAUSE/RESUME MUSIC', 208, 280, 20, clLightGray);

      DrawText('UP-DOWN for VOLUME CONTROL', 320, 334, 10, clDarkGreen);
      DrawRectangle(300, 360, 200, 12, clLightGray);
      DrawRectangleLines(300, 360, 200, 12, clGray);
      DrawRectangle(Round(300 + Volume * 200 - 5), 352, 10, 28, clDarkGray);

    EndDrawing;
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadMusicStream(Music);   // Unload music stream buffers from RAM

  CloseAudioDevice;           // Close audio device (music streaming is automatically stopped)

  CloseWindow;                // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
