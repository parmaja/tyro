program audio_raw_stream;
{*******************************************************************************************
*
*   raylib [audio] example - Raw audio streaming
*
*   This example has been created using raylib 1.6 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example created by Ramon Santamaria (@raysan5) and reviewed by James Hofmann (@triplefox)
*
*   Copyright (c) 2015-2019 Ramon Santamaria (@raysan5) and James Hofmann (@triplefox)
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
  MAX_SAMPLES            =   512;
  MAX_SAMPLES_PER_UPDATE =  4096;

  SampleRate = 22050;

  ScreenWidth = 800;
  ScreenHeight = 450;

  MaxSmallint = High(SmallInt)-1;

type
  TSmallintArray = array[0..MaxSmallint] of SmallInt;
  PSmallintArray = ^TSmallintArray;

var
  Stream: TAudioStream;
  Data, WriteBuf: PSmallintArray;
  mousePosition: TVector2;
  Frequency: Single;
  OldFrequency: Single;
  readCursor: Integer;
  waveLength: Integer; //in sample rate pieces
  oldWavelength: integer;
  position: TVector2;
  fp: Single;
  i: Integer;
  writeCursor: Integer;
  writeLength: Integer;
  readLength: Integer;
  s: utf8string;
begin
  InitLibrary;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - raw audio streaming');
  InitAudioDevice;  // Initialize audio device

  // Init raw audio stream (sample rate: 22050, sample size: 16bit, channels: 1-mono)
  Stream := LoadAudioStream(SampleRate, 16, 1);

  // Buffer for the single cycle waveform we are synthesizing
  Data      := AllocMem(Sizeof(SmallInt) * MAX_SAMPLES);

  // Frame buffer, describing the waveform when repeated over the course of a frame
  WriteBuf := AllocMem(sizeof(SmallInt) * MAX_SAMPLES_PER_UPDATE);

  PlayAudioStream(Stream);        // Start processing stream buffer (no data loaded currently)

  // Position read in to determine next frequency
  mousePosition := Vector2Of( -100, -100 );

  // Cycles per second (hz)
  Frequency := 440.0;

  // Previous value, used to test if sine needs to be rewritten, and to smoothly modulate frequency
  OldFrequency := 1.0;

  // Cursor to read and copy the samples of the sine wave buffer
  readCursor := 0;

  // Computed size in samples of the sine wave
  waveLength := 1;

  position := Vector2Of( 0, 0 );

  SetTargetFPS(30);               // Set our game to run at 30 frames-per-second

  //--------------------------------------------------------------------------------------

  // Main game loop
  while (not WindowShouldClose()) do   // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------

    if (IsMouseButtonDown(MOUSE_BUTTON_LEFT)) then
    begin
        // Sample mouse input.
        mousePosition := TVector2(GetMousePosition());
        fp := mousePosition.y;
        frequency := 40 + fp;
    end;

    // Rewrite the sine wave.
    // Compute two cycles to allow the buffer padding, simplifying any modulation, resampling, etc.
    if (frequency <> oldFrequency) then
    begin
        // Compute wavelength. Limit size in both directions.
        oldWavelength := waveLength;
        waveLength := Round(SampleRate / frequency);
        //WriteLn(waveLength);
        if (waveLength > (MAX_SAMPLES div 2)) then
          waveLength := MAX_SAMPLES div 2;
        if (waveLength < 1) then
          waveLength := 1;

        // Write sine wave.

        for i := 0 to waveLength * 2 - 1 do
          TSmallintArray(Data^)[i] := Round((Sin(((2 * PI * i / waveLength))) * High(SmallInt)-1));

        // Scale read cursor's position to minimize transition artifacts
        readCursor := Round(readCursor * (waveLength div oldWavelength));
        oldFrequency := frequency;
    end;

    // Refill audio stream if required
    if (IsAudioStreamProcessed(Stream)) then
    begin
      // Synthesize a buffer that is exactly the requested size
      writeCursor := 0;

      while (writeCursor < MAX_SAMPLES_PER_UPDATE) do
      begin
          // Start by trying to write the whole chunk at once
          writeLength := MAX_SAMPLES_PER_UPDATE - writeCursor;

          // Limit to the maximum readable size
          readLength := waveLength - readCursor;

          if (writeLength > readLength) then
            writeLength := readLength;

          // Write the slice
          Move(Data^[readCursor], WriteBuf^[writeCursor], writeLength * sizeof(SmallInt));

          // Update cursors and loop audio
          readCursor := (readCursor + writeLength) div waveLength;

          writeCursor := writeCursor + writeLength;
      end;

      // Copy finished frame to audio stream
      UpdateAudioStream(Stream, WriteBuf, MAX_SAMPLES_PER_UPDATE);
    end;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing;

    ClearBackground(clWhite);

    s := Format('sine frequency: %d', [Round(frequency)]);
    DrawText(PUtf8Char(s), GetScreenWidth() - 220, 10, 20, clRed);
    DrawText('click mouse button to change frequency', 10, 10, 20, clDarkGray);

    // Draw the current buffer state proportionate to the screen
    for i := 0 to screenWidth -1 do
    begin
      position.x := i;
      position.y := (ScreenHeight div 2) + 50 * Data[i * MAX_SAMPLES div screenWidth] div MaxSmallint;

      DrawPixelV(position, clRed);
    end;

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  Freemem(data);                 // Unload sine wave data
  Freemem(WriteBuf);             // Unload write buffer

  UnloadAudioStream(Stream);   // Close raw audio stream and delete buffers from RAM
  CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
