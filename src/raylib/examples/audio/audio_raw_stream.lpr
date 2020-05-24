program audio_raw_stream;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  RayLib3;

const
  MAX_SAMPLES            =   512;
  MAX_SAMPLES_PER_UPDATE =  4096;

  ScreenWidth = 800;
  ScreenHeight = 450;

var
  Stream: TAudioStream;
  Data, writeBuf: PShortInt;
  mousePosition: TVector2;
  Frequency, OldFrequency: Single;
  readCursor, waveLength: Integer;
  position: TVector2;
  fp: Single;
  oldWavelength: integer;
  i: Integer;
  writeCursor: Integer;
  writeLength: Integer;
  readLength: Integer;
begin
  RayLib.Load;
  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - raw audio streaming');
  InitAudioDevice();  // Initialize audio device

  // Init raw audio stream (sample rate: 22050, sample size: 16bit-short, channels: 1-mono)
  Stream := InitAudioStream(22050, 16, 1);

  // Buffer for the single cycle waveform we are synthesizing
  Data := AllocMem(sizeof(ShortInt) * MAX_SAMPLES);

  // Frame buffer, describing the waveform when repeated over the course of a frame
  WriteBuf := AllocMem(sizeof(ShortInt) * MAX_SAMPLES_PER_UPDATE);

  PlayAudioStream(Stream);        // Start processing stream buffer (no data loaded currently)

  // Position read in to determine next frequency
  mousePosition := TVector2.Create( -100.0, -100.0 );

  // Cycles per second (hz)
  Frequency := 440.0;

  // Previous value, used to test if sine needs to be rewritten, and to smoothly modulate frequency
  OldFrequency := 1.0;

  // Cursor to read and copy the samples of the sine wave buffer
  readCursor := 0;

  // Computed size in samples of the sine wave
  waveLength := 1;

  position := TVector2.Create( 0, 0 );

  SetTargetFPS(30);               // Set our game to run at 30 frames-per-second

  //--------------------------------------------------------------------------------------

  // Main game loop
  while (not WindowShouldClose()) do   // Detect window close button or ESC key
  begin
  // Update
  //----------------------------------------------------------------------------------

  // Sample mouse input.
  mousePosition := GetMousePosition();

  if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) then
  begin
      fp := mousePosition.y;
      frequency := 40 + fp;
  end;

  // Rewrite the sine wave.
  // Compute two cycles to allow the buffer padding, simplifying any modulation, resampling, etc.
  if (frequency <> oldFrequency) then
  begin
      // Compute wavelength. Limit size in both directions.
      oldWavelength := waveLength;
      waveLength := Integer(22050 / frequency);
      if (waveLength > MAX_SAMPLES / 2) then
        waveLength := MAX_SAMPLES div 2;
      if (waveLength < 1) then
        waveLength := 1;

      // Write sine wave.
      for i := 0 to waveLength*2 do
      begin
          Data[i] := Round((Sin(((2 * PI * i / waveLength))) * 32000));
      end;

      // Scale read cursor's position to minimize transition artifacts
      readCursor := Round(readCursor * (waveLength / oldWavelength));
      oldFrequency := frequency;
  end;

  // Refill audio stream if required
  if (IsAudioStreamProcessed(stream)) then
  begin
      // Synthesize a buffer that is exactly the requested size
      writeCursor := 0;

      while (writeCursor < MAX_SAMPLES_PER_UPDATE) do
      begin
          // Start by trying to write the whole chunk at once
          writeLength := MAX_SAMPLES_PER_UPDATE - writeCursor;

          // Limit to the maximum readable size
          readLength := waveLength-readCursor;

          if (writeLength > readLength) then
            writeLength := readLength;

          // Write the slice
          Move((writeBuf + writeCursor)^, (data + readCursor)^, writeLength * sizeof(ShortInt));

          // Update cursors and loop audio
          readCursor := (readCursor + writeLength) div waveLength;

          writeCursor := writeCursor + writeLength;
      end;

      // Copy finished frame to audio stream
      UpdateAudioStream(Stream, writeBuf, MAX_SAMPLES_PER_UPDATE);
  end;
  //----------------------------------------------------------------------------------

  // Draw
  //----------------------------------------------------------------------------------
  BeginDrawing;

      ClearBackground(RAYWHITE);

      DrawText(PChar(Format('sine frequency: %i', [frequency])), GetScreenWidth() - 220, 10, 20, RED);
      DrawText('click mouse button to change frequency', 10, 10, 20, DARKGRAY);

      // Draw the current buffer state proportionate to the screen
      for i := 0 to screenWidth -1 do
      begin
          position.x := i;
          position.y := 250 + 50 * data[i*MAX_SAMPLES div screenWidth] / 32000;

          DrawPixelV(position, RED);
      end;

  EndDrawing();
  //----------------------------------------------------------------------------------
end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  Freemem(data);                 // Unload sine wave data
  Freemem(writeBuf);             // Unload write buffer

  CloseAudioStream(Stream);   // Close raw audio stream and delete buffers from RAM
  CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------



end.

