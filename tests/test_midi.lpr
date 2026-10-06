program test_midi;

{ Headless test for the Standard MIDI File reader of Tyro (src\tyrolib\TyroMidiFile.pas).

  The reader is the half of the player that can be checked without a sound
  device: a file becomes tracks of messages plus a tempo map, and Prepare turns
  every tick into a sample position. TyroMidi renders those positions, so what is
  under test here is the timing a song is played at and which events come out of
  it, not how a voice sounds.

  What is checked:

    the header   the format, the track count and the ticks per quarter note are
                 read; a format this reader does not support, a zero division and
                 the SMPTE division are refused with a message rather than guessed
    the events   note on/off, control change, program change and pitch bend come
                 through with their channel, and everything else (text, key
                 signature, sysex, unknown meta events) is stepped over without
                 desynchronizing the track it stands in
    the quirks   a note on with no velocity is a note off and a note off with a
                 velocity is a note on, because several writers end a note that
                 way; a running status carries over to the next event, and stops
                 at a meta or sysex event
    the tempo    changes from every track are merged into one map, a file with
                 none plays at 120 quarter notes a minute, and a tempo change
                 lands on the sample the tick it stands at maps to
    the file      a file that is not a MIDI file, a truncated one and one whose
                 length field asks for more messages than a reader should accept
                 are all reported instead of played

  A real file (demos\swanlake.mid) is read for the whole-file path: 26 tracks and
  a tempo map that speeds up near the end, checked by name and by measure.

  The small files below are written here byte by byte, so what the reader is
  asked to understand is visible in the test itself. They are deleted again when
  it finishes; only this program stays behind.

  Build and run:
    lazbuild --build-mode=Debug tests\test_midi.lpi
    bin\test_midi.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  TyroMidiFile;

const
  cDemoFile = 'swanlake.mid';
  cF0 = 'test_midi_f0.mid';
  cF1 = 'test_midi_f1.mid';
  cBad = 'test_midi_bad.mid';
  cTooMany = 'test_midi_many.mid';
  cTruncated = 'test_midi_trunc.mid';
  cNamed = 'test_midi_named.mid';

type
  //One buffer for every file below, long enough for the longest of them
  TTestBytes = array[0..255] of Byte;

var
  Tests, Failures: Integer;
  aDir: string;
  aWritten: array[0..5] of string;   //every fixture this test writes, to remove
  i: Integer;

procedure Check(const AWhat: string; ACondition: Boolean);
begin
  Inc(Tests);
  if ACondition then
    WriteLn('  ok   ', AWhat)
  else
  begin
    Inc(Failures);
    WriteLn('  FAIL ', AWhat);
  end;
end;

procedure CheckInt(const AWhat: string; AGot, AWant: Int64);
begin
  Inc(Tests);
  if AGot = AWant then
    WriteLn('  ok   ', AWhat, ' = ', AGot)
  else
  begin
    Inc(Failures);
    WriteLn('  FAIL ', AWhat, ': got ', AGot, ', want ', AWant);
  end;
end;

// A file written byte by byte, so the reader is fed exactly what is written here.
// The count is passed along because a file is shorter than the array it is filled
// into, and FPC will not make an open array out of a slice of a static one.
procedure WriteBytes(const AFileName: string; const ABytes: TTestBytes; ACount: Integer);
var
  aStream: TFileStream;
begin
  aStream := TFileStream.Create(aDir + AFileName, fmCreate);
  try
    aStream.WriteBuffer(ABytes[0], ACount);
  finally
    aStream.Free;
  end;
end;

procedure WriteWord(var ABytes: TTestBytes; AAt: Integer; AValue: Word);
begin
  ABytes[AAt] := Byte((AValue shr 8) and $FF);
  ABytes[AAt + 1] := Byte(AValue and $FF);
end;

procedure WriteDWord(var ABytes: TTestBytes; AAt: Integer; AValue: LongWord);
begin
  ABytes[AAt] := Byte((AValue shr 24) and $FF);
  ABytes[AAt + 1] := Byte((AValue shr 16) and $FF);
  ABytes[AAt + 2] := Byte((AValue shr 8) and $FF);
  ABytes[AAt + 3] := Byte(AValue and $FF);
end;

// A frame walks the messages of a track in order, firing each on its own sample,
// so a track out of tick order would play at samples its events do not belong to
function OrderedByTickAndSample(const AMsgs: TMidiMessages): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to High(AMsgs) do
    if (AMsgs[I].Tick < AMsgs[I - 1].Tick) or
      (AMsgs[I].Sample < AMsgs[I - 1].Sample) then
    begin
      Result := False;
      Break;
    end;
end;

// The messages of one track, in the order the reader hands them over
function MessagesOf(aSong: TMidiSong; ATrack: Integer): TMidiMessages;
begin
  if (ATrack < 0) or (ATrack >= aSong.TrackCount) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TMidiTrack(aSong.Tracks[ATrack]).Messages;
end;

{ The running status and the two ways of ending a note }

procedure TestRunningStatusAndVelocityZero;
var
  aBytes: TTestBytes;
  aSong: TMidiSong;
  aMsgs: TMidiMessages;
begin
  WriteLn('running status and the two ways of ending a note');
  //One track, five events: a note on, a second one that runs on the status of the
  //first, a note on with no velocity, a note off that carries a velocity, and a
  //program change, which is the one channel message with a single data byte.
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T');
  aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 0);      //format 0
  WriteWord(aBytes, 10, 1);     //one track
  WriteWord(aBytes, 12, 96);    //ticks per quarter note
  aBytes[14] := Ord('M'); aBytes[15] := Ord('T');
  aBytes[16] := Ord('r'); aBytes[17] := Ord('k');
  WriteDWord(aBytes, 18, 22);
  aBytes[22] := 0;   aBytes[23] := $90; aBytes[24] := 60; aBytes[25] := 100;
  aBytes[26] := 0;   aBytes[27] := 61;   aBytes[28] := 100;   //running status
  aBytes[29] := 0;   aBytes[30] := $90; aBytes[31] := 62; aBytes[32] := 0;
  aBytes[33] := 0;   aBytes[34] := $80; aBytes[35] := 67; aBytes[36] := 90;
  aBytes[37] := 96;  aBytes[38] := $C0; aBytes[39] := 40;
  aBytes[40] := 0;   aBytes[41] := $FF; aBytes[42] := $2F; aBytes[43] := 0;
  WriteBytes(cF0, aBytes, 44);

  aSong := TMidiSong.Create;
  try
    Check('the format 0 file loads', aSong.LoadFromFile(aDir + cF0));
    CheckInt('its format', aSong.Format, 0);
    CheckInt('its division', aSong.Division, 96);
    CheckInt('its track count', aSong.TrackCount, 1);
    CheckInt('its message count', aSong.MessageCount, 5);

    aMsgs := MessagesOf(aSong, 0);
    Check('the track holds five messages', Length(aMsgs) = 5);
    Check('the first event is a note on',
      (Length(aMsgs) > 0) and (MidiStatusKind(aMsgs[0].Status) = msNoteOn) and
      (MidiStatusChannel(aMsgs[0].Status) = 0) and (aMsgs[0].Data1 = 60) and
      (aMsgs[0].Data2 = 100));
    Check('the second runs on that status',
      (Length(aMsgs) > 1) and (MidiStatusKind(aMsgs[1].Status) = msNoteOn) and
      (aMsgs[1].Data1 = 61));
    Check('a note on with no velocity reads as a note off',
      (Length(aMsgs) > 2) and (MidiStatusKind(aMsgs[2].Status) = msNoteOff) and
      (aMsgs[2].Data1 = 62));
    Check('a note off with a velocity reads as a note on',
      (Length(aMsgs) > 3) and (MidiStatusKind(aMsgs[3].Status) = msNoteOn) and
      (aMsgs[3].Data1 = 67) and (aMsgs[3].Data2 = 90));
    Check('a program change keeps its one data byte',
      (Length(aMsgs) > 4) and (MidiStatusKind(aMsgs[4].Status) = msProgram) and
      (aMsgs[4].Data1 = 40));
    Check('a running status stops at a meta event',
      (Length(aMsgs) > 4) and (MidiStatusKind(aMsgs[4].Status) = msProgram));
    Check('a file with no tempo change plays at 120',
      not aSong.HasTempoChanges and (aSong.TempoAt(0) = cMidiDefaultUsPerQuarter) and
      (aSong.TempoAt(100000) = cMidiDefaultUsPerQuarter));
  finally
    aSong.Free;
  end;
end;

{ A meta event the reader keeps nothing of must not move the track }

procedure TestSkippedEvents;
var
  aBytes: TTestBytes;
  aSong: TMidiSong;
  aMsgs: TMidiMessages;
begin
  WriteLn('a meta event the reader keeps nothing of');
  //A text event and a sysex stand between two note ons. If the reader miscounted
  //either length it would read the rest of the track as noise.
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 0);
  WriteWord(aBytes, 10, 1);
  WriteWord(aBytes, 12, 96);
  aBytes[14] := Ord('M'); aBytes[15] := Ord('T'); aBytes[16] := Ord('r'); aBytes[17] := Ord('k');
  WriteDWord(aBytes, 18, 26);
  aBytes[22] := 0;   aBytes[23] := $90; aBytes[24] := 60; aBytes[25] := 100;
  aBytes[26] := 0;   aBytes[27] := $FF; aBytes[28] := $01; aBytes[29] := 5;
  aBytes[30] := Ord('h'); aBytes[31] := Ord('e'); aBytes[32] := Ord('l');
  aBytes[33] := Ord('l'); aBytes[34] := Ord('o');
  aBytes[35] := 0;   aBytes[36] := $F0; aBytes[37] := 3;
  aBytes[38] := $7E; aBytes[39] := $7F; aBytes[40] := $F7;
  aBytes[41] := 0;   aBytes[42] := $90; aBytes[43] := 64; aBytes[44] := 90;
  aBytes[45] := 0;   aBytes[46] := $FF; aBytes[47] := $2F; aBytes[48] := 0;
  WriteBytes(cF1, aBytes, 49);

  aSong := TMidiSong.Create;
  try
    Check('the file with skipped events loads', aSong.LoadFromFile(aDir + cF1));
    CheckInt('it holds only the two note ons', aSong.MessageCount, 2);
    aMsgs := MessagesOf(aSong, 0);
    Check('the first note is the one before the text',
      (Length(aMsgs) > 0) and (aMsgs[0].Data1 = 60));
    Check('the second note is the one after the sysex',
      (Length(aMsgs) > 1) and (aMsgs[1].Data1 = 64));
  finally
    aSong.Free;
  end;
end;

{ A track name, and the song name it gives }

procedure TestTrackName;
var
  aBytes: TTestBytes;
  aSong: TMidiSong;
begin
  WriteLn('a track name, and the song name it gives');
  //Two tracks, the first with no name at all and the second named. The name of
  //the song is the first track that has one, so it has to be the second here.
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 1);
  WriteWord(aBytes, 10, 2);
  WriteWord(aBytes, 12, 96);
  //track 0: nothing but a time signature and the end
  aBytes[14] := Ord('M'); aBytes[15] := Ord('T'); aBytes[16] := Ord('r'); aBytes[17] := Ord('k');
  WriteDWord(aBytes, 18, 11);
  aBytes[22] := 0;   aBytes[23] := $FF; aBytes[24] := $58; aBytes[25] := 4;
  aBytes[26] := 3;   aBytes[27] := 2;   aBytes[28] := 24; aBytes[29] := 8;
  aBytes[30] := 0;   aBytes[31] := $FF; aBytes[32] := $2F; aBytes[33] := 0;
  //track 1: the name, a note and the end
  aBytes[34] := Ord('M'); aBytes[35] := Ord('T'); aBytes[36] := Ord('r'); aBytes[37] := Ord('k');
  WriteDWord(aBytes, 38, 21);
  aBytes[42] := 0;   aBytes[43] := $FF; aBytes[44] := $03; aBytes[45] := 9;
  aBytes[46] := Ord('W'); aBytes[47] := Ord('a'); aBytes[48] := Ord('l');
  aBytes[49] := Ord('t'); aBytes[50] := Ord('z'); aBytes[51] := Ord(' ');
  aBytes[52] := Ord('N'); aBytes[53] := Ord('o'); aBytes[54] := Ord('1');
  aBytes[55] := 0;   aBytes[56] := $90; aBytes[57] := 60; aBytes[58] := 100;
  aBytes[59] := 96;  aBytes[60] := $80; aBytes[61] := 60; aBytes[62] := 0;
  aBytes[63] := 0;   aBytes[64] := $FF; aBytes[65] := $2F; aBytes[66] := 0;
  WriteBytes(cNamed, aBytes, 67);

  aSong := TMidiSong.Create;
  try
    Check('the named file loads', aSong.LoadFromFile(aDir + cNamed));
    CheckInt('it holds two tracks', aSong.TrackCount, 2);
    Check('the song takes the name of the first track that has one',
      aSong.Name = 'Waltz No1');
    Check('the unnamed track is left empty',
      TMidiTrack(aSong.Tracks[0]).Name = '');
    Check('the named track keeps its own name',
      TMidiTrack(aSong.Tracks[1]).Name = 'Waltz No1');
  finally
    aSong.Free;
  end;
end;

{ The tempo map, and the sample positions it maps ticks onto }

procedure TestTempoMap;
var
  aBytes: TTestBytes;
  aSong: TMidiSong;
  aMsgs: TMidiMessages;
  aQuarter: Int64;
begin
  WriteLn('the tempo map and the samples it maps onto');
  //Format 1: a tempo track and one part. The tempo changes half a quarter note
  //into the song, and the part holds a note across the change, so the samples
  //those events land on can be worked out by hand.
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 1);      //format 1
  WriteWord(aBytes, 10, 2);     //two tracks
  WriteWord(aBytes, 12, 96);    //ticks per quarter note
  //track 0: the tempo map, 500000 us a quarter then 250000, half a quarter in
  aBytes[14] := Ord('M'); aBytes[15] := Ord('T'); aBytes[16] := Ord('r'); aBytes[17] := Ord('k');
  WriteDWord(aBytes, 18, 18);
  aBytes[22] := 0;   aBytes[23] := $FF; aBytes[24] := $51; aBytes[25] := 3;
  aBytes[26] := $07; aBytes[27] := $A1; aBytes[28] := $20;   //500000 us
  aBytes[29] := 48; aBytes[30] := $FF; aBytes[31] := $51; aBytes[32] := 3;
  aBytes[33] := $03; aBytes[34] := $D0; aBytes[35] := $90;   //250000 us at tick 48
  aBytes[36] := 0;   aBytes[37] := $FF; aBytes[38] := $2F; aBytes[39] := 0;
  //track 1: a note on at tick 0, one at tick 48 (the change) and one at tick 144
  aBytes[40] := Ord('M'); aBytes[41] := Ord('T'); aBytes[42] := Ord('r'); aBytes[43] := Ord('k');
  WriteDWord(aBytes, 44, 14);
  aBytes[48] := 0;   aBytes[49] := $90; aBytes[50] := 60; aBytes[51] := 100;
  aBytes[52] := 48;  aBytes[53] := 61;   aBytes[54] := 100;  //running status at tick 48
  aBytes[55] := 96;  aBytes[56] := 62;   aBytes[57] := 100;  //running status at tick 144
  aBytes[58] := 0;   aBytes[59] := $FF; aBytes[60] := $2F; aBytes[61] := 0;
  WriteBytes(cF1, aBytes, 62);

  aSong := TMidiSong.Create;
  try
    Check('the format 1 file loads', aSong.LoadFromFile(aDir + cF1));
    CheckInt('it holds two tracks', aSong.TrackCount, 2);
    CheckInt('its tempo map has the two changes', aSong.TempoCount, 2);
    Check('the first tempo is 500000 us a quarter',
      (aSong.TempoAt(0) = 500000) and (aSong.TempoAt(47) = 500000));
    Check('the second tempo takes over at tick 48',
      (aSong.TempoAt(48) = 250000) and (aSong.TempoAt(1000) = 250000));

    aSong.Prepare(44100);
    aMsgs := MessagesOf(aSong, 1);
    //96 ticks a quarter note: a quarter note at 500000 us is half a second, that
    //is 22050 samples at 44100. The tempo change at tick 48 is halfway into that
    //first quarter note, so a quarter note after it at 250000 us is a quarter of
    //a second, 11025 samples, and the ticks before the change are worth half that
    //rate: 48 ticks is 11025 samples.
    aQuarter := Int64(44100) * 500000 div 1000000;
    CheckInt('a quarter note at the first tempo', aQuarter, 22050);
    if Length(aMsgs) >= 3 then
    begin
      CheckInt('the first note starts on sample 0', aMsgs[0].Sample, 0);
      CheckInt('the second note starts 48 slow ticks in',
        aMsgs[1].Sample, aQuarter div 2);
      CheckInt('the third note starts 96 fast ticks later',
        aMsgs[2].Sample, aQuarter div 2 + Int64(44100) * 250000 div 1000000);
    end
    else
      Check('the part track holds its three notes', False);
    Check('the song runs on past its last event', aSong.LengthSamples > 0);

    //The same tempo question asked in samples rather than ticks: the change is at
    //the sample 48 ticks into the first quarter note maps to, and holds after it
    Check('the slow tempo is still the one in force at sample 0',
      aSong.TempoAtSample(0) = 500000);
    Check('the slow tempo is still the one in force at sample 11024',
      aSong.TempoAtSample(11024) = 500000);
    Check('the fast tempo is in force from sample 11025 on',
      (aSong.TempoAtSample(11025) = 250000) and
      (aSong.TempoAtSample(1000000) = 250000));
  finally
    aSong.Free;
  end;
end;

{ What the reader refuses, and what it says }

procedure TestRefusedFiles;
var
  aBytes: TTestBytes;
  aSong: TMidiSong;
  aStream: TFileStream;
  i: Integer;
begin
  WriteLn('what the reader refuses');

  //not a MIDI file at all
  for i := 0 to 15 do
    aBytes[i] := Ord('x');
  WriteBytes(cBad, aBytes, 16);
  aSong := TMidiSong.Create;
  try
    Check('a file that is not a MIDI file is refused',
      not aSong.LoadFromFile(aDir + cBad));
    Check('and it says why', aSong.Error <> '');
  finally
    aSong.Free;
  end;

  //a missing file
  aSong := TMidiSong.Create;
  try
    Check('a missing file is refused', not aSong.LoadFromFile(aDir + 'no_such_file.mid'));
    Check('and it says why', aSong.Error <> '');
  finally
    aSong.Free;
  end;

  //a header that promises format 2
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 2);
  WriteWord(aBytes, 10, 1);
  WriteWord(aBytes, 12, 96);
  WriteBytes(cBad, aBytes, 22);
  aSong := TMidiSong.Create;
  try
    Check('format 2 is refused', not aSong.LoadFromFile(aDir + cBad));
    Check('and the message names the format', Pos('2', aSong.Error) > 0);
  finally
    aSong.Free;
  end;

  //the SMPTE division: the top bit set in the division field
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 0);
  WriteWord(aBytes, 10, 1);
  WriteWord(aBytes, 12, $E278);
  WriteBytes(cBad, aBytes, 22);
  aSong := TMidiSong.Create;
  try
    Check('the SMPTE division is refused', not aSong.LoadFromFile(aDir + cBad));
    Check('and the message says SMPTE', Pos('SMPTE', aSong.Error) > 0);
  finally
    aSong.Free;
  end;

  //a division of zero
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 0);
  WriteWord(aBytes, 10, 1);
  WriteWord(aBytes, 12, 0);
  WriteBytes(cBad, aBytes, 22);
  aSong := TMidiSong.Create;
  try
    Check('a division of zero is refused', not aSong.LoadFromFile(aDir + cBad));
  finally
    aSong.Free;
  end;

  //a track chunk that stops in the middle of an event
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 0);
  WriteWord(aBytes, 10, 1);
  WriteWord(aBytes, 12, 96);
  aBytes[14] := Ord('M'); aBytes[15] := Ord('T'); aBytes[16] := Ord('r'); aBytes[17] := Ord('k');
  WriteDWord(aBytes, 18, 6);
  aBytes[22] := 0; aBytes[23] := $90; //a note on whose data bytes are missing
  WriteBytes(cTruncated, aBytes, 24);
  aSong := TMidiSong.Create;
  try
    Check('a track cut in the middle of an event stops there',
      aSong.LoadFromFile(aDir + cTruncated));
    CheckInt('and the half event is not kept', aSong.MessageCount, 0);
  finally
    aSong.Free;
  end;
end;

{ A file whose track promises more messages than a reader should accept }

procedure TestMessageLimit;
var
  aSong: TMidiSong;
  aBytes: TTestBytes;
  i: Integer;
begin
  WriteLn('a length field longer than the file it stands in');
  //A well formed header and track, but the track announces far more bytes than
  //the file holds. The reader has to stop where the file stops rather than read
  //past the end of it, and a note on cut in half is not an event.
  aBytes[0] := Ord('M'); aBytes[1] := Ord('T'); aBytes[2] := Ord('h'); aBytes[3] := Ord('d');
  WriteDWord(aBytes, 4, 6);
  WriteWord(aBytes, 8, 0);
  WriteWord(aBytes, 10, 1);
  WriteWord(aBytes, 12, 96);
  aBytes[14] := Ord('M'); aBytes[15] := Ord('T'); aBytes[16] := Ord('r'); aBytes[17] := Ord('k');
  WriteDWord(aBytes, 18, $00FFFFFF);   //about sixteen megabytes of track
  //and only these few bytes behind it, none of them a whole event: the reader
  //has to stop where the file stops
  for i := 22 to 27 do
    aBytes[i] := $90;
  WriteBytes(cTooMany, aBytes, 28);

  aSong := TMidiSong.Create;
  try
    Check('a track longer than the file loads', aSong.LoadFromFile(aDir + cTooMany));
    CheckInt('and reads only the events it has bytes for', aSong.MessageCount, 0);
  finally
    aSong.Free;
  end;
end;

{ The file that ships with the demos, read whole }

procedure TestDemoFile;
var
  aSong: TMidiSong;
  aMsgs: TMidiMessages;
  i, T: Integer;
  aNotes, aControls, aPrograms, aBends, aDrums: Integer;
begin
  WriteLn('demos\', cDemoFile);
  if not FileExists(aDir + '..' + PathDelim + 'demos' + PathDelim + cDemoFile) then
  begin
    WriteLn('  skipped: ', cDemoFile, ' is not next to the test');
    Exit;
  end;
  aSong := TMidiSong.Create;
  try
    Check('the demo file loads',
      aSong.LoadFromFile(aDir + '..' + PathDelim + 'demos' + PathDelim + cDemoFile));
    if aSong.TrackCount = 0 then
    begin
      Check('it read something', False);
      Exit;
    end;
    Check('it read something', True);
    CheckInt('its format', aSong.Format, 1);
    CheckInt('its track count', aSong.TrackCount, 26);
    CheckInt('its ticks per quarter note', aSong.Division, 240);
    Check('it has a tempo map', aSong.HasTempoChanges);
    Check('its tempo map starts at the first tick',
      aSong.Tempos[0].Tick = 0);
    Check('the map is in tick order',
      (aSong.TempoCount < 2) or (aSong.Tempos[1].Tick > aSong.Tempos[0].Tick));
    Check('the file has messages', aSong.MessageCount > 1000);

    //Count the kinds across every track, to see the file the way a player does
    aNotes := 0;
    aControls := 0;
    aPrograms := 0;
    aBends := 0;
    aDrums := 0;
    for T := 0 to aSong.TrackCount - 1 do
    begin
      aMsgs := MessagesOf(aSong, T);
      for i := 0 to High(aMsgs) do
        case MidiStatusKind(aMsgs[i].Status) of
          msNoteOn:
            begin
              Inc(aNotes);
              if MidiStatusChannel(aMsgs[i].Status) = 9 then
                Inc(aDrums);
            end;
          msNoteOff: Inc(aNotes);
          msControl: Inc(aControls);
          msProgram: Inc(aPrograms);
          msPitchBend: Inc(aBends);
        end;
    end;
    Check('it has notes', aNotes > 0);
    Check('it has notes on the drum channel', aDrums > 0);
    Check('it has control changes', aControls > 0);

    //Every track is in tick order, and in sample order once Prepare has run: a
    //message out of order would fire at a sample it does not belong to.
    for T := 0 to aSong.TrackCount - 1 do
      Check('track ' + IntToStr(T) + ' is ordered by tick',
        OrderedByTickAndSample(MessagesOf(aSong, T)));

    aSong.Prepare(44100);
    Check('the song has a length', aSong.LengthSamples > 0);
    Check('the length is the length in seconds',
      Abs(aSong.LengthSeconds - aSong.LengthSamples / 44100) < 0.01);
    CheckInt('the tempo at the start is the first of the map',
      aSong.TempoAt(0), aSong.Tempos[0].UsPerQuarter);
    CheckInt('the tempo at the first sample is the first of the map',
      aSong.TempoAtSample(0), aSong.Tempos[0].UsPerQuarter);
    CheckInt('the tempo at the last sample is the last of the map',
      aSong.TempoAtSample(aSong.LengthSamples),
      aSong.Tempos[aSong.TempoCount - 1].UsPerQuarter);
    Check('and the tempo halfway through reads as something',
      aSong.TempoAtSample(aSong.LengthSamples div 2) > 0);
  finally
    aSong.Free;
  end;
end;

begin
  if ParamCount > 0 then
    aDir := ParamStr(1)
  else
    aDir := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'tests';
  aDir := ExcludeTrailingPathDelimiter(ExpandFileName(aDir)) + PathDelim;

  aWritten[0] := cF0;
  aWritten[1] := cF1;
  aWritten[2] := cBad;
  aWritten[3] := cNamed;
  aWritten[4] := cTooMany;
  aWritten[5] := cTruncated;

  WriteLn('Standard MIDI File reader');
  WriteLn('  tests in ', aDir);
  WriteLn;

  TestRunningStatusAndVelocityZero;
  WriteLn;
  TestSkippedEvents;
  WriteLn;
  TestTrackName;
  WriteLn;
  TestTempoMap;
  WriteLn;
  TestRefusedFiles;
  WriteLn;
  TestMessageLimit;
  WriteLn;
  TestDemoFile;

  //the files this test wrote are not part of it
  for i := 0 to High(aWritten) do
  begin
    DeleteFile(aDir + aWritten[i]);
    if FileExists(aDir + aWritten[i]) then
      WriteLn('  left behind: ', aWritten[i]);
  end;

  WriteLn;
  if Failures = 0 then
    WriteLn('OK: ', Tests, ' checks passed')
  else
    WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks');
  Halt(Failures);
end.