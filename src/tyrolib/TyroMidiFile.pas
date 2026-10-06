unit TyroMidiFile;
{**
 *  This file is part of the "Tyro"
 *
 *  @license   MIT
 *
 *  @author    Zaher Dirkey
 *
 *  Standard MIDI File reader: the format behind .mid / .midi. It is the one chip
 *  music format the raylib audio backend cannot decode (TyroRadio names what it
 *  does bundle: WAV, OGG, MP3, FLAC, QOA, XM, MOD), so it is read here and
 *  played by the synth in TyroMidi.
 *
 *  A file is a header chunk followed by track chunks. Every track is a stream of
 *  (delta ticks, event) pairs, and the deltas accumulate into absolute tick
 *  positions. Only the events a player acts on are kept: note on/off, control
 *  change, program change and pitch bend. Every other event (text, key
 *  signature, sysex, ...) is read past by its own length, so an unknown one
 *  never desynchronizes the track it stands in.
 *
 *  Nothing in this unit touches audio, a window or raylib, so a headless test
 *  can load a file and inspect it (tests\test_midi.lpr).
 *
 *  Format 0 (one merged track) and format 1 (a tempo track plus the parts) are
 *  read. Format 2 and the SMPTE time division are reported as errors rather than
 *  guessed at, because a wrong guess is a song at the wrong speed.
 *
 *  ref: https://midi.org/standardmidifiles
 *       https://www.faqs.org/rfcs/rfc6295.html
 *}

{$IFDEF FPC}
{$MODE delphi}
{$else}
{$POINTERMATH ON}
{$ENDIF}

{$M+}{$H+}

interface

uses
  Classes, SysUtils, Math, Generics.Collections;

const
  { A MIDI file has no events past these; a guard against a corrupt length field
    asking for an endless allocation. }
  cMidiMaxMessages = 4000000;
  cMidiMaxTracks = 1024;

  { 500000 microseconds per quarter note is the tempo a file plays at when it
    says nothing: 120 quarter notes a minute. }
  cMidiDefaultUsPerQuarter = 500000;

type

  { A message a player acts on. Data1/Data2 follow the meaning of Status:
    note off/on  Data1 = note, Data2 = velocity
    control      Data1 = controller, Data2 = value
    program      Data1 = program
    pitch bend   Data1 and Data2 = the 14 bit bend, low 7 bits first }

  TMidiMessage = record
    Tick: Int64;   //ticks from the start of the file
    Sample: Int64; //samples from the start of playback, filled by Prepare
    Status: Byte;
    Data1: Byte;
    Data2: Byte;
  end;
  TMidiMessages = array of TMidiMessage;

  { One point of the tempo map }

  TMidiTempoChange = record
    Tick: Int64;
    UsPerQuarter: Int64;
  end;
  TMidiTempoChanges = array of TMidiTempoChange;

  { Denominator is the power of two: 2 is a quarter, 3 an eighth, 4 a sixteenth }

  TMidiSignature = record
    Tick: Int64;
    Numerator: Byte;
    Denominator: Byte;
  end;
  TMidiSignatures = array of TMidiSignature;

  { The status byte of a channel message, kind and channel (channel 0 based).
    msOther is anything this reader steps over. }

  TMidiStatus = (msNoteOff, msNoteOn, msControl, msProgram, msPitchBend, msOther);

  { TMidiTrack }

  TMidiTrack = class
  public
    Name: string;
    Messages: TMidiMessages;
  end;

  { TMidiTracks }

  TMidiTracks = class(TObjectList<TMidiTrack>);

  { TMidiSong }

  TMidiSong = class
  private
    FName: string;
    FFormat: Integer;
    FDivision: Integer;
    FTracks: TMidiTracks;
    FTempos: TMidiTempoChanges;
    FSignatures: TMidiSignatures;
    FFoundTempos: TMidiTempoChanges; //every change of every track, in read order
    FLengthTicks: Int64;
    FLengthSamples: Int64;
    FSampleRate: Integer;
    FError: string;
    FMessageCount: Integer;
    procedure ParseTrack(AStream: TStream; ALength: Int64);
    procedure MergeTempoMap;
    function SegmentSamples(ADeltaTicks: Int64; ASegment: Integer): Int64;
  public
    constructor Create;
    destructor Destroy; override;

    { Reads the file into messages and a tempo map. Answers False and fills
      Error when it is not a MIDI file this reader can make sense of. }
    function LoadFromFile(const AFileName: string): Boolean;

    { Turns every tick into a sample position, so playback can be driven by
      samples instead of by the wall clock. Answer the rate the player renders
      at. Fills LengthTicks and LengthSeconds as well. }
    procedure Prepare(ASampleRate: Integer);

    function LengthSeconds: Single;
    function TrackCount: Integer;
    function TempoCount: Integer;
    function MessageCount: Integer;
    function TempoAt(ATick: Int64): Int64; //UsPerQuarter in force at ATick
    function TempoAtSample(ASample: Int64): Int64; //and in force at a sample
    function HasTempoChanges: Boolean;

    property Name: string read FName;
    property Format: Integer read FFormat;
    property Division: Integer read FDivision; //ticks per quarter note
    property Tracks: TMidiTracks read FTracks;
    property Tempos: TMidiTempoChanges read FTempos;
    property Signatures: TMidiSignatures read FSignatures;
    property LengthTicks: Int64 read FLengthTicks;
    property LengthSamples: Int64 read FLengthSamples;
    property SampleRate: Integer read FSampleRate;
    property Error: string read FError;
  end;

{ The kind of a channel message status byte, and the channel it names }
function MidiStatusKind(AStatus: Byte): TMidiStatus;
function MidiStatusChannel(AStatus: Byte): Integer;
function MidiIsChannelMessage(AStatus: Byte): Boolean;

implementation

{ Big endian readers. A MIDI file stores every number most significant byte
  first, which is the other way round from everything Pascal does. }

function ReadByte(AStream: TStream; out AValue: Byte): Boolean;
begin
  Result := AStream.Read(AValue, 1) = 1;
end;

function ReadWord(AStream: TStream; out AValue: Word): Boolean;
var
  aHi, aLo: Byte;
begin
  Result := ReadByte(AStream, aHi) and ReadByte(AStream, aLo);
  if Result then
    AValue := (Word(aHi) shl 8) or Word(aLo);
end;

function ReadDWord(AStream: TStream; out AValue: UInt32): Boolean;
var
  aBytes: array[0..3] of Byte;
  I: Integer;
begin
  Result := AStream.Read(aBytes, 4) = 4;
  if not Result then
  begin
    AValue := 0;
    Exit;
  end;
  AValue := 0;
  for I := 0 to 3 do
    AValue := (AValue shl 8) or UInt32(aBytes[I]);
end;

// A variable length quantity: 7 bits per byte, the top bit set on every byte
// but the last. Capped at four bytes, the longest a MIDI file may hold, so a
// corrupt one cannot spin here.
function ReadVarLong(AStream: TStream; out AValue: UInt64): Boolean;
var
  b: Byte;
  I: Integer;
begin
  AValue := 0;
  Result := False;
  for I := 0 to 3 do
  begin
    if not ReadByte(AStream, b) then
      Exit;
    AValue := (AValue shl 7) or UInt64(b and $7F);
    if (b and $80) = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// Read past ALength bytes, never past the end of what the stream holds
procedure SkipBytes(AStream: TStream; ALength: UInt64);
var
  aLeft: Int64;
begin
  aLeft := AStream.Size - AStream.Position;
  if aLeft <= 0 then
    Exit;
  if Int64(ALength) < aLeft then
    aLeft := Int64(ALength);
  AStream.Position := AStream.Position + aLeft;
end;

// The bytes of a text meta event, kept as they are
function ReadString(AStream: TStream; ALength: UInt64): string;
var
  b: Byte;
  I: UInt64;
begin
  Result := '';
  for I := 1 to ALength do
  begin
    if not ReadByte(AStream, b) then
      Break;
    Result := Result + Chr(b);
  end;
end;

{ MidiStatus* }

function MidiStatusKind(AStatus: Byte): TMidiStatus;
begin
  case AStatus and $F0 of
    $80: Result := msNoteOff;
    $90: Result := msNoteOn;
    $B0: Result := msControl;
    $C0: Result := msProgram;
    $E0: Result := msPitchBend;
  else
    Result := msOther;
  end;
end;

function MidiStatusChannel(AStatus: Byte): Integer;
begin
  Result := AStatus and $0F;
end;

function MidiIsChannelMessage(AStatus: Byte): Boolean;
begin
  //0x80..0xEF are the channel messages, 0xF0..0xFF the common ones
  Result := (AStatus >= $80) and (AStatus < $F0);
end;

{ TMidiSong }

constructor TMidiSong.Create;
begin
  inherited Create;
  FTracks := TMidiTracks.Create(True);
  FFormat := 0;
  FDivision := 0;
  FSampleRate := 0;
end;

destructor TMidiSong.Destroy;
begin
  FreeAndNil(FTracks);
  inherited Destroy;
end;

// Sort the messages of a track by tick. The format asks for them in order, so
// this is nearly free on a real file and puts a hand-edited one right.
procedure SortMessages(var AMessages: TMidiMessages);
var
  I, J: Integer;
  aItem: TMidiMessage;
begin
  for I := 1 to High(AMessages) do
  begin
    aItem := AMessages[I];
    J := I - 1;
    while (J >= 0) and (AMessages[J].Tick > aItem.Tick) do
    begin
      AMessages[J + 1] := AMessages[J];
      Dec(J);
    end;
    AMessages[J + 1] := aItem;
  end;
end;

// The tempo map of the whole file: every track's changes in tick order, with a
// default at the start when the file does not open with one of its own.
procedure TMidiSong.MergeTempoMap;
var
  I, J, N: Integer;
  aItem: TMidiTempoChange;
begin
  if Length(FFoundTempos) = 0 then
  begin
    SetLength(FTempos, 1);
    FTempos[0].Tick := 0;
    FTempos[0].UsPerQuarter := cMidiDefaultUsPerQuarter;
    Exit;
  end;

  SetLength(FTempos, Length(FFoundTempos) + 1);
  FTempos[0].Tick := 0;
  FTempos[0].UsPerQuarter := cMidiDefaultUsPerQuarter;
  for I := 0 to High(FFoundTempos) do
    FTempos[I + 1] := FFoundTempos[I];
  N := Length(FTempos);

  //drop the default when the file opens with a change of its own at tick 0
  if (FTempos[1].Tick = 0) and (N > 2) then
  begin
    FTempos[0] := FTempos[1];
    for I := 1 to N - 2 do
      FTempos[I] := FTempos[I + 1];
    Dec(N);
  end;
  SetLength(FTempos, N);

  //insertion sort, stable, over a list that is nearly ordered already
  for I := 2 to High(FTempos) do
  begin
    aItem := FTempos[I];
    J := I - 1;
    while (J >= 1) and (FTempos[J].Tick > aItem.Tick) do
    begin
      FTempos[J + 1] := FTempos[J];
      Dec(J);
    end;
    FTempos[J + 1] := aItem;
  end;
end;

procedure TMidiSong.ParseTrack(AStream: TStream; ALength: Int64);
var
  aEndPos, aTick: Int64;
  aDelta, aLen, aConsumed: UInt64;
  aByte, aD1, aD2, aMeta: Byte;
  aStatus: Byte;
  aEvent: Byte;
  aRunning: Boolean;
  aTrack: TMidiTrack;
  aKind: TMidiStatus;
  aTempo: UInt32;
  aItem: TMidiTempoChange;
begin
  aEndPos := AStream.Position + ALength;
  aTrack := TMidiTrack.Create;
  FTracks.Add(aTrack);
  aTick := 0;
  aStatus := 0;
  aRunning := False;
  while AStream.Position < aEndPos do
  begin
    if not ReadVarLong(AStream, aDelta) then
      Break;
    Inc(aTick, aDelta);
    if not ReadByte(AStream, aByte) then
      Break;

    if aByte = $FF then
    begin
      //a meta event: a type, a length, and that many bytes
      if not ReadByte(AStream, aMeta) then
        Break;
      if not ReadVarLong(AStream, aLen) then
        Break;
      aConsumed := 0;
      if aMeta = $03 then //track name
      begin
        aTrack.Name := ReadString(aStream, aLen);
        aConsumed := aLen;
      end
      else if (aMeta = $51) and (aLen >= 3) then
      begin
        //set tempo: microseconds per quarter note, most significant byte first
        if ReadByte(AStream, aByte) and ReadByte(AStream, aD1) and ReadByte(AStream, aD2) then
        begin
          aTempo := (UInt32(aByte) shl 16) or (UInt32(aD1) shl 8) or UInt32(aD2);
          if aTempo > 0 then
          begin
            SetLength(FFoundTempos, Length(FFoundTempos) + 1);
            aItem.Tick := aTick;
            aItem.UsPerQuarter := Int64(aTempo);
            FFoundTempos[High(FFoundTempos)] := aItem;
          end;
        end;
        aConsumed := 3;
      end
      else if (aMeta = $58) and (aLen >= 2) then
      begin
        //time signature: the beats on top, the note as a power of two
        if ReadByte(AStream, aByte) and ReadByte(AStream, aD1) then
        begin
          SetLength(FSignatures, Length(FSignatures) + 1);
          FSignatures[High(FSignatures)].Tick := aTick;
          FSignatures[High(FSignatures)].Numerator := aByte;
          FSignatures[High(FSignatures)].Denominator := aD1;
        end;
        aConsumed := 2;
      end;
      //end of track ($2F) and everything else carries nothing this reader keeps
      if aLen > aConsumed then
        SkipBytes(AStream, aLen - aConsumed);
      if aMeta = $2F then
        Break;
      //a meta event ends the run of the channel status before it
      aRunning := False;
    end
    else if (aByte = $F0) or (aByte = $F7) then
    begin
      //system exclusive: a length and that many bytes, nothing in them to play
      if not ReadVarLong(AStream, aLen) then
        Break;
      SkipBytes(AStream, aLen);
      aRunning := False;
    end
    else
    begin
      if aByte >= $80 then
      begin
        aStatus := aByte;
        aRunning := True;
      end
      else if not aRunning then
        Break; //a data byte with nothing to run on: the track is broken here

      //program change and channel pressure carry one data byte, the rest two.
      //Under a running status the byte already read is the first of them.
      if (aStatus and $F0) in [$C0, $D0] then
      begin
        if aByte < $80 then
          aD1 := aByte
        else if not ReadByte(AStream, aD1) then
          Break;
        aD2 := 0;
      end
      else
      begin
        if aByte < $80 then
        begin
          aD1 := aByte;
          if not ReadByte(AStream, aD2) then
            Break;
        end
        else if not ReadByte(AStream, aD1) or not ReadByte(AStream, aD2) then
          Break;
      end;

      aKind := MidiStatusKind(aStatus);
      if aKind = msOther then
        Continue;

      //What is stored is the message as it means, which is not always the status
      //byte it arrived with. aStatus itself is left alone: it is the status the
      //next event runs on.
      aEvent := aStatus;
      if aKind = msNoteOn then
      begin
        //a note on with no velocity is how several writers end a note
        if aD2 = 0 then
          aEvent := $80 or (aEvent and $0F);
      end
      else if aKind = msNoteOff then
      begin
        //and a note off with a velocity is how others end one
        if aD2 > 0 then
          aEvent := $90 or (aEvent and $0F);
      end;

      SetLength(aTrack.Messages, Length(aTrack.Messages) + 1);
      aTrack.Messages[High(aTrack.Messages)].Tick := aTick;
      aTrack.Messages[High(aTrack.Messages)].Status := aEvent;
      aTrack.Messages[High(aTrack.Messages)].Data1 := aD1;
      aTrack.Messages[High(aTrack.Messages)].Data2 := aD2;
      Inc(FMessageCount);
      if FMessageCount >= cMidiMaxMessages then
      begin
        FError := 'the file holds more messages than this reader accepts (' +
          IntToStr(cMidiMaxMessages) + ')';
        Break;
      end;
    end;
  end;

  SortMessages(aTrack.Messages);
end;

function TMidiSong.LoadFromFile(const AFileName: string): Boolean;
var
  aStream: TFileStream;
  aId: array[0..3] of AnsiChar;
  aLength: UInt32;
  aWord: Word;
  aTracks, aRead: Integer;
begin
  Result := False;
  FError := '';
  FName := '';
  FFormat := 0;
  FDivision := 0;
  FLengthTicks := 0;
  FLengthSamples := 0;
  FMessageCount := 0;
  SetLength(FTempos, 0);
  SetLength(FSignatures, 0);
  SetLength(FFoundTempos, 0);
  FTracks.Clear;

  if not FileExists(AFileName) then
  begin
    FError := 'file not found: ' + AFileName;
    Exit;
  end;
  aStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    if (aStream.Read(aId, 4) <> 4) or (aId <> 'MThd') then
    begin
      FError := 'not a MIDI file: the MThd header chunk is missing';
      Exit;
    end;
    if not ReadDWord(aStream, aLength) or (aLength < 6) then
    begin
      FError := 'truncated header';
      Exit;
    end;
    if not ReadWord(aStream, aWord) then
    begin
      FError := 'truncated header';
      Exit;
    end;
    FFormat := aWord;
    if not ReadWord(aStream, aWord) then
    begin
      FError := 'truncated header';
      Exit;
    end;
    aTracks := aWord;
    if not ReadWord(aStream, aWord) then
    begin
      FError := 'truncated header';
      Exit;
    end;
    FDivision := aWord;
    //a header may be longer than the six bytes read here
    if aLength > 6 then
      SkipBytes(aStream, aLength - 6);

    if FFormat > 1 then
    begin
      FError := 'unsupported MIDI format ' + IntToStr(FFormat) +
        ': only 0 (one track) and 1 (a tempo track plus parts) are read';
      Exit;
    end;
    if FDivision = 0 then
    begin
      FError := 'the header says there are no ticks per quarter note';
      Exit;
    end;
    if (FDivision and $8000) <> 0 then
    begin
      FError := 'unsupported time division: SMPTE (frames per second) is not read';
      Exit;
    end;
    if (aTracks <= 0) or (aTracks > cMidiMaxTracks) then
    begin
      FError := 'the header announces ' + IntToStr(aTracks) + ' tracks, which is not a usable number';
      Exit;
    end;

    //track chunks, up to the announced count. A chunk this reader does not know
    //is stepped over by its own length instead of ending the read.
    aRead := 0;
    while (aRead < aTracks) and (aStream.Position < aStream.Size) do
    begin
      if aStream.Read(aId, 4) <> 4 then
        Break;
      if not ReadDWord(aStream, aLength) then
        Break;
      if aId = 'MTrk' then
      begin
        ParseTrack(aStream, Int64(aLength));
        if FError <> '' then
        begin
          Result := False;
          Exit;
        end;
        Inc(aRead);
      end
      else if aLength = $FFFFFFFF then
        Break //a length of all ones means the rest of the file
      else
        SkipBytes(aStream, aLength);
    end;

    if FTracks.Count = 0 then
    begin
      FError := 'the file holds no track';
      Exit;
    end;

    MergeTempoMap;

    //the name of the song is the name of the first track that carries one
    for aRead := 0 to FTracks.Count - 1 do
      if TMidiTrack(FTracks[aRead]).Name <> '' then
      begin
        FName := TMidiTrack(FTracks[aRead]).Name;
        Break;
      end;
    Result := True;
  finally
    aStream.Free;
  end;
end;

function TMidiSong.SegmentSamples(ADeltaTicks: Int64; ASegment: Integer): Int64;
var
  aSeconds: Double;
begin
  if (ASegment < 0) or (ASegment > High(FTempos)) or (FDivision <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  //ticks -> microseconds -> samples, in floating point so that a long gap cannot
  //overflow before it is rounded
  aSeconds := ADeltaTicks * (FTempos[ASegment].UsPerQuarter / FDivision) / 1000000;
  if aSeconds < 0 then
    aSeconds := 0;
  Result := Int64(Round(aSeconds * FSampleRate));
end;

procedure TMidiSong.Prepare(ASampleRate: Integer);
var
  iTrack, iMsg, iSeg, iLast: Integer;
  aTrack: TMidiTrack;
  aSample: Int64;
begin
  if ASampleRate <= 0 then
    ASampleRate := 44100;
  FSampleRate := ASampleRate;
  FLengthTicks := 0;
  FLengthSamples := 0;
  if (FDivision <= 0) or (Length(FTempos) = 0) then
    Exit;

  for iTrack := 0 to FTracks.Count - 1 do
  begin
    aTrack := TMidiTrack(FTracks[iTrack]);
    if Length(aTrack.Messages) = 0 then
      Continue;
    iSeg := 0;
    aSample := 0;
    for iMsg := 0 to High(aTrack.Messages) do
    begin
      //close every tempo segment the message has walked past, then place it
      while (iSeg + 1 <= High(FTempos)) and
        (FTempos[iSeg + 1].Tick <= aTrack.Messages[iMsg].Tick) do
      begin
        aSample := aSample + SegmentSamples(aTrack.Messages[iMsg].Tick -
          FTempos[iSeg].Tick, iSeg);
        Inc(iSeg);
      end;
      aTrack.Messages[iMsg].Sample := aSample +
        SegmentSamples(aTrack.Messages[iMsg].Tick - FTempos[iSeg].Tick, iSeg);
      if aTrack.Messages[iMsg].Tick > FLengthTicks then
        FLengthTicks := aTrack.Messages[iMsg].Tick;
      if aTrack.Messages[iMsg].Sample > FLengthSamples then
        FLengthSamples := aTrack.Messages[iMsg].Sample;
    end;
  end;

  //one bar past the last event, so a song whose last note is held out by a
  //tempo change at the end is not cut short
  iLast := Length(FTempos) - 1;
  if (iLast > 0) and (FLengthTicks > 0) then
    FLengthSamples := FLengthSamples + SegmentSamples(FDivision - (FLengthTicks mod FDivision), iLast);
end;

function TMidiSong.LengthSeconds: Single;
begin
  if FSampleRate <= 0 then
    Result := 0
  else
    Result := FLengthSamples / FSampleRate;
end;

function TMidiSong.TrackCount: Integer;
begin
  Result := FTracks.Count;
end;

function TMidiSong.TempoCount: Integer;
begin
  Result := Length(FTempos);
end;

function TMidiSong.MessageCount: Integer;
begin
  Result := FMessageCount;
end;

function TMidiSong.HasTempoChanges: Boolean;
begin
  Result := Length(FTempos) > 1;
end;

function TMidiSong.TempoAt(ATick: Int64): Int64;
var
  I: Integer;
begin
  Result := cMidiDefaultUsPerQuarter;
  for I := 0 to High(FTempos) do
    if FTempos[I].Tick <= ATick then
      Result := FTempos[I].UsPerQuarter
    else
      Break;
end;

// The tempo in force at a sample, walked the same way Prepare walks the map: the
// segments are laid end to end in samples, and the last one that starts at or
// before this sample is the one in force. Answering this from the sample clock
// rather than from ticks is what keeps the tempo a script reads in step with the
// position it reads beside it.
function TMidiSong.TempoAtSample(ASample: Int64): Int64;
var
  I: Integer;
  aSegmentEnd: Int64;
begin
  Result := cMidiDefaultUsPerQuarter;
  if (FSampleRate <= 0) or (Length(FTempos) = 0) then
    Exit;
  //Each segment runs from where the one before it ended to where its successor
  //begins; the last one runs to the end of time.
  for I := 0 to High(FTempos) do
  begin
    if I + 1 <= High(FTempos) then
    begin
      aSegmentEnd := SegmentSamples(FTempos[I + 1].Tick - FTempos[I].Tick, I);
      if ASample < aSegmentEnd then
      begin
        Result := FTempos[I].UsPerQuarter;
        Exit;
      end;
    end
    else
    begin
      //the last change holds to the end of the song
      Result := FTempos[I].UsPerQuarter;
      Exit;
    end;
  end;
end;

end.