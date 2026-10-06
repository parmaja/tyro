unit TyroMidi;
{**
 *  This file is part of the "Tyro"
 *
 *  @license   MIT
 *
 *  @author    Zaher Dirkey
 *
 *  Standard MIDI File player: a small synth that renders the file read by
 *  TyroMidiFile into one raylib audio stream.
 *
 *  Why a synth and not a decoder: raylib has no MIDI decoder, and the engine
 *  already owns both halves of what a note needs (TyroSounds generates a
 *  waveform from a procedure, TRayAudio is a continuous stream to feed, and
 *  nothing used it before). So the player keeps a fixed pool of voices, renders
 *  their mix each frame and hands it to the audio device.
 *
 *  Timing runs on a sample clock: TyroMidiFile has already turned every tick
 *  into a sample position through the tempo map, so an event fires on the sample
 *  it belongs to and the song cannot drift. The wall clock decides how many of
 *  those samples each update renders, which keeps the device fed without an
 *  assumption about raylib's buffering, and does not care whether a frame was
 *  ever drawn.
 *
 *  A General MIDI program does not name an instrument, it names one of sixteen
 *  families. Those are mapped onto the waveform generators TyroSounds already
 *  has, so a file plays as a chip tune with the right kind of tone per family,
 *  not as the sampled orchestra the file asks for. Channel 10 is a drum map of
 *  its own, over the notes it names.
 *
 *  Like every other audio owner in the engine this player belongs to the main
 *  thread: the Lua side reaches it through a queue object (TMidiPlayObject in
 *  TyroLua), and its getters are read under a lock from the script thread.
 *}

{$IFDEF FPC}
{$MODE delphi}
{$else}
{$POINTERMATH ON}
{$ENDIF}

{$M+}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Math,
  RayLib, RayClasses,
  TyroSounds, TyroMidiFile, TyroSpectrum;

const
  cMidiSampleRate = 44100;
  cMidiVoices = 32;          //notes that can ring at once
  cMidiChannels = 16;
  cMidiDrumChannel = 9;      //channel 10, counted from zero
  cMidiBendRange = 2;        //semitones at a full pitch bend
  { The longest block one frame may render. A frame that asks for more (a stall)
    lets the rest of its audio go instead of spending a quarter of a second
    inside the draw loop. }
  cMidiMaxBlockFrames = cMidiSampleRate div 4;

type

  TMidiPlayerState = (mpIdle, mpPlaying, mpPaused, mpStopped, mpError);
  TMidiEnvStage = (mesAttack, mesDecay, mesSustain, mesRelease);

  { What one note is played with }

  TMidiTone = record
    Proc: TWaveformProc;
    Freq: Single;        //Hz the note starts at, 0 for a noise tone
    FreqTarget: Single;  //Hz it slides to, Freq when there is no slide
    FreqSlew: Single;    //0 = arrive at once, else the share taken per sample
    Attack: Single;      //seconds to full
    Decay: Single;       //seconds from full down to the held level
    Sustain: Single;     //level held while the key is down, 0 for a drum
    Release: Single;     //seconds to fall silent once released
    Noise: Boolean;      //a noise tone: its pitch is the generator's own
  end;

  { One note in flight }

  TMidiVoice = record
    Active: Boolean;
    Drum: Boolean;
    Channel: Integer;
    Note: Integer;
    Proc: TWaveformProc;
    Pos: Int64;          //samples since the note started
    Freq: Single;
    FreqTarget: Single;
    FreqSlew: Single;
    AmpL, AmpR: Single;  //velocity and pan, already split per side
    Env: Single;         //envelope level, 0..1
    Stage: TMidiEnvStage;
    SustainLevel: Single;
    AttackRate: Single;  //per sample, so even a very short ramp still moves
    DecayRate: Single;
    ReleaseRate: Single;
    Release: Single;
    Started: Int64;      //the sample the note started on, for stealing
  end;
  TMidiVoiceList = array of TMidiVoice;

  { What one MIDI channel remembers while it plays }

  TMidiChannel = record
    Patch: Integer;
    Volume: Single;
    Pan: Single;
    Bend: Single;
    Sustain: Boolean;
  end;
  TMidiChannelList = array[0..cMidiChannels - 1] of TMidiChannel;

  { TMidiPlayer }

  TMidiPlayer = class(TRayUpdate)
  private
    FLock: TCriticalSection;
    FSong: TMidiSong;
    FStream: TRayAudio;
    FState: TMidiPlayerState;
    FError: string;
    FName: string;
    FFileName: string;
    FSamples: Int64;      //frames rendered since Play: the song clock
    FLastClock: QWord;    //milliseconds at the last Update
    FSampleRate: Integer;
    FChannels: TMidiChannelList;
    FVoices: TMidiVoiceList;
    FCursors: array of Integer;  //the next message of every track
    FNextEventSample: Int64;     //sample of the next event, -1 when none is left
    FNextTrack: Integer;
    FBlock: array of Single;     //interleaved stereo frames for the device
    procedure ResetChannels;
    procedure CloseStream;
    function OpenStream: Boolean;
    procedure SetError(const AMessage: string);
    procedure Finished;
    function AllocVoice(AChannel, ANote: Integer): Integer;
    procedure ReleaseVoices(AChannel, ANote: Integer);
    procedure ReleaseChannel(AChannel: Integer);
    procedure RefreshNextEvent;
    procedure DispatchEvent;
    procedure NoteOn(AChannel, ANote, AVelocity: Integer);
    procedure NoteOff(AChannel, ANote: Integer);
    procedure Control(AChannel, AController, AValue: Integer);
    procedure PitchBend(AChannel, AValue: Integer);
    function RenderVoice(AIndex: Integer): Single;
    procedure Render(AFrames: Integer);
    function GetStateString: string;
    function GetPlaying: Boolean;
    function GetError: string;
    function GetName: string;
    function GetFileName: string;
    function GetPosition: Single;
    function GetLength: Single;
    function GetTracks: Integer;
    function GetTempo: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play(const AFileName: string);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure Update; override;

    property State: TMidiPlayerState read FState;
    property StateString: string read GetStateString;
    property Error: string read GetError;
    property Playing: Boolean read GetPlaying;
    property Name: string read GetName;
    property FileName: string read GetFileName;
    property Position: Single read GetPosition;
    property Length: Single read GetLength;
    property Tracks: Integer read GetTracks;
    property Tempo: Integer read GetTempo;
  end;

var
  MidiPlayer: TMidiPlayer = nil;

implementation

const

  { The General MIDI families. A family names a kind of tone, not an
    instrument, so each one is a shape plus an envelope. }

  mpPiano = 0;  mpBell = 1;   mpOrgan = 2;  mpGuitar = 3;  mpBass = 4;
  mpStrings = 5; mpEnsemble = 6; mpBrass = 7; mpReed = 8;   mpPipe = 9;
  mpLead = 10;  mpPad = 11;    mpFX = 12;    mpEthnic = 13; mpPerc = 14;

{ Time }

// A wall clock in milliseconds. raylib has one too, but it only starts counting
// once a window has been opened, so a script that never shows one would leave a
// song standing still at its first sample. This clock runs from the moment the
// process does, which is also the clock the engine hands a script as `clock()`.
function Milliseconds: QWord;
begin
  Result := QWord(GetTickCount64);
end;

{ The tone of a note }

// The MIDI notes are equal tempered: A4 (note 69) is 440 Hz and every semitone
// is a twelfth root of two up.
function NoteFreq(ANote: Integer): Single;
begin
  Result := 440 * Power(2, (ANote - 69) / 12);
end;

// Which family a General MIDI program belongs to. The file writes the program
// as 0..127, eight programs to a family, and the families are listed in the
// order the specification numbers them.
function PatchOf(AProgram: Integer): Integer;
begin
  case AProgram div 8 of
    0: Result := mpPiano;     //piano
    1: Result := mpBell;      //chromatic percussion
    2: Result := mpOrgan;     //organ
    3: Result := mpGuitar;    //guitar
    4: Result := mpBass;      //bass
    5: Result := mpStrings;   //strings
    6: Result := mpEnsemble;  //ensemble
    7: Result := mpBrass;     //brass
    8: Result := mpReed;      //reed
    9: Result := mpPipe;      //pipe
    10: Result := mpLead;     //synth lead
    11: Result := mpPad;      //synth pad
    12: Result := mpFX;       //synth effects
    13: Result := mpEthnic;   //ethnic
    14: Result := mpPerc;     //percussive
  else
    Result := mpFX;           //sound effects
  end;
end;

// The drum of channel 10. The numbers are the General MIDI percussion map; a
// drum is a short decay, so it never reaches the sustain of a melodic tone.
function DrumTone(ANote: Integer): TMidiTone;
begin
  Result.Proc := @Noise_Waveform;
  Result.Freq := 8000;
  Result.FreqTarget := 8000;
  Result.FreqSlew := 0;
  Result.Attack := 0.001;
  Result.Decay := 0.15;
  Result.Sustain := 0;
  Result.Release := 0.02;
  Result.Noise := True;
  case ANote of
    35, 36:
      begin //bass drum: a thud that falls in pitch as it rings out
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 130;
        Result.FreqTarget := 45;
        Result.FreqSlew := 0.004;
        Result.Decay := 0.30;
      end;
    37: //side stick
      begin
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 320;
        Result.Decay := 0.06;
      end;
    38, 40: Result.Decay := 0.18; //snare
    39: Result.Decay := 0.28;      //hand clap
    41:
      begin //low floor tom
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 150;
        Result.Decay := 0.35;
      end;
    42, 44: Result.Decay := 0.05; //closed hi-hat
    43:
      begin //high floor tom
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 170;
        Result.Decay := 0.35;
      end;
    45, 47, 48:
      begin //low, mid and high toms
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 110;
        Result.Decay := 0.35;
      end;
    46: Result.Decay := 0.40;      //open hi-hat
    49, 57: Result.Decay := 1.20;   //crash cymbals
    50:
      begin //high tom
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 200;
        Result.Decay := 0.35;
      end;
    51, 59: Result.Decay := 0.90;   //ride cymbals
    52:
      begin //chinese cymbal
        Result.Decay := 1.20;
        Result.Freq := 6000;
      end;
    53:
      begin //ride bell
        Result.Proc := @Square_Waveform;
        Result.Noise := False;
        Result.Freq := 540;
        Result.Decay := 0.60;
      end;
    54: Result.Decay := 0.25;        //tambourine
    55: Result.Decay := 1.20;        //splash cymbal
    56:
      begin //cowbell
        Result.Proc := @Square_Waveform;
        Result.Noise := False;
        Result.Freq := 800;
        Result.Decay := 0.40;
      end;
    58: Result.Decay := 0.70;        //vibraslap
    60, 61:
      begin //bongos
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 420;
        Result.Decay := 0.30;
      end;
    62, 63, 64:
      begin //congas
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 300;
        Result.Decay := 0.30;
      end;
    65, 66:
      begin //timbales
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 520;
        Result.Decay := 0.30;
      end;
    67, 68:
      begin //agogo bells
        Result.Proc := @Square_Waveform;
        Result.Noise := False;
        Result.Freq := 1200;
        Result.Decay := 0.40;
      end;
    69, 70:
      begin //cabasa and maracas
        Result.Decay := 0.12;
      end;
    71, 72:
      begin //short and long whistle
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 2500;
        Result.Decay := 0.25;
      end;
    73, 74:
      begin //guiros
        Result.Decay := 0.30;
      end;
    75:
      begin //claves
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 2400;
        Result.Decay := 0.08;
      end;
    76, 77:
      begin //wood blocks
        Result.Proc := @Square_Waveform;
        Result.Noise := False;
        Result.Freq := 1000;
        Result.Decay := 0.08;
      end;
    78, 79:
      begin //cuica
        Result.Proc := @Sin_Waveform;
        Result.Noise := False;
        Result.Freq := 2000;
        Result.Decay := 0.30;
      end;
    80, 81:
      begin //triangles
        Result.Proc := @Square_Waveform;
        Result.Noise := False;
        Result.Freq := 6000;
        Result.Decay := 0.90;
      end;
  end;
end;

// The tone of a note on a channel: the drum it names on channel 10, else the
// family the program of the channel selects.
function ToneOf(AChannel, AProgram, ANote: Integer): TMidiTone;
var
  aPatch: Integer;
begin
  if AChannel = cMidiDrumChannel then
  begin
    Result := DrumTone(ANote);
    Exit;
  end;

  aPatch := PatchOf(AProgram);
  Result.Noise := False;
  Result.Freq := 0;
  Result.FreqTarget := 0;
  Result.FreqSlew := 0;
  case aPatch of
    mpPiano:
      begin
        Result.Proc := @Piano_Waveform;
        Result.Attack := 0.002;
        Result.Decay := 0.9;
        Result.Sustain := 0.25;
        Result.Release := 0.15;
      end;
    mpBell:
      begin
        Result.Proc := @Sin_Waveform;
        Result.Attack := 0.002;
        Result.Decay := 1.2;
        Result.Sustain := 0.1;
        Result.Release := 0.3;
      end;
    mpOrgan:
      begin
        Result.Proc := @Square_Waveform;
        Result.Attack := 0.01;
        Result.Decay := 0.05;
        Result.Sustain := 0.9;
        Result.Release := 0.06;
      end;
    mpGuitar:
      begin
        Result.Proc := @Saw_Waveform;
        Result.Attack := 0.005;
        Result.Decay := 0.5;
        Result.Sustain := 0.4;
        Result.Release := 0.12;
      end;
    mpBass:
      begin
        Result.Proc := @Saw_Waveform;
        Result.Attack := 0.005;
        Result.Decay := 0.4;
        Result.Sustain := 0.5;
        Result.Release := 0.10;
      end;
    mpStrings:
      begin
        Result.Proc := @Saw_Waveform;
        Result.Attack := 0.08;
        Result.Decay := 0.3;
        Result.Sustain := 0.8;
        Result.Release := 0.3;
      end;
    mpEnsemble:
      begin
        Result.Proc := @Saw_Waveform;
        Result.Attack := 0.06;
        Result.Decay := 0.3;
        Result.Sustain := 0.85;
        Result.Release := 0.25;
      end;
    mpBrass:
      begin
        Result.Proc := @Square_Waveform;
        Result.Attack := 0.03;
        Result.Decay := 0.2;
        Result.Sustain := 0.8;
        Result.Release := 0.12;
      end;
    mpReed:
      begin
        Result.Proc := @Square_Waveform;
        Result.Attack := 0.02;
        Result.Decay := 0.15;
        Result.Sustain := 0.85;
        Result.Release := 0.10;
      end;
    mpPipe:
      begin
        Result.Proc := @Sin_Waveform;
        Result.Attack := 0.04;
        Result.Decay := 0.1;
        Result.Sustain := 0.9;
        Result.Release := 0.12;
      end;
    mpLead:
      begin
        Result.Proc := @Square_Waveform;
        Result.Attack := 0.004;
        Result.Decay := 0.1;
        Result.Sustain := 0.7;
        Result.Release := 0.08;
      end;
    mpPad:
      begin
        Result.Proc := @Saw_Waveform;
        Result.Attack := 0.25;
        Result.Decay := 0.5;
        Result.Sustain := 0.9;
        Result.Release := 0.4;
      end;
    mpFX:
      begin
        Result.Proc := @Noise_Waveform;
        Result.Noise := True;
        Result.Freq := 8000;
        Result.FreqTarget := 8000;
        Result.Attack := 0.005;
        Result.Decay := 0.4;
        Result.Sustain := 0.3;
        Result.Release := 0.2;
      end;
    mpEthnic:
      begin
        Result.Proc := @Saw_Waveform;
        Result.Attack := 0.01;
        Result.Decay := 0.4;
        Result.Sustain := 0.6;
        Result.Release := 0.15;
      end;
  else
    begin
      Result.Proc := @Noise_Waveform;
      Result.Noise := True;
      Result.Freq := 8000;
      Result.FreqTarget := 8000;
      Result.Attack := 0.001;
      Result.Decay := 0.12;
      Result.Sustain := 0;
      Result.Release := 0.03;
    end;
  end;
end;

{ TMidiPlayer }

constructor TMidiPlayer.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FState := mpIdle;
  FSampleRate := cMidiSampleRate;
  SetLength(FVoices, cMidiVoices);
  SetLength(FBlock, cMidiMaxBlockFrames * 2);
  ResetChannels;
  RayUpdates.Add(Self);
end;

destructor TMidiPlayer.Destroy;
begin
  Stop;
  FLock.Free;
  RayUpdates.Remove(Self);
  inherited Destroy;
end;

procedure TMidiPlayer.ResetChannels;
var
  I: Integer;
begin
  for I := 0 to High(FChannels) do
  begin
    FChannels[I].Patch := 0;
    FChannels[I].Volume := 1;
    FChannels[I].Pan := 0;
    FChannels[I].Bend := 0;
    FChannels[I].Sustain := False;
  end;
end;

procedure TMidiPlayer.CloseStream;
begin
  if FStream = nil then
    Exit;
  //stop feeding the analyzer before the stream it reads from goes away
  if Spectrum <> nil then
    Spectrum.Detach;
  FreeAndNil(FStream);
end;

function TMidiPlayer.OpenStream: Boolean;
begin
  RayLibSound.Open;
  if not IsAudioDeviceReady then
  begin
    Result := False;
    Exit;
  end;
  //f32 samples, interleaved stereo: the rate the song is prepared for, and the
  //layout the spectrum processor reads
  FStream := TRayAudio.Create(FSampleRate, 32, 2);
  Result := FStream <> nil;
  if Result and (Spectrum <> nil) then
    Spectrum.Attach(FStream.Stream);
end;

procedure TMidiPlayer.SetError(const AMessage: string);
begin
  FLock.Enter;
  try
    FError := AMessage;
  finally
    FLock.Leave;
  end;
  FState := mpError;
end;

procedure TMidiPlayer.Finished;
begin
  CloseStream;
  FState := mpStopped;
end;

procedure TMidiPlayer.Play(const AFileName: string);
var
  aSong: TMidiSong;
  I: Integer;
begin
  Stop;
  aSong := TMidiSong.Create;
  try
    if not aSong.LoadFromFile(AFileName) then
    begin
      SetError(aSong.Error);
      Exit;
    end;
    aSong.Prepare(FSampleRate);

    if not OpenStream then
    begin
      SetError('the audio device is not available');
      Exit;
    end;

    //the new song goes in whole, so a getter on the script thread never sees a
    //half filled one
    FLock.Enter;
    try
      FreeAndNil(FSong);
      FSong := aSong;
      aSong := nil;
      FName := FSong.Name;
      FFileName := AFileName;
      FError := '';
    finally
      FLock.Leave;
    end;

    ResetChannels;
    for I := 0 to High(FVoices) do
      FillChar(FVoices[I], SizeOf(TMidiVoice), 0);
    SetLength(FCursors, FSong.TrackCount);
    for I := 0 to High(FCursors) do
      FCursors[I] := 0;
    FSamples := 0;
    FLastClock := Milliseconds;
    RefreshNextEvent;
    FState := mpPlaying;
  finally
    aSong.Free;
  end;
end;

procedure TMidiPlayer.Pause;
begin
  if FState <> mpPlaying then
    Exit;
  if FStream <> nil then
    FStream.Pause;
  //Update stands still while the player is held, so the wall clock is read again
  //here: otherwise the time spent paused would be rendered all at once on resume
  FLastClock := Milliseconds;
  FState := mpPaused;
end;

procedure TMidiPlayer.Resume;
begin
  if FState <> mpPaused then
    Exit;
  //nothing was rendered while the player was held, so the device simply picks
  //the song up where it left and there is nothing to catch up on
  if FStream <> nil then
    FStream.Play;
  FLastClock := Milliseconds;
  FState := mpPlaying;
end;

procedure TMidiPlayer.Stop;
begin
  CloseStream;
  FLock.Enter;
  try
    FState := mpStopped;
  finally
    FLock.Leave;
  end;
end;

{ Voices }

// An idle voice, else the oldest one already on its way out, else the oldest of
// all. Stealing a note that still rings is what keeps a dense file from going
// quiet instead of merely thinning out.
function TMidiPlayer.AllocVoice(AChannel, ANote: Integer): Integer;
var
  I, aBest: Integer;
  aVoice: ^TMidiVoice;
begin
  aBest := -1;
  for I := 0 to High(FVoices) do
    if not FVoices[I].Active then
    begin
      aBest := I;
      Break;
    end;
  if aBest < 0 then
    for I := 0 to High(FVoices) do
      if FVoices[I].Stage = mesRelease then
        if (aBest < 0) or (FVoices[I].Started < FVoices[aBest].Started) then
          aBest := I;
  if aBest < 0 then
  begin
    aBest := 0;
    for I := 1 to High(FVoices) do
      if FVoices[I].Started < FVoices[aBest].Started then
        aBest := I;
  end;

  aVoice := @FVoices[aBest];
  aVoice.Active := True;
  aVoice.Channel := AChannel;
  aVoice.Note := ANote;
  aVoice.Pos := 0;
  aVoice.Env := 0;
  aVoice.Stage := mesAttack;
  aVoice.Started := FSamples;
  Result := aBest;
end;

// Every voice of a note goes, oldest first: a pitch struck twice has two voices,
// and only the last one belongs to the key that has just come up.
procedure TMidiPlayer.ReleaseVoices(AChannel, ANote: Integer);
var
  I, aBest: Integer;
  aHeld: Single;
begin
  repeat
    aBest := -1;
    for I := 0 to High(FVoices) do
      if FVoices[I].Active and (FVoices[I].Channel = AChannel) and
        (FVoices[I].Note = ANote) and (FVoices[I].Stage <> mesRelease) then
        if (aBest < 0) or (FVoices[I].Started < FVoices[aBest].Started) then
          aBest := I;
    if aBest < 0 then
      Break;
    //a note that was held briefly cannot have a long tail: its release is capped
    //at half of what it was held for, or the next note of the same pitch would
    //ring into this one
    aHeld := (FSamples - FVoices[aBest].Started) / FSampleRate;
    if aHeld > 2 then
      aHeld := 2;
    if aHeld * 0.5 + 0.02 < FVoices[aBest].Release then
      FVoices[aBest].Release := aHeld * 0.5 + 0.02;
    if FVoices[aBest].Release < 0.01 then
      FVoices[aBest].Release := 0.01;
    FVoices[aBest].Stage := mesRelease;
    FVoices[aBest].ReleaseRate := 1 / (FVoices[aBest].Release * FSampleRate);
  until False;
end;

// Everything on a channel goes, which is what the all notes off controllers do
procedure TMidiPlayer.ReleaseChannel(AChannel: Integer);
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    if FVoices[I].Active and (FVoices[I].Channel = AChannel) and
      (FVoices[I].Stage <> mesRelease) then
    begin
      FVoices[I].Stage := mesRelease;
      FVoices[I].ReleaseRate := 1 / (FVoices[I].Release * FSampleRate);
    end;
end;

procedure TMidiPlayer.NoteOn(AChannel, ANote, AVelocity: Integer);
var
  aTone: TMidiTone;
  v: Integer;
  aVel, aPan, aAngle: Single;
begin
  if (AChannel < 0) or (AChannel > High(FChannels)) or (ANote < 0) or (ANote > 127) then
    Exit;
  aTone := ToneOf(AChannel, FChannels[AChannel].Patch, ANote);
  v := AllocVoice(AChannel, ANote);

  if aTone.Noise then
  begin
    FVoices[v].Drum := True;
    FVoices[v].Freq := aTone.Freq;
    FVoices[v].FreqTarget := aTone.FreqTarget;
    FVoices[v].FreqSlew := aTone.FreqSlew;
  end
  else
  begin
    FVoices[v].Drum := False;
    FVoices[v].Freq := NoteFreq(ANote);
    FVoices[v].FreqTarget := FVoices[v].Freq;
    FVoices[v].FreqSlew := 0;
  end;
  FVoices[v].Proc := aTone.Proc;
  if not Assigned(FVoices[v].Proc) then
    FVoices[v].Proc := @Sin_Waveform;

  //velocity, then the channel volume, then the pan split, so the two sides of a
  //note keep the level they would have alone
  aVel := (AVelocity / 127) * FChannels[AChannel].Volume * 0.22;
  if aVel > 1 then
    aVel := 1;
  aPan := FChannels[AChannel].Pan;
  if aPan < -1 then
    aPan := -1;
  if aPan > 1 then
    aPan := 1;
  aAngle := (aPan + 1) * Pi / 4; //equal power across the field
  FVoices[v].AmpL := Cos(aAngle) * aVel;
  FVoices[v].AmpR := Sin(aAngle) * aVel;

  FVoices[v].SustainLevel := aTone.Sustain;
  FVoices[v].AttackRate := 1 / (Max(aTone.Attack, 0.0005) * FSampleRate);
  if aTone.Sustain >= 1 then
    FVoices[v].DecayRate := 0 //there is nothing to fall to
  else if aTone.Decay <= 0 then
    FVoices[v].DecayRate := 1 / FSampleRate
  else
    FVoices[v].DecayRate := (1 - aTone.Sustain) / (aTone.Decay * FSampleRate);
  FVoices[v].Release := Max(aTone.Release, 0.01);
  FVoices[v].ReleaseRate := 1 / (FVoices[v].Release * FSampleRate);
end;

procedure TMidiPlayer.NoteOff(AChannel, ANote: Integer);
var
  I: Integer;
begin
  if (AChannel < 0) or (AChannel > High(FChannels)) then
    Exit;
  if FChannels[AChannel].Sustain then
  begin
    //the pedal is down: the keys are up but the notes ring on until it lifts
    for I := 0 to High(FVoices) do
      if FVoices[I].Active and (FVoices[I].Channel = AChannel) and
        (FVoices[I].Note = ANote) and (FVoices[I].Stage <> mesRelease) then
        FVoices[I].Stage := mesSustain;
    Exit;
  end;
  ReleaseVoices(AChannel, ANote);
end;

procedure TMidiPlayer.Control(AChannel, AController, AValue: Integer);
begin
  if (AChannel < 0) or (AChannel > High(FChannels)) then
    Exit;
  case AController of
    7: FChannels[AChannel].Volume := AValue / 127;
    10: FChannels[AChannel].Pan := (AValue - 64) / 63;
    64:
      begin
        FChannels[AChannel].Sustain := AValue >= 64;
        if not FChannels[AChannel].Sustain then
          ReleaseChannel(AChannel);
      end;
    120, 123: ReleaseChannel(AChannel);
  end;
end;

procedure TMidiPlayer.PitchBend(AChannel, AValue: Integer);
var
  I: Integer;
  aSemitones: Single;
begin
  if (AChannel < 0) or (AChannel > High(FChannels)) then
    Exit;
  //a 14 bit value around 8192, the centre being no bend at all
  FChannels[AChannel].Bend := (AValue - 8192) / 8192;
  aSemitones := FChannels[AChannel].Bend * cMidiBendRange;
  for I := 0 to High(FVoices) do
    if FVoices[I].Active and (FVoices[I].Channel = AChannel) and not FVoices[I].Drum then
    begin
      //the slide keeps a bend from stepping on every voice at once
      FVoices[I].FreqSlew := 0.05;
      FVoices[I].FreqTarget := NoteFreq(FVoices[I].Note + Round(aSemitones));
    end;
end;

{ The clock }

// The soonest event of any track, in samples. Every track shares one clock, so
// this is what a frame waits for before it renders anything.
procedure TMidiPlayer.RefreshNextEvent;
var
  I: Integer;
  aTrack: TMidiTrack;
begin
  FNextEventSample := -1;
  FNextTrack := -1;
  if FSong = nil then
    Exit;
  for I := 0 to FSong.TrackCount - 1 do
  begin
    aTrack := TMidiTrack(FSong.Tracks[I]);
    if (I <= High(FCursors)) and (FCursors[I] <= High(aTrack.Messages)) then
      if (FNextEventSample < 0) or (aTrack.Messages[FCursors[I]].Sample < FNextEventSample) then
      begin
        FNextEventSample := aTrack.Messages[FCursors[I]].Sample;
        FNextTrack := I;
      end;
  end;
end;

procedure TMidiPlayer.DispatchEvent;
var
  aTrack: TMidiTrack;
  aMsg: TMidiMessage;
  aChannel: Integer;
begin
  if (FNextTrack < 0) or (FSong = nil) then
    Exit;
  aTrack := TMidiTrack(FSong.Tracks[FNextTrack]);
  if FCursors[FNextTrack] > High(aTrack.Messages) then
  begin
    RefreshNextEvent;
    Exit;
  end;
  aMsg := aTrack.Messages[FCursors[FNextTrack]];
  Inc(FCursors[FNextTrack]);
  aChannel := MidiStatusChannel(aMsg.Status);
  case MidiStatusKind(aMsg.Status) of
    msNoteOn: NoteOn(aChannel, aMsg.Data1, aMsg.Data2);
    msNoteOff: NoteOff(aChannel, aMsg.Data1);
    msControl: Control(aChannel, aMsg.Data1, aMsg.Data2);
    msProgram: if (aChannel >= 0) and (aChannel <= High(FChannels)) then
      FChannels[aChannel].Patch := aMsg.Data1;
    msPitchBend: PitchBend(aChannel, aMsg.Data1 + aMsg.Data2 * 128);
  else
    ; //msOther never reaches a track: the reader drops it while it parses
  end;
  RefreshNextEvent;
end;

{ Rendering }

// One voice, one sample: slide the pitch, step the envelope, read the waveform.
// The generators take a running index, so a voice counts its own samples; the
// index stays inside a positive 32 bit range so a long note cannot run into an
// integer overflow.
function TMidiPlayer.RenderVoice(AIndex: Integer): Single;
var
  v: ^TMidiVoice;
  aPhase: Integer;
begin
  v := @FVoices[AIndex];
  if v.FreqSlew > 0 then
  begin
    v.Freq := v.Freq + (v.FreqTarget - v.Freq) * v.FreqSlew;
    if Abs(v.FreqTarget - v.Freq) < 0.5 then
    begin
      v.Freq := v.FreqTarget;
      v.FreqSlew := 0;
    end;
  end
  else
    v.Freq := v.FreqTarget;

  case v.Stage of
    mesAttack:
      begin
        v.Env := v.Env + v.AttackRate;
        if v.Env >= 1 then
        begin
          v.Env := 1;
          v.Stage := mesDecay;
        end;
      end;
    mesDecay:
      begin
        v.Env := v.Env - v.DecayRate;
        if v.Env <= v.SustainLevel then
        begin
          v.Env := v.SustainLevel;
          //a drum holds nothing: once it has fallen it is done
          if v.SustainLevel <= 0 then
            v.Stage := mesRelease
          else
            v.Stage := mesSustain;
        end;
      end;
    mesSustain: ;
    mesRelease:
      begin
        v.Env := v.Env - v.ReleaseRate;
        if v.Env <= 0 then
        begin
          v.Env := 0;
          v.Active := False;
        end;
      end;
  end;

  aPhase := Integer(v.Pos and $7FFFFFFF);
  Result := v.Proc(aPhase, FSampleRate, v.Freq) * v.Env;
  Inc(v.Pos);
end;

// AFrames frames of the mix of every voice that is ringing. An event whose
// sample falls inside the block is acted on before the sample it belongs to is
// rendered, so an onset is heard on its own tick instead of a block late.
procedure TMidiPlayer.Render(AFrames: Integer);
var
  I, V: Integer;
  aCur, aEnd: Int64;
  aLeft, aRight, aSample: Single;
begin
  aCur := FSamples;
  aEnd := FSamples + AFrames;
  for I := 0 to AFrames - 1 do
  begin
    while (FNextEventSample >= 0) and (FNextEventSample <= aCur) do
      DispatchEvent;
    aLeft := 0;
    aRight := 0;
    for V := 0 to High(FVoices) do
      if FVoices[V].Active then
      begin
        aSample := RenderVoice(V);
        aLeft := aLeft + aSample * FVoices[V].AmpL;
        aRight := aRight + aSample * FVoices[V].AmpR;
      end;
    //voices add up, so the sum can pass one; a soft knee keeps that a rounding
    //of the loudness instead of a hard clip
    if aLeft > 1 then
      aLeft := 1 / (1 + (aLeft - 1) * 0.5)
    else if aLeft < -1 then
      aLeft := -1 / (1 + (-aLeft - 1) * 0.5);
    if aRight > 1 then
      aRight := 1 / (1 + (aRight - 1) * 0.5)
    else if aRight < -1 then
      aRight := -1 / (1 + (-aRight - 1) * 0.5);
    FBlock[I * 2] := aLeft;
    FBlock[I * 2 + 1] := aRight;
    Inc(aCur);
  end;
  FSamples := aEnd;
end;

procedure TMidiPlayer.Update;
var
  aWanted: Integer;
  aNow: QWord;
begin
  if FState <> mpPlaying then
    Exit;
  if (FSong = nil) or (FStream = nil) then
    Exit;

  //Render as many frames as the wall clock says have gone by since the last
  //Update. That clock does not care whether a frame was drawn, so a script that
  //never shows a window still plays, and a frame that took too long is made up
  //for by the next one rather than left as a gap in the song.
  aNow := Milliseconds;
  if aNow <= FLastClock then
    Exit;
  aWanted := Round((aNow - FLastClock) / 1000 * FSampleRate);
  FLastClock := aNow;
  if aWanted <= 0 then
    Exit;
  if aWanted > cMidiMaxBlockFrames then
  begin
    //A stall. Keep the song clock where the wall clock is and let the audio that
    //could not be rendered in time go, or the song would stay behind for good.
    Inc(FSamples, aWanted - cMidiMaxBlockFrames);
    aWanted := cMidiMaxBlockFrames;
  end;

  Render(aWanted);
  FStream.UpdateData(@FBlock[0], aWanted);

  //the last event is past and the tail it leaves has been rendered
  if (FNextEventSample < 0) and (FSamples >= FSong.LengthSamples) then
    Finished;
end;

{ Getters. The main thread owns the player and writes these while a script
  thread reads them, so each is read under the lock. }

function TMidiPlayer.GetStateString: string;
begin
  case FState of
    mpIdle: Result := 'idle';
    mpPlaying: Result := 'playing';
    mpPaused: Result := 'paused';
    mpError: Result := 'error';
  else
    Result := 'stopped';
  end;
end;

function TMidiPlayer.GetPlaying: Boolean;
begin
  Result := FState = mpPlaying;
end;

function TMidiPlayer.GetError: string;
begin
  FLock.Enter;
  try
    Result := FError;
  finally
    FLock.Leave;
  end;
end;

function TMidiPlayer.GetName: string;
begin
  FLock.Enter;
  try
    Result := FName;
  finally
    FLock.Leave;
  end;
end;

function TMidiPlayer.GetFileName: string;
begin
  FLock.Enter;
  try
    Result := FFileName;
  finally
    FLock.Leave;
  end;
end;

function TMidiPlayer.GetPosition: Single;
begin
  if FSampleRate <= 0 then
    Result := 0
  else
    Result := FSamples / FSampleRate;
end;

function TMidiPlayer.GetLength: Single;
begin
  Result := 0;
  FLock.Enter;
  try
    if FSong <> nil then
      Result := FSong.LengthSeconds;
  finally
    FLock.Leave;
  end;
end;

function TMidiPlayer.GetTracks: Integer;
begin
  Result := 0;
  FLock.Enter;
  try
    if FSong <> nil then
      Result := FSong.TrackCount;
  finally
    FLock.Leave;
  end;
end;

// Quarter notes a minute, the unit a tempo is written in. A quarter note lasts
// UsPerQuarter microseconds, so 60 million divided by that is the answer.
function TMidiPlayer.GetTempo: Integer;
var
  aUs: Int64;
begin
  Result := 0;
  FLock.Enter;
  try
    if FSong <> nil then
    begin
      aUs := FSong.TempoAtSample(FSamples);
      if aUs > 0 then
        Result := Round(60000000 / aUs);
    end;
  finally
    FLock.Leave;
  end;
end;

initialization
  MidiPlayer := TMidiPlayer.Create;

finalization
  FreeAndNil(MidiPlayer);

end.
