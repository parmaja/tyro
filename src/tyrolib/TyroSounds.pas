unit TyroSounds;
{$IFDEF FPC}
{$MODE delphi}
{$else}
{$POINTERMATH ON}
{$ENDIF}

{$M+}{$H+}
{**
 * This file is part of the 'Tyro'
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey , zaher, zaherdirkey
 *
 *}

{////////////////////////////////////////////////////////////////////////////-
*  	Music Macro Language
*   https://en.wikipedia.org/wiki/Music_Macro_Language
*
*   This file is used part of the 'Tyro'
*   @license   The MIT License (MIT) Included in this distribution
*   @author    Zaher Dirkey <zaherdirkey at yahoo dot com>
//////////////////////////////////////////////////////////////////////////////-
*  Look at this site, it have many of songs
*  https://archeagemmllibrary.com/
*	 usefull refereces
*
*  http://web.mit.edu/18.06/www/Essays/linear-algebra-and-music.pdf
//////////////////////////////////////////////////////////////////////////////-}

interface

uses
  Classes, SysUtils, mnClasses, mnUtils, Math,
  Melodies,
  RayLib, RayClasses;

{$define FADE} //fadein fadeout generated sound to reduce tick at the end of sound

type

  TWaveformProc = function(Index: Integer; SampleRate: Integer; Frequency: Single): Single;

  { TWaveForm }

  TWaveForm = class(TmnNamedObject)
  public
    Proc: TWaveformProc;
  end;

  { TWaveForms }

  TWaveForms = Class(TmnNamedObjectList<TWaveForm>)
  public
    procedure Add(Name: string; Proc: TWaveformProc);
  end;

  { TTyroRayWave }

  TTyroRayWave = class(TRaySound)
  protected
  public
    procedure Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single = 100; SampleRate: Integer = 44100; BitRate: Integer = 16);
  end;

  { TTyroRayAudio }

  TTyroRayAudio = class(TRayAudio)
  protected
  public
    procedure Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single = 100; SampleRate: Integer = 44100; BitRate: Integer = 16);
  end;

  TTyroMelodySound = TTyroRayWave;

  { TRayMelodyChannel }

  TRayMelodyChannel = class(TMelodyChannel)
  public
    Amplitude: Single;
    SampleRate: Integer;
    BitRate: Integer;
    Sound: TTyroMelodySound;
    Waveform: TWaveForm;
    procedure SetInstrument(Instrument: String); override;
    procedure SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single); override;
    function PlaySound: Boolean; override;
    function IsPlaying: Boolean; override;
    function StopSound: Boolean; override;

    procedure Prepare; override;
    procedure Update; override;
    procedure Unprepare; override;
    constructor Create(AMelody: TMelody); override;
    destructor Destroy; override;
  end;

  { TRayMelody }

  TRayMelody = class(TMelody)
  private
  protected
    procedure BeforePlay; override;
    procedure AfterPlay; override;
    function CheckTerminated: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateChannel: TMelodyChannel; override;
  end;

var
  WaveForms: TWaveForms = nil;

procedure PlayMML(const Song: TmmlSong);

procedure PlayWaveform(Proc: TWaveformProc; Freq, Duration: Single); overload;
procedure PlayWaveform(WaveformName: string; Freq, Duration: Single); overload;
procedure UpdateMelodies;
procedure UpdateWaveforms;
procedure ShutdownMelodies;
procedure ShutdownWaveforms;

function WhiteNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function PitchedNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function LFSRNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Sin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Piano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function OldPiano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Square_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Saw_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Tri_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Pulse25_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Pulse125_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Rect_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Dsin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Organ_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Bell_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Pluck_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Flute_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function FM_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function PinkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function DarkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function HashNoise(Index: Integer): Single;

implementation

var
  PlayingMelodies: TList = nil;
  PlayingWaveforms: TList = nil;

{ TTyroRayAudio }

procedure TTyroRayAudio.Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single; SampleRate: Integer; BitRate: Integer);
var
  aData: array of SmallInt;
  SampleCount, SampleSize: Integer;
  i: Integer;
  s: Int64;
  v: Smallint;
  {$ifdef FADE}
  WaveSamples: Integer;
  Starting, Ending: Integer;
  Delta: Single;
  {$endif}
begin
  if (Duration <= 0) or (SampleRate <= 0) or not Assigned(Proc) then
    Exit;
  aData := nil;
  SampleCount := Round(Duration * SampleRate);
  SampleSize := Sizeof(Smallint) * 8; // I use 16 bit only
  SetLength(aData, SampleCount);
  if Frequency <> 0 then
  begin
    Amplitude := Amplitude * ((Power(2, SampleSize) / 2) - 1) / 100;
    {$ifdef FADE}
    //Guard degenerate frequencies: round(Frequency)=0 would be a div-by-zero
    //and WaveSamples<=0 would make Delta = 100/0 = Inf (crash on Round(Inf)).
    if round(Frequency) > 0 then
      WaveSamples := SampleRate div round(Frequency)
    else
      WaveSamples := SampleRate;
    if WaveSamples <= 0 then
      WaveSamples := 1;
    Starting := WaveSamples * 3;
    //Keep the fade window inside the sound, a note shorter than 3 periods
    //would otherwise fade in over its whole length and be nearly silent.
    if Starting > SampleCount div 2 then
      Starting := SampleCount div 2;
    if Starting <= 0 then
      Starting := 1;
    Ending := SampleCount - Starting;
    Delta := 100 / Starting;
    {$endif}
    for i := 0 to SampleCount -1 do
    begin
      s := Round(Proc(i, SampleRate, Frequency) * Amplitude);
      //A waveform may overshoot +-1 a little, a wrapped SmallInt would click.
      if s > High(SmallInt) then
        s := High(SmallInt)
      else if s < Low(SmallInt) then
        s := Low(SmallInt);
      v := s;
      {$ifdef FADE}
      if i < Starting then
        v := Round(v * i * Delta / 100);
      if i > Ending then
        v := Round(v * (SampleCount - i) * Delta / 100);
      {$endif}
      aData[i] := v;
    end;
  end
  else
  begin
    for i := 0 to Length(aData) -1 do
      aData[i] := 0;
  end;
  if SampleCount > 0 then
    UpdateData(@aData[0], SampleCount);
end;

{ TTyroRayWave }

procedure TTyroRayWave.Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single; SampleRate: Integer; BitRate: Integer);
var
  Wave: TWave;
  i: Integer;
  s: Int64;
  v: Smallint;
  {$ifdef FADE}
  WaveSamples: Integer;
  Starting, Ending: Integer;
  Delta: Single;
  {$endif}
  //aData: array of SmallInt;
  aData: PSmallInt;
begin
  if (Duration <= 0) or (SampleRate <= 0) or not Assigned(Proc) then
    Exit;
  Wave := Default(TWave);
  Wave.FrameCount := Round(Duration * SampleRate);
  Wave.SampleRate := SampleRate; // By default 44100 Hz
  Wave.SampleSize := Sizeof(Smallint) * 8; // I use 16 bit only
  Wave.Channels := 1;                  // By default 1 channel (mono)
  //aData := nil;
  //SetLength(aData, Wave.FrameCount);
  if Wave.FrameCount = 0 then
    Exit;
  aData := RayLib.MemAlloc(Wave.FrameCount * SizeOf(SmallInt));
  if aData = nil then
    raise Exception.Create('Unable to allocate generated waveform');
  if Frequency <> 0 then
  begin
    Amplitude := Amplitude * ((Power(2, Wave.SampleSize) / 2) - 1) / 100;

    {$ifdef FADE}
    //Guard degenerate frequencies: round(Frequency)=0 would be a div-by-zero
    //and WaveSamples<=0 would make Delta = 100/0 = Inf (crash on Round(Inf)).
    if round(Frequency) > 0 then
      WaveSamples := SampleRate div round(Frequency)
    else
      WaveSamples := SampleRate;
    if WaveSamples <= 0 then
      WaveSamples := 1;
    Starting := WaveSamples * 3;
    //Keep the fade window inside the sound, a note shorter than 3 periods
    //would otherwise fade in over its whole length and be nearly silent.
    if Starting > Integer(Wave.FrameCount) div 2 then
      Starting := Integer(Wave.FrameCount) div 2;
    if Starting <= 0 then
      Starting := 1;
    Ending := Integer(Wave.FrameCount) - Starting;
    Delta := 100 / Starting;
    {$endif}

    for i := 0 to Integer(Wave.FrameCount) -1 do
    begin
      s := Round(Proc(i, SampleRate, Frequency) * Amplitude);
      //A waveform may overshoot +-1 a little, a wrapped SmallInt would click.
      if s > High(SmallInt) then
        s := High(SmallInt)
      else if s < Low(SmallInt) then
        s := Low(SmallInt);
      v := s;
      {$ifdef FADE}
      if i < Starting then
        v := Round(v * i * Delta / 100);
      if i > Ending then
        v := Round(v * (Integer(Wave.FrameCount) - i) * Delta / 100);
      {$endif}
      aData[i] := v;
    end;
  end
  else
    for i := 0 to Integer(Wave.FrameCount) - 1 do
      aData[i] := 0;
  Wave.Data := aData;
  //ExportWave(Wave, PChar('c:\temp\'+IntTOStr(round(Frequency))+'.wav'));
  if Sound.FrameCount <> 0 then
    UnloadSound(Sound);
  Sound := Default(TSound);
  Sound := LoadSoundFromWave(Wave);
  //RayLib.MemFree(aData);
  //aData := nil;
  //Wave.Data := nil;
  UnloadWave(Wave);
  //aData := nil;
  if Wave.Data <> nil then //maybe move it to generate
  begin
    //RayLib.MemFree(Wave.Data); //* no it is our memory
    //Wave.Data := nil;
    //UnloadWave(Wave);
  end;
end;

{ TWaveForms }

procedure TWaveForms.Add(Name: string; Proc: TWaveformProc);
var
  WaveForm: TWaveForm;
begin
  WaveForm := TWaveForm.Create;
  WaveForm.Proc := Proc;
  WaveForm.Name := Name;
  inherited Add(WaveForm);
end;

{ TRayMelodyChannel }

procedure TRayMelodyChannel.SetInstrument(Instrument: String);
begin
  inherited;
  if Instrument = '' then
    Waveform := WaveForms[0]
  else
  begin
    Waveform := WaveForms.Find(Instrument);
    if Waveform = nil then
      raise EMelodyException.Create('Waveform not exists ' + Instrument);
  end;
end;

procedure TRayMelodyChannel.SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single);
begin
  inherited;
  if Waveform = nil then
    raise EMelodyException.Create('Waveform not defined');
  //if (Frequency > 0) and (Duration > 0) then
    Sound.Generate(Waveform.Proc, Frequency, Duration, Amplitude, SampleRate, BitRate);
end;

function TRayMelodyChannel.PlaySound: Boolean;
begin
  inherited;
  Sound.Play;
  Result := True;
end;

function TRayMelodyChannel.IsPlaying: Boolean;
begin
  Result := Sound.IsPlaying;
end;

function TRayMelodyChannel.StopSound: Boolean;
begin
  inherited;
  Result := Sound.IsPlaying;
  Sound.Stop;
end;

procedure TRayMelodyChannel.Prepare;
begin
  inherited Prepare;
  Amplitude := 100;
end;

procedure TRayMelodyChannel.Update;
begin
  inherited Update;
  Sound.Update;
end;

procedure TRayMelodyChannel.Unprepare;
begin
  inherited;
end;

constructor TRayMelodyChannel.Create(AMelody: TMelody);
begin
  inherited;
  SampleRate := cDefaultSampleRate;
  BitRate := 16;
  Sound := TTyroMelodySound.Create(False);
  //Sound := TTyroMelodySound.Create;
  SetInstrument('');
end;

destructor TRayMelodyChannel.Destroy;
begin
  FreeAndNil(Sound);
  inherited;
end;

{ TMelody }

procedure TRayMelody.BeforePlay;
begin
  inherited;
  RayLibSound.Open;
end;

procedure TRayMelody.AfterPlay;
begin
  inherited AfterPlay;
  RayLibSound.Close;
end;

function TRayMelody.CheckTerminated: Boolean;
begin
  Result := inherited CheckTerminated;
  //Sleep(1);
end;

constructor TRayMelody.Create;
begin
  inherited Create;
end;

destructor TRayMelody.Destroy;
begin
  inherited Destroy;
end;

function TRayMelody.CreateChannel: TMelodyChannel;
begin
  Result := TRayMelodyChannel.Create(Self);
end;

procedure PlayMML(const Song: TmmlSong);
var
  Melody: TRayMelody;
begin
  if Length(Song) = 0 then
    Exit;
  if PlayingMelodies = nil then
    Exit;
  Melody := TRayMelody.Create;
  try
    Melody.BeginPlay(Song);
    if Melody.Playing then
    begin
      PlayingMelodies.Add(Melody);
      Melody := nil;
    end;
  finally
    Melody.Free;
  end;
end;

procedure PlayWaveform(Proc: TWaveformProc; Freq, Duration: Single);
var
  Wave: TTyroRayWave;
begin
  if (Duration <= 0) or (Freq < 0) or not Assigned(Proc) then
    Exit;
  if PlayingWaveforms = nil then
    Exit;
  RayLibSound.Open;
  Wave := TTyroRayWave.Create;
  try
    // Lua's music.sound duration is expressed in milliseconds.
    Wave.Generate(Proc, Freq, Duration / 1000, 100);
    Wave.Play;
    PlayingWaveforms.Add(Wave);
    Wave := nil;
  finally
    Wave.Free;
  end;
end;

procedure PlayWaveform(WaveformName: string; Freq, Duration: Single); overload;
var
  wf: TWaveForm;
  wp: TWaveformProc;
begin
  wf :=WaveForms.Find(WaveformName);
  if wf = nil then
    wp := @Sin_Waveform
  else
    wp := wf.Proc;
  PlayWaveform(wp, Freq, Duration);
end;

procedure UpdateMelodies;
var
  I: Integer;
  Melody: TRayMelody;
begin
  if PlayingMelodies = nil then
    Exit;
  for I := PlayingMelodies.Count - 1 downto 0 do
  begin
    Melody := TRayMelody(PlayingMelodies[I]);
    if not Melody.UpdatePlay then
    begin
      PlayingMelodies.Delete(I);
      Melody.Free;
    end;
  end;
end;

procedure UpdateWaveforms;
var
  I: Integer;
  Wave: TTyroRayWave;
begin
  if PlayingWaveforms = nil then
    Exit;
  for I := PlayingWaveforms.Count - 1 downto 0 do
  begin
    Wave := TTyroRayWave(PlayingWaveforms[I]);
    if not Wave.IsPlaying then
    begin
      PlayingWaveforms.Delete(I);
      Wave.Free;
    end;
  end;
end;

procedure ShutdownWaveforms;
var
  I: Integer;
begin
  if PlayingWaveforms = nil then
    Exit;
  for I := PlayingWaveforms.Count - 1 downto 0 do
    TObject(PlayingWaveforms[I]).Free;
  PlayingWaveforms.Clear;
end;

procedure ShutdownMelodies;
var
  I: Integer;
begin
  if PlayingMelodies = nil then
    Exit;
  for I := PlayingMelodies.Count - 1 downto 0 do
    TObject(PlayingMelodies[I]).Free;
  PlayingMelodies.Clear;
end;

function LFSRNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Period, T: Integer;
  LFSR: Word;
  Bit: Word;
  i: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := GetRandomValue(-100,+100) / 100;
    Exit;
  end;
  Period := Round(SampleRate / Frequency);
  if Period < 1 then Period := 1;
  T := Index div Period;
  // seed LFSR based on period; the multiply wraps on purpose, so keep it in
  // UInt64 and take the low word instead of overflowing a 32 bit integer.
  LFSR := Word((UInt64(T) * 131542391 + 1009) and $FFFF);
  if LFSR = 0 then LFSR := $ACE1;
  // advance LFSR a few steps to get variation
  for i := 0 to (T mod 8) do
  begin
    Bit := ((LFSR shr 0) xor (LFSR shr 2) xor (LFSR shr 3) xor (LFSR shr 5)) and 1;
    LFSR := (LFSR shr 1) or (Bit shl 15);
  end;
  // output bit as -1 or 1 for harsher tone
  if (LFSR and 1) = 1 then
    Result := 1.0
  else
    Result := -1.0;
end;

function PitchedNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
begin
  // LFSR-based pitched noise (periodic like NES noise)
  Result := LFSRNoise_Waveform(Index, SampleRate, Frequency);
end;

function WhiteNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
begin
  Result := GetRandomValue(-100,+100) / 100;
end;

function Sin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
begin
  //The old version mixed 'Index mod Round(period)' with '/ period' (unrounded),
  //which made the phase jump every cycle and caused an audible glitch.
  //A sine is naturally periodic, no wrapping is needed at all.
  if (Frequency <= 0) or (SampleRate <= 0) then
    Result := 0
  else
    Result := Sin(2 * Pi * Frequency * Index / SampleRate);
  //WriteLn('Sample = ' + FloatTOStr(Sample) + ' WaveSamples = '+ FloatToStr(wavesamples) + ' Result: ' + FloatToStr(Result));
end;

//Additive piano: inharmonic partials, per-partial decay, hammer transient.
//The old version was (sin f - sin 2f - sin f/2) with one shared envelope:
//  - exp(-log10(50)*t/3) only fell to 18% after 3s (log10, not ln), so notes
//    sounded sustained like an organ instead of dying.
//  - the f/2 term is a subharmonic, pianos do not have one, and it was as loud
//    as the fundamental so the pitch read an octave low and muddy.
//  - one envelope for every partial kept the timbre static; a real piano loses
//    its brightness first because the upper partials decay faster.
function Piano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  Inharmonicity = 0.0004; //string stiffness, partials sit slightly sharp of k*f
  PartialCount = 8;
var
  t, Rate, Sum, Norm, Amp, Decay, Fk, Attack: Double;
  k: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Rate := 0.7 + Frequency / 880; //higher notes die quicker
  Attack := 1 - Exp(-t * 400);   //~2.5ms hammer attack
  Sum := 0;
  Norm := 0;
  for k := 1 to PartialCount do
  begin
    Fk := Frequency * k * Sqrt(1 + Inharmonicity * k * k);
    if Fk >= SampleRate / 2 then
      Break;
    Amp := 1 / (k * Sqrt(k));                      //rolls off faster than 1/k
    Decay := Exp(-Rate * (0.6 + 0.5 * k) * t);     //brightness dies first
    Sum := Sum + Amp * Sin(2 * Pi * Fk * Index / SampleRate) * Decay;
    Norm := Norm + Amp;
  end;
  if Norm > 0 then
    Sum := Sum / Norm;
  //Hammer noise at the start of the note
  Result := (Sum + HashNoise(Index) * Exp(-t * 150) * 0.12) * Attack;
end;

//The original piano generator, kept under a separate name so it can be A/B
//compared with the additive one above. It is a fixed (sin f - sin 2f - sin f/2)
//wave times a single envelope: exp(-log10(50)*t/3) is log10, not ln, so it only
//falls to ~18% after 3s, and the f/2 term is a subharmonic pianos do not have.
//ref: http://web.mit.edu/6.02/www/s2007/lab2.pdf
function OldPiano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  a, b,
  Sample, Fade: Single;
begin
//https://stackoverflow.com/questions/20037947/fade-out-function-of-audio-between-samplerate-changes
//  if not connected then
  fade := exp(-log10(50) * index / SampleRate / 3); //fadeout
  sample := sin(index * (2 * pi) * frequency / SampleRate);
  a := sin(index * (2 * pi) * frequency * 2 / SampleRate);
  b := sin(index * (2 * pi) * frequency / 2 / SampleRate);
  sample := (sample - a - b) / 3;
  Result := sample * fade;
end;

// Where in the wave Index stands, 0..1. Returns -1 only for a frequency or
// sample rate that has no meaningful period.
function WavePosition(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  //Was '(Index mod Round(SampleRate/Frequency)) / Round(...)' which quantized
  //the pitch (440 Hz became 441 Hz). Frac() keeps the exact frequency.
  Result := -1;
  if (Frequency <= 0) or (SampleRate <= 0) then
    Exit;
  Phase := Frequency; //widen to Double before multiplying by Index
  Result := Frac(Phase * Index / SampleRate);
end;

function Square_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.5 then
    Result := 1
  else
    Result := -1;
end;

function Saw_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else
    Result := (Pos * 2) - 1;
end;

function Tri_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.5 then
    Result := (Pos * 4) - 1
  else
    Result := 3 - (Pos * 4);
end;

{ --- deterministic noise helpers -------------------------------------------
  A waveform proc has no memory between calls, so the noise layers derive their
  randomness from the index. Same index always gives the same value, which also
  makes an MML note render the same again on every play. }

function HashNoise(Index: Integer): Single;
var
  h: UInt64;
begin
  //A 32 bit wrap is intended here. Kept in UInt64 and masked so it wraps the
  //same way but does not trip the overflow/range checks the application is
  //built with ({$Q+}, {$R+}), which used to raise ERangeError on this line.
  h := (UInt64(Cardinal(Index)) * 1664525 + 1013904223) and $FFFFFFFF;
  h := h xor (h shr 13);
  h := (h * 1274126177) and $FFFFFFFF;
  Result := (h and $7FFFFFFF) / 1073741824 - 1;
end;

{ --- new waveforms --------------------------------------------------------- }

// NES/Game Boy style pulse, 25% duty (thinner than Square)
function Pulse25_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.25 then
    Result := 1
  else
    Result := -1;
end;

// Even thinner 12.5% duty pulse, very nasal/chip-like
function Pulse125_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.125 then
    Result := 1
  else
    Result := -1;
end;

// Rectified sine, Abs(Sin), hollow organ-ish tone
function Rect_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else
    Result := Abs(Sin(2 * Pi * Pos));
end;

// Sine shaped by its own absolute value, smoother than square with a little grit
function Dsin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
  s: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else
  begin
    s := Sin(2 * Pi * Pos);
    Result := s * Abs(s);
  end;
end;

// Additive harmonics, Hammond-style organ. Normalized by the sum of weights.
function Organ_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  Result := (Sin(Phase) + 0.6 * Sin(2 * Phase) + 0.4 * Sin(3 * Phase) + 0.3 * Sin(4 * Phase)) / 2.3;
end;

// Inharmonic partials each with its own decay, struck-bell character.
function Bell_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase, t: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  t := Index / SampleRate;
  Result := (Sin(Phase) * Exp(-1.5 * t)
           + 0.6 * Sin(2.76 * Phase) * Exp(-2.8 * t)
           + 0.4 * Sin(5.40 * Phase) * Exp(-4.2 * t)
           + 0.25 * Sin(8.93 * Phase) * Exp(-6.0 * t)) / 2.25;
end;

// Karplus-Strong flavored pluck: harmonics decay faster the higher they are,
// which is what makes a plucked string lose its brightness over time.
function Pluck_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  f, MaxH, k: Integer;
  Sum, Norm, PhaseStep, t, Decay: Double;
begin
  Result := 0;
  if (Frequency <= 0) or (SampleRate <= 0) then
    Exit;
  f := Round(Frequency);
  if f < 1 then
    f := 1;
  //Only harmonics below Nyquist can be heard, cap the count for speed.
  MaxH := (SampleRate div 2) div f;
  if MaxH > 24 then
    MaxH := 24;
  if MaxH < 1 then
  begin
    Result := Sin(2 * Pi * Frequency * Index / SampleRate);
    Exit;
  end;
  PhaseStep := 2 * Pi * Frequency / SampleRate;
  t := Index / SampleRate;
  Sum := 0;
  Norm := 0;
  for k := 1 to MaxH do
  begin
    Decay := Exp(-t * k * 3.0);
    Sum := Sum + Sin(PhaseStep * k * Index) * Decay / k;
    Norm := Norm + 1 / k;
  end;
  Result := Sum / Norm;
end;

// Mostly sine with soft upper harmonics plus a breath of noise.
function Flute_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  Result := (Sin(Phase) + 0.15 * Sin(2 * Phase) + 0.05 * Sin(3 * Phase)) / 1.3
          + HashNoise(Index) * 0.02;
end;

// Two operator FM, one function covers metal/brass/electric-piano flavors.
function FM_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  Result := Sin(Phase + 2.0 * Sin(2 * Phase));
end;

// Voss style pink noise: one random layer per octave, held for 2^k samples.
function PinkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  k: Integer;
  Sum: Single;
begin
  Sum := 0;
  for k := 0 to 6 do
    Sum := Sum + HashNoise(Index shr k) / (k + 1);
  Result := Sum / 2.593; //sum of 1/(k+1) for k = 0..6
end;

// Brown-ish noise: octave layers with more weight on the slow (low frequency)
// ones. Summing the weights stays just under 2, so the output cannot exceed 1.
function DarkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  k: Integer;
  Sum, Weight: Double;
begin
  Sum := 0;
  Weight := 1;
  for k := 0 to 7 do
  begin
    Sum := Sum + HashNoise(Index shr k) * Weight;
    Weight := Weight / 2;
  end;
  Result := Sum / 2;
end;

initialization
  WaveForms := TWaveForms.Create;
  //Default one is the first one
  Waveforms.Add('Sin', @Sin_Waveform);
  Waveforms.Add('Piano', @Piano_Waveform);
  Waveforms.Add('OldPiano', @OldPiano_Waveform);
  Waveforms.Add('Square', @Square_Waveform);
  Waveforms.Add('Pulse25', @Pulse25_Waveform);
  Waveforms.Add('Pulse125', @Pulse125_Waveform);
  Waveforms.Add('Saw', @Saw_Waveform);
  Waveforms.Add('Tri', @Tri_Waveform);
  Waveforms.Add('Rect', @Rect_Waveform);
  Waveforms.Add('Dsin', @Dsin_Waveform);
  Waveforms.Add('Organ', @Organ_Waveform);
  Waveforms.Add('Bell', @Bell_Waveform);
  Waveforms.Add('Pluck', @Pluck_Waveform);
  Waveforms.Add('Flute', @Flute_Waveform);
  Waveforms.Add('FM', @FM_Waveform);

  //Noises
  Waveforms.Add('Noise', @WhiteNoise_Waveform);
  Waveforms.Add('Pink', @PinkNoise_Waveform);
  Waveforms.Add('Dark', @DarkNoise_Waveform);
  Waveforms.Add('Pitched', @PitchedNoise_Waveform);

  PlayingMelodies := TList.Create;
  PlayingWaveforms := TList.Create;
finalization
  ShutdownMelodies;
  ShutdownWaveforms;

  FreeAndNil(PlayingMelodies);
  FreeAndNil(PlayingWaveforms);

  FreeAndNil(WaveForms);
end.

