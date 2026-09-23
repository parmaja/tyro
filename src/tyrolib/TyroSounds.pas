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
    FWaveForms: TWaveForms;
  protected
    procedure BeforePlay; override;
    procedure AfterPlay; override;
    function CheckTerminated: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateChannel: TMelodyChannel; override;
    property WaveForms: TWaveForms read FWaveForms;
  end;

procedure PlayMML(const Song: TmmlSong);
procedure PlayWaveform(Freq, Duration: Single);
procedure UpdateMelodies;
procedure UpdateWaveforms;
procedure ShutdownMelodies;
procedure ShutdownWaveforms;

function Noise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Sin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Piano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;

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
    Amplitude := (Amplitude * ((Power(2, SampleSize) / 2) - 1) / 100) - 1;
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
    Ending := SampleCount - WaveSamples * 3;
    Delta := 100 / (WaveSamples * 3);
    {$endif}
    for i := 0 to SampleCount -1 do
    begin
      v := Round(Proc(i, SampleRate, Frequency) * Amplitude);
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
    Amplitude := (Amplitude * ((Power(2, Wave.SampleSize) / 2) - 1) / 100) - 1;

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
    Ending := Wave.FrameCount - WaveSamples * 3;
    Delta := 100 / (WaveSamples * 3);
    {$endif}

    for i := 0 to Wave.FrameCount -1 do
    begin
      v := Round(Proc(i, SampleRate, Frequency) * Amplitude);
      {$ifdef FADE}
      if i < Starting then
        v := Round(v * i * Delta / 100);
      if i > Ending then
        v := Round(v * (Wave.FrameCount - i) * Delta / 100);
      {$endif}
      aData[i] := v;
    end;
  end
  else
    for i := 0 to Wave.FrameCount - 1 do
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
    Waveform := (Melody as TRayMelody).WaveForms[0]
  else
  begin
    Waveform := (Melody as TRayMelody).WaveForms.Find(Instrument);
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
  FWaveForms := TWaveForms.Create;
  //Default one is the first one
  Waveforms.Add('Sin', Sin_Waveform);
  Waveforms.Add('Noise', @Noise_Waveform);
end;

destructor TRayMelody.Destroy;
begin
  FreeAndNil(FWaveForms);
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

procedure PlayWaveform(Freq, Duration: Single);
var
  Wave: TTyroRayWave;
begin
  if (Duration <= 0) or (Freq < 0) then
    Exit;
  if PlayingWaveforms = nil then
    Exit;
  RayLibSound.Open;
  Wave := TTyroRayWave.Create;
  try
    // Lua's music.sound duration is expressed in milliseconds.
    Wave.Generate(@Sin_Waveform, Freq, Duration / 1000, 100);
    Wave.Play;
    PlayingWaveforms.Add(Wave);
    Wave := nil;
  finally
    Wave.Free;
  end;
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

function Noise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
begin
  Result := GetRandomValue(-100,+100) / 100;
end;

function Sin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Sample, WaveSamples: Single;
begin
  if Frequency > 0 then
  begin
    WaveSamples := SampleRate / Frequency;
    //Round(WaveSamples) can be 0 for Frequency > 2*SampleRate; Index mod 0
    //would raise an integer division error.
    if Round(WaveSamples) > 0 then
    begin
      Sample := Index mod Round(WaveSamples);
      Result := Sin(2*Pi * (Sample / WaveSamples));
    end
    else
      Result := 0;
  end
  else
    Result := 0;
  //WriteLn('Sample = ' + FloatTOStr(Sample) + ' WaveSamples = '+ FloatToStr(wavesamples) + ' Result: ' + FloatToStr(Result));
end;

//ref: http://web.mit.edu/6.02/www/s2007/lab2.pdf
function Piano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
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

initialization
  PlayingMelodies := TList.Create;
  PlayingWaveforms := TList.Create;
finalization
  ShutdownMelodies;
  ShutdownWaveforms;
  FreeAndNil(PlayingMelodies);
  FreeAndNil(PlayingWaveforms);
end.

