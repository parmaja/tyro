unit TyroSounds;
{$IFDEF FPC}
{$MODE delphi}
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
  RayLib3, RayClasses;

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

procedure PlayWaveform(Freq, Duration: Single);

function Noise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Sin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Piano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;

implementation

{ TTyroRayAudio }

procedure TTyroRayAudio.Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single; SampleRate: Integer; BitRate: Integer);
var
  Data: Pointer;
  SampleCount, SampleSize: Cardinal;
  i: Cardinal;
  v: Smallint;
  {$ifdef FADE}
  WaveSamples: Cardinal;
  Starting, Ending: Cardinal;
  Delta: Single;
  {$endif}
  aSize: Integer;
begin
  SampleCount := Round(Duration * SampleRate);
  SampleSize := Sizeof(Smallint) * 8; // I use 16 bit only
  aSize := SampleCount * Sizeof(Smallint);
  Data := RayLib3.MemAlloc(aSize);
  if Frequency <> 0 then
  begin
    Amplitude := (Amplitude * ((Power(2, SampleSize) / 2) - 1) / 100) - 1;
    {$ifdef FADE}
    WaveSamples := SampleRate div round(Frequency);
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
      PSmallInt(Data)[i] := v;
    end;
  end
  else
    FillChar(Data^, aSize, #0);
  UpdateData(Data, SampleCount);

  if Data <> nil then //maybe move it to generate
  begin
{    RayLib3.MemFree(Data);
    Data := nil;}
  end;
end;

{ TTyroRayWave }

procedure TTyroRayWave.Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single; SampleRate: Integer; BitRate: Integer);
var
  Wave: TWave;
  i: Cardinal;
  v: Smallint;
  {$ifdef FADE}
  WaveSamples: Cardinal;
  Starting, Ending: Cardinal;
  Delta: Single;
  {$endif}
  aSize: Integer;
begin
  Wave.SampleCount := Round(Duration * SampleRate);
  Wave.SampleRate := SampleRate; // By default 44100 Hz
  Wave.SampleSize := Sizeof(Smallint) * 8; // I use 16 bit only
  Wave.Channels := 1;                  // By default 1 channel (mono)
  aSize := Wave.SampleCount * Sizeof(Smallint);
  Wave.Data := GetMem(aSize);
  if Frequency <> 0 then
  begin
    Amplitude := (Amplitude * ((Power(2, Wave.SampleSize) / 2) - 1) / 100) - 1;
    {$ifdef FADE}
    WaveSamples := SampleRate div round(Frequency);
    Starting := WaveSamples * 3;
    Ending := Wave.SampleCount - WaveSamples * 3;
    Delta := 100 / (WaveSamples * 3);
    {$endif}
    for i := 0 to Wave.SampleCount -1 do
    begin
      v := Round(Proc(i, SampleRate, Frequency) * Amplitude);
      {$ifdef FADE}
      if i < Starting then
        v := Round(v * i * Delta / 100);
      if i > Ending then
        v := Round(v * (Wave.SampleCount - i) * Delta / 100);
      {$endif}
      PSmallInt(Wave.Data)[i] := v;
    end;
  end
  else
    FillChar(Wave.Data^, aSize, #0);
  //ExportWave(Wave, PChar('c:\temp\'+IntTOStr(round(Frequency))+'.wav'));
  UnloadSound(Sound);
  Sound := LoadSoundFromWave(Wave);
  if Wave.Data <> nil then //maybe move it to generate
  begin
    RayLib3.MemFree(Wave.Data);
    Wave.Data := nil;
    UnloadWave(Wave);
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
  Waveforms.Add('Noise', Noise_Waveform());
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

procedure PlayWaveform(Freq, Duration: Single);
var
  Wave: TTyroRayWave;
begin
  RayLibSound.Open;
  Wave := TTyroRayWave.Create;
  Wave.Generate(@Sin_Waveform, Freq, Duration, 100);
  Wave.Play;
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
    if WaveSamples > 0 then
    begin
      Sample := Index mod WaveSamples;
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
  fade := 1;
//  if not connected then
      fade := exp(-log10(50) * index / SampleRate / 3); //fadeout
  sample := sin(index * (2 * pi) * frequency / SampleRate);
  a := sin(index * (2 * pi) * frequency * 2 / SampleRate);
  b := sin(index * (2 * pi) * frequency / 2 / SampleRate);
  sample := (sample - a - b) / 3;
  Result := sample * fade;
end;

initialization
end.

