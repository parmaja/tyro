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

type
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
    Wave: TWave;
  public
    destructor Destroy; override;
    procedure Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single = 100; SampleRate: Integer = 44100; BitRate: Integer = 16);
  end;

  { TRayMelodyChannel }

  TRayMelodyChannel = class(TMelodyChannel)
  public
    Amplitude: Single;
    SampleRate: Integer;
    BitRate: Integer;
    Sound: TTyroRayWave;
    Waveform: TWaveForm;
    procedure SetInstrument(Instrument: String); override;
    procedure SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single); override;
    function PlaySound: Boolean; override;
    function StopSound: Boolean; override;

    procedure Prepare; override;
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
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateChannel: TMelodyChannel; override;
    property WaveForms: TWaveForms read FWaveForms;
  end;

procedure PlayWaveform(Freq, Duration: Single);

implementation

{ TTyroRayWave }

destructor TTyroRayWave.Destroy;
begin
  inherited Destroy;
end;

procedure TTyroRayWave.Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single; SampleRate: Integer; BitRate: Integer);
var
  i: Integer;
  v: Smallint;
  WaveSamples: Integer;
  starting, ending: Integer;
  delta: single;
  aSize: Integer;
begin
  Wave.SampleCount := Round(Duration * SampleRate); //rest keep it empty;
  Wave.SampleRate := SampleRate; // By default 44100 Hz
  Wave.SampleSize := Sizeof(Smallint) * 8; // I use 16 bit
  Wave.Channels := 1;                  // By default 1 channel (mono)
  if Wave.Data <> nil then
    Freemem(Wave.Data);
  aSize := Wave.SampleCount * Sizeof(Smallint);
  Wave.Data := GetMem(aSize);
  if Frequency <> 0 then
  begin
    Amplitude := (Amplitude * (Power(2, Wave.SampleSize) / 2) / 100) -1;
    WaveSamples := SampleRate div round(Frequency);
    starting := WaveSamples * 3;
    ending := Wave.SampleCount - WaveSamples * 3;
    delta := 100 / (WaveSamples * 3);
    for i := 0 to Wave.SampleCount -1 do
    begin
      v := Round(Proc(i, SampleRate, Frequency) * Amplitude);

      if i < starting then
        v := round(v * i * delta / 100);
      if i > ending then
        v := round(v * (Wave.SampleCount - i) * delta / 100);
      PSmallInt(Wave.Data)[i] := v;
    end;
  end
  else
    FillChar(Wave.Data^, aSize, #0);
  ExportWave(Wave, 'c:\temp\tune.wav');
  Sound := LoadSoundFromWave(Wave);
  if Wave.Data <> nil then //maybe move it to generate
  begin
    Freemem(Wave.Data);
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
  Sound.Generate(Waveform.Proc, Frequency, Duration, Amplitude, SampleRate, BitRate);
end;

function TRayMelodyChannel.PlaySound: Boolean;
begin
  inherited;
  Sound.Play;
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

procedure TRayMelodyChannel.Unprepare;
begin
  inherited;
end;

constructor TRayMelodyChannel.Create(AMelody: TMelody);
begin
  inherited;
  SampleRate := 44100;
  BitRate := 16;
  Sound := TTyroRayWave.Create;
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

initialization
end.

