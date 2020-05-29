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

  { TRayMelodyChannel }

  TRayMelodyChannel = class(TMelodyChannel)
  public
    Amplitude: Single;
    SampleRate: Integer;
    BitRate: Integer;
    Sound: TRayWave;
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

implementation

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
  Sound.Generate(Waveform.Proc, Frequency, Duration, Amplitude, Rest, SampleRate, BitRate);
  //Sound.Generate(Waveform.Proc, 2, 1, 10, Rest, 20, BitRate);
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
  Amplitude := 1000;//MaxSmallint div 2;
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
  Sound := TRayWave.Create;
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
  //Waveforms.Add('Sin', Sin_Waveform);
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


initialization
end.

