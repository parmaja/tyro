unit RayClasses;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{**
 *  This file is part of Tyro project, ported from C header raylib.h
 *
 * @project   https://www.github.com/parmaja/tyro
 *
 * @license   MIT (https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey zaherdirkey on internet
 *
 * This library not really tested as it, I just used some functions, please be patient and report any but you find.
 *}

interface

uses
  Classes, SysUtils, Contnrs, Math,
  mnClasses, mnUtils,
  RayLib3;

type
  TRayObject = class(TObject)
  public
    ID: Integer;
  end;

  TRayUpdate = class(TRayObject)
  public
    procedure Update; virtual; abstract;
  end;

  { TRayUpdateList }

  TRayUpdateList = class(TmnObjectList<TRayUpdate>)
  public
    procedure Update;
  end;

  { Images }

  { TRayImage }

  TRayImage = class(TRayObject)
  private
    FImage: RayLib3.TImage;
  protected
  public
  end;

  { TRayPlaying }

  TRayPlaying = class(TRayUpdate)
  public
    procedure Play; virtual;
    procedure Stop; virtual;
  end;

  TWaveformProc = function(index, samples, frequency, rate:Integer; connected: Boolean): Integer;

  { TMusicPlaying }

  TMusicPlaying = class(TRayPlaying)
  public
    Music: TMusic;
    procedure Play; override;
    procedure Stop; override;
    procedure Update; override;
  end;

  { TWavePlaying }

  TWavePlaying = class(TRayPlaying)
  public
    Wave: TWave;
    Sound: TSound;
    procedure Generate(Proc: TWaveformProc; Freq, Period: Integer; Rest: Integer = 0; SampleRate: Integer = 44100; BitRate: Integer = 16);
    procedure Play; override;
    procedure Stop; override;
    procedure Update; override;
  end;

  { Sound }

  { TRayLibSound }

  TRayLibSound = class(TRayObject)
  public
    Playing: TObjectList;
    procedure Init;
    constructor Create;
    destructor Destroy; override;
    procedure PlayMusicFile(FileName: string);
    procedure PlaySound(Freq, Period: integer);
  end;

var
  RaySound: TRayLibSound = nil;
  RayUpdates: TRayUpdateList = nil;

implementation

var
  FAudioDeviceInitialized: Boolean = False;

function WaveformSin(index, samples, frequency, rate:Integer; connected: Boolean): Integer;
begin
  Result := round(sin((index * frequency) * ((2 * pi) / rate)));
end;

//ref: http://web.mit.edu/6.02/www/s2007/lab2.pdf
function WaveformPiano(index, samples, frequency, rate: Integer; Connected: Boolean): Integer;
var
  a, b,
  Sample, Fade: Single;
begin
  //https://stackoverflow.com/questions/20037947/fade-out-function-of-audio-between-samplerate-changes
  fade := 1;
  if not connected then
      fade := exp(-log10(50) * index / samples / 3); //fadeout
  sample := sin(index * (2 * pi) * frequency / rate);
  a := sin(index * (2 * pi) * frequency * 2 / rate);
  b := sin(index * (2 * pi) * frequency / 2 / rate);
  sample := (sample - a - b) / 3;
  Result := Round(sample * fade);
end;

{ TWavePlaying }

procedure TWavePlaying.Generate(Proc: TWaveformProc; Freq, Period, Rest: Integer; SampleRate: Integer; BitRate: Integer);
var
  i, aSize: Integer;
  v: Integer;
begin
  //aSize := (Period + Rest) * Rate, Rate, 16, 1) //rest keep it empty
  aSize := SampleRate;

  Wave.SampleCount := aSize;
  Wave.SampleRate := 44100; // By default 44100 Hz
  Wave.SampleSize := 16;               // By default 32 bit float samples
  Wave.channels := 1;                  // By default 1 channel (mono)
  Wave.Data := GetMem(aSize * Sizeof(smallint));
  for i := 0 to aSize -1 do
  begin
    v := GetRandomValue(0, 32000);
    PSmallInt(Wave.Data)[i] := v;
  end;
end;

procedure TWavePlaying.Play;
begin
  inherited Play;
  Sound := LoadSoundFromWave(Wave);
  PlaySound(Sound);
  while IsSoundPlaying(Sound) do
  begin

  end;
  //UnloadSound(Sound);
end;

procedure TWavePlaying.Stop;
begin
  inherited Stop;
end;

procedure TWavePlaying.Update;
begin

end;

{ TRayUpdateList }

procedure TRayUpdateList.Update;
var
  Item: TRayUpdate;
begin
  for Item in Self do
  begin
    Item.Update;
  end;
end;

{ TMusicPlaying }

const
  MAX_WAVE_LENGTH_SECONDS =  10;     // Max length for wave: 10 seconds
  WAVE_SAMPLE_RATE   =   44100;     // Default sample rate

procedure TMusicPlaying.Play;
begin
  inherited;
  PlayMusicStream(Music);
end;

procedure TMusicPlaying.Stop;
begin
  inherited;
  StopMusicStream(Music);
end;

procedure TMusicPlaying.Update;
begin
  UpdateMusicStream(Music);
end;

{ TRayPlaying }

procedure TRayPlaying.Play;
begin

end;

procedure TRayPlaying.Stop;
begin

end;

{ TRayLibSound }

procedure TRayLibSound.Init;
begin
  if not FAudioDeviceInitialized then
    InitAudioDevice();
  FAudioDeviceInitialized := True;
end;

constructor TRayLibSound.Create;
begin
  inherited Create;
  Playing:=TObjectList.Create;
end;

destructor TRayLibSound.Destroy;
begin
  FreeAndNil(Playing);
  if FAudioDeviceInitialized then
    CloseAudioDevice();
  inherited Destroy;
end;

procedure TRayLibSound.PlayMusicFile(FileName: string);
var
  MusicPlaying: TMusicPlaying;
begin
  Init;
  MusicPlaying := TMusicPlaying.Create;
  MusicPlaying.Music := LoadMusicStream(PUTF8Char(FileName));
  Playing.Add(MusicPlaying);
  RayUpdates.Add(MusicPlaying);
  MusicPlaying.Play;
end;

procedure TRayLibSound.PlaySound(Freq, Period: integer);
var
  WavePlaying: TWavePlaying;
begin
  Init;
  WavePlaying := TWavePlaying.Create;
  WavePlaying.Generate(@WaveformSin, Freq, Period);
  Playing.Add(WavePlaying);
  //RayUpdates.Add(MusicPlaying);
  WavePlaying.Play;
end;

initialization
  RayUpdates := TRayUpdateList.Create;
  RaySound := TRayLibSound.Create;
finalization
  FreeAndNil(RaySound);
end.

