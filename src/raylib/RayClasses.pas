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

  { TRayPlay }

  TRayPlay = class abstract (TRayUpdate)
  public
    procedure Play; virtual;
    function IsPlaying: Boolean; virtual;
    procedure Stop; virtual;
  end;

  TWaveformProc = function(index, samples, frequency, rate:Integer; connected: Boolean): Integer;

  { TRayMusic }

  TRayMusic = class(TRayPlay)
  public
    Music: TMusic;
    procedure Play; override;
    function IsPlaying: Boolean; override;
    procedure Stop; override;
    procedure Update; override;
  end;

  { TRayWave }

  TRayWave = class(TRayPlay)
  private
  protected
    Wave: TWave;
  public
    Sound: TSound;
    procedure Generate(Proc: TWaveformProc; Freq, Duration: Single; Rest: Single = 0; SampleRate: Integer = 44100; BitRate: Integer = 16);
    procedure Play; override;
    function IsPlaying: Boolean; override;
    procedure Stop; override;
    procedure Update; override;
    destructor Destroy; override;
  end;

  { TRayLibSound }

  TRayLibSound = class(TRayObject)
  public
    Playing: TObjectList;
    procedure Init;
    constructor Create;
    destructor Destroy; override;
    procedure PlayMusicFile(FileName: string);
    procedure PlaySound(Freq, Duration: integer);
  end;

function WaveformSin(index, samples, frequency, rate:Integer; connected: Boolean): Integer;
function WaveformPiano(index, samples, frequency, rate: Integer; Connected: Boolean): Integer;

var
  RayLibSound: TRayLibSound = nil;
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

{ TRayWave }

procedure TRayWave.Generate(Proc: TWaveformProc; Freq, Duration: Single; Rest: Single; SampleRate: Integer; BitRate: Integer);
var
  i: Integer;
  v: Integer;
begin
  Wave.SampleCount := Round((Duration + Rest) * SampleRate); //rest keep it empty;
  Wave.SampleRate := 44100; // By default 44100 Hz
  Wave.SampleSize := Sizeof(Smallint) * 8; // I use 16 bit
  Wave.Channels := 1;                  // By default 1 channel (mono)
  if Wave.Data <> nil then
    Freemem(Wave.Data);
  Wave.Data := GetMem(Wave.SampleCount * Sizeof(Smallint));
  for i := 0 to Wave.SampleCount -1 do
  begin
    v := GetRandomValue(0, MaxSmallint);
    PSmallInt(Wave.Data)[i] := v;
  end;
  Sound := LoadSoundFromWave(Wave);
end;

procedure TRayWave.Play;
begin
  inherited Play;
  PlaySound(Sound);
{  while IsSoundPlaying(Sound) do
  begin

  end;}
end;

function TRayWave.IsPlaying: Boolean;
begin
  Result := IsSoundPlaying(Sound);
end;

procedure TRayWave.Stop;
begin
  StopSound(Sound);
  UnloadSound(Sound);
end;

procedure TRayWave.Update;
begin

end;

destructor TRayWave.Destroy;
begin
  inherited;
  if Wave.Data <> nil then
  begin
    Freemem(Wave.Data);
    Wave.Data := nil;
  end;
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

{ TRayMusic }

const
  cDefaultSampleRate = 44100;     // Default sample rate

procedure TRayMusic.Play;
begin
  inherited;
  PlayMusicStream(Music);
end;

function TRayMusic.IsPlaying: Boolean;
begin
  Result := IsMusicPlaying(Music);
end;

procedure TRayMusic.Stop;
begin
  inherited;
  StopMusicStream(Music);
end;

procedure TRayMusic.Update;
begin
  UpdateMusicStream(Music);
end;

{ TRayPlay }

procedure TRayPlay.Play;
begin

end;

function TRayPlay.IsPlaying: Boolean;
begin
  Result := True;
end;

procedure TRayPlay.Stop;
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
  MusicPlaying: TRayMusic;
begin
  Init;
  MusicPlaying := TRayMusic.Create;
  MusicPlaying.Music := LoadMusicStream(PUTF8Char(FileName));
  Playing.Add(MusicPlaying);
  RayUpdates.Add(MusicPlaying);
  MusicPlaying.Play;
end;

procedure TRayLibSound.PlaySound(Freq, Duration: integer);
var
  WavePlaying: TRayWave;
begin
  Init;
  WavePlaying := TRayWave.Create;
  WavePlaying.Generate(@WaveformSin, Freq, Duration);
  Playing.Add(WavePlaying);
  //RayUpdates.Add(MusicPlaying);
  WavePlaying.Play;
end;

initialization
  RayUpdates := TRayUpdateList.Create;
  RayLibSound := TRayLibSound.Create;
finalization
  FreeAndNil(RayLibSound);
  FreeAndNil(RayUpdates);
end.

