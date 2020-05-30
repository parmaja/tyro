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

  TRayPlayState = (plyStop, plyPlay, plyPause);
  { TRayPlay }

  TRayPlay = class abstract (TRayUpdate)
  public
    State: TRayPlayState;
    procedure Play; virtual;
    function IsPlaying: Boolean; virtual;
    procedure Pause; virtual;
    procedure Stop; virtual;
  end;

  { TRayMusic }

  TRayMusic = class(TRayPlay)
  public
    Music: TMusic;
    procedure Play; override;
    function IsPlaying: Boolean; override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Update; override;
  end;

  { TRayWave }

  TRaySound = class(TRayPlay)
  private
  protected
  public
    Sound: TSound;
    procedure Play; override;
    function IsPlaying: Boolean; override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Update; override;
    destructor Destroy; override;
  end;

  { TRayLibSound }

  TRayLibSound = class(TRayObject)
  public
    Playing: TObjectList;
    procedure Open;
    procedure Close;
    constructor Create;
    destructor Destroy; override;
    procedure PlayMusicFile(FileName: string);
  end;

var
  RayLibSound: TRayLibSound = nil;
  RayUpdates: TRayUpdateList = nil; //move it to Tyro classes

implementation

var
  FAudioDeviceInitialized: Integer = 0;

procedure TRaySound.Play;
begin
  inherited;
  PlaySound(Sound);
{  while IsSoundPlaying(Sound) do
  begin

  end;}
end;

function TRaySound.IsPlaying: Boolean;
begin
  Result := IsSoundPlaying(Sound);
end;

procedure TRaySound.Stop;
begin
  if State > plyStop then
    StopSound(Sound);
  inherited;
end;

procedure TRaySound.Pause;
begin
  inherited;
end;

procedure TRaySound.Update;
begin
  inherited;
end;

destructor TRaySound.Destroy;
begin
  inherited;
  UnloadSound(Sound);
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

procedure TRayMusic.Pause;
begin
  inherited Pause;
  PauseMusicStream(Music);
end;

procedure TRayMusic.Update;
begin
  UpdateMusicStream(Music);
end;

{ TRayPlay }

procedure TRayPlay.Play;
begin
  State := plyPlay;
end;

function TRayPlay.IsPlaying: Boolean;
begin
  Result := False;
end;

procedure TRayPlay.Pause;
begin
  State := plyPause;
end;

procedure TRayPlay.Stop;
begin
  State := plyStop;
end;

{ TRayLibSound }

procedure TRayLibSound.Open;
begin
  if FAudioDeviceInitialized = 0 then
    InitAudioDevice();
  InterlockedIncrement(FAudioDeviceInitialized);
end;

procedure TRayLibSound.Close;
begin
  if FAudioDeviceInitialized > 0 then
  begin
    InterlockedDecrement(FAudioDeviceInitialized);
    if FAudioDeviceInitialized = 0 then
      CloseAudioDevice;
  end;
end;

constructor TRayLibSound.Create;
begin
  inherited Create;
  Playing := TObjectList.Create;
end;

destructor TRayLibSound.Destroy;
begin
  FreeAndNil(Playing);
  if FAudioDeviceInitialized > 0 then
    CloseAudioDevice();
  inherited Destroy;
end;

procedure TRayLibSound.PlayMusicFile(FileName: string);
var
  Music: TRayMusic;
begin
  Open;
  Music := TRayMusic.Create;
  Music.Music := LoadMusicStream(PUTF8Char(FileName));
  Playing.Add(Music);
  RayUpdates.Add(Music);
  Music.Play;
end;

initialization
  RayUpdates := TRayUpdateList.Create;
  RayLibSound := TRayLibSound.Create;
finalization
  FreeAndNil(RayLibSound);
  FreeAndNil(RayUpdates);
end.

