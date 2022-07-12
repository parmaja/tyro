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
  RayLib;

const
  cDefaultSampleRate = 44100;     // Default sample rate

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
    FImage: RayLib.TImage;
  protected
  public
  end;

  TRayPlayState = (plyStop, plyPlay, plyPause);
  { TRayPlay }

  TRayPlay = class abstract (TRayUpdate)
  public
    State: TRayPlayState;
    procedure Play; virtual;
    function IsPlaying: Boolean; virtual; abstract;
    procedure Pause; virtual;
    procedure Stop; virtual;
  end;

  { TRayMusic }

  TRayMusic = class(TRayPlay)
  protected
    Music: TMusic;
  public
    procedure Play; override;
    function IsPlaying: Boolean; override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Update; override;
  end;

  { TRaySound }

  TRaySound = class(TRayPlay)
  private
  protected
    Sound: TSound;
    IsMutli: Boolean;
    procedure UpdateData(Data: Pointer; SampleCount: Cardinal); virtual;
  public
    constructor Create(AIsMutli: Boolean); overload;
    procedure Play; override;
    function IsPlaying: Boolean; override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Update; override;
    destructor Destroy; override;
  end;

  { TRayAudio }

  TRayAudio = class(TRayPlay)
  private
  protected
    AudioStream: TAudioStream;
  public
    constructor Create(SampleRate: Cardinal; BitRate: Cardinal; Channels: Cardinal);
    procedure UpdateData(Data: Pointer; SampleCount: Cardinal); virtual;
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
    procedure PlayMusicFile(FileName: utf8string);
  end;

  { TRayFont }

  TRayFont = class(TRayObject)
  public
    Data: TFont;
  public
    RefCount: Boolean;
    Width: Integer;
    Height: Integer;
    procedure Add;
    procedure Release;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: utf8string);
    procedure LoadFromString(const DataString: rawbytestring; fontSize: Integer);
    procedure LoadDefault;
  end;

var
  RayLibSound: TRayLibSound = nil;
  RayUpdates: TRayUpdateList = nil; //move it to Tyro classes

function MouseX: Integer;
function MouseY: Integer;

implementation

var
  FAudioDeviceInitialized: Integer = 0;

{ TRayFont }

procedure TRayFont.Add;
begin
end;

procedure TRayFont.Release;
begin
end;

constructor TRayFont.Create;
begin

end;

destructor TRayFont.Destroy;
begin
  UnloadFont(Data);
  inherited Destroy;
end;

procedure TRayFont.LoadDefault;
begin
  Data := GetFontDefault();
  Height := Data.BaseSize;
end;

procedure TRayFont.LoadFromFile(FileName: utf8string);
begin
  Data := LoadFont(PUTF8Char(FileName));
end;

procedure TRayFont.LoadFromString(const DataString: rawbytestring; fontSize: Integer);
var
  img: TImage;
const
  DEFAULT_FIRST_CHAR = 32;
begin
  img := LoadImageFromMemory('.png', PByte(DataString), Length(DataString));
  Data := LoadFontFromImage(img, clMagenta, 32);
  UnloadImage(img);
end;

{ TRayAudio }

constructor TRayAudio.Create(SampleRate: Cardinal; BitRate: Cardinal; Channels: Cardinal);
begin
  inherited Create;
  AudioStream := LoadAudioStream(SampleRate, BitRate, Channels);
  Play;
end;

procedure TRayAudio.UpdateData(Data: Pointer; SampleCount: Cardinal);
begin
  UpdateAudioStream(AudioStream, Data, SampleCount);
  PlayAudioStream(AudioStream);
end;

procedure TRayAudio.Play;
begin
  inherited;
  PlayAudioStream(AudioStream);
end;

function TRayAudio.IsPlaying: Boolean;
begin
  Result := IsAudioStreamPlaying(AudioStream);
end;

procedure TRayAudio.Stop;
begin
  inherited Stop;
  StopAudioStream(AudioStream);
end;

procedure TRayAudio.Pause;
begin
  inherited Pause;
  PauseAudioStream(AudioStream);
end;

procedure TRayAudio.Update;
begin
  IsAudioStreamProcessed(AudioStream);
end;

destructor TRayAudio.Destroy;
begin
  inherited Destroy;
  UnloadAudioStream(AudioStream);
end;

procedure TRaySound.UpdateData(Data: Pointer; SampleCount: Cardinal);
begin
  UpdateSound(Sound, Data, SampleCount);
end;

constructor TRaySound.Create(AIsMutli: Boolean);
begin
  inherited Create;
  IsMutli := AIsMutli;
end;

procedure TRaySound.Play;
begin
  inherited;
  if IsMutli then
    PlaySoundMulti(Sound)
  else
    PlaySound(Sound);
end;

function TRaySound.IsPlaying: Boolean;
begin
  Result := IsSoundPlaying(Sound);
end;

procedure TRaySound.Stop;
begin
  if State > plyStop then
  begin
    if IsMutli then
      PlaySoundMulti(Sound)
    else
      StopSound(Sound);
  end;
  inherited;
end;

procedure TRaySound.Pause;
begin
  inherited;
end;

procedure TRaySound.Update;
begin
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

procedure TRayMusic.Play;
begin
  inherited;
  PlayMusicStream(Music);
end;

function TRayMusic.IsPlaying: Boolean;
begin
  Result := IsMusicStreamPlaying(Music);
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
    if not IsAudioDeviceReady then
      InitAudioDevice;

  //InterlockedIncrement(FAudioDeviceInitialized);
end;

procedure TRayLibSound.Close;
begin
  if FAudioDeviceInitialized > 0 then
  begin
    {if FAudioDeviceInitialized = 0 then
      CloseAudioDevice;} //leave it open
    {$ifdef FPC}
    InterlockedDecrement(FAudioDeviceInitialized);
    {$else}
    AtomicIncrement(FAudioDeviceInitialized, 1);
    {$endif}
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

procedure TRayLibSound.PlayMusicFile(FileName: utf8string);
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

function MouseX: Integer;
begin
  Result := RayLib.GetMouseX;
end;

function MouseY: Integer;
begin
  Result := RayLib.GetMouseY;
end;

initialization
  RayUpdates := TRayUpdateList.Create;
  RayLibSound := TRayLibSound.Create;
finalization
  FreeAndNil(RayLibSound);
  FreeAndNil(RayUpdates);
end.

