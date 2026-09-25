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
  Classes, SysUtils, Contnrs, Types, Math,
  mnLogs, mnClasses, mnUtils,
  mnBDF,
  RayLib;

const
  cDefaultSampleRate     = 44100;   // Default sample rate
  cFontSize   = 16;      // Font Size
  cFirstChar   = 32;      // Font first character (32 - space)
  cNumChar   = 95;      // ASCII 32..126 is 95 glyphs

type

  { TRectHelper }

  TRectHelper = record helper for TRect
    function ToString: string;
  end;

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
    MusicStream: TMusic;
  public
    destructor Destroy; override;

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
    procedure Shutdown;
    constructor Create;
    destructor Destroy; override;
    procedure PlayMusicFile(FileName: utf8string);
  end;

  { TRayFont }

  TRayFont = class(TRayObject)
  public
    Data: TFont;
    procedure Loaded;
  public
    RefCount: Boolean;
    Width: Integer;
    Height: Integer;
    Scale: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: utf8string; FontSize: Integer = 0);
    procedure LoadFromString(const DataString: rawbytestring; FontSize: Integer);
    procedure LoadFromBDF(FileName: utf8string; FontSize: Integer = 0);
    procedure LoadFromMemory(FileType: string; const FontData: Pointer; DataSize: Integer; FontSize: Integer; Codepoints: PInteger = nil; CodepointsCount: Integer = 0);
    procedure LoadDefault;
    procedure Unload;
  end;

var
  RayLibSound: TRayLibSound = nil;
  RayUpdates: TRayUpdateList = nil;

function MouseX: Integer;
function MouseY: Integer;

implementation

var
  FAudioDeviceInitialized: Integer = 0;

{ TRayFont }

procedure TRayFont.Loaded;
var
  charSize: TVector2;
begin
  charSize := RayLib.MeasureTextEx(Data, 'A', Data.BaseSize, 1);
  Height := Floor(charSize.y);
  Width := Floor(charSize.x);
  //Width := Data.Glyphs[0].advanceX;;
end;

constructor TRayFont.Create;
begin
  inherited Create;
  Scale := 1;
end;

destructor TRayFont.Destroy;
begin
  //The default font belongs to raylib and is released by CloseWindow.
  if not RefCount then
    Unload
  else
  begin
    Data := Default(TFont);
    Width := 0;
    Height := 0;
  end;
  inherited Destroy;
end;

procedure TRayFont.LoadDefault;
begin
  Unload;
  Data := GetFontDefault();
  RefCount := True;
  Loaded;
end;

procedure TRayFont.Unload;
begin
  if (Data.Texture.ID <> 0) and not RefCount then
    RayLib.UnloadFont(Data);
  Data := Default(TFont);
  RefCount := False;
  Width := 0;
  Height := 0;
end;

procedure TRayFont.LoadFromFile(FileName: utf8string; FontSize: Integer);
begin
  if SameText(ExtractFileExt(FileName), '.bdf') then
  begin
    LoadFromBDF(FileName, FontSize);
    exit;
  end;
  if SysUtils.FileExists(FileName) then
  begin
    Unload;
    if FontSize = 0 then
      Data := RayLib.LoadFont(PUTF8Char(FileName))
    else
      Data := RayLib.LoadFontEx(PUTF8Char(FileName), FontSize, nil, 255);
    RefCount := False;
    if Data.Texture.ID<=2 then
      Log.WriteLn('Fail to load font: ' + FileName);
    //GenTextureMipmaps(Data.texture);
    SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
    Loaded;
  end
  else
    raise Exception.Create('Font file not exists ' + FileName);
end;

procedure TRayFont.LoadFromString(const DataString: rawbytestring; FontSize: Integer);
var
  img: TImage;
begin
  Unload;
  img := LoadImageFromMemory('.png', PByte(DataString), Length(DataString));
  try
    if img.Data = nil then
      raise Exception.Create('Unable to decode font image');
    ImageAlphaPremultiply(img);
    Data := LoadFontFromImage(img, clMagenta, cFirstChar);
    RefCount := False;
    if not IsFontValid(Data) then
      raise Exception.Create('Unable to load font from image');
    SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
  finally
    if img.Data <> nil then
      UnloadImage(img);
  end;
  Loaded;
  //Height := Height * 2;
  //Width := Width * 2;
end;

procedure TRayFont.LoadFromMemory(FileType: string; const FontData: Pointer; DataSize: Integer; FontSize: Integer; Codepoints: PInteger; CodepointsCount: Integer);
begin
  Unload;
  Data := LoadFontFromMemory(PUTF8Char(FileType), FontData, DataSize, FontSize, Codepoints, CodepointsCount);
  RefCount := False;
  SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
  Loaded;
end;

procedure TRayFont.LoadFromBDF(FileName: utf8string; FontSize: Integer);
var
  BDF: TBDF;
  img: TImage;
  Stream: TMemoryStream;
  i: Integer;
begin
  Unload;
  //Codepoints := nil;
  if SysUtils.FileExists(FileName) then
  begin
    BDF := TBDF.Create;
    try
      BDF.LoadFromFile(FileName);
      try
        Stream := BDF.EncodeToPNG;
        Stream.Position:= 0;
        //Stream.SaveToFile('c:\temp\1.png');
        //Stream.Position:= 0;

        if FontSize = 0 then
          FontSize := BDF.Height;

      img := LoadImageFromMemory('.png', Stream.Memory, Stream.Size);
      try
        if img.Data = nil then
          raise Exception.Create('Unable to decode BDF font image');
        //ImageAlphaPremultiply(img);
        Data := LoadFontFromImage(img, clMagenta, cFirstChar);
        RefCount := False;
        if not IsFontValid(Data) then
          raise Exception.Create('Unable to load BDF font image');
        SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
      finally
        if img.Data <> nil then
          UnloadImage(img);
      end;
        Loaded;
        Log.WriteLn('Font loaded: ' + FileName);
      finally
        Stream.Free;
      end;
    finally
      BDF.Free;
    end;
  end
  else
    raise Exception.Create('Font file not exists ' + FileName);
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
  if AudioStream.Buffer <> nil then
  begin
    StopAudioStream(AudioStream);
    UnloadAudioStream(AudioStream);
    AudioStream := Default(TAudioStream);
  end;
  inherited Destroy;
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
    //PlaySoundMulti(Sound)
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
      //PlaySoundMulti(Sound)
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
  if Sound.FrameCount > 0 then
  begin
    StopSound(Sound);
    UnloadSound(Sound);
    Sound := Default(TSound);
  end;
  inherited;
end;

{ TRectHelper }

function TRectHelper.ToString: string;
begin
  Result := Format('(%d, %d, %d, %d)', [Self.Left, Self.Top, Self.Right, Self.Bottom]);
end;

{ TRayUpdateList }

procedure TRayUpdateList.Update;
var
  Snapshot: array of TRayUpdate;
  Item: TRayUpdate;
  I: Integer;
  Music: TRayMusic;
begin
  SetLength(Snapshot, Count);
  for I := 0 to Count - 1 do
    Snapshot[I] := Items[I];
  for I := 0 to High(Snapshot) do
  begin
    Item := Snapshot[I];
    if IndexOf(Item) >= 0 then
      Item.Update;
    // Removed entries are skipped; newly-added entries run next cycle.
  end;
  // Reclaim one-shot file music only after iteration: its destructor removes
  // itself from this update list.
  if (RayLibSound <> nil) and (RayLibSound.Playing <> nil) then
    for I := RayLibSound.Playing.Count - 1 downto 0 do
      if TObject(RayLibSound.Playing[I]) is TRayMusic then
      begin
        Music := TRayMusic(RayLibSound.Playing[I]);
        if Music.State = plyStop then
          RayLibSound.Playing.Delete(I);
      end;
end;

{ TRayMusic }

destructor TRayMusic.Destroy;
begin
  RayUpdates.Remove(Self);
  if MusicStream.Stream.Buffer <> nil then
  begin
    StopMusicStream(MusicStream);
    UnloadMusicStream(MusicStream);
    MusicStream := Default(TMusic);
  end;
  inherited;
end;

procedure TRayMusic.Play;
begin
  inherited;
  PlayMusicStream(MusicStream);
end;

function TRayMusic.IsPlaying: Boolean;
begin
  Result := IsMusicStreamPlaying(MusicStream);
end;

procedure TRayMusic.Stop;
begin
  inherited;
  StopMusicStream(MusicStream);
end;

procedure TRayMusic.Pause;
begin
  inherited Pause;
  PauseMusicStream(MusicStream);
end;

procedure TRayMusic.Update;
begin
  if State <> plyPlay then
    Exit;
  UpdateMusicStream(MusicStream);
  if not IsPlaying then
    State := plyStop;
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
  if not IsAudioDeviceReady then
    InitAudioDevice;
  if IsAudioDeviceReady then
    FAudioDeviceInitialized := 1
  else
    FAudioDeviceInitialized := 0;
end;

procedure TRayLibSound.Close;
begin
  //The audio device is process-wide. A finished sound must not close it while
  //radio, music, or another generated sound still owns backend resources.
end;

constructor TRayLibSound.Create;
begin
  inherited Create;
  Playing := TObjectList.Create;
end;

procedure TRayLibSound.Shutdown;
begin
  if Playing <> nil then
    Playing.Clear;
  if IsAudioDeviceReady then
    CloseAudioDevice;
  FAudioDeviceInitialized := 0;
end;

destructor TRayLibSound.Destroy;
begin
  //TRayMusic destructors unregister themselves from RayUpdates, so callers
  //must keep that list alive until this owned list has been released.
  Shutdown;
  FreeAndNil(Playing);
  inherited Destroy;
end;

procedure TRayLibSound.PlayMusicFile(FileName: utf8string);
var
  Music: TRayMusic;
begin
  Open;
  Music := TRayMusic.Create;
  try
    Music.MusicStream := LoadMusicStream(PUTF8Char(FileName));
    if Music.MusicStream.Stream.Buffer = nil then
      raise Exception.Create('Unable to load music: ' + FileName);
    // music.play() is one-shot; completed streams are reclaimed below.
    Music.MusicStream.Looping := False;
    Playing.Add(Music);
    RayUpdates.Add(Music);
    Music.Play;
    Music := nil;
  finally
    Music.Free;
  end;
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
  RayUpdates := TRayUpdateList.Create(False); //Do not own controls
  RayLibSound := TRayLibSound.Create;
finalization
  FreeAndNil(RayLibSound);
  FreeAndNil(RayUpdates);
end.

