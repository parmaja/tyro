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
  cDefaultSampleRate     = 44100;   // Default sample rate
  cFontSize   = 16;      // Font Size
  cFirstChar   = 32;      // Font first character (32 - space)
  cNumChar   = 95;      // ASCII 32..126 is 95 glyphs

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
    procedure Add;
    procedure Release;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: utf8string; FontSize: Integer = 0);
    procedure LoadFromString(const DataString: rawbytestring; FontSize: Integer);
    procedure LoadDefault;
    {** Generate a mono fixed font texture image (PNG) in memory from TTF data,
      then load the font from that PNG. Returns the PNG bytes. }
    function GenerateFontTexturePNGFromTTF(const FontData: Pointer; DataSize: Integer;
      FontSize: Integer): rawbytestring;
    {** Load a font from TTF/OTF data in memory (uses raylib LoadFontFromMemory). }
    procedure LoadFromTTFMemory(const FontData: Pointer; DataSize: Integer; FontSize: Integer);
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

procedure TRayFont.Add;
begin
end;

procedure TRayFont.Release;
begin
end;

constructor TRayFont.Create;
begin
  inherited Create;
  Scale := 1;
end;

destructor TRayFont.Destroy;
begin
  RayLib.UnloadFont(Data);
  inherited Destroy;
end;

procedure TRayFont.LoadDefault;
begin
  RayLib.UnloadFont(Data);
  Data := GetFontDefault();
  Loaded;
end;

procedure TRayFont.LoadFromFile(FileName: utf8string; FontSize: Integer);
begin
  if SysUtils.FileExists(FileName) then
  begin
    RayLib.UnloadFont(Data);
    Data := Default(TFont);
    if FontSize = 0 then
      Data := RayLib.LoadFont(PUTF8Char(FileName))
    else
      Data := RayLib.LoadFontEx(PUTF8Char(FileName), FontSize, nil, 255);
    //GenTextureMipmaps(Data.texture);
    SetTextureFilter(Data.texture, TEXTURE_FILTER_TRILINEAR );
    Loaded;
  end;
end;

procedure TRayFont.LoadFromString(const DataString: rawbytestring; fontSize: Integer);
var
  img: TImage;
begin
  RayLib.UnloadFont(Data);
  img := LoadImageFromMemory('.png', PByte(DataString), Length(DataString));
  Data := LoadFontFromImage(img, clMagenta, cFirstChar);
  SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
  UnloadImage(img);
  Loaded;
end;

procedure TRayFont.LoadFromTTFMemory(const FontData: Pointer; DataSize: Integer; FontSize: Integer);
begin
  RayLib.UnloadFont(Data);
  Data := Default(TFont);
  Data := LoadFontFromMemory('ttf', FontData, DataSize, FontSize, nil, 0);
  SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
  Loaded;
end;

function TRayFont.GenerateFontTexturePNGFromTTF(const FontData: Pointer; DataSize: Integer;
  FontSize: Integer): rawbytestring;
var
  Glyphs: PGlyphInfo;
  GlyphsRecs: PPRectangle;
  GlyphCount: Integer;
  AtlasImage: TImage;
  PNGSize: Integer;
  PNGData: PByte;
  Codepoints: array[0..94] of Integer;
  i: Integer;
begin
  Result := '';

  { Build a default codepoint list: ASCII 32..126 (printable ASCII) }
  for i := 0 to 94 do
    Codepoints[i] := 32 + i;

  { Load glyph data for the printable ASCII range from TTF in memory }
  Glyphs := LoadFontData(FontData, DataSize, FontSize, @Codepoints[0], 95, FONT_BITMAP, @GlyphCount);
  if (Glyphs = nil) or (GlyphCount <= 0) then
  begin
    UnloadFontData(Glyphs, GlyphCount);
    Exit;
  end;

  { Allocate array of rectangles for glyph packing using raylib's allocator
    so UnloadFont can free it consistently }
  GlyphsRecs := MemAlloc(GlyphCount * SizeOf(TRectangle));

  { Generate the font atlas image from the glyph data }
  AtlasImage := GenImageFontAtlas(Glyphs, GlyphsRecs, GlyphCount, FontSize, 2, 0);

  { Export the atlas image to PNG in memory }
  PNGData := ExportImageToMemory(AtlasImage, '.png', PNGSize);
  if (PNGData <> nil) and (PNGSize > 0) then
  begin
    SetLength(Result, PNGSize);
    if PNGSize > 0 then
      Move(PNGData^, PByte(Result)^, PNGSize);
    { Free the PNG memory allocated by raylib }
    MemFree(PNGData);
  end;

  { Load the font from the generated PNG in memory.
    The atlas image is used to create the GPU texture, while the glyph
    data and rectangle array provide the per-character layout. }
  if Length(Result) > 0 then
  begin
    RayLib.UnloadFont(Data);
    Data := Default(TFont);
    Data.BaseSize := FontSize;
    Data.GlyphCount := GlyphCount;
    Data.GlyphPadding := 2;
    Data.Texture := LoadTextureFromImage(AtlasImage);
    Data.Recs := GlyphsRecs^;
    Data.Glyphs := Glyphs;
    SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
    Loaded;
  end
  else
  begin
    { Fallback cleanup when PNG export failed }
    UnloadImage(AtlasImage);
    MemFree(GlyphsRecs);
    UnloadFontData(Glyphs, GlyphCount);
    Exit;
  end;

  { Cleanup the temporary atlas image (texture is already on GPU) }
  UnloadImage(AtlasImage);
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

destructor TRayMusic.Destroy;
begin
  RayUpdates.Remove(Self);
  UnloadMusicStream(MusicStream);
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
  UpdateMusicStream(MusicStream);
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
  Music.MusicStream := LoadMusicStream(PUTF8Char(FileName));

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
  RayUpdates := TRayUpdateList.Create(False); //Do not own controls
  RayLibSound := TRayLibSound.Create;
finalization
  FreeAndNil(RayLibSound);
  FreeAndNil(RayUpdates);
end.

