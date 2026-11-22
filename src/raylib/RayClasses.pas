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
  mnLogs, mnClasses, mnUtils, mnBDF,
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
    procedure Inflate(D: Integer); overload;
  end;

  { TPointHelper }

  TPointHelper = record helper for TPoint
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

  { TRayTexture }

  TRayTexture = class(TRayObject)
  private
    FLoaded: Boolean;
    FTexture: RayLib.TTexture2D;
  protected
  public
    procedure Unload;
    function LoadFromFile(const AFileName: string): Boolean;
    function IsValid: Boolean;
  end;

  { TRayImage }

  TRayImage = class(TRayObject)
  private
    FImage: RayLib.TImage;
  protected
  public
    procedure Unload;
    function LoadFromFile(const AFileName: string): Boolean;
    function IsValid: Boolean;
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
    //The raylib stream behind this audio. A caller that has to hand it to
    //something else (the spectrum processor reads it) cannot see the field,
    //which is protected.
    property Stream: TAudioStream read AudioStream;
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

  //The glyph set a codepage name asks raylib to bake: array of Unicode
  //values. See NormalizeCodePage/BuildCodePoints in the implementation.
  TFontCodePoints = array of Integer;

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
    // Identity for the resource font list: a repeated load of the same
    // source (file, size, codepage) reuses one baked font. Ready turns on
    // only once the atlas is really loaded, and FontId is the number a
    // script keeps as its handle.
    FontId: Integer;
    SourceName: string;
    SourceSize: Integer;
    SourceCodePage: string;
    Ready: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string; FontSize: Integer; const ACodePage: string = '');
    procedure LoadFromString(const DataString: rawbytestring; FontSize: Integer);
    procedure LoadFromBDF(FileName: utf8string; FontSize: Integer = 0);
    procedure LoadFromMemory(FileType: utf8string; const FontData: Pointer; DataSize: Integer; FontSize: Integer; Codepoints: PInteger = nil; CodepointsCount: Integer = 0);
    procedure LoadDefault;
    procedure Unload;
  end;

var
  RayLibSound: TRayLibSound = nil;
  RayUpdates: TRayUpdateList = nil;

function MouseX: Integer;
function MouseY: Integer;

function OffsetRectangle(const ARectangle: TRectangle; OffsetX, OffsetY: Single): TRectangle; overload;

// 'cp864'/'ARABIC'/... all fold to one lowercase name so the font book can
// cache by codepage; '' stays '' (raylib's own default set).
function NormalizeCodePage(const ACodePage: string): string;
// The Unicode values to bake for a codepage; answers False for '' (use
// raylib's default). Unknown names raise.
function BuildCodePoints(const ACodePage: string; out Points: TFontCodePoints): Boolean;

implementation

uses
  ucp864; //the CP864 table behind the 'arabic' codepage

var
  FAudioDeviceInitialized: Integer = 0;

{ TRayFont }

{ A file that is not TrueType data does not fail in raylib: LoadFontEx hands
  back its built-in tiny ASCII default font, valid and unrelated to the file.
  So a corrupt or lying .ttf would silently shrink a game's text. The sfnt
  version tag is checked here for the extensions that promise one; everything
  else (a bare .fnt, an extension-less name) stays raylib's business. }
function TrueTypeHeaderMatches(const FileName: string): Boolean;
var
  Stream: TFileStream;
  Tag: array[0..3] of Byte;
  Ext: string;

  function IsText(const S: string): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to 4 do
      if Tag[I - 1] <> Ord(S[I]) then
        Exit(False);
    Result := True;
  end;

begin
  Result := True;
  Ext := LowerCase(ExtractFileExt(FileName));
  if (Ext <> '.ttf') and (Ext <> '.otf') and (Ext <> '.ttc') then
    Exit;
  Result := False;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      if Stream.Size < 4 then
        Exit;
      Stream.ReadBuffer(Tag, 4);
    finally
      Stream.Free;
    end;
    Result := ((Tag[0] = 0) and (Tag[1] = 1) and (Tag[2] = 0) and (Tag[3] = 0)) //version 1.0
      or IsText('true') or IsText('typ1')
      or IsText('OTTO') //OpenType CFF saved as .ttf is legal
      or ((Ext = '.ttc') and IsText('ttcf'));
  except
    Result := False;
  end;
end;

{ Every spelling of a codepage lands on one cache key: 'cp864' is just how
  DOS names the Arabic set, and the book must not bake the same atlas twice
  under two aliases. Case is folded too. '' stays '' (raylib's default set). }
function NormalizeCodePage(const ACodePage: string): string;
begin
  Result := LowerCase(ACodePage);
  if Result = 'cp864' then
    Result := 'arabic';
  if Result = 'cp1252' then
    Result := 'ansi';
end;

{ The codepoint set a named codepage promises, as raylib wants it: an array
  of Unicode values to bake. '' means "raylib's own default" (the printable
  ASCII, 95 glyphs — small but Latin-only). 'ansi' adds Latin-1,
  'arabic' bakes exactly the characters CP864 can carry — which is
  ASCII plus the shaped forms minibidi emits — so Arabic games fit in a
  small atlas. Unknown names raise: a typo must not silently mean ASCII. }
function BuildCodePoints(const ACodePage: string; out Points: TFontCodePoints): Boolean;

  procedure AddPoint(var List: TFontCodePoints; AValue: Integer);
  var
    I: Integer;
  begin
    for I := 0 to High(List) do
      if List[I] = AValue then
        Exit;
    SetLength(List, Length(List) + 1);
    List[High(List)] := AValue;
  end;

var
  B, C: Integer;
  W: WideChar;
  Name: string;
begin
  Name := NormalizeCodePage(ACodePage);
  Result := Name <> '';
  SetLength(Points, 0);
  if not Result then
    Exit;
  for B := $20 to $7E do //printable ASCII rides every codepage
    AddPoint(Points, B);
  if (Name = 'ansi') or (Name = 'cp1252') then
  begin
    for B := $A0 to $FF do
      AddPoint(Points, B);
  end
  else if Name = 'arabic' then
  begin
    //Every glyph CP864 spells, at its true Unicode value: a shaped string
    //from minibidi stays UTF-8 and draws from this same atlas.
    for B := $20 to $FF do
    begin
      cp864_mbtowc(AnsiChar(B), W);
      C := Ord(W);
      if (C <> 0) and (C <> $FFFD) then
        AddPoint(Points, C);
    end;
  end
  else if Name = 'everything' then
  begin
    for B := $20 to $D7FF do
      AddPoint(Points, B);
    for B := $E000 to $FFFF do
      AddPoint(Points, B);
  end
  else
    raise Exception.Create('Unknown font codepage: ' + ACodePage);
end;

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
  Ready := True;
  Loaded;
end;

procedure TRayFont.Unload;
begin
  if (Data.Texture.ID <> 0) and not RefCount then
    RayLib.UnloadFont(Data);
  Data := Default(TFont);
  RefCount := False;
  Ready := False;
  Width := 0;
  Height := 0;
end;

procedure TRayFont.LoadFromFile(FileName: string; FontSize: Integer; const ACodePage: string);
var
  NewData: TFont;
  Points: TFontCodePoints;
  HasPoints: Boolean;
begin
  if SameText(ExtractFileExt(FileName), '.bdf') then //while RayLib doesn't load it (idk why)
    LoadFromBDF(FileName, FontSize)
  else if SysUtils.FileExists(FileName) then
  begin
    //Load first, commit after: a font file that exists but fails to parse
    //must not leave the script without the font it was drawing with. The
    //size=0 branch spells the UTF-8 cast the same way the next line does,
    //because a bare PUTF8Char(string) is only a pointer cast in Delphi.
    if not TrueTypeHeaderMatches(FileName) then
      raise Exception.Create('Fail to load font: ' + FileName);
    HasPoints := BuildCodePoints(ACodePage, Points);
    if HasPoints and (Length(Points) = 0) then
      raise Exception.Create('Font codepage carries no glyphs: ' + ACodePage);
    if FontSize = 0 then
      NewData := RayLib.LoadFont(PUTF8Char(UTF8String(FileName)))
    else if HasPoints then
      NewData := RayLib.LoadFontEx(PUTF8Char(UTF8String(FileName)), FontSize, @Points[0], Length(Points))
    else
      NewData := RayLib.LoadFontEx(PUTF8Char(UTF8String(FileName)), FontSize, nil, 0);
    if not IsFontValid(NewData) then
      raise Exception.Create('Fail to load font: ' + FileName);
    //A huge request ('everything' at a big size) can outbake the atlas:
    //raylib hands back an empty shell, and dividing by its zero BaseSize
    //later would be an EInvalidOp from the queue. Fail as a font instead.
    if (NewData.GlyphCount <= 0) or (NewData.BaseSize <= 0) then
      raise Exception.Create('Font is too big for one atlas at this size: ' + ACodePage);
    Unload;
    Data := NewData;
    RefCount := False;
    SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
    SourceName := FileName;
    SourceSize := FontSize;
    SourceCodePage := NormalizeCodePage(ACodePage);
    Ready := True;
    Loaded;
  end
  else
    raise Exception.Create('Font file not exists ' + FileName);
end;

procedure TRayFont.LoadFromString(const DataString: rawbytestring; FontSize: Integer);
var
  img: TImage;
  NewData: TFont;
begin
  img := LoadImageFromMemory('.png', PByte(DataString), Length(DataString));
  try
    if img.Data = nil then
      raise Exception.Create('Unable to decode font image');
    ImageAlphaPremultiply(img);
    NewData := LoadFontFromImage(img, clMagenta, cFirstChar);
    if not IsFontValid(NewData) then
    begin
      UnloadFont(NewData); //a valid-but-rejected atlas still owns its texture
      raise Exception.Create('Unable to load font from image');
    end;
  finally
    if img.Data <> nil then
      UnloadImage(img);
  end;
  //Only now the old font leaves: a bad image cannot blank a running scene.
  Unload;
  Data := NewData;
  RefCount := False;
  SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
  Ready := True;
  Loaded;
  //Height := Height * 2;
  //Width := Width * 2;
end;

procedure TRayFont.LoadFromMemory(FileType: utf8string; const FontData: Pointer; DataSize: Integer; FontSize: Integer; Codepoints: PInteger; CodepointsCount: Integer);
begin
  Unload;
  Data := LoadFontFromMemory(PUTF8Char(FileType), FontData, DataSize, FontSize, Codepoints, CodepointsCount);
  RefCount := False;
  Ready := True;
  SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
  Loaded;
end;

procedure TRayFont.LoadFromBDF(FileName: utf8string; FontSize: Integer);
var
  BDF: TBDF;
  img: TImage;
  Stream: TMemoryStream;
  NewData: TFont;
begin
  //Codepoints := nil;
  if not SysUtils.FileExists(FileName) then
    raise Exception.Create('Font file not exists ' + FileName);
  BDF := TBDF.Create;
  try
    BDF.LoadFromFile(FileName);
    Stream := BDF.EncodeToPNG;
    try
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
        NewData := LoadFontFromImage(img, clMagenta, cFirstChar);
        if not IsFontValid(NewData) then
        begin
          UnloadFont(NewData);
          raise Exception.Create('Unable to load BDF font image');
        end;
      finally
        if img.Data <> nil then
          UnloadImage(img);
      end;
    finally
      Stream.Free;
    end;
  finally
    BDF.Free;
  end;
  //The parsed font is in hand: only now the old one leaves, so a broken BDF
  //cannot blank a scene that is still drawing.
  Unload;
  Data := NewData;
  RefCount := False;
  SetTextureFilter(Data.texture, TEXTURE_FILTER_POINT);
  Ready := True;
  Loaded;
  Log.WriteLn('Font loaded: ' + FileName);
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

procedure TRectHelper.Inflate(D: Integer);
begin
  InflateRect(Self, D, D);
end;

{ TPointHelper }

function TPointHelper.ToString: string;
begin
  Result := Format('(%d, %d)', [Self.X, Self.Y]);
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

{ TRayTexture }

procedure TRayTexture.Unload;
begin
  RayLib.UnloadTexture(FTexture);
  FTexture := Default(TTexture2D);
  FLoaded := False;
end;

function TRayTexture.LoadFromFile(const AFileName: string): Boolean;
begin
  FTexture := RayLib.LoadTexture(PUTF8Char(UTF8String(AFileName)));
  FLoaded := IsValid;
  Result := FLoaded;
end;

function TRayTexture.IsValid: Boolean;
begin
  Result := RayLib.IsTextureValid(FTexture);
end;

{ TRayImage }

procedure TRayImage.Unload;
begin
  UnloadImage(FImage);
  FImage := Default(RayLib.TImage);
end;

function TRayImage.LoadFromFile(const AFileName: string): Boolean;
begin
  FImage := RayLib.LoadImage(PUTF8Char(UTF8String(AFileName)));
  Result := IsValid;
end;

function TRayImage.IsValid: Boolean;
begin
  Result := IsImageValid(FImage);
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

function OffsetRectangle(const ARectangle: TRectangle; OffsetX, OffsetY: Single): TRectangle;
begin
  Result := ARectangle;
  Result.X :=  Result.X + OffsetX;
  Result.Y := Result.Y + OffsetY;
end;


initialization
  RayUpdates := TRayUpdateList.Create(False); //Do not own controls
  RayLibSound := TRayLibSound.Create;
finalization
  FreeAndNil(RayLibSound);
  FreeAndNil(RayUpdates);
end.

