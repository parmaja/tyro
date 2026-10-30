unit SpriteAtlas;
{**
 *  .sprite file support for Tyro
 *
 *  A .sprite file is a ZIP archive holding a texture atlas: one PNG image
 *  plus the JSON descriptor emitted by TexturePacker / Phaser / PixiJS (both
 *  the "JSON Hash" and "JSON Array" layouts).  Loading such a file yields one
 *  RGBA texture per frame, exactly like the Aseprite reader, so a .sprite
 *  asset can be handed to the sprite loader the same way:
 *      sprite:load("hero.sprite")
 *
 *  Supported:
 *    - ZIP entries stored (method 0) or deflate-compressed (method 8)
 *    - frames described as an object (Phaser 3 / TexturePacker hash) or as an
 *      array (Phaser 2 / PixiJS), with the frame name taken from the key, or
 *      from a "filename" / "name" member
 *    - the nested "frame" rectangle, or flat x/y/w/h and x/y/width/height
 *    - rotated frames (packed 90 degrees clockwise in the sheet)
 *    - trimmed frames, restored against "sourceSize"/"spriteSourceSize"
 *    - optional per-frame "duration" in milliseconds
 *
 *  @license   MIT
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}

interface

uses
  Classes, SysUtils, Math, RayLib, Aseprites, mnDON;

type
  { One archived file located in the ZIP central directory. }
  TZipEntry = record
    Name: string;
    Method: Word;
    CompSize: Cardinal;
    UncompSize: Cardinal;
    DataOffset: Int64;             // absolute offset of the entry's payload
  end;
  TZipEntryArray = array of TZipEntry;

  { One frame: a rectangle inside the atlas PNG plus its placement metadata. }
  TSpriteAtlasFrame = record
    Name: string;
    X: Integer;            // rectangle inside the atlas PNG
    Y: Integer;
    W: Integer;            // packed (possibly rotated) size
    H: Integer;
    Rotated: Boolean;      // packed 90 degrees clockwise
    Trimmed: Boolean;
    SrcX: Integer;         // position of the kept pixels in the original frame
    SrcY: Integer;
    OriginalW: Integer;    // untrimmed frame size (spriteSourceSize/sourceSize)
    OriginalH: Integer;
    DurationMs: Integer;   // 0 when the descriptor has no timing
  end;

  { TSpriteAtlas }

  TSpriteAtlas = class(TObject)
  private
    FData: TBytes;
    FEntries: TZipEntryArray;
    FAtlas: TImage;
    FHasAtlas: Boolean;
    FFrames: array of TSpriteAtlasFrame;
    FImageHint: string;
    FDescriptor: string;      //the JSON descriptor exactly as it was stored
    FLastError: string;
    procedure Clear;
    function ReadZipDirectory: Boolean;
    function FindEntry(const AExt, APreferName: string; out AIndex: Integer): Boolean;
    function EntryData(AIndex: Integer; out AData: TBytes): Boolean;
    procedure ParseDescriptor(ARoot: TDON_Value);
    function BuildFrameImage(const AFrame: TSpriteAtlasFrame): TImage;
    procedure AppendFrame(const AFrame: TSpriteAtlasFrame);
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(const FileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function GetFrameCount: Integer;
    function GetFrame(AFrame: Integer): TSpriteAtlasFrame;
    function GetFrameDuration(AFrame: Integer): Integer;
    // Decodes one frame to a CPU RGBA8 image; caller must UnloadImage() it.
    function GetFrameImage(AFrame: Integer): TImage;
    // Decodes a frame and uploads it; caller must UnloadTexture() it.
    function RenderTexture(AFrame: Integer): TTexture2D;
    property Width: Integer read GetWidth;    // atlas PNG size
    property Height: Integer read GetHeight;
    //* The JSON descriptor of the archive. The sprite editor stores its own
    //* animation metadata inside it (meta.tyro) and reads it back from here;
    //* the renderer itself only uses the frames it describes.
    property Descriptor: string read FDescriptor;
    property LastError: string read FLastError;
  end;

  { Convenience helpers used by the sprite loader (same flow as the PNG path) }

  TSpriteAtlasWriteFrame = record
    Name: string;
    Image: TImage;
    DurationMs: Integer;
  end;

  function IsSpriteFile(const FileName: string): Boolean;
  // Decodes every frame of a .sprite as an individual GPU texture plus its
  // duration in milliseconds; returns the number of frames (0 on failure).
  // The caller owns the textures and should free them with SpriteFreeTextures.
  function SpriteLoadFrameTextures(const FileName: string; out ATextures: TAseTextures; out ADurations: TAseFrameDurations): Integer;
  procedure SpriteFreeTextures(var ATextures: TAseTextures);
  function SaveSpriteAtlas(const FileName: string; const AFrames: array of TSpriteAtlasWriteFrame; const AImageName: string = 'sheet.png'; const AExtraMeta: string = ''): Boolean;

implementation

const
  cMaxFileSize = 512 * 1024 * 1024;   // cap on raw .sprite file bytes
  cMaxFrames = 65536;                 // cap on frames decoded from one atlas
  cSigLocal = $04034B50;              // local file header
  cSigCentral = $02014B50;            // central directory file header

{ Little-endian byte readers (all indices are pre-validated by the caller) }

function LE16(const B: TBytes; P: Integer): Word; inline;
begin
  Result := Word(B[P]) or (Word(B[P + 1]) shl 8);
end;

function LE32(const B: TBytes; P: Integer): Cardinal; inline;
begin
  Result := Cardinal(B[P]) or (Cardinal(B[P + 1]) shl 8) or
            (Cardinal(B[P + 2]) shl 16) or (Cardinal(B[P + 3]) shl 24);
end;

function BytesToStr(const B: TBytes): string;
begin
  Result := '';
  if Length(B) = 0 then
    Exit;
  SetLength(Result, Length(B));
  Move(B[0], Result[1], Length(B));
end;

{ Locate the End Of Central Directory record, scanning back over up to the
  64 KiB comment the format allows. }
function FindEOCD(const B: TBytes; out AOffset: Integer): Boolean;
var
  I, Start: Integer;
begin
  Result := False;
  AOffset := -1;
  if Length(B) < 22 then
    Exit;
  Start := Length(B) - 22 - 65535;
  if Start < 0 then
    Start := 0;
  for I := Length(B) - 22 downto Start do
    if (B[I] = $50) and (B[I + 1] = $4B) and (B[I + 2] = $05) and (B[I + 3] = $06) then
    begin
      AOffset := I;
      Exit(True);
    end;
end;

{ Walks the central directory and records every entry. Zip64 archives are
  rejected (the sizes/counts use escape markers we do not follow). }
function ScanZipDirectory(const B: TBytes; out AEntries: TZipEntryArray): Boolean;
var
  EOCD, Pos, I, Count, NameLen, ExtraLen, CommentLen: Integer;
  LocalPos: Int64;
  E: TZipEntry;
  Sig: Cardinal;
begin
  Result := False;
  AEntries := nil;
  if not FindEOCD(B, EOCD) then
    Exit;
  Count := LE16(B, EOCD + 10);
  Pos := Integer(LE32(B, EOCD + 16));      // central directory offset
  if (Count = 0) or (Count > cMaxFrames * 4) then
    Exit;
  SetLength(AEntries, Count);
  for I := 0 to Count - 1 do
  begin
    if (Pos < 0) or (Pos + 46 > Length(B)) then
      Exit;
    Sig := LE32(B, Pos);
    if Sig <> cSigCentral then
      Exit;
    E.Method := LE16(B, Pos + 10);
    E.CompSize := LE32(B, Pos + 20);
    E.UncompSize := LE32(B, Pos + 24);
    NameLen := LE16(B, Pos + 28);
    ExtraLen := LE16(B, Pos + 30);
    CommentLen := LE16(B, Pos + 32);
    LocalPos := LE32(B, Pos + 42);
    if (E.CompSize = $FFFFFFFF) or (E.UncompSize = $FFFFFFFF) or (LocalPos < 0) then
      Exit;                                // zip64: unsupported
    if Pos + 46 + NameLen > Length(B) then
      Exit;
    if NameLen > 0 then
      E.Name := BytesToStr(Copy(B, Pos + 46, NameLen))
    else
      E.Name := '';
    if (LocalPos + 30 > Length(B)) or (LE32(B, LocalPos) <> cSigLocal) then
      Exit;
    E.DataOffset := LocalPos + 30 + LE16(B, LocalPos + 26) + LE16(B, LocalPos + 28);
    if E.DataOffset + Int64(E.CompSize) > Int64(Length(B)) then
      Exit;
    AEntries[I] := E;
    Inc(Pos, 46 + NameLen + ExtraLen + CommentLen);
  end;
  Result := True;
end;

{ Raw DEFLATE (method 8) or a plain copy (method 0) of one entry. }
function ExtractZipEntry(const B: TBytes; const E: TZipEntry; out AData: TBytes): Boolean;
var
  N: Integer;
begin
  Result := False;
  AData := nil;
  if E.UncompSize = 0 then
  begin
    Result := True;                        // an empty file is valid
    Exit;
  end;
  if E.UncompSize > Cardinal(cMaxFileSize) then
    Exit;
  SetLength(AData, Integer(E.UncompSize));
  case E.Method of
    0: // stored
      begin
        if E.CompSize <> E.UncompSize then
          Exit;
        Move(B[E.DataOffset], AData[0], Integer(E.UncompSize));
        Result := True;
      end;
    8: // deflate
      begin
        N := InflateRawBuffer(@B[E.DataOffset], Integer(E.CompSize), @AData[0], Integer(E.UncompSize));
        Result := (N = Integer(E.UncompSize));
      end;
  end;
  if not Result then
    AData := nil;
end;

{ Small typed accessors over the parsed JSON tree (nil-safe). }

function DonChild(V: TDON_Value; const Name: string): TDON_Value;
begin
  if V = nil then
    Result := nil
  else
    Result := V.Values[Name];
end;

function DonInt(V: TDON_Value; const Name: string; Def: Integer): Integer;
var
  C: TDON_Value;
begin
  Result := Def;
  C := DonChild(V, Name);
  if C <> nil then
    Result := C.AsInteger;
end;

function DonBool(V: TDON_Value; const Name: string; Def: Boolean): Boolean;
var
  C: TDON_Value;
begin
  Result := Def;
  C := DonChild(V, Name);
  if C <> nil then
    Result := C.AsBoolean;
end;

function DonStr(V: TDON_Value; const Name: string; const Def: string): string;
var
  C: TDON_Value;
begin
  Result := Def;
  C := DonChild(V, Name);
  if C <> nil then
    Result := C.AsString;
end;

{ Reads one frame entry.  Item is the frame object; AName is the object key
  when frames is a hash (empty for the array layout). }
function ParseFrameItem(Item: TDON_Value; const AName: string; out AFrame: TSpriteAtlasFrame): Boolean;
var
  Rect, SS, Src: TDON_Value;
  Name: string;
begin
  Result := False;
  AFrame := Default(TSpriteAtlasFrame);
  if Item = nil then
    Exit;

  Rect := DonChild(Item, 'frame');
  if (Rect <> nil) and (Rect is TDON_Object) then
  begin
    AFrame.X := DonInt(Rect, 'x', 0);
    AFrame.Y := DonInt(Rect, 'y', 0);
    AFrame.W := DonInt(Rect, 'w', DonInt(Rect, 'width', 0));
    AFrame.H := DonInt(Rect, 'h', DonInt(Rect, 'height', 0));
  end
  else
  begin
    AFrame.X := DonInt(Item, 'x', DonInt(Item, 'left', 0));
    AFrame.Y := DonInt(Item, 'y', DonInt(Item, 'top', 0));
    AFrame.W := DonInt(Item, 'w', DonInt(Item, 'width', 0));
    AFrame.H := DonInt(Item, 'h', DonInt(Item, 'height', 0));
  end;
  if (AFrame.W <= 0) or (AFrame.H <= 0) then
    Exit;

  Name := AName;
  if Name = '' then
    Name := DonStr(Item, 'filename', '');
  if Name = '' then
    Name := DonStr(Item, 'name', '');
  AFrame.Name := Name;

  AFrame.Rotated := DonBool(Item, 'rotated', False);
  AFrame.Trimmed := DonBool(Item, 'trimmed', False);

  SS := DonChild(Item, 'sourceSize');
  if SS <> nil then
  begin
    AFrame.OriginalW := DonInt(SS, 'w', DonInt(SS, 'width', 0));
    AFrame.OriginalH := DonInt(SS, 'h', DonInt(SS, 'height', 0));
  end;
  Src := DonChild(Item, 'spriteSourceSize');
  if Src <> nil then
  begin
    AFrame.SrcX := DonInt(Src, 'x', 0);
    AFrame.SrcY := DonInt(Src, 'y', 0);
  end;

  AFrame.DurationMs := DonInt(Item, 'duration', 0);
  if (AFrame.DurationMs <= 0) and (Rect <> nil) then
    AFrame.DurationMs := DonInt(Rect, 'duration', 0);
  Result := True;
end;

{ TSpriteAtlas }

constructor TSpriteAtlas.Create;
begin
  inherited Create;
  FAtlas := Default(TImage);
end;

destructor TSpriteAtlas.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSpriteAtlas.Clear;
begin
  if FHasAtlas and (FAtlas.Data <> nil) then
    RayLib.UnloadImage(FAtlas);
  FAtlas := Default(TImage);
  FHasAtlas := False;
  FData := nil;
  FEntries := nil;
  FFrames := nil;
  FImageHint := '';
  FDescriptor := '';
end;

procedure TSpriteAtlas.AppendFrame(const AFrame: TSpriteAtlasFrame);
var
  N: Integer;
begin
  N := Length(FFrames);
  if N >= cMaxFrames then
    Exit;
  SetLength(FFrames, N + 1);
  FFrames[N] := AFrame;
end;

function TSpriteAtlas.ReadZipDirectory: Boolean;
begin
  Result := ScanZipDirectory(FData, FEntries);
end;

function TSpriteAtlas.FindEntry(const AExt, APreferName: string; out AIndex: Integer): Boolean;
var
  I, Fallback: Integer;
  Want: string;
begin
  Result := False;
  AIndex := -1;
  Fallback := -1;
  Want := LowerCase(ExtractFileName(APreferName));
  for I := 0 to Length(FEntries) - 1 do
  begin
    if LowerCase(ExtractFileExt(FEntries[I].Name)) <> AExt then
      Continue;
    if (Want <> '') and (LowerCase(ExtractFileName(FEntries[I].Name)) = Want) then
    begin
      AIndex := I;
      Exit(True);
    end;
    if Fallback < 0 then
      Fallback := I;
  end;
  if Fallback >= 0 then
  begin
    AIndex := Fallback;
    Result := True;
  end;
end;

function TSpriteAtlas.EntryData(AIndex: Integer; out AData: TBytes): Boolean;
begin
  Result := False;
  AData := nil;
  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
    Exit;
  Result := ExtractZipEntry(FData, FEntries[AIndex], AData);
end;

procedure TSpriteAtlas.ParseDescriptor(ARoot: TDON_Value);
var
  FramesNode: TDON_Value;
  Obj: TDON_Object;
  Arr: TDON_Array;
  F: TSpriteAtlasFrame;
  I: Integer;
begin
  FFrames := nil;
  if ARoot = nil then
    Exit;

  FImageHint := DonStr(DonChild(ARoot, 'meta'), 'image', '');

  FramesNode := DonChild(ARoot, 'frames');
  if FramesNode = nil then
    FramesNode := DonChild(ARoot, 'textures');   // some exporters use this key
  if (FramesNode = nil) and (ARoot is TDON_Array) then
    FramesNode := ARoot;
  if FramesNode = nil then
    Exit;

  if FramesNode is TDON_Object then
  begin
    Obj := TDON_Object(FramesNode);
    for I := 0 to Obj.Count - 1 do
      if ParseFrameItem(Obj.Pairs[I].Value, Obj.Pairs[I].Name, F) then
        AppendFrame(F);
  end
  else if FramesNode is TDON_Array then
  begin
    Arr := TDON_Array(FramesNode);
    for I := 0 to Arr.Count - 1 do
      if ParseFrameItem(Arr[I], '', F) then
        AppendFrame(F);
  end;
end;

function TSpriteAtlas.BuildFrameImage(const AFrame: TSpriteAtlasFrame): TImage;
var
  Region: TRectangle;
  Crop, Canvas: TImage;
  ExpW, ExpH, Row, SrcCol, DstX, Cols: Integer;
  SrcP, DstP: PByte;
begin
  Result := Default(TImage);
  if not FHasAtlas then
    Exit;

  // Clamp the source rectangle so ImageFromImage never reads out of bounds.
  Region.X := AFrame.X;
  Region.Y := AFrame.Y;
  Region.Width := AFrame.W;
  Region.Height := AFrame.H;
  if Region.X < 0 then
  begin
    Region.Width := Region.Width + Region.X;
    Region.X := 0;
  end;
  if Region.Y < 0 then
  begin
    Region.Height := Region.Height + Region.Y;
    Region.Y := 0;
  end;
  if Region.X + Region.Width > FAtlas.Width then
    Region.Width := FAtlas.Width - Region.X;
  if Region.Y + Region.Height > FAtlas.Height then
    Region.Height := FAtlas.Height - Region.Y;
  if (Region.Width < 1) or (Region.Height < 1) then
    Exit;

  Crop := RayLib.ImageFromImage(FAtlas, Region);
  if Crop.Data = nil then
    Exit;
  if AFrame.Rotated then
    RayLib.ImageRotateCCW(Crop);           // undo the 90-degree clockwise packing

  ExpW := AFrame.OriginalW;
  ExpH := AFrame.OriginalH;
  if ExpW <= 0 then
    ExpW := Crop.Width;
  if ExpH <= 0 then
    ExpH := Crop.Height;

  if (ExpW = Crop.Width) and (ExpH = Crop.Height) then
  begin
    Result := Crop;                        // already the untrimmed frame
    Exit;
  end;

  // Trimmed frame: paint the kept pixels back inside the original-size frame.
  Canvas := RayLib.GenImageColor(ExpW, ExpH, clBlank);
  if Canvas.Data = nil then
  begin
    RayLib.UnloadImage(Crop);
    Exit;
  end;
  for Row := 0 to Crop.Height - 1 do
  begin
    if (AFrame.SrcY + Row < 0) or (AFrame.SrcY + Row >= ExpH) then
      Continue;
    SrcCol := 0;
    DstX := AFrame.SrcX;
    if DstX < 0 then
    begin
      SrcCol := -DstX;
      DstX := 0;
    end;
    Cols := Crop.Width - SrcCol;
    if DstX + Cols > ExpW then
      Cols := ExpW - DstX;
    if Cols <= 0 then
      Continue;
    SrcP := PByte(Crop.Data) + (Row * Crop.Width + SrcCol) * 4;
    DstP := PByte(Canvas.Data) + ((AFrame.SrcY + Row) * ExpW + DstX) * 4;
    Move(SrcP^, DstP^, Cols * 4);
  end;
  RayLib.UnloadImage(Crop);
  Result := Canvas;
end;

function TSpriteAtlas.GetWidth: Integer;
begin
  if FHasAtlas then
    Result := FAtlas.Width
  else
    Result := 0;
end;

function TSpriteAtlas.GetHeight: Integer;
begin
  if FHasAtlas then
    Result := FAtlas.Height
  else
    Result := 0;
end;

function TSpriteAtlas.GetFrameCount: Integer;
begin
  Result := Length(FFrames);
end;

function TSpriteAtlas.GetFrame(AFrame: Integer): TSpriteAtlasFrame;
begin
  if (AFrame >= 0) and (AFrame < Length(FFrames)) then
    Result := FFrames[AFrame]
  else
    Result := Default(TSpriteAtlasFrame);
end;

function TSpriteAtlas.GetFrameDuration(AFrame: Integer): Integer;
begin
  Result := GetFrame(AFrame).DurationMs;
end;

function TSpriteAtlas.GetFrameImage(AFrame: Integer): TImage;
begin
  Result := Default(TImage);
  if (AFrame < 0) or (AFrame >= Length(FFrames)) then
    Exit;
  Result := BuildFrameImage(FFrames[AFrame]);
end;

function TSpriteAtlas.RenderTexture(AFrame: Integer): TTexture2D;
var
  Img: TImage;
begin
  Result := Default(TTexture2D);
  Img := GetFrameImage(AFrame);
  if Img.Data = nil then
    Exit;
  Result := RayLib.LoadTextureFromImage(Img);
  RayLib.UnloadImage(Img);
end;

function TSpriteAtlas.LoadFromFile(const FileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  FLastError := '';
  if not SysUtils.FileExists(FileName) then
  begin
    FLastError := 'File not found: ' + FileName;
    Exit;
  end;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

function TSpriteAtlas.LoadFromStream(AStream: TStream): Boolean;
var
  JsonData, PngData: TBytes;
  Root: TDON_Value;
  JsonIndex, PngIndex: Integer;
begin
  Result := False;
  FLastError := '';
  Clear;
  try
    if AStream.Size <= 0 then
    begin
      FLastError := 'Empty .sprite file';
      Exit;
    end;
    if AStream.Size > cMaxFileSize then
    begin
      FLastError := 'Sprite file too large: ' + IntToStr(AStream.Size);
      Exit;
    end;
    SetLength(FData, AStream.Size);
    AStream.Position := 0;
    AStream.ReadBuffer(FData[0], Length(FData));

    if not ReadZipDirectory then
    begin
      FLastError := 'Not a valid ZIP archive';
      Exit;
    end;

    if not FindEntry('.json', '', JsonIndex) then
    begin
      FLastError := 'ZIP archive has no JSON descriptor';
      Exit;
    end;
    if not EntryData(JsonIndex, JsonData) then
    begin
      FLastError := 'Unable to read the JSON descriptor';
      Exit;
    end;

    FDescriptor := BytesToStr(JsonData);
    Root := nil;
    try
      try
        Root := JsonParseValueString(FDescriptor);
      except
        on E: Exception do
        begin
          FLastError := 'Invalid JSON descriptor: ' + E.Message;
          Exit;
        end;
      end;
      ParseDescriptor(Root);
    finally
      Root.Free;
    end;

    if Length(FFrames) = 0 then
    begin
      FLastError := 'JSON descriptor does not describe any frames';
      Exit;
    end;

    if not FindEntry('.png', FImageHint, PngIndex) then
    begin
      FLastError := 'ZIP archive has no PNG texture';
      Exit;
    end;
    if not EntryData(PngIndex, PngData) then
    begin
      FLastError := 'Unable to read the PNG texture';
      Exit;
    end;

    FAtlas := RayLib.LoadImageFromMemory('.png', @PngData[0], Length(PngData));
    if (FAtlas.Data = nil) or (FAtlas.Width <= 0) or (FAtlas.Height <= 0) then
    begin
      FLastError := 'Unable to decode the atlas PNG';
      FAtlas := Default(TImage);
      Exit;
    end;
    // Work in RGBA8 so frame crops and un-trimming can copy plain pixels.
    RayLib.ImageFormat(FAtlas, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
    FHasAtlas := True;
    Result := True;
  except
    on E: Exception do
    begin
      if FLastError = '' then
        FLastError := E.Message;
      Result := False;
    end;
  end;
end;

{ Convenience helpers }

function IsSpriteFile(const FileName: string): Boolean;
begin
  Result := LowerCase(ExtractFileExt(FileName)) = '.sprite';
end;

function SpriteLoadFrameTextures(const FileName: string; out ATextures: TAseTextures; out ADurations: TAseFrameDurations): Integer;
var
  Atlas: TSpriteAtlas;
  I: Integer;
begin
  Result := 0;
  ATextures := nil;
  ADurations := nil;
  Atlas := TSpriteAtlas.Create;
  try
    if not Atlas.LoadFromFile(FileName) then
      Exit;
    Result := Atlas.GetFrameCount;
    if Result <= 0 then
      Exit;
    SetLength(ATextures, Result);
    SetLength(ADurations, Result);
    for I := 0 to Result - 1 do
    begin
      ATextures[I] := Atlas.RenderTexture(I);
      ADurations[I] := Atlas.GetFrameDuration(I);
      if ADurations[I] < 1 then
        ADurations[I] := 100;
      if ATextures[I].id <= 0 then
        Exit; // partial failure: caller releases whatever was uploaded
    end;
  finally
    Atlas.Free;
  end;
end;

procedure SpriteFreeTextures(var ATextures: TAseTextures);
begin
  AsepriteFreeTextures(ATextures);
end;

function StrBytes(const S: string): TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S) > 0 then
    Move(S[1], Result[0], Length(S));
end;

procedure PutU16(MS: TMemoryStream; V: Word);
begin
  MS.WriteBuffer(V, 2);
end;

procedure PutU32(MS: TMemoryStream; V: Cardinal);
begin
  MS.WriteBuffer(V, 4);
end;

function EntryCRC(const D: TBytes): Cardinal;
begin
  if Length(D) = 0 then
    Result := 0
  else
    Result := RayLib.ComputeCRC32(@D[0], Length(D));
end;

function RawDeflate(const D: TBytes): TBytes;
var
  C: PByte;
  CS: Integer;
begin
  Result := nil;
  if Length(D) = 0 then
    Exit;
  CS := 0;
  C := RayLib.CompressData(@D[0], Length(D), @CS);
  if (C = nil) or (CS <= 0) then
  begin
    if C <> nil then
      RayLib.MemFree(C);
    Exit;
  end;
  SetLength(Result, CS);
  Move(C[0], Result[0], CS);
  RayLib.MemFree(C);
end;

function BuildZipArchive(const ANames: array of string; const ADatas: array of TBytes): TBytes;
var
  MS: TMemoryStream;
  NameB, Payload: TBytes;
  Offsets: array of Cardinal;
  RealSizes: array of Cardinal;
  Methods: array of Word;
  I: Integer;
  CDStart, CDSize: Cardinal;
  Deflated: TBytes;
begin
  MS := TMemoryStream.Create;
  try
    SetLength(Offsets, Length(ANames));
    SetLength(RealSizes, Length(ANames));
    SetLength(Methods, Length(ANames));
    for I := 0 to High(ANames) do
    begin
      NameB := StrBytes(ANames[I]);
      Deflated := RawDeflate(ADatas[I]);
      if (Deflated <> nil) and (Length(Deflated) < Length(ADatas[I])) then
      begin
        Payload := Deflated;
        Methods[I] := 8; // Deflate
      end
      else
      begin
        Payload := ADatas[I];
        Methods[I] := 0; // Stored
      end;

      RealSizes[I] := Length(Payload);
      Offsets[I] := Cardinal(MS.Position);

      PutU32(MS, $04034B50);          // local file header
      PutU16(MS, 20);                 // version needed
      PutU16(MS, 0);                  // flags
      PutU16(MS, Methods[I]);         // method: 0 stored / 8 deflate
      PutU16(MS, 0);                  // mod time
      PutU16(MS, 0);                  // mod date
      PutU32(MS, EntryCRC(ADatas[I]));
      PutU32(MS, RealSizes[I]);
      PutU32(MS, Length(ADatas[I]));
      PutU16(MS, Length(NameB));
      PutU16(MS, 0);                  // extra len
      if Length(NameB) > 0 then
        MS.WriteBuffer(NameB[0], Length(NameB));
      if Length(Payload) > 0 then
        MS.WriteBuffer(Payload[0], Length(Payload));
    end;

    CDStart := Cardinal(MS.Position);
    for I := 0 to High(ANames) do
    begin
      NameB := StrBytes(ANames[I]);
      PutU32(MS, $02014B50);          // central directory header
      PutU16(MS, 20);                 // version made by
      PutU16(MS, 20);                 // version needed
      PutU16(MS, 0);                  // flags
      PutU16(MS, Methods[I]);
      PutU16(MS, 0);
      PutU16(MS, 0);
      PutU32(MS, EntryCRC(ADatas[I]));
      PutU32(MS, RealSizes[I]);
      PutU32(MS, Length(ADatas[I]));
      PutU16(MS, Length(NameB));
      PutU16(MS, 0);                  // extra len
      PutU16(MS, 0);                  // comment len
      PutU16(MS, 0);                  // disk number
      PutU16(MS, 0);                  // internal attrs
      PutU32(MS, 0);                  // external attrs
      PutU32(MS, Offsets[I]);
      if Length(NameB) > 0 then
        MS.WriteBuffer(NameB[0], Length(NameB));
    end;
    CDSize := Cardinal(MS.Position) - CDStart;

    PutU32(MS, $06054B50);            // end of central directory
    PutU16(MS, 0);                    // disk number
    PutU16(MS, 0);                    // disk with cd
    PutU16(MS, Length(ANames));       // entries on disk
    PutU16(MS, Length(ANames));       // total entries
    PutU32(MS, CDSize);
    PutU32(MS, CDStart);
    PutU16(MS, 0);                    // comment len

    SetLength(Result, MS.Size);
    if MS.Size > 0 then
    begin
      MS.Position := 0;
      MS.ReadBuffer(Result[0], MS.Size);
    end;
  finally
    MS.Free;
  end;
end;

function SaveSpriteAtlas(const FileName: string; const AFrames: array of TSpriteAtlasWriteFrame; const AImageName: string = 'sheet.png'; const AExtraMeta: string = ''): Boolean;
var
  N, I: Integer;
  MaxW, MaxH, Cols, Rows: Integer;
  AtlasW, AtlasH: Integer;
  AtlasImg: TImage;
  DstX, DstY, Col, Row: Integer;
  Json: string;
  PngBytes, ZipBytes: TBytes;
  PngSize: Integer;
  PngPtr: PByte;
  FS: TFileStream;
  BaseName: string;
begin
  Result := False;
  N := Length(AFrames);
  if N <= 0 then
    Exit;

  MaxW := 0;
  MaxH := 0;
  for I := 0 to N - 1 do
  begin
    if AFrames[I].Image.Width > MaxW then
      MaxW := AFrames[I].Image.Width;
    if AFrames[I].Image.Height > MaxH then
      MaxH := AFrames[I].Image.Height;
  end;

  if (MaxW <= 0) or (MaxH <= 0) then
    Exit;

  Cols := Ceil(Sqrt(N));
  if Cols < 1 then
    Cols := 1;
  Rows := (N + Cols - 1) div Cols;
  AtlasW := Cols * MaxW;
  AtlasH := Rows * MaxH;

  AtlasImg := RayLib.GenImageColor(AtlasW, AtlasH, clBlank);
  RayLib.ImageFormat(AtlasImg, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));

  // Build JSON descriptor
  BaseName := ChangeFileExt(ExtractFileName(AImageName), '');
  if BaseName = '' then
    BaseName := 'sheet';

  Json := '{"frames":[';
  for I := 0 to N - 1 do
  begin
    Col := I mod Cols;
    Row := I div Cols;
    DstX := Col * MaxW;
    DstY := Row * MaxH;

    RayLib.ImageDrawImage(@AtlasImg, AFrames[I].Image, DstX, DstY, clWhite);

    if I > 0 then
      Json := Json + ',';

    Json := Json + Format(
      '{"filename":"%s","frame":{"x":%d,"y":%d,"w":%d,"h":%d},' +
      '"rotated":false,"trimmed":false,' +
      '"spriteSourceSize":{"x":0,"y":0,"w":%d,"h":%d},' +
      '"sourceSize":{"w":%d,"h":%d},"duration":%d}',
      [AFrames[I].Name, DstX, DstY, AFrames[I].Image.Width, AFrames[I].Image.Height,
       AFrames[I].Image.Width, AFrames[I].Image.Height,
       AFrames[I].Image.Width, AFrames[I].Image.Height,
       AFrames[I].DurationMs]);
  end;

  Json := Json + Format('],"meta":{"app":"TyroSpriteEditor","image":"%s","size":{"w":%d,"h":%d}',
    [AImageName, AtlasW, AtlasH]);
  //AExtraMeta is a ready-made JSON object member list ("key":value,...). The
  //sprite editor uses it to keep its actions, durations and palette beside the
  //atlas frames; every other reader ignores it.
  if Trim(AExtraMeta) <> '' then
    Json := Json + ',' + AExtraMeta;
  Json := Json + '}}';

  PngSize := 0;
  PngPtr := RayLib.ExportImageToMemory(AtlasImg, '.png', PngSize);
  RayLib.UnloadImage(AtlasImg);

  if (PngPtr = nil) or (PngSize <= 0) then
    Exit;

  SetLength(PngBytes, PngSize);
  Move(PngPtr^, PngBytes[0], PngSize);
  RayLib.MemFree(PngPtr);

  ZipBytes := BuildZipArchive([BaseName + '.json', AImageName], [StrBytes(Json), PngBytes]);

  try
    FS := TFileStream.Create(FileName, fmCreate);
    try
      if Length(ZipBytes) > 0 then
        FS.WriteBuffer(ZipBytes[0], Length(ZipBytes));
      Result := True;
    finally
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

end.
