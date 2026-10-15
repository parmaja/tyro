program test_sprite_atlas;

{ Headless test for the .sprite atlas reader (src\tyrolib\SpriteAtlas.pas):

    - reads a ZIP archive built in memory (stored and deflate entries)
    - parses the JSON descriptor in both the array (Phaser 2 / PixiJS) and
      hash (Phaser 3 / TexturePacker) layouts
    - checks frame rectangles, names, per-frame duration
    - decodes frames back to RGBA pixels, including trimmed frames that must
      be restored to their original size, and rotated frames that must be
      turned upright again

  No window is opened: every image function used here works on CPU memory
  only, so the test runs without a GL context.

  Build and run:
    lazbuild --build-mode=Debug tests\test_sprite_atlas.lpi
    bin\test_sprite_atlas.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  RayLib,
  Aseprites, SpriteAtlas;

var
  Tests, Failures: Integer;

procedure Check(const AName: string; AOk: Boolean; const ADetail: string = '');
begin
  Inc(Tests);
  if AOk then
    WriteLn('  PASS  ', AName)
  else
  begin
    Inc(Failures);
    WriteLn('  FAIL  ', AName, '  <', ADetail, '>');
  end;
end;

function RGBA(R, G, B, A: Byte): Cardinal;
begin
  Result := Cardinal(R) or (Cardinal(G) shl 8) or (Cardinal(B) shl 16) or (Cardinal(A) shl 24);
end;

procedure SetPixel(var Img: TImage; X, Y: Integer; C: Cardinal);
var
  P: PByte;
begin
  P := PByte(Img.Data) + (Y * Img.Width + X) * 4;
  P^ := Byte(C and $FF);
  P[1] := Byte((C shr 8) and $FF);
  P[2] := Byte((C shr 16) and $FF);
  P[3] := Byte((C shr 24) and $FF);
end;

function PixelAt(const Img: TImage; X, Y: Integer): Cardinal;
var
  P: PByte;
begin
  P := PByte(Img.Data) + (Y * Img.Width + X) * 4;
  Result := Cardinal(P^) or (Cardinal(P[1]) shl 8) or (Cardinal(P[2]) shl 16) or (Cardinal(P[3]) shl 24);
end;

function StrBytes(const S: string): TBytes;
begin
  SetLength(Result, Length(S));
  if Length(S) > 0 then
    Move(S[1], Result[0], Length(S));
end;

function ImageToPNG(var Img: TImage): TBytes;
var
  Size: Integer;
  P: PByte;
begin
  Size := 0;
  P := ExportImageToMemory(Img, '.png', Size);
  SetLength(Result, Size);
  if Size > 0 then
    Move(P^, Result[0], Size);
  MemFree(P);
end;

function CRC(const D: TBytes): Cardinal;
begin
  if Length(D) = 0 then
    Result := 0
  else
    Result := ComputeCRC32(@D[0], Length(D));
end;

{ CompressData in this raylib build already emits a bare raw-DEFLATE stream
  (no zlib header/Adler trailer), which is exactly what a ZIP method-8 entry
  stores, so the payload is used verbatim. }
function RawDeflate(const D: TBytes): TBytes;
var
  C: PByte;
  CS: Integer;
begin
  Result := nil;
  if Length(D) = 0 then
    Exit;
  CS := 0;
  C := CompressData(@D[0], Length(D), @CS);
  if (C = nil) or (CS <= 0) then
  begin
    if C <> nil then
      MemFree(C);
    Exit;
  end;
  SetLength(Result, CS);
  Move(C[0], Result[0], CS);
  MemFree(C);
end;

procedure PutU16(MS: TMemoryStream; V: Word);
begin
  MS.WriteBuffer(V, 2);
end;

procedure PutU32(MS: TMemoryStream; V: Cardinal);
begin
  MS.WriteBuffer(V, 4);
end;

{ Minimal ZIP writer: local headers, central directory, EOCD. Entries are
  either stored (method 0) or raw-deflate compressed (method 8). }
function BuildZip(ADeflate: Boolean; const ANames: array of string; const ADatas: array of TBytes): TBytes;
var
  MS: TMemoryStream;
  NameB, Payload: TBytes;
  Offsets: array of Cardinal;
  RealSizes: array of Cardinal;
  I: Integer;
  CDStart, CDSize: Cardinal;
begin
  MS := TMemoryStream.Create;
  try
    SetLength(Offsets, Length(ANames));
    SetLength(RealSizes, Length(ANames));
    for I := 0 to High(ANames) do
    begin
      NameB := StrBytes(ANames[I]);
      if ADeflate then
        Payload := RawDeflate(ADatas[I])
      else
        Payload := ADatas[I];
      RealSizes[I] := Length(Payload);
      Offsets[I] := Cardinal(MS.Position);

      PutU32(MS, $04034B50);          // local file header
      PutU16(MS, 20);                 // version needed
      PutU16(MS, 0);                  // flags
      PutU16(MS, Ord(ADeflate) * 8);  // method: 0 stored / 8 deflate
      PutU16(MS, 0);                  // mod time
      PutU16(MS, 0);                  // mod date
      PutU32(MS, CRC(ADatas[I]));
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
      PutU16(MS, Ord(ADeflate) * 8);  // method
      PutU16(MS, 0);                  // time
      PutU16(MS, 0);                  // date
      PutU32(MS, CRC(ADatas[I]));
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

function LoadAtlas(const AZip: TBytes): TSpriteAtlas;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    if Length(AZip) > 0 then
      MS.WriteBuffer(AZip[0], Length(AZip));
    MS.Position := 0;
    Result := TSpriteAtlas.Create;
    if not Result.LoadFromStream(MS) then
    begin
      WriteLn('  (loader error: ', Result.LastError, ')');
      Result.Free;
      Result := nil;
    end;
  finally
    MS.Free;
  end;
end;

{ Makes a WxH RGBA8 canvas filled by AColorFn(x, y). }
type
  TColorFn = function(X, Y: Integer): Cardinal;

function MakeImage(W, H: Integer; AColorFn: TColorFn): TImage;
var
  X, Y: Integer;
begin
  Result := GenImageColor(W, H, clBlank);
  ImageFormat(Result, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      SetPixel(Result, X, Y, AColorFn(X, Y));
end;

function RedBlue(X, Y: Integer): Cardinal;
begin
  if X < 8 then
    Result := RGBA(255, 0, 0, 255)
  else
    Result := RGBA(0, 0, 255, 255);
end;

function MostlyBlank(X, Y: Integer): Cardinal;
begin
  if (X < 2) and (Y < 2) then
    Result := RGBA(255, 0, 0, 255)
  else
    Result := RGBA(0, 0, 0, 0);
end;

function Ramp(X, Y: Integer): Cardinal;
begin
  // 3x2 image with six distinct pixels: index 0..5
  Result := RGBA(10 + (Y * 3 + X) * 20, 20 + (Y * 3 + X) * 20, 30 + (Y * 3 + X) * 20, 255);
end;

var
  Atlas: TSpriteAtlas;
  AtlasImg, FrameImg: TImage;
  Png, Zip: TBytes;
  Json: string;
  F: TSpriteAtlasFrame;

begin
  Tests := 0;
  Failures := 0;
  InitLibrary;   // resolve the raylib entry points (no window is opened)
  WriteLn('test_sprite_atlas');

  Check('IsSpriteFile(.sprite)', IsSpriteFile('hero.sprite'));
  Check('IsSpriteFile is case-insensitive', IsSpriteFile('HERO.SPRITE'));
  Check('IsSpriteFile rejects .png', not IsSpriteFile('hero.png'));

  // --------------------------------------------------------------------
  // 1) array layout, stored (method 0): two 8x8 frames in a 16x8 sheet
  // --------------------------------------------------------------------
  AtlasImg := MakeImage(16, 8, RedBlue);
  Png := ImageToPNG(AtlasImg);
  UnloadImage(AtlasImg);
  Json :=
    '{"frames":[' +
    '{"filename":"red","frame":{"x":0,"y":0,"w":8,"h":8},"rotated":false,"trimmed":false,' +
    '"spriteSourceSize":{"x":0,"y":0,"w":8,"h":8},"sourceSize":{"w":8,"h":8}},' +
    '{"filename":"blue","frame":{"x":8,"y":0,"w":8,"h":8},"rotated":false,"trimmed":false,' +
    '"spriteSourceSize":{"x":0,"y":0,"w":8,"h":8},"sourceSize":{"w":8,"h":8}}' +
    '],"meta":{"image":"atlas.png"}}';
  Zip := BuildZip(False, ['atlas.json', 'atlas.png'], [StrBytes(Json), Png]);

  Atlas := LoadAtlas(Zip);
  Check('array layout: loaded', Atlas <> nil);
  if Atlas <> nil then
  begin
    Check('array layout: frame count = 2', Atlas.GetFrameCount = 2, IntToStr(Atlas.GetFrameCount));
    Check('array layout: atlas size 16x8', (Atlas.Width = 16) and (Atlas.Height = 8),
      IntToStr(Atlas.Width) + 'x' + IntToStr(Atlas.Height));
    F := Atlas.GetFrame(0);
    Check('array layout: frame 0 name', F.Name = 'red', F.Name);
    Check('array layout: frame 0 rect', (F.X = 0) and (F.Y = 0) and (F.W = 8) and (F.H = 8));

    FrameImg := Atlas.GetFrameImage(0);
    Check('array layout: frame 0 size 8x8', (FrameImg.Width = 8) and (FrameImg.Height = 8));
    Check('array layout: frame 0 is red', PixelAt(FrameImg, 4, 4) = RGBA(255, 0, 0, 255));
    UnloadImage(FrameImg);

    FrameImg := Atlas.GetFrameImage(1);
    Check('array layout: frame 1 is blue', PixelAt(FrameImg, 4, 4) = RGBA(0, 0, 255, 255));
    UnloadImage(FrameImg);

    Check('array layout: default duration 0', Atlas.GetFrameDuration(0) = 0);
    Atlas.Free;
  end;

  // --------------------------------------------------------------------
  // 2) hash layout, deflate (method 8) + trimmed frame
  // --------------------------------------------------------------------
  AtlasImg := MakeImage(8, 8, MostlyBlank);
  Png := ImageToPNG(AtlasImg);
  UnloadImage(AtlasImg);
  Json :=
    '{"frames":{"red":{"frame":{"x":0,"y":0,"w":2,"h":2},"rotated":false,"trimmed":true,' +
    '"spriteSourceSize":{"x":3,"y":3,"w":2,"h":2},"sourceSize":{"w":8,"h":8},"duration":40}},' +
    '"meta":{"image":"sheet.png"}}';
  Zip := BuildZip(True, ['desc.json', 'sheet.png'], [StrBytes(Json), Png]);

  Atlas := LoadAtlas(Zip);
  Check('hash+deflate+trim: loaded', Atlas <> nil);
  if Atlas <> nil then
  begin
    Check('hash layout: frame count = 1', Atlas.GetFrameCount = 1);
    F := Atlas.GetFrame(0);
    Check('hash layout: frame name from key', F.Name = 'red', F.Name);
    Check('hash layout: trimmed flag', F.Trimmed);
    Check('hash layout: duration = 40', Atlas.GetFrameDuration(0) = 40);
    FrameImg := Atlas.GetFrameImage(0);
    Check('trim: restored to source size 8x8', (FrameImg.Width = 8) and (FrameImg.Height = 8),
      IntToStr(FrameImg.Width) + 'x' + IntToStr(FrameImg.Height));
    Check('trim: red at spriteSourceSize offset', PixelAt(FrameImg, 3, 3) = RGBA(255, 0, 0, 255));
    Check('trim: transparent outside the kept area', PixelAt(FrameImg, 0, 0) = RGBA(0, 0, 0, 0));
    UnloadImage(FrameImg);
    Atlas.Free;
  end;

  // --------------------------------------------------------------------
  // 3) rotated frame: a 3x2 original packed 90 degrees clockwise
  // --------------------------------------------------------------------
  AtlasImg := MakeImage(3, 2, Ramp);
  ImageRotateCW(AtlasImg);                 // now 2x3, exactly how a packer stores it
  Png := ImageToPNG(AtlasImg);
  UnloadImage(AtlasImg);
  Json :=
    '{"frames":[' +
    '{"filename":"tile","frame":{"x":0,"y":0,"w":2,"h":3},"rotated":true,"trimmed":false,' +
    '"spriteSourceSize":{"x":0,"y":0,"w":3,"h":2},"sourceSize":{"w":3,"h":2}}' +
    '],"meta":{"image":"rot.png"}}';
  Zip := BuildZip(False, ['rot.json', 'rot.png'], [StrBytes(Json), Png]);

  Atlas := LoadAtlas(Zip);
  Check('rotated: loaded', Atlas <> nil);
  if Atlas <> nil then
  begin
    FrameImg := Atlas.GetFrameImage(0);
    Check('rotated: restored to 3x2', (FrameImg.Width = 3) and (FrameImg.Height = 2),
      IntToStr(FrameImg.Width) + 'x' + IntToStr(FrameImg.Height));
    // explicit per-pixel comparison against the original ramp
    Check('rotated: pixels match the original',
      (PixelAt(FrameImg, 0, 0) = Ramp(0, 0)) and
      (PixelAt(FrameImg, 1, 0) = Ramp(1, 0)) and
      (PixelAt(FrameImg, 2, 0) = Ramp(2, 0)) and
      (PixelAt(FrameImg, 0, 1) = Ramp(0, 1)) and
      (PixelAt(FrameImg, 1, 1) = Ramp(1, 1)) and
      (PixelAt(FrameImg, 2, 1) = Ramp(2, 1)));
    UnloadImage(FrameImg);
    Atlas.Free;
  end;

  // --------------------------------------------------------------------
  // 4) rejects a non-ZIP file
  // --------------------------------------------------------------------
  Atlas := LoadAtlas(StrBytes('this is definitely not a zip archive'));
  Check('non-ZIP rejected', Atlas = nil);

  // --------------------------------------------------------------------
  // 5) optional: a .sprite produced by an external zipper (Python zipfile)
  //    passed as the first command-line argument
  // --------------------------------------------------------------------
  if ParamCount >= 1 then
  begin
    Atlas := TSpriteAtlas.Create;
    try
      Check('external .sprite: loaded', Atlas.LoadFromFile(ParamStr(1)),
        Atlas.LastError);
      if Atlas.GetFrameCount > 0 then
      begin
        FrameImg := Atlas.GetFrameImage(0);
        Check('external .sprite: frame 0 decodes to a non-empty image',
          (FrameImg.Width > 0) and (FrameImg.Height > 0) and (FrameImg.Data <> nil),
          IntToStr(FrameImg.Width) + 'x' + IntToStr(FrameImg.Height));
        if FrameImg.Data <> nil then
          UnloadImage(FrameImg);
      end;
    finally
      Atlas.Free;
    end;
  end;

  WriteLn;
  WriteLn(Format('%d checks, %d failure(s)', [Tests, Failures]));
  if Failures = 0 then
    Halt(0)
  else
    Halt(Failures);
end.
