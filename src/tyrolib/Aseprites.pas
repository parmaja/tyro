unit Aseprites;

{**
 *  Aseprite (.ase / .aseprite) file support for Tyro
 *
 *  Parses, renders and writes the Aseprite binary format described in the
 *  official file format specification:
 *    https://github.com/aseprite/aseprite/blob/main/docs/ase-file-specs.md
 *
 *  The renderer composites all cels (layers) of a frame into a single RGBA8
 *  image, so an Aseprite file can be loaded into a sprite exactly like a PNG:
 *      sprite:load("hero.ase")
 *
 *  Supported:
 *    - Frames with per-frame duration (animation metadata)
 *    - Normal / group layers (opacity + visibility propagate through groups)
 *    - Raw (0), linked (1) and zlib-compressed (2) image cels
 *    - RGBA (32 bpp), grayscale (16 bpp) and indexed (8 bpp) color depths
 *    - New (0x2019) and old (0x0004 / 0x0011) palette chunks
 *    - Cel z-index ordering (rendered back-to-front like Aseprite)
 *    - A minimal encoder (AsepriteSaveRGBA) to write single-frame .ase files
 *
 *  Not rendered (parsed and skipped): tilemap cels, slices, tags, masks.
 *
 *  @license   MIT
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, RayLib;

type
  { A layer description read from a Layer Chunk (0x2004). }
  TAseLayer = record
    Name: string;
    LayerType: Word;    // 0 = normal, 1 = group, 2 = tilemap
    ChildLevel: Word;
    Opacity: Byte;
    BlendMode: Word;
    Visible: Boolean;
    Background: Boolean;
    Reference: Boolean;
    // Resolved after parsing: opacity/visibility including parent groups
    EffectiveOpacity: Byte;
    EffectiveVisible: Boolean;
  end;

  { One cel: a piece of image data on a layer for one frame. }
  TAseCel = record
    LayerIndex: Integer;
    X: Integer;         // position in the sprite (can be negative)
    Y: Integer;
    Opacity: Byte;      // per-cel opacity (0..255)
    ZIndex: SmallInt;   // z-order adjustment for this cel
    CelType: Word;      // 0 = raw, 1 = linked, 2 = compressed, 3 = tilemap
    LinkFrame: Integer; // for linked cels: frame holding the pixels
    Width: Integer;
    Height: Integer;
    Pixels: PByte;      // RGBA8 (straight alpha), owned by TAseprite
  end;

  { A frame: duration in milliseconds plus the list of cels. }
  TAseFrame = record
    Duration: Integer;
    Cels: array of TAseCel;
  end;

  TAsePaletteEntry = record
    R, G, B, A: Byte;
    Valid: Boolean;
  end;

  { TAseprite }

  TAseprite = class(TObject)
  private
    FData: TBytes;
    FPos: Integer;
    FFileSize: Integer;
    FHeaderFrames: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FColorDepth: Integer;   // 8 = indexed, 16 = grayscale, 32 = RGBA
    FSpriteFlags: Cardinal;
    FSpeed: Word;
    FTransparentIndex: Byte;
    FPalette: array of TAsePaletteEntry;
    FLayers: array of TAseLayer;
    FFrames: array of TAseFrame;
    FLastError: string;
    procedure Fatal(const AMsg: string);
    procedure EnsureAvailable(ACount: Integer);
    procedure Clear;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: Cardinal;
    function ReadShort: SmallInt;
    function ReadString: string;
    procedure ReadHeader;
    procedure ReadFrame;
    procedure ReadLayerChunk;
    procedure ReadCelChunk(AChunkStart, AChunkSize: Integer);
    procedure ReadPaletteChunk(AChunkStart, AChunkSize: Integer);
    procedure ReadOldPaletteChunk(AChunkStart, AChunkSize: Integer; A6Bit: Boolean);
    procedure AddCel(const ACel: TAseCel);
    procedure ResolveLayerHierarchy;
    function IsBackgroundLayer(ALayer: Integer): Boolean;
    function GetLayerOpacity(ALayer: Integer): Byte;
    function GetLayerVisible(ALayer: Integer): Boolean;
    procedure DecodeToRGBA(ASrc: PByte; APixelCount: Integer; ADst: PByte; ABackground: Boolean);
    function ResolveLinkedCel(AFrame, ALayer: Integer): TAseCel;
    procedure BlitCel(ADst: PByte; const ACel: TAseCel; AOpacityFactor: Single);
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(const FileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function GetFrameCount: Integer;
    function GetFrameDuration(AFrame: Integer): Integer;
    // Renders frame AFrame into a newly allocated RGBA8 buffer (Width*Height*4
    // bytes, straight alpha). The caller owns the buffer and must FreeMem it.
    function RenderBuffer(AFrame: Integer): PByte;
    // Renders a frame into a raylib TImage; caller must UnloadImage() it.
    function RenderImage(AFrame: Integer): TImage;
    // Renders a frame and uploads it to the GPU; caller must UnloadTexture() it.
    function RenderTexture(AFrame: Integer = 0): TTexture2D;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property ColorDepth: Integer read FColorDepth;
    property LastError: string read FLastError;
  end;

  { Convenience helpers used by the sprite loader (same flow as the PNG path) }

  function IsAsepriteFile(const FileName: string): Boolean;
  function AsepriteLoadImage(const FileName: string; AFrame: Integer = 0): TImage;
  function AsepriteLoadTexture(const FileName: string; AFrame: Integer = 0): TTexture2D;
  // Renders every frame side by side into one texture (a spritesheet strip)
  function AsepriteLoadSheetTexture(const FileName: string; out AFrameCount: Integer; out AFrameWidth, AFrameHeight: Integer): TTexture2D;

type
  { Frame arrays returned by AsepriteLoadFrameTextures. Each texture is an
    independent GPU upload owned by the caller; release them with
    AsepriteFreeTextures after installing them into a sprite. }
  TAseTextures = array of TTexture2D;
  TAseFrameDurations = array of Integer;

  // Renders every frame of an animation as an individual GPU texture plus its
  // duration in milliseconds; returns the number of frames (0 on failure).
  // The caller owns the textures and must free them with AsepriteFreeTextures.
  function AsepriteLoadFrameTextures(const FileName: string; out ATextures: TAseTextures; out ADurations: TAseFrameDurations): Integer;
  procedure AsepriteFreeTextures(var ATextures: TAseTextures);
  // Encodes a single-frame RGBA8 sprite as a valid .ase file.
  function AsepriteSaveRGBA(const FileName: string; AWidth, AHeight: Integer; APixels: PByte; ADuration: Integer = 100; const ALayerName: string = 'Layer 1'): Boolean;

implementation

const
  cAseMagic = $A5E0;
  cFrameMagic = $F1FA;

  // Chunk types we handle (all others are skipped by their size field)
  cChunkLayer = $2004;
  cChunkCel = $2005;
  cChunkPalette = $2019;
  cChunkOldPalette6 = $0011;   // 6-bit colors (0..63)
  cChunkOldPalette8 = $0004;   // 8-bit colors

  cMaxDim = 8192;              // sanity cap for sprite/cel dimensions
  cMaxLayers = 1024;           // cap on layer chunks (guards SetLength growth)
  cMaxPaletteEntries = 4096;   // cap on palette size (palette chunk declares a u32 size)
  cMaxFileSize = 512 * 1024 * 1024;          // cap on raw Aseprite file bytes
  cMaxSheetFrames = 4096;                    // cap on frames in one sheet texture
  cMaxSheetPixels = 64 * 1024 * 1024;        // cap on sheet texture pixels (RGBA)

type
  TRenderItem = record
    Cel: TAseCel;
    Order: Integer;
    Z: Integer;
  end;

{ Pure-Pascal zlib (RFC 1950) / DEFLATE (RFC 1951) support.
  zlib1.dll is not available on the target machines, so both directions are
  implemented from scratch:
    - ZlibCompressBuf writes a "stored" (uncompressed) DEFLATE stream wrapped
      in a valid zlib header/trailer; any real inflate reads it.
    - ZlibDecompressBuf fully decodes stored, fixed-Huffman and dynamic-Huffman
      blocks exactly as produced by zlib (and therefore Aseprite). }

type
  TZBitReader = record
    Src: PByte;
    SrcLen: Integer;
    Pos: Integer;        // next source byte
    BitBuf: Cardinal;    // pending bits, MSB first
    BitCount: Integer;   // number of pending bits in BitBuf
    Failed: Boolean;
  end;

  TZNode = record
    Child0: Integer;     // child index for bit 0, -1 = none
    Child1: Integer;     // child index for bit 1, -1 = none
    Symbol: Integer;     // decoded symbol at a leaf, -1 = internal node
  end;

  { Canonical Huffman decoding tree (codes are consumed MSB first). }
  TZTree = class(TObject)
  private
    FNodes: array of TZNode;
  public
    Root: Integer;
    constructor Create;
    procedure Clear;
    procedure AddCode(ACode: Cardinal; ALen, ASymbol: Integer);
    function Decode(var BR: TZBitReader): Integer;
  end;

procedure BitInit(var BR: TZBitReader; ASrc: PByte; ASrcLen: Integer);
begin
  BR.Src := ASrc;
  BR.SrcLen := ASrcLen;
  BR.Pos := 0;
  BR.BitBuf := 0;
  BR.BitCount := 0;
  BR.Failed := False;
end;

function BitRead(var BR: TZBitReader; ACount: Integer): Cardinal;
var
  V: Cardinal;
begin
  if BR.Failed then
    Exit(0);
  while BR.BitCount < ACount do
  begin
    if BR.Pos >= BR.SrcLen then
    begin
      BR.Failed := True;            // truncated bit stream
      Exit(0);
    end;
    // DEFLATE packs bits LSB-first: push the new byte above the pending bits
    // so later reads see earlier-transmitted bits first.
    BR.BitBuf := BR.BitBuf or (Cardinal(BR.Src[BR.Pos]) shl BR.BitCount);
    Inc(BR.Pos);
    Inc(BR.BitCount, 8);
  end;
  V := BR.BitBuf and ((Cardinal(1) shl ACount) - 1);
  BR.BitBuf := BR.BitBuf shr ACount;
  Dec(BR.BitCount, ACount);
  Result := V;
end;

procedure BitAlignByte(var BR: TZBitReader);
begin
  BR.BitCount := BR.BitCount and (not 7);
end;

constructor TZTree.Create;
begin
  inherited Create;
  Root := -1;
end;

procedure TZTree.Clear;
begin
  FNodes := nil;
  Root := -1;
end;

procedure TZTree.AddCode(ACode: Cardinal; ALen, ASymbol: Integer);
var
  I, Bit, Node, NewIdx: Integer;
begin
  if Root < 0 then
  begin
    SetLength(FNodes, 1);
    FNodes[0].Child0 := -1;
    FNodes[0].Child1 := -1;
    FNodes[0].Symbol := -1;
    Root := 0;
  end;
  Node := Root;
  for I := ALen - 1 downto 0 do
  begin
    Bit := (Integer(ACode) shr I) and 1;
    if Bit = 0 then
    begin
      if FNodes[Node].Child0 < 0 then
      begin
        NewIdx := Length(FNodes);
        SetLength(FNodes, NewIdx + 1);
        FNodes[NewIdx].Child0 := -1;
        FNodes[NewIdx].Child1 := -1;
        FNodes[NewIdx].Symbol := -1;
        FNodes[Node].Child0 := NewIdx;
      end;
      Node := FNodes[Node].Child0;
    end
    else
    begin
      if FNodes[Node].Child1 < 0 then
      begin
        NewIdx := Length(FNodes);
        SetLength(FNodes, NewIdx + 1);
        FNodes[NewIdx].Child0 := -1;
        FNodes[NewIdx].Child1 := -1;
        FNodes[NewIdx].Symbol := -1;
        FNodes[Node].Child1 := NewIdx;
      end;
      Node := FNodes[Node].Child1;
    end;
  end;
  FNodes[Node].Symbol := ASymbol;
end;

function TZTree.Decode(var BR: TZBitReader): Integer;
var
  Node: Integer;
  Bit: Cardinal;
begin
  Result := -1;
  Node := Root;
  if Node < 0 then
    Exit;
  while FNodes[Node].Symbol < 0 do
  begin
    Bit := BitRead(BR, 1);
    if BR.Failed then
      Exit;
    if Bit = 0 then
      Node := FNodes[Node].Child0
    else
      Node := FNodes[Node].Child1;
    if Node < 0 then
      Exit;                // invalid code prefix
  end;
  Result := FNodes[Node].Symbol;
end;

{ Build a canonical Huffman tree from code lengths (RFC 1951, 3.2.2). }
procedure BuildHuffTree(const ALens: array of Integer; ACount: Integer; ATree: TZTree);
var
  BlCount: array[0..16] of Integer;
  NextCode: array[0..17] of Cardinal;
  I, Len, MaxLen: Integer;
begin
  for I := 0 to 16 do
    BlCount[I] := 0;
  MaxLen := 0;
  for I := 0 to ACount - 1 do
  begin
    Len := ALens[I];
    if Len > 15 then
      Len := 15;
    if Len > MaxLen then
      MaxLen := Len;
    if Len > 0 then
      Inc(BlCount[Len]);
  end;
  NextCode[0] := 0;
  NextCode[1] := 0;
  for Len := 1 to 15 do
    NextCode[Len + 1] := (NextCode[Len] + Cardinal(BlCount[Len])) shl 1;
  ATree.Clear;
  for I := 0 to ACount - 1 do
  begin
    Len := ALens[I];
    if Len = 0 then
      Continue;
    if Len > 15 then
      Len := 15;
    ATree.AddCode(NextCode[Len], Len, I);
    Inc(NextCode[Len]);
  end;
end;

{ RFC 1950 Adler-32 checksum. }
function Adler32(AData: PByte; ASize: Integer): Cardinal;
const
  cMod = 65521;
var
  A, B: Cardinal;
  N, I: Integer;
begin
  A := 1;
  B := 0;
  N := 0;
  while N < ASize do
  begin
    I := ASize - N;
    if I > 5552 then
      I := 5552;         // keep A and B far below 2^31 between mods
    while I > 0 do
    begin
      A := A + AData[N];
      B := B + A;
      Inc(N);
      Dec(I);
    end;
    A := A mod cMod;
    B := B mod cMod;
  end;
  Result := (B shl 16) or A;
end;

{ zlib wrapper (RFC 1950) around a stored-BLOCK DEFLATE stream. }
function ZlibCompressBuf(AData: PByte; ASize: Integer): TBytes;
const
  cMaxStored = 65535;
var
  A: Cardinal;
  BlockLen, Remaining, OutPos, I: Integer;
  OneComp: Word;
begin
  Result := nil;
  if (AData = nil) or (ASize <= 0) then
    Exit;
  SetLength(Result, 6 + ASize + (ASize div cMaxStored + 1) * 5);
  Result[0] := $78;                 // CMF: deflate, 32K window
  Result[1] := $01;                 // FLG: no compression preset, FCHECK valid
  OutPos := 2;
  Remaining := ASize;
  I := 0;
  while Remaining > 0 do
  begin
    BlockLen := Remaining;
    if BlockLen > cMaxStored then
      BlockLen := cMaxStored;
    if I + BlockLen >= ASize then
      Result[OutPos] := $01        // BFINAL set, BTYPE = 00 (stored)
    else
      Result[OutPos] := $00;       // BFINAL clear, BTYPE = 00
    Inc(OutPos);
    Result[OutPos] := Byte(BlockLen and $FF);
    Result[OutPos + 1] := Byte((BlockLen shr 8) and $FF);
    OneComp := Word(not BlockLen); // NLEN = one's complement of LEN
    Result[OutPos + 2] := Byte(OneComp and $FF);
    Result[OutPos + 3] := Byte((OneComp shr 8) and $FF);
    Inc(OutPos, 4);
    Move(AData[I], Result[OutPos], BlockLen);
    Inc(OutPos, BlockLen);
    Inc(I, BlockLen);
    Dec(Remaining, BlockLen);
  end;
  A := Adler32(AData, ASize);
  Result[OutPos] := Byte((A shr 24) and $FF);
  Result[OutPos + 1] := Byte((A shr 16) and $FF);
  Result[OutPos + 2] := Byte((A shr 8) and $FF);
  Result[OutPos + 3] := Byte(A and $FF);
  Inc(OutPos, 4);
  SetLength(Result, OutPos);
end;

{ Raw DEFLATE (RFC 1951) inflate. Returns the number of output bytes, or -1. }
function InflateRaw(ASrc: PByte; ASrcLen: Integer; ADst: PByte; ADstSize: Integer): Integer;
const
  cLenBase: array[257..285] of Word = (3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,
                                       35,43,51,59,67,83,99,115,131,163,195,227,258);
  cLenExtra: array[257..285] of Byte = (0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,
                                        4,4,4,4,5,5,5,5,0);
  cDistBase: array[0..29] of Word = (1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,
                                     257,385,513,769,1025,1537,2049,3073,4097,6145,
                                     8193,12289,16385,24577);
  cDistExtra: array[0..29] of Byte = (0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,
                                      9,9,10,10,11,11,12,12,13,13);
  cClOrder: array[0..18] of Byte = (16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15);
var
  BR: TZBitReader;
  LitTree, DistTree, ClTree: TZTree;
  CLens: array[0..18] of Integer;
  BLens: array of Integer;
  HLIT, HDIST, HCLEN, I, Sym, Len, DistSym, CNT, PrevLen, Dist: Integer;
  OutPos: Integer;
  BFINAL: Boolean;
  BTYPE: Integer;
  BlockLen: Cardinal;
  FixedBuilt: Boolean;
begin
  Result := -1;
  if (ASrc = nil) or (ASrcLen <= 0) or (ADst = nil) or (ADstSize <= 0) then
    Exit;
  BitInit(BR, ASrc, ASrcLen);
  LitTree := TZTree.Create;
  DistTree := TZTree.Create;
  ClTree := TZTree.Create;
  SetLength(BLens, 320);            // 288 literal/length + 32 distance codes
  OutPos := 0;
  FixedBuilt := False;
  try
    repeat
      BFINAL := BitRead(BR, 1) <> 0;
      if BR.Failed then
        Exit(-1);
      BTYPE := Integer(BitRead(BR, 2));
      if BTYPE = 3 then
        Exit(-1);                   // reserved block type

      if BTYPE = 0 then             // ---- stored block
      begin
        BitAlignByte(BR);
        BlockLen := BitRead(BR, 16);
        if BitRead(BR, 16) <> (BlockLen xor $FFFF) then
          Exit(-1);                 // NLEN does not match LEN
        if BR.Failed then
          Exit(-1);
        if BlockLen > Cardinal(BR.SrcLen - BR.Pos) then
          Exit(-1);
        if OutPos + Integer(BlockLen) > ADstSize then
          Exit(-1);
        if BlockLen > 0 then
        begin
          Move(BR.Src[BR.Pos], ADst[OutPos], BlockLen);
          Inc(BR.Pos, Integer(BlockLen));
          Inc(OutPos, Integer(BlockLen));
        end;
        BR.BitCount := 0;           // data is byte-aligned again
      end
      else
      begin
        if BTYPE = 1 then           // ---- fixed Huffman tables
        begin
          if not FixedBuilt then
          begin
            for I := 0 to 143 do BLens[I] := 8;
            for I := 144 to 255 do BLens[I] := 9;
            for I := 256 to 279 do BLens[I] := 7;
            for I := 280 to 287 do BLens[I] := 8;
            BuildHuffTree(BLens, 288, LitTree);
            for I := 0 to 31 do BLens[I] := 5;
            BuildHuffTree(BLens, 32, DistTree);
            FixedBuilt := True;
          end;
        end
        else                        // ---- dynamic Huffman tables
        begin
          HLIT := Integer(BitRead(BR, 5)) + 257;
          HDIST := Integer(BitRead(BR, 5)) + 1;
          HCLEN := Integer(BitRead(BR, 4)) + 4;
          for I := 0 to 18 do
            CLens[I] := 0;
          for I := 0 to HCLEN - 1 do
            CLens[cClOrder[I]] := Integer(BitRead(BR, 3));
          if BR.Failed then
            Exit(-1);
          BuildHuffTree(CLens, 19, ClTree);

          I := 0;
          PrevLen := 0;
          while I < HLIT + HDIST do
          begin
            Sym := ClTree.Decode(BR);
            if BR.Failed or (Sym < 0) then
              Exit(-1);
            if Sym < 16 then
            begin
              BLens[I] := Sym;
              PrevLen := Sym;
              Inc(I);
            end
            else
            begin
              case Sym of
                16: begin CNT := Integer(BitRead(BR, 2)) + 3; Len := PrevLen; end;
                17: begin CNT := Integer(BitRead(BR, 3)) + 3; Len := 0; end;
                18: begin CNT := Integer(BitRead(BR, 7)) + 11; Len := 0; end;
              else
                Exit(-1);
              end;
              if BR.Failed then
                Exit(-1);
              while (CNT > 0) and (I < HLIT + HDIST) do
              begin
                BLens[I] := Len;
                Inc(I);
                Dec(CNT);
              end;
            end;
          end;
          BuildHuffTree(BLens, HLIT, LitTree);
          BuildHuffTree(Copy(BLens, HLIT, HDIST), HDIST, DistTree);
        end;

        { Symbol loop, shared by fixed and dynamic blocks. }
        while True do
        begin
          Sym := LitTree.Decode(BR);
          if BR.Failed or (Sym < 0) then
            Exit(-1);
          if Sym < 256 then
          begin
            if OutPos >= ADstSize then
              Exit(-1);
            ADst[OutPos] := Byte(Sym);
            Inc(OutPos);
            Continue;
          end;
          if Sym = 256 then
            Break;                   // end of block
          if Sym > 285 then
            Sym := 285;              // 286/287 duplicate length 258
          Len := cLenBase[Sym] + Integer(BitRead(BR, cLenExtra[Sym]));
          if BR.Failed then
            Exit(-1);
          DistSym := DistTree.Decode(BR);
          if BR.Failed or (DistSym < 0) or (DistSym > 29) then
            Exit(-1);
          Dist := cDistBase[DistSym] + Integer(BitRead(BR, cDistExtra[DistSym]));
          if BR.Failed then
            Exit(-1);
          if Dist > OutPos then
            Exit(-1);                // match before the start of the output
          if OutPos + Len > ADstSize then
            Exit(-1);
          for I := 0 to Len - 1 do
          begin
            ADst[OutPos] := ADst[OutPos - Dist];   // byte-by-byte: overlap safe
            Inc(OutPos);
          end;
        end;
      end;
    until BFINAL;
    Result := OutPos;
  finally
    LitTree.Free;
    DistTree.Free;
    ClTree.Free;
  end;
end;

{ Decompress a zlib stream (RFC 1950) into a buffer of exactly AExpected bytes. }
function ZlibDecompressBuf(AData: PByte; ASrcSize, AExpected: Integer): PByte;
var
  N: Integer;
  A: Cardinal;
begin
  Result := nil;
  if (AData = nil) or (ASrcSize < 6) or (AExpected <= 0) then
    Exit;
  { zlib header sanity: deflate + 32K window, no preset dictionary, FCHECK ok }
  if ((AData[0] and $0F) <> 8) or (((Integer(AData[0]) shl 8) or AData[1]) mod 31 <> 0) or
     ((AData[1] and $20) <> 0) then
    Exit;
  Result := GetMem(AExpected);
  N := InflateRaw(AData + 2, ASrcSize - 2, Result, AExpected);
  if N <> AExpected then
  begin
    FreeMem(Result);
    Result := nil;
    Exit;
  end;
  A := (Cardinal(AData[ASrcSize - 4]) shl 24) or (Cardinal(AData[ASrcSize - 3]) shl 16) or
       (Cardinal(AData[ASrcSize - 2]) shl 8) or AData[ASrcSize - 1];
  if A <> Adler32(Result, N) then
  begin
    FreeMem(Result);
    Result := nil;
  end;
end;

{ Little-endian writer helpers }

procedure PutByte(MS: TMemoryStream; V: Byte);
begin
  MS.WriteBuffer(V, 1);
end;

procedure PutWord(MS: TMemoryStream; V: Word);
begin
  MS.WriteBuffer(V, 2);
end;

procedure PutShort(MS: TMemoryStream; V: SmallInt);
begin
  MS.WriteBuffer(V, 2);
end;

procedure PutDWord(MS: TMemoryStream; V: Cardinal);
begin
  MS.WriteBuffer(V, 4);
end;

procedure PutString(MS: TMemoryStream; const S: string);
var
  B: RawByteString;
begin
  B := UTF8Encode(S);
  PutWord(MS, Length(B));
  if Length(B) > 0 then
    MS.WriteBuffer(B[1], Length(B));
end;

procedure PatchDWord(MS: TMemoryStream; APosition: Int64; V: Cardinal);
var
  Save: Int64;
begin
  Save := MS.Position;
  MS.Seek(APosition, soBeginning);
  PutDWord(MS, V);
  MS.Seek(Save, soBeginning);
end;

{ TAseprite }

constructor TAseprite.Create;
begin
  inherited Create;
  FColorDepth := 32;
end;

destructor TAseprite.Destroy;
begin
  Clear;
  inherited;
end;

procedure TAseprite.Clear;
var
  I, J: Integer;
begin
  for I := 0 to Length(FFrames) - 1 do
    for J := 0 to Length(FFrames[I].Cels) - 1 do
      if FFrames[I].Cels[J].Pixels <> nil then
      begin
        FreeMem(FFrames[I].Cels[J].Pixels);
        FFrames[I].Cels[J].Pixels := nil;
      end;
  FFrames := nil;
  FLayers := nil;
  FPalette := nil;
  FData := nil;
  FPos := 0;
  FHeaderFrames := 0;
end;

procedure TAseprite.Fatal(const AMsg: string);
begin
  FLastError := AMsg;
  raise Exception.Create(AMsg);
end;

procedure TAseprite.EnsureAvailable(ACount: Integer);
begin
  if (ACount < 0) or (FPos + ACount > Length(FData)) then
    Fatal('Unexpected end of Aseprite file');
end;

function TAseprite.ReadByte: Byte;
begin
  EnsureAvailable(1);
  Result := FData[FPos];
  Inc(FPos);
end;

function TAseprite.ReadWord: Word;
begin
  EnsureAvailable(2);
  Result := FData[FPos] or (FData[FPos + 1] shl 8);
  Inc(FPos, 2);
end;

function TAseprite.ReadDWord: Cardinal;
begin
  EnsureAvailable(4);
  Result := FData[FPos] or (FData[FPos + 1] shl 8) or
            (FData[FPos + 2] shl 16) or (FData[FPos + 3] shl 24);
  Inc(FPos, 4);
end;

function TAseprite.ReadShort: SmallInt;
begin
  Result := SmallInt(ReadWord);
end;

function TAseprite.ReadString: string;
var
  L: Word;
  S: RawByteString;
begin
  Result := '';
  L := ReadWord;
  if L > 0 then
  begin
    EnsureAvailable(L);
    SetLength(S, L);
    Move(FData[FPos], S[1], L);
    Inc(FPos, L);
    Result := S;   // keep the UTF-8 bytes as-is (names are cosmetic)
  end;
end;

procedure TAseprite.ReadHeader;
var
  Magic: Word;
begin
  if Length(FData) < 128 then
    Fatal('Not an Aseprite file (header too small)');

  FFileSize := ReadDWord;          // 0..3
  Magic := ReadWord;               // 4..5  (0xA5E0)
  if Magic <> cAseMagic then
    Fatal('Not an Aseprite file (bad magic number)');

  FHeaderFrames := ReadWord;       // 6..7
  FWidth := ReadWord;              // 8..9
  FHeight := ReadWord;             // 10..11
  FColorDepth := ReadWord;         // 12..13 (8, 16 or 32)
  FSpriteFlags := ReadDWord;       // 14..17
  FSpeed := ReadWord;              // 18..19 (deprecated, default duration)
  ReadDWord;                       // 20..23 (set to zero)
  ReadDWord;                       // 24..27 (set to zero)
  FTransparentIndex := ReadByte;   // 28
  FPos := FPos + 3;                // 29..31 (ignore)
  ReadWord;                        // 32..33 (number of colors for indexed)
  ReadByte;                        // 34 (pixel width ratio)
  ReadByte;                        // 35 (pixel height ratio)
  ReadShort;                       // 36..37 (grid x)
  ReadShort;                       // 38..39 (grid y)
  ReadWord;                        // 40..41 (grid width)
  ReadWord;                        // 42..43 (grid height)
  FPos := FPos + 84;               // 44..127 (reserved)

  if (FWidth <= 0) or (FHeight <= 0) or (FWidth > cMaxDim) or (FHeight > cMaxDim) then
    Fatal('Unsupported Aseprite dimensions');

  // Default palette (everything invalid/transparent until a palette chunk)
  SetLength(FPalette, 256);
  FillChar(FPalette[0], Length(FPalette) * SizeOf(TAsePaletteEntry), 0);
end;

procedure TAseprite.ReadFrame;
var
  FrameStart, FrameEnd, ChunkStart: Integer;
  BytesInFrame: Cardinal;
  FrameMagic: Word;
  OldCount, NewCount, Count: Cardinal;
  Duration: Word;
  ChunkSize: Cardinal;
  ChunkType: Word;
  F: TAseFrame;
  I: Integer;
begin
  FrameStart := FPos;
  BytesInFrame := ReadDWord;
  FrameMagic := ReadWord;
  if FrameMagic <> cFrameMagic then
    Fatal('Bad frame magic number');

  OldCount := ReadWord;
  Duration := ReadWord;
  FPos := FPos + 2;                // reserved
  NewCount := ReadDWord;

  Count := NewCount;
  if Count = 0 then
    Count := OldCount;
  if Count >= $FFFF then
    Count := NewCount;

  FrameEnd := FrameStart + BytesInFrame;
  if FrameEnd > Length(FData) then
    FrameEnd := Length(FData);
  if FrameEnd < FPos then
    FrameEnd := FPos;

  F := Default(TAseFrame);
  F.Duration := Duration;
  if F.Duration = 0 then
    F.Duration := FSpeed;          // old files only have the header speed
  SetLength(FFrames, Length(FFrames) + 1);
  FFrames[Length(FFrames) - 1] := F;

  for I := 1 to Count do
  begin
    if FPos + 6 > FrameEnd then
      Break;
    ChunkStart := FPos;
    ChunkSize := ReadDWord;
    ChunkType := ReadWord;
    if ChunkStart + ChunkSize > Length(FData) then
      Fatal('Truncated chunk');
    if ChunkSize < 6 then
      ChunkSize := 6;              // tolerate a broken size field

    case ChunkType of
      cChunkLayer:         ReadLayerChunk;
      cChunkCel:           ReadCelChunk(ChunkStart, ChunkSize);
      cChunkPalette:       ReadPaletteChunk(ChunkStart, ChunkSize);
      cChunkOldPalette6:   ReadOldPaletteChunk(ChunkStart, ChunkSize, True);
      cChunkOldPalette8:   ReadOldPaletteChunk(ChunkStart, ChunkSize, False);
    end;

    FPos := ChunkStart + ChunkSize; // always land on the next chunk
  end;

  FPos := FrameEnd;
end;

procedure TAseprite.ReadLayerChunk;
var
  L: TAseLayer;
  Flags: Word;
begin
  L := Default(TAseLayer);
  Flags := ReadWord;
  L.Visible := (Flags and 1) <> 0;
  L.Background := (Flags and 8) <> 0;
  L.Reference := (Flags and 64) <> 0;
  L.LayerType := ReadWord;
  L.ChildLevel := ReadWord;
  ReadWord;                        // default layer width (ignored)
  ReadWord;                        // default layer height (ignored)
  L.BlendMode := ReadWord;
  L.Opacity := ReadByte;
  FPos := FPos + 3;                // reserved
  L.Name := ReadString;

  if L.LayerType = 2 then
    FPos := FPos + 4;              // tileset index (tilemap layers)
  if (FSpriteFlags and 4) <> 0 then
    FPos := FPos + 16;             // layer UUID

  if Length(FLayers) >= cMaxLayers then
    Fatal('Too many layers in Aseprite file');
  SetLength(FLayers, Length(FLayers) + 1);
  FLayers[Length(FLayers) - 1] := L;
end;

procedure TAseprite.ReadCelChunk(AChunkStart, AChunkSize: Integer);
var
  ChunkEnd: Integer;
  Cel: TAseCel;
  W, H, PixelCount, SrcBytes, CompLen: Integer;
  Raw: PByte;
  BPP: Integer;
begin
  ChunkEnd := AChunkStart + AChunkSize;
  Cel := Default(TAseCel);
  Cel.LayerIndex := ReadWord;
  Cel.X := ReadShort;
  Cel.Y := ReadShort;
  Cel.Opacity := ReadByte;
  Cel.CelType := ReadWord;
  Cel.ZIndex := ReadShort;
  FPos := FPos + 5;                // reserved

  BPP := FColorDepth div 8;        // bytes per pixel in the file
  case Cel.CelType of
    0: // Raw image data
      begin
        W := ReadWord;
        H := ReadWord;
        PixelCount := W * H;
        SrcBytes := PixelCount * BPP;
        if (W > 0) and (H > 0) and (W <= cMaxDim) and (H <= cMaxDim) and
           (FPos + SrcBytes <= ChunkEnd) then
        begin
          Cel.Width := W;
          Cel.Height := H;
          Cel.Pixels := GetMem(PixelCount * 4);
          DecodeToRGBA(@FData[FPos], PixelCount, Cel.Pixels, IsBackgroundLayer(Cel.LayerIndex));
          FPos := FPos + SrcBytes;
        end
        else
          FPos := ChunkEnd;
      end;
    1: // Linked cel: pixels come from another frame, same layer
      Cel.LinkFrame := ReadWord;
    2: // Compressed (zlib) image data
      begin
        W := ReadWord;
        H := ReadWord;
        PixelCount := W * H;
        CompLen := ChunkEnd - FPos;
        if (W > 0) and (H > 0) and (W <= cMaxDim) and (H <= cMaxDim) and (CompLen > 0) then
        begin
          Raw := ZlibDecompressBuf(@FData[FPos], CompLen, PixelCount * BPP);
          if Raw <> nil then
          begin
            Cel.Width := W;
            Cel.Height := H;
            Cel.Pixels := GetMem(PixelCount * 4);
            DecodeToRGBA(Raw, PixelCount, Cel.Pixels, IsBackgroundLayer(Cel.LayerIndex));
            FreeMem(Raw);
          end;
        end;
        FPos := ChunkEnd;
      end;
    3: // Compressed tilemap: not rendered
      FPos := ChunkEnd;
    else
      FPos := ChunkEnd;
  end;

  if Cel.CelType <= 2 then
    AddCel(Cel);
end;

procedure TAseprite.ReadPaletteChunk(AChunkStart, AChunkSize: Integer);
var
  ChunkEnd: Integer;
  NewSize, First, Last: Cardinal;
  I, Count: Integer;
  Flags: Word;
  Entry: TAsePaletteEntry;
begin
  ChunkEnd := AChunkStart + AChunkSize;
  NewSize := ReadDWord;
  First := ReadDWord;
  Last := ReadDWord;
  FPos := FPos + 8;                // reserved

  if NewSize > cMaxPaletteEntries then
    NewSize := cMaxPaletteEntries;
  if NewSize > Cardinal(Length(FPalette)) then
    SetLength(FPalette, NewSize);

  if (First <= Last) then
    Count := Integer(Last) - Integer(First) + 1
  else
    Count := 0;

  for I := 0 to Count - 1 do
  begin
    Flags := ReadWord;
    Entry := Default(TAsePaletteEntry);
    Entry.R := ReadByte;
    Entry.G := ReadByte;
    Entry.B := ReadByte;
    Entry.A := ReadByte;
    Entry.Valid := True;
    if Integer(First) + I < Length(FPalette) then
      FPalette[Integer(First) + I] := Entry;
    if (Flags and 1) <> 0 then
      ReadString;                  // color name
  end;
  FPos := ChunkEnd;
end;

procedure TAseprite.ReadOldPaletteChunk(AChunkStart, AChunkSize: Integer; A6Bit: Boolean);
var
  ChunkEnd: Integer;
  Packets: Word;
  I, J, Index: Integer;
  Skip: Byte;
  Count: Integer;
  R, G, B: Integer;
  Entry: TAsePaletteEntry;
begin
  ChunkEnd := AChunkStart + AChunkSize;
  if Length(FPalette) < 256 then
    SetLength(FPalette, 256);
  Packets := ReadWord;
  Index := 0;
  for I := 0 to Packets - 1 do
  begin
    if FPos + 2 > ChunkEnd then
      Break;
    Skip := ReadByte;
    Count := ReadByte;
    if Count = 0 then
      Count := 256;
    Inc(Index, Skip);
    for J := 0 to Count - 1 do
    begin
      if FPos + 3 > ChunkEnd then
        Break;
      R := ReadByte;
      G := ReadByte;
      B := ReadByte;
      if A6Bit then
      begin
        R := R * 255 div 63;
        G := G * 255 div 63;
        B := B * 255 div 63;
      end;
      if Index < Length(FPalette) then
      begin
        Entry := Default(TAsePaletteEntry);
        Entry.R := Byte(R);
        Entry.G := Byte(G);
        Entry.B := Byte(B);
        Entry.A := 255;
        Entry.Valid := True;
        FPalette[Index] := Entry;
      end;
      Inc(Index);
    end;
  end;
  FPos := ChunkEnd;
end;

procedure TAseprite.AddCel(const ACel: TAseCel);
var
  F: Integer;
begin
  F := Length(FFrames) - 1;
  if F < 0 then
    Exit;
  SetLength(FFrames[F].Cels, Length(FFrames[F].Cels) + 1);
  FFrames[F].Cels[Length(FFrames[F].Cels) - 1] := ACel;
end;

procedure TAseprite.ResolveLayerHierarchy;
var
  I, J, Parent: Integer;
begin
  // Layers appear in the file from bottom to top (layer 0 = background).
  // A layer's group parent is the nearest previous layer whose child level
  // is exactly one less, as described in the specification.
  for I := 0 to Length(FLayers) - 1 do
  begin
    Parent := -1;
    for J := I - 1 downto 0 do
      if FLayers[J].ChildLevel = FLayers[I].ChildLevel - 1 then
      begin
        Parent := J;
        Break;
      end;
    if Parent >= 0 then
    begin
      FLayers[I].EffectiveOpacity := Byte(Round(FLayers[I].Opacity * FLayers[Parent].EffectiveOpacity / 255));
      FLayers[I].EffectiveVisible := FLayers[I].Visible and FLayers[Parent].EffectiveVisible;
    end
    else
    begin
      FLayers[I].EffectiveOpacity := FLayers[I].Opacity;
      FLayers[I].EffectiveVisible := FLayers[I].Visible;
    end;
  end;
end;

function TAseprite.IsBackgroundLayer(ALayer: Integer): Boolean;
begin
  Result := (ALayer >= 0) and (ALayer < Length(FLayers)) and FLayers[ALayer].Background;
end;

function TAseprite.GetLayerOpacity(ALayer: Integer): Byte;
begin
  if (ALayer >= 0) and (ALayer < Length(FLayers)) then
    Result := FLayers[ALayer].EffectiveOpacity
  else
    Result := 255;
end;

function TAseprite.GetLayerVisible(ALayer: Integer): Boolean;
begin
  if (ALayer >= 0) and (ALayer < Length(FLayers)) then
    Result := FLayers[ALayer].EffectiveVisible
  else
    Result := True;
end;

procedure TAseprite.DecodeToRGBA(ASrc: PByte; APixelCount: Integer; ADst: PByte; ABackground: Boolean);
var
  I: Integer;
  P, Q: PByte;
  Idx: Byte;
  V: Byte;
begin
  case FColorDepth of
    8: // Indexed: one byte per pixel, look up the palette
      begin
        P := ASrc;
        Q := ADst;
        for I := 0 to APixelCount - 1 do
        begin
          Idx := P^;
          if (not ABackground) and (Idx = FTransparentIndex) then
          begin
            Q^ := 0; Q[1] := 0; Q[2] := 0; Q[3] := 0;
          end
          else if (Idx < Cardinal(Length(FPalette))) and FPalette[Idx].Valid then
          begin
            Q^ := FPalette[Idx].R; Q[1] := FPalette[Idx].G;
            Q[2] := FPalette[Idx].B; Q[3] := FPalette[Idx].A;
          end
          else
          begin
            Q^ := 0; Q[1] := 0; Q[2] := 0; Q[3] := 0;
          end;
          Inc(P);
          Inc(Q, 4);
        end;
      end;
    16: // Grayscale: two bytes per pixel (value, alpha)
      begin
        P := ASrc;
        Q := ADst;
        for I := 0 to APixelCount - 1 do
        begin
          V := P^;
          Q^ := V; Q[1] := V; Q[2] := V;
          Q[3] := P[1];
          Inc(P, 2);
          Inc(Q, 4);
        end;
      end;
    32: // RGBA: four bytes per pixel
      if APixelCount > 0 then
        Move(ASrc^, ADst^, APixelCount * 4);
  end;
end;

function TAseprite.ResolveLinkedCel(AFrame, ALayer: Integer): TAseCel;
var
  I, N, Depth: Integer;
  Found: Boolean;
begin
  Result := Default(TAseCel);
  I := AFrame;
  Depth := 0;
  while (I >= 0) and (I < Length(FFrames)) and (Depth < 64) do
  begin
    Found := False;
    for N := 0 to Length(FFrames[I].Cels) - 1 do
    begin
      if FFrames[I].Cels[N].LayerIndex = ALayer then
      begin
        Found := True;
        if FFrames[I].Cels[N].CelType = 1 then
        begin
          if FFrames[I].Cels[N].LinkFrame = I then
            Exit;                  // cycle guard
          I := FFrames[I].Cels[N].LinkFrame;
          Inc(Depth);
        end
        else
        begin
          Result := FFrames[I].Cels[N];
          Exit;
        end;
        Break;
      end;
    end;
    if not Found then
      Exit;
  end;
end;

procedure TAseprite.BlitCel(ADst: PByte; const ACel: TAseCel; AOpacityFactor: Single);
var
  SX, SY, TW, TH: Integer;
  SrcP, DstP: PByte;
  X, Y: Integer;
  R, G, B, A: Byte;
  Sa, Da, OutA: Single;
begin
  if (ACel.Pixels = nil) or (ACel.Width <= 0) or (ACel.Height <= 0) then
    Exit;

  // Clip the cel against the sprite canvas.  The destination column/row where
  // the first kept source pixel lands is ACel.X + SX (resp. ACel.Y + SY), so
  // the right/bottom clip must use that offset, otherwise a cel that has a
  // negative position AND overflows the canvas would write past the row end.
  SX := 0;
  SY := 0;
  TW := ACel.Width;
  TH := ACel.Height;
  if ACel.X < 0 then
  begin
    SX := -ACel.X;
    Dec(TW, SX);       // drop the source columns left of the canvas
  end;
  if ACel.Y < 0 then
  begin
    SY := -ACel.Y;
    Dec(TH, SY);       // drop the source rows above the canvas
  end;
  if ACel.X + SX + TW > FWidth then
    TW := FWidth - (ACel.X + SX);
  if ACel.Y + SY + TH > FHeight then
    TH := FHeight - (ACel.Y + SY);
  if (TW <= 0) or (TH <= 0) then
    Exit;

  for Y := 0 to TH - 1 do
  begin
    SrcP := ACel.Pixels + ((SY + Y) * ACel.Width + SX) * 4;
    DstP := ADst + ((ACel.Y + Y) * FWidth + (ACel.X + SX)) * 4;
    for X := 0 to TW - 1 do
    begin
      R := SrcP^;
      G := SrcP[1];
      B := SrcP[2];
      A := SrcP[3];
      Sa := (A / 255) * AOpacityFactor;
      if Sa > 0.001 then
      begin
        Da := DstP[3] / 255;
        OutA := Sa + Da * (1 - Sa);
        if OutA <= 0.0001 then
        begin
          DstP^ := 0; DstP[1] := 0; DstP[2] := 0; DstP[3] := 0;
        end
        else
        begin
          DstP^ := Byte(Round((R * Sa + DstP^ * Da * (1 - Sa)) / OutA));
          DstP[1] := Byte(Round((G * Sa + DstP[1] * Da * (1 - Sa)) / OutA));
          DstP[2] := Byte(Round((B * Sa + DstP[2] * Da * (1 - Sa)) / OutA));
          DstP[3] := Byte(Round(OutA * 255));
        end;
      end;
      Inc(SrcP, 4);
      Inc(DstP, 4);
    end;
  end;
end;

function TAseprite.LoadFromFile(const FileName: string): Boolean;
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

function TAseprite.LoadFromStream(AStream: TStream): Boolean;
var
  N: Integer;
begin
  Result := False;
  FLastError := '';
  Clear;
  try
    if AStream.Size > cMaxFileSize then
      Fatal('Aseprite file too large: ' + IntToStr(AStream.Size));
    SetLength(FData, AStream.Size);
    if Length(FData) > 0 then
    begin
      AStream.Position := 0;
      AStream.ReadBuffer(FData[0], Length(FData));
    end;

    ReadHeader;
    N := 0;
    while (N < FHeaderFrames) and (FPos + 16 <= Length(FData)) do
    begin
      ReadFrame;
      Inc(N);
    end;
    ResolveLayerHierarchy;
    Result := Length(FFrames) > 0;
    if not Result then
      FLastError := 'No frames found in Aseprite file';
  except
    on E: Exception do
    begin
      if FLastError = '' then
        FLastError := E.Message;
      Result := False;
    end;
  end;
  // The raw file bytes are no longer needed once the cels are decoded
  FData := nil;
  FPos := 0;
end;

function TAseprite.GetFrameCount: Integer;
begin
  Result := Length(FFrames);
end;

function TAseprite.GetFrameDuration(AFrame: Integer): Integer;
begin
  if (AFrame >= 0) and (AFrame < Length(FFrames)) then
    Result := FFrames[AFrame].Duration
  else
    Result := 0;
end;

function TAseprite.RenderBuffer(AFrame: Integer): PByte;
var
  Dst: PByte;
  List: array of TRenderItem;
  C, I, J: Integer;
  Cel: TAseCel;
  L: Integer;
  Factor: Single;
  Key: TRenderItem;
begin
  Result := nil;
  if (AFrame < 0) or (AFrame >= Length(FFrames)) then
    Exit;
  if (FWidth <= 0) or (FHeight <= 0) then
    Exit;

  Dst := RayLib.MemAlloc(FWidth * FHeight * 4);
  FillChar(Dst^, FWidth * FHeight * 4, 0);

  C := 0;
  for I := 0 to Length(FFrames[AFrame].Cels) - 1 do
  begin
    Cel := FFrames[AFrame].Cels[I];
    L := Cel.LayerIndex;
    if (L < 0) or (L >= Length(FLayers)) then
      Continue;
    if FLayers[L].LayerType = 2 then
      Continue;                    // tilemap layers are not rendered
    if FLayers[L].Reference then
      Continue;                    // reference layers are guides only
    if not GetLayerVisible(L) then
      Continue;
    if Cel.CelType = 1 then        // linked cel: fetch the pixels
    begin
      Cel := ResolveLinkedCel(AFrame, L);
      if Cel.Pixels = nil then
        Continue;
    end;
    if Cel.Pixels = nil then
      Continue;

    // Render order: back to front by (layer index + z-index), then z-index
    SetLength(List, C + 1);
    List[C].Cel := Cel;
    List[C].Order := L + Cel.ZIndex;
    List[C].Z := Cel.ZIndex;
    Inc(C);
  end;

  // Insertion sort (cel lists are small)
  for I := 1 to C - 1 do
  begin
    Key := List[I];
    J := I - 1;
    while (J >= 0) and
          ((List[J].Order > Key.Order) or
           ((List[J].Order = Key.Order) and (List[J].Z > Key.Z))) do
    begin
      List[J + 1] := List[J];
      Dec(J);
    end;
    List[J + 1] := Key;
  end;

  for J := 0 to C - 1 do
  begin
    Factor := (List[J].Cel.Opacity / 255) * (GetLayerOpacity(List[J].Cel.LayerIndex) / 255);
    if Factor < 0.003 then
      Continue;
    BlitCel(Dst, List[J].Cel, Factor);
  end;

  Result := Dst;
end;

function TAseprite.RenderImage(AFrame: Integer): TImage;
begin
  Result := Default(TImage);
  Result.Data := RenderBuffer(AFrame);
  if Result.Data = nil then
    Exit;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.Mipmaps := 1;
  Result.Format := Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8);
end;

function TAseprite.RenderTexture(AFrame: Integer): TTexture2D;
var
  Img: TImage;
begin
  Result := Default(TTexture2D);
  Img := RenderImage(AFrame);
  if Img.Data = nil then
    Exit;
  Result := RayLib.LoadTextureFromImage(Img);
  RayLib.UnloadImage(Img);
end;

{ Convenience helpers }

function IsAsepriteFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.ase') or (Ext = '.aseprite');
end;

function AsepriteLoadImage(const FileName: string; AFrame: Integer): TImage;
var
  Ase: TAseprite;
begin
  Result := Default(TImage);
  Ase := TAseprite.Create;
  try
    if Ase.LoadFromFile(FileName) then
      Result := Ase.RenderImage(AFrame);
  finally
    Ase.Free;
  end;
end;

function AsepriteLoadTexture(const FileName: string; AFrame: Integer): TTexture2D;
var
  Ase: TAseprite;
begin
  Result := Default(TTexture2D);
  Ase := TAseprite.Create;
  try
    if Ase.LoadFromFile(FileName) then
      Result := Ase.RenderTexture(AFrame);
  finally
    Ase.Free;
  end;
end;

function AsepriteLoadSheetTexture(const FileName: string; out AFrameCount: Integer; out AFrameWidth, AFrameHeight: Integer): TTexture2D;
var
  Ase: TAseprite;
  I, Y: Integer;
  Img: TImage;
  Buf: PByte;
  TotalPixels: Int64;
begin
  Result := Default(TTexture2D);
  AFrameCount := 0;
  AFrameWidth := 0;
  AFrameHeight := 0;
  Ase := TAseprite.Create;
  try
    if not Ase.LoadFromFile(FileName) then
      Exit;
    AFrameCount := Ase.GetFrameCount;
    if (AFrameCount <= 0) or (Ase.Width <= 0) or (Ase.Height <= 0) then
      Exit;
    //Compositing every frame side-by-side multiplies the dimensions; do the
    //math in Int64 and reject files that would exceed sane sheet limits before
    //the Integer width can overflow or MemAlloc can fail silently.
    TotalPixels := Int64(Ase.Width) * AFrameCount * Ase.Height;
    if (AFrameCount > cMaxSheetFrames) or (TotalPixels > cMaxSheetPixels) then
      Exit;

    Img := Default(TImage);
    Img.Width := Integer(TotalPixels div Ase.Height);
    Img.Height := Ase.Height;
    Img.Mipmaps := 1;
    Img.Format := Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8);
    Img.Data := RayLib.MemAlloc(NativeUInt(TotalPixels) * 4);
    if Img.Data = nil then
      Exit; //allocation failed: return "not loaded" instead of crashing later
    FillChar(Img.Data^, NativeUInt(TotalPixels) * 4, 0);

    //Only claim the frame layout once the texture buffer actually exists.
    AFrameWidth := Ase.Width;
    AFrameHeight := Ase.Height;

    for I := 0 to AFrameCount - 1 do
    begin
      Buf := Ase.RenderBuffer(I);
      if Buf = nil then
        Continue;
      for Y := 0 to Ase.Height - 1 do
        Move((Buf + Y * Ase.Width * 4)^,
             (PByte(Img.Data) + Y * Img.Width * 4 + I * Ase.Width * 4)^,
             Ase.Width * 4);
      RayLib.MemFree(Buf);
    end;

    Result := RayLib.LoadTextureFromImage(Img);
    RayLib.UnloadImage(Img);
  finally
    Ase.Free;
  end;
end;

function AsepriteLoadFrameTextures(const FileName: string; out ATextures: TAseTextures; out ADurations: TAseFrameDurations): Integer;
var
  Ase: TAseprite;
  I: Integer;
begin
  Result := 0;
  ATextures := nil;
  ADurations := nil;
  Ase := TAseprite.Create;
  try
    if not Ase.LoadFromFile(FileName) then
      Exit;
    Result := Ase.GetFrameCount;
    if Result <= 0 then
      Exit;
    SetLength(ATextures, Result);
    SetLength(ADurations, Result);
    for I := 0 to Result - 1 do
    begin
      ATextures[I] := Ase.RenderTexture(I);
      ADurations[I] := Ase.GetFrameDuration(I);
      if ADurations[I] < 1 then
        ADurations[I] := 100;
      if ATextures[I].id <= 0 then
        Exit; // partial failure: caller releases whatever was uploaded
    end;
  finally
    Ase.Free;
  end;
end;

procedure AsepriteFreeTextures(var ATextures: TAseTextures);
var
  I: Integer;
  Unloaded: TList<Cardinal>;
begin
  if ATextures = nil then
    Exit;
  Unloaded := TList<Cardinal>.Create;
  try
    for I := 0 to Length(ATextures) - 1 do
      if (ATextures[I].id > 0) and (Unloaded.IndexOf(ATextures[I].id) < 0) then
      begin
        Unloaded.Add(ATextures[I].id);
        RayLib.UnloadTexture(ATextures[I]);
      end;
  finally
    Unloaded.Free;
  end;
  ATextures := nil;
end;

function AsepriteSaveRGBA(const FileName: string; AWidth, AHeight: Integer; APixels: PByte; ADuration: Integer; const ALayerName: string): Boolean;
var
  Compressed: TBytes;
  MS: TMemoryStream;
  FramePos: Int64;
  LayerStart, CelStart: Int64;
  I: Integer;
  Size: Int64;
begin
  Result := False;
  if (AWidth <= 0) or (AHeight <= 0) or (AWidth > cMaxDim) or (AHeight > cMaxDim) or (APixels = nil) then
    Exit;

  Compressed := ZlibCompressBuf(APixels, AWidth * AHeight * 4);
  if Compressed = nil then
    Exit;

  MS := TMemoryStream.Create;
  try
    // ---- 128-byte file header
    PutDWord(MS, 0);               // file size, patched at the end
    PutWord(MS, cAseMagic);
    PutWord(MS, 1);                // frames
    PutWord(MS, AWidth);
    PutWord(MS, AHeight);
    PutWord(MS, 32);               // RGBA
    PutDWord(MS, 1);               // flags: layer opacity has a valid value
    PutWord(MS, ADuration);        // speed (default duration, deprecated)
    PutDWord(MS, 0);
    PutDWord(MS, 0);
    PutByte(MS, 0);                // transparent palette entry (indexed only)
    for I := 0 to 2 do
      PutByte(MS, 0);              // ignore
    PutWord(MS, 0);                // number of colors (indexed only)
    PutByte(MS, 1);                // pixel width
    PutByte(MS, 1);                // pixel height
    PutShort(MS, 0);               // grid x
    PutShort(MS, 0);               // grid y
    PutWord(MS, 0);                // grid width
    PutWord(MS, 0);                // grid height
    for I := 0 to 83 do
      PutByte(MS, 0);              // reserved

    // ---- frame header
    FramePos := MS.Position;
    PutDWord(MS, 0);               // bytes in frame, patched later
    PutWord(MS, cFrameMagic);
    PutWord(MS, 2);                // old chunk count
    PutWord(MS, ADuration);        // frame duration
    PutByte(MS, 0);
    PutByte(MS, 0);
    PutDWord(MS, 0);               // new chunk count (0 -> use the old field)

    // ---- layer chunk (0x2004)
    LayerStart := MS.Position;
    PutDWord(MS, 0);               // chunk size, patched later
    PutWord(MS, cChunkLayer);
    PutWord(MS, 1);                // flags: visible
    PutWord(MS, 0);                // type: normal image layer
    PutWord(MS, 0);                // child level
    PutWord(MS, 0);                // default layer width (ignored)
    PutWord(MS, 0);                // default layer height (ignored)
    PutWord(MS, 0);                // blend mode: normal
    PutByte(MS, 255);              // opacity
    for I := 0 to 2 do
      PutByte(MS, 0);              // reserved
    PutString(MS, ALayerName);
    PatchDWord(MS, LayerStart, MS.Position - LayerStart);

    // ---- cel chunk (0x2005), compressed image
    CelStart := MS.Position;
    PutDWord(MS, 0);               // chunk size, patched later
    PutWord(MS, cChunkCel);
    PutWord(MS, 0);                // layer index
    PutShort(MS, 0);               // x
    PutShort(MS, 0);               // y
    PutByte(MS, 255);              // opacity
    PutWord(MS, 2);                // cel type: compressed image
    PutShort(MS, 0);               // z-index
    for I := 0 to 4 do
      PutByte(MS, 0);              // reserved
    PutWord(MS, AWidth);
    PutWord(MS, AHeight);
    MS.WriteBuffer(Compressed[0], Length(Compressed));
    PatchDWord(MS, CelStart, MS.Position - CelStart);

    // ---- patch frame size and file size
    Size := MS.Size - FramePos;
    PatchDWord(MS, FramePos, Size);
    PatchDWord(MS, 0, MS.Size);

    MS.SaveToFile(FileName);
    Result := True;
  finally
    MS.Free;
  end;
end;

end.