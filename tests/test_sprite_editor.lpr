program test_sprite_editor;

{ Headless test for the sprite editor document layer
  (src\tyrolib\TyroSpriteEditor.pas).

  No window is opened: LoadFile/SaveFile only touch CPU images, the .sprite
  writer and the JSON descriptor, so the whole document round trip can be
  checked without a GL context.

  Covered here:
    - .png load/save round trip keeps every pixel
    - .sprite load/save keeps frames, per-frame durations and pixels
    - the editor's actions, loop flags and palette survive a save, because they
      live in the descriptor's meta.tyro object
    - a missing file is reported as a failure instead of crashing

  Build and run:
    lazbuild --build-mode=Debug tests\test_sprite_editor.lpi
    bin\test_sprite_editor.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  RayLib,
  TyroClasses, TyroSpriteEditor, SpriteAtlas;

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
  Result := Cardinal(R) or (Cardinal(G) shl 8) or (Cardinal(B) shl 16) or
    (Cardinal(A) shl 24);
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
  Result := Cardinal(P^) or (Cardinal(P[1]) shl 8) or
    (Cardinal(P[2]) shl 16) or (Cardinal(P[3]) shl 24);
end;

function MakeImage(W, H: Integer; C: Cardinal): TImage;
var
  X, Y: Integer;
begin
  Result := GenImageColor(W, H, clBlank);
  ImageFormat(Result, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      SetPixel(Result, X, Y, C);
end;

function LoadImageFile(const AFileName: string): TImage;
begin
  Result := RayLib.LoadImage(PUTF8Char(UTF8String(AFileName)));
  if Result.Data <> nil then
    ImageFormat(Result, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
end;

function TempPath(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + AName;
end;

var
  Editor: TSpriteEditor;
  Img, Loaded: TImage;
  W: array of TSpriteAtlasWriteFrame;
  Atlas: TSpriteAtlas;
  PngFile, SpriteFile: string;
  X, Y: Integer;
  Same: Boolean;
  Meta: string;

begin
  Tests := 0;
  Failures := 0;
  InitLibrary;   // resolve the raylib entry points (no window is opened)
  Res := TTyroResources.Create;
  WriteLn('test_sprite_editor');

  PngFile := TempPath('tmp_sprite_editor.png');
  SpriteFile := TempPath('tmp_sprite_editor.sprite');

  // --------------------------------------------------------------------
  // 1) .png round trip
  // --------------------------------------------------------------------
  Img := MakeImage(8, 8, RGBA(0, 0, 0, 0));
  for Y := 0 to 7 do
    for X := 0 to 7 do
      SetPixel(Img, X, Y, RGBA(X * 32, Y * 32, 128, 255));
  RayLib.ExportImage(Img, PUTF8Char(UTF8String(PngFile)));
  UnloadImage(Img);
  Check('.png test file written', SysUtils.FileExists(PngFile));

  Editor := TSpriteEditor.Create(nil);
  try
    Check('.png load', Editor.LoadFile(PngFile), Editor.FileName);
    Check('.png load keeps the file name', SameFileName(Editor.FileName, PngFile));
    Check('.png save', Editor.SaveFile());

    Loaded := LoadImageFile(PngFile);
    Check('.png saved image reloads', Loaded.Data <> nil);
    Same := True;
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if PixelAt(Loaded, X, Y) <> RGBA(X * 32, Y * 32, 128, 255) then
          Same := False;
    Check('.png round trip keeps every pixel', Same);
    if Loaded.Data <> nil then
      UnloadImage(Loaded);

    Check('missing file is a failure', not Editor.LoadFile(TempPath('nope_does_not_exist.png')));
  finally
    Editor.Free;
  end;

  // --------------------------------------------------------------------
  // 2) .sprite round trip, including the editor metadata
  // --------------------------------------------------------------------
  SetLength(W, 2);
  W[0].Name := 'frame_000';
  W[0].Image := MakeImage(4, 4, RGBA(255, 0, 0, 255));
  W[0].DurationMs := 60;
  W[1].Name := 'frame_001';
  W[1].Image := MakeImage(4, 4, RGBA(0, 0, 255, 255));
  W[1].DurationMs := 120;
  Meta := '"tyro":{"version":1,"width":4,"height":4,' +
    '"front":"FFFFFFFF","back":"00000000",' +
    '"palette":["FF0000FF","00FF00FF"],' +
    '"actions":[{"name":"idle","loop":true,"frames":[0]},' +
    '{"name":"walking","loop":false,"frames":[0,1]}]}';
  Check('.sprite test file written', SaveSpriteAtlas(SpriteFile, W, 'sheet.png', Meta));
  UnloadImage(W[0].Image);
  UnloadImage(W[1].Image);

  Editor := TSpriteEditor.Create(nil);
  try
    Check('.sprite load', Editor.LoadFile(SpriteFile), Editor.FileName);
    Check('.sprite save', Editor.SaveFile());
  finally
    Editor.Free;
  end;

  Atlas := TSpriteAtlas.Create;
  try
    Check('.sprite reloads', Atlas.LoadFromFile(SpriteFile), Atlas.LastError);
    Check('.sprite keeps both frames', Atlas.GetFrameCount = 2,
      IntToStr(Atlas.GetFrameCount));
    Check('.sprite keeps frame 0 duration', Atlas.GetFrameDuration(0) = 60,
      IntToStr(Atlas.GetFrameDuration(0)));
    Check('.sprite keeps frame 1 duration', Atlas.GetFrameDuration(1) = 120,
      IntToStr(Atlas.GetFrameDuration(1)));

    Img := Atlas.GetFrameImage(0);
    Check('.sprite frame 0 is red', PixelAt(Img, 1, 1) = RGBA(255, 0, 0, 255));
    UnloadImage(Img);
    Img := Atlas.GetFrameImage(1);
    Check('.sprite frame 1 is blue', PixelAt(Img, 1, 1) = RGBA(0, 0, 255, 255));
    UnloadImage(Img);

    Check('.sprite meta keeps action "idle"', Pos('"idle"', Atlas.Descriptor) > 0);
    Check('.sprite meta keeps action "walking"', Pos('"walking"', Atlas.Descriptor) > 0);
    Check('.sprite meta keeps the loop flag', Pos('"loop":false', Atlas.Descriptor) > 0);
    Check('.sprite meta keeps the palette', Pos('"palette"', Atlas.Descriptor) > 0);
    Check('.sprite meta keeps the canvas size', Pos('"width":4', Atlas.Descriptor) > 0);
  finally
    Atlas.Free;
  end;

  if SysUtils.FileExists(PngFile) then
    SysUtils.DeleteFile(PngFile);
  if SysUtils.FileExists(SpriteFile) then
    SysUtils.DeleteFile(SpriteFile);

  WriteLn;
  WriteLn(Format('%d checks, %d failure(s)', [Tests, Failures]));
  if Failures = 0 then
    Halt(0)
  else
    Halt(Failures);
end.
