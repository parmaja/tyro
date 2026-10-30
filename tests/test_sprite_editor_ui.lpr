program test_sprite_editor_ui;

{ Visual and interaction smoke test for the sprite editor
  (src\tyrolib\TyroSpriteEditor.pas).

  It opens a real window, paints the editor with a small generated .sprite and
  saves what the frame looked like to bin\sprite_editor_ui.png, so the layout,
  the panels, the palette, the toolbar icons and the animated preview can be
  eyeballed. A second screenshot shows another tool and another frame.

  It then drives the editor with synthetic mouse clicks and keys:

    * pixel tool + left click paints the front color and Ctrl+S writes it,
    * a palette click changes the front color used by the next click,
    * Ctrl+Z undoes both strokes and Ctrl+S writes the original pixels back,
    * the animation names, durations and canvas size survive every save.

  Build and run:
    lazbuild --build-mode=Debug tests\test_sprite_editor_ui.lpi
    bin\test_sprite_editor_ui.exe

  Exit status: 0 when every check passed. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  RayLib,
  TyroClasses, TyroControls, TyroSpriteEditor, SpriteAtlas;

const
  cWidth = 1000;
  cHeight = 680;

type
  { PaintWindow and the layout queries are protected; this only makes them
    callable from the test. }
  TTestSpriteEditor = class(TSpriteEditor)
  public
    procedure PaintTo(ACanvas: TTyroCanvas);
    //* Client point -> window point, the same mapping the engine uses when it
    //* routes mouse events to a control.
    function ToWindow(AX, AY: Integer): TPoint;
    //* Centre of frame pixel (X, Y) in window coordinates.
    function PixelPoint(X, Y: Integer): TPoint;
    //* Centre of a tool button / palette swatch in window coordinates.
    function ToolPoint(AIndex: Integer): TPoint;
    function SwatchPoint(AIndex: Integer): TPoint;
  end;

procedure TTestSpriteEditor.PaintTo(ACanvas: TTyroCanvas);
begin
  PaintWindow(ACanvas);
end;

function TTestSpriteEditor.ToWindow(AX, AY: Integer): TPoint;
begin
  //* Inverse of MapToClient: client -> window (the editor sits at 0, 0).
  Result := Point(AX + InnerRect.Left, AY + InnerRect.Top);
end;

function TTestSpriteEditor.PixelPoint(X, Y: Integer): TPoint;
var
  R: TRect;
  Z: Integer;
begin
  GetImageScreenRect(R);
  Z := Round(Zoom);
  Result := ToWindow(R.Left + X * Z + Z div 2, R.Top + Y * Z + Z div 2);
end;

function TTestSpriteEditor.ToolPoint(AIndex: Integer): TPoint;
var
  R: TRect;
begin
  R := ToolRect(AIndex);
  Result := ToWindow((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
end;

function TTestSpriteEditor.SwatchPoint(AIndex: Integer): TPoint;
var
  R: TRect;
begin
  R := PaletteSwatchRect(AIndex);
  Result := ToWindow((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
end;

var
  Checks, Failures: Integer;

procedure Check(ACondition: Boolean; const AName: string);
begin
  Inc(Checks);
  if ACondition then
    WriteLn('PASS ', AName)
  else
  begin
    Inc(Failures);
    WriteLn('FAIL ', AName);
  end;
end;

function MakeFrame(W, H: Integer; ABody, AAccent: TColor): TImage;
var
  Img: TImage;
begin
  Img := GenImageColor(W, H, clBlank);
  ImageFormat(Img, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
  ImageDrawRectangle(@Img, 6, 4, W - 12, H - 10, ABody);
  ImageDrawRectangle(@Img, 9, 8, W - 18, 6, AAccent);
  ImageDrawRectangle(@Img, 9, H - 14, W - 18, 5, AAccent);
  ImageDrawRectangle(@Img, 12, 16, 4, 4, clWhite);
  ImageDrawRectangle(@Img, W - 16, 16, 4, 4, clWhite);
  Result := Img;
end;

function TempPath(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + AName;
end;

{ Returns the pixel as "R,G,B,A" so failures print something readable. }
function PixelText(const Img: TImage; X, Y: Integer): string;
var
  P: PByte;
begin
  if (Img.Data = nil) or (X < 0) or (Y < 0) or (X >= Img.Width) or
    (Y >= Img.Height) then
    Exit('out of range');
  P := PByte(Img.Data) + (Y * Img.Width + X) * 4;
  Result := Format('%d,%d,%d,%d', [P^, P[1], P[2], P[3]]);
end;

function PixelIs(const Img: TImage; X, Y: Integer;
  R, G, B, A: Byte): Boolean;
var
  P: PByte;
begin
  Result := False;
  if (Img.Data = nil) or (X < 0) or (Y < 0) or (X >= Img.Width) or
    (Y >= Img.Height) then
    Exit;
  P := PByte(Img.Data) + (Y * Img.Width + X) * 4;
  Result := (P^ = R) and (P[1] = G) and (P[2] = B) and (P[3] = A);
end;

{ Decodes one frame of the .sprite on disk; caller must UnloadImage it. }
function LoadFrameImage(const AFileName: string; AFrame: Integer): TImage;
var
  A: TSpriteAtlas;
begin
  Result := Default(TImage);
  A := TSpriteAtlas.Create;
  try
    if A.LoadFromFile(AFileName) then
      Result := A.GetFrameImage(AFrame);
  finally
    A.Free;
  end;
end;

procedure SaveScreenShot(const ACanvas: TTyroCanvas; AEditor: TTestSpriteEditor;
  const AFileName: string);
var
  Shot: TImage;
begin
  BeginDrawing;
  ClearBackground(clBlack);
  AEditor.PaintTo(ACanvas);
  Shot := LoadImageFromScreen;
  EndDrawing;
  RayLib.ExportImage(Shot, PUTF8Char(UTF8String(AFileName)));
  UnloadImage(Shot);
end;

var
  Canvas: TTyroTextureCanvas;
  Editor: TTestSpriteEditor;
  W: array of TSpriteAtlasWriteFrame;
  Atlas: TSpriteAtlas;
  AtlasFile, ShotFile, UntitledFile: string;
  Shot: TImage;
  I: Integer;
  K: TKeyboardKey;
  P: TPoint;
  R: TRect;
  Meta: string;

begin
  Checks := 0;
  Failures := 0;

  AtlasFile := TempPath('tmp_sprite_editor_ui.sprite');
  ShotFile := TempPath('sprite_editor_ui.png');
  UntitledFile := TempPath('tmp_untitled.sprite');

  InitLibrary;

  SetLength(W, 4);
  for I := 0 to 3 do
  begin
    W[I].Name := Format('frame_%.3d', [I]);
    W[I].Image := MakeFrame(32, 32,
      TColor.Create(90 + I * 30, 60 + I * 20, 140 - I * 20, 255),
      TColor.Create(255, 210 - I * 20, 90 + I * 20, 255));
    W[I].DurationMs := 120;
  end;
  Meta := '"tyro":{"version":1,"width":32,"height":32,' +
    '"front":"FFFFFFFF","back":"00000000",' +
    '"palette":["1A1C2CFF","5D275DFF","B13E53FF","EF7D57FF","CD75FFFF",' +
    '"A7F070FF","38B764FF","257179FF","29366FFF","3B5DC9FF","41A6F6FF",' +
    '"73EFF7FF","F4F4F4FF","94B0C2FF","566C86FF","333C57FF"],' +
    '"actions":[{"name":"idle","loop":true,"frames":[0,1]},' +
    '{"name":"walking","loop":true,"frames":[0,1,2,3]},' +
    '{"name":"attack","loop":false,"frames":[2,3]}]}';
  if not SaveSpriteAtlas(AtlasFile, W, 'sheet.png', Meta) then
  begin
    WriteLn('FAIL could not write the sample .sprite');
    Halt(1);
  end;
  for I := 0 to High(W) do
    UnloadImage(W[I].Image);

  SetTraceLogLevel([LOG_WARNING]);
  InitWindow(cWidth, cHeight, 'Tyro sprite editor');
  SetTargetFPS(30);
  try
    Res := TTyroResources.Create;
    Res.Load;

    Canvas := TTyroTextureCanvas.Create(cWidth, cHeight, False);
    Editor := TTestSpriteEditor.Create(nil);
    try
      Editor.BoundsRect := Rect(0, 0, cWidth, cHeight);
      //Same frame the engine gives it: margin + thick border + padding around
      //the client area. Mouse events arrive relative to the outer rect, so the
      //tests below would land on the wrong tool/pixel if that offset was not
      //removed.
      Editor.Margin := 10;
      Editor.Border := brdThick;
      Editor.Padding := 5;
      Editor.Show;

      Check(Editor.LoadFile(AtlasFile), 'loads a .sprite file');
      Editor.Update;

      //--- the two layout screenshots -------------------------------------
      SaveScreenShot(Canvas, Editor, ShotFile);
      Check(SysUtils.FileExists(ShotFile), 'writes the first screenshot');

      K := KEY_THREE;
      Editor.KeyDown(K, []);
      K := KEY_RIGHT;
      Editor.KeyDown(K, []);
      SaveScreenShot(Canvas, Editor,
        TempPath('sprite_editor_ui_tool.png'));
      Check(SysUtils.FileExists(TempPath('sprite_editor_ui_tool.png')),
        'writes the second screenshot');

      //--- every tool button selects its own tool at its real position -----
      P := Editor.MapToClient(Point(Editor.InnerRect.Left, Editor.InnerRect.Top));
      Check((P.X = 0) and (P.Y = 0),
        'MapToClient maps the inner corner to the client origin');
      R := Editor.MapToClient(Editor.InnerRect);
      Check((R.Left = 0) and (R.Top = 0) and
        (R.Right = Editor.ClientRect.Width) and
        (R.Bottom = Editor.ClientRect.Height),
        'MapToClient maps the inner rect to the client rect');

      for I := 0 to Ord(High(TSpriteTool)) do
      begin
        P := Editor.ToolPoint(I);
        Editor.MouseDown(mbLeft, [], P.X, P.Y);
        Check(Editor.Tool = TSpriteTool(I),
          Format('tool button %d selects its tool', [I]));
      end;

      //--- paint pixel (8, 8) with the pixel tool and save -----------------
      K := KEY_LEFT;
      Editor.KeyDown(K, []);
      K := KEY_TWO;
      Editor.KeyDown(K, []);
      P := Editor.PixelPoint(8, 8);
      Editor.MouseDown(mbLeft, [], P.X, P.Y);
      K := KEY_S;
      Editor.KeyDown(K, [ssCtrl]);

      Shot := LoadFrameImage(AtlasFile, 0);
      try
        WriteLn('  painted pixel  : ', PixelText(Shot, 8, 8));
        Check(PixelIs(Shot, 8, 8, 255, 255, 255, 255),
          'left click painted the front color on the clicked pixel');
        Check(PixelIs(Shot, 1, 1, 0, 0, 0, 0),
          'left click did not touch the rest of the frame');
      finally
        UnloadImage(Shot);
      end;

      Shot := LoadFrameImage(AtlasFile, 1);
      try
        Check(PixelIs(Shot, 8, 8, 120, 80, 120, 255),
          'the other frame was left alone');
      finally
        UnloadImage(Shot);
      end;

      //--- pick the first palette color and paint pixel (10, 10) ----------
      P := Editor.SwatchPoint(0);
      Editor.MouseDown(mbLeft, [], P.X, P.Y);
      P := Editor.PixelPoint(10, 10);
      Editor.MouseDown(mbLeft, [], P.X, P.Y);
      K := KEY_S;
      Editor.KeyDown(K, [ssCtrl]);

      Shot := LoadFrameImage(AtlasFile, 0);
      try
        WriteLn('  palette pixel  : ', PixelText(Shot, 10, 10));
        Check(PixelIs(Shot, 10, 10, 26, 28, 44, 255),
          'a palette click sets the front color used by the next click');
        Check(PixelIs(Shot, 8, 8, 255, 255, 255, 255),
          'the earlier stroke is still there');
      finally
        UnloadImage(Shot);
      end;

      //--- undo both strokes and save the original pixels -----------------
      K := KEY_Z;
      Editor.KeyDown(K, [ssCtrl]);
      Editor.KeyDown(K, [ssCtrl]);
      K := KEY_S;
      Editor.KeyDown(K, [ssCtrl]);

      Shot := LoadFrameImage(AtlasFile, 0);
      try
        WriteLn('  undone pixel 8 : ', PixelText(Shot, 8, 8));
        WriteLn('  undone pixel 10: ', PixelText(Shot, 10, 10));
        Check(PixelIs(Shot, 8, 8, 90, 60, 140, 255),
          'Ctrl+Z undid the first stroke');
        Check(PixelIs(Shot, 10, 10, 255, 210, 90, 255),
          'Ctrl+Z undid the second stroke');
      finally
        UnloadImage(Shot);
      end;

      //--- duplicate the active frame, save, then delete it again ---------
      K := KEY_D;
      Editor.KeyDown(K, [ssCtrl]);
      K := KEY_S;
      Editor.KeyDown(K, [ssCtrl]);

      Shot := LoadFrameImage(AtlasFile, 4);
      try
        Check(PixelIs(Shot, 8, 8, 90, 60, 140, 255),
          'Ctrl+D duplicated the active frame at the end of the atlas');
      finally
        UnloadImage(Shot);
      end;

      K := KEY_DELETE;
      Editor.KeyDown(K, []);
      K := KEY_S;
      Editor.KeyDown(K, [ssCtrl]);

      Atlas := TSpriteAtlas.Create;
      try
        Check(Atlas.LoadFromFile(AtlasFile), 'reloads after deleting a frame');
        Check(Atlas.GetFrameCount = 4, 'Delete removed the duplicated frame');
      finally
        Atlas.Free;
      end;

      //--- a brand new document with a suggested save target ---------------
      Editor.NewDocument(16, 16, UntitledFile);
      Check(Editor.FileName = UntitledFile, 'a new document keeps its suggested name');
      K := KEY_S;
      Editor.KeyDown(K, [ssCtrl]);

      Check(SysUtils.FileExists(UntitledFile),
        'Ctrl+S writes a new document to its suggested name');

      Shot := LoadFrameImage(UntitledFile, 0);
      try
        Check((Shot.Width = 16) and (Shot.Height = 16),
          'the new document was saved at its own size');
      finally
        UnloadImage(Shot);
      end;
    finally
      Editor.Free;
      Canvas.Free;
    end;

    //--- the saved document itself ----------------------------------------
    Atlas := TSpriteAtlas.Create;
    try
      Check(Atlas.LoadFromFile(AtlasFile), 'reloads the saved .sprite');
      Check(Atlas.GetFrameCount = 4, 'saved file still has 4 frames');
      Check(Atlas.GetFrameDuration(0) = 120, 'frame duration survived the save');
      Check(Pos('"walking"', Atlas.Descriptor) > 0,
        'animation names survived the save');
      Check(Pos('"meta"', Atlas.Descriptor) > 0,
        'editor metadata survived the save');
    finally
      Atlas.Free;
    end;
  finally
    CloseWindow;
  end;

  if SysUtils.FileExists(AtlasFile) then
    SysUtils.DeleteFile(AtlasFile);
  if SysUtils.FileExists(UntitledFile) then
    SysUtils.DeleteFile(UntitledFile);

  WriteLn;
  WriteLn(Format('%d checks, %d failures', [Checks, Failures]));
  if Failures = 0 then
  begin
    WriteLn('OK');
    Halt(0);
  end;
  WriteLn('FAILED');
  Halt(1);
end.
