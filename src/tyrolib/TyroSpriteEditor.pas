unit TyroSpriteEditor;
{$ifdef FPC}
{$MODE DELPHI}
{$endif}
{$H+}{$M+}
{**
 *  This file is part of the "Tyro"
 *
 *  @license   MIT
 *
 *  @author    Zaher Dirkey
 *
 *  Full sprite editor used by the console command:
 *
 *    edit player.sprite
 *    edit player.png
 *    sprite player.sprite
 *
 *  .png is edited as a single frame. .sprite keeps the existing ZIP atlas
 *  format and stores the editor's actions, durations and palette inside the
 *  descriptor's meta.tyro object, so the runtime keeps reading the same file.
 *}

interface

uses
  Classes, SysUtils, Types, Math, StrUtils, Generics.Collections,
  mnDON,
  RayLib, RayClasses,
  TyroClasses, TyroControls, SpriteAtlas;

const
  cSpriteEditorVersion = 1;
  cSpriteEditorMaxUndo = 40;
  cSpriteEditorDefaultDuration = 100;
  cSpriteEditorTopButtons = 8;

  cSpriteEditorToolbarH = 42;
  cSpriteEditorToolsW = 52;
  cSpriteEditorRightW = 232;
  cSpriteEditorPaletteH = 86;

  cSpriteEditorBg: Cardinal = $15171FFF;
  cSpriteEditorPanel: Cardinal = $1E2130FF;
  cSpriteEditorPanelHi: Cardinal = $262A3DFF;
  cSpriteEditorLine: Cardinal = $343A52FF;
  cSpriteEditorText: Cardinal = $E6E9F2FF;
  cSpriteEditorDim: Cardinal = $8C93AAFF;
  cSpriteEditorAccent: Cardinal = $CD75FFFF;
  cSpriteEditorAccent2: Cardinal = $41A6F6FF;
  cSpriteEditorGood: Cardinal = $A7F070FF;
  cSpriteEditorBad: Cardinal = $EF7D57FF;

type
  TSpriteTool = (stSelect, stPixel, stPen, stFill, stPick, stCircle, stRect, stRotate);
  TSpriteDragMode = (dmNone, dmPaint, dmShape, dmSelect, dmPan);

  TSpriteIcon = (icNone, icSave, icUndo, icRedo, icMirrorH, icMirrorV,
    icRotateCW, icRotateCCW, icGrid, icPlus, icMinus, icClose, icPlay,
    icLoop, icAction, icFrame, icProps, icPalette, icTools);

  { One global frame. Actions reference these by index. }
  TSpriteFrame = class(TObject)
  public
    Image: TImage;
    DurationMs: Integer;
    Name: string;
    constructor Create(AWidth, AHeight: Integer); overload;
    constructor Create(const ASource: TImage); overload;
    destructor Destroy; override;
  end;

  { A named animation made of global frame indices. }
  TSpriteAction = class(TObject)
  public
    Name: string;
    Frames: TList<Integer>;
    Loop: Boolean;
    constructor Create(const AName: string);
    destructor Destroy; override;
    procedure AddFrame(AIndex: Integer);
  end;

  { Whole-document undo entry. Frame images are deep copies. }
  TSpriteSnapshot = class(TObject)
  public
    Images: array of TImage;
    Durations: array of Integer;
    ActionNames: TStringList;
    ActionFrames: TStringList;
    ActiveAction: Integer;
    ActiveFrame: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  { The editor control. It paints its own immediate-mode UI. }
  TSpriteEditor = class(TTyroControl)
  private
    FFileName: string;
    FCanvasW, FCanvasH: Integer;
    FFrames: TObjectList<TSpriteFrame>;
    FActions: TObjectList<TSpriteAction>;
    FActiveAction: Integer;
    FActiveFrame: Integer;
    FTool: TSpriteTool;
    FFrontColor, FBackColor: TColor;
    FPalette: array of TColor;
    FZoom: Single;
    FPanX, FPanY: Single;
    FShowGrid: Boolean;
    FModified: Boolean;
    FStatus: string;
    FStatusTimer: Double;
    FPreviewTimer: Double;
    FPreviewFrame: Integer;
    FSelRect: TRect;
    FSelValid: Boolean;
    FCopyImage: TImage;
    FCopyValid: Boolean;
    FUndo: TObjectList<TSpriteSnapshot>;
    FRedo: TObjectList<TSpriteSnapshot>;
    FDragMode: TSpriteDragMode;
    FDragButton: TMouseButton;
    FDragStart: TPoint;
    FDragLast: TPoint;
    FDragEnd: TPoint;
    FLastPixel: TPoint;
    FLastPixelValid: Boolean;
    FMouseX, FMouseY: Integer;
    FMouseInCanvas: Boolean;
    FMiddleDown: Boolean;
    FMiddleLast: TPoint;
    FRenaming: Boolean;
    FRenameBuffer: string;
    FTextures: array of TTexture2D;
    FTextureDirty: array of Boolean;

    { small JSON helpers, nil-safe like the ones in SpriteAtlas }
    function DonChild(V: TDON_Value; const Name: string): TDON_Value;
    function DonInt(V: TDON_Value; const Name: string; ADef: Integer): Integer;
    function DonBool(V: TDON_Value; const Name: string; ADef: Boolean): Boolean;
    function DonStr(V: TDON_Value; const Name: string; const ADef: string): string;

    procedure BuildDefaultPalette;
    procedure AddDefaultAction;
    procedure ClearDocument;
    procedure ResetView;
    function LoadPng(const AFileName: string): Boolean;
    function LoadSprite(const AFileName: string): Boolean;
    procedure ParseMeta(const ADescriptor: string);
    function BuildMeta: string;
    function SavePng: Boolean;
    function SaveSprite: Boolean;
    procedure SetStatus(const S: string);

    function ActiveActionObject: TSpriteAction;
    function CurrentFrameIndex: Integer;
    function CurrentFrame: TSpriteFrame;
    procedure SelectAction(AIndex: Integer);
    procedure SelectFrameInAction(AIndex: Integer);
    function ActionFrameCount: Integer;
    function FrameAtActionIndex(AIndex: Integer): Integer;
    procedure AddAction;
    procedure DeleteAction;
    procedure AddFrameCopy;
    procedure AddFrameBlank;
    procedure DeleteActiveFrame;
    procedure MoveFrameInAction(ADelta: Integer);

    procedure ClearTextures;
    procedure RebuildTextureArray;
    procedure MarkFrameDirty(AIndex: Integer);
    function FrameTexture(AIndex: Integer): TTexture2D;

    procedure PushUndo;
    function CaptureSnapshot: TSpriteSnapshot;
    procedure ClearSnapshots(AList: TObjectList<TSpriteSnapshot>);
    procedure RestoreSnapshot(ASnapshot: TSpriteSnapshot);
    procedure Undo;
    procedure Redo;

    procedure RotateFrame(AClockwise: Boolean);
    procedure MirrorFrame(AHorizontal: Boolean);
    function PixelColor(AFrame, X, Y: Integer): TColor;
    procedure PutPixel(AFrame, X, Y: Integer; AColor: TColor; ABlend: Boolean);
    procedure DrawPixelLine(AFrame: Integer; X0, Y0, X1, Y1: Integer;
      AColor: TColor; ABlend: Boolean);
    procedure DrawSoftDot(AFrame: Integer; CX, CY: Single; AColor: TColor);
    procedure FloodFill(AFrame, X, Y: Integer; AColor: TColor);
    procedure DrawCirclePixels(AFrame, CX, CY, R: Integer; AColor: TColor;
      AFilled: Boolean);
    procedure DrawRectPixels(AFrame: Integer; const R: TRect; AColor: TColor;
      AFilled: Boolean);
    procedure ClearPixels(AFrame: Integer; const R: TRect);

    procedure CopySelection;
    procedure CutSelection;
    procedure PasteSelection;
    procedure DeleteSelection;
    procedure SelectAll;
    function NormalizedSelection: TRect;
    procedure ApplyShape;
    procedure BeginToolAt(PX, PY: Integer; AButton: TMouseButton);
    procedure ContinueToolAt(PX, PY: Integer);
    procedure EndToolAt(PX, PY: Integer);

    procedure HandleMouse;
    procedure HandleWheel;
    procedure HandleLeftClick(AX, AY: Integer);
    procedure HandleRightClick(AX, AY: Integer);
    procedure HandleKeyTool(AKey: TKeyboardKey);
    procedure CommitRename;

  protected
    //* Layout queries. They are in client coordinates, the same space DoPaint
    //* draws in, so a subclass can add overlays or hit-testing of its own.
    function ToolRect(AIndex: Integer): TRect;
    function TopButtonRect(AIndex, ACount: Integer): TRect;
    function CanvasAreaRect: TRect;
    function ToolsAreaRect: TRect;
    function RightAreaRect: TRect;
    function PaletteAreaRect: TRect;
    function ActionsAreaRect: TRect;
    function FramesAreaRect: TRect;
    function PropsAreaRect: TRect;
    function ActionItemRect(AIndex: Integer): TRect;
    function FrameItemRect(AIndex: Integer): TRect;
    function PaletteSwatchRect(AIndex: Integer): TRect;
    function FGColorRect: TRect;
    function BGColorRect: TRect;
    function DurationMinusRect: TRect;
    function DurationPlusRect: TRect;
    function LoopRect: TRect;
    function RenameRect: TRect;
    function ActionAddRect: TRect;
    function ActionDelRect: TRect;
    function FrameAddRect: TRect;
    function FrameDelRect: TRect;
    function FrameLeftRect: TRect;
    function FrameRightRect: TRect;
    function PreviewRect: TRect;

    procedure GetImageScreenRect(out R: TRect);
    procedure ScreenToPixel(SX, SY: Integer; out PX, PY: Integer);
    procedure ZoomAt(SX, SY: Integer; AFactor: Single);

    function GetHasDocument: Boolean;

    procedure DrawPanel(ACanvas: TTyroCanvas; const R: TRect;
      const ATitle: string; AIcon: Integer);
    procedure DrawButton(ACanvas: TTyroCanvas; const R: TRect;
      const ACaption: string; AActive, AEnabled: Boolean; AIcon: Integer = 0);
    procedure DrawIcon(ACanvas: TTyroCanvas; const R: TRect; AIcon: Integer;
      AColor: TColor);
    procedure DrawToolIcon(ACanvas: TTyroCanvas; const R: TRect;
      ATool: TSpriteTool; AColor: TColor);
    procedure PaintToolbar(ACanvas: TTyroCanvas);
    procedure PaintTools(ACanvas: TTyroCanvas);
    procedure PaintCanvas(ACanvas: TTyroCanvas);
    procedure PaintActions(ACanvas: TTyroCanvas);
    procedure PaintFrames(ACanvas: TTyroCanvas);
    procedure PaintProperties(ACanvas: TTyroCanvas);
    procedure PaintPalette(ACanvas: TTyroCanvas);
    procedure PaintPreview(ACanvas: TTyroCanvas; const R: TRect);
    procedure PaintSelection(ACanvas: TTyroCanvas);
    procedure PaintChecker(ACanvas: TTyroCanvas; const R: TRect);
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    procedure Update; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;
    procedure KeyPress(var Key: TUTF8Char); override;
    //* Starts an empty document. AFileName is only a suggested save target:
    //* the file is not created until the user saves.
    procedure NewDocument(AWidth, AHeight: Integer; const AFileName: string = '');
    function LoadFile(const AFileName: string): Boolean;
    function SaveFile: Boolean;
    property FileName: string read FFileName;
    //* True once a document is loaded or created; the engine uses it to avoid
    //* replacing the user's work when the editor is toggled back on.
    property HasDocument: Boolean read GetHasDocument;
    property Modified: Boolean read FModified;
    //* Current zoom factor (screen pixels per image pixel).
    property Zoom: Single read FZoom;
    //* Active tool; selecting a tool in the rail updates it.
    property Tool: TSpriteTool read FTool;
  end;

implementation

{ ---------------------------------------------------------------------------
  Small standalone helpers
  --------------------------------------------------------------------------- }

function InsideRect(const R: TRect; X, Y: Integer): Boolean;
begin
  Result := (X >= R.Left) and (X < R.Right) and (Y >= R.Top) and (Y < R.Bottom);
end;

function MakeRect(L, T, W, H: Integer): TRect;
begin
  Result := Rect(L, T, L + W, T + H);
end;

function ClampInt(AValue, AMin, AMax: Integer): Integer;
begin
  if AValue < AMin then
    Result := AMin
  else if AValue > AMax then
    Result := AMax
  else
    Result := AValue;
end;

function ClampFloat(AValue, AMin, AMax: Single): Single;
begin
  if AValue < AMin then
    Result := AMin
  else if AValue > AMax then
    Result := AMax
  else
    Result := AValue;
end;

//Short color constructor. Keeping it in one place avoids the helper-constructor
//overload ambiguity FPC hits when TColor.CreateRGBA is used directly as an
//argument in some expressions. Accepts $RRGGBBAA like TColor.CreateRGBA.
function SC(AColor: Cardinal): TColor; inline;
begin
  Result.RGBA.Red := Byte(AColor shr 24);
  Result.RGBA.Green := Byte(AColor shr 16);
  Result.RGBA.Blue := Byte(AColor shr 8);
  Result.RGBA.Alpha := Byte(AColor);
end;

function MakeColor(R, G, B, A: Byte): TColor;
begin
  Result := SC((Cardinal(R) shl 24) or (Cardinal(G) shl 16) or
    (Cardinal(B) shl 8) or Cardinal(A));
end;

//FPC cannot pick between the Integer and Single DrawText overloads when the
//color argument is a function call returning a record (like SC above), so all
//editor text goes through this non-overloaded wrapper.
procedure DrawTextC(ACanvas: TTyroCanvas; X, Y: Integer; const S: utf8string;
  const AColor: TColor); inline;
var
  C: TColor;
begin
  C := AColor;
  ACanvas.DrawText(X, Y, S, C);
end;

function BlendColor(const ADst, ASrc: TColor): TColor;
var
  A, IA: Integer;
begin
  A := ASrc.RGBA.Alpha;
  if A >= 255 then
    Result := ASrc
  else if A <= 0 then
    Result := ADst
  else
  begin
    IA := 255 - A;
    Result := MakeColor(
      (ASrc.RGBA.Red * A + ADst.RGBA.Red * IA) div 255,
      (ASrc.RGBA.Green * A + ADst.RGBA.Green * IA) div 255,
      (ASrc.RGBA.Blue * A + ADst.RGBA.Blue * IA) div 255,
      Byte(ASrc.RGBA.Alpha + (ADst.RGBA.Alpha * IA) div 255));
  end;
end;

function ColorToHex(const C: TColor): string;
begin
  Result := '#' + IntToHex(C.RGBA.Red, 2) + IntToHex(C.RGBA.Green, 2) +
    IntToHex(C.RGBA.Blue, 2) + IntToHex(C.RGBA.Alpha, 2);
end;

function HexToColor(const S: string; const ADef: TColor): TColor;
var
  T: string;
  V: Integer;
begin
  Result := ADef;
  T := Trim(S);
  if (T <> '') and (T[1] = '#') then
    Delete(T, 1, 1);
  if Length(T) <> 8 then
    Exit;
  if TryStrToInt('$' + T, V) then
    Result := SC(Cardinal(V));
end;

function JsonEscape(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '\': Result := Result + '\\';
      '"': Result := Result + '\"';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if Ord(C) < 32 then
        Result := Result + '\u' + IntToHex(Ord(C), 4)
      else
        Result := Result + C;
    end;
  end;
end;

function ToolName(ATool: TSpriteTool): string;
begin
  case ATool of
    stSelect: Result := 'Select';
    stPixel: Result := 'Pixel';
    stPen: Result := 'Pen';
    stFill: Result := 'Fill';
    stPick: Result := 'Pick';
    stCircle: Result := 'Circle';
    stRect: Result := 'Rectangle';
    stRotate: Result := 'Rotate';
  else
    Result := '';
  end;
end;

{ ---------------------------------------------------------------------------
  Document objects
  --------------------------------------------------------------------------- }

constructor TSpriteFrame.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  Image := RayLib.GenImageColor(AWidth, AHeight, clBlank);
  RayLib.ImageFormat(Image, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
  DurationMs := cSpriteEditorDefaultDuration;
  Name := '';
end;

constructor TSpriteFrame.Create(const ASource: TImage);
begin
  inherited Create;
  Image := RayLib.ImageCopy(ASource);
  RayLib.ImageFormat(Image, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
  DurationMs := cSpriteEditorDefaultDuration;
  Name := '';
end;

destructor TSpriteFrame.Destroy;
begin
  if Image.Data <> nil then
    RayLib.UnloadImage(Image);
  Image.Data := nil;
  inherited Destroy;
end;

constructor TSpriteAction.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
  Frames := TList<Integer>.Create;
  Loop := True;
end;

destructor TSpriteAction.Destroy;
begin
  Frames.Free;
  inherited Destroy;
end;

procedure TSpriteAction.AddFrame(AIndex: Integer);
begin
  Frames.Add(AIndex);
end;

constructor TSpriteSnapshot.Create;
begin
  inherited Create;
  ActionNames := TStringList.Create;
  ActionFrames := TStringList.Create;
end;

destructor TSpriteSnapshot.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(Images) do
    if Images[I].Data <> nil then
      RayLib.UnloadImage(Images[I]);
  SetLength(Images, 0);
  ActionNames.Free;
  ActionFrames.Free;
  inherited Destroy;
end;

{ ---------------------------------------------------------------------------
  Construction
  --------------------------------------------------------------------------- }

constructor TSpriteEditor.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := [csClip, csOpaque, csFocus, csRepeatKeys];
  FFrames := TObjectList<TSpriteFrame>.Create(True);
  FActions := TObjectList<TSpriteAction>.Create(True);
  //Snapshots are owned by hand: Undo/Redo take one out of the list and use it
  //after it has been removed, so the list must not free it on Delete.
  FUndo := TObjectList<TSpriteSnapshot>.Create(False);
  FRedo := TObjectList<TSpriteSnapshot>.Create(False);
  FCanvasW := 32;
  FCanvasH := 32;
  FActiveAction := -1;
  FActiveFrame := 0;
  FTool := stPixel;
  FFrontColor := clWhite;
  FBackColor := clBlank;
  BuildDefaultPalette;
  FZoom := 8;
  FPanX := 0;
  FPanY := 0;
  FShowGrid := True;
  FModified := False;
  FStatus := '';
  FStatusTimer := 0;
  FPreviewTimer := 0;
  FPreviewFrame := 0;
  FSelValid := False;
  FillChar(FCopyImage, SizeOf(FCopyImage), 0);
  FCopyValid := False;
  FDragMode := dmNone;
  FLastPixelValid := False;
  FMiddleDown := False;
  FRenaming := False;
  FRenameBuffer := '';
  BackColor := SC(cSpriteEditorBg);
  SetBoundsRect(Rect(0, 0, 900, 600));
  AddDefaultAction;
end;

destructor TSpriteEditor.Destroy;
begin
  ClearTextures;
  if FCopyImage.Data <> nil then
    RayLib.UnloadImage(FCopyImage);
  FCopyImage.Data := nil;
  ClearSnapshots(FUndo);
  ClearSnapshots(FRedo);
  FUndo.Free;
  FRedo.Free;
  FActions.Free;
  FFrames.Free;
  inherited Destroy;
end;

procedure TSpriteEditor.BuildDefaultPalette;
const
  cColors: array[0..31] of Cardinal = (
    $1A1C2CFF, $5D275DFF, $B13E53FF, $EF7D57FF,
    $CD75FFFF, $A7F070FF, $38B764FF, $257179FF,
    $29366FFF, $3B5DC9FF, $41A6F6FF, $73EFF7FF,
    $F4F4F4FF, $94B0C2FF, $566C86FF, $333C57FF,
    $000000FF, $FFFFFFFF, $7E2553FF, $008751FF,
    $AB5236FF, $5F574FFF, $C2C3C7FF, $F1E8FFFF,
    $004DFFFF, $A300FFFF, $EC27FFFF, $00E436FF,
    $29ADFFFF, $83769CFF, $77A8FFFF, $CCAAFFFF);
var
  I: Integer;
begin
  SetLength(FPalette, Length(cColors));
  for I := 0 to High(cColors) do
    FPalette[I] := SC(cColors[I]);
end;

procedure TSpriteEditor.AddDefaultAction;
var
  A: TSpriteAction;
begin
  A := TSpriteAction.Create('idle');
  if FFrames.Count > 0 then
    A.AddFrame(0);
  FActions.Add(A);
  FActiveAction := 0;
  FActiveFrame := 0;
end;

procedure TSpriteEditor.ClearDocument;
begin
  FRenaming := False;
  FSelValid := False;
  ClearTextures;
  FActions.Clear;
  FFrames.Clear;
  ClearSnapshots(FUndo);
  ClearSnapshots(FRedo);
  FActiveAction := -1;
  FActiveFrame := 0;
  FModified := False;
end;

procedure TSpriteEditor.ResetView;
begin
  FZoom := 8;
  if FCanvasW > 0 then
  begin
    while (FCanvasW * FZoom > 512) and (FZoom > 1) do
      FZoom := FZoom / 2;
  end;
  if FZoom < 1 then
    FZoom := 1;
  FPanX := 0;
  FPanY := 0;
  FPreviewTimer := 0;
  FPreviewFrame := 0;
end;

{ ---------------------------------------------------------------------------
  JSON helpers
  --------------------------------------------------------------------------- }

function TSpriteEditor.DonChild(V: TDON_Value; const Name: string): TDON_Value;
begin
  if V = nil then
    Result := nil
  else
    Result := V.Values[Name];
end;

function TSpriteEditor.DonInt(V: TDON_Value; const Name: string;
  ADef: Integer): Integer;
var
  C: TDON_Value;
begin
  Result := ADef;
  C := DonChild(V, Name);
  if C <> nil then
    Result := C.AsInteger;
end;

function TSpriteEditor.DonBool(V: TDON_Value; const Name: string;
  ADef: Boolean): Boolean;
var
  C: TDON_Value;
begin
  Result := ADef;
  C := DonChild(V, Name);
  if C <> nil then
    Result := C.AsBoolean;
end;

function TSpriteEditor.DonStr(V: TDON_Value; const Name: string;
  const ADef: string): string;
var
  C: TDON_Value;
begin
  Result := ADef;
  C := DonChild(V, Name);
  if C <> nil then
    Result := C.AsString;
end;

{ ---------------------------------------------------------------------------
  Loading
  --------------------------------------------------------------------------- }

function TSpriteEditor.GetHasDocument: Boolean;
begin
  Result := FFrames.Count > 0;
end;

procedure TSpriteEditor.NewDocument(AWidth, AHeight: Integer;
  const AFileName: string);
var
  F: TSpriteFrame;
begin
  ClearDocument;
  FCanvasW := Max(1, AWidth);
  FCanvasH := Max(1, AHeight);
  FFileName := AFileName;
  F := TSpriteFrame.Create(FCanvasW, FCanvasH);
  F.Name := 'frame_000';
  FFrames.Add(F);
  AddDefaultAction;
  ResetView;
  RebuildTextureArray;
  FModified := False;
  SetStatus('New sprite');
end;

function TSpriteEditor.LoadFile(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Result := False;
  if Trim(AFileName) = '' then
  begin
    SetStatus('No file name given');
    Exit;
  end;

  ClearDocument;
  FFileName := AFileName;
  Ext := LowerCase(ExtractFileExt(AFileName));
  if Ext = '.sprite' then
    Result := LoadSprite(AFileName)
  else
    Result := LoadPng(AFileName);

  if Result then
  begin
    if FActions.Count = 0 then
      AddDefaultAction;
    if FActiveAction < 0 then
      FActiveAction := 0;
    FModified := False;
    ResetView;
    RebuildTextureArray;
    SetStatus('Loaded ' + ExtractFileName(AFileName));
  end
  else
  begin
    if FStatus = '' then
      SetStatus('Could not load ' + ExtractFileName(AFileName));
    AddDefaultAction;
    RebuildTextureArray;
  end;
end;

function TSpriteEditor.LoadPng(const AFileName: string): Boolean;
var
  Img: TImage;
  F: TSpriteFrame;
begin
  Result := False;
  if not SysUtils.FileExists(AFileName) then
  begin
    SetStatus('File not found: ' + AFileName);
    Exit;
  end;
  Img := RayLib.LoadImage(PUTF8Char(UTF8String(AFileName)));
  if Img.Data = nil then
  begin
    SetStatus('Could not read image: ' + ExtractFileName(AFileName));
    Exit;
  end;
  try
    RayLib.ImageFormat(Img, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
    FCanvasW := Img.Width;
    FCanvasH := Img.Height;
    F := TSpriteFrame.Create(Img);
    F.Name := 'frame_000';
    FFrames.Add(F);
  finally
    RayLib.UnloadImage(Img);
  end;
  AddDefaultAction;
  Result := True;
end;

function TSpriteEditor.LoadSprite(const AFileName: string): Boolean;
var
  Atlas: TSpriteAtlas;
  I: Integer;
  Img: TImage;
  F: TSpriteFrame;
begin
  Result := False;
  if not SysUtils.FileExists(AFileName) then
  begin
    SetStatus('File not found: ' + AFileName);
    Exit;
  end;
  Atlas := TSpriteAtlas.Create;
  try
    if not Atlas.LoadFromFile(AFileName) then
    begin
      SetStatus('Could not read sprite: ' + ExtractFileName(AFileName));
      Exit;
    end;
    if Atlas.GetFrameCount <= 0 then
    begin
      SetStatus('Sprite has no frames');
      Exit;
    end;
    FCanvasW := 0;
    FCanvasH := 0;
    for I := 0 to Atlas.GetFrameCount - 1 do
    begin
      Img := Atlas.GetFrameImage(I);
      if Img.Data = nil then
        Continue;
      try
        RayLib.ImageFormat(Img, Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
        F := TSpriteFrame.Create(Img);
        F.Name := Format('frame_%.3d', [I]);
        F.DurationMs := Atlas.GetFrameDuration(I);
        if F.DurationMs <= 0 then
          F.DurationMs := cSpriteEditorDefaultDuration;
        FFrames.Add(F);
        if Img.Width > FCanvasW then
          FCanvasW := Img.Width;
        if Img.Height > FCanvasH then
          FCanvasH := Img.Height;
      finally
        RayLib.UnloadImage(Img);
      end;
    end;
    if FFrames.Count = 0 then
    begin
      SetStatus('Sprite frames could not be decoded');
      Exit;
    end;
    if FCanvasW <= 0 then
      FCanvasW := 32;
    if FCanvasH <= 0 then
      FCanvasH := 32;
    ParseMeta(Atlas.Descriptor);
    Result := True;
  finally
    Atlas.Free;
  end;
end;

procedure TSpriteEditor.ParseMeta(const ADescriptor: string);
var
  Root, Meta, ActionsNode, ActionNode, FramesNode, PaletteNode, Item: TDON_Value;
  Arr, FrameArr, PalArr: TDON_Array;
  I, J, Idx: Integer;
  A: TSpriteAction;
begin
  if Trim(ADescriptor) = '' then
    Exit;
  Root := JsonParseValueString(ADescriptor);
  if Root = nil then
    Exit;
  try
    Meta := DonChild(DonChild(Root, 'meta'), 'tyro');
    if Meta = nil then
      Exit;

    FCanvasW := DonInt(Meta, 'width', FCanvasW);
    FCanvasH := DonInt(Meta, 'height', FCanvasH);
    if FCanvasW <= 0 then
      FCanvasW := 32;
    if FCanvasH <= 0 then
      FCanvasH := 32;

    FFrontColor := HexToColor(DonStr(Meta, 'front', ''), FFrontColor);
    FBackColor := HexToColor(DonStr(Meta, 'back', ''), FBackColor);

    PaletteNode := DonChild(Meta, 'palette');
    if PaletteNode is TDON_Array then
    begin
      PalArr := TDON_Array(PaletteNode);
      if PalArr.Count > 0 then
      begin
        SetLength(FPalette, PalArr.Count);
        for I := 0 to PalArr.Count - 1 do
          FPalette[I] := HexToColor(PalArr[I].AsString, clWhite);
      end;
    end;

    ActionsNode := DonChild(Meta, 'actions');
    if ActionsNode is TDON_Array then
    begin
      FActions.Clear;
      Arr := TDON_Array(ActionsNode);
      for I := 0 to Arr.Count - 1 do
      begin
        ActionNode := Arr[I];
        A := TSpriteAction.Create(DonStr(ActionNode, 'name',
          'action' + IntToStr(I + 1)));
        A.Loop := DonBool(ActionNode, 'loop', True);
        FramesNode := DonChild(ActionNode, 'frames');
        if FramesNode is TDON_Array then
        begin
          FrameArr := TDON_Array(FramesNode);
          for J := 0 to FrameArr.Count - 1 do
          begin
            Item := FrameArr[J];
            if Item is TDON_Object then
              Idx := DonInt(Item, 'index', -1)
            else if Item <> nil then
              Idx := Item.AsInteger
            else
              Idx := -1;
            if (Idx >= 0) and (Idx < FFrames.Count) then
              A.AddFrame(Idx);
          end;
        end;
        if A.Frames.Count = 0 then
        begin
          for J := 0 to FFrames.Count - 1 do
            A.AddFrame(J);
        end;
        FActions.Add(A);
      end;
    end;

    if (FActions.Count > 0) and (FActiveAction < 0) then
      FActiveAction := 0;
  finally
    Root.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  Saving
  --------------------------------------------------------------------------- }

function TSpriteEditor.BuildMeta: string;
var
  I, J, Idx: Integer;
  A: TSpriteAction;
  S: string;
begin
  Result := '"tyro":{"version":' + IntToStr(cSpriteEditorVersion) +
    ',"width":' + IntToStr(FCanvasW) + ',"height":' + IntToStr(FCanvasH) +
    ',"front":"' + ColorToHex(FFrontColor) + '"' +
    ',"back":"' + ColorToHex(FBackColor) + '","palette":[';

  for I := 0 to High(FPalette) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + '"' + ColorToHex(FPalette[I]) + '"';
  end;

  Result := Result + '],"actions":[';
  for I := 0 to FActions.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    A := FActions[I];
    Result := Result + '{"name":"' + JsonEscape(A.Name) + '","loop":' +
      BoolToStr(A.Loop, 'true', 'false') + ',"frames":[';
    S := '';
    for J := 0 to A.Frames.Count - 1 do
    begin
      Idx := A.Frames[J];
      if J > 0 then
        S := S + ',';
      S := S + IntToStr(Idx);
    end;
    Result := Result + S + ']}';
  end;
  Result := Result + ']}';
end;

function TSpriteEditor.SavePng: Boolean;
var
  Idx: Integer;
begin
  Result := False;
  if FFrames.Count = 0 then
  begin
    SetStatus('Nothing to save');
    Exit;
  end;
  Idx := CurrentFrameIndex;
  if (Idx < 0) or (Idx >= FFrames.Count) then
    Idx := 0;
  try
    RayLib.ExportImage(FFrames[Idx].Image, PUTF8Char(UTF8String(FFileName)));
    FModified := False;
    SetStatus('Saved ' + ExtractFileName(FFileName));
    Result := True;
  except
    on E: Exception do
      SetStatus('Save failed: ' + E.Message);
  end;
end;

function TSpriteEditor.SaveSprite: Boolean;
var
  W: array of TSpriteAtlasWriteFrame;
  I: Integer;
begin
  Result := False;
  if FFrames.Count = 0 then
  begin
    SetStatus('Nothing to save');
    Exit;
  end;
  SetLength(W, FFrames.Count);
  for I := 0 to FFrames.Count - 1 do
  begin
    W[I].Name := Format('frame_%.3d', [I]);
    W[I].Image := FFrames[I].Image;
    W[I].DurationMs := FFrames[I].DurationMs;
  end;
  if SaveSpriteAtlas(FFileName, W, 'sheet.png', BuildMeta) then
  begin
    FModified := False;
    SetStatus('Saved ' + ExtractFileName(FFileName));
    Result := True;
  end
  else
    SetStatus('Save failed: ' + ExtractFileName(FFileName));
end;

function TSpriteEditor.SaveFile: Boolean;
begin
  if Trim(FFileName) = '' then
  begin
    SetStatus('No file name to save to');
    Exit(False);
  end;
  if LowerCase(ExtractFileExt(FFileName)) = '.sprite' then
    Result := SaveSprite
  else
    Result := SavePng;
end;

procedure TSpriteEditor.SetStatus(const S: string);
begin
  FStatus := S;
  FStatusTimer := 4.5;
end;

{ ---------------------------------------------------------------------------
  Actions and frames
  --------------------------------------------------------------------------- }

function TSpriteEditor.ActiveActionObject: TSpriteAction;
begin
  if (FActiveAction >= 0) and (FActiveAction < FActions.Count) then
    Result := FActions[FActiveAction]
  else
    Result := nil;
end;

function TSpriteEditor.CurrentFrameIndex: Integer;
var
  A: TSpriteAction;
  I: Integer;
begin
  Result := -1;
  A := ActiveActionObject;
  if (A <> nil) and (A.Frames.Count > 0) then
  begin
    I := ClampInt(FActiveFrame, 0, A.Frames.Count - 1);
    Result := A.Frames[I];
  end
  else if FFrames.Count > 0 then
    Result := ClampInt(FActiveFrame, 0, FFrames.Count - 1);
  if (Result < 0) or (Result >= FFrames.Count) then
    Result := -1;
end;

function TSpriteEditor.CurrentFrame: TSpriteFrame;
var
  I: Integer;
begin
  I := CurrentFrameIndex;
  if (I >= 0) and (I < FFrames.Count) then
    Result := FFrames[I]
  else
    Result := nil;
end;

procedure TSpriteEditor.SelectAction(AIndex: Integer);
begin
  if FActions.Count = 0 then
    Exit;
  FActiveAction := ClampInt(AIndex, 0, FActions.Count - 1);
  FActiveFrame := 0;
  FPreviewFrame := 0;
  FPreviewTimer := 0;
  FRenaming := False;
  Invalidate;
end;

procedure TSpriteEditor.SelectFrameInAction(AIndex: Integer);
var
  A: TSpriteAction;
begin
  A := ActiveActionObject;
  if (A = nil) or (A.Frames.Count = 0) then
    Exit;
  FActiveFrame := ClampInt(AIndex, 0, A.Frames.Count - 1);
  FPreviewFrame := FActiveFrame;
  FPreviewTimer := 0;
  Invalidate;
end;

function TSpriteEditor.ActionFrameCount: Integer;
var
  A: TSpriteAction;
begin
  A := ActiveActionObject;
  if A <> nil then
    Result := A.Frames.Count
  else
    Result := 0;
end;

function TSpriteEditor.FrameAtActionIndex(AIndex: Integer): Integer;
var
  A: TSpriteAction;
begin
  Result := -1;
  A := ActiveActionObject;
  if (A <> nil) and (AIndex >= 0) and (AIndex < A.Frames.Count) then
    Result := A.Frames[AIndex];
end;

procedure TSpriteEditor.AddAction;
var
  A: TSpriteAction;
  N, I: Integer;
  Base: string;
begin
  PushUndo;
  Base := 'action';
  N := FActions.Count + 1;
  for I := 0 to FActions.Count - 1 do
    if SameText(FActions[I].Name, Base + IntToStr(N)) then
      Inc(N);
  A := TSpriteAction.Create(Base + IntToStr(N));
  if CurrentFrameIndex >= 0 then
    A.AddFrame(CurrentFrameIndex)
  else if FFrames.Count > 0 then
    A.AddFrame(0);
  FActions.Add(A);
  FActiveAction := FActions.Count - 1;
  FActiveFrame := 0;
  FPreviewFrame := 0;
  SetStatus('Added action ' + A.Name);
  Invalidate;
end;

procedure TSpriteEditor.DeleteAction;
var
  I: Integer;
begin
  if FActions.Count <= 1 then
  begin
    SetStatus('At least one action is required');
    Exit;
  end;
  PushUndo;
  I := FActiveAction;
  FActions.Delete(I);
  if FActiveAction >= FActions.Count then
    FActiveAction := FActions.Count - 1;
  FActiveFrame := 0;
  FPreviewFrame := 0;
  SetStatus('Action removed');
  Invalidate;
end;

procedure TSpriteEditor.AddFrameCopy;
var
  A: TSpriteAction;
  F: TSpriteFrame;
  Idx: Integer;
begin
  A := ActiveActionObject;
  if A = nil then
    Exit;
  Idx := CurrentFrameIndex;
  if (Idx < 0) or (Idx >= FFrames.Count) then
  begin
    SetStatus('Select a frame first');
    Exit;
  end;
  PushUndo;
  F := TSpriteFrame.Create(FFrames[Idx].Image);
  F.DurationMs := FFrames[Idx].DurationMs;
  F.Name := Format('frame_%.3d', [FFrames.Count]);
  FFrames.Add(F);
  A.Frames.Insert(ClampInt(FActiveFrame + 1, 0, A.Frames.Count), FFrames.Count - 1);
  FActiveFrame := A.Frames.IndexOf(FFrames.Count - 1);
  RebuildTextureArray;
  FPreviewFrame := FActiveFrame;
  SetStatus('Frame duplicated');
  Invalidate;
end;

procedure TSpriteEditor.AddFrameBlank;
var
  A: TSpriteAction;
  F: TSpriteFrame;
begin
  A := ActiveActionObject;
  if A = nil then
    Exit;
  PushUndo;
  F := TSpriteFrame.Create(FCanvasW, FCanvasH);
  F.Name := Format('frame_%.3d', [FFrames.Count]);
  FFrames.Add(F);
  A.Frames.Insert(ClampInt(FActiveFrame + 1, 0, A.Frames.Count), FFrames.Count - 1);
  FActiveFrame := A.Frames.IndexOf(FFrames.Count - 1);
  RebuildTextureArray;
  FPreviewFrame := FActiveFrame;
  SetStatus('Blank frame added');
  Invalidate;
end;

procedure TSpriteEditor.DeleteActiveFrame;
var
  Idx, I, J: Integer;
  A: TSpriteAction;
begin
  if FFrames.Count <= 1 then
  begin
    SetStatus('At least one frame is required');
    Exit;
  end;
  Idx := CurrentFrameIndex;
  if (Idx < 0) or (Idx >= FFrames.Count) then
    Exit;
  PushUndo;

  for I := 0 to FActions.Count - 1 do
  begin
    A := FActions[I];
    for J := A.Frames.Count - 1 downto 0 do
    begin
      if A.Frames[J] = Idx then
        A.Frames.Delete(J)
      else if A.Frames[J] > Idx then
        A.Frames[J] := A.Frames[J] - 1;
    end;
  end;

  FFrames.Delete(Idx);
  RebuildTextureArray;
  for I := 0 to FActions.Count - 1 do
    if FActions[I].Frames.Count = 0 then
      FActions[I].AddFrame(0);

  A := ActiveActionObject;
  if A <> nil then
    FActiveFrame := ClampInt(FActiveFrame, 0, A.Frames.Count - 1);
  FPreviewFrame := FActiveFrame;
  SetStatus('Frame deleted');
  Invalidate;
end;

procedure TSpriteEditor.MoveFrameInAction(ADelta: Integer);
var
  A: TSpriteAction;
  NewIndex, V: Integer;
begin
  A := ActiveActionObject;
  if (A = nil) or (A.Frames.Count < 2) then
    Exit;
  NewIndex := FActiveFrame + ADelta;
  if (NewIndex < 0) or (NewIndex >= A.Frames.Count) then
    Exit;
  PushUndo;
  V := A.Frames[FActiveFrame];
  A.Frames[FActiveFrame] := A.Frames[NewIndex];
  A.Frames[NewIndex] := V;
  FActiveFrame := NewIndex;
  FPreviewFrame := NewIndex;
  Invalidate;
end;

{ ---------------------------------------------------------------------------
  GPU textures for the editor canvas and preview
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.ClearTextures;
var
  I: Integer;
begin
  for I := 0 to High(FTextures) do
    if FTextures[I].ID <> 0 then
      RayLib.UnloadTexture(FTextures[I]);
  SetLength(FTextures, 0);
  SetLength(FTextureDirty, 0);
end;

procedure TSpriteEditor.RebuildTextureArray;
var
  I: Integer;
begin
  ClearTextures;
  SetLength(FTextures, FFrames.Count);
  SetLength(FTextureDirty, FFrames.Count);
  for I := 0 to FFrames.Count - 1 do
  begin
    FillChar(FTextures[I], SizeOf(FTextures[I]), 0);
    FTextureDirty[I] := True;
  end;
end;

procedure TSpriteEditor.MarkFrameDirty(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Length(FTextureDirty)) then
    FTextureDirty[AIndex] := True;
  FModified := True;
end;

function TSpriteEditor.FrameTexture(AIndex: Integer): TTexture2D;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (AIndex < 0) or (AIndex >= FFrames.Count) then
    Exit;
  if AIndex >= Length(FTextures) then
    RebuildTextureArray;
  if FTextureDirty[AIndex] or (FTextures[AIndex].ID = 0) then
  begin
    if FTextures[AIndex].ID <> 0 then
      RayLib.UnloadTexture(FTextures[AIndex]);
    FTextures[AIndex] := RayLib.LoadTextureFromImage(FFrames[AIndex].Image);
    RayLib.SetTextureFilter(FTextures[AIndex], TEXTURE_FILTER_POINT);
    FTextureDirty[AIndex] := False;
  end;
  Result := FTextures[AIndex];
end;

{ ---------------------------------------------------------------------------
  Undo / redo
  --------------------------------------------------------------------------- }

function TSpriteEditor.CaptureSnapshot: TSpriteSnapshot;
var
  I, J: Integer;
  A: TSpriteAction;
  S: string;
begin
  Result := TSpriteSnapshot.Create;
  SetLength(Result.Images, FFrames.Count);
  SetLength(Result.Durations, FFrames.Count);
  for I := 0 to FFrames.Count - 1 do
  begin
    Result.Images[I] := RayLib.ImageCopy(FFrames[I].Image);
    Result.Durations[I] := FFrames[I].DurationMs;
  end;

  for I := 0 to FActions.Count - 1 do
  begin
    A := FActions[I];
    Result.ActionNames.Add(A.Name);
    S := '';
    for J := 0 to A.Frames.Count - 1 do
    begin
      if J > 0 then
        S := S + ',';
      S := S + IntToStr(A.Frames[J]);
    end;
    Result.ActionFrames.Add(S);
  end;
  Result.ActiveAction := FActiveAction;
  Result.ActiveFrame := FActiveFrame;
end;

procedure TSpriteEditor.RestoreSnapshot(ASnapshot: TSpriteSnapshot);
var
  I, J: Integer;
  F: TSpriteFrame;
  A: TSpriteAction;
  Parts: TStringList;
begin
  if ASnapshot = nil then
    Exit;

  FFrames.Clear;
  for I := 0 to High(ASnapshot.Images) do
  begin
    F := TSpriteFrame.Create(ASnapshot.Images[I]);
    if I < Length(ASnapshot.Durations) then
      F.DurationMs := ASnapshot.Durations[I];
    F.Name := Format('frame_%.3d', [I]);
    FFrames.Add(F);
  end;

  FActions.Clear;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := True;
    for I := 0 to ASnapshot.ActionNames.Count - 1 do
    begin
      A := TSpriteAction.Create(ASnapshot.ActionNames[I]);
      if I < ASnapshot.ActionFrames.Count then
      begin
        Parts.DelimitedText := ASnapshot.ActionFrames[I];
        for J := 0 to Parts.Count - 1 do
          if Trim(Parts[J]) <> '' then
            A.AddFrame(StrToIntDef(Parts[J], 0));
      end;
      if A.Frames.Count = 0 then
        for J := 0 to FFrames.Count - 1 do
          A.AddFrame(J);
      FActions.Add(A);
    end;
  finally
    Parts.Free;
  end;

  if FActions.Count = 0 then
    AddDefaultAction
  else
  begin
    FActiveAction := ClampInt(ASnapshot.ActiveAction, 0, FActions.Count - 1);
    FActiveFrame := ClampInt(ASnapshot.ActiveFrame, 0,
      Max(0, FActions[FActiveAction].Frames.Count - 1));
  end;
  FPreviewFrame := FActiveFrame;
  FPreviewTimer := 0;
  FSelValid := False;
  RebuildTextureArray;
  FModified := True;
  Invalidate;
end;

procedure TSpriteEditor.ClearSnapshots(AList: TObjectList<TSpriteSnapshot>);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AList[I].Free;
  AList.Clear;
end;

procedure TSpriteEditor.PushUndo;
var
  Snap: TSpriteSnapshot;
begin
  Snap := CaptureSnapshot;
  FUndo.Add(Snap);
  while FUndo.Count > cSpriteEditorMaxUndo do
  begin
    FUndo[0].Free;
    FUndo.Delete(0);
  end;
  ClearSnapshots(FRedo);
  FModified := True;
end;

procedure TSpriteEditor.Undo;
var
  Snap: TSpriteSnapshot;
begin
  if FUndo.Count = 0 then
  begin
    SetStatus('Nothing to undo');
    Exit;
  end;
  Snap := FUndo.Last;
  FUndo.Delete(FUndo.Count - 1);
  FRedo.Add(CaptureSnapshot);
  RestoreSnapshot(Snap);
  Snap.Free;
  SetStatus('Undo');
end;

procedure TSpriteEditor.Redo;
var
  Snap: TSpriteSnapshot;
begin
  if FRedo.Count = 0 then
  begin
    SetStatus('Nothing to redo');
    Exit;
  end;
  Snap := FRedo.Last;
  FRedo.Delete(FRedo.Count - 1);
  FUndo.Add(CaptureSnapshot);
  RestoreSnapshot(Snap);
  Snap.Free;
  SetStatus('Redo');
end;

{ ---------------------------------------------------------------------------
  Image operations
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.RotateFrame(AClockwise: Boolean);
var
  Idx: Integer;
begin
  Idx := CurrentFrameIndex;
  if (Idx < 0) or (Idx >= FFrames.Count) then
    Exit;
  if FCanvasW <> FCanvasH then
  begin
    SetStatus('Rotate needs a square canvas');
    Exit;
  end;
  PushUndo;
  if AClockwise then
    RayLib.ImageRotateCW(FFrames[Idx].Image)
  else
    RayLib.ImageRotateCCW(FFrames[Idx].Image);
  MarkFrameDirty(Idx);
  SetStatus('Rotated frame ' + IntToStr(Idx + 1));
  Invalidate;
end;

procedure TSpriteEditor.MirrorFrame(AHorizontal: Boolean);
var
  Idx: Integer;
begin
  Idx := CurrentFrameIndex;
  if (Idx < 0) or (Idx >= FFrames.Count) then
    Exit;
  PushUndo;
  if AHorizontal then
    RayLib.ImageFlipHorizontal(FFrames[Idx].Image)
  else
    RayLib.ImageFlipVertical(FFrames[Idx].Image);
  MarkFrameDirty(Idx);
  SetStatus('Mirrored frame ' + IntToStr(Idx + 1));
  Invalidate;
end;

function TSpriteEditor.PixelColor(AFrame, X, Y: Integer): TColor;
begin
  if (AFrame >= 0) and (AFrame < FFrames.Count) then
    Result := RayLib.GetImageColor(FFrames[AFrame].Image, X, Y)
  else
    Result := clBlank;
end;

procedure TSpriteEditor.PutPixel(AFrame, X, Y: Integer; AColor: TColor;
  ABlend: Boolean);
var
  Dst: TColor;
begin
  if (AFrame < 0) or (AFrame >= FFrames.Count) then
    Exit;
  if (X < 0) or (Y < 0) or (X >= FCanvasW) or (Y >= FCanvasH) then
    Exit;
  if ABlend then
  begin
    Dst := RayLib.GetImageColor(FFrames[AFrame].Image, X, Y);
    AColor := BlendColor(Dst, AColor);
  end;
  RayLib.ImageDrawPixel(@FFrames[AFrame].Image, X, Y, AColor);
end;

procedure TSpriteEditor.DrawPixelLine(AFrame: Integer; X0, Y0, X1, Y1: Integer;
  AColor: TColor; ABlend: Boolean);
var
  DX, DY, SX, SY, Err, E2: Integer;
begin
  DX := Abs(X1 - X0);
  DY := -Abs(Y1 - Y0);
  if X0 < X1 then SX := 1 else SX := -1;
  if Y0 < Y1 then SY := 1 else SY := -1;
  Err := DX + DY;
  while True do
  begin
    PutPixel(AFrame, X0, Y0, AColor, ABlend);
    if (X0 = X1) and (Y0 = Y1) then
      Break;
    E2 := 2 * Err;
    if E2 >= DY then
    begin
      Err := Err + DY;
      X0 := X0 + SX;
    end;
    if E2 <= DX then
    begin
      Err := Err + DX;
      Y0 := Y0 + SY;
    end;
  end;
end;

procedure TSpriteEditor.DrawSoftDot(AFrame: Integer; CX, CY: Single;
  AColor: TColor);
var
  X, Y, R: Integer;
  DX, DY, D, A: Single;
  C: TColor;
begin
  R := 2;
  for Y := Floor(CY) - R to Ceil(CY) + R do
    for X := Floor(CX) - R to Ceil(CX) + R do
    begin
      DX := X + 0.5 - CX;
      DY := Y + 0.5 - CY;
      D := Sqrt(DX * DX + DY * DY);
      if D <= 1.35 then
      begin
        if D <= 0.55 then
          A := 1
        else
          A := (1.35 - D) / 0.8;
        A := ClampFloat(A, 0, 1);
        C := AColor.SetAlpha(Byte(Round(AColor.RGBA.Alpha * A)));
        PutPixel(AFrame, X, Y, C, True);
      end;
    end;
end;

procedure TSpriteEditor.FloodFill(AFrame, X, Y: Integer; AColor: TColor);
var
  Target: TColor;
  Stack: array of TPoint;
  SP, Cap: Integer;
  P: TPoint;
  C: TColor;

  procedure Push(AX, AY: Integer);
  begin
    if (AX < 0) or (AY < 0) or (AX >= FCanvasW) or (AY >= FCanvasH) then
      Exit;
    if SP >= Cap then
    begin
      Cap := Cap * 2;
      SetLength(Stack, Cap);
    end;
    Stack[SP].X := AX;
    Stack[SP].Y := AY;
    Inc(SP);
  end;

begin
  if (AFrame < 0) or (AFrame >= FFrames.Count) then
    Exit;
  if (X < 0) or (Y < 0) or (X >= FCanvasW) or (Y >= FCanvasH) then
    Exit;
  Target := RayLib.GetImageColor(FFrames[AFrame].Image, X, Y);
  if Target = AColor then
    Exit;

  Cap := Max(16, FCanvasW * FCanvasH);
  SetLength(Stack, Cap);
  SP := 0;
  Push(X, Y);
  while SP > 0 do
  begin
    Dec(SP);
    P := Stack[SP];
    C := RayLib.GetImageColor(FFrames[AFrame].Image, P.X, P.Y);
    if not (C = Target) then
      Continue;
    RayLib.ImageDrawPixel(@FFrames[AFrame].Image, P.X, P.Y, AColor);
    Push(P.X - 1, P.Y);
    Push(P.X + 1, P.Y);
    Push(P.X, P.Y - 1);
    Push(P.X, P.Y + 1);
  end;
end;

procedure TSpriteEditor.DrawCirclePixels(AFrame, CX, CY, R: Integer;
  AColor: TColor; AFilled: Boolean);
var
  DY, DX: Integer;
begin
  if R < 0 then
    Exit;
  if R = 0 then
  begin
    PutPixel(AFrame, CX, CY, AColor, False);
    Exit;
  end;
  for DY := -R to R do
  begin
    DX := Round(Sqrt(Max(0, R * R - DY * DY)));
    if AFilled then
      DrawPixelLine(AFrame, CX - DX, CY + DY, CX + DX, CY + DY, AColor, False)
    else
    begin
      PutPixel(AFrame, CX - DX, CY + DY, AColor, False);
      PutPixel(AFrame, CX + DX, CY + DY, AColor, False);
      if DX > 0 then
      begin
        PutPixel(AFrame, CX - DX + 1, CY + DY, AColor, False);
        PutPixel(AFrame, CX + DX - 1, CY + DY, AColor, False);
      end;
    end;
  end;
end;

procedure TSpriteEditor.DrawRectPixels(AFrame: Integer; const R: TRect;
  AColor: TColor; AFilled: Boolean);
var
  Y: Integer;
  L, T, RR, B: Integer;
begin
  L := R.Left;
  T := R.Top;
  RR := R.Right;
  B := R.Bottom;
  if RR <= L then
    RR := L + 1;
  if B <= T then
    B := T + 1;
  if AFilled then
  begin
    for Y := T to B - 1 do
      DrawPixelLine(AFrame, L, Y, RR - 1, Y, AColor, False);
  end
  else
  begin
    DrawPixelLine(AFrame, L, T, RR - 1, T, AColor, False);
    DrawPixelLine(AFrame, L, B - 1, RR - 1, B - 1, AColor, False);
    DrawPixelLine(AFrame, L, T, L, B - 1, AColor, False);
    DrawPixelLine(AFrame, RR - 1, T, RR - 1, B - 1, AColor, False);
  end;
end;

procedure TSpriteEditor.ClearPixels(AFrame: Integer; const R: TRect);
var
  X, Y: Integer;
begin
  if (AFrame < 0) or (AFrame >= FFrames.Count) then
    Exit;
  for Y := Max(0, R.Top) to Min(FCanvasH, R.Bottom) - 1 do
    for X := Max(0, R.Left) to Min(FCanvasW, R.Right) - 1 do
      RayLib.ImageDrawPixel(@FFrames[AFrame].Image, X, Y, clBlank);
end;

{ ---------------------------------------------------------------------------
  Selection and clipboard
  --------------------------------------------------------------------------- }

function TSpriteEditor.NormalizedSelection: TRect;
var
  L, T, R, B: Integer;
begin
  L := Min(FSelRect.Left, FSelRect.Right);
  T := Min(FSelRect.Top, FSelRect.Bottom);
  R := Max(FSelRect.Left, FSelRect.Right);
  B := Max(FSelRect.Top, FSelRect.Bottom);
  L := ClampInt(L, 0, FCanvasW);
  T := ClampInt(T, 0, FCanvasH);
  R := ClampInt(R, 0, FCanvasW);
  B := ClampInt(B, 0, FCanvasH);
  Result := Rect(L, T, R, B);
end;

procedure TSpriteEditor.CopySelection;
var
  Sel: TRect;
  Idx: Integer;
  Tmp: TImage;
begin
  if not FSelValid then
  begin
    SetStatus('Select an area first');
    Exit;
  end;
  Sel := NormalizedSelection;
  if (Sel.Right <= Sel.Left) or (Sel.Bottom <= Sel.Top) then
    Exit;
  Idx := CurrentFrameIndex;
  if Idx < 0 then
    Exit;
  Tmp := RayLib.ImageCopy(FFrames[Idx].Image);
  RayLib.ImageCrop(Tmp, TRectangle.Create(Sel.Left, Sel.Top,
    Sel.Right - Sel.Left, Sel.Bottom - Sel.Top));
  if FCopyImage.Data <> nil then
    RayLib.UnloadImage(FCopyImage);
  FCopyImage := Tmp;
  FCopyValid := True;
  SetStatus(Format('Copied %dx%d', [Sel.Right - Sel.Left, Sel.Bottom - Sel.Top]));
end;

procedure TSpriteEditor.CutSelection;
var
  Sel: TRect;
  Idx: Integer;
begin
  if not FSelValid then
    Exit;
  CopySelection;
  Sel := NormalizedSelection;
  Idx := CurrentFrameIndex;
  if (Idx < 0) or (Sel.Right <= Sel.Left) or (Sel.Bottom <= Sel.Top) then
    Exit;
  PushUndo;
  ClearPixels(Idx, Sel);
  MarkFrameDirty(Idx);
  SetStatus('Cut selection');
  Invalidate;
end;

procedure TSpriteEditor.PasteSelection;
var
  Sel: TRect;
  Idx, X, Y: Integer;
begin
  if not FCopyValid then
  begin
    SetStatus('Clipboard is empty');
    Exit;
  end;
  Idx := CurrentFrameIndex;
  if Idx < 0 then
    Exit;
  PushUndo;
  if FSelValid then
  begin
    Sel := NormalizedSelection;
    X := Sel.Left;
    Y := Sel.Top;
  end
  else
  begin
    X := 0;
    Y := 0;
  end;
  RayLib.ImageDrawImage(@FFrames[Idx].Image, FCopyImage, X, Y, clWhite);
  FSelRect := Rect(X, Y, X + FCopyImage.Width, Y + FCopyImage.Height);
  FSelValid := True;
  MarkFrameDirty(Idx);
  SetStatus('Pasted');
  Invalidate;
end;

procedure TSpriteEditor.DeleteSelection;
var
  Sel: TRect;
  Idx: Integer;
begin
  if not FSelValid then
    Exit;
  Sel := NormalizedSelection;
  Idx := CurrentFrameIndex;
  if (Idx < 0) or (Sel.Right <= Sel.Left) or (Sel.Bottom <= Sel.Top) then
    Exit;
  PushUndo;
  ClearPixels(Idx, Sel);
  MarkFrameDirty(Idx);
  SetStatus('Selection cleared');
  Invalidate;
end;

procedure TSpriteEditor.SelectAll;
begin
  FSelRect := Rect(0, 0, FCanvasW, FCanvasH);
  FSelValid := True;
  Invalidate;
end;

procedure TSpriteEditor.ApplyShape;
var
  Idx, R: Integer;
  C: TColor;
  L, T, RR, B: Integer;
  DX, DY: Integer;
begin
  Idx := CurrentFrameIndex;
  if Idx < 0 then
    Exit;
  if FDragButton = mbRight then
    C := FBackColor
  else
    C := FFrontColor;

  case FTool of
    stCircle:
      begin
        DX := FDragEnd.X - FDragStart.X;
        DY := FDragEnd.Y - FDragStart.Y;
        R := Round(Sqrt(DX * DX + DY * DY));
        DrawCirclePixels(Idx, FDragStart.X, FDragStart.Y, R, C,
          RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT));
      end;
    stRect:
      begin
        L := Min(FDragStart.X, FDragEnd.X);
        T := Min(FDragStart.Y, FDragEnd.Y);
        RR := Max(FDragStart.X, FDragEnd.X) + 1;
        B := Max(FDragStart.Y, FDragEnd.Y) + 1;
        DrawRectPixels(Idx, Rect(L, T, RR, B), C,
          RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT));
      end;
  end;
  MarkFrameDirty(Idx);
end;

{ ---------------------------------------------------------------------------
  Tool interaction
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.BeginToolAt(PX, PY: Integer; AButton: TMouseButton);
var
  Idx: Integer;
  C: TColor;
begin
  if (PX < 0) or (PY < 0) or (PX >= FCanvasW) or (PY >= FCanvasH) then
    Exit;
  Idx := CurrentFrameIndex;
  if Idx < 0 then
  begin
    SetStatus('No frame to draw on');
    Exit;
  end;

  FDragButton := AButton;
  FDragStart := Point(PX, PY);
  FDragEnd := FDragStart;

  case FTool of
    stSelect:
      begin
        FSelValid := True;
        FSelRect := Rect(PX, PY, PX + 1, PY + 1);
        FDragMode := dmSelect;
      end;
    stPixel:
      begin
        PushUndo;
        if AButton = mbRight then C := FBackColor else C := FFrontColor;
        PutPixel(Idx, PX, PY, C, False);
        FLastPixel := Point(PX, PY);
        FLastPixelValid := True;
        FDragMode := dmPaint;
        MarkFrameDirty(Idx);
      end;
    stPen:
      begin
        PushUndo;
        if AButton = mbRight then C := FBackColor else C := FFrontColor;
        DrawSoftDot(Idx, PX + 0.5, PY + 0.5, C);
        FLastPixel := Point(PX, PY);
        FLastPixelValid := True;
        FDragMode := dmPaint;
        MarkFrameDirty(Idx);
      end;
    stFill:
      begin
        PushUndo;
        if AButton = mbRight then C := FBackColor else C := FFrontColor;
        FloodFill(Idx, PX, PY, C);
        MarkFrameDirty(Idx);
        SetStatus('Filled');
        FDragMode := dmNone;
      end;
    stPick:
      begin
        C := PixelColor(Idx, PX, PY);
        if AButton = mbRight then
          FBackColor := C
        else
          FFrontColor := C;
        SetStatus('Picked ' + ColorToHex(C));
        FDragMode := dmNone;
      end;
    stCircle, stRect:
      begin
        PushUndo;
        FDragMode := dmShape;
      end;
    stRotate:
      begin
        RotateFrame(AButton <> mbRight);
        FDragMode := dmNone;
      end;
  else
    FDragMode := dmNone;
  end;
  Invalidate;
end;

procedure TSpriteEditor.ContinueToolAt(PX, PY: Integer);
var
  Idx: Integer;
  C: TColor;
  X, Y: Integer;
  Steps, I: Integer;
  FX, FY, TX, TY: Single;
begin
  if FDragMode = dmNone then
    Exit;
  Idx := CurrentFrameIndex;
  if Idx < 0 then
    Exit;

  case FDragMode of
    dmSelect:
      begin
        FSelRect := Rect(FDragStart.X, FDragStart.Y, PX + 1, PY + 1);
        FSelValid := True;
      end;
    dmShape:
      begin
        FDragEnd := Point(PX, PY);
      end;
    dmPaint:
      begin
        if FDragButton = mbRight then C := FBackColor else C := FFrontColor;
        if FTool = stPen then
        begin
          FX := FLastPixel.X + 0.5;
          FY := FLastPixel.Y + 0.5;
          TX := PX + 0.5;
          TY := PY + 0.5;
          Steps := Max(1, Ceil(Sqrt((TX - FX) * (TX - FX) + (TY - FY) * (TY - FY)) * 2));
          for I := 1 to Steps do
            DrawSoftDot(Idx, FX + (TX - FX) * I / Steps,
              FY + (TY - FY) * I / Steps, C);
        end
        else
          DrawPixelLine(Idx, FLastPixel.X, FLastPixel.Y, PX, PY, C, False);
        FLastPixel := Point(PX, PY);
        FLastPixelValid := True;
        MarkFrameDirty(Idx);
      end;
  end;
  Invalidate;
end;

procedure TSpriteEditor.EndToolAt(PX, PY: Integer);
var
  Sel: TRect;
begin
  case FDragMode of
    dmShape:
      begin
        FDragEnd := Point(PX, PY);
        ApplyShape;
        SetStatus(ToolName(FTool) + ' drawn');
      end;
    dmSelect:
      begin
        Sel := NormalizedSelection;
        FSelValid := (Sel.Right > Sel.Left) and (Sel.Bottom > Sel.Top);
        if FSelValid then
          SetStatus(Format('Selected %dx%d', [Sel.Right - Sel.Left, Sel.Bottom - Sel.Top]))
        else
          SetStatus('Selection cleared');
      end;
    dmPaint:
      begin
        SetStatus(ToolName(FTool) + ' applied');
      end;
  end;
  FDragMode := dmNone;
  FLastPixelValid := False;
  Invalidate;
end;

{ ---------------------------------------------------------------------------
  Mouse and wheel
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: integer);
var
  P: TPoint;
begin
  inherited;
  if not Visible then
    Exit;
  if Button <> mbLeft then
    Exit;
  Focused := True;
  //* The engine hands us window-local points; MapToClient takes window
  //* (screen) coordinates and returns the client space DoPaint draws in.
  P := MapToClient(Point(x + WindowRect.Left, y + WindowRect.Top));
  HandleLeftClick(P.X, P.Y);
end;

procedure TSpriteEditor.HandleMouse;
var
  mp: TVector2;
  P: TPoint;
  PX, PY: Integer;
begin
  mp := RayLib.GetMousePosition;
  P := MapToClient(Point(Round(mp.X), Round(mp.Y)));
  FMouseX := P.X;
  FMouseY := P.Y;
  FMouseInCanvas := InsideRect(CanvasAreaRect, FMouseX, FMouseY);

  if not Focused then
    Exit;

  if RayLib.IsMouseButtonPressed(MOUSE_BUTTON_MIDDLE) then
  begin
    FMiddleDown := True;
    FMiddleLast := Point(FMouseX, FMouseY);
  end;
  if FMiddleDown then
  begin
    if RayLib.IsMouseButtonDown(MOUSE_BUTTON_MIDDLE) then
    begin
      FPanX := FPanX + (FMouseX - FMiddleLast.X);
      FPanY := FPanY + (FMouseY - FMiddleLast.Y);
      FMiddleLast := Point(FMouseX, FMouseY);
      Invalidate;
    end
    else
      FMiddleDown := False;
  end;

  if RayLib.IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
    HandleRightClick(FMouseX, FMouseY);

  if FDragMode <> dmNone then
  begin
    if ((FDragButton = mbRight) and RayLib.IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) or
       ((FDragButton <> mbRight) and RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT)) then
    begin
      ScreenToPixel(FMouseX, FMouseY, PX, PY);
      PX := ClampInt(PX, 0, FCanvasW - 1);
      PY := ClampInt(PY, 0, FCanvasH - 1);
      if FMouseInCanvas then
        ContinueToolAt(PX, PY);
    end
    else
    begin
      ScreenToPixel(FMouseX, FMouseY, PX, PY);
      PX := ClampInt(PX, 0, FCanvasW - 1);
      PY := ClampInt(PY, 0, FCanvasH - 1);
      EndToolAt(PX, PY);
    end;
  end;

  if FMouseInCanvas then
    RayLib.SetMouseCursor(MOUSE_CURSOR_CROSSHAIR)
  else
    RayLib.SetMouseCursor(MOUSE_CURSOR_DEFAULT);
end;

procedure TSpriteEditor.HandleWheel;
var
  Wheel: Single;
  Factor: Single;
  Ctrl, Shift: Boolean;
begin
  Wheel := RayLib.GetMouseWheelMove;
  if Wheel = 0 then
    Exit;
  if not Focused then
    Exit;

  Ctrl := RayLib.IsKeyDown(KEY_LEFT_CONTROL) or RayLib.IsKeyDown(KEY_RIGHT_CONTROL);
  Shift := RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT);
  if Ctrl then
  begin
    Factor := Power(1.2, Wheel);
    ZoomAt(FMouseX, FMouseY, Factor);
  end
  else if Shift then
    FPanX := FPanX + Wheel * 28
  else
    FPanY := FPanY + Wheel * 28;
  Invalidate;
end;

procedure TSpriteEditor.HandleLeftClick(AX, AY: Integer);
var
  I, Idx: Integer;
  A: TSpriteAction;
  Sel: TRect;
  PX, PY: Integer;
  R: TRect;
  C: TColor;
begin
  if not InsideRect(ClientRect, AX, AY) then
    Exit;

  for I := 0 to cSpriteEditorTopButtons - 1 do
    if InsideRect(TopButtonRect(I, cSpriteEditorTopButtons), AX, AY) then
    begin
      case I of
        0: SaveFile;
        1: Undo;
        2: Redo;
        3: MirrorFrame(True);
        4: MirrorFrame(False);
        5: RotateFrame(True);
        6: RotateFrame(False);
        7:
          begin
            FShowGrid := not FShowGrid;
            SetStatus('Grid ' + BoolToStr(FShowGrid, 'on', 'off'));
          end;
      end;
      Exit;
    end;

  for I := 0 to Ord(High(TSpriteTool)) do
    if InsideRect(ToolRect(I), AX, AY) then
    begin
      FTool := TSpriteTool(I);
      SetStatus(ToolName(FTool));
      Exit;
    end;

  if InsideRect(PaletteAreaRect, AX, AY) then
  begin
    for I := 0 to High(FPalette) do
      if InsideRect(PaletteSwatchRect(I), AX, AY) then
      begin
        FFrontColor := FPalette[I];
        SetStatus('Front color ' + ColorToHex(FFrontColor));
        Exit;
      end;
    if InsideRect(FGColorRect, AX, AY) then
    begin
      C := FFrontColor;
      FFrontColor := FBackColor;
      FBackColor := C;
      SetStatus('Front/back colors swapped');
      Exit;
    end;
    if InsideRect(BGColorRect, AX, AY) then
    begin
      FBackColor := clBlank;
      SetStatus('Back color set to transparent');
      Exit;
    end;
    Exit;
  end;

  if InsideRect(ActionsAreaRect, AX, AY) then
  begin
    if InsideRect(ActionAddRect, AX, AY) then
    begin
      AddAction;
      Exit;
    end;
    if InsideRect(ActionDelRect, AX, AY) then
    begin
      DeleteAction;
      Exit;
    end;
    for I := 0 to FActions.Count - 1 do
      if InsideRect(ActionItemRect(I), AX, AY) then
      begin
        SelectAction(I);
        Exit;
      end;
    Exit;
  end;

  if InsideRect(FramesAreaRect, AX, AY) then
  begin
    if InsideRect(FrameAddRect, AX, AY) then
    begin
      AddFrameBlank;
      Exit;
    end;
    if InsideRect(FrameDelRect, AX, AY) then
    begin
      DeleteActiveFrame;
      Exit;
    end;
    if InsideRect(FrameLeftRect, AX, AY) then
    begin
      MoveFrameInAction(-1);
      Exit;
    end;
    if InsideRect(FrameRightRect, AX, AY) then
    begin
      MoveFrameInAction(1);
      Exit;
    end;
    A := ActiveActionObject;
    if A <> nil then
      for I := 0 to A.Frames.Count - 1 do
        if InsideRect(FrameItemRect(I), AX, AY) then
        begin
          SelectFrameInAction(I);
          Exit;
        end;
    Exit;
  end;

  if InsideRect(PropsAreaRect, AX, AY) then
  begin
    if InsideRect(RenameRect, AX, AY) then
    begin
      A := ActiveActionObject;
      if A <> nil then
      begin
        FRenaming := True;
        FRenameBuffer := A.Name;
        SetStatus('Type the action name, Enter to accept');
      end;
      Exit;
    end;
    if InsideRect(LoopRect, AX, AY) then
    begin
      A := ActiveActionObject;
      if A <> nil then
      begin
        PushUndo;
        A.Loop := not A.Loop;
        SetStatus('Loop ' + BoolToStr(A.Loop, 'on', 'off'));
      end;
      Exit;
    end;
    Idx := CurrentFrameIndex;
    if (Idx >= 0) and InsideRect(DurationMinusRect, AX, AY) then
    begin
      PushUndo;
      FFrames[Idx].DurationMs := Max(10, FFrames[Idx].DurationMs - 10);
      SetStatus('Duration ' + IntToStr(FFrames[Idx].DurationMs) + ' ms');
      Exit;
    end;
    if (Idx >= 0) and InsideRect(DurationPlusRect, AX, AY) then
    begin
      PushUndo;
      FFrames[Idx].DurationMs := FFrames[Idx].DurationMs + 10;
      SetStatus('Duration ' + IntToStr(FFrames[Idx].DurationMs) + ' ms');
      Exit;
    end;
    Exit;
  end;

  if InsideRect(CanvasAreaRect, AX, AY) then
  begin
    R := PreviewRect;
    if InsideRect(R, AX, AY) then
      Exit;
    ScreenToPixel(AX, AY, PX, PY);
    if (PX >= 0) and (PY >= 0) and (PX < FCanvasW) and (PY < FCanvasH) then
      BeginToolAt(PX, PY, mbLeft);
    Exit;
  end;
end;

procedure TSpriteEditor.HandleRightClick(AX, AY: Integer);
var
  I: Integer;
  PX, PY: Integer;
begin
  if not InsideRect(ClientRect, AX, AY) then
    Exit;

  if InsideRect(PaletteAreaRect, AX, AY) then
  begin
    for I := 0 to High(FPalette) do
      if InsideRect(PaletteSwatchRect(I), AX, AY) then
      begin
        FBackColor := FPalette[I];
        SetStatus('Back color ' + ColorToHex(FBackColor));
        Exit;
      end;
    Exit;
  end;

  if InsideRect(CanvasAreaRect, AX, AY) then
  begin
    if InsideRect(PreviewRect, AX, AY) then
      Exit;
    ScreenToPixel(AX, AY, PX, PY);
    if (PX >= 0) and (PY >= 0) and (PX < FCanvasW) and (PY < FCanvasH) then
      BeginToolAt(PX, PY, mbRight);
  end;
end;

{ ---------------------------------------------------------------------------
  Keyboard
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.CommitRename;
var
  A: TSpriteAction;
  S: string;
begin
  A := ActiveActionObject;
  if (A <> nil) then
  begin
    S := Trim(FRenameBuffer);
    if S <> '' then
    begin
      PushUndo;
      A.Name := S;
      SetStatus('Action renamed to ' + S);
    end;
  end;
  FRenaming := False;
  FRenameBuffer := '';
  Invalidate;
end;

procedure TSpriteEditor.HandleKeyTool(AKey: TKeyboardKey);
begin
  case AKey of
    KEY_ONE: FTool := stSelect;
    KEY_TWO: FTool := stPixel;
    KEY_THREE: FTool := stPen;
    KEY_FOUR: FTool := stFill;
    KEY_FIVE: FTool := stPick;
    KEY_SIX: FTool := stCircle;
    KEY_SEVEN: FTool := stRect;
    KEY_EIGHT: FTool := stRotate;
  end;
  SetStatus(ToolName(FTool));
end;

procedure TSpriteEditor.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
begin
  inherited;
  if not Visible then
    Exit;

  if FRenaming then
  begin
    case Key of
      KEY_ESCAPE:
        begin
          FRenaming := False;
          FRenameBuffer := '';
          SetStatus('Rename cancelled');
        end;
      KEY_BACKSPACE:
        if Length(FRenameBuffer) > 0 then
          Delete(FRenameBuffer, Length(FRenameBuffer), 1);
      KEY_ENTER:
        CommitRename;
    end;
    Invalidate;
    Exit;
  end;

  if ssCtrl in Shift then
  begin
    case Key of
      KEY_S: SaveFile;
      KEY_Z: if ssShift in Shift then Redo else Undo;
      KEY_Y: Redo;
      KEY_C: CopySelection;
      KEY_X: CutSelection;
      KEY_V: PasteSelection;
      KEY_A: SelectAll;
      KEY_D: AddFrameCopy;
      KEY_INSERT: CopySelection;
    end;
    Invalidate;
    Exit;
  end;

  if ssShift in Shift then
  begin
    case Key of
      KEY_INSERT: PasteSelection;
      KEY_DELETE: DeleteSelection;
      KEY_LEFT: MoveFrameInAction(-1);
      KEY_RIGHT: MoveFrameInAction(1);
    end;
    Invalidate;
    Exit;
  end;

  case Key of
    KEY_INSERT: CopySelection;
    KEY_DELETE:
      if FSelValid then
        DeleteSelection
      else
        DeleteActiveFrame;
    KEY_ESCAPE:
      begin
        FDragMode := dmNone;
        FSelValid := False;
        FRenaming := False;
        SetStatus('Cancelled');
      end;
    KEY_LEFT: SelectFrameInAction(FActiveFrame - 1);
    KEY_RIGHT: SelectFrameInAction(FActiveFrame + 1);
    KEY_UP: SelectAction(FActiveAction - 1);
    KEY_DOWN: SelectAction(FActiveAction + 1);
    KEY_ONE, KEY_TWO, KEY_THREE, KEY_FOUR, KEY_FIVE, KEY_SIX, KEY_SEVEN, KEY_EIGHT:
      HandleKeyTool(Key);
    KEY_G:
      begin
        FShowGrid := not FShowGrid;
        SetStatus('Grid ' + BoolToStr(FShowGrid, 'on', 'off'));
      end;
  end;
  Invalidate;
end;

procedure TSpriteEditor.KeyPress(var Key: TUTF8Char);
begin
  inherited;
  if FRenaming and (Key <> '') then
  begin
    if Ord(Key[1]) >= 32 then
      FRenameBuffer := FRenameBuffer + Key;
    Invalidate;
  end;
end;

{ ---------------------------------------------------------------------------
  Layout rectangles
  --------------------------------------------------------------------------- }

function TSpriteEditor.CanvasAreaRect: TRect;
var
  W, H: Integer;
begin
  W := ClientRect.Width;
  H := ClientRect.Height;
  Result := Rect(cSpriteEditorToolsW, cSpriteEditorToolbarH,
    Max(cSpriteEditorToolsW, W - cSpriteEditorRightW),
    Max(cSpriteEditorToolbarH, H - cSpriteEditorPaletteH));
end;

function TSpriteEditor.ToolsAreaRect: TRect;
var
  H: Integer;
begin
  H := ClientRect.Height;
  Result := Rect(0, cSpriteEditorToolbarH, cSpriteEditorToolsW,
    Max(cSpriteEditorToolbarH, H - cSpriteEditorPaletteH));
end;

function TSpriteEditor.RightAreaRect: TRect;
var
  W, H: Integer;
begin
  W := ClientRect.Width;
  H := ClientRect.Height;
  Result := Rect(Max(0, W - cSpriteEditorRightW), cSpriteEditorToolbarH, W,
    Max(cSpriteEditorToolbarH, H));
end;

function TSpriteEditor.PaletteAreaRect: TRect;
var
  W, H: Integer;
begin
  W := ClientRect.Width;
  H := ClientRect.Height;
  Result := Rect(cSpriteEditorToolsW, Max(0, H - cSpriteEditorPaletteH),
    Max(cSpriteEditorToolsW, W - cSpriteEditorRightW), H);
end;

function TSpriteEditor.ActionsAreaRect: TRect;
var
  R: TRect;
begin
  R := RightAreaRect;
  Result := Rect(R.Left, R.Top, R.Right,
    R.Top + Round((R.Bottom - R.Top) * 0.42));
end;

function TSpriteEditor.FramesAreaRect: TRect;
var
  R: TRect;
begin
  R := RightAreaRect;
  Result := Rect(R.Left, ActionsAreaRect.Bottom, R.Right,
    R.Top + Round((R.Bottom - R.Top) * 0.68));
end;

function TSpriteEditor.PropsAreaRect: TRect;
var
  R: TRect;
begin
  R := RightAreaRect;
  Result := Rect(R.Left, FramesAreaRect.Bottom, R.Right, R.Bottom);
end;

function TSpriteEditor.ToolRect(AIndex: Integer): TRect;
begin
  Result := Rect(6, cSpriteEditorToolbarH + 8 + AIndex * 46, 46,
    cSpriteEditorToolbarH + 8 + AIndex * 46 + 40);
end;

function TSpriteEditor.TopButtonRect(AIndex, ACount: Integer): TRect;
const
  BtnW = 30;
  Gap = 4;
var
  R: TRect;
begin
  R.Right := ClientRect.Right - 8 - (ACount - 1 - AIndex) * (BtnW + Gap);
  R.Left := R.Right - BtnW;
  R.Top := 7;
  R.Bottom := 35;
  Result := R;
end;

function TSpriteEditor.ActionItemRect(AIndex: Integer): TRect;
var
  R: TRect;
begin
  R := ActionsAreaRect;
  Result := Rect(R.Left + 8, R.Top + 28 + AIndex * 22, R.Right - 8,
    R.Top + 46 + AIndex * 22);
end;

function TSpriteEditor.FrameItemRect(AIndex: Integer): TRect;
var
  R: TRect;
begin
  R := FramesAreaRect;
  Result := Rect(R.Left + 8 + AIndex * 48, R.Top + 28,
    R.Left + 52 + AIndex * 48, R.Top + 72);
end;

function TSpriteEditor.PaletteSwatchRect(AIndex: Integer): TRect;
var
  R: TRect;
  Cols, SwW, SwH, Gap, Col, Row: Integer;
begin
  R := PaletteAreaRect;
  SwW := 18;
  SwH := 18;
  Gap := 3;
  Cols := (R.Width - 128) div (SwW + Gap);
  if Cols < 1 then
    Cols := 1;
  if Cols > 16 then
    Cols := 16;
  Col := AIndex mod Cols;
  Row := AIndex div Cols;
  Result := Rect(R.Left + 12 + Col * (SwW + Gap),
    R.Top + 24 + Row * (SwH + Gap),
    R.Left + 12 + Col * (SwW + Gap) + SwW,
    R.Top + 24 + Row * (SwH + Gap) + SwH);
end;

function TSpriteEditor.FGColorRect: TRect;
var
  R: TRect;
begin
  R := PaletteAreaRect;
  Result := Rect(R.Right - 116, R.Top + 22, R.Right - 76, R.Top + 62);
end;

function TSpriteEditor.BGColorRect: TRect;
var
  R: TRect;
begin
  R := PaletteAreaRect;
  Result := Rect(R.Right - 68, R.Top + 22, R.Right - 28, R.Top + 62);
end;

function TSpriteEditor.RenameRect: TRect;
var
  R: TRect;
begin
  R := PropsAreaRect;
  Result := Rect(R.Left + 10, R.Top + 28, R.Right - 10, R.Top + 50);
end;

function TSpriteEditor.LoopRect: TRect;
var
  R: TRect;
begin
  R := PropsAreaRect;
  Result := Rect(R.Left + 10, R.Top + 60, R.Left + 24, R.Top + 74);
end;

function TSpriteEditor.DurationMinusRect: TRect;
var
  R: TRect;
begin
  R := PropsAreaRect;
  Result := Rect(R.Left + 10, R.Top + 86, R.Left + 28, R.Top + 104);
end;

function TSpriteEditor.DurationPlusRect: TRect;
var
  R: TRect;
begin
  R := PropsAreaRect;
  Result := Rect(R.Left + 86, R.Top + 86, R.Left + 104, R.Top + 104);
end;

function TSpriteEditor.ActionAddRect: TRect;
var
  R: TRect;
begin
  R := ActionsAreaRect;
  Result := Rect(R.Right - 46, R.Top + 4, R.Right - 28, R.Top + 22);
end;

function TSpriteEditor.ActionDelRect: TRect;
var
  R: TRect;
begin
  R := ActionsAreaRect;
  Result := Rect(R.Right - 24, R.Top + 4, R.Right - 6, R.Top + 22);
end;

function TSpriteEditor.FrameAddRect: TRect;
var
  R: TRect;
begin
  R := FramesAreaRect;
  Result := Rect(R.Right - 70, R.Top + 4, R.Right - 52, R.Top + 22);
end;

function TSpriteEditor.FrameDelRect: TRect;
var
  R: TRect;
begin
  R := FramesAreaRect;
  Result := Rect(R.Right - 48, R.Top + 4, R.Right - 30, R.Top + 22);
end;

function TSpriteEditor.FrameLeftRect: TRect;
var
  R: TRect;
begin
  R := FramesAreaRect;
  Result := Rect(R.Right - 70, R.Top + 24, R.Right - 52, R.Top + 42);
end;

function TSpriteEditor.FrameRightRect: TRect;
var
  R: TRect;
begin
  R := FramesAreaRect;
  Result := Rect(R.Right - 48, R.Top + 24, R.Right - 30, R.Top + 42);
end;

function TSpriteEditor.PreviewRect: TRect;
var
  R: TRect;
begin
  R := CanvasAreaRect;
  Result := Rect(R.Right - 112, R.Top + 10, R.Right - 10, R.Top + 112);
end;

{ ---------------------------------------------------------------------------
  Coordinate mapping
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.GetImageScreenRect(out R: TRect);
var
  Area: TRect;
  W, H: Integer;
begin
  Area := CanvasAreaRect;
  W := Round(FCanvasW * FZoom);
  H := Round(FCanvasH * FZoom);
  R.Left := Area.Left + (Area.Width - W) div 2 + Round(FPanX);
  R.Top := Area.Top + (Area.Height - H) div 2 + Round(FPanY);
  R.Right := R.Left + W;
  R.Bottom := R.Top + H;
end;

procedure TSpriteEditor.ScreenToPixel(SX, SY: Integer; out PX, PY: Integer);
var
  R: TRect;
begin
  GetImageScreenRect(R);
  if FZoom <= 0 then
  begin
    PX := -1;
    PY := -1;
    Exit;
  end;
  PX := Floor((SX - R.Left) / FZoom);
  PY := Floor((SY - R.Top) / FZoom);
end;

procedure TSpriteEditor.ZoomAt(SX, SY: Integer; AFactor: Single);
var
  R, Area: TRect;
  PX, PY, NewZoom: Single;
  CX, CY: Single;
begin
  GetImageScreenRect(R);
  if FZoom <= 0 then
    Exit;
  PX := (SX - R.Left) / FZoom;
  PY := (SY - R.Top) / FZoom;
  NewZoom := ClampFloat(FZoom * AFactor, 1, 40);
  Area := CanvasAreaRect;
  CX := Area.Left + Area.Width / 2;
  CY := Area.Top + Area.Height / 2;
  FZoom := NewZoom;
  FPanX := SX - CX - PX * FZoom + FCanvasW * FZoom / 2;
  FPanY := SY - CY - PY * FZoom + FCanvasH * FZoom / 2;
end;

{ ---------------------------------------------------------------------------
  Per-frame update
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.Update;
var
  DT: Double;
  A: TSpriteAction;
  Dur, Idx: Integer;
begin
  inherited;
  if not Visible then
    Exit;

  DT := RayLib.GetFrameTime();
  if FStatusTimer > 0 then
  begin
    FStatusTimer := FStatusTimer - DT;
    if FStatusTimer <= 0 then
      FStatus := '';
  end;

  A := ActiveActionObject;
  if (A <> nil) and (A.Frames.Count > 0) then
  begin
    FPreviewFrame := ClampInt(FPreviewFrame, 0, A.Frames.Count - 1);
    FPreviewTimer := FPreviewTimer + DT * 1000;
    Idx := A.Frames[FPreviewFrame];
    if (Idx >= 0) and (Idx < FFrames.Count) then
      Dur := FFrames[Idx].DurationMs
    else
      Dur := cSpriteEditorDefaultDuration;
    if Dur <= 0 then
      Dur := cSpriteEditorDefaultDuration;
    while FPreviewTimer >= Dur do
    begin
      FPreviewTimer := FPreviewTimer - Dur;
      if A.Frames.Count <= 1 then
      begin
        FPreviewTimer := 0;
        Break;
      end;
      FPreviewFrame := (FPreviewFrame + 1) mod A.Frames.Count;
      Idx := A.Frames[FPreviewFrame];
      if (Idx >= 0) and (Idx < FFrames.Count) then
        Dur := FFrames[Idx].DurationMs
      else
        Dur := cSpriteEditorDefaultDuration;
      if Dur <= 0 then
        Dur := cSpriteEditorDefaultDuration;
    end;
  end;

  HandleMouse;
  HandleWheel;
  Invalidate;
end;

{ ---------------------------------------------------------------------------
  Painting helpers
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.DrawPanel(ACanvas: TTyroCanvas; const R: TRect;
  const ATitle: string; AIcon: Integer);
var
  HR: TRect;
begin
  ACanvas.FillRectangle(RectangleOf(R), SC(cSpriteEditorPanel));
  ACanvas.DrawRect(R, 1, SC(cSpriteEditorLine));
  HR := Rect(R.Left + 8, R.Top + 3, R.Right - 8, R.Top + 24);
  if AIcon <> Ord(icNone) then
    DrawIcon(ACanvas, Rect(HR.Left, HR.Top + 2, HR.Left + 16, HR.Top + 18),
      AIcon, SC(cSpriteEditorAccent));
  DrawTextC(ACanvas, HR.Left + 21, HR.Top + 1, ATitle,
    SC(cSpriteEditorText));
  ACanvas.DrawLine(HR.Left, HR.Bottom, HR.Right, HR.Bottom,
    SC(cSpriteEditorLine));
end;

procedure TSpriteEditor.DrawButton(ACanvas: TTyroCanvas; const R: TRect;
  const ACaption: string; AActive, AEnabled: Boolean; AIcon: Integer);
var
  Bg, Fg: TColor;
  Hover: Boolean;
  TW: Single;
  TX, TY: Integer;
  IR: TRect;
begin
  Hover := InsideRect(R, FMouseX, FMouseY);
  if not AEnabled then
  begin
    Bg := SC($181B26FF);
    Fg := SC($555B70FF);
  end
  else if AActive then
  begin
    Bg := SC($3B4260FF);
    Fg := SC(cSpriteEditorAccent);
  end
  else if Hover then
  begin
    Bg := SC(cSpriteEditorPanelHi);
    Fg := SC(cSpriteEditorText);
  end
  else
  begin
    Bg := SC($232838FF);
    Fg := SC(cSpriteEditorText);
  end;

  ACanvas.FillRectangleRounded(RectangleOf(R), 0.22, 4, Bg);
  if AActive then
    ACanvas.DrawRectangleRounded(RectangleOf(R), 0.22, 4, 1,
      SC(cSpriteEditorAccent))
  else
    ACanvas.DrawRectangleRounded(RectangleOf(R), 0.22, 4, 1,
      SC(cSpriteEditorLine));
  if (AIcon <> Ord(icNone)) and (ACaption = '') then
  begin
    IR := Rect(R.Left + (R.Right - R.Left - 18) div 2,
      R.Top + (R.Bottom - R.Top - 18) div 2,
      R.Left + (R.Right - R.Left - 18) div 2 + 18,
      R.Top + (R.Bottom - R.Top - 18) div 2 + 18);
    DrawIcon(ACanvas, IR, AIcon, Fg);
  end
  else
  begin
    if AIcon <> Ord(icNone) then
      DrawIcon(ACanvas, Rect(R.Left + 5, R.Top + 4, R.Left + 21, R.Top + 20),
        AIcon, Fg);
    TW := ACanvas.MeasureText(ACaption);
    TX := R.Left + Round((R.Right - R.Left - TW) / 2);
    if AIcon <> Ord(icNone) then
      TX := R.Left + 24;
    TY := R.Top + Round((R.Bottom - R.Top - Res.Font.Height) / 2);
    DrawTextC(ACanvas, TX, TY, ACaption, Fg);
  end;
end;

procedure TSpriteEditor.DrawIcon(ACanvas: TTyroCanvas; const R: TRect;
  AIcon: Integer; AColor: TColor);
var
  CX, CY, S, I: Integer;
  A: Single;

  procedure ArcLine(CXc, CYc, Rad, A1, A2: Single);
  var
    K: Integer;
    PX, PY, NX, NY: Single;
  begin
    PX := CXc + Cos(A1) * Rad;
    PY := CYc + Sin(A1) * Rad;
    for K := 1 to 10 do
    begin
      A := A1 + (A2 - A1) * K / 10;
      NX := CXc + Cos(A) * Rad;
      NY := CYc + Sin(A) * Rad;
      ACanvas.DrawLineF(PX, PY, NX, NY, AColor);
      PX := NX;
      PY := NY;
    end;
  end;

  procedure ArrowHead(X, Y, DX, DY: Single);
  begin
    ACanvas.DrawLineF(X, Y, X - DX * 4 - DY * 3, Y - DY * 4 + DX * 3, AColor);
    ACanvas.DrawLineF(X, Y, X - DX * 4 + DY * 3, Y - DY * 4 - DX * 3, AColor);
  end;

begin
  if (AIcon <= Ord(icNone)) or (AIcon > Ord(High(TSpriteIcon))) then
    Exit;
  CX := (R.Left + R.Right) div 2;
  CY := (R.Top + R.Bottom) div 2;
  S := Min(R.Right - R.Left, R.Bottom - R.Top);
  if S < 6 then
    Exit;
  ACanvas.Pen := 1;

  case TSpriteIcon(AIcon) of
    icSave:
      begin
        ACanvas.DrawRect(Rect(CX - 6, CY - 7, CX + 6, CY + 7), 1, AColor);
        ACanvas.FillRect(CX - 3, CY - 6, 6, 4, AColor);
        ACanvas.FillRect(CX - 4, CY + 1, 8, 5, AColor);
      end;
    icUndo:
      begin
        ArcLine(CX, CY, 7, Pi * 0.15, Pi * 1.45);
        ArrowHead(CX - 6, CY + 2, -1, 0.4);
      end;
    icRedo:
      begin
        ArcLine(CX, CY, 7, Pi * 1.85, Pi * 0.55);
        ArrowHead(CX + 6, CY + 2, 1, 0.4);
      end;
    icMirrorH:
      begin
        ACanvas.DrawLine(CX, CY - 7, CX, CY + 7, AColor);
        ACanvas.FillTriangle(CX - 3, CY, CX - 8, CY - 4, CX - 8, CY + 4, AColor);
        ACanvas.FillTriangle(CX + 3, CY, CX + 8, CY - 4, CX + 8, CY + 4, AColor);
      end;
    icMirrorV:
      begin
        ACanvas.DrawLine(CX - 7, CY, CX + 7, CY, AColor);
        ACanvas.FillTriangle(CX, CY - 3, CX - 4, CY - 8, CX + 4, CY - 8, AColor);
        ACanvas.FillTriangle(CX, CY + 3, CX - 4, CY + 8, CX + 4, CY + 8, AColor);
      end;
    icRotateCW:
      begin
        ArcLine(CX, CY, 7, Pi * 0.9, Pi * 2.1);
        ArrowHead(CX + 5, CY + 6, 1, 0.2);
      end;
    icRotateCCW:
      begin
        ArcLine(CX, CY, 7, Pi * 0.1, Pi * 1.3);
        ArrowHead(CX - 5, CY + 6, -1, 0.2);
      end;
    icGrid:
      begin
        for I := -1 to 1 do
        begin
          ACanvas.DrawLine(CX + I * 5, CY - 7, CX + I * 5, CY + 7, AColor);
          ACanvas.DrawLine(CX - 7, CY + I * 5, CX + 7, CY + I * 5, AColor);
        end;
      end;
    icPlus:
      begin
        ACanvas.DrawLine(CX - 5, CY, CX + 5, CY, AColor);
        ACanvas.DrawLine(CX, CY - 5, CX, CY + 5, AColor);
      end;
    icMinus:
      ACanvas.DrawLine(CX - 5, CY, CX + 5, CY, AColor);
    icClose:
      begin
        ACanvas.DrawLine(CX - 5, CY - 5, CX + 5, CY + 5, AColor);
        ACanvas.DrawLine(CX + 5, CY - 5, CX - 5, CY + 5, AColor);
      end;
    icPlay:
      ACanvas.FillTriangle(CX - 4, CY - 6, CX + 7, CY, CX - 4, CY + 6, AColor);
    icLoop:
      begin
        ArcLine(CX, CY, 6, Pi * 0.2, Pi * 1.6);
        ArcLine(CX, CY, 6, Pi * 1.2, Pi * 2.6);
        ArrowHead(CX + 5, CY - 4, 1, -0.3);
      end;
    icAction:
      begin
        ACanvas.DrawRect(Rect(CX - 7, CY - 6, CX + 7, CY + 6), 1, AColor);
        ACanvas.DrawLine(CX - 3, CY - 2, CX + 4, CY - 2, AColor);
        ACanvas.DrawLine(CX - 3, CY + 2, CX + 4, CY + 2, AColor);
      end;
    icFrame:
      begin
        ACanvas.DrawRect(Rect(CX - 7, CY - 7, CX + 7, CY + 7), 1, AColor);
        ACanvas.DrawLine(CX - 4, CY + 4, CX - 1, CY - 2, AColor);
        ACanvas.DrawLine(CX - 1, CY - 2, CX + 3, CY + 4, AColor);
        ACanvas.FillCircle(CX + 3, CY - 3, 1, AColor);
      end;
    icProps:
      begin
        ACanvas.DrawLine(CX - 6, CY - 4, CX + 6, CY - 4, AColor);
        ACanvas.DrawLine(CX - 6, CY, CX + 6, CY, AColor);
        ACanvas.DrawLine(CX - 6, CY + 4, CX + 6, CY + 4, AColor);
        ACanvas.FillCircle(CX - 3, CY - 4, 2, AColor);
        ACanvas.FillCircle(CX + 2, CY, 2, AColor);
        ACanvas.FillCircle(CX - 1, CY + 4, 2, AColor);
      end;
    icPalette:
      begin
        ACanvas.DrawCircle(CX, CY, 7, AColor);
        ACanvas.FillCircle(CX - 3, CY - 2, 1, AColor);
        ACanvas.FillCircle(CX + 3, CY - 2, 1, AColor);
        ACanvas.FillCircle(CX, CY + 3, 1, AColor);
      end;
    icTools:
      begin
        ACanvas.DrawLine(CX - 5, CY + 5, CX + 4, CY - 4, AColor);
        ACanvas.FillTriangle(CX + 3, CY - 5, CX + 7, CY - 7, CX + 5, CY - 3, AColor);
      end;
  end;
end;

procedure TSpriteEditor.DrawToolIcon(ACanvas: TTyroCanvas; const R: TRect;
  ATool: TSpriteTool; AColor: TColor);
var
  CX, CY: Integer;
  I: Integer;

  procedure ArcLine(CXc, CYc, Rad, A1, A2: Single);
  var
    K: Integer;
    PX, PY, NX, NY: Single;
    A: Single;
  begin
    PX := CXc + Cos(A1) * Rad;
    PY := CYc + Sin(A1) * Rad;
    for K := 1 to 8 do
    begin
      A := A1 + (A2 - A1) * K / 8;
      NX := CXc + Cos(A) * Rad;
      NY := CYc + Sin(A) * Rad;
      ACanvas.DrawLineF(PX, PY, NX, NY, AColor);
      PX := NX;
      PY := NY;
    end;
  end;

begin
  CX := (R.Left + R.Right) div 2;
  CY := (R.Top + R.Bottom) div 2;
  ACanvas.Pen := 1;
  case ATool of
    stSelect:
      begin
        for I := 0 to 2 do
        begin
          ACanvas.DrawLine(CX - 7 + I * 6, CY - 7, CX - 4 + I * 6, CY - 7, AColor);
          ACanvas.DrawLine(CX - 7 + I * 6, CY + 7, CX - 4 + I * 6, CY + 7, AColor);
          ACanvas.DrawLine(CX - 7, CY - 7 + I * 6, CX - 7, CY - 4 + I * 6, AColor);
          ACanvas.DrawLine(CX + 7, CY - 7 + I * 6, CX + 7, CY - 4 + I * 6, AColor);
        end;
        ACanvas.FillTriangle(CX - 1, CY - 1, CX - 1, CY + 7, CX + 6, CY + 3, AColor);
      end;
    stPixel:
      begin
        ACanvas.FillRect(CX - 3, CY - 3, 6, 6, AColor);
        ACanvas.DrawRect(Rect(CX - 5, CY - 5, CX + 5, CY + 5), 1, AColor);
      end;
    stPen:
      begin
        ACanvas.DrawLineF(CX - 6, CY + 6, CX + 3, CY - 3, AColor);
        ACanvas.FillTriangle(CX + 3, CY - 4, CX + 7, CY - 7, CX + 4, CY - 2, AColor);
        ACanvas.DrawLineF(CX - 6, CY + 6, CX - 2, CY + 6, AColor);
      end;
    stFill:
      begin
        ACanvas.DrawRect(Rect(CX - 6, CY - 5, CX + 2, CY + 4), 1, AColor);
        ACanvas.DrawLineF(CX + 2, CY - 2, CX + 6, CY + 2, AColor);
        ACanvas.FillTriangle(CX + 4, CY + 2, CX + 8, CY + 2, CX + 6, CY + 7, AColor);
        ACanvas.FillCircle(CX - 3, CY - 6, 1, AColor);
      end;
    stPick:
      begin
        ACanvas.DrawLineF(CX - 6, CY + 6, CX + 4, CY - 4, AColor);
        ACanvas.DrawCircle(CX + 4, CY - 5, 3, AColor);
        ACanvas.DrawLineF(CX - 6, CY + 6, CX - 2, CY + 6, AColor);
      end;
    stCircle:
      ACanvas.DrawCircle(CX, CY, 6, AColor);
    stRect:
      ACanvas.DrawRect(Rect(CX - 7, CY - 5, CX + 7, CY + 5), 1, AColor);
    stRotate:
      begin
        ArcLine(CX, CY, 6, Pi * 0.9, Pi * 2.1);
        ACanvas.DrawLineF(CX + 5, CY + 5, CX + 7, CY + 2, AColor);
        ACanvas.DrawLineF(CX + 5, CY + 5, CX + 2, CY + 5, AColor);
      end;
  end;
end;

{ ---------------------------------------------------------------------------
  Paint: toolbar and tools
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.PaintToolbar(ACanvas: TTyroCanvas);
var
  W: Integer;
  R: TRect;
  S: string;
  I: Integer;
  Icons: array[0..7] of TSpriteIcon;
  Active: array[0..7] of Boolean;
  Enabled: array[0..7] of Boolean;
begin
  W := ClientRect.Width;
  R := Rect(0, 0, W, cSpriteEditorToolbarH);
  ACanvas.FillGradientV(R, SC($242A3DFF),
    SC($171A24FF));
  ACanvas.DrawLine(0, cSpriteEditorToolbarH - 1, W, cSpriteEditorToolbarH - 1,
    SC(cSpriteEditorLine));

  DrawTextC(ACanvas, 12, 7, 'SPRITE EDITOR', SC(cSpriteEditorAccent));
  S := ExtractFileName(FFileName);
  if S = '' then
    S := 'untitled';
  if FModified then
    S := S + ' *';
  DrawTextC(ACanvas, 12, 22, S, SC(cSpriteEditorDim));

  Icons[0] := icSave;     Active[0] := False; Enabled[0] := FFrames.Count > 0;
  Icons[1] := icUndo;     Active[1] := False; Enabled[1] := FUndo.Count > 0;
  Icons[2] := icRedo;     Active[2] := False; Enabled[2] := FRedo.Count > 0;
  Icons[3] := icMirrorH;  Active[3] := False; Enabled[3] := FFrames.Count > 0;
  Icons[4] := icMirrorV;  Active[4] := False; Enabled[4] := FFrames.Count > 0;
  Icons[5] := icRotateCW; Active[5] := False; Enabled[5] := FFrames.Count > 0;
  Icons[6] := icRotateCCW;Active[6] := False; Enabled[6] := FFrames.Count > 0;
  Icons[7] := icGrid;     Active[7] := FShowGrid; Enabled[7] := True;

  for I := 0 to cSpriteEditorTopButtons - 1 do
    DrawButton(ACanvas, TopButtonRect(I, cSpriteEditorTopButtons), '',
      Active[I], Enabled[I], Ord(Icons[I]));

  DrawTextC(ACanvas, 250, 13, Format('Zoom %d%%   Canvas %dx%d   Frame %d/%d',
    [Round(FZoom * 100), FCanvasW, FCanvasH,
     ClampInt(FActiveFrame + 1, 0, ActionFrameCount),
     ActionFrameCount]), SC(cSpriteEditorDim));
end;

procedure TSpriteEditor.PaintTools(ACanvas: TTyroCanvas);
var
  R: TRect;
  I: Integer;
  TR: TRect;
  Active: Boolean;
  Bg, Fg: TColor;
begin
  R := ToolsAreaRect;
  ACanvas.FillRectangle(RectangleOf(R), SC($191C28FF));
  ACanvas.DrawLine(R.Right - 1, R.Top, R.Right - 1, R.Bottom,
    SC(cSpriteEditorLine));
  DrawIcon(ACanvas, Rect(18, R.Top + 12, 34, R.Top + 28), Ord(icTools),
    SC(cSpriteEditorAccent));

  for I := 0 to Ord(High(TSpriteTool)) do
  begin
    TR := ToolRect(I);
    if TR.Bottom > R.Bottom - 4 then
      Break;
    Active := FTool = TSpriteTool(I);
    if InsideRect(TR, FMouseX, FMouseY) then
      Bg := SC(cSpriteEditorPanelHi)
    else if Active then
      Bg := SC($3B4260FF)
    else
      Bg := SC($232838FF);
    Fg := SC(cSpriteEditorText);
    if Active then
      Fg := SC(cSpriteEditorAccent);
    ACanvas.FillRectangleRounded(RectangleOf(TR), 0.22, 4, Bg);
    ACanvas.DrawRectangleRounded(RectangleOf(TR), 0.22, 4, 1,
      SC(IfThen(Active, cSpriteEditorAccent, cSpriteEditorLine)));
    DrawToolIcon(ACanvas, Rect(TR.Left + 10, TR.Top + 9,
      TR.Right - 10, TR.Bottom - 9), TSpriteTool(I), Fg);
  end;
end;

{ ---------------------------------------------------------------------------
  Paint: canvas, checkerboard, preview, selection
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.PaintChecker(ACanvas: TTyroCanvas; const R: TRect);
const
  Cell = 8;
var
  X, Y: Integer;
  C1, C2: TColor;
begin
  C1 := SC($2A2E3CFF);
  C2 := SC($343949FF);
  ACanvas.FillRect(R, C1);
  Y := R.Top;
  while Y < R.Bottom do
  begin
    X := R.Left;
    while X < R.Right do
    begin
      if (((X - R.Left) div Cell) + ((Y - R.Top) div Cell)) mod 2 = 1 then
        ACanvas.FillRect(Rect(X, Y, Min(X + Cell, R.Right), Min(Y + Cell, R.Bottom)), C2);
      Inc(X, Cell);
    end;
    Inc(Y, Cell);
  end;
end;

procedure TSpriteEditor.PaintSelection(ACanvas: TTyroCanvas);
var
  Sel, R: TRect;
  X, Y, I: Integer;
  C: TColor;
begin
  if not FSelValid then
    Exit;
  Sel := NormalizedSelection;
  if (Sel.Right <= Sel.Left) or (Sel.Bottom <= Sel.Top) then
    Exit;
  GetImageScreenRect(R);
  Sel.Left := R.Left + Round(Sel.Left * FZoom);
  Sel.Top := R.Top + Round(Sel.Top * FZoom);
  Sel.Right := R.Left + Round(Sel.Right * FZoom);
  Sel.Bottom := R.Top + Round(Sel.Bottom * FZoom);
  ACanvas.DrawRect(Sel, 1, clWhite);
  C := clBlack;
  I := 0;
  X := Sel.Left;
  while X < Sel.Right do
  begin
    if (I mod 2) = 0 then
    begin
      ACanvas.FillRect(X, Sel.Top, Min(4, Sel.Right - X), 1, C);
      ACanvas.FillRect(X, Sel.Bottom - 1, Min(4, Sel.Right - X), 1, C);
    end;
    Inc(X, 4);
    Inc(I);
  end;
  I := 0;
  Y := Sel.Top;
  while Y < Sel.Bottom do
  begin
    if (I mod 2) = 0 then
    begin
      ACanvas.FillRect(Sel.Left, Y, 1, Min(4, Sel.Bottom - Y), C);
      ACanvas.FillRect(Sel.Right - 1, Y, 1, Min(4, Sel.Bottom - Y), C);
    end;
    Inc(Y, 4);
    Inc(I);
  end;
end;

procedure TSpriteEditor.PaintPreview(ACanvas: TTyroCanvas; const R: TRect);
var
  Idx, W, H, Scale: Integer;
  Box: TRect;
  Tex: TTexture2D;
  A: TSpriteAction;
  X, Y: Integer;
begin
  ACanvas.FillRectangle(RectangleOf(R), SC($11131BFF));
  ACanvas.DrawRect(R, 1, SC(cSpriteEditorLine));
  DrawTextC(ACanvas, R.Left + 6, R.Top + 4, 'PREVIEW',
    SC(cSpriteEditorAccent));

  Box := Rect(R.Left + 6, R.Top + 20, R.Right - 6, R.Bottom - 6);
  PaintChecker(ACanvas, Box);

  A := ActiveActionObject;
  if (A <> nil) and (A.Frames.Count > 0) then
    Idx := A.Frames[ClampInt(FPreviewFrame, 0, A.Frames.Count - 1)]
  else
    Idx := CurrentFrameIndex;
  if (Idx < 0) or (Idx >= FFrames.Count) then
    Exit;

  W := FFrames[Idx].Image.Width;
  H := FFrames[Idx].Image.Height;
  if (W <= 0) or (H <= 0) then
    Exit;
  Scale := Min((Box.Width - 8) div W, (Box.Height - 8) div H);
  if Scale < 1 then
    Scale := 1;
  X := Box.Left + (Box.Width - W * Scale) div 2;
  Y := Box.Top + (Box.Height - H * Scale) div 2;
  Tex := FrameTexture(Idx);
  if Tex.ID <> 0 then
    RayLib.DrawTexturePro(Tex, RectangleOf(0, 0, Tex.Width, Tex.Height),
      RectangleOf(ACanvas.Origin.X + X, ACanvas.Origin.Y + Y,
        W * Scale, H * Scale), Vector2Of(0, 0), 0, clWhite);
end;

procedure TSpriteEditor.PaintCanvas(ACanvas: TTyroCanvas);
var
  Area, R, P: TRect;
  Idx: Integer;
  Tex: TTexture2D;
  X, Y: Integer;
  PX, PY: Integer;
  Status: string;
  C: TColor;
  DX, DY, Rad: Single;
begin
  Area := CanvasAreaRect;
  ACanvas.FillRectangle(RectangleOf(Area), SC($101219FF));
  //subtle dot texture so the work area does not look flat
  X := Area.Left + 8;
  while X < Area.Right do
  begin
    Y := Area.Top + 8;
    while Y < Area.Bottom do
    begin
      ACanvas.DrawPixel(X, Y, SC($1A1D28FF));
      Inc(Y, 16);
    end;
    Inc(X, 16);
  end;

  GetImageScreenRect(R);
  PaintChecker(ACanvas, R);
  ACanvas.DrawRect(R, 1, SC(cSpriteEditorLine));

  Idx := CurrentFrameIndex;
  if Idx >= 0 then
  begin
    Tex := FrameTexture(Idx);
    if Tex.ID <> 0 then
      RayLib.DrawTexturePro(Tex, RectangleOf(0, 0, Tex.Width, Tex.Height),
        RectangleOf(ACanvas.Origin.X + R.Left, ACanvas.Origin.Y + R.Top,
          R.Right - R.Left, R.Bottom - R.Top), Vector2Of(0, 0), 0, clWhite);
  end;

  if FShowGrid and (FZoom >= 6) then
  begin
    C := SC($24FFFFFF);
    for X := 1 to FCanvasW - 1 do
      ACanvas.FillRect(R.Left + Round(X * FZoom), R.Top, 1, R.Bottom - R.Top, C);
    for Y := 1 to FCanvasH - 1 do
      ACanvas.FillRect(R.Left, R.Top + Round(Y * FZoom), R.Right - R.Left, 1, C);
  end;

  PaintSelection(ACanvas);

  if (FDragMode = dmShape) and FMouseInCanvas then
  begin
    if FDragButton = mbRight then
      C := FBackColor
    else
      C := FFrontColor;
    if C.RGBA.Alpha = 0 then
      C := clWhite;
    if FTool = stCircle then
    begin
      DX := FDragEnd.X - FDragStart.X;
      DY := FDragEnd.Y - FDragStart.Y;
      Rad := Sqrt(DX * DX + DY * DY);
      ACanvas.DrawCircle(R.Left + Round((FDragStart.X + 0.5) * FZoom),
        R.Top + Round((FDragStart.Y + 0.5) * FZoom), Round(Rad * FZoom), C);
    end
    else if FTool = stRect then
    begin
      P := Rect(R.Left + Round(Min(FDragStart.X, FDragEnd.X) * FZoom),
        R.Top + Round(Min(FDragStart.Y, FDragEnd.Y) * FZoom),
        R.Left + Round((Max(FDragStart.X, FDragEnd.X) + 1) * FZoom),
        R.Top + Round((Max(FDragStart.Y, FDragEnd.Y) + 1) * FZoom));
      ACanvas.DrawRect(P, 1, C);
    end;
  end;

  if FMouseInCanvas and (FDragMode = dmNone) then
  begin
    ScreenToPixel(FMouseX, FMouseY, PX, PY);
    if (PX >= 0) and (PY >= 0) and (PX < FCanvasW) and (PY < FCanvasH) then
    begin
      P := Rect(R.Left + Round(PX * FZoom), R.Top + Round(PY * FZoom),
        R.Left + Round((PX + 1) * FZoom), R.Top + Round((PY + 1) * FZoom));
      ACanvas.DrawRect(P, 1, SC(cSpriteEditorAccent));
    end;
  end;

  PaintPreview(ACanvas, PreviewRect);

  Status := FStatus;
  if Status = '' then
    Status := 'Tool: ' + ToolName(FTool) + '   Left: front   Right: back   Ctrl+Wheel: zoom';
  ACanvas.FillRect(Rect(Area.Left, Area.Bottom - 20, Area.Right, Area.Bottom),
    SC($101219C0));
  DrawTextC(ACanvas, Area.Left + 8, Area.Bottom - 18, Status,
    SC(cSpriteEditorDim));
end;

{ ---------------------------------------------------------------------------
  Paint: actions, frames, properties, palette
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.PaintActions(ACanvas: TTyroCanvas);
var
  R, IR: TRect;
  I: Integer;
  A: TSpriteAction;
  Bg: TColor;
begin
  R := ActionsAreaRect;
  DrawPanel(ACanvas, R, 'ACTIONS', Ord(icAction));
  DrawButton(ACanvas, ActionAddRect, '', False, True, Ord(icPlus));
  DrawButton(ACanvas, ActionDelRect, '', False, FActions.Count > 1, Ord(icClose));

  for I := 0 to FActions.Count - 1 do
  begin
    IR := ActionItemRect(I);
    if IR.Top >= R.Bottom - 2 then
      Break;
    A := FActions[I];
    if I = FActiveAction then
      Bg := SC($343B58FF)
    else if InsideRect(IR, FMouseX, FMouseY) then
      Bg := SC(cSpriteEditorPanelHi)
    else
      Bg := SC($1A1D2AFF);
    ACanvas.FillRectangleRounded(RectangleOf(IR), 0.18, 3, Bg);
    if I = FActiveAction then
      ACanvas.FillRect(IR.Left, IR.Top + 2, 3, IR.Bottom - IR.Top - 4,
        SC(cSpriteEditorAccent));
    DrawTextC(ACanvas, IR.Left + 9, IR.Top + 3, A.Name,
      SC(cSpriteEditorText));
    DrawTextC(ACanvas, IR.Right - 8 - Round(ACanvas.MeasureText(IntToStr(A.Frames.Count))),
      IR.Top + 3, IntToStr(A.Frames.Count), SC(cSpriteEditorDim));
  end;
end;

procedure TSpriteEditor.PaintFrames(ACanvas: TTyroCanvas);
var
  R, IR, BR: TRect;
  I, Idx, W, H, Scale: Integer;
  A: TSpriteAction;
  Tex: TTexture2D;
  C: TColor;
begin
  R := FramesAreaRect;
  DrawPanel(ACanvas, R, 'FRAMES', Ord(icFrame));
  DrawButton(ACanvas, FrameAddRect, '', False, True, Ord(icPlus));
  DrawButton(ACanvas, FrameDelRect, '', False, FFrames.Count > 1, Ord(icClose));
  DrawButton(ACanvas, FrameLeftRect, '<', False, ActionFrameCount > 1);
  DrawButton(ACanvas, FrameRightRect, '>', False, ActionFrameCount > 1);

  A := ActiveActionObject;
  if A = nil then
    Exit;
  for I := 0 to A.Frames.Count - 1 do
  begin
    IR := FrameItemRect(I);
    if IR.Left >= R.Right - 4 then
      Break;
    Idx := A.Frames[I];
    if (Idx < 0) or (Idx >= FFrames.Count) then
      Continue;
    BR := Rect(IR.Left + 2, IR.Top + 2, IR.Right - 2, IR.Bottom - 2);
    PaintChecker(ACanvas, BR);
    Tex := FrameTexture(Idx);
    if Tex.ID <> 0 then
    begin
      W := FFrames[Idx].Image.Width;
      H := FFrames[Idx].Image.Height;
      Scale := Min((BR.Width - 4) div Max(1, W), (BR.Height - 4) div Max(1, H));
      if Scale < 1 then
        Scale := 1;
      RayLib.DrawTexturePro(Tex, RectangleOf(0, 0, Tex.Width, Tex.Height),
        RectangleOf(ACanvas.Origin.X + BR.Left + (BR.Width - W * Scale) div 2,
          ACanvas.Origin.Y + BR.Top + (BR.Height - H * Scale) div 2,
          W * Scale, H * Scale), Vector2Of(0, 0), 0, clWhite);
    end;
    if I = FActiveFrame then
      C := SC(cSpriteEditorAccent)
    else
      C := SC(cSpriteEditorLine);
    ACanvas.DrawRect(IR, 1, C);
    ACanvas.FillRect(IR.Left + 1, IR.Bottom - 12, 18, 11,
      SC($B0000000));
    DrawTextC(ACanvas, IR.Left + 3, IR.Bottom - 11, IntToStr(I + 1),
      SC(cSpriteEditorText));
  end;
end;

procedure TSpriteEditor.PaintProperties(ACanvas: TTyroCanvas);
var
  R, RR, CR: TRect;
  A: TSpriteAction;
  Idx: Integer;
  S: string;
  C: TColor;
begin
  R := PropsAreaRect;
  DrawPanel(ACanvas, R, 'PROPERTIES', Ord(icProps));
  A := ActiveActionObject;

  RR := RenameRect;
  ACanvas.FillRectangle(RectangleOf(RR), SC($11131BFF));
  if FRenaming then
    C := SC(cSpriteEditorAccent)
  else
    C := SC(cSpriteEditorLine);
  ACanvas.DrawRect(RR, 1, C);
  if A <> nil then
  begin
    if FRenaming then
      S := FRenameBuffer
    else
      S := A.Name;
    DrawTextC(ACanvas, RR.Left + 5, RR.Top + 5, S,
      SC(cSpriteEditorText));
    if FRenaming and (Round(RayLib.GetTime() * 2) mod 2 = 0) then
      ACanvas.FillRect(RR.Left + 6 + Round(ACanvas.MeasureText(S)), RR.Top + 5,
        1, RR.Bottom - RR.Top - 10, SC(cSpriteEditorAccent));
  end
  else
    DrawTextC(ACanvas, RR.Left + 5, RR.Top + 5, 'No action',
      SC(cSpriteEditorDim));

  CR := LoopRect;
  ACanvas.FillRect(CR, SC($11131BFF));
  ACanvas.DrawRect(CR, 1, SC(cSpriteEditorLine));
  if (A <> nil) and A.Loop then
  begin
    ACanvas.DrawLine(CR.Left + 3, CR.Top + 7, CR.Left + 6, CR.Top + 10,
      SC(cSpriteEditorGood));
    ACanvas.DrawLine(CR.Left + 6, CR.Top + 10, CR.Right - 3, CR.Top + 3,
      SC(cSpriteEditorGood));
  end;
  DrawTextC(ACanvas, CR.Right + 6, CR.Top, 'Loop animation',
    SC(cSpriteEditorText));

  Idx := CurrentFrameIndex;
  DrawButton(ACanvas, DurationMinusRect, '', False, Idx >= 0, Ord(icMinus));
  DrawButton(ACanvas, DurationPlusRect, '', False, Idx >= 0, Ord(icPlus));
  if Idx >= 0 then
    S := IntToStr(FFrames[Idx].DurationMs) + ' ms'
  else
    S := '-';
  DrawTextC(ACanvas, DurationMinusRect.Right + 6, DurationMinusRect.Top + 3, S,
    SC(cSpriteEditorText));

  DrawTextC(ACanvas, R.Left + 10, DurationMinusRect.Bottom + 8,
    Format('Canvas %dx%d', [FCanvasW, FCanvasH]),
    SC(cSpriteEditorDim));
  if Idx >= 0 then
    DrawTextC(ACanvas, R.Left + 10, DurationMinusRect.Bottom + 22,
      Format('Global frame %d of %d', [Idx + 1, FFrames.Count]),
      SC(cSpriteEditorDim));
  DrawTextC(ACanvas, R.Left + 10, DurationMinusRect.Bottom + 36,
    'Ctrl+S save   Ctrl+Z undo',
    SC($60677EFF));
end;

procedure TSpriteEditor.PaintPalette(ACanvas: TTyroCanvas);
var
  R, SR, FR, BR: TRect;
  I: Integer;
  C: TColor;
  S: string;
begin
  R := PaletteAreaRect;
  ACanvas.FillRectangle(RectangleOf(R), SC($181B26FF));
  ACanvas.DrawLine(R.Left, R.Top, R.Right, R.Top,
    SC(cSpriteEditorLine));
  DrawTextC(ACanvas, R.Left + 12, R.Top + 5, 'PALETTE',
    SC(cSpriteEditorAccent));
  DrawTextC(ACanvas, R.Left + 104, R.Top + 5, 'Left click = front   Right click = back',
    SC(cSpriteEditorDim));

  for I := 0 to High(FPalette) do
  begin
    SR := PaletteSwatchRect(I);
    if SR.Bottom > R.Bottom - 2 then
      Break;
    C := FPalette[I];
    if C.RGBA.Alpha = 0 then
    begin
      PaintChecker(ACanvas, SR);
      ACanvas.DrawLine(SR.Left, SR.Top, SR.Right - 1, SR.Bottom - 1,
        SC(cSpriteEditorBad));
    end
    else
      ACanvas.FillRect(SR, C);
    if C = FFrontColor then
      ACanvas.DrawRect(SR, 2, clWhite)
    else if C = FBackColor then
      ACanvas.DrawRect(SR, 2, SC(cSpriteEditorAccent2))
    else
      ACanvas.DrawRect(SR, 1, SC($0B0D13FF));
  end;

  FR := FGColorRect;
  BR := BGColorRect;
  PaintChecker(ACanvas, FR);
  if FFrontColor.RGBA.Alpha > 0 then
    ACanvas.FillRect(FR, FFrontColor);
  ACanvas.DrawRect(FR, 2, clWhite);
  DrawTextC(ACanvas, FR.Left + 4, FR.Bottom + 3, 'FG',
    SC(cSpriteEditorText));

  PaintChecker(ACanvas, BR);
  if FBackColor.RGBA.Alpha > 0 then
    ACanvas.FillRect(BR, FBackColor);
  ACanvas.DrawRect(BR, 2, SC(cSpriteEditorAccent2));
  DrawTextC(ACanvas, BR.Left + 4, BR.Bottom + 3, 'BG',
    SC(cSpriteEditorText));

  S := ColorToHex(FFrontColor) + ' / ' + ColorToHex(FBackColor);
  DrawTextC(ACanvas, R.Right - 8 - Round(ACanvas.MeasureText(S)), R.Top + 5, S,
    SC(cSpriteEditorDim));
end;

{ ---------------------------------------------------------------------------
  Main paint
  --------------------------------------------------------------------------- }

procedure TSpriteEditor.DoPaint(ACanvas: TTyroCanvas);
begin
  inherited;
  if (ClientRect.Width <= 0) or (ClientRect.Height <= 0) then
    Exit;
  PaintCanvas(ACanvas);
  PaintTools(ACanvas);
  PaintActions(ACanvas);
  PaintFrames(ACanvas);
  PaintProperties(ACanvas);
  PaintPalette(ACanvas);
  PaintToolbar(ACanvas);
  ACanvas.DrawRect(Rect(0, 0, ClientRect.Width, ClientRect.Height), 1,
    SC($0B0D13FF));
end;

end.
