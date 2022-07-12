unit TyroClasses;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey 
 *
 *}

{$ifdef FPC}
{$mode delphi}
{$H+}{$M+}
{$endif}

interface

uses
  Classes, SysUtils, Types, SyncObjs,
  mnClasses, mnUtils, mnLogs,
  RayLib, RayClasses,
  Melodies, TyroSounds;

const
  clFrenchSkyBlue: TRGBAColor = (Red: $77; Green: $B5; Blue: $FE; Alpha: $FF);
  clCornflowerBlue: TRGBAColor = (Red: $5d; Green: $9f; Blue: $f9; Alpha: $FF);

type

  TTyroCanvas = class;

  { TTyroImage }

  TTyroImage = class(TObject)
  private
    FImage: TImage;
  protected
    property Image: TImage read FImage;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function LoadTexture: TTexture2D;
    procedure Circle(X, Y, R: Integer; Color: TColor);
  end;

  { TTyroCanvas }

  TTyroCanvas = class abstract(TObject)
  private
    FOriginX, FOriginY: Integer;
    FLastX, FLastY: Integer;
    FPenColor: TColor;
    FBackColor: TColor;
    FPenWidth: Integer;
    FWidth, FHeight: Integer;
    function GetPenAlpha: Byte;
    procedure SetPenAlpha(AValue: Byte);
    procedure SetHeight(AValue: Integer);
    procedure SetPenColor(AValue: TColor);
    procedure SetPenWidth(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetBackColor(const Value: TColor);
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;

    procedure SetOrigin(X, Y: Integer);
    procedure ResetOrigin;
    procedure BeginDraw; virtual;
    procedure EndDraw; virtual;
    procedure PostDraw; virtual;

    procedure DrawCircle(X, Y, R: Integer; Color: TColor; Fill: Boolean = false);
    procedure DrawText(X, Y: Integer; S: utf8string; Color: TColor);
    procedure DrawPixel(X, Y: Integer; Color: TColor);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: TColor); overload;
    procedure DrawLine(X1, Y1, X2, Y2: Integer); overload;
    procedure DrawLine(X1, Y1, X2, Y2: Single; Color: TColor); overload;
    procedure DrawLine(X1, Y1, X2, Y2: Single); overload;

    procedure DrawLineTo(X2, Y2: Integer; Color: TColor);
    procedure DrawRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor; Fill: Boolean); overload;
    procedure DrawRectangle(ARectangle: TRect; Color: TColor; Fill: Boolean); overload;
    procedure DrawRectangle(X: Single; Y: Single; AWidth: Single; AHeight: Single; Color: TColor; Fill: Boolean); overload;
    procedure DrawRectangle(ARectangle: TRectangle; Color: TColor; Fill: Boolean); overload;

    procedure DrawRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean); overload;
    procedure DrawRect(ARectangle: TRect; Color: TColor; Fill: Boolean); overload;

    procedure Clear;
    property PenAlpha: Byte read GetPenAlpha write SetPenAlpha;
    property PenWidth: Integer read FPenWidth write SetPenWidth;
    property PenColor: TColor read FPenColor write SetPenColor;
    property BackColor: TColor read FBackColor write SetBackColor;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TTyroTextureCanvas = class(TTyroCanvas)
  private
    FTextureMode: Boolean;
    FTexture: TRenderTexture2D;
  public
    constructor Create(AWidth, AHeight: Integer; ATextureMode: Boolean = False);
    destructor Destroy; override;
    procedure BeginDraw; override;
    procedure EndDraw; override;
    procedure PostDraw; override;
    property Texture: TRenderTexture2D read FTexture;
  end;

  TTyroResource = class(TmnNamedObject)
  public
    ResType: string;
    ResData: rawbytestring;
    constructor Create(const AResName, AResType: string; const AResData: rawbytestring);
  end;

  TTyroResources = class(TmnNamedObjectList<TTyroResource>)
  public
    Font: TRayFont;
    WorkSpace: utf8string;
    function Find(const ResName, ResType: string): TTyroResource; overload;
    procedure Load; virtual;
    procedure Add(const ResName, ResType: string; const ResData: rawbytestring); overload;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  function IntToColor(I: integer): TColor;
  function ColorToInt(C: TColor): integer;

const
  ScreenCharWidth = 40;
  ScreenCharHeight = 30;
  ScreenFontSize = 16;
  ScreenWidth: Integer = ScreenCharWidth * ScreenFontSize;
  ScreenHeight: Integer = ScreenCharHeight * ScreenFontSize;
  cFramePerSeconds = 60;

var
  Resources: TTyroResources = nil;
  Lock: TCriticalSection = nil;

implementation

uses
  minibidi;

function IntToColor(I: Integer): TColor;
begin
  Result.RGBA.Red := I and $ff;
  I := I shr 8;
  Result.RGBA.Green := I and $ff;
  I := I shr 8;
  Result.RGBA.Blue := I and $ff;
  I := I shr 8;
  Result.RGBA.Alpha := I and $ff;
end;

function ColorToInt(C: TColor): Integer;
begin
  Result := C.RGBA.Alpha;
  Result := Result shl 8;
  Result := Result or C.RGBA.Blue;
  Result := Result shl 8;
  Result := Result or C.RGBA.Green;
  Result := Result shl 8;
  Result := Result or C.RGBA.Red;
end;

{ TTyroImage }

constructor TTyroImage.Create(AWidth, AHeight: Integer);
begin
  //FImage := GetTextureData(FTexture.texture);
  FImage := GenImageColor(AWidth, AHeight, clRed);
end;

destructor TTyroImage.Destroy;
begin
  UnloadImage(FImage);
  inherited Destroy;
end;

function TTyroImage.LoadTexture: TTexture2D;
begin
  Result := LoadTextureFromImage(FImage);
end;

procedure TTyroImage.Circle(X, Y, R: Integer; Color: TColor);
begin
  ImageDrawCircle(FImage, X, Y, R, Color);
end;

{ TTyroCanvas }

procedure TTyroCanvas.SetBackColor(const Value: TColor);
begin
  FBackColor := Value;
end;

procedure TTyroCanvas.SetHeight(AValue: Integer);
begin
  if FHeight =AValue then Exit;
  FHeight :=AValue;
end;

procedure TTyroCanvas.SetPenColor(AValue: TColor);
begin
  FPenColor := AValue;
end;

procedure TTyroCanvas.SetPenWidth(AValue: Integer);
begin
  if FPenWidth =AValue then
    Exit;
  FPenWidth :=AValue;
end;

function TTyroCanvas.GetPenAlpha: Byte;
begin
  Result := FPenColor.RGBA.Alpha;
end;

procedure TTyroCanvas.SetPenAlpha(AValue: Byte);
begin
  FPenColor.RGBA.Alpha := AValue;
end;

procedure TTyroCanvas.SetWidth(AValue: Integer);
begin
  if FWidth =AValue then Exit;
  FWidth :=AValue;
end;

constructor TTyroCanvas.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FPenWidth := 1;
  FPenColor := clBlack;
  //FBackgroundColor := TColor.CreateRGBA($0892D0FF);
  //FBackgroundColor := TColor.CreateRGBA($B0C4DEFF); //Light Steel Blue
//  FBackColor := TColor.CreateRGBA($77B5FEFF); //French Sky Blue
  FBackColor := clFrenchSkyBlue;

  {BeginTextureMode(FTexture);
  ClearBackground(BackColor);
  DrawText(10, 10, 'Ready!');
  EndTextureMode();}
end;

destructor TTyroCanvas.Destroy;
begin
  inherited;
end;

procedure TTyroCanvas.SetOrigin(X, Y: Integer);
begin
  FOriginX := X;
  FOriginY := Y;
end;

procedure TTyroCanvas.ResetOrigin;
begin
  FOriginX := 0;
  FOriginY := 0;
end;

procedure TTyroCanvas.BeginDraw;
begin
end;

procedure TTyroCanvas.EndDraw;
begin
end;

procedure TTyroCanvas.DrawCircle(X, Y, R: Integer; Color: TColor; Fill: Boolean = false);
begin
  if Fill then
    RayLib.DrawCircle(X + FOriginX, Y + FOriginY, R, Color)
  else
    RayLib.DrawCircleLines(X + FOriginX, Y + FOriginY, R, Color);
  FLastX := X;
  FLastY := Y;
end;

procedure TTyroCanvas.DrawRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor; Fill: Boolean);
begin
  if Fill then
    RayLib.DrawRectangle(X + FOriginX, Y + FOriginY, AWidth, AHeight, Color);
  RayLib.DrawRectangleLinesEx(RectangleOf(X + FOriginX, Y + FOriginY, AWidth, AHeight), PenWidth, Color);
  FLastX := X + AWidth;
  FLastY := Y + AHeight;
end;

procedure TTyroCanvas.DrawRectangle(ARectangle: TRect; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(ARectangle.Left, ARectangle.Top, ARectangle.Right - ARectangle.Left, ARectangle.Bottom - ARectangle.Top, Color, Fill);
end;

procedure TTyroCanvas.DrawRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(ALeft, ATop, ARight - ALeft, ABottom - ATop, Color, Fill);
end;

procedure TTyroCanvas.DrawRect(ARectangle: TRect; Color: TColor; Fill: Boolean);
begin
  DrawRect(ARectangle.Left, ARectangle.Top, ARectangle.Right, ARectangle.Bottom, Color, Fill);
end;

procedure TTyroCanvas.DrawRectangle(ARectangle: TRectangle; Color: TColor; Fill: Boolean);
begin
  if Fill then
    RayLib.DrawRectangleRec(ARectangle, Color);

  RayLib.DrawRectangleLinesEx(ARectangle, PenWidth, Color);
end;

procedure TTyroCanvas.DrawRectangle(X, Y, AWidth, AHeight: Single; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(RectangleOf(X, Y, AWidth, AHeight), Color, Fill)
end;

procedure TTyroCanvas.DrawText(X, Y: Integer; S: utf8string; Color: TColor);
begin
  RayLib.DrawTextEx(Resources.Font.Data, PUTF8Char(S), Vector2Of(x + FOriginX, y + FOriginY), Resources.Font.Height, 0, Color);
end;

procedure TTyroCanvas.DrawPixel(X, Y: Integer; Color: TColor);
begin
  RayLib.DrawPixel(X + FOriginX, Y + FOriginY, Color);
  FLastX := X;
  FLastY := Y;
end;

procedure TTyroCanvas.DrawLine(X1, Y1, X2, Y2: Integer; Color: TColor);
begin
  RayLib.DrawLineEx(Vector2Of(X1 + FOriginX, Y1 + FOriginY), Vector2Of(X2 + FOriginX, Y2 + FOriginY), PenWidth, Color);
  FLastX := X2;
  FLastY := Y2;
end;

procedure TTyroCanvas.DrawLine(X1, Y1, X2, Y2: Integer);
begin
  DrawLine(X1, Y1, X2, Y2, PenColor);
end;

procedure TTyroCanvas.DrawLine(X1, Y1, X2, Y2: Single);
begin
  DrawLine(X1, Y1, X2, Y2, PenColor);
end;

procedure TTyroCanvas.DrawLine(X1, Y1, X2, Y2: Single; Color: TColor);
begin
  DrawLineEx(TVector2.Create(X1 + FOriginX, Y1 + FOriginY), TVector2.Create(X2 + FOriginX, Y2 + FOriginY), PenWidth, Color);
end;

procedure TTyroCanvas.DrawLineTo(X2, Y2: Integer; Color: TColor);
begin
  DrawLine(FLastX + FOriginX, FLastY + FOriginY, X2 + FOriginX, Y2 + FOriginY, Color);
end;

procedure TTyroCanvas.PostDraw;
begin
end;

procedure TTyroCanvas.Clear;
begin
  ClearBackground(FBackColor);
end;

{ TTyroTextureCanvas }

procedure TTyroTextureCanvas.BeginDraw;
begin
  inherited;
  if FTextureMode then
    RayLib.BeginTextureMode(FTexture);
end;

constructor TTyroTextureCanvas.Create(AWidth, AHeight: Integer; ATextureMode: Boolean);
begin
  inherited Create(AWidth, AHeight);
  FTextureMode := ATextureMode;
  if FTextureMode then
    FTexture := LoadRenderTexture(Width, Height);
end;

destructor TTyroTextureCanvas.Destroy;
begin
  if FTextureMode then
    UnloadRenderTexture(FTexture);
  inherited;
end;

procedure TTyroTextureCanvas.PostDraw;
begin
  inherited;
  if FTextureMode then
    with FTexture do
      RayLib.DrawTextureRec(Texture, TRectangle.Create(0, 0, Texture.Width, -Texture.height), Vector2Of(0, 0), clWhite);
end;

procedure TTyroTextureCanvas.EndDraw;
begin
  if FTextureMode then
    RayLib.EndTextureMode();
  inherited;
end;

{ TTyroResources }

procedure TTyroResources.Add(const ResName, ResType: string; const ResData: rawbytestring);
begin
  Add(TTyroResource.Create(ResName, ResType, ResData));
end;

constructor TTyroResources.Create;
begin
  inherited;
  Font := TRayFont.Create;
end;

destructor TTyroResources.Destroy;
begin
  FreeAndNil(Font);
  if Resources = Self then
    Resources := nil;
  inherited;
end;

function TTyroResources.Find(const ResName, ResType: string): TTyroResource;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, ResName) and SameText(Items[i].ResType, ResType)  then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TTyroResources.Load;
var
  res: TTyroResource;
begin
  {$include 'font.inc'}
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'alpha_beta.png'));
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'Terminess-Bold.ttf'));
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'dejavu.fnt'));
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'DejaVuSansMono-Bold.ttf'));
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'terminus.ttf'));
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'fonts/Chroma.png'));
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'fonts/alpha_beta.png'));
  //Font := LoadFont(PUTF8Char(Main.WorkSpace + 'fonts/ChiKareGo2.ttf'));
  //Font := LoadFontEx(PUTF8Char(Main.WorkSpace + 'fonts/terminus.ttf'), 12, nil, 255);
  //Font := LoadFontEx(PUTF8Char(Main.WorkSpace + 'fonts/AnonymousPro-Regular.ttf'), 12, nil, 255);
  //Font := LoadFont(PUTF8Char('computer_pixel.fon.ttf'));
  //Font := LoadFontEx(PUTF8Char(Main.WorkSpace + 'fonts/tahoma.ttf'), ScreenFontSize, nil, $FFFF); //Good for arabic but it take huge memory

  res := Find('font', 'png');
  if res <> nil then
  begin
    Font.LoadFromString(res.ResData, 16);
    Font.Height := Font.Data.BaseSize * 2;
  end
  else if SysUtils.FileExists(WorkSpace + 'font.png') then
  begin
    Font.LoadFromFile(WorkSpace + 'font.png');
    Font.Height := Font.Data.BaseSize * 2;
  end
  else
  begin
    Font.LoadDefault;
  end;

  SetTextureFilter(Font.Data.texture, TEXTURE_FILTER_POINT);
end;

{ TTyroResource }


{ TTyroResource }

constructor TTyroResource.Create(const AResName, AResType: string; const AResData: rawbytestring);
begin
  Name := AResName;
  ResType := AResType;
  ResData := AResData;
end;

initialization
  Lock := TCriticalSection.Create;
finalization
  FreeAndNil(Lock);
end.

