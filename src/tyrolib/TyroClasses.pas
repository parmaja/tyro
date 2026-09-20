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
  mnClasses, mnUtils, mnLogs, mnConfigs,
  RayLib, RayClasses,
  Melodies, TyroSounds;

const
  clFrenchSkyBlue: TRGBAColor = (Red: $77; Green: $B5; Blue: $FE; Alpha: $FF);
  clCornflowerBlue: TRGBAColor = (Red: $5d; Green: $9f; Blue: $f9; Alpha: $FF);

  // Built-in post-processing shaders (GLSL 330 fragment sources).
  // All share two uniforms:
  //   value - a 0..1 effect parameter (water height, glow boundary, intensity...)
  //   area  - optional rectangle {x, y, w, h} in canvas pixels (y from the top)
  //           that restricts where the effect applies (w <= 0 = full canvas)
  cShaderPreamble =
    '#version 330' + #10 +
    'uniform vec2 resolution;' + #10 +
    'uniform float time;' + #10 +
    'uniform float value;' + #10 +
    'uniform vec4 area;' + #10 +
    'uniform sampler2D texture0;' + #10 +
    'uniform vec4 colDiffuse;' + #10 +
    'in vec2 fragTexCoord;' + #10 +
    'in vec4 fragColor;' + #10 +
    'out vec4 finalColor;' + #10;

  // Common region setup:
  //   px      canvas pixel position (y grows upward, 0 = bottom)
  //   localY  0..1 height within the area measured from its bottom (or canvas)
  //   soft    soft mask of the area rectangle (1 inside, fades on the border)
  cShaderRegion =
    'vec2 px = fragTexCoord * resolution;' + #10 +
    'float localY = fragTexCoord.y;' + #10 +
    'float soft = 1.0;' + #10 +
    'if (area.z > 0.0)' + #10 +
    '{' + #10 +
    '    vec2 d = abs(px - (area.xy + area.zw * 0.5)) - area.zw * 0.5;' + #10 +
    '    float dist = max(d.x, d.y);' + #10 +
    '    soft = 1.0 - smoothstep(0.0, 3.0, dist);' + #10 +
    '    localY = clamp((px.y - area.y) / area.w, 0.0, 1.0);' + #10 +
    '}' + #10;

  cWaterEffectShader =
    cShaderPreamble +
    'void main()' + #10 +
    '{' + #10 +
    cShaderRegion +
    '    // water fills the bottom of the canvas/area; value = water surface height' + #10 +
    '    float waterTop = clamp(value, 0.001, 1.0);' + #10 +
    '    float region = 1.0 - smoothstep(max(0.0, waterTop - 0.22), waterTop, localY);' + #10 +
    '' + #10 +
    '    // horizontal wave displacement, growing closer to the surface' + #10 +
    '    vec2 tc = fragTexCoord;' + #10 +
    '    float wave = 0.0;' + #10 +
    '    wave += sin(localY * 40.0 + time * 2.2) * 0.012;' + #10 +
    '    wave += sin(localY * 25.0 - time * 1.6) * 0.009;' + #10 +
    '    wave += sin(localY * 12.0 + time * 0.9) * 0.006;' + #10 +
    '    tc.x += wave * region * soft;' + #10 +
    '' + #10 +
    '    vec4 color = texture(texture0, tc);' + #10 +
    '' + #10 +
    '    // blue water tint over the region' + #10 +
    '    vec4 water = vec4(0.10, 0.35, 0.75, 1.0);' + #10 +
    '    color.rgb = mix(color.rgb, color.rgb * 0.5 + water.rgb * 0.6, region * 0.45 * soft);' + #10 +
    '' + #10 +
    '    // foam line around the water surface' + #10 +
    '    float band = 1.0 - smoothstep(0.0, 0.035, abs(localY - waterTop));' + #10 +
    '    float foam = (0.5 + 0.5 * sin(tc.x * 50.0 + time * 3.0)) * (0.5 + 0.5 * sin(tc.x * 31.0 - time * 2.3));' + #10 +
    '    color.rgb += vec3(0.95, 0.98, 1.0) * band * foam * 0.4 * soft;' + #10 +
    '' + #10 +
    '    finalColor = color;' + #10 +
    '}';

  cGlowEffectShader =
    cShaderPreamble +
    'void main()' + #10 +
    '{' + #10 +
    cShaderRegion +
    '    vec2 texelSize = 1.0 / resolution;' + #10 +
    '    vec4 color = texture(texture0, fragTexCoord);' + #10 +
    '' + #10 +
    '    // small blurred sample' + #10 +
    '    vec4 blur = vec4(0.0);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2(-2.0, -2.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2( 0.0, -2.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2( 2.0, -2.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2(-2.0,  0.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2( 0.0,  0.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2( 2.0,  0.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2(-2.0,  2.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2( 0.0,  2.0) * texelSize);' + #10 +
    '    blur += texture(texture0, fragTexCoord + vec2( 2.0,  2.0) * texelSize);' + #10 +
    '    blur /= 9.0;' + #10 +
    '' + #10 +
    '    // bright pixels leak light into the blur; value limits the region height' + #10 +
    '    float bright = dot(color.rgb, vec3(0.2126, 0.7152, 0.0722));' + #10 +
    '    float pulse = 0.75 + 0.25 * sin(time * 2.0);' + #10 +
    '    vec3 glow = blur.rgb * bright * bright * pulse * 1.8;' + #10 +
    '    float region = 1.0 - smoothstep(value - 0.02, value, localY);' + #10 +
    '    glow *= region * soft;' + #10 +
    '' + #10 +
    '    finalColor = vec4(color.rgb + glow, color.a);' + #10 +
    '}';

  cGrayEffectShader =
    cShaderPreamble +
    'void main()' + #10 +
    '{' + #10 +
    cShaderRegion +
    '    vec4 color = texture(texture0, fragTexCoord);' + #10 +
    '    float lum = dot(color.rgb, vec3(0.299, 0.587, 0.114));' + #10 +
    '    vec3 gray = mix(color.rgb, vec3(lum), clamp(value, 0.0, 1.0));' + #10 +
    '    finalColor = mix(color, vec4(gray, color.a), soft);' + #10 +
    '}';

  cSepiaEffectShader =
    cShaderPreamble +
    'void main()' + #10 +
    '{' + #10 +
    cShaderRegion +
    '    vec4 color = texture(texture0, fragTexCoord);' + #10 +
    '    vec3 sep = vec3(' + #10 +
    '        dot(color.rgb, vec3(0.393, 0.769, 0.189)),' + #10 +
    '        dot(color.rgb, vec3(0.349, 0.686, 0.168)),' + #10 +
    '        dot(color.rgb, vec3(0.272, 0.534, 0.131)));' + #10 +
    '    vec3 outC = mix(color.rgb, sep, clamp(value, 0.0, 1.0));' + #10 +
    '    finalColor = mix(color, vec4(outC, color.a), soft);' + #10 +
    '}';

  cInvertEffectShader =
    cShaderPreamble +
    'void main()' + #10 +
    '{' + #10 +
    cShaderRegion +
    '    vec4 color = texture(texture0, fragTexCoord);' + #10 +
    '    vec3 inv = mix(color.rgb, 1.0 - color.rgb, clamp(value, 0.0, 1.0));' + #10 +
    '    finalColor = mix(color, vec4(inv, color.a), soft);' + #10 +
    '}';

  cVignetteEffectShader =
    cShaderPreamble +
    'void main()' + #10 +
    '{' + #10 +
    cShaderRegion +
    '    vec4 color = texture(texture0, fragTexCoord);' + #10 +
    '    float dist = distance(fragTexCoord, vec2(0.5, 0.5));' + #10 +
    '    float vig = 1.0 - smoothstep(0.25, 0.75, dist) * clamp(value, 0.0, 1.0);' + #10 +
    '    vec3 dark = color.rgb * vig;' + #10 +
    '    finalColor = mix(color, vec4(dark, color.a), soft);' + #10 +
    '}';

  cPixelateEffectShader =
    cShaderPreamble +
    'void main()' + #10 +
    '{' + #10 +
    cShaderRegion +
    '    vec4 color = texture(texture0, fragTexCoord);' + #10 +
    '    float block = clamp(value, 0.002, 1.0) * 16.0;' + #10 +
    '    vec2 cells = resolution / block;' + #10 +
    '    vec2 q = floor(fragTexCoord * cells) / cells;' + #10 +
    '    finalColor = mix(color, texture(texture0, q), soft);' + #10 +
    '}';

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

  TTyroEffect = (fxNone, fxWater, fxGlow, fxGray, fxSepia, fxInvert, fxVignette, fxPixelate, fxCustom);

  TTyroCanvas = class abstract(TObject)
  private
    FOriginX, FOriginY: Integer;
    FLastX, FLastY: Integer;
    FPenColor: TColor;
    FBackColor: TColor;
    FPenSize: Integer;
    FWidth, FHeight: Integer;
    function GetPenAlpha: Byte;
    procedure SetPenAlpha(AValue: Byte);
    procedure SetHeight(AValue: Integer);
    procedure SetPenColor(AValue: TColor);
    procedure SetPenSize(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetBackColor(const Value: TColor);
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;

    procedure SetOrigin(X, Y: Integer);
    procedure ResetOrigin;
    procedure BeginDraw; virtual;
    procedure EndDraw; virtual;
    procedure PostDraw(AX: Integer = 0; AY: Integer = 0); virtual;
    procedure Resize(AWidth, AHeight: Integer); virtual;

    procedure DrawCircle(X, Y, R: Integer; Color: TColor; Fill: Boolean = false);
    procedure DrawText(X, Y: Integer; S: utf8string; Color: TColor); overload;
    procedure DrawText(X, Y: Single; S: utf8string; Color: TColor); overload;
    procedure DrawPixel(X, Y: Integer; Color: TColor);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: TColor); overload;
    procedure DrawLine(X1, Y1, X2, Y2: Integer); overload;
    procedure DrawLineF(X1, Y1, X2, Y2: Single; Color: TColor); overload;
    procedure DrawLineF(X1, Y1, X2, Y2: Single); overload;

    procedure DrawLineTo(X2, Y2: Integer; Color: TColor);
    procedure FillRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor); overload;
    procedure FillRectangle(Rect: TRect; Color: TColor); overload;
    procedure DrawRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor; Fill: Boolean); overload;
    procedure DrawRectangle(ARectangle: TRect; Color: TColor; Fill: Boolean); overload;
    procedure DrawRectangle(X: Single; Y: Single; AWidth: Single; AHeight: Single; Color: TColor; Fill: Boolean); overload;
    procedure DrawRectangle(ARectangle: TRectangle; Color: TColor; Fill: Boolean); overload;

    procedure FillRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor); overload;
    procedure DrawRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean); overload;
    procedure DrawRect(ARectangle: TRect; Color: TColor; Fill: Boolean); overload;
    //Only outline
    procedure DrawRect(ARectangle: TRect; Size: Integer; Color: TColor); overload;
    procedure BeginClip(ARectangle: TRect);
    procedure EndClip;

    procedure Clear;
    procedure ClearBackground(const AColor: TColor); virtual;

    //Post-processing shader effect ("water", "glow", "gray", "sepia",
    //"invert", "vignette", "pixelate", "custom", "none")
    procedure SetEffect(const AEffectName: string); virtual;
    procedure LoadCustomEffect(const AFileName: string); virtual;
    function GetEffectName: string; virtual;
    procedure SetEffectValue(AValue: Single); virtual;
    function GetEffectValue: Single; virtual;
    procedure SetEffectArea(const AArea: TRectangle); virtual;
    function GetEffectArea: TRectangle; virtual;

    property PenAlpha: Byte read GetPenAlpha write SetPenAlpha;
    property PenSize: Integer read FPenSize write SetPenSize;
    property PenColor: TColor read FPenColor write SetPenColor;
    //Maybe TextColor
    property BackColor: TColor read FBackColor write SetBackColor;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TTyroTextureCanvas = class(TTyroCanvas)
  private
    FTextureMode: Boolean;
    FTexture: TRenderTexture2D;
    FEffect: TTyroEffect;
    FShader: TShader;
    FShaderTimeLoc: Integer;
    FShaderResLoc: Integer;
    FShaderValueLoc: Integer;
    FShaderAreaLoc: Integer;
    FEffectValue: Single;
    FValueExplicit: Boolean;
    FEffectArea: TRectangle;
    FAreaExplicit: Boolean;
    FCustomShaderFile: string;
    procedure LoadEffect(ACode: rawbytestring; AEffect: TTyroEffect);
    procedure UnloadEffect;
    procedure SetValueUniform;
    procedure SetAreaUniform;
  public
    constructor Create(AWidth, AHeight: Integer; ATextureMode: Boolean = False);
    destructor Destroy; override;
    procedure BeginDraw; override;
    procedure EndDraw; override;
    procedure PostDraw(AX: Integer = 0; AY: Integer = 0); override;
    procedure Resize(AWidth, AHeight: Integer); override;
    procedure SetEffect(const AEffectName: string); override;
    procedure LoadCustomEffect(const AFileName: string); override;
    function GetEffectName: string; override;
    procedure SetEffectValue(AValue: Single); override;
    function GetEffectValue: Single; override;
    procedure SetEffectArea(const AArea: TRectangle); override;
    function GetEffectArea: TRectangle; override;
    property Texture: TRenderTexture2D read FTexture;
  end;

  TTyroResource = class(TmnNamedObject)
  public
    ResType: string;
    ResData: rawbytestring;
    constructor Create(const AResName, AResType: string; const AResData: rawbytestring);
  end;

  { TTyroResources }

  TTyroResources = class(TmnNamedObjectList<TTyroResource>)
  protected
    procedure LoadConfig;
  public
    Font: TRayFont;
    Config: TConfFile;
    WorkSpace: utf8string;
    CurrentDirectory: string;
    function GuessFileName(const FileName: string; InDirectory: string = ''): string;
    function Find(const ResName, ResType: string): TTyroResource; overload;
    procedure Load; virtual;
    procedure Add(const ResName, ResType: string; const ResData: rawbytestring); overload;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  function StrToColor(Value: String): TColor;
  function IntToColor(I: Integer): TColor;
  function ColorToInt(C: TColor): Integer;

const
  ScreenCharWidth = 40;
  ScreenCharHeight = 30;
  ScreenFontSize = 16;
  ScreenWidth: Integer = ScreenCharWidth * ScreenFontSize;
  ScreenHeight: Integer = ScreenCharHeight * ScreenFontSize;

var
  Resources: TTyroResources = nil;
  Lock: TCriticalSection = nil;

const
  cFramePerSeconds: Integer = 60;

implementation

uses
  minibidi;

function StrToColor(Value: String): TColor;
var
  s: string;
  v: Integer;
begin
  Result := clBlack;
  s := Trim(Value);
  if s = '' then
    Exit;
  if s[1] = '#' then
    s := Copy(s, 2, MaxInt);
  if Length(s) = 3 then
    s := s[1] + s[1] + s[2] + s[2] + s[3] + s[3];
  if Length(s) = 6 then
  begin
    v := StrToIntDef('$' + s, -1);
    if v >= 0 then
    begin
      Result.RGBA.Red := (v shr 16) and $FF;
      Result.RGBA.Green := (v shr 8) and $FF;
      Result.RGBA.Blue := v and $FF;
      Result.RGBA.Alpha := $FF;
      Exit;
    end;
  end;
  if SameText(Value, 'black') then
    Result := clBlack
  else if SameText(Value, 'white') then
    Result := clWhite
  else if SameText(Value, 'gray') or SameText(Value, 'grey') then
    Result := clGray
  else if SameText(Value, 'lightgray') then
    Result := clLightgray
  else if SameText(Value, 'darkgray') then
    Result := clDarkGray
  else if SameText(Value, 'red') then
    Result := clRed
  else if SameText(Value, 'maroon') then
    Result := clMaroon
  else if SameText(Value, 'green') then
    Result := clGreen
  else if SameText(Value, 'lime') then
    Result := clLime
  else if SameText(Value, 'darkgreen') then
    Result := clDarkgreen
  else if SameText(Value, 'blue') then
    Result := clBlue
  else if SameText(Value, 'skyblue') then
    Result := clSkyBlue
  else if SameText(Value, 'darkblue') then
    Result := clDarkblue
  else if SameText(Value, 'yellow') then
    Result := clYellow
  else if SameText(Value, 'gold') then
    Result := clGold
  else if SameText(Value, 'orange') then
    Result := clOrange
  else if SameText(Value, 'pink') then
    Result := clPink
  else if SameText(Value, 'purple') then
    Result := clPurple
  else if SameText(Value, 'violet') then
    Result := clViolet
  else if SameText(Value, 'darkpurple') then
    Result := clDarkpurple
  else if SameText(Value, 'brown') then
    Result := clBrown
  else if SameText(Value, 'darkbrown') then
    Result := clDarkbrown
  else if SameText(Value, 'beige') then
    Result := clBeige
  else if SameText(Value, 'magenta') then
    Result := clMagenta
  else if SameText(Value, 'raywhite') then
    Result := clRayWhite;
end;

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

procedure TTyroCanvas.SetPenSize(AValue: Integer);
begin
  if FPenSize =AValue then
    Exit;
  FPenSize :=AValue;
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
  FPenSize := 1;
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
  RayLib.DrawRectangleLinesEx(RectangleOf(X + FOriginX, Y + FOriginY, AWidth, AHeight), PenSize, Color);
  FLastX := X + AWidth;
  FLastY := Y + AHeight;
end;

procedure TTyroCanvas.DrawRectangle(ARectangle: TRect; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(ARectangle.Left, ARectangle.Top, ARectangle.Width, ARectangle.Height, Color, Fill);
end;

procedure TTyroCanvas.DrawRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(ALeft, ATop, ARight - ALeft, ABottom - ATop, Color, Fill);
end;

procedure TTyroCanvas.DrawRect(ARectangle: TRect; Color: TColor; Fill: Boolean);
begin
  DrawRect(ARectangle.Left, ARectangle.Top, ARectangle.Right, ARectangle.Bottom, Color, Fill);
end;

procedure TTyroCanvas.DrawRect(ARectangle: TRect; Size: Integer; Color: TColor);
begin
  RayLib.DrawRectangleLinesEx(RectangleOf(ARectangle.Left + FOriginX, ARectangle.Top + FOriginY, ARectangle.Width-Size, ARectangle.Height-Size), Size, Color);
  FLastX := FOriginX + ARectangle.Right;
  FLastY := FOriginY + ARectangle.Bottom;
end;

procedure TTyroCanvas.BeginClip(ARectangle: TRect);
begin
  BeginScissorMode(ARectangle.Left + FOriginX, ARectangle.Top + FOriginY, ARectangle.Width, ARectangle.Height);
end;

procedure TTyroCanvas.EndClip;
begin
  EndScissorMode;
end;

procedure TTyroCanvas.DrawRectangle(ARectangle: TRectangle; Color: TColor; Fill: Boolean);
begin
  if Fill then
    RayLib.DrawRectangleRec(ARectangle, Color);

  RayLib.DrawRectangleLinesEx(ARectangle, PenSize, Color);
end;

procedure TTyroCanvas.FillRect(ALeft: Integer; ATop: Integer; ARight: Integer; ABottom: Integer; Color: TColor);
begin
  FillRectangle(ALeft, ATop, ARight - ALeft, ABottom - ATop, Color);
end;

procedure TTyroCanvas.DrawRectangle(X: Single; Y: Single; AWidth: Single;
  AHeight: Single; Color: TColor; Fill: Boolean);
begin
  DrawRectangle(RectangleOf(X, Y, AWidth, AHeight), Color, Fill)
end;

procedure TTyroCanvas.DrawText(X, Y: Integer; S: utf8string; Color: TColor);
begin
  RayLib.DrawTextEx(Resources.Font.Data, PUTF8Char(S), Vector2Of(x + FOriginX, y + FOriginY), Resources.Font.Height, 0, Color);
end;

procedure TTyroCanvas.DrawText(X, Y: Single; S: utf8string; Color: TColor);
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
  RayLib.DrawLineEx(Vector2Of(X1 + FOriginX, Y1 + FOriginY), Vector2Of(X2 + FOriginX, Y2 + FOriginY), PenSize, Color);
  FLastX := X2;
  FLastY := Y2;
end;

procedure TTyroCanvas.DrawLine(X1, Y1, X2, Y2: Integer);
begin
  DrawLine(X1, Y1, X2, Y2, PenColor);
end;

procedure TTyroCanvas.DrawLineF(X1, Y1, X2, Y2: Single);
begin
  DrawLineF(X1, Y1, X2, Y2, PenColor);
end;

procedure TTyroCanvas.DrawLineF(X1, Y1, X2, Y2: Single; Color: TColor);
begin
  DrawLineEx(TVector2.Create(X1 + FOriginX, Y1 + FOriginY), TVector2.Create(X2 + FOriginX, Y2 + FOriginY), PenSize, Color);
end;

procedure TTyroCanvas.DrawLineTo(X2, Y2: Integer; Color: TColor);
begin
  DrawLine(FLastX + FOriginX, FLastY + FOriginY, X2 + FOriginX, Y2 + FOriginY, Color);
end;

procedure TTyroCanvas.FillRectangle(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer; Color: TColor);
begin
  RayLib.DrawRectangle(X + FOriginX, Y + FOriginY, AWidth, AHeight, Color);
  FLastX := FOriginX + X + AWidth;
  FLastY := FOriginY + Y + AHeight;
end;

procedure TTyroCanvas.FillRectangle(Rect: TRect; Color: TColor);
begin
  RayLib.DrawRectangle(Rect.Left + FOriginX, Rect.Top + FOriginY, Rect.Width, Rect.Height, Color);
  FLastX := FOriginX + Rect.Right;
  FLastY := FOriginY + Rect.Bottom;
end;

procedure TTyroCanvas.PostDraw(AX: Integer = 0; AY: Integer = 0);
begin
end;

procedure TTyroCanvas.Resize(AWidth, AHeight: Integer);
begin
  if (FWidth = AWidth) and (FHeight = AHeight) then Exit;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TTyroCanvas.Clear;
begin
  ClearBackground(FBackColor);
end;

procedure TTyroCanvas.ClearBackground(const AColor: TColor);
begin
  RayLib.ClearBackground(AColor);
end;

procedure TTyroCanvas.SetEffect(const AEffectName: string);
begin
end;

procedure TTyroCanvas.LoadCustomEffect(const AFileName: string);
begin
end;

function TTyroCanvas.GetEffectName: string;
begin
  Result := 'none';
end;

procedure TTyroCanvas.SetEffectValue(AValue: Single);
begin
end;

function TTyroCanvas.GetEffectValue: Single;
begin
  Result := 1.0;
end;

procedure TTyroCanvas.SetEffectArea(const AArea: TRectangle);
begin
end;

function TTyroCanvas.GetEffectArea: TRectangle;
begin
  Result := TRectangle.Create(0, 0, 0, 0);
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
  UnloadEffect;
  if FTextureMode then
    UnloadRenderTexture(FTexture);
  inherited;
end;

procedure TTyroTextureCanvas.LoadEffect(ACode: rawbytestring; AEffect: TTyroEffect);
begin
  if ACode <> '' then
  begin
    FShader := RayLib.LoadShaderFromMemory(nil, PUTF8Char(ACode));
    if RayLib.IsShaderValid(FShader) then
    begin
      FShaderTimeLoc := RayLib.GetShaderLocation(FShader, 'time');
      FShaderResLoc := RayLib.GetShaderLocation(FShader, 'resolution');
      FShaderValueLoc := RayLib.GetShaderLocation(FShader, 'value');
      FShaderAreaLoc := RayLib.GetShaderLocation(FShader, 'area');
      FEffect := AEffect;
      SetValueUniform;
      SetAreaUniform;
    end;
  end;
end;

procedure TTyroTextureCanvas.UnloadEffect;
begin
  if FShader.ID <> 0 then
    RayLib.UnloadShader(FShader);
  FShader := Default(TShader);
  FShaderTimeLoc := -1;
  FShaderResLoc := -1;
  FShaderValueLoc := -1;
  FShaderAreaLoc := -1;
  FEffect := fxNone;
end;

procedure TTyroTextureCanvas.SetValueUniform;
var
  v: Single;
begin
  if (FShader.ID <> 0) and RayLib.IsShaderValid(FShader) and (FShaderValueLoc >= 0) then
  begin
    v := GetEffectValue;
    RayLib.SetShaderValue(FShader, FShaderValueLoc, PUTF8Char(Pointer(@v)), Ord(SHADER_UNIFORM_FLOAT));
  end;
end;

procedure TTyroTextureCanvas.SetAreaUniform;
var
  vArea: TRectangle;
  A: TRectangle;
begin
  if (FShader.ID <> 0) and RayLib.IsShaderValid(FShader) and (FShaderAreaLoc >= 0) then
  begin
    A := GetEffectArea;
    //canvas y grows down, shader px.y grows up -> flip vertically
    vArea.x := A.x;
    vArea.y := Height - (A.y + A.height);
    vArea.width := A.width;
    vArea.height := A.height;
    RayLib.SetShaderValue(FShader, FShaderAreaLoc, PUTF8Char(Pointer(@vArea)), Ord(SHADER_UNIFORM_VEC4));
  end;
end;

procedure TTyroTextureCanvas.PostDraw(AX: Integer = 0; AY: Integer = 0);
var
  Shader: TShader;
  vTime: Single;
  vRes: TVector2;
begin
  inherited;
  if FTextureMode then
  begin
    Shader := FShader;
    if (Shader.ID <> 0) and RayLib.IsShaderValid(Shader) then
    begin
      RayLib.BeginShaderMode(Shader);
      vTime := RayLib.GetTime();
      if FShaderTimeLoc >= 0 then
        RayLib.SetShaderValue(Shader, FShaderTimeLoc, PUTF8Char(Pointer(@vTime)), Ord(SHADER_UNIFORM_FLOAT));
      vRes := Vector2Of(Width, Height);
      if FShaderResLoc >= 0 then
        RayLib.SetShaderValue(Shader, FShaderResLoc, PUTF8Char(Pointer(@vRes)), Ord(SHADER_UNIFORM_VEC2));
      if FShaderValueLoc >= 0 then
      begin
        vTime := GetEffectValue;
        RayLib.SetShaderValue(Shader, FShaderValueLoc, PUTF8Char(Pointer(@vTime)), Ord(SHADER_UNIFORM_FLOAT));
      end;
      if FShaderAreaLoc >= 0 then
        SetAreaUniform;
    end;
    with FTexture do
      RayLib.DrawTextureRec(Texture, TRectangle.Create(0, 0, Texture.Width, -Texture.height), Vector2Of(AX, AY), clWhite);
    if (Shader.ID <> 0) and RayLib.IsShaderValid(Shader) then
      RayLib.EndShaderMode;
  end;
end;

procedure TTyroTextureCanvas.SetEffect(const AEffectName: string);
begin
  if SameText(AEffectName, GetEffectName) then
    Exit;
  UnloadEffect;
  if SameText(AEffectName, 'water') then
    LoadEffect(cWaterEffectShader, fxWater)
  else if SameText(AEffectName, 'glow') then
    LoadEffect(cGlowEffectShader, fxGlow)
  else if SameText(AEffectName, 'gray') then
    LoadEffect(cGrayEffectShader, fxGray)
  else if SameText(AEffectName, 'sepia') then
    LoadEffect(cSepiaEffectShader, fxSepia)
  else if SameText(AEffectName, 'invert') then
    LoadEffect(cInvertEffectShader, fxInvert)
  else if SameText(AEffectName, 'vignette') then
    LoadEffect(cVignetteEffectShader, fxVignette)
  else if SameText(AEffectName, 'pixelate') then
    LoadEffect(cPixelateEffectShader, fxPixelate);
end;

function TTyroTextureCanvas.GetEffectName: string;
begin
  case FEffect of
    fxWater: Result := 'water';
    fxGlow: Result := 'glow';
    fxGray: Result := 'gray';
    fxSepia: Result := 'sepia';
    fxInvert: Result := 'invert';
    fxVignette: Result := 'vignette';
    fxPixelate: Result := 'pixelate';
    fxCustom: Result := FCustomShaderFile;
  else
    Result := 'none';
  end;
end;

procedure TTyroTextureCanvas.LoadCustomEffect(const AFileName: string);
var
  s: string;
  fs: TFileStream;
  Code: rawbytestring;
begin
  s := Resources.GuessFileName(AFileName, '');
  if not SysUtils.FileExists(s) then
    Exit;
  fs := TFileStream.Create(s, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Code, fs.Size);
    if fs.Size > 0 then
      fs.Read(Code[1], fs.Size);
  finally
    fs.Free;
  end;
  if Code <> '' then
  begin
    UnloadEffect;
    FCustomShaderFile := AFileName;
    LoadEffect(Code, fxCustom);
  end;
end;

procedure TTyroTextureCanvas.SetEffectValue(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;
  FEffectValue := AValue;
  FValueExplicit := True;
  SetValueUniform;
end;

function TTyroTextureCanvas.GetEffectValue: Single;
begin
  if FValueExplicit then
    Result := FEffectValue
  else
    case FEffect of
      fxWater: Result := 0.34;
      fxVignette: Result := 0.5;
      fxPixelate: Result := 0.06;
      fxGlow: Result := 1.0;
    else
      Result := 1.0;
    end;
end;

procedure TTyroTextureCanvas.SetEffectArea(const AArea: TRectangle);
begin
  FEffectArea := AArea;
  FAreaExplicit := True;
  SetAreaUniform;
end;

function TTyroTextureCanvas.GetEffectArea: TRectangle;
begin
  if FAreaExplicit then
    Result := FEffectArea
  else
    Result := TRectangle.Create(0, 0, Width, Height);
end;

procedure TTyroTextureCanvas.EndDraw;
begin
  if FTextureMode then
    RayLib.EndTextureMode();
  inherited;
end;

procedure TTyroTextureCanvas.Resize(AWidth, AHeight: Integer);
begin
  if (Width = AWidth) and (Height = AHeight) then Exit;
  if FTextureMode then
    UnloadRenderTexture(FTexture);
  inherited Resize(AWidth, AHeight);
  if FTextureMode then
    FTexture := LoadRenderTexture(Width, Height);
end;

{ TTyroResources }

procedure TTyroResources.Add(const ResName, ResType: string; const ResData: rawbytestring);
begin
  Add(TTyroResource.Create(ResName, ResType, ResData));
end;

constructor TTyroResources.Create;
begin
  inherited;
  WorkSpace:= ExtractFilePath(ParamStr(0));
  CurrentDirectory := GetCurrentDir;
  Config := TConfFile.Create;
  Font := TRayFont.Create;
  {$include 'font.inc'}
  LoadConfig;
end;

destructor TTyroResources.Destroy;
begin
  FreeAndNil(Font);
  FreeAndNil(Config);
  if Resources = Self then
    Resources := nil;
  inherited;
end;

procedure TTyroResources.LoadConfig;
const
  cConfigFile = 'tyro.conf';
var
  aFileName: string;
begin
  aFileName := IncludePathDelimiter(WorkSpace) + cConfigFile;
  if not SysUtils.FileExists(aFileName) then
    aFileName := IncludePathDelimiter(CurrentDirectory) + cConfigFile;
  try
    if SysUtils.FileExists(aFileName) then
      Config.LoadFromFile(aFileName);
  except
    on E: Exception do
    begin
      Log.WriteLn('Config: ' + E.Message);
      Exit;
    end;
  end;
end;

function TTyroResources.GuessFileName(const FileName: string; InDirectory: string): string;
var
  s: string;
begin
  if not SysUtils.FileExists(FileName) or (ExtractFileDir(FileName) = '') then
  begin
    if (InDirectory <> '') then
    begin
      s := IncludePathDelimiter(InDirectory) + FileName;
      if SysUtils.FileExists(s) then
        exit(s);
    end;

    if (InDirectory = '') or (not SameFileName(Resources.CurrentDirectory, InDirectory)) then
    begin
      s := IncludePathDelimiter(Resources.CurrentDirectory) + FileName;
      if SysUtils.FileExists(s) then
        exit(s);
    end;

    s := IncludePathDelimiter(Resources.WorkSpace) + 'assets' + PathDelim + FileName;
    if SysUtils.FileExists(s) then
      Exit(s);

    s := IncludePathDelimiter(Resources.WorkSpace) + FileName;
    if SysUtils.FileExists(s) then
      Exit(s);

    Result := FileName;
  end
  else
    Result := FileName;
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
  aFontName: string;
  aFontSize: integer;
begin
  aFontName := Config.Sections.ReadString('font', 'font', '');
  aFontName := ExpandToPath(aFontName, WorkSpace);
  if (aFontName = '') or not (SysUtils.FileExists(aFontName)) then
  begin
    res := Find('font', 'png');
    if res <> nil then
      Font.LoadFromString(res.ResData, 16)
    else if SysUtils.FileExists(WorkSpace + 'font.png') then
      Font.LoadFromFile(WorkSpace + 'font.png')
    else
      Font.LoadDefault;
  end
  else
  begin
    aFontSize := Config.Sections.ReadInt64('font', 'size', 0);
    Font.LoadFromFile(aFontName, aFontSize);
  end;
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

