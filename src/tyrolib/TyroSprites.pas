unit TyroSprites;
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections,
  RayLib, RayClasses,
  TyroScripts, TyroClasses;

const
  cSpriteInvalid: Integer = 0;

type
  { TSprite }

  TSprite = class(TObject)
  public
    Handle: Integer;
    Texture: TTexture2D;
    Name: string;
    X: single;
    Y: single;
    Angle: single;
    Scale: single;
    Visible: boolean;
    constructor Create(AHandle: Integer; ATexture: TTexture2D; const AName: string);
  end;

  { TSpriteStore }

  TSprites = class(TObject)
  private
    FLock: TCriticalSection;
    FItems: TDictionary<Integer, TSprite>;
    FNextHandle: Integer;
    function GetCount: integer;
    function GetItems(Index: Integer): TSprite;
  public
    constructor Create;
    destructor Destroy; override;
    // Add a loaded texture to the store, returns a handle
    function Add(ATexture: TTexture2D; const AName: string = ''): integer;
    function FindByName(const AName: string): integer;

    function GetTexture(Handle: integer): TTexture2D;
    procedure SetPosition(Handle: integer; X, Y: single);
    procedure SetAngle(Handle: integer; Angle: single);
    procedure SetScale(Handle: integer; Scale: single);
    procedure SetVisible(Handle: integer; AVisible: boolean);
    function GetX(Handle: integer): single;
    function GetY(Handle: integer): single;
    function GetAngle(Handle: integer): single;
    function GetScale(Handle: integer): single;
    function GetVisible(Handle: integer): boolean;
    function GetWidth(Handle: integer): integer;
    function GetHeight(Handle: integer): integer;

    // Draw all valid sprites (called in the engine's draw loop)
    procedure DrawAll;
    // Draw a single sprite (queued from Lua)
    procedure DrawSprite(Handle: integer; ACanvas: TTyroCanvas; AX, AY: single; AAngle: single; AScale: single; ATint: TColor);
    property Count: integer read GetCount;
    property Items[Index: Integer]: TSprite read GetItems; default;
  end;

  { TLoadSpriteObject }

  TLoadSpriteObject = class(TQueueObject)
  private
    FFileName: string;
    FName: string;
    FHandleResult: integer;
  public
    constructor Create(AFileName: string; const AName: string);
    destructor Destroy; override;
    procedure DoExecute; override;
    property FileName: string read FFileName;
    property Name: string read FName;
    property HandleResult: integer read FHandleResult;
  end;

  { TDrawSpriteObject }

  {TDrawSpriteObject = class(TDrawObject)
  private
    FSpriteStore: TSprites;
    FHandle: integer;
    fX, fY: single;
    fAngle: single;
    fScale: single;
    fTint: TColor;
  public
    constructor Create(ASpriteStore: TSprites; AHandle: integer; AX, AY: single; ACanvas: TTyroCanvas; AAngle: single; AScale: single; ATint: TColor);
    procedure DoExecute; override;
  end;}

implementation

uses
  TyroEngines;

{ TSprite }

constructor TSprite.Create(AHandle: Integer; ATexture: TTexture2D; const AName: string);
begin
  inherited Create;
  Handle := AHandle;
  Texture := ATexture;
  Name := AName;
  X := 0;
  Y := 0;
  Angle := 0;
  Scale := 1.0;
  Visible := True;
end;

{ TSprites }

constructor TSprites.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FItems := TDictionary<Integer, TSprite>.Create;
  FNextHandle := 1;
end;

destructor TSprites.Destroy;
var
  Sprite: TSprite;
begin
  for Sprite in FItems.Values do
  begin
    RayLib.UnloadTexture(Sprite.Texture);
  end;
  FItems.Free;
  FLock.Free;
  inherited;
end;

function TSprites.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TSprites.GetItems(Index: Integer): TSprite;
begin
  Result := FItems[Index];
end;

function TSprites.Add(ATexture: TTexture2D; const AName: string): integer;
begin
  Result := cSpriteInvalid;
  FLock.Enter;
  try
    if (ATexture.id > 0) then
    begin
      FItems.Add(FNextHandle, TSprite.Create(FNextHandle, ATexture, AName));
      Result := FNextHandle;
      Inc(FNextHandle);
    end;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetTexture(Handle: integer): TTexture2D;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Texture
    else
    begin
      Result.ID := 0;
      Result.Width := 0;
      Result.Height := 0;
      Result.Mipmaps := 1;
      Result.Format := 0;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetPosition(Handle: integer; X, Y: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
    begin
      Sprite.X := X;
      Sprite.Y := Y;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetAngle(Handle: integer; Angle: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Angle := Angle;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetScale(Handle: integer; Scale: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Scale := Scale;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetVisible(Handle: integer; AVisible: boolean);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Visible := AVisible;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetX(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.X
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetY(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Y
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetAngle(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Angle
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetScale(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Scale
    else
      Result := 1;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetVisible(Handle: integer): boolean;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Visible
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetWidth(Handle: integer): integer;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Texture.width
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetHeight(Handle: integer): integer;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Texture.height
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.FindByName(const AName: string): integer;
var
  Sprite: TSprite;
begin
  Result := cSpriteInvalid;
  FLock.Enter;
  try
    for Sprite in FItems.Values do
      if (Sprite.Name = AName) then
      begin
        Result := Sprite.Handle;
        Exit;
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.DrawAll;
var
  Sprite: TSprite;
  Pos: TVector2;
begin
  FLock.Enter;
  try
    for Sprite in FItems.Values do
    begin
      if Sprite.Visible and (Sprite.Texture.id > 0) then
      begin
        Pos := TVector2.Create(Sprite.X, Sprite.Y);
        RayLib.DrawTextureEx(Sprite.Texture, Pos, Sprite.Angle, Sprite.Scale, clWhite);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.DrawSprite(Handle: integer; ACanvas: TTyroCanvas; AX, AY: single; AAngle: single; AScale: single; ATint: TColor);
var
  Sprite: TSprite;
  Pos: TVector2;
begin
  FLock.Enter;
  try
    if (not FItems.TryGetValue(Handle, Sprite)) then
      Exit;
    if (AX > -1) or (AY > -1) then
    begin
      Sprite.X := AX;
      Sprite.Y := AY;
    end;
    if AAngle >= 0 then
      Sprite.Angle := AAngle;
    if AScale >= 0 then
      Sprite.Scale := AScale;
    Pos := TVector2.Create(Sprite.X, Sprite.Y);
  finally
    FLock.Leave;
  end;
  RayLib.DrawTextureEx(Sprite.Texture, Pos, Sprite.Angle, Sprite.Scale, ATint);
end;

{ TLoadSpriteObject }

constructor TLoadSpriteObject.Create(AFileName: string; const AName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FName := AName;
  FHandleResult := cSpriteInvalid;
  EventNeeded;;
end;

destructor TLoadSpriteObject.Destroy;
begin
  inherited;
end;

procedure TLoadSpriteObject.DoExecute;
var
  aTexture: TTexture2D;
begin
  aTexture := RayLib.LoadTexture(PUTF8Char(FFileName));
  if aTexture.id > 0 then
    FHandleResult := Main.Sprites.Add(aTexture, FName)
  else
  begin
    if IsConsole then
      WriteLn('Sprite not loaded: ' + FFileName);
    FHandleResult := 0;
  end;
end;

{ TDrawSpriteObject }

{constructor TDrawSpriteObject.Create(ASpriteStore: TSprites; AHandle: integer; AX, AY: single; ACanvas: TTyroCanvas; AAngle: single; AScale: single; ATint: TColor);
begin
  inherited Create(ACanvas);
  FSpriteStore := ASpriteStore;
  fHandle := AHandle;
  fX := AX;
  fY := AY;
  fAngle := AAngle;
  fScale := AScale;
  fTint := ATint;
end;

procedure TDrawSpriteObject.DoExecute;
begin
  if not Assigned(FSpriteStore) then
    Exit;
  FSpriteStore.DrawSprite(fHandle, Canvas, fX, fY, fAngle, fScale, fTint);
end;}

end.
