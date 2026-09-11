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
  { Physics kind of a sprite body }
  TSpriteKind = (
    skDynamic,
    skKinematic,
    skStatic
  );

  { Snapshot of everything TPhysics needs to build/sync a body for one sprite }
  TSpritePhysicsState = record
    Collides: Boolean;
    Kind: TSpriteKind;
    X: single;
    Y: single;
    Angle: single;
    Scale: single;
    Width: single;   // texture width * scale
    Height: single;  // texture height * scale
    Mass: single;
    Friction: single;
    Bouncy: single;
    Radius: single;
  end;

  { Per-sprite script hook (implemented by TyroLua; run on the main thread) }
  ISpriteScript = interface
    procedure Update;
    procedure Draw;
    procedure OnCollide(AOtherHandle: Integer; const AState: string);
  end;

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
    Collides: boolean;
    Kind: TSpriteKind;
    Mass: single;
    Friction: single;
    Bouncy: single;
    Radius: single;
    Script: ISpriteScript;
    constructor Create(AHandle: Integer; ATexture: TTexture2D; const AName: string);
    destructor Destroy; override;
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
    function GetName(Handle: integer): string;

    // Physics configuration (safely callable from the script thread)
    procedure SetCollide(Handle: integer; ACollide: boolean);
    procedure SetKind(Handle: integer; AKind: TSpriteKind);
    procedure SetMass(Handle: integer; AMass: single);
    procedure SetFriction(Handle: integer; AFriction: single);
    procedure SetBouncy(Handle: integer; ABouncy: single);
    procedure SetRadius(Handle: integer; ARadius: single);
    function GetCollide(Handle: integer): boolean;
    function GetKind(Handle: integer): TSpriteKind;
    function GetMass(Handle: integer): single;
    function GetFriction(Handle: integer): single;
    function GetBouncy(Handle: integer): single;
    function GetRadius(Handle: integer): single;

    // Collects handles of colliding sprites (used by TPhysics before stepping)
    procedure GetCollideList(var AHandles: TArray<Integer>);
    function GetPhysicsState(Handle: integer; out AState: TSpritePhysicsState): boolean;

    // Per-sprite Lua script (safe from any thread; callbacks fire on the main thread)
    procedure SetScript(Handle: integer; AScript: ISpriteScript);
    function GetScript(Handle: integer): ISpriteScript;
    function HasScript(Handle: integer): boolean;

    // Update all scripted sprites (called on the main thread after physics step)
    procedure UpdateScripts;

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
  Collides := False;
  Kind := skDynamic;
  Mass := 1.0;
  Friction := 0.5;
  Bouncy := 0.0;
  Radius := 0.0;
  Script := nil;
end;

destructor TSprite.Destroy;
begin
  Script := nil;
  inherited;
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

function TSprites.GetName(Handle: integer): string;
var
  Sprite: TSprite;
begin
  Result := '';
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Name;
  finally
    FLock.Leave;
  end;
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

{ Physics helpers }

procedure TSprites.SetCollide(Handle: integer; ACollide: boolean);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Collides := ACollide;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetKind(Handle: integer; AKind: TSpriteKind);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Kind := AKind;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetMass(Handle: integer; AMass: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Mass := AMass;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetFriction(Handle: integer; AFriction: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Friction := AFriction;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetBouncy(Handle: integer; ABouncy: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Bouncy := ABouncy;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetRadius(Handle: integer; ARadius: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Radius := ARadius;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetCollide(Handle: integer): boolean;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Collides
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetKind(Handle: integer): TSpriteKind;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Kind
    else
      Result := skDynamic;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetMass(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Mass
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetFriction(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Friction
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetBouncy(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Bouncy
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetRadius(Handle: integer): single;
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Radius
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.GetCollideList(var AHandles: TArray<Integer>);
var
  Sprite: TSprite;
  Handles: TArray<Integer>;
  I: Integer;
begin
  AHandles := nil;
  FLock.Enter;
  try
    if IsConsole then WriteLn('DBG gcl entered');
    Handles := FItems.Keys.ToArray;
    if IsConsole then WriteLn('DBG gcl keys=' + IntToStr(Length(Handles)));
    for I := 0 to Length(Handles) - 1 do
    begin
      if not FItems.TryGetValue(Handles[I], Sprite) then
        Continue;
      if Sprite = nil then
        Continue;
      if IsConsole then WriteLn('DBG gcl sprite=' + IntToHex(NativeUInt(Sprite), 16) + ' collides=' + BoolToStr(Sprite.Collides, True));
      if Sprite.Collides then
      begin
        SetLength(AHandles, Length(AHandles) + 1);
        AHandles[Length(AHandles) - 1] := Sprite.Handle;
      end;
    end;
    if IsConsole then WriteLn('DBG gcl done n=' + IntToStr(Length(AHandles)));
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetPhysicsState(Handle: integer; out AState: TSpritePhysicsState): boolean;
var
  Sprite: TSprite;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
    begin
      AState.Collides := Sprite.Collides;
      AState.Kind := Sprite.Kind;
      AState.X := Sprite.X;
      AState.Y := Sprite.Y;
      AState.Angle := Sprite.Angle;
      AState.Scale := Sprite.Scale;
      AState.Width := Sprite.Texture.width * Sprite.Scale;
      AState.Height := Sprite.Texture.height * Sprite.Scale;
      AState.Mass := Sprite.Mass;
      AState.Friction := Sprite.Friction;
      AState.Bouncy := Sprite.Bouncy;
      AState.Radius := Sprite.Radius;
      Result := True;
    end;
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

procedure TSprites.SetScript(Handle: integer; AScript: ISpriteScript);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Script := AScript;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetScript(Handle: integer): ISpriteScript;
var
  Sprite: TSprite;
begin
  Result := nil;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Script;
  finally
    FLock.Leave;
  end;
end;

function TSprites.HasScript(Handle: integer): boolean;
begin
  Result := GetScript(Handle) <> nil;
end;

procedure TSprites.UpdateScripts;
var
  Sprite: TSprite;
  Scripts: TList<ISpriteScript>;
  I: Integer;
begin
  Scripts := TList<ISpriteScript>.Create;
  try
    FLock.Enter;
    try
      for Sprite in FItems.Values do
        if Sprite.Script <> nil then
          Scripts.Add(Sprite.Script);
    finally
      FLock.Leave;
    end;
    for I := 0 to Scripts.Count - 1 do
      Scripts[I].Update;
  finally
    Scripts.Free;
  end;
end;

procedure TSprites.DrawAll;
var
  Sprite: TSprite;
  Pos: TVector2;
  Scripts: TList<ISpriteScript>;
  I: Integer;
begin
  Scripts := TList<ISpriteScript>.Create;
  try
    FLock.Enter;
    try
      for Sprite in FItems.Values do
      begin
        if Sprite.Visible and (Sprite.Texture.id > 0) then
        begin
          Pos := TVector2.Create(Sprite.X, Sprite.Y);
          RayLib.DrawTextureEx(Sprite.Texture, Pos, Sprite.Angle, Sprite.Scale, clWhite);
        end;
        if Sprite.Visible and (Sprite.Script <> nil) then
          Scripts.Add(Sprite.Script);
      end;
    finally
      FLock.Leave;
    end;
    // on_draw runs after the texture blit, outside the store lock
    for I := 0 to Scripts.Count - 1 do
      Scripts[I].Draw;
  finally
    Scripts.Free;
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
