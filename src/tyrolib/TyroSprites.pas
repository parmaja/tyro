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
    // Animation state: when the sprite was loaded from an Aseprite file that
    // has more than one frame, AnimFrames holds one texture per frame and
    // Texture is swapped to the current frame by UpdateAnims / SetAnimFrame.
    AnimFrames: array of TTexture2D;
    AnimFrameMs: array of Integer;  // per-frame duration in ms (from the file)
    FrameCount: Integer;            // 0 = not animated (single image)
    AnimFrame: Integer;             // current frame index
    AnimTime: Single;               // seconds spent on the current frame
    AnimSpeed: Single;              // frames-per-second override; 0 = file durations
    Playing: Boolean;               // true while the animation advances each frame
    Looping: Boolean;
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
    // Unloads the GPU textures owned by a sprite (its animation frames and, if
    // distinct, its single frame texture) and resets the sprite to unloaded.
    procedure FreeSpriteFrames(ASprite: TSprite);
  public
    constructor Create;
    destructor Destroy; override;
    // Add a loaded texture to the store, returns a handle
    function Add(ATexture: TTexture2D; const AName: string = ''): integer;
    // Add a texture-less sprite (script-only markers, etc.), returns a handle
    function AddEmpty(const AName: string = ''): integer;
    // Replace the texture of an existing sprite in place, returns success
    function SetTexture(Handle: integer; ATexture: TTexture2D): boolean;
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

    // Animation: installs a set of frames (one texture per animation frame)
    // onto a sprite, replacing whatever texture/frames it had. The textures
    // become owned by the sprite; do not free them afterwards.
    function InstallFrames(Handle: integer; const AFrames: array of TTexture2D; const AFrameMs: array of Integer): boolean;
    procedure SetAnimFrame(Handle: integer; AFrame: integer);
    function GetAnimFrame(Handle: integer): integer;
    function GetFrameCount(Handle: integer): integer;
    function GetFrameMs(Handle: integer; AFrame: integer): integer;
    procedure SetAnimSpeed(Handle: integer; AFPS: single);
    function GetAnimSpeed(Handle: integer): single;
    procedure SetPlaying(Handle: integer; APlaying: boolean);
    function GetPlaying(Handle: integer): boolean;
    procedure SetLooping(Handle: integer; ALooping: boolean);
    function GetLooping(Handle: integer): boolean;
    // Advances all playing animations by DT seconds and swaps each sprite's
    // Texture to the current frame (called every frame on the main thread).
    procedure UpdateAnims(DT: single);

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
    // Dispatch on_draw() for visible scripted sprites (called after the
    // legacy Graphic layer is blitted, so script overlays stay on top)
    procedure DrawScripts;
    // Draw a single sprite (queued from Lua)
    procedure DrawSprite(Handle: integer; ACanvas: TTyroCanvas; AX, AY: single; AAngle: single; AScale: single; ATint: TColor);
    property Count: integer read GetCount;
    property Items[Index: Integer]: TSprite read GetItems; default;
  end;

  { TCreateSpriteObject }

  { Creates a texture-less sprite on the main thread and returns its handle. }
  TCreateSpriteObject = class(TQueueObject)
  private
    FName: string;
    FHandleResult: integer;
  public
    constructor Create(const AName: string);
    procedure DoExecute; override;
    property HandleResult: integer read FHandleResult;
  end;

  { TLoadSpriteObject }

  TLoadSpriteObject = class(TQueueObject)
  private
    FFileName: string;
    FName: string;
    FExistingHandle: integer;
    FHandleResult: integer;
  public
    constructor Create(AFileName: string; const AName: string; AExistingHandle: integer = 0);
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
  TyroEngines, Aseprites;

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
  AnimFrames := nil;
  AnimFrameMs := nil;
  FrameCount := 0;
  AnimFrame := 0;
  AnimTime := 0;
  AnimSpeed := 0;
  Playing := False;
  Looping := True;
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
    FreeSpriteFrames(Sprite);
    Sprite.Free;
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

{ Animation helpers }

procedure TSprites.FreeSpriteFrames(ASprite: TSprite);
var
  Unloaded: TList<Cardinal>;
  I: Integer;
begin
  if ASprite = nil then
    Exit;
  Unloaded := TList<Cardinal>.Create;
  try
    for I := 0 to Length(ASprite.AnimFrames) - 1 do
      if (ASprite.AnimFrames[I].id > 0) and (Unloaded.IndexOf(ASprite.AnimFrames[I].id) < 0) then
      begin
        Unloaded.Add(ASprite.AnimFrames[I].id);
        RayLib.UnloadTexture(ASprite.AnimFrames[I]);
      end;
    if (ASprite.Texture.id > 0) and (Unloaded.IndexOf(ASprite.Texture.id) < 0) then
      RayLib.UnloadTexture(ASprite.Texture);
    ASprite.AnimFrames := nil;
    ASprite.AnimFrameMs := nil;
    ASprite.FrameCount := 0;
    ASprite.AnimFrame := 0;
    ASprite.AnimTime := 0;
    ASprite.Playing := False;
    ASprite.Texture := Default(TTexture2D);
  finally
    Unloaded.Free;
  end;
end;

function TSprites.InstallFrames(Handle: integer; const AFrames: array of TTexture2D; const AFrameMs: array of Integer): boolean;
var
  Sprite: TSprite;
  I: Integer;
  aMs: Integer;
begin
  Result := False;
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Sprite) then
      Exit;
    if Length(AFrames) <= 0 then
      Exit;
    FreeSpriteFrames(Sprite);
    SetLength(Sprite.AnimFrames, Length(AFrames));
    SetLength(Sprite.AnimFrameMs, Length(AFrames));
    for I := 0 to Length(AFrames) - 1 do
    begin
      Sprite.AnimFrames[I] := AFrames[I];
      if I < Length(AFrameMs) then
        aMs := AFrameMs[I]
      else
        aMs := 100;
      if aMs < 1 then
        aMs := 100;
      Sprite.AnimFrameMs[I] := aMs;
    end;
    Sprite.FrameCount := Length(AFrames);
    Sprite.AnimFrame := 0;
    Sprite.AnimTime := 0;
    Sprite.Playing := False;
    Sprite.Looping := True;
    Sprite.Texture := Sprite.AnimFrames[0];
    Result := True;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetAnimFrame(Handle: integer; AFrame: integer);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
    begin
      if (Sprite.FrameCount > 0) and (AFrame >= 0) then
      begin
        if AFrame >= Sprite.FrameCount then
          AFrame := Sprite.FrameCount - 1;
        Sprite.AnimFrame := AFrame;
        Sprite.AnimTime := 0;
        Sprite.Texture := Sprite.AnimFrames[AFrame];
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetAnimFrame(Handle: integer): integer;
var
  Sprite: TSprite;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.AnimFrame;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetFrameCount(Handle: integer): integer;
var
  Sprite: TSprite;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.FrameCount;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetFrameMs(Handle: integer; AFrame: integer): integer;
var
  Sprite: TSprite;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) and (AFrame >= 0) and (AFrame < Length(Sprite.AnimFrameMs)) then
      Result := Sprite.AnimFrameMs[AFrame];
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetAnimSpeed(Handle: integer; AFPS: single);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.AnimSpeed := AFPS;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetAnimSpeed(Handle: integer): single;
var
  Sprite: TSprite;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.AnimSpeed;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetPlaying(Handle: integer; APlaying: boolean);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Playing := APlaying;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetPlaying(Handle: integer): boolean;
var
  Sprite: TSprite;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Playing;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetLooping(Handle: integer; ALooping: boolean);
var
  Sprite: TSprite;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Sprite.Looping := ALooping;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetLooping(Handle: integer): boolean;
var
  Sprite: TSprite;
begin
  Result := True;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
      Result := Sprite.Looping;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.UpdateAnims(DT: single);
var
  Sprite: TSprite;
  Handles: TArray<Integer>;
  I: Integer;
  TimePer: single;
begin
  if DT <= 0 then
    Exit;
  FLock.Enter;
  try
    Handles := FItems.Keys.ToArray;
    for I := 0 to Length(Handles) - 1 do
    begin
      if not FItems.TryGetValue(Handles[I], Sprite) then
        Continue;
      if Sprite = nil then
        Continue;
      if not Sprite.Playing then
        Continue;
      if Sprite.FrameCount <= 0 then
        Continue;
      if Sprite.AnimFrame >= Sprite.FrameCount then
        Sprite.AnimFrame := 0;
      if Sprite.AnimSpeed > 0 then
        TimePer := 1 / Sprite.AnimSpeed
      else
        TimePer := Sprite.AnimFrameMs[Sprite.AnimFrame] / 1000;
      if TimePer <= 0 then
        TimePer := 0.1;
      Sprite.AnimTime := Sprite.AnimTime + DT;
      //Clamp catch-up: after a stall (pause, resize, lag) a large DT must not
      //fast-forward many animation frames in a single tick - advance at most
      //one frame and drop the excess time.
      if Sprite.AnimTime >= TimePer then
      begin
        Sprite.AnimTime := 0;
        if Sprite.AnimFrame + 1 < Sprite.FrameCount then
          Inc(Sprite.AnimFrame)
        else if Sprite.Looping then
          Sprite.AnimFrame := 0
        else
        begin
          Sprite.AnimFrame := Sprite.FrameCount - 1;
          Sprite.Playing := False;
        end;
      end;
      Sprite.Texture := Sprite.AnimFrames[Sprite.AnimFrame];
    end;
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

function TSprites.AddEmpty(const AName: string): integer;
var
  EmptyTexture: TTexture2D;
begin
  Result := cSpriteInvalid;
  EmptyTexture := Default(TTexture2D);
  FLock.Enter;
  try
    FItems.Add(FNextHandle, TSprite.Create(FNextHandle, EmptyTexture, AName));
    Result := FNextHandle;
    Inc(FNextHandle);
  finally
    FLock.Leave;
  end;
end;

function TSprites.SetTexture(Handle: integer; ATexture: TTexture2D): boolean;
var
  Sprite: TSprite;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Sprite) then
    begin
      if (ATexture.id > 0) and (ATexture.id = Sprite.Texture.id) then
      begin
        //Re-installing the very texture this sprite already owns: freeing it
        //first would leave a dangling id (self-texture use-after-free).
        Result := True;
        Exit;
      end;
      // The store owns every installed texture, including a previous
      // non-animated one. Drop it before taking ownership of the replacement.
      FreeSpriteFrames(Sprite);
      Sprite.Texture := ATexture;
      Result := True;
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
    Handles := FItems.Keys.ToArray;
    for I := 0 to Length(Handles) - 1 do
    begin
      if not FItems.TryGetValue(Handles[I], Sprite) then
        Continue;
      if Sprite = nil then
        Continue;
      if Sprite.Collides then
      begin
        SetLength(AHandles, Length(AHandles) + 1);
        AHandles[Length(AHandles) - 1] := Sprite.Handle;
      end;
    end;
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
begin
  FLock.Enter;
  try
    for Sprite in FItems.Values do
      if Sprite.Visible and (Sprite.Texture.id > 0) then
      begin
        Pos := TVector2.Create(Sprite.X, Sprite.Y);
        RayLib.DrawTextureEx(Sprite.Texture, Pos, Sprite.Angle, Sprite.Scale, clWhite);
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.DrawScripts;
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
        if Sprite.Visible and (Sprite.Script <> nil) then
          Scripts.Add(Sprite.Script);
    finally
      FLock.Leave;
    end;
    // on_draw runs after the Graphic layer blit, outside the store lock
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

{ TCreateSpriteObject }

constructor TCreateSpriteObject.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FHandleResult := cSpriteInvalid;
  //EventNeeded;
end;

procedure TCreateSpriteObject.DoExecute;
begin
  FHandleResult := Main.Sprites.AddEmpty(FName);
end;

{ TLoadSpriteObject }

constructor TLoadSpriteObject.Create(AFileName: string; const AName: string; AExistingHandle: integer);
begin
  inherited Create;
  FFileName := AFileName;
  FName := AName;
  FExistingHandle := AExistingHandle;
  FHandleResult := cSpriteInvalid;
end;

destructor TLoadSpriteObject.Destroy;
begin
  inherited;
end;

procedure TLoadSpriteObject.DoExecute;
var
  aTexture: TTexture2D;
  aFrames: TAseTextures;
  aFrameMs: TAseFrameDurations;
  aCount: Integer;
  aHandle: integer;
begin
  aTexture := Default(TTexture2D);
  aFrames := nil;
  aFrameMs := nil;
  aCount := 0;
  if IsAsepriteFile(FFileName) then
  begin
    aCount := AsepriteLoadFrameTextures(FFileName, aFrames, aFrameMs);
    if aCount = 1 then
    begin
      //A single-frame .aseprite is a static sprite: hand its frame to the
      //single-texture path below (nil the slot so a rollback free skips it).
      aTexture := aFrames[0];
      aFrames[0] := Default(TTexture2D);
    end;
  end
  else
    aTexture := RayLib.LoadTexture(PUTF8Char(FFileName));

  if aCount > 1 then
  begin
    // animated sprite: install every frame (textures become sprite-owned)
    if FExistingHandle > cSpriteInvalid then
    begin
      if Main.Sprites.InstallFrames(FExistingHandle, aFrames, aFrameMs) then
        FHandleResult := FExistingHandle
      else
        FHandleResult := cSpriteInvalid;
    end
    else
    begin
      aHandle := Main.Sprites.AddEmpty(FName);
      if (aHandle > cSpriteInvalid) and Main.Sprites.InstallFrames(aHandle, aFrames, aFrameMs) then
        FHandleResult := aHandle
      else
        FHandleResult := cSpriteInvalid;
    end;
    if FHandleResult <= cSpriteInvalid then
      AsepriteFreeTextures(aFrames); // rollback the GPU uploads
  end
  else if aTexture.id > 0 then
  begin
    if FExistingHandle > cSpriteInvalid then
    begin
      // materialize a texture created by Sprites.new() in place
      if Main.Sprites.SetTexture(FExistingHandle, aTexture) then
        FHandleResult := FExistingHandle
      else
      begin
        RayLib.UnloadTexture(aTexture); //store did not take ownership
        FHandleResult := cSpriteInvalid;
      end;
    end
    else
      FHandleResult := Main.Sprites.Add(aTexture, FName); //takes ownership
  end
  else
  begin
    if aFrames <> nil then
      AsepriteFreeTextures(aFrames); //should be empty; free defensively
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
