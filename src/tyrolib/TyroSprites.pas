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
  cSpriteMax = 256;

type
  { TSpriteRecord }

  TSpriteRecord = record
    Handle: Integer;
    Texture: TTexture2D;
    Valid: boolean;
    Name: string;
    X: single;
    Y: single;
    Angle: single;
    Scale: single;
    Visible: boolean;
  end;
  PSpriteRecord = ^TSpriteRecord;

  { TSpriteStore }

  //TODO convert to use TDictionary<Integer, TSpriteRecord>
  TSprites = class
  private
    FLock: TCriticalSection;
    FItems: array[0..cSpriteMax - 1] of TSpriteRecord; //TODO remove this
    FCount: integer;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Add a loaded texture to the store, returns a handle
    function AddTexture(ATexture: TTexture2D; const AName: string = ''): integer;
    function IsValid(Handle: integer): boolean;
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
    function FindByName(const AName: string): integer;
    // Draw all valid sprites (called in the engine's draw loop)
    procedure DrawAll;
    // Draw a single sprite (queued from Lua)
    procedure DrawOne(Handle: integer; ACanvas: TTyroCanvas; AX, AY: single; AAngle: single; AScale: single; ATint: TColor);
    property Count: integer read GetCount;
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

  TDrawSpriteObject = class(TDrawObject)
  private
    FSpriteStore: TSprites;
    fHandle: integer;
    fX, fY: single;
    fAngle: single;
    fScale: single;
    fTint: TColor;
  public
    constructor Create(ASpriteStore: TSprites; AHandle: integer; AX, AY: single; ACanvas: TTyroCanvas; AAngle: single; AScale: single; ATint: TColor);
    procedure DoExecute; override;
  end;

implementation

uses
  TyroEngines;

{ TSprites }

constructor TSprites.Create;
var
  i: integer;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCount := 0;
  for i := 0 to cSpriteMax - 1 do
  begin
    FItems[i].Valid := False;
    FItems[i].Name := '';
    FItems[i].X := 0;
    FItems[i].Y := 0;
    FItems[i].Angle := 0;
    FItems[i].Scale := 1.0;
    FItems[i].Visible := True;
  end;
end;

destructor TSprites.Destroy;
var
  i: integer;
begin
  for i := 0 to cSpriteMax - 1 do
    if FItems[i].Valid then
      RayLib.UnloadTexture(FItems[i].Texture);
  FLock.Free;
  inherited;
end;

function TSprites.GetCount: integer;
begin
  Result := FCount;
end;

function TSprites.AddTexture(ATexture: TTexture2D; const AName: string): integer;
var
  i: integer;
begin
  Result := cSpriteInvalid;
  FLock.Enter;
  try
    if (ATexture.id > 0) then
    begin
      for i := 0 to cSpriteMax - 1 do
        if not FItems[i].Valid then
        begin
          FItems[i].Texture := ATexture;
          FItems[i].Valid := True;
          FItems[i].Name := AName;
          FItems[i].X := 0;
          FItems[i].Y := 0;
          FItems[i].Angle := 0;
          FItems[i].Scale := 1.0;
          FItems[i].Visible := True;
          Inc(FCount);
          Result := i;
          Exit;
        end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TSprites.IsValid(Handle: integer): boolean;
begin
  FLock.Enter;
  try
    Result := (Handle > cSpriteInvalid) and (Handle < cSpriteMax) and FItems[Handle].Valid;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetTexture(Handle: integer): TTexture2D;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].Texture
    else
      Result.ID := 0; Result.Width := 0; Result.Height := 0; Result.Mipmaps := 1; Result.Format := 0;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetPosition(Handle: integer; X, Y: single);
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
    begin
      FItems[Handle].X := X;
      FItems[Handle].Y := Y;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetAngle(Handle: integer; Angle: single);
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      FItems[Handle].Angle := Angle;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetScale(Handle: integer; Scale: single);
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      FItems[Handle].Scale := Scale;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.SetVisible(Handle: integer; AVisible: boolean);
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      FItems[Handle].Visible := AVisible;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetX(Handle: integer): single;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].X
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetY(Handle: integer): single;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].Y
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetAngle(Handle: integer): single;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].Angle
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetScale(Handle: integer): single;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].Scale
    else
      Result := 1;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetVisible(Handle: integer): boolean;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].Visible
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetWidth(Handle: integer): integer;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].Texture.width
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.GetHeight(Handle: integer): integer;
begin
  FLock.Enter;
  try
    if IsValid(Handle) then
      Result := FItems[Handle].Texture.height
    else
      Result := 0;
  finally
    FLock.Leave;
  end;
end;

function TSprites.FindByName(const AName: string): integer;
var
  i: integer;
begin
  Result := cSpriteInvalid;
  FLock.Enter;
  try
    for i := 0 to cSpriteMax - 1 do
      if FItems[i].Valid and (FItems[i].Name = AName) then
      begin
        Result := i;
        Exit;
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.DrawAll;
var
  i: integer;
  Rec: TTexture2D;
  Pos: TVector2;
  Ang: single;
  Scl: single;
begin
  FLock.Enter;
  try
    for i := 0 to cSpriteMax - 1 do
    begin
      if FItems[i].Valid and FItems[i].Visible and (FItems[i].Texture.id > 0) then
      begin
        Rec := FItems[i].Texture;
        Pos := TVector2.Create(FItems[i].X, FItems[i].Y);
        Ang := FItems[i].Angle;
        Scl := FItems[i].Scale;
        RayLib.DrawTextureEx(Rec, Pos, Ang, Scl, clWhite);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSprites.DrawOne(Handle: integer; ACanvas: TTyroCanvas; AX, AY: single; AAngle: single; AScale: single; ATint: TColor);
var
  Rec: TTexture2D;
  Pos: TVector2;
  Ang: single;
  Scl: single;
begin
  FLock.Enter;
  try
    if not IsValid(Handle) then
      Exit;
    if (AX > -1) or (AY > -1) then
    begin
      FItems[Handle].X := AX;
      FItems[Handle].Y := AY;
    end;
    if AAngle >= 0 then
      FItems[Handle].Angle := AAngle;
    if AScale >= 0 then
      FItems[Handle].Scale := AScale;
    Rec := FItems[Handle].Texture;
    Pos := TVector2.Create(FItems[Handle].X, FItems[Handle].Y);
    Ang := FItems[Handle].Angle;
    Scl := FItems[Handle].Scale;
  finally
    FLock.Leave;
  end;
  RayLib.DrawTextureEx(Rec, Pos, Ang, Scl, ATint);
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
    FHandleResult := Main.Sprites.AddTexture(aTexture, FName)
  else
  begin
    if IsConsole then
      WriteLn('Sprite not loaded: ' + FFileName);
    FHandleResult := cSpriteInvalid;
  end;
end;

{ TDrawSpriteObject }

constructor TDrawSpriteObject.Create(ASpriteStore: TSprites; AHandle: integer; AX, AY: single; ACanvas: TTyroCanvas; AAngle: single; AScale: single; ATint: TColor);
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
  FSpriteStore.DrawOne(fHandle, Canvas, fX, fY, fAngle, fScale, fTint);
end;

end.
