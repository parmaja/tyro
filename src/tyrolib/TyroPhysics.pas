unit TyroPhysics;
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, Math, SyncObjs, Generics.Collections,
  chipmunk,
  TyroSprites;

type
  TContactState = (
    csBegin, // shapes started touching
    csEnd    // shapes separated
  );

  TCollisionEvent = record
    HandleA: NativeInt;
    HandleB: NativeInt;
    State: TContactState;
  end;

  { Snapshot of the structural config currently applied to a body }
  TBodyConfig = record
    Kind: TSpriteKind;
    IsCircle: Boolean;
    ShapeA: single; // circle: radius, box: width (scaled)
    ShapeB: single; // box: height (scaled)
  end;

  { TPhysics: Chipmunk2D world, driven by the engine's main thread.
    Sprite config/positions are read from TSprites (thread-safe store).
    Collision events are buffered here and drained by the script thread
    via Poll() (used by collision.pump in Lua). }
  TPhysics = class(TObject)
  private
    FLock: TCriticalSection;        // guards FEvents and gravity request
    FSpace: cpSpace;
    FSprites: TSprites;
    FEvents: TList<TCollisionEvent>;
    FHandleToBody: TDictionary<Integer, cpBody>;
    FHandleToShape: TDictionary<Integer, cpShape>;
    FApplied: TDictionary<Integer, TBodyConfig>;
    FGravityX: cpFloat;
    FGravityY: cpFloat;
    FGravityDirty: Boolean;
    procedure OnCollision(A, B: cpBody; AState: TContactState);
    function FindBody(Handle: Integer): cpBody;
    function FindShape(Handle: Integer): cpShape;
    function MakeConfig(const AState: TSpritePhysicsState): TBodyConfig;
    function SameConfig(const AConfig: TBodyConfig; const AState: TSpritePhysicsState): boolean;
    function SameFloat(A, B: cpFloat): boolean;
    // Main-thread only (no locking), assumes FSpace <> nil
    procedure RemoveBody(Handle: Integer);
    procedure UpdateConfig(Handle: Integer; const AState: TSpritePhysicsState);
    function AddBody(Handle: Integer; const AState: TSpritePhysicsState): cpBody;
  public
    constructor Create(ASprites: TSprites);
    destructor Destroy; override;
    procedure RemoveAll;
    // Safe from the script thread; applied at the next Step
    procedure SetGravity(AX, AY: cpFloat);
    function GetGravityX: cpFloat;
    function GetGravityY: cpFloat;
    // Main thread, called every frame before drawing
    procedure Step(ADt: Single);
    // Script thread: drain pending collision events into AEvents (up to its capacity)
    procedure Poll(var AEvents: array of TCollisionEvent; out ACount: Integer);
    function BodyCount: Integer;
  end;

implementation

{ Chipmunk collision callbacks - executed on the main thread inside cpSpaceStep }

function chipmunkBeginContact(arb: cpArbiter; space: cpSpace; userData: cpDataPointer): cpBool; cdecl;
var
  Impl: TPhysics;
  a, b: cpBody;
begin
  a := nil;
  b := nil;
  Impl := TPhysics(userData);
  if Assigned(Impl) then
  begin
    cpArbiterGetBodies(arb, @a, @b);
    Impl.OnCollision(a, b, csBegin);
  end;
  Result := 1; // let the contact continue
end;

procedure chipmunkEndContact(arb: cpArbiter; space: cpSpace; userData: cpDataPointer); cdecl;
var
  Impl: TPhysics;
  a, b: cpBody;
begin
  a := nil;
  b := nil;
  Impl := TPhysics(userData);
  if Assigned(Impl) then
  begin
    cpArbiterGetBodies(arb, @a, @b);
    Impl.OnCollision(a, b, csEnd);
  end;
end;

{ TPhysics }

constructor TPhysics.Create(ASprites: TSprites);
var
  Handler: PcpCollisionHandler;
begin
  inherited Create;
  FSprites := ASprites;
  FLock := TCriticalSection.Create;
  FEvents := TList<TCollisionEvent>.Create;
  FHandleToBody := TDictionary<Integer, cpBody>.Create;
  FHandleToShape := TDictionary<Integer, cpShape>.Create;
  FApplied := TDictionary<Integer, TBodyConfig>.Create;
  FGravityX := 0;
  FGravityY := 980;
  FGravityDirty := True;
  FSpace := nil;
  try
    InitPhysicsLibrary;
    FSpace := cpSpaceNew;
    if FSpace <> nil then
    begin
      cpSpaceSetIterations(FSpace, 10);
      cpSpaceSetDamping(FSpace, 1.0);
      cpSpaceSetCollisionSlop(FSpace, 0.1);
      cpSpaceSetSleepTimeThreshold(FSpace, 0.0); // keep all bodies active for reliable onCollide
      Handler := cpSpaceAddDefaultCollisionHandler(FSpace);
      if Handler <> nil then
      begin
        Handler^.beginFunc := @chipmunkBeginContact;
        Handler^.separateFunc := @chipmunkEndContact;
        Handler^.userData := Pointer(Self);
      end;
    end;
  except
    on E: Exception do
    begin
      if IsConsole then
        WriteLn('Physics not available: ' + E.Message);
      FSpace := nil;
    end;
  end;
end;

destructor TPhysics.Destroy;
begin
  if FSpace <> nil then
    RemoveAll;
  FreeAndNil(FEvents);
  FreeAndNil(FHandleToBody);
  FreeAndNil(FHandleToShape);
  FreeAndNil(FApplied);
  FreeAndNil(FLock);
  inherited;
end;

function TPhysics.FindBody(Handle: Integer): cpBody;
begin
  if not FHandleToBody.TryGetValue(Handle, Result) then
    Result := nil;
end;

function TPhysics.FindShape(Handle: Integer): cpShape;
begin
  if not FHandleToShape.TryGetValue(Handle, Result) then
    Result := nil;
end;

function TPhysics.MakeConfig(const AState: TSpritePhysicsState): TBodyConfig;
begin
  Result.Kind := AState.Kind;
  Result.IsCircle := (AState.Radius * AState.Scale) > 0;
  if Result.IsCircle then
  begin
    Result.ShapeA := AState.Radius * AState.Scale;
    Result.ShapeB := 0;
  end
  else
  begin
    Result.ShapeA := AState.Width;
    Result.ShapeB := AState.Height;
  end;
end;

function TPhysics.SameFloat(A, B: cpFloat): boolean;
begin
  Result := Abs(A - B) < 0.001;
end;

function TPhysics.SameConfig(const AConfig: TBodyConfig; const AState: TSpritePhysicsState): boolean;
var
  C: TBodyConfig;
begin
  C := MakeConfig(AState);
  Result := (AConfig.Kind = C.Kind)
    and (AConfig.IsCircle = C.IsCircle)
    and SameFloat(AConfig.ShapeA, C.ShapeA)
    and SameFloat(AConfig.ShapeB, C.ShapeB);
end;

procedure TPhysics.RemoveBody(Handle: Integer);
var
  Body: cpBody;
  Shape: cpShape;
begin
  if FHandleToBody.TryGetValue(Handle, Body) then
  begin
    if FHandleToShape.TryGetValue(Handle, Shape) then
    begin
      cpSpaceRemoveShape(FSpace, Shape);
      cpShapeFree(Shape);
      FHandleToShape.Remove(Handle);
    end;
    cpSpaceRemoveBody(FSpace, Body);
    cpBodyFree(Body);
    FHandleToBody.Remove(Handle);
    FApplied.Remove(Handle);
  end;
end;

procedure TPhysics.RemoveAll;
var
  Handle: Integer;
  Keys: TList<Integer>;
begin
  if FSpace = nil then
    Exit;
  Keys := TList<Integer>.Create;
  try
    for Handle in FHandleToBody.Keys do
      Keys.Add(Handle);
    for Handle in Keys do
      RemoveBody(Handle);
  finally
    Keys.Free;
  end;
end;

procedure TPhysics.UpdateConfig(Handle: Integer; const AState: TSpritePhysicsState);
var
  Body: cpBody;
  Shape: cpShape;
  Moment, R: cpFloat;
begin
  Body := FindBody(Handle);
  if Body = nil then
    Exit;
  Shape := FindShape(Handle);
  if AState.Kind = skDynamic then
  begin
    cpBodySetMass(Body, AState.Mass);
    R := AState.Radius * AState.Scale;
    if R > 0 then
      Moment := cpMomentForCircle(AState.Mass, 0, R, cpvzero)
    else
      Moment := cpMomentForBox(AState.Mass, AState.Width, AState.Height);
    cpBodySetMoment(Body, Moment);
  end;
  if Shape <> nil then
  begin
    cpShapeSetFriction(Shape, AState.Friction);
    cpShapeSetElasticity(Shape, AState.Bouncy);
  end;
end;

function TPhysics.AddBody(Handle: Integer; const AState: TSpritePhysicsState): cpBody;
var
  Mass, Moment, W, H, R: cpFloat;
  Body: cpBody;
  Shape: cpShape;
begin
  Result := nil;
  if FSpace = nil then
    Exit;
  if IsConsole then WriteLn('DBG AddBody ' + IntToStr(Handle));
  W := AState.Width;
  H := AState.Height;
  R := AState.Radius * AState.Scale;
  if (R <= 0) and ((W <= 0) or (H <= 0)) then
    Exit; // nothing to shape yet (no texture loaded and no radius set)
  case AState.Kind of
    skDynamic:
    begin
      if R > 0 then
      begin
        Moment := cpMomentForCircle(AState.Mass, 0, R, cpvzero);
        Body := cpBodyNew(AState.Mass, Moment);
      end
      else
      begin
        Moment := cpMomentForBox(AState.Mass, W, H);
        Body := cpBodyNew(AState.Mass, Moment);
      end;
    end;
    skKinematic:
      Body := cpBodyNewKinematic;
    skStatic:
      Body := cpBodyNewStatic;
  else
    Exit;
  end;
  cpBodySetUserData(Body, Pointer(NativeInt(Handle)));
  cpBodySetPosition(Body, cpv(AState.X, AState.Y));
  cpBodySetAngle(Body, DegToRad(AState.Angle));
  if R > 0 then
    Shape := cpCircleShapeNew(Body, R, cpvzero)
  else
    Shape := cpBoxShapeNew(Body, W, H, 0);
  cpShapeSetFriction(Shape, AState.Friction);
  cpShapeSetElasticity(Shape, AState.Bouncy);
  cpSpaceAddBody(FSpace, Body);
  cpSpaceAddShape(FSpace, Shape);
  FHandleToBody.Add(Handle, Body);
  FHandleToShape.Add(Handle, Shape);
  FApplied.Add(Handle, MakeConfig(AState));
  Result := Body;
end;

procedure TPhysics.OnCollision(A, B: cpBody; AState: TContactState);
var
  e: TCollisionEvent;
begin
  if (A = nil) or (B = nil) then
    Exit;
  e.HandleA := NativeInt(cpBodyGetUserData(A));
  e.HandleB := NativeInt(cpBodyGetUserData(B));
  if (e.HandleA <= 0) or (e.HandleB <= 0) then
    Exit;
  e.State := AState;
  FLock.Enter;
  try
    FEvents.Add(e);
  finally
    FLock.Leave;
  end;
end;

procedure TPhysics.SetGravity(AX, AY: cpFloat);
begin
  FLock.Enter;
  try
    FGravityX := AX;
    FGravityY := AY;
    FGravityDirty := True;
  finally
    FLock.Leave;
  end;
end;

function TPhysics.GetGravityX: cpFloat;
begin
  FLock.Enter;
  try
    Result := FGravityX;
  finally
    FLock.Leave;
  end;
end;

function TPhysics.GetGravityY: cpFloat;
begin
  FLock.Enter;
  try
    Result := FGravityY;
  finally
    FLock.Leave;
  end;
end;

procedure TPhysics.Step(ADt: Single);
var
  Keys, Dyn: TList<Integer>;
  Colliders: TArray<Integer>;
  I: Integer;
  AHandle: Integer;
  Body: cpBody;
  State: TSpritePhysicsState;
  Config: TBodyConfig;
  Pos: cpVect;
  IsColliding: boolean;
begin
  if FSpace = nil then
    Exit;
  if ADt <= 0 then
    Exit;

  FLock.Enter;
  try
    if FGravityDirty then
    begin
      cpSpaceSetGravity(FSpace, cpv(FGravityX, FGravityY));
      FGravityDirty := False;
    end;
  finally
    FLock.Leave;
  end;
  if IsConsole then WriteLn('DBG phase1-collect');
  Dyn := TList<Integer>.Create;
  Keys := TList<Integer>.Create;
  try
    // 1) Current colliding sprite handles
    FSprites.GetCollideList(Colliders);
    if IsConsole then WriteLn('DBG phase1-done n=' + IntToStr(Length(Colliders)));

    // 2) Remove bodies for sprites that no longer collide
    for AHandle in FHandleToBody.Keys do
      Keys.Add(AHandle);
    for I := 0 to Keys.Count - 1 do
    begin
      IsColliding := False;
      for AHandle in Colliders do
        if AHandle = Keys[I] then
        begin
          IsColliding := True;
          Break;
        end;
      if not IsColliding then
        RemoveBody(Keys[I]);
    end;
    Keys.Clear;

    // 3) Add/rebuild/reconfigure bodies for colliding sprites
    if IsConsole then WriteLn('DBG phase2-remove n=' + IntToStr(Length(Colliders)));
    for I := 0 to Length(Colliders) - 1 do
    begin
      AHandle := Colliders[I];
      if not FSprites.GetPhysicsState(AHandle, State) then
      begin
        if IsConsole then WriteLn('DBG phase2-badstate h=' + IntToStr(AHandle));
        Continue;
      end;
      if IsConsole then WriteLn('DBG phase2-state h=' + IntToStr(AHandle) + ' w=' + FloatToStr(State.Width) + ' r=' + FloatToStr(State.Radius));
      if FApplied.TryGetValue(AHandle, Config) then
      begin
        if SameConfig(Config, State) then
          UpdateConfig(AHandle, State)
        else
        begin
          RemoveBody(AHandle);
          AddBody(AHandle, State);
        end;
      end
      else
        AddBody(AHandle, State);
    end;

    // 4) Kinematic/static sprites drive their bodies from script state
    for AHandle in FHandleToBody.Keys do
      Keys.Add(AHandle);
    for I := 0 to Keys.Count - 1 do
    begin
      AHandle := Keys[I];
      Body := FindBody(AHandle);
      if Body = nil then
        Continue;
      if not FSprites.GetPhysicsState(AHandle, State) then
        Continue;
      if State.Kind = skDynamic then
        Dyn.Add(AHandle)
      else
      begin
        cpBodySetPosition(Body, cpv(State.X, State.Y));
        cpBodySetAngle(Body, DegToRad(State.Angle));
        cpSpaceReindexShapesForBody(FSpace, Body);
      end;
    end;

// 5) advance the simulation
  if IsConsole then WriteLn('DBG preStep');
  cpSpaceStep(FSpace, ADt);
  if IsConsole then WriteLn('DBG postStep');

    // 6) write back dynamic bodies -> sprites
    for I := 0 to Dyn.Count - 1 do
    begin
      Body := FindBody(Dyn[I]);
      if Body = nil then
        Continue;
      Pos := cpBodyGetPosition(Body);
      FSprites.SetPosition(Dyn[I], Pos.x, Pos.y);
      FSprites.SetAngle(Dyn[I], RadToDeg(cpBodyGetAngle(Body)));
    end;
  finally
    Dyn.Free;
    Keys.Free;
  end;
end;

procedure TPhysics.Poll(var AEvents: array of TCollisionEvent; out ACount: Integer);
var
  I, Max: Integer;
begin
  ACount := 0;
  FLock.Enter;
  try
    Max := Length(AEvents);
    if Max > FEvents.Count then
      Max := FEvents.Count;
    for I := 0 to Max - 1 do
    begin
      AEvents[I] := FEvents[0];
      FEvents.Delete(0);
    end;
    ACount := Max;
  finally
    FLock.Leave;
  end;
end;

function TPhysics.BodyCount: Integer;
begin
  FLock.Enter;
  try
    Result := FHandleToBody.Count;
  finally
    FLock.Leave;
  end;
end;

end.