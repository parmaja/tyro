unit TyroPhysics;
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, Math, SyncObjs, Types, Generics.Collections,
  mnClasses,
  RayLib,
  chipmunk,
  TyroSprites, TyroTiles;

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
    ShapeA: Single; // circle: radius, box: width (scaled)
    ShapeB: Single; // box: height (scaled)
  end;

  { The wall set one colliding tilemap owns in the space: one static body
    per solid cell (a body's position is its box's centre, so each cell is
    its own shape), plus the signature the boxes were built from. The
    bodies carry no userData, so OnCollision drops their events: the walls
    push, they never report - a sprite's on_collide still fires against
    other sprites the same as ever. }
  TTileCollider = class
  public
    Body: cpBody;
    Shapes: TList<cpShape>;
    Version: Integer; // the map's DataVersion the boxes came from
    AX: Single;       // the map's x/y/scale they stand at
    AY: Single;
    AScale: Single;
  end;

  { The joint kinds Lua's collision.joint hands out: pin is the rigid rod,
    slide the leash, rotator the motor (a damped rotary spring) and limit
    the passive angle clamp (a rotary limit joint). }
  TJointKind = (jkPin, jkSlide, jkRotator, jkAngleLimit);

  { TJoint: one script-made joint between two sprites. The record is born
    on the script thread as a request (Constraint = nil) and lives in
    FJointRequests until the main thread's Step finds bodies for both
    sprites - which happens once they carry collide = true - and hooks the
    Chipmunk constraint onto the space. A joint leaves with the first sprite
    that loses its body, and rebuilding a sprite (its shape or kind changes)
    rebuilds the body, so that drops its joints too. Dist/Min/Max are pixels,
    Angle/Stiffness-facing angles radians: collision.joint converts degrees. }
  TJoint = class
  public
    Id: Integer;
    Kind: TJointKind;
    HandleA, HandleB: Integer;
    AnchorA, AnchorB: cpVect; // sprite-local px, the point each body holds onto
    Dist: cpFloat;            // pin: the rod length
    Min: cpFloat;             // slide: closest / limit: min angle
    Max: cpFloat;             // slide: furthest  / limit: max angle
    Angle: cpFloat;           // rotator: the rest angle b holds beside a
    Stiffness: cpFloat;       // rotator: how hard the motor turns
    Damping: cpFloat;         // rotator: how quickly the turn settles
    MaxForce: cpFloat;        // 0 = the solver may pull as hard as it must
    Constraint: cpConstraint; // nil while the joint still waits for its bodies
    WasTaut: Boolean;         // slide: the last beat's verdict, for the edges
  end;

  { One rope edge the solver just crossed: the pair and whether the leash
    has now snapped taut or gone slack. The engine beat forwards these to
    on("joint_stretch"), like the collision queue forwards contacts. }
  TJointEvent = record
    HandleA, HandleB: Integer;
    Taut: Boolean;
  end;

  { TPhysics: Chipmunk2D world, driven by the engine's main thread.
    Sprite config/positions are read from TSprites (thread-safe store).
    Collision events are buffered here and drained by the script thread
    via Poll() (used by collision.pump in Lua).
    Events touching a sprite that owns a Lua script are moved to a separate
    queue by SplitScriptedEvents (main thread) and fire on_collide directly. }
  TPhysics = class(TObject)
  private
    FLock: TmnCriticalSection;        // guards FEvents, FScriptedEvents and gravity request
    FSpace: cpSpace;
    FSprites: TSprites;
    FEvents: TList<TCollisionEvent>;
    FScriptedEvents: TList<TCollisionEvent>;
    FHandleToBody: TDictionary<Integer, cpBody>;
    FHandleToShape: TDictionary<Integer, cpShape>;
    FApplied: TDictionary<Integer, TBodyConfig>;
    FGravityX: cpFloat;
    FGravityY: cpFloat;
    FGravityDirty: Boolean;
    // Tilemap walls (map.collide): the store to read, and one wall set per
    // map that opted in. Main thread only, like the rest of the space.
    FTiles: TTiles;
    FMapColliders: TDictionary<Integer, TTileCollider>;
    // Joints (collision.joint). The script thread only ever touches the
    // lists below behind FLock; the Chipmunk side (the space) is built and
    // torn down by the main thread's Step, like the gravity request.
    FJoints: TList<TJoint>;         // the ones the space already carries
    FJointRequests: TList<TJoint>;  // born here, waiting for both bodies
    FJointRemoves: TList<Integer>;  // free()/clear() ids, dropped next Step
    FJointIds: TDictionary<Integer, Integer>; // live ids; the script thread
                                              // asks it "is that joint alive?"
    FJointNextId: Integer;
    // The taut/slack edges the slides crossed since the last beat. Step
    // writes and the engine beat drains - both on the main thread - so this
    // queue needs no lock (unlike the collision events, no script-thread
    // pump reads it).
    FStretchEvents: TList<TJointEvent>;
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
    // Main-thread only (no locking): build the Chipmunk constraint of one
    // request, or nil when the space refuses it. The request's numbers are
    // already final (collision.joint resolved the defaults).
    function MakeConstraint(const J: TJoint; BodyA, BodyB: cpBody): cpConstraint;
    // Main-thread only (locks): the Step's two joint chores - drop the
    // free()/clear() ids, then try to give every waiting joint its bodies.
    procedure ApplyJointRemovals;
    procedure ApplyJointRequests;
    // Main-thread only (from RemoveBody): every joint touching this sprite
    // leaves now - its body is about to disappear under the constraint.
    procedure RemoveJointsOfHandle(AHandle: Integer);
    // Main-thread only (Step, after the solve): every live slide measures
    // its anchors, and a beat that crosses the taut edge lands one event in
    // FStretchEvents. Pins and motors never stretch - they hold by design.
    procedure CheckStretch;
    // Main-thread only (no locking): drop one map's wall set with its bodies
    procedure RemoveMapCollider(Handle: Integer);
    // Main-thread only (no locking), assumes FSpace <> nil: bring the tile
    // walls up to date - maps that stopped colliding lose their set, maps
    // whose signature (grid, solidity, or the map's own x/y/scale) changed
    // are rebuilt from the store's solid boxes.
    procedure SyncTileMaps;
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
    // Main thread, after Step: move events touching a scripted sprite out of
    // the legacy buffer so on_collide can fire without double-dispatch
    procedure SplitScriptedEvents;
    // Main thread: drain scripted collision events into AEvents (up to capacity)
    procedure PollScripted(var AEvents: array of TCollisionEvent; out ACount: Integer);
    // Script thread: drain pending collision events into AEvents (up to its capacity)
    procedure Poll(var AEvents: array of TCollisionEvent; out ACount: Integer);
    function BodyCount: Integer;
    // Joints for collision.joint in Lua. Add/Free/Clear/Count are safe from
    // the script thread: they only move the lists behind FLock, and the main
    // thread's Step does the Chipmunk side. AddJoint answers the id right
    // away, so even a still-waiting joint can be freed the beat it was armed.
    function AddJoint(AKind: TJointKind; AHandleA, AHandleB: Integer;
      const AAnchorA, AAnchorB: cpVect;
      ADist, AMin, AMax, AAngle, AStiffness, ADamping, AMaxForce: cpFloat): Integer;
    function FreeJoint(AId: Integer): Boolean;
    function ClearJoints: Integer;
    function JointCount: Integer;
    // Main-thread only (the engine beat): drain the taut/slack edges so they
    // can reach on("joint_stretch"). Like the collision queue, up to the
    // buffer's room, and the drained ones leave for good.
    procedure PollStretch(var AEvents: array of TJointEvent; out ACount: Integer);
    // The tilemap store the walls (map.collide) are read from; the engine
    // hands it over once at startup. Without it the space only knows sprites.
    property Tiles: TTiles read FTiles write FTiles;
  end;

implementation

uses
  TyroEngines;

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
  FLock := TmnCriticalSection.Create;
  FEvents := TList<TCollisionEvent>.Create;
  FScriptedEvents := TList<TCollisionEvent>.Create;
  FHandleToBody := TDictionary<Integer, cpBody>.Create;
  FHandleToShape := TDictionary<Integer, cpShape>.Create;
  FApplied := TDictionary<Integer, TBodyConfig>.Create;
  FGravityX := 0;
  FGravityY := 980;
  FGravityDirty := True;
  FTiles := nil;
  FMapColliders := TDictionary<Integer, TTileCollider>.Create;
  FJoints := TList<TJoint>.Create;
  FJointRequests := TList<TJoint>.Create;
  FJointRemoves := TList<Integer>.Create;
  FJointIds := TDictionary<Integer, Integer>.Create;
  FJointNextId := 1; // joint id 0 stays invalid, so joint_free(0) always answers false
  FStretchEvents := TList<TJointEvent>.Create;
  FSpace := nil;
  try
    InitPhysicsLibrary;
    FSpace := cpSpaceNew;
    if FSpace <> nil then
    begin
      cpSpaceSetIterations(FSpace, 10);
      cpSpaceSetDamping(FSpace, 1.0);
      cpSpaceSetCollisionSlop(FSpace, 0.1);
      cpSpaceSetSleepTimeThreshold(FSpace, Infinity); // never sleep; keep all bodies active for reliable onCollide
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
  //RemoveAll runs even when the library never loaded: the joint requests a
  //script armed without a space still own their records, and so does every
  //list below. The space's own teardown happens only when there is one.
  RemoveAll;
  if FSpace <> nil then
  begin
    cpSpaceFree(FSpace);
    FSpace := nil;
  end;
  FMapColliders.Free;
  FStretchEvents.Free;
  FJointIds.Free;
  FJointRemoves.Free;
  FJointRequests.Free;
  FJoints.Free;
  FreeAndNil(FEvents);
  FreeAndNil(FScriptedEvents);
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
  //The joints go first: RemoveBody is about to take the body out from under
  //whatever the space still pulls on, and a constraint with a freed body is
  //a crash at the next solve. A waiting request on this sprite leaves too -
  //collide off means no body, no joint.
  RemoveJointsOfHandle(Handle);
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
  Handle, I: Integer;
  J: TJoint;
  Keys: TList<Integer>;
begin
  Keys := TList<Integer>.Create;
  try
    if FSpace <> nil then
    begin
      for Handle in FHandleToBody.Keys do
        Keys.Add(Handle);
      for Handle in Keys do
        RemoveBody(Handle);
    end;
  finally
    Keys.Free;
  end;
  //The tile walls ride the same teardown: a rerun's ResetPhysics leaves an
  //empty space, and the next Step rebuilds the sets from the store.
  for Handle in FMapColliders.Keys.ToArray do
    RemoveMapCollider(Handle);
  //The joints leave with the world, live ones and waiting requests alike -
  //a rerun's scripts re-arm theirs, and id lookups start from an empty book.
  FLock.Enter;
  try
    for I := FJoints.Count - 1 downto 0 do
    begin
      J := FJoints[I];
      if J.Constraint <> nil then
      begin
        if FSpace <> nil then
          cpSpaceRemoveConstraint(FSpace, J.Constraint);
        cpConstraintFree(J.Constraint);
      end;
      J.Free;
      FJoints.Delete(I);
    end;
    for I := FJointRequests.Count - 1 downto 0 do
    begin
      FJointRequests[I].Free;
      FJointRequests.Delete(I);
    end;
    FJointIds.Clear;
    FJointRemoves.Clear;
    FStretchEvents.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TPhysics.RemoveMapCollider(Handle: Integer);
var
  Col: TTileCollider;
  Shape: cpShape;
begin
  if not FMapColliders.TryGetValue(Handle, Col) then
    Exit;
  for Shape in Col.Shapes do
  begin
    if FSpace <> nil then
      cpSpaceRemoveShape(FSpace, Shape);
    cpShapeFree(Shape);
  end;
  Col.Shapes.Free;
  if (FSpace <> nil) and (Col.Body <> nil) then
    cpSpaceRemoveBody(FSpace, Col.Body);
  if Col.Body <> nil then
    cpBodyFree(Col.Body);
  Col.Free;
  FMapColliders.Remove(Handle);
end;

{ ---- joints: the collision.joint family behind the Lua door ---- }

function TPhysics.AddJoint(AKind: TJointKind; AHandleA, AHandleB: Integer;
  const AAnchorA, AAnchorB: cpVect;
  ADist, AMin, AMax, AAngle, AStiffness, ADamping, AMaxForce: cpFloat): Integer;
var
  J: TJoint;
begin
  //Safe from the script thread: the id is booked here, the space work waits
  //for the main thread's Step - which gives the joint its constraint once
  //both sprites carry a body.
  J := TJoint.Create;
  J.Kind := AKind;
  J.HandleA := AHandleA;
  J.HandleB := AHandleB;
  J.AnchorA := AAnchorA;
  J.AnchorB := AAnchorB;
  J.Dist := ADist;
  J.Min := AMin;
  J.Max := AMax;
  J.Angle := AAngle;
  J.Stiffness := AStiffness;
  J.Damping := ADamping;
  J.MaxForce := AMaxForce;
  J.Constraint := nil;
  FLock.Enter;
  try
    J.Id := FJointNextId;
    Inc(FJointNextId);
    FJointRequests.Add(J);
    Result := J.Id;
  finally
    FLock.Leave;
  end;
end;

function TPhysics.FreeJoint(AId: Integer): Boolean;
var
  I: Integer;
begin
  //The id may sit in the waiting list (no bodies yet) or in the space. Both
  //roads end at FJointRemoves, which the next Step digs - a request that is
  //not in the space yet simply leaves the lists; a live one has its
  //constraint unhooked there. Freeing twice answers false.
  Result := False;
  if AId = 0 then
    Exit;
  FLock.Enter;
  try
    if FJointRemoves.IndexOf(AId) >= 0 then
      Exit; // already on its way out - freeing twice answers false
    if FJointIds.ContainsKey(AId) then
    begin
      FJointRemoves.Add(AId);
      Result := True;
    end
    else
    begin
      for I := 0 to FJointRequests.Count - 1 do
        if FJointRequests[I].Id = AId then
        begin
          FJointRemoves.Add(AId);
          Result := True;
          Break;
        end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TPhysics.ClearJoints: Integer;
var
  Id, I: Integer;
begin
  // The live ids join the removal list - the next Step digs them out of the
  // space - but the book is emptied right here: from this beat joints()
  // reads 0 and joint_free() of those ids answers false. A waiting request
  // never reached the space, so it is dropped on the spot.
  FLock.Enter;
  try
    Result := FJointIds.Count + FJointRequests.Count;
    for Id in FJointIds.Keys do
      FJointRemoves.Add(Id);
    FJointIds.Clear;
    for I := FJointRequests.Count - 1 downto 0 do
    begin
      FJointRequests[I].Free;
      FJointRequests.Delete(I);
    end;
  finally
    FLock.Leave;
  end;
end;

function TPhysics.JointCount: Integer;
begin
  //Safe from the script thread: waiting requests count too - the script
  //armed them, so for its purposes they exist.
  FLock.Enter;
  try
    Result := FJointIds.Count + FJointRequests.Count;
  finally
    FLock.Leave;
  end;
end;

function TPhysics.MakeConstraint(const J: TJoint; BodyA, BodyB: cpBody): cpConstraint;
begin
  case J.Kind of
    jkPin:
    begin
      //7.0.3 pins the anchor distance as it stands at creation; a script
      //that named a length overwrites it. Dist < 0 = "keep what it is".
      Result := cpPinJointNew(BodyA, BodyB, J.AnchorA, J.AnchorB);
      if (Result <> nil) and (J.Dist >= 0) then
        cpPinJointSetDist(Result, J.Dist);
    end;
    jkSlide:
      Result := cpSlideJointNew(BodyA, BodyB, J.AnchorA, J.AnchorB, J.Min, J.Max);
    jkRotator:
      Result := cpDampedRotarySpringNew(BodyA, BodyB, J.Angle, J.Stiffness, J.Damping);
  else
    Result := cpRotaryLimitJointNew(BodyA, BodyB, J.Min, J.Max);
  end;
  if (Result <> nil) and (J.MaxForce > 0) then
    cpConstraintSetMaxForce(Result, J.MaxForce);
end;

procedure TPhysics.ApplyJointRemovals;
var
  I, K, Id: Integer;
  J: TJoint;
begin
  if FJointRemoves.Count = 0 then
    Exit;
  FLock.Enter;
  try
    for I := 0 to FJointRemoves.Count - 1 do
    begin
      Id := FJointRemoves[I];
      for K := FJoints.Count - 1 downto 0 do
        if FJoints[K].Id = Id then
        begin
          J := FJoints[K];
          if J.Constraint <> nil then
          begin
            if FSpace <> nil then
              cpSpaceRemoveConstraint(FSpace, J.Constraint);
            cpConstraintFree(J.Constraint);
            J.Constraint := nil;
          end;
          FJoints.Delete(K);
          J.Free;
        end;
      for K := FJointRequests.Count - 1 downto 0 do
        if FJointRequests[K].Id = Id then
        begin
          FJointRequests[K].Free;
          FJointRequests.Delete(K);
        end;
      FJointIds.Remove(Id);
    end;
    FJointRemoves.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TPhysics.ApplyJointRequests;
var
  I: Integer;
  J: TJoint;
  BodyA, BodyB: cpBody;
  StateA, StateB: TSpritePhysicsState;
begin
  if (FSpace = nil) or (FJointRequests.Count = 0) then
    Exit;
  FLock.Enter;
  try
    I := FJointRequests.Count - 1;
    while I >= 0 do
    begin
      J := FJointRequests[I];
      if (not FSprites.GetPhysicsState(J.HandleA, StateA))
        or (not FSprites.GetPhysicsState(J.HandleB, StateB)) then
      begin
        //one of the sprites is gone before the joint could land
        FJointRequests.Delete(I);
        J.Free;
      end
      else
      begin
        BodyA := FindBody(J.HandleA);
        BodyB := FindBody(J.HandleB);
        if (BodyA <> nil) and (BodyB <> nil) then
        begin
          J.Constraint := MakeConstraint(J, BodyA, BodyB);
          if J.Constraint <> nil then
          begin
            cpSpaceAddConstraint(FSpace, J.Constraint);
            FJointIds.Add(J.Id, 0);
            FJointRequests.Delete(I);
            FJoints.Add(J);
          end;
        end;
        //a sprite that is not physical yet keeps the request waiting: the
        //joints land the beat their bodies appear, and collision.joint has
        //already switched both sprites to collide, so that is the next one.
      end;
      Dec(I);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TPhysics.RemoveJointsOfHandle(AHandle: Integer);
var
  I: Integer;
  J: TJoint;
begin
  FLock.Enter;
  try
    for I := FJoints.Count - 1 downto 0 do
    begin
      J := FJoints[I];
      if (J.HandleA = AHandle) or (J.HandleB = AHandle) then
      begin
        if J.Constraint <> nil then
        begin
          if FSpace <> nil then
            cpSpaceRemoveConstraint(FSpace, J.Constraint);
          cpConstraintFree(J.Constraint);
          J.Constraint := nil;
        end;
        FJointIds.Remove(J.Id);
        FJoints.Delete(I);
        J.Free;
      end;
    end;
    for I := FJointRequests.Count - 1 downto 0 do
    begin
      J := FJointRequests[I];
      if (J.HandleA = AHandle) or (J.HandleB = AHandle) then
      begin
        FJointRequests.Delete(I);
        J.Free;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TPhysics.CheckStretch;
const
  cTautEpsilon = 1.0; // px of slack the solver always leaves at the cap
var
  I: Integer;
  J: TJoint;
  BodyA, BodyB: cpBody;
  PosA, PosB: cpVect;
  RA, RB, AX, AY, BX, BY, D: cpFloat;
  Taut: Boolean;
  Ev: TJointEvent;
begin
  // Only the slides stretch: a pin holds its rod and a rotator its angle,
  // so both are "already taut" every beat and would say nothing new.
  if FJoints.Count = 0 then
    Exit;
  // A run that never drives the engine beat never drains here: the buffer
  // keeps its recent history instead of growing forever.
  if FStretchEvents.Count > 1024 then
    FStretchEvents.Clear;
  for I := 0 to FJoints.Count - 1 do
  begin
    J := FJoints[I];
    if (J.Kind <> jkSlide) or (J.Constraint = nil) then
      Continue;
    BodyA := FindBody(J.HandleA);
    BodyB := FindBody(J.HandleB);
    if (BodyA = nil) or (BodyB = nil) then
      Continue;
    // the anchor points where the solver left them, turned by each body
    PosA := cpBodyGetPosition(BodyA);
    RA := cpBodyGetAngle(BodyA);
    AX := PosA.x + J.AnchorA.x * Cos(RA) - J.AnchorA.y * Sin(RA);
    AY := PosA.y + J.AnchorA.x * Sin(RA) + J.AnchorA.y * Cos(RA);
    PosB := cpBodyGetPosition(BodyB);
    RB := cpBodyGetAngle(BodyB);
    BX := PosB.x + J.AnchorB.x * Cos(RB) - J.AnchorB.y * Sin(RB);
    BY := PosB.y + J.AnchorB.x * Sin(RB) + J.AnchorB.y * Cos(RB);
    D := Sqrt((BX - AX) * (BX - AX) + (BY - AY) * (BY - AY));
    Taut := D >= J.Max - cTautEpsilon;
    if Taut <> J.WasTaut then
    begin
      J.WasTaut := Taut;
      Ev.HandleA := J.HandleA;
      Ev.HandleB := J.HandleB;
      Ev.Taut := Taut;
      FStretchEvents.Add(Ev);
    end;
  end;
end;

procedure TPhysics.PollStretch(var AEvents: array of TJointEvent; out ACount: Integer);
var
  I, Max: Integer;
begin
  ACount := 0;
  Max := Length(AEvents);
  if Max > FStretchEvents.Count then
    Max := FStretchEvents.Count;
  for I := 0 to Max - 1 do
  begin
    AEvents[I] := FStretchEvents[0];
    FStretchEvents.Delete(0);
  end;
  ACount := Max;
end;

//The tile walls' beat, just before cpSpaceStep: the sprites' positions were
//already synced above, so the walls stand where the board paints when the
//solver runs. A map's signature is (DataVersion, x, y, scale): any grid
//write, solidity flip or transform lands on the next step as a rebuild -
//one static body per solid cell, silent to the event queues.
procedure TPhysics.SyncTileMaps;
var
  Maps: TArray<Integer>;
  Handle, Version, I, J: Integer;
  X, Y, Scale: Single;
  Col: TTileCollider;
  Boxes: TArray<TRectangle>;
  Sh: cpShape;
  Needs: Boolean;
begin
  if (FSpace = nil) or (FTiles = nil) then
    Exit;
  FTiles.GetCollideMapList(Maps);
  //maps that left the list (collide off or freed) lose their wall set
  if FMapColliders.Count > 0 then
  begin
    for Handle in FMapColliders.Keys.ToArray do
    begin
      Needs := False;
      for I := 0 to Length(Maps) - 1 do
        if Maps[I] = Handle then
        begin
          Needs := True;
          Break;
        end;
      if not Needs then
        RemoveMapCollider(Handle);
    end;
  end;
  for I := 0 to Length(Maps) - 1 do
  begin
    Handle := Maps[I];
    if not FTiles.GetTileMapSignature(Handle, Version, X, Y, Scale) then
    begin
      RemoveMapCollider(Handle); //the map is gone
      Continue;
    end;
    if FMapColliders.TryGetValue(Handle, Col) then
    begin
      if (Col.Version = Version) and SameFloat(Col.AX, X) and
        SameFloat(Col.AY, Y) and SameFloat(Col.AScale, Scale) then
        Continue; //nothing moved since the last build
      RemoveMapCollider(Handle);
    end;
    FTiles.CollectSolidBoxes(Handle, Boxes);
    Col := TTileCollider.Create;
    Col.Shapes := TList<cpShape>.Create;
    Col.Version := Version;
    Col.AX := X;
    Col.AY := Y;
    Col.AScale := Scale;
    Col.Body := nil;
    for J := 0 to High(Boxes) do
    begin
      //One static body per cell: cpBoxShapeNew centres its box on the body,
      //and the relative-offset sibling is not part of the binding. The cell
      //centre is the body position; the shape rides it at rest.
      Col.Body := cpBodyNewStatic;
      cpBodySetPosition(Col.Body,
        cpv(Boxes[J].X + Boxes[J].Width * 0.5,
            Boxes[J].Y + Boxes[J].Height * 0.5));
      cpSpaceAddBody(FSpace, Col.Body);
      Sh := cpBoxShapeNew(Col.Body, Boxes[J].Width, Boxes[J].Height, 0);
      Col.Shapes.Add(Sh);
      cpSpaceAddShape(FSpace, Sh);
    end;
    FMapColliders.Add(Handle, Col);
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
    //The touch knobs ride the same every-frame refresh as friction: a
    //sensor or filter change lands on the next step without rebuilding the
    //shape, and a body that just joined carries the defaults anyway.
    cpShapeSetSensor(Shape, cpBool(Ord(AState.Sensor)));
    cpShapeSetFilter(Shape, cpShapeFilterNew(AState.Group, AState.Category, AState.Mask));
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
  if IsDebug then
    WriteLn('DBG AddBody ' + IntToStr(Handle));
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
  cpShapeSetSensor(Shape, cpBool(Ord(AState.Sensor)));
  cpShapeSetFilter(Shape, cpShapeFilterNew(AState.Group, AState.Category, AState.Mask));
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
  // Route events touching a scripted sprite straight to the scripted queue
  // so threads draining FEvents (collision.pump) can not steal them.
  if FSprites.HasScript(e.HandleA) or FSprites.HasScript(e.HandleB) then
  begin
    FLock.Enter;
    try
      FScriptedEvents.Add(e);
    finally
      FLock.Leave;
    end;
  end
  else
  begin
    FLock.Enter;
    try
      FEvents.Add(e);
    finally
      FLock.Leave;
    end;
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
  Vel: cpVect;
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
  //free()/clear() dig their joints out of the space before anything looks
  //at the bodies they hung on.
  ApplyJointRemovals;
  if IsDebug then
    WriteLn('DBG phase1-collect');
  Dyn := TList<Integer>.Create;
  Keys := TList<Integer>.Create;
  try
    // 1) Current colliding sprite handles
    FSprites.GetCollideList(Colliders);
    if IsDebug then
      WriteLn('DBG phase1-done n=' + IntToStr(Length(Colliders)));

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
    if IsDebug then
      WriteLn('DBG phase2-remove n=' + IntToStr(Length(Colliders)));
    for I := 0 to Length(Colliders) - 1 do
    begin
      AHandle := Colliders[I];
      if not FSprites.GetPhysicsState(AHandle, State) then
      begin
        if IsDebug then
          WriteLn('DBG phase2-badstate h=' + IntToStr(AHandle));
        Continue;
      end;
      if IsDebug then
        WriteLn('DBG phase2-state h=' + IntToStr(AHandle) + ' w=' + FloatToStr(State.Width) + ' r=' + FloatToStr(State.Radius));
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
      begin
        Dyn.Add(AHandle);
        //Apply the script-visible velocity to the body. Between steps the
        //store holds the last simulated velocity, so re-applying it is a
        //no-op for a coasting body and only takes effect when a script wrote
        //a new value since the previous write-back (phase 6).
        cpBodySetVelocity(Body, cpv(State.VX, State.VY));
      end
      else
      begin
        cpBodySetPosition(Body, cpv(State.X, State.Y));
        cpBodySetAngle(Body, DegToRad(State.Angle));
        cpSpaceReindexShapesForBody(FSpace, Body);
      end;
    end;

// 5) advance the simulation
  //The joints land now: every body the sprites asked for stands in the space
  //since phase 3, so a waiting request whose pair is complete gets its
  //constraint before the solver ever runs.
  ApplyJointRequests;
  //The tile walls answer for the board before the solver runs: a map whose
  //grid or transform moved since the last step is rebuilt here, so the
  //sprites step against the walls the picture shows.
  SyncTileMaps;
  if IsDebug then
    WriteLn('DBG preStep');
  cpSpaceStep(FSpace, ADt);
  if IsDebug then
    WriteLn('DBG postStep');

    // 6) write back dynamic bodies -> sprites
    for I := 0 to Dyn.Count - 1 do
    begin
      Body := FindBody(Dyn[I]);
      if Body = nil then
        Continue;
      Pos := cpBodyGetPosition(Body);
      FSprites.SetPosition(Dyn[I], Pos.x, Pos.y);
      FSprites.SetAngle(Dyn[I], RadToDeg(cpBodyGetAngle(Body)));
      Vel := cpBodyGetVelocity(Body);
      FSprites.SetVelocity(Dyn[I], Vel.x, Vel.y);
    end;

    // 7) the ropes report: a leash that just snapped taut (or let go) has
    // its edge queued for the engine beat's on("joint_stretch").
    CheckStretch;
  finally
    Dyn.Free;
    Keys.Free;
  end;
end;

procedure TPhysics.SplitScriptedEvents;
var
  I: Integer;
  ev: TCollisionEvent;
begin
  FLock.Enter;
  try
    I := 0;
    while I < FEvents.Count do
    begin
      ev := FEvents[I];
      if FSprites.HasScript(ev.HandleA) or FSprites.HasScript(ev.HandleB) then
      begin
        FScriptedEvents.Add(ev);
        FEvents.Delete(I);
      end
      else
        Inc(I);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TPhysics.PollScripted(var AEvents: array of TCollisionEvent; out ACount: Integer);
var
  I, Max: Integer;
begin
  ACount := 0;
  FLock.Enter;
  try
    Max := Length(AEvents);
    if Max > FScriptedEvents.Count then
      Max := FScriptedEvents.Count;
    for I := 0 to Max - 1 do
    begin
      AEvents[I] := FScriptedEvents[0];
      FScriptedEvents.Delete(0);
    end;
    ACount := Max;
  finally
    FLock.Leave;
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
