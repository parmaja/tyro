unit chipmunk;
// Pascal bindings for Chipmunk2D 7.0.3 (https://github.com/slembcke/Chipmunk2D)
// MIT license - see src/chipmunk/LICENSE.txt
//
// Dynamic library loading mirrors src/raylib/RayLib.pas using the MiniLib mnLibraries package.
// The helper static-inline functions (cpv, cpvadd, ...) are reimplemented in Pascal below.
{$ifdef fpc}
{$mode delphi}
{$endif}
{$M+}{$H+}{$A8}
{$MINENUMSIZE 4}
{$POINTERMATH ON}

interface

uses
  mnLibraries, SysUtils;

type
  cpFloat = Double;
  cpBool = Byte;
  cpBitmask = Cardinal;
  cpTimestamp = Cardinal;
  cpGroup = PtrUInt;
  cpCollisionType = PtrUInt;
  cpDataPointer = Pointer;

  cpSpace = Pointer;
  cpBody = Pointer;
  cpShape = Pointer;
  cpArbiter = Pointer;
  cpConstraint = Pointer;

  { 2D vector (C: cpVect with double x, y) }
  cpVect = record
    x, y: cpFloat;
  end;

  { Fast collision filtering (C: cpShapeFilter) }
  cpShapeFilter = record
    group: cpGroup;
    categories: cpBitmask;
    mask: cpBitmask;
  end;

  PcpShape = ^cpShape;
  PcpBody = ^cpBody;
  PcpArbiter = ^cpArbiter;
  PcpSpace = ^cpSpace;
  PcpVect = ^cpVect;

  cpBodyType = (
    CP_BODY_TYPE_DYNAMIC,
    CP_BODY_TYPE_KINEMATIC,
    CP_BODY_TYPE_STATIC
  );

  cpCollisionBeginFunc = function(arb: cpArbiter; space: cpSpace; userData: cpDataPointer): cpBool; cdecl;
  cpCollisionPreSolveFunc = function(arb: cpArbiter; space: cpSpace; userData: cpDataPointer): cpBool; cdecl;
  cpCollisionPostSolveFunc = procedure(arb: cpArbiter; space: cpSpace; userData: cpDataPointer); cdecl;
  cpCollisionSeparateFunc = procedure(arb: cpArbiter; space: cpSpace; userData: cpDataPointer); cdecl;

  { Collision handler struct (C: cpCollisionHandler) }
  cpCollisionHandler = record
    typeA, typeB: cpCollisionType;
    beginFunc: cpCollisionBeginFunc;
    preSolveFunc: cpCollisionPreSolveFunc;
    postSolveFunc: cpCollisionPostSolveFunc;
    separateFunc: cpCollisionSeparateFunc;
    userData: cpDataPointer;
  end;
  PcpCollisionHandler = ^cpCollisionHandler;

const
  CP_NO_GROUP: cpGroup = 0;
  CP_ALL_CATEGORIES: cpBitmask = $ffffffff;
  CP_WILDCARD_COLLISION_TYPE: cpCollisionType = cpCollisionType(-1);

  { CP_SHAPE_FILTER_ALL }
  cCPShapeFilterAll: cpShapeFilter = (group: 0; categories: $ffffffff; mask: $ffffffff);

type
  { TmncChipmunk }
  TmncChipmunk = class(TmnLibrary)
  protected
    procedure Link; override;
  end;

var
  ChipmunkLibrary: TmncChipmunk = nil;

// -- Space --
var
  cpSpaceNew: function(): cpSpace; cdecl = nil;
  cpSpaceFree: procedure(space: cpSpace); cdecl = nil;
  cpSpaceGetIterations: function(const space: cpSpace): Integer; cdecl = nil;
  cpSpaceSetIterations: procedure(space: cpSpace; iterations: Integer); cdecl = nil;
  cpSpaceGetGravity: function(const space: cpSpace): cpVect; cdecl = nil;
  cpSpaceSetGravity: procedure(space: cpSpace; gravity: cpVect); cdecl = nil;
  cpSpaceGetDamping: function(const space: cpSpace): cpFloat; cdecl = nil;
  cpSpaceSetDamping: procedure(space: cpSpace; damping: cpFloat); cdecl = nil;
  cpSpaceGetSleepTimeThreshold: function(const space: cpSpace): cpFloat; cdecl = nil;
  cpSpaceSetSleepTimeThreshold: procedure(space: cpSpace; sleepTimeThreshold: cpFloat); cdecl = nil;
  cpSpaceGetCollisionSlop: function(const space: cpSpace): cpFloat; cdecl = nil;
  cpSpaceSetCollisionSlop: procedure(space: cpSpace; collisionSlop: cpFloat); cdecl = nil;
  cpSpaceGetCurrentTimeStep: function(const space: cpSpace): cpFloat; cdecl = nil;
  cpSpaceSetUserData: procedure(space: cpSpace; userData: cpDataPointer); cdecl = nil;
  cpSpaceGetUserData: function(const space: cpSpace): cpDataPointer; cdecl = nil;
  cpSpaceGetStaticBody: function(const space: cpSpace): cpBody; cdecl = nil;
  cpSpaceAddBody: function(space: cpSpace; body: cpBody): cpBody; cdecl = nil;
  cpSpaceAddShape: function(space: cpSpace; shape: cpShape): cpShape; cdecl = nil;
  cpSpaceRemoveBody: procedure(space: cpSpace; body: cpBody); cdecl = nil;
  cpSpaceRemoveShape: procedure(space: cpSpace; shape: cpShape); cdecl = nil;
  cpSpaceContainsBody: function(space: cpSpace; body: cpBody): cpBool; cdecl = nil;
  cpSpaceAddDefaultCollisionHandler: function(space: cpSpace): PcpCollisionHandler; cdecl = nil;
  cpSpaceReindexShapesForBody: procedure(space: cpSpace; body: cpBody); cdecl = nil;
  cpSpaceStep: procedure(space: cpSpace; dt: cpFloat); cdecl = nil;
  cpSpaceIsLocked: function(space: cpSpace): cpBool; cdecl = nil;

// -- Body --
var
  cpBodyNew: function(mass: cpFloat; moment: cpFloat): cpBody; cdecl = nil;
  cpBodyNewKinematic: function(): cpBody; cdecl = nil;
  cpBodyNewStatic: function(): cpBody; cdecl = nil;
  cpBodyFree: procedure(body: cpBody); cdecl = nil;
  cpBodyActivate: procedure(body: cpBody); cdecl = nil;
  cpBodyIsSleeping: function(const body: cpBody): cpBool; cdecl = nil;
  cpBodyGetType: function(body: cpBody): cpBodyType; cdecl = nil;
  cpBodySetType: procedure(body: cpBody; aType: cpBodyType); cdecl = nil;
  cpBodyGetMass: function(const body: cpBody): cpFloat; cdecl = nil;
  cpBodySetMass: procedure(body: cpBody; m: cpFloat); cdecl = nil;
  cpBodyGetMoment: function(const body: cpBody): cpFloat; cdecl = nil;
  cpBodySetMoment: procedure(body: cpBody; i: cpFloat); cdecl = nil;
  cpBodyGetPosition: function(const body: cpBody): cpVect; cdecl = nil;
  cpBodySetPosition: procedure(body: cpBody; pos: cpVect); cdecl = nil;
  cpBodyGetVelocity: function(const body: cpBody): cpVect; cdecl = nil;
  cpBodySetVelocity: procedure(body: cpBody; velocity: cpVect); cdecl = nil;
  cpBodyGetAngle: function(const body: cpBody): cpFloat; cdecl = nil;
  cpBodySetAngle: procedure(body: cpBody; a: cpFloat); cdecl = nil;
  cpBodyGetAngularVelocity: function(const body: cpBody): cpFloat; cdecl = nil;
  cpBodySetAngularVelocity: procedure(body: cpBody; angularVelocity: cpFloat); cdecl = nil;
  cpBodyGetUserData: function(const body: cpBody): cpDataPointer; cdecl = nil;
  cpBodySetUserData: procedure(body: cpBody; userData: cpDataPointer); cdecl = nil;
  cpBodyApplyForceAtWorldPoint: procedure(body: cpBody; force: cpVect; point: cpVect); cdecl = nil;
  cpBodyApplyForceAtLocalPoint: procedure(body: cpBody; force: cpVect; point: cpVect); cdecl = nil;
  cpBodyApplyImpulseAtWorldPoint: procedure(body: cpBody; impulse: cpVect; point: cpVect); cdecl = nil;
  cpBodyApplyImpulseAtLocalPoint: procedure(body: cpBody; impulse: cpVect; point: cpVect); cdecl = nil;

// -- Shape --
var
  cpShapeFree: procedure(shape: cpShape); cdecl = nil;
  cpShapeGetBody: function(const shape: cpShape): cpBody; cdecl = nil;
  cpShapeGetSensor: function(const shape: cpShape): cpBool; cdecl = nil;
  cpShapeSetSensor: procedure(shape: cpShape; sensor: cpBool); cdecl = nil;
  cpShapeGetElasticity: function(const shape: cpShape): cpFloat; cdecl = nil;
  cpShapeSetElasticity: procedure(shape: cpShape; elasticity: cpFloat); cdecl = nil;
  cpShapeGetFriction: function(const shape: cpShape): cpFloat; cdecl = nil;
  cpShapeSetFriction: procedure(shape: cpShape; friction: cpFloat); cdecl = nil;
  cpShapeGetCollisionType: function(const shape: cpShape): cpCollisionType; cdecl = nil;
  cpShapeSetCollisionType: procedure(shape: cpShape; collisionType: cpCollisionType); cdecl = nil;
  cpShapeGetFilter: function(const shape: cpShape): cpShapeFilter; cdecl = nil;
  cpShapeSetFilter: procedure(shape: cpShape; filter: cpShapeFilter); cdecl = nil;
  cpShapeGetUserData: function(const shape: cpShape): cpDataPointer; cdecl = nil;
  cpShapeSetUserData: procedure(shape: cpShape; userData: cpDataPointer); cdecl = nil;
  cpCircleShapeNew: function(body: cpBody; radius: cpFloat; offset: cpVect): cpShape; cdecl = nil;
  cpCircleShapeGetOffset: function(const shape: cpShape): cpVect; cdecl = nil;
  cpCircleShapeGetRadius: function(const shape: cpShape): cpFloat; cdecl = nil;
  cpBoxShapeNew: function(body: cpBody; width: cpFloat; height: cpFloat; radius: cpFloat): cpShape; cdecl = nil;

// -- Moments / areas --
var
  cpMomentForCircle: function(m: cpFloat; r1: cpFloat; r2: cpFloat; offset: cpVect): cpFloat; cdecl = nil;
  cpMomentForBox: function(m: cpFloat; width: cpFloat; height: cpFloat): cpFloat; cdecl = nil;

// -- Arbiter --
var
  cpArbiterGetBodies: procedure(const arb: cpArbiter; a: PcpBody; b: PcpBody); cdecl = nil;
  cpArbiterGetShapes: procedure(const arb: cpArbiter; a: PcpShape; b: PcpShape); cdecl = nil;
  cpArbiterIsFirstContact: function(const arb: cpArbiter): cpBool; cdecl = nil;
  cpArbiterIsRemoval: function(const arb: cpArbiter): cpBool; cdecl = nil;
  cpArbiterGetCount: function(const arb: cpArbiter): Integer; cdecl = nil;
  cpArbiterGetNormal: function(const arb: cpArbiter): cpVect; cdecl = nil;
  cpArbiterGetDepth: function(const arb: cpArbiter; i: Integer): cpFloat; cdecl = nil;

// Pascal reimplementations of Chipmunk's static-inline helpers
function cpv(const x, y: cpFloat): cpVect;
function cpvzero: cpVect;
function cpveql(const v1, v2: cpVect): cpBool;
function cpvadd(const v1, v2: cpVect): cpVect;
function cpvsub(const v1, v2: cpVect): cpVect;
function cpvneg(const v: cpVect): cpVect;
function cpvmult(const v: cpVect; const s: cpFloat): cpVect;
function cpvdot(const v1, v2: cpVect): cpFloat;
function cpvcross(const v1, v2: cpVect): cpFloat;
function cpvperp(const v: cpVect): cpVect;
function cpvlength(const v: cpVect): cpFloat;
function cpvlengthsq(const v: cpVect): cpFloat;
function cpvnormalize(const v: cpVect): cpVect;
function cpvnormalize_safe(const v: cpVect): cpVect;
function cpfmax(const a, b: cpFloat): cpFloat;
function cpfmin(const a, b: cpFloat): cpFloat;
function cpfclamp(const f, min, max: cpFloat): cpFloat;
function cpfclamp01(const f: cpFloat): cpFloat;
function cpShapeFilterNew(group: cpGroup; categories: cpBitmask; mask: cpBitmask): cpShapeFilter;

procedure InitPhysicsLibrary;

implementation

function cpv(const x, y: cpFloat): cpVect;
begin
  Result.x := x;
  Result.y := y;
end;

function cpvzero: cpVect;
begin
  Result := cpv(0, 0);
end;

function cpveql(const v1, v2: cpVect): cpBool;
begin
  Result := Byte((v1.x = v2.x) and (v1.y = v2.y));
end;

function cpvadd(const v1, v2: cpVect): cpVect;
begin
  Result := cpv(v1.x + v2.x, v1.y + v2.y);
end;

function cpvsub(const v1, v2: cpVect): cpVect;
begin
  Result := cpv(v1.x - v2.x, v1.y - v2.y);
end;

function cpvneg(const v: cpVect): cpVect;
begin
  Result := cpv(-v.x, -v.y);
end;

function cpvmult(const v: cpVect; const s: cpFloat): cpVect;
begin
  Result := cpv(v.x * s, v.y * s);
end;

function cpvdot(const v1, v2: cpVect): cpFloat;
begin
  Result := v1.x * v2.x + v1.y * v2.y;
end;

function cpvcross(const v1, v2: cpVect): cpFloat;
begin
  Result := v1.x * v2.y - v1.y * v2.x;
end;

function cpvperp(const v: cpVect): cpVect;
begin
  Result := cpv(-v.y, v.x);
end;

function cpvlength(const v: cpVect): cpFloat;
begin
  Result := Sqrt(cpvdot(v, v));
end;

function cpvlengthsq(const v: cpVect): cpFloat;
begin
  Result := cpvdot(v, v);
end;

function cpvnormalize(const v: cpVect): cpVect;
begin
  Result := cpvmult(v, 1.0 / cpvlength(v));
end;

function cpvnormalize_safe(const v: cpVect): cpVect;
begin
  if cpvlengthsq(v) > 0 then
    Result := cpvnormalize(v)
  else
    Result := cpvzero;
end;

function cpfmax(const a, b: cpFloat): cpFloat;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function cpfmin(const a, b: cpFloat): cpFloat;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function cpfclamp(const f, min, max: cpFloat): cpFloat;
begin
  Result := cpfmin(cpfmax(f, min), max);
end;

function cpfclamp01(const f: cpFloat): cpFloat;
begin
  Result := cpfmax(0, cpfmin(f, 1));
end;

function cpShapeFilterNew(group: cpGroup; categories: cpBitmask; mask: cpBitmask): cpShapeFilter;
begin
  Result.group := group;
  Result.categories := categories;
  Result.mask := mask;
end;

procedure TmncChipmunk.Link;
begin
  RaiseError := True; //Raise error if one of these functions does not exist
  GetAddress(@cpSpaceNew, 'cpSpaceNew');
  GetAddress(@cpSpaceFree, 'cpSpaceFree');
  GetAddress(@cpSpaceGetIterations, 'cpSpaceGetIterations');
  GetAddress(@cpSpaceSetIterations, 'cpSpaceSetIterations');
  GetAddress(@cpSpaceGetGravity, 'cpSpaceGetGravity');
  GetAddress(@cpSpaceSetGravity, 'cpSpaceSetGravity');
  GetAddress(@cpSpaceGetDamping, 'cpSpaceGetDamping');
  GetAddress(@cpSpaceSetDamping, 'cpSpaceSetDamping');
  GetAddress(@cpSpaceGetSleepTimeThreshold, 'cpSpaceGetSleepTimeThreshold');
  GetAddress(@cpSpaceSetSleepTimeThreshold, 'cpSpaceSetSleepTimeThreshold');
  GetAddress(@cpSpaceGetCollisionSlop, 'cpSpaceGetCollisionSlop');
  GetAddress(@cpSpaceSetCollisionSlop, 'cpSpaceSetCollisionSlop');
  GetAddress(@cpSpaceGetCurrentTimeStep, 'cpSpaceGetCurrentTimeStep');
  GetAddress(@cpSpaceSetUserData, 'cpSpaceSetUserData');
  GetAddress(@cpSpaceGetUserData, 'cpSpaceGetUserData');
  GetAddress(@cpSpaceGetStaticBody, 'cpSpaceGetStaticBody');
  GetAddress(@cpSpaceAddBody, 'cpSpaceAddBody');
  GetAddress(@cpSpaceAddShape, 'cpSpaceAddShape');
  GetAddress(@cpSpaceRemoveBody, 'cpSpaceRemoveBody');
  GetAddress(@cpSpaceRemoveShape, 'cpSpaceRemoveShape');
  GetAddress(@cpSpaceContainsBody, 'cpSpaceContainsBody');
  GetAddress(@cpSpaceAddDefaultCollisionHandler, 'cpSpaceAddDefaultCollisionHandler');
  GetAddress(@cpSpaceReindexShapesForBody, 'cpSpaceReindexShapesForBody');
  GetAddress(@cpSpaceStep, 'cpSpaceStep');
  GetAddress(@cpSpaceIsLocked, 'cpSpaceIsLocked');

  GetAddress(@cpBodyNew, 'cpBodyNew');
  GetAddress(@cpBodyNewKinematic, 'cpBodyNewKinematic');
  GetAddress(@cpBodyNewStatic, 'cpBodyNewStatic');
  GetAddress(@cpBodyFree, 'cpBodyFree');
  GetAddress(@cpBodyActivate, 'cpBodyActivate');
  GetAddress(@cpBodyIsSleeping, 'cpBodyIsSleeping');
  GetAddress(@cpBodyGetType, 'cpBodyGetType');
  GetAddress(@cpBodySetType, 'cpBodySetType');
  GetAddress(@cpBodyGetMass, 'cpBodyGetMass');
  GetAddress(@cpBodySetMass, 'cpBodySetMass');
  GetAddress(@cpBodyGetMoment, 'cpBodyGetMoment');
  GetAddress(@cpBodySetMoment, 'cpBodySetMoment');
  GetAddress(@cpBodyGetPosition, 'cpBodyGetPosition');
  GetAddress(@cpBodySetPosition, 'cpBodySetPosition');
  GetAddress(@cpBodyGetVelocity, 'cpBodyGetVelocity');
  GetAddress(@cpBodySetVelocity, 'cpBodySetVelocity');
  GetAddress(@cpBodyGetAngle, 'cpBodyGetAngle');
  GetAddress(@cpBodySetAngle, 'cpBodySetAngle');
  GetAddress(@cpBodyGetAngularVelocity, 'cpBodyGetAngularVelocity');
  GetAddress(@cpBodySetAngularVelocity, 'cpBodySetAngularVelocity');
  GetAddress(@cpBodyGetUserData, 'cpBodyGetUserData');
  GetAddress(@cpBodySetUserData, 'cpBodySetUserData');
  GetAddress(@cpBodyApplyForceAtWorldPoint, 'cpBodyApplyForceAtWorldPoint');
  GetAddress(@cpBodyApplyForceAtLocalPoint, 'cpBodyApplyForceAtLocalPoint');
  GetAddress(@cpBodyApplyImpulseAtWorldPoint, 'cpBodyApplyImpulseAtWorldPoint');
  GetAddress(@cpBodyApplyImpulseAtLocalPoint, 'cpBodyApplyImpulseAtLocalPoint');

  GetAddress(@cpShapeFree, 'cpShapeFree');
  GetAddress(@cpShapeGetBody, 'cpShapeGetBody');
  GetAddress(@cpShapeGetSensor, 'cpShapeGetSensor');
  GetAddress(@cpShapeSetSensor, 'cpShapeSetSensor');
  GetAddress(@cpShapeGetElasticity, 'cpShapeGetElasticity');
  GetAddress(@cpShapeSetElasticity, 'cpShapeSetElasticity');
  GetAddress(@cpShapeGetFriction, 'cpShapeGetFriction');
  GetAddress(@cpShapeSetFriction, 'cpShapeSetFriction');
  GetAddress(@cpShapeGetCollisionType, 'cpShapeGetCollisionType');
  GetAddress(@cpShapeSetCollisionType, 'cpShapeSetCollisionType');
  GetAddress(@cpShapeGetFilter, 'cpShapeGetFilter');
  GetAddress(@cpShapeSetFilter, 'cpShapeSetFilter');
  GetAddress(@cpShapeGetUserData, 'cpShapeGetUserData');
  GetAddress(@cpShapeSetUserData, 'cpShapeSetUserData');
  GetAddress(@cpCircleShapeNew, 'cpCircleShapeNew');
  GetAddress(@cpCircleShapeGetOffset, 'cpCircleShapeGetOffset');
  GetAddress(@cpCircleShapeGetRadius, 'cpCircleShapeGetRadius');
  GetAddress(@cpBoxShapeNew, 'cpBoxShapeNew');

  GetAddress(@cpMomentForCircle, 'cpMomentForCircle');
  GetAddress(@cpMomentForBox, 'cpMomentForBox');

  GetAddress(@cpArbiterGetBodies, 'cpArbiterGetBodies');
  GetAddress(@cpArbiterGetShapes, 'cpArbiterGetShapes');
  GetAddress(@cpArbiterIsFirstContact, 'cpArbiterIsFirstContact');
  GetAddress(@cpArbiterIsRemoval, 'cpArbiterIsRemoval');
  GetAddress(@cpArbiterGetCount, 'cpArbiterGetCount');
  GetAddress(@cpArbiterGetNormal, 'cpArbiterGetNormal');
  GetAddress(@cpArbiterGetDepth, 'cpArbiterGetDepth');
end;

procedure InitPhysicsLibrary;
begin
  ChipmunkLibrary.Load;
end;

initialization
  ChipmunkLibrary := TmncChipmunk.Create('chipmunk.dll');
finalization
  FreeAndNil(ChipmunkLibrary);
end.