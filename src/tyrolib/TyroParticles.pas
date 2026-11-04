unit TyroParticles;
{**
 *  This file is part of the "Tyro"
 *
 *  @license   MIT
 *
 *  @author    Zaher Dirkey
 *
 *  Engine-owned particle sparks: the explosion kit of a 2D/3D sprite game.
 *
 *  A script throws a burst with particles.emit(x, y [, count] [, options]) and
 *  forgets about it: the store owns every spark from then on. The engine steps
 *  the sparks during Update (the same step the sprite animations get, so a
 *  paused scene freezes them too) and draws them once per frame right after
 *  the 3D models, so an explosion always reads on top of the jets and sprites
 *  that made it. No per-frame call is needed from the script, exactly like the
 *  model pass.
 *
 *  Sparks live in plain screen space - x grows right, y grows down, gravity
 *  pulls toward the bottom edge, and the whole pass rides the camera offset
 *  (margin and shake) like the models do. That keeps one pixel world shared
 *  with the canvas/world drawing, so a 2D game and a 3D-model game throw the
 *  same explosion.
 *
 *  The store is one flat array under a lock (the script thread emits, the
 *  draw thread steps and draws), and the array is capped: when the sky is
 *  already full of sparks a new emit is dropped whole, so a script that fires
 *  wildly can never grow the list without bound.
 *}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, Math, Types, SyncObjs,
  mnClasses,
  RayLib, TyroClasses;

type
  { One spark: a screen-space dot with a velocity, a fading life and the ink
    it was born with. Dead sparks are swapped off the end of the array. }
  TParticle = record
    X, Y: Single;
    VX, VY: Single;
    Life: Single;           // seconds left
    TTL: Single;            // full lifetime at birth, for the fade curve
    Size: Single;           // radius in px at birth
    Gravity: Single;        // px/s^2, positive pulls toward the bottom edge
    Drag: Single;           // fraction of the speed shed per second
    Color: TColor;
  end;

  { One burst request: the shape every spark of an emit() is born from. The
    Lua layer fills the record with the script's options (or the friendly
    defaults), so the store itself needs no knowledge of the table format. }
  TParticleBurst = record
    X, Y: Single;           // screen pixel the burst throws from
    Color: TColor;          // the spark's ink; it fades out over its lifetime
    Count: Integer;         // how many sparks the burst throws
    Speed: Single;          // launch speed, px/s (jittered per spark)
    Life: Single;           // seconds a spark lives (jittered per spark)
    Size: Single;           // spark radius in px (jittered per spark)
    Gravity: Single;        // px/s^2, positive pulls toward the bottom edge
    Drag: Single;           // fraction of the speed shed per second
    Spread: Single;         // degrees; 360 throws a full ring
    Direction: Single;      // degrees; the middle of the spread, 0 = right
  end;

  { TParticles }

  { The store. One dense array, swap-remove when a spark dies. Like TModels3D
    it hands the script thread a lock, and the draw thread reads the live
    sparks under the same lock. }
  TParticles = class
  private
    const
      cParticleMax = 2048; //hard cap; a full sky drops new bursts
  private
    FLock: TmnCriticalSection;
    FItems: array of TParticle;
    FCount: Integer;
    //* The sparks' own xorshift state, seeded from the clock at Create. A
    //dedicated stream (not System.Random) on purpose: the game scripts' rand()
    //reads that global stream, and an engine singleton must not shuffle it -
    //the same run should spawn the same squadron every time.
    FRand: Cardinal;
    function NextRand: single;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    //* Drop every spark (an engine rerun does this).
    procedure Clear;
    // Throw one burst of sparks. Thread safe (the script thread calls it from
    // particles.emit). Returns the number of sparks born; 0 when the sky is
    // already full.
    function Emit(const Burst: TParticleBurst): integer;
    overload;
    // Convenience form for engine-side calls that just want a round burst.
    function Emit(X, Y: single; ACount: integer; AColor: TColor;
      ASpeed, ALife, ASize: single): integer; overload;
    // Advance every spark by one frame (main thread, from the engine Update).
    procedure Step(ADt: single);
    // Draw every spark (main thread, after the 3D models). AX/AY are the
    // Camera2D offset so the sparks shake with the world like the models do.
    procedure Draw(AX, AY: single);
    // True when at least one spark is alive, so the caller can skip the pass.
    function HasAny: boolean;
    property Count: integer read GetCount;
  end;

implementation

{ TParticles }

constructor TParticles.Create;
begin
  inherited Create;
  FLock := TmnCriticalSection.Create;
  SetLength(FItems, cParticleMax);
  FCount := 0;
  //Sparks are decoration, but two runs that explode the same jet should not
  //spray the identical pattern; seed the jitter from the clock. Zero would
  //lock a xorshift at zero, so nudge it into play.
  FRand := Cardinal(GetTickCount64 and $7FFFFFFF) or $5F3759DF;
end;

//xorshift32: a handful of bit ops, good enough for a spark's jitter, and it
//never touches the global Random the scripts use (see FRand).
function TParticles.NextRand: single;
begin
  FRand := FRand xor (FRand shl 13);
  FRand := FRand xor (FRand shr 17);
  FRand := FRand xor (FRand shl 5);
  Result := FRand / Cardinal($FFFFFFFF);
end;

destructor TParticles.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TParticles.Clear;
begin
  FLock.Enter;
  try
    FCount := 0;
  finally
    FLock.Leave;
  end;
end;

function TParticles.GetCount: integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

//Every spawn value gets a jitter so a burst of 30 sparks sprays like a real
//blast instead of one cloned ring: the angle walks the spread with a wobble,
//and the speed, lifetime and size shrink a little per spark.
function TParticles.Emit(const Burst: TParticleBurst): integer;
var
  I, N: Integer;
  P: TParticle;
  Ang: Single;
  StepAng: Single;
begin
  Result := 0;
  N := Burst.Count;
  if N <= 0 then
    Exit;
  FLock.Enter;
  try
    if FCount + N > cParticleMax then
      N := cParticleMax - FCount; //fill what fits, drop the rest
    if N <= 0 then
      Exit;
    StepAng := Burst.Spread;
    if N > 1 then
      StepAng := Burst.Spread / N;
    for I := 0 to N - 1 do
    begin
      Ang := (Burst.Direction - Burst.Spread * 0.5 + StepAng * (I + 0.5)
        + (NextRand - 0.5) * StepAng) * Pi / 180;
      P.X := Burst.X + (NextRand - 0.5) * 4;
      P.Y := Burst.Y + (NextRand - 0.5) * 4;
      P.VX := Cos(Ang) * Burst.Speed * (0.5 + NextRand * 0.65);
      P.VY := Sin(Ang) * Burst.Speed * (0.5 + NextRand * 0.65);
      P.TTL := Burst.Life * (0.7 + NextRand * 0.3);
      P.Life := P.TTL;
      P.Size := Burst.Size * (0.6 + NextRand * 0.8);
      P.Gravity := Burst.Gravity;
      P.Drag := Burst.Drag;
      P.Color := Burst.Color;
      FItems[FCount] := P;
      Inc(FCount);
    end;
    Result := N;
  finally
    FLock.Leave;
  end;
end;

function TParticles.Emit(X, Y: single; ACount: integer; AColor: TColor;
  ASpeed, ALife, ASize: single): integer;
var
  Burst: TParticleBurst;
begin
  Burst.X := X;
  Burst.Y := Y;
  Burst.Color := AColor;
  Burst.Count := ACount;
  Burst.Speed := ASpeed;
  Burst.Life := ALife;
  Burst.Size := ASize;
  Burst.Gravity := 0;
  Burst.Drag := 1;
  Burst.Spread := 360;
  Burst.Direction := 0;
  Result := Emit(Burst);
end;

//Drag first, then gravity, so a heavy gravity burst still slows down instead
//of accelerating forever; the ttl drives the fade in Draw.
procedure TParticles.Step(ADt: single);
var
  I: Integer;
  P: TParticle;
  Keep: Single;
begin
  if ADt <= 0 then
    Exit;
  FLock.Enter;
  try
    I := 0;
    while I < FCount do
    begin
      P := FItems[I];
      P.Life := P.Life - ADt;
      if P.Life <= 0 then
      begin
        Dec(FCount);
        FItems[I] := FItems[FCount]; //swap the last spark into the hole
        Continue;
      end;
      Keep := 1 - P.Drag * ADt;
      if Keep < 0 then
        Keep := 0;
      P.VX := P.VX * Keep;
      P.VY := P.VY * Keep + P.Gravity * ADt;
      P.X := P.X + P.VX * ADt;
      P.Y := P.Y + P.VY * ADt;
      FItems[I] := P;
      Inc(I);
    end;
  finally
    FLock.Leave;
  end;
end;

//A spark fades and shrinks along its life: alpha rides the remaining time
//straight down, the radius keeps a small core so the last frame still shows a
//dot instead of a vanishing smear.
procedure TParticles.Draw(AX, AY: single);
var
  I: Integer;
  P: TParticle;
  Col: TColor;
  Remain: Single;
begin
  FLock.Enter;
  try
    for I := 0 to FCount - 1 do
    begin
      P := FItems[I];
      Remain := P.Life / P.TTL;
      Col := P.Color;
      Col.RGBA.Alpha := Byte(Round(P.Color.RGBA.Alpha * Remain));
      RayLib.DrawCircleV(Vector2Of(P.X + AX, P.Y + AY),
        P.Size * (0.35 + 0.65 * Remain), Col);
    end;
  finally
    FLock.Leave;
  end;
end;

function TParticles.HasAny: boolean;
begin
  FLock.Enter;
  try
    Result := FCount > 0;
  finally
    FLock.Leave;
  end;
end;

end.
