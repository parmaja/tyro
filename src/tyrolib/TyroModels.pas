unit TyroModels;
{**
 *  This file is part of the "Tyro"
 *
 *  @license   MIT
 *
 *  @author    Zaher Dirkey
 *
 *  Native 3D model support: the "3D sprite" of a 2D game.
 *
 *  The store (TModels3D) owns the GPU models, one per object, and hands out
 *  handles, exactly like TSprites/TTiles do for their kinds. A script creates a
 *  model with models.new(), loads a mesh file with model:load() (GLB/glTF 2.0
 *  is the recommended one-file format: meshes, materials and textures in one
 *  package), then moves it by writing x/y like any sprite. The engine draws
 *  every visible model once per frame in its own 3D pass, so no per-frame
 *  draw call is needed from the script.
 *
 *  The 3D pass uses a top-down ORTHOGRAPHIC camera mapped pixel for pixel onto
 *  the window, so model x/y are screen pixels: x grows to the right, y grows
 *  down, and the model's own +z lands on screen "up". The remaining world axis
 *  (y, into the screen) is the model's depth: lower z (nearer the eye) draws a
 *  model in front of the ones behind it. Angle spins the model around the view
 *  axis (clockwise on screen, the sprite convention); Tilt tips it around the
 *  screen's horizontal axis, which foreshortens it for a cheap banking pose.
 *
 *  A model with Shadow on casts a real shadow: its mesh is drawn a second time
 *  through a plane-projection matrix that flattens it along the light ray onto
 *  the world's ground plane (world.light / world.ambient / world.ground in
 *  Lua), so the silhouette keeps the mesh's shape and banking, and a cloud
 *  that floats between the viewer and the sea covers the shadows below it.
 *
 *  LoadModel uploads to the GPU, so load runs on the main thread through the
 *  queue (TLoadModelObject). The bounding box is captured at load time for two
 *  jobs: the store reports width/height/depth to the script (to size a model
 *  in pixels), and the box centre is the draw pivot, so a model whose mesh sits
 *  off the origin (the usual Sketchfab export) still rotates around itself.
 *}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, Math, Types, SyncObjs, Generics.Collections,
  mnClasses,
  RayLib, RayClasses,
  TyroScripts, TyroClasses;

const
  cModel3DInvalid: Integer = 0;

type
  { TModel3D }

  { One 3D model: the raylib model it draws, its bounding box (centre = pivot,
    sizes for the script to scale by) and the transform the engine applies in
    the per-frame 3D pass. X/Y are screen pixels, Z is depth into the screen. }
  TModel3D = class(TTyroObject)
  public
    Handle: Integer;
    Name: string;
    Model: TModel;          // raylib model; MeshCount = 0 while nothing is loaded
    Center: TVector3;       // bounding-box centre: the pivot the model turns around
    SizeX: Single;          // bounding-box size along the model's own axes
    SizeY: Single;
    SizeZ: Single;
    Visible: Boolean;
    X: Single;              // screen pixel position (like a sprite)
    Y: Single;
    Z: Single;              // depth (bigger = farther from the eye, behind)
    Angle: Single;          // rotation around the view axis, degrees, clockwise on screen
    Tilt: Single;           // rotation around the screen's horizontal axis, degrees
    Roll: Single;           // rotation around the model's own axis (the nose-tail
                            // line of a jet), degrees - the classic bank-into-turn
    Shadow: Boolean;        // cast a shadow onto the world's ground plane
    Scale: Single;
    Tint: TColor;
    constructor Create(AHandle: Integer; const AName: string);
    destructor Destroy; override;
    function Loaded: Boolean;
  end;

  { TModels3D }

  { The store. Mirrors TTiles: a lock, a dictionary keyed by handle and an
    ever-increasing handle counter, so a handle a script still holds can never
    address a model of a later run. The engine drains the visible models once
    per frame inside DrawAll, reading the live transform under the same lock. }
  TModels3D = class(TObject)
  private
    FLock: TmnCriticalSection;
    FItems: TDictionary<Integer, TModel3D>;
    FNextHandle: Integer;
    //Cast-shadow light (see DrawAll). LightDir is the direction the light
    //travels, Ambient (0..1) fills the shadow with sky light, and GroundY is
    //the world-depth plane the shadows fall on (the "sea floor" behind the
    //models). They live on the store (not a single model) because every model
    //shares one sun, exactly like a real top-down scene.
    FLightDir: TVector3;
    FAmbient: Single;
    FGroundY: Single;
    function GetCount: integer;
    function GetItems(Index: Integer): TModel3D;
    //Frees the GPU model a slot owns and resets it to unloaded.
    procedure FreeModelData(AItem: TModel3D);
  public
    constructor Create;
    destructor Destroy; override;
    //* Unload every model and empty the store (an engine rerun does this).
    procedure Clear;
    // Add a mesh-less model, returns a handle
    function AddEmpty(const AName: string = ''): integer;
    // Generate a UV sphere mesh into an existing handle (main thread, like
    // Load). The script scales/positions it like any other model; it is the
    // primitive a top-down "cloud puff" or a placeholder is built from, so a
    // game needs no mesh file to put a real 3D shape on screen.
    function BuildSphere(Handle: integer; ARadius: Single): boolean;
    function FindByName(const AName: string): integer;
    function Exists(Handle: integer): boolean;

    // Load (or replace) one model's mesh from a file. Must run on the main
    // thread: LoadModel uploads buffers/textures. Returns success.
    function Load(Handle: integer; const AFileName: string): boolean;

    // -- cast-shadow light (mirrored on the Lua 'world' table) --
    procedure SetLight(X, Y, Z: single);
    procedure GetLight(out X, Y, Z: single);
    procedure SetAmbient(AValue: single);
    function GetAmbient: single;
    procedure SetGroundY(AValue: single);
    function GetGroundY: single;

    function GetName(Handle: integer): string;
    procedure SetName(Handle: integer; const AName: string);
    function IsLoaded(Handle: integer): boolean;
    function GetWidth(Handle: integer): Single;
    function GetHeight(Handle: integer): Single;
    function GetDepth(Handle: integer): Single;

    procedure SetVisible(Handle: integer; AVisible: boolean);
    function GetVisible(Handle: integer): boolean;
    procedure SetPosition(Handle: integer; AX, AY, AZ: single);
    procedure SetPositionComponent(Handle: integer; AX: Boolean; Value: single);
    procedure SetZ(Handle: integer; Value: single);
    function GetX(Handle: integer): single;
    function GetY(Handle: integer): single;
    function GetZ(Handle: integer): single;
    procedure SetAngle(Handle: integer; AAngle: single);
    function GetAngle(Handle: integer): single;
    procedure SetTilt(Handle: integer; ATilt: single);
    function GetTilt(Handle: integer): single;
    procedure SetRoll(Handle: integer; ARoll: single);
    function GetRoll(Handle: integer): single;
    procedure SetShadow(Handle: integer; AShadow: boolean);
    function GetShadow(Handle: integer): boolean;
    procedure SetScale(Handle: integer; AScale: single);
    function GetScale(Handle: integer): single;
    procedure SetTint(Handle: integer; ATint: TColor);
    function GetTint(Handle: integer): TColor;
    // 0..255 opacity, folded into the tint's alpha channel: a see-through
    // jet shows what is behind it, and a stack of soft spheres reads as one
    // misty volume instead of hard white balls.
    procedure SetAlpha(Handle: integer; AAlpha: integer);
    function GetAlpha(Handle: integer): integer;

    // True when at least one visible model holds a mesh, so the caller can skip
    // the whole BeginMode3D/EndMode3D pass on a models-free frame.
    function HasVisible: boolean;

    // Draw every visible model under the top-down pixel camera. AX/AY shift the
    // whole pass so the models ride with the 2D camera (window margin, shake);
    // AWidth/AHeight are the screen size in pixels.
    procedure DrawAll(AX, AY: single; AWidth, AHeight: Integer);

    property Count: integer read GetCount;
    property Items[Index: Integer]: TModel3D read GetItems; default;
  end;

  { TLoadModelObject }

  { Loads a mesh file into a model on the main thread (the upload needs the GL
    context) and returns whether it succeeded. }
  TLoadModelObject = class(TQueueObject)
  private
    FFileName: string;
    FHandle: integer;
    FResult: boolean;
  public
    constructor Create(AHandle: integer; const AFileName: string);
    procedure DoExecute; override;
    property HandleResult: boolean read FResult;
  end;

  { TSphereModelObject }

  { Builds the builtin sphere mesh into a model on the main thread (the upload
    needs the GL context) and returns whether it succeeded. }
  TSphereModelObject = class(TQueueObject)
  private
    FRadius: Single;
    FHandle: integer;
    FResult: boolean;
  public
    constructor Create(AHandle: integer; ARadius: Single);
    procedure DoExecute; override;
    property HandleResult: boolean read FResult;
  end;

implementation

uses
  TyroEngines;

{ TModel3D }

constructor TModel3D.Create(AHandle: Integer; const AName: string);
begin
  inherited Create;
  Handle := AHandle;
  Name := AName;
  Model := Default(TModel);
  Center := Vector3Of(0, 0, 0);
  SizeX := 0;
  SizeY := 0;
  SizeZ := 0;
  Visible := True;
  X := 0;
  Y := 0;
  Z := 0;
  Angle := 0;
  Tilt := 0;
  Roll := 0;
  Shadow := True;
  Scale := 1;
  Tint := clWhite;
end;

destructor TModel3D.Destroy;
begin
  //The store only frees models on the main thread (Clear/Load), so the GL
  //context is alive here, same rule as the sprite/texture owners.
  if Loaded then
    RayLib.UnloadModel(Model);
  Model := Default(TModel);
  inherited;
end;

function TModel3D.Loaded: Boolean;
begin
  Result := Model.MeshCount > 0;
end;

{ TModels3D }

constructor TModels3D.Create;
begin
  inherited Create;
  FLock := TmnCriticalSection.Create;
  FItems := TDictionary<Integer, TModel3D>.Create;
  FNextHandle := 1;
  //A high sun a little behind and above the viewer: light travels toward the
  //far end of the depth axis (+y), tilted slightly so shadows slide a bit away
  //from the model instead of hiding under it - the classic flying-shadow look,
  //now as the mesh's real projected silhouette.
  FLightDir := Vector3Of(0.06, 1, -0.035);
  FAmbient := 0.45;
  FGroundY := 300;
end;

destructor TModels3D.Destroy;
begin
  Clear;
  FItems.Free;
  FLock.Free;
  inherited;
end;

procedure TModels3D.FreeModelData(AItem: TModel3D);
begin
  if AItem = nil then
    Exit;
  if AItem.Loaded then
    RayLib.UnloadModel(AItem.Model);
  AItem.Model := Default(TModel);
end;

procedure TModels3D.Clear;
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    for Item in FItems.Values do
      Item.Free;
    FItems.Clear;
    //A rerun starts under its own sky: restore the default sun until the new
    //script calls world.light/world.ambient/world.ground again.
    FLightDir := Vector3Of(0.06, 1, -0.035);
    FAmbient := 0.45;
    FGroundY := 300;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TModels3D.GetItems(Index: Integer): TModel3D;
begin
  Result := FItems[Index];
end;

function TModels3D.AddEmpty(const AName: string): integer;
begin
  FLock.Enter;
  try
    FItems.Add(FNextHandle, TModel3D.Create(FNextHandle, AName));
    Result := FNextHandle;
    Inc(FNextHandle);
  finally
    FLock.Leave;
  end;
end;

//Generate a UV sphere and upload it into an existing slot, mirroring Load:
//GenMeshSphere/LoadModelFromMesh are by-value record calls, but the same
//hazard hit only DrawModelEx (whose binding signature did not match this
//build); DrawModel and LoadModel pass and return full records every frame
//without trouble, so the sphere is safe here. Must run on the main thread.
function TModels3D.BuildSphere(Handle: integer; ARadius: Single): boolean;
var
  Item: TModel3D;
  NewModel: TModel;
  Mesh: TMesh;
  Box: TBoundingBox;
begin
  Result := False;
  if ARadius <= 0 then
    ARadius := 1;
  //12 rings / 16 slices: smooth enough for a puff seen from above, cheap for
  //dozens of clouds.
  Mesh := RayLib.GenMeshSphere(ARadius, 12, 16);
  NewModel := RayLib.LoadModelFromMesh(Mesh);
  if NewModel.MeshCount <= 0 then
  begin
    if IsConsole then
      WriteLn('Sphere model not built: radius ' + FloatToStr(ARadius));
    Exit;
  end;

  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Item) then
    begin
      RayLib.UnloadModel(NewModel); //store did not take ownership
      Exit;
    end;
    FreeModelData(Item);
    Item.Model := NewModel;

    //Same bookkeeping as Load: the box feeds the script-side sizes and the
    //pivot, so the sphere stays centred on its x/y/z however it is moved.
    Box := RayLib.GetModelBoundingBox(NewModel);
    Item.Center := Vector3Of(
      (Box.Min.X + Box.Max.X) * 0.5,
      (Box.Min.Y + Box.Max.Y) * 0.5,
      (Box.Min.Z + Box.Max.Z) * 0.5);
    Item.SizeX := Abs(Box.Max.X - Box.Min.X);
    Item.SizeY := Abs(Box.Max.Y - Box.Min.Y);
    Item.SizeZ := Abs(Box.Max.Z - Box.Min.Z);
    Result := True;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.FindByName(const AName: string): integer;
var
  Item: TModel3D;
begin
  Result := cModel3DInvalid;
  if AName = '' then
    Exit;
  FLock.Enter;
  try
    for Item in FItems.Values do
      if Item.Name = AName then
      begin
        Result := Item.Handle;
        Exit;
      end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.Exists(Handle: integer): boolean;
begin
  FLock.Enter;
  try
    Result := FItems.ContainsKey(Handle);
  finally
    FLock.Leave;
  end;
end;

function TModels3D.Load(Handle: integer; const AFileName: string): boolean;
var
  Item: TModel3D;
  NewModel: TModel;
  Box: TBoundingBox;
begin
  Result := False;
  //LoadModel needs the GL context: the caller routes this through the queue.
  NewModel := RayLib.LoadModel(PUTF8Char(UTF8String(AFileName)));
  if NewModel.MeshCount <= 0 then
  begin
    if IsConsole then
      WriteLn('Model not loaded: ' + AFileName);
    Exit;
  end;

  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Item) then
    begin
      RayLib.UnloadModel(NewModel); //store did not take ownership
      Exit;
    end;
    FreeModelData(Item);
    Item.Model := NewModel;

    //The bounding box feeds both the script-side size properties and the draw
    //pivot: meshes that sit off the origin still turn around themselves.
    Box := RayLib.GetModelBoundingBox(NewModel);
    Item.Center := Vector3Of(
      (Box.Min.X + Box.Max.X) * 0.5,
      (Box.Min.Y + Box.Max.Y) * 0.5,
      (Box.Min.Z + Box.Max.Z) * 0.5);
    Item.SizeX := Abs(Box.Max.X - Box.Min.X);
    Item.SizeY := Abs(Box.Max.Y - Box.Min.Y);
    Item.SizeZ := Abs(Box.Max.Z - Box.Min.Z);
    Result := True;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetName(Handle: integer): string;
var
  Item: TModel3D;
begin
  Result := '';
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Name;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetName(Handle: integer; const AName: string);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Name := AName;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.IsLoaded(Handle: integer): boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Loaded;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetWidth(Handle: integer): Single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.SizeX;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetHeight(Handle: integer): Single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.SizeY;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetDepth(Handle: integer): Single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.SizeZ;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetVisible(Handle: integer; AVisible: boolean);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Visible := AVisible;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetVisible(Handle: integer): boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Visible;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetPosition(Handle: integer; AX, AY, AZ: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      Item.X := AX;
      Item.Y := AY;
      Item.Z := AZ;
    end;
  finally
    FLock.Leave;
  end;
end;

//Which axis: X and Y are screen pixels, the third slot is depth, so the setter
//takes a component flag like TTiles.SetPositionComponent but with three cases.
procedure TModels3D.SetPositionComponent(Handle: integer; AX: Boolean; Value: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      if AX then
        Item.X := Value
      else
        Item.Y := Value;
    end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetX(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.X;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetZ(Handle: integer; Value: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Z := Value;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetY(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Y;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetZ(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Z;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetAngle(Handle: integer; AAngle: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Angle := AAngle;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAngle(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Angle;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetTilt(Handle: integer; ATilt: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Tilt := ATilt;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetTilt(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Tilt;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetRoll(Handle: integer; ARoll: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Roll := ARoll;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetRoll(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Roll;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetShadow(Handle: integer; AShadow: boolean);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Shadow := AShadow;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetShadow(Handle: integer): boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Shadow;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetScale(Handle: integer; AScale: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Scale := AScale;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetScale(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 1;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Scale;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetTint(Handle: integer; ATint: TColor);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Tint := ATint;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetTint(Handle: integer): TColor;
var
  Item: TModel3D;
begin
  Result := clWhite;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Tint;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetAlpha(Handle: integer; AAlpha: integer);
var
  Item: TModel3D;
begin
  if AAlpha < 0 then
    AAlpha := 0
  else if AAlpha > 255 then
    AAlpha := 255;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Tint := Item.Tint.RGBA.ReplaceAlpha(Byte(AAlpha));
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAlpha(Handle: integer): integer;
var
  Item: TModel3D;
begin
  Result := 255;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Tint.RGBA.Alpha;
  finally
    FLock.Leave;
  end;
end;

//The light is one shared sun (the store's), read by every cast shadow; the
//script reaches it through world.light/world.ambient/world.ground.
procedure TModels3D.SetLight(X, Y, Z: single);
begin
  FLock.Enter;
  try
    //A light with no depth component never reaches the ground plane; keep the
    //last direction instead of producing a division by zero at draw time.
    if Y = 0 then
      Exit;
    FLightDir := Vector3Of(X, Y, Z);
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.GetLight(out X, Y, Z: single);
begin
  FLock.Enter;
  try
    X := FLightDir.X;
    Y := FLightDir.Y;
    Z := FLightDir.Z;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetAmbient(AValue: single);
begin
  FLock.Enter;
  try
    FAmbient := Min(1, Max(0, AValue));
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAmbient: single;
begin
  FLock.Enter;
  try
    Result := FAmbient;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetGroundY(AValue: single);
begin
  FLock.Enter;
  try
    FGroundY := AValue;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetGroundY: single;
begin
  FLock.Enter;
  try
    Result := FGroundY;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.HasVisible: boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    for Item in FItems.Values do
      if Item.Visible and Item.Loaded then
        Exit(True);
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.DrawAll(AX, AY: single; AWidth, AHeight: Integer);
const
  //Distance of the eye above the pixel plane. The ortho frustum clips at
  //0.01..1000, so this leaves the models room for a +/-399 depth range.
  cModel3DDistance = 400.0;
  //Cast-shadow look: the mesh's real silhouette projected onto the ground
  //plane along the light ray. Ambient light fills the shadow, so at full
  //ambient the alpha drops to 0 and the whole pass switches itself off.
  cModel3DShadowMaxAlpha = 150;
  cModel3DShadowInk: TColor = (RGBA: (Red: 8; Green: 8; Blue: 20; Alpha: 0));
var
  Cam: TCamera3D;
  Item: TModel3D;
  S: Single;
  Pos: TVector3;
  ShadowM: TMatrix;
  ShadowTint: TColor;
  ItemShadowTint: TColor;
  KX, KZ, G, LY: Single;
  Alpha: Integer;
  AnyVisible: Boolean;
begin
  AnyVisible := False;
  FLock.Enter;
  try
    for Item in FItems.Values do
      if Item.Visible and Item.Loaded then
      begin
        AnyVisible := True;
        Break;
      end;
    if not AnyVisible then
      Exit;
    if AWidth <= 0 then
      AWidth := 1;
    if AHeight <= 0 then
      AHeight := 1;

    //The projection maps P onto the plane y=G along L: x'=x-kx*(y-G),
    //y'=G, z'=z-kz*(y-G) with kx=L.x/L.y, kz=L.z/L.y. One matrix does it for
    //the whole mesh, so the shadow is the mesh's true silhouette - wing, bank
    //and all. Note the field filling: rlMultMatrixf receives the raw float
    //array and raylib reads it as an OpenGL column-major matrix (element at
    //row+col*4), so with raylib's field naming the translation lands in
    //m3,m7,m11,m15 - filling m12..m14 instead transposes the shear and the
    //w row turns the flat shadow into fanned perspective streaks.
    LY := FLightDir.Y;
    if LY = 0 then
      LY := 1; //SetLight keeps it non-zero; belt and braces for the division.
    KX := FLightDir.X / LY;
    KZ := FLightDir.Z / LY;
    G := FGroundY;
    ShadowM.m0 := 1;  ShadowM.m1 := -KX; ShadowM.m2 := 0;  ShadowM.m3 := KX * G;
    ShadowM.m4 := 0;  ShadowM.m5 := 0;   ShadowM.m6 := 0;  ShadowM.m7 := G;
    ShadowM.m8 := 0;  ShadowM.m9 := -KZ; ShadowM.m10 := 1; ShadowM.m11 := KZ * G;
    ShadowM.m12 := 0; ShadowM.m13 := 0;  ShadowM.m14 := 0; ShadowM.m15 := 1;
    Alpha := Round((1 - FAmbient) * cModel3DShadowMaxAlpha);
    ShadowTint := cModel3DShadowInk;
    ShadowTint.RGBA.Alpha := Byte(Alpha);

    //Top-down orthographic camera: world +x is screen right, world -z is screen
    //down (so a model's own +z points "up" like a sprite's), and world +y runs
    //away from the viewer as the depth axis. For an ortho camera raylib uses
    //Fovy as the visible height in world units, so Fovy = pixels maps one world
    //unit to one screen pixel; the ortho box is centred on the camera axis, so
    //pixel coordinates are shifted by half the screen below.
    Cam.Position := Vector3Of(0, -cModel3DDistance, 0);
    Cam.Target := Vector3Of(0, 0, 0);
    Cam.Up := Vector3Of(0, 0, 1);
    Cam.Fovy := AHeight;
    Cam.Projection := Ord(CAMERA_ORTHOGRAPHIC); //the field is a plain Integer
    RayLib.BeginMode3D(Cam);
    try
      //The models are real solids: the depth test hides their far faces. It
      //is switched back off at the end of the pass - every layer above the
      //3D scene (sparks, controls, the screen buffer) is 2D at one fixed
      //depth, and the depth left in the buffer would otherwise punch the
      //models' silhouettes into those layers (a cloud hiding the HUD text).
      RayLib.rlEnableDepthTest;
      //Pass 1 - the shadows, cast onto the world's ground plane before any
      //model touches the depth buffer. They write no depth themselves
      //(rlDisableDepthMask), so overlapping shadows blend together into a
      //denser shade instead of clipping each other, and any model drawn in
      //pass 2 simply paints over the shadow beneath it: a cloud between the
      //viewer and the sea hides the shadow under it, as it should.
      if Alpha > 0 then
      begin
        RayLib.rlDisableDepthMask;
        for Item in FItems.Values do
        begin
          if (not Item.Visible) or (not Item.Loaded) or (not Item.Shadow) then
            Continue;
          //A see-through model (a misty cloud puff) casts a see-through
          //shadow; the pass writes no depth, so overlapping puff shadows
          //stack into a denser shade exactly like the puffs themselves.
          ItemShadowTint := ShadowTint;
          if Item.Tint.RGBA.Alpha < 255 then
            ItemShadowTint.RGBA.Alpha := Byte(Round(Alpha * Item.Tint.RGBA.Alpha / 255));
          S := Item.Scale;
          if S <= 0 then
            S := 1;
          Pos.X := Item.X + AX - AWidth * 0.5;
          Pos.Y := Item.Z;
          Pos.Z := AHeight * 0.5 - (Item.Y + AY);
          RayLib.rlPushMatrix;
          //The projection first (outermost), then the model's own transform:
          //the mesh rides into the world, and the sun flattens it onto the sea.
          RayLib.rlMultMatrixf(@ShadowM);
          RayLib.rlTranslatef(Pos.X, Pos.Y, Pos.Z);
          RayLib.rlRotatef(Item.Angle, 0, 1, 0);
          RayLib.rlRotatef(Item.Roll, 0, 0, 1);
          RayLib.rlRotatef(Item.Tilt, 1, 0, 0);
          RayLib.rlScalef(S, S, S);
          RayLib.rlTranslatef(-Item.Center.X, -Item.Center.Y, -Item.Center.Z);
          RayLib.DrawModel(Item.Model, Vector3Of(0, 0, 0), 1.0, ItemShadowTint);
          RayLib.rlPopMatrix;
        end;
        RayLib.rlEnableDepthMask;
      end;

      //Pass 2 - the models themselves.
      for Item in FItems.Values do
      begin
        if (not Item.Visible) or (not Item.Loaded) then
          Continue;
        S := Item.Scale;
        if S <= 0 then
          S := 1;

        //Screen-pixel anchor: x runs right, y runs down (so it becomes -z), and
        //the model's Z depth rides along the world y axis into the screen.
        Pos.X := Item.X + AX - AWidth * 0.5;
        Pos.Y := Item.Z;                              //depth (bigger = farther)
        Pos.Z := AHeight * 0.5 - (Item.Y + AY);       //pixel y, screen down

        //Build the transform on rlgl's matrix stack rather than DrawModelEx:
        //the by-value records that binding passes have an ABI mismatch against
        //this raylib build, while the float-only rlTranslate/Rotate/Scale calls
        //compose the exact same position + spin + bank + pivot transform.
        RayLib.rlPushMatrix;
        RayLib.rlTranslatef(Pos.X, Pos.Y, Pos.Z);
        //Spin around the view axis (world y), then roll around the model's own
        //axis, then pitch around the screen's horizontal axis (world x).
        //Applied after the pivot translate below, so all of them turn the
        //model about its own centre, not the world origin.
        RayLib.rlRotatef(Item.Angle, 0, 1, 0);
        RayLib.rlRotatef(Item.Roll, 0, 0, 1);
        RayLib.rlRotatef(Item.Tilt, 1, 0, 0);
        RayLib.rlScalef(S, S, S);
        //Shift the (still unrotated) box centre onto the origin so the model
        //turns about its own middle even when its mesh sits off the origin.
        RayLib.rlTranslatef(-Item.Center.X, -Item.Center.Y, -Item.Center.Z);
        RayLib.DrawModel(Item.Model, Vector3Of(0, 0, 0), 1.0, Item.Tint);
        RayLib.rlPopMatrix;
      end;
    finally
      RayLib.EndMode3D;
      //Leave the depth test off for the 2D layers painted after this pass.
      RayLib.rlDisableDepthTest;
    end;
  finally
    FLock.Leave;
  end;
end;

{ TLoadModelObject }

constructor TLoadModelObject.Create(AHandle: integer; const AFileName: string);
begin
  inherited Create;
  FHandle := AHandle;
  FFileName := AFileName;
  FResult := False;
end;

procedure TLoadModelObject.DoExecute;
begin
  if Engine.Models <> nil then
    FResult := Engine.Models.Load(FHandle, FFileName);
end;

{ TSphereModelObject }

constructor TSphereModelObject.Create(AHandle: integer; ARadius: Single);
begin
  inherited Create;
  FHandle := AHandle;
  FRadius := ARadius;
  FResult := False;
end;

procedure TSphereModelObject.DoExecute;
begin
  if Engine.Models <> nil then
    FResult := Engine.Models.BuildSphere(FHandle, FRadius);
end;

end.
