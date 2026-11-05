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
 *  The shadow is painted through a flat ink material rather than the model's
 *  own: a shadow is only the outline a body leaves, so the mesh's texture and
 *  shading stay out of it and the silhouette reads as one solid shade. The
 *  silhouettes are inked onto a screen-sized sheet first and that sheet is laid
 *  over the world in one stroke, so no part of a body can ink one spot twice
 *  and the shade cannot turn into a map of the model inside it.
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
    //The shadow's penumbra in screen pixels: the sun is no point source, so a
    //shadow edge is never a knife. The inked sheet is Gaussian-blurred by this
    //radius before it is laid, which turns every silhouette's rim into a soft
    //band (0 keeps the old hard ink). One sun, one softness for the scene,
    //exactly like the light itself.
    FSoftness: Single;
    //The shadow ink: raylib's default material, which is the plain unlit shader
    //and a 1x1 white pixel, so whatever color is written into its albedo comes
    //out at the other end unchanged - a solid silhouette with no texture and no
    //shading in it. Built on the first frame that casts a shadow (the build
    //needs the GL context) and freed with the store.
    FShadowMat: TMaterial;
    FShadowReady: Boolean;
    //The shadow sheet: a screen-sized cover the silhouettes are inked onto
    //before any of it reaches the world. On the sheet the ink is opaque, so a
    //spot is either covered or not and a part of a body crossing another part
    //of itself cannot lay a second, darker coat. Laid over the world in one
    //stroke with the ink's own alpha. Built on the first frame that casts a
    //shadow (it needs the GL context), rebuilt when the window changes size and
    //freed with the store.
    FShadowSheet: TRenderTexture2D;
    //The blur pass: a second sheet of the same size the ink travels through
    //for the vertical leg of the separable Gaussian, and the small blur shader
    //that smears it. Built on the first frame that casts a soft shadow (they
    //need the GL context), rebuilt on a window resize, freed with the store.
    FBlurSheet: TRenderTexture2D;
    FBlurShader: TShader;
    FBlurReady: Boolean;
    function GetCount: integer;
    function GetItems(Index: Integer): TModel3D;
    //Frees the GPU model a slot owns and resets it to unloaded.
    procedure FreeModelData(AItem: TModel3D);
    //Releases the shared shadow ink (main thread, GL context alive).
    procedure FreeShadowMaterial;
    //The store's one shadow ink, built on demand (see FShadowMat). Its Maps is
    //nil when it could not be built, and the caller then casts no shade.
    function ShadowMaterial: TMaterial;
    //One model's meshes, all painted with the shadow ink. The model's pose is
    //already on the rlgl matrix stack, so the meshes go down with the identity.
    procedure DrawShadowMeshes(AModel: TModel; AInk: TColor);
    //Releases the shadow sheet (main thread, GL context alive).
    procedure FreeShadowSheet;
    //The store's shadow sheet, built on demand and rebuilt when the window
    //changes size (see FShadowSheet). False when it could not be made, and the
    //world then stays without a shade.
    function ShadowSheet(AWidth, AHeight: Integer): Boolean;
    //One body's shadow pose, pushed onto the rlgl matrix stack: the sun's
    //projection outermost, then the body's own transform, so the mesh rides
    //into the world and the light flattens it onto the sea. The caller closes
    //the matrix it opened here.
    procedure PushShadowPose(AItem: TModel3D; const AProjection: TMatrix;
      AX, AY: Single; AWidth, AHeight: Integer);
    //Wipes the sheet and inks it with the silhouette of every body whose own
    //alpha is ATintAlpha (255 = the ordinary, solid bodies, inked all together;
    //a see-through body is inked on its own so its thinness stays its own).
    //True when it inked anything, which is when the sheet is worth laying.
    function InkShadowSheet(const ACam: TCamera3D; const AProjection: TMatrix;
      AX, AY: Single; AWidth, AHeight: Integer; ATintAlpha: Byte): Boolean;
    //Lays the inked sheet over the world: one coat of ink, under every model.
    procedure LayShadowSheet(AAlpha: Integer);
    //True when the blur shader and its second sheet stand ready at the
    //window's size (built on demand, like the shadow sheet). False only means
    //the shade goes down hard instead of not going down at all.
    function BlurPassReady: Boolean;
    //Smears the inked sheet: one horizontal Gaussian leg into the blur sheet
    //and one vertical leg back, so the sheet itself ends up soft and the lay
    //step never learns a blur happened. Needs the GL context; call it between
    //InkShadowSheet and LayShadowSheet, when BlurPassReady said yes.
    procedure BlurShadowSheet;
    //Releases the blur sheet and shader (main thread, GL context alive).
    procedure FreeBlurPass;
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
    //The penumbra radius in screen pixels (see FSoftness): 0..6, clamped.
    procedure SetSoftness(AValue: single);
    function GetSoftness: single;

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

type
  //raylib hands a model's meshes out as a plain C array. This is the Pascal
  //handle to walk it: the element count is only a range-check cover, nothing
  //is ever allocated through it.
  TMeshArray = array[0..MaxInt div SizeOf(TMesh) - 1] of TMesh;
  PMeshArray = ^TMeshArray;

const
  //Cast-shadow look: the ink the silhouette is laid in, and how dark it can
  //get. Ambient light fills a shadow, so at full ambient the alpha drops to 0
  //and the whole shade switches itself off. Here rather than in DrawAll
  //because the sheet, the ink and the coat that lays it are three hands on the
  //same colour.
  cModel3DShadowMaxAlpha = 150;
  cModel3DShadowInk: TColor = (RGBA: (Red: 8; Green: 8; Blue: 20; Alpha: 0));
  //The shadow meshes are posed on the rlgl matrix stack, exactly like the
  //models themselves, so the matrix DrawMesh is handed is the identity: the
  //mesh transform is composed with the current stack matrix inside DrawMesh.
  cModel3DIdentity: TMatrix =
    (m0: 1;  m4: 0;  m8: 0;  m12: 0;
     m1: 0;  m5: 1;  m9: 0;  m13: 0;
     m2: 0;  m6: 0;  m10: 1; m14: 0;
     m3: 0;  m7: 0;  m11: 0; m15: 1);

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
  //Hard ink until a script asks for a penumbra (world.shadowsoft), so every
  //game but the ones that opt in keep the old crisp silhouette.
  FSoftness := 0;
  FBlurReady := False;
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

//raylib's default material is the flat, unlit shader and its 1x1 white pixel:
//write a color into its albedo and that exact color - its alpha included -
//comes out over the background. That is all a shadow should be, so neither the
//mesh's texture nor its lighting shows up inside it. Built here rather than in
//Create because the material wants the GL context, which only exists once the
//engine has a window.
function TModels3D.ShadowMaterial: TMaterial;
begin
  if not FShadowReady then
  begin
    FShadowMat := RayLib.LoadMaterialDefault;
    FShadowReady := (FShadowMat.Shader.ID > 0) and (FShadowMat.Maps <> nil);
  end;
  Result := FShadowMat;
end;

procedure TModels3D.FreeShadowMaterial;
begin
  if FShadowReady then
  begin
    //raylib keeps the default shader and its white pixel itself, so this only
    //hands back the maps array LoadMaterialDefault allocated for our copy.
    RayLib.UnloadMaterial(FShadowMat);
    FShadowMat := Default(TMaterial);
    FShadowReady := False;
  end;
end;

//The ink goes onto the material's albedo (map 0), and the flat shader has
//nothing else to add, so every mesh of the model comes out in that one color.
//The meshes ride the rlgl matrix stack the caller set up, hence the identity.
procedure TModels3D.DrawShadowMeshes(AModel: TModel; AInk: TColor);
var
  I: Integer;
  InkMat: TMaterial;
begin
  if AModel.Meshes = nil then
    Exit;
  InkMat := FShadowMat;
  InkMat.Maps^.Color := AInk;
  for I := 0 to AModel.MeshCount - 1 do
    RayLib.DrawMesh(PMeshArray(AModel.Meshes)^[I], InkMat, cModel3DIdentity);
end;

procedure TModels3D.FreeShadowSheet;
begin
  if FShadowSheet.ID <> 0 then
  begin
    RayLib.UnloadRenderTexture(FShadowSheet);
    FShadowSheet := Default(TRenderTexture2D);
  end;
end;

//The sheet is a piece of the window's size, so a resized window simply gets a
//new one. Made here rather than in Create because it wants the GL context,
//which only exists once the engine has a window.
function TModels3D.ShadowSheet(AWidth, AHeight: Integer): Boolean;
begin
  if (FShadowSheet.ID <> 0)
    and ((FShadowSheet.Texture.Width <> AWidth) or
         (FShadowSheet.Texture.Height <> AHeight)) then
    FreeShadowSheet;
  if FShadowSheet.ID = 0 then
  begin
    FShadowSheet := RayLib.LoadRenderTexture(AWidth, AHeight);
    if not RayLib.IsRenderTextureValid(FShadowSheet) then
    begin
      //UnloadRenderTexture on a sheet that was never made would hand back a
      //buffer that does not exist, so it is dropped instead.
      FShadowSheet := Default(TRenderTexture2D);
      Exit(False);
    end;
    //The sheet is read back one texel to one pixel, so it must never be
    //smeared: a smoothed sheet would soften the silhouette's edge.
    RayLib.SetTextureFilter(FShadowSheet.Texture, TEXTURE_FILTER_POINT);
  end;
  Result := True;
end;

//The projection is pushed first so it ends up the outermost matrix: the whole
//mesh is flattened along the light ray onto the sea, and the body's own
//transform under it decides which part of the mesh that is.
procedure TModels3D.PushShadowPose(AItem: TModel3D; const AProjection: TMatrix;
  AX, AY: Single; AWidth, AHeight: Integer);
var
  S: Single;
begin
  S := AItem.Scale;
  if S <= 0 then
    S := 1;
  RayLib.rlPushMatrix;
  RayLib.rlMultMatrixf(@AProjection);
  //The same screen-pixel anchor the models get, so the shade belongs to the
  //body that casts it: x runs right, y runs down (so it becomes -z), and the
  //body's own Z depth rides along the world y axis.
  RayLib.rlTranslatef(AItem.X + AX - AWidth * 0.5, AItem.Z,
    AHeight * 0.5 - (AItem.Y + AY));
  RayLib.rlRotatef(AItem.Angle, 0, 1, 0);
  RayLib.rlRotatef(AItem.Roll, 0, 0, 1);
  RayLib.rlRotatef(AItem.Tilt, 1, 0, 0);
  RayLib.rlScalef(S, S, S);
  RayLib.rlTranslatef(-AItem.Center.X, -AItem.Center.Y, -AItem.Center.Z);
end;

//Wiping the sheet and inking it from scratch is cheaper than keeping it tidy
//between frames, and it is the only way to be sure an old silhouette cannot
//leak into today's shade. On the sheet the ink goes down opaque: what the sheet
//records is whether a spot is covered at all, so a part of a body crossing
//another part of itself - or another body entirely - leaves a spot that is
//already inked exactly as it was. The whole shade is therefore one flat
//colour, and only the stroke that lays it brings the alpha.
function TModels3D.InkShadowSheet(const ACam: TCamera3D;
  const AProjection: TMatrix; AX, AY: Single; AWidth, AHeight: Integer;
  ATintAlpha: Byte): Boolean;
var
  Item: TModel3D;
  Ink: TColor;
begin
  Result := False;
  RayLib.BeginTextureMode(FShadowSheet);
  try
    RayLib.ClearBackground(clBlank);
    RayLib.BeginMode3D(ACam);
    try
      //The sheet's own depth buffer hides the far skins of a body, and that is
      //all it does: an already inked spot stays exactly as inked when another
      //part of the same body - or another body - lands on it, so nothing here
      //needs to be culled. Culling stays off on purpose: the mesh is looked at
      //mirrored (see below), which turns its windings around, and a skin is
      //only ever wanted here as ink.
      RayLib.rlEnableDepthTest;
      RayLib.rlEnableDepthMask;
      RayLib.rlDisableBackfaceCulling;
      Ink := cModel3DShadowInk;
      Ink.RGBA.Alpha := 255;
      //The stroke that lays the sheet reads it bottom up, so the ink goes on
      //the other way: the world is mirrored across the middle line of the
      //screen - and only that way, because the stroke turns just the one over.
      //World z is the screen's up, and the screen's middle sits at z zero (the
      //anchor of every model is the screen centre less its offset), so a
      //negative scale on z is the mirror.
      RayLib.rlPushMatrix;
      RayLib.rlScalef(1, 1, -1);
      try
        for Item in FItems.Values do
        begin
          if (not Item.Visible) or (not Item.Loaded) or (not Item.Shadow) then
            Continue;
          if Item.Tint.RGBA.Alpha <> ATintAlpha then
            Continue;
          PushShadowPose(Item, AProjection, AX, AY, AWidth, AHeight);
          DrawShadowMeshes(Item.Model, Ink);
          RayLib.rlPopMatrix;
          Result := True;
        end;
      finally
        RayLib.rlPopMatrix;
      end;
    finally
      RayLib.EndMode3D;
    end;
  finally
    RayLib.EndTextureMode;
  end;
end;

//One stroke of ink over the world. The sheet already carries the ink's own
//color, so the tint only brings the alpha: wherever a body covers, the world
//shows the ink at that strength, and everywhere else it shows through whole.
//This is a plain 2D coat of paint, so the depth test - left on by the sheet -
//goes back off; the model pass switches it on again for real solids.
procedure TModels3D.LayShadowSheet(AAlpha: Integer);
begin
  RayLib.rlDisableDepthTest;
  RayLib.DrawTexture(FShadowSheet.Texture, 0, 0, clWhite.ReplaceAlpha(Byte(AAlpha)));
end;

//The blur's fragment shader: a nine-tap Gaussian along one direction, the
//direction and step given in texels through the `blur` uniform - one shader
//serves both legs, which is the point of a separable blur. The weights are
//raylib's own blur sample (a normalised sigma-2 kernel), so the ink keeps its
//total strength and only its edges smear. The default vertex shader does the
//quad, so nothing here needs to agree with rlgl about attributes.
const
  cShadowBlurFs: UTF8String = '''
#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec2 blur;

out vec4 finalColor;

void main()
{
    vec4 sum = vec4(0.0);
    sum += 0.000012686 * texture(texture0, fragTexCoord - 4.0 * blur);
    sum += 0.0004992833 * texture(texture0, fragTexCoord - 3.0 * blur);
    sum += 0.01970073 * texture(texture0, fragTexCoord - 2.0 * blur);
    sum += 0.1559384 * texture(texture0, fragTexCoord - 1.0 * blur);
    sum += 0.6368684 * texture(texture0, fragTexCoord);
    sum += 0.1559384 * texture(texture0, fragTexCoord + 1.0 * blur);
    sum += 0.01970073 * texture(texture0, fragTexCoord + 2.0 * blur);
    sum += 0.0004992833 * texture(texture0, fragTexCoord + 3.0 * blur);
    sum += 0.000012686 * texture(texture0, fragTexCoord + 4.0 * blur);
    finalColor = sum * colDiffuse * fragColor;
}
''';

procedure TModels3D.FreeBlurPass;
begin
  if FBlurSheet.ID <> 0 then
  begin
    RayLib.UnloadRenderTexture(FBlurSheet);
    FBlurSheet := Default(TRenderTexture2D);
  end;
  if FBlurReady then
  begin
    RayLib.UnloadShader(FBlurShader);
    FBlurShader := Default(TShader);
    FBlurReady := False;
  end;
end;

//Like the shadow sheet, the parts are built lazily - they want the GL context
//the engine only owns once there is a window - and the sheet is a piece of the
//window's size, so a resize just costs a new one.
function TModels3D.BlurPassReady: Boolean;
begin
  Result := False;
  if not FBlurReady then
  begin
    FBlurShader := RayLib.LoadShaderFromMemory(nil, PUTF8Char(cShadowBlurFs));
    FBlurReady := RayLib.IsShaderValid(FBlurShader);
  end;
  if not FBlurReady then
    Exit;
  if (FBlurSheet.ID <> 0)
    and ((FBlurSheet.Texture.Width <> FShadowSheet.Texture.Width)
      or (FBlurSheet.Texture.Height <> FShadowSheet.Texture.Height)) then
  begin
    RayLib.UnloadRenderTexture(FBlurSheet);
    FBlurSheet := Default(TRenderTexture2D);
  end;
  if FBlurSheet.ID = 0 then
  begin
    FBlurSheet := RayLib.LoadRenderTexture(FShadowSheet.Texture.Width,
      FShadowSheet.Texture.Height);
    if not RayLib.IsRenderTextureValid(FBlurSheet) then
    begin
      //Dropped, not unloaded: a sheet that was never made has no buffer to
      //hand back.
      FBlurSheet := Default(TRenderTexture2D);
      Exit(False);
    end;
  end;
  //The ink sheet is normally read texel-for-texel (a hard silhouette); the
  //blur's taps sample between texels, so from the first soft frame on the
  //Gaussian reads it bilinear.
  RayLib.SetTextureFilter(FShadowSheet.Texture, TEXTURE_FILTER_BILINEAR);
  Result := True;
end;

//Two legs of the same kernel: horizontal into the blur sheet, vertical back.
//The legs write full screens at one fixed depth, so the depth test the inking
//left switched on must go - and LayShadowSheet's own DisableDepthTest keeps
//the world side of the deal exactly as it was.
procedure TModels3D.BlurShadowSheet;
var
  Blur: TVector2;
  BlurLoc: Integer;
  W, H: Integer;
begin
  W := FShadowSheet.Texture.Width;
  H := FShadowSheet.Texture.Height;
  BlurLoc := RayLib.GetShaderLocation(FBlurShader, 'blur');

  Blur := Vector2Of(FSoftness / W, 0);
  RayLib.BeginTextureMode(FBlurSheet);
  RayLib.rlDisableDepthTest;
  RayLib.ClearBackground(clBlank);
  RayLib.BeginShaderMode(FBlurShader);
  if BlurLoc >= 0 then
    RayLib.SetShaderValue(FBlurShader, BlurLoc, @Blur, Ord(SHADER_UNIFORM_VEC2));
  RayLib.DrawTexture(FShadowSheet.Texture, 0, 0, clWhite);
  RayLib.EndShaderMode;
  RayLib.EndTextureMode;

  Blur := Vector2Of(0, FSoftness / H);
  RayLib.BeginTextureMode(FShadowSheet);
  RayLib.rlDisableDepthTest;
  RayLib.ClearBackground(clBlank);
  RayLib.BeginShaderMode(FBlurShader);
  if BlurLoc >= 0 then
    RayLib.SetShaderValue(FBlurShader, BlurLoc, @Blur, Ord(SHADER_UNIFORM_VEC2));
  RayLib.DrawTexture(FBlurSheet.Texture, 0, 0, clWhite);
  RayLib.EndShaderMode;
  RayLib.EndTextureMode;
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
    FSoftness := 0;
    //...and the shadow ink goes with it: it belongs to this run's GL context
    //and is rebuilt on the next frame that casts a shadow.
    FreeShadowMaterial;
    FreeShadowSheet;
    FreeBlurPass;
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

procedure TModels3D.SetSoftness(AValue: single);
begin
  FLock.Enter;
  try
    //0 is the old hard ink; past six pixels a jet's shade is mostly haze, so
    //the dial stops where it stops being information.
    FSoftness := Min(6, Max(0, AValue));
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetSoftness: single;
begin
  FLock.Enter;
  try
    Result := FSoftness;
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
var
  Cam: TCamera3D;
  Item: TModel3D;
  S: Single;
  Pos: TVector3;
  ShadowM: TMatrix;
  ItemAlpha: Integer;
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

    //Top-down orthographic camera: world +x is screen right, world -z is screen
    //down (so a model's own +z points "up" like a sprite's), and world +y runs
    //away from the viewer as the depth axis. For an ortho camera raylib uses
    //Fovy as the visible height in world units, so Fovy = pixels maps one world
    //unit to one screen pixel; the ortho box is centred on the camera axis, so
    //pixel coordinates are shifted by half the screen below. The sheet is inked
    //through this same camera, which is why the stroke that lays it needs no
    //placing of its own: it lands where the world already is.
    Cam.Position := Vector3Of(0, -cModel3DDistance, 0);
    Cam.Target := Vector3Of(0, 0, 0);
    Cam.Up := Vector3Of(0, 0, 1);
    Cam.Fovy := AHeight;
    Cam.Projection := Ord(CAMERA_ORTHOGRAPHIC); //the field is a plain Integer

    //The shade the bodies drop on the world, laid before a single model is
    //drawn: the sea under it, every model over it, so a cloud that floats
    //between the viewer and the water hides the shade under it, as it should.
    //The silhouettes are inked onto the sheet first and the sheet goes down in
    //one stroke, so the shade is one flat colour wherever anything covers: a
    //canopy over a wing, or one body over another, cannot make it deeper, any
    //more than a spot can be out of one sun twice. Without the ink or without
    //the sheet there is no shade at all (a store that could not build them
    //leaves the sea bare).
    if (Alpha > 0) and (ShadowMaterial.Maps <> nil)
      and ShadowSheet(AWidth, AHeight) then
    begin
      //The solid bodies share one sheet: their shade is the same shade, so one
      //stroke of it covers all of them.
      if InkShadowSheet(Cam, ShadowM, AX, AY, AWidth, AHeight, 255) then
      begin
        if (FSoftness > 0) and BlurPassReady then
          BlurShadowSheet;
        LayShadowSheet(Alpha);
      end;
      //A see-through body (a misty cloud puff) is inked on the sheet on its own
      //and laid over the rest, so its thinness stays its own and the puffs
      //stack into a denser shade exactly like the puffs themselves do.
      for Item in FItems.Values do
      begin
        if (not Item.Visible) or (not Item.Loaded) or (not Item.Shadow) then
          Continue;
        if Item.Tint.RGBA.Alpha >= 255 then
          Continue;
        ItemAlpha := Round(Alpha * Item.Tint.RGBA.Alpha / 255);
        if (ItemAlpha > 0)
          and InkShadowSheet(Cam, ShadowM, AX, AY, AWidth, AHeight,
            Item.Tint.RGBA.Alpha) then
        begin
          if (FSoftness > 0) and BlurPassReady then
            BlurShadowSheet;
          LayShadowSheet(ItemAlpha);
        end;
      end;
    end;

    RayLib.BeginMode3D(Cam);
    try
      //The models are real solids: the depth test hides their far faces. It
      //is switched back off at the end of the pass - every layer above the
      //3D scene (sparks, controls, the screen buffer) is 2D at one fixed
      //depth, and the depth left in the buffer would otherwise punch the
      //models' silhouettes into those layers (a cloud hiding the HUD text).
      RayLib.rlEnableDepthTest;
      //The models themselves, painted over the shade.
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
