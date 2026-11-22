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
 *  draw call is needed from the script. A model's Layer picks the side of the
 *  2D world it rides: 0 (the default) draws after the sprites, over them as
 *  ever, and a negative layer takes a first 3D pass before the 2D world so
 *  the whole scene covers the body (and its shade). Sprites answer from their
 *  side: one whose layer reaches cLayerTop3D is drawn after the models, over
 *  them.
 *
 *  The 3D pass uses a top-down ORTHOGRAPHIC camera mapped pixel for pixel onto
 *  the window, so model x/y are screen pixels: x grows to the right, y grows
 *  down, and the model's own +z lands on screen "up". The remaining world axis
 *  (y, into the screen) is the model's depth: lower z (nearer the eye) draws a
 *  model in front of the ones behind it. Angle spins the model around the view
 *  axis (clockwise on screen, the sprite convention); Tilt tips it around the
 *  screen's horizontal axis, which foreshortens it for a cheap banking pose.
 *
 *  The store also carries one free scene camera (world.camera in Lua): with it
 *  on, the same world is looked at through a script-driven perspective eye and
 *  the model x/y/z are read as plain world coordinates instead of screen
 *  pixels. The top-down ortho pass stays the default, so every 2D game is
 *  untouched, and the ground-plane cast shadows stay with it - a free 3D scene
 *  has real depth instead of a projected shade.
 *
 *  The built-in shapes (models.sphere/box/cylinder/cone/torus in Lua) are
 *  meshes the engine generates itself, so a game needs no asset file at all
 *  for its vector blocks. They stand on the engine's Z axis: the generators
 *  raylib spins around Y carry an axis-fix matrix the pass composes onto the
 *  mesh, and a model:face() aim rotates any model in the world so a ship or a
 *  hoop can point along an arbitrary path (a torus-riding demo, say).
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
 *
 *  A skinned GLB also carries its skeletal animation clips: Load grabs them
 *  with LoadModelAnimations beside the mesh, the script cuts to one with
 *  model:play("Walk"), and the engine's per-frame Step advances every playing
 *  clip and writes its pose onto the model's bone matrices. DrawModel draws
 *  whatever pose the last Step left, so the 3D pass itself never learns that
 *  the mesh is alive - a static jet draws exactly the frame it always did.
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
  //The glow value that shines everywhere (model.glow = true in Lua): every
  //material keeps its flat skin, so the whole model reads as a lamp - what
  //a thruster flame wants. A plain text instead names one material to shine.
  cModel3DGlowAll = '*';
  //How bright a shining material can be asked for (model.bright), and the detent
  //its level snaps to. The detent keeps a script that RIMS the shine - a lamp
  //pulsing, an engine spooling up - from compiling a fresh dress every frame:
  //the level rides in the shader's source, so each distinct step costs one
  //small program, and a pulse across the useful range costs a handful.
  cModel3DBrightMax = 16;
  cModel3DBrightStep = 0.125;

type
  //The material names a loaded glTF file carries, one per material index in
  //the file's own order. raylib's Material record has no name field, so a
  //script that wants to single one material out (model.glow) has to hear the
  //names from the file itself - Load keeps this copy for that.
  TMatNames = array of string;

  //One shine dress: the level it burns a shining material at (model.bright)
  //and the shader that does it. The level rides in the source instead of a
  //uniform, because a material is picked by the SHADER it wears - and one
  //model's materials all share the dress, so a per-draw value would colour the
  //wrong faces. A script asks for one or two levels a run, so the store only
  //ever keeps a handful of these.
  TShineShader = record
    Bright: Single;
    Shader: TShader;
  end;

  { TModel3D }

  //raylib hands the animation clips of a file out as a plain C array. This is
  //the Pascal handle to walk it: the element count is only a range-check
  //cover, nothing is ever allocated through it.
  TModelAnimArray = array[0..MaxInt div SizeOf(TModelAnimation) - 1] of TModelAnimation;
  PModelAnimArray = ^TModelAnimArray;

  { One 3D model: the raylib model it draws, its bounding box (centre = pivot,
    sizes for the script to scale by) and the transform the engine applies in
    the per-frame 3D pass. X/Y are screen pixels, Z is depth into the screen. }
  TModel3D = class(TTyroAnimatedObject)
  public
    Model: TModel;          // raylib model; MeshCount = 0 while nothing is loaded
    Center: TVector3;       // bounding-box centre: the pivot the model turns around
    SizeX: Single;          // bounding-box size along the model's own axes
    SizeY: Single;
    SizeZ: Single;
    Z: Single;              // depth (bigger = farther from the eye, behind)
    Tilt: Single;           // rotation around the screen's horizontal axis, degrees
    Roll: Single;           // rotation around the model's own axis (the nose-tail
                            // line of a jet), degrees - the classic bank-into-turn
    Shadow: Boolean;        // cast a shadow onto the world's ground plane
    //Free 3D pose: the world point the model's own +y aims at and the
    //direction its +z leans toward (see FaceMatrixOf in the pass). Face is
    //optional - without it the plain Angle/Roll/Tilt Euler pose stands.
    FaceTo: TVector3;
    FaceUp: TVector3;
    HasFace: Boolean;
    //The axis fix a model carries: a rotation the pass composes onto the
    //mesh so a nose that lives on another own axis (see Nose) lands on the
    //face()'s +y. Engine-built shapes never need one; a loaded file does if
    //the script names its nose.
    AxisFix: TMatrix;
    HasAxisFix: Boolean;
    Nose: string;           // which drawn axis is the nose: "y" (default),
                            // "-y", "z", "-z", "x" or "-x"
    //Glow: the material that shines instead of being lit. "" leaves every
    //material to the lamps, cModel3DGlowAll shines the whole model (a
    //thruster flame says true and lands there), and any other text names one
    //material of the loaded file - compare MatNames, which Load read from
    //the file's JSON head because raylib drops the names. A glowing material
    //never wears the light coat (see WearLight), so it keeps raylib's own
    //flat shader and shows at full bright whatever the lights do.
    Glow: string;
    //How hard the shining material burns (model.bright in Lua): 1 is the colour
    //the file was painted with, and more pushes that colour up toward white -
    //the difference between a coloured panel and a lamp you can look at. It
    //only speaks for a material that shines (see Glow), and like the shine it
    //only shows while the lights are on.
    Bright: Single;
    MatNames: TMatNames;
    //Skeletal animation: raylib's clips of the loaded file (nil/0 when the
    //mesh has no skeleton or no animation), plus the playback state. Current
    //is -1 when nothing has ever played and the mesh sits in its bind pose;
    //Frame is the position inside that clip, in keyframes, and Step advances
    //it by Dt * Speed each frame the model is playing.
    Animations: PModelAnimation;
    AnimCount: Integer;
    Current: Integer;
    Frame: Single;
    Speed: Single;          // keyframes per second (glTF clips bake at 30)
    Playing: Boolean;
    //The light coat: when world.lights is on the pass swaps the materials'
    //shader to the light one, and the plain ones wait here to be worn again
    //the moment it goes off (see TModels3D.WearLight/WearOwn).
    WearingLight: Boolean;
    OwnShaders: array of TShader;
    constructor Create(AHandle: Integer; const AName: string);
    destructor Destroy; override;
    function Loaded: Boolean;
    //Releases the clip array Load took from the file and resets the playback
    //state. Only ever called on the main thread, like the model itself.
    procedure FreeAnimations;
    //The current clip, or nil when there is none.
    function CurrentAnim: PModelAnimation;
    //True when material m shines on its own: the whole-model glow, or the
    //one whose name the script wrote into Glow (compare MatNames).
    function GlowAt(m: Integer): Boolean;
  end;

  { TTyroModelShape }

  { The built-in shapes BuildShape generates, with the A/B/C reading:
      msSphere     A = radius
      msBox        A, B, C = size along x, y, z
      msCylinder   A = radius, B = height (axis along z)
      msCone       A = radius, B = height (axis along z)
      msTorus      A = ring radius, B = tube radius (the ring lies in x/y
                   around the z axis)
    Every shape is centred on the origin and stands on the engine's Z axis. }
  TTyroModelShape = (msSphere, msBox, msCylinder, msCone, msTorus);

  { TModels3D }

  { The store. Mirrors TTiles: a lock, a dictionary keyed by handle and an
    ever-increasing handle counter, so a handle a script still holds can never
    address a model of a later run. The engine drains the visible models once
    per frame inside Draw, reading the live transform under the same lock. }
  TModels3D = class(TObject)
  private
    FLock: TmnCriticalSection;
    FItems: TDictionary<Integer, TModel3D>;
    FNextHandle: Integer;
    //Cast-shadow light (see Draw). LightDir is the direction the light
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
    //The free scene camera (the Lua world.camera/world.ortho). Off by default:
    //the models then ride the top-down pixel ortho pass as ever. On, the same
    //world is looked at through this perspective eye and the model x/y/z are
    //read as plain world coordinates. The parameters stay stored either way,
    //so flipping back to the free camera restores the shot where it was left.
    FFreeCam: Boolean;
    FCamPos: TVector3;
    FCamTarget: TVector3;
    FCamUp: TVector3;
    FFov: Single;
    //Wire mode: the models are painted as their edges only - DrawModelWires
    //instead of DrawModel - for the glowing-vector look some games want.
    FWire: Boolean;
    // -- scene lights (mirrored on the Lua 'world' table) --
    //The flat dress is the classic one: every model inked in its solid tint.
    //With the lights on the solids put on a lambert skin: the sky's ambient
    //fill (world.ambient), a sun shining along the world.light ray, and up to
    //four bulbs hung anywhere in the world (SetPointLight). The material's
    //shader is built lazily on the first lit frame - it wants the GL context
    //the way the shadow ink does - and a world that cannot compile it simply
    //keeps its flat dress.
    FLightOn: Boolean;
    FLightTried: Boolean;
    FLightReady: Boolean;
    FLightShader: TShader;
    FLightLocAmbient: Integer;
    FLightLocSun: Integer;
    FLightLocPL: array[0..3] of Integer;
    FLightLocPLC: array[0..3] of Integer;
    //The shine dresses built so far, one per level asked for (see ShineShader).
    FShine: array of TShineShader;
    //The bulbs, index 0..3: a world-space place, a range where the light has
    //faded out (Range <= 0 means the bulb is dark), and its color as 0..1
    //parts with the intensity already folded into them.
    FPointLights: array[0..3] of record
      X, Y, Z, Range: Single;
      R, G, B: Single;
    end;
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
    //Shared walk of HasUnder/HasTop: true when a visible, loaded model sits on
    //the asked side of the 2D world (Layer < 0 or not).
    function HasSide(AUnder: boolean): boolean;
    //Frees the GPU model a slot owns and resets it to unloaded.
    procedure FreeModelData(AItem: TModel3D);
    //Cut one model to a clip (AIndex < 0 = stop) and lift its play flag; the
    //work behind PlayAnim/PlayAnimByName, called with the lock already held.
    function PlayItemAnim(AItem: TModel3D; AIndex: integer): boolean;
    //The name raylib left in a clip's fixed 32 bytes, called with the lock
    //already held.
    function GetItemAnimName(AItem: TModel3D; AIndex: integer): string;
    //Releases the shared shadow ink (main thread, GL context alive).
    procedure FreeShadowMaterial;
    //The store's one shadow ink, built on demand (see FShadowMat). Its Maps is
    //nil when it could not be built, and the caller then casts no shade.
    function ShadowMaterial: TMaterial;
    //The light dress (see cLightVs/cLightFs): built once, on the main thread,
    //and only tried once a run - a shader the GPU will not compile leaves the
    //world in its flat dress rather than spoiling the picture.
    procedure EnsureLightShader;
    procedure FreeLightShader;
    //The shine dress for one level (see TModel3D.Bright): compiled the first
    //frame that asks for that level - the GL context is alive by then, and a
    //compile does not belong in the draw - and kept for the rest of the run.
    //A GPU that will not compile it leaves the material in its own skin, which
    //is the ordinary full-bright shine with no boost: less hot, never wrong.
    function ShineShader(ABright: Single): TShader;
    procedure FreeShineShaders;
    //The dress swap for one body: on, the materials copy their plain shaders
    //aside and wear the light one; off, the originals come back. Both are a
    //no-op when the model already dresses as asked.
    procedure WearLight(Item: TModel3D);
    procedure WearOwn(Item: TModel3D);
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
    //Wipes the sheet and inks it with the silhouette of every body on this
    //pass (AUnder: layer < 0 or not) whose own alpha is ATintAlpha
    //(255 = the ordinary, solid bodies, inked all together; a see-through body
    //is inked on its own so its thinness stays its own).
    //True when it inked anything, which is when the sheet is worth laying.
    function InkShadowSheet(const ACam: TCamera3D; const AProjection: TMatrix;
      AX, AY: Single; AWidth, AHeight: Integer; ATintAlpha: Byte;
      AUnder: Boolean): Boolean;
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
    //* Unload one model now (model:free()). The mesh buffers and textures go
    //* with it, so this only runs on the main thread (see TFreeModelObject).
    //* Handles are not reused: a stale model table simply stops resolving.
    function Remove(Handle: integer): boolean;
    // Add a mesh-less model, returns a handle
    function AddEmpty(const AName: string = ''): integer;
    // Generate one of the built-in shapes into an existing handle (main
    // thread, like Load). The script scales/positions it like any other model;
    // these are the primitives a game builds its vector world from, so it needs
    // no mesh file to put a real 3D shape on screen. See TTyroModelShape for
    // what A, B and C read per shape.
    function BuildShape(Handle: integer; AShape: TTyroModelShape;
      A, B, C: Single): boolean;
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

    // -- free scene camera (mirrored on the Lua 'world' table) --
    //The eye, the point it looks at and its up vector, in the model pass's
    //world space (x right, y into the screen, z up). Setting it also switches
    //the pass to the free camera; SetFreeCam(False) hands the world back to
    //the top-down ortho eye the 2D games ride.
    procedure SetCamera(PX, PY, PZ, TX, TY, TZ, UX, UY, UZ: single);
    procedure GetCamera(out PX, PY, PZ, TX, TY, TZ, UX, UY, UZ: single);
    procedure SetFreeCam(AEnabled: boolean);
    function GetFreeCam: boolean;
    //Vertical field of view in degrees (clamped 10..170), read only while the
    //free camera is on; 60 is the plain-eye default.
    procedure SetFov(AValue: single);
    function GetFov: single;
    //Paint every model as wire instead of solid (mirrored on world.wire).
    procedure SetWire(AEnabled: boolean);
    function GetWire: boolean;
    //Dress the solids in light: the sun's ray, the ambient fill and the
    //bulbs are all read from this store's own fields (mirrored on
    //world.lights / world.light / world.ambient / world.pointlight).
    procedure SetLighting(AEnabled: boolean);
    function GetLighting: boolean;
    procedure SetPointLight(ANum: integer; PX, PY, PZ, ARange, R, G, B: single);

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
    //Which side of the 2D world the model rides (see Draw's AUnder and
    //TTyroAnimatedObject.Layer): 0 (the default) flies over the 2D scene as
    //ever, a negative layer sinks the whole body - and its shade - under it.
    procedure SetLayer(Handle: integer; ALayer: integer);
    function GetLayer(Handle: integer): integer;
    procedure SetScale(Handle: integer; AScale: single);
    function GetScale(Handle: integer): single;
    procedure SetTint(Handle: integer; ATint: TColor);
    function GetTint(Handle: integer): TColor;
    // 0..255 opacity, folded into the tint's alpha channel: a see-through
    // jet shows what is behind it, and a stack of soft spheres reads as one
    // misty volume instead of hard white balls.
    procedure SetAlpha(Handle: integer; AAlpha: integer);
    function GetAlpha(Handle: integer): integer;

    // -- model facing (mirrored on the Lua model:face) --
    //Aim the model's own +y at the world point (TX,TY,TZ) and lean its +z
    //toward the direction (UX,UY,UZ). The pose is composed before the plain
    //angles, which then trim on top of it. ClearFace hands the model back to
    //the Angle/Roll/Tilt Euler pose.
    procedure SetFace(Handle: integer; TX, TY, TZ, UX, UY, UZ: single);
    procedure ClearFace(Handle: integer);
    // -- model nose axis (mirrored on the Lua model.nose) --
    //The model's own axis that face() and roll treat as the nose-tail line.
    //Engine-built shapes fly with their +y ("y", the default). A loaded file
    //whose nose sits on another axis says so here - a glb whose nose ends up
    //on -y after the loader's upright conversion wears "-y" - and the store
    //fills the reserved AxisFix matrix so face()'s +y finds the nose. The
    //mesh itself is never touched; the fix rides along in the draw stack.
    procedure SetNose(Handle: integer; const ANose: string);
    function GetNose(Handle: integer): string;

    //The glowing material of a model (see TModel3D.Glow): "" for none,
    //cModel3DGlowAll for the whole model, or one material name of the loaded
    //file. Writing it takes the light coat off, so the next frame dresses
    //again with the new answer.
    procedure SetGlow(Handle: integer; const AGlow: string);
    function GetGlow(Handle: integer): string;

    //How hard a model's shining material burns (model.bright in Lua): 1 is the
    //colour as painted, more pushes it toward white. Clamped to cModel3DBrightMax
    //and snapped to cModel3DBrightStep; like SetGlow, it takes the light coat off
    //so the next frame dresses with the level asked for.
    procedure SetBright(Handle: integer; ABright: Single);
    function GetBright(Handle: integer): Single;

    // -- skeletal animation (mirrored on the Lua model:play/model:stop) --
    //Advance every playing clip by the frame time and pose its model. Runs on
    //the main thread from the engine's Update, next to the sprite animations,
    //so a paused scene freezes a walker mid-stride with everything else.
    procedure Step(ADt: single);
    function GetAnimCount(Handle: integer): integer;
    function GetAnimName(Handle: integer; AIndex: integer): string;
    function GetAnimFrames(Handle: integer; AIndex: integer): integer;
    //Cut to AIndex (0 = the file's first clip), looping; AIndex < 0 stops and
    //keeps the current pose. True when a clip was found.
    function PlayAnim(Handle: integer; AIndex: integer): boolean;
    //Same by clip name ("Walk"); False when no clip carries that name.
    function PlayAnimByName(Handle: integer; const AName: string): boolean;
    procedure StopAnim(Handle: integer);
    function IsAnimPlaying(Handle: integer): boolean;
    function GetAnimCurrent(Handle: integer): integer;
    //Position inside the current clip, in seconds (the 30 fps bake converts).
    procedure SetAnimTime(Handle: integer; ASeconds: single);
    function GetAnimTime(Handle: integer): single;
    //Playback rate as a keyframe count per second; the 30 of a fresh clip is
    //its authored speed, 60 doubles it.
    procedure SetAnimSpeed(Handle: integer; AValue: single);
    function GetAnimSpeed(Handle: integer): single;

    // True when at least one visible, loaded model rides this side of the 2D
    // world (see Layer/Draw): HasTop the layer >= 0 bodies over it,
    // HasUnder the layer < 0 bodies below it. Lets the caller skip the whole
    // BeginMode3D/EndMode3D pass on an empty side.
    function HasUnder: boolean;
    function HasTop: boolean;

    // Draw every visible model under the top-down pixel camera. AX/AY shift
    // the whole pass so the models ride with the 2D camera (window margin,
    // shake); AWidth/AHeight are the screen size in pixels. AUnder picks the
    // side of the 2D world this pass draws (see Layer): True only the
    // layer < 0 bodies, drawn before the 2D world so it covers them; False
    // (the default) the rest, riding over it as ever.
    procedure Draw(AX, AY: single; AWidth, AHeight: Integer; AUnder: boolean = False);

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

  { TShapeModelObject }

  { Builds one of the engine-generated shapes (see TTyroModelShape) into a
    model on the main thread (the upload needs the GL context) and returns
    whether it succeeded. }
  TShapeModelObject = class(TQueueObject)
  private
    FShape: TTyroModelShape;
    FA: Single;
    FB: Single;
    FC: Single;
    FHandle: integer;
    FResult: boolean;
  public
    constructor Create(AHandle: integer; AShape: TTyroModelShape;
      A, B, C: Single);
    procedure DoExecute; override;
    property HandleResult: boolean read FResult;
  end;

  { TFreeModelObject }

  { Frees one model (model:free) on the main thread, where its mesh buffers
    and textures can be unloaded. The script thread waits (Run synchronizes),
    so after the call the model is really gone from the draw pass. }
  TFreeModelObject = class(TQueueObject)
  private
    FHandle: integer;
    FRemoved: boolean;
  public
    constructor Create(AHandle: integer);
    procedure DoExecute; override;
    property Removed: boolean read FRemoved;
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
  //Where the free eye parks until a script takes it over: the same spot the
  //ortho pass hovers (its cModel3DDistance), so a first world.camera() starts
  //from the shot the script already sees.
  cModel3DCamRest = 400.0;
  //Cast-shadow look: the ink the silhouette is laid in, and how dark it can
  //get. Ambient light fills a shadow, so at full ambient the alpha drops to 0
  //and the whole shade switches itself off. Here rather than in Draw
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
  //The axis fix the generated cylinder, cone and torus carry: raylib spins
  //all three around its Y axis, the engine's world stands them on Z. For the
  //column-vector reading rlMultMatrixf gets (see the note in Draw) that is
  //the map (x, y, z) -> (x, -z, y): Y lands on +Z, and Z swings to -Y.
  cModel3DAxisZUp: TMatrix =
    (m0: 1;  m4: 0;  m8: 0;  m12: 0;
     m1: 0;  m5: 0;  m9: 1;  m13: 0;
     m2: 0;  m6: -1; m10: 0; m14: 0;
     m3: 0;  m7: 0;  m11: 0; m15: 1);
  //The nose fixes (model.nose, see SetNose): each brings a named own axis
  //onto the +y that face() aims. Column i is where the model's i-th axis
  //lands, matching the column-vector reading rlMultMatrixf gets.
  //Nose "z": (x, y, z) -> (x, z, -y) - the nose lies down to +y, the flat
  //back swings to -z, wings stay on x. A proper rotation, winding kept.
  cModel3DAxisNoseZ: TMatrix =
    (m0: 1;  m4: 0;  m8: 0;  m12: 0;
     m1: 0;  m5: 0;  m9: -1; m13: 0;
     m2: 0;  m6: 1;  m10: 0; m14: 0;
     m3: 0;  m7: 0;  m11: 0; m15: 1);
  //Nose "x": (x, y, z) -> (-y, x, z) - +x rises to +y, +z stays up.
  cModel3DAxisNoseX: TMatrix =
    (m0: 0;  m4: 1;  m8: 0;  m12: 0;
     m1: -1; m5: 0;  m9: 0;  m13: 0;
     m2: 0;  m6: 0;  m10: 1; m14: 0;
     m3: 0;  m7: 0;  m11: 0; m15: 1);
  //Nose "-x": (x, y, z) -> (y, -x, z) - -x rises to +y, +z stays up.
  cModel3DAxisNoseNegX: TMatrix =
    (m0: 0;  m4: -1; m8: 0;  m12: 0;
     m1: 1;  m5: 0;  m9: 0;  m13: 0;
     m2: 0;  m6: 0;  m10: 1; m14: 0;
     m3: 0;  m7: 0;  m11: 0; m15: 1);
  //Nose "-y": (x, y, z) -> (-x, -y, z) - a half turn about +z: a file whose
  //nose sits on the BACK of the convention axis comes around nose-first,
  //wings trading ends, sky side staying sky side.
  cModel3DAxisNoseNegY: TMatrix =
    (m0: -1; m4: 0;  m8: 0;  m12: 0;
     m1: 0;  m5: -1; m9: 0;  m13: 0;
     m2: 0;  m6: 0;  m10: 1; m14: 0;
     m3: 0;  m7: 0;  m11: 0; m15: 1);

{ TModel3D }

//The few vector maths the facing basis needs. The raylib math functions are
//not bound, and a basis turn needs nothing beyond these three.
function V3Length(const A: TVector3): Single;
begin
  Result := Sqrt(A.X * A.X + A.Y * A.Y + A.Z * A.Z);
end;

function V3Dot(const A, B: TVector3): Single;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

function V3Cross(const A, B: TVector3): TVector3;
begin
  Result := Vector3Of(
    A.Y * B.Z - A.Z * B.Y,
    A.Z * B.X - A.X * B.Z,
    A.X * B.Y - A.Y * B.X);
end;

//Zero-vector-safe normalise: (0,0,0) in, (0,0,0) out (the caller decides the
//fallback), any other vector comes out one unit long.
function V3Unit(const A: TVector3): TVector3;
var
  L: Single;
begin
  L := V3Length(A);
  if L < 1e-6 then
    Result := Vector3Of(0, 0, 0)
  else
    Result := Vector3Of(A.X / L, A.Y / L, A.Z / L);
end;

//The pose face() asks for: the model's own +y aimed from APos at ATarget, its
//+z leaning toward AUp (flattened to lie square across the sight line). The
//result is a plain rotation - det +1, no mirror - and a degenerate aim
// (target on the model, or up straight along the sight line) still lands on
//an upright frame rather than a NaN pose. For the column-vector reading of
//rlMultMatrixf the axes live in the COLUMNS: +y (the aim) goes in m1,m5,m9.
function FaceMatrixOf(const APos, ATarget, AUp: TVector3): TMatrix;
var
  D, U, X: TVector3;
begin
  D := V3Unit(Vector3Of(ATarget.X - APos.X, ATarget.Y - APos.Y,
    ATarget.Z - APos.Z));
  if (D.X = 0) and (D.Y = 0) and (D.Z = 0) then
    D := Vector3Of(0, 1, 0); //aim at itself: face "up the screen" as the model does bare
  U := V3Unit(AUp);
  if (U.X = 0) and (U.Y = 0) and (U.Z = 0) then
    U := Vector3Of(0, 0, 1);
  //Lean the up vector square across the sight line; when it sat right along
  //it, pick any standing axis instead of a zero one.
  U := Vector3Of(U.X - D.X * V3Dot(D, U), U.Y - D.Y * V3Dot(D, U),
    U.Z - D.Z * V3Dot(D, U));
  U := V3Unit(U);
  if (U.X = 0) and (U.Y = 0) and (U.Z = 0) then
  begin
    if Abs(D.Z) < 0.9 then
      U := Vector3Of(0, 0, 1)
    else
      U := Vector3Of(0, 1, 0);
    U := V3Unit(Vector3Of(U.X - D.X * V3Dot(D, U), U.Y - D.Y * V3Dot(D, U),
      U.Z - D.Z * V3Dot(D, U)));
  end;
  //Right-handed frame: with +y = D and +z = U, +x completes it as y cross z.
  X := V3Cross(D, U);

  Result.m0 := X.X;  Result.m4 := X.Y;  Result.m8 := X.Z;  Result.m12 := 0;
  Result.m1 := D.X;  Result.m5 := D.Y;  Result.m9 := D.Z;  Result.m13 := 0;
  Result.m2 := U.X;  Result.m6 := U.Y;  Result.m10 := U.Z; Result.m14 := 0;
  Result.m3 := 0;    Result.m7 := 0;    Result.m11 := 0;   Result.m15 := 1;
end;

constructor TModel3D.Create(AHandle: Integer; const AName: string);
begin
  inherited Create(AHandle, AName);
  Model := Default(TModel);
  Center := Vector3Of(0, 0, 0);
  SizeX := 0;
  SizeY := 0;
  SizeZ := 0;
  Z := 0;
  Tilt := 0;
  Roll := 0;
  Shadow := True;
  FaceTo := Vector3Of(0, 0, 0);
  FaceUp := Vector3Of(0, 0, 1);
  HasFace := False;
  AxisFix := cModel3DIdentity;
  HasAxisFix := False;
  Nose := 'y';
  Bright := 1;
  Animations := nil;
  AnimCount := 0;
  Current := -1;
  Frame := 0;
  Speed := 30;
end;

destructor TModel3D.Destroy;
begin
  //The store only frees models on the main thread (Clear/Load), so the GL
  //context is alive here, same rule as the sprite/texture owners.
  FreeAnimations;
  if Loaded then
    RayLib.UnloadModel(Model);
  Model := Default(TModel);
  inherited;
end;

function TModel3D.Loaded: Boolean;
begin
  Result := Model.MeshCount > 0;
end;

procedure TModel3D.FreeAnimations;
begin
  if Animations <> nil then
  begin
    RayLib.UnloadModelAnimations(PModelAnimArray(Animations)^[0], AnimCount);
    Animations := nil;
  end;
  AnimCount := 0;
  Current := -1;
  Frame := 0;
  Playing := False;
end;

function TModel3D.CurrentAnim: PModelAnimation;
begin
  if (Animations <> nil) and (Current >= 0) and (Current < AnimCount) then
    Result := @PModelAnimArray(Animations)^[Current]
  else
    Result := nil;
end;

function TModel3D.GlowAt(m: Integer): boolean;
begin
  if Glow = '' then
    Result := False
  else if Glow = cModel3DGlowAll then
    Result := True //the whole model shines - a thruster flame
  else if (m >= 0) and (m <= High(MatNames)) then
    Result := SameText(MatNames[m], Glow) //the one material the file names
  else
    Result := False;
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
  //The free eye starts parked at the ortho pass's own viewpoint, so the first
  //world.camera() call starts from the shot the script can already see, and
  //the pass itself stays on the top-down ortho until one asks for the free
  //camera.
  FFreeCam := False;
  FCamPos := Vector3Of(0, -cModel3DCamRest, 0);
  FCamTarget := Vector3Of(0, 0, 0);
  FCamUp := Vector3Of(0, 0, 1);
  FFov := 60;
  FWire := False;
  FLightOn := False;
  FLightTried := False;
  FLightReady := False;
  FLightShader := Default(TShader);
  FillChar(FPointLights, SizeOf(FPointLights), 0);
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
  AItem.FreeAnimations;
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
  Pos: TVector3;
  FaceM: TMatrix;
begin
  S := AItem.Scale;
  if S = 0 then
    S := 1;
  RayLib.rlPushMatrix;
  RayLib.rlMultMatrixf(@AProjection);
  //The same screen-pixel anchor the models get, so the shade belongs to the
  //body that casts it: x runs right, y runs down (so it becomes -z), and the
  //body's own Z depth rides along the world y axis.
  Pos := Vector3Of(AItem.X + AX - AWidth * 0.5, AItem.Z,
    AHeight * 0.5 - (AItem.Y + AY));
  RayLib.rlTranslatef(Pos.X, Pos.Y, Pos.Z);
  //The body's full pose from the pass, cast-shadow parts included: the shade
  //is the drawn silhouette flattened, so facing and axis fix ride along.
  if AItem.HasFace then
  begin
    FaceM := FaceMatrixOf(Pos, AItem.FaceTo, AItem.FaceUp);
    RayLib.rlMultMatrixf(@FaceM);
  end;
  RayLib.rlRotatef(AItem.Angle, 0, 1, 0);
  RayLib.rlRotatef(AItem.Roll, 0, 0, 1);
  RayLib.rlRotatef(AItem.Tilt, 1, 0, 0);
  if AItem.HasAxisFix then
    RayLib.rlMultMatrixf(@AItem.AxisFix);
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
  ATintAlpha: Byte; AUnder: Boolean): Boolean;
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
          //Only this pass's side of the 2D world casts a shade in it: an under
          //body is inked in the under pass, an over body in the over pass.
          if ((Item.Layer < 0) <> AUnder) then
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

// The light dress for the solids (world.lights): a pair of shaders where the
// vertex stage hands the fragment stage the world position and the world
// normal, and the fragment stage pays the lambert bill - the ambient fill of
// the sky, the sun along the world.light ray, and up to four point bulbs
// fading out over their range. The names are the contract: rlgl fills mvp,
// matModel and matNormal into any shader that asks by these names (the
// normal matrix arrives as transpose(inverse(model)), which keeps the
// inside-out liner - a mirrored mesh - lit from the right side), and
// DrawModel writes the model's tint into colDiffuse.
const
  cLightVs: UTF8String = '''
#version 330

in vec3 vertexPosition;
in vec3 vertexNormal;
in vec2 vertexTexcoords;
in vec4 vertexColor;

uniform mat4 mvp;
uniform mat4 matModel;
uniform mat4 matNormal;

out vec2 fragTexCoord;
out vec4 fragColor;
out vec3 fragPos;
out vec3 fragNormal;

void main()
{
    fragTexCoord = vertexTexcoords;
    fragColor = vertexColor;
    fragPos = (matModel * vec4(vertexPosition, 1.0)).xyz;
    fragNormal = normalize(mat3(matNormal) * vertexNormal);
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
''';

  cLightFs: UTF8String = '''
#version 330

in vec2 fragTexCoord;
in vec4 fragColor;
in vec3 fragPos;
in vec3 fragNormal;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

uniform float amb;          // the sky's fill light, 0..1
uniform vec4 sun;           // the ray the sunlight travels along; w = 0 means overcast
uniform vec4 pl[4];         // the bulbs: position, with the fade-out range in w
uniform vec4 plc[4];        // their colors, intensity already folded in

out vec4 finalColor;

void main()
{
    vec3 albedo = texture(texture0, fragTexCoord).rgb * colDiffuse.rgb * fragColor.rgb;
    vec3 n = normalize(fragNormal);
    vec3 lit = albedo * amb;

    if (sun.w > 0.0)
    {
        // the light travels along sun.xyz, so the way *to* it is the opposite
        lit += albedo * max(dot(n, -sun.xyz), 0.0);
    }

    for (int i = 0; i < 4; i++)
    {
        if (pl[i].w > 0.0)
        {
            vec3 toLight = pl[i].xyz - fragPos;
            float d = length(toLight);
            // the bulb fades the way a shadow edge does: square the falloff
            float att = clamp(1.0 - d / pl[i].w, 0.0, 1.0);
            att *= att;
            lit += albedo * plc[i].rgb * max(dot(n, toLight / (d + 0.0001)), 0.0) * att;
        }
    }

    finalColor = vec4(min(lit, vec3(1.0)), colDiffuse.a);
}
''';

// The light dress: the shader pair is tried once a run, here on the main
// thread where the GL context lives (Draw is about to ask for it). A GPU
// that will not compile it leaves the world in its flat dress - the models
// simply never hear about the swap - rather than spoiling the frame every
// frame. The uniform names are the shader's own ("amb", "sun", and the two
// bulb arrays); raylib's own names (mvp, matModel, matNormal, colDiffuse)
// need no fetching - rlgl and DrawModel fill those in for whoever asks.
procedure TModels3D.EnsureLightShader;
var
  i: Integer;
  Uf: UTF8String;
begin
  if FLightTried then
    Exit;
  FLightTried := True;
  FLightShader := RayLib.LoadShaderFromMemory(PUTF8Char(cLightVs), PUTF8Char(cLightFs));
  FLightReady := RayLib.IsShaderValid(FLightShader);
  if not FLightReady then
    Exit;
  FLightLocAmbient := RayLib.GetShaderLocation(FLightShader, 'amb');
  FLightLocSun := RayLib.GetShaderLocation(FLightShader, 'sun');
  for i := 0 to 3 do
  begin
    Uf := 'pl[' + IntToStr(i) + ']';
    FLightLocPL[i] := RayLib.GetShaderLocation(FLightShader, PUTF8Char(Uf));
    Uf := 'plc[' + IntToStr(i) + ']';
    FLightLocPLC[i] := RayLib.GetShaderLocation(FLightShader, PUTF8Char(Uf));
  end;
end;

procedure TModels3D.FreeLightShader;
begin
  if FLightShader.ID <> 0 then
    RayLib.UnloadShader(FLightShader);
  FLightShader := Default(TShader);
  FLightReady := False;
  FLightTried := False;
end;

// The shine dress: what a material that answers to no lamp wears when the script
// asks for more than its painted colour. This is raylib's own flat look - the
// file's colour times the model's tint, with the tint's alpha riding along -
// except the colour is pushed up by the level asked for, so a hot intake or a
// lamp burns PAST the colour the file was painted with instead of stopping at
// it. The level is written into the source where HOT sits: a material is chosen
// by the shader it wears, not by a value set per draw, and one model's materials
// all share the dress.
function ShineSource(ABright: Single): UTF8String;
const
  cShineFs = '''
#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

out vec4 finalColor;

void main()
{
    vec4 albedo = texture(texture0, fragTexCoord) * colDiffuse * fragColor;
    finalColor = vec4(min(albedo.rgb * %HOT%, vec3(1.0)), albedo.a);
}
''';
var
  S: string;
  Whole, Part: Integer;
begin
  // The level in GLSL's own spelling: always a dot, whatever the run's locale
  // decides a number looks like.
  Whole := Trunc(ABright);
  Part := Round((ABright - Whole) * 1000);
  if Part >= 1000 then      // the rounding carried: the next whole level up
  begin
    Inc(Whole);
    Part := 0;
  end;
  S := IntToStr(Part);
  while Length(S) < 3 do
    S := '0' + S;
  Result := StringReplace(cShineFs, '%HOT%', IntToStr(Whole) + '.' + S, [rfReplaceAll]);
end;

// The dress for one level, built the first time the run asks for it and kept
// afterwards - including the time it failed, so a GPU that will not compile it
// is asked once and the material simply keeps its own flat skin.
function TModels3D.ShineShader(ABright: Single): TShader;
var
  i, N: Integer;
begin
  for i := 0 to High(FShine) do
    if Abs(FShine[i].Bright - ABright) < 0.001 then
      Exit(FShine[i].Shader);
  Result := RayLib.LoadShaderFromMemory(PUTF8Char(cLightVs),
    PUTF8Char(ShineSource(ABright)));
  if not RayLib.IsShaderValid(Result) then
  begin
    if Result.ID <> 0 then
      RayLib.UnloadShader(Result);
    Result := Default(TShader);
  end;
  N := Length(FShine);
  SetLength(FShine, N + 1);
  FShine[N].Bright := ABright;
  FShine[N].Shader := Result;
end;

// The dresses belong to the GL context, so a rerun hands them all back and the
// next run compiles whatever it asks for again.
procedure TModels3D.FreeShineShaders;
var
  i: Integer;
begin
  for i := 0 to High(FShine) do
    if FShine[i].Shader.ID <> 0 then
      RayLib.UnloadShader(FShine[i].Shader);
  SetLength(FShine, 0);
end;

// On: the model's materials copy their own shaders aside once and put on the
// light one. The records are plain data (program id + location table), and
// every later DrawModel of this model dresses the same - so the swap sticks
// until WearOwn hands the originals back.
procedure TModels3D.WearLight(Item: TModel3D);
var
  m: Integer;
  Hot: TShader;
begin
  if Item.WearingLight then
    Exit;
  SetLength(Item.OwnShaders, Item.Model.MaterialCount);
  for m := 0 to Item.Model.MaterialCount - 1 do
  begin
    Item.OwnShaders[m] := Item.Model.Materials[m].Shader;
    //A glowing material stays in its own flat skin: no coat, no lambert -
    //the face keeps the full-bright colour the file painted it, whatever
    //the lamps do to everything else. Asked to burn harder (model.bright) it
    //wears the shine dress instead, which is that same flat look with the
    //colour pushed up; a dress this GPU refused leaves the skin on.
    if not Item.GlowAt(m) then
      Item.Model.Materials[m].Shader := FLightShader
    else if Item.Bright <> 1 then
    begin
      Hot := ShineShader(Item.Bright);
      if Hot.ID <> 0 then
        Item.Model.Materials[m].Shader := Hot;
    end;
  end;
  Item.WearingLight := True;
end;

procedure TModels3D.WearOwn(Item: TModel3D);
var
  m: Integer;
begin
  if not Item.WearingLight then
    Exit;
  for m := 0 to High(Item.OwnShaders) do
    if m < Item.Model.MaterialCount then
      Item.Model.Materials[m].Shader := Item.OwnShaders[m];
  SetLength(Item.OwnShaders, 0);
  Item.WearingLight := False;
end;

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
    //...and under its own eye: the free camera goes back to its rest behind
    //the ortho viewpoint, switched off, until the new script calls
    //world.camera again.
    FFreeCam := False;
    FCamPos := Vector3Of(0, -cModel3DCamRest, 0);
    FCamTarget := Vector3Of(0, 0, 0);
    FCamUp := Vector3Of(0, 0, 1);
    FFov := 60;
    FWire := False;
    //...the lights go off with the rerun too: every bulb is this run's, and
    //the light shader belongs to the GL context that just closed (the models
    //are new bodies anyway, and they never wore it yet).
    FLightOn := False;
    FLightTried := False;
    FillChar(FPointLights, SizeOf(FPointLights), 0);
    FreeLightShader;
    //...and so do the shine dresses: they belong to the closed context too,
    //and the next run compiles whatever level it asks for.
    FreeShineShaders;
    //...and the world is painted solid again: the rerun earns its own
    //world.wire call, just like it earns its own sky and eye.
    //...and the shadow ink goes with it: it belongs to this run's GL context
    //and is rebuilt on the next frame that casts a shadow.
    FreeShadowMaterial;
    FreeShadowSheet;
    FreeBlurPass;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.Remove(Handle: integer): boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      FItems.Remove(Handle);
      Item.Free; //unloads mesh and textures: main thread, GL context alive
      Result := True;
    end;
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

//The engine's own torus, built as a plain indexed mesh: A is the ring radius
//(centre to tube centre), B the tube radius, and the result stands on the
//engine's Z so it needs no axis fix. raylib 6.2's GenMeshTorus ignores its
//ring radius (the mesh only ever grows with the tube size) and hangs on its
//second call in a session, so it cannot be used here. The vertex arrays use
//raylib's own allocator so UnloadMesh frees them back into the same heap.
function BuildTorusMesh(Radius, Tube: Single; RingSegs, TubeSegs: Integer): TMesh;
var
  i, j, vi, ii, VertCount: Integer;
  theta, phi, ring: Single;
  Verts, Norms, Uvs: PSingle;
  Cols: PByte;
  Idx: PWord;
begin
  Result := Default(TMesh);
  if RingSegs < 3 then RingSegs := 3;
  if TubeSegs < 3 then TubeSegs := 3;
  if RingSegs > 255 then RingSegs := 255;
  if TubeSegs > 255 then TubeSegs := 255;
  //A closed grid: (RingSegs+1) x (TubeSegs+1) vertices, the last row/column
  //duplicated for a clean UV seam, stitched into quads by index.
  VertCount := (RingSegs + 1) * (TubeSegs + 1);
  Verts := MemAlloc(VertCount * SizeOf(Single) * 3);
  Norms := MemAlloc(VertCount * SizeOf(Single) * 3);
  Uvs := MemAlloc(VertCount * SizeOf(Single) * 2);
  //The white vertex colors are not decoration: rlgl only reads the mesh's
  //normals into the VBO when it also has per-vertex colors, and a mesh
  //without them uploads as flat position+texcoord and draws unlit - what
  //every GenMesh* builder fills, so the torus fills it too.
  Cols := MemAlloc(VertCount * 4);
  Idx := MemAlloc(RingSegs * TubeSegs * 6 * SizeOf(Word));
  vi := 0;
  for i := 0 to RingSegs do
  begin
    theta := (2 * Pi * i) / RingSegs;
    for j := 0 to TubeSegs do
    begin
      phi := (2 * Pi * j) / TubeSegs;
      ring := Radius + Tube * Cos(phi);
      Verts[vi * 3] := ring * Cos(theta);
      Verts[vi * 3 + 1] := ring * Sin(theta);
      Verts[vi * 3 + 2] := Tube * Sin(phi);
      Norms[vi * 3] := Cos(phi) * Cos(theta);
      Norms[vi * 3 + 1] := Cos(phi) * Sin(theta);
      Norms[vi * 3 + 2] := Sin(phi);
      Uvs[vi * 2] := i / RingSegs;
      Uvs[vi * 2 + 1] := j / TubeSegs;
      Cols[vi * 4] := 255;
      Cols[vi * 4 + 1] := 255;
      Cols[vi * 4 + 2] := 255;
      Cols[vi * 4 + 3] := 255;
      Inc(vi);
    end;
  end;
  ii := 0;
  for i := 0 to RingSegs - 1 do
    for j := 0 to TubeSegs - 1 do
    begin
      //Two triangles for the quad (i,j)-(i+1,j+1) of the grid.
      Idx[ii] := i * (TubeSegs + 1) + j;
      Idx[ii + 1] := (i + 1) * (TubeSegs + 1) + j;
      Idx[ii + 2] := i * (TubeSegs + 1) + (j + 1);
      Idx[ii + 3] := (i + 1) * (TubeSegs + 1) + j;
      Idx[ii + 4] := (i + 1) * (TubeSegs + 1) + (j + 1);
      Idx[ii + 5] := i * (TubeSegs + 1) + (j + 1);
      Inc(ii, 6);
    end;
  Result.VertexCount := VertCount;
  Result.TriangleCount := RingSegs * TubeSegs * 2;
  Result.Vertices := Verts;
  Result.Texcoords := Uvs;
  Result.Normals := Norms;
  Result.Colors := Cols;
  Result.Indices := Idx;
end;

//The engine's own sphere: a lat-long grid standing on the Z axis, one ring
//per step from the +z pole down, slices around. The pole quads collapse to
//points and the seam column repeats the first one for a clean UV wrap - like
//the GenMesh grids, and just as harmless.
function BuildSphereMesh(Radius: Single; Rings, Slices: Integer): TMesh;
var
  i, j, vi, ii, VertCount: Integer;
  theta, phi, st: Single;
  Verts, Norms, Uvs: PSingle;
  Cols: PByte;
  Idx: PWord;

  procedure Corner(ATheta, APhi, AU, AV: Single);
  var
    x, y, z: Single;
  begin
    st := Radius * Sin(ATheta);
    x := st * Cos(APhi);
    y := st * Sin(APhi);
    z := Radius * Cos(ATheta);
    Verts[vi * 3] := x;
    Verts[vi * 3 + 1] := y;
    Verts[vi * 3 + 2] := z;
    Norms[vi * 3] := x / Radius;
    Norms[vi * 3 + 1] := y / Radius;
    Norms[vi * 3 + 2] := z / Radius;
    Uvs[vi * 2] := AU;
    Uvs[vi * 2 + 1] := AV;
    Cols[vi * 4] := 255;
    Cols[vi * 4 + 1] := 255;
    Cols[vi * 4 + 2] := 255;
    Cols[vi * 4 + 3] := 255;
    Inc(vi);
  end;

begin
  Result := Default(TMesh);
  if Rings < 2 then Rings := 2;
  if Slices < 3 then Slices := 3;
  if Radius <= 0 then Radius := 1;
  VertCount := (Rings + 1) * (Slices + 1);
  Verts := MemAlloc(VertCount * SizeOf(Single) * 3);
  Norms := MemAlloc(VertCount * SizeOf(Single) * 3);
  Uvs := MemAlloc(VertCount * SizeOf(Single) * 2);
  Cols := MemAlloc(VertCount * 4);
  Idx := MemAlloc(Rings * Slices * 6 * SizeOf(Word));
  vi := 0;
  for i := 0 to Rings do
    for j := 0 to Slices do
      Corner((Pi * i) / Rings, (2 * Pi * j) / Slices, j / Slices, i / Rings);
  ii := 0;
  for i := 0 to Rings - 1 do
    for j := 0 to Slices - 1 do
    begin
      Idx[ii] := i * (Slices + 1) + j;
      Idx[ii + 1] := (i + 1) * (Slices + 1) + j;
      Idx[ii + 2] := i * (Slices + 1) + (j + 1);
      Idx[ii + 3] := (i + 1) * (Slices + 1) + j;
      Idx[ii + 4] := (i + 1) * (Slices + 1) + (j + 1);
      Idx[ii + 5] := i * (Slices + 1) + (j + 1);
      Inc(ii, 6);
    end;
  Result.VertexCount := VertCount;
  Result.TriangleCount := Rings * Slices * 2;
  Result.Vertices := Verts;
  Result.Texcoords := Uvs;
  Result.Normals := Norms;
  Result.Colors := Cols;
  Result.Indices := Idx;
end;

//The engine's own drum, standing on the Z axis: one side band with radial
//normals, then a centre-and-rim cap on each end. Smooth on the mantle, hard
//edged where the lids meet it - the look a post or a wheel wants.
function BuildCylinderMesh(Radius, Height: Single; Slices: Integer): TMesh;
var
  i, j, vi, ii, VertCount: Integer;
  ang, x, y, z, n: Single;
  SideVerts, CapVerts: Integer;
  Verts, Norms, Uvs: PSingle;
  Cols: PByte;
  Idx: PWord;

  procedure Corner(X, Y, Z, NX, NY, NZ, AU, AV: Single);
  begin
    Verts[vi * 3] := X;
    Verts[vi * 3 + 1] := Y;
    Verts[vi * 3 + 2] := Z;
    Norms[vi * 3] := NX;
    Norms[vi * 3 + 1] := NY;
    Norms[vi * 3 + 2] := NZ;
    Uvs[vi * 2] := AU;
    Uvs[vi * 2 + 1] := AV;
    Cols[vi * 4] := 255;
    Cols[vi * 4 + 1] := 255;
    Cols[vi * 4 + 2] := 255;
    Cols[vi * 4 + 3] := 255;
    Inc(vi);
  end;

begin
  Result := Default(TMesh);
  if Slices < 3 then Slices := 3;
  if Radius <= 0 then Radius := 1;
  if Height <= 0 then Height := 1;
  SideVerts := 2 * (Slices + 1);
  CapVerts := 2 * (Slices + 2);
  VertCount := SideVerts + CapVerts;
  Verts := MemAlloc(VertCount * SizeOf(Single) * 3);
  Norms := MemAlloc(VertCount * SizeOf(Single) * 3);
  Uvs := MemAlloc(VertCount * SizeOf(Single) * 2);
  Cols := MemAlloc(VertCount * 4);
  Idx := MemAlloc(Slices * 6 * SizeOf(Word) + 2 * Slices * 3 * SizeOf(Word));
  vi := 0;
  for i := 0 to 1 do
    for j := 0 to Slices do
    begin
      ang := (2 * Pi * j) / Slices;
      Corner(Radius * Cos(ang), Radius * Sin(ang), (i - 0.5) * Height,
        Cos(ang), Sin(ang), 0, j / Slices, i);
    end;
  for i := 0 to 1 do
  begin
    if i = 1 then n := 1 else n := -1;
    z := (i - 0.5) * Height;
    Corner(0, 0, z, 0, 0, n, 0.5, 0.5);
    for j := 0 to Slices do
    begin
      ang := (2 * Pi * j) / Slices;
      x := Radius * Cos(ang);
      y := Radius * Sin(ang);
      Corner(x, y, z, 0, 0, n, (Cos(ang) + 1) * 0.5, (Sin(ang) + 1) * 0.5);
    end;
  end;
  ii := 0;
  for j := 0 to Slices - 1 do
  begin
    //The mantle: two triangles per step, wound so the normals face out.
    Idx[ii] := j;
    Idx[ii + 1] := j + 1;
    Idx[ii + 2] := Slices + 1 + j;
    Idx[ii + 3] := Slices + 1 + j;
    Idx[ii + 4] := j + 1;
    Idx[ii + 5] := Slices + 2 + j;
    Inc(ii, 6);
  end;
  for i := 0 to 1 do
  begin
    //The lids: a fan from the centre vertex; the bottom one winds the other
    //way so its flat normal still faces out.
    vi := SideVerts + i * CapVerts div 2;
    for j := 0 to Slices - 1 do
      if i = 1 then
      begin
        Idx[ii] := vi;
        Idx[ii + 1] := vi + 1 + j;
        Idx[ii + 2] := vi + 2 + j;
      end
      else
      begin
        Idx[ii] := vi;
        Idx[ii + 1] := vi + 2 + j;
        Idx[ii + 2] := vi + 1 + j;
      end;
      Inc(ii, 3);
  end;
  Result.VertexCount := VertCount;
  Result.TriangleCount := Slices * 4;
  Result.Vertices := Verts;
  Result.Texcoords := Uvs;
  Result.Normals := Norms;
  Result.Colors := Cols;
  Result.Indices := Idx;
end;

//The engine's own cone: a base ring and an apex ring (the apex sits on one
//point but keeps one normal per angle, so the mantle shades smooth), plus the
//base lid. Standing on the Z axis like the drum.
function BuildConeMesh(Radius, Height: Single; Slices: Integer): TMesh;
var
  j, vi, ii, VertCount, CapStart: Integer;
  ang, x, y, nlen: Single;
  Verts, Norms, Uvs: PSingle;
  Cols: PByte;
  Idx: PWord;

  procedure Corner(X, Y, Z, NX, NY, NZ, AU, AV: Single);
  begin
    Verts[vi * 3] := X;
    Verts[vi * 3 + 1] := Y;
    Verts[vi * 3 + 2] := Z;
    Norms[vi * 3] := NX;
    Norms[vi * 3 + 1] := NY;
    Norms[vi * 3 + 2] := NZ;
    Uvs[vi * 2] := AU;
    Uvs[vi * 2 + 1] := AV;
    Cols[vi * 4] := 255;
    Cols[vi * 4 + 1] := 255;
    Cols[vi * 4 + 2] := 255;
    Cols[vi * 4 + 3] := 255;
    Inc(vi);
  end;

begin
  Result := Default(TMesh);
  if Slices < 3 then Slices := 3;
  if Radius <= 0 then Radius := 1;
  if Height <= 0 then Height := 1;
  nlen := Sqrt(Height * Height + Radius * Radius);
  VertCount := 2 * (Slices + 1) + (Slices + 2);
  Verts := MemAlloc(VertCount * SizeOf(Single) * 3);
  Norms := MemAlloc(VertCount * SizeOf(Single) * 3);
  Uvs := MemAlloc(VertCount * SizeOf(Single) * 2);
  Cols := MemAlloc(VertCount * 4);
  Idx := MemAlloc(Slices * 9 * SizeOf(Word));
  vi := 0;
  for j := 0 to Slices do
  begin
    ang := (2 * Pi * j) / Slices;
    Corner(Radius * Cos(ang), Radius * Sin(ang), -Height * 0.5,
      Height * Cos(ang) / nlen, Height * Sin(ang) / nlen, Radius / nlen,
      j / Slices, 0);
  end;
  for j := 0 to Slices do
  begin
    ang := (2 * Pi * j) / Slices;
    Corner(0, 0, Height * 0.5,
      Height * Cos(ang) / nlen, Height * Sin(ang) / nlen, Radius / nlen,
      j / Slices, 1);
  end;
  CapStart := 2 * (Slices + 1);
  Corner(0, 0, -Height * 0.5, 0, 0, -1, 0.5, 0.5);
  for j := 0 to Slices do
  begin
    ang := (2 * Pi * j) / Slices;
    x := Radius * Cos(ang);
    y := Radius * Sin(ang);
    Corner(x, y, -Height * 0.5, 0, 0, -1,
      (Cos(ang) + 1) * 0.5, (Sin(ang) + 1) * 0.5);
  end;
  ii := 0;
  for j := 0 to Slices - 1 do
  begin
    //The mantle, base edge first so the slope normals face out; the apex
    //quads close to points at the top, like the poles of the sphere.
    Idx[ii] := j;
    Idx[ii + 1] := Slices + 1 + j;
    Idx[ii + 2] := j + 1;
    Idx[ii + 3] := Slices + 1 + j;
    Idx[ii + 4] := Slices + 2 + j;
    Idx[ii + 5] := j + 1;
    Inc(ii, 3);
    Inc(ii, 3);
  end;
  for j := 0 to Slices - 1 do
  begin
    //The lid, wound the other way for its down-facing normal.
    Idx[ii] := CapStart;
    Idx[ii + 1] := CapStart + 2 + j;
    Idx[ii + 2] := CapStart + 1 + j;
    Inc(ii, 3);
  end;
  Result.VertexCount := VertCount;
  Result.TriangleCount := Slices * 3;
  Result.Vertices := Verts;
  Result.Texcoords := Uvs;
  Result.Normals := Norms;
  Result.Colors := Cols;
  Result.Indices := Idx;
end;

//Generate one of the built-in shapes and put it into an existing slot,
//mirroring Load. Only the box comes from raylib (GenMeshCube, whose output
//arrives already on the GPU); the sphere, drum, cone and torus are built by
//the engine's own generators above, standing on the world's z like everything
//else here - no axis fixes anywhere. The engine-built meshes therefore need
//their one UploadMesh here, before LoadModelFromMesh takes them: that call
//trusts a mesh with no VBO to be already uploaded the way raylib's builders
//deliver it, and a mesh that never went up draws nothing at all. Must run on
//the main thread, where the GL context lives.
function TModels3D.BuildShape(Handle: integer; AShape: TTyroModelShape;
  A, B, C: Single): boolean;
var
  Item: TModel3D;
  NewModel: TModel;
  Mesh: TMesh;
  Box: TBoundingBox;
  EngineBuilt: Boolean;
begin
  Result := False;
  //Unit sizes where the script left a hole, so a bare models.box() still
  //builds something rather than a degenerate point.
  if A <= 0 then
    A := 1;
  if B <= 0 then
    B := 1;
  if C <= 0 then
    C := 1;
  EngineBuilt := True;
  case AShape of
    msBox:
      begin
        Mesh := RayLib.GenMeshCube(A, B, C);
        EngineBuilt := False;
      end;
    msCylinder:
      //24 slices: round enough for a post or a wheel at game distances.
      Mesh := BuildCylinderMesh(A, B, 24);
    msCone:
      Mesh := BuildConeMesh(A, B, 24);
    msTorus:
      //64 steps around the loop, 24 around the tube: smooth enough to ride,
      //cheap enough to build a whole track and a score of hoops from it.
      Mesh := BuildTorusMesh(A, B, 64, 24);
  else
    //msSphere - 16 rings / 24 slices: smooth enough for a planet in close
    //view, cheap for dozens of cloud puffs and stars.
    Mesh := BuildSphereMesh(A, 16, 24);
  end;
  if EngineBuilt then
    UploadMesh(@Mesh, False);
  NewModel := RayLib.LoadModelFromMesh(Mesh);
  if NewModel.MeshCount <= 0 then
  begin
    if IsConsole then
      WriteLn('Model shape not built: kind ' + IntToStr(Ord(AShape)) +
        ' (' + FloatToStr(A) + ', ' + FloatToStr(B) + ', ' + FloatToStr(C) + ')');
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
    //The shape carries no file, so no material names - and like Load, the
    //fresh mesh starts in its own skin (the old dress went with the old mesh).
    Item.MatNames := nil;
    Item.WearingLight := False;
    SetLength(Item.OwnShaders, 0);

    //Same bookkeeping as Load: the box feeds the script-side sizes and the
    //pivot, so the shape stays centred on its x/y/z however it is moved.
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

{ The material names of a glTF file. raylib uploads the meshes and forgets
  the materials were ever named - its Material record carries no name - so a
  script that asks for one by name (model.glow) gets the names back out of
  the package's JSON head, the one plain-text part of the file. }

function GltfMatNames(const AFileName: string): TMatNames;
const
  cGlbMagic = $46546C67;  //'glTF' as the package spells it little-endian
  cJsonChunk = $4E4F534A; //and 'JSON' for the chunk type right behind it
  cMaxJson = $400000;     //4 MB of JSON is already a very talkative model
var
  FS: TFileStream;
  Head: array[0..19] of Byte;
  Json: string;
  Code: RawByteString;
  n, i, j, Len, Depth, Slot: Integer;
  C: Char;
  InStr, Esc: Boolean;
  S, Key: string;

  function LE(At: Integer): Cardinal;
  begin
    Result := Head[At] or (Cardinal(Head[At + 1]) shl 8)
      or (Cardinal(Head[At + 2]) shl 16) or (Cardinal(Head[At + 3]) shl 24);
  end;

begin
  Result := nil;
  if not SameText(ExtractFileExt(AFileName), '.glb') and
     not SameText(ExtractFileExt(AFileName), '.gltf') then
    Exit;
  //The JSON head: in a .glb it rides as the file's first chunk, plain text
  //behind the 20-byte double header - only that chunk is read, the binary
  //belly stays on the disk. A plain .gltf is JSON from byte one.
  FS := nil;
  try
    try
      FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
      if FS.Size > SizeOf(Head) then
      begin
        FS.ReadBuffer(Head, SizeOf(Head));
        if (LE(0) = cGlbMagic) and (LE(16) = cJsonChunk) then
        begin
          n := LE(12); //the JSON chunk's own length
          if (n > 0) and (n <= cMaxJson) and
            (Cardinal(FS.Size) >= n + SizeOf(Head)) then
          begin
            SetLength(Code, n);
            FS.ReadBuffer(Code[1], n);
            Json := string(Code);
          end;
        end
        else if (LE(0) <> cGlbMagic) and (FS.Size <= cMaxJson) then
        begin
          FS.Position := 0;
          SetLength(Code, FS.Size);
          FS.ReadBuffer(Code[1], Length(Code));
          Json := string(Code);
        end;
      end;
    except
      Json := '';
    end;
  finally
    FS.Free;
  end;
  Len := Length(Json);
  i := Pos('"materials"', Json);
  if (i = 0) or (Len = 0) then
    Exit;
  while (i <= Len) and (Json[i] <> '[') do
    Inc(i);
  if i > Len then
    Exit;
  //Walk the array counting the brackets under it, with quoted text read as a
  //whole so nothing written inside a string fools the walk. Every '{' one
  //level in opens a material and books its slot right away - a nameless
  //material cannot slide the ones after it off their index - and a "name"
  //property of one is copied in.
  Depth := 0;
  Slot := -1;
  InStr := False;
  Esc := False;
  Key := '';
  S := '';
  while i <= Len do
  begin
    C := Json[i];
    if InStr then
    begin
      if Esc then
      begin
        S := S + C;
        Esc := False;
      end
      else if C = '\' then
        S := S + C //the escape rides along - quoted quotes are rare air
      else if C = '"' then
      begin
        InStr := False;
        if Depth = 2 then
        begin
          //a property of the material: the text with a colon ahead of it is
          //the property's name, the one behind a "name" is the material's
          j := i + 1;
          while (j <= Len) and ((Json[j] = ' ') or (Json[j] = #9) or
            (Json[j] = #13) or (Json[j] = #10)) do
            Inc(j);
          if (j <= Len) and (Json[j] = ':') then
            Key := S
          else if (Key = 'name') and (Slot >= 0) then
            Result[Slot] := S;
        end;
      end
      else
        S := S + C;
    end
    else if C = '"' then
    begin
      InStr := True;
      Esc := False;
      S := '';
    end
    else if (C = '[') or (C = '{') then
    begin
      Inc(Depth);
      if (C = '{') and (Depth = 2) then
      begin
        Inc(Slot);
        SetLength(Result, Slot + 1);
        Result[Slot] := '';
      end;
    end
    else if (C = ']') or (C = '}') then
    begin
      if (C = ']') and (Depth = 1) then
        Break; //the materials array ends here
      Dec(Depth);
    end;
    Inc(i);
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
  AAnims: PModelAnimation;
  ACount: Integer;
  m: Integer;
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
    //raylib drops the glTF material names once the mesh is on the GPU, so
    //the package's own JSON head is read for them - that is what model.glow
    //matches. The loader keeps one spare skin of its own (the white default
    //material primitives without one wear) ahead of the file's list, so the
    //names start at the material behind it. The fresh model also starts in
    //its own skin again: the dress and the saved shaders belonged to the
    //model that just went away.
    Item.MatNames := GltfMatNames(AFileName);
    //raylib's glTF loader keeps one white default material at index 0 for
    //primitives that name none, so the file's list lines up one behind it -
    //and only does when that spare is there. If a build ever lines them up
    //one-to-one, the names stay where they are.
    if (Length(Item.MatNames) > 0) and
      (Item.Model.MaterialCount = Length(Item.MatNames) + 1) then
    begin
      SetLength(Item.MatNames, Item.Model.MaterialCount);
      for m := High(Item.MatNames) - 1 downto 0 do
        Item.MatNames[m + 1] := Item.MatNames[m];
      Item.MatNames[0] := '';
    end;
    Item.WearingLight := False;
    SetLength(Item.OwnShaders, 0);

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

    //The animation clips ride in the same file: grab them right behind the
    //mesh (this is already the queued, main-thread step). A jet or a cloud
    //puff carries none and lands back with an empty list, exactly as before.
    ACount := 0;
    AAnims := RayLib.LoadModelAnimations(PUTF8Char(UTF8String(AFileName)), ACount);
    if AAnims = nil then
      ACount := 0;
    Item.Animations := AAnims;
    Item.AnimCount := ACount;
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

procedure TModels3D.SetLayer(Handle: integer; ALayer: integer);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Layer := ALayer;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetLayer(Handle: integer): integer;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Layer;
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

//A clip is posed by writing its bone transforms onto the model, so Step only
//has to land the frame; the 3D pass draws whatever pose the last Step left.
//glTF loaders bake a clip's keyframes at 30 per second, which is why Speed
//counts keyframes and the seconds-facing setter below folds the same rate.
const
  cModel3DAnimFPS = 30;

procedure TModels3D.Step(ADt: single);
var
  Item: TModel3D;
  Anim: PModelAnimation;
  LoopLen: Integer;
begin
  FLock.Enter;
  try
    for Item in FItems.Values do
    begin
      if (not Item.Playing) or (not Item.Loaded) then
        Continue;
      Anim := Item.CurrentAnim;
      if (Anim = nil) or (Anim^.KeyframeCount < 2) or
        (Item.Model.BoneMatrices = nil) then
        Continue;
      //The bake repeats the first pose at the last keyframe so the cycle
      //closes; the loop therefore runs over KeyframeCount-1 frames and wraps.
      LoopLen := Anim^.KeyframeCount - 1;
      if LoopLen < 1 then
        Continue;
      Item.Frame := Item.Frame + ADt * Item.Speed;
      while Item.Frame >= LoopLen do
        Item.Frame := Item.Frame - LoopLen;
      RayLib.UpdateModelAnimation(Item.Model, Anim^, Item.Frame);
    end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAnimCount(Handle: integer): integer;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.AnimCount;
  finally
    FLock.Leave;
  end;
end;

//Cut one model to a clip and lift its play flag; AIndex < 0 stops and keeps
//the current pose. Re-playing the clip already running does NOT rewind, so a
//script can call play() every frame it wants the walker striding without the
//stride stuttering. A fresh cut poses frame zero at once so the switch shows
//this frame rather than a step later. The caller holds the lock.
function TModels3D.PlayItemAnim(AItem: TModel3D; AIndex: integer): boolean;
var
  Anim: PModelAnimation;
begin
  Result := False;
  if AIndex < 0 then
  begin
    AItem.Playing := False;
    Exit(True);
  end;
  if (AItem.Animations = nil) or (AIndex >= AItem.AnimCount) then
    Exit;
  AItem.Playing := True;
  if AIndex <> AItem.Current then
  begin
    AItem.Current := AIndex;
    AItem.Frame := 0;
    Anim := @PModelAnimArray(AItem.Animations)^[AIndex];
    if (AItem.Model.BoneMatrices <> nil) and (Anim^.KeyframeCount > 0) then
      RayLib.UpdateModelAnimation(AItem.Model, Anim^, 0);
  end;
  Result := True;
end;

function TModels3D.GetItemAnimName(AItem: TModel3D; AIndex: integer): string;
begin
  Result := '';
  if (AItem.Animations <> nil) and (AIndex >= 0) and (AIndex < AItem.AnimCount) then
    //raylib leaves the clip name zero-padded in its fixed 32 bytes; reading it
    //as a C string stops at the first #0.
    Result := string(PAnsiChar(@PModelAnimArray(AItem.Animations)^[AIndex].Name[0]));
end;

function TModels3D.GetAnimName(Handle: integer; AIndex: integer): string;
var
  Item: TModel3D;
begin
  Result := '';
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := GetItemAnimName(Item, AIndex);
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAnimFrames(Handle: integer; AIndex: integer): integer;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      if (Item.Animations <> nil) and (AIndex >= 0) and (AIndex < Item.AnimCount) then
        Result := PModelAnimArray(Item.Animations)^[AIndex].KeyframeCount;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.PlayAnim(Handle: integer; AIndex: integer): boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := PlayItemAnim(Item, AIndex);
  finally
    FLock.Leave;
  end;
end;

function TModels3D.PlayAnimByName(Handle: integer; const AName: string): boolean;
var
  Item: TModel3D;
  I: Integer;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      for I := 0 to Item.AnimCount - 1 do
        if GetItemAnimName(Item, I) = AName then
          Exit(PlayItemAnim(Item, I));
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.StopAnim(Handle: integer);
begin
  PlayAnim(Handle, -1);
end;

function TModels3D.IsAnimPlaying(Handle: integer): boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Playing;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAnimCurrent(Handle: integer): integer;
var
  Item: TModel3D;
begin
  Result := -1;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Current;
  finally
    FLock.Leave;
  end;
end;

//Scrub to a moment in the current clip: the frame is written back and the
//model posed at once, so a paused walker can be stepped through its stride
//one frame at a time. The seconds the script sees fold the 30 fps bake.
procedure TModels3D.SetAnimTime(Handle: integer; ASeconds: single);
var
  Item: TModel3D;
  Anim: PModelAnimation;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      Anim := Item.CurrentAnim;
      if (Anim <> nil) and (Anim^.KeyframeCount > 1) then
      begin
        Item.Frame := ASeconds * cModel3DAnimFPS;
        if Item.Frame < 0 then
          Item.Frame := 0
        else if Item.Frame > Anim^.KeyframeCount - 1 then
          Item.Frame := Anim^.KeyframeCount - 1;
        if Item.Model.BoneMatrices <> nil then
          RayLib.UpdateModelAnimation(Item.Model, Anim^, Item.Frame);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAnimTime(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      if Item.CurrentAnim <> nil then
        Result := Item.Frame / cModel3DAnimFPS;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetAnimSpeed(Handle: integer; AValue: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      //0 stops the clock but keeps the play flag, a negative speed walks the
      //clip backwards; the cap is only there so a typo can't eat the frame.
      if AValue > 600 then
        AValue := 600;
      Item.Speed := AValue;
    end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetAnimSpeed(Handle: integer): single;
var
  Item: TModel3D;
begin
  Result := 30;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Speed;
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

//Setting the shot from the script thread always takes the free camera on: a
//script that wants the old top-down pass after this calls world.ortho().
procedure TModels3D.SetCamera(PX, PY, PZ, TX, TY, TZ, UX, UY, UZ: single);
begin
  FLock.Enter;
  try
    FCamPos := Vector3Of(PX, PY, PZ);
    //An eye aimed at itself has no direction to look: keep the old target
    //then (and the origin when there is no better one yet).
    if (TX = PX) and (TY = PY) and (TZ = PZ) then
    begin
      if (FCamTarget.X = PX) and (FCamTarget.Y = PY) and (FCamTarget.Z = PZ) then
        FCamTarget := Vector3Of(0, 0, 0);
    end
    else
      FCamTarget := Vector3Of(TX, TY, TZ);
    if (UX <> 0) or (UY <> 0) or (UZ <> 0) then
      FCamUp := Vector3Of(UX, UY, UZ);
    FFreeCam := True;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.GetCamera(out PX, PY, PZ, TX, TY, TZ, UX, UY, UZ: single);
begin
  FLock.Enter;
  try
    PX := FCamPos.X;
    PY := FCamPos.Y;
    PZ := FCamPos.Z;
    TX := FCamTarget.X;
    TY := FCamTarget.Y;
    TZ := FCamTarget.Z;
    UX := FCamUp.X;
    UY := FCamUp.Y;
    UZ := FCamUp.Z;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetFreeCam(AEnabled: boolean);
begin
  FLock.Enter;
  try
    FFreeCam := AEnabled;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetFreeCam: boolean;
begin
  FLock.Enter;
  try
    Result := FFreeCam;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetFov(AValue: single);
begin
  FLock.Enter;
  try
    //Past a right angle the wide-angle edge of the frame already swallows the
    //sides, and below ten degrees it is a telephoto peek - neither helps a kid.
    FFov := Min(170, Max(10, AValue));
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetFov: single;
begin
  FLock.Enter;
  try
    Result := FFov;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetWire(AEnabled: boolean);
begin
  FLock.Enter;
  try
    FWire := AEnabled;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetWire: boolean;
begin
  FLock.Enter;
  try
    Result := FWire;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetLighting(AEnabled: boolean);
begin
  FLock.Enter;
  try
    FLightOn := AEnabled;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetLighting: boolean;
begin
  FLock.Enter;
  try
    Result := FLightOn;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetPointLight(ANum: integer; PX, PY, PZ, ARange, R, G, B: single);
begin
  if (ANum < 0) or (ANum > 3) then
    Exit;
  FLock.Enter;
  try
    FPointLights[ANum].X := PX;
    FPointLights[ANum].Y := PY;
    FPointLights[ANum].Z := PZ;
    FPointLights[ANum].Range := ARange;
    FPointLights[ANum].R := R;
    FPointLights[ANum].G := G;
    FPointLights[ANum].B := B;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetFace(Handle: integer; TX, TY, TZ, UX, UY, UZ: single);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      Item.FaceTo := Vector3Of(TX, TY, TZ);
      if (UX = 0) and (UY = 0) and (UZ = 0) then
        Item.FaceUp := Vector3Of(0, 0, 1) //a zero lean would flatten the basis
      else
        Item.FaceUp := Vector3Of(UX, UY, UZ);
      Item.HasFace := True;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.ClearFace(Handle: integer);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.HasFace := False;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetNose(Handle: integer; const ANose: string);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      Item.Nose := ANose;
      // The fix rotates the mesh so the named nose axis lands on the +y that
      // face() aims, keeping a right-handed frame (a true rotation, so the
      // winding - and with it the light - is untouched). Each is the map of
      // the model's own axes onto the engine's (columns are the images).
      if ANose = 'z' then
      begin
        // +z -> +y, +y -> -z, +x -> +x: a nose-up file lies down to fly.
        Item.AxisFix := cModel3DAxisNoseZ;
        Item.HasAxisFix := True;
      end
      else if ANose = '-z' then
      begin
        // -z -> +y: the same swing the other way - and the very map the old
        // Y-spun generators wanted, so it borrows that constant.
        Item.AxisFix := cModel3DAxisZUp;
        Item.HasAxisFix := True;
      end
      else if ANose = 'x' then
      begin
        // +x -> +y, +y -> -x, +z -> +z.
        Item.AxisFix := cModel3DAxisNoseX;
        Item.HasAxisFix := True;
      end
      else if ANose = '-x' then
      begin
        // -x -> +y, +y -> +x, +z -> +z.
        Item.AxisFix := cModel3DAxisNoseNegX;
        Item.HasAxisFix := True;
      end
      else if ANose = '-y' then
      begin
        // -y -> +y: a half turn about +z, for a file whose nose points
        // backwards down the engine's own convention axis. Wings swap ends
        // and the belly keeps facing down.
        Item.AxisFix := cModel3DAxisNoseNegY;
        Item.HasAxisFix := True;
      end
      else
      begin
        // "y" (the default): the mesh already flies with the box convention.
        Item.AxisFix := cModel3DIdentity;
        Item.HasAxisFix := False;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetNose(Handle: integer): string;
var
  Item: TModel3D;
begin
  Result := 'y';
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Nose;
  finally
    FLock.Leave;
  end;
end;

procedure TModels3D.SetGlow(Handle: integer; const AGlow: string);
var
  Item: TModel3D;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      if Item.Glow = AGlow then
        Exit;
      Item.Glow := AGlow;
      //The dress was put on under the old answer, so take it off: the next
      //frame's WearLight dresses again and the newly-named material - or the
      //one that no longer glows - gets its shader right then.
      if Item.WearingLight then
        WearOwn(Item);
    end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetGlow(Handle: integer): string;
var
  Item: TModel3D;
begin
  Result := '';
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Glow;
  finally
    FLock.Leave;
  end;
end;

// How hard the shining material burns. The level snaps to the detent (see
// cModel3DBrightStep) so a script that pulses the shine cannot ask for a new
// shader every frame, and the dress the old level wore comes off here: the next
// frame's WearLight puts the right one on.
procedure TModels3D.SetBright(Handle: integer; ABright: Single);
var
  Item: TModel3D;
begin
  if ABright < 0 then
    ABright := 0
  else if ABright > cModel3DBrightMax then
    ABright := cModel3DBrightMax;
  ABright := Round(ABright / cModel3DBrightStep) * cModel3DBrightStep;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
    begin
      if Abs(Item.Bright - ABright) < 0.001 then
        Exit;
      Item.Bright := ABright;
      if Item.WearingLight then
        WearOwn(Item);
    end;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.GetBright(Handle: integer): Single;
var
  Item: TModel3D;
begin
  Result := 1;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Bright;
  finally
    FLock.Leave;
  end;
end;

function TModels3D.HasSide(AUnder: boolean): boolean;
var
  Item: TModel3D;
begin
  Result := False;
  FLock.Enter;
  try
    for Item in FItems.Values do
      if Item.Visible and Item.Loaded and ((Item.Layer < 0) = AUnder) then
        Exit(True);
  finally
    FLock.Leave;
  end;
end;

function TModels3D.HasUnder: boolean;
begin
  Result := HasSide(True);
end;

function TModels3D.HasTop: boolean;
begin
  Result := HasSide(False);
end;

procedure TModels3D.Draw(AX, AY: single; AWidth, AHeight: Integer; AUnder: boolean);
const
  //Distance of the eye above the pixel plane. The ortho frustum clips at
  //0.01..1000, so this leaves the models room for a +/-399 depth range.
  cModel3DDistance = 400.0;
type
  //One body on the painter's list: where it stands, how far down the sight
  //line it is, and whether it is glass - a see-through skin, which is painted
  //after the solids and does not keep its depth.
  TPaintOne = record
    Item: TModel3D;
    Pos: TVector3;
    Depth: Single;
    Glass: Boolean;
  end;
var
  Cam: TCamera3D;
  Item: TModel3D;
  S: Single;
  Pos: TVector3;
  FaceM: TMatrix;
  ShadowM: TMatrix;
  ItemAlpha: Integer;
  KX, KZ, G, LY: Single;
  Alpha: Integer;
  AnyVisible: Boolean;
  LitNow: Boolean;
  SeeThrough: Boolean;
  PaintList: array of TPaintOne;
  One, Hold: TPaintOne;
  N, j: Integer;
  DX, DY, DZ, DL: Single;
  i: Integer;
  Amb, L: Single;
  Sun4, Light4: array[0..3] of Single;
begin
  AnyVisible := False;
  FLock.Enter;
  try
    for Item in FItems.Values do
      if Item.Visible and Item.Loaded and ((Item.Layer < 0) = AUnder) then
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
    if FFreeCam then
    begin
      //The script-driven eye (world.camera): the same z-up world, looked at
      //through a perspective frustum. raylib's perspective clips at the same
      //0.01..1000 as the ortho box, so a free 3D world has to live inside
      //that reach - no tighter or looser than the pass that drew it before.
      Cam.Position := FCamPos;
      Cam.Target := FCamTarget;
      Cam.Up := FCamUp;
      Cam.Fovy := FFov;
      Cam.Projection := Ord(CAMERA_PERSPECTIVE); //the field is a plain Integer
    end
    else
    begin
      Cam.Position := Vector3Of(0, -cModel3DDistance, 0);
      Cam.Target := Vector3Of(0, 0, 0);
      Cam.Up := Vector3Of(0, 0, 1);
      Cam.Fovy := AHeight;
      Cam.Projection := Ord(CAMERA_ORTHOGRAPHIC); //the field is a plain Integer
    end;

    //The sight line, unit long: the axis the see-through bodies are measured
    //along. Under the ortho eye the camera looks straight down the world's y, so
    //the same measure is the pass's own depth - bigger = farther - unchanged.
    DX := Cam.Target.X - Cam.Position.X;
    DY := Cam.Target.Y - Cam.Position.Y;
    DZ := Cam.Target.Z - Cam.Position.Z;
    DL := Sqrt(DX * DX + DY * DY + DZ * DZ);
    if DL = 0 then
      DL := 1; //an eye aimed at itself: keep the division finite
    DX := DX / DL;
    DY := DY / DL;
    DZ := DZ / DL;

    //The shade the bodies drop on the world, laid before a single model is
    //drawn: the sea under it, every model over it, so a cloud that floats
    //between the viewer and the water hides the shade under it, as it should.
    //The silhouettes are inked onto the sheet first and the sheet goes down in
    //one stroke, so the shade is one flat colour wherever anything covers: a
    //canopy over a wing, or one body over another, cannot make it deeper, any
    //more than a spot can be out of one sun twice. Without the ink or without
    //the sheet there is no shade at all (a store that could not build them
    //leaves the sea bare). Only the ortho eye lays this shade: the ground
    //plane it falls on is a top-down notion, and a free 3D scene reads its
    //depth directly instead.
    if (not FFreeCam) and (Alpha > 0) and (ShadowMaterial.Maps <> nil)
      and ShadowSheet(AWidth, AHeight) then
    begin
      //The solid bodies share one sheet: their shade is the same shade, so one
      //stroke of it covers all of them.
      if InkShadowSheet(Cam, ShadowM, AX, AY, AWidth, AHeight, 255, AUnder) then
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
        if ((Item.Layer < 0) <> AUnder) then
          Continue;
        if Item.Tint.RGBA.Alpha >= 255 then
          Continue;
        ItemAlpha := Round(Alpha * Item.Tint.RGBA.Alpha / 255);
        if (ItemAlpha > 0)
          and InkShadowSheet(Cam, ShadowM, AX, AY, AWidth, AHeight,
            Item.Tint.RGBA.Alpha, AUnder) then
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
      //A rider can end up inside a body - the tube of a torus is the classic
      //case - and a world that throws away its back faces then reads as
      //painted cloth with the sky showing through. Every body here is solid
      //and the depth test has already decided what is in front, so the far
      //skins are simply painted too.
      RayLib.rlDisableBackfaceCulling;
      // The scene lights: the frame decides here whether the solids wear the
      // light dress (built lazily, on this GL-alive thread). Wires skip it -
      // a bare grid takes its mood from the bloom instead. The light itself
      // is the store's own kit: the sky's ambient fill, the sun along the
      // cast-shadow ray normalised, and the four bulbs as they were hung.
      // The settings go into the shared shader with its program active; they
      // sit there for every model that wears it this frame.
      LitNow := False;
      if FLightOn and (not FWire) then
      begin
        EnsureLightShader;
        LitNow := FLightReady;
      end;
      if LitNow then
      begin
        RayLib.BeginShaderMode(FLightShader);
        try
          Amb := FAmbient;
          RayLib.SetShaderValue(FLightShader, FLightLocAmbient,
            PUTF8Char(Pointer(@Amb)), Ord(SHADER_UNIFORM_FLOAT));
          L := Sqrt(FLightDir.X * FLightDir.X + FLightDir.Y * FLightDir.Y
            + FLightDir.Z * FLightDir.Z);
          if L = 0 then
            L := 1; //SetLight keeps the ray alive; belt and braces again.
          Sun4[0] := FLightDir.X / L;
          Sun4[1] := FLightDir.Y / L;
          Sun4[2] := FLightDir.Z / L;
          Sun4[3] := 1; //the sun shines whenever the lights are on
          RayLib.SetShaderValue(FLightShader, FLightLocSun,
            PUTF8Char(Pointer(@Sun4[0])), Ord(SHADER_UNIFORM_VEC4));
          for i := 0 to 3 do
          begin
            Light4[0] := FPointLights[i].X;
            Light4[1] := FPointLights[i].Y;
            Light4[2] := FPointLights[i].Z;
            Light4[3] := FPointLights[i].Range; //<= 0: the bulb stays dark
            RayLib.SetShaderValue(FLightShader, FLightLocPL[i],
              PUTF8Char(Pointer(@Light4[0])), Ord(SHADER_UNIFORM_VEC4));
            Light4[0] := FPointLights[i].R;
            Light4[1] := FPointLights[i].G;
            Light4[2] := FPointLights[i].B;
            Light4[3] := 1;
            RayLib.SetShaderValue(FLightShader, FLightLocPLC[i],
              PUTF8Char(Pointer(@Light4[0])), Ord(SHADER_UNIFORM_VEC4));
          end;
        finally
          RayLib.EndShaderMode;
        end;
      end;
      //The models themselves, painted over the shade - in an order of the
      //pass's own making, not the store's. The solids go first, exactly as they
      //were born; the see-through bodies follow them far end first, and none of
      //those keeps its depth in the buffer. That is what makes a translucent
      //body behave like glass whatever order the scripts built the world in: it
      //always finds what it looks through already painted, and it never hides a
      //body from the next stroke. (The starfield behind the glass tube of
      //games/torustrooper is what asked for this.) Two skins of the same glass
      //at one tint and one alpha blend the same picture either way, so the
      //see-through bodies sort by depth alone, never among the solids.
      SetLength(PaintList, 0);
      for Item in FItems.Values do
      begin
        if (not Item.Visible) or (not Item.Loaded) then
          Continue;
        //This pass draws its own side of the 2D world only: the under bodies
        //came through here before the 2D world, the over bodies after it.
        if ((Item.Layer < 0) <> AUnder) then
          Continue;
        //Screen-pixel anchor under the ortho eye: x runs right, y runs down
        //(so it becomes -z), and the model's Z depth rides along the world y
        //axis into the screen. Under the free eye the three are simply the
        //model's world coordinates - the same x-right/y-into/z-up space the
        //top-down pass already lives in, only with a perspective eye on it.
        if FFreeCam then
          One.Pos := Vector3Of(Item.X, Item.Y, Item.Z)
        else
        begin
          One.Pos.X := Item.X + AX - AWidth * 0.5;
          One.Pos.Y := Item.Z;                          //depth (bigger = farther)
          One.Pos.Z := AHeight * 0.5 - (Item.Y + AY);   //pixel y, screen down
        end;
        //How far down the sight line the body stands - the same measure the
        //depth test judges by, so the glass sorts the way it covers.
        One.Depth := (One.Pos.X - Cam.Position.X) * DX +
          (One.Pos.Y - Cam.Position.Y) * DY + (One.Pos.Z - Cam.Position.Z) * DZ;
        One.Item := Item;
        One.Glass := Item.Tint.RGBA.Alpha < 255;
        N := Length(PaintList);
        SetLength(PaintList, N + 1);
        PaintList[N] := One;
      end;
      //An insertion sort: the list is only the bodies on screen and it comes in
      //nearly ordered, which is this sort's best case - and it is stable, so the
      //solids keep the store's own order while the glass slides behind them,
      //the farther skin before the nearer one.
      for i := 1 to High(PaintList) do
      begin
        Hold := PaintList[i];
        j := i - 1;
        while (j >= 0) and PaintList[j].Glass and
          ((not Hold.Glass) or (PaintList[j].Depth < Hold.Depth)) do
        begin
          PaintList[j + 1] := PaintList[j];
          Dec(j);
        end;
        PaintList[j + 1] := Hold;
      end;
      for i := 0 to High(PaintList) do
      begin
        Item := PaintList[i].Item;
        SeeThrough := PaintList[i].Glass;
        Pos := PaintList[i].Pos;
        //The dress: the light skin while the lights are on, the model's own
        //the moment they are not (both calls no-op when nothing changes).
        if LitNow then WearLight(Item)
        else WearOwn(Item);
        S := Item.Scale;
        //Zero would flatten the body away to nothing, but a negative scale is
        //kept on purpose: it mirrors the mesh, and with the winding it flips
        //the facing - the trick that lets one solid wear an inside-out skin.
        if S = 0 then
          S := 1;

        //Build the transform on rlgl's matrix stack rather than DrawModelEx:
        //the by-value records that binding passes have an ABI mismatch against
        //this raylib build, while the float-only rlTranslate/Rotate/Scale calls
        //compose the exact same position + spin + bank + pivot transform.
        RayLib.rlPushMatrix;
        RayLib.rlTranslatef(Pos.X, Pos.Y, Pos.Z);
        //A faced model turns to its aim first, so the three angles that follow
        //become trims on top of the pose (their fixed axes now read against
        //the model's own frame: Angle spins it along its own line of sight).
        if Item.HasFace then
        begin
          FaceM := FaceMatrixOf(Pos, Item.FaceTo, Item.FaceUp);
          RayLib.rlMultMatrixf(@FaceM);
        end;
        //Spin around the view axis (world y), then roll around the model's own
        //axis, then pitch around the screen's horizontal axis (world x).
        //Applied after the pivot translate below, so all of them turn the
        //model about its own centre, not the world origin.
        RayLib.rlRotatef(Item.Angle, 0, 1, 0);
        RayLib.rlRotatef(Item.Roll, 0, 0, 1);
        RayLib.rlRotatef(Item.Tilt, 1, 0, 0);
        //A generated Y-spun shape stands up here: the fix goes under the
        //angles and above the pivot shift, so the model turns about its own
        //centre in whatever direction it was aimed.
        if Item.HasAxisFix then
          RayLib.rlMultMatrixf(@Item.AxisFix);
        RayLib.rlScalef(S, S, S);
        //Shift the (still unrotated) box centre onto the origin so the model
        //turns about its own middle even when its mesh sits off the origin.
        RayLib.rlTranslatef(-Item.Center.X, -Item.Center.Y, -Item.Center.Z);
        //A see-through body keeps its own depth out of the buffer: glass must
        //not decide what the world behind it looks like, or the skin hides
        //whatever the draw hands it next - the starfield behind the tube goes
        //missing instead of shining through. The depth test stays in charge the
        //other way round: a solid body in front of the glass still hides it.
        if SeeThrough then
          RayLib.rlDisableDepthMask;
        if FWire then
          //The wire pass: the model's own edges in its tint, no faces. The
          //games pair this with shader.effect="glow" for the hot-vector look.
          RayLib.DrawModelWires(Item.Model, Vector3Of(0, 0, 0), 1.0, Item.Tint)
        else
          RayLib.DrawModel(Item.Model, Vector3Of(0, 0, 0), 1.0, Item.Tint);
        if SeeThrough then
          RayLib.rlEnableDepthMask;
        RayLib.rlPopMatrix;
      end;
    finally
      RayLib.EndMode3D;
      //The batched 3D pass only lets its drawcalls fall when the mode ends,
      //so the depth test and the culling switch stay set all the way through
      //that last stroke; the 2D layers painted after this get their habits
      //back here.
      RayLib.rlEnableBackfaceCulling;
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

{ TShapeModelObject }

constructor TShapeModelObject.Create(AHandle: integer; AShape: TTyroModelShape;
  A, B, C: Single);
begin
  inherited Create;
  FHandle := AHandle;
  FShape := AShape;
  FA := A;
  FB := B;
  FC := C;
  FResult := False;
end;

procedure TShapeModelObject.DoExecute;
begin
  if Engine.Models <> nil then
    FResult := Engine.Models.BuildShape(FHandle, FShape, FA, FB, FC);
end;

{ TFreeModelObject }

constructor TFreeModelObject.Create(AHandle: integer);
begin
  inherited Create;
  FHandle := AHandle;
  FRemoved := False;
end;

procedure TFreeModelObject.DoExecute;
begin
  if Engine.Models <> nil then
    FRemoved := Engine.Models.Remove(FHandle);
end;

end.
