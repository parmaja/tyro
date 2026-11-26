unit TyroLua;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

{$ifdef FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$endif}
{$H+}{$M+}
{$define DEBUG_LUA}

interface

uses
  Classes, SysUtils, Types, Generics.Collections,
  LuaAPI, LuaClasses,
  RayLib, RayClasses, //remove it
  chipmunk, //cpVect for the joint anchors
  mnUtils, mnLogs, mnClasses, mnDON,
  TyroScripts, TyroSounds, TyroClasses, Melodies, TyroSprites, TyroTiles, TyroModels, TyroParticles, TyroPhysics, TyroParallax,
  TyroControls, TyroEngines, TyroInput,
  TyroRadio, TyroSpectrum;

type
  TLuaScript = class;

  { TLuaTimer }

  // One after()/every() timer. FnRef is the Lua registry reference of the
  // callback; Due and Interval ride the scene clock the engine feeds the
  // pump in EngineUpdate, so world.timescale stretches a timer with the
  // rest of the scene and pause() sleeps it with update().
  TLuaTimer = class
  public
    Id: Integer;
    Due: Double;     // scene-clock milliseconds it fires at
    Interval: Double; // the period in ms (every), 0 for a one-shot (after)
    Periodic: Boolean;
    FnRef: Integer;
    Dead: Boolean;
  end;

  { TLuaTween }

  // One tween() property glide. SubjRef and DoneRef are Lua registry
  // references; every frame's value goes back through the subject's
  // __newindex, so a sprite property rides the very same path a script
  // write takes. Elapsed rides the scene clock the engine feeds the pump
  // in EngineUpdate, so world.timescale stretches a tween with the rest
  // of the scene and pause() sleeps it, exactly like TLuaTimer.
  TLuaTween = class
  public
    Id: Integer;
    SubjRef: Integer;  // registry reference of the table being tweened
    Prop: string;      // field/property written back every frame
    From: Double;      // value at tween start
    Target: Double;    // value the glide lands on
    Duration: Double;  // scene-clock ms the glide takes
    Elapsed: Double;   // scene-clock ms progressed so far
    Easing: Integer;   // curve index, see TweenEase
    DoneRef: Integer;  // finish callback ref, LUA_NOREF when there is none
    Dead: Boolean;
  end;

  { TLuaHandler }

  // One on() subscription. FnRef is the Lua registry reference of the
  // callback. Kind decides whose word carries it: hkUser fires only when a
  // script emit()s the name; hkKey/hkMouse fire when the engine's beat sees
  // that key/button pressed (the on("space") binding); and the reserved
  // names "update", "draw" and "collision" are fired by the engine itself,
  // "update" every beat and "collision" from every drained physics event.
  // Names are kept lowercase: the bus does not care about case.
  TLuaHandler = class
  public
    Id: Integer;
    Name: string;    // event name, normalized lowercase
    Btn: string;     // mouse-button part of "mouse.x" names (hkMouse only)
    FnRef: Integer;
    Kind: Integer;
    Dead: Boolean;
  end;

  { TLuaScene }

  // One named scene registered through scene.add(name, opts). The four
  // callbacks are registry refs so they survive until the state is closed;
  // only the current scene's update/draw are stepped by the engine.
  TLuaScene = class
  public
    Name: string;       // lowercase key
    EnterRef: Integer;
    UpdateRef: Integer;
    DrawRef: Integer;
    LeaveRef: Integer;
  end;

  { TTyroLuaObject }

  TTyroLuaObject = class abstract(TLuaObject)
  private
    FName: string;
    FScript: TLuaScript;
    FLastHintField: string;
  protected
    procedure Created; virtual;
    function Getter(L: PLua_State): integer; override;
  public
    constructor Create(const AName: string; AScript: TLuaScript); virtual;
    procedure AfterConstruction; override;
    procedure Init; virtual;
    procedure Register(L: PLua_State); override;
    property Script: TLuaScript read FScript;
    property Name: string read FName;
  end;

  { TLuaCustomCanvas }

  TLuaCustomCanvas = class abstract(TTyroLuaObject)
  protected
    Canvas: TTyroCanvas;
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    procedure Init; override;
    function Clear_func(L: Plua_State): integer; cdecl;
    function Text_func(L: Plua_State): integer; cdecl;
    function TextCenter_func(L: Plua_State): integer; cdecl;
    //canvas.font("file.ttf" [, size [, codepage]]) -> id: load AND switch.
    function Font_func(L: Plua_State): integer; cdecl;
    function Circle_func(L: Plua_State): integer; cdecl;
    function Triangle_func(L: Plua_State): integer; cdecl;
    function Rectangle_func(L: Plua_State): integer; cdecl;
    function Image_func(L: Plua_State): integer; cdecl;
    function GradientH_func(L: Plua_State): integer; cdecl;
    function GradientV_func(L: Plua_State): integer; cdecl;
    function GradientCircle_func(L: Plua_State): integer; cdecl;
    function Line_func(L: Plua_State): integer; cdecl;
    function Point_func(L: Plua_State): integer; cdecl;
    function Test_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaCanvas }

  TLuaCanvas = class(TLuaCustomCanvas)
  public
    constructor Create(const AName: string; AScript: TLuaScript); override;
  end;

  { TLuaScreen }

  TLuaScreen = class(TLuaCustomCanvas)
  public
    constructor Create(const AName: string; AScript: TLuaScript); override;
  end;

  { TLuaImage }

  //A paint sheet: the canvas draw API pointed at a render texture that
  //images.new made, so a script can paint a sprite without a file. The engine
  //owns the texture (Engine.Images); this object only points at it. Methods
  //are dot-called like the canvas globals: sheet.rectangle(4, 4, 8, 6, true).
  TLuaImage = class(TLuaCustomCanvas)
  private
    function GetSheet: TTyroCanvas;
  public
    constructor Create(const AName: string; AScript: TLuaScript;
      ACanvas: TTyroCanvas); overload;
    //Builds the sheet's Lua table and leaves it on the stack: the inherited
    //canvas draw methods bound to this very sheet (RegisterMethod carries
    //Self), plus the property accessors from TLuaCustomCanvas. The __sheet and
    //__name keys are written raw before the metatable goes on, because the
    //sheet's own __newindex would otherwise swallow them (like RegisterSprite).
    function RegisterImage(AIndex: Integer; const AName: string): Integer;
    property Sheet: TTyroCanvas read GetSheet;
  end;

  { TLuaImages }

  //The global `images` table. It owns every sheet handed out by images.new();
  //the sheets are pure memory (their render textures live in Engine.Images),
  //so freeing them with the script is safe on any thread.
  TLuaImages = class(TTyroLuaObject)
  private
    FItems: TList; //of TLuaImage, freed in Destroy
    function GetItem(AIndex: Integer): TLuaImage;
  public
    constructor Create(const AName: string; AScript: TLuaScript); override;
    destructor Destroy; override;
    function New_func(L: Plua_State): integer; cdecl;
    function Save_func(L: Plua_State): integer; cdecl;
    function SaveSprite_func(L: Plua_State): integer; cdecl;
    //The paint sheet behind a table argument (a sheet made by images.new),
    //or nil when the argument is anything else.
    function SheetArg(L: Plua_State; AIndex: Integer): TLuaImage;
    property Items[AIndex: Integer]: TLuaImage read GetItem;
  end;

  { TLuaShader }

  TLuaShader = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Load_func(L: PLua_State): integer; cdecl;
  end;

  { TLuaConsole }

  TLuaConsole = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Print_func(L: Plua_State): integer; cdecl;
    function PrintLn_func(L: Plua_State): integer; cdecl;
    function Show_func(L: Plua_State): integer; cdecl;
    function Read_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaOutput }

  TLuaOutput = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    function Clear_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaWindow }

  TLuaWindow = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
  published
    function Show_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaWorld }

  TLuaWorld = class(TLuaCustomCanvas)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    constructor Create(const AName: string; AScript: TLuaScript); override;
    procedure Init; override;
    function Shake_func(L: Plua_State): integer; cdecl;
    //world.light([dx,dy,dz]) -> the sun ray the model shadows are projected
    //along (x right, y into the screen, z up); the y part must be non-zero.
    function Light_func(L: Plua_State): integer; cdecl;
    //world.ambient([v]) -> how much sky light fills the shadows (0..1).
    function Ambient_func(L: Plua_State): integer; cdecl;
    //world.ground([depth]) -> the model.z plane the shadows land on.
    function Ground_func(L: Plua_State): integer; cdecl;
    //world.shadowsoft([px]) -> the penumbra radius the shadows are blurred by
    //(screen pixels, 0..6); 0 keeps their edge hard.
    function ShadowSoft_func(L: Plua_State): integer; cdecl;
    //world.camera([x,y,z [, tx,ty,tz [, upx,upy,upz]]]) -> the free 3D eye.
    //Setting it hands the model pass from the top-down ortho camera to this
    //perspective one, and the models' x/y/z then read as world coordinates.
    function Camera_func(L: Plua_State): integer; cdecl;
    //world.ortho() -> back to the default top-down pixel camera.
    function Ortho_func(L: Plua_State): integer; cdecl;
    //world.fov([deg]) -> the free camera's vertical field of view.
    function Fov_func(L: Plua_State): integer; cdecl;
    //world.wire([on]) -> paint the models as their edges only (the vector look).
    function Wire_func(L: Plua_State): integer; cdecl;
    //world.lights([on]) -> dress the solids in light: sky fill, sun and bulbs.
    function Lights_func(L: Plua_State): integer; cdecl;
    //world.pointlight(i, x, y, z [, color [, range]]) -> one of four bulbs.
    function PointLight_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaCamera }

  // The 2D world camera, the Lua `camera` table. camera.x/camera.y is the
  // world point parked on the window's top-left corner (0 = the classic
  // world origin), and camera.zoom scales the picture around that eye point
  //(clamped 0.05..32; 1 keeps the pixel-perfect mapping). The whole world
  //pass rides it - the board, parallax, sprites, models and sparks - while
  //the screen canvas, the controls and the console stay glued to the window
  //like the HUD they carry.
  TLuaCamera = class(TTyroLuaObject)
  protected
    function Getter(L: PLua_State): integer; override;
    function Setter(L: PLua_State): integer; override;
  public
    //camera.to_world(sx, sy) -> wx, wy: a window pixel (mouse.x, mouse.y)
    //back into the world the camera looks at.
    function ToWorld_func(L: Plua_State): integer; cdecl;
    //camera.to_screen(wx, wy) -> sx, sy: a world point onto the window.
    function ToScreen_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaFont }

  // font.load(name[, size[, codepage]]) -> id   (cached; see help/fonts.md)
  // font.use(id) / font.free(id) / font.current()
  // font.height is the height of the font in use.
  TLuaFont = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Load_func(L: Plua_State): integer; cdecl;
    function Use_func(L: Plua_State): integer; cdecl;
    function Free_func(L: Plua_State): integer; cdecl;
    function Current_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaColors }

  TLuaColors = class(TTyroLuaObject)
  protected
  type
    TLuaColor = record
      Name: string;
      Color: TColor;
    end;

  var
    Colors: array of TLuaColor;
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;

    procedure Created; override;
  public
    procedure AddColor(AName: string; AColor: TColor);
  end;

  { TLuaMusic }

  TLuaMusic = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Beep_func(L: Plua_State): integer; cdecl;
    function Sound_func(L: Plua_State): integer; cdecl;
    function Play_func(L: Plua_State): integer; cdecl;
    function MML_func(L: Plua_State): integer; cdecl;
    //music.stop(): silence all playing melodies, music.mml(..., true) loops
    //included.
    function Stop_func(L: Plua_State): integer; cdecl;
  end;


  { TLuaSound }

  TLuaSound = class(TTyroLuaObject)
  protected
    //sound.muted = true/false - the engine-wide silence switch (the SoundMuted
    //global in TyroSounds); music.play/music.mml/music.sound and sound.* all
    //drop their start request quietly while it is true.
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Beep_func(L: Plua_State): integer; cdecl; deprecated; //TODO move to Sound
    //Cache the file to play any time, faster
    function Load_func(L: Plua_State): integer; cdecl;
    function Unload_func(L: Plua_State): integer; cdecl;
    //sound.play("file.wav" [, volume])
    function Play_func(L: Plua_State): integer; cdecl;
    //sound.noise("waveformname" [, freq[, ms]])
    function Noise_func(L: Plua_State): integer; cdecl;
    //sound.loopPlay()/pitch()/volume()/stop()
    function LoopPlay_func(L: Plua_State): integer; cdecl;
    function LoopPitch_func(L: Plua_State): integer; cdecl;
    function LoopVolume_func(L: Plua_State): integer; cdecl;
    function LoopStop_func(L: Plua_State): integer; cdecl;
    //sound.generate(file, freq, ms [, wave [, volume]]) -> true | nil, err
    function Generate_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaRadio }

  TLuaRadio = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Play_func(L: Plua_State): integer; cdecl;
    function Pause_func(L: Plua_State): integer; cdecl;
    function Resume_func(L: Plua_State): integer; cdecl;
    function Stop_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaSpectrum }

  TLuaSpectrum = class(TTyroLuaObject)
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
  end;

  { TRadioAction }

  TRadioAction = (raPlay, raPause, raResume, raStop);

  { Routes radio.play/pause/resume/stop into the main window thread (the same
    one that drives RayUpdates.Update and the raylib audio device). }
  TRadioPlayObject = class(TQueueObject)
  private
    FAction: TRadioAction;
    FURL: string;
  public
    constructor Create(AAction: TRadioAction); overload;
    constructor Create(AAction: TRadioAction; const AURL: string); overload;
    procedure DoExecute; override;
  end;

  { TShowSpectrumObject }

  { Shows the spectrum analyzer panel on the main window thread (the panel is
    part of the control tree painted by the main drawing cycle). }
  TShowSpectrumObject = class(TQueueObject)
  private
    FX, FY, FW, FH: Integer;
  public
    constructor Create(AX, AY, AW, AH: Integer);
    procedure DoExecute; override;
  end;

  { THideSpectrumObject }

  THideSpectrumObject = class(TQueueObject)
  public
    procedure DoExecute; override;
  end;

  { TLuaSprite }

  TLuaSprite = class(TTyroLuaObject)
  private
  protected
    function Getter(L: Plua_State): integer; override;
    function Setter(L: Plua_State): integer; override;
  public
    function RegisterSprite(AHandle: Integer): Integer;
    function PushExisting(L: Plua_State; AHandle: Integer): Integer;
    procedure InitSpriteTable(L: Plua_State; AIdx: Integer; AHandle: Integer; const AName: string = '');
    procedure PushClass(L: Plua_State);
    function Construct_func(L: Plua_State): integer; cdecl;
    function Load_func(L: Plua_State): integer; cdecl;
    function FromImage_func(L: Plua_State): integer; cdecl;
    function LoadScript_func(L: Plua_State): integer; cdecl;
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    function Move_func(L: Plua_State): integer; cdecl;
    function Width_func(L: Plua_State): integer; cdecl;
    function Height_func(L: Plua_State): integer; cdecl;
    function Play_func(L: Plua_State): integer; cdecl;
    function Stop_func(L: Plua_State): integer; cdecl;
    function Action_func(L: Plua_State): integer; cdecl;
    function FrameCount_func(L: Plua_State): integer; cdecl;
    function Free_func(L: Plua_State): integer; cdecl;
  end;

  TLuaSprites = class(TTyroLuaObject)
  protected
  public
    //sprites
    function New_func(L: Plua_State): integer; cdecl;
    function PuffOrBlob(L: Plua_State; AHard: boolean): integer; cdecl;
    function Puff_func(L: Plua_State): integer; cdecl;
    function Blob_func(L: Plua_State): integer; cdecl;
    function Find_func(L: Plua_State): integer; cdecl;
    function Call_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaTile }

  { One tilemap object handed to the script by tiles.new(). Mirrors TLuaSprite:
    methods are table fields that receive the map table as argument 1, properties
    resolve through the metatable getter/setter (map.cols, map.x, ...). }
  TLuaTile = class(TTyroLuaObject)
  private
  protected
    function Getter(L: Plua_State): integer; override;
    function Setter(L: Plua_State): integer; override;
  public
    function RegisterTile(AHandle: Integer): Integer;
    //map:load(file [, tilew [, tileh [, columns [, spacing [, margin]]]]])
    function Load_func(L: Plua_State): integer; cdecl;
    //map:resize(cols, rows)
    function Resize_func(L: Plua_State): integer; cdecl;
    //map:fill(tile) / map:clear()
    function Fill_func(L: Plua_State): integer; cdecl;
    function Clear_func(L: Plua_State): integer; cdecl;
    //map:set(col, row, tile) / map:get(col, row)
    function Set_func(L: Plua_State): integer; cdecl;
    function Get_func(L: Plua_State): integer; cdecl;
    //map:data() -> flat array of tile ids (row-major)
    function Data_func(L: Plua_State): integer; cdecl;
    //map:solidtile(tile [, solid])
    function SolidTile_func(L: Plua_State): integer; cdecl;
    //map:solid(col, row) -> true when the cell is outside the map or a solid tile
    function Solid_func(L: Plua_State): integer; cdecl;
    //map:draw([x, y [, scale]]) -> enqueue one native draw command
    function Draw_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaTiles }

  TLuaTiles = class(TTyroLuaObject)
  protected
  public
    //tiles.new([name]) -> a new map object
    function New_func(L: Plua_State): integer; cdecl;
    //tiles.find(name) -> the named map object, or nil
    function Find_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaModel }

  { One 3D model object handed to the script by models.new(). Mirrors TLuaTile:
    methods are table fields that receive the model table as argument 1,
    properties resolve through the metatable getter/setter (model.x, model.angle,
    ...). The engine draws every visible model itself in the per-frame 3D pass,
    so there is no draw() here - like a sprite, not like a tilemap. }
  TLuaModel = class(TTyroLuaObject)
  private
  protected
    function Getter(L: Plua_State): integer; override;
    function Setter(L: Plua_State): integer; override;
  public
    function RegisterModel(AHandle: Integer): Integer;
    function PushExisting(L: Plua_State; AHandle: Integer): Integer;
    procedure InitModelTable(L: Plua_State; AIdx: Integer; AHandle: Integer; const AName: string = '');
    procedure PushClass(L: Plua_State);
    function Construct_func(L: Plua_State): integer; cdecl;
    //model:load(file) -> load (or replace) the mesh from a GLB/glTF/OBJ file
    function Load_func(L: Plua_State): integer; cdecl;
    //model:move(x, y [, z])
    function Move_func(L: Plua_State): integer; cdecl;
    //model:show() / model:hide()
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    //model:play([what]) -> play a skeletal animation by index or name.
    function Play_func(L: Plua_State): integer; cdecl;
    //model:stop() -> stop the skeletal animation, keeping the current pose.
    function Stop_func(L: Plua_State): integer; cdecl;
    //model:face(x,y,z [, upx,upy,upz]) -> aim the model's own +y at a world
    //point and lean its +z toward an up direction; no args clears the aim.
    function Face_func(L: Plua_State): integer; cdecl;
    //model:free() (alias destroy) -> unload the model now and drop it from
    //the draw pass; the table stops addressing anything afterwards.
    function Free_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaModels }

  TLuaModels = class(TTyroLuaObject)
  protected
    // Shared body of the shape builders (sphere/box/cylinder/cone/torus):
    // the optional first argument is the name (a number in that slot means
    // no name, like controls.new), the ones after it the size numbers, each
    // falling back to its unit default. The mesh build uploads to the GPU,
    // so - like load() - it runs on the main thread through the engine
    // queue. Leaves the model table on the stack and returns it.
    function BuildShapeModel(L: Plua_State; AShape: TTyroModelShape;
      ADef, BDef, CDef: Single): integer;
  public
    //models.new([name]) -> a new model object
    function New_func(L: Plua_State): integer; cdecl;
    //models.sphere([name]) -> a model built from the builtin unit sphere mesh
    function Sphere_func(L: Plua_State): integer; cdecl;
    //models.box([name] [, w, h, d]) -> a box of those sizes (unit by default)
    function Box_func(L: Plua_State): integer; cdecl;
    //models.cylinder([name] [, radius] [, height]) -> a drum standing on z
    function Cylinder_func(L: Plua_State): integer; cdecl;
    //models.cone([name] [, radius] [, height]) -> a cone standing on z
    function Cone_func(L: Plua_State): integer; cdecl;
    //models.torus([name] [, radius] [, tube]) -> a ring lying in x/y around z
    function Torus_func(L: Plua_State): integer; cdecl;
    //models.find(name) -> the named model object, or nil
    function Find_func(L: Plua_State): integer; cdecl;
    //models(name) -> __call metamethod, same as models.find
    function Call_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaParticles }

  { The global 'particles' table: throw a burst of sparks and let the engine
    step and draw it (see TyroParticles). }
  TLuaParticles = class(TTyroLuaObject)
  protected
    function Getter(L: PLua_State): integer; override;
  public
    //particles.emit(x, y [, count] [, options]) - throws a burst of sparks
    function Emit_func(L: Plua_State): integer; cdecl;
    //particles.clear() - drops every live spark
    function Clear_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaParallax }

  { One scrolling parallax background handed to the script by parallax.new().
    The methods are table fields that receive the parallax table as argument 1,
    the properties resolve through the metatable getter/setter (bg.offset,
    bg.speed, bg.visible, ...). }
  TLuaParallax = class(TTyroLuaObject)
  protected
    function Getter(L: Plua_State): integer; override;
    function Setter(L: Plua_State): integer; override;
    //Common body of add/addground (not part of the Lua surface).
    function DoAdd(L: Plua_State; AAnchor: TParallaxAnchor): integer;
  public
    function RegisterParallax(AHandle: Integer): Integer;
    //bg:add(file, speed [, y [, scale]]) - a layer anchored to the top edge
    function Add_func(L: Plua_State): integer; cdecl;
    //bg:addground(file, speed [, y [, scale]]) - a layer anchored to the floor
    function AddGround_func(L: Plua_State): integer; cdecl;
    //bg:clear() - drop every layer and unload its texture
    function Clear_func(L: Plua_State): integer; cdecl;
    //bg:reset() - put the scroll offset back to 0
    function Reset_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaParallaxes }

  TLuaParallaxes = class(TTyroLuaObject)
  protected
  public
    //parallax.new([name]) -> a new parallax background object
    function New_func(L: Plua_State): integer; cdecl;
    //parallax.find(name) -> the named parallax object, or nil
    function Find_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaControls }

  { Generic control creation and inspection. controls.new('button', ...)
    creates a control by class name ('button', 'panel', 'label', 'checkbox',
    'edit', 'spectrum', 'listbox', 'image'); the returned handle addresses a
    control owned by the main window. The same object backs both the
    'controls' table and the legacy 'buttons' alias table, so old scripts keep
    working. }
  TLuaControls = class(TTyroLuaObject)
  private
    FItems: TList; //of TTyroControl (owned by the main window, not by us)
    function GetControl(AHandle: Integer): TTyroControl;
    function FindByName(const AName: string): Integer;
  protected
    function Setter(L: PLua_State): integer; override;
    function Getter(L: PLua_State): integer; override;
  public
    function RegisterControl(AHandle: Integer): Integer;
    function New_func(L: Plua_State): integer; cdecl;
    function Caption_func(L: Plua_State): integer; cdecl;
    function Checked_func(L: Plua_State): integer; cdecl;
    function Position_func(L: Plua_State): integer; cdecl;
    function Move_func(L: Plua_State): integer; cdecl;
    function Width_func(L: Plua_State): integer; cdecl;
    function Height_func(L: Plua_State): integer; cdecl;
    function Visible_func(L: Plua_State): integer; cdecl;
    function Show_func(L: Plua_State): integer; cdecl;
    function Hide_func(L: Plua_State): integer; cdecl;
    function Hover_func(L: Plua_State): integer; cdecl;
    function Down_func(L: Plua_State): integer; cdecl;
    function Clicked_func(L: Plua_State): integer; cdecl;
    function Focused_func(L: Plua_State): integer; cdecl;
    function Focus_func(L: Plua_State): integer; cdecl;
    function Border_func(L: Plua_State): integer; cdecl;
    function BackColor_func(L: Plua_State): integer; cdecl;
    function Name_func(L: Plua_State): integer; cdecl;
    function Align_func(L: Plua_State): integer; cdecl;
    function Parent_func(L: Plua_State): integer; cdecl;
    function Items_func(L: Plua_State): integer; cdecl;
    function Item_func(L: Plua_State): integer; cdecl;
    function AddItem_func(L: Plua_State): integer; cdecl;
    function Clear_func(L: Plua_State): integer; cdecl;
    function ViewCount_func(L: Plua_State): integer; cdecl;
    function ItemIndex_func(L: Plua_State): integer; cdecl;
    //text: btn.text = "Hi" / controls.text(handle [, "Hi"]); a caption control
    //keeps it in Caption, an edit in the shared Text
    function Text_func(L: Plua_State): integer; cdecl;
    //image control: img.load("logo.png") / controls.load(handle, "logo.png"),
    //the new state answers the call and reads back through the loaded field
    function LoadImage_func(L: Plua_State): integer; cdecl;
    constructor Create(const AName: string; AScript: TLuaScript); override;
    destructor Destroy; override;
  end;

{ TLuaSpriteScript }

  // One Lua state per sprite (a TLua recording). Routed through handler
  // globals that the main thread polls: on_update(), on_draw(), on_collide(other, state).
  // When attached, the file is compiled and Run()ed once, so its top-level code
  // runs immediately; the handlers themselves are plain functions we look up each tick.
  TLuaSpriteScript = class(TInterfacedObject, ISpriteScript)
  private
    FScript: TLuaScript;
    FHandle: Integer;
    FLua: TLua;
    FFileName: string;
    FSprite: TLuaSprite; // proxy object whose getter/setter back the self/other tables
    procedure DoError(S: string; const AHandler: string);
    procedure BuildSelf; // registers the self global table (properties read/write main sprite)
    procedure BuildGlobals; // global helpers shared from the main script state (time, rand, println)
    procedure BuildColors; // registers the colors global table
    procedure BuildDraw; // registers the draw global table
    procedure CallHandler(const AName: utf8string; AArgs: Integer); // pcall a global handler (args already on stack), ignore if not a function
  public
    constructor Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
    destructor Destroy; override;
    function Load: Boolean; // load file and run top-level code once
    procedure Update; // dispatch on_update()
    procedure Draw;   // dispatch on_draw()
    procedure OnCollide(AOtherHandle: Integer; const AState: string); // dispatch on_collide(other, state)
    // draw.* helpers for on_draw; safe only while a frame is active (main thread)
    function Circle_func(L: Plua_State): integer; cdecl;
    function Rectangle_func(L: Plua_State): integer; cdecl;
    function Line_func(L: Plua_State): integer; cdecl;
    function Text_func(L: Plua_State): integer; cdecl;
    property FileName: string read FFileName;
  end;

  { Loads a per-sprite script on the main thread (engine canvas queue), then
    attaches the ISpriteScript to the sprite. }
  TLoadSpriteScriptObject = class(TQueueObject)
  private
    FScript: TLuaScript;
    FHandle: Integer;
    FFileName: string;
  public
    constructor Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
    procedure DoExecute; override;
  end;

  { TLuaFile }

  { A file handle handed to the script by openfile(). The object owns the
    stream; the Lua table registered for it keeps closures pointing at this
    instance, so the owning script frees every handle after closing its state.
    Binary modes ('b' in the mode string) read and write raw bytes, the
    remaining modes add the newline translation io does. }
  TLuaFile = class(TTyroLuaObject)
  private
    FFileName: string;
    FMode: string;
    //All reads and writes go through this one stream; text mode only adds
    //newline translation (see ReadTextFormat / Write_func)
    FStream: TFileStream;
    FIsText: Boolean;
    FIsOpen: Boolean;
    FError: string;
    //A pending f:lines() iterator; TStringList holds the lines it still owes
    FLines: TStringList;
    //How many of those lines LinesNext_func has handed out already. The position
    //lives here rather than in the loop control variable because Pluto's generic
    //for does not pass the value the iterator returned back to the next call.
    FLinesPos: Integer;
    function ModeFlag(const AFlag: Char): Boolean;
    //Reads one token ('n') or one line ('l'/'L') and pushes exactly one value
    function ReadTextFormat(L: PLua_State; const AFormat: UTF8Char): Boolean;
    //Index of the first value the script passed; see the implementation
    function FirstArgIndex(L: PLua_State): Integer;
  public
    destructor Destroy; override;

    //Opens the (already validated) path; returns False and sets Error on failure
    function Open(const AFileName, AMode: string): Boolean;
    function Read_func(L: Plua_State): integer; cdecl;
    function Write_func(L: Plua_State): integer; cdecl;
    function Close_func(L: Plua_State): integer; cdecl;
    function Lines_func(L: Plua_State): integer; cdecl;
    //Iterates the lines that Lines_func has already queued, so a plain
    //"for line in f:lines()" loop works. Returns nil once the list is drained.
    function LinesNext_func(L: Plua_State): integer; cdecl;
    property IsOpen: Boolean read FIsOpen;
    property Error: string read FError;
  end;

{ TLuaCollision }

  TLuaCollision = class(TTyroLuaObject)
  private
    procedure FireEvent(L: Plua_State; AHandle, AOtherHandle: Integer; const AState: string);
  protected
    function Getter(L: Plua_State): integer; override;
    function Setter(L: Plua_State): integer; override;
  public
    // collision.pump() drains the queue and fires sprite.onCollide(other, state)
    function Pump_func(L: Plua_State): integer; cdecl;
    //The drain on the engine's beat: when the script listens with
    //on("collision", fn), the frame pump calls this itself, so every event
    //also reaches the bus as (spriteA, spriteB, state) besides the two
    //per-sprite onCollide handlers. Returns the number of events drained.
    function PumpEvents: Integer;
    //The stretch beat: drains the taut/slack edges the slide joints crossed
    //in the last Step and forwards them to on("joint_stretch") as
    //(spriteA, spriteB, "taut"/"slack"). Drained even when nobody listens,
    //so the physics buffer never fills; returns the number dispatched.
    function PumpStretch: Integer;
    // The joint family - see help/joints.md. joint(a, b, opts) binds two
    // sprites with a pin/slide/rotator/limit; joint_free(id) drops one,
    // joints() counts the book and joints_clear() empties it.
    function Joint_func(L: Plua_State): integer; cdecl;
    function JointFree_func(L: Plua_State): integer; cdecl;
    function Joints_func(L: Plua_State): integer; cdecl;
    function JointsClear_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaMouse }

  // mouse.x / mouse.y / mouse.wheel are read-only fields; mouse.pressed(button)
  // is the press edge, mouse.down(button) is held, mouse.released(button) is
  // the release edge and mouse.up(button) is not-held
  TLuaMouse = class(TTyroLuaObject)
  protected
    function Getter(L: Plua_State): integer; override;
  public
    function Pressed_func(L: Plua_State): integer; cdecl;
    function Down_func(L: Plua_State): integer; cdecl;
    function Released_func(L: Plua_State): integer; cdecl;
    function Up_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaTouch }

  // touch.x / touch.y / touch.count / touch.down / touch.up / touch.pressed /
  // touch.released read the frame the engine stepped this beat; touch.point(i)
  // walks the fingers. On a desktop the mouse stands in for a finger, so one
  // script runs on both a window and a tablet.
  TLuaTouch = class(TTyroLuaObject)
  protected
    function Getter(L: Plua_State): integer; override;
  public
    function Point_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaQueue }

  // queue.depth / queue.holds / ... : a read-only window on the engine's
  // drawing queue counters (TQueueStats in TyroEngines), so a script can wait
  // for its queued commands to run: while queue.depth > 0 do sleep(1) end.
  TLuaQueue = class(TTyroLuaObject)
  protected
    function Getter(L: Plua_State): integer; override;
  end;

  { TLuaKeyboard }

  // keyboard.pressed(key) is the press edge, keyboard.down(key) is held,
  // keyboard.released(key) is the release edge and keyboard.up(key) is not-held
  TLuaKeyboard = class(TTyroLuaObject)
  public
    function Pressed_func(L: Plua_State): integer; cdecl;
    function Down_func(L: Plua_State): integer; cdecl;
    function Released_func(L: Plua_State): integer; cdecl;
    function Up_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaGamepad }

  TLuaGamepad = class(TTyroLuaObject)
  public
    function Pressed_func(L: Plua_State): integer; cdecl;
    function Down_func(L: Plua_State): integer; cdecl;
    function Released_func(L: Plua_State): integer; cdecl;
    function Up_func(L: Plua_State): integer; cdecl;
    function Axis_func(L: Plua_State): integer; cdecl;
    function Available_func(L: Plua_State): integer; cdecl;
    function Name_func(L: Plua_State): integer; cdecl;
    function Count_func(L: Plua_State): integer; cdecl;
    function Axes_func(L: Plua_State): integer; cdecl;
    function Vibrate_func(L: Plua_State): integer; cdecl;
    function Mappings_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaGame }

  //Global `game` table: the shared gameplay helpers every kid's game repeats.
  //Every function is a thin binding on the matching TTyroEngine method (see
  //TyroEngines.pas), so the logic lives in the engine and this class only
  //translates to and from Lua.
  TLuaGame = class(TTyroLuaObject)
  protected
    function Getter(L: PLua_State): integer; override;
    function Setter(L: PLua_State): integer; override;
  public
    //game.paused / game.level are properties; the calls below are the verbs.
    function TogglePause_func(L: Plua_State): integer; cdecl;
    function PauseGame_func(L: Plua_State): integer; cdecl;
    function ResumeGame_func(L: Plua_State): integer; cdecl;
    function Restart_func(L: Plua_State): integer; cdecl;
    function Difficulty_func(L: Plua_State): integer; cdecl;
    function PausePressed_func(L: Plua_State): integer; cdecl;
    function PauseDown_func(L: Plua_State): integer; cdecl;
    function StartPressed_func(L: Plua_State): integer; cdecl;
    function FirePressed_func(L: Plua_State): integer; cdecl;
    function FireDown_func(L: Plua_State): integer; cdecl;
    function APressed_func(L: Plua_State): integer; cdecl;
    function ADown_func(L: Plua_State): integer; cdecl;
    function BPressed_func(L: Plua_State): integer; cdecl;
    function BDown_func(L: Plua_State): integer; cdecl;
    function XPressed_func(L: Plua_State): integer; cdecl;
    function XDown_func(L: Plua_State): integer; cdecl;
    function YPressed_func(L: Plua_State): integer; cdecl;
    function YDown_func(L: Plua_State): integer; cdecl;
    function LPressed_func(L: Plua_State): integer; cdecl;
    function LDown_func(L: Plua_State): integer; cdecl;
    function RPressed_func(L: Plua_State): integer; cdecl;
    function RDown_func(L: Plua_State): integer; cdecl;
    function SelectPressed_func(L: Plua_State): integer; cdecl;
    function SelectDown_func(L: Plua_State): integer; cdecl;
    function StartDown_func(L: Plua_State): integer; cdecl;
    function Clamp_func(L: Plua_State): integer; cdecl;
    function Lerp_func(L: Plua_State): integer; cdecl;
    function Sign_func(L: Plua_State): integer; cdecl;
    function Dist_func(L: Plua_State): integer; cdecl;
    function Aabb_func(L: Plua_State): integer; cdecl;
    //rng(seed) -> a callable table; see RngCall_func for the call shape.
    function Rng_func(L: Plua_State): integer; cdecl;
    //__call metamethod of the tables Rng_func hands out.
    function RngCall_func(L: Plua_State): integer; cdecl;
    function HiLoad_func(L: Plua_State): integer; cdecl;
    function HiSave_func(L: Plua_State): integer; cdecl;
  end;

  { TLuaScript }

  TLuaScript = class(TTyroScript)
  private
  protected
    Lua: TLua;

    Canvas: TLuaCanvas; //Board
    World: TLuaWorld;
    Screen: TLuaScreen; //Screen
    Console: TLuaConsole;
    Window: TLuaWindow;
    Colors: TLuaColors;
    Font: TLuaFont;
    Music: TLuaMusic;
    Sound: TLuaSound;
    Radio: TLuaRadio;
    Sprite: TLuaSprite;
    Sprites: TLuaSprites;
    //Global `images` table; owns every paint sheet handed out by images.new()
    Images: TLuaImages;
    Tile: TLuaTile;
    Tiles: TLuaTiles;
    Model: TLuaModel;
    Models: TLuaModels;
    Particles: TLuaParticles;
    Parallax: TLuaParallax;
    Parallaxes: TLuaParallaxes;
    Controls: TLuaControls;
    Output: TLuaOutput;
    Collision: TLuaCollision;
    Shader: TLuaShader;
    Spectrum: TLuaSpectrum;
    Mouse: TLuaMouse;
    Touch: TLuaTouch;
    Keyboard: TLuaKeyboard;
    Gamepad: TLuaGamepad;
    Game: TLuaGame;
    QueueStats: TLuaQueue;
    Camera: TLuaCamera;
    //Owns every TLuaFile handed out by openfile(); freed after Lua.Close,
    //because the Lua closures hold pointers into those objects
    Files: TList;
    //Scene timers handed out by after()/every() (TLuaTimer), swept inside
    //the engine-driven frame. The registry refs die with the Lua state, so
    //Destroy releases the list only.
    Timers: TList; //of TLuaTimer
    //The scene clock in milliseconds: only the stretched engine steps
    //advance it, so timers sleep through pause() and crawl with the scene.
    TimerClock: Double;
    TimerNextId: Integer;
    //Tween glides handed out by tween()/tween.from() (TLuaTween), swept in
    //the same engine-driven beat as the timers; each tween carries its own
    //elapsed time, so no shared clock is needed. The registry refs die with
    //the Lua state, so Destroy releases the list only.
    Tweens: TList; //of TLuaTween
    TweenNextId: Integer;
    //Event bus handed out by on()/off()/emit() (TLuaHandler). User events
    //run when the script emits them; the engine fires the reserved names:
    //key/mouse bindings and "update" ride the frame beat, "collision" comes
    //from the drained physics queue. Refs die with the state, so Destroy
    //releases the list only.
    Handlers: TList; //of TLuaHandler
    HandlerNextId: Integer;
    //scene.add/go/list: named menu/play/over states so games stop hand-rolling
    //their own state machines. Refs die with the Lua state, like timers.
    Scenes: TList; //of TLuaScene
    CurrentScene: string;
    function NewTimer(L: Plua_State; AMs: Double; APeriodic: Boolean): Integer;
    procedure PumpTimers(ADeltaTime: Double);
    function HasTimers: Boolean;
    procedure PumpTweens(ADeltaTime: Double);
    function HasTweens: Boolean;
    //Assembles the callable `tween` table (tween(...), tween.to/from/kill/
    //killall/count) on the current state from the __tween_* helpers and
    //hides the raw names again. Runs right after they are registered.
    procedure TweenPrelude;
    function HasHandlers: Boolean;
    function HandlerCountNamed(const AName: string): Integer;
    function FindHandler(AId: Integer): TLuaHandler;
    procedure SweepHandlers;
    //Runs every live handler of AName with the ACount values sitting on the
    //stack from AFirst up (they stay put: each handler gets its own copy).
    //Returns how many ran; errors land in DoError with the event's name.
    function Dispatch(const AName: string; L: Plua_State; AFirst, ACount: Integer): Integer;
    //Fires the hkKey/hkMouse bindings whose key/button was pressed this frame.
    procedure PumpInputs;
    procedure DoError(S: string);
    procedure Run; override;
    //Look up the global AName and pcall it on the main thread. AArgs arguments
    //must already be on the stack (they are consumed); an absent or non-function
    //global just drains the stack and is ignored. Errors are reported via DoError.
    procedure CallGlobal(const AName: string; AArgs: Integer);
    //pcall a callback held by a Lua registry ref. AFirst..AFirst+ACount-1
    //are the arguments already on the stack; they are consumed by the call.
    procedure CallRef(ARef: Integer; AFirst, ACount: Integer);
    function FindScene(const AName: string): TLuaScene;
    procedure ReleaseScene(AScene: TLuaScene);
  protected

    //input & timing

    function FrameTime_func(L: Plua_State): integer; cdecl;
    function TotalTime_func(L: Plua_State): integer; cdecl;
    function Millis_func(L: Plua_State): integer; cdecl;
    function RandomValue_func(L: Plua_State): integer; cdecl;
    function Screenshot_func(L: Plua_State): integer; cdecl;
    function Exit_func(L: Plua_State): integer; cdecl;
    function SceneAdd_func(L: Plua_State): integer; cdecl;
    function SceneGo_func(L: Plua_State): integer; cdecl;
    function SceneCurrent_func(L: Plua_State): integer; cdecl;
    function SceneList_func(L: Plua_State): integer; cdecl;
    function SceneRemove_func(L: Plua_State): integer; cdecl;
    function SceneClear_func(L: Plua_State): integer; cdecl;
    function Clock_func(L: Plua_State): integer; cdecl;
    function Time_func(L: Plua_State): integer; cdecl;
    //Text metrics for the built-in font, e.g. to centre a label:
    //local w = textwidth('GAME OVER')
    function TextWidth_func(L: Plua_State): integer; cdecl;
    function TextHeight_func(L: Plua_State): integer; cdecl;
    function Rgba_func(L: Plua_State): integer; cdecl;

    //scene timers
    //after(ms, fn) / every(ms, fn) -> a timer id, cancel(id) -> true when a
    //timer was stopped. The callbacks run inside the engine-driven frame,
    //the same beat that calls update(); while the top-level chunk is still
    //running there is no such beat, so nothing fires before it returns.
    function After_func(L: Plua_State): integer; cdecl;
    function Every_func(L: Plua_State): integer; cdecl;
    function Cancel_func(L: Plua_State): integer; cdecl;

    //juice tweens
    //tween(obj, prop, value, ms [, easing [, done]]) -> tween id, the same
    //kind of id: tween.kill(id) stops a glide, tween.killall(obj) stops all
    //of one subject's. tween.from() spells the start value out instead of
    //reading it. The glides step inside the engine-driven frame, the beat
    //that calls update(), on the scene clock.
    function Tween_func(L: Plua_State): integer; cdecl;
    function TweenFrom_func(L: Plua_State): integer; cdecl;
    function TweenKill_func(L: Plua_State): integer; cdecl;
    function TweenKillAll_func(L: Plua_State): integer; cdecl;
    function TweenCount_func(L: Plua_State): integer; cdecl;
    //Shared body of the two arming calls; AHasFrom picks tween.from's
    //spelling (obj, prop, from, to, ms ...) over tween()'s (obj, prop,
    //to, ms ...). Returns the new id, or -1 with (nil, reason) pushed.
    function PushTween(L: Plua_State; AHasFrom: Boolean): Integer;

    //event bus
    //on(name, fn) -> handler id; off(id) -> true when a live handler was
    //unhooked; emit(name, ...) runs the handlers and answers how many fired.
    function On_func(L: Plua_State): integer; cdecl;
    function Off_func(L: Plua_State): integer; cdecl;
    function Emit_func(L: Plua_State): integer; cdecl;

    //file access

    function OpenFile_func(L: Plua_State): integer; cdecl;
    function Require_func(L: Plua_State): integer; cdecl;

    //network & json

    function HttpGet_func(L: Plua_State): integer; cdecl;
    function HttpPost_func(L: Plua_State): integer; cdecl;
    function JsonDecode_func(L: Plua_State): integer; cdecl;
    function JsonEncode_func(L: Plua_State): integer; cdecl;
    //savejson(value, "file") / loadjson("file"): the table-to-file pair on
    //top of json_encode/json_decode, with openfile's workspace rules.
    function SaveJson_func(L: Plua_State): integer; cdecl;
    function LoadJson_func(L: Plua_State): integer; cdecl;
    function Save_func(L: Plua_State): integer; cdecl;
    function Load_func(L: Plua_State): integer; cdecl;
    procedure GetSaveDoc(L: Plua_State);
    procedure InvalidateSaveCache(L: Plua_State; const AResolved: string);

   public
    procedure Init; override;
    destructor Destroy; override;
    procedure Stop; override;
    function RunLine(const ALine: string; out AOutput: string): Boolean; override;
    //Engine-driven mode: the top-level chunk returned normally and exposed
    //update()/draw() globals. The engine calls these on the main thread each frame.
    function HasEngineCallbacks: Boolean; override;
    procedure EngineUpdate(ADeltaTime: Double); override;
    procedure EngineDraw; override;

    procedure AddQueueObject(AQueueObject: TQueueObject); override;
    //Global environment hooks: unresolved globals resolve to sprites/controls by name
    function __global_getter(L: Plua_State): integer; cdecl;
    function __global_setter(L: Plua_State): integer; cdecl;

  end;

const
  // Integer key base in the Lua registry for "sprite handle -> sprite table"
  cSpriteRegistryBase = $00700000; //HUH
  // Integer key base in the Lua registry for "control handle -> control table"
  cControlRegistryBase = $00800000;
  // Integer key base in the Lua registry for "tilemap handle -> tilemap table"
  cTileRegistryBase = $00900000;
  // Integer key base in the Lua registry for "parallax handle -> parallax table"
  cParallaxRegistryBase = $00A00000;
  // Integer key base in the Lua registry for "model handle -> model table"
  cModelRegistryBase = $00B00000;
  // Key in the Lua registry for the table that caches what require already ran
  cRequireCache = 'tyro.require.cache';

implementation

//Stable addresses for the Lua registry: the per-game save.json cache (see
//GetSaveDoc). The document lives under SaveDocKey, the mtime it was read at
//under SaveAgeKey.
var
  SaveDocKey: Integer;
  SaveAgeKey: Integer;

//A runaway update()/draw() runs on the main thread, so F5 (polled by
//ProcessInput) can never reach it. Install a temporary count hook around each
//callback and abort Lua after a few seconds of one callback body.
const
  cCallbackTimeoutMs = 3000;

var
  GCallbackDeadline: QWord = 0;
  GCallbackTimedOut: Boolean = False;
  GCallbackOldHook: lua_Hook = nil;
  GCallbackOldMask: Integer = 0;
  GCallbackOldCount: Integer = 0;

procedure CallbackWatchHook(L: Plua_State; ar: Plua_Debug); cdecl;
begin
  //ar is supplied by Lua; the deadline, not the line, decides the abort.
  if (GCallbackDeadline <> 0) and (GetTickCount64 > GCallbackDeadline) then
  begin
    GCallbackTimedOut := True;
    luaL_error(L, PUTF8Char('Callback took too long - possible infinite loop'));
  end;
  //Keep the state-termination check the script's original hook performs.
  if Assigned(GCallbackOldHook) then
    GCallbackOldHook(L, ar);
end;

procedure BeginCallbackWatch(L: Plua_State);
begin
  GCallbackOldHook := lua_gethook(L);
  GCallbackOldMask := lua_gethookmask(L);
  GCallbackOldCount := lua_gethookcount(L);
  GCallbackTimedOut := False;
  GCallbackDeadline := GetTickCount64 + cCallbackTimeoutMs;
  lua_sethook(L, @CallbackWatchHook, LUA_MASKCOUNT, 100);
end;

procedure EndCallbackWatch(L: Plua_State);
begin
  lua_sethook(L, GCallbackOldHook, GCallbackOldMask, GCallbackOldCount);
  GCallbackDeadline := 0;
  GCallbackOldHook := nil;
end;

//Handler kinds for the event bus (TLuaHandler.Kind) and the names the engine
//itself emits: the reserved on() spellings listed in help/events.md.
const
  cHandlerUser = 0;   // only a script emit() runs it
  cHandlerKey = 1;    // on("space"): the engine's beat polls that key
  cHandlerMouse = 2;  // on("mouse.left"): ... and that mouse button
  cHandlerTouch = 3;  // on("touch"): the beat a finger (or the mouse) lands
  cEventUpdate = 'update'; // every frame, with the dt
  cEventDraw = 'draw';     // every paint
  cEventCollision = 'collision'; // from the physics queue: (a, b, state)
  cEventJointStretch = 'joint_stretch'; // a slide hit or left its max: (a, b, "taut"/"slack")
  cEventTouch = 'touch'; // a finger (or the mouse) landed: (x, y)

//A raw sleep() cannot be interrupted: the Lua count hook only runs between
//bytecodes, not while a C call blocks. Sleep in small steps and watch the
//engine's stop flag so F5 can end a script that is waiting.
procedure InterruptibleSleep(AMs: Int64);
const
  cStep = 20;
var
  Step: Integer;
begin
  while (AMs > 0) and not StopScriptRequested do
  begin
    Step := cStep;
    if AMs < Step then
      Step := AMs;
    sleep(Step);
    Dec(AMs, Step);
  end;
end;

//global functions
function sleep_func(L: Plua_State): integer; cdecl;
var
  n: int64;
begin
  //Round, not truncate: sleep(16.7) for a 60 FPS frame must not collapse
  //to 16 ms and drift against the display cadence.
  n := Round(L.ToNumber(1));
  //sleep() is a yield point: drop the enqueue baseline so the wait itself is
  //not reported as a stall between two queued commands.
  InterruptibleSleep(n);
  Result := 0;
end;

// pause(ms): like sleep(), but also freezes the scene for the duration. The
// main loop stops simulating physics, sprite animation/scripts and controls,
// so the frame the script queued before the call stays on screen while the
// script waits; audio and input keep running. Use it for holds/cutscenes that
// want a still picture, and sleep() when the scene should keep moving.
function pause_func(L: Plua_State): integer; cdecl;
var
  n: int64;
begin
  n := Round(L.ToNumber(1));
  if Engine <> nil then
    Engine.BeginScriptPause;
  try
    InterruptibleSleep(n);
  finally
    if Engine <> nil then
      Engine.EndScriptPause;
  end;
  Result := 0;
end;

// enter(): hold the processing queue. The drawing commands the script queues
// after this call are NOT executed by the main loop until leave() balances it,
// so the whole group between enter() and leave() is drawn at one drawing
// frame. Nested enter() calls count up and need the same number of leave()s.
function enter_func(L: Plua_State): integer; cdecl;
begin
  if Engine <> nil then
    Engine.HoldQueue;
  Result := 0;
end;

// leave(): release the hold taken by enter(). When the last hold is released
// the main loop drains the whole held group in the next drawing cycle without
// stopping at the per-frame time budget.
function leave_func(L: Plua_State): integer; cdecl;
begin
  if Engine <> nil then
    Engine.ReleaseQueue;
  Result := 0;
end;

// True when FileName already carries a drive ("C:\x", "C:x") or a UNC root
// ("\\host\x"), and so has to be used as it stands. Pasting such a name onto a
// base folder only builds a name that cannot exist, and saying "outside the
// workspace" is the honest answer instead.
function IsRootedName(const FileName: string): Boolean;
begin
  Result := (Length(FileName) > 1) and ((FileName[2] = ':') or
    ((FileName[1] = '\') and (FileName[2] = '\')));
end;

// Resolves a script-supplied file name to an absolute path and returns it only
// when it lands inside the workspace, or '' when no candidate did. The bases are
// tried in the order the rest of Tyro does: the name as given (against the
// process directory), then the script folder (ABasePath), then the workspace
// root. Every candidate is expanded before the containment test, so a name built
// from "..", a drive-relative form or a UNC path cannot slip past it.
function ResolveAndValidatePath(const FileName, ABasePath: string): string;
var
  Candidates: array[0..2] of string;
  Candidate: string;
  i: Integer;
begin
  Result := '';
  if (FileName = '') or (Res = nil) then
    Exit;
  if IsRootedName(FileName) then
  begin
    Candidate := ExpandFileName(FileName);
    if Res.IsPathInsideWorkspace(Candidate) then
      Result := Candidate;
    Exit;
  end;
  Candidates[0] := FileName;
  Candidates[1] := IncludePathDelimiter(ABasePath) + FileName;
  Candidates[2] := IncludePathDelimiter(Res.WorkPath) + FileName;
  for i := Low(Candidates) to High(Candidates) do
    if Candidates[i] <> '' then
      // ExpandFileName collapses "..", relative and UNC forms, so the
      // containment test can trust a plain prefix comparison.
      if Res.IsPathInsideWorkspace(ExpandFileName(Candidates[i])) then
        Exit(ExpandFileName(Candidates[i]));
end;

function log_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
    Log.WriteLn(L.ToString(i));
  end;
  Result := 0;
end;

// Global environment __index: called when a global name is not stored raw in
// the globals table. Raw globals win; then the name is resolved against the
// sprite list (by name) and the control list (buttons by caption); the final
// fallback is an empty table. lua_rawget/rawset keep this recursion-free.
function TLuaScript.__global_getter(L: Plua_State): integer; cdecl;
var
  aName: string;
  AHandle: integer;
begin
  Result := 1;
  //1) Existing globals win; raw read never re-enters __index
  L.PushValue(2);
  lua_rawget(L, 1);
  if not lua_isnil(L, -1) then
    Exit;
  L.Pop(1); //[globals, key]

  //2) Only string names can refer to a sprite/control
  if lua_type(L, 2) <> LUA_TSTRING then
  begin
    L.PushNil;
    Exit;
  end;

  aName := L.ToString(2);

  //3) Sprites: reuse/create the sprite proxy bound to the found handle
  AHandle := Engine.Sprites.FindByName(aName);
  if AHandle > cSpriteInvalid then
  begin
    lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle); //[globals, key, sprite?]
    if not lua_istable(L, -1) then
    begin
      L.Pop(1);
      Sprite.RegisterSprite(AHandle); //[globals, key, sprite]
    end;
    //cache it raw into the globals table and return it
    L.PushValue(-1); //[globals, key, sprite, sprite]
    L.PushValue(2); //[globals, key, sprite, sprite, key]
    lua_rawset(L, 1); //globals[key] = sprite -> [globals, key, sprite]
    L.Remove(1); //[key, sprite]
    L.Remove(1); //[sprite]
    Exit;
  end;

  //5) Controls: named controls (Name field set at creation) resolve by name
  AHandle := Controls.FindByName(aName);
  if AHandle > 0 then
  begin
    lua_rawgeti(L, LUA_REGISTRYINDEX, cControlRegistryBase + AHandle);
    if not lua_istable(L, -1) then
    begin
      L.Pop(1);
      Controls.RegisterControl(AHandle);
    end;
    L.PushValue(-1);
    L.PushValue(2);
    lua_rawset(L, 1);
    L.Remove(1); L.Remove(1);
    Exit;
  end;

  //5b) Named tilemaps resolve by name, like sprites and controls.
  AHandle := Engine.Tiles.FindByName(aName);
  if AHandle > cTileMapInvalid then
  begin
    lua_rawgeti(L, LUA_REGISTRYINDEX, cTileRegistryBase + AHandle);
    if not lua_istable(L, -1) then
    begin
      L.Pop(1);
      Tile.RegisterTile(AHandle);
    end;
    L.PushValue(-1);
    L.PushValue(2);
    lua_rawset(L, 1);
    L.Remove(1); L.Remove(1);
    Exit;
  end;

  //5c) Named 3D models resolve by name, like sprites, controls and tilemaps.
  AHandle := Engine.Models.FindByName(aName);
  if AHandle > cModel3DInvalid then
  begin
    lua_rawgeti(L, LUA_REGISTRYINDEX, cModelRegistryBase + AHandle);
    if not lua_istable(L, -1) then
    begin
      L.Pop(1);
      Model.RegisterModel(AHandle);
    end;
    L.PushValue(-1);
    L.PushValue(2);
    lua_rawset(L, 1);
    L.Remove(1); L.Remove(1);
    Exit;
  end;

  //6) Preserve normal Lua semantics for an unresolved global.
  L.PushNil;
end;

// Global environment __newindex: assign the value raw into the globals table.
// lua_rawset never re-enters __newindex (no recursion).
function TLuaScript.__global_setter(L: Plua_State): integer; cdecl;
begin
  lua_rawset(L, 1);
  Result := 0;
end;

{ TLuaConsole }

function TLuaConsole.Setter(L: PLua_State): integer;
var
  i: integer;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'height' then
      Engine.Console.Height := i
    else if field = 'width' then
      Engine.Console.Width := i
    else if field = 'margin' then
      Engine.Console.Margin := i
  end
  else if L.IsString(-1) then
  begin
    if field = 'align' then
    begin
      //* TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
      if L.ToString(-1) = 'none' then
        Engine.Console.Align := TAlign(0)
      else if L.ToString(-1) = 'left' then
        Engine.Console.Align := TAlign(1)
      else if L.ToString(-1) = 'top' then
        Engine.Console.Align := TAlign(2)
      else if L.ToString(-1) = 'right' then
        Engine.Console.Align := TAlign(3)
      else if L.ToString(-1) = 'bottom' then
        Engine.Console.Align := TAlign(4)
      else if L.ToString(-1) = 'client' then
        Engine.Console.Align := TAlign(5);
    end;
  end;
end;

function TLuaConsole.Getter(L: PLua_State): integer;
var
  i: Integer;
  field: UTF8String;
begin
  Result := 0;
  field := lua_tostring(L, 2);
  if field = 'active' then
  begin
    lua_pushboolean(L, Engine.Console.Visible);
    Result := 1;
  end
  else if field = 'align' then
  begin
    // TAlign = (alNone=0, alLeft=1, alTop=2, alRight=3, alBottom=4, alClient=5)
    i := Ord(Engine.Console.Align);
    case i of
      0: lua_pushstring(L, 'none');
      1: lua_pushstring(L, 'left');
      2: lua_pushstring(L, 'top');
      3: lua_pushstring(L, 'right');
      4: lua_pushstring(L, 'bottom');
      5: lua_pushstring(L, 'client');
    else
      lua_pushstring(L, 'none');
    end;
    Result := 1;
  end
  else if field = 'height' then
  begin
    lua_pushinteger(L, Engine.Console.Height);
    Result := 1;
  end
  else if field = 'width' then
  begin
    lua_pushinteger(L, Engine.Console.Width);
    Result := 1;
  end
  else if field = 'margin' then
  begin
    lua_pushinteger(L, Engine.Console.Margin);
    Result := 1;
  end;
end;

{ TLuaWindow }

function TLuaWindow.Setter(L: PLua_State): integer;
var
  i: integer;
  field: string;
  color: string;
begin
  Result := 0;
  field := L.ToString(2);
  if lua_isinteger(L, -1) then
  begin
    if field = 'margin' then
      Engine.Window.Margin := L.ToInteger(-1);
  end
  else if lua_isboolean(L, -1) then
  begin
    if field = 'sizable' then
      Engine.Sizable := L.ToBoolean(-1);
  end;
end;

function TLuaWindow.Getter(L: PLua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'sizable' then
  begin
    L.PushBoolean(Engine.Sizable);
    Result := 1;
  end
  else if field = 'margin' then
  begin
    L.PushInteger(Engine.Window.Margin);
    Result := 1;
  end;
end;

{ TLuaWorld }

function TLuaWorld.Setter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'title' then
  begin
    if lua_isstring(L, -1) then
    begin
      Engine.World.Title := L.ToString(-1);
      Result := 1;
      exit;
    end
  end;
  if field = 'timescale' then
  begin
    //Slow motion for the whole scene: the engine multiplies every scene
    //step's frame time by this factor (the store clamps it away from 0).
    Engine.World.Timescale := L.ToNumber(-1);
    Result := 1;
    exit;
  end;
  if field = 'enable' then
  begin
    //Presentation switch: while false the engine keeps draining the queue
    //(loads of sprites, textures and models still land) but presents only
    //the screen canvas - the world stays dark for a loading splash.
    if lua_isboolean(L, -1) then
    begin
      Engine.World.Enabled := L.ToBoolean(-1);
      Result := 1;
      exit;
    end
  end;
  if Result = 0 then
    Result := inherited;
end;

function TLuaWorld.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);

  if field = 'shaking' then
  begin
    L.PushBoolean(Engine.World.Shaking);
    Result := 1;
  end
  else if field = 'title' then
  begin
    L.PushString(Engine.World.Title);
    Result := 1;
  end
  else if field = 'timescale' then
  begin
    L.PushNumber(Engine.World.Timescale);
    Result := 1;
  end
  else if field = 'enable' then
  begin
    L.PushBoolean(Engine.World.Enabled);
    Result := 1;
  end;
  if Result = 0 then
    Result := inherited;
end;

constructor TLuaWorld.Create(const AName: string; AScript: TLuaScript);
begin
  inherited;
  Canvas := Engine.World;
end;

procedure TLuaWorld.Init;
begin
  Script.Lua.State.Register(Name, 'shake', Self, Self.Shake_func);
  Script.Lua.State.Register(Name, 'light', Self, Self.Light_func);
  Script.Lua.State.Register(Name, 'ambient', Self, Self.Ambient_func);
  Script.Lua.State.Register(Name, 'ground', Self, Self.Ground_func);
  Script.Lua.State.Register(Name, 'shadowsoft', Self, Self.ShadowSoft_func);
  Script.Lua.State.Register(Name, 'camera', Self, Self.Camera_func);
  Script.Lua.State.Register(Name, 'ortho', Self, Self.Ortho_func);
  Script.Lua.State.Register(Name, 'fov', Self, Self.Fov_func);
  Script.Lua.State.Register(Name, 'wire', Self, Self.Wire_func);
  Script.Lua.State.Register(Name, 'lights', Self, Self.Lights_func);
  Script.Lua.State.Register(Name, 'pointlight', Self, Self.PointLight_func);
  //Lua.State.Register('world', World); //Should be last one for world
  inherited;
end;

function TLuaWorld.Shake_func(L: Plua_State): integer; cdecl;
var
  ms, power: Integer;
begin
  ms := 0;
  power := 0;
  if L.ArgsCount > 0 then
    ms := Round(L.ToNumber(1));
  if L.ArgsCount > 1 then
    power := Round(L.ToNumber(2));
  if Engine <> nil then
    //Thread safe: the engine takes the request from the script thread and
    //jitters the world camera on the next frames.
    Engine.World.Shake(ms, power);
  Result := 0;
end;

// world.light() -> dx, dy, dz   |   world.light(dx, dy, dz)
// The sun ray the 3D shadows are cast along, in the model pass's world space:
// x slides the shadow right, y (non-zero) travels into the screen toward the
// ground, z slides it up. One shared sun for every model; the store keeps it
// behind its lock, so the script thread can set it while the draw thread reads.
function TLuaWorld.Light_func(L: Plua_State): integer; cdecl;
var
  x, y, z: Single;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 3 then
    Engine.Models.SetLight(L.ToNumber(1), L.ToNumber(2), L.ToNumber(3))
  else if L.ArgsCount > 0 then
    Script.DoError('light() takes no arguments (read) or three numbers (set)');
  Engine.Models.GetLight(x, y, z);
  L.PushNumber(x);
  L.PushNumber(y);
  L.PushNumber(z);
  Result := 3;
end;

// world.ambient() -> v   |   world.ambient(v)
// How much sky light fills the shadows: 0 is a hard sun (darkest shadows),
// 1 is all ambient light, so nothing stays dark.
function TLuaWorld.Ambient_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 1 then
    Engine.Models.SetAmbient(L.ToNumber(1));
  L.PushNumber(Engine.Models.GetAmbient);
  Result := 1;
end;

// world.ground() -> depth   |   world.ground(depth)
// The model.z plane the shadows fall on - the sea floor a world below the
// flying models. It must sit behind them (a bigger z than the models wear,
// farther from the eye, so every shadow lands on it).
function TLuaWorld.Ground_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 1 then
    Engine.Models.SetGroundY(L.ToNumber(1));
  L.PushNumber(Engine.Models.GetGroundY);
  Result := 1;
end;

// world.shadowsoft() -> px   |   world.shadowsoft(px)
// How far the cast shadows' edges smear, in screen pixels: the sun is no point,
// so a real shadow has a soft rim, and this blurs the silhouette by that radius
// before it lands. 0 (the default) keeps the crisp silhouette; 6 is mostly haze.
function TLuaWorld.ShadowSoft_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 1 then
    Engine.Models.SetSoftness(L.ToNumber(1));
  L.PushNumber(Engine.Models.GetSoftness);
  Result := 1;
end;

// world.camera() -> px,py,pz, tx,ty,tz, ux,uy,uz
// world.camera(px,py,pz [, tx,ty,tz [, upx,upy,upz]])
// The free 3D eye: position, the point it looks at, and which way is up on
// the screen - all in the model world space (x right, y into the screen,
// z up). Setting it switches the model pass from the top-down ortho camera
// to this perspective one, and every model's x/y/z then means a world
// position instead of screen pixels. With three numbers the eye looks at the
// origin, with six up is world +z; the store keeps them behind its lock.
function TLuaWorld.Camera_func(L: Plua_State): integer; cdecl;
var
  x, y, z, tx, ty, tz, ux, uy, uz: Single;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 9 then
    Engine.Models.SetCamera(L.ToNumber(1), L.ToNumber(2), L.ToNumber(3),
      L.ToNumber(4), L.ToNumber(5), L.ToNumber(6),
      L.ToNumber(7), L.ToNumber(8), L.ToNumber(9))
  else if L.ArgsCount >= 6 then
    Engine.Models.SetCamera(L.ToNumber(1), L.ToNumber(2), L.ToNumber(3),
      L.ToNumber(4), L.ToNumber(5), L.ToNumber(6), 0, 0, 1)
  else if L.ArgsCount >= 3 then
    Engine.Models.SetCamera(L.ToNumber(1), L.ToNumber(2), L.ToNumber(3),
      0, 0, 0, 0, 0, 1)
  else if L.ArgsCount > 0 then
    Script.DoError('camera() takes no arguments (read), or 3, 6 or 9 numbers (set)');
  Engine.Models.GetCamera(x, y, z, tx, ty, tz, ux, uy, uz);
  L.PushNumber(x);
  L.PushNumber(y);
  L.PushNumber(z);
  L.PushNumber(tx);
  L.PushNumber(ty);
  L.PushNumber(tz);
  L.PushNumber(ux);
  L.PushNumber(uy);
  L.PushNumber(uz);
  Result := 9;
end;

// world.ortho()
// Hand the model pass back to the default top-down ortho camera: x/y are
// screen pixels again and the cast shadows return with the sea plane. The
// free camera's own shot stays stored, so a later world.camera() without
// numbers resumes exactly where it was left.
function TLuaWorld.Ortho_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  Engine.Models.SetFreeCam(False);
end;

// world.fov() -> deg   |   world.fov(deg)
// The free camera's vertical field of view in degrees (10..170, 60 default):
// the wider the angle, more of the looping world bends into the frame.
function TLuaWorld.Fov_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 1 then
    Engine.Models.SetFov(L.ToNumber(1));
  L.PushNumber(Engine.Models.GetFov);
  Result := 1;
end;

// world.wire() -> true/false   |   world.wire(true/false)
// Paint every model as its wire - the mesh's own edges in its tint, no faces.
// The glowing-vector look pairs it with shader.effect = "glow".
function TLuaWorld.Wire_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 1 then
    Engine.Models.SetWire(L.ToBoolean(1));
  L.PushBoolean(Engine.Models.GetWire);
  Result := 1;
end;

// world.lights() -> true/false   |   world.lights(true/false)
// Dress the solids in light: the sky's ambient fill (world.ambient), a sun
// shining along the world.light ray, and up to four point bulbs hanging in
// the world (world.pointlight). The lambert shading is what makes a 3D world
// read as 3D; the wire pass dresses flat, as a bare grid takes its mood from
// the bloom. Every run starts with the lights off.
function TLuaWorld.Lights_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount >= 1 then
    Engine.Models.SetLighting(L.ToBoolean(1));
  L.PushBoolean(Engine.Models.GetLighting);
  Result := 1;
end;

// world.pointlight(i, x, y, z [, color [, range]])
// Hang one of the four bulbs (i = 1..4) at a world-space place. The color
// takes the same forms as canvas.color ("gold", a number), default white;
// the range is how far its light reaches before fading out, default 300.
// The bulb travels with whatever the script re-hangs - a headlight that
// follows the ship is the classic trick. Ask for just (i) and the bulb is
// unscrewed. The dress must be on (world.lights(true)) for a bulb to shine.
function TLuaWorld.PointLight_func(L: Plua_State): integer; cdecl;
var
  i: Integer;
  Range: Single;
  C: TColor;
begin
  Result := 0;
  if (Engine = nil) or (Engine.Models = nil) then
    Exit;
  if L.ArgsCount < 1 then
  begin
    Script.DoError('pointlight(i, x, y, z [, color [, range]]) wants a bulb number');
    Exit;
  end;
  i := L.ToInteger(1) - 1;
  if (i < 0) or (i > 3) then
  begin
    Script.DoError('pointlight bulbs are numbered 1..4');
    Exit;
  end;
  if L.ArgsCount < 4 then
  begin
    Engine.Models.SetPointLight(i, 0, 0, 0, 0, 0, 0, 0); // unscrew the bulb
    Exit;
  end;
  Range := 300;
  if L.ArgsCount >= 6 then
    Range := L.ToNumber(6);
  if L.ArgsCount >= 5 then
  begin
    if L.IsString(5) then
      C := StrToColor(L.ToString(5))
    else
      C := IntToColor(round(L.ToNumber(5)));
  end
  else
    C := clWhite;
  Engine.Models.SetPointLight(i, L.ToNumber(2), L.ToNumber(3), L.ToNumber(4),
    Range, C.RGBA.Red / 255, C.RGBA.Green / 255, C.RGBA.Blue / 255);
end;

{ TLuaCamera }

//The pixel of the window the world is painted from on this frame: the margin,
//or the shake jitter while a shake runs - the exact rule the engine's draw
//loop hands Camera2D.Offset, so to_world/to_screen and the picture agree.
procedure CameraScreenOffset(out AOffX, AOffY: Single);
begin
  if Engine.World.Shaking then
  begin
    AOffX := Engine.World.ShakeX;
    AOffY := Engine.World.ShakeY;
  end
  else
  begin
    AOffX := Engine.Window.Margin;
    AOffY := Engine.Window.Margin;
  end;
end;

function TLuaCamera.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'x' then
  begin
    L.PushNumber(Engine.World.CameraX);
    Result := 1;
  end
  else if field = 'y' then
  begin
    L.PushNumber(Engine.World.CameraY);
    Result := 1;
  end
  else if field = 'zoom' then
  begin
    L.PushNumber(Engine.World.CameraZoom);
    Result := 1;
  end;
end;

function TLuaCamera.Setter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'x' then
  begin
    Engine.World.CameraX := L.ToNumber(-1);
    Result := 1;
  end
  else if field = 'y' then
  begin
    Engine.World.CameraY := L.ToNumber(-1);
    Result := 1;
  end
  else if field = 'zoom' then
  begin
    //The store clamps it (0.05..32), so a wild number still lands on a
    //usable eye.
    Engine.World.CameraZoom := L.ToNumber(-1);
    Result := 1;
  end;
  if Result = 0 then
    Result := inherited;
end;

// camera.to_world(sx, sy) -> wx, wy
// A window pixel (mouse.x, mouse.y) back into the world: undo the margin or
// shake, divide by the zoom, and step out from the eye point.
function TLuaCamera.ToWorld_func(L: Plua_State): integer; cdecl;
var
  OffX, OffY, Zoom: Single;
begin
  CameraScreenOffset(OffX, OffY);
  Zoom := Engine.World.CameraZoom;
  L.PushNumber((L.ToNumber(1) - OffX) / Zoom + Engine.World.CameraX);
  L.PushNumber((L.ToNumber(2) - OffY) / Zoom + Engine.World.CameraY);
  Result := 2;
end;

// camera.to_screen(wx, wy) -> sx, sy
// The way round: a world point onto the window, e.g. to hang a control or a
// screen-canvas marker on a world position.
function TLuaCamera.ToScreen_func(L: Plua_State): integer; cdecl;
var
  OffX, OffY, Zoom: Single;
begin
  CameraScreenOffset(OffX, OffY);
  Zoom := Engine.World.CameraZoom;
  L.PushNumber((L.ToNumber(1) - Engine.World.CameraX) * Zoom + OffX);
  L.PushNumber((L.ToNumber(2) - Engine.World.CameraY) * Zoom + OffY);
  Result := 2;
end;

{ TLuaFont }

function TLuaFont.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaFont.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'height' then
  begin
    L.PushInteger(Res.Font.Height);
    Result := 1;
  end
  else if field = 'glyphs' then
  begin
    //how many glyphs the font in use actually baked — what a codepage
    //('arabic' smaller, 'everything' huge) chose.
    L.PushInteger(Res.Font.Data.GlyphCount);
    Result := 1;
  end;
end;

{ TLuaMusic }

function TLuaMusic.Setter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'volume' then
  begin
    //music.volume = 0.7 - melody-only level, clamped by SetMusicVolume
    if L.IsNumber(-1) then
      SetMusicVolume(L.ToNumber(-1));
  end;
  if field = 'muted' then
    //music.muted is an alias of sound.muted: one switch for the whole engine.
    SoundMuted := L.ToBoolean(-1);
end;

function TLuaMusic.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'volume' then
  begin
    L.PushNumber(GetMusicVolume);
    Result := 1;
  end;
  if field = 'muted' then
  begin
    L.PushBoolean(SoundMuted);
    Result := 1;
  end;
end;

{ TLuaSound }

function TLuaSound.Setter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'muted' then
    //sound.muted = true - silence every following play request, music and
    //sound alike (the playback entry points in TyroSounds check SoundMuted).
    SoundMuted := L.ToBoolean(-1);
end;

function TLuaSound.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'muted' then
  begin
    L.PushBoolean(SoundMuted);
    Result := 1;
  end;
end;

{ TTyroLuaObject }

procedure TTyroLuaObject.Created;
begin
end;

constructor TTyroLuaObject.Create(const AName: string; AScript: TLuaScript);
begin
  inherited Create;
  FScript := AScript;
  FName := AName;
end;

procedure TTyroLuaObject.AfterConstruction;
begin
  inherited;
  Created;
end;

procedure TTyroLuaObject.Init;
begin
end;

procedure TTyroLuaObject.Register(L: PLua_State);
begin
  inherited;
end;

function EditDistance(const A, B: string): Integer;
var
  LA, LB, I, J, Cost: Integer;
  D: array of array of Integer;
begin
  LA := Length(A);
  LB := Length(B);
  SetLength(D, LA + 1, LB + 1);
  for I := 0 to LA do
    D[I][0] := I;
  for J := 0 to LB do
    D[0][J] := J;
  for I := 1 to LA do
    for J := 1 to LB do
    begin
      if UpCase(A[I]) = UpCase(B[J]) then
        Cost := 0
      else
        Cost := 1;
      D[I][J] := D[I - 1][J] + 1;
      if D[I][J - 1] + 1 < D[I][J] then
        D[I][J] := D[I][J - 1] + 1;
      if D[I - 1][J - 1] + Cost < D[I][J] then
        D[I][J] := D[I - 1][J - 1] + Cost;
    end;
  Result := D[LA][LB];
end;

function TTyroLuaObject.Getter(L: PLua_State): integer;
var
  Field, Cand, Best: string;
  BestDist, Dist: Integer;
begin
  Result := inherited Getter(L);
  if (Result = 0) and (FName <> '') and (lua_type(L, 2) = LUA_TSTRING) and lua_istable(L, 1) then
  begin
    Field := L.ToString(2);
    if (Field = '') or (Field[1] = '_') or (Field = FLastHintField) then
      Exit;
    Best := '';
    BestDist := 3;
    lua_pushnil(L);
    while lua_next(L, 1) <> 0 do
    begin
      lua_pop(L, 1); //pop value, keep key
      if lua_type(L, -1) = LUA_TSTRING then
      begin
        Cand := L.ToString(-1);
        if (Cand <> '') and (Cand[1] <> '_') then
        begin
          Dist := EditDistance(Field, Cand);
          //Also catch common prefixes like "rect" -> "rectangle"
          if (Length(Field) >= 3) and (Pos(LowerCase(Field), LowerCase(Cand)) = 1) and (Dist > 1) then
            Dist := 1;
          if Dist < BestDist then
          begin
            BestDist := Dist;
            Best := Cand;
          end;
        end;
      end;
    end;
    if Best <> '' then
    begin
      FLastHintField := Field;
      if Engine <> nil then
        Engine.Notice(Format('Hint: Unknown "%s.%s" - did you mean "%s.%s"?',
          [FName, Field, FName, Best]))
      else
        Log.WriteLn(Format('Hint: Unknown "%s.%s" - did you mean "%s.%s"?',
          [FName, Field, FName, Best]));
    end;
  end;
end;

{ TLuaColors }

function TLuaColors.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaColors.Getter(L: PLua_State): integer;
var
  c: integer;
  index: integer;
  field: string;
begin
  Result := 0;
  if L.IsNumber(2) then
  begin
    //colors[0] is the first entry (the same order as the raw colors table)
    index := L.ToInteger(2);
    if (index >= 0) and (index < Length(Colors)) then
    begin
      c := ColorToInt(Colors[index].Color);
      L.PushInteger(c);
      Result := 1;
    end;
  end
  else
  begin
    field := L.ToString(2);
    if field = 'count' then
    begin
      L.PushInteger(Length(Colors));
      Result := 1;
    end;
  end;
  if Result = 0 then
    Result := inherited;
end;

procedure TLuaColors.AddColor(AName: string; AColor: TColor);
var
  aItem: TLuaColor;
begin
  aItem.Name := aName;
  aItem.Color := AColor;
  SetLength(Colors, Length(Colors) + 1);
  Colors[Length(Colors) - 1] := aItem;
end;

procedure TLuaColors.Created;
begin
  AddColor('white', clWhite);
  AddColor('silver', clLightgray);
  AddColor('gray', clGray);
  AddColor('lightgray', clLightgray);
  AddColor('darkgray', clDarkGray);
  AddColor('black', clBlack);

  AddColor('red', clRed);
  AddColor('maroon', clMaroon);
  AddColor('yellow', clYellow);
  AddColor('gold', clGold);
  AddColor('orange', clOrange);
  AddColor('pink', clPink);
  AddColor('olive', clOlive);
  AddColor('lime', clLime);
  AddColor('green', clGreen);
  AddColor('darkgreen', clDarkgreen);
  AddColor('aqua', clCyan);
  AddColor('cyan', clCyan);
  AddColor('skyblue', clSkyBlue);
  AddColor('teal', clTeal);
  AddColor('blue', clBlue);
  AddColor('darkblue', clDarkblue);
  AddColor('navy', clNavy);
  AddColor('purple', clPurple);
  AddColor('violet', clViolet);
  AddColor('darkpurple', clDarkpurple);
  AddColor('fuchsia', clMagenta);
  AddColor('magenta', clMagenta);
  AddColor('brown', clBrown);
  AddColor('darkbrown', clDarkbrown);
  AddColor('beige', clBeige);
  AddColor('raywhite', clRayWhite);
  //Fully transparent; use it for a canvas.clear() that lets the sprites behind
  //it show through (see the canvas.front HUD mode).
  AddColor('blank', clBlank);
  AddColor('transparent', clBlank);
  AddColor('none', clBlank);
end;

{ TLuaCustomCanvas }

function TLuaCustomCanvas.Setter(L: PLua_State): integer;
var
  i: Integer;
  aColor: TColor;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'color' then
  begin
    // lua_isstring answers true for numbers, so a packed color used to be read
    // back from its text and StrToColor lost any alpha of 128 or more (the
    // value passes High(Integer)). Take a real Lua integer by its bits instead.
    if L.IsInteger(-1) then
      aColor := Int64ToColor(L.ToInt64(-1))
    else if L.IsString(-1) then
      aColor := StrToColor(L.ToString(-1))
    else
      aColor := IntToColor(L.ToInteger(-1));
    FScript.AddQueueObject(TDrawSetColorObject.Create(Canvas, aColor));
    Result := 1;
  end
  else if field = 'alpha' then
  begin
    i := L.ToInteger(-1);
    FScript.AddQueueObject(TDrawSetAlphaObject.Create(Canvas, i));
    Result := 1;
  end
  else if field = 'backcolor' then
  begin
    //Read like 'color' above: a packed alpha of 128 or more passes
    //High(Integer), so the number has to be taken by its bits.
    if L.IsInteger(-1) then
      aColor := Int64ToColor(L.ToInt64(-1))
    else if L.IsString(-1) then
      aColor := StrToColor(L.ToString(-1))
    else
      aColor := IntToColor(L.ToInteger(-1));
    FScript.AddQueueObject(TDrawSetBackColorObject.Create(Canvas, aColor));
    Result := 1;
  end
  else if (field = 'pen') then
  begin
    i := L.ToInteger(-1);
    if i < 1 then
      i := 1;
    FScript.AddQueueObject(TDrawSetPenObject.Create(Canvas, i));
    Result := 1;
  end;
  if Result = 0 then
    Result := inherited;
end;

function TLuaCustomCanvas.Getter(L: PLua_State): integer;
var
  i: Integer;
  field: string;
begin
  Result := 0;
  field := L.ToString(2);

  if field = 'color' then
  begin
    i := ColorToInt(Canvas.Color);
    L.PushInteger(i);
    Result := 1;
  end
  else if field = 'backcolor' then
  begin
    i := ColorToInt(Canvas.BackColor);
    L.PushInteger(i);
    Result := 1;
  end
  else if field = 'width' then
  begin
    L.PushInteger(Canvas.Width);
    Result := 1;
  end
  else if field = 'height' then
  begin
    L.PushInteger(Canvas.Height);
    Result := 1;
  end
  else if (field = 'pen') then
  begin
    L.PushInteger(Canvas.Pen);
    Result := 1;
  end;
  if Result = 0 then
    Result := inherited;
end;

procedure TLuaCustomCanvas.Init;
begin
  with Script.Lua do
  begin
    State.Register(Name, 'clear', Self, Self.Clear_func);
    State.Register(Name, 'text', Self, Self.Text_func);
    State.Register(Name, 'text_center', Self, Self.TextCenter_func);
    State.Register(Name, 'font', Self, Self.Font_func);
    State.Register(Name, 'circle', Self, Self.Circle_func);
    State.Register(Name, 'triangle', Self, Self.Triangle_func);
    State.Register(Name, 'rectangle', Self, Self.Rectangle_func);
    State.Register(Name, 'image', Self, Self.Image_func);
    State.Register(Name, 'line', Self, Self.Line_func);
    State.Register(Name, 'point', Self, Self.Point_func);
    State.Register(Name, 'test', Self, Self.Test_func);

    lua_getglobal(State, PUTF8Char(UTF8String(Name)));
    State.NewTable;
    State.RegisterMethod('rectH', Self.GradientH_func);
    State.RegisterMethod('rectV', Self.GradientV_func);
    State.RegisterMethod('circle', Self.GradientCircle_func);
    State.SetField(-2, 'gradient');
    State.Pop;
    State.Register(Name, Self); //Should be last one
  end;
end;

{ TLuaShader }

function TLuaShader.Setter(L: PLua_State): integer;
var
  field: string;
  area: TRectangle;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'effect' then
  begin
    if L.IsString(-1) then
      FScript.AddQueueObject(TSetEffectObject.Create(Engine.Board, L.ToString(-1)));
  end
  else if field = 'value' then
  begin
    if L.IsNumber(-1) then
      FScript.AddQueueObject(TSetEffectValueObject.Create(Engine.Board, L.ToNumber(-1)));
  end
  else if field = 'area' then
  begin
    if lua_istable(L, -1) then
    begin
      area := Default(TRectangle);
      lua_rawgeti(L, -1, 1);
      if L.IsNumber(-1) then
        area.x := L.ToNumber(-1);
      lua_pop(L, 1);
      lua_rawgeti(L, -1, 2);
      if L.IsNumber(-1) then
        area.y := L.ToNumber(-1);
      lua_pop(L, 1);
      lua_rawgeti(L, -1, 3);
      if L.IsNumber(-1) then
        area.width := L.ToNumber(-1);
      lua_pop(L, 1);
      lua_rawgeti(L, -1, 4);
      if L.IsNumber(-1) then
        area.height := L.ToNumber(-1);
      lua_pop(L, 1);
      FScript.AddQueueObject(TSetEffectAreaObject.Create(Engine.Board, area));
    end;
  end;
end;

function TLuaShader.Getter(L: PLua_State): integer;
var
  field: string;
  area: TRectangle;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'effect' then
  begin
    if Engine.Board <> nil then
      L.PushString(Engine.Board.GetEffectName)
    else
      L.PushString('none');
    Result := 1;
  end
  else if field = 'value' then
  begin
    if Engine.Board <> nil then
      L.PushNumber(Engine.Board.GetEffectValue)
    else
      L.PushNumber(1.0);
    Result := 1;
  end
  else if field = 'ok' then
  begin
    //Did a shader actually compile? shader.load only queues, so the answer
    //is only valid from the frame after the load (and for the built-in
    //effects set through shader.effect).
    if Engine.Board <> nil then
      L.PushBoolean(Engine.Board.EffectOK)
    else
      L.PushBoolean(False);
    Result := 1;
  end
  else if field = 'area' then
  begin
    if Engine.Board <> nil then
      area := Engine.Board.GetEffectArea
    else
      //No board yet: answer an empty rectangle rather than dereferencing it.
      area := Default(TRectangle);
    lua_createtable(L, 4, 0);
    lua_pushnumber(L, area.x);
    lua_rawseti(L, -2, 1);
    lua_pushnumber(L, area.y);
    lua_rawseti(L, -2, 2);
    lua_pushnumber(L, area.width);
    lua_rawseti(L, -2, 3);
    lua_pushnumber(L, area.height);
    lua_rawseti(L, -2, 4);
    Result := 1;
  end;
end;

function TLuaShader.Load_func(L: PLua_State): integer; cdecl;
begin
  //Load a custom fragment shader from a file
  if L.IsString(1) then
    FScript.AddQueueObject(TLoadShaderObject.Create(Engine.Board, L.ToString(1)));
  Result := 0;
end;

// Build the global `args` table: the command line parameters the engine was
// started with, tyro.exe excluded (Res.Args). It is a plain 1-based Lua array
// of strings, so `tyro game.lua fast` gives args == {"game.lua", "fast"} and
// scripts can count them with #args. Shared by the engine script and the
// per-sprite states so `args` reads the same everywhere.
procedure PushArgsGlobal(L: PLua_State);
var
  i: Integer;
begin
  lua_createtable(L, 0, 0);
  if Res <> nil then
  begin
    for i := 0 to Res.Args.Count - 1 do
    begin
      lua_pushinteger(L, i + 1);
      lua_pushstring(L, PUTF8Char(UTF8String(Res.Args[i])));
      lua_rawset(L, -3);
    end;
  end;
  lua_setglobal(L, 'args');
end;

procedure TLuaScript.Init;
var
  i: integer;
begin
  inherited;
  Lua.Init;
  Lua.State.RegisterGlobal('version', TyroVersion);
  PushArgsGlobal(Lua.State);
  Lua.State.RegisterGlobal('log', @log_func);
  Lua.State.RegisterGlobal('sleep', @sleep_func);
  //pause(ms): like sleep(), but also freezes the scene (physics/sprites/controls)
  Lua.State.RegisterGlobal('pause', @pause_func);

  Canvas := TLuaCanvas.Create('canvas', Self);
  Screen := TLuaScreen.Create('screen', Self);
  Window := TLuaWindow.Create('window', Self);
  World := TLuaWorld.Create('world', Self);
  Console := TLuaConsole.Create('console', Self);
  Colors := TLuaColors.Create('colors', Self);
  Font := TLuaFont.Create('font', Self);
  Music := TLuaMusic.Create('music', Self);
  Sound := TLuaSound.Create('sound', Self);
  Radio := TLuaRadio.Create('radio', Self);
  Spectrum := TLuaSpectrum.Create('spectrum', Self);
  Sprite := TLuaSprite.Create('sprite', Self);
  Sprites := TLuaSprites.Create('sprites', Self);
  Images := TLuaImages.Create('images', Self);
  Tile := TLuaTile.Create('tile', Self);
  Tiles := TLuaTiles.Create('tiles', Self);
  Model := TLuaModel.Create('model', Self);
  Models := TLuaModels.Create('models', Self);
  Particles := TLuaParticles.Create('particles', Self);
  Parallax := TLuaParallax.Create('parallax', Self);
  Parallaxes := TLuaParallaxes.Create('parallaxes', Self);
  Controls := TLuaControls.Create('controls', Self);
  Output := TLuaOutput.Create('output', Self);
  Collision := TLuaCollision.Create('collision', Self);
  Shader := TLuaShader.Create('shader', Self);
  Mouse := TLuaMouse.Create('mouse', Self);
  Touch := TLuaTouch.Create('touch', Self);
  Keyboard := TLuaKeyboard.Create('keyboard', Self);
  Gamepad := TLuaGamepad.Create('gamepad', Self);
  Game := TLuaGame.Create('game', Self);
  QueueStats := TLuaQueue.Create('queue', Self);
  Camera := TLuaCamera.Create('camera', Self);

  Files := TList.Create; //of TLuaFile, freed in Destroy
  Timers := TList.Create; //of TLuaTimer, freed in Destroy
  TimerClock := 0;
  TimerNextId := 1; //timer id 0 stays invalid, so cancel(0) always answers false
  Tweens := TList.Create; //of TLuaTween, freed in Destroy
  TweenNextId := 1; //tween id 0 stays invalid, so tween.kill(0) always answers false
  Handlers := TList.Create; //of TLuaHandler, freed in Destroy
  HandlerNextId := 1;
  Scenes := TList.Create; //of TLuaScene, freed in Destroy
  CurrentScene := ''; //handler id 0 stays invalid, so off(0) always answers false

  //window
  Lua.State.Register('window', 'show', Window, Window.Show_func);
  Lua.State.Register('window', Window); //Should be last one for window

  //world
  Lua.State.RegisterGlobal('shake', World.Shake_func);//deprecated
  World.Init;

  //2D world camera: camera.x / camera.y / camera.zoom are fields,
  //camera.to_world / camera.to_screen map window pixels and world points
  Lua.State.Register('camera', 'to_world', Camera, Camera.ToWorld_func);
  Lua.State.Register('camera', 'to_screen', Camera, Camera.ToScreen_func);
  Lua.State.Register('camera', Camera); //Should be last one

  //global functions
  Lua.State.RegisterGlobal('print', Console.Print_func);
  Lua.State.RegisterGlobal('println', Console.PrintLn_func);

  //console
  Lua.State.Register('console', 'print', Console, Console.Print_func);
  Lua.State.Register('console', 'println', Console, Console.PrintLn_func);
  Lua.State.Register('console', 'show', Console, Console.Show_func);
  Lua.State.Register('console', 'read', Console, Console.Read_func);
  Lua.State.Register('console', Console); //Should be last one

  //canvas
  Canvas.Init;
  Screen.Init;

  //shader (property-style: shader.effect, shader.value, shader.area)
  Lua.State.Register('shader', 'load', Shader, Shader.Load_func);
  Lua.State.Register('shader', Shader); //Should be last one

  //font
  //font: font.load returns an id, the font list caches it; font.use(id)
  //activates a cached font, font.free(id) releases one, font.current()
  //answers the id in use. codepage picks the glyph set ('arabic'/'cp864').
  Lua.State.Register('font', 'load', Font, Font.Load_func);
  Lua.State.Register('font', 'use', Font, Font.Use_func);
  Lua.State.Register('font', 'free', Font, Font.Free_func);
  Lua.State.Register('font', 'current', Font, Font.Current_func);
  Lua.State.Register('font', Font); //Should be last one

  //music
  Lua.State.RegisterGlobal('beep', Music.Beep_func);

  Lua.State.Register('music', 'beep', Music, Music.Beep_func);
  Lua.State.Register('music', 'sound', Music, Music.Sound_func);
  Lua.State.Register('music', 'play', Music, Music.Play_func);
  Lua.State.Register('music', 'mml', Music, Music.MML_func);
  Lua.State.Register('music', 'stop', Music, Music.Stop_func);
  Lua.State.Register('music', Music); //Should be last one

  //sound (one-shot file SFX + gapless/seamless looping playback)
  //  sound.load("file.wav")              preload a file into the sound cache
  //  sound.unload("file.wav")            drop a preloaded file from the cache
  //  sound.play("file.wav" [, volume])   one-shot SFX from an audio file
  //  sound.loopPlay(freq [, volume])
  //  sound.loopPlay(wave, freq [, volume])
  //  sound.pitch(freq)         retune the running loop
  //  sound.volume(0..1)
  //  sound.stop()
  Lua.State.Register('sound', 'load', Sound, Sound.Load_func);
  Lua.State.Register('sound', 'unload', Sound, Sound.Unload_func);
  Lua.State.Register('sound', 'beep', Sound, Sound.Beep_func);
  Lua.State.Register('sound', 'play', Sound, Sound.Play_func);
  Lua.State.Register('sound', 'noise', Sound, Sound.Noise_func);
  Lua.State.Register('sound', 'generate', Sound, Sound.Generate_func);
  Lua.State.Register('sound', 'loopPlay', Sound, Sound.LoopPlay_func);
  Lua.State.Register('sound', 'loopPitch', Sound, Sound.LoopPitch_func);
  Lua.State.Register('sound', 'loopVolume', Sound, Sound.LoopVolume_func);
  Lua.State.Register('sound', 'loopStop', Sound, Sound.LoopStop_func);
  Lua.State.Register('sound', Sound); //Should be last one

  //radio (radio.play(url), radio.pause(), radio.resume(), radio.stop()
  // + getters: radio.title, radio.station, radio.state, radio.playing ...)
  Lua.State.Register('radio', 'play', Radio, Radio.Play_func);
  Lua.State.Register('radio', 'pause', Radio, Radio.Pause_func);
  Lua.State.Register('radio', 'resume', Radio, Radio.Resume_func);
  Lua.State.Register('radio', 'stop', Radio, Radio.Stop_func);
  Lua.State.Register('radio', Radio); //Should be last one

  //spectrum (spectrum.show(x, y, w, h), spectrum.hide()
  // + getters/setters: spectrum.bars, spectrum.active, spectrum.visible)
  Lua.State.Register('spectrum', 'show', Spectrum, Spectrum.Show_func);
  Lua.State.Register('spectrum', 'hide', Spectrum, Spectrum.Hide_func);
  Lua.State.Register('spectrum', Spectrum); //Should be last one

  //input & timing (input tables + global functions)
  //mouse: mouse.x / mouse.y / mouse.wheel are fields, mouse.pressed("left") is the button edge
  Lua.State.Register('mouse', 'pressed', Mouse, Mouse.Pressed_func);
  Lua.State.Register('mouse', 'down', Mouse, Mouse.Down_func);
  Lua.State.Register('mouse', 'released', Mouse, Mouse.Released_func);
  Lua.State.Register('mouse', 'up', Mouse, Mouse.Up_func);
  Lua.State.Register('mouse', Mouse); //Should be last one
  //touch: touch.x / touch.y / touch.count / touch.down / touch.pressed are
  //fields of the frame the engine stepped; touch.point(i) walks the fingers.
  //On a desktop the mouse stands in, so the same script works on a tablet.
  Lua.State.Register('touch', 'point', Touch, Touch.Point_func);
  Lua.State.Register('touch', Touch); //Should be last one

  //keyboard: keyboard.pressed("space") / keyboard.down("a") / keyboard.released / keyboard.up
  Lua.State.Register('keyboard', 'pressed', Keyboard, Keyboard.Pressed_func);
  Lua.State.Register('keyboard', 'down', Keyboard, Keyboard.Down_func);
  Lua.State.Register('keyboard', 'released', Keyboard, Keyboard.Released_func);
  Lua.State.Register('keyboard', 'up', Keyboard, Keyboard.Up_func);
  Lua.State.Register('keyboard', Keyboard); //Should be last one

  //gamepad: gamepad.pressed("a" [, gamepad]) / gamepad.down / gamepad.released / gamepad.up
  //         gamepad.axis("leftx" [, gamepad]) / gamepad.available([gamepad]) / gamepad.name([gamepad])
  //         gamepad.count() / gamepad.axes([gamepad]) / gamepad.vibrate(l, r [, dur, pad])
  //         gamepad.mappings(text) adds SDL/GLFW mappings, returns nonzero on success
  Lua.State.Register('gamepad', 'pressed', Gamepad, Gamepad.Pressed_func);
  Lua.State.Register('gamepad', 'down', Gamepad, Gamepad.Down_func);
  Lua.State.Register('gamepad', 'released', Gamepad, Gamepad.Released_func);
  Lua.State.Register('gamepad', 'up', Gamepad, Gamepad.Up_func);
  Lua.State.Register('gamepad', 'axis', Gamepad, Gamepad.Axis_func);
  Lua.State.Register('gamepad', 'available', Gamepad, Gamepad.Available_func);
  Lua.State.Register('gamepad', 'name', Gamepad, Gamepad.Name_func);
  Lua.State.Register('gamepad', 'count', Gamepad, Gamepad.Count_func);
  Lua.State.Register('gamepad', 'axes', Gamepad, Gamepad.Axes_func);
  Lua.State.Register('gamepad', 'vibrate', Gamepad, Gamepad.Vibrate_func);
  Lua.State.Register('gamepad', 'rumble', Gamepad, Gamepad.Vibrate_func);
  Lua.State.Register('gamepad', 'mappings', Gamepad, Gamepad.Mappings_func);
  Lua.State.Register('gamepad', Gamepad); //Should be last one //TODO Remove it

  //joystick: alias to gamepad for joystick/flight stick/gamepad controls
  Lua.State.Register('joystick', 'pressed', Gamepad, Gamepad.Pressed_func);
  Lua.State.Register('joystick', 'down', Gamepad, Gamepad.Down_func);
  Lua.State.Register('joystick', 'released', Gamepad, Gamepad.Released_func);
  Lua.State.Register('joystick', 'up', Gamepad, Gamepad.Up_func);
  Lua.State.Register('joystick', 'axis', Gamepad, Gamepad.Axis_func);
  Lua.State.Register('joystick', 'available', Gamepad, Gamepad.Available_func);
  Lua.State.Register('joystick', 'name', Gamepad, Gamepad.Name_func);
  Lua.State.Register('joystick', 'count', Gamepad, Gamepad.Count_func);
  Lua.State.Register('joystick', 'axes', Gamepad, Gamepad.Axes_func);
  Lua.State.Register('joystick', 'vibrate', Gamepad, Gamepad.Vibrate_func);
  Lua.State.Register('joystick', 'rumble', Gamepad, Gamepad.Vibrate_func);
  Lua.State.Register('joystick', 'mappings', Gamepad, Gamepad.Mappings_func);
  Lua.State.Register('joystick', Gamepad); //Should be last one

  //game: the shared gameplay button pairs; the implementations are TTyroEngine
  //methods.
  Lua.State.Register('game', 'pause_pressed', Game, Game.PausePressed_func);
  Lua.State.Register('game', 'pause_down', Game, Game.PauseDown_func);
  Lua.State.Register('game', 'pause', Game, Game.PauseGame_func);
  Lua.State.Register('game', 'resume', Game, Game.ResumeGame_func);
  Lua.State.Register('game', 'toggle_pause', Game, Game.TogglePause_func);
  Lua.State.Register('game', 'restart', Game, Game.Restart_func);
  Lua.State.Register('game', 'difficulty', Game, Game.Difficulty_func);
  Lua.State.Register('game', 'start_pressed', Game, Game.StartPressed_func);
  Lua.State.Register('game', 'fire_pressed', Game, Game.FirePressed_func);
  Lua.State.Register('game', 'fire_down', Game, Game.FireDown_func);
  Lua.State.Register('game', 'a_pressed', Game, Game.APressed_func);
  Lua.State.Register('game', 'a_down', Game, Game.ADown_func);
  Lua.State.Register('game', 'b_pressed', Game, Game.BPressed_func);
  Lua.State.Register('game', 'b_down', Game, Game.BDown_func);
  Lua.State.Register('game', 'x_pressed', Game, Game.XPressed_func);
  Lua.State.Register('game', 'x_down', Game, Game.XDown_func);
  Lua.State.Register('game', 'y_pressed', Game, Game.YPressed_func);
  Lua.State.Register('game', 'y_down', Game, Game.YDown_func);
  Lua.State.Register('game', 'l_pressed', Game, Game.LPressed_func);
  Lua.State.Register('game', 'l_down', Game, Game.LDown_func);
  Lua.State.Register('game', 'r_pressed', Game, Game.RPressed_func);
  Lua.State.Register('game', 'r_down', Game, Game.RDown_func);
  Lua.State.Register('game', 'select_pressed', Game, Game.SelectPressed_func);
  Lua.State.Register('game', 'select_down', Game, Game.SelectDown_func);
  Lua.State.Register('game', 'start_down', Game, Game.StartDown_func);
  Lua.State.Register('game', Game); //Should be last one

  //scene: named menu/play/gameover states. add(name, opts) keeps four
  //callbacks; go(name [, args]) runs leave/enter; the current scene's
  //update(dt)/draw() ride the same engine beat as the global callbacks.
  Lua.State.RegisterTable('scene');
  Lua.State.Register('scene', 'add', Self, SceneAdd_func);
  Lua.State.Register('scene', 'go', Self, SceneGo_func);
  Lua.State.Register('scene', 'current', Self, SceneCurrent_func);
  Lua.State.Register('scene', 'list', Self, SceneList_func);
  Lua.State.Register('scene', 'remove', Self, SceneRemove_func);
  Lua.State.Register('scene', 'clear', Self, SceneClear_func);

  //score: the best scores kept in one small text file per game.
  Lua.State.Register('score', 'hi_load', Game, Game.HiLoad_func);
  Lua.State.Register('score', 'hi_save', Game, Game.HiSave_func);

  Lua.State.RegisterGlobal('frametime', FrameTime_func);
  Lua.State.RegisterGlobal('time', TotalTime_func);
  Lua.State.RegisterGlobal('millis', Millis_func);
  //scene timers: after(ms, fn) fires once, every(ms, fn) repeats; both hand
  //back a timer id that cancel(id) stops. The callbacks run in the
  //engine-driven frame, on the scene clock (timescale-stretched, paused by
  //pause()).
  Lua.State.RegisterGlobal('after', After_func);
  Lua.State.RegisterGlobal('every', Every_func);
  Lua.State.RegisterGlobal('cancel', Cancel_func);
  //juice tweens: the raw helpers assemble into the callable table `tween`
  //through a tiny prelude, so both spellings work:
  //   tween(player, "x", 300, 400, "outback")
  //   tween.from(scorelabel, "scale", 1.4, 1, 150, "out")
  Lua.State.RegisterGlobal('__tween_to', Tween_func);
  Lua.State.RegisterGlobal('__tween_from', TweenFrom_func);
  Lua.State.RegisterGlobal('__tween_kill', TweenKill_func);
  Lua.State.RegisterGlobal('__tween_killall', TweenKillAll_func);
  Lua.State.RegisterGlobal('__tween_count', TweenCount_func);
  TweenPrelude;
  //event bus: on(name, fn) hooks a callback, emit(name, ...) runs them all,
  //off(id) unhooks one. Names that spell a key ("space", "w") or a mouse
  //button ("mouse.left") are polled by the engine itself; the reserved names
  //"update", "draw" and "collision" fire from the frame beat and the physics
  //queue. See help/events.md.
  Lua.State.RegisterGlobal('on', On_func);
  Lua.State.RegisterGlobal('off', Off_func);
  Lua.State.RegisterGlobal('emit', Emit_func);
  Lua.State.RegisterGlobal('rand', RandomValue_func);
  Lua.State.RegisterGlobal('screenshot', Screenshot_func);
  Lua.State.RegisterGlobal('exit', Exit_func);
  Lua.State.RegisterGlobal('restart', Game.Restart_func);
  Lua.State.RegisterGlobal('clock', Clock_func);
  Lua.State.RegisterGlobal('now', Time_func);
  Lua.State.RegisterGlobal('textwidth', TextWidth_func);
  Lua.State.RegisterGlobal('textheight', TextHeight_func);
  Lua.State.RegisterGlobal('rgba', Rgba_func);
  Lua.State.RegisterGlobal('rgb', Rgba_func);

  //math helpers and the seeded rng: plain globals, no table to reach for
  Lua.State.RegisterGlobal('clamp', Game.Clamp_func);
  Lua.State.RegisterGlobal('lerp', Game.Lerp_func);
  Lua.State.RegisterGlobal('sign', Game.Sign_func);
  Lua.State.RegisterGlobal('dist', Game.Dist_func);
  Lua.State.RegisterGlobal('aabb', Game.Aabb_func);
  Lua.State.RegisterGlobal('rng', Game.Rng_func);

  //enter/leave: hold the drawing queue so the commands queued in between are
  //all drawn at one drawing cycle instead of being spread over frames
  Lua.State.RegisterGlobal('enter', enter_func);
  Lua.State.RegisterGlobal('leave', leave_func);

  //file access: openfile(name [, mode]) -> file handle, or nil + error
  //This is the only way a script reaches the file system; the io/os libraries
  //are removed in TLua.Init, so nothing here can be bypassed from Lua.
  Lua.State.RegisterGlobal('openfile', OpenFile_func);

  //network: http_get(url) -> body | nil, err. The request runs on the script
  //thread (TTyroEngine.HttpGet), so the render loop keeps drawing while it is
  //in flight. json_decode(text) -> value | nil, err turns a JSON document into
  //the matching Lua value (object->table, array->table, number, boolean,
  //null->nil); together they let a script talk to web APIs, e.g.
  //local t = json_decode(http_get("https://api.open-meteo.com/v1/forecast?..."))
  Lua.State.RegisterGlobal('http_get', HttpGet_func);
  Lua.State.RegisterGlobal('http_post', HttpPost_func);
  Lua.State.RegisterGlobal('json_decode', JsonDecode_func);
  Lua.State.RegisterGlobal('json_encode', JsonEncode_func);
  //savejson(value, "file") writes the JSON text of a value into a workspace
  //file, loadjson("file") reads it back: a table saved is a table found again.
  Lua.State.RegisterGlobal('savejson', SaveJson_func);
  Lua.State.RegisterGlobal('loadjson', LoadJson_func);
  //save("score", 120) / load("score"): the key/value pair over one save.json
  //in the game's folder - persistence without inventing file names.
  Lua.State.RegisterGlobal('save', Save_func);
  Lua.State.RegisterGlobal('load', Load_func);

  //require(name) is the stock loader's replacement: the Lua one is removed in
  //TLua.Init along with dofile/loadfile/package, and this one reads only from
  //the workspace and the app folder. The table that remembers what already ran
  //lives in the registry, since package.loaded is no longer there to hold it.
  Lua.State.RegisterGlobal('require', Require_func);
  lua_newtable(Lua.State); //[cache]
  lua_setfield(Lua.State, LUA_REGISTRYINDEX, PUTF8Char(cRequireCache));

  // Sprite system: Sprites.new creates a sprite, Sprites("name") finds by name
  Lua.State.RegisterTable('sprites');
  Lua.State.Register('sprites', 'new', Self, Sprites.New_func);
  Lua.State.Register('sprites', 'puff', Self, Sprites.Puff_func);
  Lua.State.Register('sprites', 'blob', Self, Sprites.Blob_func);
  Lua.State.Register('sprites', 'find', Self, Sprites.Find_func);
  // Set __call in the Sprites metatable so Sprites("name") works; Lua passes the table as arg 1
  Lua.State.Register('sprites', '__call', Self, Sprites.Call_func, True);
  // Pluto class for `new Sprite([name [, file]])` and `class MySprite extends Sprite`
  Sprite.PushClass(Lua.State);
  lua_setglobal(Lua.State, 'Sprite');

  // Paint sheets: images.new(w, h) makes a small canvas the script draws on
  // itself (no file); sprite:fromimage(sheet) turns those pixels into a sprite
  // texture. The sheet keeps the whole canvas API: color, rectangle, circle,
  // triangle, line, point, text, clear.
  Lua.State.RegisterTable('images');
  Lua.State.Register('images', 'new', Self, Images.New_func);
  Lua.State.Register('images', 'save', Self, Images.Save_func);
  Lua.State.Register('images', 'savesprite', Self, Images.SaveSprite_func);

  // Tilemap system: tiles.new(...) creates a map, tiles.find(name) finds one.
  // A map table (returned by tiles.new) carries map:load/resize/set/get/data/
  // solidtile/solid/draw plus the property fields (map.cols, map.x, ...).
  Lua.State.RegisterTable('tiles');
  Lua.State.Register('tiles', 'new', Self, Tiles.New_func);
  Lua.State.Register('tiles', 'find', Self, Tiles.Find_func);

  // 3D models: models.new([name]) creates a model, models.find(name) /
  // models("name") find one. A model object carries load/move/show/hide plus the
  // property fields (model.x, model.y, model.angle, ...). The engine draws every
  // visible model once per frame; no draw call is needed from the script.
  Lua.State.RegisterTable('models');
  Lua.State.Register('models', 'new', Self, Models.New_func);
  Lua.State.Register('models', 'sphere', Self, Models.Sphere_func);
  Lua.State.Register('models', 'box', Self, Models.Box_func);
  Lua.State.Register('models', 'cylinder', Self, Models.Cylinder_func);
  Lua.State.Register('models', 'cone', Self, Models.Cone_func);
  Lua.State.Register('models', 'torus', Self, Models.Torus_func);
  Lua.State.Register('models', 'find', Self, Models.Find_func);
  Lua.State.Register('models', '__call', Self, Models.Call_func, True);
  // Pluto class for `new Model([name [, file]])` and `class MyModel extends Model`
  Model.PushClass(Lua.State);
  lua_setglobal(Lua.State, 'Model');

  // Particle sparks: particles.emit(x, y [, count] [, options]) throws a burst
  // of sparks at a screen pixel and the engine steps and draws it until the
  // last spark fades - no per-frame call needed. Like a sprite, the burst's
  // layer picks its side of the 3D models (see cLayerTop3D): the default 0
  // burns under them, a layer at or above the split rains over them.
  // options is a table with any of: color, speed, life, size, gravity, drag,
  // spread, direction, layer. particles.count reads how many sparks are alive.
  Lua.State.RegisterTable('particles');
  Lua.State.Register('particles', 'emit', Particles, Particles.Emit_func);
  Lua.State.Register('particles', 'clear', Particles, Particles.Clear_func);
  Lua.State.Register('particles', Particles); //Should be last one for particles

  // Parallax backgrounds: parallax.new([name]) creates one, parallax.find(name)
  // finds a named one. A parallax object carries add/addground/clear/reset plus
  // the property fields (bg.offset, bg.speed, bg.visible, bg.tile, bg.groundy).
  Lua.State.RegisterTable('parallax');
  Lua.State.Register('parallax', 'new', Self, Parallaxes.New_func);
  Lua.State.Register('parallax', 'find', Self, Parallaxes.Find_func);

  //controls (generic control table; 'buttons' is a legacy alias to the same
  //object so old scripts keep working). Every function here takes the control as
  //its first argument and each of them also has a field spelling on the control
  //table itself (btn.width = 200 for controls.width(btn, 200)), except for the
  //actions - show, hide, move, position, focus, additem, item, clear, load -
  //which stay methods on the control table and take a colon call.
  Lua.State.RegisterTable('controls');
  Lua.State.Register('controls', 'new', Controls, Controls.New_func);
  //Lua.State.Register('controls', 'find', Controls, Controls.Find_func); TODO find by handle or name
  Lua.State.Register('controls', 'position', Controls, Controls.Position_func);
  Lua.State.Register('controls', 'move', Controls, Controls.Move_func);
  Lua.State.Register('controls', 'width', Controls, Controls.Width_func);
  Lua.State.Register('controls', 'height', Controls, Controls.Height_func);
  Lua.State.Register('controls', 'visible', Controls, Controls.Visible_func);
  Lua.State.Register('controls', 'show', Controls, Controls.Show_func);
  Lua.State.Register('controls', 'hide', Controls, Controls.Hide_func);
  Lua.State.Register('controls', 'hover', Controls, Controls.Hover_func);
  Lua.State.Register('controls', 'down', Controls, Controls.Down_func);
  Lua.State.Register('controls', 'clicked', Controls, Controls.Clicked_func);
  Lua.State.Register('controls', 'focused', Controls, Controls.Focused_func);
  Lua.State.Register('controls', 'focus', Controls, Controls.Focus_func);
  Lua.State.Register('controls', 'border', Controls, Controls.Border_func);
  Lua.State.Register('controls', 'backcolor', Controls, Controls.BackColor_func);
  Lua.State.Register('controls', 'name', Controls, Controls.Name_func);
  Lua.State.Register('controls', 'align', Controls, Controls.Align_func);
  Lua.State.Register('controls', 'parent', Controls, Controls.Parent_func);
  //caption/text: both spellings work on a caption control, and text also
  //covers an edit; btn.caption and btn.text are the field spellings of both.
  //Keep the table forms too: docs, demos and existing scripts use them, and
  //they are thin wrappers around the same queued operations as the fields.
  Lua.State.Register('controls', 'caption', Controls, Controls.Caption_func);
  Lua.State.Register('controls', 'text', Controls, Controls.Text_func);
  Lua.State.Register('controls', 'checked', Controls, Controls.Checked_func);
  //listbox: items/viewcount/itemindex are fields on the control table
  //(lst.items), the three actions additem/item/clear are also methods there
  //(lst:additem("a")); the table form takes the handle first
  //(controls.additem(lst, "a"))
  Lua.State.Register('controls', 'items', Controls, Controls.Items_func);
  Lua.State.Register('controls', 'item', Controls, Controls.Item_func);
  Lua.State.Register('controls', 'additem', Controls, Controls.AddItem_func);
  Lua.State.Register('controls', 'clear', Controls, Controls.Clear_func);
  Lua.State.Register('controls', 'viewcount', Controls, Controls.ViewCount_func);
  Lua.State.Register('controls', 'itemindex', Controls, Controls.ItemIndex_func);
  //image control: load the texture an image shows (img.load(...) is the same)
  Lua.State.Register('controls', 'load', Controls, Controls.LoadImage_func);
  Lua.State.Register('controls', Controls); //should be last one

  //output (catches print/println/log)
  Lua.State.RegisterTable('output');
  Lua.State.Register('output', 'show', Output, Output.Show_func);
  Lua.State.Register('output', 'hide', Output, Output.Hide_func);
  Lua.State.Register('output', 'clear', Output, Output.Clear_func);
  Lua.State.Register('output', Output); //should be last one
  // Collision system: collision.pump() drains events and fires sprite.onCollide(other, state)
  Lua.State.Register('collision', 'pump', Collision, Collision.Pump_func);
  // Joints: collision.joint(a, b, opts) ties two sprites with a pin/slide/
  // rotator/limit, collision.joint_free(id) drops one again. The sprites get
  // collide = true on the way in - a rope tied to a non-physical sprite holds
  // nothing. See help/joints.md.
  Lua.State.Register('collision', 'joint', Collision, Collision.Joint_func);
  Lua.State.Register('collision', 'joint_free', Collision, Collision.JointFree_func);
  Lua.State.Register('collision', 'joints', Collision, Collision.Joints_func);
  Lua.State.Register('collision', 'joints_clear', Collision, Collision.JointsClear_func);
  Lua.State.Register('collision', Collision); // should be last — wires getter/setter metamethods

  // queue: read-only counters over the drawing queue (see TQueueStats), e.g.
  // "while queue.depth > 0 do sleep(1) end" waits for the queued commands to run.
  Lua.State.Register('queue', QueueStats); //should be last one for queue

  Lua.State.BeginTable;
  for i := 0 to Length(Colors.Colors) - 1 do
    Lua.State.Register(Colors.Colors[i].Name, ColorToInt(Colors.Colors[i].Color));
  Lua.State.EndTableGlobal('colors', Colors);

  //Attach a metatable to the global environment so an unresolved global name
  //resolves to a sprite or control by name (e.g. richard.move(...) when
  //"richard" is a sprite). The globals table is fetched with
  //lua_pushglobaltable (LUA_RIDX_GLOBALS) for Lua 5.5 compatibility;
  //__index/__newindex use raw access (no recursion).
  //
  //Metamethods MUST be registered with RegisterMeta: the plain
  //Register(Name, Method) injects the table as an extra first argument, which
  //is what "obj:method()" needs, but it shifts the metamethod arguments to
  //(table, table, key[, value]) and __newindex would then leave two values on
  //the Lua stack (corrupting it).
  lua_pushglobaltable(Lua.State); //[globals]
  Lua.State.NewTable; //[globals, meta]
  Lua.State.RegisterMeta('__index', __global_getter);
  Lua.State.RegisterMeta('__newindex', __global_setter);
  Lua.State.SetMetaTable(-2); //globals.metatable = meta -> [globals]
  Lua.State.Pop(1); //[]
end;

destructor TLuaScript.Destroy;
var
  aFile: TLuaFile;
begin
  // Lua holds light-userdata/method pointers to these facade objects, so close
  // the state before releasing them. Lua.Close is a no-op when State is nil.
  Lua.Close;
  //Any file still open is closed and released here; the closures that referenced
  //it died with the state, so nothing can call back into it afterwards.
  if Files <> nil then
  begin
    while Files.Count > 0 do
    begin
      aFile := TLuaFile(Files[Files.Count - 1]);
      Files.Delete(Files.Count - 1);
      aFile.Free;
    end;
    FreeAndNil(Files);
  end;
  //The timers go with the state: their registry refs died when Lua.Close
  //shut it down, so only the list items need releasing here.
  if Timers <> nil then
  begin
    while Timers.Count > 0 do
    begin
      TLuaTimer(Timers[Timers.Count - 1]).Free;
      Timers.Delete(Timers.Count - 1);
    end;
    FreeAndNil(Timers);
  end;
  //The tweens leave the same way: their refs died with the state.
  if Tweens <> nil then
  begin
    while Tweens.Count > 0 do
    begin
      TLuaTween(Tweens[Tweens.Count - 1]).Free;
      Tweens.Delete(Tweens.Count - 1);
    end;
    FreeAndNil(Tweens);
  end;
  //The bus handlers leave with the state too, refs and all.
  if Handlers <> nil then
  begin
    while Handlers.Count > 0 do
    begin
      TLuaHandler(Handlers[Handlers.Count - 1]).Free;
      Handlers.Delete(Handlers.Count - 1);
    end;
    FreeAndNil(Handlers);
  end;
  //Scenes die with the state too: their refs were released by Lua.Close.
  if Scenes <> nil then
  begin
    while Scenes.Count > 0 do
    begin
      TLuaScene(Scenes[Scenes.Count - 1]).Free;
      Scenes.Delete(Scenes.Count - 1);
    end;
    FreeAndNil(Scenes);
  end;
  FreeAndNil(Shader);
  FreeAndNil(Game);
  FreeAndNil(Gamepad);
  FreeAndNil(Keyboard);
  FreeAndNil(Mouse);
  FreeAndNil(QueueStats);
  FreeAndNil(Collision);
  FreeAndNil(Output);
  FreeAndNil(Controls);
  FreeAndNil(Sprites);
  FreeAndNil(Sprite);
  //the sheets die with the state that holds their closures; their render
  //textures live in Engine.Images and are unloaded by the engine
  FreeAndNil(Images);
  FreeAndNil(Parallaxes);
  FreeAndNil(Parallax);
  FreeAndNil(Particles);
  FreeAndNil(Models);
  FreeAndNil(Model);
  FreeAndNil(Tiles);
  FreeAndNil(Tile);
  FreeAndNil(Spectrum);
  FreeAndNil(Radio);
  FreeAndNil(Music);
  FreeAndNil(Sound);
  FreeAndNil(Font);
  FreeAndNil(Colors);
  FreeAndNil(Console);
  FreeAndNil(Window);
  FreeAndNil(World);
  FreeAndNil(Screen);
  FreeAndNil(Canvas);
  inherited;
end;

procedure TLuaScript.DoError(S: string);
begin
  //CallHandler runtime errors never return to the main chunk, so LastError is
  //empty for them; mark the run failed so the --exit CLI sees it too.
  if FLastError = '' then
    FLastError := S;
  if IsConsole then
    WriteLn(S);
  if (Engine <> nil) and (TThread.Current.ThreadID = MainThreadID) then
    Engine.ReportScriptError(S);
end;

procedure TLuaScript.Run;
var
  Msg: string;
begin
  // A stopped script object may be run again from the interactive console.
  if Lua.State = nil then
    Exit;
  Lua.SetReady;
  GCallbackTimedOut := False;
  //Publish the failure state for the CLI --exit lifecycle: the worker thread
  //reads this after the run and the Engine loop surfaces it as the exit code.
  FLastError := '';
  //WriteLn('Run Script');
  //Sleep(1000);
  if not Lua.State.RunString(Source.Text, Msg) then
  begin
    FLastError := Msg;
    DoError(Msg);
  end;
end;

procedure TLuaScript.Stop;
begin
  //abort the running Lua bytecode: HookCount calls luaL_error when
  //this state's status is terminated, and RunString returns on the next hook tick
  if Lua.State <> nil then
    Lua.SetTerminated;
  inherited;
end;

// Look up the global AName and pcall it. The caller pushes the arguments (AArgs
// of them) first; an absent or non-function global drains the stack and returns.
// The call itself lives in LuaClasses (TLuaHelper.CallGlobal).
procedure TLuaScript.CallGlobal(const AName: string; AArgs: Integer);
var
  Msg: string;
begin
  if Lua.State = nil then
    Exit;
  if not Lua.State.CallGlobal(AName, AArgs, Msg) then
    DoError(AName + ': ' + Msg);
end;

procedure TLuaScript.CallRef(ARef: Integer; AFirst, ACount: Integer);
var
  Msg: string;
begin
  if (ARef = LUA_NOREF) or (Lua.State = nil) then
    Exit;
  lua_rawgeti(Lua.State, LUA_REGISTRYINDEX, ARef); //[.. args.., fn]
  lua_insert(Lua.State, AFirst); //[.., fn, args..]
  if lua_pcall(Lua.State, ACount, 0, 0) <> LUA_OK then
  begin
    Msg := Lua.State.ToString(-1);
    Lua.State.Pop(1);
    DoError(Msg);
  end;
end;

function TLuaScript.FindScene(const AName: string): TLuaScene;
var
  i: Integer;
  Key: string;
begin
  Result := nil;
  if Scenes = nil then
    Exit;
  Key := LowerCase(AName);
  for i := 0 to Scenes.Count - 1 do
    if TLuaScene(Scenes[i]).Name = Key then
      Exit(TLuaScene(Scenes[i]));
end;

// The engine takes over only when the returned script defines at least one of
// the frame callbacks, or armed a timer - an `after(...)` with no update() is
// a scene too, and its callback needs the same engine-driven beat.
function TLuaScript.HasEngineCallbacks: Boolean;
begin
  Result := (Lua.State <> nil) and
    (Lua.State.FunctionExists('update') or Lua.State.FunctionExists('draw')
     or HasTimers or HasTweens or HasHandlers
     or ((Scenes <> nil) and (Scenes.Count > 0)));
end;

procedure TLuaScript.EngineUpdate(ADeltaTime: Double);
var
  S: TLuaScene;
begin
  if Lua.State = nil then
    Exit;
  BeginCallbackWatch(Lua.State);
  try
  //The timer beat rides the stretched frame time: fire everything that is
  //due first, so a callback's marks show in the very frame that answers it.
  PumpTimers(ADeltaTime);
  //glides step on the same beat, so a done callback can arm the next tween
  //and see its first value in the very next frame.
  PumpTweens(ADeltaTime);
  //a script that listens with on("collision") gets the physics queue drained
  //into the bus; collision.pump() in a script still works (it just drains
  //first and finds nothing when the bus already carried the events).
  if (Collision <> nil) and (HandlerCountNamed(cEventCollision) > 0) then
    Collision.PumpEvents;
  //ropes sing on the same beat: on("joint_stretch", fn) hears every slide
  //that snapped taut or let go, as (a, b, "taut"/"slack"). The drain runs
  //whether or not a listener exists, so the physics buffer never fills.
  if Collision <> nil then
    Collision.PumpStretch;
  //fire-and-forget input bindings: on("space", fn) answers on the press beat
  PumpInputs;
  //a timer callback may have called exit(); do not step into a stopped state.
  if Stopped then
    Exit;
  Lua.State.PushNumber(ADeltaTime);
  //on("update", fn) handlers ride the same delta the global update() gets;
  //the pushed dt stays on the stack for CallGlobal to consume.
  if HandlerCountNamed(cEventUpdate) > 0 then
    Dispatch(cEventUpdate, Lua.State, lua_gettop(Lua.State), 1);
  if Stopped then
    Exit; //an update handler may have called exit()
  CallGlobal('update', 1);
  //The current scene's update rides the same dt, after the global one.
  if (not Stopped) and (Scenes <> nil) and (CurrentScene <> '') then
  begin
    S := FindScene(CurrentScene);
    if S <> nil then
    begin
      Lua.State.PushNumber(ADeltaTime);
      CallRef(S.UpdateRef, lua_gettop(Lua.State), 1);
    end;
  end;
  finally
    EndCallbackWatch(Lua.State);
    if GCallbackTimedOut then
    begin
      DoError('update() took too long - possible infinite loop; stopping the run');
      Stop;
    end;
  end;
end;

procedure TLuaScript.EngineDraw;
var
  S: TLuaScene;
begin
  if Lua.State = nil then
    Exit;
  BeginCallbackWatch(Lua.State);
  try
    CallGlobal('draw', 0);
    //The current scene paints after the global draw, like on("draw").
    if (Scenes <> nil) and (CurrentScene <> '') then
    begin
      S := FindScene(CurrentScene);
      if S <> nil then
        CallRef(S.DrawRef, lua_gettop(Lua.State) + 1, 0);
    end;
    //on("draw", fn) listeners paint after the global draw(), same beat
    if HandlerCountNamed(cEventDraw) > 0 then
      Dispatch(cEventDraw, Lua.State, 0, 0);
  finally
    EndCallbackWatch(Lua.State);
    if GCallbackTimedOut then
    begin
      DoError('draw() took too long - possible infinite loop; stopping the run');
      Stop;
    end;
  end;
end;

// True while at least one after()/every() timer still waits. A script that
// armed one has a scene the engine must keep stepping (HasEngineCallbacks),
// and once the list drains the run simply stays driven like an update() one.
function TLuaScript.HasTimers: Boolean;
begin
  Result := (Timers <> nil) and (Timers.Count > 0);
end;

// Shared body of after() and every(): take (ms, fn) and hand back the timer
// id, or -1 when the call does not carry a function to run. The due point
// rides the scene clock, so a timer armed while the top-level chunk still
// runs counts from the moment the engine takes the frame loop over.
function TLuaScript.NewTimer(L: Plua_State; AMs: Double; APeriodic: Boolean): Integer;
var
  T: TLuaTimer;
begin
  Result := -1;
  if (Timers = nil) or (Lua.State = nil) then
    Exit;
  if not lua_isfunction(L, 2) then
    Exit;
  if AMs < 0 then
    AMs := 0;
  if APeriodic and (AMs < 1) then
    AMs := 1; //every(0) would fire on every frame - one millisecond is its floor
  T := TLuaTimer.Create;
  T.Id := TimerNextId;
  Inc(TimerNextId);
  T.Interval := AMs;
  T.Periodic := APeriodic;
  T.Dead := False;
  T.Due := TimerClock + AMs;
  lua_pushvalue(L, 2);
  T.FnRef := luaL_ref(L, LUA_REGISTRYINDEX);
  Timers.Add(T);
  Result := T.Id;
end;

//The timer beat: advance the scene clock, fire everything that is due, and
//sweep the dead. Callbacks may arm and cancel timers while they run, so the
//due list is a snapshot taken before the first call and every callback
//re-checks Dead. An every() re-arms from now, so a frame that slept (lag, a
//pause) costs one tick, never a burst of them.
procedure TLuaScript.PumpTimers(ADeltaTime: Double);
var
  I, N: Integer;
  T: TLuaTimer;
  Due: array of TLuaTimer;
  Msg: string;
begin
  if Timers = nil then
    Exit;
  TimerClock := TimerClock + ADeltaTime * 1000;
  if Timers.Count = 0 then
    Exit;
  SetLength(Due, 0);
  for I := 0 to Timers.Count - 1 do
  begin
    T := TLuaTimer(Timers[I]);
    if (not T.Dead) and (TimerClock >= T.Due) then
    begin
      if T.Periodic then
        //re-arm from now: a frame that slept (lag, a pause) costs one tick,
        //never a burst of them
        T.Due := TimerClock + T.Interval;
      N := Length(Due);
      SetLength(Due, N + 1);
      Due[N] := T;
    end;
  end;
  for I := 0 to High(Due) do
  begin
    T := Due[I];
    if T.Dead then
      Continue; //an earlier callback this very beat cancelled it
    lua_rawgeti(Lua.State, LUA_REGISTRYINDEX, T.FnRef);
    if lua_pcall(Lua.State, 0, 0, 0) <> LUA_OK then
    begin
      Msg := string(UTF8String(lua_tostring(Lua.State, -1)));
      lua_pop(Lua.State, 1);
      DoError(Format('timer #%d: %s', [T.Id, Msg]));
    end;
    if not T.Periodic then
      T.Dead := True; //a one-shot leaves the world the moment it answers
    if Stopped then
      Break; //the callback called exit()
  end;
  //sweep: dead timers leave the list, and their registry refs with them, so
  //a cancelled or answered callback's function is released at once.
  for I := Timers.Count - 1 downto 0 do
  begin
    T := TLuaTimer(Timers[I]);
    if T.Dead then
    begin
      if Lua.State <> nil then
        luaL_unref(Lua.State, LUA_REGISTRYINDEX, T.FnRef);
      Timers.Delete(I);
      T.Free;
    end;
  end;
end;

// after(ms, fn) -> timer id
// One shot: fn runs once, ms of scene time later. world.timescale stretches
// the wait with the rest of the scene and pause() sleeps it; the id feeds
// cancel() to unscrew the shot before it fires.
function TLuaScript.After_func(L: Plua_State): integer; cdecl;
var
  Id: Integer;
begin
  Id := NewTimer(L, L.ToNumber(1), False);
  if Id >= 0 then
    L.PushInteger(Id)
  else
    L.PushNil;
  Result := 1;
end;

// every(ms, fn) -> timer id
// The repeating sibling: fn runs again and again, ms of scene time apart,
// until cancel(id) or the run ends. Like after(), the wait rides the scene
// clock, so slow motion thins the ticks out and a long frame costs one
// tick, not a burst.
function TLuaScript.Every_func(L: Plua_State): integer; cdecl;
var
  Id: Integer;
begin
  Id := NewTimer(L, L.ToNumber(1), True);
  if Id >= 0 then
    L.PushInteger(Id)
  else
    L.PushNil;
  Result := 1;
end;

// cancel(id) -> true when a waiting timer was stopped, false when the id is
// unknown or already answered. The callback never runs again afterwards.
function TLuaScript.Cancel_func(L: Plua_State): integer; cdecl;
var
  I, Id: Integer;
  T: TLuaTimer;
begin
  Result := 1;
  Id := L.ToInteger(1);
  if Timers <> nil then
    for I := 0 to Timers.Count - 1 do
    begin
      T := TLuaTimer(Timers[I]);
      if (T.Id = Id) and (not T.Dead) then
      begin
        T.Dead := True; //the pump's sweep releases the reference
        L.PushBoolean(True);
        Exit;
      end;
    end;
  L.PushBoolean(False);
end;

{ ---- juice tweens ---- }

// Easing curve kinds: a tween's curve is base * 3 + direction. The names a
// script passes are folded onto these by ParseEasing.
const
  cEaseSine = 0;   cEaseQuad = 1;   cEaseCubic = 2;  cEaseExpo = 3;
  cEaseBack = 4;   cEaseElastic = 5; cEaseBounce = 6;
  cEaseIn = 0;     cEaseOut = 1;    cEaseInOut = 2;
  cEaseLinear = 21; //the only base without directions

// One bounce for progress t in [0..1]: the classic falling-ball polynomial,
// used for bounce-out directly and reflected for bounce-in.
function EaseBounceOut(t: Double): Double;
const
  n1 = 7.5625;
  d1 = 2.75;
begin
  if t < 1 / d1 then
    Result := n1 * t * t
  else if t < 2 / d1 then
  begin
    t := t - 1.5 / d1;
    Result := n1 * t * t + 0.75;
  end
  else if t < 2.5 / d1 then
  begin
    t := t - 2.25 / d1;
    Result := n1 * t * t + 0.9375;
  end
  else
  begin
    t := t - 2.625 / d1;
    Result := n1 * t * t + 0.984375;
  end;
end;

// 2 raised to e, without pulling Math into this unit (the expo and elastic
// curves are built on it).
function EasePow2(e: Double): Double;
begin
  Result := Exp(e * 0.6931471805599453);
end;

// The eased value for progress t in [0..1] on one curve: the standard Penner
// set, mirrored for in/out and stitched in the middle for inout. The ends are
// exact: TweenEase(kind, 0) = 0 and TweenEase(kind, 1) = 1 for every curve,
// so a glide always lands where the script said it would.
function TweenEase(AKind: Integer; t: Double): Double;
const
  cPi = 3.141592653589793;
  sBack = 1.70158;
  sBack2 = 2.5949095; // the back-inout overshoot
  c4 = 2 * cPi / 3;   // elastic period 0.3
  c5 = 2 * cPi / 4.5; // elastic-inout period 0.45
var
  b, d: Integer;
  p, q: Double;
begin
  if AKind = cEaseLinear then
    Exit(t);
  if t <= 0 then
    Exit(0);
  if t >= 1 then
    Exit(1);
  b := AKind div 3;
  d := AKind mod 3;
  Result := 0;
  case b of
    cEaseSine:
      case d of
        cEaseIn: Result := 1 - Cos(t * cPi / 2);
        cEaseOut: Result := Sin(t * cPi / 2);
      else Result := -(Cos(cPi * t) - 1) / 2;
      end;
    cEaseQuad:
      case d of
        cEaseIn: Result := t * t;
        cEaseOut: Result := 1 - (1 - t) * (1 - t);
      else
        if t < 0.5 then
          Result := 2 * t * t
        else
          Result := 1 - Sqr(-2 * t + 2) / 2;
      end;
    cEaseCubic:
      case d of
        cEaseIn: Result := t * t * t;
        cEaseOut: Result := 1 + Sqr(t - 1) * (t - 1);
      else
        if t < 0.5 then
          Result := 4 * t * t * t
        else
        begin
          p := -2 * t + 2;
          Result := 1 + p * p * p / 2;
        end;
      end;
    cEaseExpo:
      case d of
        cEaseIn: Result := EasePow2(10 * t - 10);
        cEaseOut: Result := 1 - EasePow2(-10 * t);
      else
        if t < 0.5 then
          Result := EasePow2(20 * t - 10) / 2
        else
          Result := (2 - EasePow2(-20 * t + 10)) / 2;
      end;
    cEaseBack:
      case d of
        cEaseIn: Result := t * t * ((sBack + 1) * t - sBack);
        cEaseOut:
        begin
          p := t - 1;
          Result := 1 + p * p * ((sBack + 1) * p + sBack);
        end;
      else
        p := 2 * t;
        if t < 0.5 then
          Result := p * p * ((sBack2 + 1) * p - sBack2) / 2
        else
        begin
          q := p - 2;
          Result := (q * q * ((sBack2 + 1) * q + sBack2) + 2) / 2;
        end;
      end;
    cEaseElastic:
      case d of
        cEaseIn: Result := -EasePow2(10 * t - 10) * Sin((t * 10 - 10.75) * c4);
        cEaseOut: Result := EasePow2(-10 * t) * Sin((t * 10 - 0.75) * c4) + 1;
      else
        if t < 0.5 then
          Result := EasePow2(20 * t - 10) * Sin((20 * t - 11.125) * c5) / 2
        else
          Result := EasePow2(-20 * t + 10) * Sin((20 * t - 11.125) * c5) / 2 + 1;
      end;
    cEaseBounce:
      case d of
        cEaseIn: Result := 1 - EaseBounceOut(1 - t);
        cEaseOut: Result := EaseBounceOut(t);
      else
        if t < 0.5 then
          Result := (1 - EaseBounceOut(1 - 2 * t)) * 0.5
        else
          Result := (1 + EaseBounceOut(2 * t - 1)) * 0.5;
      end;
  end;
end;

// Fold an easing name onto a curve kind. Base + direction in either order,
// case and separators ignored: "backout", "outback" and "BACK_OUT" all pick
// the back/out curve. Bases: sine quad cubic expo back elastic bounce. The
// friendly aliases: "smooth" is the gentle sine in-out, "snap" the back-out
// overshoot, "wobble" the elastic settle. A bare base means its out curve,
// "linear"/"none" glide straight, and anything unknown lands on quad-out -
// the plain decelerating glide a tween gets when the name is left out.
function ParseEasing(const AName: string): Integer;
const
  cBases: array[0..6] of string =
    ('sine', 'quad', 'cubic', 'expo', 'back', 'elastic', 'bounce');
  cDirs: array[0..2] of string = ('inout', 'out', 'in'); //longest first
  cDirKind: array[0..2] of Integer = (cEaseInOut, cEaseOut, cEaseIn);
  function FindBase(const A: string): Integer;
  var K: Integer;
  begin
    for K := 0 to High(cBases) do
      if cBases[K] = A then
        Exit(K);
    Result := -1;
  end;
var
  s, x, base: string;
  I, K, d, b: Integer;
begin
  x := '';
  for I := 1 to Length(AName) do
    if not ((AName[I] = '_') or (AName[I] = '-') or (AName[I] = ' ')) then
      x := x + LowerCase(AName[I]);
  s := x;
  if (s = '') or (s = 'linear') or (s = 'none') then
    Exit(cEaseLinear);
  if s = 'smooth' then
    Exit(cEaseSine * 3 + cEaseInOut);
  if s = 'snap' then
    Exit(cEaseBack * 3 + cEaseOut);
  if s = 'wobble' then
    Exit(cEaseElastic * 3 + cEaseOut);
  base := s;
  d := -1;
  for I := 0 to 2 do //direction at the end: "backinout", "backout", "backin"
    if (Length(s) > Length(cDirs[I]))
      and (Copy(s, Length(s) - Length(cDirs[I]) + 1, Length(cDirs[I])) = cDirs[I]) then
    begin
      d := cDirKind[I];
      base := Copy(s, 1, Length(s) - Length(cDirs[I]));
      Break;
    end;
  b := FindBase(base);
  if (b < 0) and (d >= 0) then
  begin //direction at the front instead: "outback", "inoutback", "inback"
    base := s;
    d := -1;
    for I := 0 to 2 do
      if (Length(s) > Length(cDirs[I]))
        and (Copy(s, 1, Length(cDirs[I])) = cDirs[I]) then
      begin
        d := cDirKind[I];
        base := Copy(s, Length(cDirs[I]) + 1, MaxInt);
        Break;
      end;
    b := FindBase(base);
  end;
  if b < 0 then
    Exit(cEaseQuad * 3 + cEaseOut); //unknown name: the plain glide
  if d < 0 then
    d := cEaseOut; //a bare base eases out
  Result := b * 3 + d;
end;

// Shared body of tween() and tween.from(); only where the start value comes
// from differs. tween() reads the property as it stands, through the
// subject's __index, so a sprite answers with its live engine value;
// tween.from() takes the start value as an argument and writes it once, at
// once - the punch pattern: set big now, glide back over ms. The subject and
// an optional finish callback are kept in the Lua registry for the life of
// the glide. On success the id is returned; on bad arguments -1 is returned
// and (nil, reason) already sits on L for the caller to hand back, so a kid
// can print(tween(spr, 'x', 'big', 300)) and learn what was wrong.
function TLuaScript.PushTween(L: Plua_State; AHasFrom: Boolean): Integer;
var
  T: TLuaTween;
  ITo, IMs, IEasing, IDone: Integer;
  FromV, ToV: Double;
  PropName: string;
begin
  Result := -1;
  if (Tweens = nil) or (Lua.State = nil) then
  begin
    L.PushNil;
    L.PushString('tween: not running inside a script');
    Exit;
  end;
  if not lua_istable(L, 1) then
  begin
    L.PushNil;
    L.PushString('tween: the first argument is the thing to move, a sprite or your own table');
    Exit;
  end;
  if not lua_isstring(L, 2) then
  begin
    L.PushNil;
    L.PushString('tween: the second argument is the property name, like "x"');
    Exit;
  end;
  PropName := L.ToString(2);
  ITo := 3 + Ord(AHasFrom); //from: (obj, prop, from, to, ms ...) - to: (obj, prop, to, ms ...)
  IMs := ITo + 1;
  IEasing := IMs + 1;
  IDone := IEasing + 1;
  if AHasFrom and not lua_isnumber(L, 3) then
  begin
    L.PushNil;
    L.PushString('tween.from: the start value must be a number');
    Exit;
  end;
  if not lua_isnumber(L, ITo) then
  begin
    L.PushNil;
    L.PushString('tween: the target value must be a number');
    Exit;
  end;
  if not lua_isnumber(L, IMs) then
  begin
    L.PushNil;
    L.PushString('tween: the duration is milliseconds, a number like 300');
    Exit;
  end;
  if (not lua_isnoneornil(L, IDone)) and (not lua_isfunction(L, IDone)) then
  begin
    L.PushNil;
    L.PushString('tween: the last argument must be a function to run when it lands, or nothing');
    Exit;
  end;
  ToV := L.ToNumber(ITo);
  if AHasFrom then
    FromV := L.ToNumber(3)
  else
  begin //read the property now - __index answers for sprites, raw fields for tables
    L.PushString(PropName);
    if (lua_gettable(L, 1) <> LUA_TNUMBER) then
    begin
      lua_pop(L, 1);
      L.PushNil;
      L.PushString('tween: ' + PropName + ' has no number to glide from - give it a value first or use tween.from');
      Exit;
    end;
    FromV := lua_tonumber(L, -1);
    lua_pop(L, 1);
  end;
  T := TLuaTween.Create;
  T.Id := TweenNextId;
  Inc(TweenNextId);
  lua_pushvalue(L, 1);
  T.SubjRef := luaL_ref(L, LUA_REGISTRYINDEX);
  T.Prop := PropName;
  T.From := FromV;
  T.Target := ToV;
  T.Duration := L.ToNumber(IMs);
  if T.Duration < 1 then
    T.Duration := 1; //a zero-length glide still lands, on the next beat
  T.Elapsed := 0;
  if lua_isstring(L, IEasing) then
    T.Easing := ParseEasing(L.ToString(IEasing))
  else
    T.Easing := cEaseQuad * 3 + cEaseOut; //the name left out: the plain glide
  if lua_isfunction(L, IDone) then
  begin
    lua_pushvalue(L, IDone);
    T.DoneRef := luaL_ref(L, LUA_REGISTRYINDEX);
  end
  else
    T.DoneRef := LUA_NOREF;
  T.Dead := False;
  Tweens.Add(T);
  if AHasFrom then
  begin //the punch: the start value lands before the glide begins
    L.PushNumber(FromV);
    lua_setfield(L, 1, PUTF8Char(UTF8String(PropName)));
  end;
  Result := T.Id;
end;

// The tween beat: advance every glide, write each value back through the
// subject's __newindex (the very path a script's `spr.x = v` takes), and let
// finished glides answer with their callback - after every value landed, so
// a handler sees the whole finished world, like the timer pump. Callbacks
// may kill or arm tweens while they run; the sweep at the end only frees
// tweens no callback still needs, and releases their registry refs.
procedure TLuaScript.PumpTweens(ADeltaTime: Double);
var
  I, ND: Integer;
  T: TLuaTween;
  Done: array of TLuaTween;
  V, P: Double;
  Msg: string;
begin
  if (Tweens = nil) or (Tweens.Count = 0) or (Lua.State = nil) then
    Exit;
  ND := 0;
  SetLength(Done, Tweens.Count);
  for I := 0 to Tweens.Count - 1 do
  begin
    T := TLuaTween(Tweens[I]);
    if T.Dead then
      Continue; //a callback earlier in this beat killed it
    //the engine feeds the beat in seconds, the glide counts milliseconds
    T.Elapsed := T.Elapsed + ADeltaTime * 1000;
    if T.Elapsed >= T.Duration then
      P := 1
    else if T.Elapsed <= 0 then
      P := 0
    else
      P := T.Elapsed / T.Duration;
    if P >= 1 then
      V := T.Target //land exact: no easing curve may leave the value adrift
    else
      V := T.From + (T.Target - T.From) * TweenEase(T.Easing, P);
    lua_rawgeti(Lua.State, LUA_REGISTRYINDEX, T.SubjRef);
    lua_pushnumber(Lua.State, V);
    lua_setfield(Lua.State, -2, PUTF8Char(UTF8String(T.Prop)));
    lua_pop(Lua.State, 1);
    if (P >= 1) and (not T.Dead) then
    begin
      T.Dead := True; //a finished glide leaves the world the moment it lands
      if T.DoneRef <> LUA_NOREF then
      begin
        Done[ND] := T;
        Inc(ND);
      end;
    end;
  end;
  for I := 0 to ND - 1 do
  begin
    T := Done[I];
    lua_rawgeti(Lua.State, LUA_REGISTRYINDEX, T.DoneRef);
    lua_rawgeti(Lua.State, LUA_REGISTRYINDEX, T.SubjRef); //the handler meets its subject
    if lua_pcall(Lua.State, 1, 0, 0) <> LUA_OK then
    begin
      Msg := string(UTF8String(lua_tostring(Lua.State, -1)));
      lua_pop(Lua.State, 1);
      DoError(Format('tween #%d: %s', [T.Id, Msg]));
    end;
    luaL_unref(Lua.State, LUA_REGISTRYINDEX, T.DoneRef);
    T.DoneRef := LUA_NOREF;
    if Stopped then
      Break; //the callback called exit()
  end;
  //sweep: dead glides leave the list, and their subject refs with them.
  for I := Tweens.Count - 1 downto 0 do
  begin
    T := TLuaTween(Tweens[I]);
    if T.Dead then
    begin
      if Lua.State <> nil then
      begin
        luaL_unref(Lua.State, LUA_REGISTRYINDEX, T.SubjRef);
        if T.DoneRef <> LUA_NOREF then
          luaL_unref(Lua.State, LUA_REGISTRYINDEX, T.DoneRef);
      end;
      Tweens.Delete(I);
      T.Free;
    end;
  end;
end;

// True while at least one glide is still moving; a script that armed one has
// a scene the engine must keep stepping, the same way HasTimers keeps an
// after()-only script alive.
function TLuaScript.HasTweens: Boolean;
begin
  Result := (Tweens <> nil) and (Tweens.Count > 0);
end;

// The small Lua bootstrap behind the tween table: `tween(...)` finds its
// __call and forwards to tween.to, and the raw __tween_* helper names are
// hidden again so the globals keep the one name.
procedure TLuaScript.TweenPrelude;
var
  Msg: string;
begin
  if Lua.State = nil then
    Exit;
  if not Lua.State.RunString(
    'tween = setmetatable({'
    + 'to = __tween_to, from = __tween_from, kill = __tween_kill, '
    + 'killall = __tween_killall, count = __tween_count}, '
    + '{__call = function(self, ...) return self.to(...) end}); '
    + '__tween_to = nil __tween_from = nil __tween_kill = nil '
    + '__tween_killall = nil __tween_count = nil ', Msg) then
    DoError('tween prelude: ' + Msg);
end;

// tween(obj, prop, value, ms [, easing [, done]]) -> tween id
// Glide one numeric property - a sprite's x, y, angle, scale, alpha ..., a
// control's, or any field of your own table - from where it stands to the
// value, in ms of scene time. Easing names: linear plus sine/quad/cubic/expo/
// back/elastic/bounce each with in/out/inout, and the aliases smooth, snap,
// wobble; leaving the name out gives the plain decelerating glide. done runs
// when it lands. The id feeds tween.kill().
function TLuaScript.Tween_func(L: Plua_State): integer; cdecl;
var
  Id: Integer;
begin
  Id := PushTween(L, False);
  if Id >= 0 then
  begin
    L.PushInteger(Id);
    Result := 1;
  end
  else
    Result := 2; //PushTween left (nil, reason) on the stack
end;

// tween.from(obj, prop, startValue, endValue, ms [, easing [, done]]) -> id
// The spelling for punches and flashes: startValue is written at once and
// the glide runs from it to endValue - a number that was already there never
// has to be read back, and a field the subject does not have yet is fine.
function TLuaScript.TweenFrom_func(L: Plua_State): integer; cdecl;
var
  Id: Integer;
begin
  Id := PushTween(L, True);
  if Id >= 0 then
  begin
    L.PushInteger(Id);
    Result := 1;
  end
  else
    Result := 2; //PushTween left (nil, reason) on the stack
end;

// tween.kill(id) -> true when a gliding tween was stopped, false when the id
// is unknown or already landed. The property stays where the glide left it.
function TLuaScript.TweenKill_func(L: Plua_State): integer; cdecl;
var
  I, Id: Integer;
  T: TLuaTween;
begin
  Result := 1;
  Id := L.ToInteger(1);
  if Tweens <> nil then
    for I := 0 to Tweens.Count - 1 do
    begin
      T := TLuaTween(Tweens[I]);
      if (T.Id = Id) and (not T.Dead) then
      begin
        T.Dead := True; //the pump's sweep releases the references
        L.PushBoolean(True);
        Exit;
      end;
    end;
  L.PushBoolean(False);
end;

// tween.killall(obj) -> how many glides on that exact subject were stopped.
// Before a respawn, so an old glide cannot drag the new state around.
function TLuaScript.TweenKillAll_func(L: Plua_State): integer; cdecl;
var
  I, N: Integer;
  T: TLuaTween;
begin
  N := 0;
  if (Tweens <> nil) and (Lua.State <> nil) then
    for I := 0 to Tweens.Count - 1 do
    begin
      T := TLuaTween(Tweens[I]);
      if T.Dead then
        Continue;
      lua_rawgeti(Lua.State, LUA_REGISTRYINDEX, T.SubjRef);
      if lua_rawequal(Lua.State, -1, 1) then
      begin
        T.Dead := True;
        Inc(N);
      end;
      lua_pop(Lua.State, 1);
    end;
  L.PushInteger(N);
  Result := 1;
end;

// tween.count() -> how many glides are still moving (Ctrl+F10 sees it too).
function TLuaScript.TweenCount_func(L: Plua_State): integer; cdecl;
var
  I, N: Integer;
  T: TLuaTween;
begin
  N := 0;
  if Tweens <> nil then
    for I := 0 to Tweens.Count - 1 do
      if not TLuaTween(Tweens[I]).Dead then
        Inc(N);
  L.PushInteger(N);
  Result := 1;
end;

{ ---- event bus ---- }

//True while at least one on() handler is hooked: a script that bound a key
//or an on("update") listener has a scene the engine must keep stepping, the
//same way HasTimers keeps an after()-only script alive.
function TLuaScript.HasHandlers: Boolean;
begin
  Result := (Handlers <> nil) and (Handlers.Count > 0);
end;

//How many live handlers listen to this exact (lowercase) name.
function TLuaScript.HandlerCountNamed(const AName: string): Integer;
var
  I: Integer;
  H: TLuaHandler;
begin
  Result := 0;
  if Handlers <> nil then
    for I := 0 to Handlers.Count - 1 do
    begin
      H := TLuaHandler(Handlers[I]);
      if (not H.Dead) and (H.Name = AName) then
        Inc(Result);
    end;
end;

//The live handler carrying this id, or nil. Handlers are looked up by id
//(never by pointer) across dispatches, so a sweep running inside a nested
//emit can free one while an outer loop still holds its id.
function TLuaScript.FindHandler(AId: Integer): TLuaHandler;
var
  I: Integer;
  H: TLuaHandler;
begin
  Result := nil;
  if (Handlers = nil) or (AId = 0) then
    Exit;
  for I := 0 to Handlers.Count - 1 do
  begin
    H := TLuaHandler(Handlers[I]);
    if (H.Id = AId) and (not H.Dead) then
      Exit(H);
  end;
end;

//off()'d and answered handlers leave the list here, with their refs. Called
//at the end of every dispatch and by off(); the run's Destroy takes the rest.
procedure TLuaScript.SweepHandlers;
var
  I: Integer;
  H: TLuaHandler;
begin
  if Handlers = nil then
    Exit;
  for I := Handlers.Count - 1 downto 0 do
  begin
    H := TLuaHandler(Handlers[I]);
    if H.Dead then
    begin
      if Lua.State <> nil then
        luaL_unref(Lua.State, LUA_REGISTRYINDEX, H.FnRef);
      Handlers.Delete(I);
      H.Free;
    end;
  end;
end;

//Run every live handler of AName with a copy of the ACount values stacked
//from AFirst up. Handlers armed or off()'d by an earlier handler still obey
//this beat's snapshot: newly armed ones start answering from the next emit,
//off()'d ones do not run again even mid-dispatch (their ids are re-checked
//against the live list before every call). A handler's error is reported
//with the event's name and the next one runs anyway.
function TLuaScript.Dispatch(const AName: string; L: Plua_State; AFirst, ACount: Integer): Integer;
var
  I, J, N, Id: Integer;
  H: TLuaHandler;
  Live: array of Integer;
  Msg: string;
begin
  Result := 0;
  if (Handlers = nil) or (Lua.State = nil) then
    Exit;
  N := 0;
  SetLength(Live, Handlers.Count);
  for I := 0 to Handlers.Count - 1 do
  begin
    H := TLuaHandler(Handlers[I]);
    if (not H.Dead) and (H.Name = AName) then
    begin
      Live[N] := H.Id;
      Inc(N);
    end;
  end;
  for I := 0 to N - 1 do
  begin
    H := FindHandler(Live[I]); //nil when a nested sweep already freed it
    if H = nil then
      Continue;
    lua_rawgeti(L, LUA_REGISTRYINDEX, H.FnRef);
    for J := 0 to ACount - 1 do
      lua_pushvalue(L, AFirst + J);
    if lua_pcall(L, ACount, 0, 0) <> LUA_OK then
    begin
      Msg := string(UTF8String(lua_tostring(L, -1)));
      lua_pop(L, 1);
      DoError(Format('on "%s" #%d: %s', [AName, H.Id, Msg]));
    end;
    Inc(Result);
    if Stopped then
      Break; //a handler called exit()
  end;
  SweepHandlers;
end;

//The input beat: every hkKey/hkMouse binding whose key or button made its
//press edge this frame runs now - exactly what keyboard.pressed would have
//answered in update(), without writing the update() poll. The snapshot of
//ids keeps a handler that binds or unhooks one from disturbing this beat.
procedure TLuaScript.PumpInputs;
var
  I, J, N, Id: Integer;
  H, H2: TLuaHandler;
  Live: array of Integer;
  Frame: TyroInput.TTouchFrame;
  TouchRead: Boolean;
begin
  if (Handlers = nil) or (Lua.State = nil) then
    Exit;
  N := 0;
  SetLength(Live, Handlers.Count);
  for I := 0 to Handlers.Count - 1 do
  begin
    H := TLuaHandler(Handlers[I]);
    if (not H.Dead) and (H.Kind <> cHandlerUser) then
    begin
      Live[N] := H.Id;
      Inc(N);
    end;
  end;
  TouchRead := False;
  for I := 0 to N - 1 do
  begin
    H := FindHandler(Live[I]);
    if H = nil then
      Continue;
    //One beat serves a name once: Dispatch runs every listener hooked to it,
    //so a second handler on the same key, button or finger must not make the
    //whole group run twice.
    J := 0;
    while J < I do
    begin
      H2 := FindHandler(Live[J]);
      if (H2 <> nil) and (H2.Name = H.Name) then
        Break;
      Inc(J);
    end;
    if J < I then
      Continue; //an earlier entry this beat already carried this name
    case H.Kind of
      cHandlerKey:
        if TyroInput.IsKeyPressed(H.Name) then
          Dispatch(H.Name, Lua.State, 0, 0);
      cHandlerMouse:
        if TyroInput.IsMouseButtonPressed(H.Btn) then
          Dispatch(H.Name, Lua.State, 0, 0);
      cHandlerTouch:
        begin
          if not TouchRead then
          begin
            Frame := TyroInput.TouchFrame; //one read per beat, shared by the beat
            TouchRead := True;
          end;
          if Frame.Pressed then
          begin
            //on("touch", fn) hears (x, y): where the finger (or the mouse) landed
            Lua.State.PushInteger(Frame.X);
            Lua.State.PushInteger(Frame.Y);
            Dispatch(H.Name, Lua.State, lua_gettop(Lua.State) - 1, 2);
            lua_pop(Lua.State, 2);
          end;
        end;
    end;
    if Stopped then
      Exit; //a binding called exit(); the sweep waits for the next dispatch
  end;
end;

// on(name, fn) -> handler id
// Hook a callback to a name. emit("name", ...) runs every handler hooked to
// it, in hooking order, passing the extra values straight through. Some names
// the engine itself emits: a key name (on("space", fn), on("w", fn)) and
// "mouse.left" / "mouse.right" / "mouse.middle" fire on the press beat;
// "collision" hears the physics queue as (a, b, "enter"/"leave"); "update"
// runs every frame with the dt, "draw" every paint. Keys here are shortcuts,
// not locks: the same press can also be polled by keyboard.pressed("space")
// in update(), and both see it. Names ignore case. Avoid F1..F8, the editor's
// own keys - they reach those too.
function TLuaScript.On_func(L: Plua_State): integer; cdecl;
var
  H: TLuaHandler;
  Btn: string;
  Button: RayLib.TMouseButton;
begin
  if (Handlers = nil) or (Lua.State = nil) then
  begin
    L.PushNil;
    L.PushString('on: not running inside a script');
    Exit(2);
  end;
  if not lua_isstring(L, 1) then
  begin
    L.PushNil;
    L.PushString('on: the first argument is the event name, like "hit" or "space"');
    Exit(2);
  end;
  if not lua_isfunction(L, 2) then
  begin
    L.PushNil;
    L.PushString('on: the second argument is the function to run');
    Exit(2);
  end;
  H := TLuaHandler.Create;
  H.Id := HandlerNextId;
  Inc(HandlerNextId);
  H.Name := LowerCase(L.ToString(1));
  H.Kind := cHandlerUser;
  if (H.Name <> cEventUpdate) and (H.Name <> cEventDraw) and (H.Name <> cEventCollision) then
  begin
    if KeyNameToEnum(H.Name) <> KEY_NULL then
      H.Kind := cHandlerKey
    else if H.Name = cEventTouch then
      H.Kind := cHandlerTouch // on("touch", fn): the beat a finger lands
    else if Copy(H.Name, 1, 6) = 'mouse.' then
    begin
      Btn := Copy(H.Name, 7, MaxInt);
      if TryMouseButtonNameToEnum(Btn, Button) then
      begin
        H.Kind := cHandlerMouse;
        H.Btn := Btn;
      end;
    end;
  end;
  lua_pushvalue(L, 2);
  H.FnRef := luaL_ref(L, LUA_REGISTRYINDEX);
  H.Dead := False;
  Handlers.Add(H);
  L.PushInteger(H.Id);
  Result := 1;
end;

// off(id) -> true when a live handler was unhooked, false for an unknown or
// already-gone id. An off()'d callback never runs again, even mid-dispatch.
function TLuaScript.Off_func(L: Plua_State): integer; cdecl;
var
  H: TLuaHandler;
begin
  Result := 1;
  H := FindHandler(L.ToInteger(1));
  if H <> nil then
  begin
    H.Dead := True;
    L.PushBoolean(True);
    if Lua.State <> nil then
      SweepHandlers;
  end
  else
    L.PushBoolean(False);
end;

// emit(name, ...) -> number of handlers that ran
// The script's own turn: every on(name) handler runs now, in hook order,
// with the extra values as arguments. Returns 0 (not an error) when nothing
// listens, so games can fire optional hooks: if emit("boss_hit", dmg) == 0
// then nobody was watching.
function TLuaScript.Emit_func(L: Plua_State): integer; cdecl;
var
  Name: string;
begin
  if not lua_isstring(L, 1) then
  begin
    L.PushNil;
    L.PushString('emit: the first argument is the event name');
    Exit(2);
  end;
  Name := LowerCase(L.ToString(1));
  if lua_gettop(L) > 1 then
    L.PushInteger(Dispatch(Name, L, 2, lua_gettop(L) - 1))
  else
    L.PushInteger(Dispatch(Name, L, 0, 0));
  Result := 1;
end;

// Runs a single console line on the persistent Lua state (same globals as the
// Engine script), so assignments like "x = 5" stay alive for later lines. Any
// return values are echoed to the terminal (REPL style); syntax/runtime errors
// are reported through AOutput.
function TLuaScript.RunLine(const ALine: string; out AOutput: string): Boolean;
var
  n: Integer;
  p: PUTF8Char;
  S, Msg, AChunk: UTF8String;

  //pcall the chunk already loaded on the stack and echo any return values
  //(REPL style); runtime errors are reported through AOutput
  procedure RunChunk;
  var
    i: Integer;
  begin
    if lua_pcall(Lua.State, 0, LUA_MULTRET, 0) = LUA_OK then
    begin
      Result := True;
      n := lua_gettop(Lua.State);
      S := '';
      for i := 1 to n do
      begin
        if i > 1 then
          S := S + #9;
        //luaL_tolstring pushes a string representation of the value at index i
        //onto the top of the stack; pop that copy and concatenate it. The
        //original return values remain below and are discarded after the loop.
        p := luaL_tolstring(Lua.State, i, nil);
        S := S + PUTF8Char(p);
        lua_pop(Lua.State, 1);
      end;
      if S <> '' then
        AOutput := S;
        //Log.WriteLn(S);
      //Discard the converted return values from the persistent Lua stack.
      lua_pop(Lua.State, n);
    end
    else
    begin
      AOutput := Lua.State.ToString(-1);
      Lua.State.Pop(1);
    end;
  end;

begin
  Result := False;
  AOutput := '';
  if Lua.State = nil then
    Exit;
  //A previous Stop() left this state terminated. A console line starts a new
  //execution in the same persistent Lua state.
  Lua.SetReady;
  AChunk := ALine;
  if luaL_loadstring(Lua.State, PUTF8Char(AChunk)) = 0 then
    RunChunk
  else
  begin
    Msg := lua_tostring(Lua.State, -1);
    lua_pop(Lua.State, 1);
    //bare expression ("x", "1 + 1"): retry as "return ..." so it prints a value
    if luaL_loadstring(Lua.State, PUTF8Char('return ' + AChunk)) = 0 then
      RunChunk
    else
    begin
      lua_pop(Lua.State, 1);
      AOutput := Msg; //genuine syntax error: report the first one
    end;
  end;
end;

procedure TLuaScript.AddQueueObject(AQueueObject: TQueueObject);
{$ifdef DEBUG_LUA}
var
  ar: lua_Debug;
{$endif}
begin
  {$ifdef DEBUG_LUA}
  //Debug-only: stamp the queued command with the offending Lua source line.
  if Lua.State.GetStack(1, ar) then
  begin
    Lua.State.GetInfo('nSl', ar);
    AQueueObject.LineNo := ar.currentline;
  end;
  {$endif}
  inherited;
end;

function TLuaCustomCanvas.Clear_func(L: Plua_State): integer; cdecl;
var
  c: integer;
begin
  c := L.ArgsCount;
  if c >= 1 then
  begin
    if L.IsNumber(1) then
      FScript.AddQueueObject(TClearObject.Create(Canvas, IntToColor(round(L.ToNumber(1)))))
    else if L.IsString(1) then
      FScript.AddQueueObject(TClearObject.Create(Canvas, StrToColor(L.ToString(1))))
    else
      FScript.AddQueueObject(TClearObject.Create(Canvas));
  end
  else
    FScript.AddQueueObject(TClearObject.Create(Canvas));
  Result := 0;
end;

function TLuaWindow.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  w, h: integer;
begin
  c := L.ArgsCount;
  w := ScreenWidth;
  h := ScreenHeight;
  if c > 0 then
    w := round(L.ToNumber(1));
  if c > 1 then
    h := round(L.ToNumber(2));
  FScript.RunQueueObject(TWindowObject.Create(w, h));
  Result := 0;
end;

function TLuaConsole.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
begin
  c := L.ArgsCount;
  x := 0;
  y := Engine.World.Height div 2;
  w := Engine.World.Width;
  h := y;
  if c >= 2 then
  begin
    x := round(L.ToNumber(1));
    y := round(L.ToNumber(2));
  end;
  if c >= 4 then
  begin
    w := round(L.ToNumber(3));
    h := round(L.ToNumber(4));
  end;
  if (w > 0) and (h > 0) then
    FScript.RunQueueObject(TShowConsoleObject.Create(x, y, w, h))
  else
    FScript.RunQueueObject(TShowConsoleObject.Create(x, y));
  Result := 0;
end;

function TLuaCustomCanvas.Text_func(L: Plua_State): integer; cdecl;
var
  x, y: integer;
  s: string;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  s := L.ToString(3);
  FScript.AddQueueObject(TDrawTextObject.Create(Canvas, x, y, s));
  Result := 0;
end;

// canvas.text_center(y, s): draw s on the same line as text() but centred
// sideways on this sheet (a canvas/screen/world's width, or the sheet of
// images.new). Measured with Res.Font, exactly like textwidth(), so titles
// and game-over banners no longer need the (width - textwidth(s)) / 2 dance.
function TLuaCustomCanvas.TextCenter_func(L: Plua_State): integer; cdecl;
var
  x, y: integer;
  s: string;
  w: Integer;
begin
  y := round(L.ToNumber(1));
  s := L.ToString(2);
  w := Round(RayLib.MeasureTextEx(Res.Font.Data,
    PUTF8Char(UTF8String(s)), Res.Font.Height, 0).x);
  x := (Canvas.Width - w) div 2;
  FScript.AddQueueObject(TDrawTextObject.Create(Canvas, x, y, s));
  Result := 0;
end;

function TLuaCustomCanvas.Font_func(L: Plua_State): integer; cdecl;
var
  aFile, aCode: string;
  aSize: Integer;
  Shell: TRayFont;
begin
  //Short sugar over font.load + font.use: the two calls are queued in order,
  //so text drawn after this call uses the new face on the very next frame.
  aFile := Res.GuessFileName(L.ToString(1), Res.WorkPath);
  aCode := '';
  if L.IsNumber(2) then
    aSize := L.ToInteger(2)
  else
  begin
    aSize := 0;
    if L.ArgsCount >= 2 then
      aCode := L.ToString(2);
  end;
  if L.ArgsCount >= 3 then
    aCode := L.ToString(3);
  if (aSize <> 0) and ((aSize < 1) or (aSize > 256)) then
  begin
    if aSize < 1 then aSize := 1 else aSize := 256;
  end;
  Shell := Res.FontShell(aFile, aSize, aCode);
  FScript.AddQueueObject(TLoadFontObject.Create(Shell.FontId));
  FScript.AddQueueObject(TUseFontObject.Create(Shell.FontId));
  L.PushInteger(Shell.FontId);
  Result := 1;
end;

function TLuaCustomCanvas.Circle_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, r: integer;
  f: boolean;
begin
  f := False;
  c := L.ArgsCount;
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  r := round(L.ToNumber(3));
  if c >= 4 then
    f := L.ToBoolean(4);
  FScript.AddQueueObject(TDrawCircleObject.Create(Canvas, x, y, r, f));
  Result := 0;
end;

function TLuaCustomCanvas.Triangle_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x1, y1, x2, y2, x3, y3: integer;
  f: boolean;
begin
  f := False;
  c := L.ArgsCount;
  x1 := round(L.ToNumber(1));
  y1 := round(L.ToNumber(2));
  x2 := round(L.ToNumber(3));
  y2 := round(L.ToNumber(4));
  x3 := round(L.ToNumber(5));
  y3 := round(L.ToNumber(6));
  if c >= 7 then
    f := L.ToBoolean(7);
  FScript.AddQueueObject(TDrawTriangleObject.Create(Canvas, x1, y1, x2, y2, x3, y3, f));
  Result := 0;
end;

function TLuaCustomCanvas.Rectangle_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
  f: boolean;
begin
  f := False;
  c := L.ArgsCount;
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  w := round(L.ToNumber(3));
  h := round(L.ToNumber(4));
  if c >= 4 then
    f := L.ToBoolean(5);
  FScript.AddQueueObject(TDrawRectangleObject.Create(Canvas, x, y, w, h, f));
  Result := 0;
end;

// x.image(x, y, file [, scale]) paints a picture on the canvas: (x, y) is its
// top-left corner (like text and rectangles) and scale grows the file's own
// pixels (1 = native, omitted). The current color tints the picture, so setting
// x.alpha first fades it. The file is looked up the way every other script asset
// is (script folder, then the engine's own) and its texture is cached, so
// drawing the same picture every frame costs nothing.
function TLuaCustomCanvas.Image_func(L: Plua_State): integer;
cdecl;
var
  c: integer;
  x, y: integer;
  scale: Single;
begin
  c := L.ArgsCount;
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  scale := 1;
  if (c >= 4) and L.IsNumber(4) then
    scale := L.ToNumber(4);
  FScript.AddQueueObject(TDrawImageObject.Create(Canvas, x, y, L.ToString(3), scale));
  Result := 0;
end;

// canvas gradient colors take the same forms as canvas.color/canvas.clear: a
// colors.* integer or a color name string. False means the argument was not
// given, so the draw command keeps using the canvas color for that side.
function CanvasColorArg(L: Plua_State; Index: Integer; out Color: TColor): Boolean;
begin
  Color := clBlank;
  if L.IsString(Index) then
  begin
    Color := StrToColor(L.ToString(Index));
    Result := True;
  end
  else if L.IsNumber(Index) then
  begin
    Color := IntToColor(round(L.ToNumber(Index)));
    Result := True;
  end
  else
    Result := False;
end;

// canvas.gradientV(x, y, w, h [, startColor, endColor]) -- top to bottom
function TLuaCustomCanvas.GradientV_func(L: Plua_State): integer; cdecl;
var
  x, y, w, h: integer;
  startColor, endColor: TColor;
  o: TDrawGradientObject;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  w := round(L.ToNumber(3));
  h := round(L.ToNumber(4));
  o := TDrawGradientObject.Create(Canvas, x, y, w, h, clBlank, clBlank, True);
  o.fHasStartColor := CanvasColorArg(L, 5, startColor);
  o.fStartColor := startColor;
  o.fHasEndColor := CanvasColorArg(L, 6, endColor);
  o.fEndColor := endColor;
  FScript.AddQueueObject(o);
  Result := 0;
end;

// canvas.gradientH(x, y, w, h [, startColor, endColor]) -- left to right
function TLuaCustomCanvas.GradientH_func(L: Plua_State): integer; cdecl;
var
  x, y, w, h: integer;
  startColor, endColor: TColor;
  o: TDrawGradientObject;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  w := round(L.ToNumber(3));
  h := round(L.ToNumber(4));
  o := TDrawGradientObject.Create(Canvas, x, y, w, h, clBlank, clBlank, False);
  o.fHasStartColor := CanvasColorArg(L, 5, startColor);
  o.fStartColor := startColor;
  o.fHasEndColor := CanvasColorArg(L, 6, endColor);
  o.fEndColor := endColor;
  FScript.AddQueueObject(o);
  Result := 0;
end;

// canvas.gradient.circle(x, y, r [, innerColor, outerColor]) -- radial gradient
function TLuaCustomCanvas.GradientCircle_func(L: Plua_State): integer; cdecl;
var
  x, y, r: integer;
  innerColor, outerColor: TColor;
  o: TDrawCircleGradientObject;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  r := round(L.ToNumber(3));
  o := TDrawCircleGradientObject.Create(Canvas, x, y, r, clBlank, clBlank);
  o.fHasInnerColor := CanvasColorArg(L, 4, innerColor);
  o.fInnerColor := innerColor;
  o.fHasOuterColor := CanvasColorArg(L, 5, outerColor);
  o.fOuterColor := outerColor;
  FScript.AddQueueObject(o);
  Result := 0;
end;

function TLuaCustomCanvas.Line_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x1, y1, x2, y2, w: integer;
begin
  c := L.ArgsCount;
  x1 := round(L.ToNumber(1));
  y1 := round(L.ToNumber(2));
  if c >= 4 then
  begin
    x2 := round(L.ToNumber(3));
    y2 := round(L.ToNumber(4));
    //Optional 5th argument is a per-call thickness; with none the canvas pen
    //size set through canvas.pen applies.
    w := 0;
    if c >= 5 then
      w := round(L.ToNumber(5));
    if w > 0 then
      FScript.AddQueueObject(TDrawLineObject.Create(Canvas, x1, y1, x2, y2, w))
    else
      FScript.AddQueueObject(TDrawLineObject.Create(Canvas, x1, y1, x2, y2));
  end
  else
    FScript.AddQueueObject(TDrawLineToObject.Create(Canvas, x1, y1));
  Result := 0;
end;

function TLuaCustomCanvas.Point_func(L: Plua_State): integer; cdecl;
var
  x, y: integer;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  FScript.AddQueueObject(TDrawPointObject.Create(Canvas, x, y));
  Result := 0;
end;

// canvas.test(ms [, x, y, w, h]) -- diagnostic: enqueue a drawing command that
// takes about ms milliseconds to execute on the main thread (it draws a marker
// rectangle, then spins). Use it to emulate slow drawing: a single draw call
// longer than a frame, to check that a queued body is still committed as one
// group. Defaults: ms = 50, a full-width 2px bar.
function TLuaCustomCanvas.Test_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  ms, x, y, w, h: integer;
  f: Boolean;
begin
  c := L.ArgsCount;
  ms := 50;
  if c >= 1 then
    ms := round(L.ToNumber(1));
  if ms < 0 then
    ms := 0;
  x := 0;
  y := 0;
  w := Canvas.Width;
  h := 2;
  f := True;
  if c >= 5 then
  begin
    x := round(L.ToNumber(2));
    y := round(L.ToNumber(3));
    w := round(L.ToNumber(4));
    h := round(L.ToNumber(5));
  end;
  FScript.AddQueueObject(TSlowDrawObject.Create(Canvas, ms, x, y, w, h, f));
  Result := 0;
end;

{ TLuaCanvas }

constructor TLuaCanvas.Create(const AName: string; AScript: TLuaScript);
begin
  inherited;
  Canvas := Engine.Board;
end;

{ TLuaScreen }

constructor TLuaScreen.Create(const AName: string; AScript: TLuaScript);
begin
  inherited;
  Canvas := Engine.Screen;
end;

function TLuaConsole.Print_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(s, False)); //TODO
  Result := 0;
end;

function TLuaConsole.PrintLn_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
begin
  c := L.ArgsCount;
  s := '';
  for i := 1 to c do
  begin
    if i > 1 then
      s := s + #9;
    s := s + L.ToString(i);
  end;
  FScript.AddQueueObject(TPrintObject.Create(s, True));//TODO
  Result := 0;
end;

{ TLuaOutput }

function TLuaOutput.Setter(L: PLua_State): integer;
var
  i: integer;
  field: string;
  r: TRect;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'visible' then
    Engine.Output.Visible := L.ToBoolean(-1)
  else if L.IsInteger(-1) or L.IsNumber(-1) then
  begin
    i := L.ToInteger(-1);
    if field = 'height' then
      Engine.Output.Height := i
    else if field = 'width' then
      Engine.Output.Width := i
    else if (field = 'left') or (field = 'x') then
    begin
      r := Engine.Output.BoundsRect;
      Engine.Output.BoundsRect := Rect(i, r.Top, i + r.Width, r.Bottom);
    end
    else if (field = 'top') or (field = 'y') then
    begin
      r := Engine.Output.BoundsRect;
      Engine.Output.BoundsRect := Rect(r.Left, i, r.Right, i + r.Height);
    end
    else if field = 'margin' then
      Engine.Output.Margin := i
    else if field = 'maxlines' then
      Engine.Output.MaxLines := i
    else if field = 'border' then
      //* 0=none, 1=thin, 2=thick, 3=sizable
      case i of
        1: Engine.Output.Border := brdThin;
        2: Engine.Output.Border := brdThick;
        3: Engine.Output.Border := brdSizable;
      else
        Engine.Output.Border := brdNone;
      end;
  end
  else if L.IsString(-1) then
  begin
    if field = 'color' then
      Engine.Output.Color := StrToColor(L.ToString(-1))
    else if field = 'backColor' then
      Engine.Output.BackColor := StrToColor(L.ToString(-1));
  end;
end;

function TLuaOutput.Getter(L: Plua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'visible' then
  begin
    lua_pushboolean(L, Engine.Output.Visible);
    Result := 1;
  end
  else if field = 'height' then
  begin
    lua_pushinteger(L, Engine.Output.Height);
    Result := 1;
  end
  else if field = 'width' then
  begin
    lua_pushinteger(L, Engine.Output.Width);
    Result := 1;
  end
  else if (field = 'left') or (field = 'x') then
  begin
    lua_pushinteger(L, Engine.Output.BoundsRect.Left);
    Result := 1;
  end
  else if (field = 'top') or (field = 'y') then
  begin
    lua_pushinteger(L, Engine.Output.BoundsRect.Top);
    Result := 1;
  end
  else if field = 'border' then
  begin
    case Engine.Output.Border of
      brdThin: lua_pushinteger(L, 1);
      brdThick: lua_pushinteger(L, 2);
      brdSizable: lua_pushinteger(L, 3);
    else
      lua_pushinteger(L, 0);
    end;
    Result := 1;
  end
  else if field = 'lines' then
  begin
    lua_pushinteger(L, Engine.Output.LineCount);
    Result := 1;
  end
  else if field = 'maxlines' then
  begin
    lua_pushinteger(L, Engine.Output.MaxLines);
    Result := 1;
  end
  else if field = 'margin' then
  begin
    lua_pushinteger(L, Engine.Output.Margin);
    Result := 1;
  end;
end;

// output.show() | output.show(x, y) | output.show(x, y, w, h) -> show and place
function TLuaOutput.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
begin
  c := L.ArgsCount;
  x := 0;
  y := 0;
  w := 0;
  h := 0;
  if c >= 2 then
  begin
    x := round(L.ToNumber(1));
    y := round(L.ToNumber(2));
  end;
  if c >= 4 then
  begin
    w := round(L.ToNumber(3));
    h := round(L.ToNumber(4));
  end;
  if (w > 0) and (h > 0) then
    FScript.RunQueueObject(TShowOutputObject.Create(x, y, w, h))
  else
    FScript.RunQueueObject(TShowOutputObject.Create(x, y));
  Result := 0;
end;

function TLuaOutput.Hide_func(L: Plua_State): integer; cdecl;
begin
  FScript.RunQueueObject(THideOutputObject.Create);
  Result := 0;
end;

function TLuaOutput.Clear_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TClearOutputObject.Create);
  Result := 0;
end;

//A duration passed from Lua is milliseconds; the waveform generator allocates
//SampleRate * Seconds samples, so an outrageous value must never reach it.
//Ten seconds is already far longer than any game effect needs.
const
  cMaxSoundPeriodMs = 10000;

function ClampSoundPeriod(APeriod: Integer): Integer;
begin
  if APeriod > cMaxSoundPeriodMs then
  begin
    Log.WriteLn(Format('Hint: sound duration %d ms is too long; using %d ms',
      [APeriod, cMaxSoundPeriodMs]));
    Result := cMaxSoundPeriodMs;
  end
  else
    Result := APeriod;
end;

{ TLuaMusic }

function TLuaMusic.Beep_func(L: Plua_State): integer; cdecl;
var
  c, Period: integer;
begin
  c := L.ArgsCount;
  if c = 1 then
  begin
    Period := round(L.ToNumber(1));
  end
  else
    Period := 500;
  FScript.AddQueueObject(TBeepObject.Create(ClampSoundPeriod(Period)));
  Result := 0;
end;

function TLuaMusic.Play_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  s := L.ToString(1);
  s := Res.GuessFileName(s, Res.WorkPath);
  FScript.AddQueueObject(TPlayMusicFileObject.Create(s));
  Result := 0;
end;

function TLuaMusic.MML_func(L: Plua_State): integer; cdecl;
var
  i, c: integer;
  s: string;
  Song: TmmlSong;
  Loop: Boolean;
begin
  Song := nil;
  Loop := False;
  c := L.ArgsCount;
  //Optional trailing boolean: music.mml(song1, song2, ..., true) replays the
  //tune endlessly until music.stop().
  if (c > 0) and lua_isboolean(L, c) then
  begin
    Loop := L.ToBoolean(c);
    c := c - 1;
  end;
  SetLength(Song, c);
  for i := 0 to c - 1 do
  begin
    s := L.ToString(i + 1);
    Song[i] := s;
  end;
  //Audio objects and raylib audio calls stay on the Engine thread. Playback is
  //advanced incrementally by TTyroMain.Update, so this does not block drawing.
  FScript.AddQueueObject(TPlayMMLObject.Create(Song, Loop));
  Result := 0;
end;

function TLuaMusic.Stop_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TStopMMLObject.Create);
  Result := 0;
end;

function TLuaMusic.Sound_func(L: Plua_State): integer; cdecl;
var
  Freq, Period: integer;
  WaveName: string;
begin
  //Use lua_type, not L.IsString: lua_isstring is also true for numbers, so
  //music.sound(freq, ms) would be misread as music.sound(name, freq [, ms]).
  if L.GetType(1) = LUA_TSTRING then
  begin
    WaveName := L.ToString(1);
    Freq := round(L.ToNumber(2));
    if l.ArgsCount = 3 then
      Period := round(L.ToNumber(3))
    else
      Period := 500;
  end
  else
  begin
    WaveName := '';
    Freq := round(L.ToNumber(1));
    if l.ArgsCount = 2 then
      Period := round(L.ToNumber(2))
    else
      Period := 500;
  end;
  Script.AddQueueObject(TPlaySoundObject.Create(Freq, ClampSoundPeriod(Period), WaveName));
  Result := 0;
end;

{ TLuaSound }

function TLuaSound.Noise_func(L: Plua_State): integer; cdecl;
var
  c, Freq, Period: integer;
begin
  c := L.ArgsCount;
  if c = 1 then
  begin
    Freq := 0;
    Period := round(L.ToNumber(1));
    FScript.AddQueueObject(TPlayNoiseObject.Create(Freq, ClampSoundPeriod(Period)));
  end
  else
  begin
    Freq := round(L.ToNumber(1));
    Period := round(L.ToNumber(2));
    FScript.AddQueueObject(TPlayNoiseObject.Create(Freq, ClampSoundPeriod(Period)));
  end;
  Result := 0;
end;

function TLuaSound.Beep_func(L: Plua_State): integer; cdecl;
var
  c, Period: integer;
begin
  c := L.ArgsCount;
  if c = 1 then
  begin
    Period := round(L.ToNumber(1));
  end
  else
    Period := 500;
  FScript.AddQueueObject(TBeepObject.Create(ClampSoundPeriod(Period)));
  Result := 0;
end;

function TLuaSound.Load_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // sound.load("file.wav") - decode a file once and keep it resident so later
  // sound.play calls are instant. The file is resolved like every other script
  // asset (script dir, workspace assets/, app folder). A missing file is
  // ignored, like sound.play; the cached sound stays until sound.unload.
  s := L.ToString(1);
  s := Res.GuessFileName(s, Res.WorkPath);
  FScript.AddQueueObject(TLoadSoundFileObject.Create(s));
  Result := 0;
end;

function TLuaSound.Unload_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // sound.unload("file.wav") - release a sound.load()ed file from the cache.
  s := L.ToString(1);
  s := Res.GuessFileName(s, Res.WorkPath);
  FScript.AddQueueObject(TUnloadSoundFileObject.Create(s));
  Result := 0;
end;

function TLuaSound.Play_func(L: Plua_State): integer; cdecl;
var
  WaveName, ResolvedFile: string;
  Freq, Period: integer;
  Volume: Single;
  Success: Boolean;
begin
  Success := False;
  try
    if (L.ArgsCount > 0) and not L.IsNil(1) then
    begin
      if lua_type(L, 1) = LUA_TNUMBER then
      begin
        // sound.play(freq [, ms])
        Freq := round(L.ToNumber(1));
        if Freq > 0 then
        begin
          if L.ArgsCount >= 2 then
            Period := round(L.ToNumber(2))
          else
            Period := 500;
          if Period <= 0 then
            Period := 500;
          Script.AddQueueObject(TPlaySoundObject.Create(Freq, ClampSoundPeriod(Period), ''));
          Success := True;
        end;
      end
      else if lua_type(L, 1) = LUA_TSTRING then
      begin
        WaveName := L.ToString(1);
        if WaveName <> '' then
        begin
          if SysUtils.ExtractFileExt(WaveName) <> '' then
          begin
            // sound.play("file.wav" [, volume]) - one-shot SFX from an audio file
            Volume := 1;
            if L.ArgsCount >= 2 then
              Volume := L.ToNumber(2);
            ResolvedFile := Res.GuessFileName(WaveName, Res.WorkPath);
            if (ResolvedFile <> '') and SysUtils.FileExists(ResolvedFile) then
            begin
              FScript.AddQueueObject(TPlaySoundFileObject.Create(ResolvedFile, Volume));
              Success := True;
            end
            else if (SoundCache <> nil) and (SoundCache.Find(ResolvedFile) <> nil) then
            begin
              FScript.AddQueueObject(TPlaySoundFileObject.Create(ResolvedFile, Volume));
              Success := True;
            end;
          end
          else
          begin
            // sound.play("WaveformName" [, freq [, ms]])
            Freq := 440;
            Period := 500;
            if L.ArgsCount >= 2 then
              Freq := round(L.ToNumber(2));
            if L.ArgsCount >= 3 then
              Period := round(L.ToNumber(3));
            if Freq <= 0 then
              Freq := 440;
            if Period <= 0 then
              Period := 500;
            if (WaveForms = nil) or (WaveForms.Find(WaveName) <> nil) then
            begin
              Script.AddQueueObject(TPlaySoundObject.Create(Freq, ClampSoundPeriod(Period), WaveName));
              Success := True;
            end;
          end;
        end;
      end;
    end;
  except
    Success := False;
  end;
  L.PushBoolean(Success);
  Result := 1;
end;

function TLuaSound.LoopPlay_func(L: Plua_State): integer; cdecl;
var
  WaveName: string;
  Freq, Volume: Single;
  c: Integer;
begin
  // sound.loopPlay(freq [, volume])
  // sound.loopPlay(wave, freq [, volume])
  c := L.ArgsCount;
  Volume := 1;
  //lua_type, not L.IsString: lua_isstring is true for numbers too.
  if lua_type(L, 1) = LUA_TSTRING then
  begin
    WaveName := L.ToString(1);
    Freq := L.ToNumber(2);
    if c >= 3 then
      Volume := L.ToNumber(3);
  end
  else
  begin
    WaveName := '';
    Freq := L.ToNumber(1);
    if c >= 2 then
      Volume := L.ToNumber(2);
  end;
  FScript.AddQueueObject(TStartLoopSoundObject.Create(WaveName, Freq, Volume));
  Result := 0;
end;

function TLuaSound.LoopPitch_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TSetLoopPitchObject.Create(L.ToNumber(1)));
  Result := 0;
end;

function TLuaSound.LoopVolume_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TSetLoopVolumeObject.Create(L.ToNumber(1)));
  Result := 0;
end;

function TLuaSound.LoopStop_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TStopLoopSoundObject.Create);
  Result := 0;
end;

function TLuaSound.Generate_func(L: Plua_State): integer; cdecl;
var
  aName, aKind, aResolved: string;
  aFreq, aMs: Integer;
  aVolume: Double;
begin
  if not L.IsString(1) then
  begin
    L.PushNil;
    L.PushString('sound.generate: missing file name');
    Exit(2);
  end;
  aName := L.ToString(1);
  aFreq := L.ToInteger(2);
  aMs := L.ToInteger(3);
  aKind := 'sine';
  if L.ArgsCount >= 4 then
    aKind := L.ToString(4);
  aVolume := 0.5;
  if L.ArgsCount >= 5 then
    aVolume := L.ToNumber(5);
  aResolved := ResolveAndValidatePath(aName, Res.WorkPath);
  if aResolved = '' then
  begin
    L.PushNil;
    L.PushString('sound.generate: ''' + aName + ''' resolves outside the workspace');
    Exit(2);
  end;
  if GenerateWaveFile(aResolved, aFreq, aMs, aKind, aVolume) then
  begin
    L.PushBoolean(True);
    Result := 1;
  end
  else
  begin
    L.PushNil;
    L.PushString('sound.generate: bad arguments or cannot write ' + aName);
    Exit(2);
  end;
end;

{ TLuaRadio }

function TLuaRadio.Getter(L: Plua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'title' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Title)));
    Result := 1;
  end
  else if field = 'station' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Station)));
    Result := 1;
  end
  else if field = 'genre' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Genre)));
    Result := 1;
  end
  else if field = 'bitrate' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Bitrate)));
    Result := 1;
  end
  else if field = 'url' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.URL)));
    Result := 1;
  end
  else if field = 'state' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.StateString)));
    Result := 1;
  end
  else if field = 'error' then
  begin
    lua_pushstring(L, PAnsiChar(AnsiString(RadioPlayer.Error)));
    Result := 1;
  end
  else if field = 'playing' then
  begin
    lua_pushboolean(L, RadioPlayer.Playing);
    Result := 1;
  end
  else if field = 'buffered' then
  begin
    lua_pushinteger(L, RadioPlayer.Buffered);
    Result := 1;
  end;
end;

function TLuaRadio.Setter(L: PLua_State): integer;
begin
  Result := 0;
end;

function TLuaRadio.Play_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raPlay, L.ToString(1)));
  Result := 0;
end;

function TLuaRadio.Pause_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raPause));
  Result := 0;
end;

function TLuaRadio.Resume_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raResume));
  Result := 0;
end;

function TLuaRadio.Stop_func(L: Plua_State): integer; cdecl;
begin
  FScript.AddQueueObject(TRadioPlayObject.Create(raStop));
  Result := 0;
end;

{ TRadioPlayObject }

constructor TRadioPlayObject.Create(AAction: TRadioAction);
begin
  inherited Create;
  FAction := AAction;
end;

constructor TRadioPlayObject.Create(AAction: TRadioAction; const AURL: string);
begin
  Create(AAction);
  FURL := AURL;
end;

procedure TRadioPlayObject.DoExecute;
begin
  if RadioPlayer = nil then
    Exit;
  case FAction of
    raPlay: RadioPlayer.Play(FURL);
    raPause: RadioPlayer.Pause;
    raResume: RadioPlayer.Resume;
    raStop: RadioPlayer.Stop;
  end;
end;

{ TLuaSpectrum }

function TLuaSpectrum.Getter(L: Plua_State): Integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'active' then
  begin
    lua_pushboolean(L, Spectrum.Active);
    Result := 1;
  end
  else if field = 'bars' then
  begin
    lua_pushinteger(L, Spectrum.Bars);
    Result := 1;
  end
  else if field = 'visible' then
  begin
    lua_pushboolean(L, Spectrum.Visible);
    Result := 1;
  end;
end;

function TLuaSpectrum.Setter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if (field = 'bars') and L.IsInteger(-1) then
    Spectrum.RequestedBars := L.ToInteger(-1);
end;

function TLuaSpectrum.Show_func(L: Plua_State): integer; cdecl;
var
  c: integer;
  x, y, w, h: integer;
begin
  c := L.ArgsCount;
  x := 120;
  y := 80;
  w := 400;
  h := 200;
  if c >= 2 then
  begin
    x := round(L.ToNumber(1));
    y := round(L.ToNumber(2));
  end;
  if c >= 4 then
  begin
    w := round(L.ToNumber(3));
    h := round(L.ToNumber(4));
  end;
  FScript.RunQueueObject(TShowSpectrumObject.Create(x, y, w, h));
  Result := 0;
end;

function TLuaSpectrum.Hide_func(L: Plua_State): integer; cdecl;
begin
  FScript.RunQueueObject(THideSpectrumObject.Create);
  Result := 0;
end;

{ TShowSpectrumObject }

constructor TShowSpectrumObject.Create(AX, AY, AW, AH: Integer);
begin
  inherited Create;
  FX := AX;
  FY := AY;
  FW := AW;
  FH := AH;
end;

procedure TShowSpectrumObject.DoExecute;
begin
  Spectrum.Show(FX, FY, FW, FH);
end;

{ THideSpectrumObject }

procedure THideSpectrumObject.DoExecute;
begin
  Spectrum.Hide;
end;

{ TLuaMouse }

function TLuaMouse.Getter(L: Plua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'x' then
  begin
    L.PushInteger(TyroInput.MouseX);
    Result := 1;
  end
  else if field = 'y' then
  begin
    L.PushInteger(TyroInput.MouseY);
    Result := 1;
  end
  else if field = 'wheel' then
  begin
    // mouse.wheel - wheel movement this frame in scroll steps (0 = no scroll)
    L.PushNumber(TyroInput.MouseWheelMove);
    Result := 1;
  end;
end;

{ TLuaQueue }

function TLuaQueue.Getter(L: Plua_State): integer;
var
  field: string;
  Stats: TQueueStats;
begin
  Result := 0;
  field := L.ToString(2);
  Stats := Default(TQueueStats);
  if Engine <> nil then
    Stats := Engine.QueueStats;
  if field = 'depth' then
    L.PushInteger(Stats.Depth)
  else if field = 'holds' then
    L.PushInteger(Stats.Holds)
  else if field = 'frames' then
    L.PushInteger(Stats.Frames)
  else if field = 'drainms' then
    L.PushNumber(Stats.DrainMs)
  else if field = 'lastgap' then
    L.PushInteger(Stats.LastGap)
  else if field = 'maxgap' then
    L.PushInteger(Stats.MaxGap)
  else if field = 'limit' then
    L.PushInteger(Stats.Limit)
  else if field = 'waits' then
    L.PushInteger(Stats.Waits)
  else if field = 'blocked' then
    L.PushInteger(Stats.Blocked)
  else
    Exit;
  Result := 1;
end;

function TLuaMouse.Pressed_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // mouse.pressed(["left"]) - true once on the press edge
  s := L.ToString(1);
  if s = '' then
    s := 'left';
  L.PushBoolean(TyroInput.IsMouseButtonPressed(s));
  Result := 1;
end;

function TLuaMouse.Down_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // mouse.down(["left"]) - true every frame while the button is held
  s := L.ToString(1);
  if s = '' then
    s := 'left';
  L.PushBoolean(TyroInput.IsMouseButtonDown(s));
  Result := 1;
end;

function TLuaMouse.Released_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // mouse.released(["left"]) - true once on the release edge
  s := L.ToString(1);
  if s = '' then
    s := 'left';
  L.PushBoolean(TyroInput.IsMouseButtonReleased(s));
  Result := 1;
end;

function TLuaMouse.Up_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // mouse.up(["left"]) - true while the button is not held
  s := L.ToString(1);
  if s = '' then
    s := 'left';
  L.PushBoolean(TyroInput.IsMouseButtonUp(s));
  Result := 1;
end;

{ TLuaTouch }

//Every field answers the frame the engine stepped at the top of this beat, so
//polling twice in one frame answers the same thing, and the numbers agree with
//each other. On a tablet those frames are raylib's own points; on a desktop
//the mouse stands in for a finger, which is what lets one script run on both.
function TLuaTouch.Getter(L: Plua_State): integer;
var
  Field: string;
  Frame: TyroInput.TTouchFrame;
begin
  Result := 0;
  Field := L.ToString(2);
  Frame := TyroInput.TouchFrame;
  if Field = 'x' then
    L.PushInteger(Frame.X)
  else if Field = 'y' then
    L.PushInteger(Frame.Y)
  else if Field = 'count' then
    L.PushInteger(Frame.Count)
  else if Field = 'down' then
    L.PushBoolean(Frame.Down)
  else if Field = 'up' then
    L.PushBoolean(not Frame.Down)
  else if Field = 'pressed' then
    L.PushBoolean(Frame.Pressed)
  else if Field = 'released' then
    L.PushBoolean(Frame.Released)
  else if Field = 'mouse' then
    L.PushBoolean(Frame.FromMouse) // the cursor is standing in for a finger
  else
    Exit;
  Result := 1;
end;

//touch.point(i) -> x, y | nil
//The i-th contact of this frame, counted from 1 in the order the glass felt
//them. Out of range (or nothing touching) answers nil, so a walk over the
//fingers is a plain `for i = 1, touch.count do`.
function TLuaTouch.Point_func(L: Plua_State): integer; cdecl;
var
  I: Integer;
  Frame: TyroInput.TTouchFrame;
begin
  I := L.ToInteger(1);
  Frame := TyroInput.TouchFrame;
  if (I < 1) or (I > Frame.Count) then
  begin
    L.PushNil;
    Exit(1);
  end;
  L.PushInteger(Frame.Points[I - 1].X);
  L.PushInteger(Frame.Points[I - 1].Y);
  Result := 2;
end;

{ TLuaKeyboard }

function TLuaKeyboard.Pressed_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // keyboard.pressed("space") - true once on the press edge
  s := L.ToString(1);
  L.PushBoolean(TyroInput.IsKeyPressed(s));
  Result := 1;
end;

function TLuaKeyboard.Down_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // keyboard.down("a") - true every frame while held
  s := L.ToString(1);
  L.PushBoolean(TyroInput.IsKeyDown(s));
  Result := 1;
end;

function TLuaKeyboard.Released_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // keyboard.released("space") - true once on the release edge
  s := L.ToString(1);
  L.PushBoolean(TyroInput.IsKeyReleased(s));
  Result := 1;
end;

function TLuaKeyboard.Up_func(L: Plua_State): integer; cdecl;
var
  s: string;
begin
  // keyboard.up("a") - true while the key is not held
  s := L.ToString(1);
  L.PushBoolean(TyroInput.IsKeyUp(s));
  Result := 1;
end;

{ TLuaGamepad }

function TLuaGamepad.Pressed_func(L: Plua_State): integer; cdecl;
var
  s: string;
  Pad: Integer;
begin
  // gamepad.pressed("a" [, gamepad]) - true once on the press edge (any pad if omitted)
  s := L.ToString(1);
  Pad := -1;
  if L.ArgsCount >= 2 then
    Pad := round(L.ToNumber(2));
  L.PushBoolean(TyroInput.IsGamepadPressed(s, Pad));
  Result := 1;
end;

function TLuaGamepad.Down_func(L: Plua_State): integer; cdecl;
var
  s: string;
  Pad: Integer;
begin
  // gamepad.down("a" [, gamepad]) - true every frame while held (any pad if omitted)
  s := L.ToString(1);
  Pad := -1;
  if L.ArgsCount >= 2 then
    Pad := round(L.ToNumber(2));
  L.PushBoolean(TyroInput.IsGamepadDown(s, Pad));
  Result := 1;
end;

function TLuaGamepad.Released_func(L: Plua_State): integer; cdecl;
var
  s: string;
  Pad: Integer;
begin
  // gamepad.released("a" [, gamepad]) - true once on the release edge (any pad if omitted)
  s := L.ToString(1);
  Pad := -1;
  if L.ArgsCount >= 2 then
    Pad := round(L.ToNumber(2));
  L.PushBoolean(TyroInput.IsGamepadReleased(s, Pad));
  Result := 1;
end;

function TLuaGamepad.Up_func(L: Plua_State): integer; cdecl;
var
  s: string;
  Pad: Integer;
begin
  // gamepad.up("a" [, gamepad]) - true when button is not held
  s := L.ToString(1);
  Pad := -1;
  if L.ArgsCount >= 2 then
    Pad := round(L.ToNumber(2));
  L.PushBoolean(TyroInput.IsGamepadUp(s, Pad));
  Result := 1;
end;

function TLuaGamepad.Axis_func(L: Plua_State): integer; cdecl;
var
  s: string;
  Pad: Integer;
begin
  // gamepad.axis("leftx" [, gamepad]) - axis movement -1..1 (0 if absent, active pad if omitted)
  s := L.ToString(1);
  Pad := -1;
  if L.ArgsCount >= 2 then
    Pad := round(L.ToNumber(2));
  L.PushNumber(TyroInput.GamepadAxis(s, Pad));
  Result := 1;
end;

function TLuaGamepad.Available_func(L: Plua_State): integer; cdecl;
var
  Pad: Integer;
begin
  // gamepad.available([gamepad]) - true if connected (any gamepad if omitted)
  Pad := -1;
  if L.ArgsCount >= 1 then
    Pad := round(L.ToNumber(1));
  L.PushBoolean(TyroInput.GamepadAvailable(Pad));
  Result := 1;
end;

function TLuaGamepad.Name_func(L: Plua_State): integer; cdecl;
var
  Pad: Integer;
begin
  // gamepad.name([gamepad]) - controller name, '' when not connected
  Pad := -1;
  if L.ArgsCount >= 1 then
    Pad := round(L.ToNumber(1));
  L.PushString(TyroInput.GamepadName(Pad));
  Result := 1;
end;

function TLuaGamepad.Count_func(L: Plua_State): integer; cdecl;
begin
  // gamepad.count() - number of connected gamepads (0..4)
  L.PushInteger(TyroInput.GamepadCount);
  Result := 1;
end;

function TLuaGamepad.Axes_func(L: Plua_State): integer; cdecl;
var
  Pad: Integer;
begin
  // gamepad.axes([gamepad]) - number of axes
  Pad := -1;
  if L.ArgsCount >= 1 then
    Pad := round(L.ToNumber(1));
  L.PushInteger(TyroInput.GamepadAxisCount(Pad));
  Result := 1;
end;

function TLuaGamepad.Vibrate_func(L: Plua_State): integer; cdecl;
var
  LeftMotor, RightMotor, Duration: Single;
  Pad: Integer;
begin
  // gamepad.vibrate(leftMotor, rightMotor [, duration=0.1, gamepad=-1])
  LeftMotor := 0;
  RightMotor := 0;
  Duration := 0.1;
  Pad := -1;
  if L.ArgsCount >= 1 then
    LeftMotor := L.ToNumber(1);
  if L.ArgsCount >= 2 then
    RightMotor := L.ToNumber(2);
  if L.ArgsCount >= 3 then
    Duration := L.ToNumber(3);
  if L.ArgsCount >= 4 then
    Pad := round(L.ToNumber(4));
  TyroInput.GamepadVibrate(LeftMotor, RightMotor, Duration, Pad);
  Result := 0;
end;

function TLuaGamepad.Mappings_func(L: Plua_State): integer; cdecl;
begin
  // gamepad.mappings(text) - add SDL/GLFW gamepad mappings, nonzero on success
  L.PushNumber(TyroInput.SetGamepadMappings(L.ToString(1)));
  Result := 1;
end;

{ TLuaGame }

function TLuaGame.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'paused' then
  begin
    L.PushBoolean(Engine.GamePaused);
    Result := 1;
  end
  else if field = 'level' then
  begin
    L.PushInteger(Engine.GameLevel);
    Result := 1;
  end;
  if Result = 0 then
    Result := inherited;
end;

function TLuaGame.Setter(L: PLua_State): integer;
var
  field: string;
  n: Integer;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'paused' then
  begin
    Engine.GamePaused := L.ToBoolean(-1);
    Result := 1;
  end
  else if field = 'level' then
  begin
    n := L.ToInteger(-1);
    if n < 1 then n := 1;
    if n > 10 then n := 10;
    Engine.GameLevel := n;
    Result := 1;
  end;
  if Result = 0 then
    Result := inherited;
end;

function TLuaGame.TogglePause_func(L: Plua_State): integer; cdecl;
begin
  Engine.ToggleGamePause;
  L.PushBoolean(Engine.GamePaused);
  Result := 1;
end;

function TLuaGame.PauseGame_func(L: Plua_State): integer; cdecl;
begin
  Engine.GamePaused := True;
  Result := 0;
end;

function TLuaGame.ResumeGame_func(L: Plua_State): integer; cdecl;
begin
  Engine.GamePaused := False;
  Result := 0;
end;

function TLuaGame.Restart_func(L: Plua_State): integer; cdecl;
begin
  //Also available as the global restart() for kids who look for that name.
  Engine.RequestRestart;
  Result := 0;
end;

function TLuaGame.Difficulty_func(L: Plua_State): integer; cdecl;
var
  n: Integer;
begin
  if L.ArgsCount > 0 then
  begin
    n := L.ToInteger(1);
    if n < 1 then n := 1;
    if n > 10 then n := 10;
    Engine.GameLevel := n;
  end;
  //One level is normal speed; higher levels crawl faster, lower never slow.
  Engine.World.TimeScale := 1.0 + (Engine.GameLevel - 1) * 0.08;
  L.PushInteger(Engine.GameLevel);
  Result := 1;
end;

function TLuaGame.PausePressed_func(L: Plua_State): integer; cdecl;
begin
  //game.pause_pressed() - true once when p (keyboard) or start/menu (pad) is pressed
  L.PushBoolean(Engine.PausePressed);
  Result := 1;
end;

function TLuaGame.PauseDown_func(L: Plua_State): integer; cdecl;
begin
  //game.pause_down() - true every frame while p or a pad start/menu is held
  L.PushBoolean(Engine.PauseDown);
  Result := 1;
end;

function TLuaGame.StartPressed_func(L: Plua_State): integer; cdecl;
begin
  //game.start_pressed() - true once on space/enter or start/menu/a/x/fire
  L.PushBoolean(Engine.StartPressed);
  Result := 1;
end;

function TLuaGame.FirePressed_func(L: Plua_State): integer; cdecl;
begin
  //game.fire_pressed() - true once on space/x or a/x/b/y/rb/rt/fire/trigger
  L.PushBoolean(Engine.FirePressed);
  Result := 1;
end;

function TLuaGame.FireDown_func(L: Plua_State): integer; cdecl;
begin
  //game.fire_down() - true every frame while any fire button is held
  L.PushBoolean(Engine.FireDown);
  Result := 1;
end;

function TLuaGame.APressed_func(L: Plua_State): integer; cdecl;
begin
  //game.a_pressed() - true once on space or gamepad a
  L.PushBoolean(Engine.APressed);
  Result := 1;
end;

function TLuaGame.ADown_func(L: Plua_State): integer; cdecl;
begin
  //game.a_down() - true every frame while space or gamepad a is held
  L.PushBoolean(Engine.ADown);
  Result := 1;
end;

function TLuaGame.BPressed_func(L: Plua_State): integer; cdecl;
begin
  //game.b_pressed() - true once on x or gamepad b
  L.PushBoolean(Engine.BPressed);
  Result := 1;
end;

function TLuaGame.BDown_func(L: Plua_State): integer; cdecl;
begin
  //game.b_down() - true every frame while x or gamepad b is held
  L.PushBoolean(Engine.BDown);
  Result := 1;
end;

function TLuaGame.XPressed_func(L: Plua_State): integer; cdecl;
begin
  //game.x_pressed() - true once on c or gamepad x
  L.PushBoolean(Engine.XPressed);
  Result := 1;
end;

function TLuaGame.XDown_func(L: Plua_State): integer; cdecl;
begin
  //game.x_down() - true every frame while c or gamepad x is held
  L.PushBoolean(Engine.XDown);
  Result := 1;
end;

function TLuaGame.YPressed_func(L: Plua_State): integer; cdecl;
begin
  //game.y_pressed() - true once on v or gamepad y
  L.PushBoolean(Engine.YPressed);
  Result := 1;
end;

function TLuaGame.YDown_func(L: Plua_State): integer; cdecl;
begin
  //game.y_down() - true every frame while v or gamepad y is held
  L.PushBoolean(Engine.YDown);
  Result := 1;
end;

function TLuaGame.LPressed_func(L: Plua_State): integer; cdecl;
begin
  //game.l_pressed() - true once on z or gamepad lb/lt (the classic bomb key)
  L.PushBoolean(Engine.LPressed);
  Result := 1;
end;

function TLuaGame.LDown_func(L: Plua_State): integer; cdecl;
begin
  //game.l_down() - true every frame while z or gamepad lb/lt is held
  L.PushBoolean(Engine.LDown);
  Result := 1;
end;

function TLuaGame.RPressed_func(L: Plua_State): integer; cdecl;
begin
  //game.r_pressed() - true once on b or gamepad rb/rt
  L.PushBoolean(Engine.RPressed);
  Result := 1;
end;

function TLuaGame.RDown_func(L: Plua_State): integer; cdecl;
begin
  //game.r_down() - true every frame while b or gamepad rb/rt is held
  L.PushBoolean(Engine.RDown);
  Result := 1;
end;

function TLuaGame.SelectPressed_func(L: Plua_State): integer; cdecl;
begin
  //game.select_pressed() - true once on tab or gamepad select/back
  L.PushBoolean(Engine.SelectPressed);
  Result := 1;
end;

function TLuaGame.SelectDown_func(L: Plua_State): integer; cdecl;
begin
  //game.select_down() - true every frame while tab or select/back is held
  L.PushBoolean(Engine.SelectDown);
  Result := 1;
end;

function TLuaGame.StartDown_func(L: Plua_State): integer; cdecl;
begin
  //game.start_down() - true every frame while any confirm button is held
  L.PushBoolean(Engine.StartDown);
  Result := 1;
end;

function TLuaGame.Clamp_func(L: Plua_State): integer; cdecl;
begin
  //clamp(v, lo, hi)
  L.PushNumber(Engine.ClampValue(L.ToNumber(1), L.ToNumber(2), L.ToNumber(3)));
  Result := 1;
end;

function TLuaGame.Lerp_func(L: Plua_State): integer; cdecl;
begin
  //lerp(a, b, t)
  L.PushNumber(Engine.LerpValue(L.ToNumber(1), L.ToNumber(2), L.ToNumber(3)));
  Result := 1;
end;

function TLuaGame.Sign_func(L: Plua_State): integer; cdecl;
begin
  //sign(v) -> -1, 0 or 1
  L.PushInteger(Engine.SignValue(L.ToNumber(1)));
  Result := 1;
end;

function TLuaGame.Dist_func(L: Plua_State): integer; cdecl;
begin
  //dist(x1, y1, x2, y2) - straight-line distance between two points
  L.PushNumber(Engine.PointsDist(L.ToNumber(1), L.ToNumber(2),
    L.ToNumber(3), L.ToNumber(4)));
  Result := 1;
end;

function TLuaGame.Aabb_func(L: Plua_State): integer; cdecl;
begin
  //aabb(x1, y1, w1, h1, x2, y2, w2, h2) - do two boxes overlap?
  //(x, y is each box's top-left corner)
  L.PushBoolean(Engine.BoxesOverlap(L.ToNumber(1), L.ToNumber(2),
    L.ToNumber(3), L.ToNumber(4), L.ToNumber(5), L.ToNumber(6),
    L.ToNumber(7), L.ToNumber(8)));
  Result := 1;
end;

//__call of the table rng(seed) hands out: the generator state lives in
//the raw field 's', so this one plain method is the whole object. The table
//itself arrives as arg 1, the call arguments start at 2.
function TLuaGame.RngCall_func(L: Plua_State): integer; cdecl;
var
  State: Int64;
  Norm, Lo, Hi: Double;
  c: Integer;
begin
  L.GetField(1, 's');
  State := L.ToInt64(-1);
  L.Pop(1);
  Norm := Engine.RngNext(State);
  L.PushInteger(State);
  L.SetField(1, 's');
  c := L.ArgsCount;
  if c <= 1 then
    L.PushNumber(Norm) //r() -> 0 .. <1
  else
  begin
    Lo := 1;           //r(n) -> 1 .. n
    Hi := L.ToNumber(2);
    if c > 2 then
    begin
      Lo := Hi;        //r(a, b) -> a .. b
      Hi := L.ToNumber(3);
    end;
    L.PushInteger(Trunc(Lo + Norm * (Hi - Lo + 1)));
  end;
  Result := 1;
end;

function TLuaGame.Rng_func(L: Plua_State): integer; cdecl;
var
  Seed: Int64;
begin
  //rng(seed) -> a repeatable random function, like the global rand() but
  //private: r() is 0..<1, r(n) is 1..n, r(a, b) is a..b. Two runs with the
  //same seed produce the same sequence.
  if L.ArgsCount >= 1 then
    Seed := L.ToInt64(1)
  else
    Seed := 12345;
  L.NewTable; //[rng]
  L.PushInteger(Seed);
  L.SetField(-2, 's');
  L.NewTable; //[rng, meta]
  L.RegisterMeta('__call', RngCall_func);
  L.SetMetaTable(-2); //rng.metatable = meta
  Result := 1;
end;

function TLuaGame.HiLoad_func(L: Plua_State): integer; cdecl;
var
  ADefault: Int64;
begin
  //score.hi_load("cave.hi" [, default]) - the best score kept in a small text
  //file next to the game; default (0) when the file is missing or unreadable.
  ADefault := 0;
  if L.ArgsCount >= 2 then
    ADefault := L.ToInteger(2);
  L.PushInteger(Engine.LoadHighScore(L.ToString(1), ADefault));
  Result := 1;
end;

function TLuaGame.HiSave_func(L: Plua_State): integer; cdecl;
begin
  //score.hi_save("cave.hi", score) - writes only when score is a new best and
  //answers the best either way, so `hi = score.hi_save("cave.hi", score)` is
  //all a game needs. Fractional scores are truncated, so `hi_save(file, 987.6)`
  //is not silently dropped to 0.
  L.PushInteger(Engine.SaveHighScore(L.ToString(1), Trunc(L.ToNumber(2))));
  Result := 1;
end;

function TLuaScript.FrameTime_func(L: Plua_State): integer; cdecl;
begin
  L.PushNumber(TyroInput.FrameTime);
  Result := 1;
end;

function TLuaScript.TotalTime_func(L: Plua_State): integer; cdecl;
begin
  L.PushNumber(TyroInput.TotalTime);
  Result := 1;
end;

// millis() - high-resolution monotonic clock in milliseconds (RayLib.GetTime,
// the same timer the engine's frame budget uses). Unlike clock(), which is
// quantised to the OS tick, this is fine enough to time a frame body, e.g.
// "local t0 = millis() ... print(millis() - t0)".
function TLuaScript.Millis_func(L: Plua_State): integer; cdecl;
begin
  L.PushNumber(RayLib.GetTime * 1000);
  Result := 1;
end;

function TLuaScript.RandomValue_func(L: Plua_State): integer; cdecl;
var
  minv, maxv: integer;
begin
  minv := round(L.ToNumber(1));
  maxv := round(L.ToNumber(2));
  L.PushInteger(TyroInput.RandomValue(minv, maxv));
  Result := 1;
end;

function TLuaScript.Exit_func(L: Plua_State): integer; cdecl;
begin
  //Stop the script as soon as the current call finishes
  Stop;
  Result := 0;
end;

procedure TLuaScript.ReleaseScene(AScene: TLuaScene);
begin
  if (AScene = nil) or (Lua.State = nil) then
    Exit;
  if AScene.EnterRef <> LUA_NOREF then
    luaL_unref(Lua.State, LUA_REGISTRYINDEX, AScene.EnterRef);
  if AScene.UpdateRef <> LUA_NOREF then
    luaL_unref(Lua.State, LUA_REGISTRYINDEX, AScene.UpdateRef);
  if AScene.DrawRef <> LUA_NOREF then
    luaL_unref(Lua.State, LUA_REGISTRYINDEX, AScene.DrawRef);
  if AScene.LeaveRef <> LUA_NOREF then
    luaL_unref(Lua.State, LUA_REGISTRYINDEX, AScene.LeaveRef);
  AScene.EnterRef := LUA_NOREF;
  AScene.UpdateRef := LUA_NOREF;
  AScene.DrawRef := LUA_NOREF;
  AScene.LeaveRef := LUA_NOREF;
end;

function TLuaScript.SceneAdd_func(L: Plua_State): integer; cdecl;
const
  cFields: array[0..3] of string = ('enter', 'update', 'draw', 'leave');
var
  Name, Field: string;
  S: TLuaScene;
  Ref, j: Integer;
begin
  Name := LowerCase(L.ToString(1));
  if (Name = '') or not L.IsTable(2) then
  begin
    L.PushBoolean(False);
    Exit(1);
  end;
  S := FindScene(Name);
  if S = nil then
  begin
    S := TLuaScene.Create;
    S.Name := Name;
    S.EnterRef := LUA_NOREF;
    S.UpdateRef := LUA_NOREF;
    S.DrawRef := LUA_NOREF;
    S.LeaveRef := LUA_NOREF;
    Scenes.Add(S);
  end
  else
    ReleaseScene(S);
  for j := 0 to High(cFields) do
  begin
    Field := cFields[j];
    lua_getfield(L, 2, PUTF8Char(UTF8String(Field)));
    if lua_isfunction(L, -1) then
      Ref := luaL_ref(L, LUA_REGISTRYINDEX)
    else
    begin
      lua_pop(L, 1);
      Ref := LUA_NOREF;
    end;
    case j of
      0: S.EnterRef := Ref;
      1: S.UpdateRef := Ref;
      2: S.DrawRef := Ref;
      3: S.LeaveRef := Ref;
    end;
  end;
  L.PushBoolean(True);
  Result := 1;
end;

function TLuaScript.SceneGo_func(L: Plua_State): integer; cdecl;
var
  S, Old: TLuaScene;
  Top, Count: Integer;
begin
  S := FindScene(L.ToString(1));
  if S = nil then
  begin
    L.PushBoolean(False);
    Exit(1);
  end;
  Top := lua_gettop(L);
  if CurrentScene <> '' then
  begin
    Old := FindScene(CurrentScene);
    if Old <> nil then
      CallRef(Old.LeaveRef, Top + 1, 0); //arguments stay untouched
  end;
  CurrentScene := S.Name;
  Count := Top - 1;
  if Count > 0 then
    CallRef(S.EnterRef, 2, Count)
  else
    CallRef(S.EnterRef, Top + 1, 0);
  L.PushBoolean(True);
  Result := 1;
end;

function TLuaScript.SceneCurrent_func(L: Plua_State): integer; cdecl;
begin
  L.PushString(CurrentScene);
  Result := 1;
end;

function TLuaScript.SceneList_func(L: Plua_State): integer; cdecl;
var
  i: Integer;
begin
  L.BeginTable;
  if Scenes <> nil then
    for i := 0 to Scenes.Count - 1 do
    begin
      L.PushString(TLuaScene(Scenes[i]).Name);
      lua_rawseti(L, -2, i + 1);
    end;
  Result := 1;
end;

function TLuaScript.SceneRemove_func(L: Plua_State): integer; cdecl;
var
  S: TLuaScene;
begin
  S := FindScene(L.ToString(1));
  if S = nil then
  begin
    L.PushBoolean(False);
    Exit(1);
  end;
  if CurrentScene = S.Name then
    CurrentScene := '';
  ReleaseScene(S);
  Scenes.Remove(S);
  S.Free;
  L.PushBoolean(True);
  Result := 1;
end;

function TLuaScript.SceneClear_func(L: Plua_State): integer; cdecl;
var
  i: Integer;
  S: TLuaScene;
begin
  if Scenes <> nil then
  begin
    for i := Scenes.Count - 1 downto 0 do
    begin
      S := TLuaScene(Scenes[i]);
      ReleaseScene(S);
      S.Free;
    end;
    Scenes.Clear;
  end;
  CurrentScene := '';
  Result := 0;
end;

function TLuaScript.Clock_func(L: Plua_State): integer; cdecl;
begin
  L.PushNumber(TThread.GetTickCount / 1000.0);
  Result := 1;
end;

function TLuaScript.Time_func(L: Plua_State): integer; cdecl;
var
  t: TDateTime;
  secs: Int64;
begin
  t := Now;
  secs := Trunc(t * 86400);
  L.PushInteger(secs);
  Result := 1;
end;

function TLuaScript.TextWidth_func(L: Plua_State): integer; cdecl;
begin
  //Measure with the very font canvas.text draws with, so this can be used to
  //centre or right-align text without hard-coding character widths.
  L.PushInteger(Round(RayLib.MeasureTextEx(Res.Font.Data,
    PUTF8Char(UTF8String(L.ToString(1))), Res.Font.Height, 0).x));
  Result := 1;
end;

function TLuaScript.TextHeight_func(L: Plua_State): integer; cdecl;
begin
  L.PushInteger(Res.Font.Height);
  Result := 1;
end;

function TLuaScript.Rgba_func(L: Plua_State): integer; cdecl;
var
  r, g, b, a: Integer;
  c: Integer;
  i: Integer;
begin
  c := L.ArgsCount;
  r := 0;
  g := 0;
  b := 0;
  a := 255;
  if c >= 1 then
    r := Round(L.ToInteger(1)) and 255;
  if c >= 2 then
    g := Round(L.ToInteger(2)) and 255;
  if c >= 3 then
    b := Round(L.ToInteger(3)) and 255;
  if c >= 4 then
  begin
    if not L.IsNil(4) then
      a := Round(L.ToInteger(4)) and 255;
  end;
  // Pack as AARRGGBB with shifts: a * 16777216 would raise EIntOverflow
  // in range-checked builds whenever alpha > 127.
  i := Integer(Cardinal(r) or (Cardinal(g) shl 8) or (Cardinal(b) shl 16) or (Cardinal(a) shl 24));
  L.PushInteger(i);
  Result := 1;
end;

function TLuaScript.Screenshot_func(L: Plua_State): integer; cdecl;
begin
  if L.ArgsCount > 0 then
    // TakeScreenshot must run on the Engine thread after the frame is
    // presented, so queue the request and let the engine capture it.
    Engine.QueueScreenshot(L.ToString(1));
  Result := 0;
end;

{ TLuaFile }

// Pushes AData onto the Lua stack with its exact byte count, so binary content
// that holds #0 bytes survives the trip. PushString goes through a
// null-terminated PChar and would cut the string at the first #0.
procedure PushBytes(L: PLua_State; const AData: AnsiString);
begin
  if Length(AData) > 0 then
    lua_pushlstring(L, PUTF8Char(PAnsiChar(@AData[1])), Length(AData))
  else
    lua_pushstring(L, PUTF8Char(''));
end;

// Drops the ACount values Lua passed in as arguments, so a function that built
// its own table on top of them leaves exactly that table as the result. lua_pop
// would take from the top and eat the new table instead, so the arguments are
// removed one by one from the bottom, where lua_remove shifts them down.
procedure PopArgs(L: PLua_State; ACount: Integer);
begin
  while ACount > 0 do
  begin
    L.Remove(1);
    Dec(ACount);
  end;
end;

function TLuaFile.ModeFlag(const AFlag: Char): Boolean;
begin
  Result := Pos(AFlag, LowerCase(FMode)) > 0;
end;

//Index of the first value the script really passed. Register() injects the
//handle table as argument 1, so "f.write('x')" has its value at index 2 while
//the natural colon spelling "f:write('x')" repeats the table at index 2 and
//starts at 3. Comparing argument 2 with the injected table tells the two apart,
//so both spellings read the same values.
function TLuaFile.FirstArgIndex(L: PLua_State): Integer;
begin
  if lua_rawequal(L, 1, 2) then
    Result := 3
  else
    Result := 2;
end;

destructor TLuaFile.Destroy;
begin
  FreeAndNil(FLines);
  if FStream <> nil then
  begin
    FreeAndNil(FStream); //also flushes whatever the script left buffered
    FIsOpen := False;
  end;
  inherited Destroy;
end;

//Opens AFileName with the mode letters io.open understands: 'r' (default),
//'w' (truncate), 'a' (append), 'x' (create, never overwrite), an optional '+'
//(also readable) and an optional 'b'. Everything runs through TFileStream; 'b'
//only switches off the newline translation that the text modes apply. AFileName
//has already passed ResolveAndValidatePath, so it is known to be inside the
//workspace.
function TLuaFile.Open(const AFileName, AMode: string): Boolean;
var
  Flags: Integer;
begin
  Result := False;
  FError := '';
  FFileName := AFileName;
  FMode := AMode;
  if AFileName = '' then
  begin
    FError := 'no file name';
    Exit;
  end;
  FIsText := not ModeFlag('b');

  if ModeFlag('x') then
  begin
    //exclusive create: a file that is already there is never touched
    if SysUtils.FileExists(AFileName) then
    begin
      FError := 'file already exists';
      Exit;
    end;
    if ModeFlag('+') then
      Flags := fmOpenReadWrite or fmCreate or fmShareDenyWrite
    else
      Flags := fmCreate or fmShareDenyWrite; //create or truncate, still readable
  end
  else if ModeFlag('w') then
    Flags := fmCreate or fmShareDenyWrite //create or truncate, still readable
  else if ModeFlag('a') and ModeFlag('+') then
    Flags := fmOpenReadWrite or fmShareDenyWrite
  else if ModeFlag('a') then
  begin
    //append: update an existing file, create it when it is missing
    if SysUtils.FileExists(AFileName) then
      Flags := fmOpenReadWrite or fmShareDenyWrite
    else
      Flags := fmCreate or fmShareDenyWrite
  end
  else if ModeFlag('+') then
    //read and write, without truncating
    Flags := fmOpenReadWrite or fmShareDenyWrite
  else
    Flags := fmOpenRead or fmShareDenyWrite;

  try
    FStream := TFileStream.Create(AFileName, Flags);
  except
    on E: Exception do
    begin
      FStream := nil;
      FError := E.Message;
      Exit;
    end;
  end;
  if ModeFlag('a') then
  begin
    try
      FStream.Seek(0, soEnd); //append always writes at the end
    except
      on E: Exception do
      begin
        FError := E.Message;
        FreeAndNil(FStream);
        Exit;
      end;
    end;
  end;
  FIsOpen := True;
  Result := True;
end;

//Reads one token ("n") or one line ("l" without the newline, "L" with it) and
//pushes exactly one value, following the io library's text-mode conventions: at
//the end of the stream it pushes nil, and a number it cannot parse pushes nil
//plus a message (and returns False so the caller returns two values).
function TLuaFile.ReadTextFormat(L: PLua_State; const AFormat: UTF8Char): Boolean;
var
  Buf: AnsiString;
  Num: Double;
  Whole: Int64;
  c: Char;
  StartPos: Int64;
begin
  Result := True;
  Buf := '';
  StartPos := FStream.Position;

  if AFormat = 'n' then
  begin
    //"n" is a token read, not a line read: drop the blanks first, then take
    //everything up to the next one
    while FStream.Position < FStream.Size do
    begin
      c := #0;
      FStream.ReadBuffer(c, 1);
      if c > ' ' then
      begin
        Buf := c;
        Break;
      end;
    end;
    if Buf = '' then
    begin
      L.PushNil; //nothing but blanks left, like io
      Exit;
    end;
    while FStream.Position < FStream.Size do
    begin
      c := #0;
      FStream.ReadBuffer(c, 1);
      if c <= ' ' then
        Break;
      Buf := Buf + c;
    end;
  end
  else
  begin
    //"l"/"L" are line reads: stop at the newline, or at the end of the file
    while FStream.Position < FStream.Size do
    begin
      c := #0;
      FStream.ReadBuffer(c, 1);
      if c = #10 then
        Break;
      Buf := Buf + c;
    end;
    if FStream.Position = StartPos then
    begin
      L.PushNil; //end of the file, like io
      Exit;
    end;
    //a trailing #13 belongs to the CRLF pair, never to the returned text
    if (Buf <> '') and (Buf[Length(Buf)] = #13) then
      Delete(Buf, Length(Buf), 1);
    if AFormat = 'L' then
      Buf := Buf + #10;
  end;

  if AFormat = 'n' then
  begin
    //A whole number comes back as an integer, the way io.read("n") hands it over;
    //the spellings TryStrToInt64 rejects (a fraction, an exponent) go on as a float
    if TryStrToInt64(Buf, Whole) then
      L.PushInteger(Whole)
    else if TryStrToFloat(Buf, Num) then
      L.PushNumber(Num)
    else
    begin
      L.PushNil;
      L.PushString('malformed number near ''' + Buf + '''');
      Result := False;
    end;
  end
  else
    PushBytes(L, Buf);
end;

//file:read([what [, ...]]) with the io.read formats: "a"/"*a" for everything
//left, "l"/"*l" for one line, "L"/"*L" for a line keeping its newline, "n" for
//the next number (io style) and a plain number for that many bytes. Formats
//only apply to text streams; on a binary stream they read raw bytes.
function TLuaFile.Read_func(L: Plua_State): integer; cdecl;
var
  aFormat: AnsiString;
  Fmt: AnsiChar;
  Count: Int64;
  Buf: AnsiString;
  idx: Integer;
begin
  if not FIsOpen then
  begin
    L.PushNil;
    L.PushString('file is closed');
    Exit(2);
  end;
  idx := FirstArgIndex(L);
  if L.ArgsCount < idx then
    aFormat := 'l'
  else
    aFormat := AnsiString(L.ToString(idx));
  if (aFormat <> '') and (aFormat[1] = '*') then
    Delete(aFormat, 1, 1);
  if aFormat = '' then
    aFormat := 'l';
  //The letter keeps the case the script wrote, because that is what tells "l"
  //from "L"; the other letters are accepted in either case.
  Fmt := aFormat[1];

  if FIsText and (Fmt in ['l', 'L', 'n']) then
  begin
    //the text formats push one value, or nil plus a message on a bad number
    if ReadTextFormat(L, Fmt) then
      Exit(1)
    else
      Exit(2);
  end;

  if (Fmt = 'a') or (Fmt = 'A') then
  begin
    Buf := '';
    Count := FStream.Size - FStream.Position;
    SetLength(Buf, Count);
    if Count > 0 then
      FStream.ReadBuffer(Buf[1], Count);
    PushBytes(L, Buf);
    Exit(1);
  end;

  if not TryStrToInt64(aFormat, Count) then
    Count := 0;
  if Count <= 0 then
  begin
    L.PushNil;
    L.PushString('invalid format');
    Exit(2);
  end;
  if Count > FStream.Size - FStream.Position then
    Count := FStream.Size - FStream.Position;
  Buf := '';
  SetLength(Buf, Count);
  if Count > 0 then
    FStream.ReadBuffer(Buf[1], Count);
  PushBytes(L, Buf);
  Result := 1;
end;

//file:write(...) -> the file handle, so calls chain: f:write(..):close().
//A leading number is the byte count of the string that follows it, matching
//io.write. The values start at FirstArgIndex, so "f:write(x)" and
//"f.write(f, x)" behave the same.
function TLuaFile.Write_func(L: Plua_State): integer; cdecl;
var
  i, first: Integer;
  Buf, OutBuf: AnsiString;
begin
  if not FIsOpen then
  begin
    L.PushNil;
    L.PushString('file is closed');
    Exit(2);
  end;
  Buf := '';
  first := FirstArgIndex(L);
  i := first;
  while i <= L.ArgsCount do
  begin
    //"f:write(#s, s)" writes only the first n bytes of s. Only a real number
    //is a byte count, like the stock io.write: L.IsNumber also accepts a
    //string that converts, so f:write(tostring(7) .. "\n") would be read as
    //"write 7 bytes" and silently write nothing at all. The count applies only
    //when another argument follows it, which is a test on the index, not on the
    //number's value: f.write(3, "abcdef") has 3 = ArgsCount but still means
    //"write 3 bytes of the string".
    if (i = first) and (lua_type(L, i) = LUA_TNUMBER) and (i < L.ArgsCount) then
    begin
      Inc(i); //now on the string that follows the count
      Buf := Buf + Copy(AnsiString(L.ToString(i)), 1, Integer(L.ToInteger(first)));
      Inc(i);
    end
    else
    begin
      Buf := Buf + AnsiString(L.ToString(i));
      Inc(i);
    end;
  end;

  if FIsText and (Buf <> '') then
  begin
    //a text stream writes CRLF: put a #13 in front of every #10 that does not
    //already have one, so a script writing "\r\n" does not get "\r\r\n"
    OutBuf := '';
    for i := 1 to Length(Buf) do
    begin
      if (Buf[i] = #10) and ((i = 1) or (Buf[i - 1] <> #13)) then
        OutBuf := OutBuf + #13;
      OutBuf := OutBuf + Buf[i];
    end;
    Buf := OutBuf;
  end;

  if Length(Buf) > 0 then
    //Writing past the end extends the file by itself, and growing it through
    //TStream.Size would also move the file pointer, which is not what a write
    //in the middle of the stream should do
    FStream.WriteBuffer(Buf[1], Length(Buf));
  L.PushValue(1); //the handle table, for chaining
  Result := 1;
end;

function TLuaFile.Close_func(L: Plua_State): integer; cdecl;
begin
  if FIsOpen then
  begin
    FreeAndNil(FStream); //also flushes the buffered tail
    FIsOpen := False;
  end;
  //true, not the handle: closing is the end of the road, and a script that
  //chains on it should not end up writing to a handle that is already gone
  L.PushBoolean(True);
  Result := 1;
end;

//file:lines() -> an iterator over the lines that are left, so "for line in
//f:lines()" works. Lines are streamed in small chunks: the old version read
//the whole remainder into one AnsiString, which could spike memory when a kid
//pointed it at a large file. A second call replaces the pending iterator.
function TLuaFile.Lines_func(L: Plua_State): integer; cdecl;
const
  cLineChunk = 65536;
  cMaxLines = 100000;
  cMaxLineLen = 1024 * 1024;
var
  Buf, Pending, LineStr: AnsiString;
  I, Got, idx, base: Integer;
begin
  if not FIsOpen then
  begin
    L.PushNil;
    L.PushString('file is closed');
    Exit(2);
  end;
  if FLines = nil then
    FLines := TStringList.Create
  else
    FLines.Clear;
  FLinesPos := 0;

  SetLength(Buf, cLineChunk);
  Pending := '';
  while (FStream.Position < FStream.Size) and (FLines.Count < cMaxLines) do
  begin
    Got := FStream.Read(Buf[1], cLineChunk);
    if Got <= 0 then
      Break;
    for I := 1 to Got do
      if Buf[I] = #10 then
      begin
        LineStr := Pending;
        //Strip the #13 of a CRLF pair so both line endings read the same.
        if (LineStr <> '') and (LineStr[Length(LineStr)] = #13) then
          Delete(LineStr, Length(LineStr), 1);
        FLines.Add(LineStr);
        Pending := '';
        if FLines.Count >= cMaxLines then
          Break;
      end
      else if Length(Pending) < cMaxLineLen then
        Pending := Pending + Buf[I];
  end;
  //A last line without a trailing newline still counts.
  if (Pending <> '') and (FLines.Count < cMaxLines) then
  begin
    if Pending[Length(Pending)] = #13 then
      Delete(Pending, Length(Pending), 1);
    FLines.Add(Pending);
  end;
  if ((FStream.Position < FStream.Size) or (FLines.Count >= cMaxLines)) and
     (Engine <> nil) then
    Engine.Notice(Format('Hint: file:lines() stopped at %d lines', [cMaxLines]));

  //Return the same three values the io library does: the step function, the
  //state handed back on every step, and the control variable. Lua takes the top
  //Result values as they stand, so they go on the stack function first and
  //control last.
  base := L.ArgsCount; //the arguments are still on the stack
  L.BeginTable; //[.., iterator]
  L.Register('linesnext', LinesNext_func);
  idx := base + 1; //where the iterator table sits
  L.PushValue(idx); //[.., iterator, iterator] a copy to read the field from
  L.GetField(L.ArgsCount, 'linesnext'); //[.., iterator, iterator, step]
  L.Remove(L.ArgsCount - 1); //[.., iterator, step] drop the copy
  lua_insert(L, idx); //[.., step, iterator] the step function has to come first
  L.PushInteger(0); //[.., step, iterator, 0] the control variable
  PopArgs(L, base); //[step, iterator, 0]
  Result := 3;
end;

//One step of the f:lines() iterator: it hands out the next queued line and the
//count of lines done so far, and yields nil once the queue is drained. The count
//is only returned for the callers that drive the iterator themselves; Pluto's
//generic for hands the previous line back instead of it, which is why the
//position is kept in FLinesPos and not taken from an argument.
function TLuaFile.LinesNext_func(L: Plua_State): integer; cdecl;
begin
  if (FLines = nil) or (FLinesPos < 0) or (FLinesPos >= FLines.Count) then
  begin
    L.PushNil;
    Exit(1);
  end;
  L.PushString(FLines[FLinesPos]);
  Inc(FLinesPos);
  L.PushInteger(FLinesPos);
  Result := 2;
end;

//openfile(name [, mode]) -> a file handle, or nil plus an error message.
//This is the only file access a script has: TLua.Init removes the io and os
//libraries from the globals, so nothing here can be bypassed from Lua code.
function TLuaScript.OpenFile_func(L: Plua_State): integer; cdecl;
var
  aName, aMode, Resolved: string;
  aFile: TLuaFile;
  base: Integer;
begin
  if L.ArgsCount < 1 then
  begin
    L.PushNil;
    L.PushString('openfile: missing file name');
    Exit(2);
  end;
  aName := L.ToString(1);
  aMode := 'r';
  if (L.ArgsCount >= 2) and (L.ToString(2) <> '') then
    aMode := L.ToString(2);

  //Expand the name first (relative, "." / ".." and drive-relative forms) and
  //keep it only when it lands inside the workspace. The check runs on the
  //expanded path, so a name that walks out of the workspace is refused before
  //any handle exists.
  Resolved := ResolveAndValidatePath(aName, Res.WorkPath);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('openfile: ''' + aName + ''' resolves outside the workspace');
    Exit(2);
  end;

  aFile := TLuaFile.Create('file', Self);
  if not aFile.Open(Resolved, aMode) then
  begin
    L.PushNil;
    L.PushString('openfile: cannot open ''' + aName + ''' (' + aFile.Error + ')');
    aFile.Free;
    Exit(2);
  end;
  //the script owns the handle for as long as its state lives; Destroy frees them
  Files.Add(aFile);

  base := L.ArgsCount; //the arguments are still on the stack
  Lua.State.BeginTable; //[file]
  //Register injects the table as argument 1, so every method below reads its own
  //arguments from index 2, the way the rest of the Tyro bindings do.
  Lua.State.Register('read', aFile.Read_func);
  Lua.State.Register('write', aFile.Write_func);
  Lua.State.Register('close', aFile.Close_func);
  Lua.State.Register('lines', aFile.Lines_func);
  PopArgs(L, base); //leave the handle table as the only value
  Result := 1;
end;

//Turns a module name into the file it stands for, without the ending: "foo.bar"
//is "foo\bar", the way the stock require reads it. A name that already carries an
//ending keeps its folder part and loses the ".tyro" or ".lua", so both spellings
//find the same file. Nothing is opened here, the name only becomes a path; the
//containment check decides whether the path may be used at all.
function ModuleStem(const AName: string): string;
var
  s: string;
  i: Integer;
  c: Char;
begin
  s := AName;
  if LowerCase(ExtractFileExt(s)) = '.tyro' then
    s := Copy(s, 1, Length(s) - 5)
  else if LowerCase(ExtractFileExt(s)) = '.lua' then
    s := Copy(s, 1, Length(s) - 4);
  //dots that are part of ".." must stay; other dots become the path separator
  Result := '';
  i := 1;
  while i <= Length(s) do
  begin
    c := s[i];
    if (c = '.') and (i + 1 <= Length(s)) and (s[i + 1] = '.') then
    begin
      //double dot: keep both as dots (and let ExpandFileName collapse them)
      Result := Result + '..';
      Inc(i, 2);
      Continue;
    end;
    if c = '.' then
    begin
      Result := Result + PathDelim;
      Inc(i);
      Continue;
    end;
    //treat slashes and backslashes the same as path delimiters too
    if (c = '/') or (c = '\') then
    begin
      Result := Result + PathDelim;
      Inc(i);
      Continue;
    end;
    Result := Result + c;
    Inc(i);
  end;
end;

//Expands <root>\<name> and returns it only when the file is really there and
//lands inside root, so a name built from "..", a drive letter or a UNC root
//cannot walk out of the folder it is checked against. The existence test is
//what lets require walk a list of endings: without it the first ending would
//"win" for every name and the rest of the list would never be reached.
function ResolveInRoot(const FileName, ARoot: string): string;
var
  Candidate: string;
begin
  Result := '';
  if (FileName = '') or (ARoot = '') or (Res = nil) then
    Exit;
  if IsRootedName(FileName) then
    Candidate := ExpandFileName(FileName)
  else
    Candidate := ExpandFileName(IncludePathDelimiter(ARoot) + FileName);
  if Res.IsPathInside(Candidate, ARoot) and SysUtils.FileExists(Candidate) then
    Result := Candidate;
end;

//require("name") -> runs a Lua file from the workspace or the app folder and
//returns whatever it returned, the same contract the stock require has. The
//stock one is gone (see TLua.Init) because it can load a native module from
//anywhere on disk: this loader expands the name first and refuses anything that
//does not land inside the workspace or the app folder. A module that returned a
//value is remembered in the registry under the file it came from, so the second
//require hands that value straight back and "m", "m.tyro" and "m.lua" share one
//entry.
function TLuaScript.Require_func(L: Plua_State): integer; cdecl;
var
  aName, Stem, Resolved, aMsg: string;
  Roots: array[0..1] of string;
  Extensions: array[0..1] of string;
  i, j: Integer;
begin
  if L.ArgsCount < 1 then
  begin
    L.PushNil;
    L.PushString('require: missing module name');
    Exit(2);
  end;
  aName := L.ToString(1);
  Stem := ModuleStem(aName);
  Resolved := '';

  //the workspace comes first, then the app folder; inside each, a .tyro file
  //comes before one of Tyro's own .lua scripts
  Extensions[0] := '.tyro';
  Extensions[1] := '.lua';
  Roots[0] := Res.WorkPath;
  Roots[1] := Res.AppPath;
  for i := 0 to 1 do
    for j := 0 to 1 do
      if Resolved = '' then
        Resolved := ResolveInRoot(Stem + Extensions[j], Roots[i]);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('require: ''' + aName + ''' is not a .tyro or .lua file inside the workspace or the app folder');
    Exit(2);
  end;

  //already run? the cached value is the answer, as it is with the stock loader
  lua_getfield(L, LUA_REGISTRYINDEX, PUTF8Char(cRequireCache)); //[cache]
  lua_pushstring(L, PUTF8Char(UTF8String(Resolved))); //[cache, path]
  lua_rawget(L, -2); //[cache, value]
  if not lua_isnil(L, -1) then
  begin
    Result := 1;
    Exit;
  end;
  lua_pop(L, 2); //[]

  if luaL_loadfile(L, PUTF8Char(UTF8String(Resolved))) <> LUA_OK then
  begin
    //the loader left its message on the stack; hand it back the way the stock
    //require does, nil plus a message
    aMsg := 'require: ' + aName + ': ' + L.ToString(-1);
    L.Pop(1);
    L.PushNil;
    L.PushString(aMsg);
    Exit(2);
  end;
  //The stock require calls the chunk with the module name as its argument.
  //Use pcall: a module that calls error() must not longjmp past these managed
  //Pascal locals (aName, Resolved, aMsg); return nil + message like the loader.
  lua_pushstring(L, PUTF8Char(UTF8String(aName))); //[chunk, name]
  if lua_pcall(L, 1, 1, 0) <> LUA_OK then //[error]
  begin
    aMsg := 'require: ' + aName + ': ' + L.ToString(-1);
    L.Pop(1);
    L.PushNil;
    L.PushString(aMsg);
    Exit(2);
  end;
  //[value]

  if lua_isnil(L, -1) then
    //a module that returns nothing runs again on the next require, as before
    L.Pop(1)
  else
  begin
    lua_getfield(L, LUA_REGISTRYINDEX, PUTF8Char(cRequireCache)); //[value, cache]
    lua_pushstring(L, PUTF8Char(UTF8String(Resolved))); //[value, cache, path]
    lua_pushvalue(L, -3); //[value, cache, path, value]
    lua_rawset(L, -3); //cache[path] = value -> [value, cache]
    L.Pop(1); //[value]
  end;
  Result := 1;
end;

//http_get(url) -> body | nil, err
//Fetches a URL over HTTP/HTTPS through TTyroEngine.HttpGet. The request runs
//on the script thread, so the render loop keeps drawing while it is in flight;
//the script itself blocks until the answer (or the failure) arrives.
function TLuaScript.HttpGet_func(L: Plua_State): integer; cdecl;
var
  aBody, aError: string;
begin
  if not L.IsString(1) then
  begin
    L.PushNil;
    L.PushString('http_get: missing url');
    Exit(2);
  end;
  if StopScriptRequested then
  begin
    L.PushNil;
    L.PushString('http_get: stopped');
    Exit(2);
  end;
  aError := 'http_get: engine is not running';
  if (Engine = nil) or not Engine.HttpGet(L.ToString(1), aBody, aError) then
  begin
    L.PushNil;
    L.PushString(aError);
    Exit(2);
  end;
  //The socket call itself has a bounded 10 s connect / 15 s read timeout;
  //once it returns, F5 must win over delivering a stale response.
  if StopScriptRequested then
  begin
    L.PushNil;
    L.PushString('http_get: stopped');
    Exit(2);
  end;
  L.PushString(aBody);
  Result := 1;
end;

//Push one parsed JSON value as the matching Lua value. Objects become tables
//keyed by their member names, arrays become tables with 1..n integer keys,
//numbers/booleans/strings map to their Lua counterparts and null becomes nil.
procedure PushDonValue(L: Plua_State; AValue: TDON_Value);
var
  i: Integer;
  Obj: TDON_Object;
  Arr: TDON_Array;
begin
  if AValue = nil then
    L.PushNil
  else if AValue is TDON_Object then
  begin
    Obj := TDON_Object(AValue);
    lua_createtable(L, 0, Obj.Count);
    for i := 0 to Obj.Count - 1 do
    begin
      lua_pushstring(L, PUTF8Char(UTF8String(Obj.Pairs[i].Name)));
      PushDonValue(L, Obj.Pairs[i].Value);
      lua_rawset(L, -3);
    end;
  end
  else if AValue is TDON_Array then
  begin
    Arr := TDON_Array(AValue);
    lua_createtable(L, Arr.Count, 0);
    for i := 0 to Arr.Count - 1 do
    begin
      lua_pushinteger(L, i + 1);
      PushDonValue(L, Arr.Item[i]);
      lua_rawset(L, -3);
    end;
  end
  else if AValue is TDON_Number then
    L.PushNumber(TDON_Number(AValue).Value)
  else if AValue is TDON_Boolean then
    L.PushBoolean(TDON_Boolean(AValue).Value)
  else if AValue.IsNull then
    L.PushNil //JSON null (parsed as the identifier 'null')
  else
    L.PushString(AValue.AsString);
end;

const
  cJsonMaxDepth = 128;

//Append S to AOut as a JSON string member: quoted, with the escapes JSON
//requires. Bytes above $7F ride along untouched: Lua strings are UTF-8 and
//UTF-8 is valid JSON, so accents and glyphs survive the round trip.
procedure JsonAppendEscaped(var AOut: string; const S: string);
var
  i: Integer;
  c: Char;
begin
  AOut := AOut + '"';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
      '"': AOut := AOut + '\"';
      '\': AOut := AOut + '\\';
      #8: AOut := AOut + '\b';
      #12: AOut := AOut + '\f';
      #10: AOut := AOut + '\n';
      #13: AOut := AOut + '\r';
      #9: AOut := AOut + '\t';
    else
      if c < ' ' then
        AOut := AOut + Format('\u%.4x', [Ord(c)])
      else
        AOut := AOut + c;
    end;
  end;
  AOut := AOut + '"';
end;

//One Lua number as JSON text. Integers print plain; a float always uses a
//dot as the decimal separator, whatever the machine's locale says. JSON has
//no room for NaN or infinity, so those become null (like JSON.stringify).
function JsonNumberText(L: Plua_State; AIdx: Integer): string;
var
  v: Double;
  aSettings: TFormatSettings;
begin
  if lua_isinteger(L, AIdx) then
    Exit(IntToStr(lua_tointeger(L, AIdx)));
  v := lua_tonumber(L, AIdx);
  if (v <> v) or ((v <> 0) and (v = v + v)) then
    Exit('null'); //NaN, +Inf, -Inf
  aSettings := FormatSettings;
  aSettings.DecimalSeparator := '.';
  aSettings.ThousandSeparator := ',';
  Result := FloatToStr(v, aSettings);
end;

//Encode the Lua value at AIdx as JSON text, appended to AOut. A sequence of
//1..n members becomes an array, everything else an object with the keys as
//member names (numbers and booleans are quoted into names; keys of no JSON
//name are skipped). nil answers null, and inside a table functions and
//userdata are skipped rather than refused - a save file with a stray
//callback in it still writes. AVisited guards a table that holds itself.
//Returns False with a message when the value cannot be written.
function EncodeJsonValue(L: Plua_State; AIdx: Integer; var AOut: string;
  AVisited: TList<Pointer>; ADepth: Integer; out AError: string): Boolean;
var
  i, n, count, tt, absIdx: Integer;
  p: Pointer;
  IsArray, IsNil, first: Boolean;
  key: string;
begin
  Result := False;
  if ADepth > cJsonMaxDepth then
  begin
    AError := 'nested over ' + IntToStr(cJsonMaxDepth) + ' levels deep';
    Exit;
  end;
  if AIdx < 0 then
    absIdx := lua_gettop(L) + AIdx + 1
  else
    absIdx := AIdx;
  tt := lua_type(L, absIdx);
  case tt of
    LUA_TNONE, LUA_TNIL:
      AOut := AOut + 'null';
    LUA_TBOOLEAN:
      if lua_toboolean(L, absIdx) then
        AOut := AOut + 'true'
      else
        AOut := AOut + 'false';
    LUA_TNUMBER:
      AOut := AOut + JsonNumberText(L, absIdx);
    LUA_TSTRING:
      JsonAppendEscaped(AOut, L.ToString(absIdx));
    LUA_TTABLE:
    begin
      p := lua_topointer(L, absIdx);
      if AVisited.IndexOf(p) >= 0 then
      begin
        AError := 'circular reference';
        Exit;
      end;
      AVisited.Add(p);
      try
        //A plain 1..n sequence is an array; a hole, or any key riding
        //beside the sequence, makes the whole table an object instead.
        n := Integer(lua_rawlen(L, absIdx));
        IsArray := n > 0;
        if IsArray then
          for i := 1 to n do
          begin
            lua_rawgeti(L, absIdx, i);
            IsNil := lua_isnil(L, -1);
            lua_pop(L, 1);
            if IsNil then
            begin
              IsArray := False;
              Break;
            end;
          end;
        if IsArray then
        begin
          count := 0;
          lua_pushnil(L);
          while lua_next(L, absIdx) <> 0 do
          begin
            Inc(count);
            lua_pop(L, 1);
          end;
          if count <> n then
            IsArray := False;
        end;
        if IsArray then
        begin
          AOut := AOut + '[';
          for i := 1 to n do
          begin
            if i > 1 then
              AOut := AOut + ',';
            lua_rawgeti(L, absIdx, i);
            try
              tt := lua_type(L, -1);
              if (tt = LUA_TFUNCTION) or (tt = LUA_TUSERDATA) or (tt = LUA_TTHREAD) then
                AOut := AOut + 'null' //a hole the script left, as JSON sees it
              else if not EncodeJsonValue(L, -1, AOut, AVisited, ADepth + 1, AError) then
                Exit;
            finally
              lua_pop(L, 1);
            end;
          end;
          AOut := AOut + ']';
        end
        else
        begin
          AOut := AOut + '{';
          first := True;
          lua_pushnil(L); //the first key
          while lua_next(L, absIdx) <> 0 do
          begin
            //key at -2, value at -1
            key := '';
            case lua_type(L, -2) of
              LUA_TSTRING:
                key := L.ToString(-2);
              LUA_TNUMBER:
                key := JsonNumberText(L, -2);
              LUA_TBOOLEAN:
                if lua_toboolean(L, -2) then
                  key := 'true'
                else
                  key := 'false';
            end;
            if key <> '' then
            begin
              tt := lua_type(L, -1);
              if (tt = LUA_TFUNCTION) or (tt = LUA_TUSERDATA) or (tt = LUA_TTHREAD) then
                key := '' //members without a JSON value are left out
              else
              begin
                if first then
                  first := False
                else
                  AOut := AOut + ',';
                JsonAppendEscaped(AOut, key);
                AOut := AOut + ':';
                if not EncodeJsonValue(L, -1, AOut, AVisited, ADepth + 1, AError) then
                  Exit;
              end;
            end;
            lua_pop(L, 1); //pop the value, keep the key for the next lua_next
          end;
          AOut := AOut + '}';
        end;
      finally
        AVisited.Remove(p);
      end;
    end;
  else
    AError := 'cannot encode ' + string(lua_typename(L, tt));
    Exit;
  end;
  Result := True;
end;

//Encode the value at AIdx as a full JSON document, guarding the top-level
//types a document cannot be (a function is not JSON). Shared by json_encode,
//savejson and the table body of http_post.
function LuaValueToJson(L: Plua_State; AIdx: Integer; out AJson: string;
  out AError: string): Boolean;
var
  Visited: TList<Pointer>;
  tt: Integer;
begin
  AJson := '';
  tt := lua_type(L, AIdx);
  if (tt = LUA_TFUNCTION) or (tt = LUA_TUSERDATA) or (tt = LUA_TTHREAD) or
    (tt = LUA_TLIGHTUSERDATA) then
  begin
    AError := 'cannot encode ' + string(lua_typename(L, tt));
    Exit(False);
  end;
  Visited := TList<Pointer>.Create;
  try
    Result := EncodeJsonValue(L, AIdx, AJson, Visited, 0, AError);
  finally
    Visited.Free;
  end;
end;

//json_decode(text) -> value | nil, err
//Parses a JSON document (the answer of a web API, a file, ...) into the
//matching Lua value; see PushDonValue for the mapping. On invalid input it
//returns nil plus the parser message, like openfile does for a bad path.
function TLuaScript.JsonDecode_func(L: Plua_State): integer; cdecl;
var
  aText: string;
  Root: TDON_Value;
begin
  if not L.IsString(1) then
  begin
    L.PushNil;
    L.PushString('json_decode: missing json text');
    Exit(2);
  end;
  aText := L.ToString(1);
  Root := nil;
  try
    try
      Root := JsonParseValueString(aText);
    except
      on E: Exception do
      begin
        L.PushNil;
        L.PushString('json_decode: ' + E.Message);
        Exit(2);
      end;
    end;
    PushDonValue(L, Root);
    Result := 1;
  finally
    Root.Free;
  end;
end;

//json_encode(value) -> json text | nil, err
//The mirror of json_decode: a 1..n sequence becomes an array, any other
//table an object keyed by its member names, numbers/booleans/strings their
//JSON twins and nil null. Functions and userdata are skipped inside a table
//but refused at the top level, and a table that holds itself answers nil
//plus a message - like the other helpers, it never raises.
function TLuaScript.JsonEncode_func(L: Plua_State): integer; cdecl;
var
  aJson, aError: string;
begin
  if not LuaValueToJson(L, 1, aJson, aError) then
  begin
    L.PushNil;
    L.PushString('json_encode: ' + aError);
    Exit(2);
  end;
  L.PushString(aJson);
  Result := 1;
end;

//http_post(url, body [, content_type]) -> response text | nil, err
//The request runs on the script thread, like http_get. The body is a string
//or a table: a table is encoded with json_encode and defaults to
//application/json, a plain string defaults to text/plain. Without a body
//the request posts nothing.
function TLuaScript.HttpPost_func(L: Plua_State): integer; cdecl;
var
  aUrl, aData, aResp, aError, aType: string;
begin
  if not L.IsString(1) then
  begin
    L.PushNil;
    L.PushString('http_post: missing url');
    Exit(2);
  end;
  aUrl := L.ToString(1);
  aData := '';
  aType := 'application/json';
  if L.ArgsCount >= 2 then
  begin
    if L.IsString(2) then
    begin
      aData := L.ToString(2);
      aType := 'text/plain; charset=utf-8';
    end
    else
    begin
      //a table (or number/boolean/nil): the JSON text of it
      if not LuaValueToJson(L, 2, aData, aError) then
      begin
        L.PushNil;
        L.PushString('http_post: ' + aError);
        Exit(2);
      end;
    end;
  end;
  if (L.ArgsCount >= 3) and L.IsString(3) then
    aType := L.ToString(3);
  if StopScriptRequested then
  begin
    L.PushNil;
    L.PushString('http_post: stopped');
    Exit(2);
  end;
  aResp := '';
  aError := 'http_post: engine is not running';
  if (Engine = nil) or not Engine.HttpPost(aUrl, aData, aType, aResp, aError) then
  begin
    L.PushNil;
    L.PushString(aError);
    Exit(2);
  end;
  if StopScriptRequested then
  begin
    L.PushNil;
    L.PushString('http_post: stopped');
    Exit(2);
  end;
  L.PushString(aResp);
  Result := 1;
end;

//savejson(value, "file") -> true | nil, err
//Write the JSON text of a value into a workspace file - the same name rules
//openfile applies, so a path that walks outside the workspace is refused.
//Returns nil plus a message for an unencodable value or a failed write.
function TLuaScript.SaveJson_func(L: Plua_State): integer; cdecl;
var
  aName, aJson, aError, Resolved: string;
begin
  if not LuaValueToJson(L, 1, aJson, aError) then
  begin
    L.PushNil;
    L.PushString('savejson: ' + aError);
    Exit(2);
  end;
  if (L.ArgsCount < 2) or (not L.IsString(2)) or (L.ToString(2) = '') then
  begin
    L.PushNil;
    L.PushString('savejson: missing file name');
    Exit(2);
  end;
  aName := L.ToString(2);
  Resolved := ResolveAndValidatePath(aName, Res.WorkPath);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('savejson: ''' + aName + ''' resolves outside the workspace');
    Exit(2);
  end;
  try
    SaveFileUTF8String(Resolved, UTF8String(aJson));
    InvalidateSaveCache(L, Resolved); // the page moved: the cache is history
  except
    on E: Exception do
    begin
      L.PushNil;
      L.PushString('savejson: ' + E.Message);
      Exit(2);
    end;
  end;
  L.PushBoolean(True);
  Result := 1;
end;

//loadjson("file") -> value | nil, err
//Read a workspace file and parse it with json_decode's rules. The pair of
//savejson: a table saved is a table found again. Answers nil plus a message
//for a name outside the workspace, a missing file or invalid JSON.
function TLuaScript.LoadJson_func(L: Plua_State): integer; cdecl;
var
  aName, Resolved, aText: string;
  Root: TDON_Value;
begin
  if (not L.IsString(1)) or (L.ToString(1) = '') then
  begin
    L.PushNil;
    L.PushString('loadjson: missing file name');
    Exit(2);
  end;
  aName := L.ToString(1);
  Resolved := ResolveAndValidatePath(aName, Res.WorkPath);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('loadjson: ''' + aName + ''' resolves outside the workspace');
    Exit(2);
  end;
  //RayLib's FileExists (the raylib one) shadows SysUtils' in this unit.
  if not SysUtils.FileExists(Resolved) then
  begin
    L.PushNil;
    L.PushString('loadjson: file not found: ' + aName);
    Exit(2);
  end;
  try
    //LoadFileString reads UTF-8 and drops a BOM, which a JSON parser
    //counts as text, not as a byte-order mark.
    aText := LoadFileString(Resolved);
  except
    on E: Exception do
    begin
      L.PushNil;
      L.PushString('loadjson: ' + E.Message);
      Exit(2);
    end;
  end;
  Root := nil;
  try
    try
      Root := JsonParseValueString(aText);
    except
      on E: Exception do
      begin
        L.PushNil;
        L.PushString('loadjson: ' + E.Message);
        Exit(2);
      end;
    end;
    PushDonValue(L, Root);
    Result := 1;
  finally
    Root.Free;
  end;
end;

//The per-game save.json as a Lua table living in the registry under two
//sentinel addresses (SaveDocKey, SaveAgeKey): the document and the mtime
//stamp it was read at (-1 = no file, -2 = stale on purpose). The pair stays
//alive across script reloads, which is what makes `load("score")` answer the
//value `save("score", ...)` wrote even after F5. An absent or corrupted
//document starts as an empty page. A mtime that moved while no one was
//looking - a hand-edited file, an editor writing the same name - drops the
//cache on the next read; FPC's FileAge carries only two-second granularity,
//so a FAST foreign write is caught instead by InvalidateSaveCache, which
//savejson calls when its file is our page. Leaves the document on the
//stack; Save_func refreshes the stamp after its own write.
procedure TLuaScript.GetSaveDoc(L: Plua_State);
var
  FileName, Text: string;
  Root: TDON_Value;
  Age: Int64;
  HasDoc: Boolean;
begin
  FileName := IncludePathDelimiter(Res.WorkPath) + 'save.json';
  // the cached document and the mtime it was read at
  HasDoc := lua_rawgetp(L, LUA_REGISTRYINDEX, @SaveDocKey) = LUA_TTABLE;
  Age := -2;
  if lua_rawgetp(L, LUA_REGISTRYINDEX, @SaveAgeKey) = LUA_TNUMBER then
    Age := lua_tointeger(L, -1);
  lua_pop(L, 1); // stamp
  if HasDoc and (Age = FileAge(FileName)) then
    Exit; // the document on the stack is fresh (-2 matches nothing: rebuild)
  if HasDoc then
    lua_pop(L, 1); // stale (or deliberately invalidated) document
  // (Re)build: the JSON object in the file, or an empty page. The read is
  // done here rather than through LoadJson_func because that helper reads its
  // file name at absolute index 1, which belongs to the calling script.
  HasDoc := False;
  Age := -1; // "no file" is a stamp of its own
  if SysUtils.FileExists(FileName) then
  begin
    Age := FileAge(FileName);
    Root := nil;
    try
      try
        Text := LoadFileString(FileName); // UTF-8, BOM dropped
        Root := JsonParseValueString(Text);
        if Root is TDON_Object then
        begin
          PushDonValue(L, Root); // a fresh table of data, no aliases
          HasDoc := True;
        end;
      except
        on E: Exception do
          ; // a broken page starts empty: a first run and a bad file are kin
      end;
    finally
      Root.Free;
    end;
  end;
  if not HasDoc then
    lua_newtable(L);
  lua_pushvalue(L, -1); // keep a copy: publishing pops the stack slot
  lua_rawsetp(L, LUA_REGISTRYINDEX, @SaveDocKey); // publish the document
  lua_pushinteger(L, Age);
  lua_rawsetp(L, LUA_REGISTRYINDEX, @SaveAgeKey); // and its stamp
end;

//InvalidateSaveCache: called by savejson/loadjson when the file they touched
//IS the save page. Reading tells us nothing new and writing changes the
//truth, so in both cases a cached document must not look fresh anymore: the
//stamp becomes -2, which no real mtime matches.
procedure TLuaScript.InvalidateSaveCache(L: Plua_State; const AResolved: string);
begin
  if (L = nil) or (AResolved = '') then
    Exit;
  if SameFileName(AResolved,
    ExpandFileName(IncludePathDelimiter(Res.WorkPath) + 'save.json')) then
  begin
    lua_pushinteger(L, -2);
    lua_rawsetp(L, LUA_REGISTRYINDEX, @SaveAgeKey);
  end;
end;

//save("score", 120) / save("hiscores", {..}) -> true | nil, err
//Keeps a key in the game's own save.json, the file behind load(). The value
//is anything json_encode takes: numbers, text, booleans, tables, null. Omit
//the value, or pass nil, and the key is dropped. Each call writes the file
//at once, so closing the window - or crashing - never loses a score.
function TLuaScript.Save_func(L: Plua_State): integer; cdecl;
var
  FileName, Key, Json, Err: string;
  Wrote: Boolean;
begin
  // an empty name would be skipped by the encoder, so refuse it here
  if (not L.IsString(1)) or (L.ToString(1) = '') then
  begin
    L.PushNil;
    L.PushString('save: the first argument must be a name');
    Exit(2);
  end;
  Key := L.ToString(1);
  if (L.ArgsCount >= 2) and (lua_type(L, 2) <> LUA_TNIL) then
  begin
    // probe the value once: a function is not JSON and never lands on disk
    if not LuaValueToJson(L, 2, Json, Err) then
    begin
      L.PushNil;
      L.PushString('save: ' + Err);
      Exit(2);
    end;
  end;
  try
    GetSaveDoc(L);
    lua_replace(L, 1); // the document at index 1, whatever else was on the stack
    if L.ArgsCount >= 2 then
      L.PushValue(2) // even a bare nil: setfield then deletes the key
    else
      lua_pushnil(L);
    lua_setfield(L, 1, PUTF8Char(UTF8String(Key)));
    FileName := IncludePathDelimiter(Res.WorkPath) + 'save.json';
    Wrote := False;
    try
      if not LuaValueToJson(L, 1, Json, Err) then
        raise Exception.Create(Err);
      SaveFileUTF8String(FileName, UTF8String(Json));
      Wrote := True;
    except
      on E: Exception do
        Err := E.Message;
    end;
  finally
    lua_settop(L, 0);
  end;
  if not Wrote then
  begin
    L.PushNil;
    L.PushString('save: ' + Err);
    Exit(2);
  end;
  // our own write must not look like a foreign edit to the next call
  lua_pushinteger(L, FileAge(FileName));
  lua_rawsetp(L, LUA_REGISTRYINDEX, @SaveAgeKey);
  L.PushBoolean(True);
  Result := 1;
end;

//load("score") -> value | nil
//The mirror of save: answers what the key holds, or nil when the game never
//saved it. A missing or broken save.json is a page of empty keys, not an
//error: a first run and a hand-hacked file both just start fresh.
function TLuaScript.Load_func(L: Plua_State): integer; cdecl;
var
  Key: string;
begin
  if (not L.IsString(1)) or (L.ToString(1) = '') then
  begin
    L.PushNil;
    L.PushString('load: the first argument must be a name');
    Exit(2);
  end;
  Key := L.ToString(1);
  GetSaveDoc(L);
  lua_replace(L, 1); // the document takes the key's slot
  if lua_getfield(L, 1, PUTF8Char(UTF8String(Key))) = LUA_TNIL then
    lua_settop(L, 0)
  else
    lua_replace(L, 1);
  lua_settop(L, 1);
  Result := 1;
end;

function TLuaConsole.Read_func(L: Plua_State): integer; cdecl;
var
  s: string;
  c: integer;
  Reader: TReadConsoleObject;
begin
  s := '> ';
  c := L.ArgsCount;
  if c > 0 then
    s := L.ToString(1);

  Reader := TReadConsoleObject.Create(s);
  try
    try
      // Run the DoExecute on the Engine thread via Synchronize.
      // The object is NOT freed by the engine; we free it here.
      Reader.Run(Script.Thread);
      //Wait in short slices so F5 / stop can end a script that is waiting
      //for input instead of freezing on an un-signalled event.
      while not Reader.Wait(100) do
        if StopScriptRequested then
          Break;
      if StopScriptRequested then
        L.PushString('')
      else
        // Push the result string to Lua
        L.PushString(Reader.ResultString);
      Result := 1;
    finally
    end;
  finally
    Reader.Free;
  end;
end;

//font.load(name[, size[, codepage]]) -> id
//Reserves (or reuses) an entry in the font book and queues its load, then
//answers the id right away — the atlas itself bakes on the main thread, in
//queue order, exactly as before. Loading the same source twice answers the
//same id and never bakes the atlas twice. The old bare `font.load(name)`
//keeps working: the return value is simply unused, and the font is made
//active just as it always was.
function TLuaFont.Load_func(L: Plua_State): integer; cdecl;
var
  aFile: string;
  aSize: integer;
  aCodePage: string;
  Shell: TRayFont;
begin
  aFile := L.ToString(1);
  // Load font from current directory (ScriptPath or WorkSpace)
  aFile := Res.GuessFileName(aFile, Res.WorkPath);
  aCodePage := '';
  if L.IsNumber(2) then
    aSize := L.ToInteger(2) //LoadFontEx
  else
  begin
    aSize := 0; //LoadFont
    // size omitted: a second argument can only be the codepage
    if L.ArgsCount >= 2 then
      aCodePage := L.ToString(2);
  end;
  if L.ArgsCount >= 3 then
    aCodePage := L.ToString(3);
  //A huge size makes raylib bake an enormous glyph atlas (and can raise
  //EInvalidOp). Keep it in a sane range before it reaches the loader.
  if (aSize <> 0) and ((aSize < 1) or (aSize > 256)) then
  begin
    if aSize < 1 then
      aSize := 1
    else
      aSize := 256;
    if Engine <> nil then
      Engine.Notice(Format('Hint: font size is out of range; using %d', [aSize]));
  end;
  Shell := Res.FontShell(aFile, aSize, aCodePage);
  FScript.AddQueueObject(TLoadFontObject.Create(Shell.FontId));
  L.PushInteger(Shell.FontId);
  Result := 1;
end;

//font.use(id) -> true | nil, msg
//Makes a book entry the active font from the next queue beat. The draw
//commands read the active font when the queue runs, so a script can write
//font.use(title) then canvas.text(...) in one beat and get the right font.
function TLuaFont.Use_func(L: Plua_State): integer; cdecl;
var
  aId: Integer;
begin
  aId := L.ToInteger(1);
  if Res.Fonts.Item(aId) = nil then
  begin
    L.PushNil;
    L.PushString('font.use: no font with id ' + IntToStr(aId));
    Exit(2);
  end;
  FScript.AddQueueObject(TUseFontObject.Create(aId));
  L.PushBoolean(True);
  Result := 1;
end;

//font.free(id) -> true | nil, msg
//Releases a cached font's atlas. The one in use must be switched away first
//(font.use(other) or font.load of another) — freeing it under the renderer
//would blank the scene mid-frame.
function TLuaFont.Free_func(L: Plua_State): integer; cdecl;
var
  aId: Integer;
  Shell: TRayFont;
begin
  aId := L.ToInteger(1);
  Shell := Res.Fonts.Item(aId);
  if Shell = nil then
  begin
    L.PushNil;
    L.PushString('font.free: no font with id ' + IntToStr(aId));
    Exit(2);
  end;
  if Res.Font = Shell then
  begin
    L.PushNil;
    L.PushString('font.free: font ' + IntToStr(aId) + ' is the one in use, switch with font.use() first');
    Exit(2);
  end;
  FScript.AddQueueObject(TFreeFontObject.Create(aId));
  L.PushBoolean(True);
  Result := 1;
end;

//font.current() -> id  (the font text commands draw with right now)
function TLuaFont.Current_func(L: Plua_State): integer; cdecl;
begin
  if Res.Font = nil then
    L.PushInteger(0)
  else
    L.PushInteger(Res.Font.FontId);
  Result := 1;
end;

{ Sprites }

// First script argument of a sprite method. A dot call on a registered sprite
// table (spr.load(...)) injects the sprite table as argument 1, so the script's
// arguments start at 2. A colon call on a registered sprite table (spr:load(...))
// passes the receiver again as argument 2, so the script's arguments start at 3.
// A Pluto class method call (spr:load(...) on a `new Sprite` or subclass instance)
// is not table-injected, so slot 1 is the receiver and arguments start at 2.
function SpriteArgBase(L: Plua_State): Integer;
begin
  if (L.ArgsCount >= 2) and L.IsTable(1) and L.IsTable(2) and
    lua_rawequal(L, 1, 2) then
    Result := 3
  else
    Result := 2;
end;

// Copy the string-keyed fields from the class table at ASrcIdx onto the
// instance at AAbsIdx. Metamethod-like names ('__...') are skipped, and a
// field the instance already carries raw is kept, so engine-bound methods
// always win over same-named class fields and internal calls stay intact.
procedure LuaCopyClassFields(L: Plua_State; AAbsIdx, ASrcIdx: Integer);
var
  s: PChar;
begin
  lua_pushnil(L);
  while lua_next(L, ASrcIdx) <> 0 do
  begin
    // stack: [..., key, val]
    if lua_type(L, -2) = LUA_TSTRING then
    begin
      s := PChar(lua_tostring(L, -2));
      if (s <> nil) and not ((s[0] = '_') and (s[1] = '_')) then
      begin
        lua_pushvalue(L, -2); // key
        lua_rawget(L, AAbsIdx);
        if lua_isnil(L, -1) then
        begin
          lua_pop(L, 1);
          lua_pushvalue(L, -2); // key
          lua_pushvalue(L, -2); // val
          lua_rawset(L, AAbsIdx);
        end
        else
          lua_pop(L, 1);
      end;
    end;
    lua_pop(L, 1); // pop val, keep key for the next lua_next
  end;
end;

// Populate the table at AIdx with the sprite's __handle, __name, bound methods,
// property metatable, and registry entry. Used by both RegisterSprite (for
// sprites.new) and Construct_func (for Pluto's `new Sprite` / `extends Sprite`).
// When AIdx is a Pluto subclass instance (`class Ship extends Sprite`), its old
// metatable is remembered; after the property metatable is installed, the whole
// class chain (metatable.__index = the class, and up to four levels) is copied
// raw onto the instance, so subclass methods and fields stay callable.
procedure TLuaSprite.InitSpriteTable(L: Plua_State; AIdx: Integer; AHandle: Integer; const AName: string);
var
  absIdx, depth: Integer;
begin
  absIdx := lua_absindex(L, AIdx);

  // A Pluto instance carries a class metatable; keep it alive on the stack
  // while the property metatable replaces it, then copy from it at the end.
  // A plain table (sprites.new path) has none, so push a nil sentinel and the
  // walk below simply stops.
  if lua_getmetatable(L, absIdx) = 0 then
    lua_pushnil(L);

  // Push duplicate at top of stack because L.Register() expects the target table
  // at -1 and -2.
  L.PushValue(absIdx);
  L.PushValue(absIdx); //[sprite, sprite]

  L.PushInteger(AHandle);
  L.SetField(-2, '__handle'); //sprite.__handle = AHandle
  if AName <> '' then
  begin
    L.PushString(AName);
    L.SetField(-2, '__name');
  end;

  // methods receive the sprite table injected as argument 1, so both spr.load()
  // and spr:load() work identically
  L.Register('load', Load_func);
  L.Register('fromimage', FromImage_func);
  L.Register('loadscript', LoadScript_func);
  L.Register('show', Show_func);
  L.Register('hide', Hide_func);
  L.Register('move', Move_func);
  L.Register('width', Width_func);
  L.Register('height', Height_func);
  L.Register('play', Play_func);
  L.Register('stop', Stop_func);
  L.Register('pause', Stop_func);
  L.Register('action', Action_func);
  L.Register('framecount', FrameCount_func);
  L.Register('free', Free_func);
  L.Register('destroy', Free_func);

  // metatable with property getter/setter (no table injection, Lua passes the
  // table as arg 1)
  L.NewTable; //[sprite, sprite, meta]
  L.RegisterMeta('__index', __getter);
  L.RegisterMeta('__newindex', __setter);
  L.SetMetaTable(-2); //sprite.metatable = meta

  // remember this sprite table so collision.pump() can find it by handle
  if AHandle > cSpriteInvalid then
  begin
    L.PushValue(-1); // duplicate the sprite table
    lua_rawseti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle);
  end;

  L.Pop(2); // pop the two working duplicates, the old metatable stays on top

  // walk the old class chain and copy its fields onto the instance
  depth := 0;
  while (depth < 4) and lua_istable(L, -1) do
  begin
    LuaCopyClassFields(L, absIdx, lua_absindex(L, -1));
    lua_pushliteral(L, PUTF8Char('__index'));
    lua_rawget(L, -2);
    lua_remove(L, -2); // drop the current table, keep its chain target
    Inc(depth);
  end;
  lua_pop(L, 1); // pop the chain end (nil or non-table)
end;

// Push the global `Sprite` class table onto the stack so scripts can use Pluto's
// `new Sprite([name [, file]])` or `class MySprite extends Sprite`.
// Note: no `new` field on purpose. Pluto's `new T(...)` prefers a raw (even
// inherited) `T.new` over `T.__construct`, so a `new` field here would swallow
// both `new Sprite(...)` and `new Subclass(...)` and skip the file argument.
procedure TLuaSprite.PushClass(L: Plua_State);
begin
  L.NewTable; //[Sprite]
  L.RegisterMeta('__construct', Construct_func);
  L.RegisterMeta('find', Script.Sprites.Find_func);
  L.RegisterMeta('puff', Script.Sprites.Puff_func);
  L.RegisterMeta('blob', Script.Sprites.Blob_func);
  // Make `Sprite("name")` callable as a shortcut for `sprites("name")`
  L.NewTable; //[Sprite, meta]
  L.RegisterMeta('__call', Script.Sprites.Call_func);
  L.SetMetaTable(-2);
end;

// Pluto constructor: `new Sprite([name [, file [, script]]])`
// Also supports `new Sprite("ship.png")`: if only one string argument is given
// and it carries an image/sprite extension (.png, .aseprite, .ase, .sprite,
// .bmp, .jpg, .jpeg, .gif), it is treated as the texture file to load immediately
// and its base name becomes the sprite name.
function TLuaSprite.Construct_func(L: Plua_State): integer; cdecl;
var
  b: Integer;
  aArg1, aName, aFile, aScriptFile, ext: string;
  handle: Integer;
  CreateObj: TCreateSpriteObject;
  LoadObj: TLoadSpriteObject;
  ScriptObj: TLoadSpriteScriptObject;
begin
  Result := 0;
  if not L.IsTable(1) then
    Exit;
  // Pluto calls __construct as `__construct(self, ...)`, and subclasses may
  // also reach it through `super(...)`, so the script arguments start at 2.
  // If slot 2 is a table too (a `super(parent, self, ...)` layout), the base
  // table is slot 2.
  if (L.ArgsCount >= 2) and L.IsTable(2) then
    b := 3
  else
    b := 2;

  aName := '';
  aFile := '';
  aScriptFile := '';
  if (L.ArgsCount >= b) and L.IsString(b) then
  begin
    aArg1 := L.ToString(b);
    if (L.ArgsCount >= b + 1) and L.IsString(b + 1) then
    begin
      aName := aArg1;
      aFile := L.ToString(b + 1);
      if (L.ArgsCount >= b + 2) and L.IsString(b + 2) then
        aScriptFile := L.ToString(b + 2);
    end
    else
    begin
      ext := LowerCase(ExtractFileExt(aArg1));
      if (ext = '.png') or (ext = '.aseprite') or (ext = '.ase') or
         (ext = '.sprite') or (ext = '.bmp') or (ext = '.jpg') or
         (ext = '.jpeg') or (ext = '.gif') then
      begin
        aFile := aArg1;
        aName := ChangeFileExt(ExtractFileName(aArg1), '');
      end
      else
        aName := aArg1;
    end;
  end;

  CreateObj := TCreateSpriteObject.Create(aName);
  try
    CreateObj.Run(Script.Thread);
    handle := CreateObj.HandleResult;
  finally
    CreateObj.Free;
  end;
  if handle <= cSpriteInvalid then
    handle := -1;

  InitSpriteTable(L, b - 1, handle, aName);

  if (aFile <> '') and (handle > cSpriteInvalid) then
  begin
    aFile := Res.GuessFileName(aFile);
    LoadObj := TLoadSpriteObject.Create(aFile, aName, handle);
    try
      LoadObj.Run(Script.Thread);
      handle := LoadObj.HandleResult;
      if handle > cSpriteInvalid then
      begin
        L.PushString('__handle');
        L.PushInteger(handle);
        lua_rawset(L, b - 1);
        L.PushValue(b - 1);
        lua_rawseti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + handle);
        if aScriptFile <> '' then
        begin
          ScriptObj := TLoadSpriteScriptObject.Create(Script, handle, aScriptFile);
          FScript.AddQueueObject(ScriptObj);
        end;
      end
      else
        Script.DoError('Sprite not loaded: ' + aFile);
    finally
      LoadObj.Free;
    end;
  end;
end;

function TLuaSprite.RegisterSprite(AHandle: integer): integer;
begin
  with Script do
  begin
    Lua.State.BeginTable; //[sprite]
    InitSpriteTable(Lua.State, -1, AHandle);
    Result := 1;
  end;
end;

// Push the sprite table already registered for AHandle from the handle
// registry, or build a new one when no script table exists yet. find/call
// use this so every lookup of a sprite returns the very same table.
function TLuaSprite.PushExisting(L: Plua_State; AHandle: Integer): Integer;
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle);
  if lua_istable(L, -1) then
    Exit(1);
  L.Pop(1);
  Result := RegisterSprite(AHandle);
end;

// Sprites.new("name"?) -> creates a new sprite object, returns it on Lua stack
function TLuaSprites.New_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
  CreateObj: TCreateSpriteObject;
begin
  if L.ArgsCount >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  // Create the sprite object immediately on the Engine thread so script-only
  // ("texture-less") sprites get a real handle; load() swaps the texture in place.
  CreateObj := TCreateSpriteObject.Create(aName);
  try
    CreateObj.Run(Script.Thread); //Will run in Synchronize
    handle := CreateObj.HandleResult;
  finally
    CreateObj.Free;
  end;
  if handle > cSpriteInvalid then
    Script.Sprite.RegisterSprite(handle)
  else
    Script.Sprite.RegisterSprite(-1);
  // Store optional name
  if aName <> '' then
  begin
    L.PushString(aName);
    L.SetField(-2, '__name');
  end;
  Result := 1;
end;

{ TLuaImage }

constructor TLuaImage.Create(const AName: string; AScript: TLuaScript;
  ACanvas: TTyroCanvas);
begin
  inherited Create(AName, AScript);
  Canvas := ACanvas;
end;

function TLuaImage.GetSheet: TTyroCanvas;
begin
  Result := Canvas;
end;

//Build the sheet's Lua table. RegisterMethod keeps the argument indexes the
//inherited canvas methods expect (no table injected), so a sheet is dot-called
//exactly like the canvas globals: sheet.circle(10, 10, 4, true).
function TLuaImage.RegisterImage(AIndex: Integer; const AName: string): Integer;
begin
  with Script do
  begin
    Lua.State.BeginTable; //[sheet]
    //raw keys, set before the metatable: __sheet lets fromimage() find this
    //object back, __name is only for messages
    Lua.State.PushInteger(AIndex);
    Lua.State.SetField(-2, '__sheet'); //sheet.__sheet = AIndex
    if AName <> '' then
    begin
      Lua.State.PushString(AName);
      Lua.State.SetField(-2, '__name');
    end;
    Lua.State.RegisterMethod('clear', Self.Clear_func);
    Lua.State.RegisterMethod('text', Self.Text_func);
    Lua.State.RegisterMethod('text_center', Self.TextCenter_func);
    Lua.State.RegisterMethod('circle', Self.Circle_func);
    Lua.State.RegisterMethod('triangle', Self.Triangle_func);
    Lua.State.RegisterMethod('rectangle', Self.Rectangle_func);
    Lua.State.RegisterMethod('line', Self.Line_func);
    Lua.State.RegisterMethod('point', Self.Point_func);
    //properties: sheet.color, sheet.backcolor, sheet.pen, sheet.width ...
    Lua.State.NewTable; //[sheet, meta]
    //Self on purpose: inside "with Script" a bare __getter would bind the
    //script's accessor, and only this object knows its own Canvas.
    Lua.State.RegisterMeta('__index', Self.__getter);
    Lua.State.RegisterMeta('__newindex', Self.__setter);
    Lua.State.SetMetaTable(-2); //sheet.metatable = meta -> [sheet]
    Result := 1;
  end;
end;

{ TLuaImages }

constructor TLuaImages.Create(const AName: string; AScript: TLuaScript);
begin
  inherited Create(AName, AScript);
  FItems := TList.Create; //of TLuaImage
end;

destructor TLuaImages.Destroy;
var
  i: Integer;
begin
  if FItems <> nil then
  begin
    for i := FItems.Count - 1 downto 0 do
      TLuaImage(FItems[i]).Free; //the render texture lives in Engine.Images
    FreeAndNil(FItems);
  end;
  inherited;
end;

function TLuaImages.GetItem(AIndex: Integer): TLuaImage;
begin
  if (FItems <> nil) and (AIndex >= 0) and (AIndex < FItems.Count) then
    Result := TLuaImage(FItems[AIndex])
  else
    Result := nil;
end;

//images.new(width, height [, name]) -> a paint sheet the script draws on
//itself, the way a `canvas`, but living only in memory: no PNG has to exist
//before the game runs. sprite:fromimage(sheet) installs the pixels.
function TLuaImages.New_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  aWidth, aHeight, idx: Integer;
  Sheet: TLuaImage;
  CreateObj: TCreateImageObject;
begin
  if L.ArgsCount < 2 then
  begin
    L.PushNil;
    L.PushString('images.new: needs a width and a height');
    Exit(2);
  end;
  aWidth := L.ToInteger(1);
  aHeight := L.ToInteger(2);
  if (aWidth < 1) or (aHeight < 1) or (aWidth > 4096) or (aHeight > 4096) then
  begin
    L.PushNil;
    L.PushString('images.new: width and height must be between 1 and 4096');
    Exit(2);
  end;
  aName := '';
  if L.ArgsCount >= 3 then
    aName := L.ToString(3);
  //A render texture is GL memory: create it on the Engine thread and let the
  //engine own it (Engine.Images), a script thread can neither make nor free
  //GL objects safely.
  CreateObj := TCreateImageObject.Create(aWidth, aHeight);
  try
    CreateObj.Run(Script.Thread); //Will run in Synchronize
    if CreateObj.Canvas = nil then
    begin
      L.PushNil;
      L.PushString('images.new: could not create the paint sheet');
      Exit(2);
    end;
    Sheet := TLuaImage.Create(aName, Script, CreateObj.Canvas);
    idx := FItems.Add(Sheet);
    //the table carries its raw __sheet index; fromimage() maps it back here
    Result := Sheet.RegisterImage(idx, aName); //[sheet]
  finally
    CreateObj.Free;
  end;
end;

function TLuaImages.SheetArg(L: Plua_State; AIndex: Integer): TLuaImage;
var
  idx: Integer;
begin
  Result := nil;
  if not L.IsTable(AIndex) then
    Exit;
  L.GetField(AIndex, '__sheet');
  if L.IsNumber(-1) then
    idx := L.ToInteger(-1)
  else
    idx := -1;
  lua_pop(L, 1);
  if idx >= 0 then
    Result := Items[idx];
end;

//images.save(sheet, 'file.png') -> queue a PNG write of the sheet's pixels.
//Queued like every drawing call: the sheet's paints must all land before
//its pixels are downloaded. Returns true when the write is on its way.
function TLuaImages.Save_func(L: Plua_State): integer; cdecl;
var
  Sheet: TLuaImage;
  aName, Resolved: string;
begin
  Sheet := SheetArg(L, 1);
  if Sheet = nil then
  begin
    L.PushNil;
    L.PushString('images.save: needs a paint sheet from images.new()');
    Exit(2);
  end;
  aName := L.ToString(2);
  if aName = '' then
  begin
    L.PushNil;
    L.PushString('images.save: missing file name');
    Exit(2);
  end;
  //Like openfile, a relative name may only land inside the workspace.
  Resolved := ResolveAndValidatePath(aName, Res.WorkPath);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('images.save: ''' + aName + ''' resolves outside the workspace');
    Exit(2);
  end;
  Script.AddQueueObject(TImageSaveObject.Create(Sheet.Sheet, Resolved));
  L.PushBoolean(True);
  Result := 1;
end;

//images.savesprite('file.sprite', frames) -> cut the frame rectangles out
//of ONE paint sheet and pack them as a .sprite atlas (PNG + JSON in a ZIP,
//the sprite editor's own format). frames is an array of
//  { image = <sheet>, name = 'ride_n', x = 0, y = 0, w = 32, h = 32
//    [, duration = 100] }
//x/y/w/h may be left out to take the whole sheet; duration is milliseconds.
//Like the other image calls this is queued; true means the write is on its
//way. Returns nil plus a message on bad input.
function TLuaImages.SaveSprite_func(L: Plua_State): integer; cdecl;
var
  i, n, frTop, sw, sh: Integer;
  Sheet, Cur: TLuaImage;
  aName, Resolved, BaseName: string;
  Frames: array of TSheetFrame;
  Ok: Boolean;

  //number field of the frame table on top of the stack, or Fallback
  function NumField(const AField: string; ADefault: Integer): Integer;
  begin
    L.GetField(frTop, AField);
    if L.IsNumber(-1) then
      Result := L.ToInteger(-1)
    else
      Result := ADefault;
    lua_pop(L, 1);
  end;

begin
  aName := L.ToString(1);
  if aName = '' then
  begin
    L.PushNil;
    L.PushString('images.savesprite: missing file name');
    Exit(2);
  end;
  if not L.IsTable(2) then
  begin
    L.PushNil;
    L.PushString('images.savesprite: needs a list of frames');
    Exit(2);
  end;
  n := Integer(lua_rawlen(L, 2));
  if n < 1 then
  begin
    L.PushNil;
    L.PushString('images.savesprite: the frame list is empty');
    Exit(2);
  end;

  Sheet := nil;
  sw := 0;
  sh := 0;
  Ok := True;
  SetLength(Frames, n);
  for i := 1 to n do
  begin
    lua_rawgeti(L, 2, i);
    frTop := lua_gettop(L);
    Ok := lua_istable(L, frTop);
    if Ok then
    begin
      //all frames share the one sheet the first frame names
      L.GetField(frTop, 'image');
      Cur := SheetArg(L, -1);
      lua_pop(L, 1);
      if Cur = nil then
        Ok := False
      else if Sheet = nil then
      begin
        Sheet := Cur;
        L.GetField(frTop, 'image');
        L.GetField(-1, 'width');
        sw := L.ToInteger(-1);
        lua_pop(L, 2);
        L.GetField(frTop, 'image');
        L.GetField(-1, 'height');
        sh := L.ToInteger(-1);
        lua_pop(L, 2);
      end
      else if Sheet <> Cur then
        Ok := False; //one atlas, one paint sheet
    end;
    if Ok then
    begin
      Frames[i - 1].Name := '';
      L.GetField(frTop, 'name');
      if L.IsString(-1) then
        Frames[i - 1].Name := L.ToString(-1);
      lua_pop(L, 1);
      if Frames[i - 1].Name = '' then
        Frames[i - 1].Name := 'frame' + IntToStr(i - 1);
      Frames[i - 1].X := NumField('x', 0);
      Frames[i - 1].Y := NumField('y', 0);
      Frames[i - 1].W := NumField('w', sw);
      Frames[i - 1].H := NumField('h', sh);
      Frames[i - 1].DurationMs := NumField('duration', 0);
      if (Frames[i - 1].X < 0) or (Frames[i - 1].Y < 0)
        or (Frames[i - 1].W < 1) or (Frames[i - 1].H < 1)
        or (Frames[i - 1].X + Frames[i - 1].W > sw)
        or (Frames[i - 1].Y + Frames[i - 1].H > sh) then
        Ok := False;
    end;
    lua_settop(L, frTop - 1); //drop the frame table
    if not Ok then
    begin
      L.PushNil;
      L.PushString('images.savesprite: frame ' + IntToStr(i) + ' must name one'
        + ' shared sheet and a rectangle inside it');
      Exit(2);
    end;
  end;

  Resolved := ResolveAndValidatePath(aName, Res.WorkPath);
  if Resolved = '' then
  begin
    L.PushNil;
    L.PushString('images.savesprite: ''' + aName + ''' resolves outside the workspace');
    Exit(2);
  end;
  BaseName := ChangeFileExt(ExtractFileName(Resolved), '');
  if BaseName = '' then
    BaseName := 'sheet';
  Script.AddQueueObject(TSaveSpriteObject.Create(Sheet.Sheet, Resolved,
    BaseName + '.png', Frames));
  L.PushBoolean(True);
  Result := 1;
end;

// Sprites.puff("name" [, size] [, count]) / Sprites.blob("name" [, size] [, count])
// -> shared body. Puff: a sprite carrying an engine-generated soft puff
// texture: a solid core with a feathered, lumpy rim. The arcade cloud
// and smoke trick: stack a few of these with jittered position/scale/alpha and
// the soft edges fuse into one fluffy mass. Blob: the SAME lumpy rim, but the
// alpha cuts off at the coast within a pixel - an organic silhouette with a
// crisp edge for islands, rocks and decor that must not look fluffy. Size is
// the texture in px (default
// 128). A count above 1 builds that many in ONE hop and returns a table
// of them - a whole sky of clouds should not stall on one round-trip per puff.
function TLuaSprites.PuffOrBlob(L: Plua_State; AHard: boolean): integer; cdecl;
var
  aName, aOne: string;
  aSize, aCount, I, handle: integer;
  PuffObj: TPuffSpriteObject;

  procedure PushNamed(const AName: string);
  begin
    if AName <> '' then
    begin
      L.PushString(AName);
      L.SetField(-2, '__name');
    end;
  end;

begin
  if L.ArgsCount >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  if L.ArgsCount >= 2 then
    aSize := Round(L.ToNumber(2))
  else
    aSize := 128;
  if L.ArgsCount >= 3 then
    aCount := Round(L.ToNumber(3))
  else
    aCount := 1;
  PuffObj := TPuffSpriteObject.Create(aName, aSize, aCount, AHard);
  try
    PuffObj.Run(Script.Thread); //Will run in Synchronize
    if Length(PuffObj.Handles) <= 1 then
    begin
      if Length(PuffObj.Handles) > 0 then
        handle := PuffObj.Handles[0]
      else
        handle := cSpriteInvalid;
      if handle > cSpriteInvalid then
        Script.Sprite.RegisterSprite(handle)
      else
        Script.Sprite.RegisterSprite(-1);
      PushNamed(aName);
      Result := 1;
    end
    else
    begin
      // the batch form: puffs = Sprites.puff("cloud", 128, 40)
      lua_createtable(L, Length(PuffObj.Handles), 0);
      for I := 0 to High(PuffObj.Handles) do
      begin
        handle := PuffObj.Handles[I];
        if handle > cSpriteInvalid then
          Script.Sprite.RegisterSprite(handle)
        else
          Script.Sprite.RegisterSprite(-1);
        if aName <> '' then
        begin
          aOne := aName + IntToStr(I + 1);
          PushNamed(aOne);
        end;
        lua_rawseti(L, -2, I + 1);
      end;
      Result := 1;
    end;
  finally
    PuffObj.Free;
  end;
end;

// Sprites.puff("name" [, size] [, count]) -> the soft cloud puff, see PuffOrBlob
function TLuaSprites.Puff_func(L: Plua_State): integer; cdecl;
begin
  Result := PuffOrBlob(L, False);
end;

// Sprites.blob("name" [, size] [, count]) -> the crisp-rim twin of the puff
function TLuaSprites.Blob_func(L: Plua_State): integer; cdecl;
begin
  Result := PuffOrBlob(L, True);
end;

// Sprites.find("name") -> returns a sprite object for the named sprite, or nil
function TLuaSprites.Find_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(1);
  handle := Engine.Sprites.FindByName(aName);
  if handle > cSpriteInvalid then
    Result := Script.Sprite.PushExisting(L, handle)
  else
  begin
    L.PushNil;
    Result := 1;
  end;
end;

// Sprites("name") -> __call metamethod, same as Sprites.find
function TLuaSprites.Call_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(2); // first arg after table self
  handle := Engine.Sprites.FindByName(aName);
  if handle > cSpriteInvalid then
    Result := Script.Sprite.PushExisting(L, handle)
  else
  begin
    L.PushNil;
    Result := 1;
  end;
end;

//__handle must be read raw: the 'controls' and 'sprites' tables carry no
//__handle of their own, so lua_getfield would re-enter the Getter sitting on
//their __index and recurse until Lua reports "C stack overflow". That is what
//answers a lookup of a name the table does not hold, so the read has to stay
//recursion-free.
//The key goes on top of an unchanged idx: lua_rawget pops the key and pushes
//the raw value in its place, leaving the frame exactly as it was found (a
//leaked value would inflate L.ArgsCount and turn every getter into a setter).
//0 means "not a sprite/control table".
function GetSpriteHandle(L: Plua_State; idx: integer): integer;
begin
  L.PushString('__handle');
  lua_rawget(L, idx);
  Result := L.PopInteger;
end;

function GetControlTableHandle(L: Plua_State; idx: integer): integer;
begin
  L.PushString('__handle');
  lua_rawget(L, idx);
  Result := L.PopInteger;
end;

//Same raw read as GetSpriteHandle, for a tilemap table.
function GetTileHandle(L: Plua_State; idx: integer): integer;
begin
  L.PushString('__handle');
  lua_rawget(L, idx);
  Result := L.PopInteger;
end;

//a caption control (button/label/checkbox) keeps its text in Caption, every
//other control (an edit) in the shared Text. TSetControlTextObject writes to
//the same place, so a read here always answers what a write stored.
function GetControlText(AControl: TTyroControl): utf8string;
begin
  if AControl is TTyroCaptionControl then
    Result := TTyroCaptionControl(AControl).Caption
  else
    Result := AControl.Text;
end;

//Maps a border style value onto the enum: 0=none, 1=thin, 2=thick, 3=sizable.
//Anything out of range means "no border", the same answer controls.border
//gives a setter for a style it does not know.
function BorderOfValue(AValue: Integer): TBorder;
begin
  case AValue of
    1: Result := brdThin;
    2: Result := brdThick;
    3: Result := brdSizable;
  else
    Result := brdNone;
  end;
end;

//Maps a Lua value onto the docking edge: 'none', 'left', 'top', 'right',
//'bottom' or 'client', or the enum value 0..5. False when the value names no
//edge, so the caller can answer nil instead of silently docking nowhere.
function TryAlignOfValue(const AValue: string; out AAlign: TAlign): Boolean;
var
  s: string;
  n: Integer;
begin
  s := LowerCase(Trim(AValue));
  if TryStrToInt(s, n) and (n >= Ord(alNone)) and (n <= Ord(alClient)) then
    AAlign := TAlign(n)
  else if s = 'none' then
    AAlign := alNone
  else if s = 'left' then
    AAlign := alLeft
  else if s = 'top' then
    AAlign := alTop
  else if s = 'right' then
    AAlign := alRight
  else if s = 'bottom' then
    AAlign := alBottom
  else if s = 'client' then
    AAlign := alClient
  else
    Exit(False);
  Result := True;
end;

//The name of a docking edge, as controls.align and the align field read it.
function AlignName(AAlign: TAlign): string;
begin
  case AAlign of
    alLeft: Result := 'left';
    alTop: Result := 'top';
    alRight: Result := 'right';
    alBottom: Result := 'bottom';
    alClient: Result := 'client';
  else
    Result := 'none';
  end;
end;

//The border style as the number the border field and controls.border use.
function BorderStyleValue(ABorder: TBorder): Integer;
begin
  case ABorder of
    brdThin: Result := 1;
    brdThick: Result := 2;
    brdSizable: Result := 3;
  else
    Result := 0;
  end;
end;

//Resolves the container of a control: the handle a script can pass straight
//back into controls.parent, so 0 answers "the Engine window". Shared by the
//parent field and controls.parent.
function ParentHandleOf(AControls: TList; AControl: TTyroControl): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to AControls.Count - 1 do
  begin
    if (TTyroControl(AControls[i]) <> nil) and (TTyroControl(AControls[i]) = AControl.Parent) then
    begin
      Result := i + 1;
      Break;
    end;
  end;
end;

//Where the first value a script passed sits in the arguments of a control
//function. The control is always argument 1, either as a table or as a handle.
//A method registered on the control table gets that table pushed in front of the
//script's arguments, so lst.additem("a") arrives as (table, "a") - but Lua also
//passes the receiver to a colon call, so lst:additem("a") arrives as
//(table, table, "a"). Both spellings mean the same call, so a method reads its
//values from this position and not from a fixed one.
function FirstArg(L: PLua_State): Integer;
begin
  Result := 2;
  if (L.ArgsCount >= 2) and L.IsTable(1) and L.IsTable(2) and
    lua_rawequal(L, 1, 2) then
    Result := 3;
end;

{ TLuaControls }

constructor TLuaControls.Create(const AName: string; AScript: TLuaScript);
begin
  inherited;
  FItems := TList.Create;
end;

destructor TLuaControls.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TLuaControls.GetControl(AHandle: Integer): TTyroControl;
begin
  Result := nil;
  if (AHandle >= 1) and (AHandle <= FItems.Count) then
    Result := TTyroControl(FItems[AHandle - 1]);
end;

function TLuaControls.FindByName(const AName: string): Integer;
var
  i: Integer;
  c: TTyroControl;
begin
  Result := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    c := TTyroControl(FItems[i]);
    if (c <> nil) and (SameText(c.Name, AName)) then
    begin
      Result := i + 1;
      Exit;
    end;
  end;
end;

function TLuaControls.Setter(L: PLua_State): integer;
var
  ctrl: TTyroControl;
  parentCtrl: TTyroControl;
  field: string;
  aAlign: TAlign;
  r: TRect;
  v: Integer;
  Items: TStringList;
begin
  Result := 0;
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    Exit;
  field := LowerCase(L.ToString(2));

  if (field = 'x') or (field = 'left') or (field = 'top') or (field = 'y') or (field = 'width') or (field = 'height') then
  begin
    //geometry: a write changes one number of the rect and keeps the rest, so
    //x/y move the control and width/height resize it
    r := ctrl.BoundsRect;
    v := round(L.ToNumber(3));
    if (field = 'x') or (field = 'left') then
      r := Rect(v, r.Top, v + r.Width, r.Bottom)
    else if (field = 'y') or (field = 'top') then
      r := Rect(r.Left, v, r.Right, v + r.Height)
    else if field = 'width' then
      r := Rect(r.Left, r.Top, r.Left + v, r.Bottom)
    else
      r := Rect(r.Left, r.Top, r.Right, r.Top + v);
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Exit;
  end
  else if field = 'visible' then
  begin
    FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl, L.ToBoolean(3)));
    Exit;
  end
  else if field = 'border' then
  begin
    FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, BorderOfValue(round(L.ToNumber(3)))));
    Exit;
  end
  else if field = 'backcolor' then
  begin
    if L.IsNumber(3) then
      FScript.RunQueueObject(TSetControlBackColorObject.Create(ctrl, IntToColor(round(L.ToNumber(3)))));
    Exit;
  end
  else if field = 'name' then
  begin
    FScript.RunQueueObject(TSetControlNameObject.Create(ctrl, L.ToString(3)));
    Exit;
  end
  else if field = 'align' then
  begin
    if TryAlignOfValue(L.ToString(3), aAlign) then
      FScript.RunQueueObject(TSetControlAlignObject.Create(ctrl, aAlign));
    Exit;
  end
  else if field = 'parent' then
  begin
    //a nil or zero handle moves the control back to the Engine window
    if lua_isnil(L, 3) or (L.IsNumber(3) and (round(L.ToNumber(3)) <= 0)) then
      FScript.RunQueueObject(TSetControlParentObject.Create(ctrl, Engine.Window))
    else
    begin
      if L.IsTable(3) then
        parentCtrl := GetControl(GetControlTableHandle(L, 3))
      else
        parentCtrl := GetControl(round(L.ToNumber(3)));
      if parentCtrl <> nil then
        FScript.RunQueueObject(TSetControlParentObject.Create(ctrl, parentCtrl));
    end;
    Exit;
  end
  else if (field = 'text') or (field = 'caption') then
  begin
    //btn.text = "Hi" is the property form of controls.text(handle, "Hi")
    FScript.RunQueueObject(TSetControlTextObject.Create(ctrl, L.ToString(3)));
    Exit;
  end
  else if field = 'checked' then
  begin
    //chk.checked = true, the property form of controls.checked(handle, v)
    if ctrl is TTyroCheckBox then
      FScript.RunQueueObject(TSetControlCheckedObject.Create(TTyroCheckBox(ctrl), L.ToBoolean(3)));
    Exit;
  end
  else if field = 'file' then
  begin
    //img.file = "logo.png" is the property form of img.load(...)
    if ctrl is TTyroImage then
      FScript.RunQueueObject(TLoadControlImageObject.Create(ctrl, L.ToString(3)));
    Exit;
  end
  else if ctrl is TTyroListBox then
  begin
    //listbox fields; the ones that need a value only act on a listbox
    if field = 'items' then
    begin
      //lst.items = {"a", "b"} replaces the list, a single string replaces it
      //with one item and nil (or false) empties it
      Items := TStringList.Create;
      try
        if L.IsTable(3) then
        begin
          lua_pushnil(L);
          while lua_next(L, 3) <> 0 do
          begin
            //the key stays below the value, so the value is on top
            Items.Add(L.ToString(-1));
            lua_pop(L, 1);
          end;
        end
        else if not lua_isnil(L, 3) and not lua_isboolean(L, 3) then
          Items.Add(L.ToString(3));
        FScript.RunQueueObject(TSetControlItemsObject.Create(TTyroListBox(ctrl), Items));
      finally
        Items.Free;
      end;
      Exit;
    end
    else if field = 'itemindex' then
    begin
      FScript.RunQueueObject(TSetControlItemIndexObject.Create(TTyroListBox(ctrl), round(L.ToNumber(3))));
      Exit;
    end
    else if field = 'viewcount' then
    begin
      FScript.RunQueueObject(TSetControlViewCountObject.Create(TTyroListBox(ctrl), round(L.ToNumber(3))));
      Exit;
    end;
  end;
  // fallback: store raw
  L.PushValue(2); L.PushValue(3); lua_rawset(L, 1);
end;

//A control handle is a table of properties: every name below answers the
//current state of the control, and the Setter accepts the same names to change
//it. A control table also carries the few action methods (show, hide, move,
//position, focus, additem, item, clear, load) that have no property meaning.
function TLuaControls.Getter(L: PLua_State): integer;
var
  ctrl: TTyroControl;
  field: string;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  field := LowerCase(L.ToString(2));
  if field = 'count' then
  begin
    L.PushInteger(FItems.Count);
    Result := 1;
    Exit;
  end;
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if (field = 'x') or (field = 'left') then
  begin
    L.PushInteger(ctrl.BoundsRect.Left);
    Result := 1;
    Exit;
  end
  else if (field = 'y') or (field = 'top') then
  begin
    L.PushInteger(ctrl.BoundsRect.Top);
    Result := 1;
    Exit;
  end
  else if field = 'width' then
  begin
    L.PushInteger(ctrl.Width);
    Result := 1;
    Exit;
  end
  else if field = 'height' then
  begin
    L.PushInteger(ctrl.Height);
    Result := 1;
    Exit;
  end
  else if field = 'visible' then
  begin
    L.PushBoolean(ctrl.Visible);
    Result := 1;
    Exit;
  end
  else if field = 'hover' then
  begin
    L.PushBoolean(ctrl.Hover);
    Result := 1;
    Exit;
  end
  else if field = 'down' then
  begin
    L.PushBoolean(ctrl.Down);
    Result := 1;
    Exit;
  end
  else if field = 'clicked' then
  begin
    L.PushBoolean(ctrl.Clicked);
    Result := 1;
    Exit;
  end
  else if field = 'focused' then
  begin
    L.PushBoolean(ctrl.Focused);
    Result := 1;
    Exit;
  end
  else if field = 'name' then
  begin
    L.PushString(ctrl.Name);
    Result := 1;
    Exit;
  end
  else if field = 'parent' then
  begin
    //0 is the Engine window, a value controls.parent takes back unchanged
    L.PushInteger(ParentHandleOf(FItems, ctrl));
    Result := 1;
    Exit;
  end
  else if field = 'border' then
  begin
    L.PushInteger(BorderStyleValue(ctrl.Border));
    Result := 1;
    Exit;
  end
  else if field = 'backcolor' then
  begin
    L.PushInteger(ColorToInt(ctrl.BackColor));
    Result := 1;
    Exit;
  end
  else if field = 'align' then
  begin
    L.PushString(AlignName(ctrl.Align));
    Result := 1;
    Exit;
  end
  else if (field = 'text') or (field = 'caption') then
  begin
    //the caption of a button/label/checkbox, the edited text of an edit
    L.PushString(GetControlText(ctrl));
    Result := 1;
    Exit;
  end
  else if field = 'checked' then
  begin
    L.PushBoolean((ctrl is TTyroCheckBox) and TTyroCheckBox(ctrl).Checked);
    Result := 1;
    Exit;
  end
  else if field = 'loaded' then
  begin
    //true once an image control holds a texture
    L.PushBoolean((ctrl is TTyroImage) and TTyroImage(ctrl).Loaded);
    Result := 1;
    Exit;
  end
  else if ctrl is TTyroListBox then
  begin
    //listbox fields; the ones that need a listbox answer nil on anything else
    if field = 'items' then
      L.PushInteger(TTyroListBox(ctrl).Items.Count)
    else if field = 'itemindex' then
      L.PushInteger(TTyroListBox(ctrl).ItemIndex)
    else if field = 'viewcount' then
      L.PushInteger(TTyroListBox(ctrl).ViewCount)
    else
    begin
      L.PushValue(2);
      lua_rawget(L, 1);
      Result := 1;
      Exit;
    end;
    Result := 1;
    Exit;
  end;
  // fallback
  L.PushValue(2); lua_rawget(L, 1); Result := 1;
end;

function TLuaControls.RegisterControl(AHandle: Integer): Integer;
var
  ctrl: TTyroControl;
  base: Integer;
begin
  with Script do
  begin
    Lua.State.BeginTable;
    base := Lua.State.ArgsCount;

    Lua.State.PushValue(-1);
    Lua.State.PushInteger(AHandle);
    Lua.State.SetField(-2, '__handle');

    ctrl := GetControl(AHandle);
    if ctrl <> nil then
    begin
      Lua.State.PushString(ctrl.Name);
      Lua.State.SetField(-2, '__name');
    end;

    // A control table is a property bag first: the Getter answers every field
    // (x, width, text, checked, itemindex, ...) and the Setter takes it back, so
    // a field must NOT be registered here as a method - a raw field wins over
    // __index and over __newindex, which would make the property both unreadable
    // and silently dead. Only the actions that have no property spelling are
    // registered, and they take their control from the self argument, so they
    // are called with the colon form (btn:show()). controls.<name>(handle, ...)
    // takes a handle or a table and stays the plain spelling.
    if ctrl is TTyroListBox then
    begin
      Lua.State.Register('clear', Clear_func);
      Lua.State.Register('additem', AddItem_func);
      Lua.State.Register('item', Item_func);
    end
    else if ctrl is TTyroImage then
    begin
      //loaded is read as a field (img.loaded), not a call, see Controls.Getter
      Lua.State.Register('load', LoadImage_func);
    end;

    if ctrl is TTyroControl then
    begin
      Lua.State.Register('focus', Focus_func);
    end;

    if ctrl is TTyroLayout then
    begin
      Lua.State.Register('show', Show_func);
      Lua.State.Register('hide', Hide_func);
      Lua.State.Register('move', Move_func);
      Lua.State.Register('position', Position_func);
    end;

    Lua.State.NewTable;
    Lua.State.RegisterMeta('__index', Controls.__getter);
    Lua.State.RegisterMeta('__newindex', Controls.__setter);
    Lua.State.SetMetaTable(-2);

    if AHandle >= 1 then
    begin
      Lua.State.PushValue(-1);
      lua_rawseti(Lua.State, LUA_REGISTRYINDEX, cControlRegistryBase + AHandle);
    end;

    Lua.State.Remove(base);
    Result := 1;
  end;
end;

//controls.new(class, name, x?, y?, w?, h?) -> handle
//(created on the Engine thread, self-drawn by the engine's drawing cycle; the
//returned handle is 1-based over the controls created by this script)
//legacy call buttons.new(caption, x?, y?, w?, h?, borderSize?) still works
function TLuaControls.New_func(L: Plua_State): integer; cdecl;
var
  clsName, caption, aName: string;
  c: Integer;
  x, y, w, h: Integer;
  popStart: Integer; //position of the first optional numeric argument
  argIndex: Integer;
  CreateObj: TCreateControlObject;
begin
  Result := 1;
  c := L.ArgsCount;
  caption := L.ToString(1);
  clsName := LowerCase(caption);
  x := 0;
  y := 0;
  aName := '';
  w := 500; h := 200;
  if (clsName = 'button') or (clsName = 'panel') or (clsName = 'label') or
     (clsName = 'checkbox') or (clsName = 'edit') or (clsName = 'spectrum') or
     (clsName = 'listbox') or (clsName = 'image') then
  begin
    //new style: controls.new(class, name, x?, y?, w?, h?)
    //if second arg is number, skip the name
    clsName := caption;
    aName := '';
    if c >= 2 then
    begin
      if L.IsNumber(2) then
      begin
        popStart := 2;
        caption := '';
      end
      else
      begin
        aName := L.ToString(2);
        popStart := 3;
        caption := '';
        if (clsName = 'button') or (clsName = 'label') or (clsName = 'checkbox') or
           (clsName = 'edit') then
          caption := aName
        else if clsName = 'image' then
          caption := aName;
      end;
    end
    else
    begin
      popStart := 2;
      caption := '';
    end;
    if clsName = 'button' then
    begin
      w := 100; h := 32;
    end
    else if clsName = 'panel' then
    begin
      w := 100; h := 100;
    end
    else if clsName = 'label' then
    begin
      w := 120; h := 24;
    end
    else if clsName = 'checkbox' then
    begin
      w := 120; h := 24;
    end
    else if clsName = 'edit' then
    begin
      w := 140; h := 28;
    end
    else if clsName = 'spectrum' then
    begin
      w := 500; h := 200;
    end
    else if clsName = 'listbox' then
    begin
      w := 160; h := 120;
    end
    else if clsName = 'image' then
    begin
      //same default as TTyroImage itself; a loaded texture that is larger
      //grows the control, a smaller one keeps these bounds
      w := 64; h := 64;
    end;
  end
  else
  begin
    //legacy: buttons.new(caption, x?, y?, w?, h?, borderSize?)
    clsName := 'button';
    popStart := 2;
    w := 100; h := 32;
  end;

  argIndex := popStart;
  if c >= argIndex then
  begin
    if L.IsNumber(argIndex) then
    begin
      x := round(L.ToNumber(argIndex));
      argIndex := argIndex + 1;
    end;
  end;
  if c >= argIndex then
  begin
    if L.IsNumber(argIndex) then
    begin
      y := round(L.ToNumber(argIndex));
      argIndex := argIndex + 1;
    end;
  end;
  if c >= argIndex then
  begin
    if L.IsNumber(argIndex) then
    begin
      w := round(L.ToNumber(argIndex));
      argIndex := argIndex + 1;
    end;
  end;
  if c >= argIndex then
  begin
    if L.IsNumber(argIndex) then
    begin
      h := round(L.ToNumber(argIndex));
      argIndex := argIndex + 1;
    end;
  end;

  CreateObj := TCreateControlObject.Create(clsName, caption, x, y, w, h, aName);
  try
    CreateObj.Run(Script.Thread); //Will run in Synchronize
    if CreateObj.Control <> nil then
    begin
      FItems.Add(CreateObj.TakeControl);
      RegisterControl(FItems.Count); //push control table
      Result := 1;
    end
    else
      L.PushNil;
  finally
    CreateObj.Free;
  end;
end;

//controls.caption(handle [, text]) -> get/set the caption (button/label/checkbox)
function TLuaControls.Caption_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if ctrl is TTyroCaptionControl then
  begin
    if L.ArgsCount >= 2 then
    begin
      FScript.RunQueueObject(TSetControlTextObject.Create(ctrl, L.ToString(2)));
      Result := 0;
    end
    else
    begin
      L.PushString((ctrl as TTyroCaptionControl).Caption);
      Result := 1;
    end;
  end
  else
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
end;

//controls.text(handle [, text]) -> get/set the control text: the caption of a
//button, label or checkbox, the edited text of an edit. Reading it answers the
//same string as controls.caption does for a caption control.
function TLuaControls.Text_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    FScript.RunQueueObject(TSetControlTextObject.Create(ctrl, L.ToString(2)));
    Result := 0;
  end
  else
  begin
    L.PushString(GetControlText(ctrl));
    Result := 1;
  end;
end;

function TLuaControls.Checked_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if ctrl is TTyroCheckBox then
  begin
    if L.ArgsCount >= 2 then
    begin
      FScript.RunQueueObject(TSetControlCheckedObject.Create(ctrl as TTyroCheckBox, L.ToBoolean(2)));
      Result := 0;
    end
    else
    begin
      L.PushBoolean((ctrl as TTyroCheckBox).Checked);
      Result := 1;
    end;
  end
  else
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
end;

//controls.position(handle) -> x, y ; controls.position(handle, x, y) -> move
function TLuaControls.Position_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  r: TRect;
  i: Integer;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  i := FirstArg(L);
  if (i + 1) <= L.ArgsCount then
  begin
    r := ctrl.BoundsRect;
    r := Rect(round(L.ToNumber(i)), round(L.ToNumber(i + 1)),
              round(L.ToNumber(i)) + r.Width, round(L.ToNumber(i + 1)) + r.Height);
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ctrl.BoundsRect.Left);
    L.PushInteger(ctrl.BoundsRect.Top);
    Result := 2;
  end;
end;

//controls.move(handle, x, y) -> move the control (keeps its size)
function TLuaControls.Move_func(L: Plua_State): integer; cdecl;
begin
  Result := Position_func(L);
end;

//controls.width(handle [, w]) -> get/set the width
function TLuaControls.Width_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  r: TRect;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    r := ctrl.BoundsRect;
    r.Right := r.Left + round(L.ToNumber(2));
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ctrl.Width);
    Result := 1;
  end;
end;

//controls.height(handle [, h]) -> get/set the height
function TLuaControls.Height_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  r: TRect;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    r := ctrl.BoundsRect;
    r.Bottom := r.Top + round(L.ToNumber(2));
    FScript.RunQueueObject(TSetControlBoundsObject.Create(ctrl, r));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ctrl.Height);
    Result := 1;
  end;
end;

//controls.visible(handle [, value]) -> get/set visibility
function TLuaControls.Visible_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.ArgsCount >= 2 then
 begin
  FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl,
    L.ToBoolean(2)));
    Result := 0;
  end
  else
  begin
    L.PushBoolean(ctrl.Visible);
    Result := 1;
  end;
end;

//controls.show(handle) / btn:show() -> make the control visible
function TLuaControls.Show_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  //btn:show() hands over its control table as the self argument,
  //controls.show(handle) hands over a handle
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl <> nil then
    FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl, True));
  Result := 0;
end;

//controls.hide(handle) / btn:hide() -> make the control invisible
function TLuaControls.Hide_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl <> nil then
    FScript.RunQueueObject(TSetControlVisibleObject.Create(ctrl, False));
  Result := 0;
end;

//controls.hover(handle) -> is the mouse over the control
function TLuaControls.Hover_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Hover);
  Result := 1;
end;

//controls.down(handle) -> is the control pressed (mouse down over it)
function TLuaControls.Down_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Down);
  Result := 1;
end;

//controls.clicked(handle) -> true once after the control is released (click)
function TLuaControls.Clicked_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Clicked);
  Result := 1;
end;

//controls.focused(handle) -> is the control focused (owns keyboard input)
function TLuaControls.Focused_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
    L.PushBoolean(False)
  else
    L.PushBoolean(ctrl.Focused);
  Result := 1;
end;

//controls.focus(handle) -> move the keyboard focus to the control (on the
//Engine thread, like every change of the input state)
function TLuaControls.Focus_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl <> nil then
    FScript.RunQueueObject(TSetControlFocusObject.Create(ctrl));
  Result := 0;
end;

//controls.border(handle [, style]) -> get/set the border style
//0=none, 1=thin, 2=thick, 3=sizable
function TLuaControls.Border_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    FScript.RunQueueObject(TSetControlBorderObject.Create(ctrl, BorderOfValue(round(L.ToNumber(2)))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(BorderStyleValue(ctrl.Border));
    Result := 1;
  end;
end;

//controls.backcolor(handle [, color]) -> get/set the back color as an int
//(use colors.name or an #rrggbb int from the colors table)
function TLuaControls.BackColor_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.ArgsCount >= 2 then
 begin
  FScript.RunQueueObject(TSetControlBackColorObject.Create(ctrl,
    IntToColor(round(L.ToNumber(2)))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(ColorToInt(ctrl.BackColor));
    Result := 1;
  end;
end;

//controls.name(handle [, name]) -> get/set the control name (named controls
//resolve as Lua globals, e.g. controls.new('button', 'OK', 0, 0, ..., 'ok')
//makes the global 'ok' a handle)
function TLuaControls.Name_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
 if L.ArgsCount >= 2 then
 begin
    FScript.RunQueueObject(TSetControlNameObject.Create(ctrl, L.ToString(2)));
    Result := 0;
  end
  else
  begin
    L.PushString(ctrl.Name);
    Result := 1;
  end;
end;

//controls.align(handle [, value]) -> get/set the docking edge of a control.
//Values: 'none', 'left', 'top', 'right', 'bottom', 'client' (or 0..5).
function TLuaControls.Align_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  aAlign: TAlign;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if L.ArgsCount >= 2 then
  begin
    if TryAlignOfValue(L.ToString(2), aAlign) then
    begin
      FScript.RunQueueObject(TSetControlAlignObject.Create(ctrl, aAlign));
      Result := 0;
    end
    else
    begin
      L.PushNil;
      Result := 1;
    end;
  end
  else
  begin
    L.PushString(AlignName(ctrl.Align));
    Result := 1;
  end;
end;

//controls.parent(handle [, parentHandle]) -> get/set the container. The getter
//returns 0 when the control belongs to the Engine window, and the setter moves
//it back there for a nil or zero handle.
function TLuaControls.Parent_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  parentCtrl: TTyroControl;
  newParent: TTyroLayout;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if ctrl = nil then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;

  if L.ArgsCount >= 2 then
  begin
    if lua_isnil(L, 2) or (L.IsNumber(2) and (round(L.ToNumber(2)) <= 0)) then
      newParent := Engine.Window
  else
  begin
    if L.IsTable(2) then
    parentCtrl := GetControl(GetControlTableHandle(L, 2))
  else
    parentCtrl := GetControl(round(L.ToNumber(2)));
    if parentCtrl = nil then
    begin
      L.PushNil;
      Result := 1;
      Exit;
    end;
    newParent := parentCtrl;
  end;
    FScript.RunQueueObject(TSetControlParentObject.Create(ctrl, newParent));
    Result := 0;
  end
  else
  begin
    //Return 0 for the Engine window so the result can be passed straight back
    //to the setter.
    L.PushInteger(ParentHandleOf(FItems, ctrl));
    Result := 1;
  end;
end;

//controls.items(handle [, item1, item2, ...]) -> replace all items; the items
//may also come as one table of strings (controls.items(h, {"a", "b"})), the
//spelling the items field uses. With no extra argument returns the item count.
function TLuaControls.Items_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  lb: TTyroListBox;
  i: Integer;
  Items: TStringList;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  lb := TTyroListBox(ctrl);
  if L.ArgsCount >= 2 then
  begin
    Items := TStringList.Create;
    try
      //a table of items as one argument, else one item per argument
      if L.IsTable(2) then
      begin
        lua_pushnil(L);
        while lua_next(L, 2) <> 0 do
        begin
          //the key stays below the value, so the value is on top
          Items.Add(L.ToString(-1));
          lua_pop(L, 1);
        end;
      end
      else
        for i := 2 to L.ArgsCount do
          Items.Add(L.ToString(i));
      FScript.RunQueueObject(TSetControlItemsObject.Create(lb, Items));
    finally
      Items.Free;
    end;
    Result := 0;
  end
  else
  begin
    L.PushInteger(lb.Items.Count);
    Result := 1;
  end;
end;

//controls.item(handle, index [, text]) -> get/set a single item
function TLuaControls.Item_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  lb: TTyroListBox;
  idx, i: Integer;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  lb := TTyroListBox(ctrl);
  i := FirstArg(L);
  if i > L.ArgsCount then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  idx := round(L.ToNumber(i));
  if (i + 1) <= L.ArgsCount then
  begin
    FScript.RunQueueObject(TSetControlItemObject.Create(lb, idx, L.ToString(i + 1)));
    Result := 0;
  end
  else
  begin
    if (idx < 0) or (idx >= lb.Items.Count) then
      L.PushNil
    else
      L.PushString(lb.Items[idx]);
    Result := 1;
  end;
end;

//controls.additem(handle, text) -> append an item
function TLuaControls.AddItem_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  i: Integer;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  i := FirstArg(L);
  if (ctrl <> nil) and (ctrl is TTyroListBox) and (i <= L.ArgsCount) then
    FScript.RunQueueObject(TAddControlItemObject.Create(TTyroListBox(ctrl), L.ToString(i)));
  Result := 0;
end;

//controls.clear(handle) -> remove all items
function TLuaControls.Clear_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl <> nil) and (ctrl is TTyroListBox) then
    FScript.RunQueueObject(TClearControlItemsObject.Create(TTyroListBox(ctrl)));
  Result := 0;
end;

//img.load("logo.png") / img:load("logo.png") / controls.load(handle, "logo.png")
//Loads the texture an image control shows, replacing the one it holds. An
//empty (or missing) file name releases it. The upload runs on the Engine thread
//(it needs the GL context the control is painted with) and a texture bigger
//than the control grows it. Answers with the new state, so a file that could
//not be read reports false just like img.loaded does.
function TLuaControls.LoadImage_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
  aFile: string;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  //A method registered on the control table is handed that table as argument 1,
  //and lst:load("a.png") passes it a second time, so the file name sits one slot
  //lower than for lst.load("a.png"). The last argument is the file in every
  //accepted spelling.
  if L.ArgsCount >= 2 then
    aFile := L.ToString(L.ArgsCount)
  else
    aFile := '';
  if (ctrl <> nil) and (ctrl is TTyroImage) then
  begin
    FScript.RunQueueObject(TLoadControlImageObject.Create(ctrl, aFile));
    //the upload already happened, so the flag is the answer of the call
    L.PushBoolean(TTyroImage(ctrl).Loaded);
  end
  else
    L.PushBoolean(False);
  Result := 1;
end;

//controls.viewcount(handle [, n]) -> get/set the visible rows (0 = boundsrect size)
function TLuaControls.ViewCount_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    FScript.RunQueueObject(TSetControlViewCountObject.Create(TTyroListBox(ctrl), round(L.ToNumber(2))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(TTyroListBox(ctrl).ViewCount);
    Result := 1;
  end;
end;

//controls.itemindex(handle [, n]) -> get/set the selected row (-1 = none)
function TLuaControls.ItemIndex_func(L: Plua_State): integer; cdecl;
var
  ctrl: TTyroControl;
begin
  if L.IsTable(1) then
    ctrl := GetControl(GetControlTableHandle(L, 1))
  else
    ctrl := GetControl(round(L.ToNumber(1)));
  if (ctrl = nil) or not (ctrl is TTyroListBox) then
  begin
    L.PushNil;
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= 2 then
  begin
    FScript.RunQueueObject(TSetControlItemIndexObject.Create(TTyroListBox(ctrl), round(L.ToNumber(2))));
    Result := 0;
  end
  else
  begin
    L.PushInteger(TTyroListBox(ctrl).ItemIndex);
    Result := 1;
  end;
end;
function TLuaSprite.Load_func(L: Plua_State): integer; cdecl;
var
  b: integer;
  aFile, aName, aScriptFile: string;
  handle: integer;
  LoadObj: TLoadSpriteObject;
  ScriptObj: TLoadSpriteScriptObject;
begin
  b := SpriteArgBase(L);
  aFile := Res.GuessFileName(L.ToString(b));
  if L.ArgsCount >= b + 1 then
    aScriptFile := L.ToString(b + 1);
  L.GetField(1, '__name');
  if L.IsString(-1) then
    aName := L.ToString(-1)
  else
    aName := ExtractFileName(aFile);
  L.Pop(1);
  // reuse the handle created by Sprites.new() if this sprite already exists
  handle := GetSpriteHandle(L, 1);
  if handle <= cSpriteInvalid then
    handle := cSpriteInvalid;
  LoadObj := TLoadSpriteObject.Create(aFile, aName, handle);
  try
    LoadObj.Run(Script.Thread);
    handle := LoadObj.HandleResult;
    if handle > cSpriteInvalid then
    begin
      L.PushInteger(handle);
      L.SetField(1, '__handle');
      // re-index the sprite table under its real handle (it was keyed as unloaded)
      L.PushValue(1);
      lua_rawseti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + handle);
      // optional per-sprite script; compiled on the Engine thread by the engine queue
      if aScriptFile <> '' then
      begin
        ScriptObj := TLoadSpriteScriptObject.Create(Script, handle, aScriptFile);
        FScript.AddQueueObject(ScriptObj);
      end;
    end
    else
      Script.DoError('Sprite not loaded: ' + aFile);
  finally
    LoadObj.Free;
  end;
  Result := 0;
end;

// sprite:fromimage(sheet) -> installs the pixels of a paint sheet made by
// images.new() as this sprite's texture. It can be called again after the
// sheet is redrawn, and on a pooled sprite to swap its look, the same way
// sprite:load() swaps a file. The sheet keeps its own copy of the pixels, so
// one sheet can dress any number of sprites.
function TLuaSprite.FromImage_func(L: Plua_State): integer;
var
  b: Integer;
  aName: string;
  handle, idx: Integer;
  Sheet: TLuaImage;
begin
  b := SpriteArgBase(L);
  L.GetField(1, '__name');
  if L.IsString(-1) then
    aName := L.ToString(-1)
  else
    aName := '';
  L.Pop(1);
  handle := GetSpriteHandle(L, 1);
  if not L.IsTable(b) then
  begin
    Script.DoError('sprite:fromimage: needs a paint sheet from images.new()');
    Exit(0);
  end;
  L.GetField(b, '__sheet');
  if L.IsInteger(-1) then
    idx := L.PopInteger
  else
  begin
    L.Pop(1);
    idx := -1;
  end;
  Sheet := Script.Images.Items[idx];
  if Sheet = nil then
  begin
    Script.DoError('sprite:fromimage: ''' + L.ToString(b) + ''' is not a paint sheet');
    Exit(0);
  end;
  //Queued, not Run(): the sheet's draw commands live in the same queue and
  //must all land before its pixels are copied out of the texture.
  FScript.AddQueueObject(TSpriteImageObject.Create(Sheet.Sheet, aName, handle));
  Result := 0;
end;

// sprite:loadscript("file.tyro") -> compile & attach a per-sprite script (Engine thread)
function TLuaSprite.LoadScript_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
  aScriptFile: string;
  ScriptObj: TLoadSpriteScriptObject;
begin
  b := SpriteArgBase(L);
  handle := GetSpriteHandle(L, 1);
  aScriptFile := L.ToString(b);
  if (handle > cSpriteInvalid) and (aScriptFile <> '') then
  begin
    ScriptObj := TLoadSpriteScriptObject.Create(Script, handle, aScriptFile);
    FScript.AddQueueObject(ScriptObj);
  end;
  Result := 0;
end;

function TLuaSprite.Show_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Engine.Sprites.SetVisible(handle, True);
  Result := 0;
end;

function TLuaSprite.Hide_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Engine.Sprites.SetVisible(handle, False);
  Result := 0;
end;

function TLuaSprite.Move_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
  x, y: single;
begin
  b := SpriteArgBase(L);
  x := L.ToNumber(b);
  y := L.ToNumber(b + 1);
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Engine.Sprites.SetPosition(handle, x, y);
  Result := 0;
end;

function TLuaSprite.Width_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Engine.Sprites.GetWidth(handle));
  Result := 1;
end;

function TLuaSprite.Height_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Engine.Sprites.GetHeight(handle));
  Result := 1;
end;

// sprite:play([fps]) -> restart and play the animation. fps overrides the
// per-frame durations stored in the .aseprite file; 0 (or omitted) keeps them.
function TLuaSprite.Play_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := SpriteArgBase(L);
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
  begin
    if L.ArgsCount >= b then
    begin
      if L.IsString(b) then
      begin
        //sprite:play("walk") selects that named action and starts it;
        //sprite:play("walk", 12) also overrides its frame rate.
        Engine.Sprites.SetAction(handle, L.ToString(b));
        if L.ArgsCount >= b + 1 then
          Engine.Sprites.SetAnimSpeed(handle, L.ToNumber(b + 1));
      end
      else
      begin
        //sprite:play(12) keeps the old meaning: speed for the current sequence.
        Engine.Sprites.SetAnimSpeed(handle, L.ToNumber(b));
        Engine.Sprites.RestartAnim(handle);
      end;
    end
    else
      Engine.Sprites.RestartAnim(handle);
    Engine.Sprites.SetPlaying(handle, True);
  end;
  Result := 0;
end;

// sprite:action() -> name of the current action ('' when none)
// sprite:action("walk") -> select it and rewind, returns true/false
function TLuaSprite.Action_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := SpriteArgBase(L);
  handle := GetSpriteHandle(L, 1);
  if L.ArgsCount >= b then
  begin
    if handle > cSpriteInvalid then
      L.PushBoolean(Engine.Sprites.SetAction(handle, L.ToString(b)))
    else
      L.PushBoolean(False);
  end
  else if handle > cSpriteInvalid then
    L.PushString(Engine.Sprites.GetActionName(handle))
  else
    L.PushString('');
  Result := 1;
end;

// sprite:stop() / sprite:pause() -> freeze the animation on the current frame
function TLuaSprite.Stop_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
    Engine.Sprites.SetPlaying(handle, False);
  Result := 0;
end;

// sprite:framecount() -> number of frames in the loaded .aseprite animation
function TLuaSprite.FrameCount_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetSpriteHandle(L, 1);
  L.PushInteger(Engine.Sprites.GetFrameCount(handle));
  Result := 1;
end;

// sprite:free() (alias destroy) -> drop the sprite now: its textures, name,
// script hook and draw entry are gone. The handle is never reused, so this
// table simply stops addressing anything: reads answer 0/false and writes do
// nothing. Returns true when a sprite was freed.
function TLuaSprite.Free_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
  FreeObj: TFreeSpriteObject;
begin
  Result := 0;
  handle := GetSpriteHandle(L, 1);
  if handle > cSpriteInvalid then
  begin
    // Drop the registry entry first: collision events and every name lookup
    // stop at once, even if a frame passes before the release runs.
    L.PushNil;
    lua_rawseti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + handle);
    FreeObj := TFreeSpriteObject.Create(handle);
    try
      FreeObj.Run(Script.Thread);
      L.PushBoolean(FreeObj.Removed);
    finally
      FreeObj.Free;
    end;
    Result := 1;
  end
  else
  begin
    L.PushBoolean(False);
    Result := 1;
  end;
end;

// Sprite __index: read properties x, y, angle, scale, visible + physics keys; unknown keys raw-read from the sprite table
function TLuaSprite.Getter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key
  handle := GetSpriteHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle > cSpriteInvalid then
  begin
    if field = 'x' then
    begin
      L.PushNumber(Engine.Sprites.GetX(handle));
      Result := 1;
    end
    else if field = 'y' then
    begin
      L.PushNumber(Engine.Sprites.GetY(handle));
      Result := 1;
    end
    else if field = 'angle' then
    begin
      L.PushNumber(Engine.Sprites.GetAngle(handle));
      Result := 1;
    end
    else if field = 'scale' then
    begin
      L.PushNumber(Engine.Sprites.GetScale(handle));
      Result := 1;
    end
    else if field = 'visible' then
    begin
      L.PushBoolean(Engine.Sprites.GetVisible(handle));
      Result := 1;
    end
    else if field = 'collide' then
    begin
      L.PushBoolean(Engine.Sprites.GetCollide(handle));
      Result := 1;
    end
    else if field = 'kind' then
    begin
      case Engine.Sprites.GetKind(handle) of
        skKinematic: L.PushString('kinematic');
        skStatic: L.PushString('static');
      else
        L.PushString('dynamic');
      end;
      Result := 1;
    end
    else if field = 'mass' then
    begin
      L.PushNumber(Engine.Sprites.GetMass(handle));
      Result := 1;
    end
    else if field = 'friction' then
    begin
      L.PushNumber(Engine.Sprites.GetFriction(handle));
      Result := 1;
    end
    else if field = 'bouncy' then
    begin
      L.PushNumber(Engine.Sprites.GetBouncy(handle));
      Result := 1;
    end
    else if field = 'radius' then
    begin
      L.PushNumber(Engine.Sprites.GetRadius(handle));
      Result := 1;
    end
    else if field = 'sensor' then
    begin
      L.PushBoolean(Engine.Sprites.GetSensor(handle));
      Result := 1;
    end
    else if field = 'group' then
    begin
      L.PushInteger(Engine.Sprites.GetGroup(handle));
      Result := 1;
    end
    else if field = 'category' then
    begin
      L.PushInteger(Engine.Sprites.GetCategory(handle));
      Result := 1;
    end
    else if field = 'mask' then
    begin
      L.PushInteger(Engine.Sprites.GetMask(handle));
      Result := 1;
    end
    else if field = 'vx' then
    begin
      L.PushNumber(Engine.Sprites.GetVelocityX(handle));
      Result := 1;
    end
    else if field = 'vy' then
    begin
      L.PushNumber(Engine.Sprites.GetVelocityY(handle));
      Result := 1;
    end
    else if field = 'frames' then
    begin
      L.PushInteger(Engine.Sprites.GetFrameCount(handle));
      Result := 1;
    end
    else if field = 'actions' then
    begin
      // number of named actions in the file (0 for plain sheets)
      L.PushInteger(Engine.Sprites.GetActionCount(handle));
      Result := 1;
    end
    else if field = 'frame' then
    begin
      L.PushInteger(Engine.Sprites.GetAnimFrame(handle));
      Result := 1;
    end
    else if field = 'playing' then
    begin
      L.PushBoolean(Engine.Sprites.GetPlaying(handle));
      Result := 1;
    end
    else if field = 'speed' then
    begin
      L.PushNumber(Engine.Sprites.GetAnimSpeed(handle));
      Result := 1;
    end
    else if field = 'looping' then
    begin
      L.PushBoolean(Engine.Sprites.GetLooping(handle));
      Result := 1;
    end
    else if field = 'flipH' then
    begin
      L.PushBoolean(Engine.Sprites.GetFlipH(handle));
      Result := 1;
    end
    else if field = 'flipV' then
    begin
      L.PushBoolean(Engine.Sprites.GetFlipV(handle));
      Result := 1;
    end
    else if field = 'alpha' then
    begin
      L.PushInteger(Engine.Sprites.GetAlpha(handle));
      Result := 1;
    end
    else if field = 'tint' then
    begin
      L.PushInteger(ColorToInt(Engine.Sprites.GetTint(handle)));
      Result := 1;
    end
    else if field = 'layer' then
    begin
      L.PushInteger(Engine.Sprites.GetLayer(handle));
      Result := 1;
    end
    else if field = 'shader' then
    begin
      L.PushString(Engine.Sprites.GetShader(handle));
      Result := 1;
    end
    else if (field = 'shaderValue') or (field = 'shader_value') then
    begin
      L.PushNumber(Engine.Sprites.GetShaderValue(handle));
      Result := 1;
    end
    else if field = 'width' then
    begin
      L.PushValue(2); // key
      lua_rawget(L, 1); // raw table value
      if lua_isnil(L, -1) then
      begin
        L.Pop(1);
        L.PushInteger(Engine.Sprites.GetWidth(handle));
      end;
      Result := 1;
    end
    else if field = 'height' then
    begin
      L.PushValue(2); // key
      lua_rawget(L, 1); // raw table value
      if lua_isnil(L, -1) then
      begin
        L.Pop(1);
        L.PushInteger(Engine.Sprites.GetHeight(handle));
      end;
      Result := 1;
    end
    else if field = 'name' then
    begin
      L.PushString(Engine.Sprites.GetName(handle));
      Result := 1;
    end;
  end;
  if Result = 0 then
  begin
    // unknown key (e.g. onCollide) or unloaded sprite: read raw from the sprite table
    L.PushValue(2);
    lua_rawget(L, 1);
    Result := 1;
  end;
end;

// Sprite __newindex: write properties x, y, angle, scale, visible + physics keys; unknown keys raw-stored
function TLuaSprite.Setter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
  k: TSpriteKind;
begin
  // arg 1 is the table, arg 2 is the key, arg 3 is the value
  handle := GetSpriteHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle <= cSpriteInvalid then
  begin
    // unloaded sprite: store unknown keys (like onCollide) raw so they survive load()
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
    Exit;
  end;
  if (field = 'x') or (field = 'y') then
  begin
    //One lock round-trip: the other axis is read inside the store under the
    //same lock instead of GetX + GetY + SetPosition (three of them).
    Engine.Sprites.SetPositionComponent(handle, field = 'x', L.ToNumber(3));
  end
  else if field = 'angle' then
  begin
    Engine.Sprites.SetAngle(handle, L.ToNumber(3));
  end
  else if field = 'scale' then
  begin
    Engine.Sprites.SetScale(handle, L.ToNumber(3));
  end
  else if field = 'visible' then
  begin
    Engine.Sprites.SetVisible(handle, L.ToBoolean(3));
  end
  else if field = 'collide' then
  begin
    Engine.Sprites.SetCollide(handle, L.ToBoolean(3));
  end
  else if field = 'kind' then
  begin
    if L.ToString(3) = 'kinematic' then
      k := skKinematic
    else if L.ToString(3) = 'static' then
      k := skStatic
    else
      k := skDynamic;
    Engine.Sprites.SetKind(handle, k);
  end
  else if field = 'mass' then
  begin
    Engine.Sprites.SetMass(handle, L.ToNumber(3));
  end
  else if field = 'friction' then
  begin
    Engine.Sprites.SetFriction(handle, L.ToNumber(3));
  end
  else if field = 'bouncy' then
  begin
    Engine.Sprites.SetBouncy(handle, L.ToNumber(3));
  end
  else if field = 'radius' then
  begin
    Engine.Sprites.SetRadius(handle, L.ToNumber(3));
  end
  else if field = 'sensor' then
  begin
    Engine.Sprites.SetSensor(handle, L.ToBoolean(3));
  end
  else if field = 'group' then
  begin
    Engine.Sprites.SetGroup(handle, Cardinal(L.ToInteger(3)));
  end
  else if field = 'category' then
  begin
    Engine.Sprites.SetCategory(handle, Cardinal(L.ToInteger(3)));
  end
  else if field = 'mask' then
  begin
    Engine.Sprites.SetMask(handle, Cardinal(L.ToInteger(3)));
  end
  else if field = 'vx' then
  begin
    Engine.Sprites.SetVelocity(handle, L.ToNumber(3), Engine.Sprites.GetVelocityY(handle));
  end
  else if field = 'vy' then
  begin
    Engine.Sprites.SetVelocity(handle, Engine.Sprites.GetVelocityX(handle), L.ToNumber(3));
  end
  else if field = 'frame' then
  begin
    Engine.Sprites.SetAnimFrame(handle, L.ToInteger(3));
  end
  else if field = 'playing' then
  begin
    Engine.Sprites.SetPlaying(handle, L.ToBoolean(3));
  end
  else if field = 'speed' then
  begin
    Engine.Sprites.SetAnimSpeed(handle, L.ToNumber(3));
  end
  else if field = 'looping' then
  begin
    Engine.Sprites.SetLooping(handle, L.ToBoolean(3));
  end
  else if field = 'flipH' then
  begin
    Engine.Sprites.SetFlipH(handle, L.ToBoolean(3));
  end
  else if field = 'flipV' then
  begin
    Engine.Sprites.SetFlipV(handle, L.ToBoolean(3));
  end
  else if (field = 'alpha') or (field = 'tint') or (field = 'layer') then
  begin
    //style properties live in the engine, never in the sprite table: a raw
    //store here would shadow the metatable getter (Lua reads the table before
    //consulting __index), so a non-number is ignored instead of stored
    if L.IsNumber(3) then
    begin
      if field = 'alpha' then
        //0..255 opacity, folded into the tint's alpha channel
        Engine.Sprites.SetAlpha(handle, Integer(Round(L.ToNumber(3))))
      else if field = 'tint' then
        //same integer format as canvas.color / colors.* ($AABBGGRR)
        Engine.Sprites.SetTint(handle, IntToColor(L.ToInteger(3)))
      else
        //draw order: lower layers are drawn first (behind)
        Engine.Sprites.SetLayer(handle, L.ToInteger(3));
    end;
  end
  else if field = 'shader' then
  begin
    Engine.Sprites.SetShader(handle, L.ToString(3));
  end
  else if (field = 'shaderValue') or (field = 'shader_value') then
  begin
    Engine.Sprites.SetShaderValue(handle, L.ToNumber(3));
  end
  else if field = 'name' then
  begin
    Engine.Sprites.SetName(handle, L.ToString(3));
    L.PushString('__name');
    L.PushValue(3);
    lua_rawset(L, 1);
  end
  {else if (field = 'width') or (field = 'height') then
  begin
    // Store raw: GPU textures cannot be resized in place; the stored value
    // acts as an override that scripts can read back via sprite.width/height.
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
  end}
  else
  begin
    // unknown key (e.g. onCollide = function): store it raw in the sprite table
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
  end;
end;

{ TLuaTile }

//First script argument of a tilemap method. A dot call (map.get(col, row))
//injects the map table as argument 1, so the script's arguments start at 2.
//A colon call (map:get(col, row)) additionally passes the receiver table, so
//the map arrives again as argument 2 and the script's arguments start at 3.
//Tile methods accept only numbers and strings, so a table in slot 2 can only
//be the receiver of a colon call.
function TileArgBase(L: Plua_State): Integer;
begin
  if L.IsTable(2) then
    Result := 3
  else
    Result := 2;
end;

// Build a tilemap table bound to AHandle and leave it on the Lua stack. Mirrors
// TLuaSprite.RegisterSprite: the methods are plain table fields that receive
// the map table injected as argument 1, while the properties resolve through
// the metatable getter/setter (Lua passes the table on its own there).
function TLuaTile.RegisterTile(AHandle: integer): integer;
var
  base: integer;
begin
  with Script do
  begin
    Lua.State.BeginTable; //[map]
    base := Lua.State.ArgsCount; //index of the map table

    //keep a duplicate, the -3 addressing in Register() hits the map table
    Lua.State.PushValue(-1); //[map, map]

    Lua.State.PushInteger(AHandle);
    Lua.State.SetField(-2, '__handle'); //map.__handle = AHandle

    //methods receive the map table injected as argument 1
    //Self.Load_func on purpose: inside `with Script do' a bare Load_func would
    //silently bind TLuaScript.Load_func (the global load() persistence call),
    //not the tileset loader.
    Lua.State.Register('load', Self.Load_func);
    Lua.State.Register('resize', Resize_func);
    Lua.State.Register('fill', Fill_func);
    Lua.State.Register('clear', Clear_func);
    Lua.State.Register('set', Set_func);
    Lua.State.Register('get', Get_func);
    Lua.State.Register('data', Data_func);
    Lua.State.Register('solidtile', SolidTile_func);
    Lua.State.Register('solid', Solid_func);
    Lua.State.Register('draw', Draw_func);

    //metatable with property getter/setter (no table injection, Lua passes the table as arg 1)
    Lua.State.NewTable; //[map, map, meta]
    Lua.State.RegisterMeta('__index', __getter);
    Lua.State.RegisterMeta('__newindex', __setter);
    Lua.State.SetMetaTable(-2); //map.metatable = meta

    // remember this map table so it can be found by handle
    if AHandle > cTileMapInvalid then
    begin
      Lua.State.PushValue(-1); // duplicate the map table
      lua_rawseti(Lua.State, LUA_REGISTRYINDEX, cTileRegistryBase + AHandle);
    end;

    Lua.State.Remove(base); //drop the first reference, keep one on the stack
    Result := 1;
  end;
end;

// tiles.new("name"?) -> creates a new map object, returns it on the Lua stack
function TLuaTiles.New_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  if L.ArgsCount >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  //AddEmpty only touches the store's dictionary under its lock, so it is safe
  //to call straight from the script thread; only the texture upload needs the
  //main thread, and load() routes that through the engine queue.
  handle := Engine.Tiles.AddEmpty(aName);
  if handle > cTileMapInvalid then
    Script.Tile.RegisterTile(handle)
  else
    Script.Tile.RegisterTile(cTileMapInvalid);
  // Store optional name
  if aName <> '' then
  begin
    L.PushString(aName);
    L.SetField(-2, '__name');
  end;
  Result := 1;
end;

// tiles.find("name") -> returns the named map object, or nil
function TLuaTiles.Find_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(1);
  handle := Engine.Tiles.FindByName(aName);
  if handle > cTileMapInvalid then
    Script.Tile.RegisterTile(handle)
  else
    L.PushNil;
  Result := 1;
end;

// map.load(file [, tilew [, tileh [, columns [, spacing [, margin]]]]])
// Loads (or replaces) the tileset texture. The upload needs the GL context, so
// it runs on the main thread through the engine queue.
function TLuaTile.Load_func(L: Plua_State): integer; cdecl;
var
  b: integer;
  aFile: string;
  handle: integer;
  tw, th, cols, spacing, margin: integer;
  LoadObj: TLoadTileMapObject;
begin
  b := TileArgBase(L);
  aFile := Res.GuessFileName(L.ToString(b));
  tw := 16;
  th := 16;
  cols := 0;
  spacing := 0;
  margin := 0;
  if L.ArgsCount >= b + 1 then
    tw := L.ToInteger(b + 1);
  if L.ArgsCount >= b + 2 then
    th := L.ToInteger(b + 2);
  if L.ArgsCount >= b + 3 then
    cols := L.ToInteger(b + 3);
  if L.ArgsCount >= b + 4 then
    spacing := L.ToInteger(b + 4);
  if L.ArgsCount >= b + 5 then
    margin := L.ToInteger(b + 5);

  handle := GetTileHandle(L, 1);
  if handle <= cTileMapInvalid then
    handle := Engine.Tiles.AddEmpty('');
  LoadObj := TLoadTileMapObject.Create(handle, aFile, tw, th, cols, spacing, margin);
  try
    LoadObj.Run(Script.Thread);
    if LoadObj.HandleResult then
    begin
      L.PushInteger(handle);
      L.SetField(1, '__handle');
      // re-index the map table under its real handle
      L.PushValue(1);
      lua_rawseti(L, LUA_REGISTRYINDEX, cTileRegistryBase + handle);
    end
    else
      Script.DoError('Tileset not loaded: ' + aFile);
  finally
    LoadObj.Free;
  end;
  Result := 0;
end;

// map.resize(cols, rows) -> grow/shrink the grid, keeping the overlap
function TLuaTile.Resize_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := TileArgBase(L);
  handle := GetTileHandle(L, 1);
  if handle > cTileMapInvalid then
    Engine.Tiles.Resize(handle, L.ToInteger(b), L.ToInteger(b + 1));
  Result := 0;
end;

// map.fill(tile) -> set every cell to one tile id (0 clears the map)
function TLuaTile.Fill_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := TileArgBase(L);
  handle := GetTileHandle(L, 1);
  if handle > cTileMapInvalid then
    Engine.Tiles.Fill(handle, L.ToInteger(b));
  Result := 0;
end;

// map.clear() -> every cell empty
function TLuaTile.Clear_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetTileHandle(L, 1);
  if handle > cTileMapInvalid then
    Engine.Tiles.ClearMap(handle);
  Result := 0;
end;

// map.set(col, row, tile) -> set one cell (out-of-range is ignored)
function TLuaTile.Set_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := TileArgBase(L);
  handle := GetTileHandle(L, 1);
  if handle > cTileMapInvalid then
    Engine.Tiles.SetTile(handle, L.ToInteger(b), L.ToInteger(b + 1), L.ToInteger(b + 2));
  Result := 0;
end;

// map.get(col, row) -> the tile id of one cell (0 when empty or out of range)
function TLuaTile.Get_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := TileArgBase(L);
  handle := GetTileHandle(L, 1);
  if handle > cTileMapInvalid then
    L.PushInteger(Engine.Tiles.GetTile(handle, L.ToInteger(b), L.ToInteger(b + 1)))
  else
    L.PushInteger(0);
  Result := 1;
end;

// map.data() -> a flat array of tile ids, row-major (index = row * cols + col + 1)
function TLuaTile.Data_func(L: Plua_State): integer; cdecl;
var
  handle, i: integer;
  data: TArray<Integer>;
begin
  handle := GetTileHandle(L, 1);
  data := nil;
  if handle > cTileMapInvalid then
    Engine.Tiles.GetData(handle, data);
  L.NewTable;
  for i := 0 to Length(data) - 1 do
  begin
    L.PushInteger(data[i]);
    lua_rawseti(L, -2, i + 1);
  end;
  Result := 1;
end;

// map.solidtile(tile [, solid]) -> read or set whether a tile id is a wall
function TLuaTile.SolidTile_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := TileArgBase(L);
  handle := GetTileHandle(L, 1);
  if handle <= cTileMapInvalid then
  begin
    L.PushBoolean(False);
    Result := 1;
    Exit;
  end;
  if L.ArgsCount >= b + 1 then
  begin
    Engine.Tiles.SetSolidTile(handle, L.ToInteger(b), L.ToBoolean(b + 1));
    Result := 0;
  end
  else
  begin
    L.PushBoolean(Engine.Tiles.GetSolidTile(handle, L.ToInteger(b)));
    Result := 1;
  end;
end;

// map.solid(col, row) -> true when the cell is outside the map or a solid tile
function TLuaTile.Solid_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := TileArgBase(L);
  handle := GetTileHandle(L, 1);
  if handle > cTileMapInvalid then
    L.PushBoolean(Engine.Tiles.SolidAt(handle, L.ToInteger(b), L.ToInteger(b + 1)))
  else
    L.PushBoolean(True);
  Result := 1;
end;

// map.draw([x, y [, scale]]) -> enqueue ONE native draw command; the tile loop
// runs on the main thread. Position/scale default to the map's own properties.
function TLuaTile.Draw_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
  x, y, scale: single;
begin
  b := TileArgBase(L);
  handle := GetTileHandle(L, 1);
  if handle <= cTileMapInvalid then
    Exit(0);
  x := Engine.Tiles.GetX(handle);
  y := Engine.Tiles.GetY(handle);
  scale := Engine.Tiles.GetScale(handle);
  if L.ArgsCount >= b then
    x := L.ToNumber(b);
  if L.ArgsCount >= b + 1 then
    y := L.ToNumber(b + 1);
  if L.ArgsCount >= b + 2 then
    scale := L.ToNumber(b + 2);
  FScript.AddQueueObject(TDrawTileMapObject.Create(Engine.Tiles, handle, x, y, scale, clWhite, Engine.Board));
  Result := 0;
end;

// map.__index: read properties cols, rows, width, height, tilewidth,
// tileheight, columns, spacing, margin, visible, x, y, scale, name, collide;
// unknown keys raw-read from the map table.
function TLuaTile.Getter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key
  handle := GetTileHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle > cTileMapInvalid then
  begin
    if field = 'cols' then
    begin
      L.PushInteger(Engine.Tiles.GetCols(handle));
      Result := 1;
    end
    else if field = 'rows' then
    begin
      L.PushInteger(Engine.Tiles.GetRows(handle));
      Result := 1;
    end
    else if field = 'width' then
    begin
      L.PushInteger(Engine.Tiles.GetWidth(handle));
      Result := 1;
    end
    else if field = 'height' then
    begin
      L.PushInteger(Engine.Tiles.GetHeight(handle));
      Result := 1;
    end
    else if field = 'tilewidth' then
    begin
      L.PushInteger(Engine.Tiles.GetTileWidth(handle));
      Result := 1;
    end
    else if field = 'tileheight' then
    begin
      L.PushInteger(Engine.Tiles.GetTileHeight(handle));
      Result := 1;
    end
    else if field = 'columns' then
    begin
      L.PushInteger(Engine.Tiles.GetColumns(handle));
      Result := 1;
    end
    else if field = 'spacing' then
    begin
      L.PushInteger(Engine.Tiles.GetSpacing(handle));
      Result := 1;
    end
    else if field = 'margin' then
    begin
      L.PushInteger(Engine.Tiles.GetMargin(handle));
      Result := 1;
    end
    else if field = 'visible' then
    begin
      L.PushBoolean(Engine.Tiles.GetVisible(handle));
      Result := 1;
    end
    else if field = 'x' then
    begin
      L.PushNumber(Engine.Tiles.GetX(handle));
      Result := 1;
    end
    else if field = 'y' then
    begin
      L.PushNumber(Engine.Tiles.GetY(handle));
      Result := 1;
    end
    else if field = 'scale' then
    begin
      L.PushNumber(Engine.Tiles.GetScale(handle));
      Result := 1;
    end
    else if field = 'name' then
    begin
      L.PushString(Engine.Tiles.GetName(handle));
      Result := 1;
    end
    else if field = 'collide' then
    begin
      L.PushBoolean(Engine.Tiles.GetCollide(handle));
      Result := 1;
    end;
  end;
  if Result = 0 then
  begin
    // unknown key or unloaded map: read raw from the map table
    L.PushValue(2);
    lua_rawget(L, 1);
    Result := 1;
  end;
end;

// map.__newindex: write properties visible, x, y, scale, name, collide; unknown
// keys raw-stored in the map table.
function TLuaTile.Setter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key, arg 3 is the value
  handle := GetTileHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle <= cTileMapInvalid then
  begin
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
    Exit;
  end;
  if field = 'visible' then
  begin
    Engine.Tiles.SetVisible(handle, L.ToBoolean(3));
  end
  else if field = 'x' then
  begin
    Engine.Tiles.SetPositionComponent(handle, True, L.ToNumber(3));
  end
  else if field = 'y' then
  begin
    Engine.Tiles.SetPositionComponent(handle, False, L.ToNumber(3));
  end
  else if field = 'scale' then
  begin
    Engine.Tiles.SetScale(handle, L.ToNumber(3));
  end
  else if field = 'name' then
  begin
    Engine.Tiles.SetName(handle, L.ToString(3));
  end
  else if field = 'collide' then
  begin
    Engine.Tiles.SetCollide(handle, L.ToBoolean(3));
  end
  else
  begin
    // unknown key: store it raw in the map table
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
  end;
end;

{ TLuaModel }

//Read the model table's __handle raw, without re-entering the metatable getter
//(see GetSpriteHandle for why). 0 means "not a model table".
function GetModelHandle(L: Plua_State; idx: integer): integer;
begin
  L.PushString('__handle');
  lua_rawget(L, idx);
  Result := L.PopInteger;
end;

//First script argument of a model method. The same rule as TileArgBase: a dot
//call injects the model table as argument 1, a colon call passes the receiver
//again as argument 2. Model methods take numbers and strings only, so a table
//in slot 2 can only be the receiver of a colon call.
function ModelArgBase(L: Plua_State): Integer;
begin
  if L.IsTable(2) then
    Result := 3
  else
    Result := 2;
end;

// Populate the table at AIdx with the model's __handle, __name, bound methods,
// property metatable, and registry entry. Used by both RegisterModel (for
// models.new) and Construct_func (for Pluto's `new Model` / `extends Model`).
procedure TLuaModel.InitModelTable(L: Plua_State; AIdx: Integer; AHandle: Integer; const AName: string);
var
  absIdx, depth: Integer;
begin
  absIdx := lua_absindex(L, AIdx);

  // Keep a Pluto class metatable alive on the stack while the property
  // metatable replaces it; a plain table (models.new path) gets a nil sentinel.
  if lua_getmetatable(L, absIdx) = 0 then
    lua_pushnil(L);

  L.PushValue(absIdx);
  L.PushValue(absIdx); //[model, model]

  L.PushInteger(AHandle);
  L.SetField(-2, '__handle');
  if AName <> '' then
  begin
    L.PushString(AName);
    L.SetField(-2, '__name');
  end;

  L.Register('load', Load_func);
  L.Register('move', Move_func);
  L.Register('show', Show_func);
  L.Register('hide', Hide_func);
  L.Register('play', Play_func);
  L.Register('stop', Stop_func);
  L.Register('face', Face_func);
  L.Register('free', Free_func);
  L.Register('destroy', Free_func);

  L.NewTable; //[model, model, meta]
  L.RegisterMeta('__index', __getter);
  L.RegisterMeta('__newindex', __setter);
  L.SetMetaTable(-2);

  if AHandle > cModel3DInvalid then
  begin
    L.PushValue(-1);
    lua_rawseti(L, LUA_REGISTRYINDEX, cModelRegistryBase + AHandle);
  end;

  L.Pop(2); // pop the two working duplicates, the old metatable stays on top

  // walk the old class chain and copy its fields onto the instance
  depth := 0;
  while (depth < 4) and lua_istable(L, -1) do
  begin
    LuaCopyClassFields(L, absIdx, lua_absindex(L, -1));
    lua_pushliteral(L, PUTF8Char('__index'));
    lua_rawget(L, -2);
    lua_remove(L, -2); // drop the current table, keep its chain target
    Inc(depth);
  end;
  lua_pop(L, 1); // pop the chain end (nil or non-table)
end;

procedure TLuaModel.PushClass(L: Plua_State);
begin
  L.NewTable; //[Model]
  L.RegisterMeta('__construct', Construct_func);
  L.RegisterMeta('sphere', Script.Models.Sphere_func);
  L.RegisterMeta('box', Script.Models.Box_func);
  L.RegisterMeta('cylinder', Script.Models.Cylinder_func);
  L.RegisterMeta('cone', Script.Models.Cone_func);
  L.RegisterMeta('torus', Script.Models.Torus_func);
  L.RegisterMeta('find', Script.Models.Find_func);
  L.NewTable; //[Model, meta]
  L.RegisterMeta('__call', Script.Models.Call_func);
  L.SetMetaTable(-2);
end;

// Pluto constructor: `new Model([name [, file]])` or `new Model("jet.glb")`
function TLuaModel.Construct_func(L: Plua_State): integer; cdecl;
var
  b: Integer;
  aArg1, aName, aFile, ext: string;
  handle: Integer;
  LoadObj: TLoadModelObject;
begin
  Result := 0;
  if not L.IsTable(1) then
    Exit;
  if (L.ArgsCount >= 2) and L.IsTable(2) then
    b := 3
  else
    b := 2;

  aName := '';
  aFile := '';
  if (L.ArgsCount >= b) and L.IsString(b) then
  begin
    aArg1 := L.ToString(b);
    if (L.ArgsCount >= b + 1) and L.IsString(b + 1) then
    begin
      aName := aArg1;
      aFile := L.ToString(b + 1);
    end
    else
    begin
      ext := LowerCase(ExtractFileExt(aArg1));
      if (ext = '.glb') or (ext = '.gltf') or (ext = '.obj') or
         (ext = '.iqm') or (ext = '.m3d') or (ext = '.vox') then
      begin
        aFile := aArg1;
        aName := ChangeFileExt(ExtractFileName(aArg1), '');
      end
      else
        aName := aArg1;
    end;
  end;

  handle := Engine.Models.AddEmpty(aName);
  if handle <= cModel3DInvalid then
    handle := cModel3DInvalid;

  InitModelTable(L, b - 1, handle, aName);

  if (aFile <> '') and (handle > cModel3DInvalid) then
  begin
    aFile := Res.GuessFileName(aFile);
    LoadObj := TLoadModelObject.Create(handle, aFile);
    try
      LoadObj.Run(Script.Thread);
      if not LoadObj.HandleResult then
        Script.DoError('Model not loaded: ' + aFile);
    finally
      LoadObj.Free;
    end;
  end;
end;

// Build a model table bound to AHandle and leave it on the Lua stack. Mirrors
// TLuaTile.RegisterTile: the methods are plain table fields receiving the table
// as argument 1, the properties resolve through the metatable getter/setter.
function TLuaModel.RegisterModel(AHandle: integer): integer;
begin
  with Script do
  begin
    Lua.State.BeginTable; //[model]
    InitModelTable(Lua.State, -1, AHandle);
    Result := 1;
  end;
end;

// Push the model table already registered for AHandle from the handle
// registry, or build a new one. find/call use this so every lookup of a
// model returns the very same table.
function TLuaModel.PushExisting(L: Plua_State; AHandle: Integer): Integer;
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, cModelRegistryBase + AHandle);
  if lua_istable(L, -1) then
    Exit(1);
  L.Pop(1);
  Result := RegisterModel(AHandle);
end;

// model.load(file) -> load (or replace) the mesh from a GLB/glTF/OBJ file.
// LoadModel uploads to the GPU, so it runs on the main thread through the
// engine queue.
function TLuaModel.Load_func(L: Plua_State): integer; cdecl;
var
  b: integer;
  aFile: string;
  handle: integer;
  LoadObj: TLoadModelObject;
begin
  b := ModelArgBase(L);
  aFile := Res.GuessFileName(L.ToString(b));

  handle := GetModelHandle(L, 1);
  if handle <= cModel3DInvalid then
    handle := Engine.Models.AddEmpty('');
  LoadObj := TLoadModelObject.Create(handle, aFile);
  try
    LoadObj.Run(Script.Thread);
    if LoadObj.HandleResult then
    begin
      L.PushInteger(handle);
      L.SetField(1, '__handle');
      // re-index the model table under its real handle
      L.PushValue(1);
      lua_rawseti(L, LUA_REGISTRYINDEX, cModelRegistryBase + handle);
    end
    else
      Script.DoError('Model not loaded: ' + aFile);
  finally
    LoadObj.Free;
  end;
  Result := 0;
end;

// model:move(x, y [, z]) -> x/y are screen pixels; z is depth toward the
// viewer (bigger draws in front) and keeps its current value when omitted.
function TLuaModel.Move_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
  z: single;
begin
  b := ModelArgBase(L);
  handle := GetModelHandle(L, 1);
  if handle > cModel3DInvalid then
  begin
    if L.ArgsCount >= b + 2 then
      z := L.ToNumber(b + 2)
    else
      z := Engine.Models.GetZ(handle);
    Engine.Models.SetPosition(handle, L.ToNumber(b), L.ToNumber(b + 1), z);
  end;
  Result := 0;
end;

function TLuaModel.Show_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetModelHandle(L, 1);
  if handle > cModel3DInvalid then
    Engine.Models.SetVisible(handle, True);
  Result := 0;
end;

function TLuaModel.Hide_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetModelHandle(L, 1);
  if handle > cModel3DInvalid then
    Engine.Models.SetVisible(handle, False);
  Result := 0;
end;

// model:play([what]) -> start a skeletal animation. `what` is a clip name
// (model:play("Walk")) or its index (model:play(0)); with no argument it
// (re)starts whatever clip the model last played, or the first in the file.
// Returns whether a clip was found, so a script can check the name it asked
// for. The engine poses the bones every frame while it plays.
function TLuaModel.Play_func(L: Plua_State): integer; cdecl;
var
  b, handle, current: integer;
  ok: boolean;
begin
  b := ModelArgBase(L);
  handle := GetModelHandle(L, 1);
  ok := False;
  if handle > cModel3DInvalid then
  begin
    if L.ArgsCount >= b then
    begin
      //a name or a number: a string hits the name path, else it is an index.
      if L.IsString(b) then
        ok := Engine.Models.PlayAnimByName(handle, L.ToString(b))
      else
        ok := Engine.Models.PlayAnim(handle, L.ToInteger(b));
    end
    else
    begin
      //no argument: replay the last clip, or clip 0 when nothing played yet.
      current := Engine.Models.GetAnimCurrent(handle);
      if current < 0 then
        current := 0;
      ok := Engine.Models.PlayAnim(handle, current);
    end;
  end;
  L.PushBoolean(ok);
  Result := 1;
end;

// model:stop() -> stop the skeletal animation; the model keeps the pose it
// reached. A stopped model still draws, it just no longer moves.
function TLuaModel.Stop_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  handle := GetModelHandle(L, 1);
  if handle > cModel3DInvalid then
    Engine.Models.StopAnim(handle);
  Result := 0;
end;

// model:face(x, y, z [, upx, upy, upz]) -> turn the model so its own +y axis
// aims at that world point and its own +z leans toward the up direction
// (world up by default). This is how a ship rides a curved track or a hoop
// stands square to the road: the point and the lean follow the surface, and
// angle/roll/tilt become trims on top of the pose. model:face() with no
// numbers drops the aim and hands the model back to its plain angles.
function TLuaModel.Face_func(L: Plua_State): integer; cdecl;
var
  b, handle: integer;
begin
  b := ModelArgBase(L);
  handle := GetModelHandle(L, 1);
  if handle > cModel3DInvalid then
  begin
    if L.ArgsCount >= b + 2 then
    begin
      if L.ArgsCount >= b + 5 then
        Engine.Models.SetFace(handle, L.ToNumber(b), L.ToNumber(b + 1), L.ToNumber(b + 2),
          L.ToNumber(b + 3), L.ToNumber(b + 4), L.ToNumber(b + 5))
      else
        Engine.Models.SetFace(handle, L.ToNumber(b), L.ToNumber(b + 1), L.ToNumber(b + 2),
          0, 0, 1);
    end
    else
      Engine.Models.ClearFace(handle);
  end;
  Result := 0;
end;

// model:free() (alias destroy) -> unload the model now: its mesh, textures and
// draw entry are gone. The handle is never reused, so this table simply stops
// addressing anything: reads answer 0 and writes do nothing. Returns true when
// a model was freed.
function TLuaModel.Free_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
  FreeObj: TFreeModelObject;
begin
  Result := 0;
  handle := GetModelHandle(L, 1);
  if handle > cModel3DInvalid then
  begin
    // Drop the name-registry entry first so the global alias of a named model
    // resolves to a fresh table only after a real rebuild.
    L.PushNil;
    lua_rawseti(L, LUA_REGISTRYINDEX, cModelRegistryBase + handle);
    FreeObj := TFreeModelObject.Create(handle);
    try
      FreeObj.Run(Script.Thread);
      L.PushBoolean(FreeObj.Removed);
    finally
      FreeObj.Free;
    end;
    Result := 1;
  end
  else
  begin
    L.PushBoolean(False);
    Result := 1;
  end;
end;

function TLuaModel.Getter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key
  handle := GetModelHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle > cModel3DInvalid then
  begin
    if field = 'x' then
    begin
      L.PushNumber(Engine.Models.GetX(handle));
      Result := 1;
    end
    else if field = 'y' then
    begin
      L.PushNumber(Engine.Models.GetY(handle));
      Result := 1;
    end
    else if field = 'z' then
    begin
      L.PushNumber(Engine.Models.GetZ(handle));
      Result := 1;
    end
    else if field = 'angle' then
    begin
      L.PushNumber(Engine.Models.GetAngle(handle));
      Result := 1;
    end
    else if field = 'tilt' then
    begin
      L.PushNumber(Engine.Models.GetTilt(handle));
      Result := 1;
    end
    else if field = 'roll' then
    begin
      L.PushNumber(Engine.Models.GetRoll(handle));
      Result := 1;
    end
    else if field = 'shadow' then
    begin
      L.PushBoolean(Engine.Models.GetShadow(handle));
      Result := 1;
    end
    else if field = 'scale' then
    begin
      L.PushNumber(Engine.Models.GetScale(handle));
      Result := 1;
    end
    else if field = 'visible' then
    begin
      L.PushBoolean(Engine.Models.GetVisible(handle));
      Result := 1;
    end
    //model.layer picks the side of the 2D world: 0 (the default) flies over
    //the sprites as ever, a negative layer sinks the whole body - and its
    //shade - under the 2D world. See also cLayerTop3D for the other direction
    //(a sprite.layer at or above it rides over every model).
    else if field = 'layer' then
    begin
      L.PushInteger(Engine.Models.GetLayer(handle));
      Result := 1;
    end
    else if field = 'tint' then
    begin
      L.PushInteger(ColorToInt(Engine.Models.GetTint(handle)));
      Result := 1;
    end
    else if field = 'alpha' then
    begin
      L.PushInteger(Engine.Models.GetAlpha(handle));
      Result := 1;
    end
    else if field = 'name' then
    begin
      L.PushString(Engine.Models.GetName(handle));
      Result := 1;
    end
    //model.nose names the model's own nose axis for face(): "y" (the box
    //default), or "-y"/"z"/"-z"/"x"/"-x" for a loaded file that draws with
    //its nose elsewhere - find the axis by eye, the loader's upright swap
    //often leaves a Blender jet asking for "-y".
    else if field = 'nose' then
    begin
      L.PushString(Engine.Models.GetNose(handle));
      Result := 1;
    end
    //model.glow names the material that shines instead of being lit: a name
    //from the loaded file ("Engine"), true (or "*") for the whole model - a
    //thruster flame - and "" (the default) or false to leave it to the lamps.
    //A glowing material never wears the light coat, so world.lights cannot
    //dim it; a name the file does not carry simply never matches.
    else if field = 'glow' then
    begin
      L.PushString(Engine.Models.GetGlow(handle));
      Result := 1;
    end
    //model.bright is how hard that shining material burns: 1 (the default) is
    //the colour the file was painted with, 2 pushes it twice as hot toward
    //white, and the level snaps to 1/8 steps. It speaks for a glowing material
    //only - a body the lamps light simply keeps its shading - and like the
    //shine itself it only shows while world.lights is on.
    else if field = 'bright' then
    begin
      L.PushNumber(Engine.Models.GetBright(handle));
      Result := 1;
    end
    //read-only: the loaded mesh's bounding box, so a script can scale to a
    //pixel size: m.scale = 40 / math.max(m.width, m.depth)
    else if field = 'width' then
    begin
      L.PushNumber(Engine.Models.GetWidth(handle));
      Result := 1;
    end
    else if field = 'height' then
    begin
      L.PushNumber(Engine.Models.GetHeight(handle));
      Result := 1;
    end
    else if field = 'depth' then
    begin
      L.PushNumber(Engine.Models.GetDepth(handle));
      Result := 1;
    end
    else if field = 'loaded' then
    begin
      L.PushBoolean(Engine.Models.IsLoaded(handle));
      Result := 1;
    end
    // -- skeletal animation --
    else if field = 'animCount' then
    begin
      L.PushInteger(Engine.Models.GetAnimCount(handle));
      Result := 1;
    end
    else if field = 'anim' then
    begin
      //the clip now posed, or -1 when the mesh still sits in its bind pose
      L.PushInteger(Engine.Models.GetAnimCurrent(handle));
      Result := 1;
    end
    else if field = 'animPlaying' then
    begin
      L.PushBoolean(Engine.Models.IsAnimPlaying(handle));
      Result := 1;
    end
    else if field = 'animTime' then
    begin
      L.PushNumber(Engine.Models.GetAnimTime(handle));
      Result := 1;
    end
    else if field = 'animSpeed' then
    begin
      L.PushNumber(Engine.Models.GetAnimSpeed(handle));
      Result := 1;
    end
    else if field = 'animName' then
    begin
      L.PushString(Engine.Models.GetAnimName(handle, Engine.Models.GetAnimCurrent(handle)));
      Result := 1;
    end;
  end;
  if Result = 0 then
  begin
    // unknown key or unloaded model: read raw from the model table
    L.PushValue(2);
    lua_rawget(L, 1);
    Result := 1;
  end;
end;

// model.__newindex: write properties x, y, z, angle, tilt, roll, shadow,
// scale, visible, tint, alpha, name, layer, anim, animPlaying, animTime,
// animSpeed; unknown keys raw-stored in the model table.
function TLuaModel.Setter(L: Plua_State): integer;
var
  handle: integer;
  current: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key, arg 3 is the value
  handle := GetModelHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle <= cModel3DInvalid then
  begin
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
    Exit;
  end;
  if field = 'x' then
  begin
    Engine.Models.SetPositionComponent(handle, True, L.ToNumber(3));
  end
  else if field = 'y' then
  begin
    Engine.Models.SetPositionComponent(handle, False, L.ToNumber(3));
  end
  else if field = 'z' then
  begin
    Engine.Models.SetZ(handle, L.ToNumber(3));
  end
  else if field = 'angle' then
  begin
    Engine.Models.SetAngle(handle, L.ToNumber(3));
  end
  else if field = 'tilt' then
  begin
    Engine.Models.SetTilt(handle, L.ToNumber(3));
  end
  else if field = 'roll' then
  begin
    Engine.Models.SetRoll(handle, L.ToNumber(3));
  end
  else if field = 'shadow' then
  begin
    Engine.Models.SetShadow(handle, L.ToBoolean(3));
  end
  else if field = 'scale' then
  begin
    Engine.Models.SetScale(handle, L.ToNumber(3));
  end
  else if field = 'visible' then
  begin
    Engine.Models.SetVisible(handle, L.ToBoolean(3));
  end
  else if field = 'tint' then
  begin
    Engine.Models.SetTint(handle, IntToColor(L.ToInteger(3)));
  end
  else if field = 'alpha' then
  begin
    //0..255 opacity, folded into the tint's alpha channel (like sprites)
    Engine.Models.SetAlpha(handle, Integer(Round(L.ToNumber(3))));
  end
  else if field = 'name' then
  begin
    Engine.Models.SetName(handle, L.ToString(3));
  end
  else if field = 'layer' then
  begin
    //0 (the default) rides over the 2D world, a negative layer sinks the whole
    //body under it (see the getter note)
    Engine.Models.SetLayer(handle, L.ToInteger(3));
  end
  else if field = 'nose' then
  begin
    Engine.Models.SetNose(handle, LowerCase(L.ToString(3)));
  end
  else if field = 'bright' then
  begin
    //how hot the shining material burns (1 = as painted); the store clamps the
    //level and snaps it to its detent, so a pulse costs one shader, not one
    //per frame
    Engine.Models.SetBright(handle, L.ToNumber(3));
  end
  else if field = 'glow' then
  begin
    //a material name of the loaded file, or true/"*" for the whole model,
    //or false/"" for none (see the getter note)
    if L.IsString(3) then
      Engine.Models.SetGlow(handle, L.ToString(3))
    else
    begin
      if L.ToBoolean(3) then
        Engine.Models.SetGlow(handle, cModel3DGlowAll)
      else
        Engine.Models.SetGlow(handle, '');
    end;
  end
  else if field = 'anim' then
  begin
    //write a clip name or index to cut to it (like model:play); a negative
    //number stops the animation and keeps the current pose.
    if L.IsString(3) then
      Engine.Models.PlayAnimByName(handle, L.ToString(3))
    else
      Engine.Models.PlayAnim(handle, L.ToInteger(3));
  end
  else if field = 'animPlaying' then
  begin
    //true resumes/plays the current clip (the file's first when none played
    //yet), false stops and holds the pose.
    if L.ToBoolean(3) then
    begin
      current := Engine.Models.GetAnimCurrent(handle);
      if current < 0 then
        current := 0;
      Engine.Models.PlayAnim(handle, current);
    end
    else
      Engine.Models.StopAnim(handle);
  end
  else if field = 'animTime' then
  begin
    //scrub inside the current clip, in seconds.
    Engine.Models.SetAnimTime(handle, L.ToNumber(3));
  end
  else if field = 'animSpeed' then
  begin
    //keyframes per second; a clip's fresh 30 is its authored pace.
    Engine.Models.SetAnimSpeed(handle, L.ToNumber(3));
  end
  else
  begin
    // unknown key: store it raw in the model table
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
  end;
end;

{ TLuaModels }

// models.new("name"?) -> creates a new model object, returns it on the Lua stack
function TLuaModels.New_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  if L.ArgsCount >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  //AddEmpty only touches the store's dictionary under its lock, so it is safe
  //to call straight from the script thread; only the mesh upload needs the
  //main thread, and load() routes that through the engine queue.
  handle := Engine.Models.AddEmpty(aName);
  if handle > cModel3DInvalid then
    Script.Model.RegisterModel(handle)
  else
    Script.Model.RegisterModel(cModel3DInvalid);
  // Store optional name
  if aName <> '' then
  begin
    L.PushString(aName);
    L.SetField(-2, '__name');
  end;
  Result := 1;
end;

// Shared body of the shape builders - see the class note. The queue object
// runs on the script thread's behalf, so the model table it hands back is
// complete by the time the script reads it.
function TLuaModels.BuildShapeModel(L: Plua_State; AShape: TTyroModelShape;
  ADef, BDef, CDef: Single): integer;
var
  b: integer;
  aName: string;
  aSizeA, aSizeB, aSizeC: Single;
  handle: integer;
  BuildObj: TShapeModelObject;
begin
  if (L.ArgsCount >= 1) and L.IsString(1) then
  begin
    aName := L.ToString(1);
    b := 2;
  end
  else
  begin
    aName := '';
    b := 1;
  end;
  if L.ArgsCount >= b then
    aSizeA := L.ToNumber(b)
  else
    aSizeA := ADef;
  if L.ArgsCount >= b + 1 then
    aSizeB := L.ToNumber(b + 1)
  else
    aSizeB := BDef;
  if L.ArgsCount >= b + 2 then
    aSizeC := L.ToNumber(b + 2)
  else
    aSizeC := CDef;
  handle := Engine.Models.AddEmpty(aName);
  if handle > cModel3DInvalid then
  begin
    BuildObj := TShapeModelObject.Create(handle, AShape, aSizeA, aSizeB, aSizeC);
    try
      BuildObj.Run(Script.Thread);
      if not BuildObj.HandleResult then
        Script.DoError('Model shape not built');
    finally
      BuildObj.Free;
    end;
    Script.Model.RegisterModel(handle);
  end
  else
    Script.Model.RegisterModel(cModel3DInvalid);
  // Store optional name
  if aName <> '' then
  begin
    L.PushString(aName);
    L.SetField(-2, '__name');
  end;
  Result := 1;
end;

// models.sphere("name"? [, radius]) -> a model whose mesh is the builtin
// unit sphere (or one of the given radius), scaled like any model.
function TLuaModels.Sphere_func(L: Plua_State): integer; cdecl;
begin
  Result := BuildShapeModel(L, msSphere, 1, 0, 0);
end;

// models.box("name"? [, w, h, d]) -> a flat-shaded box with those sizes
// along x/y/z (unit by default) - the vector block of a 3D world.
function TLuaModels.Box_func(L: Plua_State): integer; cdecl;
begin
  Result := BuildShapeModel(L, msBox, 1, 1, 1);
end;

// models.cylinder("name"? [, radius] [, height]) -> a drum standing on the
// z axis (unit by default).
function TLuaModels.Cylinder_func(L: Plua_State): integer; cdecl;
begin
  Result := BuildShapeModel(L, msCylinder, 1, 1, 0);
end;

// models.cone("name"? [, radius] [, height]) -> a cone standing on the z
// axis: its point aims at +z (unit by default).
function TLuaModels.Cone_func(L: Plua_State): integer; cdecl;
begin
  Result := BuildShapeModel(L, msCone, 1, 1, 0);
end;

// models.torus("name"? [, radius] [, tube]) -> a ring lying in x/y around
// the z axis: the donut a looping track and its boost hoops are built from
// (ring radius 1, thin tube by default).
function TLuaModels.Torus_func(L: Plua_State): integer; cdecl;
begin
  Result := BuildShapeModel(L, msTorus, 1, 0.25, 0);
end;

// models.find("name") -> returns the named model object, or nil
function TLuaModels.Find_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(1);
  handle := Engine.Models.FindByName(aName);
  if handle > cModel3DInvalid then
    Result := Script.Model.PushExisting(L, handle)
  else
  begin
    L.PushNil;
    Result := 1;
  end;
end;

// models("name") -> __call metamethod, same as models.find
function TLuaModels.Call_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(2); // first arg after table self
  handle := Engine.Models.FindByName(aName);
  if handle > cModel3DInvalid then
    Result := Script.Model.PushExisting(L, handle)
  else
  begin
    L.PushNil;
    Result := 1;
  end;
end;

{ TLuaParticles }

//particles.count -> how many sparks are alive right now. Everything else
//falls through to the base getter.
function TLuaParticles.Getter(L: PLua_State): integer;
var
  field: string;
begin
  Result := 0;
  field := L.ToString(2);
  if field = 'count' then
  begin
    if (Engine <> nil) and (Engine.Particles <> nil) then
      L.PushInteger(Engine.Particles.Count)
    else
      L.PushInteger(0);
    Result := 1;
  end
  else
    Result := inherited;
end;

// particles.emit(x, y [, count] [, options])
// Throws one burst of sparks at the screen pixel (x, y). The engine owns them
// from here: it steps them every frame (a paused scene freezes them with the
// world) and draws them on top of the 3D models until they fade. Options,
// all optional:
//   color     ink name or value            (default 'orange')
//   speed     launch speed px/s            (default 140)
//   life      seconds per spark            (default 0.5)
//   size      spark radius px              (default 3)
//   gravity   px/s^2 toward the bottom     (default 240)
//   drag      speed shed per second        (default 1.5)
//   spread    degrees of the burst         (default 360, a full ring)
//   direction middle of the spread, deg    (default 0, right; -90 is up)
function TLuaParticles.Emit_func(L: Plua_State): integer; cdecl;
var
  Burst: TParticleBurst;

  //Raw-read one number from the options table; a missing field simply keeps
  //the default already sitting in Burst.
  procedure NumField(const AField: string; var AValue: single);
  begin
    lua_getfield(L, 4, PUTF8Char(UTF8String(AField)));
    if not lua_isnil(L, -1) then
      AValue := L.ToNumber(-1);
    lua_pop(L, 1);
  end;

begin
  Result := 0;
  if (Engine = nil) or (Engine.Particles = nil) then
    Exit;
  if L.ArgsCount < 2 then
  begin
    Script.DoError('particles.emit(x, y [, count] [, options]) needs at least x and y');
    Exit;
  end;
  Burst.X := L.ToNumber(1);
  Burst.Y := L.ToNumber(2);
  Burst.Count := 12;
  if L.ArgsCount >= 3 then
    Burst.Count := L.ToInteger(3);
  Burst.Color := clOrange;
  Burst.Speed := 140;
  Burst.Life := 0.5;
  Burst.Size := 3;
  Burst.Gravity := 240;
  Burst.Drag := 1.5;
  Burst.Spread := 360;
  Burst.Direction := 0;
  Burst.Layer := 0;
  if (L.ArgsCount >= 4) and L.IsTable(4) then
  begin
    lua_getfield(L, 4, PUTF8Char(UTF8String('color')));
    if L.IsString(-1) then
      Burst.Color := StrToColor(L.ToString(-1))
    else if not lua_isnil(L, -1) then
      Burst.Color := IntToColor(L.ToInteger(-1));
    lua_pop(L, 1);
    NumField('speed', Burst.Speed);
    NumField('life', Burst.Life);
    NumField('size', Burst.Size);
    NumField('gravity', Burst.Gravity);
    NumField('drag', Burst.Drag);
    NumField('spread', Burst.Spread);
    NumField('direction', Burst.Direction);
    //layer picks the side of the 3D pass (see cLayerTop3D), exactly like a
    //sprite's: the default 0 burns BELOW the models (a flame the jet carries,
    //not debris thrown over it); a missing field stays 0.
    lua_getfield(L, 4, PUTF8Char(UTF8String('layer')));
    if not lua_isnil(L, -1) then
      Burst.Layer := L.ToInteger(-1);
    lua_pop(L, 1);
  end;
  if Burst.Life <= 0 then
    Burst.Life := 0.1;
  L.PushInteger(Engine.Particles.Emit(Burst));
  Result := 1;
end;

//particles.clear() - drop every live spark (e.g. when a round ends).
function TLuaParticles.Clear_func(L: Plua_State): integer; cdecl;
begin
  Result := 0;
  if (Engine <> nil) and (Engine.Particles <> nil) then
    Engine.Particles.Clear;
end;

{ TLuaParallax }

//Read the parallax table's __handle raw, without re-entering the metatable
//getter (see GetSpriteHandle for why). 0 means "not a parallax table".
function GetParallaxHandle(L: Plua_State; idx: integer): integer;
begin
  L.PushString('__handle');
  lua_rawget(L, idx);
  Result := L.PopInteger;
end;

//First script argument of a parallax method. A dot call (bg.add(...)) injects
//the parallax table as argument 1, so the script's arguments start at 2. A colon
//call (bg:add(...)) passes the receiver again as argument 2 and the arguments
//start at 3. The first argument is always a string (a file name), so a table in
//slot 2 can only be the receiver of a colon call.
function ParallaxArgBase(L: Plua_State): Integer;
begin
  if L.IsTable(2) then
    Result := 3
  else
    Result := 2;
end;

// Build a parallax table bound to AHandle and leave it on the Lua stack. Mirrors
// TLuaTile.RegisterTile: the methods are plain table fields receiving the table
// as argument 1, the properties resolve through the metatable getter/setter.
function TLuaParallax.RegisterParallax(AHandle: integer): integer;
var
  base: integer;
begin
  with Script do
  begin
    Lua.State.BeginTable; //[bg]
    base := Lua.State.ArgsCount; //index of the parallax table

    //keep a duplicate, the -3 addressing in Register() hits the parallax table
    Lua.State.PushValue(-1); //[bg, bg]

    Lua.State.PushInteger(AHandle);
    Lua.State.SetField(-2, '__handle'); //bg.__handle = AHandle

    //methods receive the parallax table injected as argument 1
    Lua.State.Register('add', Add_func);
    Lua.State.Register('addground', AddGround_func);
    Lua.State.Register('clear', Clear_func);
    Lua.State.Register('reset', Reset_func);

    //metatable with property getter/setter (no table injection)
    Lua.State.NewTable; //[bg, bg, meta]
    Lua.State.RegisterMeta('__index', __getter);
    Lua.State.RegisterMeta('__newindex', __setter);
    Lua.State.SetMetaTable(-2); //bg.metatable = meta

    //remember this table so it can be found by handle
    if AHandle > cParallaxInvalid then
    begin
      Lua.State.PushValue(-1); // duplicate the parallax table
      lua_rawseti(Lua.State, LUA_REGISTRYINDEX, cParallaxRegistryBase + AHandle);
    end;

    Lua.State.Remove(base); //drop the first reference, keep one on the stack
    Result := 1;
  end;
end;

//Common body of add/addground: read the file and the optional speed/y/scale and
//load the layer on the main thread (the texture upload needs the GL context).
function TLuaParallax.DoAdd(L: Plua_State; AAnchor: TParallaxAnchor): integer;
var
  b, handle: integer;
  aFile: string;
  aSpeed, aY, aScale: Single;
  LoadObj: TAddParallaxLayerObject;
begin
  Result := 0;
  b := ParallaxArgBase(L);
  handle := GetParallaxHandle(L, 1);
  if handle <= cParallaxInvalid then
    Exit;
  aFile := L.ToString(b);
  aSpeed := 1.0;
  aY := 0;
  aScale := 1.0;
  if L.ArgsCount >= b + 1 then
    aSpeed := L.ToNumber(b + 1);
  if L.ArgsCount >= b + 2 then
    aY := L.ToNumber(b + 2);
  if L.ArgsCount >= b + 3 then
    aScale := L.ToNumber(b + 3);
  LoadObj := TAddParallaxLayerObject.Create(handle, aFile, aSpeed, aY, aScale, AAnchor);
  try
    LoadObj.Run(Script.Thread);
  finally
    LoadObj.Free;
  end;
end;

// bg:add(file, speed [, y [, scale]]) - a layer anchored to the top edge
function TLuaParallax.Add_func(L: Plua_State): integer; cdecl;
begin
  Result := DoAdd(L, paTop);
end;

// bg:addground(file, speed [, y [, scale]]) - a layer glued to the floor
function TLuaParallax.AddGround_func(L: Plua_State): integer; cdecl;
begin
  Result := DoAdd(L, paBottom);
end;

// bg:clear() - drop every layer and unload its texture (main thread, as the
//unload is a GL call)
function TLuaParallax.Clear_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
  ClearObj: TClearParallaxLayersObject;
begin
  Result := 0;
  handle := GetParallaxHandle(L, 1);
  if handle <= cParallaxInvalid then
    Exit;
  ClearObj := TClearParallaxLayersObject.Create(handle);
  try
    ClearObj.Run(Script.Thread);
  finally
    ClearObj.Free;
  end;
end;

// bg:reset() - put the scroll offset back to 0
function TLuaParallax.Reset_func(L: Plua_State): integer; cdecl;
var
  handle: integer;
begin
  Result := 0;
  handle := GetParallaxHandle(L, 1);
  if handle > cParallaxInvalid then
    Engine.Parallaxes.SetOffset(handle, 0);
end;

// parallax.new("name"?) -> creates a new parallax object, returns it on the stack
function TLuaParallaxes.New_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  if L.ArgsCount >= 1 then
    aName := L.ToString(1)
  else
    aName := '';
  //Add only inserts into the store's dictionary under its lock, so it is safe
  //to call straight from the script thread; only the layer texture upload needs
  //the main thread, and add() routes that through the engine thread.
  handle := Engine.Parallaxes.Add(aName);
  if handle > cParallaxInvalid then
    Script.Parallax.RegisterParallax(handle)
  else
    Script.Parallax.RegisterParallax(cParallaxInvalid);
  // Store optional name
  if aName <> '' then
  begin
    L.PushString(aName);
    L.SetField(-2, '__name');
  end;
  Result := 1;
end;

// parallax.find("name") -> returns the named parallax object, or nil
function TLuaParallaxes.Find_func(L: Plua_State): integer; cdecl;
var
  aName: string;
  handle: integer;
begin
  aName := L.ToString(1);
  handle := Engine.Parallaxes.Find(aName);
  if handle > cParallaxInvalid then
    Script.Parallax.RegisterParallax(handle)
  else
    L.PushNil;
  Result := 1;
end;

// Parallax __index: read properties offset, speed, visible, tile, groundy,
// layers, name; unknown keys raw-read from the parallax table.
function TLuaParallax.Getter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key
  handle := GetParallaxHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle > cParallaxInvalid then
  begin
    if field = 'offset' then
    begin
      L.PushNumber(Engine.Parallaxes.GetOffset(handle));
      Result := 1;
    end
    else if field = 'speed' then
    begin
      L.PushNumber(Engine.Parallaxes.GetGlobalSpeed(handle));
      Result := 1;
    end
    else if field = 'visible' then
    begin
      L.PushBoolean(Engine.Parallaxes.GetVisible(handle));
      Result := 1;
    end
    else if field = 'tile' then
    begin
      L.PushBoolean(Engine.Parallaxes.GetTile(handle));
      Result := 1;
    end
    else if field = 'layers' then
    begin
      L.PushInteger(Engine.Parallaxes.GetLayerCount(handle));
      Result := 1;
    end
    else if field = 'groundy' then
    begin
      if (Engine.Board <> nil) then
        L.PushNumber(Engine.Parallaxes.GetGroundY(handle, Engine.Board.Height))
      else
        L.PushNumber(0);
      Result := 1;
    end
    else if field = 'name' then
    begin
      L.PushString(Engine.Parallaxes.GetName(handle));
      Result := 1;
    end;
  end;
  if Result = 0 then
  begin
    // unknown key or unloaded parallax: read raw from the parallax table
    L.PushValue(2);
    lua_rawget(L, 1);
    Result := 1;
  end;
end;

// Parallax __newindex: write properties offset, speed, visible, tile; unknown
// keys raw-stored in the parallax table.
function TLuaParallax.Setter(L: Plua_State): integer;
var
  handle: integer;
  field: string;
begin
  // arg 1 is the table, arg 2 is the key, arg 3 is the value
  handle := GetParallaxHandle(L, 1);
  field := L.ToString(2);
  Result := 0;
  if handle <= cParallaxInvalid then
  begin
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
    Exit;
  end;
  if field = 'offset' then
  begin
    Engine.Parallaxes.SetOffset(handle, L.ToNumber(3));
  end
  else if field = 'speed' then
  begin
    Engine.Parallaxes.SetGlobalSpeed(handle, L.ToNumber(3));
  end
  else if field = 'visible' then
  begin
    Engine.Parallaxes.SetVisible(handle, L.ToBoolean(3));
  end
  else if field = 'tile' then
  begin
    Engine.Parallaxes.SetTile(handle, L.ToBoolean(3));
  end
  else
  begin
    // unknown key: store it raw in the parallax table
    L.PushValue(2);
    L.PushValue(3);
    lua_rawset(L, 1);
  end;
end;

{ TLuaSpriteScript }

// Builds a sprite table (like Sprites.new returns) bound to AHandle and leaves
// it on the Lua stack. Properties route through the proxy TLuaSprite getter/
// setter, so self.x / self.kind / ... work and unknown keys are raw-stored.
procedure PushSpriteTable(L: Plua_State; AHandle: Integer; ASprite: TLuaSprite);
begin
  L.NewTable; //[t]
  L.PushInteger(AHandle);
  L.SetField(-2, '__handle'); //t.__handle = AHandle -> [t]
  L.PushString(Engine.Sprites.GetName(AHandle));
  L.SetField(-2, '__name'); //t.__name = sprite name -> [t]
  L.NewTable; //[t, meta]

  //RegisterMeta, not Register: Lua already passes the table as argument 1 to
  //__index/__newindex, so the table must not be injected a second time.
  L.RegisterMeta('__index', ASprite.__getter);
  L.RegisterMeta('__newindex', ASprite.__setter);

  L.SetMetaTable(-2); //t.metatable = meta -> [t]
end;

// draw.* color for sprite scripts: the colors table stores AARRGGBB as a Lua
// integer. Read it back via the Lua number (int64-safe) and mask bytes
// explicitly so large values never get mangled by 32-bit arithmetic.
function SpriteColorValue(L: Plua_State; Idx: Integer): TColor;
var
  v: UInt32;
begin
  v := UInt32(Trunc(L.ToNumber(Idx)));
  Result.RGBA.Red := Byte(v and $FF);
  Result.RGBA.Green := Byte((v shr 8) and $FF);
  Result.RGBA.Blue := Byte((v shr 16) and $FF);
  Result.RGBA.Alpha := Byte((v shr 24) and $FF);
end;

constructor TLuaSpriteScript.Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
begin
  inherited Create;
  FScript := AScript;
  FHandle := AHandle;
  FFileName := AFileName;
  FSprite := TLuaSprite.Create('sprite', AScript); // proxy; only its property getter/setter are used
  FLua.Init;
  BuildSelf;
  BuildGlobals;
  BuildColors;
  BuildDraw;
end;

destructor TLuaSpriteScript.Destroy;
begin
  FLua.Close;
  FSprite.Free;
  inherited;
end;

procedure TLuaSpriteScript.DoError(S: string; const AHandler: string);
begin
  FScript.DoError('[' + FFileName + '] ' + AHandler + ': ' + S);
end;

function TLuaSpriteScript.Load: Boolean;
var
  Msg: string;
begin
  Result := False;
  if FFileName = '' then
    Exit;
  FFileName := Res.GuessFileName(FFileName);
  Result := FLua.State.RunFile(FFileName, Msg);
  if not Result then
    DoError(Msg, 'load');
end;

procedure TLuaSpriteScript.BuildSelf;
begin
  PushSpriteTable(FLua.State, FHandle, FSprite); //[self]
  lua_setglobal(FLua.State, 'self'); //[]
end;

procedure TLuaSpriteScript.BuildGlobals;
begin
  // Reuse the Engine script's globals so per-sprite states can call the same
  // helpers (the bound FScript outlives this state).
  FLua.State.RegisterGlobal('time', FScript.TotalTime_func);
  FLua.State.RegisterGlobal('rand', FScript.RandomValue_func);
  FLua.State.RegisterGlobal('println', FScript.Console.PrintLn_func);
  FLua.State.RegisterGlobal('rgba', FScript.Rgba_func);
  FLua.State.RegisterGlobal('rgb', FScript.Rgba_func);
  PushArgsGlobal(FLua.State);
end;

procedure TLuaSpriteScript.BuildColors;
var
  i: integer;
begin
  FLua.State.BeginTable;
  for i := 0 to Length(FScript.Colors.Colors) - 1 do
    FLua.State.Register(FScript.Colors.Colors[i].Name, ColorToInt(FScript.Colors.Colors[i].Color));
  FLua.State.EndTableGlobal('colors', FScript.Colors);
end;

procedure TLuaSpriteScript.BuildDraw;
begin
  FLua.State.RegisterTable('draw');
  FLua.State.Register('draw', 'circle', Self, Circle_func);
  FLua.State.Register('draw', 'rectangle', Self, Rectangle_func);
  FLua.State.Register('draw', 'line', Self, Line_func);
  FLua.State.Register('draw', 'text', Self, Text_func);
end;

// Look up the global handler AName and pcall it. The caller has pushed the
// arguments (AArgs of them) below the handler position; an absent/ non-
// function handler just drains the stack and is ignored.
procedure TLuaSpriteScript.CallHandler(const AName: utf8string; AArgs: Integer);
var
  Msg: string;
begin
  lua_getglobal(FLua.State, PUTF8Char(AName)); //[args..., func]
  if not lua_isfunction(FLua.State, -1) then
  begin
    FLua.State.Pop(1 + AArgs); // discard the function and the caller args
    Exit;
  end;
  if AArgs > 0 then
    lua_insert(FLua.State, 1); //[func, args...]
  if lua_pcall(FLua.State, AArgs, 0, 0) <> 0 then
  begin
    Msg := FLua.State.ToString(-1);
    FLua.State.Pop(1);
    DoError(Msg, AName);
  end;
end;

procedure TLuaSpriteScript.Update;
begin
  CallHandler('on_update', 0);
end;

procedure TLuaSpriteScript.Draw;
begin
  CallHandler('on_draw', 0);
end;

procedure TLuaSpriteScript.OnCollide(AOtherHandle: Integer; const AState: string);
begin
  PushSpriteTable(FLua.State, AOtherHandle, FSprite); //[other]
  FLua.State.PushString(AState); //[other, state]
  CallHandler('on_collide', 2);
end;

// draw.circle(x, y, radius, color, fill?) - sprite/world coordinates
function TLuaSpriteScript.Circle_func(L: Plua_State): integer; cdecl;
var
  x, y, r: single;
  f: boolean;
begin
  x := L.ToNumber(1);
  y := L.ToNumber(2);
  r := L.ToNumber(3);
  if L.ArgsCount >= 5 then
    f := L.ToBoolean(5)
  else
    f := True;
  if f then
    RayLib.DrawCircle(round(x), round(y), r, SpriteColorValue(L, 4))
  else
    RayLib.DrawCircleLines(round(x), round(y), r, SpriteColorValue(L, 4));
  Result := 0;
end;

// draw.rectangle(x, y, w, h, color, fill?) - sprite/world coordinates
function TLuaSpriteScript.Rectangle_func(L: Plua_State): integer; cdecl;
var
  x, y, w, h: integer;
  f: boolean;
begin
  x := round(L.ToNumber(1));
  y := round(L.ToNumber(2));
  w := round(L.ToNumber(3));
  h := round(L.ToNumber(4));
  if L.ArgsCount >= 6 then
    f := L.ToBoolean(6)
  else
    f := True;
  if f then
    RayLib.DrawRectangle(x, y, w, h, SpriteColorValue(L, 5))
  else
    RayLib.DrawRectangleLinesEx(RectangleOf(x, y, w, h), 1, SpriteColorValue(L, 5));
  Result := 0;
end;

// draw.line(x1, y1, x2, y2, color)
function TLuaSpriteScript.Line_func(L: Plua_State): integer; cdecl;
begin
  RayLib.DrawLineEx(Vector2Of(L.ToNumber(1), L.ToNumber(2)), Vector2Of(L.ToNumber(3), L.ToNumber(4)), 1, SpriteColorValue(L, 5));
  Result := 0;
end;

// draw.text(x, y, text, color)
function TLuaSpriteScript.Text_func(L: Plua_State): integer; cdecl;
begin
  RayLib.DrawTextEx(Res.Font.Data, PUTF8Char(UTF8String(L.ToString(3))), Vector2Of(L.ToNumber(1), L.ToNumber(2)), Res.Font.Height, 0, SpriteColorValue(L, 4));
  Result := 0;
end;

{ TLoadSpriteScriptObject }

constructor TLoadSpriteScriptObject.Create(AScript: TLuaScript; AHandle: Integer; const AFileName: string);
begin
  inherited Create;
  FScript := AScript;
  FHandle := AHandle;
  FFileName := AFileName;
end;

procedure TLoadSpriteScriptObject.DoExecute;
var
  Scr: TLuaSpriteScript;
begin
  if FHandle <= cSpriteInvalid then
    Exit;
  Scr := TLuaSpriteScript.Create(FScript, FHandle, FFileName);
  try
    if Scr.Load then
      Engine.Sprites.SetScript(FHandle, Scr)
    else
      Scr.Free;
  except
    on E: Exception do
    begin
      Scr.Free;
      FScript.DoError('Sprite script failed: ' + FFileName + ': ' + E.ClassName + ': ' + E.Message);
    end;
  end;
end;
{ TLuaCollision }

function TLuaCollision.Getter(L: Plua_State): integer;
begin
  Result := 0;
  if Engine.Physics = nil then
    Exit;
  if L.ToString(2) = 'gravityx' then
  begin
    L.PushNumber(Engine.Physics.GetGravityX);
    Result := 1;
  end
  else if L.ToString(2) = 'gravityy' then
  begin
    L.PushNumber(Engine.Physics.GetGravityY);
    Result := 1;
  end
  else if L.ToString(2) = 'bodies' then
  begin
    L.PushInteger(Engine.Physics.BodyCount);
    Result := 1;
  end;
end;

function TLuaCollision.Setter(L: Plua_State): integer;
var
  x, y: single;
begin
  Result := 0;
  if Engine.Physics = nil then
    Exit;
  x := Engine.Physics.GetGravityX;
  y := Engine.Physics.GetGravityY;
  if L.ToString(2) = 'gravityx' then
    x := L.ToNumber(3)
  else if L.ToString(2) = 'gravityy' then
    y := L.ToNumber(3);
  Engine.Physics.SetGravity(x, y);
end;

// Fire the onCollide handler of the sprite AHandle, passing the other sprite and the contact state
procedure TLuaCollision.FireEvent(L: Plua_State; AHandle, AOtherHandle: Integer; const AState: string);
begin
  // get the target sprite table from the registry
  lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AHandle);
  if not L.IsTable(-1) then
  begin
    L.Pop(1);
    Exit;
  end;
  // read its onCollide field raw (bypasses __index)
  L.PushString('onCollide');
  lua_rawget(L, -2);
  if not lua_isfunction(L, -1) then
  begin
    L.Pop(2);
    Exit;
  end;
  // push the other sprite (or nil) and the state string as arguments
  lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + AOtherHandle);
  if not lua_istable(L, -1) then
  begin
    L.Pop(1);
    L.PushNil;
  end;
  L.PushString(AState);
  if lua_pcall(L, 2, 0, 0) <> 0 then
  begin
    Script.DoError('onCollide: ' + L.ToString(-1));
    L.Pop(1);
  end;
  L.Pop(1); // remove the target sprite table
end;

// collision.pump() -> number of events dispatched; fires onCollide handlers for both sides
function TLuaCollision.Pump_func(L: Plua_State): integer; cdecl;
begin
  L.PushInteger(PumpEvents);
  Result := 1;
end;

//Shared drain: every physics event goes to both sprites' onCollide handlers,
//and - while the script listens - to every on("collision") bus handler as
//(spriteA, spriteB, state). The bus route runs after the two per-sprite
//callbacks, so a listener never sees an event before the sprites themselves.
function TLuaCollision.PumpEvents: Integer;
var
  Evs: array[0..1023] of TCollisionEvent;
  ACount: Integer;
  I: Integer;
  ev: TCollisionEvent;
  L: Plua_State;
  Base: Integer;
  State: string;
begin
  Result := 0;
  if (Engine.Physics = nil) or (Script = nil) or (Script.Lua.State = nil) then
    Exit;
  L := Script.Lua.State;
  ACount := 0;
  Engine.Physics.Poll(Evs, ACount);
  for I := 0 to ACount - 1 do
  begin
    ev := Evs[I];
    if ev.State = csBegin then
      State := 'enter'
    else
      State := 'leave';
    FireEvent(L, ev.HandleA, ev.HandleB, State);
    FireEvent(L, ev.HandleB, ev.HandleA, State);
    if Script.HandlerCountNamed(cEventCollision) > 0 then
    begin
      Base := lua_gettop(L);
      lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + ev.HandleA);
      if not lua_istable(L, -1) then begin lua_pop(L, 1); L.PushNil; end;
      lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + ev.HandleB);
      if not lua_istable(L, -1) then begin lua_pop(L, 1); L.PushNil; end;
      L.PushString(State);
      Script.Dispatch(cEventCollision, L, Base + 1, 3);
      lua_pop(L, 3);
    end;
    Inc(Result);
  end;
end;

//The stretch beat, run by EngineUpdate every frame. The drain happens even
//when the bus is deaf - an unlistened snap is history, not a backlog - and
//when somebody does listen, each edge arrives as (spriteA, spriteB, state)
//exactly like the collision events do.
function TLuaCollision.PumpStretch: Integer;
var
  Evs: array[0..255] of TJointEvent;
  ACount: Integer;
  I: Integer;
  L: Plua_State;
  Base: Integer;
begin
  Result := 0;
  if (Engine.Physics = nil) or (Script = nil) or (Script.Lua.State = nil) then
    Exit;
  L := Script.Lua.State;
  Engine.Physics.PollStretch(Evs, ACount);
  if (ACount = 0) or (Script.HandlerCountNamed(cEventJointStretch) = 0) then
    Exit; // drained either way; nobody to tell
  for I := 0 to ACount - 1 do
  begin
    Base := lua_gettop(L);
    lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + Evs[I].HandleA);
    if not lua_istable(L, -1) then begin lua_pop(L, 1); L.PushNil; end;
    lua_rawgeti(L, LUA_REGISTRYINDEX, cSpriteRegistryBase + Evs[I].HandleB);
    if not lua_istable(L, -1) then begin lua_pop(L, 1); L.PushNil; end;
    if Evs[I].Taut then
      L.PushString('taut')
    else
      L.PushString('slack');
    Script.Dispatch(cEventJointStretch, L, Base + 1, 3);
    lua_pop(L, 3);
    Inc(Result);
  end;
end;

// Read one numeric field of the opts table at ATable, leaving the stack as
// it was. Answers false (and keeps AVal) when the field is absent.
function TLuaCollision.Joint_func(L: Plua_State): integer; cdecl;

  function OptNum(ATable: Integer; const AName: string; var AVal: cpFloat): Boolean;
  var
    F: UTF8String;
  begin
    F := UTF8String(AName);
    lua_getfield(L, ATable, PUTF8Char(F));
    Result := lua_isnumber(L, -1);
    if Result then
      AVal := lua_tonumber(L, -1);
    lua_pop(L, 1);
  end;

var
  HA, HB: Integer;
  Kind: string;
  K: TJointKind;
  StateA, StateB: TSpritePhysicsState;
  Ax, Ay, Bx, By, Dist, MinV, MaxV, AngleDeg, Strength, Damping: cpFloat;
  HasMin, HasMax, HasStrength: Boolean;
  RadA, WAX, WAY, WBX, WBY, AnchorDist: cpFloat;
begin
  if (Engine = nil) or (Engine.Physics = nil) then
  begin
    L.PushNil;
    L.PushString('joint: physics are not running');
    Exit(2);
  end;
  if (not lua_istable(L, 1)) or (not lua_istable(L, 2)) then
  begin
    L.PushNil;
    L.PushString('joint: the first two arguments are the sprites to join');
    Exit(2);
  end;
  HA := GetSpriteHandle(L, 1);
  HB := GetSpriteHandle(L, 2);
  if (HA <= cSpriteInvalid) or (HB <= cSpriteInvalid) then
  begin
    L.PushNil;
    L.PushString('joint: the first two arguments are the sprites to join');
    Exit(2);
  end;
  if HA = HB then
  begin
    L.PushNil;
    L.PushString('joint: a sprite cannot be joined to itself');
    Exit(2);
  end;
  Kind := 'pin';
  if lua_istable(L, 3) then
  begin
    lua_getfield(L, 3, PUTF8Char(UTF8String('kind')));
    if lua_isstring(L, -1) then
      Kind := LowerCase(L.ToString(-1));
    lua_pop(L, 1);
  end;
  if Kind = 'pin' then
    K := jkPin
  else if (Kind = 'slide') or (Kind = 'rope') or (Kind = 'leash') then
    K := jkSlide
  else if (Kind = 'rotator') or (Kind = 'motor') then
    K := jkRotator
  else if (Kind = 'limit') or (Kind = 'hinge') then
    K := jkAngleLimit
  else
  begin
    L.PushNil;
    L.PushString('joint: kind must be "pin", "slide", "rotator" or "limit"');
    Exit(2);
  end;
  // Defaults chosen per kind below; a motor that pulls with no strength is
  // no motor, so the rotator starts stronger than the pull cap of the rest.
  Ax := 0; Ay := 0; Bx := 0; By := 0;
  Dist := -1; MinV := 0; MaxV := 0; AngleDeg := 0;
  Strength := 0; Damping := 30;
  if K = jkRotator then
    Strength := 300;
  HasMin := False; HasMax := False; HasStrength := False;
  if lua_istable(L, 3) then
  begin
    OptNum(3, 'ax', Ax);
    OptNum(3, 'ay', Ay);
    OptNum(3, 'bx', Bx);
    OptNum(3, 'by', By);
    if not OptNum(3, 'length', Dist) then
      OptNum(3, 'dist', Dist);
    HasMin := OptNum(3, 'min', MinV);
    HasMax := OptNum(3, 'max', MaxV);
    OptNum(3, 'angle', AngleDeg);
    HasStrength := OptNum(3, 'strength', Strength);
    OptNum(3, 'damping', Damping);
  end;
  if not Engine.Sprites.GetPhysicsState(HA, StateA)
    or not Engine.Sprites.GetPhysicsState(HB, StateB) then
  begin
    L.PushNil;
    L.PushString('joint: one of the sprites is gone');
    Exit(2);
  end;
  // The gap the joint is armed with, measured between the two anchor points
  // where they actually stand - local offsets turned by each sprite's angle.
  RadA := StateA.Angle * Pi / 180;
  WAX := StateA.X + Ax * Cos(RadA) - Ay * Sin(RadA);
  WAY := StateA.Y + Ax * Sin(RadA) + Ay * Cos(RadA);
  RadA := StateB.Angle * Pi / 180;
  WBX := StateB.X + Bx * Cos(RadA) - By * Sin(RadA);
  WBY := StateB.Y + Bx * Sin(RadA) + By * Cos(RadA);
  AnchorDist := Sqrt((WBX - WAX) * (WBX - WAX) + (WBY - WAY) * (WBY - WAY));
  case K of
    jkPin:
    begin
      if Dist < 0 then
        Dist := AnchorDist; // "as far apart as they are right now"
      MinV := 0; MaxV := 0; Damping := 0;
    end;
    jkSlide:
    begin
      if not HasMax then
        MaxV := AnchorDist; // a leash to where the partner stands today
      if not HasMin then
        MinV := 0;
      Dist := -1; Damping := 0;
      if MinV < 0 then
      begin
        L.PushNil;
        L.PushString('joint: slide min cannot be negative');
        Exit(2);
      end;
      if MaxV < MinV then
      begin
        L.PushNil;
        L.PushString('joint: slide needs max at least min');
        Exit(2);
      end;
    end;
    jkRotator:
    begin
      // Strength is the spring here; the pull cap does not apply to it.
      // 7.0.3's rotary spring drives (a - b) toward its rest angle, so the
      // script's "b turns +angle" goes in negated. (The limit joint reads
      // (b - a) - there the degrees transfer as they are.)
      Dist := -1; MinV := 0; MaxV := 0;
      AngleDeg := -AngleDeg * Pi / 180;
      if (not HasStrength) or (Strength <= 0) then
        Strength := 300;
      if Damping < 0 then
        Damping := 30;
    end;
  else
    // jkAngleLimit: degrees in, radians down; the pull cap is 'strength'.
    if not HasMin then
      MinV := -180;
    if not HasMax then
      MaxV := 180;
    Dist := -1; Damping := 0;
    if MaxV <= MinV then
    begin
      L.PushNil;
      L.PushString('joint: limit needs max above min');
      Exit(2);
    end;
    MinV := MinV * Pi / 180;
    MaxV := MaxV * Pi / 180;
    AngleDeg := 0;
  end;
  // A joint needs two physical sprites: tie the rope to a sprite that does
  // not ride the world and it holds nothing, so the join switches them on.
  Engine.Sprites.SetCollide(HA, True);
  Engine.Sprites.SetCollide(HB, True);
  if K = jkRotator then
    // strength was the spring's own; the pull cap stays the solver's choice
    L.PushInteger(Engine.Physics.AddJoint(K, HA, HB, cpv(Ax, Ay), cpv(Bx, By),
      Dist, MinV, MaxV, AngleDeg, Strength, Damping, 0))
  else
    // for the rest strength is the pull cap, and stiffness/damping are unused
    L.PushInteger(Engine.Physics.AddJoint(K, HA, HB, cpv(Ax, Ay), cpv(Bx, By),
      Dist, MinV, MaxV, 0, 0, 0, Strength));
  Result := 1;
end;

// collision.joint_free(id) -> true when that joint left the book (or leaves
// it on the next beat - a still-waiting one is dropped straight away).
function TLuaCollision.JointFree_func(L: Plua_State): integer; cdecl;
begin
  if (Engine <> nil) and (Engine.Physics <> nil) then
    L.PushBoolean(Engine.Physics.FreeJoint(L.ToInteger(1)))
  else
    L.PushBoolean(False);
  Result := 1;
end;

// collision.joints() -> how many joints the script armed (waiting ones too).
function TLuaCollision.Joints_func(L: Plua_State): integer; cdecl;
begin
  if (Engine <> nil) and (Engine.Physics <> nil) then
    L.PushInteger(Engine.Physics.JointCount)
  else
    L.PushInteger(0);
  Result := 1;
end;

// collision.joints_clear() -> how many joints were on the book; all leave.
function TLuaCollision.JointsClear_func(L: Plua_State): integer; cdecl;
begin
  if (Engine <> nil) and (Engine.Physics <> nil) then
    L.PushInteger(Engine.Physics.ClearJoints)
  else
    L.PushInteger(0);
  Result := 1;
end;

initialization
  ScriptTypes.RegisterLanguage('Lua', ['', '.tyro', '.lua', '.pluto'], TLuaScript);
end.
