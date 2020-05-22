unit RayLib3;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{**
 *  This file is part of tyro project, ported from C header raylib.h
 *
 * @license   MIT (https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey zaherdirkey on internet
 *}

{**********************************************************************************************
*
*   raylib - A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)
*
*   FEATURES:
*       - NO external dependencies, all required libraries included with raylib
*       - Multiplatform: Windows, Linux, FreeBSD, OpenBSD, NetBSD, DragonFly, MacOS, UWP, Android, Raspberry Pi, HTML5.
*       - Written in plain C code (C99) in PascalCase/camelCase notation
*       - Hardware accelerated with OpenGL (1.1, 2.1, 3.3 or ES2 - choose at compile)
*       - Unique OpenGL abstraction layer (usable as standalone module): [rlgl]
*       - Multiple Fonts formats supported (TTF, XNA fonts, AngelCode fonts)
*       - Outstanding texture formats support, including compressed formats (DXT, ETC, ASTC)
*       - Full 3d support for 3d Shapes, Models, Billboards, Heightmaps and more!
*       - Flexible Materials system, supporting classic maps and PBR maps
*       - Skeletal Animation support (CPU bones-based animation)
*       - Shaders support, including Model shaders and Postprocessing shaders
*       - Powerful math module for Vector, Matrix and Quaternion operations: [raymath]
*       - Audio loading and playing with streaming support (WAV, OGG, MP3, FLAC, XM, MOD)
*       - VR stereo rendering with configurable HMD device parameters
*       - Bindings to multiple programming languages available!
*
*   NOTES:
*       One custom font is loaded by default when InitWindow() [core]
*       If using OpenGL 3.3 or ES2, one default shader is loaded automatically (internally defined) [rlgl]
*       If using OpenGL 3.3 or ES2, several vertex buffers (VAO/VBO) are created to manage lines-triangles-quads
*
*   DEPENDENCIES (included):
*       [core] rglfw (github.com/glfw/glfw) for window/context management and input (only PLATFORM_DESKTOP)
*       [rlgl] glad (github.com/Dav1dde/glad) for OpenGL 3.3 extensions loading (only PLATFORM_DESKTOP)
*       [raudio] miniaudio (github.com/dr-soft/miniaudio) for audio device/context management
*
*   OPTIONAL DEPENDENCIES (included):
*       [core] rgif (Charlie Tangora, Ramon Santamaria) for GIF recording
*       [textures] stb_image (Sean Barret) for images loading (BMP, TGA, PNG, JPEG, HDR...)
*       [textures] stb_image_write (Sean Barret) for image writting (BMP, TGA, PNG, JPG)
*       [textures] stb_image_resize (Sean Barret) for image resizing algorithms
*       [textures] stb_perlin (Sean Barret) for Perlin noise image generation
*       [text] stb_truetype (Sean Barret) for ttf fonts loading
*       [text] stb_rect_pack (Sean Barret) for rectangles packing
*       [models] par_shapes (Philip Rideout) for parametric 3d shapes generation
*       [models] tinyobj_loader_c (Syoyo Fujita) for models loading (OBJ, MTL)
*       [models] cgltf (Johannes Kuhlmann) for models loading (glTF)
*       [raudio] stb_vorbis (Sean Barret) for OGG audio loading
*       [raudio] dr_flac (David Reid) for FLAC audio file loading
*       [raudio] dr_mp3 (David Reid) for MP3 audio file loading
*       [raudio] jar_xm (Joshua Reisenauer) for XM audio module loading
*       [raudio] jar_mod (Joshua Reisenauer) for MOD audio module loading
*
*
*   LICENSE: zlib/libpng
*
*   raylib is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software:
*
*   Copyright (c) 2013-2020 Ramon Santamaria (@raysan5)
*
*   This software is provided "as-is", without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
**********************************************************************************************}

interface
{*
* Tools used to translate
*       https://github.com/neslib/Chet
*       http://www.astonshell.com/freeware/c2pas32/
*
*}

uses
  Types, Classes, SysUtils,
  mnLibraries; // take it from github/parmaja/minilib

{$MINENUMSIZE 4} //All enum must be sized as Integer

type
  PPUTF8Char = ^PUTF8Char;

  { TColor }

  TColor = record
    constructor Create(ARed, AGreen, ABlue, AAlpha: Byte);
    case Cardinal of
      0: (Value: Cardinal);
      1: (Red, Green, Blue, Alpha: Byte);
  end;

  PColor = ^TColor;


const
  Lightgray: TColor = (Red: 200; Green: 200; Blue: 200; Alpha: 255);   // Light Gray
  Gray:      TColor = (Red: 130; Green: 130; Blue: 130; Alpha: 255);   // Gray
  DarkGray:  TColor = (Red: 80; Green: 80; Blue: 80; Alpha: 255);      // Dark Gray
  Yellow:    TColor = (Red: 253; Green: 249; Blue: 0; Alpha: 255);     // Yellow
  Gold:      TColor = (Red: 255; Green: 203; Blue: 0; Alpha: 255);     // Gold
  Orange:    TColor = (Red: 255; Green: 161; Blue: 0; Alpha: 255);     // Orange
  Pink:      TColor = (Red: 255; Green: 109; Blue: 194; Alpha: 255);   // Pink
  Red:       TColor = (Red: 230; Green: 41; Blue: 55; Alpha: 255);     // Red
  Maroon:    TColor = (Red: 190; Green: 33; Blue: 55; Alpha: 255);     // Maroon
  Green:     TColor = (Red: 0; Green: 228; Blue: 48; Alpha: 255);      // Green
  Lime:      TColor = (Red: 0; Green: 158; Blue: 47; Alpha: 255);      // Lime
  Darkgreen: TColor = (Red: 0; Green: 117; Blue: 44; Alpha: 255);      // Dark Green
  SkyBlue:   TColor = (Red: 102; Green: 191; Blue: 255; Alpha: 255);   // Sky Blue
  Blue:      TColor = (Red: 0; Green: 121; Blue: 241; Alpha: 255);     // Blue
  Darkblue:  TColor = (Red: 0; Green: 82; Blue: 172; Alpha: 255);      // Dark Blue
  Purple:    TColor = (Red: 200; Green: 122; Blue: 255; Alpha: 255);   // Purple
  Violet:    TColor = (Red: 135; Green: 60; Blue: 190; Alpha: 255);    // Violet
  Darkpurple:TColor = (Red: 112; Green: 31; Blue: 126; Alpha: 255);    // Dark Purple
  Beige:     TColor = (Red: 211; Green: 176; Blue: 131; Alpha: 255);   // Beige
  Brown:     TColor = (Red: 127; Green: 106; Blue: 79; Alpha: 255);    // Brown
  Darkbrown: TColor = (Red: 76; Green: 63; Blue: 47; Alpha: 255);      // Dark Brown
  White:     TColor = (Red: 255; Green: 255; Blue: 255; Alpha: 255);   // White
  Black:     TColor = (Red: 0; Green: 0; Blue: 0; Alpha: 255);         // Black
  Blank:     TColor = (Red: 0; Green: 0; Blue: 0; Alpha: 0);           // Blank (Transparent)
  Magenta:   TColor = (Red: 255; Green: 0; Blue: 255; Alpha: 255);     // Magenta
  RayWhite:  TColor = (Red: 245; Green: 245; Blue: 245; Alpha: 255);   // My own White (raylib logo)

type
  // Vector2 type

  { TVector2 }

  TVector2 = packed record
    X: Single;
    Y: Single;
    constructor Create(AX, AY: Single);
  end;
  PVector2 = ^TVector2;

  // Vector3 type
  TVector3 = packed record
    x: Single;
    y: Single;
    z: Single;
  end;
  PVector3 = ^TVector3;

  // Vector4 type
  TVector4 = packed record
    x: Single;
    y: Single;
    z: Single;
    w: Single;
  end;
  PVector4 = ^TVector4;

  // Quaternion type, same as Vector4
  TQuaternion = PVector4;

  // Matrix type (OpenGL style 4x4 - right handed, column major)
  TMatrix = record
      m0, m4, m8, m12: Single;
      m1, m5, m9, m13: Single;
      m2, m6, m10, m14: Single;
      m3, m7, m11, m15: Single;
  end;

  // Rectangle type

  { TRectangle }

  TRectangle = packed record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
    constructor Create(AX, AY, AWidth, AHeight: Single);
  end;
  PRectangle = ^TRectangle;
  PPRectangle = ^PRectangle;

  // Image type, bpp always RGBA (32bit)
  // NOTE: Data stored in CPU memory (RAM)
  TImage = packed record
    Data: Pointer;     // Image raw data
    Width: Integer;    // Image base width
    Height: Integer;   // Image base height
    Mipmaps: Integer;  // Mipmap levels, 1 by default
    Format: Integer;   // Data format (PixelFormat type)
  end;
  PImage = ^TImage;

  // Texture2D type
  // NOTE: Data stored in GPU memory
  TTexture2D = packed record
    ID: Cardinal;      // OpenGL texture id
    Width: Integer;    // Texture base width
    Height: Integer;   // Texture base height
    Mipmaps: Integer;  // Mipmap levels, 1 by default
    Format: Integer;   // Data format (PixelFormat type)
  end;
  PTexture2D = ^TTexture2D;

  // Texture type, same as Texture2D
  TTexture = TTexture2D;
  PTexture = ^PTexture2D;

  // TextureCubemap type, actually, same as Texture2D
  TTextureCubemap = TTexture2D;
  PTextureCubemap = ^PTexture2D;

  // RenderTexture2D type, for texture rendering
  // RenderTexture2D type, for texture rendering
  TRenderTexture2D = packed record
    ID: Cardinal;          // OpenGL Framebuffer Object (FBO) id
    Texture: TTexture2D;   // Color buffer attachment texture
    Depth: TTexture2D;     // Depth buffer attachment texture
    DepthTexture: Boolean; // Track if depth attachment is a texture or renderbuffer
  end;
  PRenderTexture2D = ^TRenderTexture2D;

  // RenderTexture type, same as RenderTexture2D
  TRenderTexture = TRenderTexture2D;
  PRenderTexture = ^TRenderTexture;

  // N-Patch layout info
  TNPatchInfo = packed record
    SourceRec: TRectangle; // Region in the texture
    Left: Integer;         // left border offset
    Top: Integer;          // top border offset
    Right: Integer;        // right border offset
    Bottom: Integer;       // bottom border offset
    AType: Integer;        // layout of the n-patch: 3x3, 1x3 or 3x1
  end;
  PNPatchInfo = ^TNPatchInfo;

// Font character info
// Font character info
  TCharInfo = packed record
    Value: Integer;    // Character value (Unicode)
    OffsetX: Integer;  // Character offset X when drawing
    OffsetY: Integer;  // Character offset Y when drawing
    AdvanceX: Integer; // Character advance position X
    Image: TImage;     // Character image data
  end;
  PCharInfo = ^TCharInfo;

  // Font type, includes texture and charSet array data
  TFont = packed record
    BaseSize: Integer;
    CharsCount: Integer;
    Texture: TTexture2D;
    Recs: PRectangle;
    Chars: PCharInfo;
  end;
  PFont = ^TFont;

  TSpriteFont = TFont; // SpriteFont type fallback, defaults to Font
  PSpriteFont = ^TSpriteFont;

  // Camera type, defines a camera position/orientation in 3d space
  TCamera3D = packed record
    Position: TVector3;    // Camera position
    Target: TVector3;      // Camera target it looks-at
    Up: TVector3;          // Camera up vector (rotation over its axis)
    Fovy: Single;          // Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
    AType: Integer;        // Camera type, defines projection type: TCamera_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
  end;
  PCamera3D = ^TCamera3D;

  TCamera = TCamera3D;      // Camera type fallback, defaults to Camera3D
  PCamera = ^TCamera;

  // Camera2D type, defines a 2d camera
  TCamera2D = packed record
    Offset: TVector2;
    Target: TVector2;
    Rotation: Single;
    Zoom: Single;
  end;
  PCamera2D = ^TCamera2D;

  // Vertex data definning a mesh
  // NOTE: Data stored in CPU memory (and GPU)
  TMesh = packed record
      VertexCount: Integer;        // Number of vertices stored in arrays
      TriangleCount: integer;      // Number of triangles stored (indexed or not)

      // Default vertex data
      Vertices: PSingle;        // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
      Texcoords: PSingle;       // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
      Texcoords2: PSingle;      // Vertex second texture coordinates (useful for lightmaps) (shader-location = 5)
      Normals: PSingle;         // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
      Tangents: PSingle;        // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
      Colors: PByte;            // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
      Indices: PWord;           // Vertex indices (in case vertex data comes indexed)

      // Animation vertex data
      AnimVertices: PSingle;    // Animated vertex positions (after bones transformations)
      AnimNormals: PSingle;     // Animated normals (after bones transformations)
      BoneIds: PInteger;           // Vertex bone ids, up to 4 bones influence by vertex (skinning)
      BoneWeights: PSingle;     // Vertex bone weight, up to 4 bones influence by vertex (skinning)

      // OpenGL identifiers
      VaoID: Cardinal;     // OpenGL Vertex Array Object id
      VboID: PCardinal;    // OpenGL Vertex Buffer Objects id (default vertex data)
  end;
  PMesh = TMesh;

  // Shader type (generic)
  TShader = packed record
    ID: Cardinal;         // Shader program id
    Locs: PInteger;       // Shader locations array (MAX_SHADER_LOCATIONS)
  end;
  PShader = ^TShader;


  // Material texture map
  TMaterialMap = packed record
    Texture: TTexture2D;   // Material map texture
    Color: TColor;         // Material map color
    Value: Single;         // Material map value
  end;
  PMaterialMap = ^TMaterialMap;

  // Material type (generic)
  TMaterial = packed record
    Shader: TShader;         // Material shader
    Maps: PMaterialMap;     // Material maps array (MAX_MATERIAL_MAPS)
    Params: PSingle;         // Material generic parameters (if required)
  end;
  PMaterial = ^TMaterial;

  // Transformation properties
  TTransform = packed record
    Translation: TVector3;   // Translation
    Rotation: TQuaternion;   // Rotation
    Scale: TVector3;         // Scale
  end;
  PTransform = ^TTransform;
  PPTransform = ^PTransform;

  // Bone information
  TBoneInfo = packed record
    Name: array[0..31] of AnsiChar;
    Parent: Integer;
  end;
  PBoneInfo = ^TBoneInfo;

  // Model type
  TModel = packed record
    Transform : TMatrix;        // Local transform matrix

    MeshCount : Integer;        // Number of meshes
    Meshes : PMesh;             // Meshes array

    MaterialCount : Integer;    // Number of materials
    Materials : PMaterial;      // Materials array
    MeshMaterial : PInteger;    // Mesh material number

    // Animation data
    BoneCount : Integer;        // Number of bones
    Bones : PBoneInfo;          // Bones information (skeleton)
    BindPose : PTransform;      // Bones base transformation (pose)
  end;
  PModel = ^TModel;

  // Model animation
  TModelAnimation = packed record
    BoneCount : Integer;        // Number of bones
    BoneInfo : PBoneInfo;       // Bones information (skeleton)

    FrameCount: Integer;       // Number of animation frames
    FramePoses: PPTransform;   // Poses array by frame
  end;
  PModelAnimation = ^TModelAnimation;


  // Ray type (useful for raycast)
  TRay = packed record
    Position : TVector3;        // Ray position (origin)
    Direction : TVector3;       // Ray direction
  end;
  PRay = ^TRay;

  // Raycast hit information
  TRayHitInfo = packed record
    Hit : Boolean;              // Did the ray hit something?
    Distance : Single;          // Distance to nearest hit
    position : TVector3;        // Position of nearest hit
    normal : TVector3;          // Surface normal of hit
  end;
  PRayHitInfo = ^TRayHitInfo;

  // Bounding box type
  TBoundingBox = packed record
    Min : TVector3;             // Minimum vertex box-corner
    Max : TVector3;             // Maximum vertex box-corner
  end;
  PBoundingBox = ^TBoundingBox;

  // Wave type, defines audio wave data
  TWave = packed record
    SampleCount : Cardinal;     // Total number of samples
    SampleRate : Cardinal;      // Frequency (samples per second)
    SampleSize : Cardinal;      // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    Channels : Cardinal;        // Number of channels (1-mono, 2-stereo)
    Data : Pointer;             // Buffer data pointer
  end;
  PWave = ^TWave;

  //
  TrAudioBuffer = record
  end;
  PrAudioBuffer = ^TrAudioBuffer;

  // Audio stream type
  // NOTE: Useful to create custom audio streams not bound to a specific file
  TAudioStream = packed record
    SampleRate: Cardinal;       // Frequency (samples per second)
    SampleSize: Cardinal;       // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    Channels: Cardinal;         // Number of channels (1-mono, 2-stereo)

    Buffer : PrAudioBuffer;     // Pointer to internal data used by the audio system
  end;
  PAudioStream = ^TAudioStream;

  // Sound source type
  TSound = packed record
    SampleCount : Cardinal;     // Total number of samples
    Stream : TAudioStream       // Audio stream
  end;
  PSound = ^TSound;

  // Music stream type (audio file streaming from memory)
  // NOTE: Anything longer than ~10 seconds should be streamed
  TMusic = record
    CtxType : Integer;          // Type of music context (audio filetype)
    CtxData : Pointer;          // Audio context data, depends on type

    SampleCount : Cardinal;     // Total number of samples
    LoopCount : Cardinal;       // Loops count (times music will play), 0 means infinite loop

    Stream : TAudioStream;      // Audio stream
  end;
  PMusic = ^TMusic;

  // Head-Mounted-Display device parameters
  TVrDeviceInfo = packed record
    hResolution: Integer;                            // HMD horizontal resolution in pixels
    vResolution: Integer;                            // HMD vertical resolution in pixels
    hScreenSize: Single;                             // HMD horizontal size in meters
    vScreenSize: Single;                             // HMD vertical size in meters
    vScreenCenter: Single;                           // HMD screen center in meters
    EyeToScreenDistance: Single;                     // HMD distance between eye and display in meters
    LensSeparationDistance: Single;                  // HMD lens separation distance in meters
    InterpupillaryDistance: Single;                  // HMD IPD (distance between pupils) in meters
    LensDistortionValues: array[0..3] of Single;     // HMD lens distortion constant parameters
    ChromaAbCorrection: array[0..3] of Single;       // HMD chromatic aberration correction parameters
  end;
  PVrDeviceInfo = ^TVrDeviceInfo;

  //----------------------------------------------------------------------------------
  // Enumerators Definition
  //----------------------------------------------------------------------------------
  // System config flags
  // NOTE: Used for bit masks
  TConfigFlag = (
    FLAG_RESERVED           = 1,    // Reserved
    FLAG_FULLSCREEN_MODE    = 2,    // Set to run program in fullscreen
    FLAG_WINDOW_RESIZABLE   = 4,    // Set to allow resizable window
    FLAG_WINDOW_UNDECORATED = 8,    // Set to disable window decoration (frame and buttons)
    FLAG_WINDOW_TRANSPARENT = 16,   // Set to allow transparent window
    FLAG_WINDOW_HIDDEN      = 128,  // Set to create the window initially hidden
    FLAG_WINDOW_ALWAYS_RUN  = 256,  // Set to allow windows running while minimized
    FLAG_MSAA_4X_HINT       = 32,   // Set to try enabling MSAA 4X
    FLAG_VSYNC_HINT         = 64    // Set to try enabling V-Sync on GPU
  );

  // Trace log type
  TTraceLogType = set of (
    LOG_ALL = 0,        // Display all logs
    LOG_TRACE,
    LOG_DEBUG,
    LOG_INFO,
    LOG_WARNING,
    LOG_ERROR,
    LOG_FATAL,
    LOG_NONE            // Disable logging
  );

const
  // Keyboard keys
  // Alphanumeric keys
  KEY_APOSTROPHE      = 39;
  KEY_COMMA           = 44;
  KEY_MINUS           = 45;
  KEY_PERIOD          = 46;
  KEY_SLASH           = 47;
  KEY_ZERO            = 48;
  KEY_ONE             = 49;
  KEY_TWO             = 50;
  KEY_THREE           = 51;
  KEY_FOUR            = 52;
  KEY_FIVE            = 53;
  KEY_SIX             = 54;
  KEY_SEVEN           = 55;
  KEY_EIGHT           = 56;
  KEY_NINE            = 57;
  KEY_SEMICOLON       = 59;
  KEY_EQUAL           = 61;
  KEY_A               = 65;
  KEY_B               = 66;
  KEY_C               = 67;
  KEY_D               = 68;
  KEY_E               = 69;
  KEY_F               = 70;
  KEY_G               = 71;
  KEY_H               = 72;
  KEY_I               = 73;
  KEY_J               = 74;
  KEY_K               = 75;
  KEY_L               = 76;
  KEY_M               = 77;
  KEY_N               = 78;
  KEY_O               = 79;
  KEY_P               = 80;
  KEY_Q               = 81;
  KEY_R               = 82;
  KEY_S               = 83;
  KEY_T               = 84;
  KEY_U               = 85;
  KEY_V               = 86;
  KEY_W               = 87;
  KEY_X               = 88;
  KEY_Y               = 89;
  KEY_Z               = 90;

  // Function keys
  KEY_SPACE           = 32;
  KEY_ESCAPE          = 256;
  KEY_ENTER           = 257;
  KEY_TAB             = 258;
  KEY_BACKSPACE       = 259;
  KEY_INSERT          = 260;
  KEY_DELETE          = 261;
  KEY_RIGHT           = 262;
  KEY_LEFT            = 263;
  KEY_DOWN            = 264;
  KEY_UP              = 265;
  KEY_PAGE_UP         = 266;
  KEY_PAGE_DOWN       = 267;
  KEY_HOME            = 268;
  KEY_END             = 269;
  KEY_CAPS_LOCK       = 280;
  KEY_SCROLL_LOCK     = 281;
  KEY_NUM_LOCK        = 282;
  KEY_PRINT_SCREEN    = 283;
  KEY_PAUSE           = 284;
  KEY_F1              = 290;
  KEY_F2              = 291;
  KEY_F3              = 292;
  KEY_F4              = 293;
  KEY_F5              = 294;
  KEY_F6              = 295;
  KEY_F7              = 296;
  KEY_F8              = 297;
  KEY_F9              = 298;
  KEY_F10             = 299;
  KEY_F11             = 300;
  KEY_F12             = 301;
  KEY_LEFT_SHIFT      = 340;
  KEY_LEFT_CONTROL    = 341;
  KEY_LEFT_ALT        = 342;
  KEY_LEFT_SUPER      = 343;
  KEY_RIGHT_SHIFT     = 344;
  KEY_RIGHT_CONTROL   = 345;
  KEY_RIGHT_ALT       = 346;
  KEY_RIGHT_SUPER     = 347;
  KEY_KB_MENU         = 348;
  KEY_LEFT_BRACKET    = 91;
  KEY_BACKSLASH       = 92;
  KEY_RIGHT_BRACKET   = 93;
  KEY_GRAVE           = 96;

  // Keypad keys
  KEY_KP_0            = 320;
  KEY_KP_1            = 321;
  KEY_KP_2            = 322;
  KEY_KP_3            = 323;
  KEY_KP_4            = 324;
  KEY_KP_5            = 325;
  KEY_KP_6            = 326;
  KEY_KP_7            = 327;
  KEY_KP_8            = 328;
  KEY_KP_9            = 329;
  KEY_KP_DECIMAL      = 330;
  KEY_KP_DIVIDE       = 331;
  KEY_KP_MULTIPLY     = 332;
  KEY_KP_SUBTRACT     = 333;
  KEY_KP_ADD          = 334;
  KEY_KP_ENTER        = 335;
  KEY_KP_EQUAL        = 336;

type
  // Android buttons
  TAndroidButton = (
    KEY_BACK            = 4,
    KEY_MENU            = 82,
    KEY_VOLUME_UP       = 24,
    KEY_VOLUME_DOWN     = 25
  );

  // Mouse buttons
  TMouseButton = (
    MOUSE_LEFT_BUTTON   = 0,
    MOUSE_RIGHT_BUTTON  = 1,
    MOUSE_MIDDLE_BUTTON = 2
  );

  // Gamepad number
  TGamepadNumber = (
      GAMEPAD_PLAYER1     = 0,
      GAMEPAD_PLAYER2     = 1,
      GAMEPAD_PLAYER3     = 2,
      GAMEPAD_PLAYER4     = 3
  );

  // Gamepad Buttons
  TGamepadButto = (
      // This is here just for error checking
      GAMEPAD_BUTTON_UNKNOWN = 0,

      // This is normally a DPAD
      GAMEPAD_BUTTON_LEFT_FACE_UP,
      GAMEPAD_BUTTON_LEFT_FACE_RIGHT,
      GAMEPAD_BUTTON_LEFT_FACE_DOWN,
      GAMEPAD_BUTTON_LEFT_FACE_LEFT,

      // This normally corresponds with PlayStation and Xbox controllers
      // XBOX: [Y,X,A,B]
      // PS3: [Triangle,Square,Cross,Circle]
      // No support for 6 button controllers though..
      GAMEPAD_BUTTON_RIGHT_FACE_UP,
      GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,
      GAMEPAD_BUTTON_RIGHT_FACE_DOWN,
      GAMEPAD_BUTTON_RIGHT_FACE_LEFT,

      // Triggers
      GAMEPAD_BUTTON_LEFT_TRIGGER_1,
      GAMEPAD_BUTTON_LEFT_TRIGGER_2,
      GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
      GAMEPAD_BUTTON_RIGHT_TRIGGER_2,

      // These are buttons in the center of the gamepad
      GAMEPAD_BUTTON_MIDDLE_LEFT,     //PS3 Select
      GAMEPAD_BUTTON_MIDDLE,          //PS Button/XBOX Button
      GAMEPAD_BUTTON_MIDDLE_RIGHT,    //PS3 Start

      // These are the joystick press in buttons
      GAMEPAD_BUTTON_LEFT_THUMB,
      GAMEPAD_BUTTON_RIGHT_THUMB
  );

  TGamepadAxis = (
    // This is here just for error checking
    GAMEPAD_AXIS_UNKNOWN = 0,

    // Left stick
    GAMEPAD_AXIS_LEFT_X,
    GAMEPAD_AXIS_LEFT_Y,

    // Right stick
    GAMEPAD_AXIS_RIGHT_X,
    GAMEPAD_AXIS_RIGHT_Y,

    // Pressure levels for the back triggers
    GAMEPAD_AXIS_LEFT_TRIGGER,      // [1..-1] (pressure-level)
    GAMEPAD_AXIS_RIGHT_TRIGGER      // [1..-1] (pressure-level)
  );

  // Shader location point type
  TShaderLocationIndex = (
    LOC_VERTEX_POSITION = 0,
    LOC_VERTEX_TEXCOORD01,
    LOC_VERTEX_TEXCOORD02,
    LOC_VERTEX_NORMAL,
    LOC_VERTEX_TANGENT,
    LOC_VERTEX_COLOR,
    LOC_MATRIX_MVP,
    LOC_MATRIX_MODEL,
    LOC_MATRIX_VIEW,
    LOC_MATRIX_PROJECTION,
    LOC_VECTOR_VIEW,
    LOC_COLOR_DIFFUSE,
    LOC_COLOR_SPECULAR,
    LOC_COLOR_AMBIENT,
    LOC_MAP_ALBEDO,          // LOC_MAP_DIFFUSE
    LOC_MAP_METALNESS,       // LOC_MAP_SPECULAR
    LOC_MAP_NORMAL,
    LOC_MAP_ROUGHNESS,
    LOC_MAP_OCCLUSION,
    LOC_MAP_EMISSION,
    LOC_MAP_HEIGHT,
    LOC_MAP_CUBEMAP,
    LOC_MAP_IRRADIANCE,
    LOC_MAP_PREFILTER,
    LOC_MAP_BRDF
  );

{const
  LOC_MAP_DIFFUSE  =    TShaderLocationIndex.LOC_MAP_ALBEDO;
  LOC_MAP_SPECULAR =    TShaderLocationIndex.LOC_MAP_METALNESS;} //TODO

  // Shader uniform data types
  TShaderUniformDataType = (
    UNIFORM_FLOAT = 0,
    UNIFORM_VEC2,
    UNIFORM_VEC3,
    UNIFORM_VEC4,
    UNIFORM_INT,
    UNIFORM_IVEC2,
    UNIFORM_IVEC3,
    UNIFORM_IVEC4,
    UNIFORM_SAMPLER2D
  );

  // Material map type
  TMaterialMapType = (
    MAP_ALBEDO    = 0,       // MAP_DIFFUSE
    MAP_METALNESS = 1,       // MAP_SPECULAR
    MAP_NORMAL    = 2,
    MAP_ROUGHNESS = 3,
    MAP_OCCLUSION,
    MAP_EMISSION,
    MAP_HEIGHT,
    MAP_CUBEMAP,             // NOTE: Uses GL_TEXTURE_CUBE_MAP
    MAP_IRRADIANCE,          // NOTE: Uses GL_TEXTURE_CUBE_MAP
    MAP_PREFILTER,           // NOTE: Uses GL_TEXTURE_CUBE_MAP
    MAP_BRDF
  );

{  MAP_DIFFUSE    =  MAP_ALBEDO
  MAP_SPECULAR   =  MAP_METALNESS} //TODO

  // Pixel formats
  // NOTE: Support depends on OpenGL version and platform
  TPixelFormat = (
    UNCOMPRESSED_GRAYSCALE = 1,     // 8 bit per pixel (no alpha)
    UNCOMPRESSED_GRAY_ALPHA,        // 8*2 bpp (2 channels)
    UNCOMPRESSED_R5G6B5,            // 16 bpp
    UNCOMPRESSED_R8G8B8,            // 24 bpp
    UNCOMPRESSED_R5G5B5A1,          // 16 bpp (1 bit alpha)
    UNCOMPRESSED_R4G4B4A4,          // 16 bpp (4 bit alpha)
    UNCOMPRESSED_R8G8B8A8,          // 32 bpp
    UNCOMPRESSED_R32,               // 32 bpp (1 channel - float)
    UNCOMPRESSED_R32G32B32,         // 32*3 bpp (3 channels - float)
    UNCOMPRESSED_R32G32B32A32,      // 32*4 bpp (4 channels - float)
    COMPRESSED_DXT1_RGB,            // 4 bpp (no alpha)
    COMPRESSED_DXT1_RGBA,           // 4 bpp (1 bit alpha)
    COMPRESSED_DXT3_RGBA,           // 8 bpp
    COMPRESSED_DXT5_RGBA,           // 8 bpp
    COMPRESSED_ETC1_RGB,            // 4 bpp
    COMPRESSED_ETC2_RGB,            // 4 bpp
    COMPRESSED_ETC2_EAC_RGBA,       // 8 bpp
    COMPRESSED_PVRT_RGB,            // 4 bpp
    COMPRESSED_PVRT_RGBA,           // 4 bpp
    COMPRESSED_ASTC_4x4_RGBA,       // 8 bpp
    COMPRESSED_ASTC_8x8_RGBA        // 2 bpp
  );

  // Texture parameters: filter mode
  // NOTE 1: Filtering considers mipmaps if available in the texture
  // NOTE 2: Filter is accordingly set for minification and magnification
  TTextureFilterMode = (
    FILTER_POINT = 0,               // No filter, just pixel aproximation
    FILTER_BILINEAR,                // Linear filtering
    FILTER_TRILINEAR,               // Trilinear filtering (linear with mipmaps)
    FILTER_ANISOTROPIC_4X,          // Anisotropic filtering 4x
    FILTER_ANISOTROPIC_8X,          // Anisotropic filtering 8x
    FILTER_ANISOTROPIC_16X         // Anisotropic filtering 16x
  );

  // Cubemap layout type
  TCubemapLayoutType = (
    CUBEMAP_AUTO_DETECT = 0,        // Automatically detect layout type
    CUBEMAP_LINE_VERTICAL,          // Layout is defined by a vertical line with faces
    CUBEMAP_LINE_HORIZONTAL,        // Layout is defined by an horizontal line with faces
    CUBEMAP_CROSS_THREE_BY_FOUR,    // Layout is defined by a 3x4 cross with cubemap faces
    CUBEMAP_CROSS_FOUR_BY_THREE,    // Layout is defined by a 4x3 cross with cubemap faces
    CUBEMAP_PANORAMA                // Layout is defined by a panorama image (equirectangular map)
  );

  // Texture parameters: wrap mode
  TTextureWrapMode = (
    WRAP_REPEAT = 0,        // Repeats texture in tiled mode
    WRAP_CLAMP,             // Clamps texture to edge pixel in tiled mode
    WRAP_MIRROR_REPEAT,     // Mirrors and repeats the texture in tiled mode
    WRAP_MIRROR_CLAMP       // Mirrors and clamps to border the texture in tiled mode
  );

  // Font type, defines generation method
  TFontType = (
    FONT_DEFAULT = 0,       // Default font generation, anti-aliased
    FONT_BITMAP,            // Bitmap font generation, no anti-aliasing
    FONT_SDF                // SDF font generation, requires external shader
  );

  // Color blending modes (pre-defined)
  TBlendMode = (
    BLEND_ALPHA = 0,        // Blend textures considering alpha (default)
    BLEND_ADDITIVE,         // Blend textures adding colors
    BLEND_MULTIPLIED        // Blend textures multiplying colors
  );

  // Gestures type
  // NOTE: It could be used as flags to enable only some gestures
  TGestureType = (
    GESTURE_NONE        = 0,
    GESTURE_TAP         = 1,
    GESTURE_DOUBLETAP   = 2,
    GESTURE_HOLD        = 4,
    GESTURE_DRAG        = 8,
    GESTURE_SWIPE_RIGHT = 16,
    GESTURE_SWIPE_LEFT  = 32,
    GESTURE_SWIPE_UP    = 64,
    GESTURE_SWIPE_DOWN  = 128,
    GESTURE_PINCH_IN    = 256,
    GESTURE_PINCH_OUT   = 512
  );

  // Camera system modes
  TCameraMode = (
    CAMERA_CUSTOM = 0,
    CAMERA_FREE,
    CAMERA_ORBITAL,
    CAMERA_FIRST_PERSON,
    CAMERA_THIRD_PERSON
  );

  // Camera projection modes
  TCameraType = (
    CAMERA_PERSPECTIVE = 0,
    CAMERA_ORTHOGRAPHIC
  );

  // Type of n-patch
  TNPatchType = (
    NPT_9PATCH = 0,         // Npatch defined by 3x3 tiles
    NPT_3PATCH_VERTICAL,    // Npatch defined by 1x3 tiles
    NPT_3PATCH_HORIZONTAL   // Npatch defined by 3x1 tiles
  );

  // Callbacks to be implemented by users
type
  TTraceLogCallback = procedure(LogType : Integer; Text: PAnsiChar; Args: Pointer); cdecl; //TODO check Args

//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...

//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

// Window-related functions
// Initialize window and OpenGL context
  TInitWindow = procedure(width: Integer; height: Integer; const title: PUTF8Char); cdecl;
// Check if KEY_ESCAPE pressed or Close icon pressed
  TWindowShouldClose = function: boolean; cdecl;
  TCloseWindow = procedure; cdecl;
  TIsWindowReady = function: boolean; cdecl;
  TIsWindowMinimized = function: boolean; cdecl;
  TIsWindowResized = function: boolean; cdecl;
  TIsWindowHidden = function: boolean; cdecl;
  TIsWindowFullscreen = function: boolean; cdecl;
  TToggleFullscreen = procedure; cdecl;
  TUnhideWindow = procedure; cdecl;
  THideWindow = procedure; cdecl;
  TSetWindowIcon = procedure(image: TImage); cdecl;
  TSetWindowTitle = procedure(const title: PUTF8Char); cdecl;
  TSetWindowPosition = procedure(x: Integer; y: Integer); cdecl;
  TSetWindowMonitor = procedure(monitor: Integer); cdecl;
  TSetWindowMinSize = procedure(width: Integer; height: Integer); cdecl;
  TSetWindowSize = procedure(width: Integer; height: Integer); cdecl;
  TGetWindowHandle = function: Pointer; cdecl;
  TGetScreenWidth = function: Integer; cdecl;
  TGetScreenHeight = function: Integer; cdecl;
  TGetMonitorCount = function: Integer; cdecl;
  TGetMonitorWidth = function(monitor: Integer): Integer; cdecl;
  TGetMonitorHeight = function(monitor: Integer): Integer; cdecl;
  TGetMonitorPhysicalWidth = function(monitor: Integer): Integer; cdecl;
  TGetMonitorPhysicalHeight = function(monitor: Integer): Integer; cdecl;
  TGetWindowPosition = function: TVector2; cdecl;
  TGetMonitorName = function(monitor: Integer): PUTF8Char; cdecl;
  TGetClipboardText = function: PUTF8Char; cdecl;
  TSetClipboardText = procedure(const text: PUTF8Char); cdecl;

// Cursor-related functions
  TShowCursor = procedure; cdecl;
  THideCursor = procedure; cdecl;
  TIsCursorHidden = function: boolean; cdecl;
  TEnableCursor = procedure; cdecl;
  TDisableCursor = procedure; cdecl;

// Drawing-related functions
  TClearBackground = procedure(color: TColor); cdecl;
  TBeginDrawing = procedure; cdecl;
  TEndDrawing = procedure; cdecl;
  TBeginMode2D = procedure(camera: TCamera2D); cdecl;
  TEndMode2D = procedure; cdecl;
  TBeginMode3D = procedure(camera: TCamera3D); cdecl;
  TEndMode3D = procedure; cdecl;
  TBeginTextureMode = procedure(target: TRenderTexture2D); cdecl;
  TEndTextureMode = procedure; cdecl;
  TBeginScissorMode = procedure(x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  TEndScissorMode = procedure; cdecl;

// Screen-space-related functions
  TGetMouseRay = function(mousePosition: TVector2; camera: TCamera): TRay; cdecl;
  TGetCameraMatrix = function(camera: TCamera): TMatrix; cdecl;
  TGetCameraMatrix2D = function(camera: TCamera2D): TMatrix; cdecl;
  TGetWorldToScreen = function(position: TVector3; camera: TCamera): TVector2; cdecl;
  TGetWorldToScreenEx = function(position: TVector3; camera: TCamera; width: Integer; height: Integer): TVector2; cdecl;
  TGetWorldToScreen2D = function(position: TVector2; camera: TCamera2D): TVector2; cdecl;
  TGetScreenToWorld2D = function(position: TVector2; camera: TCamera2D): TVector2; cdecl;

// Timing-related functions
  TSetTargetFPS = procedure(fps: Integer); cdecl;
  TGetFPS = function: Integer; cdecl;
  TGetFrameTime = function: Single; cdecl;
  TGetTime = function: Double; cdecl;

// Color-related functions
  TColorToInt = function(color: TColor): Integer; cdecl;
  TColorNormalize = function(color: TColor): TVector4; cdecl;
  TColorFromNormalized = function(normalized: TVector4): TColor; cdecl;
  TColorToHSV = function(color: TColor): TVector3; cdecl;
  TColorFromHSV = function(hsv: TVector3): TColor; cdecl;
  TGetColor = function(hexValue: Integer): TColor; cdecl;
  TFade = function(color: TColor; Alpha: Single): TColor; cdecl;

// Misc. functions
  TSetConfigFlags = procedure(flags: Cardinal); cdecl;
  TSetTraceLogLevel = procedure(logType: TTraceLogType); cdecl;
  TSetTraceLogExit = procedure(logType: Integer); cdecl;
  TSetTraceLogCallback = procedure(callback: TTraceLogCallback); cdecl;
  TTraceLog = procedure(logType: Integer; const text: PUTF8Char) varargs; cdecl;
  TTakeScreenshot = procedure(const fileName: PUTF8Char); cdecl;
  TGetRandomValue = function(min: Integer; max: Integer): Integer; cdecl;

// Files management functions
  TLoadFileData = function(const fileName: PUTF8Char; bytesRead: PCardinal): PByte; cdecl;
  TSaveFileData = procedure(const fileName: PUTF8Char; data: Pointer; bytesToWrite: Cardinal); cdecl;
  TLoadFileText = function(const fileName: PUTF8Char): PUTF8Char; cdecl;
  TSaveFileText = procedure(const fileName: PUTF8Char; text: PUTF8Char); cdecl;
  TFileExists = function(const fileName: PUTF8Char): boolean; cdecl;
  TIsFileExtension = function(const fileName: PUTF8Char; const ext: PUTF8Char): boolean; cdecl;
  TDirectoryExists = function(const dirPath: PUTF8Char): boolean; cdecl;
  TGetExtension = function(const fileName: PUTF8Char): PUTF8Char; cdecl;
  TGetFileName = function(const filePath: PUTF8Char): PUTF8Char; cdecl;
  TGetFileNameWithoutExt = function(const filePath: PUTF8Char): PUTF8Char; cdecl;
  TGetDirectoryPath = function(const filePath: PUTF8Char): PUTF8Char; cdecl;
  TGetPrevDirectoryPath = function(const dirPath: PUTF8Char): PUTF8Char; cdecl;
  TGetWorkingDirectory = function: PUTF8Char; cdecl;
  TGetDirectoryFiles = function(const dirPath: PUTF8Char; count: PInteger): PPUTF8Char; cdecl;
  TClearDirectoryFiles = procedure; cdecl;
  TChangeDirectory = function(const dir: PUTF8Char): boolean; cdecl;
  TIsFileDropped = function: boolean; cdecl;
  TGetDroppedFiles = function(count: PInteger): PPUTF8Char; cdecl;
  TClearDroppedFiles = procedure; cdecl;
  TGetFileModTime = function(const fileName: PUTF8Char): Integer; cdecl;

  TCompressData = function(data: PByte; dataLength: Integer; compDataLength: PInteger): PByte; cdecl;
  TDecompressData = function(compData: PByte; compDataLength: Integer; dataLength: PInteger): PByte; cdecl;

// Persistent storage management
  TSaveStorageValue = procedure(position: Cardinal; value: Integer); cdecl;
  TLoadStorageValue = function(position: Cardinal): Integer; cdecl;

  TOpenURL = procedure(const url: PUTF8Char); cdecl;

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

// Input-related functions: keyboard
  TIsKeyPressed = function(key: Integer): boolean; cdecl;
  TIsKeyDown = function(key: Integer): boolean; cdecl;
  TIsKeyReleased = function(key: Integer): boolean; cdecl;

// Input-related functions: gamepads
  TIsKeyUp = function(key: Integer): boolean; cdecl;
  TSetExitKey = procedure(key: Integer); cdecl;
  TGetKeyPressed = function: Integer; cdecl;
  TIsGamepadAvailable = function(gamepad: Integer): boolean; cdecl;
  TIsGamepadName = function(gamepad: Integer; const name: PUTF8Char): boolean; cdecl;
  TGetGamepadName = function(gamepad: Integer): PUTF8Char; cdecl;
  TIsGamepadButtonPressed = function(gamepad: Integer; button: Integer): boolean; cdecl;
  TIsGamepadButtonDown = function(gamepad: Integer; button: Integer): boolean; cdecl;
  TIsGamepadButtonReleased = function(gamepad: Integer; button: Integer): boolean; cdecl;
  TIsGamepadButtonUp = function(gamepad: Integer; button: Integer): boolean; cdecl;
  TGetGamepadButtonPressed = function: Integer; cdecl;
  TGetGamepadAxisCount = function(gamepad: Integer): Integer; cdecl;
  TGetGamepadAxisMovement = function(gamepad: Integer; axis: Integer): Single; cdecl;

// Input-related functions: mouse
  TIsMouseButtonPressed = function(button: Integer): boolean; cdecl;
  TIsMouseButtonDown = function(button: Integer): boolean; cdecl;
  TIsMouseButtonReleased = function(button: Integer): boolean; cdecl;
  TIsMouseButtonUp = function(button: Integer): boolean; cdecl;
  TGetMouseX = function: Integer; cdecl;
  TGetMouseY = function: Integer; cdecl;
  TGetMousePosition = function: TVector2; cdecl;
  TSetMousePosition = procedure(x: Integer; y: Integer); cdecl;
  TSetMouseOffset = procedure(offsetX: Integer; offsetY: Integer); cdecl;
  TSetMouseScale = procedure(scaleX: Single; scaleY: Single); cdecl;
  TGetMouseWheelMove = function: Integer; cdecl;

// Input-related functions: touch
  TGetTouchX = function: Integer; cdecl;
  TGetTouchY = function: Integer; cdecl;
  TGetTouchPosition = function(index: Integer): TVector2; cdecl;

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: gestures)
//------------------------------------------------------------------------------------
  TSetGesturesEnabled = procedure(gestureFlags: Cardinal); cdecl;
  TIsGestureDetected = function(gesture: Integer): boolean; cdecl;
  TGetGestureDetected = function: Integer; cdecl;
  TGetTouchPointsCount = function: Integer; cdecl;
  TGetGestureHoldDuration = function: Single; cdecl;
  TGetGestureDragVector = function: TVector2; cdecl;
  TGetGestureDragAngle = function: Single; cdecl;
  TGetGesturePinchVector = function: TVector2; cdecl;
  TGetGesturePinchAngle = function: Single; cdecl;

//------------------------------------------------------------------------------------
// Camera System Functions (Module: camera)
//------------------------------------------------------------------------------------
  TSetCameraMode = procedure(camera: TCamera; mode: Integer); cdecl;
  TUpdateCamera = procedure(camera: PCamera); cdecl;

  TSetCameraPanControl = procedure(panKey: Integer); cdecl;
  TSetCameraAltControl = procedure(altKey: Integer); cdecl;
  TSetCameraSmoothZoomControl = procedure(szKey: Integer); cdecl;
  TSetCameraMoveControls = procedure(frontKey: Integer; backKey: Integer; rightKey: Integer; leftKey: Integer; upKey: Integer; downKey: Integer); cdecl;

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------

// Basic shapes drawing functions
  TDrawPixel = procedure(posX: Integer; posY: Integer; color: TColor); cdecl;
  TDrawPixelV = procedure(position: TVector2; color: TColor); cdecl;
  TDrawLine = procedure(startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl;
  TDrawLineV = procedure(startPos: TVector2; endPos: TVector2; color: TColor); cdecl;
  TDrawLineEx = procedure(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl;
  TDrawLineBezier = procedure(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl;
  TDrawLineStrip = procedure(points: PVector2; numPoints: Integer; color: TColor); cdecl;
  TDrawCircle = procedure(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl;
  TDrawCircleSector = procedure(center: TVector2; radius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  TDrawCircleSectorLines = procedure(center: TVector2; radius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  TDrawCircleGradient = procedure(centerX: Integer; centerY: Integer; radius: Single; color1: TColor; color2: TColor); cdecl;
  TDrawCircleV = procedure(center: TVector2; radius: Single; color: TColor); cdecl;
  TDrawCircleLines = procedure(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl;
  TDrawEllipse = procedure(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl;
  TDrawEllipseLines = procedure(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl;
  TDrawRing = procedure(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  TDrawRingLines = procedure(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  TDrawRectangle = procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl;
  TDrawRectangleV = procedure(position: TVector2; size: TVector2; color: TColor); cdecl;
  TDrawRectangleRec = procedure(rec: TRectangle; color: TColor); cdecl;
  TDrawRectanglePro = procedure(rec: TRectangle; origin: TVector2; rotation: Single; color: TColor); cdecl;
  TDrawRectangleGradientV = procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl;
  TDrawRectangleGradientH = procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl;
  TDrawRectangleGradientEx = procedure(rec: TRectangle; col1: TColor; col2: TColor; col3: TColor; col4: TColor); cdecl;
  TDrawRectangleLines = procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl;
  TDrawRectangleLinesEx = procedure(rec: TRectangle; lineThick: Integer; color: TColor); cdecl;
  TDrawRectangleRounded = procedure(rec: TRectangle; roundness: Single; segments: Integer; color: TColor); cdecl;
  TDrawRectangleRoundedLines = procedure(rec: TRectangle; roundness: Single; segments: Integer; lineThick: Integer; color: TColor); cdecl;
  TDrawTriangle = procedure(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl;
  TDrawTriangleLines = procedure(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl;
  TDrawTriangleFan = procedure(points: PVector2; numPoints: Integer; color: TColor); cdecl;
  TDrawTriangleStrip = procedure(points: PVector2; pointsCount: Integer; color: TColor); cdecl;
  TDrawPoly = procedure(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl;
  TDrawPolyLines = procedure(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl;

// Basic shapes collision detection functions
  TCheckCollisionRecs = function(rec1: TRectangle; rec2: TRectangle): boolean; cdecl;
  TCheckCollisionCircles = function(center1: TVector2; radius1: Single; center2: TVector2; radius2: Single): boolean; cdecl;
  TCheckCollisionCircleRec = function(center: TVector2; radius: Single; rec: TRectangle): boolean; cdecl;
  TGetCollisionRec = function(rec1: TRectangle; rec2: TRectangle): TRectangle; cdecl;
  TCheckCollisionPointRec = function(point: TVector2; rec: TRectangle): boolean; cdecl;
  TCheckCollisionPointCircle = function(point: TVector2; center: TVector2; radius: Single): boolean; cdecl;
  TCheckCollisionPointTriangle = function(point: TVector2; p1: TVector2; p2: TVector2; p3: TVector2): boolean; cdecl;

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

// Image loading functions
// NOTE: This functions do not require GPU access
  TLoadImage = function(const fileName: PUTF8Char): TImage; cdecl;
  TLoadImageEx = function(pixels: PColor; width: Integer; height: Integer): TImage; cdecl;
  TLoadImagePro = function(data: Pointer; width: Integer; height: Integer; format: Integer): TImage; cdecl;
  TLoadImageRaw = function(const fileName: PUTF8Char; width: Integer; height: Integer; format: Integer; headerSize: Integer): TImage; cdecl;
  TUnloadImage = procedure(image: TImage); cdecl;
  TExportImage = procedure(image: TImage; const fileName: PUTF8Char); cdecl;
  TExportImageAsCode = procedure(image: TImage; const fileName: PUTF8Char); cdecl;
  TGetImageData = function(image: TImage): PColor; cdecl;
  TGetImageDataNormalized = function(image: TImage): PVector4; cdecl;

// Image generation functions
  TGenImageColor = function(width: Integer; height: Integer; color: TColor): TImage; cdecl;
  TGenImageGradientV = function(width: Integer; height: Integer; top: TColor; bottom: TColor): TImage; cdecl;
  TGenImageGradientH = function(width: Integer; height: Integer; left: TColor; right: TColor): TImage; cdecl;
  TGenImageGradientRadial = function(width: Integer; height: Integer; density: Single; inner: TColor; outer: TColor): TImage; cdecl;
  TGenImageChecked = function(width: Integer; height: Integer; checksX: Integer; checksY: Integer; col1: TColor; col2: TColor): TImage; cdecl;
  TGenImageWhiteNoise = function(width: Integer; height: Integer; factor: Single): TImage; cdecl;
  TGenImagePerlinNoise = function(width: Integer; height: Integer; offsetX: Integer; offsetY: Integer; scale: Single): TImage; cdecl;
  TGenImageCellular = function(width: Integer; height: Integer; tileSize: Integer): TImage; cdecl;

// Image manipulation functions
  TImageCopy = function(image: TImage): TImage; cdecl;
  TImageFromImage = function(image: TImage; rec: TRectangle): TImage; cdecl;
  TImageText = function(const text: PUTF8Char; fontSize: Integer; color: TColor): TImage; cdecl;
  TImageTextEx = function(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single; tint: TColor): TImage; cdecl;
  TImageToPOT = procedure(image: PImage; fillColor: TColor); cdecl;
  TImageFormat = procedure(image: PImage; newFormat: Integer); cdecl;
  TImageAlphaMask = procedure(image: PImage; alphaMask: TImage); cdecl;
  TImageAlphaClear = procedure(image: PImage; color: TColor; threshold: Single); cdecl;
  TImageAlphaCrop = procedure(image: PImage; threshold: Single); cdecl;
  TImageAlphaPremultiply = procedure(image: PImage); cdecl;
  TImageCrop = procedure(image: PImage; crop: TRectangle); cdecl;
  TImageResize = procedure(image: PImage; newWidth: Integer; newHeight: Integer); cdecl;
  TImageResizeNN = procedure(image: PImage; newWidth: Integer; newHeight: Integer); cdecl;
  TImageResizeCanvas = procedure(image: PImage; newWidth: Integer; newHeight: Integer; offsetX: Integer; offsetY: Integer; color: TColor); cdecl;
  TImageMipmaps = procedure(image: PImage); cdecl;
  TImageDither = procedure(image: PImage; rBpp: Integer; gBpp: Integer; bBpp: Integer; aBpp: Integer); cdecl;
  TImageFlipVertical = procedure(image: PImage); cdecl;
  TImageFlipHorizontal = procedure(image: PImage); cdecl;
  TImageRotateCW = procedure(image: PImage); cdecl;
  TImageRotateCCW = procedure(image: PImage); cdecl;
  TImageColorTint = procedure(image: PImage; color: TColor); cdecl;
  TImageColorInvert = procedure(image: PImage); cdecl;
  TImageColorGrayscale = procedure(image: PImage); cdecl;
  TImageColorContrast = procedure(image: PImage; contrast: Single); cdecl;
  TImageColorBrightness = procedure(image: PImage; brightness: Integer); cdecl;
  TImageColorReplace = procedure(image: PImage; color: TColor; replace: TColor); cdecl;
  TImageExtractPalette = function(image: TImage; maxPaletteSize: Integer; extractCount: PInteger): PColor; cdecl;
  TGetImageAlphaBorder = function(image: TImage; threshold: Single): TRectangle; cdecl;

// Image drawing functions
// NOTE: Image software-rendering functions (CPU)
  TImageClearBackground = procedure(dst: PImage; color: TColor); cdecl;
  TImageDrawPixel = procedure(dst: PImage; posX: Integer; posY: Integer; color: TColor); cdecl;
  TImageDrawPixelV = procedure(dst: PImage; position: TVector2; color: TColor); cdecl;
  TImageDrawLine = procedure(dst: PImage; startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl;
  TImageDrawLineV = procedure(dst: PImage; start: TVector2; &end: TVector2; color: TColor); cdecl;
  TImageDrawCircle = procedure(dst: PImage; centerX: Integer; centerY: Integer; radius: Integer; color: TColor); cdecl;
  TImageDrawCircleV = procedure(dst: PImage; center: TVector2; radius: Integer; color: TColor); cdecl;
  TImageDrawRectangle = procedure(dst: PImage; posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl;
  TImageDrawRectangleV = procedure(dst: PImage; position: TVector2; size: TVector2; color: TColor); cdecl;
  TImageDrawRectangleRec = procedure(dst: PImage; rec: TRectangle; color: TColor); cdecl;
  TImageDrawRectangleLines = procedure(dst: PImage; rec: TRectangle; thick: Integer; color: TColor); cdecl;
  TImageDraw = procedure(dst: PImage; src: TImage; srcRec: TRectangle; dstRec: TRectangle; tint: TColor); cdecl;
  TImageDrawText = procedure(dst: PImage; position: TVector2; const text: PUTF8Char; fontSize: Integer; color: TColor); cdecl;
  TImageDrawTextEx = procedure(dst: PImage; position: TVector2; font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single; color: TColor); cdecl;

// Texture loading functions
// NOTE: These functions require GPU access
  TLoadTexture = function(const fileName: PUTF8Char): TTexture2D; cdecl;
  TLoadTextureFromImage = function(image: TImage): TTexture2D; cdecl;
  TLoadTextureCubemap = function(image: TImage; layoutType: Integer): TTextureCubemap; cdecl;
  TLoadRenderTexture = function(width: Integer; height: Integer): TRenderTexture2D; cdecl;
  TUnloadTexture = procedure(texture: TTexture2D); cdecl;
  TUnloadRenderTexture = procedure(target: TRenderTexture2D); cdecl;
  TUpdateTexture = procedure(texture: TTexture2D; const pixels: Pointer); cdecl;
  TGetTextureData = function(texture: TTexture2D): TImage; cdecl;
  TGetScreenData = function: TImage; cdecl;

// Texture configuration functions
  TGenTextureMipmaps = procedure(texture: PTexture2D); cdecl;
  TSetTextureFilter = procedure(texture: TTexture2D; FilterMode: TTextureFilterMode); cdecl;
  TSetTextureWrap = procedure(texture: TTexture2D; wrapMode: Integer); cdecl;

// Texture drawing functions
  TDrawTexture = procedure(texture: TTexture2D; posX: Integer; posY: Integer; tint: TColor); cdecl;
  TDrawTextureV = procedure(texture: TTexture2D; position: TVector2; tint: TColor); cdecl;
  TDrawTextureEx = procedure(texture: TTexture2D; position: TVector2; rotation: Single; scale: Single; tint: TColor); cdecl;
  TDrawTextureRec = procedure(texture: TTexture2D; sourceRec: TRectangle; position: TVector2; tint: TColor); cdecl;
  TDrawTextureQuad = procedure(texture: TTexture2D; tiling: TVector2; offset: TVector2; quad: TRectangle; tint: TColor); cdecl;
  TDrawTexturePro = procedure(texture: TTexture2D; sourceRec: TRectangle; destRec: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl;
  TDrawTextureNPatch = procedure(texture: TTexture2D; nPatchInfo: TNPatchInfo; destRec: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl;

// Image/Texture misc functions
  TGetPixelDataSize = function(width: Integer; height: Integer; format: Integer): Integer; cdecl;

//------------------------------------------------------------------------------------
// Font Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

// Font loading/unloading functions
  TGetFontDefault = function: TFont; cdecl;
  TLoadFont = function(const fileName: PUTF8Char): TFont; cdecl;
  TLoadFontEx = function(const fileName: PUTF8Char; fontSize: Integer; fontChars: PInteger; charsCount: Integer): TFont; cdecl;
  TLoadFontFromImage = function(image: TImage; key: TColor; firstChar: Integer): TFont; cdecl;
  TLoadFontData = function(const fileName: PUTF8Char; fontSize: Integer; fontChars: PInteger; charsCount: Integer; &type: Integer): PCharInfo; cdecl;
  TGenImageFontAtlas = function(const chars: PCharInfo; recs: PPRectangle; charsCount: Integer; fontSize: Integer; padding: Integer; packMethod: Integer): TImage; cdecl;
  TUnloadFont = procedure(font: TFont); cdecl;

// Text drawing functions
  TDrawFPS = procedure(posX: Integer; posY: Integer); cdecl;
  TDrawText = procedure(const text: PUTF8Char; posX: Integer; posY: Integer; fontSize: Integer; color: TColor); cdecl;
  TDrawTextEx = procedure(font: TFont; const text: PUTF8Char; position: TVector2; fontSize: Single; spacing: Single; tint: TColor); cdecl;
  TDrawTextRec = procedure(font: TFont; const text: PUTF8Char; rec: TRectangle; fontSize: Single; spacing: Single; wordWrap: boolean; tint: TColor); cdecl;
  TDrawTextRecEx = procedure(font: TFont; const text: PUTF8Char; rec: TRectangle; fontSize: Single; spacing: Single; wordWrap: boolean; tint: TColor; selectStart: Integer; selectLength: Integer; selectTint: TColor; selectBackTint: TColor); cdecl;

  TDrawTextCodepoint = procedure(font: TFont; codepoint: Integer; position: TVector2; scale: Single; tint: TColor); cdecl;

// Text misc. functions
  TMeasureText = function(const text: PUTF8Char; fontSize: Integer): Integer; cdecl;
  TMeasureTextEx = function(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single): TVector2; cdecl;
  TGetGlyphIndex = function(font: TFont; codepoint: Integer): Integer; cdecl;

// Text strings management functions (no utf8 strings, only byte chars)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!
  TTextCopy = function(dst: PUTF8Char; const src: PUTF8Char): Integer; cdecl;
  TTextIsEqual = function(const text1: PUTF8Char; const text2: PUTF8Char): boolean; cdecl;
  TTextLength = function(const text: PUTF8Char): Cardinal; cdecl;
  TTextFormat = function(const text: PUTF8Char): PUTF8Char varargs; cdecl;
  TTextSubtext = function(const text: PUTF8Char; position: Integer; length: Integer): PUTF8Char; cdecl;
  TTextReplace = function(text: PUTF8Char; const replace: PUTF8Char; const by: PUTF8Char): PUTF8Char; cdecl;
  TTextInsert = function(const text: PUTF8Char; const insert: PUTF8Char; position: Integer): PUTF8Char; cdecl;
  TTextJoin = function(textList: PPUTF8Char; count: Integer; const delimiter: PUTF8Char): PUTF8Char; cdecl;
  TTextSplit = function(const text: PUTF8Char; delimiter: UTF8Char; count: PInteger): PPUTF8Char; cdecl;
  TTextAppend = procedure(text: PUTF8Char; const append: PUTF8Char; position: PInteger); cdecl;
  TTextFindIndex = function(const text: PUTF8Char; const find: PUTF8Char): Integer; cdecl;
  TTextToUpper = function(const text: PUTF8Char): PUTF8Char; cdecl;
  TTextToLower = function(const text: PUTF8Char): PUTF8Char; cdecl;
  TTextToPascal = function(const text: PUTF8Char): PUTF8Char; cdecl;
  TTextToInteger = function(const text: PUTF8Char): Integer; cdecl;
  TTextToUtf8 = function(codepoints: PInteger; length: Integer): PUTF8Char; cdecl;

// UTF8 text strings management functions
  TGetCodepoints = function(const text: PUTF8Char; count: PInteger): PInteger; cdecl;
  TGetCodepointsCount = function(const text: PUTF8Char): Integer; cdecl;
  TGetNextCodepoint = function(const text: PUTF8Char; bytesProcessed: PInteger): Integer; cdecl;
  TCodepointToUtf8 = function(codepoint: Integer; byteLength: PInteger): PUTF8Char; cdecl;

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Basic geometric 3D shapes drawing functions
  TDrawLine3D = procedure(startPos: TVector3; endPos: TVector3; color: TColor); cdecl;
  TDrawPoint3D = procedure(position: TVector3; color: TColor); cdecl;
  TDrawCircle3D = procedure(center: TVector3; radius: Single; rotationAxis: TVector3; rotationAngle: Single; color: TColor); cdecl;
  TDrawCube = procedure(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl;
  TDrawCubeV = procedure(position: TVector3; size: TVector3; color: TColor); cdecl;
  TDrawCubeWires = procedure(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl;
  TDrawCubeWiresV = procedure(position: TVector3; size: TVector3; color: TColor); cdecl;
  TDrawCubeTexture = procedure(texture: TTexture2D; position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl;
  TDrawSphere = procedure(centerPos: TVector3; radius: Single; color: TColor); cdecl;
  TDrawSphereEx = procedure(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl;
  TDrawSphereWires = procedure(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl;
  TDrawCylinder = procedure(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl;
  TDrawCylinderWires = procedure(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl;
  TDrawPlane = procedure(centerPos: TVector3; size: TVector2; color: TColor); cdecl;
  TDrawRay = procedure(ray: TRay; color: TColor); cdecl;
  TDrawGrid = procedure(slices: Integer; spacing: Single); cdecl;
  TDrawGizmo = procedure(position: TVector3); cdecl;
//DrawTorus(), DrawTeapot() could be useful?

//------------------------------------------------------------------------------------
// Model 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Model loading/unloading functions
  TLoadModel = function(const fileName: PUTF8Char): TModel; cdecl;
  TLoadModelFromMesh = function(mesh: TMesh): TModel; cdecl;
  TUnloadModel = procedure(model: TModel); cdecl;

// Mesh loading/unloading functions
  TLoadMeshes = function(const fileName: PUTF8Char; meshCount: PInteger): PMesh; cdecl;
  TExportMesh = procedure(mesh: TMesh; const fileName: PUTF8Char); cdecl;
  TUnloadMesh = procedure(mesh: TMesh); cdecl;

// Material loading/unloading functions
  TLoadMaterials = function(const fileName: PUTF8Char; materialCount: PInteger): PMaterial; cdecl;
  TLoadMaterialDefault = function: TMaterial; cdecl;
  TUnloadMaterial = procedure(material: TMaterial); cdecl;
  TSetMaterialTexture = procedure(material: PMaterial; mapType: Integer; texture: TTexture2D); cdecl;
  TSetModelMeshMaterial = procedure(model: PModel; meshId: Integer; materialId: Integer); cdecl;

// Model animations loading/unloading functions
  TLoadModelAnimations = function(const fileName: PUTF8Char; animsCount: PInteger): PModelAnimation; cdecl;
  TUpdateModelAnimation = procedure(model: TModel; anim: TModelAnimation; frame: Integer); cdecl;
  TUnloadModelAnimation = procedure(anim: TModelAnimation); cdecl;
  TIsModelAnimationValid = function(model: TModel; anim: TModelAnimation): boolean; cdecl;

// Mesh generation functions
  TGenMeshPoly = function(sides: Integer; radius: Single): TMesh; cdecl;
  TGenMeshPlane = function(width: Single; length: Single; resX: Integer; resZ: Integer): TMesh; cdecl;
  TGenMeshCube = function(width: Single; height: Single; length: Single): TMesh; cdecl;
  TGenMeshSphere = function(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl;
  TGenMeshHemiSphere = function(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl;
  TGenMeshCylinder = function(radius: Single; height: Single; slices: Integer): TMesh; cdecl;
  TGenMeshTorus = function(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl;
  TGenMeshKnot = function(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl;
  TGenMeshHeightmap = function(heightmap: TImage; size: TVector3): TMesh; cdecl;
  TGenMeshCubicmap = function(cubicmap: TImage; cubeSize: TVector3): TMesh; cdecl;

// Mesh manipulation functions
  TMeshBoundingBox = function(mesh: TMesh): TBoundingBox; cdecl;
  TMeshTangents = procedure(mesh: PMesh); cdecl;
  TMeshBinormals = procedure(mesh: PMesh); cdecl;

// Model drawing functions
  TDrawModel = procedure(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl;
  TDrawModelEx = procedure(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl;
  TDrawModelWires = procedure(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl;
  TDrawModelWiresEx = procedure(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl;
  TDrawBoundingBox = procedure(box: TBoundingBox; color: TColor); cdecl;
  TDrawBillboard = procedure(camera: TCamera; texture: TTexture2D; center: TVector3; size: Single; tint: TColor); cdecl;
  TDrawBillboardRec = procedure(camera: TCamera; texture: TTexture2D; sourceRec: TRectangle; center: TVector3; size: Single; tint: TColor); cdecl;

// Collision detection functions
  TCheckCollisionSpheres = function(centerA: TVector3; radiusA: Single; centerB: TVector3; radiusB: Single): boolean; cdecl;
  TCheckCollisionBoxes = function(box1: TBoundingBox; box2: TBoundingBox): boolean; cdecl;
  TCheckCollisionBoxSphere = function(box: TBoundingBox; center: TVector3; radius: Single): boolean; cdecl;
  TCheckCollisionRaySphere = function(ray: TRay; center: TVector3; radius: Single): boolean; cdecl;
  TCheckCollisionRaySphereEx = function(ray: TRay; center: TVector3; radius: Single; collisionPoint: PVector3): boolean; cdecl;
  TCheckCollisionRayBox = function(ray: TRay; box: TBoundingBox): boolean; cdecl;
  TGetCollisionRayModel = function(ray: TRay; model: TModel): TRayHitInfo; cdecl;
  TGetCollisionRayTriangle = function(ray: TRay; p1: TVector3; p2: TVector3; p3: TVector3): TRayHitInfo; cdecl;
  TGetCollisionRayGround = function(ray: TRay; groundHeight: Single): TRayHitInfo; cdecl;

//------------------------------------------------------------------------------------
// Shaders System Functions (Module: rlgl)
// NOTE: This functions are useless when using OpenGL 1.1
//------------------------------------------------------------------------------------

// Shader loading/unloading functions
  TLoadShader = function(const vsFileName: PUTF8Char; const fsFileName: PUTF8Char): TShader; cdecl;
  TLoadShaderCode = function(const vsCode: PUTF8Char; const fsCode: PUTF8Char): TShader; cdecl;
  TUnloadShader = procedure(shader: TShader); cdecl;

  TGetShaderDefault = function: TShader; cdecl;
  TGetTextureDefault = function: TTexture2D; cdecl;
  TGetShapesTexture = function: TTexture2D; cdecl;
  TGetShapesTextureRec = function: TRectangle; cdecl;
  TSetShapesTexture = procedure(texture: TTexture2D; source: TRectangle); cdecl;

// Shader configuration functions
  TGetShaderLocation = function(shader: TShader; const uniformName: PUTF8Char): Integer; cdecl;
  TSetShaderValue = procedure(shader: TShader; uniformLoc: Integer; const value: Pointer; uniformType: Integer); cdecl;
  TSetShaderValueV = procedure(shader: TShader; uniformLoc: Integer; const value: Pointer; uniformType: Integer; count: Integer); cdecl;
  TSetShaderValueMatrix = procedure(shader: TShader; uniformLoc: Integer; mat: TMatrix); cdecl;
  TSetShaderValueTexture = procedure(shader: TShader; uniformLoc: Integer; texture: TTexture2D); cdecl;
  TSetMatrixProjection = procedure(proj: TMatrix); cdecl;
  TSetMatrixModelview = procedure(view: TMatrix); cdecl;
  TGetMatrixModelview = function: TMatrix; cdecl;
  TGetMatrixProjection = function: TMatrix; cdecl;

// Texture maps generation (PBR)
// NOTE: Required shaders should be provided
  TGenTextureCubemap = function(shader: TShader; map: TTexture2D; size: Integer): TTexture2D; cdecl;
  TGenTextureIrradiance = function(shader: TShader; cubemap: TTexture2D; size: Integer): TTexture2D; cdecl;
  TGenTexturePrefilter = function(shader: TShader; cubemap: TTexture2D; size: Integer): TTexture2D; cdecl;
  TGenTextureBRDF = function(shader: TShader; size: Integer): TTexture2D; cdecl;

// Shading begin/end functions
  TBeginShaderMode = procedure(shader: TShader); cdecl;
  TEndShaderMode = procedure; cdecl;
  TBeginBlendMode = procedure(mode: Integer); cdecl;
  TEndBlendMode = procedure; cdecl;

// VR control functions
  TInitVrSimulator = procedure; cdecl;
  TCloseVrSimulator = procedure; cdecl;
  TUpdateVrTracking = procedure(camera: PCamera); cdecl;
  TSetVrConfiguration = procedure(info: TVrDeviceInfo; distortion: TShader); cdecl;
  TIsVrSimulatorReady = function: boolean; cdecl;
  TToggleVrMode = procedure; cdecl;
  TBeginVrDrawing = procedure; cdecl;
  TEndVrDrawing = procedure; cdecl;

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------

// Audio device management functions
  TInitAudioDevice = procedure; cdecl;
  TCloseAudioDevice = procedure; cdecl;
  TIsAudioDeviceReady = function: boolean; cdecl;
  TSetMasterVolume = procedure(volume: Single); cdecl;

// Wave/Sound loading/unloading functions
  TLoadWave = function(const fileName: PUTF8Char): TWave; cdecl;
  TLoadSound = function(const fileName: PUTF8Char): TSound; cdecl;
  TLoadSoundFromWave = function(wave: TWave): TSound; cdecl;
  TUpdateSound = procedure(sound: TSound; const data: Pointer; samplesCount: Integer); cdecl;
  TUnloadWave = procedure(wave: TWave); cdecl;
  TUnloadSound = procedure(sound: TSound); cdecl;
  TExportWave = procedure(wave: TWave; const fileName: PUTF8Char); cdecl;
  TExportWaveAsCode = procedure(wave: TWave; const fileName: PUTF8Char); cdecl;

// Wave/Sound management functions
  TPlaySound = procedure(sound: TSound); cdecl;
  TStopSound = procedure(sound: TSound); cdecl;
  TPauseSound = procedure(sound: TSound); cdecl;
  TResumeSound = procedure(sound: TSound); cdecl;
  TPlaySoundMulti = procedure(sound: TSound); cdecl;
  TStopSoundMulti = procedure; cdecl;
  TGetSoundsPlaying = function: Integer; cdecl;
  TIsSoundPlaying = function(sound: TSound): boolean; cdecl;
  TSetSoundVolume = procedure(sound: TSound; volume: Single); cdecl;
  TSetSoundPitch = procedure(sound: TSound; pitch: Single); cdecl;
  TWaveFormat = procedure(wave: PWave; sampleRate: Integer; sampleSize: Integer; channels: Integer); cdecl;
  TWaveCopy = function(wave: TWave): TWave; cdecl;
  TWaveCrop = procedure(wave: PWave; initSample: Integer; finalSample: Integer); cdecl;
  TGetWaveData = function(wave: TWave): PSingle; cdecl;

// Music management functions
  TLoadMusicStream = function(const fileName: PUTF8Char): TMusic; cdecl;
  TUnloadMusicStream = procedure(music: TMusic); cdecl;
  TPlayMusicStream = procedure(music: TMusic); cdecl;
  TUpdateMusicStream = procedure(music: TMusic); cdecl;
  TStopMusicStream = procedure(music: TMusic); cdecl;
  TPauseMusicStream = procedure(music: TMusic); cdecl;
  TResumeMusicStream = procedure(music: TMusic); cdecl;
  TIsMusicPlaying = function(music: TMusic): boolean; cdecl;
  TSetMusicVolume = procedure(music: TMusic; volume: Single); cdecl;
  TSetMusicPitch = procedure(music: TMusic; pitch: Single); cdecl;
  TSetMusicLoopCount = procedure(music: TMusic; count: Integer); cdecl;
  TGetMusicTimeLength = function(music: TMusic): Single; cdecl;
  TGetMusicTimePlayed = function(music: TMusic): Single; cdecl;

// AudioStream management functions
  TInitAudioStream = function(sampleRate: Cardinal; sampleSize: Cardinal; channels: Cardinal): TAudioStream; cdecl;
  TUpdateAudioStream = procedure(stream: TAudioStream; const data: Pointer; samplesCount: Integer); cdecl;
  TCloseAudioStream = procedure(stream: TAudioStream); cdecl;
  TIsAudioStreamProcessed = function(stream: TAudioStream): boolean; cdecl;
  TPlayAudioStream = procedure(stream: TAudioStream); cdecl;
  TPauseAudioStream = procedure(stream: TAudioStream); cdecl;
  TResumeAudioStream = procedure(stream: TAudioStream); cdecl;
  TIsAudioStreamPlaying = function(stream: TAudioStream): boolean; cdecl;
  TStopAudioStream = procedure(stream: TAudioStream); cdecl;
  TSetAudioStreamVolume = procedure(stream: TAudioStream; volume: Single); cdecl;
  TSetAudioStreamPitch = procedure(stream: TAudioStream; pitch: Single); cdecl;
  TSetAudioStreamBufferSizeDefault = procedure(size: Integer); cdecl;

//------------------------------------------------------------------------------------
// Network (Module: network)
//------------------------------------------------------------------------------------

// IN PROGRESS: Check rnet.h for reference

{*
  Load library dynamically, use RayLib.Load to load it
*}

  { TmncRayLib }

  TmncRayLib = class(TmnLibrary)
  protected
    procedure Loaded; override;
  public
    InitWindow: TInitWindow;
    WindowShouldClose: TWindowShouldClose;
    CloseWindow: TCloseWindow;
    IsWindowReady: TIsWindowReady;
    IsWindowMinimized: TIsWindowMinimized;
    IsWindowResized: TIsWindowResized;
    IsWindowHidden: TIsWindowHidden;
    IsWindowFullscreen: TIsWindowFullscreen;
    ToggleFullscreen: TToggleFullscreen;
    UnhideWindow: TUnhideWindow;
    HideWindow: THideWindow;
    SetWindowIcon: TSetWindowIcon;
    SetWindowTitle: TSetWindowTitle;
    SetWindowPosition: TSetWindowPosition;
    SetWindowMonitor: TSetWindowMonitor;
    SetWindowMinSize: TSetWindowMinSize;
    SetWindowSize: TSetWindowSize;
    GetWindowHandle: TGetWindowHandle;
    GetScreenWidth: TGetScreenWidth;
    GetScreenHeight: TGetScreenHeight;
    GetMonitorCount: TGetMonitorCount;
    GetMonitorWidth: TGetMonitorWidth;
    GetMonitorHeight: TGetMonitorHeight;
    GetMonitorPhysicalWidth: TGetMonitorPhysicalWidth;
    GetMonitorPhysicalHeight: TGetMonitorPhysicalHeight;
    GetWindowPosition: TGetWindowPosition;
    GetMonitorName: TGetMonitorName;
    GetClipboardText: TGetClipboardText;
    SetClipboardText: TSetClipboardText;
    ShowCursor: TShowCursor;
    HideCursor: THideCursor;
    IsCursorHidden: TIsCursorHidden;
    EnableCursor: TEnableCursor;
    DisableCursor: TDisableCursor;
    ClearBackground: TClearBackground;
    BeginDrawing: TBeginDrawing;
    EndDrawing: TEndDrawing;
    BeginMode2D: TBeginMode2D;
    EndMode2D: TEndMode2D;
    BeginMode3D: TBeginMode3D;
    EndMode3D: TEndMode3D;
    BeginTextureMode: TBeginTextureMode;
    EndTextureMode: TEndTextureMode;
    BeginScissorMode: TBeginScissorMode;
    EndScissorMode: TEndScissorMode;
    GetMouseRay: TGetMouseRay;
    GetCameraMatrix: TGetCameraMatrix;
    GetCameraMatrix2D: TGetCameraMatrix2D;
    GetWorldToScreen: TGetWorldToScreen;
    GetWorldToScreenEx: TGetWorldToScreenEx;
    GetWorldToScreen2D: TGetWorldToScreen2D;
    GetScreenToWorld2D: TGetScreenToWorld2D;
    SetTargetFPS: TSetTargetFPS;
    GetFPS: TGetFPS;
    GetFrameTime: TGetFrameTime;
    GetTime: TGetTime;
    ColorToInt: TColorToInt;
    ColorNormalize: TColorNormalize;
    ColorFromNormalized: TColorFromNormalized;
    ColorToHSV: TColorToHSV;
    ColorFromHSV: TColorFromHSV;
    GetColor: TGetColor;
    Fade: TFade;
    SetConfigFlags: TSetConfigFlags;
    SetTraceLogLevel: TSetTraceLogLevel;
    SetTraceLogExit: TSetTraceLogExit;
    SetTraceLogCallback: TSetTraceLogCallback;
    TraceLog: TTraceLog;
    TakeScreenshot: TTakeScreenshot;
    GetRandomValue: TGetRandomValue;
    LoadFileData: TLoadFileData;
    SaveFileData: TSaveFileData;
    LoadFileText: TLoadFileText;
    SaveFileText: TSaveFileText;
    FileExists: TFileExists;
    IsFileExtension: TIsFileExtension;
    DirectoryExists: TDirectoryExists;
    GetExtension: TGetExtension;
    GetFileName: TGetFileName;
    GetFileNameWithoutExt: TGetFileNameWithoutExt;
    GetDirectoryPath: TGetDirectoryPath;
    GetPrevDirectoryPath: TGetPrevDirectoryPath;
    GetWorkingDirectory: TGetWorkingDirectory;
    GetDirectoryFiles: TGetDirectoryFiles;
    ClearDirectoryFiles: TClearDirectoryFiles;
    ChangeDirectory: TChangeDirectory;
    IsFileDropped: TIsFileDropped;
    GetDroppedFiles: TGetDroppedFiles;
    ClearDroppedFiles: TClearDroppedFiles;
    GetFileModTime: TGetFileModTime;
    CompressData: TCompressData;
    DecompressData: TDecompressData;
    SaveStorageValue: TSaveStorageValue;
    LoadStorageValue: TLoadStorageValue;
    OpenURL: TOpenURL;
    IsKeyPressed: TIsKeyPressed;
    IsKeyDown: TIsKeyDown;
    IsKeyReleased: TIsKeyReleased;
    IsKeyUp: TIsKeyUp;
    SetExitKey: TSetExitKey;
    GetKeyPressed: TGetKeyPressed;
    IsGamepadAvailable: TIsGamepadAvailable;
    IsGamepadName: TIsGamepadName;
    GetGamepadName: TGetGamepadName;
    IsGamepadButtonPressed: TIsGamepadButtonPressed;
    IsGamepadButtonDown: TIsGamepadButtonDown;
    IsGamepadButtonReleased: TIsGamepadButtonReleased;
    IsGamepadButtonUp: TIsGamepadButtonUp;
    GetGamepadButtonPressed: TGetGamepadButtonPressed;
    GetGamepadAxisCount: TGetGamepadAxisCount;
    GetGamepadAxisMovement: TGetGamepadAxisMovement;
    IsMouseButtonPressed: TIsMouseButtonPressed;
    IsMouseButtonDown: TIsMouseButtonDown;
    IsMouseButtonReleased: TIsMouseButtonReleased;
    IsMouseButtonUp: TIsMouseButtonUp;
    GetMouseX: TGetMouseX;
    GetMouseY: TGetMouseY;
    GetMousePosition: TGetMousePosition;
    SetMousePosition: TSetMousePosition;
    SetMouseOffset: TSetMouseOffset;
    SetMouseScale: TSetMouseScale;
    GetMouseWheelMove: TGetMouseWheelMove;
    GetTouchX: TGetTouchX;
    GetTouchY: TGetTouchY;
    GetTouchPosition: TGetTouchPosition;
    SetGesturesEnabled: TSetGesturesEnabled;
    IsGestureDetected: TIsGestureDetected;
    GetGestureDetected: TGetGestureDetected;
    GetTouchPointsCount: TGetTouchPointsCount;
    GetGestureHoldDuration: TGetGestureHoldDuration;
    GetGestureDragVector: TGetGestureDragVector;
    GetGestureDragAngle: TGetGestureDragAngle;
    GetGesturePinchVector: TGetGesturePinchVector;
    GetGesturePinchAngle: TGetGesturePinchAngle;
    SetCameraMode: TSetCameraMode;
    UpdateCamera: TUpdateCamera;
    SetCameraPanControl: TSetCameraPanControl;
    SetCameraAltControl: TSetCameraAltControl;
    SetCameraSmoothZoomControl: TSetCameraSmoothZoomControl;
    SetCameraMoveControls: TSetCameraMoveControls;
    DrawPixel: TDrawPixel;
    DrawPixelV: TDrawPixelV;
    DrawLine: TDrawLine;
    DrawLineV: TDrawLineV;
    DrawLineEx: TDrawLineEx;
    DrawLineBezier: TDrawLineBezier;
    DrawLineStrip: TDrawLineStrip;
    DrawCircle: TDrawCircle;
    DrawCircleSector: TDrawCircleSector;
    DrawCircleSectorLines: TDrawCircleSectorLines;
    DrawCircleGradient: TDrawCircleGradient;
    DrawCircleV: TDrawCircleV;
    DrawCircleLines: TDrawCircleLines;
    DrawEllipse: TDrawEllipse;
    DrawEllipseLines: TDrawEllipseLines;
    DrawRing: TDrawRing;
    DrawRingLines: TDrawRingLines;
    DrawRectangle: TDrawRectangle;
    DrawRectangleV: TDrawRectangleV;
    DrawRectangleRec: TDrawRectangleRec;
    DrawRectanglePro: TDrawRectanglePro;
    DrawRectangleGradientV: TDrawRectangleGradientV;
    DrawRectangleGradientH: TDrawRectangleGradientH;
    DrawRectangleGradientEx: TDrawRectangleGradientEx;
    DrawRectangleLines: TDrawRectangleLines;
    DrawRectangleLinesEx: TDrawRectangleLinesEx;
    DrawRectangleRounded: TDrawRectangleRounded;
    DrawRectangleRoundedLines: TDrawRectangleRoundedLines;
    DrawTriangle: TDrawTriangle;
    DrawTriangleLines: TDrawTriangleLines;
    DrawTriangleFan: TDrawTriangleFan;
    DrawTriangleStrip: TDrawTriangleStrip;
    DrawPoly: TDrawPoly;
    DrawPolyLines: TDrawPolyLines;
    CheckCollisionRecs: TCheckCollisionRecs;
    CheckCollisionCircles: TCheckCollisionCircles;
    CheckCollisionCircleRec: TCheckCollisionCircleRec;
    GetCollisionRec: TGetCollisionRec;
    CheckCollisionPointRec: TCheckCollisionPointRec;
    CheckCollisionPointCircle: TCheckCollisionPointCircle;
    CheckCollisionPointTriangle: TCheckCollisionPointTriangle;
    LoadImage: TLoadImage;
    LoadImageEx: TLoadImageEx;
    LoadImagePro: TLoadImagePro;
    LoadImageRaw: TLoadImageRaw;
    UnloadImage: TUnloadImage;
    ExportImage: TExportImage;
    ExportImageAsCode: TExportImageAsCode;
    GetImageData: TGetImageData;
    GetImageDataNormalized: TGetImageDataNormalized;
    GenImageColor: TGenImageColor;
    GenImageGradientV: TGenImageGradientV;
    GenImageGradientH: TGenImageGradientH;
    GenImageGradientRadial: TGenImageGradientRadial;
    GenImageChecked: TGenImageChecked;
    GenImageWhiteNoise: TGenImageWhiteNoise;
    GenImagePerlinNoise: TGenImagePerlinNoise;
    GenImageCellular: TGenImageCellular;
    ImageCopy: TImageCopy;
    ImageFromImage: TImageFromImage;
    ImageText: TImageText;
    ImageTextEx: TImageTextEx;
    ImageToPOT: TImageToPOT;
    ImageFormat: TImageFormat;
    ImageAlphaMask: TImageAlphaMask;
    ImageAlphaClear: TImageAlphaClear;
    ImageAlphaCrop: TImageAlphaCrop;
    ImageAlphaPremultiply: TImageAlphaPremultiply;
    ImageCrop: TImageCrop;
    ImageResize: TImageResize;
    ImageResizeNN: TImageResizeNN;
    ImageResizeCanvas: TImageResizeCanvas;
    ImageMipmaps: TImageMipmaps;
    ImageDither: TImageDither;
    ImageFlipVertical: TImageFlipVertical;
    ImageFlipHorizontal: TImageFlipHorizontal;
    ImageRotateCW: TImageRotateCW;
    ImageRotateCCW: TImageRotateCCW;
    ImageColorTint: TImageColorTint;
    ImageColorInvert: TImageColorInvert;
    ImageColorGrayscale: TImageColorGrayscale;
    ImageColorContrast: TImageColorContrast;
    ImageColorBrightness: TImageColorBrightness;
    ImageColorReplace: TImageColorReplace;
    ImageExtractPalette: TImageExtractPalette;
    GetImageAlphaBorder: TGetImageAlphaBorder;
    ImageClearBackground: TImageClearBackground;
    ImageDrawPixel: TImageDrawPixel;
    ImageDrawPixelV: TImageDrawPixelV;
    ImageDrawLine: TImageDrawLine;
    ImageDrawLineV: TImageDrawLineV;
    ImageDrawCircle: TImageDrawCircle;
    ImageDrawCircleV: TImageDrawCircleV;
    ImageDrawRectangle: TImageDrawRectangle;
    ImageDrawRectangleV: TImageDrawRectangleV;
    ImageDrawRectangleRec: TImageDrawRectangleRec;
    ImageDrawRectangleLines: TImageDrawRectangleLines;
    ImageDraw: TImageDraw;
    ImageDrawText: TImageDrawText;
    ImageDrawTextEx: TImageDrawTextEx;
    LoadTexture: TLoadTexture;
    LoadTextureFromImage: TLoadTextureFromImage;
    LoadTextureCubemap: TLoadTextureCubemap;
    LoadRenderTexture: TLoadRenderTexture;
    UnloadTexture: TUnloadTexture;
    UnloadRenderTexture: TUnloadRenderTexture;
    UpdateTexture: TUpdateTexture;
    GetTextureData: TGetTextureData;
    GetScreenData: TGetScreenData;
    GenTextureMipmaps: TGenTextureMipmaps;
    SetTextureFilter: TSetTextureFilter;
    SetTextureWrap: TSetTextureWrap;
    DrawTexture: TDrawTexture;
    DrawTextureV: TDrawTextureV;
    DrawTextureEx: TDrawTextureEx;
    DrawTextureRec: TDrawTextureRec;
    DrawTextureQuad: TDrawTextureQuad;
    DrawTexturePro: TDrawTexturePro;
    DrawTextureNPatch: TDrawTextureNPatch;
    GetPixelDataSize: TGetPixelDataSize;
    GetFontDefault: TGetFontDefault;
    LoadFont: TLoadFont;
    LoadFontEx: TLoadFontEx;
    LoadFontFromImage: TLoadFontFromImage;
    LoadFontData: TLoadFontData;
    GenImageFontAtlas: TGenImageFontAtlas;
    UnloadFont: TUnloadFont;
    DrawFPS: TDrawFPS;
    DrawText: TDrawText;
    DrawTextEx: TDrawTextEx;
    DrawTextRec: TDrawTextRec;
    DrawTextRecEx: TDrawTextRecEx;
    DrawTextCodepoint: TDrawTextCodepoint;
    MeasureText: TMeasureText;
    MeasureTextEx: TMeasureTextEx;
    GetGlyphIndex: TGetGlyphIndex;
    TextCopy: TTextCopy;
    TextIsEqual: TTextIsEqual;
    TextLength: TTextLength;
    TextFormat: TTextFormat;
    TextSubtext: TTextSubtext;
    TextReplace: TTextReplace;
    TextInsert: TTextInsert;
    TextJoin: TTextJoin;
    TextSplit: TTextSplit;
    TextAppend: TTextAppend;
    TextFindIndex: TTextFindIndex;
    TextToUpper: TTextToUpper;
    TextToLower: TTextToLower;
    TextToPascal: TTextToPascal;
    TextToInteger: TTextToInteger;
    TextToUtf8: TTextToUtf8;
    GetCodepoints: TGetCodepoints;
    GetCodepointsCount: TGetCodepointsCount;
    GetNextCodepoint: TGetNextCodepoint;
    CodepointToUtf8: TCodepointToUtf8;
    DrawLine3D: TDrawLine3D;
    DrawPoint3D: TDrawPoint3D;
    DrawCircle3D: TDrawCircle3D;
    DrawCube: TDrawCube;
    DrawCubeV: TDrawCubeV;
    DrawCubeWires: TDrawCubeWires;
    DrawCubeWiresV: TDrawCubeWiresV;
    DrawCubeTexture: TDrawCubeTexture;
    DrawSphere: TDrawSphere;
    DrawSphereEx: TDrawSphereEx;
    DrawSphereWires: TDrawSphereWires;
    DrawCylinder: TDrawCylinder;
    DrawCylinderWires: TDrawCylinderWires;
    DrawPlane: TDrawPlane;
    DrawRay: TDrawRay;
    DrawGrid: TDrawGrid;
    DrawGizmo: TDrawGizmo;
    LoadModel: TLoadModel;
    LoadModelFromMesh: TLoadModelFromMesh;
    UnloadModel: TUnloadModel;
    LoadMeshes: TLoadMeshes;
    ExportMesh: TExportMesh;
    UnloadMesh: TUnloadMesh;
    LoadMaterials: TLoadMaterials;
    LoadMaterialDefault: TLoadMaterialDefault;
    UnloadMaterial: TUnloadMaterial;
    SetMaterialTexture: TSetMaterialTexture;
    SetModelMeshMaterial: TSetModelMeshMaterial;
    LoadModelAnimations: TLoadModelAnimations;
    UpdateModelAnimation: TUpdateModelAnimation;
    UnloadModelAnimation: TUnloadModelAnimation;
    IsModelAnimationValid: TIsModelAnimationValid;
    GenMeshPoly: TGenMeshPoly;
    GenMeshPlane: TGenMeshPlane;
    GenMeshCube: TGenMeshCube;
    GenMeshSphere: TGenMeshSphere;
    GenMeshHemiSphere: TGenMeshHemiSphere;
    GenMeshCylinder: TGenMeshCylinder;
    GenMeshTorus: TGenMeshTorus;
    GenMeshKnot: TGenMeshKnot;
    GenMeshHeightmap: TGenMeshHeightmap;
    GenMeshCubicmap: TGenMeshCubicmap;
    MeshBoundingBox: TMeshBoundingBox;
    MeshTangents: TMeshTangents;
    MeshBinormals: TMeshBinormals;
    DrawModel: TDrawModel;
    DrawModelEx: TDrawModelEx;
    DrawModelWires: TDrawModelWires;
    DrawModelWiresEx: TDrawModelWiresEx;
    DrawBoundingBox: TDrawBoundingBox;
    DrawBillboard: TDrawBillboard;
    DrawBillboardRec: TDrawBillboardRec;
    CheckCollisionSpheres: TCheckCollisionSpheres;
    CheckCollisionBoxes: TCheckCollisionBoxes;
    CheckCollisionBoxSphere: TCheckCollisionBoxSphere;
    CheckCollisionRaySphere: TCheckCollisionRaySphere;
    CheckCollisionRaySphereEx: TCheckCollisionRaySphereEx;
    CheckCollisionRayBox: TCheckCollisionRayBox;
    GetCollisionRayModel: TGetCollisionRayModel;
    GetCollisionRayTriangle: TGetCollisionRayTriangle;
    GetCollisionRayGround: TGetCollisionRayGround;
    LoadShader: TLoadShader;
    LoadShaderCode: TLoadShaderCode;
    UnloadShader: TUnloadShader;
    GetShaderDefault: TGetShaderDefault;
    GetTextureDefault: TGetTextureDefault;
    GetShapesTexture: TGetShapesTexture;
    GetShapesTextureRec: TGetShapesTextureRec;
    SetShapesTexture: TSetShapesTexture;
    GetShaderLocation: TGetShaderLocation;
    SetShaderValue: TSetShaderValue;
    SetShaderValueV: TSetShaderValueV;
    SetShaderValueMatrix: TSetShaderValueMatrix;
    SetShaderValueTexture: TSetShaderValueTexture;
    SetMatrixProjection: TSetMatrixProjection;
    SetMatrixModelview: TSetMatrixModelview;
    GetMatrixModelview: TGetMatrixModelview;
    GetMatrixProjection: TGetMatrixProjection;
    GenTextureCubemap: TGenTextureCubemap;
    GenTextureIrradiance: TGenTextureIrradiance;
    GenTexturePrefilter: TGenTexturePrefilter;
    GenTextureBRDF: TGenTextureBRDF;
    BeginShaderMode: TBeginShaderMode;
    EndShaderMode: TEndShaderMode;
    BeginBlendMode: TBeginBlendMode;
    EndBlendMode: TEndBlendMode;
    InitVrSimulator: TInitVrSimulator;
    CloseVrSimulator: TCloseVrSimulator;
    UpdateVrTracking: TUpdateVrTracking;
    SetVrConfiguration: TSetVrConfiguration;
    IsVrSimulatorReady: TIsVrSimulatorReady;
    ToggleVrMode: TToggleVrMode;
    BeginVrDrawing: TBeginVrDrawing;
    EndVrDrawing: TEndVrDrawing;
    InitAudioDevice: TInitAudioDevice;
    CloseAudioDevice: TCloseAudioDevice;
    IsAudioDeviceReady: TIsAudioDeviceReady;
    SetMasterVolume: TSetMasterVolume;
    LoadWave: TLoadWave;
    LoadSound: TLoadSound;
    LoadSoundFromWave: TLoadSoundFromWave;
    UpdateSound: TUpdateSound;
    UnloadWave: TUnloadWave;
    UnloadSound: TUnloadSound;
    ExportWave: TExportWave;
    ExportWaveAsCode: TExportWaveAsCode;
    PlaySound: TPlaySound;
    StopSound: TStopSound;
    PauseSound: TPauseSound;
    ResumeSound: TResumeSound;
    PlaySoundMulti: TPlaySoundMulti;
    StopSoundMulti: TStopSoundMulti;
    GetSoundsPlaying: TGetSoundsPlaying;
    IsSoundPlaying: TIsSoundPlaying;
    SetSoundVolume: TSetSoundVolume;
    SetSoundPitch: TSetSoundPitch;
    WaveFormat: TWaveFormat;
    WaveCopy: TWaveCopy;
    WaveCrop: TWaveCrop;
    GetWaveData: TGetWaveData;
    LoadMusicStream: TLoadMusicStream;
    UnloadMusicStream: TUnloadMusicStream;
    PlayMusicStream: TPlayMusicStream;
    UpdateMusicStream: TUpdateMusicStream;
    StopMusicStream: TStopMusicStream;
    PauseMusicStream: TPauseMusicStream;
    ResumeMusicStream: TResumeMusicStream;
    IsMusicPlaying: TIsMusicPlaying;
    SetMusicVolume: TSetMusicVolume;
    SetMusicPitch: TSetMusicPitch;
    SetMusicLoopCount: TSetMusicLoopCount;
    GetMusicTimeLength: TGetMusicTimeLength;
    GetMusicTimePlayed: TGetMusicTimePlayed;
    UpdateAudioStream: TUpdateAudioStream;
    CloseAudioStream: TCloseAudioStream;
    IsAudioStreamProcessed: TIsAudioStreamProcessed;
    PlayAudioStream: TPlayAudioStream;
    PauseAudioStream: TPauseAudioStream;
    ResumeAudioStream: TResumeAudioStream;
    IsAudioStreamPlaying: TIsAudioStreamPlaying;
    StopAudioStream: TStopAudioStream;
    SetAudioStreamVolume: TSetAudioStreamVolume;
    SetAudioStreamPitch: TSetAudioStreamPitch;
    SetAudioStreamBufferSizeDefault: TSetAudioStreamBufferSizeDefault;
  end;

var
  RayLib: TmncRayLib = nil;

function Vector2Of(X, Y: Single): TVector2;

implementation

function Vector2Of(X, Y: Single): TVector2;
begin
  Result := TVector2.Create(X, Y);
end;

{ TRectangle }

constructor TRectangle.Create(AX, AY, AWidth, AHeight: Single);
begin
  X:= AX;
  Y:= AY;
  Width := AWidth;
  Height := AHeight;
end;

{ TColor }

constructor TColor.Create(ARed, AGreen, ABlue, AAlpha: Byte);
begin
  Red := ARed;
  Green := AGreen;
  Blue := ABlue;
  Alpha := AAlpha;
end;

{ TVector2 }

constructor TVector2.Create(AX, AY: Single);
begin
  X := AX;
  Y := AY;
end;

{ TmncRayLib }

procedure TmncRayLib.Loaded;
begin
  InitWindow := GetAddress('InitWindow');
  WindowShouldClose := GetAddress('WindowShouldClose');
  CloseWindow := GetAddress('CloseWindow');
  IsWindowReady := GetAddress('IsWindowReady');
  IsWindowMinimized := GetAddress('IsWindowMinimized');
  IsWindowResized := GetAddress('IsWindowResized');
  IsWindowHidden := GetAddress('IsWindowHidden');
  IsWindowFullscreen := GetAddress('IsWindowFullscreen');
  ToggleFullscreen := GetAddress('ToggleFullscreen');
  UnhideWindow := GetAddress('UnhideWindow');
  HideWindow := GetAddress('HideWindow');
  SetWindowIcon := GetAddress('SetWindowIcon');
  SetWindowTitle := GetAddress('SetWindowTitle');
  SetWindowPosition := GetAddress('SetWindowPosition');
  SetWindowMonitor := GetAddress('SetWindowMonitor');
  SetWindowMinSize := GetAddress('SetWindowMinSize');
  SetWindowSize := GetAddress('SetWindowSize');
  GetWindowHandle := GetAddress('GetWindowHandle');
  GetScreenWidth := GetAddress('GetScreenWidth');
  GetScreenHeight := GetAddress('GetScreenHeight');
  GetMonitorCount := GetAddress('GetMonitorCount');
  GetMonitorWidth := GetAddress('GetMonitorWidth');
  GetMonitorHeight := GetAddress('GetMonitorHeight');
  GetMonitorPhysicalWidth := GetAddress('GetMonitorPhysicalWidth');
  GetMonitorPhysicalHeight := GetAddress('GetMonitorPhysicalHeight');
  GetWindowPosition := GetAddress('GetWindowPosition');
  GetMonitorName := GetAddress('GetMonitorName');
  GetClipboardText := GetAddress('GetClipboardText');
  SetClipboardText := GetAddress('SetClipboardText');
  ShowCursor := GetAddress('ShowCursor');
  HideCursor := GetAddress('HideCursor');
  IsCursorHidden := GetAddress('IsCursorHidden');
  EnableCursor := GetAddress('EnableCursor');
  DisableCursor := GetAddress('DisableCursor');
  ClearBackground := GetAddress('ClearBackground');
  BeginDrawing := GetAddress('BeginDrawing');
  EndDrawing := GetAddress('EndDrawing');
  BeginMode2D := GetAddress('BeginMode2D');
  EndMode2D := GetAddress('EndMode2D');
  BeginMode3D := GetAddress('BeginMode3D');
  EndMode3D := GetAddress('EndMode3D');
  BeginTextureMode := GetAddress('BeginTextureMode');
  EndTextureMode := GetAddress('EndTextureMode');
  BeginScissorMode := GetAddress('BeginScissorMode');
  EndScissorMode := GetAddress('EndScissorMode');
  GetMouseRay := GetAddress('GetMouseRay');
  GetCameraMatrix := GetAddress('GetCameraMatrix');
  GetCameraMatrix2D := GetAddress('GetCameraMatrix2D');
  GetWorldToScreen := GetAddress('GetWorldToScreen');
  GetWorldToScreenEx := GetAddress('GetWorldToScreenEx');
  GetWorldToScreen2D := GetAddress('GetWorldToScreen2D');
  GetScreenToWorld2D := GetAddress('GetScreenToWorld2D');
  SetTargetFPS := GetAddress('SetTargetFPS');
  GetFPS := GetAddress('GetFPS');
  GetFrameTime := GetAddress('GetFrameTime');
  GetTime := GetAddress('GetTime');
  ColorToInt := GetAddress('ColorToInt');
  ColorNormalize := GetAddress('ColorNormalize');
  ColorFromNormalized := GetAddress('ColorFromNormalized');
  ColorToHSV := GetAddress('ColorToHSV');
  ColorFromHSV := GetAddress('ColorFromHSV');
  GetColor := GetAddress('GetColor');
  Fade := GetAddress('Fade');
  SetConfigFlags := GetAddress('SetConfigFlags');
  SetTraceLogLevel := GetAddress('SetTraceLogLevel');
  SetTraceLogExit := GetAddress('SetTraceLogExit');
  SetTraceLogCallback := GetAddress('SetTraceLogCallback');
  TraceLog := GetAddress('TraceLog');
  TakeScreenshot := GetAddress('TakeScreenshot');
  GetRandomValue := GetAddress('GetRandomValue');
  LoadFileData := GetAddress('LoadFileData');
  SaveFileData := GetAddress('SaveFileData');
  LoadFileText := GetAddress('LoadFileText');
  SaveFileText := GetAddress('SaveFileText');
  FileExists := GetAddress('FileExists');
  IsFileExtension := GetAddress('IsFileExtension');
  DirectoryExists := GetAddress('DirectoryExists');
  GetExtension := GetAddress('GetExtension');
  GetFileName := GetAddress('GetFileName');
  GetFileNameWithoutExt := GetAddress('GetFileNameWithoutExt');
  GetDirectoryPath := GetAddress('GetDirectoryPath');
  GetPrevDirectoryPath := GetAddress('GetPrevDirectoryPath');
  GetWorkingDirectory := GetAddress('GetWorkingDirectory');
  GetDirectoryFiles := GetAddress('GetDirectoryFiles');
  ClearDirectoryFiles := GetAddress('ClearDirectoryFiles');
  ChangeDirectory := GetAddress('ChangeDirectory');
  IsFileDropped := GetAddress('IsFileDropped');
  GetDroppedFiles := GetAddress('GetDroppedFiles');
  ClearDroppedFiles := GetAddress('ClearDroppedFiles');
  GetFileModTime := GetAddress('GetFileModTime');
  CompressData := GetAddress('CompressData');
  DecompressData := GetAddress('DecompressData');
  SaveStorageValue := GetAddress('SaveStorageValue');
  LoadStorageValue := GetAddress('LoadStorageValue');
  OpenURL := GetAddress('OpenURL');
  IsKeyPressed := GetAddress('IsKeyPressed');
  IsKeyDown := GetAddress('IsKeyDown');
  IsKeyReleased := GetAddress('IsKeyReleased');
  IsKeyUp := GetAddress('IsKeyUp');
  SetExitKey := GetAddress('SetExitKey');
  GetKeyPressed := GetAddress('GetKeyPressed');
  IsGamepadAvailable := GetAddress('IsGamepadAvailable');
  IsGamepadName := GetAddress('IsGamepadName');
  GetGamepadName := GetAddress('GetGamepadName');
  IsGamepadButtonPressed := GetAddress('IsGamepadButtonPressed');
  IsGamepadButtonDown := GetAddress('IsGamepadButtonDown');
  IsGamepadButtonReleased := GetAddress('IsGamepadButtonReleased');
  IsGamepadButtonUp := GetAddress('IsGamepadButtonUp');
  GetGamepadButtonPressed := GetAddress('GetGamepadButtonPressed');
  GetGamepadAxisCount := GetAddress('GetGamepadAxisCount');
  GetGamepadAxisMovement := GetAddress('GetGamepadAxisMovement');
  IsMouseButtonPressed := GetAddress('IsMouseButtonPressed');
  IsMouseButtonDown := GetAddress('IsMouseButtonDown');
  IsMouseButtonReleased := GetAddress('IsMouseButtonReleased');
  IsMouseButtonUp := GetAddress('IsMouseButtonUp');
  GetMouseX := GetAddress('GetMouseX');
  GetMouseY := GetAddress('GetMouseY');
  GetMousePosition := GetAddress('GetMousePosition');
  SetMousePosition := GetAddress('SetMousePosition');
  SetMouseOffset := GetAddress('SetMouseOffset');
  SetMouseScale := GetAddress('SetMouseScale');
  GetMouseWheelMove := GetAddress('GetMouseWheelMove');
  GetTouchX := GetAddress('GetTouchX');
  GetTouchY := GetAddress('GetTouchY');
  GetTouchPosition := GetAddress('GetTouchPosition');
  SetGesturesEnabled := GetAddress('SetGesturesEnabled');
  IsGestureDetected := GetAddress('IsGestureDetected');
  GetGestureDetected := GetAddress('GetGestureDetected');
  GetTouchPointsCount := GetAddress('GetTouchPointsCount');
  GetGestureHoldDuration := GetAddress('GetGestureHoldDuration');
  GetGestureDragVector := GetAddress('GetGestureDragVector');
  GetGestureDragAngle := GetAddress('GetGestureDragAngle');
  GetGesturePinchVector := GetAddress('GetGesturePinchVector');
  GetGesturePinchAngle := GetAddress('GetGesturePinchAngle');
  SetCameraMode := GetAddress('SetCameraMode');
  UpdateCamera := GetAddress('UpdateCamera');
  SetCameraPanControl := GetAddress('SetCameraPanControl');
  SetCameraAltControl := GetAddress('SetCameraAltControl');
  SetCameraSmoothZoomControl := GetAddress('SetCameraSmoothZoomControl');
  SetCameraMoveControls := GetAddress('SetCameraMoveControls');
  DrawPixel := GetAddress('DrawPixel');
  DrawPixelV := GetAddress('DrawPixelV');
  DrawLine := GetAddress('DrawLine');
  DrawLineV := GetAddress('DrawLineV');
  DrawLineEx := GetAddress('DrawLineEx');
  DrawLineBezier := GetAddress('DrawLineBezier');
  DrawLineStrip := GetAddress('DrawLineStrip');
  DrawCircle := GetAddress('DrawCircle');
  DrawCircleSector := GetAddress('DrawCircleSector');
  DrawCircleSectorLines := GetAddress('DrawCircleSectorLines');
  DrawCircleGradient := GetAddress('DrawCircleGradient');
  DrawCircleV := GetAddress('DrawCircleV');
  DrawCircleLines := GetAddress('DrawCircleLines');
  DrawEllipse := GetAddress('DrawEllipse');
  DrawEllipseLines := GetAddress('DrawEllipseLines');
  DrawRing := GetAddress('DrawRing');
  DrawRingLines := GetAddress('DrawRingLines');
  DrawRectangle := GetAddress('DrawRectangle');
  DrawRectangleV := GetAddress('DrawRectangleV');
  DrawRectangleRec := GetAddress('DrawRectangleRec');
  DrawRectanglePro := GetAddress('DrawRectanglePro');
  DrawRectangleGradientV := GetAddress('DrawRectangleGradientV');
  DrawRectangleGradientH := GetAddress('DrawRectangleGradientH');
  DrawRectangleGradientEx := GetAddress('DrawRectangleGradientEx');
  DrawRectangleLines := GetAddress('DrawRectangleLines');
  DrawRectangleLinesEx := GetAddress('DrawRectangleLinesEx');
  DrawRectangleRounded := GetAddress('DrawRectangleRounded');
  DrawRectangleRoundedLines := GetAddress('DrawRectangleRoundedLines');
  DrawTriangle := GetAddress('DrawTriangle');
  DrawTriangleLines := GetAddress('DrawTriangleLines');
  DrawTriangleFan := GetAddress('DrawTriangleFan');
  DrawTriangleStrip := GetAddress('DrawTriangleStrip');
  DrawPoly := GetAddress('DrawPoly');
  DrawPolyLines := GetAddress('DrawPolyLines');
  CheckCollisionRecs := GetAddress('CheckCollisionRecs');
  CheckCollisionCircles := GetAddress('CheckCollisionCircles');
  CheckCollisionCircleRec := GetAddress('CheckCollisionCircleRec');
  GetCollisionRec := GetAddress('GetCollisionRec');
  CheckCollisionPointRec := GetAddress('CheckCollisionPointRec');
  CheckCollisionPointCircle := GetAddress('CheckCollisionPointCircle');
  CheckCollisionPointTriangle := GetAddress('CheckCollisionPointTriangle');
  LoadImage := GetAddress('LoadImage');
  LoadImageEx := GetAddress('LoadImageEx');
  LoadImagePro := GetAddress('LoadImagePro');
  LoadImageRaw := GetAddress('LoadImageRaw');
  UnloadImage := GetAddress('UnloadImage');
  ExportImage := GetAddress('ExportImage');
  ExportImageAsCode := GetAddress('ExportImageAsCode');
  GetImageData := GetAddress('GetImageData');
  GetImageDataNormalized := GetAddress('GetImageDataNormalized');
  GenImageColor := GetAddress('GenImageColor');
  GenImageGradientV := GetAddress('GenImageGradientV');
  GenImageGradientH := GetAddress('GenImageGradientH');
  GenImageGradientRadial := GetAddress('GenImageGradientRadial');
  GenImageChecked := GetAddress('GenImageChecked');
  GenImageWhiteNoise := GetAddress('GenImageWhiteNoise');
  GenImagePerlinNoise := GetAddress('GenImagePerlinNoise');
  GenImageCellular := GetAddress('GenImageCellular');
  ImageCopy := GetAddress('ImageCopy');
  ImageFromImage := GetAddress('ImageFromImage');
  ImageText := GetAddress('ImageText');
  ImageTextEx := GetAddress('ImageTextEx');
  ImageToPOT := GetAddress('ImageToPOT');
  ImageFormat := GetAddress('ImageFormat');
  ImageAlphaMask := GetAddress('ImageAlphaMask');
  ImageAlphaClear := GetAddress('ImageAlphaClear');
  ImageAlphaCrop := GetAddress('ImageAlphaCrop');
  ImageAlphaPremultiply := GetAddress('ImageAlphaPremultiply');
  ImageCrop := GetAddress('ImageCrop');
  ImageResize := GetAddress('ImageResize');
  ImageResizeNN := GetAddress('ImageResizeNN');
  ImageResizeCanvas := GetAddress('ImageResizeCanvas');
  ImageMipmaps := GetAddress('ImageMipmaps');
  ImageDither := GetAddress('ImageDither');
  ImageFlipVertical := GetAddress('ImageFlipVertical');
  ImageFlipHorizontal := GetAddress('ImageFlipHorizontal');
  ImageRotateCW := GetAddress('ImageRotateCW');
  ImageRotateCCW := GetAddress('ImageRotateCCW');
  ImageColorTint := GetAddress('ImageColorTint');
  ImageColorInvert := GetAddress('ImageColorInvert');
  ImageColorGrayscale := GetAddress('ImageColorGrayscale');
  ImageColorContrast := GetAddress('ImageColorContrast');
  ImageColorBrightness := GetAddress('ImageColorBrightness');
  ImageColorReplace := GetAddress('ImageColorReplace');
  ImageExtractPalette := GetAddress('ImageExtractPalette');
  GetImageAlphaBorder := GetAddress('GetImageAlphaBorder');
  ImageClearBackground := GetAddress('ImageClearBackground');
  ImageDrawPixel := GetAddress('ImageDrawPixel');
  ImageDrawPixelV := GetAddress('ImageDrawPixelV');
  ImageDrawLine := GetAddress('ImageDrawLine');
  ImageDrawLineV := GetAddress('ImageDrawLineV');
  ImageDrawCircle := GetAddress('ImageDrawCircle');
  ImageDrawCircleV := GetAddress('ImageDrawCircleV');
  ImageDrawRectangle := GetAddress('ImageDrawRectangle');
  ImageDrawRectangleV := GetAddress('ImageDrawRectangleV');
  ImageDrawRectangleRec := GetAddress('ImageDrawRectangleRec');
  ImageDrawRectangleLines := GetAddress('ImageDrawRectangleLines');
  ImageDraw := GetAddress('ImageDraw');
  ImageDrawText := GetAddress('ImageDrawText');
  ImageDrawTextEx := GetAddress('ImageDrawTextEx');
  LoadTexture := GetAddress('LoadTexture');
  LoadTextureFromImage := GetAddress('LoadTextureFromImage');
  LoadTextureCubemap := GetAddress('LoadTextureCubemap');
  LoadRenderTexture := GetAddress('LoadRenderTexture');
  UnloadTexture := GetAddress('UnloadTexture');
  UnloadRenderTexture := GetAddress('UnloadRenderTexture');
  UpdateTexture := GetAddress('UpdateTexture');
  GetTextureData := GetAddress('GetTextureData');
  GetScreenData := GetAddress('GetScreenData');
  GenTextureMipmaps := GetAddress('GenTextureMipmaps');
  SetTextureFilter := GetAddress('SetTextureFilter');
  SetTextureWrap := GetAddress('SetTextureWrap');
  DrawTexture := GetAddress('DrawTexture');
  DrawTextureV := GetAddress('DrawTextureV');
  DrawTextureEx := GetAddress('DrawTextureEx');
  DrawTextureRec := GetAddress('DrawTextureRec');
  DrawTextureQuad := GetAddress('DrawTextureQuad');
  DrawTexturePro := GetAddress('DrawTexturePro');
  DrawTextureNPatch := GetAddress('DrawTextureNPatch');
  GetPixelDataSize := GetAddress('GetPixelDataSize');
  GetFontDefault := GetAddress('GetFontDefault');
  LoadFont := GetAddress('LoadFont');
  LoadFontEx := GetAddress('LoadFontEx');
  LoadFontFromImage := GetAddress('LoadFontFromImage');
  LoadFontData := GetAddress('LoadFontData');
  GenImageFontAtlas := GetAddress('GenImageFontAtlas');
  UnloadFont := GetAddress('UnloadFont');
  DrawFPS := GetAddress('DrawFPS');
  DrawText := GetAddress('DrawText');
  DrawTextEx := GetAddress('DrawTextEx');
  DrawTextRec := GetAddress('DrawTextRec');
  DrawTextRecEx := GetAddress('DrawTextRecEx');
  DrawTextCodepoint := GetAddress('DrawTextCodepoint');
  MeasureText := GetAddress('MeasureText');
  MeasureTextEx := GetAddress('MeasureTextEx');
  GetGlyphIndex := GetAddress('GetGlyphIndex');
  TextCopy := GetAddress('TextCopy');
  TextIsEqual := GetAddress('TextIsEqual');
  TextLength := GetAddress('TextLength');
  TextFormat := GetAddress('TextFormat');
  TextSubtext := GetAddress('TextSubtext');
  TextReplace := GetAddress('TextReplace');
  TextInsert := GetAddress('TextInsert');
  TextJoin := GetAddress('TextJoin');
  TextSplit := GetAddress('TextSplit');
  TextAppend := GetAddress('TextAppend');
  TextFindIndex := GetAddress('TextFindIndex');
  TextToUpper := GetAddress('TextToUpper');
  TextToLower := GetAddress('TextToLower');
  TextToPascal := GetAddress('TextToPascal');
  TextToInteger := GetAddress('TextToInteger');
  TextToUtf8 := GetAddress('TextToUtf8');
  GetCodepoints := GetAddress('GetCodepoints');
  GetCodepointsCount := GetAddress('GetCodepointsCount');
  GetNextCodepoint := GetAddress('GetNextCodepoint');
  CodepointToUtf8 := GetAddress('CodepointToUtf8');
  DrawLine3D := GetAddress('DrawLine3D');
  DrawPoint3D := GetAddress('DrawPoint3D');
  DrawCircle3D := GetAddress('DrawCircle3D');
  DrawCube := GetAddress('DrawCube');
  DrawCubeV := GetAddress('DrawCubeV');
  DrawCubeWires := GetAddress('DrawCubeWires');
  DrawCubeWiresV := GetAddress('DrawCubeWiresV');
  DrawCubeTexture := GetAddress('DrawCubeTexture');
  DrawSphere := GetAddress('DrawSphere');
  DrawSphereEx := GetAddress('DrawSphereEx');
  DrawSphereWires := GetAddress('DrawSphereWires');
  DrawCylinder := GetAddress('DrawCylinder');
  DrawCylinderWires := GetAddress('DrawCylinderWires');
  DrawPlane := GetAddress('DrawPlane');
  DrawRay := GetAddress('DrawRay');
  DrawGrid := GetAddress('DrawGrid');
  DrawGizmo := GetAddress('DrawGizmo');
  LoadModel := GetAddress('LoadModel');
  LoadModelFromMesh := GetAddress('LoadModelFromMesh');
  UnloadModel := GetAddress('UnloadModel');
  LoadMeshes := GetAddress('LoadMeshes');
  ExportMesh := GetAddress('ExportMesh');
  UnloadMesh := GetAddress('UnloadMesh');
  LoadMaterials := GetAddress('LoadMaterials');
  LoadMaterialDefault := GetAddress('LoadMaterialDefault');
  UnloadMaterial := GetAddress('UnloadMaterial');
  SetMaterialTexture := GetAddress('SetMaterialTexture');
  SetModelMeshMaterial := GetAddress('SetModelMeshMaterial');
  LoadModelAnimations := GetAddress('LoadModelAnimations');
  UpdateModelAnimation := GetAddress('UpdateModelAnimation');
  UnloadModelAnimation := GetAddress('UnloadModelAnimation');
  IsModelAnimationValid := GetAddress('IsModelAnimationValid');
  GenMeshPoly := GetAddress('GenMeshPoly');
  GenMeshPlane := GetAddress('GenMeshPlane');
  GenMeshCube := GetAddress('GenMeshCube');
  GenMeshSphere := GetAddress('GenMeshSphere');
  GenMeshHemiSphere := GetAddress('GenMeshHemiSphere');
  GenMeshCylinder := GetAddress('GenMeshCylinder');
  GenMeshTorus := GetAddress('GenMeshTorus');
  GenMeshKnot := GetAddress('GenMeshKnot');
  GenMeshHeightmap := GetAddress('GenMeshHeightmap');
  GenMeshCubicmap := GetAddress('GenMeshCubicmap');
  MeshBoundingBox := GetAddress('MeshBoundingBox');
  MeshTangents := GetAddress('MeshTangents');
  MeshBinormals := GetAddress('MeshBinormals');
  DrawModel := GetAddress('DrawModel');
  DrawModelEx := GetAddress('DrawModelEx');
  DrawModelWires := GetAddress('DrawModelWires');
  DrawModelWiresEx := GetAddress('DrawModelWiresEx');
  DrawBoundingBox := GetAddress('DrawBoundingBox');
  DrawBillboard := GetAddress('DrawBillboard');
  DrawBillboardRec := GetAddress('DrawBillboardRec');
  CheckCollisionSpheres := GetAddress('CheckCollisionSpheres');
  CheckCollisionBoxes := GetAddress('CheckCollisionBoxes');
  CheckCollisionBoxSphere := GetAddress('CheckCollisionBoxSphere');
  CheckCollisionRaySphere := GetAddress('CheckCollisionRaySphere');
  CheckCollisionRaySphereEx := GetAddress('CheckCollisionRaySphereEx');
  CheckCollisionRayBox := GetAddress('CheckCollisionRayBox');
  GetCollisionRayModel := GetAddress('GetCollisionRayModel');
  GetCollisionRayTriangle := GetAddress('GetCollisionRayTriangle');
  GetCollisionRayGround := GetAddress('GetCollisionRayGround');
  LoadShader := GetAddress('LoadShader');
  LoadShaderCode := GetAddress('LoadShaderCode');
  UnloadShader := GetAddress('UnloadShader');
  GetShaderDefault := GetAddress('GetShaderDefault');
  GetTextureDefault := GetAddress('GetTextureDefault');
  GetShapesTexture := GetAddress('GetShapesTexture');
  GetShapesTextureRec := GetAddress('GetShapesTextureRec');
  SetShapesTexture := GetAddress('SetShapesTexture');
  GetShaderLocation := GetAddress('GetShaderLocation');
  SetShaderValue := GetAddress('SetShaderValue');
  SetShaderValueV := GetAddress('SetShaderValueV');
  SetShaderValueMatrix := GetAddress('SetShaderValueMatrix');
  SetShaderValueTexture := GetAddress('SetShaderValueTexture');
  SetMatrixProjection := GetAddress('SetMatrixProjection');
  SetMatrixModelview := GetAddress('SetMatrixModelview');
  GetMatrixModelview := GetAddress('GetMatrixModelview');
  GetMatrixProjection := GetAddress('GetMatrixProjection');
  GenTextureCubemap := GetAddress('GenTextureCubemap');
  GenTextureIrradiance := GetAddress('GenTextureIrradiance');
  GenTexturePrefilter := GetAddress('GenTexturePrefilter');
  GenTextureBRDF := GetAddress('GenTextureBRDF');
  BeginShaderMode := GetAddress('BeginShaderMode');
  EndShaderMode := GetAddress('EndShaderMode');
  BeginBlendMode := GetAddress('BeginBlendMode');
  EndBlendMode := GetAddress('EndBlendMode');
  InitVrSimulator := GetAddress('InitVrSimulator');
  CloseVrSimulator := GetAddress('CloseVrSimulator');
  UpdateVrTracking := GetAddress('UpdateVrTracking');
  SetVrConfiguration := GetAddress('SetVrConfiguration');
  IsVrSimulatorReady := GetAddress('IsVrSimulatorReady');
  ToggleVrMode := GetAddress('ToggleVrMode');
  BeginVrDrawing := GetAddress('BeginVrDrawing');
  EndVrDrawing := GetAddress('EndVrDrawing');
  InitAudioDevice := GetAddress('InitAudioDevice');
  CloseAudioDevice := GetAddress('CloseAudioDevice');
  IsAudioDeviceReady := GetAddress('IsAudioDeviceReady');
  SetMasterVolume := GetAddress('SetMasterVolume');
  LoadWave := GetAddress('LoadWave');
  LoadSound := GetAddress('LoadSound');
  LoadSoundFromWave := GetAddress('LoadSoundFromWave');
  UpdateSound := GetAddress('UpdateSound');
  UnloadWave := GetAddress('UnloadWave');
  UnloadSound := GetAddress('UnloadSound');
  ExportWave := GetAddress('ExportWave');
  ExportWaveAsCode := GetAddress('ExportWaveAsCode');
  PlaySound := GetAddress('PlaySound');
  StopSound := GetAddress('StopSound');
  PauseSound := GetAddress('PauseSound');
  ResumeSound := GetAddress('ResumeSound');
  PlaySoundMulti := GetAddress('PlaySoundMulti');
  StopSoundMulti := GetAddress('StopSoundMulti');
  GetSoundsPlaying := GetAddress('GetSoundsPlaying');
  IsSoundPlaying := GetAddress('IsSoundPlaying');
  SetSoundVolume := GetAddress('SetSoundVolume');
  SetSoundPitch := GetAddress('SetSoundPitch');
  WaveFormat := GetAddress('WaveFormat');
  WaveCopy := GetAddress('WaveCopy');
  WaveCrop := GetAddress('WaveCrop');
  GetWaveData := GetAddress('GetWaveData');
  LoadMusicStream := GetAddress('LoadMusicStream');
  UnloadMusicStream := GetAddress('UnloadMusicStream');
  PlayMusicStream := GetAddress('PlayMusicStream');
  UpdateMusicStream := GetAddress('UpdateMusicStream');
  StopMusicStream := GetAddress('StopMusicStream');
  PauseMusicStream := GetAddress('PauseMusicStream');
  ResumeMusicStream := GetAddress('ResumeMusicStream');
  IsMusicPlaying := GetAddress('IsMusicPlaying');
  SetMusicVolume := GetAddress('SetMusicVolume');
  SetMusicPitch := GetAddress('SetMusicPitch');
  SetMusicLoopCount := GetAddress('SetMusicLoopCount');
  GetMusicTimeLength := GetAddress('GetMusicTimeLength');
  GetMusicTimePlayed := GetAddress('GetMusicTimePlayed');
  UpdateAudioStream := GetAddress('UpdateAudioStream');
  CloseAudioStream := GetAddress('CloseAudioStream');
  IsAudioStreamProcessed := GetAddress('IsAudioStreamProcessed');
  PlayAudioStream := GetAddress('PlayAudioStream');
  PauseAudioStream := GetAddress('PauseAudioStream');
  ResumeAudioStream := GetAddress('ResumeAudioStream');
  IsAudioStreamPlaying := GetAddress('IsAudioStreamPlaying');
  StopAudioStream := GetAddress('StopAudioStream');
  SetAudioStreamVolume := GetAddress('SetAudioStreamVolume');
  SetAudioStreamPitch := GetAddress('SetAudioStreamPitch');
  SetAudioStreamBufferSizeDefault := GetAddress('SetAudioStreamBufferSizeDefault');
end;

initialization
  RayLib := TmncRayLib.Create('raylib');
finalization
  FreeAndNil(RayLib);
end.

