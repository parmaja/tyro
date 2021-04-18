unit RayLib3;
{$IFDEF FPC}
{$MODE delphi}
{.$modeSwitch advancedrecords}
{.$modeSwitch arrayOperators}
{$ENDIF}
{$M+}{$H+}

{$MINENUMSIZE 4} //All enum must be sized as Integer
{$Z4}
{$A8}

{**
 *  This file is part of Tyro project, ported from C header raylib.h
 *
 * @project   https://www.github.com/parmaja/tyro
 *
 * @license   MIT (https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey zaherdirkey on internet
 *
 * This library not really tested as it, I just used some functions, please be patient and report any but you find.
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
* TODO:
*       Change to use enum types inside functions
*}
uses
  Types, Classes, SysUtils,
  mnLibraries; // take it from github/parmaja/minilib

type
  PPUTF8Char = ^PUTF8Char;

  { TRGBAColor }

  TRGBAColor = record
    Red, Green, Blue, Alpha: Byte;
  end;

  { TColor }

  //AColor := TColor.Create($010203FF);
  TColor = record
    class operator Implicit(a: Cardinal): TColor;
    class operator Implicit(a: TRGBAColor): TColor;

    class operator Explicit(a: TRGBAColor): TColor;
    class operator Explicit(a: Cardinal): TColor;

    class operator Equal(a, b: TColor) : Boolean;

    case Cardinal of
      0: (Value: Cardinal);
      1: (RGBA: TRGBAColor);
  end;

  PColor = ^TColor;

  { TColorHelper }

  TColorHelper = record helper for TColor
    function Alpha(AAlpha: Byte): TColor;
    constructor Create(ARed, AGreen, ABlue, AAlpha: Byte);overload;
    constructor Create(RGBAColor: TRGBAColor); overload;
    constructor Create(AValue: Cardinal); overload;
    constructor CreateRGBA(RGBA: Cardinal); overload; //stupid idea, but ok :P
  end;

const
  Lightgray: TRGBAColor = (Red: 200; Green: 200; Blue: 200; Alpha: 255);   // Light Gray
  Gray:      TRGBAColor = (Red: 130; Green: 130; Blue: 130; Alpha: 255);   // Gray
  DarkGray:  TRGBAColor = (Red: 80; Green: 80; Blue: 80; Alpha: 255);      // Dark Gray
  Yellow:    TRGBAColor = (Red: 253; Green: 249; Blue: 0; Alpha: 255);     // Yellow
  Gold:      TRGBAColor = (Red: 255; Green: 203; Blue: 0; Alpha: 255);     // Gold
  Orange:    TRGBAColor = (Red: 255; Green: 161; Blue: 0; Alpha: 255);     // Orange
  Pink:      TRGBAColor = (Red: 255; Green: 109; Blue: 194; Alpha: 255);   // Pink
  Red:       TRGBAColor = (Red: 230; Green: 41; Blue: 55; Alpha: 255);     // Red
  Maroon:    TRGBAColor = (Red: 190; Green: 33; Blue: 55; Alpha: 255);     // Maroon
  Green:     TRGBAColor = (Red: 0; Green: 228; Blue: 48; Alpha: 255);      // Green
  Lime:      TRGBAColor = (Red: 0; Green: 158; Blue: 47; Alpha: 255);      // Lime
  Darkgreen: TRGBAColor = (Red: 0; Green: 117; Blue: 44; Alpha: 255);      // Dark Green
  SkyBlue:   TRGBAColor = (Red: 102; Green: 191; Blue: 255; Alpha: 255);   // Sky Blue
  Blue:      TRGBAColor = (Red: 0; Green: 121; Blue: 241; Alpha: 255);     // Blue
  Darkblue:  TRGBAColor = (Red: 0; Green: 82; Blue: 172; Alpha: 255);      // Dark Blue
  Purple:    TRGBAColor = (Red: 200; Green: 122; Blue: 255; Alpha: 255);   // Purple
  Violet:    TRGBAColor = (Red: 135; Green: 60; Blue: 190; Alpha: 255);    // Violet
  Darkpurple:TRGBAColor = (Red: 112; Green: 31; Blue: 126; Alpha: 255);    // Dark Purple
  Beige:     TRGBAColor = (Red: 211; Green: 176; Blue: 131; Alpha: 255);   // Beige
  Brown:     TRGBAColor = (Red: 127; Green: 106; Blue: 79; Alpha: 255);    // Brown
  Darkbrown: TRGBAColor = (Red: 76; Green: 63; Blue: 47; Alpha: 255);      // Dark Brown

  White:     TRGBAColor = (Red: 255; Green: 255; Blue: 255; Alpha: 255);   // White
  Black:     TRGBAColor = (Red: 0; Green: 0; Blue: 0; Alpha: 255);         // Black
  Blank:     TRGBAColor = (Red: 0; Green: 0; Blue: 0; Alpha: 0);           // Blank (Transparent)
  Magenta:   TRGBAColor = (Red: 255; Green: 0; Blue: 255; Alpha: 255);     // Magenta
  RayWhite:  TRGBAColor = (Red: 245; Green: 245; Blue: 245; Alpha: 255);   // My own White (raylib logo)

type
  // Vector2 type

  { TVector2 }

  TVector2 = packed record
    X: Single;
    Y: Single;
    constructor Create(AX, AY: Single); overload;
    constructor Create(I: Int64); overload;
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

  // Texture type
  // NOTE: Data stored in GPU memory
  TTexture = packed record
    ID: Cardinal;      // OpenGL texture id
    Width: Integer;    // Texture base width
    Height: Integer;   // Texture base height
    Mipmaps: Integer;  // Mipmap levels, 1 by default
    Format: Integer;   // Data format (PixelFormat type)
  end;
  PTexture = ^TTexture;

  // Texture type, same as Texture2D
  TTexture2D = TTexture;
  PTexture2D = ^PTexture;

  // TextureCubemap type, actually, same as Texture2D
  TTextureCubemap = TTexture;
  PTextureCubemap = ^PTexture;

  // RenderTexture type, for texture rendering
  // RenderTexture type, for texture rendering
  TRenderTexture = packed record
    ID: Cardinal;          // OpenGL Framebuffer Object (FBO) id
    Texture: TTexture;   // Color buffer attachment texture
    Depth: TTexture;     // Depth buffer attachment texture
  end;
  PRenderTexture = ^TRenderTexture;

  // RenderTexture type, same as RenderTexture2D
  TRenderTexture2D = TRenderTexture;
  PRenderTexture2D = ^TRenderTexture;

  // N-Patch layout info
  TNPatchInfo = packed record
    Source: TRectangle; // Region in the texture
    Left: Integer;         // left border offset
    Top: Integer;          // top border offset
    Right: Integer;        // right border offset
    Bottom: Integer;       // bottom border offset
    AType: Integer;        // layout of the n-patch: 3x3, 1x3 or 3x1
  end;
  PNPatchInfo = ^TNPatchInfo;

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
    CharsPadding: Integer;
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
    Transform: TMatrix;        // Local transform matrix

    MeshCount: Integer;        // Number of meshes
    MaterialCount: Integer;    // Number of materials
    Meshes: PMesh;             // Meshes array

    Materials: PMaterial;      // Materials array
    MeshMaterial: PInteger;    // Mesh material number

    // Animation data
    BoneCount: Integer;        // Number of bones
    Bones: PBoneInfo;          // Bones information (skeleton)
    BindPose: PTransform;      // Bones base transformation (pose)
  end;
  PModel = ^TModel;

  // Model animation
  TModelAnimation = packed record
    BoneCount: Integer;        // Number of bones
    FrameCount: Integer;       // Number of animation frames
    BoneInfo: PBoneInfo;       // Bones information (skeleton)

    FramePoses: PPTransform;   // Poses array by frame
  end;
  PModelAnimation = ^TModelAnimation;

  // Ray type (useful for raycast)
  TRay = packed record
    Position: TVector3;        // Ray position (origin)
    Direction: TVector3;       // Ray direction
  end;
  PRay = ^TRay;

  // Raycast hit information
  TRayHitInfo = packed record
    Hit: Boolean;              // Did the ray hit something?
    Distance: Single;          // Distance to nearest hit
    position: TVector3;        // Position of nearest hit
    normal: TVector3;          // Surface normal of hit
  end;
  PRayHitInfo = ^TRayHitInfo;

  // Bounding box type
  TBoundingBox = packed record
    Min: TVector3;             // Minimum vertex box-corner
    Max: TVector3;             // Maximum vertex box-corner
  end;
  PBoundingBox = ^TBoundingBox;

  // Wave type, defines audio wave data
  TWave = packed record
    SampleCount: Cardinal;     // Total number of samples
    SampleRate: Cardinal;      // Frequency (samples per second)
    SampleSize: Cardinal;      // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    Channels: Cardinal;        // Number of channels (1-mono, 2-stereo)
    Data: Pointer;             // Buffer data pointer
  end;
  PWave = ^TWave;

  //
  TrAudioBuffer = packed record
  end;
  PrAudioBuffer = ^TrAudioBuffer;

  // Audio stream type
  // NOTE: Useful to create custom audio streams not bound to a specific file
  TAudioStream = packed record
    Buffer: PrAudioBuffer;     // Pointer to internal data used by the audio system

    SampleRate: Cardinal;       // Frequency (samples per second)
    SampleSize: Cardinal;       // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    Channels: Cardinal;         // Number of channels (1-mono, 2-stereo)
  end;
  PAudioStream = ^TAudioStream;

  // Sound source type
  TSound = packed record
    Stream: TAudioStream;       // Audio stream
    SampleCount: Cardinal;     // Total number of samples
  end;
  PSound = ^TSound;

  // Music stream type (audio file streaming from memory)
  // NOTE: Anything longer than ~10 seconds should be streamed
  TMusic = packed record
    Stream: TAudioStream;      // Audio stream
    SampleCount: Cardinal;     // Total number of samples
    Looping: Boolean;           // Music looping enable

    CtxType: Integer;          // Type of music context (audio filetype)
    CtxData: Pointer;          // Audio context data, depends on type
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
  // System/Window config flags
  // NOTE: Every bit registers one state (use it with bit masks)
  // By default all flags are set to 0
  TConfigFlag = (
    FLAG_FULLSCREEN_MODE    = $00000002,   // Set to run program in fullscreen
    FLAG_WINDOW_RESIZABLE   = $00000004,   // Set to allow resizable window
    FLAG_WINDOW_UNDECORATED = $00000008,   // Set to disable window decoration (frame and buttons)
    FLAG_WINDOW_TRANSPARENT = $00000010,   // Set to allow transparent framebuffer
    FLAG_MSAA_4X_HINT       = $00000020,   // Set to try enabling MSAA 4X
    FLAG_VSYNC_HINT         = $00000040,   // Set to try enabling V-Sync on GPU
    FLAG_WINDOW_HIDDEN      = $00000080,   // Set to hide window
    FLAG_WINDOW_ALWAYS_RUN  = $00000100,   // Set to allow windows running while minimized
    FLAG_WINDOW_MINIMIZED   = $00000200,   // Set to minimize window (iconify)
    FLAG_WINDOW_MAXIMIZED   = $00000400,   // Set to maximize window (expanded to monitor)
    FLAG_WINDOW_UNFOCUSED   = $00000800,   // Set to window non focused
    FLAG_WINDOW_TOPMOST     = $00001000,   // Set to window always on top
    FLAG_WINDOW_HIGHDPI     = $00002000,   // Set to support HighDPI
    FLAG_INTERLACED_HINT    = $00010000    // Set to try enabling interlaced video format (for V3D)
  );

  // Trace log type
  TTraceLogType = (
    LOG_ALL = 0,        // Display all logs
    LOG_TRACE,
    LOG_DEBUG,
    LOG_INFO,
    LOG_WARNING,
    LOG_ERROR,
    LOG_FATAL,
    LOG_NONE            // Disable logging
  );
  TTraceLogTypes = set of TTraceLogType;

const
  // Keyboard keys (US keyboard layout)
  // NOTE: Use GetKeyPressed() to allow redefining
  // required keys for alternative layouts
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
    KEY_VOLUME_UP       = 24,
    KEY_VOLUME_DOWN     = 25,
    KEY_MENU            = 82
  );

  // Mouse buttons
  TMouseButton = (
    MOUSE_LEFT_BUTTON   = 0,
    MOUSE_RIGHT_BUTTON  = 1,
    MOUSE_MIDDLE_BUTTON = 2
  );

  // Mouse cursor types
  TMouseCursor = (
    MOUSE_CURSOR_DEFAULT       = 0,
    MOUSE_CURSOR_ARROW         = 1,
    MOUSE_CURSOR_IBEAM         = 2,
    MOUSE_CURSOR_CROSSHAIR     = 3,
    MOUSE_CURSOR_POINTING_HAND = 4,
    MOUSE_CURSOR_RESIZE_EW     = 5,     // The horizontal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NS     = 6,     // The vertical resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NWSE   = 7,     // The top-left to bottom-right diagonal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NESW   = 8,     // The top-right to bottom-left diagonal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_ALL    = 9,     // The omni-directional resize/move cursor shape
    MOUSE_CURSOR_NOT_ALLOWED   = 10     // The operation-not-allowed shape
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

  // Gamepad axis
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

  // Shader location point
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

  // Material map
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

  // Texture parameters: wrap mode
  TTextureWrapMode = (
    WRAP_REPEAT = 0,        // Repeats texture in tiled mode
    WRAP_CLAMP,             // Clamps texture to edge pixel in tiled mode
    WRAP_MIRROR_REPEAT,     // Mirrors and repeats the texture in tiled mode
    WRAP_MIRROR_CLAMP       // Mirrors and clamps to border the texture in tiled mode
  );

  // Cubemap layout
  TCubemapLayoutType = (
    CUBEMAP_AUTO_DETECT = 0,        // Automatically detect layout type
    CUBEMAP_LINE_VERTICAL,          // Layout is defined by a vertical line with faces
    CUBEMAP_LINE_HORIZONTAL,        // Layout is defined by an horizontal line with faces
    CUBEMAP_CROSS_THREE_BY_FOUR,    // Layout is defined by a 3x4 cross with cubemap faces
    CUBEMAP_CROSS_FOUR_BY_THREE,    // Layout is defined by a 4x3 cross with cubemap faces
    CUBEMAP_PANORAMA                // Layout is defined by a panorama image (equirectangular map)
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
    BLEND_MULTIPLIED,        // Blend textures multiplying colors
    BLEND_ADD_COLORS,       // Blend textures adding colors (alternative)
    BLEND_SUBTRACT_COLORS,  // Blend textures subtracting colors (alternative)
    BLEND_CUSTOM            // Belnd textures using custom src/dst factors (use SetBlendModeCustom())
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

  // N-patch types
  TNPatchType = (
    NPT_9PATCH = 0,         // Npatch defined by 3x3 tiles
    NPT_3PATCH_VERTICAL,    // Npatch defined by 1x3 tiles
    NPT_3PATCH_HORIZONTAL   // Npatch defined by 3x1 tiles
  );

  // Callbacks to be implemented by users
type
  TTraceLogCallback = procedure(LogType: Integer; Text: PAnsiChar; Args: Pointer); cdecl; //TODO check Args

//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...

//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

var
  { Window-related functions }

  // Initialize window and OpenGL context
  InitWindow: procedure(Width: Integer; Height: Integer; const Title: PUTF8Char); cdecl;
  // Check if KEY_ESCAPE pressed or Close icon pressed
  WindowShouldClose: function: Boolean; cdecl;
  // Close window and unload OpenGL context
  CloseWindow: procedure; cdecl;
  // Check if window has been initialized successfully
  IsWindowReady: function: Boolean; cdecl;
  // Check if window is currently fullscreen
  IsWindowFullscreen: function: Boolean; cdecl;
  // Check if window is currently hidden (only PLATFORM_DESKTOP)
  IsWindowHidden: function: Boolean; cdecl;
  // Check if window has been minimized (or lost focus)
  IsWindowMinimized: function: Boolean; cdecl;
  // Check if window is currently maximized (only PLATFORM_DESKTOP)
  IsWindowMaximized: function: Boolean; cdecl;
  // Check if window has been resized
  IsWindowResized: function: Boolean; cdecl;
  // Check if one specific window flag is enabled
  IsWindowState: function(flag: Cardinal): Boolean; cdecl;
  // Set window configuration state using flags
  SetWindowState: procedure(flag: Cardinal); cdecl;
  // Clear window configuration state flags
  ClearWindowState: procedure(flag: Cardinal); cdecl;
  // Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
  ToggleFullscreen: procedure; cdecl;
  // Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
  MaximizeWindow: procedure; cdecl;
  // Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
  MinimizeWindow: procedure; cdecl;
  // Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
  RestoreWindow : procedure; cdecl;
  // Set icon for window (only PLATFORM_DESKTOP)
  SetWindowIcon: procedure(image: TImage); cdecl;
  // Set title for window (only PLATFORM_DESKTOP)
  SetWindowTitle: procedure(const title: PUTF8Char); cdecl;
  // Set window position on screen (only PLATFORM_DESKTOP)
  SetWindowPosition: procedure(x: Integer; y: Integer); cdecl;
  // Set monitor for the current window (fullscreen mode)
  SetWindowMonitor: procedure(monitor: Integer); cdecl;
  // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
  SetWindowMinSize: procedure(width: Integer; height: Integer); cdecl;
  // Set window dimensions
  SetWindowSize: procedure(width: Integer; height: Integer); cdecl;
  // Get native window handle
  GetWindowHandle: function: Pointer; cdecl;
  // Get current screen width
  GetScreenWidth: function: Integer; cdecl;
  // Get current screen height
  GetScreenHeight: function: Integer; cdecl;
  // Get number of connected monitors
  GetMonitorCount: function: Integer; cdecl;
  // Get specified monitor position
  GetMonitorPosition: function(monitor: Integer): TVector2; cdecl;
  // Get primary monitor width
  GetMonitorWidth: function(monitor: Integer): Integer; cdecl;
  // Get primary monitor height
  GetMonitorHeight: function(monitor: Integer): Integer; cdecl;
  // Get primary monitor physical width in millimetres
  GetMonitorPhysicalWidth: function(monitor: Integer): Integer; cdecl;
  // Get primary monitor physical height in millimetres
  GetMonitorPhysicalHeight: function(monitor: Integer): Integer; cdecl;
  // Get specified monitor refresh rate
  GetMonitorRefreshRate: function(monitor: Integer): Integer; cdecl;
  // Get window position XY on monitor
  GetWindowPosition: function(monitor: Integer): TVector2; cdecl;
  // Get window scale DPI factor
  GetWindowScaleDPI: function(monitor: Integer): TVector2; cdecl;
  // Get the human-readable, UTF-8 encoded name of the primary monitor
  GetMonitorName: function(monitor: Integer): PUTF8Char; cdecl;
  // Set clipboard text content
  SetClipboardText: procedure(const text: PUTF8Char); cdecl;
  // Get clipboard text content
  GetClipboardText: function: PUTF8Char; cdecl;

  { Cursor-related functions }

  // Shows cursor
  ShowCursor: procedure; cdecl;
  // Hides cursor
  HideCursor: procedure; cdecl;
  // Check if cursor is not visible
  IsCursorHidden: function: Boolean; cdecl;
  // Enables cursor (unlock cursor)
  EnableCursor: procedure; cdecl;
  // Disables cursor (lock cursor)
  DisableCursor: procedure; cdecl;
  // Check if cursor is on the current screen.
  IsCursorOnScreen: function: Boolean; cdecl;

  { Drawing-related functions }

  // Set background color (framebuffer clear color)
  ClearBackground: procedure(color: TColor); cdecl;
  // Setup canvas (framebuffer) to start drawing
  BeginDrawing: procedure; cdecl;
  // End canvas drawing and swap buffers (double buffering)
  EndDrawing: procedure; cdecl;
  // Initialize 2D mode with custom camera (2D)
  BeginMode2D: procedure(camera: TCamera2D); cdecl;
  // Ends 2D mode with custom camera
  EndMode2D: procedure; cdecl;
  // Initializes 3D mode with custom camera (3D)
  BeginMode3D: procedure(camera: TCamera3D); cdecl;
  // Ends 3D mode and returns to default 2D orthographic mode
  EndMode3D: procedure; cdecl;
  // Initializes render texture for drawing
  BeginTextureMode: procedure(target: TRenderTexture2D); cdecl;
  // Ends drawing to render texture
  EndTextureMode: procedure; cdecl;
  // Begin scissor mode (define screen area for following drawing)
  BeginScissorMode: procedure(x: Integer; y: Integer; width: Integer; height: Integer); cdecl;
  // End scissor mode
  EndScissorMode: procedure; cdecl;

  { Screen-space-related functions }

  // Returns a ray trace from mouse position
  GetMouseRay: function(mousePosition: TVector2; camera: TCamera): TRay; cdecl;
  // Returns camera transform matrix (view matrix)
  GetCameraMatrix: function(camera: TCamera): TMatrix; cdecl;
  // Returns camera 2d transform matrix
  GetCameraMatrix2D: function(camera: TCamera2D): TMatrix; cdecl;
  // Returns the screen space position for a 3d world space position
  GetWorldToScreen: function(position: TVector3; camera: TCamera): TVector2; cdecl;
  // Returns size position for a 3d world space position
  GetWorldToScreenEx: function(position: TVector3; camera: TCamera; width: Integer; height: Integer): TVector2; cdecl;
  // Returns the screen space position for a 2d camera world space position
  GetWorldToScreen2D: function(position: TVector2; camera: TCamera2D): TVector2; cdecl;
  // Returns the world space position for a 2d camera screen space position
  GetScreenToWorld2D: function(position: TVector2; camera: TCamera2D): TVector2; cdecl;

  { Timing-related functions }

  // Set target FPS (maximum)
  SetTargetFPS: procedure(fps: Integer); cdecl;
  // Returns current FPS
  GetFPS: function: Integer; cdecl;
  // Returns time in seconds for last frame drawn
  GetFrameTime: function: Single; cdecl;
  // Returns elapsed time in seconds since InitWindow()
  GetTime: function: Double; cdecl;

  { Misc. functions }

  // Setup window configuration flags (view FLAGS)
  SetConfigFlags: procedure(flags: Cardinal); cdecl;
  // Set the current threshold (minimum) log level
  SetTraceLogLevel: procedure(logType: TTraceLogTypes); cdecl;
  // Set the exit threshold (minimum) log level
  SetTraceLogExit: procedure(logType: Integer); cdecl;
  // Set a trace log callback to enable custom logging
  SetTraceLogCallback: procedure(callback: TTraceLogCallback); cdecl;
  // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)
  TraceLog: procedure(logType: Integer; const text: PUTF8Char) varargs; cdecl;

  // Internal memory allocator
  MemAlloc: function(Size: Integer): Pointer; cdecl;
  // Internal memory free
  MemFree: procedure(ptr: Pointer); cdecl;
  // Takes a screenshot of current screen (saved a .png)
  TakeScreenshot: procedure(const fileName: PUTF8Char); cdecl;
  // Returns a random value between min and max (both included)
  GetRandomValue: function(min: Integer; max: Integer): Integer; cdecl;

  { Files management functions }

  // Load file data as byte array (read)
  LoadFileData: function(const fileName: PUTF8Char; bytesRead: PCardinal): PByte; cdecl;
  // Unload file data allocated by LoadFileData()
  UnloadFileData: procedure(Data: PByte); cdecl;
  // Save data to file from byte array (write)
  SaveFileData: procedure(const fileName: PUTF8Char; data: Pointer; bytesToWrite: Cardinal); cdecl;
  // Load text data from file (read), returns a '\0' terminated string
  LoadFileText: function(const fileName: PUTF8Char): PUTF8Char; cdecl;
  // Unload file text data allocated by LoadFileText()
  UnloadFileText: procedure(Text: PUTF8Char); cdecl;
  // Save text data to file (write), string must be '\0' terminated
  SaveFileText: procedure(const fileName: PUTF8Char; text: PUTF8Char); cdecl;
  // Check if file exists
  FileExists: function(const fileName: PUTF8Char): Boolean; cdecl;
  // Check if a directory path exists
  DirectoryExists: function(const dirPath: PUTF8Char): Boolean; cdecl;
  // Check file extension
  IsFileExtension: function(const fileName: PUTF8Char; const ext: PUTF8Char): Boolean; cdecl;
  // Get pointer to extension for a filename string
  GetFileExtension: function(const fileName: PUTF8Char): PUTF8Char; cdecl;
  // Get pointer to filename for a path string
  GetFileName: function(const filePath: PUTF8Char): PUTF8Char; cdecl;
  // Get filename string without extension (uses static string)
  GetFileNameWithoutExt: function(const filePath: PUTF8Char): PUTF8Char; cdecl;
  // Get full path for a given fileName with path (uses static string)
  GetDirectoryPath: function(const filePath: PUTF8Char): PUTF8Char; cdecl;
  // Get previous directory path for a given path (uses static string)
  GetPrevDirectoryPath: function(const dirPath: PUTF8Char): PUTF8Char; cdecl;
  // Get current working directory (uses static string)
  GetWorkingDirectory: function: PUTF8Char; cdecl;
  // Get filenames in a directory path (memory should be freed)
  GetDirectoryFiles: function(const dirPath: PUTF8Char; count: PInteger): PPUTF8Char; cdecl;
  // Clear directory files paths buffers (free memory)
  ClearDirectoryFiles: procedure; cdecl;
  // Change working directory, returns true if success
  ChangeDirectory: function(const dir: PUTF8Char): Boolean; cdecl;
  // Check if a file has been dropped into window
  IsFileDropped: function: Boolean; cdecl;
  // Get dropped files names (memory should be freed)
  GetDroppedFiles: function(count: PInteger): PPUTF8Char; cdecl;
  // Clear dropped files paths buffer (free memory)
  ClearDroppedFiles: procedure; cdecl;
  // Get file modification time (last write time)
  GetFileModTime: function(const fileName: PUTF8Char): Integer; cdecl;

  // Compress data (DEFLATE algorythm)
  CompressData: function(data: PByte; dataLength: Integer; compDataLength: PInteger): PByte; cdecl;
  // Decompress data (DEFLATE algorythm)
  DecompressData: function(compData: PByte; compDataLength: Integer; dataLength: PInteger): PByte; cdecl;

  { Persistent storage management }

  // Save integer value to storage file (to defined position)
  SaveStorageValue: function(position: Cardinal; value: Integer): Boolean; cdecl;
  // Load integer value from storage file (from defined position)
  LoadStorageValue: function(position: Cardinal): Integer; cdecl;

  // Open URL with default system browser (if available)
  OpenURL: procedure(const url: PUTF8Char); cdecl;

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

  { Input-related functions: keyboard }

  // Detect if a key has been pressed once
  IsKeyPressed: function(key: Integer): Boolean; cdecl;
  // Detect if a key is being pressed
  IsKeyDown: function(key: Integer): Boolean; cdecl;
  // Detect if a key has been released once
  IsKeyReleased: function(key: Integer): Boolean; cdecl;

  { Input-related functions: gamepads }

  // Detect if a key is NOT being pressed
  IsKeyUp: function(key: Integer): Boolean; cdecl;
  // Set a custom key to exit program (default is ESC)
  SetExitKey: procedure(key: Integer); cdecl;
  // Get key pressed, call it multiple times for chars queued
  GetKeyPressed: function: Integer; cdecl;
  // Get char pressed (unicode), call it multiple times for chars queued
  GetCharPressed: function: Integer; cdecl;

  // Input-related functions: gamepads

  // Detect if a gamepad is available
  IsGamepadAvailable: function(gamepad: Integer): Boolean; cdecl;
  // Check gamepad name (if available)
  IsGamepadName: function(gamepad: Integer; const name: PUTF8Char): Boolean; cdecl;
  // Return gamepad internal name id
  GetGamepadName: function(gamepad: Integer): PUTF8Char; cdecl;
  // Detect if a gamepad button has been pressed once
  IsGamepadButtonPressed: function(gamepad: Integer; button: Integer): Boolean; cdecl;
  // Detect if a gamepad button is being pressed
  IsGamepadButtonDown: function(gamepad: Integer; button: Integer): Boolean; cdecl;
  // Detect if a gamepad button has been released once
  IsGamepadButtonReleased: function(gamepad: Integer; button: Integer): Boolean; cdecl;
  // Detect if a gamepad button is NOT being pressed
  IsGamepadButtonUp: function(gamepad: Integer; button: Integer): Boolean; cdecl;
  // Get the last gamepad button pressed
  GetGamepadButtonPressed: function: Integer; cdecl;
  // Return gamepad axis count for a gamepad
  GetGamepadAxisCount: function(gamepad: Integer): Integer; cdecl;
  // Return axis movement value for a gamepad axis
  GetGamepadAxisMovement: function(gamepad: Integer; axis: Integer): Single; cdecl;

  { Input-related functions: mouse }

  // Detect if a mouse button has been pressed once
  IsMouseButtonPressed: function(button: TMouseButton): Boolean; cdecl;
  // Detect if a mouse button is being pressed
  IsMouseButtonDown: function(button: TMouseButton): Boolean; cdecl;
  // Detect if a mouse button has been released once
  IsMouseButtonReleased: function(button: TMouseButton): Boolean; cdecl;
  // Detect if a mouse button is NOT being pressed
  IsMouseButtonUp: function(button: TMouseButton): Boolean; cdecl;
  // Returns mouse position X
  GetMouseX: function: Integer; cdecl;
  // Returns mouse position Y
  GetMouseY: function: Integer; cdecl;
  // Returns mouse position XY
  {$ifdef FPC}
  GetMousePosition: function: TVector2; cdecl;
  {$else}
  GetMousePosition: function: Int64; cdecl; //Stupid Delphi
  {$endif}
  // Set mouse position XY
  SetMousePosition: procedure(x: Integer; y: Integer); cdecl;
  // Set mouse offset
  SetMouseOffset: procedure(offsetX: Integer; offsetY: Integer); cdecl;
  // Set mouse scaling
  SetMouseScale: procedure(scaleX: Single; scaleY: Single); cdecl;
  // Returns mouse wheel movement Y
  GetMouseWheelMove: function: Single; cdecl;
  // Returns mouse cursor if (MouseCursor enum)
  GetMouseCursor: function: Integer; cdecl;
  // Set mouse cursor
  SetMouseCursor: procedure(Cursor: Integer); cdecl;

  { Input-related functions: touch }

  // Returns touch position X for touch point 0 (relative to screen size)
  GetTouchX: function: Integer; cdecl;
  // Returns touch position Y for touch point 0 (relative to screen size)
  GetTouchY: function: Integer; cdecl;
  // Returns touch position XY for a touch point index (relative to screen size)
  GetTouchPosition: function(index: Integer): TVector2; cdecl;

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: gestures)
//------------------------------------------------------------------------------------

  // Enable a set of gestures using flags
  SetGesturesEnabled: procedure(gestureFlags: Cardinal); cdecl;
  // Check if a gesture have been detected
  IsGestureDetected: function(gesture: Integer): Boolean; cdecl;
  // Get latest detected gesture
  GetGestureDetected: function: Integer; cdecl;
  // Get touch points count
  GetTouchPointsCount: function: Integer; cdecl;
  // Get gesture hold time in milliseconds
  GetGestureHoldDuration: function: Single; cdecl;
  // Get gesture drag vector
  GetGestureDragVector: function: TVector2; cdecl;
  // Get gesture drag angle
  GetGestureDragAngle: function: Single; cdecl;
  // Get gesture pinch delta
  GetGesturePinchVector: function: TVector2; cdecl;
  // Get gesture pinch angle
  GetGesturePinchAngle: function: Single; cdecl;

//------------------------------------------------------------------------------------
// Camera System Functions (Module: camera)
//------------------------------------------------------------------------------------

  // Set camera mode (multiple camera modes available)
  SetCameraMode: procedure(camera: TCamera; mode: Integer); cdecl;
  // Update camera position for selected mode
  UpdateCamera: procedure(camera: PCamera); cdecl;

  // Set camera pan key to combine with mouse movement (free camera)
  SetCameraPanControl: procedure(KeyPan: Integer); cdecl;
  // Set camera alt key to combine with mouse movement (free camera)
  SetCameraAltControl: procedure(Keylt: Integer); cdecl;
  // Set camera smooth zoom key to combine with mouse (free camera)
  SetCameraSmoothZoomControl: procedure(keySmoothZoom: Integer); cdecl;
  // Set camera move controls (1st person and 3rd person cameras)
  SetCameraMoveControls: procedure(KeyFront: Integer; KeyBack: Integer; KeyRight: Integer; KeyLeft: Integer; KeyUp: Integer; KeyDown: Integer); cdecl;

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------

  { Basic shapes drawing functions }

  // Draw a pixel
  DrawPixel: procedure(posX: Integer; posY: Integer; color: TColor); cdecl;
  // Draw a pixel (Vector version)
  DrawPixelV: procedure(position: TVector2; color: TColor); cdecl;
  // Draw a line
  DrawLine: procedure(startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl;
  // Draw a line (Vector version)
  DrawLineV: procedure(startPos: TVector2; endPos: TVector2; color: TColor); cdecl;
  // Draw a line defining thickness
  DrawLineEx: procedure(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl;
  // Draw a line using cubic-bezier curves in-out
  DrawLineBezier: procedure(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl;
  // Draw lines sequence
  DrawLineStrip: procedure(points: PVector2; pointsCount: Integer; color: TColor); cdecl;
  // Draw a color-filled circle
  DrawCircle: procedure(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl;
  // Draw a piece of a circle
  DrawCircleSector: procedure(center: TVector2; radius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  // Draw circle sector outline
  DrawCircleSectorLines: procedure(center: TVector2; radius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  // Draw a gradient-filled circle
  DrawCircleGradient: procedure(centerX: Integer; centerY: Integer; radius: Single; color1: TColor; color2: TColor); cdecl;
  // Draw a color-filled circle (Vector version)
  DrawCircleV: procedure(center: TVector2; radius: Single; color: TColor); cdecl;
  // Draw circle outline
  DrawCircleLines: procedure(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl;
  // Draw ellipse
  DrawEllipse: procedure(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl;
  // Draw ellipse outline
  DrawEllipseLines: procedure(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl;
  // Draw ring
  DrawRing: procedure(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  // Draw ring outline
  DrawRingLines: procedure(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl;
  // Draw a color-filled rectangle
  DrawRectangle: procedure(posX: Integer; posY: Integer; Width: Integer; Height: Integer; Color: TColor); cdecl;
  // Draw a color-filled rectangle (Vector version)
  DrawRectangleV: procedure(position: TVector2; size: TVector2; color: TColor); cdecl;
  // Draw a color-filled rectangle
  DrawRectangleRec: procedure(rec: TRectangle; color: TColor); cdecl;
  // Draw a color-filled rectangle with pro parameters
  DrawRectanglePro: procedure(rec: TRectangle; origin: TVector2; rotation: Single; color: TColor); cdecl;
  // Draw a vertical-gradient-filled rectangle
  DrawRectangleGradientV: procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl;
  // Draw a horizontal-gradient-filled rectangle
  DrawRectangleGradientH: procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl;
  // Draw a gradient-filled rectangle with custom vertex colors
  DrawRectangleGradientEx: procedure(rec: TRectangle; col1: TColor; col2: TColor; col3: TColor; col4: TColor); cdecl;
  // Draw rectangle outline
  DrawRectangleLines: procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl;
  // Draw rectangle outline with extended parameters
  DrawRectangleLinesEx: procedure(rec: TRectangle; lineThick: Integer; color: TColor); cdecl;
  // Draw rectangle with rounded edges
  DrawRectangleRounded: procedure(rec: TRectangle; roundness: Single; segments: Integer; color: TColor); cdecl;
  // Draw rectangle with rounded edges outline
  DrawRectangleRoundedLines: procedure(rec: TRectangle; roundness: Single; segments: Integer; lineThick: Integer; color: TColor); cdecl;
  // Draw a color-filled triangle (vertex in counter-clockwise order!)
  DrawTriangle: procedure(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl;
  // Draw triangle outline (vertex in counter-clockwise order!)
  DrawTriangleLines: procedure(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl;
  // Draw a triangle fan defined by points (first vertex is the center)
  DrawTriangleFan: procedure(points: PVector2; numPoints: Integer; color: TColor); cdecl;
  // Draw a triangle strip defined by points
  DrawTriangleStrip: procedure(points: PVector2; pointsCount: Integer; color: TColor); cdecl;
  // Draw a regular polygon (Vector version)
  DrawPoly: procedure(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl;
  // Draw a polygon outline of n sides
  DrawPolyLines: procedure(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl;

  { Basic shapes collision detection functions }

  // Check collision between two rectangles
  CheckCollisionRecs: function(rec1: TRectangle; rec2: TRectangle): Boolean; cdecl;
  // Check collision between two circles
  CheckCollisionCircles: function(center1: TVector2; radius1: Single; center2: TVector2; radius2: Single): Boolean; cdecl;
  // Check collision between circle and rectangle
  CheckCollisionCircleRec: function(center: TVector2; radius: Single; rec: TRectangle): Boolean; cdecl;
  // Check if point is inside rectangle
  CheckCollisionPointRec: function(point: TVector2; rec: TRectangle): Boolean; cdecl;
  // Check if point is inside circle
  CheckCollisionPointCircle: function(point: TVector2; center: TVector2; radius: Single): Boolean; cdecl;
  // Check if point is inside a triangle
  CheckCollisionPointTriangle: function(point: TVector2; p1: TVector2; p2: TVector2; p3: TVector2): Boolean; cdecl;
  // Check the collision between two lines defined by two points each, returns collision point by reference
  CheckCollisionLines: function(startPos1, endPos1, startPos2, endPos2: TVector2; collisionPoint: PVector2): Boolean; cdecl;
  // Get collision rectangle for two rectangles collision
  GetCollisionRec: function(rec1: TRectangle; rec2: TRectangle): TRectangle; cdecl;

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

  { Image loading functions }
  // NOTE: This functions do not require GPU access

  // Load image from file into CPU memory (RAM)
  LoadImage: function(const fileName: PUTF8Char): TImage; cdecl;
  // Load image from RAW file data
  LoadImageRaw: function(const fileName: PUTF8Char; width: Integer; height: Integer; format: Integer; headerSize: Integer): TImage; cdecl;
  // Load image sequence from file (frames appended to image.data)
  LoadImageAnim: function(const fileName: PUTF8Char; frames: Integer): TImage; cdecl;
  // Load image from memory buffer, fileType refers to extension: i.e. "png"
  LoadImageFromMemory: function(const fileType: PUTF8Char; fileData: PByte; dataSize: Integer): TImage; cdecl;
  // Unload image from CPU memory (RAM)
  UnloadImage: procedure(image: TImage); cdecl;
  // Export image data to file
  ExportImage: procedure(image: TImage; const fileName: PUTF8Char); cdecl;
  // Export image as code file defining an array of bytes
  ExportImageAsCode: procedure(image: TImage; const fileName: PUTF8Char); cdecl;

  { Image generation functions }

  // Generate image: plain color
  GenImageColor: function(width: Integer; height: Integer; color: TColor): TImage; cdecl;
  // Generate image: vertical gradient
  GenImageGradientV: function(width: Integer; height: Integer; top: TColor; bottom: TColor): TImage; cdecl;
  // Generate image: horizontal gradient
  GenImageGradientH: function(width: Integer; height: Integer; left: TColor; right: TColor): TImage; cdecl;
  // Generate image: radial gradient
  GenImageGradientRadial: function(width: Integer; height: Integer; density: Single; inner: TColor; outer: TColor): TImage; cdecl;
  // Generate image: checked
  GenImageChecked: function(width: Integer; height: Integer; checksX: Integer; checksY: Integer; col1: TColor; col2: TColor): TImage; cdecl;
  // Generate image: white noise
  GenImageWhiteNoise: function(width: Integer; height: Integer; factor: Single): TImage; cdecl;
  // Generate image: perlin noise
  GenImagePerlinNoise: function(width: Integer; height: Integer; offsetX: Integer; offsetY: Integer; scale: Single): TImage; cdecl;
  // Generate image: cellular algorithm. Bigger tileSize means bigger cells
  GenImageCellular: function(width: Integer; height: Integer; tileSize: Integer): TImage; cdecl;

  { Image manipulation functions }

  // Create an image duplicate (useful for transformations)
  ImageCopy: function(image: TImage): TImage; cdecl;
  // Create an image from another image piece
  ImageFromImage: function(image: TImage; rec: TRectangle): TImage; cdecl;
  // Create an image from text (default font)
  ImageText: function(const text: PUTF8Char; fontSize: Integer; color: TColor): TImage; cdecl;
  // Create an image from text (custom sprite font)
  ImageTextEx: function(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single; tint: TColor): TImage; cdecl;
  // Convert image to POT (power-of-two)
  ImageToPOT: procedure(image: PImage; fillColor: TColor); cdecl;
  // Convert image data to desired format
  ImageFormat: procedure(image: PImage; newFormat: Integer); cdecl;
  // Crop an image to a defined rectangle
  ImageCrop: procedure(image: PImage; crop: TRectangle); cdecl;
  // Crop image depending on alpha value
  ImageAlphaCrop: procedure(image: PImage; threshold: Single); cdecl;
  // Clear alpha channel to desired color
  ImageAlphaClear: procedure(image: PImage; color: TColor; threshold: Single); cdecl;
  // Apply alpha mask to image
  ImageAlphaMask: procedure(image: PImage; alphaMask: TImage); cdecl;
  // Premultiply alpha channel
  ImageAlphaPremultiply: procedure(image: PImage); cdecl;
  // Resize image (Bicubic scaling algorithm)
  ImageResize: procedure(image: PImage; newWidth: Integer; newHeight: Integer); cdecl;
  // Resize image (Nearest-Neighbor scaling algorithm)
  ImageResizeNN: procedure(image: PImage; newWidth: Integer; newHeight: Integer); cdecl;
  // Resize canvas and fill with color
  ImageResizeCanvas: procedure(image: PImage; newWidth: Integer; newHeight: Integer; offsetX: Integer; offsetY: Integer; color: TColor); cdecl;
  // Generate all mipmap levels for a provided image
  ImageMipmaps: procedure(image: PImage); cdecl;
  // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
  ImageDither: procedure(image: PImage; rBpp: Integer; gBpp: Integer; bBpp: Integer; aBpp: Integer); cdecl;
  // Flip image vertically
  ImageFlipVertical: procedure(image: PImage); cdecl;
  // Flip image horizontally
  ImageFlipHorizontal: procedure(image: PImage); cdecl;
  // Rotate image clockwise 90deg
  ImageRotateCW: procedure(image: PImage); cdecl;
  // Rotate image counter-clockwise 90deg
  ImageRotateCCW: procedure(image: PImage); cdecl;
  // Modify image color: tint
  ImageColorTint: procedure(image: PImage; color: TColor); cdecl;
  // Modify image color: invert
  ImageColorInvert: procedure(image: PImage); cdecl;
  // Modify image color: grayscale
  ImageColorGrayscale: procedure(image: PImage); cdecl;
  // Modify image color: contrast (-100 to 100)
  ImageColorContrast: procedure(image: PImage; contrast: Single); cdecl;
  // Modify image color: brightness (-255 to 255)
  ImageColorBrightness: procedure(image: PImage; brightness: Integer); cdecl;
  // Modify image color: replace color
  ImageColorReplace: procedure(image: PImage; color: TColor; replace: TColor); cdecl;
  // Get pixel data from image as a Color struct array
  LoadImageColors: function(image: TImage): PColor; cdecl;
  // Extract color palette from image to maximum size (memory should be freed)
  LoadImagePalette: function(image: TImage; maxPaletteSize: Integer; colorsCount: PInteger): PColor; cdecl;
  // Get image alpha border rectangle
  GetImageAlphaBorder: function(image: TImage; threshold: Single): TRectangle; cdecl;
  // Unload color data loaded with LoadImageColors()
  UnloadImageColors: procedure(Colors: PColor); cdecl;
  // Unload colors palette loaded with LoadImagePalette()
  UnloadImagePalette: procedure(Colors: PColor); cdecl;

  { Image drawing functions }
  // NOTE: Image software-rendering functions (CPU)

  // Clear image background with given color
  ImageClearBackground: procedure(dst: PImage; color: TColor); cdecl;
  // Draw pixel within an image
  ImageDrawPixel: procedure(dst: PImage; posX: Integer; posY: Integer; color: TColor); cdecl;
  // Draw pixel within an image (Vector version)
  ImageDrawPixelV: procedure(dst: PImage; position: TVector2; color: TColor); cdecl;
  // Draw line within an image
  ImageDrawLine: procedure(dst: PImage; startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl;
  // Draw line within an image (Vector version)
  ImageDrawLineV: procedure(dst: PImage; start: TVector2; &end: TVector2; color: TColor); cdecl;
  // Draw circle within an image
  ImageDrawCircle: procedure(const dst: TImage; CenterX, CenterY, Radius: Integer; Color: TColor); cdecl;
  // Draw circle within an image (Vector version)
  ImageDrawCircleV: procedure(dst: PImage; center: TVector2; radius: Integer; color: TColor); cdecl;
  // Draw rectangle within an image
  ImageDrawRectangle: procedure(dst: PImage; posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl;
  // Draw rectangle within an image (Vector version)
  ImageDrawRectangleV: procedure(dst: PImage; position: TVector2; size: TVector2; color: TColor); cdecl;
  // Draw rectangle within an image
  ImageDrawRectangleRec: procedure(dst: PImage; rec: TRectangle; color: TColor); cdecl;
  // Draw rectangle lines within an image
  ImageDrawRectangleLines: procedure(dst: PImage; rec: TRectangle; thick: Integer; color: TColor); cdecl;
  // Draw a source image within a destination image (tint applied to source)
  ImageDraw: procedure(dst: PImage; src: TImage; srcRec: TRectangle; dstRec: TRectangle; tint: TColor); cdecl;
  // Draw text (using default font) within an image (destination)
  ImageDrawText: procedure(dst: PImage; const text: PUTF8Char; posX, posY: Integer; fontSize: Integer; color: TColor); cdecl;
  // Draw text (custom sprite font) within an image (destination)
  ImageDrawTextEx: procedure(dst: PImage; font: TFont; const text: PUTF8Char; position: TVector2; fontSize: Single; spacing: Single; color: TColor); cdecl;

  { Texture loading functions }
  // NOTE: These functions require GPU access

  // Load texture from file into GPU memory (VRAM)
  LoadTexture: function(const fileName: PUTF8Char): TTexture2D; cdecl;
  // Load texture from image data
  LoadTextureFromImage: function(image: TImage): TTexture2D; cdecl;
  // Load cubemap from image, multiple image cubemap layouts supported
  LoadTextureCubemap: function(image: TImage; layoutType: Integer): TTextureCubemap; cdecl;
  // Load texture for rendering (framebuffer)
  LoadRenderTexture: function(width: Integer; height: Integer): TRenderTexture2D; cdecl;
  // Unload texture from GPU memory (VRAM)
  UnloadTexture: procedure(texture: TTexture2D); cdecl;
  // Unload render texture from GPU memory (VRAM)
  UnloadRenderTexture: procedure(target: TRenderTexture2D); cdecl;
  // Update GPU texture with new data
  UpdateTexture: procedure(texture: TTexture2D; const pixels: Pointer); cdecl;
  // Update GPU texture rectangle with new data
  UpdateTextureRec: procedure(texture: TTexture2D; rec: TRectangle; const pixels: Pointer); cdecl;
  // Get pixel data from GPU texture and return an Image
  GetTextureData: function(texture: TTexture2D): TImage; cdecl;
  // Get pixel data from screen buffer and return an Image (screenshot)
  GetScreenData: function: TImage; cdecl;

  { Texture configuration functions }

  // Generate GPU mipmaps for a texture
  GenTextureMipmaps: procedure(texture: PTexture2D); cdecl;
  // Set texture scaling filter mode
  SetTextureFilter: procedure(texture: TTexture2D; FilterMode: TTextureFilterMode); cdecl;
  // Set texture wrapping mode
  SetTextureWrap: procedure(texture: TTexture2D; wrapMode: Integer); cdecl;

  { Texture drawing functions }

  // Draw a Texture2D
  DrawTexture: procedure(texture: TTexture2D; posX: Integer; posY: Integer; tint: TColor); cdecl;
  // Draw a Texture2D with position defined as Vector2
  DrawTextureV: procedure(texture: TTexture2D; position: TVector2; tint: TColor); cdecl;
  // Draw a Texture2D with extended parameters
  DrawTextureEx: procedure(texture: TTexture2D; position: TVector2; rotation: Single; scale: Single; tint: TColor); cdecl;
  // Draw a part of a texture defined by a rectangle
  DrawTextureRec: procedure(texture: TTexture2D; source: TRectangle; position: TVector2; tint: TColor); cdecl;
  // Draw texture quad with tiling and offset parameters
  DrawTextureQuad: procedure(texture: TTexture2D; tiling: TVector2; offset: TVector2; quad: TRectangle; tint: TColor); cdecl;
  // Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
  DrawTextureTiled: procedure(texture: TTexture2D; source: TRectangle; dest: TRectangle; origin: TVector2; rotation: Single; Scale: Single; tint: TColor); cdecl;
  // Draw a part of a texture defined by a rectangle with 'pro' parameters
  DrawTexturePro: procedure(texture: TTexture2D; source: TRectangle; dest: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl;
  // Draws a texture (or part of it) that stretches or shrinks nicely
  DrawTextureNPatch: procedure(texture: TTexture2D; nPatchInfo: TNPatchInfo; dest: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl;

  { Image/Texture misc functions }


  { Color-related functions }

  // Color fade-in or fade-out, alpha goes from 0.0f to 1.0f
  Fade: function(color: TColor; Alpha: Single): TColor; cdecl;
  // Returns hexadecimal value for a Color
  ColorToInt: function(color: TColor): Integer; cdecl;
  // Returns color normalized as float [0..1]
  ColorNormalize: function(color: TColor): TVector4; cdecl;
  // Returns color from normalized values [0..1]
  ColorFromNormalized: function(normalized: TVector4): TColor; cdecl;
  // Returns HSV values for a Color
  ColorToHSV: function(color: TColor): TVector3; cdecl;
  // Returns a Color from HSV values
  ColorFromHSV: function(hsv: TVector3): TColor; cdecl;
  // Returns color with alpha applied, alpha goes from 0.0f to 1.0f
  ColorAlpha: function(color: TColor; Alpha: Single): TColor; cdecl;
  // Returns src alpha-blended into dst color with tint
  ColorAlphaBlend: function(dst, src, tint: TColor): TColor; cdecl;
  // Returns a Color struct from hexadecimal value
  GetColor: function(hexValue: Integer): TColor; cdecl;
  // Get Color from a source pixel pointer of certain format
  GetPixelColor: function(srcPtr: Pointer; format: Integer): TColor; cdecl;
  // Set color formatted into destination pixel pointer
  SetPixelColor: function(dstPtr: Pointer; Color: TColor; format: Integer): TColor; cdecl;
  // Get pixel data size in bytes (image or texture)
  GetPixelDataSize: function(width: Integer; height: Integer; format: Integer): Integer; cdecl;

//------------------------------------------------------------------------------------
// Font Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

  { Font loading/unloading functions }

  // Get the default Font
  GetFontDefault: function: TFont; cdecl;
  // Load font from file into GPU memory (VRAM)
  LoadFont: function(const fileName: PUTF8Char): TFont; cdecl;
  // Load font from file with extended parameters
  LoadFontEx: function(const fileName: PUTF8Char; fontSize: Integer; fontChars: PInteger; charsCount: Integer): TFont; cdecl;
  // Load font from Image (XNA style)
  LoadFontFromImage: function(image: TImage; key: TColor; firstChar: Integer): TFont; cdecl;
  // Load font from memory buffer, fileType refers to extension: i.e. "ttf"
  LoadFontFromMemory: function(const fileType: PUTF8Char; fileData: PByte; dataSize: Integer; fontSize: Integer; fontChars: PInteger; charsCount: Integer): TFont; cdecl;
  // Load font data for further use
  LoadFontData: function(fileData: PByte; dataSize, fontSize: Integer; fontChars: PInteger; charsCount: Integer; &type: Integer): PCharInfo; cdecl;
  // Generate image font atlas using chars info
  GenImageFontAtlas: function(const chars: PCharInfo; recs: PPRectangle; charsCount: Integer; fontSize: Integer; padding: Integer; packMethod: Integer): TImage; cdecl;
  // Unload font chars info data (RAM)
  UnloadFontData: procedure(fileData: PByte; charsCount: Integer); cdecl;
  // Unload Font from GPU memory (VRAM)
  UnloadFont: procedure(font: TFont); cdecl;

  { Text drawing functions }

  // Shows current FPS
  DrawFPS: procedure(posX: Integer; posY: Integer); cdecl;
  // Draw text (using default font)
  DrawText: procedure(const Text: PUTF8Char; posX: Integer; posY: Integer; FontSize: Integer; Color: TColor); cdecl;
  // Draw text using font and additional parameters
  DrawTextEx: procedure(font: TFont; const text: Pointer; position: TVector2; fontSize: Single; spacing: Single; tint: TColor); cdecl;
  // Draw text using font inside rectangle limits
  DrawTextRec: procedure(font: TFont; const text: PUTF8Char; rec: TRectangle; fontSize: Single; spacing: Single; wordWrap: Boolean; tint: TColor); cdecl;
  DrawTextRecEx: procedure(font: TFont; const text: PUTF8Char; rec: TRectangle; fontSize: Single; spacing: Single; wordWrap: Boolean; tint: TColor; selectStart: Integer; selectLength: Integer; selectTint: TColor; selectBackTint: TColor); cdecl;

  // Draw text using font inside rectangle limits with support for text selection
  DrawTextCodepoint: procedure(font: TFont; codepoint: Integer; position: TVector2; fontSize: Single; tint: TColor); cdecl;

  { Text misc. functions }

  // Measure string width for default font
  MeasureText: function(const text: PUTF8Char; fontSize: Integer): Integer; cdecl;
  // Measure string size for Font
  MeasureTextEx: function(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single): TVector2; cdecl;
  // Get index position for a unicode character on font
  GetGlyphIndex: function(font: TFont; codepoint: Integer): Integer; cdecl;

  { Text strings management functions (no utf8 strings, only byte chars) }
  // NOTE: Some strings allocate memory internally for returned strings, just be careful!

  // Copy one string to another, returns bytes copied
  TextCopy: function(dst: PUTF8Char; const src: PUTF8Char): Integer; cdecl;
  // Check if two text string are equal
  TextIsEqual: function(const text1: PUTF8Char; const text2: PUTF8Char): Boolean; cdecl;
  // Get text length, checks for '\0' ending
  TextLength: function(const text: PUTF8Char): Cardinal; cdecl;
  // Text formatting with variables (sprintf style)
  TextFormat: function(const text: PUTF8Char): PUTF8Char varargs; cdecl;
  // Get a piece of a text string
  TextSubtext: function(const text: PUTF8Char; position: Integer; length: Integer): PUTF8Char; cdecl;
  // Replace text string (memory must be freed!)
  TextReplace: function(text: PUTF8Char; const replace: PUTF8Char; const by: PUTF8Char): PUTF8Char; cdecl;
  // Insert text in a position (memory must be freed!)
  TextInsert: function(const text: PUTF8Char; const insert: PUTF8Char; position: Integer): PUTF8Char; cdecl;
  // Join text strings with delimiter
  TextJoin: function(textList: PPUTF8Char; count: Integer; const delimiter: PUTF8Char): PUTF8Char; cdecl;
  // Split text into multiple strings
  TextSplit: function(const text: PUTF8Char; delimiter: UTF8Char; count: PInteger): PPUTF8Char; cdecl;
  // Append text at specific position and move cursor!
  TextAppend: procedure(text: PUTF8Char; const append: PUTF8Char; position: PInteger); cdecl;
  // Find first text occurrence within a string
  TextFindIndex: function(const text: PUTF8Char; const find: PUTF8Char): Integer; cdecl;
  // Get upper case version of provided string
  TextToUpper: function(const text: PUTF8Char): PUTF8Char; cdecl;
  // Get lower case version of provided string
  TextToLower: function(const text: PUTF8Char): PUTF8Char; cdecl;
  // Get Pascal case notation version of provided string
  TextToPascal: function(const text: PUTF8Char): PUTF8Char; cdecl;
  // Get integer value from text (negative values not supported)
  TextToInteger: function(const text: PUTF8Char): Integer; cdecl;
  // Encode text codepoint into utf8 text (memory must be freed!)
  TextToUtf8: function(codepoints: PInteger; length: Integer): PUTF8Char; cdecl;

  { UTF8 text strings management functions }

  // Get all codepoints in a string, codepoints count returned by parameters
  GetCodepoints: function(const text: PUTF8Char; count: PInteger): PInteger; cdecl;
  // Get total number of characters (codepoints) in a UTF8 encoded string
  GetCodepointsCount: function(const text: PUTF8Char): Integer; cdecl;
  // Returns next codepoint in a UTF8 encoded string; 0x3f('?') is returned on failure
  GetNextCodepoint: function(const text: PUTF8Char; bytesProcessed: PInteger): Integer; cdecl;
  // Encode codepoint into utf8 text (char array length returned as parameter)
  CodepointToUtf8: function(codepoint: Integer; byteLength: PInteger): PUTF8Char; cdecl;

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

  { Basic geometric 3D shapes drawing functions }

  // Draw a line in 3D world space
  DrawLine3D: procedure(startPos: TVector3; endPos: TVector3; color: TColor); cdecl;
  // Draw a point in 3D space, actually a small line
  DrawPoint3D: procedure(position: TVector3; color: TColor); cdecl;
  // Draw a circle in 3D world space
  DrawCircle3D: procedure(center: TVector3; radius: Single; rotationAxis: TVector3; rotationAngle: Single; color: TColor); cdecl;
  // Draw a color-filled triangle (vertex in counter-clockwise order!)
  DrawTriangle3D: procedure(v1: TVector3; v2: TVector3; v3: TVector3; color: TColor); cdecl;
  // Draw a triangle strip defined by points
  DrawTriangleStrip3D: procedure(Points: PVector3; pointsCount: integer; Color: TColor); cdecl;
  // Draw cube
  DrawCube: procedure(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl;
  // Draw cube (Vector version)
  DrawCubeV: procedure(position: TVector3; size: TVector3; color: TColor); cdecl;
  // Draw cube wires
  DrawCubeWires: procedure(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl;
  // Draw cube wires (Vector version)
  DrawCubeWiresV: procedure(position: TVector3; size: TVector3; color: TColor); cdecl;
  // Draw cube textured
  DrawCubeTexture: procedure(texture: TTexture2D; position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl;
  // Draw sphere
  DrawSphere: procedure(centerPos: TVector3; radius: Single; color: TColor); cdecl;
  // Draw sphere with extended parameters
  DrawSphereEx: procedure(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl;
  // Draw sphere wires
  DrawSphereWires: procedure(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl;
  // Draw a cylinder/cone
  DrawCylinder: procedure(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl;
  // Draw a cylinder/cone wires
  DrawCylinderWires: procedure(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl;
  // Draw a plane XZ
  DrawPlane: procedure(centerPos: TVector3; size: TVector2; color: TColor); cdecl;
  // Draw a ray line
  DrawRay: procedure(ray: TRay; color: TColor); cdecl;
  // Draw a grid (centered at (0, 0, 0))
  DrawGrid: procedure(slices: Integer; spacing: Single); cdecl;
  // Draw simple gizmo
  DrawGizmo: procedure(position: TVector3); cdecl;
//DrawTorus(), DrawTeapot() could be useful?

//------------------------------------------------------------------------------------
// Model 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

  { Model loading/unloading functions }

  // Load model from files (meshes and materials)
  LoadModel: function(const fileName: PUTF8Char): TModel; cdecl;
  // Load model from generated mesh (default material)
  LoadModelFromMesh: function(mesh: TMesh): TModel; cdecl;
  // Unload model from memory (RAM and/or VRAM)
  UnloadModel: procedure(model: TModel); cdecl;
  // Unload model (but not meshes) from memory (RAM and/or VRAM)
  UnloadModelKeepMeshes: procedure(model: TModel); cdecl;

  { Mesh loading/unloading functions }

  // Load meshes from model file
  LoadMeshes: function(const fileName: PUTF8Char; meshCount: PInteger): PMesh; cdecl;
  // Unload mesh from memory (RAM and/or VRAM)
  UnloadMesh: procedure(mesh: TMesh); cdecl;
  // Export mesh data to file
  ExportMesh: procedure(mesh: TMesh; const fileName: PUTF8Char); cdecl;

  { Material loading/unloading functions }

  // Load materials from model file
  LoadMaterials: function(const fileName: PUTF8Char; materialCount: PInteger): PMaterial; cdecl;
  // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
  LoadMaterialDefault: function: TMaterial; cdecl;
  // Unload material from GPU memory (VRAM)
  UnloadMaterial: procedure(material: TMaterial); cdecl;
  // Set texture for a material map type (MAP_DIFFUSE, MAP_SPECULAR...)
  SetMaterialTexture: procedure(material: PMaterial; mapType: Integer; texture: TTexture2D); cdecl;
  // Set material for a mesh
  SetModelMeshMaterial: procedure(model: PModel; meshId: Integer; materialId: Integer); cdecl;

  { Model animations loading/unloading functions }

  // Load model animations from file
  LoadModelAnimations: function(const fileName: PUTF8Char; animsCount: PInteger): PModelAnimation; cdecl;
  // Update model animation pose
  UpdateModelAnimation: procedure(model: TModel; anim: TModelAnimation; frame: Integer); cdecl;
  // Unload animation data
  UnloadModelAnimation: procedure(anim: TModelAnimation); cdecl;
  // Check model animation skeleton match
  IsModelAnimationValid: function(model: TModel; anim: TModelAnimation): Boolean; cdecl;

  { Mesh generation functions }

  // Generate polygonal mesh
  GenMeshPoly: function(sides: Integer; radius: Single): TMesh; cdecl;
  // Generate plane mesh (with subdivisions)
  GenMeshPlane: function(width: Single; length: Single; resX: Integer; resZ: Integer): TMesh; cdecl;
  // Generate cuboid mesh
  GenMeshCube: function(width: Single; height: Single; length: Single): TMesh; cdecl;
  // Generate sphere mesh (standard sphere)
  GenMeshSphere: function(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl;
  // Generate half-sphere mesh (no bottom cap)
  GenMeshHemiSphere: function(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl;
  // Generate cylinder mesh
  GenMeshCylinder: function(radius: Single; height: Single; slices: Integer): TMesh; cdecl;
  // Generate torus mesh
  GenMeshTorus: function(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl;
  // Generate trefoil knot mesh
  GenMeshKnot: function(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl;
  // Generate heightmap mesh from image data
  GenMeshHeightmap: function(heightmap: TImage; size: TVector3): TMesh; cdecl;
  // Generate cubes-based map mesh from image data
  GenMeshCubicmap: function(cubicmap: TImage; cubeSize: TVector3): TMesh; cdecl;

  { Mesh manipulation functions }

  // Compute mesh bounding box limits
  MeshBoundingBox: function(mesh: TMesh): TBoundingBox; cdecl;
  // Compute mesh tangents
  MeshTangents: procedure(mesh: PMesh); cdecl;
  // Compute mesh binormals
  MeshBinormals: procedure(mesh: PMesh); cdecl;
  // Smooth (average) vertex normals
  MeshNormalsSmooth: procedure(mesh: PMesh); cdecl;

  { Model drawing functions }

  // Draw a model (with texture if set)
  DrawModel: procedure(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl;
  // Draw a model with extended parameters
  DrawModelEx: procedure(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl;
  // Draw a model wires (with texture if set)
  DrawModelWires: procedure(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl;
  // Draw a model wires (with texture if set) with extended parameters
  DrawModelWiresEx: procedure(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl;
  // Draw bounding box (wires)
  DrawBoundingBox: procedure(box: TBoundingBox; color: TColor); cdecl;
  // Draw a billboard texture
  DrawBillboard: procedure(camera: TCamera; texture: TTexture2D; center: TVector3; size: Single; tint: TColor); cdecl;
  // Draw a billboard texture defined by sourceRec
  DrawBillboardRec: procedure(camera: TCamera; texture: TTexture2D; source: TRectangle; center: TVector3; size: Single; tint: TColor); cdecl;

  { Collision detection functions }

  // Detect collision between two spheres
  CheckCollisionSpheres: function(centerA: TVector3; radiusA: Single; centerB: TVector3; radiusB: Single): Boolean; cdecl;
  // Detect collision between two bounding boxes
  CheckCollisionBoxes: function(box1: TBoundingBox; box2: TBoundingBox): Boolean; cdecl;
  // Detect collision between box and sphere
  CheckCollisionBoxSphere: function(box: TBoundingBox; center: TVector3; radius: Single): Boolean; cdecl;
  // Detect collision between ray and sphere
  CheckCollisionRaySphere: function(ray: TRay; center: TVector3; radius: Single): Boolean; cdecl;
  // Detect collision between ray and sphere, returns collision point
  CheckCollisionRaySphereEx: function(ray: TRay; center: TVector3; radius: Single; collisionPoint: PVector3): Boolean; cdecl;
  // Detect collision between ray and box
  CheckCollisionRayBox: function(ray: TRay; box: TBoundingBox): Boolean; cdecl;
  // Get collision info between ray and mesh
  GetCollisionRayMesh: function(ray: TRay; mesh: TMesh; transform: TMatrix): TRayHitInfo; cdecl;
  // Get collision info between ray and model
  GetCollisionRayModel: function(ray: TRay; model: TModel): TRayHitInfo; cdecl;
  // Get collision info between ray and triangle
  GetCollisionRayTriangle: function(ray: TRay; p1: TVector3; p2: TVector3; p3: TVector3): TRayHitInfo; cdecl;
  // Get collision info between ray and ground plane (Y-normal plane)
  GetCollisionRayGround: function(ray: TRay; groundHeight: Single): TRayHitInfo; cdecl;

//------------------------------------------------------------------------------------
// Shaders System Functions (Module: rlgl)
// NOTE: This functions are useless when using OpenGL 1.1
//------------------------------------------------------------------------------------

  { Shader loading/unloading functions }

  // Load shader from files and bind default locations
  LoadShader: function(const vsFileName: PUTF8Char; const fsFileName: PUTF8Char): TShader; cdecl;
  // Load shader from code strings and bind default locations
  LoadShaderCode: function(const vsCode: PUTF8Char; const fsCode: PUTF8Char): TShader; cdecl;
  // Unload shader from GPU memory (VRAM)
  UnloadShader: procedure(shader: TShader); cdecl;

  // Get default shader
  GetShaderDefault: function: TShader; cdecl;
  // Get default texture
  GetTextureDefault: function: TTexture2D; cdecl;
  // Get texture to draw shapes
  GetShapesTexture: function: TTexture2D; cdecl;
  // Get texture rectangle to draw shapes
  GetShapesTextureRec: function: TRectangle; cdecl;
  // Define default texture used to draw shapes
  SetShapesTexture: procedure(texture: TTexture2D; source: TRectangle); cdecl;

  { Shader configuration functions }

  // Get shader uniform location
  GetShaderLocation: function(shader: TShader; const uniformName: PUTF8Char): Integer; cdecl;
  GetShaderLocationAttrib: function(shader: TShader; const attribName: PUTF8Char): Integer; cdecl;
  // Set shader uniform value
  SetShaderValue: procedure(shader: TShader; uniformLoc: Integer; const value: Pointer; uniformType: Integer); cdecl;
  // Set shader uniform value vector
  SetShaderValueV: procedure(shader: TShader; uniformLoc: Integer; const value: Pointer; uniformType: Integer; count: Integer); cdecl;
  // Set shader uniform value (matrix 4x4)
  SetShaderValueMatrix: procedure(shader: TShader; uniformLoc: Integer; mat: TMatrix); cdecl;
  // Set shader uniform value for texture
  SetShaderValueTexture: procedure(shader: TShader; uniformLoc: Integer; texture: TTexture2D); cdecl;
  // Set a custom projection matrix (replaces internal projection matrix)
  SetMatrixProjection: procedure(proj: TMatrix); cdecl;
  // Set a custom modelview matrix (replaces internal modelview matrix)
  SetMatrixModelview: procedure(view: TMatrix); cdecl;
  // Get internal modelview matrix
  GetMatrixModelview: function: TMatrix; cdecl;
  // Get internal projection matrix
  GetMatrixProjection: function: TMatrix; cdecl;

  { Texture maps generation (PBR) }
  // NOTE: Required shaders should be provided

  // Generate cubemap texture from 2D panorama texture
  GenTextureCubemap: function(shader: TShader; map: TTexture2D; size: Integer): TTextureCubemap; cdecl;
  // Generate irradiance texture using cubemap data
  GenTextureIrradiance: function(shader: TShader; cubemap: TTextureCubemap; size: Integer): TTextureCubemap; cdecl;
  // Generate prefilter texture using cubemap data
  GenTexturePrefilter: function(shader: TShader; cubemap: TTextureCubemap; size: Integer): TTextureCubemap; cdecl;
  // Generate BRDF texture
  GenTextureBRDF: function(shader: TShader; size: Integer): TTexture2D; cdecl;

  { Shading begin/end functions }

  // Begin custom shader drawing
  BeginShaderMode: procedure(shader: TShader); cdecl;
  // End custom shader drawing (use default shader)
  EndShaderMode: procedure; cdecl;
  // Begin blending mode (alpha, additive, multiplied)
  BeginBlendMode: procedure(mode: Integer); cdecl;
  // End blending mode (reset to default: alpha blending)
  EndBlendMode: procedure; cdecl;

  { VR control functions }

  // Init VR simulator for selected device parameters
  InitVrSimulator: procedure; cdecl;
  // Close VR simulator for current device
  CloseVrSimulator: procedure; cdecl;
  // Update VR tracking (position and orientation) and camera
  UpdateVrTracking: procedure(camera: PCamera); cdecl;
  // Set stereo rendering configuration parameters
  SetVrConfiguration: procedure(info: TVrDeviceInfo; distortion: TShader); cdecl;
  // Detect if VR simulator is ready
  IsVrSimulatorReady: function: Boolean; cdecl;
  // Enable/Disable VR experience
  ToggleVrMode: procedure; cdecl;
  // Begin VR simulator stereo rendering
  BeginVrDrawing: procedure; cdecl;
  // End VR simulator stereo rendering
  EndVrDrawing: procedure; cdecl;

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------

  { Audio device management functions }

  // Initialize audio device and context
  InitAudioDevice: procedure; cdecl;
  // Close the audio device and context
  CloseAudioDevice: procedure; cdecl;
  // Check if audio device has been initialized successfully
  IsAudioDeviceReady: function: Boolean; cdecl;
  // Set master volume (listener)
  SetMasterVolume: procedure(volume: Single); cdecl;

  { Wave/Sound loading/unloading functions }

  // Load wave data from file
  LoadWave: function(const fileName: PUTF8Char): TWave; cdecl;
  // Load wave from memory buffer, fileType refers to extension: i.e. "wav"
  LoadWaveFromMemory: function(const fileType: PUTF8Char; fileData: PByte; dataSize: Integer): TImage; cdecl;
  // Load sound from file
  LoadSound: function(const fileName: PUTF8Char): TSound; cdecl;
  // Load sound from wave data
  LoadSoundFromWave: function(wave: TWave): TSound; cdecl;
  // Update sound buffer with new data
  UpdateSound: procedure(sound: TSound; const data: Pointer; samplesCount: Integer); cdecl;
  // Unload wave data
  UnloadWave: procedure(wave: TWave); cdecl;
  // Unload sound
  UnloadSound: procedure(sound: TSound); cdecl;
  // Export wave data to file
  ExportWave: function(wave: TWave; const fileName: PUTF8Char): Boolean; cdecl;
  // Export wave sample data to code (.h)
  ExportWaveAsCode: function(wave: TWave; const fileName: PUTF8Char): Boolean; cdecl;

  { Wave/Sound management functions }

  // Play a sound
  PlaySound: procedure(Sound: TSound); cdecl;
  // Stop playing a sound
  StopSound: procedure(Sound: TSound); cdecl;
  // Pause a sound
  PauseSound: procedure(Sound: TSound); cdecl;
  // Resume a paused sound
  ResumeSound: procedure(Sound: TSound); cdecl;
  // Play a sound (using multichannel buffer pool)
  PlaySoundMulti: procedure(Sound: TSound); cdecl;
  // Stop any sound playing (using multichannel buffer pool)
  StopSoundMulti: procedure; cdecl;
  // Get number of sounds playing in the multichannel
  GetSoundsPlaying: function: Integer; cdecl;
  // Check if a sound is currently playing
  IsSoundPlaying: function(Sound: TSound): Boolean; cdecl;
  // Set volume for a sound (1.0 is max level)
  SetSoundVolume: procedure(Sound: TSound; volume: Single); cdecl;
  // Set pitch for a sound (1.0 is base level)
  SetSoundPitch: procedure(Sound: TSound; pitch: Single); cdecl;
  // Convert wave data to desired format
  WaveFormat: procedure(Wave: PWave; sampleRate: Integer; sampleSize: Integer; channels: Integer); cdecl;
  // Copy a wave to a new wave
  WaveCopy: function(Wave: TWave): TWave; cdecl;
  // Crop a wave to defined samples range
  WaveCrop: procedure(Wave: PWave; initSample: Integer; finalSample: Integer); cdecl;
  // Get samples data from wave as a floats array
  LoadWaveSamples: function(Wave: TWave): PSingle; cdecl;
  // Unload samples data loaded with LoadWaveSamples()
  UnloadWaveSamples: procedure(samples: PSingle); cdecl;

  { Music management functions }

  // Load music stream from file
  LoadMusicStream: function(const FileName: PUTF8Char): TMusic; cdecl;
  // Unload music stream
  UnloadMusicStream: procedure(Music: TMusic); cdecl;
  // Start music playing
  PlayMusicStream: procedure(Music: TMusic); cdecl;
  // Updates buffers for music streaming
  UpdateMusicStream: procedure(Music: TMusic); cdecl;
  // Stop music playing
  StopMusicStream: procedure(Music: TMusic); cdecl;
  // Pause music playing
  PauseMusicStream: procedure(Music: TMusic); cdecl;
  // Resume playing paused music
  ResumeMusicStream: procedure(Music: TMusic); cdecl;
  // Check if music is playing
  IsMusicPlaying: function(Music: TMusic): Boolean; cdecl;
  // Set volume for music (1.0 is max level)
  SetMusicVolume: procedure(Music: TMusic; volume: Single); cdecl;
  // Set pitch for a music (1.0 is base level)
  SetMusicPitch: procedure(Music: TMusic; pitch: Single); cdecl;
  // Get music time length (in seconds)
  GetMusicTimeLength: function(Music: TMusic): Single; cdecl;
  // Get current music time played (in seconds)
  GetMusicTimePlayed: function(Music: TMusic): Single; cdecl;

  { AudioStream management functions }

  // Init audio stream (to stream raw audio pcm data)
  InitAudioStream: function(SampleRate: Cardinal; SampleSize: Cardinal; Channels: Cardinal): TAudioStream; cdecl;
  // Update audio stream buffers with data
  UpdateAudioStream: procedure(Stream: TAudioStream; const Data: Pointer; SamplesCount: Integer); cdecl;
  // Close audio stream and free memory
  CloseAudioStream: procedure(stream: TAudioStream); cdecl;
  // Check if any audio stream buffers requires refill
  IsAudioStreamProcessed: function(Stream: TAudioStream): Boolean; cdecl;
  // Play audio stream
  PlayAudioStream: procedure(stream: TAudioStream); cdecl;
  // Pause audio stream
  PauseAudioStream: procedure(stream: TAudioStream); cdecl;
  // Resume audio stream
  ResumeAudioStream: procedure(stream: TAudioStream); cdecl;
  // Check if audio stream is playing
  IsAudioStreamPlaying: function(stream: TAudioStream): Boolean; cdecl;
  // Stop audio stream
  StopAudioStream: procedure(stream: TAudioStream); cdecl;
  // Set volume for audio stream (1.0 is max level)
  SetAudioStreamVolume: procedure(stream: TAudioStream; volume: Single); cdecl;
  // Set pitch for audio stream (1.0 is base level)
  SetAudioStreamPitch: procedure(stream: TAudioStream; pitch: Single); cdecl;
  // Default size for new audio streams
  SetAudioStreamBufferSizeDefault: procedure(size: Integer); cdecl;

//------------------------------------------------------------------------------------
// Network (Module: network)
//------------------------------------------------------------------------------------

// IN PROGRESS: Check rnet.h for reference

{*
  Load library dynamically, use RayLib.Load to load it
*}

type

  { TmncRayLib }

  TmncRayLib = class(TmnLibrary)
  public
  protected
    procedure Link; override;
  end;

var
  RayLib: TmncRayLib = nil;

function Vector2Of(X, Y: Single): TVector2;

implementation

function Vector2Of(X, Y: Single): TVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

{ TColor }

class operator TColor.Explicit(a: TRGBAColor): TColor;
begin
  Result.RGBA := a;
end;

class operator TColor.Explicit(a: Cardinal): TColor;
begin
  Result.Value := a;
end;

class operator TColor.Equal(a, b: TColor): Boolean;
begin
  Result := a.Value = b.Value;
end;

class operator TColor.Implicit(a: TRGBAColor): TColor;
begin
  Result.RGBA := a;
end;

class operator TColor.Implicit(a: Cardinal): TColor;
begin
  Result.Value := a;
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

function TColorHelper.Alpha(AAlpha: Byte): TColor;
begin
  Result := Self;
  Result.RGBA.Alpha := AAlpha;
end;

constructor TColorHelper.Create(ARed, AGreen, ABlue, AAlpha: Byte);
var
  aRGBAColor: TRGBAColor;
  aColor: TColor absolute aRGBAColor;
begin
  with aRGBAColor do
  begin
    Red := ARed;
    Green := AGreen;
    Blue := ABlue;
    Alpha := AAlpha;
  end;
  Self := aColor;
end;

constructor TColorHelper.Create(RGBAColor: TRGBAColor);
var
  aColor: TColor absolute RGBAColor;
begin
  Self := aColor;
end;

constructor TColorHelper.Create(AValue: Cardinal);
begin
  {$ifdef ENDIAN_LITTLE}
  Self := AValue;
  {$else}
  Self := AValue; //TODO Swap it SwapEndian()
  {$endif}
end;

constructor TColorHelper.CreateRGBA(RGBA: Cardinal);
begin
  {$ifdef ENDIAN_LITTLE}
  Self := SwapEndian(RGBA);
  {$else}
  Self := RGBA; //TODO Swap it SwapEndian()
  {$endif}
end;

{ TVector2 }

constructor TVector2.Create(AX, AY: Single);
begin
  X := AX;
  Y := AY;
end;

constructor TVector2.Create(I: Int64);
begin
  Self := TVector2(i);
end;

{ TmncRayLib }

procedure TmncRayLib.Link;
begin
  RaiseError := True; //Raise error of one of this functions not exists
  InitWindow := GetAddress('InitWindow');
  WindowShouldClose := GetAddress('WindowShouldClose');
  CloseWindow := GetAddress('CloseWindow');
  IsWindowReady := GetAddress('IsWindowReady');
  IsWindowFullscreen := GetAddress('IsWindowFullscreen');
  IsWindowHidden := GetAddress('IsWindowHidden');
  IsWindowMinimized := GetAddress('IsWindowMinimized');
  IsWindowMaximized := GetAddress('IsWindowMaximized');
  IsWindowResized := GetAddress('IsWindowResized');
  IsWindowState := GetAddress('IsWindowState');
  SetWindowState := GetAddress('SetWindowState');
  ClearWindowState := GetAddress('ClearWindowState');
  ToggleFullscreen := GetAddress('ToggleFullscreen');
  MaximizeWindow := GetAddress('MaximizeWindow');
  MinimizeWindow := GetAddress('MinimizeWindow');
  RestoreWindow := GetAddress('RestoreWindow');
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
  GetMonitorPosition := GetAddress('GetMonitorPosition');
  GetMonitorWidth := GetAddress('GetMonitorWidth');
  GetMonitorHeight := GetAddress('GetMonitorHeight');
  GetMonitorPhysicalWidth := GetAddress('GetMonitorPhysicalWidth');
  GetMonitorPhysicalHeight := GetAddress('GetMonitorPhysicalHeight');
  GetMonitorRefreshRate := GetAddress('GetMonitorRefreshRate');
  GetWindowPosition := GetAddress('GetWindowPosition');
  GetMonitorName := GetAddress('GetMonitorName');
  GetWindowScaleDPI := GetAddress('GetWindowScaleDPI');
  SetClipboardText := GetAddress('SetClipboardText');
  GetClipboardText := GetAddress('GetClipboardText');
  ShowCursor := GetAddress('ShowCursor');
  HideCursor := GetAddress('HideCursor');
  IsCursorHidden := GetAddress('IsCursorHidden');
  EnableCursor := GetAddress('EnableCursor');
  DisableCursor := GetAddress('DisableCursor');
  IsCursorOnScreen := GetAddress('IsCursorOnScreen');
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
  ColorAlpha := GetAddress('ColorAlpha');
  ColorAlphaBlend := GetAddress('ColorAlphaBlend');
  GetColor := GetAddress('GetColor');
  Fade := GetAddress('Fade');
  SetConfigFlags := GetAddress('SetConfigFlags');
  SetTraceLogLevel := GetAddress('SetTraceLogLevel');
  SetTraceLogExit := GetAddress('SetTraceLogExit');
  SetTraceLogCallback := GetAddress('SetTraceLogCallback');
  TraceLog := GetAddress('TraceLog');
  MemAlloc := GetAddress('MemAlloc');
  MemFree := GetAddress('MemFree');
  TakeScreenshot := GetAddress('TakeScreenshot');
  GetRandomValue := GetAddress('GetRandomValue');
  LoadFileData := GetAddress('LoadFileData');
  UnloadFileData := GetAddress('UnloadFileData');
  SaveFileData := GetAddress('SaveFileData');
  LoadFileText := GetAddress('LoadFileText');
  UnloadFileText := GetAddress('UnloadFileText');
  SaveFileText := GetAddress('SaveFileText');
  FileExists := GetAddress('FileExists');
  IsFileExtension := GetAddress('IsFileExtension');
  DirectoryExists := GetAddress('DirectoryExists');
  GetFileExtension := GetAddress('GetFileExtension');
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
  GetCharPressed := GetAddress('GetCharPressed');
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
  GetMouseCursor := GetAddress('GetMouseCursor');
  SetMouseCursor := GetAddress('SetMouseCursor');
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
  CheckCollisionLines := GetAddress('CheckCollisionLines');
  GetCollisionRec := GetAddress('GetCollisionRec');
  LoadImage := GetAddress('LoadImage');
  LoadImageRaw := GetAddress('LoadImageRaw');
  LoadImageAnim := GetAddress('LoadImageAnim');
  LoadImageFromMemory := GetAddress('LoadImageFromMemory');
  UnloadImage := GetAddress('UnloadImage');
  ExportImage := GetAddress('ExportImage');
  ExportImageAsCode := GetAddress('ExportImageAsCode');
  LoadImageColors := GetAddress('LoadImageColors');
  LoadImagePalette := GetAddress('LoadImagePalette');
  UnloadImageColors := GetAddress('UnloadImageColors');
  UnloadImagePalette := GetAddress('UnloadImagePalette');
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
  ImageCrop := GetAddress('ImageCrop');
  ImageAlphaCrop := GetAddress('ImageAlphaCrop');
  ImageAlphaClear := GetAddress('ImageAlphaClear');
  ImageAlphaMask := GetAddress('ImageAlphaMask');
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
  UpdateTextureRec := GetAddress('UpdateTextureRec');
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
  DrawTextureTiled := GetAddress('DrawTextureTiled');
  DrawTexturePro := GetAddress('DrawTexturePro');
  DrawTextureNPatch := GetAddress('DrawTextureNPatch');
  GetPixelColor := GetAddress('GetPixelColor');
  SetPixelColor := GetAddress('SetPixelColor');
  GetPixelDataSize := GetAddress('GetPixelDataSize');
  GetFontDefault := GetAddress('GetFontDefault');
  LoadFont := GetAddress('LoadFont');
  LoadFontEx := GetAddress('LoadFontEx');
  LoadFontFromImage := GetAddress('LoadFontFromImage');
  LoadFontFromMemory := GetAddress('LoadFontFromMemory');
  LoadFontData := GetAddress('LoadFontData');
  GenImageFontAtlas := GetAddress('GenImageFontAtlas');
  UnloadFontData := GetAddress('UnloadFontData');
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
  DrawTriangle3D := GetAddress('DrawTriangle3D');
  DrawTriangleStrip3D := GetAddress('DrawTriangleStrip3D');
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
  UnloadModelKeepMeshes := GetAddress('UnloadModelKeepMeshes');
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
  MeshNormalsSmooth := GetAddress('MeshNormalsSmooth');
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
  GetCollisionRayMesh := GetAddress('GetCollisionRayMesh');
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
  GetShaderLocationAttrib := GetAddress('GetShaderLocationAttrib');
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
  LoadWaveFromMemory := GetAddress('LoadWaveFromMemory');
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
  LoadWaveSamples := GetAddress('LoadWaveSamples');
  UnloadWaveSamples := GetAddress('UnloadWaveSamples');
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
  GetMusicTimeLength := GetAddress('GetMusicTimeLength');
  GetMusicTimePlayed := GetAddress('GetMusicTimePlayed');
  InitAudioStream := GetAddress('InitAudioStream');
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

