unit RayLib3;
{$mode Delphi}{$H+}
{*
*
*
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
*
*}
uses
  SysUtils;

type
  PPUTF8Char = ^PUTF8Char;

const
  cRaylib = 'raylib.dll';

(*
const
#define LIGHTGRAY  CLITERAL(Color){ 200, 200, 200, 255 }   // Light Gray
#define GRAY       CLITERAL(Color){ 130, 130, 130, 255 }   // Gray
#define DARKGRAY   CLITERAL(Color){ 80, 80, 80, 255 }      // Dark Gray
#define YELLOW     CLITERAL(Color){ 253, 249, 0, 255 }     // Yellow
#define GOLD       CLITERAL(Color){ 255, 203, 0, 255 }     // Gold
#define ORANGE     CLITERAL(Color){ 255, 161, 0, 255 }     // Orange
#define PINK       CLITERAL(Color){ 255, 109, 194, 255 }   // Pink
#define RED        CLITERAL(Color){ 230, 41, 55, 255 }     // Red
#define MAROON     CLITERAL(Color){ 190, 33, 55, 255 }     // Maroon
#define GREEN      CLITERAL(Color){ 0, 228, 48, 255 }      // Green
#define LIME       CLITERAL(Color){ 0, 158, 47, 255 }      // Lime
#define DARKGREEN  CLITERAL(Color){ 0, 117, 44, 255 }      // Dark Green
#define SKYBLUE    CLITERAL(Color){ 102, 191, 255, 255 }   // Sky Blue
#define BLUE       CLITERAL(Color){ 0, 121, 241, 255 }     // Blue
#define DARKBLUE   CLITERAL(Color){ 0, 82, 172, 255 }      // Dark Blue
#define PURPLE     CLITERAL(Color){ 200, 122, 255, 255 }   // Purple
#define VIOLET     CLITERAL(Color){ 135, 60, 190, 255 }    // Violet
#define DARKPURPLE CLITERAL(Color){ 112, 31, 126, 255 }    // Dark Purple
#define BEIGE      CLITERAL(Color){ 211, 176, 131, 255 }   // Beige
#define BROWN      CLITERAL(Color){ 127, 106, 79, 255 }    // Brown
#define DARKBROWN  CLITERAL(Color){ 76, 63, 47, 255 }      // Dark Brown

#define WHITE      CLITERAL(Color){ 255, 255, 255, 255 }   // White
#define BLACK      CLITERAL(Color){ 0, 0, 0, 255 }         // Black
#define BLANK      CLITERAL(Color){ 0, 0, 0, 0 }           // Blank (Transparent)
#define MAGENTA    CLITERAL(Color){ 255, 0, 255, 255 }     // Magenta
#define RAYWHITE   CLITERAL(Color){ 245, 245, 245, 255 }   // My own White (raylib logo)
}
*)

type
  TColor = DWORD;
  PColor = ^TColor;

  // Vector2 type
  TVector2 = packed record
    x: Single;
    y: Single;
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

  // Color type, RGBA (32bit)
  TRayColor = record
//    Color: DWORD;
      r: Byte;
      g: Byte;
      b: Byte;
      a: Byte;
  end;

  // Rectangle type
  TRectangle = packed record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
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
procedure InitWindow(width: Integer; height: Integer; const title: PUTF8Char); cdecl; external cRaylib name 'InitWindow';
// Check if KEY_ESCAPE pressed or Close icon pressed
function WindowShouldClose(): boolean; cdecl; external cRaylib name 'WindowShouldClose';
procedure CloseWindow; cdecl; external cRaylib name 'CloseWindow';
function IsWindowReady(): boolean; cdecl; external cRaylib name 'IsWindowReady';
function IsWindowMinimized(): boolean; cdecl; external cRaylib name 'IsWindowMinimized';
function IsWindowResized(): boolean; cdecl; external cRaylib name 'IsWindowResized';
function IsWindowHidden(): boolean; cdecl; external cRaylib name 'IsWindowHidden';
function IsWindowFullscreen(): boolean; cdecl; external cRaylib name 'IsWindowFullscreen';
procedure ToggleFullscreen; cdecl; external cRaylib name 'ToggleFullscreen';
procedure UnhideWindow; cdecl; external cRaylib name 'UnhideWindow';
procedure HideWindow; cdecl; external cRaylib name 'HideWindow';
procedure SetWindowIcon(image: TImage); cdecl; external cRaylib name 'SetWindowIcon';
procedure SetWindowTitle(const title: PUTF8Char); cdecl; external cRaylib name 'SetWindowTitle';
procedure SetWindowPosition(x: Integer; y: Integer); cdecl; external cRaylib name 'SetWindowPosition';
procedure SetWindowMonitor(monitor: Integer); cdecl; external cRaylib name 'SetWindowMonitor';
procedure SetWindowMinSize(width: Integer; height: Integer); cdecl; external cRaylib name 'SetWindowMinSize';
procedure SetWindowSize(width: Integer; height: Integer); cdecl; external cRaylib name 'SetWindowSize';
function GetWindowHandle(): Pointer; cdecl; external cRaylib name 'GetWindowHandle';
function GetScreenWidth(): Integer; cdecl; external cRaylib name 'GetScreenWidth';
function GetScreenHeight(): Integer; cdecl; external cRaylib name 'GetScreenHeight';
function GetMonitorCount(): Integer; cdecl; external cRaylib name 'GetMonitorCount';
function GetMonitorWidth(monitor: Integer): Integer; cdecl; external cRaylib name 'GetMonitorWidth';
function GetMonitorHeight(monitor: Integer): Integer; cdecl; external cRaylib name 'GetMonitorHeight';
function GetMonitorPhysicalWidth(monitor: Integer): Integer; cdecl; external cRaylib name 'GetMonitorPhysicalWidth';
function GetMonitorPhysicalHeight(monitor: Integer): Integer; cdecl; external cRaylib name 'GetMonitorPhysicalHeight';
function GetWindowPosition(): TVector2; cdecl; external cRaylib name 'GetWindowPosition';
function GetMonitorName(monitor: Integer): PUTF8Char; cdecl; external cRaylib name 'GetMonitorName';
function GetClipboardText(): PUTF8Char; cdecl; external cRaylib name 'GetClipboardText';
procedure SetClipboardText(const text: PUTF8Char); cdecl; external cRaylib name 'SetClipboardText';

// Cursor-related functions
procedure ShowCursor; cdecl; external cRaylib name 'ShowCursor';
procedure HideCursor; cdecl; external cRaylib name 'HideCursor';
function IsCursorHidden(): boolean; cdecl; external cRaylib name 'IsCursorHidden';
procedure EnableCursor; cdecl; external cRaylib name 'EnableCursor';
procedure DisableCursor; cdecl; external cRaylib name 'DisableCursor';

// Drawing-related functions
procedure ClearBackground(color: TColor); cdecl; external cRaylib name 'ClearBackground';
procedure BeginDrawing; cdecl; external cRaylib name 'BeginDrawing';
procedure EndDrawing; cdecl; external cRaylib name 'EndDrawing';
procedure BeginMode2D(camera: TCamera2D); cdecl; external cRaylib name 'BeginMode2D';
procedure EndMode2D; cdecl; external cRaylib name 'EndMode2D';
procedure BeginMode3D(camera: TCamera3D); cdecl; external cRaylib name 'BeginMode3D';
procedure EndMode3D; cdecl; external cRaylib name 'EndMode3D';
procedure BeginTextureMode(target: TRenderTexture2D); cdecl; external cRaylib name 'BeginTextureMode';
procedure EndTextureMode; cdecl; external cRaylib name 'EndTextureMode';
procedure BeginScissorMode(x: Integer; y: Integer; width: Integer; height: Integer); cdecl; external cRaylib name 'BeginScissorMode';
procedure EndScissorMode; cdecl; external cRaylib name 'EndScissorMode';

// Screen-space-related functions
function GetMouseRay(mousePosition: TVector2; camera: TCamera): TRay; cdecl; external cRaylib name 'GetMouseRay';
function GetCameraMatrix(camera: TCamera): TMatrix; cdecl; external cRaylib name 'GetCameraMatrix';
function GetCameraMatrix2D(camera: TCamera2D): TMatrix; cdecl; external cRaylib name 'GetCameraMatrix2D';
function GetWorldToScreen(position: TVector3; camera: TCamera): TVector2; cdecl; external cRaylib name 'GetWorldToScreen';
function GetWorldToScreenEx(position: TVector3; camera: TCamera; width: Integer; height: Integer): TVector2; cdecl; external cRaylib name 'GetWorldToScreenEx';
function GetWorldToScreen2D(position: TVector2; camera: TCamera2D): TVector2; cdecl; external cRaylib name 'GetWorldToScreen2D';
function GetScreenToWorld2D(position: TVector2; camera: TCamera2D): TVector2; cdecl; external cRaylib name 'GetScreenToWorld2D';

// Timing-related functions
procedure SetTargetFPS(fps: Integer); cdecl; external cRaylib name 'SetTargetFPS';
function GetFPS(): Integer; cdecl; external cRaylib name 'GetFPS';
function GetFrameTime(): Single; cdecl; external cRaylib name 'GetFrameTime';
function GetTime(): Double; cdecl; external cRaylib name 'GetTime';

// Color-related functions
function ColorToInt(color: TColor): Integer; cdecl; external cRaylib name 'ColorToInt';
function ColorNormalize(color: TColor): TVector4; cdecl; external cRaylib name 'ColorNormalize';
function ColorFromNormalized(normalized: TVector4): TColor; cdecl; external cRaylib name 'ColorFromNormalized';
function ColorToHSV(color: TColor): TVector3; cdecl; external cRaylib name 'ColorToHSV';
function ColorFromHSV(hsv: TVector3): TColor; cdecl; external cRaylib name 'ColorFromHSV';
function GetColor(hexValue: Integer): TColor; cdecl; external cRaylib name 'GetColor';
function Fade(color: TColor; Alpha: Single): TColor; cdecl; external cRaylib name 'Fade';

// Misc. functions
procedure SetConfigFlags(flags: Cardinal); cdecl; external cRaylib name 'SetConfigFlags';
procedure SetTraceLogLevel(logType: Integer); cdecl; external cRaylib name 'SetTraceLogLevel';
procedure SetTraceLogExit(logType: Integer); cdecl; external cRaylib name 'SetTraceLogExit';
procedure SetTraceLogCallback(callback: TTraceLogCallback); cdecl; external cRaylib name 'SetTraceLogCallback';
procedure TraceLog(logType: Integer; const text: PUTF8Char) varargs; cdecl; external cRaylib name 'TraceLog';
procedure TakeScreenshot(const fileName: PUTF8Char); cdecl; external cRaylib name 'TakeScreenshot';
function GetRandomValue(min: Integer; max: Integer): Integer; cdecl; external cRaylib name 'GetRandomValue';

// Files management functions
function LoadFileData(const fileName: PUTF8Char; bytesRead: PCardinal): PByte; cdecl; external cRaylib name 'LoadFileData';
procedure SaveFileData(const fileName: PUTF8Char; data: Pointer; bytesToWrite: Cardinal); cdecl; external cRaylib name 'SaveFileData';
function LoadFileText(const fileName: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'LoadFileText';
procedure SaveFileText(const fileName: PUTF8Char; text: PUTF8Char); cdecl; external cRaylib name 'SaveFileText';
function FileExists(const fileName: PUTF8Char): boolean; cdecl; external cRaylib name 'FileExists';
function IsFileExtension(const fileName: PUTF8Char; const ext: PUTF8Char): boolean; cdecl; external cRaylib name 'IsFileExtension';
function DirectoryExists(const dirPath: PUTF8Char): boolean; cdecl; external cRaylib name 'DirectoryExists';
function GetExtension(const fileName: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'GetExtension';
function GetFileName(const filePath: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'GetFileName';
function GetFileNameWithoutExt(const filePath: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'GetFileNameWithoutExt';
function GetDirectoryPath(const filePath: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'GetDirectoryPath';
function GetPrevDirectoryPath(const dirPath: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'GetPrevDirectoryPath';
function GetWorkingDirectory(): PUTF8Char; cdecl; external cRaylib name 'GetWorkingDirectory';
function GetDirectoryFiles(const dirPath: PUTF8Char; count: PInteger): PPUTF8Char; cdecl; external cRaylib name 'GetDirectoryFiles';
procedure ClearDirectoryFiles; cdecl; external cRaylib name 'ClearDirectoryFiles';
function ChangeDirectory(const dir: PUTF8Char): boolean; cdecl; external cRaylib name 'ChangeDirectory';
function IsFileDropped(): boolean; cdecl; external cRaylib name 'IsFileDropped';
function GetDroppedFiles(count: PInteger): PPUTF8Char; cdecl; external cRaylib name 'GetDroppedFiles';
procedure ClearDroppedFiles; cdecl; external cRaylib name 'ClearDroppedFiles';
function GetFileModTime(const fileName: PUTF8Char): Integer; cdecl; external cRaylib name 'GetFileModTime';

function CompressData(data: PByte; dataLength: Integer; compDataLength: PInteger): PByte; cdecl; external cRaylib name 'CompressData';
function DecompressData(compData: PByte; compDataLength: Integer; dataLength: PInteger): PByte; cdecl; external cRaylib name 'DecompressData';

// Persistent storage management
procedure SaveStorageValue(position: Cardinal; value: Integer); cdecl; external cRaylib name 'SaveStorageValue';
function LoadStorageValue(position: Cardinal): Integer; cdecl; external cRaylib name 'LoadStorageValue';

procedure OpenURL(const url: PUTF8Char); cdecl; external cRaylib name 'OpenURL';

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

// Input-related functions: keyboard
function IsKeyPressed(key: Integer): boolean; cdecl; external cRaylib name 'IsKeyPressed';
function IsKeyDown(key: Integer): boolean; cdecl; external cRaylib name 'IsKeyDown';
function IsKeyReleased(key: Integer): boolean; cdecl; external cRaylib name 'IsKeyReleased';

// Input-related functions: gamepads
function IsKeyUp(key: Integer): boolean; cdecl; external cRaylib name 'IsKeyUp';
procedure SetExitKey(key: Integer); cdecl; external cRaylib name 'SetExitKey';
function GetKeyPressed(): Integer; cdecl; external cRaylib name 'GetKeyPressed';
function IsGamepadAvailable(gamepad: Integer): boolean; cdecl; external cRaylib name 'IsGamepadAvailable';
function IsGamepadName(gamepad: Integer; const name: PUTF8Char): boolean; cdecl; external cRaylib name 'IsGamepadName';
function GetGamepadName(gamepad: Integer): PUTF8Char; cdecl; external cRaylib name 'GetGamepadName';
function IsGamepadButtonPressed(gamepad: Integer; button: Integer): boolean; cdecl; external cRaylib name 'IsGamepadButtonPressed';
function IsGamepadButtonDown(gamepad: Integer; button: Integer): boolean; cdecl; external cRaylib name 'IsGamepadButtonDown';
function IsGamepadButtonReleased(gamepad: Integer; button: Integer): boolean; cdecl; external cRaylib name 'IsGamepadButtonReleased';
function IsGamepadButtonUp(gamepad: Integer; button: Integer): boolean; cdecl; external cRaylib name 'IsGamepadButtonUp';
function GetGamepadButtonPressed(): Integer; cdecl; external cRaylib name 'GetGamepadButtonPressed';
function GetGamepadAxisCount(gamepad: Integer): Integer; cdecl; external cRaylib name 'GetGamepadAxisCount';
function GetGamepadAxisMovement(gamepad: Integer; axis: Integer): Single; cdecl; external cRaylib name 'GetGamepadAxisMovement';

// Input-related functions: mouse
function IsMouseButtonPressed(button: Integer): boolean; cdecl; external cRaylib name 'IsMouseButtonPressed';
function IsMouseButtonDown(button: Integer): boolean; cdecl; external cRaylib name 'IsMouseButtonDown';
function IsMouseButtonReleased(button: Integer): boolean; cdecl; external cRaylib name 'IsMouseButtonReleased';
function IsMouseButtonUp(button: Integer): boolean; cdecl; external cRaylib name 'IsMouseButtonUp';
function GetMouseX(): Integer; cdecl; external cRaylib name 'GetMouseX';
function GetMouseY(): Integer; cdecl; external cRaylib name 'GetMouseY';
function GetMousePosition(): TVector2; cdecl; external cRaylib name 'GetMousePosition';
procedure SetMousePosition(x: Integer; y: Integer); cdecl; external cRaylib name 'SetMousePosition';
procedure SetMouseOffset(offsetX: Integer; offsetY: Integer); cdecl; external cRaylib name 'SetMouseOffset';
procedure SetMouseScale(scaleX: Single; scaleY: Single); cdecl; external cRaylib name 'SetMouseScale';
function GetMouseWheelMove(): Integer; cdecl; external cRaylib name 'GetMouseWheelMove';

// Input-related functions: touch
function GetTouchX(): Integer; cdecl; external cRaylib name 'GetTouchX';
function GetTouchY(): Integer; cdecl; external cRaylib name 'GetTouchY';
function GetTouchPosition(index: Integer): TVector2; cdecl; external cRaylib name 'GetTouchPosition';

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: gestures)
//------------------------------------------------------------------------------------
procedure SetGesturesEnabled(gestureFlags: Cardinal); cdecl; external cRaylib name 'SetGesturesEnabled';
function IsGestureDetected(gesture: Integer): boolean; cdecl; external cRaylib name 'IsGestureDetected';
function GetGestureDetected(): Integer; cdecl; external cRaylib name 'GetGestureDetected';
function GetTouchPointsCount(): Integer; cdecl; external cRaylib name 'GetTouchPointsCount';
function GetGestureHoldDuration(): Single; cdecl; external cRaylib name 'GetGestureHoldDuration';
function GetGestureDragVector(): TVector2; cdecl; external cRaylib name 'GetGestureDragVector';
function GetGestureDragAngle(): Single; cdecl; external cRaylib name 'GetGestureDragAngle';
function GetGesturePinchVector(): TVector2; cdecl; external cRaylib name 'GetGesturePinchVector';
function GetGesturePinchAngle(): Single; cdecl; external cRaylib name 'GetGesturePinchAngle';

//------------------------------------------------------------------------------------
// Camera System Functions (Module: camera)
//------------------------------------------------------------------------------------
procedure SetCameraMode(camera: TCamera; mode: Integer); cdecl; external cRaylib name 'SetCameraMode';
procedure UpdateCamera(camera: PCamera); cdecl; external cRaylib name 'UpdateCamera';

procedure SetCameraPanControl(panKey: Integer); cdecl; external cRaylib name 'SetCameraPanControl';
procedure SetCameraAltControl(altKey: Integer); cdecl; external cRaylib name 'SetCameraAltControl';
procedure SetCameraSmoothZoomControl(szKey: Integer); cdecl; external cRaylib name 'SetCameraSmoothZoomControl';
procedure SetCameraMoveControls(frontKey: Integer; backKey: Integer; rightKey: Integer; leftKey: Integer; upKey: Integer; downKey: Integer); cdecl; external cRaylib name 'SetCameraMoveControls';

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------

// Basic shapes drawing functions
procedure DrawPixel(posX: Integer; posY: Integer; color: TColor); cdecl; external cRaylib name 'DrawPixel';
procedure DrawPixelV(position: TVector2; color: TColor); cdecl; external cRaylib name 'DrawPixelV';
procedure DrawLine(startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl; external cRaylib name 'DrawLine';
procedure DrawLineV(startPos: TVector2; endPos: TVector2; color: TColor); cdecl; external cRaylib name 'DrawLineV';
procedure DrawLineEx(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl; external cRaylib name 'DrawLineEx';
procedure DrawLineBezier(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl; external cRaylib name 'DrawLineBezier';
procedure DrawLineStrip(points: PVector2; numPoints: Integer; color: TColor); cdecl; external cRaylib name 'DrawLineStrip';
procedure DrawCircle(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl; external cRaylib name 'DrawCircle';
procedure DrawCircleSector(center: TVector2; radius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl; external cRaylib name 'DrawCircleSector';
procedure DrawCircleSectorLines(center: TVector2; radius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl; external cRaylib name 'DrawCircleSectorLines';
procedure DrawCircleGradient(centerX: Integer; centerY: Integer; radius: Single; color1: TColor; color2: TColor); cdecl; external cRaylib name 'DrawCircleGradient';
procedure DrawCircleV(center: TVector2; radius: Single; color: TColor); cdecl; external cRaylib name 'DrawCircleV';
procedure DrawCircleLines(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl; external cRaylib name 'DrawCircleLines';
procedure DrawEllipse(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl; external cRaylib name 'DrawEllipse';
procedure DrawEllipseLines(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl; external cRaylib name 'DrawEllipseLines';
procedure DrawRing(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl; external cRaylib name 'DrawRing';
procedure DrawRingLines(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Integer; endAngle: Integer; segments: Integer; color: TColor); cdecl; external cRaylib name 'DrawRingLines';
procedure DrawRectangle(posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl; external cRaylib name 'DrawRectangle';
procedure DrawRectangleV(position: TVector2; size: TVector2; color: TColor); cdecl; external cRaylib name 'DrawRectangleV';
procedure DrawRectangleRec(rec: TRectangle; color: TColor); cdecl; external cRaylib name 'DrawRectangleRec';
procedure DrawRectanglePro(rec: TRectangle; origin: TVector2; rotation: Single; color: TColor); cdecl; external cRaylib name 'DrawRectanglePro';
procedure DrawRectangleGradientV(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl; external cRaylib name 'DrawRectangleGradientV';
procedure DrawRectangleGradientH(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl; external cRaylib name 'DrawRectangleGradientH';
procedure DrawRectangleGradientEx(rec: TRectangle; col1: TColor; col2: TColor; col3: TColor; col4: TColor); cdecl; external cRaylib name 'DrawRectangleGradientEx';
procedure DrawRectangleLines(posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl; external cRaylib name 'DrawRectangleLines';
procedure DrawRectangleLinesEx(rec: TRectangle; lineThick: Integer; color: TColor); cdecl; external cRaylib name 'DrawRectangleLinesEx';
procedure DrawRectangleRounded(rec: TRectangle; roundness: Single; segments: Integer; color: TColor); cdecl; external cRaylib name 'DrawRectangleRounded';
procedure DrawRectangleRoundedLines(rec: TRectangle; roundness: Single; segments: Integer; lineThick: Integer; color: TColor); cdecl; external cRaylib name 'DrawRectangleRoundedLines';
procedure DrawTriangle(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl; external cRaylib name 'DrawTriangle';
procedure DrawTriangleLines(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl; external cRaylib name 'DrawTriangleLines';
procedure DrawTriangleFan(points: PVector2; numPoints: Integer; color: TColor); cdecl; external cRaylib name 'DrawTriangleFan';
procedure DrawTriangleStrip(points: PVector2; pointsCount: Integer; color: TColor); cdecl; external cRaylib name 'DrawTriangleStrip';
procedure DrawPoly(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl; external cRaylib name 'DrawPoly';
procedure DrawPolyLines(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl; external cRaylib name 'DrawPolyLines';

// Basic shapes collision detection functions
function CheckCollisionRecs(rec1: TRectangle; rec2: TRectangle): boolean; cdecl; external cRaylib name 'CheckCollisionRecs';
function CheckCollisionCircles(center1: TVector2; radius1: Single; center2: TVector2; radius2: Single): boolean; cdecl; external cRaylib name 'CheckCollisionCircles';
function CheckCollisionCircleRec(center: TVector2; radius: Single; rec: TRectangle): boolean; cdecl; external cRaylib name 'CheckCollisionCircleRec';
function GetCollisionRec(rec1: TRectangle; rec2: TRectangle): TRectangle; cdecl; external cRaylib name 'GetCollisionRec';
function CheckCollisionPointRec(point: TVector2; rec: TRectangle): boolean; cdecl; external cRaylib name 'CheckCollisionPointRec';
function CheckCollisionPointCircle(point: TVector2; center: TVector2; radius: Single): boolean; cdecl; external cRaylib name 'CheckCollisionPointCircle';
function CheckCollisionPointTriangle(point: TVector2; p1: TVector2; p2: TVector2; p3: TVector2): boolean; cdecl; external cRaylib name 'CheckCollisionPointTriangle';

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

// Image loading functions
// NOTE: This functions do not require GPU access
function LoadImage(const fileName: PUTF8Char): TImage; cdecl; external cRaylib name 'LoadImage';
function LoadImageEx(pixels: PColor; width: Integer; height: Integer): TImage; cdecl; external cRaylib name 'LoadImageEx';
function LoadImagePro(data: Pointer; width: Integer; height: Integer; format: Integer): TImage; cdecl; external cRaylib name 'LoadImagePro';
function LoadImageRaw(const fileName: PUTF8Char; width: Integer; height: Integer; format: Integer; headerSize: Integer): TImage; cdecl; external cRaylib name 'LoadImageRaw';
procedure UnloadImage(image: TImage); cdecl; external cRaylib name 'UnloadImage';
procedure ExportImage(image: TImage; const fileName: PUTF8Char); cdecl; external cRaylib name 'ExportImage';
procedure ExportImageAsCode(image: TImage; const fileName: PUTF8Char); cdecl; external cRaylib name 'ExportImageAsCode';
function GetImageData(image: TImage): PColor; cdecl; external cRaylib name 'GetImageData';
function GetImageDataNormalized(image: TImage): PVector4; cdecl; external cRaylib name 'GetImageDataNormalized';

// Image generation functions
function GenImageColor(width: Integer; height: Integer; color: TColor): TImage; cdecl; external cRaylib name 'GenImageColor';
function GenImageGradientV(width: Integer; height: Integer; top: TColor; bottom: TColor): TImage; cdecl; external cRaylib name 'GenImageGradientV';
function GenImageGradientH(width: Integer; height: Integer; left: TColor; right: TColor): TImage; cdecl; external cRaylib name 'GenImageGradientH';
function GenImageGradientRadial(width: Integer; height: Integer; density: Single; inner: TColor; outer: TColor): TImage; cdecl; external cRaylib name 'GenImageGradientRadial';
function GenImageChecked(width: Integer; height: Integer; checksX: Integer; checksY: Integer; col1: TColor; col2: TColor): TImage; cdecl; external cRaylib name 'GenImageChecked';
function GenImageWhiteNoise(width: Integer; height: Integer; factor: Single): TImage; cdecl; external cRaylib name 'GenImageWhiteNoise';
function GenImagePerlinNoise(width: Integer; height: Integer; offsetX: Integer; offsetY: Integer; scale: Single): TImage; cdecl; external cRaylib name 'GenImagePerlinNoise';
function GenImageCellular(width: Integer; height: Integer; tileSize: Integer): TImage; cdecl; external cRaylib name 'GenImageCellular';

// Image manipulation functions
function ImageCopy(image: TImage): TImage; cdecl; external cRaylib name 'ImageCopy';
function ImageFromImage(image: TImage; rec: TRectangle): TImage; cdecl; external cRaylib name 'ImageFromImage';
function ImageText(const text: PUTF8Char; fontSize: Integer; color: TColor): TImage; cdecl; external cRaylib name 'ImageText';
function ImageTextEx(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single; tint: TColor): TImage; cdecl; external cRaylib name 'ImageTextEx';
procedure ImageToPOT(image: PImage; fillColor: TColor); cdecl; external cRaylib name 'ImageToPOT';
procedure ImageFormat(image: PImage; newFormat: Integer); cdecl; external cRaylib name 'ImageFormat';
procedure ImageAlphaMask(image: PImage; alphaMask: TImage); cdecl; external cRaylib name 'ImageAlphaMask';
procedure ImageAlphaClear(image: PImage; color: TColor; threshold: Single); cdecl; external cRaylib name 'ImageAlphaClear';
procedure ImageAlphaCrop(image: PImage; threshold: Single); cdecl; external cRaylib name 'ImageAlphaCrop';
procedure ImageAlphaPremultiply(image: PImage); cdecl; external cRaylib name 'ImageAlphaPremultiply';
procedure ImageCrop(image: PImage; crop: TRectangle); cdecl; external cRaylib name 'ImageCrop';
procedure ImageResize(image: PImage; newWidth: Integer; newHeight: Integer); cdecl; external cRaylib name 'ImageResize';
procedure ImageResizeNN(image: PImage; newWidth: Integer; newHeight: Integer); cdecl; external cRaylib name 'ImageResizeNN';
procedure ImageResizeCanvas(image: PImage; newWidth: Integer; newHeight: Integer; offsetX: Integer; offsetY: Integer; color: TColor); cdecl; external cRaylib name 'ImageResizeCanvas';
procedure ImageMipmaps(image: PImage); cdecl; external cRaylib name 'ImageMipmaps';
procedure ImageDither(image: PImage; rBpp: Integer; gBpp: Integer; bBpp: Integer; aBpp: Integer); cdecl; external cRaylib name 'ImageDither';
procedure ImageFlipVertical(image: PImage); cdecl; external cRaylib name 'ImageFlipVertical';
procedure ImageFlipHorizontal(image: PImage); cdecl; external cRaylib name 'ImageFlipHorizontal';
procedure ImageRotateCW(image: PImage); cdecl; external cRaylib name 'ImageRotateCW';
procedure ImageRotateCCW(image: PImage); cdecl; external cRaylib name 'ImageRotateCCW';
procedure ImageColorTint(image: PImage; color: TColor); cdecl; external cRaylib name 'ImageColorTint';
procedure ImageColorInvert(image: PImage); cdecl; external cRaylib name 'ImageColorInvert';
procedure ImageColorGrayscale(image: PImage); cdecl; external cRaylib name 'ImageColorGrayscale';
procedure ImageColorContrast(image: PImage; contrast: Single); cdecl; external cRaylib name 'ImageColorContrast';
procedure ImageColorBrightness(image: PImage; brightness: Integer); cdecl; external cRaylib name 'ImageColorBrightness';
procedure ImageColorReplace(image: PImage; color: TColor; replace: TColor); cdecl; external cRaylib name 'ImageColorReplace';
function ImageExtractPalette(image: TImage; maxPaletteSize: Integer; extractCount: PInteger): PColor; cdecl; external cRaylib name 'ImageExtractPalette';
function GetImageAlphaBorder(image: TImage; threshold: Single): TRectangle; cdecl; external cRaylib name 'GetImageAlphaBorder';

// Image drawing functions
// NOTE: Image software-rendering functions (CPU)
procedure ImageClearBackground(dst: PImage; color: TColor); cdecl; external cRaylib name 'ImageClearBackground';
procedure ImageDrawPixel(dst: PImage; posX: Integer; posY: Integer; color: TColor); cdecl; external cRaylib name 'ImageDrawPixel';
procedure ImageDrawPixelV(dst: PImage; position: TVector2; color: TColor); cdecl; external cRaylib name 'ImageDrawPixelV';
procedure ImageDrawLine(dst: PImage; startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl; external cRaylib name 'ImageDrawLine';
procedure ImageDrawLineV(dst: PImage; start: TVector2; &end: TVector2; color: TColor); cdecl; external cRaylib name 'ImageDrawLineV';
procedure ImageDrawCircle(dst: PImage; centerX: Integer; centerY: Integer; radius: Integer; color: TColor); cdecl; external cRaylib name 'ImageDrawCircle';
procedure ImageDrawCircleV(dst: PImage; center: TVector2; radius: Integer; color: TColor); cdecl; external cRaylib name 'ImageDrawCircleV';
procedure ImageDrawRectangle(dst: PImage; posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl; external cRaylib name 'ImageDrawRectangle';
procedure ImageDrawRectangleV(dst: PImage; position: TVector2; size: TVector2; color: TColor); cdecl; external cRaylib name 'ImageDrawRectangleV';
procedure ImageDrawRectangleRec(dst: PImage; rec: TRectangle; color: TColor); cdecl; external cRaylib name 'ImageDrawRectangleRec';
procedure ImageDrawRectangleLines(dst: PImage; rec: TRectangle; thick: Integer; color: TColor); cdecl; external cRaylib name 'ImageDrawRectangleLines';
procedure ImageDraw(dst: PImage; src: TImage; srcRec: TRectangle; dstRec: TRectangle; tint: TColor); cdecl; external cRaylib name 'ImageDraw';
procedure ImageDrawText(dst: PImage; position: TVector2; const text: PUTF8Char; fontSize: Integer; color: TColor); cdecl; external cRaylib name 'ImageDrawText';
procedure ImageDrawTextEx(dst: PImage; position: TVector2; font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single; color: TColor); cdecl; external cRaylib name 'ImageDrawTextEx';

// Texture loading functions
// NOTE: These functions require GPU access
function LoadTexture(const fileName: PUTF8Char): TTexture2D; cdecl; external cRaylib name 'LoadTexture';
function LoadTextureFromImage(image: TImage): TTexture2D; cdecl; external cRaylib name 'LoadTextureFromImage';
function LoadTextureCubemap(image: TImage; layoutType: Integer): TTextureCubemap; cdecl; external cRaylib name 'LoadTextureCubemap';
function LoadRenderTexture(width: Integer; height: Integer): TRenderTexture2D; cdecl; external cRaylib name 'LoadRenderTexture';
procedure UnloadTexture(texture: TTexture2D); cdecl; external cRaylib name 'UnloadTexture';
procedure UnloadRenderTexture(target: TRenderTexture2D); cdecl; external cRaylib name 'UnloadRenderTexture';
procedure UpdateTexture(texture: TTexture2D; const pixels: Pointer); cdecl; external cRaylib name 'UpdateTexture';
function GetTextureData(texture: TTexture2D): TImage; cdecl; external cRaylib name 'GetTextureData';
function GetScreenData(): TImage; cdecl; external cRaylib name 'GetScreenData';

// Texture configuration functions
procedure GenTextureMipmaps(texture: PTexture2D); cdecl; external cRaylib name 'GenTextureMipmaps';
procedure SetTextureFilter(texture: TTexture2D; filterMode: Integer); cdecl; external cRaylib name 'SetTextureFilter';
procedure SetTextureWrap(texture: TTexture2D; wrapMode: Integer); cdecl; external cRaylib name 'SetTextureWrap';

// Texture drawing functions
procedure DrawTexture(texture: TTexture2D; posX: Integer; posY: Integer; tint: TColor); cdecl; external cRaylib name 'DrawTexture';
procedure DrawTextureV(texture: TTexture2D; position: TVector2; tint: TColor); cdecl; external cRaylib name 'DrawTextureV';
procedure DrawTextureEx(texture: TTexture2D; position: TVector2; rotation: Single; scale: Single; tint: TColor); cdecl; external cRaylib name 'DrawTextureEx';
procedure DrawTextureRec(texture: TTexture2D; sourceRec: TRectangle; position: TVector2; tint: TColor); cdecl; external cRaylib name 'DrawTextureRec';
procedure DrawTextureQuad(texture: TTexture2D; tiling: TVector2; offset: TVector2; quad: TRectangle; tint: TColor); cdecl; external cRaylib name 'DrawTextureQuad';
procedure DrawTexturePro(texture: TTexture2D; sourceRec: TRectangle; destRec: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl; external cRaylib name 'DrawTexturePro';
procedure DrawTextureNPatch(texture: TTexture2D; nPatchInfo: TNPatchInfo; destRec: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl; external cRaylib name 'DrawTextureNPatch';

// Image/Texture misc functions
function GetPixelDataSize(width: Integer; height: Integer; format: Integer): Integer; cdecl; external cRaylib name 'GetPixelDataSize';

//------------------------------------------------------------------------------------
// Font Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

// Font loading/unloading functions
function GetFontDefault(): TFont; cdecl; external cRaylib name 'GetFontDefault';
function LoadFont(const fileName: PUTF8Char): TFont; cdecl; external cRaylib name 'LoadFont';
function LoadFontEx(const fileName: PUTF8Char; fontSize: Integer; fontChars: PInteger; charsCount: Integer): TFont; cdecl; external cRaylib name 'LoadFontEx';
function LoadFontFromImage(image: TImage; key: TColor; firstChar: Integer): TFont; cdecl; external cRaylib name 'LoadFontFromImage';
function LoadFontData(const fileName: PUTF8Char; fontSize: Integer; fontChars: PInteger; charsCount: Integer; &type: Integer): PCharInfo; cdecl; external cRaylib name 'LoadFontData';
function GenImageFontAtlas(const chars: PCharInfo; recs: PPRectangle; charsCount: Integer; fontSize: Integer; padding: Integer; packMethod: Integer): TImage; cdecl; external cRaylib name 'GenImageFontAtlas';
procedure UnloadFont(font: TFont); cdecl; external cRaylib name 'UnloadFont';

// Text drawing functions
procedure DrawFPS(posX: Integer; posY: Integer); cdecl; external cRaylib name 'DrawFPS';
procedure DrawText(const text: PUTF8Char; posX: Integer; posY: Integer; fontSize: Integer; color: TColor); cdecl; external cRaylib name 'DrawText';
procedure DrawTextEx(font: TFont; const text: PUTF8Char; position: TVector2; fontSize: Single; spacing: Single; tint: TColor); cdecl; external cRaylib name 'DrawTextEx';
procedure DrawTextRec(font: TFont; const text: PUTF8Char; rec: TRectangle; fontSize: Single; spacing: Single; wordWrap: boolean; tint: TColor); cdecl; external cRaylib name 'DrawTextRec';
procedure DrawTextRecEx(font: TFont; const text: PUTF8Char; rec: TRectangle; fontSize: Single; spacing: Single; wordWrap: boolean; tint: TColor; selectStart: Integer; selectLength: Integer; selectTint: TColor; selectBackTint: TColor); cdecl; external cRaylib name 'DrawTextRecEx';

procedure DrawTextCodepoint(font: TFont; codepoint: Integer; position: TVector2; scale: Single; tint: TColor); cdecl; external cRaylib name 'DrawTextCodepoint';

// Text misc. functions
function MeasureText(const text: PUTF8Char; fontSize: Integer): Integer; cdecl; external cRaylib name 'MeasureText';
function MeasureTextEx(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single): TVector2; cdecl; external cRaylib name 'MeasureTextEx';
function GetGlyphIndex(font: TFont; codepoint: Integer): Integer; cdecl; external cRaylib name 'GetGlyphIndex';

// Text strings management functions (no utf8 strings, only byte chars)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!
function TextCopy(dst: PUTF8Char; const src: PUTF8Char): Integer; cdecl; external cRaylib name 'TextCopy';
function TextIsEqual(const text1: PUTF8Char; const text2: PUTF8Char): boolean; cdecl; external cRaylib name 'TextIsEqual';
function TextLength(const text: PUTF8Char): Cardinal; cdecl; external cRaylib name 'TextLength';
function TextFormat(const text: PUTF8Char): PUTF8Char varargs; cdecl; external cRaylib name 'TextFormat';
function TextSubtext(const text: PUTF8Char; position: Integer; length: Integer): PUTF8Char; cdecl; external cRaylib name 'TextSubtext';
function TextReplace(text: PUTF8Char; const replace: PUTF8Char; const by: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'TextReplace';
function TextInsert(const text: PUTF8Char; const insert: PUTF8Char; position: Integer): PUTF8Char; cdecl; external cRaylib name 'TextInsert';
function TextJoin(textList: PPUTF8Char; count: Integer; const delimiter: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'TextJoin';
function TextSplit(const text: PUTF8Char; delimiter: UTF8Char; count: PInteger): PPUTF8Char; cdecl; external cRaylib name 'TextSplit';
procedure TextAppend(text: PUTF8Char; const append: PUTF8Char; position: PInteger); cdecl; external cRaylib name 'TextAppend';
function TextFindIndex(const text: PUTF8Char; const find: PUTF8Char): Integer; cdecl; external cRaylib name 'TextFindIndex';
function TextToUpper(const text: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'TextToUpper';
function TextToLower(const text: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'TextToLower';
function TextToPascal(const text: PUTF8Char): PUTF8Char; cdecl; external cRaylib name 'TextToPascal';
function TextToInteger(const text: PUTF8Char): Integer; cdecl; external cRaylib name 'TextToInteger';
function TextToUtf8(codepoints: PInteger; length: Integer): PUTF8Char; cdecl; external cRaylib name 'TextToUtf8';

// UTF8 text strings management functions
function GetCodepoints(const text: PUTF8Char; count: PInteger): PInteger; cdecl; external cRaylib name 'GetCodepoints';
function GetCodepointsCount(const text: PUTF8Char): Integer; cdecl; external cRaylib name 'GetCodepointsCount';
function GetNextCodepoint(const text: PUTF8Char; bytesProcessed: PInteger): Integer; cdecl; external cRaylib name 'GetNextCodepoint';
function CodepointToUtf8(codepoint: Integer; byteLength: PInteger): PUTF8Char; cdecl; external cRaylib name 'CodepointToUtf8';

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Basic geometric 3D shapes drawing functions
procedure DrawLine3D(startPos: TVector3; endPos: TVector3; color: TColor); cdecl; external cRaylib name 'DrawLine3D';
procedure DrawPoint3D(position: TVector3; color: TColor); cdecl; external cRaylib name 'DrawPoint3D';
procedure DrawCircle3D(center: TVector3; radius: Single; rotationAxis: TVector3; rotationAngle: Single; color: TColor); cdecl; external cRaylib name 'DrawCircle3D';
procedure DrawCube(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl; external cRaylib name 'DrawCube';
procedure DrawCubeV(position: TVector3; size: TVector3; color: TColor); cdecl; external cRaylib name 'DrawCubeV';
procedure DrawCubeWires(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl; external cRaylib name 'DrawCubeWires';
procedure DrawCubeWiresV(position: TVector3; size: TVector3; color: TColor); cdecl; external cRaylib name 'DrawCubeWiresV';
procedure DrawCubeTexture(texture: TTexture2D; position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl; external cRaylib name 'DrawCubeTexture';
procedure DrawSphere(centerPos: TVector3; radius: Single; color: TColor); cdecl; external cRaylib name 'DrawSphere';
procedure DrawSphereEx(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl; external cRaylib name 'DrawSphereEx';
procedure DrawSphereWires(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl; external cRaylib name 'DrawSphereWires';
procedure DrawCylinder(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl; external cRaylib name 'DrawCylinder';
procedure DrawCylinderWires(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl; external cRaylib name 'DrawCylinderWires';
procedure DrawPlane(centerPos: TVector3; size: TVector2; color: TColor); cdecl; external cRaylib name 'DrawPlane';
procedure DrawRay(ray: TRay; color: TColor); cdecl; external cRaylib name 'DrawRay';
procedure DrawGrid(slices: Integer; spacing: Single); cdecl; external cRaylib name 'DrawGrid';
procedure DrawGizmo(position: TVector3); cdecl; external cRaylib name 'DrawGizmo';
//DrawTorus(), DrawTeapot() could be useful?

//------------------------------------------------------------------------------------
// Model 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Model loading/unloading functions
function LoadModel(const fileName: PUTF8Char): TModel; cdecl; external cRaylib name 'LoadModel';
function LoadModelFromMesh(mesh: TMesh): TModel; cdecl; external cRaylib name 'LoadModelFromMesh';
procedure UnloadModel(model: TModel); cdecl; external cRaylib name 'UnloadModel';

// Mesh loading/unloading functions
function LoadMeshes(const fileName: PUTF8Char; meshCount: PInteger): PMesh; cdecl; external cRaylib name 'LoadMeshes';
procedure ExportMesh(mesh: TMesh; const fileName: PUTF8Char); cdecl; external cRaylib name 'ExportMesh';
procedure UnloadMesh(mesh: TMesh); cdecl; external cRaylib name 'UnloadMesh';

// Material loading/unloading functions
function LoadMaterials(const fileName: PUTF8Char; materialCount: PInteger): PMaterial; cdecl; external cRaylib name 'LoadMaterials';
function LoadMaterialDefault(): TMaterial; cdecl; external cRaylib name 'LoadMaterialDefault';
procedure UnloadMaterial(material: TMaterial); cdecl; external cRaylib name 'UnloadMaterial';
procedure SetMaterialTexture(material: PMaterial; mapType: Integer; texture: TTexture2D); cdecl; external cRaylib name 'SetMaterialTexture';
procedure SetModelMeshMaterial(model: PModel; meshId: Integer; materialId: Integer); cdecl; external cRaylib name 'SetModelMeshMaterial';

// Model animations loading/unloading functions
function LoadModelAnimations(const fileName: PUTF8Char; animsCount: PInteger): PModelAnimation; cdecl; external cRaylib name 'LoadModelAnimations';
procedure UpdateModelAnimation(model: TModel; anim: TModelAnimation; frame: Integer); cdecl; external cRaylib name 'UpdateModelAnimation';
procedure UnloadModelAnimation(anim: TModelAnimation); cdecl; external cRaylib name 'UnloadModelAnimation';
function IsModelAnimationValid(model: TModel; anim: TModelAnimation): boolean; cdecl; external cRaylib name 'IsModelAnimationValid';

// Mesh generation functions
function GenMeshPoly(sides: Integer; radius: Single): TMesh; cdecl; external cRaylib name 'GenMeshPoly';
function GenMeshPlane(width: Single; length: Single; resX: Integer; resZ: Integer): TMesh; cdecl; external cRaylib name 'GenMeshPlane';
function GenMeshCube(width: Single; height: Single; length: Single): TMesh; cdecl; external cRaylib name 'GenMeshCube';
function GenMeshSphere(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl; external cRaylib name 'GenMeshSphere';
function GenMeshHemiSphere(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl; external cRaylib name 'GenMeshHemiSphere';
function GenMeshCylinder(radius: Single; height: Single; slices: Integer): TMesh; cdecl; external cRaylib name 'GenMeshCylinder';
function GenMeshTorus(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl; external cRaylib name 'GenMeshTorus';
function GenMeshKnot(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl; external cRaylib name 'GenMeshKnot';
function GenMeshHeightmap(heightmap: TImage; size: TVector3): TMesh; cdecl; external cRaylib name 'GenMeshHeightmap';
function GenMeshCubicmap(cubicmap: TImage; cubeSize: TVector3): TMesh; cdecl; external cRaylib name 'GenMeshCubicmap';

// Mesh manipulation functions
function MeshBoundingBox(mesh: TMesh): TBoundingBox; cdecl; external cRaylib name 'MeshBoundingBox';
procedure MeshTangents(mesh: PMesh); cdecl; external cRaylib name 'MeshTangents';
procedure MeshBinormals(mesh: PMesh); cdecl; external cRaylib name 'MeshBinormals';

// Model drawing functions
procedure DrawModel(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl; external cRaylib name 'DrawModel';
procedure DrawModelEx(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl; external cRaylib name 'DrawModelEx';
procedure DrawModelWires(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl; external cRaylib name 'DrawModelWires';
procedure DrawModelWiresEx(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl; external cRaylib name 'DrawModelWiresEx';
procedure DrawBoundingBox(box: TBoundingBox; color: TColor); cdecl; external cRaylib name 'DrawBoundingBox';
procedure DrawBillboard(camera: TCamera; texture: TTexture2D; center: TVector3; size: Single; tint: TColor); cdecl; external cRaylib name 'DrawBillboard';
procedure DrawBillboardRec(camera: TCamera; texture: TTexture2D; sourceRec: TRectangle; center: TVector3; size: Single; tint: TColor); cdecl; external cRaylib name 'DrawBillboardRec';

// Collision detection functions
function CheckCollisionSpheres(centerA: TVector3; radiusA: Single; centerB: TVector3; radiusB: Single): boolean; cdecl; external cRaylib name 'CheckCollisionSpheres';
function CheckCollisionBoxes(box1: TBoundingBox; box2: TBoundingBox): boolean; cdecl; external cRaylib name 'CheckCollisionBoxes';
function CheckCollisionBoxSphere(box: TBoundingBox; center: TVector3; radius: Single): boolean; cdecl; external cRaylib name 'CheckCollisionBoxSphere';
function CheckCollisionRaySphere(ray: TRay; center: TVector3; radius: Single): boolean; cdecl; external cRaylib name 'CheckCollisionRaySphere';
function CheckCollisionRaySphereEx(ray: TRay; center: TVector3; radius: Single; collisionPoint: PVector3): boolean; cdecl; external cRaylib name 'CheckCollisionRaySphereEx';
function CheckCollisionRayBox(ray: TRay; box: TBoundingBox): boolean; cdecl; external cRaylib name 'CheckCollisionRayBox';
function GetCollisionRayModel(ray: TRay; model: TModel): TRayHitInfo; cdecl; external cRaylib name 'GetCollisionRayModel';
function GetCollisionRayTriangle(ray: TRay; p1: TVector3; p2: TVector3; p3: TVector3): TRayHitInfo; cdecl; external cRaylib name 'GetCollisionRayTriangle';
function GetCollisionRayGround(ray: TRay; groundHeight: Single): TRayHitInfo; cdecl; external cRaylib name 'GetCollisionRayGround';

//------------------------------------------------------------------------------------
// Shaders System Functions (Module: rlgl)
// NOTE: This functions are useless when using OpenGL 1.1
//------------------------------------------------------------------------------------

// Shader loading/unloading functions
function LoadShader(const vsFileName: PUTF8Char; const fsFileName: PUTF8Char): TShader; cdecl; external cRaylib name 'LoadShader';
function LoadShaderCode(const vsCode: PUTF8Char; const fsCode: PUTF8Char): TShader; cdecl; external cRaylib name 'LoadShaderCode';
procedure UnloadShader(shader: TShader); cdecl; external cRaylib name 'UnloadShader';

function GetShaderDefault(): TShader; cdecl; external cRaylib name 'GetShaderDefault';
function GetTextureDefault(): TTexture2D; cdecl; external cRaylib name 'GetTextureDefault';
function GetShapesTexture(): TTexture2D; cdecl; external cRaylib name 'GetShapesTexture';
function GetShapesTextureRec(): TRectangle; cdecl; external cRaylib name 'GetShapesTextureRec';
procedure SetShapesTexture(texture: TTexture2D; source: TRectangle); cdecl; external cRaylib name 'SetShapesTexture';

// Shader configuration functions
function GetShaderLocation(shader: TShader; const uniformName: PUTF8Char): Integer; cdecl; external cRaylib name 'GetShaderLocation';
procedure SetShaderValue(shader: TShader; uniformLoc: Integer; const value: Pointer; uniformType: Integer); cdecl; external cRaylib name 'SetShaderValue';
procedure SetShaderValueV(shader: TShader; uniformLoc: Integer; const value: Pointer; uniformType: Integer; count: Integer); cdecl; external cRaylib name 'SetShaderValueV';
procedure SetShaderValueMatrix(shader: TShader; uniformLoc: Integer; mat: TMatrix); cdecl; external cRaylib name 'SetShaderValueMatrix';
procedure SetShaderValueTexture(shader: TShader; uniformLoc: Integer; texture: TTexture2D); cdecl; external cRaylib name 'SetShaderValueTexture';
procedure SetMatrixProjection(proj: TMatrix); cdecl; external cRaylib name 'SetMatrixProjection';
procedure SetMatrixModelview(view: TMatrix); cdecl; external cRaylib name 'SetMatrixModelview';
function GetMatrixModelview(): TMatrix; cdecl; external cRaylib name 'GetMatrixModelview';
function GetMatrixProjection(): TMatrix; cdecl; external cRaylib name 'GetMatrixProjection';

// Texture maps generation (PBR)
// NOTE: Required shaders should be provided
function GenTextureCubemap(shader: TShader; map: TTexture2D; size: Integer): TTexture2D; cdecl; external cRaylib name 'GenTextureCubemap';
function GenTextureIrradiance(shader: TShader; cubemap: TTexture2D; size: Integer): TTexture2D; cdecl; external cRaylib name 'GenTextureIrradiance';
function GenTexturePrefilter(shader: TShader; cubemap: TTexture2D; size: Integer): TTexture2D; cdecl; external cRaylib name 'GenTexturePrefilter';
function GenTextureBRDF(shader: TShader; size: Integer): TTexture2D; cdecl; external cRaylib name 'GenTextureBRDF';

// Shading begin/end functions
procedure BeginShaderMode(shader: TShader); cdecl; external cRaylib name 'BeginShaderMode';
procedure EndShaderMode; cdecl; external cRaylib name 'EndShaderMode';
procedure BeginBlendMode(mode: Integer); cdecl; external cRaylib name 'BeginBlendMode';
procedure EndBlendMode; cdecl; external cRaylib name 'EndBlendMode';

// VR control functions
procedure InitVrSimulator; cdecl; external cRaylib name 'InitVrSimulator';
procedure CloseVrSimulator; cdecl; external cRaylib name 'CloseVrSimulator';
procedure UpdateVrTracking(camera: PCamera); cdecl; external cRaylib name 'UpdateVrTracking';
procedure SetVrConfiguration(info: TVrDeviceInfo; distortion: TShader); cdecl; external cRaylib name 'SetVrConfiguration';
function IsVrSimulatorReady(): boolean; cdecl; external cRaylib name 'IsVrSimulatorReady';
procedure ToggleVrMode; cdecl; external cRaylib name 'ToggleVrMode';
procedure BeginVrDrawing; cdecl; external cRaylib name 'BeginVrDrawing';
procedure EndVrDrawing; cdecl; external cRaylib name 'EndVrDrawing';

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------

// Audio device management functions
procedure InitAudioDevice; cdecl; external cRaylib name 'InitAudioDevice';
procedure CloseAudioDevice; cdecl; external cRaylib name 'CloseAudioDevice';
function IsAudioDeviceReady(): boolean; cdecl; external cRaylib name 'IsAudioDeviceReady';
procedure SetMasterVolume(volume: Single); cdecl; external cRaylib name 'SetMasterVolume';

// Wave/Sound loading/unloading functions
function LoadWave(const fileName: PUTF8Char): TWave; cdecl; external cRaylib name 'LoadWave';
function LoadSound(const fileName: PUTF8Char): TSound; cdecl; external cRaylib name 'LoadSound';
function LoadSoundFromWave(wave: TWave): TSound; cdecl; external cRaylib name 'LoadSoundFromWave';
procedure UpdateSound(sound: TSound; const data: Pointer; samplesCount: Integer); cdecl; external cRaylib name 'UpdateSound';
procedure UnloadWave(wave: TWave); cdecl; external cRaylib name 'UnloadWave';
procedure UnloadSound(sound: TSound); cdecl; external cRaylib name 'UnloadSound';
procedure ExportWave(wave: TWave; const fileName: PUTF8Char); cdecl; external cRaylib name 'ExportWave';
procedure ExportWaveAsCode(wave: TWave; const fileName: PUTF8Char); cdecl; external cRaylib name 'ExportWaveAsCode';

// Wave/Sound management functions
procedure PlaySound(sound: TSound); cdecl; external cRaylib name 'PlaySound';
procedure StopSound(sound: TSound); cdecl; external cRaylib name 'StopSound';
procedure PauseSound(sound: TSound); cdecl; external cRaylib name 'PauseSound';
procedure ResumeSound(sound: TSound); cdecl; external cRaylib name 'ResumeSound';
procedure PlaySoundMulti(sound: TSound); cdecl; external cRaylib name 'PlaySoundMulti';
procedure StopSoundMulti; cdecl; external cRaylib name 'StopSoundMulti';
function GetSoundsPlaying(): Integer; cdecl; external cRaylib name 'GetSoundsPlaying';
function IsSoundPlaying(sound: TSound): boolean; cdecl; external cRaylib name 'IsSoundPlaying';
procedure SetSoundVolume(sound: TSound; volume: Single); cdecl; external cRaylib name 'SetSoundVolume';
procedure SetSoundPitch(sound: TSound; pitch: Single); cdecl; external cRaylib name 'SetSoundPitch';
procedure WaveFormat(wave: PWave; sampleRate: Integer; sampleSize: Integer; channels: Integer); cdecl; external cRaylib name 'WaveFormat';
function WaveCopy(wave: TWave): TWave; cdecl; external cRaylib name 'WaveCopy';
procedure WaveCrop(wave: PWave; initSample: Integer; finalSample: Integer); cdecl; external cRaylib name 'WaveCrop';
function GetWaveData(wave: TWave): PSingle; cdecl; external cRaylib name 'GetWaveData';

// Music management functions
function LoadMusicStream(const fileName: PUTF8Char): TMusic; cdecl; external cRaylib name 'LoadMusicStream';
procedure UnloadMusicStream(music: TMusic); cdecl; external cRaylib name 'UnloadMusicStream';
procedure PlayMusicStream(music: TMusic); cdecl; external cRaylib name 'PlayMusicStream';
procedure UpdateMusicStream(music: TMusic); cdecl; external cRaylib name 'UpdateMusicStream';
procedure StopMusicStream(music: TMusic); cdecl; external cRaylib name 'StopMusicStream';
procedure PauseMusicStream(music: TMusic); cdecl; external cRaylib name 'PauseMusicStream';
procedure ResumeMusicStream(music: TMusic); cdecl; external cRaylib name 'ResumeMusicStream';
function IsMusicPlaying(music: TMusic): boolean; cdecl; external cRaylib name 'IsMusicPlaying';
procedure SetMusicVolume(music: TMusic; volume: Single); cdecl; external cRaylib name 'SetMusicVolume';
procedure SetMusicPitch(music: TMusic; pitch: Single); cdecl; external cRaylib name 'SetMusicPitch';
procedure SetMusicLoopCount(music: TMusic; count: Integer); cdecl; external cRaylib name 'SetMusicLoopCount';
function GetMusicTimeLength(music: TMusic): Single; cdecl; external cRaylib name 'GetMusicTimeLength';
function GetMusicTimePlayed(music: TMusic): Single; cdecl; external cRaylib name 'GetMusicTimePlayed';

// AudioStream management functions
function InitAudioStream(sampleRate: Cardinal; sampleSize: Cardinal; channels: Cardinal): TAudioStream; cdecl; external cRaylib name 'InitAudioStream';
procedure UpdateAudioStream(stream: TAudioStream; const data: Pointer; samplesCount: Integer); cdecl; external cRaylib name 'UpdateAudioStream';
procedure CloseAudioStream(stream: TAudioStream); cdecl; external cRaylib name 'CloseAudioStream';
function IsAudioStreamProcessed(stream: TAudioStream): boolean; cdecl; external cRaylib name 'IsAudioStreamProcessed';
procedure PlayAudioStream(stream: TAudioStream); cdecl; external cRaylib name 'PlayAudioStream';
procedure PauseAudioStream(stream: TAudioStream); cdecl; external cRaylib name 'PauseAudioStream';
procedure ResumeAudioStream(stream: TAudioStream); cdecl; external cRaylib name 'ResumeAudioStream';
function IsAudioStreamPlaying(stream: TAudioStream): boolean; cdecl; external cRaylib name 'IsAudioStreamPlaying';
procedure StopAudioStream(stream: TAudioStream); cdecl; external cRaylib name 'StopAudioStream';
procedure SetAudioStreamVolume(stream: TAudioStream; volume: Single); cdecl; external cRaylib name 'SetAudioStreamVolume';
procedure SetAudioStreamPitch(stream: TAudioStream; pitch: Single); cdecl; external cRaylib name 'SetAudioStreamPitch';
procedure SetAudioStreamBufferSizeDefault(size: Integer); cdecl; external cRaylib name 'SetAudioStreamBufferSizeDefault';

//------------------------------------------------------------------------------------
// Network (Module: network)
//------------------------------------------------------------------------------------

// IN PROGRESS: Check rnet.h for reference

implementation

end.
