unit RayLib;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{$MINENUMSIZE 4} //{$Z4} All enum must be sized as Integer
{$A8}

{**
 *  RayLib 4
 *
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
*   raylib v4.0 - A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)
*
*   FEATURES:
*       - NO external dependencies, all required libraries included with raylib
*       - Multiplatform: Windows, Linux, FreeBSD, OpenBSD, NetBSD, DragonFly,
*                        MacOS, Haiku, Android, Raspberry Pi, DRM native, HTML5.
*       - Written in plain C code (C99) in PascalCase/camelCase notation
*       - Hardware accelerated with OpenGL (1.1, 2.1, 3.3, 4.3 or ES2 - choose at compile)
*       - Unique OpenGL abstraction layer (usable as standalone module): [rlgl]
*       - Multiple Fonts formats supported (TTF, XNA fonts, AngelCode fonts)
*       - Outstanding texture formats support, including compressed formats (DXT, ETC, ASTC)
*       - Full 3d support for 3d Shapes, Models, Billboards, Heightmaps and more!
*       - Flexible Materials system, supporting classic maps and PBR maps
*       - Animated 3D models supported (skeletal bones animation) (IQM)
*       - Shaders support, including Model shaders and Postprocessing shaders
*       - Powerful math module for Vector, Matrix and Quaternion operations: [raymath]
*       - Audio loading and playing with streaming support (WAV, OGG, MP3, FLAC, XM, MOD)
*       - VR stereo rendering with configurable HMD device parameters
*       - Bindings to multiple programming languages available!
*
*   NOTES:
*       - One default Font is loaded on InitWindow()->LoadFontDefault() [core, text]
*       - One default Texture2D is loaded on rlglInit(), 1x1 white pixel R8G8B8A8 [rlgl] (OpenGL 3.3 or ES2)
*       - One default Shader is loaded on rlglInit()->rlLoadShaderDefault() [rlgl] (OpenGL 3.3 or ES2)
*       - One default RenderBatch is loaded on rlglInit()->rlLoadRenderBatch() [rlgl] (OpenGL 3.3 or ES2)
*
*   DEPENDENCIES (included):
*       [rcore] rglfw (Camilla Löwy - github.com/glfw/glfw) for window/context management and input (PLATFORM_DESKTOP)
*       [rlgl] glad (David Herberth - github.com/Dav1dde/glad) for OpenGL 3.3 extensions loading (PLATFORM_DESKTOP)
*       [raudio] miniaudio (David Reid - github.com/mackron/miniaudio) for audio device/context management
*
*   OPTIONAL DEPENDENCIES (included):
*       [rcore] msf_gif (Miles Fogle) for GIF recording
*       [rcore] sinfl (Micha Mettke) for DEFLATE decompression algorythm
*       [rcore] sdefl (Micha Mettke) for DEFLATE compression algorythm
*       [rtextures] stb_image (Sean Barret) for images loading (BMP, TGA, PNG, JPEG, HDR...)
*       [rtextures] stb_image_write (Sean Barret) for image writing (BMP, TGA, PNG, JPG)
*       [rtextures] stb_image_resize (Sean Barret) for image resizing algorithms
*       [rtext] stb_truetype (Sean Barret) for ttf fonts loading
*       [rtext] stb_rect_pack (Sean Barret) for rectangles packing
*       [rmodels] par_shapes (Philip Rideout) for parametric 3d shapes generation
*       [rmodels] tinyobj_loader_c (Syoyo Fujita) for models loading (OBJ, MTL)
*       [rmodels] cgltf (Johannes Kuhlmann) for models loading (glTF)
*       [raudio] dr_wav (David Reid) for WAV audio file loading
*       [raudio] dr_flac (David Reid) for FLAC audio file loading
*       [raudio] dr_mp3 (David Reid) for MP3 audio file loading
*       [raudio] stb_vorbis (Sean Barret) for OGG audio loading
*       [raudio] jar_xm (Joshua Reisenauer) for XM audio module loading
*       [raudio] jar_mod (Joshua Reisenauer) for MOD audio module loading
*
*
*   LICENSE: zlib/libpng
*
*   raylib is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software:
*
*   Copyright (c) 2013-2021 Ramon Santamaria (@raysan5)
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

const
  RAYLIB_VERSION = '4.0';

type
  PPUTF8Char = ^PUTF8Char;

  { TRGBAColor }

  // Color, 4 components, R8G8B8A8 (32bit)
  TRGBAColor = record
    Red: Byte; // Color red value
    Green: Byte; // Color green value
    Blue: Byte; // Color blue value
    Alpha: Byte; // Color alpha value
  end;

  { TColor }

  // Color, 4 components, R8G8B8A8 (32bit)
  //AColor := TColor.Create($010203FF);
  TColor = record
    class operator Implicit(a: Cardinal): TColor;
    class operator Implicit(a: TRGBAColor): TColor;

    class operator Explicit(a: TRGBAColor): TColor;
    class operator Explicit(a: Cardinal): TColor;

    class operator Equal(a, b: TColor) : Boolean;

    procedure SetRGB(AColor: TColor);

    case Cardinal of
      0: (Value: Cardinal);
      1: (RGBA: TRGBAColor);
  end;

  PColor = ^TColor;

  { TColorHelper }

  { TRGBAColorHelper }

  TRGBAColorHelper = record helper for TRGBAColor
    function ReplaceAlpha(AAlpha: Byte): TColor;
    function ReplaceRGB(AColor: TColor): TColor; //without alpha
  end;

  { TColorHelper }

  TColorHelper = record helper for TColor
    function SetAlpha(AAlpha: Byte): TColor;
    constructor Create(ARed, AGreen, ABlue, AAlpha: Byte);overload;
    constructor Create(RGBAColor: TRGBAColor); overload;
    //constructor Create(AValue: Cardinal); overload;
    constructor CreateRGBA(RGBA: Cardinal); overload; //stupid idea, but ok :P
  end;

const
  // NOTE: We set some defines with some data types declared by raylib
  // Other modules (raymath, rlgl) also require some of those types, so,
  // to be able to use those other modules as standalone (not depending on raylib)
  // this defines are very useful for internal check and avoid type (re)definitions
{
  RL_COLOR_TYPE
  RL_RECTANGLE_TYPE
  RL_VECTOR2_TYPE
  RL_VECTOR3_TYPE
  RL_VECTOR4_TYPE
  RL_QUATERNION_TYPE
  RL_MATRIX_TYPE
}

  clLightgray: TRGBAColor = (Red: 200; Green: 200; Blue: 200; Alpha: 255);   // Light Gray
  clGray:      TRGBAColor = (Red: 130; Green: 130; Blue: 130; Alpha: 255);   // Gray
  clDarkGray:  TRGBAColor = (Red: 80; Green: 80; Blue: 80; Alpha: 255);      // Dark Gray
  clYellow:    TRGBAColor = (Red: 253; Green: 249; Blue: 0; Alpha: 255);     // Yellow
  clGold:      TRGBAColor = (Red: 255; Green: 203; Blue: 0; Alpha: 255);     // Gold
  clOrange:    TRGBAColor = (Red: 255; Green: 161; Blue: 0; Alpha: 255);     // Orange
  clPink:      TRGBAColor = (Red: 255; Green: 109; Blue: 194; Alpha: 255);   // Pink
  clRed:       TRGBAColor = (Red: 230; Green: 41; Blue: 55; Alpha: 255);     // Red
  clMaroon:    TRGBAColor = (Red: 190; Green: 33; Blue: 55; Alpha: 255);     // Maroon
  clGreen:     TRGBAColor = (Red: 0; Green: 228; Blue: 48; Alpha: 255);      // Green
  clLime:      TRGBAColor = (Red: 0; Green: 158; Blue: 47; Alpha: 255);      // Lime
  clDarkgreen: TRGBAColor = (Red: 0; Green: 117; Blue: 44; Alpha: 255);      // Dark Green
  clSkyBlue:   TRGBAColor = (Red: 102; Green: 191; Blue: 255; Alpha: 255);   // Sky Blue
  clBlue:      TRGBAColor = (Red: 0; Green: 121; Blue: 241; Alpha: 255);     // Blue
  clDarkblue:  TRGBAColor = (Red: 0; Green: 82; Blue: 172; Alpha: 255);      // Dark Blue
  clPurple:    TRGBAColor = (Red: 200; Green: 122; Blue: 255; Alpha: 255);   // Purple
  clViolet:    TRGBAColor = (Red: 135; Green: 60; Blue: 190; Alpha: 255);    // Violet
  clDarkpurple:TRGBAColor = (Red: 112; Green: 31; Blue: 126; Alpha: 255);    // Dark Purple
  clBeige:     TRGBAColor = (Red: 211; Green: 176; Blue: 131; Alpha: 255);   // Beige
  clBrown:     TRGBAColor = (Red: 127; Green: 106; Blue: 79; Alpha: 255);    // Brown
  clDarkbrown: TRGBAColor = (Red: 76; Green: 63; Blue: 47; Alpha: 255);      // Dark Brown

  clWhite:     TRGBAColor = (Red: 255; Green: 255; Blue: 255; Alpha: 255);   // White
  clBlack:     TRGBAColor = (Red: 0; Green: 0; Blue: 0; Alpha: 255);         // Black
  clBlank:     TRGBAColor = (Red: 0; Green: 0; Blue: 0; Alpha: 0);           // Blank (Transparent)
  clMagenta:   TRGBAColor = (Red: 255; Green: 0; Blue: 255; Alpha: 255);     // Magenta
  clRayWhite:  TRGBAColor = (Red: 245; Green: 245; Blue: 245; Alpha: 255);   // My own White (raylib logo)

type
  // Vector2 type

  { TVector2 }

  // Vector2, 2 components
  TVector2 = packed record
    X: Single; // Vector x component
    Y: Single; // Vector y component
    constructor Create(AX, AY: Single); overload;
    constructor Create(I: Int64); overload;
  end;
  PVector2 = ^TVector2;

  { TVector3 }

  // Vector3, 3 components
  TVector3 = packed record
    x: Single; // Vector x component
    y: Single; // Vector y component
    z: Single; // Vector z component
    constructor Create(AX, AY, AZ: Single); overload;
  end;
  PVector3 = ^TVector3;

  { TVector4 }

  // Vector4, 4 components
  TVector4 = packed record
    x: Single; // Vector x component
    y: Single; // Vector y component
    z: Single; // Vector z component
    w: Single; // Vector w component
  end;
  PVector4 = ^TVector4;

  // Quaternion, 4 components (Vector4 alias)
  TQuaternion = PVector4;

  // Matrix, 4x4 components, column major, OpenGL style, right handed
  TMatrix = record
    m0, m4, m8, m12: Single; // Matrix first row (4 components)
    m1, m5, m9, m13: Single; // Matrix second row (4 components)
    m2, m6, m10, m14: Single; // Matrix third row (4 components)
    m3, m7, m11, m15: Single; // Matrix fourth row (4 components)
  end;
  PMatrix = ^TMatrix;

  { TRectangle }

  // Rectangle, 4 components
  TRectangle = packed record
    X: Single;       // Rectangle top-left corner position x
    Y: Single;       // Rectangle top-left corner position y
    Width: Single;   // Rectangle width
    Height: Single;  // Rectangle height
    constructor Create(AX, AY, AWidth, AHeight: Single);
  end;
  PRectangle = ^TRectangle;
  PPRectangle = ^PRectangle;

  // Image, pixel data stored in CPU memory (RAM)
  TImage = packed record
    Data: Pointer;     // Image raw data
    Width: Integer;    // Image base width
    Height: Integer;   // Image base height
    Mipmaps: Integer;  // Mipmap levels, 1 by default
    Format: Integer;   // Data format (PixelFormat type)
  end;
  PImage = ^TImage;

  // Texture, tex data stored in GPU memory (VRAM)
  TTexture = packed record
    ID: Cardinal;      // OpenGL texture id
    Width: Integer;    // Texture base width
    Height: Integer;   // Texture base height
    Mipmaps: Integer;  // Mipmap levels, 1 by default
    Format: Integer;   // Data format (PixelFormat type)
  end;
  PTexture = ^TTexture;

  // Texture2D, same as Texture
  TTexture2D = TTexture;
  PTexture2D = ^PTexture;

  // TextureCubemap, same as Texture
  TTextureCubemap = TTexture;
  PTextureCubemap = ^PTexture;

  // RenderTexture, fbo for texture rendering
  TRenderTexture = packed record
    ID: Cardinal;        // OpenGL Framebuffer Object (FBO) id
    Texture: TTexture;   // Color buffer attachment texture
    Depth: TTexture;     // Depth buffer attachment texture
  end;
  PRenderTexture = ^TRenderTexture;

  // RenderTexture2D, same as RenderTexture
  TRenderTexture2D = TRenderTexture;
  PRenderTexture2D = ^TRenderTexture;

  // NPatchInfo, n-patch layout info
  TNPatchInfo = packed record
    Source: TRectangle; // Texture source rectangle
    Left: Integer;         // Left border offset
    Top: Integer;          // Top border offset
    Right: Integer;        // Right border offset
    Bottom: Integer;       // Bottom border offset
    AType: Integer;        // Layout of the n-patch: 3x3, 1x3 or 3x1
  end;
  PNPatchInfo = ^TNPatchInfo;

  // GlyphInfo, font characters glyphs info
  TGlyphInfo = packed record
    Value: Integer;    // Character value (Unicode)
    OffsetX: Integer;  // Character offset X when drawing
    OffsetY: Integer;  // Character offset Y when drawing
    AdvanceX: Integer; // Character advance position X
    Image: TImage;     // Character image data
  end;
  PGlyphInfo = ^TGlyphInfo;

  // Font, font texture and GlyphInfo array data
  TFont = packed record
    BaseSize: Integer;       // Base size (default chars height)
    GlyphCount: Integer;     // Number of glyph characters
    GlyphPadding: Integer;   // Padding around the glyph characters
    Texture: TTexture2D;     // Texture atlas containing the glyphs
    Recs: PRectangle;        // Rectangles in texture for the glyphs
    Glyphs: PGlyphInfo;      // Glyphs info data
  end;
  PFont = ^TFont;

  // Camera, defines position/orientation in 3d space
  TCamera3D = packed record
    Position: TVector3;    // Camera position
    Target: TVector3;      // Camera target it looks-at
    Up: TVector3;          // Camera up vector (rotation over its axis)
    Fovy: Single;          // Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
    Projection: Integer;   // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
  end;
  PCamera3D = ^TCamera3D;

  TCamera = TCamera3D;      // Camera type fallback, defaults to Camera3D
  PCamera = ^TCamera;

  // Camera2D, defines position/orientation in 2d space
  TCamera2D = packed record
    Offset: TVector2;       // Camera offset (displacement from target)
    Target: TVector2;       // Camera target (rotation and zoom origin)
    Rotation: Single;       // Camera rotation in degrees
    Zoom: Single;           // Camera zoom (scaling), should be 1.0f by default
  end;
  PCamera2D = ^TCamera2D;

  // Mesh, vertex data and vao/vbo
  TMesh = packed record
      VertexCount: Integer;        // Number of vertices stored in arrays
      TriangleCount: integer;      // Number of triangles stored (indexed or not)

      // Vertex attributes data
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

  // Shader
  TShader = packed record
    ID: Cardinal;         // Shader program id
    Locs: PInteger;       // Shader locations array (RL_MAX_SHADER_LOCATIONS)
  end;
  PShader = ^TShader;

  // MaterialMap
  TMaterialMap = packed record
    Texture: TTexture2D;   // Material map texture
    Color: TColor;         // Material map color
    Value: Single;         // Material map value
  end;
  PMaterialMap = ^TMaterialMap;

  // Material, includes shader and maps
  TMaterial = packed record
    Shader: TShader;        // Material shader
    Maps: PMaterialMap;     // Material maps array (MAX_MATERIAL_MAPS)
    Params: array[0..3] of Single;   // Material generic parameters (if required)
  end;
  PMaterial = ^TMaterial;

  // Transform, vectex transformation data
  TTransform = packed record
    Translation: TVector3;   // Translation
    Rotation: TQuaternion;   // Rotation
    Scale: TVector3;         // Scale
  end;
  PTransform = ^TTransform;
  PPTransform = ^PTransform;

  // Bone, skeletal animation bone
  TBoneInfo = packed record
    Name: array[0..31] of AnsiChar;  // Bone name
    Parent: Integer;                 // Bone parent
  end;
  PBoneInfo = ^TBoneInfo;

  // Model, meshes, materials and animation data
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

  // ModelAnimation
  TModelAnimation = packed record
    BoneCount: Integer;        // Number of bones
    FrameCount: Integer;       // Number of animation frames
    BoneInfo: PBoneInfo;       // Bones information (skeleton)

    FramePoses: PPTransform;   // Poses array by frame
  end;
  PModelAnimation = ^TModelAnimation;

  // Ray, ray for raycasting
  TRay = packed record
    Position: TVector3;        // Ray position (origin)
    Direction: TVector3;       // Ray direction
  end;
  PRay = ^TRay;

  // RayCollision, ray hit information
  TRayCollision = packed record
    Hit: Boolean;              // Did the ray hit something?
    Distance: Single;          // Distance to nearest hit
    Point: TVector3;           // Point of nearest hit
    Normal: TVector3;          // Surface normal of hit
  end;
  PRayCollision = ^TRayCollision;

  // BoundingBox
  TBoundingBox = packed record
    Min: TVector3;             // Minimum vertex box-corner
    Max: TVector3;             // Maximum vertex box-corner
  end;
  PBoundingBox = ^TBoundingBox;

  // Wave, audio wave data
  TWave = packed record
    FrameCount: Cardinal;      // Total number of frames (considering channels)
    SampleRate: Cardinal;      // Frequency (samples per second)
    SampleSize: Cardinal;      // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    Channels: Cardinal;        // Number of channels (1-mono, 2-stereo, ...)
    Data: Pointer;             // Buffer data pointer
  end;
  PWave = ^TWave;

  //
  TrAudioBuffer = packed record
  end;
  PrAudioBuffer = ^TrAudioBuffer;

  // AudioStream, custom audio stream
  TAudioStream = packed record
    Buffer: PrAudioBuffer;     // Pointer to internal data used by the audio system

    SampleRate: Cardinal;       // Frequency (samples per second)
    SampleSize: Cardinal;       // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    Channels: Cardinal;         // Number of channels (1-mono, 2-stereo, ...)
  end;
  PAudioStream = ^TAudioStream;

  // Sound
  TSound = packed record
    Stream: TAudioStream;       // Audio stream
    FrameCount: Cardinal;       // Total number of frames (considering channels)
  end;
  PSound = ^TSound;

  // Music, audio stream, anything longer than ~10 seconds should be streamed
  TMusic = packed record
    Stream: TAudioStream;      // Audio stream
    FrameCount: Cardinal;      // Total number of frames (considering channels)
    Looping: Boolean;          // Music looping enable

    CtxType: Integer;          // Type of music context (audio filetype)
    CtxData: Pointer;          // Audio context data, depends on type
  end;
  PMusic = ^TMusic;

  // VrDeviceInfo, Head-Mounted-Display device parameters
  TVrDeviceInfo = packed record
    hResolution: Integer;                            // Horizontal resolution in pixels
    vResolution: Integer;                            // Vertical resolution in pixels
    hScreenSize: Single;                             // Horizontal size in meters
    vScreenSize: Single;                             // Vertical size in meters
    vScreenCenter: Single;                           // Screen center in meters
    EyeToScreenDistance: Single;                     // Distance between eye and display in meters
    LensSeparationDistance: Single;                  // Lens separation distance in meters
    InterpupillaryDistance: Single;                  // IPD (distance between pupils) in meters
    LensDistortionValues: array[0..3] of Single;     // Lens distortion constant parameters
    ChromaAbCorrection: array[0..3] of Single;       // Chromatic aberration correction parameters
  end;
  PVrDeviceInfo = ^TVrDeviceInfo;

  // VrStereoConfig, VR stereo rendering configuration for simulator
  TVrStereoConfig = packed record
      projection: array[0..1] of TMatrix;           // VR projection matrices (per eye)
      ViewOffset: array[0..1] of TMatrix;           // VR view offset matrices (per eye)
      LeftLensCenter: array[0..1] of Single;        // VR left lens center
      RightLensCenter: array[0..1] of Single;       // VR right lens center
      LeftScreenCenter: array[0..1] of Single;      // VR left screen center
      RightScreenCenter: array[0..1] of Single;     // VR right screen center
      Scale: array[0..1] of Single;                 // VR distortion scale
      ScaleIn: array[0..1] of Single;               // VR distortion scale in
  end;
  PVrStereoConfig = ^TVrStereoConfig;


  //----------------------------------------------------------------------------------
  // Enumerators Definition
  //----------------------------------------------------------------------------------
  // System/Window config flags
  // NOTE: Every bit registers one state (use it with bit masks)
  // By default all flags are set to 0
  TConfigFlags = (
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

  // Trace log level
  // NOTE: Organized by priority level
  TTraceLogLevel = (
    LOG_ALL = 0,        // Display all logs
    LOG_TRACE,          // Trace logging, intended for internal use only
    LOG_DEBUG,          // Debug logging, used for internal debugging, it should be disabled on release builds
    LOG_INFO,           // Info logging, used for program execution info
    LOG_WARNING,        // Warning logging, used on recoverable failures
    LOG_ERROR,          // Error logging, used on unrecoverable failures
    LOG_FATAL,          // Fatal logging, used to abort program: exit(EXIT_FAILURE)
    LOG_NONE            // Disable logging
  );
  TTraceLogLevels = set of TTraceLogLevel;
  PTraceLogLevels = ^TTraceLogLevels;

//TODO zaher, convert to set, TKeyboardKey
  TKeyboardKey = (
    KEY_NULL            = 0,        // Key: NULL, used for no key pressed
    KEY_APOSTROPHE      = 39,       // Key: '
    KEY_COMMA           = 44,       // Key: ,
    KEY_MINUS           = 45,       // Key: -
    KEY_PERIOD          = 46,       // Key: .
    KEY_SLASH           = 47,       // Key: /
    KEY_ZERO            = 48,       // Key: 0
    KEY_ONE             = 49,       // Key: 1
    KEY_TWO             = 50,       // Key: 2
    KEY_THREE           = 51,       // Key: 3
    KEY_FOUR            = 52,       // Key: 4
    KEY_FIVE            = 53,       // Key: 5
    KEY_SIX             = 54,       // Key: 6
    KEY_SEVEN           = 55,       // Key: 7
    KEY_EIGHT           = 56,       // Key: 8
    KEY_NINE            = 57,       // Key: 9
    KEY_SEMICOLON       = 59,       // Key: ;
    KEY_EQUAL           = 61,       // Key: =
    KEY_A               = 65,       // Key: A | a
    KEY_B               = 66,       // Key: B | b
    KEY_C               = 67,       // Key: C | c
    KEY_D               = 68,       // Key: D | d
    KEY_E               = 69,       // Key: E | e
    KEY_F               = 70,       // Key: F | f
    KEY_G               = 71,       // Key: G | g
    KEY_H               = 72,       // Key: H | h
    KEY_I               = 73,       // Key: I | i
    KEY_J               = 74,       // Key: J | j
    KEY_K               = 75,       // Key: K | k
    KEY_L               = 76,       // Key: L | l
    KEY_M               = 77,       // Key: M | m
    KEY_N               = 78,       // Key: N | n
    KEY_O               = 79,       // Key: O | o
    KEY_P               = 80,       // Key: P | p
    KEY_Q               = 81,       // Key: Q | q
    KEY_R               = 82,       // Key: R | r
    KEY_S               = 83,       // Key: S | s
    KEY_T               = 84,       // Key: T | t
    KEY_U               = 85,       // Key: U | u
    KEY_V               = 86,       // Key: V | v
    KEY_W               = 87,       // Key: W | w
    KEY_X               = 88,       // Key: X | x
    KEY_Y               = 89,       // Key: Y | y
    KEY_Z               = 90,       // Key: Z | z
    KEY_LEFT_BRACKET    = 91,       // Key: [
    KEY_BACKSLASH       = 92,       // Key: '\'
    KEY_RIGHT_BRACKET   = 93,       // Key: ]
    KEY_GRAVE           = 96,       // Key: `
    // Function keys
    KEY_SPACE           = 32,       // Key: Space
    KEY_ESCAPE          = 256,      // Key: Esc
    KEY_ENTER           = 257,      // Key: Enter
    KEY_TAB             = 258,      // Key: Tab
    KEY_BACKSPACE       = 259,      // Key: Backspace
    KEY_INSERT          = 260,      // Key: Ins
    KEY_DELETE          = 261,      // Key: Del
    KEY_RIGHT           = 262,      // Key: Cursor right
    KEY_LEFT            = 263,      // Key: Cursor left
    KEY_DOWN            = 264,      // Key: Cursor down
    KEY_UP              = 265,      // Key: Cursor up
    KEY_PAGE_UP         = 266,      // Key: Page up
    KEY_PAGE_DOWN       = 267,      // Key: Page down
    KEY_HOME            = 268,      // Key: Home
    KEY_END             = 269,      // Key: End
    KEY_CAPS_LOCK       = 280,      // Key: Caps lock
    KEY_SCROLL_LOCK     = 281,      // Key: Scroll down
    KEY_NUM_LOCK        = 282,      // Key: Num lock
    KEY_PRINT_SCREEN    = 283,      // Key: Print screen
    KEY_PAUSE           = 284,      // Key: Pause
    KEY_F1              = 290,      // Key: F1
    KEY_F2              = 291,      // Key: F2
    KEY_F3              = 292,      // Key: F3
    KEY_F4              = 293,      // Key: F4
    KEY_F5              = 294,      // Key: F5
    KEY_F6              = 295,      // Key: F6
    KEY_F7              = 296,      // Key: F7
    KEY_F8              = 297,      // Key: F8
    KEY_F9              = 298,      // Key: F9
    KEY_F10             = 299,      // Key: F10
    KEY_F11             = 300,      // Key: F11
    KEY_F12             = 301,      // Key: F12
    KEY_LEFT_SHIFT      = 340,      // Key: Shift left
    KEY_LEFT_CONTROL    = 341,      // Key: Control left
    KEY_LEFT_ALT        = 342,      // Key: Alt left
    KEY_LEFT_SUPER      = 343,      // Key: Super left
    KEY_RIGHT_SHIFT     = 344,      // Key: Shift right
    KEY_RIGHT_CONTROL   = 345,      // Key: Control right
    KEY_RIGHT_ALT       = 346,      // Key: Alt right
    KEY_RIGHT_SUPER     = 347,      // Key: Super right
    KEY_KB_MENU         = 348,      // Key: KB menu
    // Keypad keys
    KEY_KP_0            = 320,      // Key: Keypad 0
    KEY_KP_1            = 321,      // Key: Keypad 1
    KEY_KP_2            = 322,      // Key: Keypad 2
    KEY_KP_3            = 323,      // Key: Keypad 3
    KEY_KP_4            = 324,      // Key: Keypad 4
    KEY_KP_5            = 325,      // Key: Keypad 5
    KEY_KP_6            = 326,      // Key: Keypad 6
    KEY_KP_7            = 327,      // Key: Keypad 7
    KEY_KP_8            = 328,      // Key: Keypad 8
    KEY_KP_9            = 329,      // Key: Keypad 9
    KEY_KP_DECIMAL      = 330,      // Key: Keypad .
    KEY_KP_DIVIDE       = 331,      // Key: Keypad /
    KEY_KP_MULTIPLY     = 332,      // Key: Keypad *
    KEY_KP_SUBTRACT     = 333,      // Key: Keypad -
    KEY_KP_ADD          = 334,      // Key: Keypad +
    KEY_KP_ENTER        = 335,      // Key: Keypad Enter
    KEY_KP_EQUAL        = 336,      // Key: Keypad =
    // Android key buttons
    KEY_BACK            = 4,        // Key: Android back button
    KEY_MENU            = 82,       // Key: Android menu button
    KEY_VOLUME_UP       = 24,       // Key: Android volume up button
    KEY_VOLUME_DOWN     = 25        // Key: Android volume down button
  );

type
  // Mouse buttons
  TMouseButton = (
    MOUSE_BUTTON_LEFT    = 0,       // Mouse button left
    MOUSE_BUTTON_RIGHT   = 1,       // Mouse button right
    MOUSE_BUTTON_MIDDLE  = 2,       // Mouse button middle (pressed wheel)
    MOUSE_BUTTON_SIDE    = 3,       // Mouse button side (advanced mouse device)
    MOUSE_BUTTON_EXTRA   = 4,       // Mouse button extra (advanced mouse device)
    MOUSE_BUTTON_FORWARD = 5,       // Mouse button fordward (advanced mouse device)
    MOUSE_BUTTON_BACK    = 6       // Mouse button back (advanced mouse device)
  );

  // Mouse cursor
  TMouseCursor = (
    MOUSE_CURSOR_DEFAULT       = 0,     // Default pointer shape
    MOUSE_CURSOR_ARROW         = 1,     // Arrow shape
    MOUSE_CURSOR_IBEAM         = 2,     // Text writing cursor shape
    MOUSE_CURSOR_CROSSHAIR     = 3,     // Cross shape
    MOUSE_CURSOR_POINTING_HAND = 4,     // Pointing hand cursor
    MOUSE_CURSOR_RESIZE_EW     = 5,     // Horizontal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NS     = 6,     // Vertical resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NWSE   = 7,     // Top-left to bottom-right diagonal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_NESW   = 8,     // The top-right to bottom-left diagonal resize/move arrow shape
    MOUSE_CURSOR_RESIZE_ALL    = 9,     // The omni-directional resize/move cursor shape
    MOUSE_CURSOR_NOT_ALLOWED   = 10     // The operation-not-allowed shape
  );

  // Gamepad Buttons
  TGamepadButton = (
    GAMEPAD_BUTTON_UNKNOWN = 0,         // Unknown button, just for error checking
    GAMEPAD_BUTTON_LEFT_FACE_UP,        // Gamepad left DPAD up button
    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,     // Gamepad left DPAD right button
    GAMEPAD_BUTTON_LEFT_FACE_DOWN,      // Gamepad left DPAD down button
    GAMEPAD_BUTTON_LEFT_FACE_LEFT,      // Gamepad left DPAD left button
    GAMEPAD_BUTTON_RIGHT_FACE_UP,       // Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
    GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,    // Gamepad right button right (i.e. PS3: Square, Xbox: X)
    GAMEPAD_BUTTON_RIGHT_FACE_DOWN,     // Gamepad right button down (i.e. PS3: Cross, Xbox: A)
    GAMEPAD_BUTTON_RIGHT_FACE_LEFT,     // Gamepad right button left (i.e. PS3: Circle, Xbox: B)
    GAMEPAD_BUTTON_LEFT_TRIGGER_1,      // Gamepad top/back trigger left (first), it could be a trailing button
    GAMEPAD_BUTTON_LEFT_TRIGGER_2,      // Gamepad top/back trigger left (second), it could be a trailing button
    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,     // Gamepad top/back trigger right (one), it could be a trailing button
    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,     // Gamepad top/back trigger right (second), it could be a trailing button
    GAMEPAD_BUTTON_MIDDLE_LEFT,         // Gamepad center buttons, left one (i.e. PS3: Select)
    GAMEPAD_BUTTON_MIDDLE,              // Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
    GAMEPAD_BUTTON_MIDDLE_RIGHT,        // Gamepad center buttons, right one (i.e. PS3: Start)
    GAMEPAD_BUTTON_LEFT_THUMB,          // Gamepad joystick pressed button left
    GAMEPAD_BUTTON_RIGHT_THUMB          // Gamepad joystick pressed button right
  );

  // Gamepad axis
  TGamepadAxis = (
    GAMEPAD_AXIS_LEFT_X        = 0,     // Gamepad left stick X axis
    GAMEPAD_AXIS_LEFT_Y        = 1,     // Gamepad left stick Y axis
    GAMEPAD_AXIS_RIGHT_X       = 2,     // Gamepad right stick X axis
    GAMEPAD_AXIS_RIGHT_Y       = 3,     // Gamepad right stick Y axis
    GAMEPAD_AXIS_LEFT_TRIGGER  = 4,     // Gamepad back trigger left, pressure level: [1..-1]
    GAMEPAD_AXIS_RIGHT_TRIGGER = 5      // Gamepad back trigger right, pressure level: [1..-1]
  );

  // Material map index
  TMaterialMapIndex = (
      MATERIAL_MAP_ALBEDO  = 0,     // Albedo material (same as: MATERIAL_MAP_DIFFUSE)
      MATERIAL_MAP_METALNESS,         // Metalness material (same as: MATERIAL_MAP_SPECULAR)
      MATERIAL_MAP_NORMAL,            // Normal material
      MATERIAL_MAP_ROUGHNESS,         // Roughness material
      MATERIAL_MAP_OCCLUSION,         // Ambient occlusion material
      MATERIAL_MAP_EMISSION,          // Emission material
      MATERIAL_MAP_HEIGHT,            // Heightmap material
      MATERIAL_MAP_CUBEMAP,           // Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
      MATERIAL_MAP_IRRADIANCE,        // Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
      MATERIAL_MAP_PREFILTER,         // Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
      MATERIAL_MAP_BRDF               // Brdf material
  );

  //MATERIAL_MAP_DIFFUSE  =    MATERIAL_MAP_ALBEDO;
  //MATERIAL_MAP_SPECULAR =    MATERIAL_MAP_METALNESS;

  // Shader location index
  TShaderLocationIndex = (
    SHADER_LOC_VERTEX_POSITION = 0, // Shader location: vertex attribute: position
    SHADER_LOC_VERTEX_TEXCOORD01,   // Shader location: vertex attribute: texcoord01
    SHADER_LOC_VERTEX_TEXCOORD02,   // Shader location: vertex attribute: texcoord02
    SHADER_LOC_VERTEX_NORMAL,       // Shader location: vertex attribute: normal
    SHADER_LOC_VERTEX_TANGENT,      // Shader location: vertex attribute: tangent
    SHADER_LOC_VERTEX_COLOR,        // Shader location: vertex attribute: color
    SHADER_LOC_MATRIX_MVP,          // Shader location: matrix uniform: model-view-projection
    SHADER_LOC_MATRIX_VIEW,         // Shader location: matrix uniform: view (camera transform)
    SHADER_LOC_MATRIX_PROJECTION,   // Shader location: matrix uniform: projection
    SHADER_LOC_MATRIX_MODEL,        // Shader location: matrix uniform: model (transform)
    SHADER_LOC_MATRIX_NORMAL,       // Shader location: matrix uniform: normal
    SHADER_LOC_VECTOR_VIEW,         // Shader location: vector uniform: view
    SHADER_LOC_COLOR_DIFFUSE,       // Shader location: vector uniform: diffuse color
    SHADER_LOC_COLOR_SPECULAR,      // Shader location: vector uniform: specular color
    SHADER_LOC_COLOR_AMBIENT,       // Shader location: vector uniform: ambient color
    SHADER_LOC_MAP_ALBEDO,          // Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
    SHADER_LOC_MAP_METALNESS,       // Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
    SHADER_LOC_MAP_NORMAL,          // Shader location: sampler2d texture: normal
    SHADER_LOC_MAP_ROUGHNESS,       // Shader location: sampler2d texture: roughness
    SHADER_LOC_MAP_OCCLUSION,       // Shader location: sampler2d texture: occlusion
    SHADER_LOC_MAP_EMISSION,        // Shader location: sampler2d texture: emission
    SHADER_LOC_MAP_HEIGHT,          // Shader location: sampler2d texture: height
    SHADER_LOC_MAP_CUBEMAP,         // Shader location: samplerCube texture: cubemap
    SHADER_LOC_MAP_IRRADIANCE,      // Shader location: samplerCube texture: irradiance
    SHADER_LOC_MAP_PREFILTER,       // Shader location: samplerCube texture: prefilter
    SHADER_LOC_MAP_BRDF             // Shader location: sampler2d texture: brdf
  );

{const
  LOC_MAP_DIFFUSE  =    TShaderLocationIndex.LOC_MAP_ALBEDO;
  LOC_MAP_SPECULAR =    TShaderLocationIndex.LOC_MAP_METALNESS;} //TODO

  // Shader uniform data types
  TShaderUniformDataType = (
    SHADER_UNIFORM_FLOAT = 0,       // Shader uniform type: float
    SHADER_UNIFORM_VEC2,            // Shader uniform type: vec2 (2 float)
    SHADER_UNIFORM_VEC3,            // Shader uniform type: vec3 (3 float)
    SHADER_UNIFORM_VEC4,            // Shader uniform type: vec4 (4 float)
    SHADER_UNIFORM_INT,             // Shader uniform type: int
    SHADER_UNIFORM_IVEC2,           // Shader uniform type: ivec2 (2 int)
    SHADER_UNIFORM_IVEC3,           // Shader uniform type: ivec3 (3 int)
    SHADER_UNIFORM_IVEC4,           // Shader uniform type: ivec4 (4 int)
    SHADER_UNIFORM_SAMPLER2D        // Shader uniform type: sampler2d
  );

  // Shader attribute data types
  TShaderAttributeDataType = (
      SHADER_ATTRIB_FLOAT = 0,        // Shader attribute type: float
      SHADER_ATTRIB_VEC2,             // Shader attribute type: vec2 (2 float)
      SHADER_ATTRIB_VEC3,             // Shader attribute type: vec3 (3 float)
      SHADER_ATTRIB_VEC4              // Shader attribute type: vec4 (4 float)
  );

  // Pixel formats
  // NOTE: Support depends on OpenGL version and platform
  TPixelFormat = (
    PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1, // 8 bit per pixel (no alpha)
    PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA,    // 8*2 bpp (2 channels)
    PIXELFORMAT_UNCOMPRESSED_R5G6B5,        // 16 bpp
    PIXELFORMAT_UNCOMPRESSED_R8G8B8,        // 24 bpp
    PIXELFORMAT_UNCOMPRESSED_R5G5B5A1,      // 16 bpp (1 bit alpha)
    PIXELFORMAT_UNCOMPRESSED_R4G4B4A4,      // 16 bpp (4 bit alpha)
    PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,      // 32 bpp
    PIXELFORMAT_UNCOMPRESSED_R32,           // 32 bpp (1 channel - float)
    PIXELFORMAT_UNCOMPRESSED_R32G32B32,     // 32*3 bpp (3 channels - float)
    PIXELFORMAT_UNCOMPRESSED_R32G32B32A32,  // 32*4 bpp (4 channels - float)
    PIXELFORMAT_COMPRESSED_DXT1_RGB,        // 4 bpp (no alpha)
    PIXELFORMAT_COMPRESSED_DXT1_RGBA,       // 4 bpp (1 bit alpha)
    PIXELFORMAT_COMPRESSED_DXT3_RGBA,       // 8 bpp
    PIXELFORMAT_COMPRESSED_DXT5_RGBA,       // 8 bpp
    PIXELFORMAT_COMPRESSED_ETC1_RGB,        // 4 bpp
    PIXELFORMAT_COMPRESSED_ETC2_RGB,        // 4 bpp
    PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA,   // 8 bpp
    PIXELFORMAT_COMPRESSED_PVRT_RGB,        // 4 bpp
    PIXELFORMAT_COMPRESSED_PVRT_RGBA,       // 4 bpp
    PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA,   // 8 bpp
    PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA    // 2 bpp
  );

  // Texture parameters: filter mode
  // NOTE 1: Filtering considers mipmaps if available in the texture
  // NOTE 2: Filter is accordingly set for minification and magnification
  TTextureFilter = (
    TEXTURE_FILTER_POINT = 0,               // No filter, just pixel aproximation
    TEXTURE_FILTER_BILINEAR,                // Linear filtering
    TEXTURE_FILTER_TRILINEAR,               // Trilinear filtering (linear with mipmaps)
    TEXTURE_FILTER_ANISOTROPIC_4X,          // Anisotropic filtering 4x
    TEXTURE_FILTER_ANISOTROPIC_8X,          // Anisotropic filtering 8x
    TEXTURE_FILTER_ANISOTROPIC_16X         // Anisotropic filtering 16x
  );

  // Texture parameters: wrap mode
  TTextureWrap = (
    TEXTURE_WRAP_REPEAT = 0,                // Repeats texture in tiled mode
    TEXTURE_WRAP_CLAMP,                     // Clamps texture to edge pixel in tiled mode
    TEXTURE_WRAP_MIRROR_REPEAT,             // Mirrors and repeats the texture in tiled mode
    TEXTURE_WRAP_MIRROR_CLAMP               // Mirrors and clamps to border the texture in tiled mode
  );

  // Cubemap layout
  TCubemapLayout = (
    CUBEMAP_LAYOUT_AUTO_DETECT = 0,         // Automatically detect layout type
    CUBEMAP_LAYOUT_LINE_VERTICAL,           // Layout is defined by a vertical line with faces
    CUBEMAP_LAYOUT_LINE_HORIZONTAL,         // Layout is defined by an horizontal line with faces
    CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR,     // Layout is defined by a 3x4 cross with cubemap faces
    CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE,     // Layout is defined by a 4x3 cross with cubemap faces
    CUBEMAP_LAYOUT_PANORAMA                 // Layout is defined by a panorama image (equirectangular map)
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
    BLEND_MULTIPLIED,       // Blend textures multiplying colors
    BLEND_ADD_COLORS,       // Blend textures adding colors (alternative)
    BLEND_SUBTRACT_COLORS,  // Blend textures subtracting colors (alternative)
    BLEND_CUSTOM            // Belnd textures using custom src/dst factors (use rlSetBlendMode())
  );

  // Gesture
  // NOTE: It could be used as flags to enable only some gestures
  TGestureType = (
    GESTURE_NONE        = 0,        // No gesture
    GESTURE_TAP         = 1,        // Tap gesture
    GESTURE_DOUBLETAP   = 2,        // Double tap gesture
    GESTURE_HOLD        = 4,        // Hold gesture
    GESTURE_DRAG        = 8,        // Drag gesture
    GESTURE_SWIPE_RIGHT = 16,       // Swipe right gesture
    GESTURE_SWIPE_LEFT  = 32,       // Swipe left gesture
    GESTURE_SWIPE_UP    = 64,       // Swipe up gesture
    GESTURE_SWIPE_DOWN  = 128,      // Swipe down gesture
    GESTURE_PINCH_IN    = 256,      // Pinch in gesture
    GESTURE_PINCH_OUT   = 512       // Pinch out gesture
  );

  // Camera system modes
  TCameraMode = (
    CAMERA_CUSTOM = 0,              // Custom camera
    CAMERA_FREE,                    // Free camera
    CAMERA_ORBITAL,                 // Orbital camera
    CAMERA_FIRST_PERSON,            // First person camera
    CAMERA_THIRD_PERSON             // Third person camera
  );

  // Camera projection
  TCameraProjection = (
    CAMERA_PERSPECTIVE = 0,         // Perspective projection
    CAMERA_ORTHOGRAPHIC             // Orthographic projection
  );

  // N-patch layout
  TNPatchLayout = (
    NPATCH_NINE_PATCH = 0,          // Npatch layout: 3x3 tiles
    NPATCH_THREE_PATCH_VERTICAL,    // Npatch layout: 1x3 tiles
    NPATCH_THREE_PATCH_HORIZONTAL   // Npatch layout: 3x1 tiles
  );

  //TODO zaher, check params/arg please
  // Callbacks to hook some internal functions
  // WARNING: This callbacks are intended for advance users
  TTraceLogCallback = procedure(LogType: Integer; Text: PUTF8Char; Args: Pointer); cdecl;
  // FileIO: Load binary data
  TLoadFileDataCallback = function(FileName: PUTF8Char; var bytesRead: Cardinal): PByte; cdecl;
  // FileIO: Save binary data
  TSaveFileDataCallback = function(FileName: PUTF8Char; var data; var bytesToWrite: Cardinal): Boolean; cdecl;
  // FileIO: Load text data
  TLoadFileTextCallback = function(FileName: PUTF8Char): PUTF8Char; cdecl;
  // FileIO: Save text data
  TSaveFileTextCallback = procedure(FileName: PUTF8Char; text: PUTF8Char); cdecl;

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
  InitWindow: procedure(Width: Integer; Height: Integer; const Title: PUTF8Char); cdecl = nil;
  // Check if KEY_ESCAPE pressed or Close icon pressed
  WindowShouldClose: function: Boolean; cdecl = nil;
  // Close window and unload OpenGL context
  CloseWindow: procedure; cdecl = nil;
  // Check if window has been initialized successfully
  IsWindowReady: function: Boolean; cdecl = nil;
  // Check if window is currently fullscreen
  IsWindowFullscreen: function: Boolean; cdecl = nil;
  // Check if window is currently hidden (only PLATFORM_DESKTOP)
  IsWindowHidden: function: Boolean; cdecl = nil;
  // Check if window has been minimized (or lost focus)
  IsWindowMinimized: function: Boolean; cdecl = nil;
  // Check if window is currently maximized (only PLATFORM_DESKTOP)
  IsWindowMaximized: function: Boolean; cdecl = nil;
  // Check if window has been resized
  IsWindowResized: function: Boolean; cdecl = nil;
  // Check if one specific window flag is enabled
  IsWindowState: function(flag: Cardinal): Boolean; cdecl = nil;
  // Set window configuration state using flags
  SetWindowState: procedure(flag: Cardinal); cdecl = nil;
  // Clear window configuration state flags
  ClearWindowState: procedure(flag: Cardinal); cdecl = nil;
  // Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
  ToggleFullscreen: procedure; cdecl = nil;
  // Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
  MaximizeWindow: procedure; cdecl = nil;
  // Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
  MinimizeWindow: procedure; cdecl = nil;
  // Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
  RestoreWindow : procedure; cdecl = nil;
  // Set icon for window (only PLATFORM_DESKTOP)
  SetWindowIcon: procedure(image: TImage); cdecl = nil;
  // Set title for window (only PLATFORM_DESKTOP)
  SetWindowTitle: procedure(const title: PUTF8Char); cdecl = nil;
  // Set window position on screen (only PLATFORM_DESKTOP)
  SetWindowPosition: procedure(x: Integer; y: Integer); cdecl = nil;
  // Set monitor for the current window (fullscreen mode)
  SetWindowMonitor: procedure(monitor: Integer); cdecl = nil;
  // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
  SetWindowMinSize: procedure(width: Integer; height: Integer); cdecl = nil;
  // Set window dimensions
  SetWindowSize: procedure(width: Integer; height: Integer); cdecl = nil;
  // Get native window handle
  GetWindowHandle: function: Pointer; cdecl = nil;
  // Get current screen width
  GetScreenWidth: function: Integer; cdecl = nil;
  // Get current screen height
  GetScreenHeight: function: Integer; cdecl = nil;
  // Get number of connected monitors
  GetMonitorCount: function: Integer; cdecl = nil;
  // Get current connected monitor
  GetCurrentMonitor: function(): Integer; cdecl = nil;
  // Get specified monitor position
  GetMonitorPosition: function(monitor: Integer): TVector2; cdecl = nil;
  // Get primary monitor width
  GetMonitorWidth: function(monitor: Integer): Integer; cdecl = nil;
  // Get primary monitor height
  GetMonitorHeight: function(monitor: Integer): Integer; cdecl = nil;
  // Get primary monitor physical width in millimetres
  GetMonitorPhysicalWidth: function(monitor: Integer): Integer; cdecl = nil;
  // Get primary monitor physical height in millimetres
  GetMonitorPhysicalHeight: function(monitor: Integer): Integer; cdecl = nil;
  // Get specified monitor refresh rate
  GetMonitorRefreshRate: function(monitor: Integer): Integer; cdecl = nil;
  // Get window position XY on monitor
  GetWindowPosition: function(monitor: Integer): TVector2; cdecl = nil;
  // Get window scale DPI factor
  GetWindowScaleDPI: function(monitor: Integer): TVector2; cdecl = nil;
  // Get the human-readable, UTF-8 encoded name of the primary monitor
  GetMonitorName: function(monitor: Integer): PUTF8Char; cdecl = nil;
  // Set clipboard text content
  SetClipboardText: procedure(const text: PUTF8Char); cdecl = nil;
  // Get clipboard text content
  GetClipboardText: function: PUTF8Char; cdecl = nil;

  // Custom frame control functions
  // NOTE: Those functions are intended for advance users that want full control over the frame processing
  // By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timming + PollInputEvents()
  // To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
  SwapScreenBuffer: procedure; cdecl = nil;                               // Swap back buffer with front buffer (screen drawing)
  PollInputEvents: procedure; cdecl = nil;                                // Register all input events
  WaitTime: procedure(ms: Single); cdecl = nil;                           // Wait for some milliseconds (halt program execution)

  { Cursor-related functions }

  // Shows cursor
  ShowCursor: procedure; cdecl = nil;
  // Hides cursor
  HideCursor: procedure; cdecl = nil;
  // Check if cursor is not visible
  IsCursorHidden: function: Boolean; cdecl = nil;
  // Enables cursor (unlock cursor)
  EnableCursor: procedure; cdecl = nil;
  // Disables cursor (lock cursor)
  DisableCursor: procedure; cdecl = nil;
  // Check if cursor is on the current screen.
  IsCursorOnScreen: function: Boolean; cdecl = nil;

  { Drawing-related functions }

  // Set background color (framebuffer clear color)
  ClearBackground: procedure(color: TColor); cdecl = nil;
  // Setup canvas (framebuffer) to start drawing
  BeginDrawing: procedure; cdecl = nil;
  // End canvas drawing and swap buffers (double buffering)
  EndDrawing: procedure; cdecl = nil;
  // Begin 2D mode with custom camera (2D)
  BeginMode2D: procedure(Camera: TCamera2D); cdecl = nil;
  // Ends 2D mode with custom camera
  EndMode2D: procedure; cdecl = nil;
  // Begin 3D mode with custom camera (3D)
  BeginMode3D: procedure(Camera: TCamera3D); cdecl = nil;
  // Ends 3D mode and returns to default 2D orthographic mode
  EndMode3D: procedure; cdecl = nil;
  // Begin drawing to render texture
  BeginTextureMode: procedure(Target: TRenderTexture2D); cdecl = nil;
  // Ends drawing to render texture
  EndTextureMode: procedure; cdecl = nil;

  // Begin custom shader drawing
  BeginShaderMode: procedure(Shader: TShader); cdecl = nil;
  // End custom shader drawing (use default shader)
  EndShaderMode: procedure; cdecl = nil;
  // Begin blending mode (alpha, additive, multiplied, subtract, custom)
  BeginBlendMode: procedure(mode: integer); cdecl = nil;
  // End blending mode (reset to default: alpha blending)
  EndBlendMode: procedure; cdecl = nil;
  // Begin scissor mode (define screen area for following drawing)
  BeginScissorMode: procedure(x: Integer; y: Integer; width: Integer; height: Integer); cdecl = nil;
  // End scissor mode
  EndScissorMode: procedure; cdecl = nil;
  // Begin stereo rendering (requires VR simulator)
  BeginVrStereoMode: procedure(Config: TVrStereoConfig); cdecl = nil;
  // End stereo rendering (requires VR simulator)
  EndVrStereoMode: procedure; cdecl = nil;

  // VR stereo config functions for VR simulator
  // Load VR stereo config for VR simulator device parameters
  LoadVrStereoConfig: function(Device: TVrDeviceInfo): TVrStereoConfig; cdecl = nil;
  // Unload VR stereo config
  UnloadVrStereoConfig: procedure(Config: TVrStereoConfig); cdecl = nil;

  // Shader management functions
  // NOTE: Shader functionality is not available on OpenGL 1.1
  // Load shader from files and bind default locations
  LoadShader: function(vsFileName: PUTF8Char; fsFileName: PUTF8Char): TShader; cdecl = nil;
  // Load shader from code strings and bind default locations
  LoadShaderFromMemory: function(vsCode: PUTF8Char; fsCode: PUTF8Char): TShader; cdecl = nil;
  // Get shader uniform location
  GetShaderLocation: function(Shader: TShader; UniformName: PUTF8Char): integer; cdecl = nil;
  // Get shader attribute location
  GetShaderLocationAttrib: function(Shader: TShader; AttribName: PUTF8Char): integer; cdecl = nil;
  // Set shader uniform value
  SetShaderValue: procedure(Shader: TShader; LocIndex: Integer; Value: PUTF8Char; uniformType: Integer); cdecl = nil;
  // Set shader uniform value vector
  SetShaderValueV: procedure(Shader: TShader; LocIndex: Integer; var Value, UniformType: Integer; Count: Integer); cdecl = nil;
  // Set shader uniform value (matrix 4x4)
  SetShaderValueMatrix: procedure(Shader: TShader; locIndex: Integer; mat: TMatrix); cdecl = nil;
  // Set shader uniform value for texture (sampler2d)
  SetShaderValueTexture: procedure(Shader: TShader; LocIndex: Integer; Texture: TTexture2D); cdecl = nil;
  // Unload shader from GPU memory (VRAM)
  UnloadShader: procedure(Shader: TShader); cdecl = nil;

  // Screen-space-related functions

  // Returns a ray trace from mouse position
  GetMouseRay: function(mousePosition: TVector2; camera: TCamera): TRay; cdecl = nil;
  // Returns camera transform matrix (view matrix)
  GetCameraMatrix: function(camera: TCamera): TMatrix; cdecl = nil;
  // Returns camera 2d transform matrix
  GetCameraMatrix2D: function(camera: TCamera2D): TMatrix; cdecl = nil;
  // Returns the screen space position for a 3d world space position
  GetWorldToScreen: function(position: TVector3; camera: TCamera): TVector2; cdecl = nil;
  // Returns size position for a 3d world space position
  GetWorldToScreenEx: function(position: TVector3; camera: TCamera; width: Integer; height: Integer): TVector2; cdecl = nil;
  // Returns the screen space position for a 2d camera world space position
  GetWorldToScreen2D: function(position: TVector2; camera: TCamera2D): TVector2; cdecl = nil;
  // Returns the world space position for a 2d camera screen space position
  GetScreenToWorld2D: function(position: TVector2; camera: TCamera2D): TVector2; cdecl = nil;

  { Timing-related functions }

  // Set target FPS (maximum)
  SetTargetFPS: procedure(fps: Integer); cdecl = nil;
  // Get current FPS
  GetFPS: function: Integer; cdecl = nil;
  // Get time in seconds for last frame drawn (delta time)
  GetFrameTime: function: Single; cdecl = nil;
  // Get elapsed time in seconds since InitWindow()
  GetTime: function: Double; cdecl = nil;

  { Misc. functions }

  // Get a random value between min and max (both included)
  GetRandomValue: function(min: Integer; max: Integer): Integer; cdecl = nil;
  // Set the seed for the random number generator
  SetRandomSeed: procedure(Seed: Cardinal); cdecl = nil;
  // Takes a screenshot of current screen (filename extension defines format)
  TakeScreenshot: procedure(const fileName: PUTF8Char); cdecl = nil;

  // Setup window configuration flags (view FLAGS)
  SetConfigFlags: procedure(flags: TConfigFlags); cdecl = nil;
  // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)
  TraceLog: procedure(logType: Integer; const text: PUTF8Char) varargs; cdecl = nil;
  // Set the current threshold (minimum) log level
  SetTraceLogLevel: procedure(logType: TTraceLogLevels); cdecl = nil;

  // Internal memory allocator
  MemAlloc: function(Size: Integer): Pointer; cdecl = nil;
  // Internal memory reallocator
  MemRealloc: procedure(Ptr: Pointer; Size: Integer); cdecl = nil;
  // Internal memory free
  MemFree: procedure(ptr: Pointer); cdecl = nil;

  // Set custom callbacks
  // WARNING: Callbacks setup is intended for advance users
  // Set custom trace log
  SetTraceLogCallback: procedure(callback: TTraceLogCallback); cdecl = nil;
  // Set custom file binary data loader
  SetLoadFileDataCallback: procedure(callback: TLoadFileDataCallback); cdecl = nil;
  // Set custom file binary data saver
  SetSaveFileDataCallback: procedure(callback: TSaveFileDataCallback); cdecl = nil;
  // Set custom file text data loader
  SetLoadFileTextCallback: procedure(callback: TLoadFileTextCallback); cdecl = nil;
  // Set custom file text data saver
  SetSaveFileTextCallback: procedure(callback: TSaveFileTextCallback); cdecl = nil;

  { Files management functions }

  // Load file data as byte array (read)
  LoadFileData: function(const fileName: PUTF8Char; bytesRead: PCardinal): PByte; cdecl = nil;
  // Unload file data allocated by LoadFileData()
  UnloadFileData: procedure(Data: PByte); cdecl = nil;
  // Save data to file from byte array (write)
  SaveFileData: procedure(const fileName: PUTF8Char; data: Pointer; bytesToWrite: Cardinal); cdecl = nil;
  // Load text data from file (read), returns a '\0' terminated string
  LoadFileText: function(const fileName: PUTF8Char): PUTF8Char; cdecl = nil;
  // Unload file text data allocated by LoadFileText()
  UnloadFileText: procedure(Text: PUTF8Char); cdecl = nil;
  // Save text data to file (write), string must be '\0' terminated
  SaveFileText: procedure(const fileName: PUTF8Char; text: PUTF8Char); cdecl = nil;
  // Check if file exists
  FileExists: function(const fileName: PUTF8Char): Boolean; cdecl = nil;
  // Check if a directory path exists
  DirectoryExists: function(const dirPath: PUTF8Char): Boolean; cdecl = nil;
  // Check file extension
  IsFileExtension: function(const fileName: PUTF8Char; const ext: PUTF8Char): Boolean; cdecl = nil;
  // Get pointer to extension for a filename string (includes dot: '.png')
  GetFileExtension: function(const fileName: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get pointer to filename for a path string
  GetFileName: function(const filePath: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get filename string without extension (uses static string)
  GetFileNameWithoutExt: function(const filePath: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get full path for a given fileName with path (uses static string)
  GetDirectoryPath: function(const filePath: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get previous directory path for a given path (uses static string)
  GetPrevDirectoryPath: function(const dirPath: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get current working directory (uses static string)
  GetWorkingDirectory: function: PUTF8Char; cdecl = nil;
  // Get filenames in a directory path (memory should be freed)
  GetDirectoryFiles: function(const dirPath: PUTF8Char; count: PInteger): PPUTF8Char; cdecl = nil;
  // Clear directory files paths buffers (free memory)
  ClearDirectoryFiles: procedure; cdecl = nil;
  // Change working directory, returns true if success
  ChangeDirectory: function(const dir: PUTF8Char): Boolean; cdecl = nil;
  // Check if a file has been dropped into window
  IsFileDropped: function: Boolean; cdecl = nil;
  // Get dropped files names (memory should be freed)
  GetDroppedFiles: function(count: PInteger): PPUTF8Char; cdecl = nil;
  // Clear dropped files paths buffer (free memory)
  ClearDroppedFiles: procedure; cdecl = nil;
  // Get file modification time (last write time)
  GetFileModTime: function(const fileName: PUTF8Char): Integer; cdecl = nil;

  // Compression/Encoding functionality
  // Compress data (DEFLATE algorythm)
  CompressData: function(data: PByte; dataLength: Integer; compDataLength: PInteger): PByte; cdecl = nil;
  // Decompress data (DEFLATE algorythm)
  DecompressData: function(compData: PByte; compDataLength: Integer; dataLength: PInteger): PByte; cdecl = nil;
  // Encode data to Base64 string
  EncodeDataBase64: function(Data: PByte; dataLength: Integer; OutputLength: PInteger): PByte; cdecl = nil;  //TODO zaher, check it by example please
  // Decode Base64 string data
  DecodeDataBase64: function(Data: PByte; outputLength: PInteger): PByte;  //TODO zaher, check it by example please

  { Persistent storage management }

  // Save integer value to storage file (to defined position)
  SaveStorageValue: function(position: Cardinal; value: Integer): Boolean; cdecl = nil;
  // Load integer value from storage file (from defined position)
  LoadStorageValue: function(position: Cardinal): Integer; cdecl = nil;

  // Open URL with default system browser (if available)
  OpenURL: procedure(const url: PUTF8Char); cdecl = nil;

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

  { Input-related functions: keyboard }

  // Check if a key has been pressed once
  IsKeyPressed: function(key: Integer): Boolean; cdecl = nil;
  // Check if a key is being pressed
  IsKeyDown: function(key: Integer): Boolean; cdecl = nil;
  // Check if a key has been released once
  IsKeyReleased: function(key: Integer): Boolean; cdecl = nil;

  { Input-related functions: gamepads }

  // Detect if a key is NOT being pressed
  IsKeyUp: function(key: Integer): Boolean; cdecl = nil;
  // Set a custom key to exit program (default is ESC)
  SetExitKey: procedure(key: Integer); cdecl = nil;
  // Get key pressed, call it multiple times for chars queued, returns 0 when the queue is empty
  GetKeyPressed: function: Integer; cdecl = nil;
  // Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
  GetCharPressed: function: Integer; cdecl = nil;

  // Input-related functions: gamepads

  // Check if a gamepad is available
  IsGamepadAvailable: function(gamepad: Integer): Boolean; cdecl = nil;
  // Get gamepad internal name id
  GetGamepadName: function(gamepad: Integer): PUTF8Char; cdecl = nil;
  // Check if a gamepad button has been pressed once
  IsGamepadButtonPressed: function(gamepad: Integer; button: Integer): Boolean; cdecl = nil;
   // Check if a gamepad button is being pressed
  IsGamepadButtonDown: function(gamepad: Integer; button: Integer): Boolean; cdecl = nil;
  // Check if a gamepad button has been released once
  IsGamepadButtonReleased: function(gamepad: Integer; button: Integer): Boolean; cdecl = nil;
  // Check if a gamepad button is NOT being pressed
  IsGamepadButtonUp: function(gamepad: Integer; button: Integer): Boolean; cdecl = nil;
  // Get the last gamepad button pressed
  GetGamepadButtonPressed: function: Integer; cdecl = nil;
  // Return gamepad axis count for a gamepad
  GetGamepadAxisCount: function(gamepad: Integer): Integer; cdecl = nil;
  // Return axis movement value for a gamepad axis
  GetGamepadAxisMovement: function(gamepad: Integer; axis: Integer): Single; cdecl = nil;
  // Set internal gamepad mappings (SDL_GameControllerDB)
  SetGamepadMappings: function(mappings: PUTF8Char): Integer; cdecl = nil;

  { Input-related functions: mouse }

  // Check if a mouse button has been pressed once
  IsMouseButtonPressed: function(button: TMouseButton): Boolean; cdecl = nil;
  // Check if a mouse button is being pressed
  IsMouseButtonDown: function(button: TMouseButton): Boolean; cdecl = nil;
  // Check if a mouse button has been released once
  IsMouseButtonReleased: function(button: TMouseButton): Boolean; cdecl = nil;
  // Check if a mouse button is NOT being pressed
  IsMouseButtonUp: function(button: TMouseButton): Boolean; cdecl = nil;
  // Get mouse position X
  GetMouseX: function: Integer; cdecl = nil;
  // Get mouse position Y
  GetMouseY: function: Integer; cdecl = nil;
  {$ifdef FPC}
  // Get mouse position XY
  GetMousePosition: function: TVector2; cdecl = nil;
  // Get mouse delta between frames
  GetMouseDelta: function: TVector2; cdecl = nil;
  {$else} //Stupid Delphi
  // Get mouse position XY
  GetMousePosition: function: Int64; cdecl = nil;
  // Get mouse delta between frames
  GetMouseDelta: function: Int64; cdecl = nil;
  {$endif}
  // Set mouse position XY
  SetMousePosition: procedure(x: Integer; y: Integer); cdecl = nil;
  // Set mouse offset
  SetMouseOffset: procedure(offsetX: Integer; offsetY: Integer); cdecl = nil;
  // Set mouse scaling
  SetMouseScale: procedure(scaleX: Single; scaleY: Single); cdecl = nil;
  // Get mouse wheel movement Y
  GetMouseWheelMove: function: Single; cdecl = nil;
  // Set mouse cursor
  SetMouseCursor: procedure(Cursor: Integer); cdecl = nil;

  { Input-related functions: touch }

  // Get touch position X for touch point 0 (relative to screen size)
  GetTouchX: function: Integer; cdecl = nil;
  // Get touch position Y for touch point 0 (relative to screen size)
  GetTouchY: function: Integer; cdecl = nil;
  // Get touch position XY for a touch point index (relative to screen size)
  GetTouchPosition: function(index: Integer): TVector2; cdecl = nil;
  // Get touch point identifier for given index
  GetTouchPointId: function(index: Integer): Integer; cdecl = nil;
  // Get number of touch points
  GetTouchPointCount: function: Integer; cdecl = nil;

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: rgestures)
//------------------------------------------------------------------------------------

  // Enable a set of gestures using flags
  SetGesturesEnabled: procedure(Flags: Cardinal); cdecl = nil;
  // Check if a gesture have been detected
  IsGestureDetected: function(gesture: Integer): Boolean; cdecl = nil;
  // Get latest detected gesture
  GetGestureDetected: function: Integer; cdecl = nil;
  // Get gesture hold time in milliseconds
  GetGestureHoldDuration: function: Single; cdecl = nil;
  // Get gesture drag vector
  GetGestureDragVector: function: TVector2; cdecl = nil;
  // Get gesture drag angle
  GetGestureDragAngle: function: Single; cdecl = nil;
  // Get gesture pinch delta
  GetGesturePinchVector: function: TVector2; cdecl = nil;
  // Get gesture pinch angle
  GetGesturePinchAngle: function: Single; cdecl = nil;

//------------------------------------------------------------------------------------
// Camera System Functions (Module: rcamera)
//------------------------------------------------------------------------------------

  // Set camera mode (multiple camera modes available)
  SetCameraMode: procedure(camera: TCamera; mode: Integer); cdecl = nil;
  // Update camera position for selected mode
  UpdateCamera: procedure(camera: PCamera); cdecl = nil;

  // Set camera pan key to combine with mouse movement (free camera)
  SetCameraPanControl: procedure(KeyPan: Integer); cdecl = nil;
  // Set camera alt key to combine with mouse movement (free camera)
  SetCameraAltControl: procedure(Keylt: Integer); cdecl = nil;
  // Set camera smooth zoom key to combine with mouse (free camera)
  SetCameraSmoothZoomControl: procedure(keySmoothZoom: Integer); cdecl = nil;
  // Set camera move controls (1st person and 3rd person cameras)
  SetCameraMoveControls: procedure(KeyFront: Integer; KeyBack: Integer; KeyRight: Integer; KeyLeft: Integer; KeyUp: Integer; KeyDown: Integer); cdecl = nil;

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------
  // Set texture and rectangle to be used on shapes drawing
  // NOTE: It can be useful when using basic shapes and one single font,
  // defining a font char white rectangle would allow drawing everything in a single draw call

  // Set texture and rectangle to be used on shapes drawing
  SetShapesTexture: procedure(texture: TTexture2D; Source: TRectangle); cdecl = nil;

  { Basic shapes drawing functions }

  // Draw a pixel
  DrawPixel: procedure(posX: Integer; posY: Integer; color: TColor); cdecl = nil;
  // Draw a pixel (Vector version)
  DrawPixelV: procedure(position: TVector2; color: TColor); cdecl = nil;
  // Draw a line
  DrawLine: procedure(startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl = nil;
  // Draw a line (Vector version)
  DrawLineV: procedure(startPos: TVector2; endPos: TVector2; color: TColor); cdecl = nil;
  // Draw a line defining thickness
  DrawLineEx: procedure(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl = nil;
  // Draw a line using cubic-bezier curves in-out
  DrawLineBezier: procedure(startPos: TVector2; endPos: TVector2; thick: Single; color: TColor); cdecl = nil;
  // Draw line using quadratic bezier curves with a control point
  DrawLineBezierQuad: procedure(startPos: TVector2; endPos: TVector2; controlPos: TVector2; thick: Single; Color: TColor); cdecl = nil;
  // Draw line using cubic bezier curves with 2 control points
  DrawLineBezierCubic: procedure(startPos: TVector2; endPos: TVector2; startControlPos: TVector2; endControlPos: TVector2; thick: Single; Color: TColor); cdecl = nil;
  // Draw a color-filled circle
  DrawCircle: procedure(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl = nil;
  // Draw a piece of a circle
  DrawCircleSector: procedure(center: TVector2; radius: Single; startAngle: Single; endAngle: Single; segments: Integer; color: TColor); cdecl = nil;
  // Draw circle sector outline
  DrawCircleSectorLines: procedure(center: TVector2; radius: Single; startAngle: Single; endAngle: Single; segments: Integer; color: TColor); cdecl = nil;
  // Draw a gradient-filled circle
  DrawCircleGradient: procedure(centerX: Integer; centerY: Integer; radius: Single; color1: TColor; color2: TColor); cdecl = nil;
  // Draw a color-filled circle (Vector version)
  DrawCircleV: procedure(center: TVector2; radius: Single; color: TColor); cdecl = nil;
  // Draw circle outline
  DrawCircleLines: procedure(centerX: Integer; centerY: Integer; radius: Single; color: TColor); cdecl = nil;
  // Draw ellipse
  DrawEllipse: procedure(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl = nil;
  // Draw ellipse outline
  DrawEllipseLines: procedure(centerX: Integer; centerY: Integer; radiusH: Single; radiusV: Single; color: TColor); cdecl = nil;
  // Draw ring
  DrawRing: procedure(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Single; endAngle: Single; segments: Integer; color: TColor); cdecl = nil;
  // Draw ring outline
  DrawRingLines: procedure(center: TVector2; innerRadius: Single; outerRadius: Single; startAngle: Single; endAngle: Single; segments: Integer; color: TColor); cdecl = nil;
  // Draw a color-filled rectangle
  DrawRectangle: procedure(posX: Integer; posY: Integer; Width: Integer; Height: Integer; Color: TColor); cdecl = nil;
  // Draw a color-filled rectangle (Vector version)
  DrawRectangleV: procedure(position: TVector2; size: TVector2; color: TColor); cdecl = nil;
  // Draw a color-filled rectangle
  DrawRectangleRec: procedure(rec: TRectangle; color: TColor); cdecl = nil;
  // Draw a color-filled rectangle with pro parameters
  DrawRectanglePro: procedure(rec: TRectangle; origin: TVector2; rotation: Single; color: TColor); cdecl = nil;
  // Draw a vertical-gradient-filled rectangle
  DrawRectangleGradientV: procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl = nil;
  // Draw a horizontal-gradient-filled rectangle
  DrawRectangleGradientH: procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color1: TColor; color2: TColor); cdecl = nil;
  // Draw a gradient-filled rectangle with custom vertex colors
  DrawRectangleGradientEx: procedure(rec: TRectangle; col1: TColor; col2: TColor; col3: TColor; col4: TColor); cdecl = nil;
  // Draw rectangle outline
  DrawRectangleLines: procedure(posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl = nil;
  // Draw rectangle outline with extended parameters
  DrawRectangleLinesEx: procedure(rec: TRectangle; lineThick: Single; color: TColor); cdecl = nil;
  // Draw rectangle with rounded edges
  DrawRectangleRounded: procedure(rec: TRectangle; roundness: Single; segments: Integer; color: TColor); cdecl = nil;
  // Draw rectangle with rounded edges outline
  DrawRectangleRoundedLines: procedure(rec: TRectangle; roundness: Single; segments: Integer; lineThick: Single; color: TColor); cdecl = nil;
  // Draw a color-filled triangle (vertex in counter-clockwise order!)
  DrawTriangle: procedure(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl = nil;
  // Draw triangle outline (vertex in counter-clockwise order!)
  DrawTriangleLines: procedure(v1: TVector2; v2: TVector2; v3: TVector2; color: TColor); cdecl = nil;
  // Draw a triangle fan defined by points (first vertex is the center)
  DrawTriangleFan: procedure(points: PVector2; PointCount: Integer; color: TColor); cdecl = nil;
  // Draw a triangle strip defined by points
  DrawTriangleStrip: procedure(points: PVector2; pointCount: Integer; color: TColor); cdecl = nil;
  // Draw a regular polygon (Vector version)
  DrawPoly: procedure(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl = nil;
  // Draw a polygon outline of n sides
  DrawPolyLines: procedure(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColor); cdecl = nil;
  // Draw a polygon outline of n sides with extended parameters
  DrawPolyLinesEx: procedure(center: TVector2; Sides: Integer; Radius: Single; Rotation: Single; lineThick: Single; Color: TColor); cdecl = nil;

  { Basic shapes collision detection functions }

  // Check collision between two rectangles
  CheckCollisionRecs: function(rec1: TRectangle; rec2: TRectangle): Boolean; cdecl = nil;
  // Check collision between two circles
  CheckCollisionCircles: function(center1: TVector2; radius1: Single; center2: TVector2; radius2: Single): Boolean; cdecl = nil;
  // Check collision between circle and rectangle
  CheckCollisionCircleRec: function(center: TVector2; radius: Single; rec: TRectangle): Boolean; cdecl = nil;
  // Check if point is inside rectangle
  CheckCollisionPointRec: function(point: TVector2; rec: TRectangle): Boolean; cdecl = nil;
  // Check if point is inside circle
  CheckCollisionPointCircle: function(point: TVector2; center: TVector2; radius: Single): Boolean; cdecl = nil;
  // Check if point is inside a triangle
  CheckCollisionPointTriangle: function(point: TVector2; p1: TVector2; p2: TVector2; p3: TVector2): Boolean; cdecl = nil;
  // Check the collision between two lines defined by two points each, returns collision point by reference
  CheckCollisionLines: function(startPos1, endPos1, startPos2, endPos2: TVector2; collisionPoint: PVector2): Boolean; cdecl = nil;
  // Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
  CheckCollisionPointLine: function(point: TVector2; p1: TVector2; p2: TVector2; threshold: Integer): Boolean; cdecl = nil;
  // Get collision rectangle for two rectangles collision
  GetCollisionRec: function(rec1: TRectangle; rec2: TRectangle): TRectangle; cdecl = nil;

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

  { Image loading functions }
  // NOTE: This functions do not require GPU access

  // Load image from file into CPU memory (RAM)
  LoadImage: function(const fileName: PUTF8Char): TImage; cdecl = nil;
  // Load image from RAW file data
  LoadImageRaw: function(const fileName: PUTF8Char; width: Integer; height: Integer; format: Integer; headerSize: Integer): TImage; cdecl = nil;
  // Load image sequence from file (frames appended to image.data)
  LoadImageAnim: function(const fileName: PUTF8Char; frames: Integer): TImage; cdecl = nil;
  // Load image from memory buffer, fileType refers to extension: i.e. "png"
  LoadImageFromMemory: function(const fileType: PUTF8Char; fileData: PByte; dataSize: Integer): TImage; cdecl = nil;
  // Load image from GPU texture data
  LoadImageFromTexture: function(texture: TTexture2D): TImage; cdecl = nil;
  // Load image from screen buffer and (screenshot)
  LoadImageFromScreen: function(): TImage; cdecl = nil;
  // Unload image from CPU memory (RAM)
  UnloadImage: procedure(image: TImage); cdecl = nil;
  // Export image data to file
  ExportImage: procedure(image: TImage; const fileName: PUTF8Char); cdecl = nil;
  // Export image as code file defining an array of bytes
  ExportImageAsCode: procedure(image: TImage; const fileName: PUTF8Char); cdecl = nil;

  { Image generation functions }

  // Generate image: plain color
  GenImageColor: function(width: Integer; height: Integer; color: TColor): TImage; cdecl = nil;
  // Generate image: vertical gradient
  GenImageGradientV: function(width: Integer; height: Integer; top: TColor; bottom: TColor): TImage; cdecl = nil;
  // Generate image: horizontal gradient
  GenImageGradientH: function(width: Integer; height: Integer; left: TColor; right: TColor): TImage; cdecl = nil;
  // Generate image: radial gradient
  GenImageGradientRadial: function(width: Integer; height: Integer; density: Single; inner: TColor; outer: TColor): TImage; cdecl = nil;
  // Generate image: checked
  GenImageChecked: function(width: Integer; height: Integer; checksX: Integer; checksY: Integer; col1: TColor; col2: TColor): TImage; cdecl = nil;
  // Generate image: white noise
  GenImageWhiteNoise: function(width: Integer; height: Integer; factor: Single): TImage; cdecl = nil;
  // Generate image: cellular algorithm, bigger tileSize means bigger cells
  GenImageCellular: function(width: Integer; height: Integer; tileSize: Integer): TImage; cdecl = nil;

  { Image manipulation functions }

  // Create an image duplicate (useful for transformations)
  ImageCopy: function(image: TImage): TImage; cdecl = nil;
  // Create an image from another image piece
  ImageFromImage: function(image: TImage; rec: TRectangle): TImage; cdecl = nil;
  // Create an image from text (default font)
  ImageText: function(const text: PUTF8Char; fontSize: Integer; color: TColor): TImage; cdecl = nil;
  // Create an image from text (custom sprite font)
  ImageTextEx: function(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single; tint: TColor): TImage; cdecl = nil;
  // Convert image to POT (power-of-two)
  ImageToPOT: procedure(image: PImage; fillColor: TColor); cdecl = nil;
  // Convert image data to desired format
  ImageFormat: procedure(image: PImage; newFormat: Integer); cdecl = nil;
  // Crop an image to a defined rectangle
  ImageCrop: procedure(image: PImage; crop: TRectangle); cdecl = nil;
  // Crop image depending on alpha value
  ImageAlphaCrop: procedure(image: PImage; threshold: Single); cdecl = nil;
  // Clear alpha channel to desired color
  ImageAlphaClear: procedure(image: PImage; color: TColor; threshold: Single); cdecl = nil;
  // Apply alpha mask to image
  ImageAlphaMask: procedure(image: PImage; alphaMask: TImage); cdecl = nil;
  // Premultiply alpha channel
  ImageAlphaPremultiply: procedure(image: PImage); cdecl = nil;
  // Resize image (Bicubic scaling algorithm)
  ImageResize: procedure(image: PImage; newWidth: Integer; newHeight: Integer); cdecl = nil;
  // Resize image (Nearest-Neighbor scaling algorithm)
  ImageResizeNN: procedure(image: PImage; newWidth: Integer; newHeight: Integer); cdecl = nil;
  // Resize canvas and fill with color
  ImageResizeCanvas: procedure(image: PImage; newWidth: Integer; newHeight: Integer; offsetX: Integer; offsetY: Integer; color: TColor); cdecl = nil;
  // Compute all mipmap levels for a provided image
  ImageMipmaps: procedure(image: PImage); cdecl = nil;
  // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
  ImageDither: procedure(image: PImage; rBpp: Integer; gBpp: Integer; bBpp: Integer; aBpp: Integer); cdecl = nil;
  // Flip image vertically
  ImageFlipVertical: procedure(image: PImage); cdecl = nil;
  // Flip image horizontally
  ImageFlipHorizontal: procedure(image: PImage); cdecl = nil;
  // Rotate image clockwise 90deg
  ImageRotateCW: procedure(image: PImage); cdecl = nil;
  // Rotate image counter-clockwise 90deg
  ImageRotateCCW: procedure(image: PImage); cdecl = nil;
  // Modify image color: tint
  ImageColorTint: procedure(image: PImage; color: TColor); cdecl = nil;
  // Modify image color: invert
  ImageColorInvert: procedure(image: PImage); cdecl = nil;
  // Modify image color: grayscale
  ImageColorGrayscale: procedure(image: PImage); cdecl = nil;
  // Modify image color: contrast (-100 to 100)
  ImageColorContrast: procedure(image: PImage; contrast: Single); cdecl = nil;
  // Modify image color: brightness (-255 to 255)
  ImageColorBrightness: procedure(image: PImage; brightness: Integer); cdecl = nil;
  // Modify image color: replace color
  ImageColorReplace: procedure(image: PImage; color: TColor; replace: TColor); cdecl = nil;
  // Get pixel data from image as a Color struct array
  LoadImageColors: function(image: TImage): PColor; cdecl = nil;
  // Extract color palette from image to maximum size (memory should be freed)
  LoadImagePalette: function(image: TImage; maxPaletteSize: Integer; colorCount: PInteger): PColor; cdecl = nil;
  // Unload color data loaded with LoadImageColors()
  UnloadImageColors: procedure(Colors: PColor); cdecl = nil;
  // Unload colors palette loaded with LoadImagePalette()
  UnloadImagePalette: procedure(Colors: PColor); cdecl = nil;
  // Get image alpha border rectangle
  GetImageAlphaBorder: function(image: TImage; threshold: Single): TRectangle; cdecl = nil;
  // Get image pixel color at (x, y) position
  GetImageColor: function(Image: TImage; x, y: Integer): TColor; cdecl = nil;

  { Image drawing functions }
  // NOTE: Image software-rendering functions (CPU)

  // Clear image background with given color
  ImageClearBackground: procedure(dst: PImage; color: TColor); cdecl = nil;
  // Draw pixel within an image
  ImageDrawPixel: procedure(dst: PImage; posX: Integer; posY: Integer; color: TColor); cdecl = nil;
  // Draw pixel within an image (Vector version)
  ImageDrawPixelV: procedure(dst: PImage; position: TVector2; color: TColor); cdecl = nil;
  // Draw line within an image
  ImageDrawLine: procedure(dst: PImage; startPosX: Integer; startPosY: Integer; endPosX: Integer; endPosY: Integer; color: TColor); cdecl = nil;
  // Draw line within an image (Vector version)
  ImageDrawLineV: procedure(dst: PImage; start: TVector2; &end: TVector2; color: TColor); cdecl = nil;
  // Draw circle within an image
  ImageDrawCircle: procedure(const dst: TImage; CenterX, CenterY, Radius: Integer; Color: TColor); cdecl = nil;
  // Draw circle within an image (Vector version)
  ImageDrawCircleV: procedure(dst: PImage; center: TVector2; radius: Integer; color: TColor); cdecl = nil;
  // Draw rectangle within an image
  ImageDrawRectangle: procedure(dst: PImage; posX: Integer; posY: Integer; width: Integer; height: Integer; color: TColor); cdecl = nil;
  // Draw rectangle within an image (Vector version)
  ImageDrawRectangleV: procedure(dst: PImage; position: TVector2; size: TVector2; color: TColor); cdecl = nil;
  // Draw rectangle within an image
  ImageDrawRectangleRec: procedure(dst: PImage; rec: TRectangle; color: TColor); cdecl = nil;
  // Draw rectangle lines within an image
  ImageDrawRectangleLines: procedure(dst: PImage; rec: TRectangle; thick: Integer; color: TColor); cdecl = nil;
  // Draw a source image within a destination image (tint applied to source)
  ImageDraw: procedure(dst: PImage; src: TImage; srcRec: TRectangle; dstRec: TRectangle; tint: TColor); cdecl = nil;
  // Draw text (using default font) within an image (destination)
  ImageDrawText: procedure(dst: PImage; const text: PUTF8Char; posX, posY: Integer; fontSize: Integer; color: TColor); cdecl = nil;
  // Draw text (custom sprite font) within an image (destination)
  ImageDrawTextEx: procedure(dst: PImage; font: TFont; const text: PUTF8Char; position: TVector2; fontSize: Single; spacing: Single; color: TColor); cdecl = nil;

  { Texture loading functions }
  // NOTE: These functions require GPU access

  // Load texture from file into GPU memory (VRAM)
  LoadTexture: function(const fileName: PUTF8Char): TTexture2D; cdecl = nil;
  // Load texture from image data
  LoadTextureFromImage: function(image: TImage): TTexture2D; cdecl = nil;
  // Load cubemap from image, multiple image cubemap layouts supported
  LoadTextureCubemap: function(image: TImage; Layout: Integer): TTextureCubemap; cdecl = nil;
  // Load texture for rendering (framebuffer)
  LoadRenderTexture: function(Width: Integer; Height: Integer): TRenderTexture2D; cdecl = nil;
  // Unload texture from GPU memory (VRAM)
  UnloadTexture: procedure(Texture: TTexture2D); cdecl = nil;
  // Unload render texture from GPU memory (VRAM)
  UnloadRenderTexture: procedure(Target: TRenderTexture2D); cdecl = nil;
  // Update GPU texture with new data
  UpdateTexture: procedure(Texture: TTexture2D; const pixels: Pointer); cdecl = nil;
  // Update GPU texture rectangle with new data
  UpdateTextureRec: procedure(Texture: TTexture2D; rec: TRectangle; const pixels: Pointer); cdecl = nil;

  { Texture configuration functions }

  // Generate GPU mipmaps for a texture
  GenTextureMipmaps: procedure(texture: PTexture2D); cdecl = nil;
  // Set texture scaling filter mode
  SetTextureFilter: procedure(texture: TTexture2D; Filter: TTextureFilter); cdecl = nil;
  // Set texture wrapping mode
  SetTextureWrap: procedure(texture: TTexture2D; Wrap: Integer); cdecl = nil;

  { Texture drawing functions }

  // Draw a Texture2D
  DrawTexture: procedure(Texture: TTexture2D; posX: Integer; posY: Integer; tint: TColor); cdecl = nil;
  // Draw a Texture2D with position defined as Vector2
  DrawTextureV: procedure(Texture: TTexture2D; position: TVector2; tint: TColor); cdecl = nil;
  // Draw a Texture2D with extended parameters
  DrawTextureEx: procedure(Texture: TTexture2D; position: TVector2; rotation: Single; scale: Single; tint: TColor); cdecl = nil;
  // Draw a part of a texture defined by a rectangle
  DrawTextureRec: procedure(Texture: TTexture2D; Source: TRectangle; Position: TVector2; tint: TColor); cdecl = nil;
  // Draw texture quad with tiling and offset parameters
  DrawTextureQuad: procedure(Texture: TTexture2D; tiling: TVector2; offset: TVector2; quad: TRectangle; tint: TColor); cdecl = nil;
  // Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
  DrawTextureTiled: procedure(Texture: TTexture2D; source: TRectangle; dest: TRectangle; origin: TVector2; rotation: Single; Scale: Single; tint: TColor); cdecl = nil;
  // Draw a part of a texture defined by a rectangle with 'pro' parameters
  DrawTexturePro: procedure(Texture: TTexture2D; source: TRectangle; dest: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl = nil;
  // Draws a texture (or part of it) that stretches or shrinks nicely
  DrawTextureNPatch: procedure(Texture: TTexture2D; nPatchInfo: TNPatchInfo; dest: TRectangle; origin: TVector2; rotation: Single; tint: TColor); cdecl = nil;
  // Draw a textured polygon
  DrawTexturePoly: procedure(texture: TTexture2D; center: TVector2; points: PVector2; texcoords: PVector2; pointCount: Integer; tint: TColor); cdecl = nil;

  { Image/Texture misc functions }

  { Color-related functions }

  // Color fade-in or fade-out, alpha goes from 0.0f to 1.0f
  Fade: function(color: TColor; Alpha: Single): TColor; cdecl = nil;
  // Get hexadecimal value for a Color
  ColorToInt: function(color: TColor): Integer; cdecl = nil;
  // Get color normalized as float [0..1]
  ColorNormalize: function(color: TColor): TVector4; cdecl = nil;
  // Get color from normalized values [0..1]
  ColorFromNormalized: function(normalized: TVector4): TColor; cdecl = nil;
  // Get HSV values for a Color, hue [0..360], saturation/value [0..1]
  ColorToHSV: function(color: TColor): TVector3; cdecl = nil;
  // Get a Color from HSV values, hue [0..360], saturation/value [0..1]
  ColorFromHSV: function(hsv: TVector3): TColor; cdecl = nil;
  // Get color with alpha applied, alpha goes from 0.0f to 1.0f
  ColorAlpha: function(Color: TColor; Alpha: Single): TColor; cdecl = nil;
  // Get src alpha-blended into dst color with tint
  ColorAlphaBlend: function(dst, src, tint: TColor): TColor; cdecl = nil;
  // Get a Color struct from hexadecimal value
  GetColor: function(hexValue: Cardinal): TColor; cdecl = nil;
  // Get Color from a source pixel pointer of certain format
  GetPixelColor: function(srcPtr: Pointer; format: Integer): TColor; cdecl = nil;
  // Set color formatted into destination pixel pointer
  SetPixelColor: function(dstPtr: Pointer; Color: TColor; format: Integer): TColor; cdecl = nil;
  // Get pixel data size in bytes (image or texture)
  GetPixelDataSize: function(width: Integer; height: Integer; format: Integer): Integer; cdecl = nil;

//------------------------------------------------------------------------------------
// Font Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

  { Font loading/unloading functions }

  // Get the default Font
  GetFontDefault: function: TFont; cdecl = nil;
  // Load font from file into GPU memory (VRAM)
  LoadFont: function(const fileName: PUTF8Char): TFont; cdecl = nil;
  // Load font from file with extended parameters
  LoadFontEx: function(const fileName: PUTF8Char; fontSize: Integer; fontChars: PInteger; glyphCount: Integer): TFont; cdecl = nil;
  // Load font from Image (XNA style)
  LoadFontFromImage: function(image: TImage; key: TColor; firstChar: Integer): TFont; cdecl = nil;
  // Load font from memory buffer, fileType refers to extension: i.e. "ttf"
  LoadFontFromMemory: function(const fileType: PUTF8Char; fileData: PByte; dataSize: Integer; fontSize: Integer; fontChars: PInteger; glyphCount: Integer): TFont; cdecl = nil;
  // Load font data for further use
  LoadFontData: function(fileData: PByte; dataSize, fontSize: Integer; fontChars: PInteger; glyphCount: Integer; &type: Integer): PGlyphInfo; cdecl = nil;
  // Generate image font atlas using chars info
  GenImageFontAtlas: function(const chars: PGlyphInfo; recs: PPRectangle; glyphCount: Integer; fontSize: Integer; padding: Integer; packMethod: Integer): TImage; cdecl = nil;
  // Unload font chars info data (RAM)
  UnloadFontData: procedure(fileData: PGlyphInfo; glyphCount: Integer); cdecl = nil;
  // Unload Font from GPU memory (VRAM)
  UnloadFont: procedure(font: TFont); cdecl = nil;

  { Text drawing functions }

  // Draw current FPS
  DrawFPS: procedure(posX: Integer; posY: Integer); cdecl = nil;
  // Draw text (using default font)
  DrawText: procedure(const Text: PUTF8Char; posX: Integer; posY: Integer; FontSize: Integer; Color: TColor); cdecl = nil;
  // Draw text using font and additional parameters
  DrawTextEx: procedure(font: TFont; const text: Pointer; position: TVector2; fontSize: Single; spacing: Single; tint: TColor); cdecl = nil;
  // Draw text using Font and pro parameters (rotation)
  DrawTextPro: procedure(Font: TFont; const text: PUTF8Char; Position: TVector2; origin: TVector2; Rotation: Single; fontSize: Single; spacing: Single; tint: TColor);

  // Draw text using font inside rectangle limits with support for text selection
  DrawTextCodepoint: procedure(font: TFont; codepoint: Integer; position: TVector2; fontSize: Single; tint: TColor); cdecl = nil;

  // Text font info functions

  // Measure string width for default font
  MeasureText: function(const text: PUTF8Char; fontSize: Integer): Integer; cdecl = nil;
  // Measure string size for Font
  MeasureTextEx: function(font: TFont; const text: PUTF8Char; fontSize: Single; spacing: Single): TVector2; cdecl = nil;
  // Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
  GetGlyphIndex: function(font: TFont; codepoint: Integer): Integer; cdecl = nil;
  // Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
  GetGlyphInfo: function(Font: TFont; codepoint: Integer): TGlyphInfo; cdecl = nil;
  // Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
  GetGlyphAtlasRec: function(Font: TFont; codepoint: integer): TRectangle; cdecl = nil;

  // Text codepoints management functions (unicode characters)
  // Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
  LoadCodepoints: function(text: PUtf8Char; count: PInteger): PInteger; cdecl = nil;
  // Unload codepoints data from memory
  UnloadCodepoints: procedure(codepoints: PInteger); cdecl = nil;
  // Get total number of codepoints in a UTF-8 encoded string
  GetCodepointCount: function(text: PUTF8Char): Integer; cdecl = nil;
  // Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
  GetCodepoint: function(text: PUTF8Char; bytesProcessed: PInteger): Integer; cdecl = nil;
  // Encode one codepoint into UTF-8 byte array (array length returned as parameter)
  CodepointToUTF8: function(codepoint: Integer; byteSize: PInteger): PUtf8Char; cdecl = nil;
  // Encode text as codepoints array into UTF-8 text string (WARNING: memory must be freed!)
  TextCodepointsToUTF8: function(codepoints: PInteger; length: Integer): PUTf8Char; cdecl = nil;

  // Text strings management functions (no UTF-8 strings, only byte chars)
  // NOTE: Some strings allocate memory internally for returned strings, just be careful!

  // Copy one string to another, returns bytes copied
  TextCopy: function(dst: PUTF8Char; const src: PUTF8Char): Integer; cdecl = nil;
  // Check if two text string are equal
  TextIsEqual: function(const text1: PUTF8Char; const text2: PUTF8Char): Boolean; cdecl = nil;
  // Get text length, checks for '\0' ending
  TextLength: function(const text: PUTF8Char): Cardinal; cdecl = nil;
  // Text formatting with variables (sprintf() style)
  TextFormat: function(const text: PUTF8Char): PUTF8Char varargs; cdecl = nil;
  // Get a piece of a text string
  TextSubtext: function(const text: PUTF8Char; position: Integer; length: Integer): PUTF8Char; cdecl = nil;
  // WARNING: Replace text string (memory must be freed!)
  TextReplace: function(text: PUTF8Char; const replace: PUTF8Char; const by: PUTF8Char): PUTF8Char; cdecl = nil;
  // WARNING: Insert text in a position (memory must be freed!)
  TextInsert: function(const text: PUTF8Char; const insert: PUTF8Char; position: Integer): PUTF8Char; cdecl = nil;
  // Join text strings with delimiter
  TextJoin: function(textList: PPUTF8Char; count: Integer; const delimiter: PUTF8Char): PUTF8Char; cdecl = nil;
  // Split text into multiple strings
  TextSplit: function(const text: PUTF8Char; delimiter: UTF8Char; count: PInteger): PPUTF8Char; cdecl = nil;
  // Append text at specific position and move cursor!
  TextAppend: procedure(text: PUTF8Char; const append: PUTF8Char; position: PInteger); cdecl = nil;
  // Find first text occurrence within a string
  TextFindIndex: function(const text: PUTF8Char; const find: PUTF8Char): Integer; cdecl = nil;
  // Get upper case version of provided string
  TextToUpper: function(const text: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get lower case version of provided string
  TextToLower: function(const text: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get Pascal case notation version of provided string
  TextToPascal: function(const text: PUTF8Char): PUTF8Char; cdecl = nil;
  // Get integer value from text (negative values not supported)
  TextToInteger: function(const text: PUTF8Char): Integer; cdecl = nil;

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

  { Basic geometric 3D shapes drawing functions }

  // Draw a line in 3D world space
  DrawLine3D: procedure(startPos: TVector3; endPos: TVector3; color: TColor); cdecl = nil;
  // Draw a point in 3D space, actually a small line
  DrawPoint3D: procedure(position: TVector3; color: TColor); cdecl = nil;
  // Draw a circle in 3D world space
  DrawCircle3D: procedure(center: TVector3; radius: Single; rotationAxis: TVector3; rotationAngle: Single; color: TColor); cdecl = nil;
  // Draw a color-filled triangle (vertex in counter-clockwise order!)
  DrawTriangle3D: procedure(v1: TVector3; v2: TVector3; v3: TVector3; color: TColor); cdecl = nil;
  // Draw a triangle strip defined by points
  DrawTriangleStrip3D: procedure(Points: PVector3; pointCount: integer; Color: TColor); cdecl = nil;
  // Draw cube
  DrawCube: procedure(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl = nil;
  // Draw cube (Vector version)
  DrawCubeV: procedure(position: TVector3; size: TVector3; color: TColor); cdecl = nil;
  // Draw cube wires
  DrawCubeWires: procedure(position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl = nil;
  // Draw cube wires (Vector version)
  DrawCubeWiresV: procedure(position: TVector3; size: TVector3; color: TColor); cdecl = nil;
  // Draw cube textured
  DrawCubeTexture: procedure(texture: TTexture2D; position: TVector3; width: Single; height: Single; length: Single; color: TColor); cdecl = nil;
  // Draw cube with a region of a texture
  DrawCubeTextureRec: procedure(Texture: TTexture2D; source: TRectangle; position: TVector3; width: Single; height: Single; length: Single; Color: TColor); cdecl = nil;
  // Draw sphere
  DrawSphere: procedure(centerPos: TVector3; radius: Single; color: TColor); cdecl = nil;
  // Draw sphere with extended parameters
  DrawSphereEx: procedure(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl = nil;
  // Draw sphere wires
  DrawSphereWires: procedure(centerPos: TVector3; radius: Single; rings: Integer; slices: Integer; color: TColor); cdecl = nil;
  // Draw a cylinder/cone
  DrawCylinder: procedure(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl = nil;
  // Draw a cylinder with base at startPos and top at endPos
  DrawCylinderEx: procedure(startPos: TVector3; endPos: TVector3; startRadius: Single; endRadius: Single; sides: Integer; Color: TColor); cdecl = nil;
  // Draw a cylinder/cone wires
  DrawCylinderWires: procedure(position: TVector3; radiusTop: Single; radiusBottom: Single; height: Single; slices: Integer; color: TColor); cdecl = nil;
  // Draw a cylinder wires with base at startPos and top at endPos
  DrawCylinderWiresEx: procedure(startPos: TVector3; endPos: TVector3; startRadius: Single; endRadius: Single; sides: Integer; Color: TColor); cdecl = nil;
  // Draw a plane XZ
  DrawPlane: procedure(centerPos: TVector3; size: TVector2; color: TColor); cdecl = nil;
  // Draw a ray line
  DrawRay: procedure(ray: TRay; color: TColor); cdecl = nil;
  // Draw a grid (centered at (0, 0, 0))
  DrawGrid: procedure(slices: Integer; spacing: Single); cdecl = nil;

//------------------------------------------------------------------------------------
// Model 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

  { Model loading/unloading functions }

  // Load model from files (meshes and materials)
  LoadModel: function(const fileName: PUTF8Char): TModel; cdecl = nil;
  // Load model from generated mesh (default material)
  LoadModelFromMesh: function(mesh: TMesh): TModel; cdecl = nil;
  // Unload model from memory (RAM and/or VRAM)
  UnloadModel: procedure(model: TModel); cdecl = nil;
  // Unload model (but not meshes) from memory (RAM and/or VRAM)
  UnloadModelKeepMeshes: procedure(model: TModel); cdecl = nil;
  // Compute model bounding box limits (considers all meshes)
  GetModelBoundingBox: function(Model: TModel): TBoundingBox; cdecl = nil;

  { Model drawing functions }

  // Draw a model (with texture if set)
  DrawModel: procedure(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl = nil;
  // Draw a model with extended parameters
  DrawModelEx: procedure(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl = nil;
  // Draw a model wires (with texture if set)
  DrawModelWires: procedure(model: TModel; position: TVector3; scale: Single; tint: TColor); cdecl = nil;
  // Draw a model wires (with texture if set) with extended parameters
  DrawModelWiresEx: procedure(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColor); cdecl = nil;
  // Draw bounding box (wires)
  DrawBoundingBox: procedure(box: TBoundingBox; color: TColor); cdecl = nil;
  // Draw a billboard texture
  DrawBillboard: procedure(camera: TCamera; texture: TTexture2D; center: TVector3; size: Single; tint: TColor); cdecl = nil;
  // Draw a billboard texture defined by sourceRec
  DrawBillboardRec: procedure(camera: TCamera; texture: TTexture2D; source: TRectangle; center: TVector3; size: Single; tint: TColor); cdecl = nil;
  // Draw a billboard texture defined by source and rotation
  DrawBillboardPro: procedure(camera: TCamera; Texture: TTexture2D; source: TRectangle; position: tVector3; up: TVector3; size: TVector2; origin: TVector2; Rotation: Single; tint: TColor); cdecl = nil;

  // Mesh management functions
  // Upload mesh vertex data in GPU and provide VAO/VBO ids
  UploadMesh: procedure(mesh: PMesh; Dynamic: Boolean); cdecl = nil;
  // Update mesh vertex data in GPU for a specific buffer index
  UpdateMeshBuffer: procedure(mesh: TMesh; index: Integer; data: PByte; dataSize: Integer; offset: Integer); cdecl = nil;
  // Unload mesh data from CPU and GPU
  UnloadMesh: procedure(mesh: TMesh); cdecl = nil;
  // Draw a 3d mesh with material and transform
  DrawMesh: procedure(mesh: TMesh; material: TMaterial; transform: TMatrix); cdecl = nil;
  // Draw multiple mesh instances with material and different transforms
  DrawMeshInstanced: procedure(Mesh: TMesh; Material: TMaterial; transforms: PMatrix; instances: Integer); cdecl = nil;
  // Export mesh data to file, returns true on success
  ExportMesh: function(Mesh: TMesh; fileName: PUTF8Char): Boolean; cdecl = nil;
  // Compute mesh bounding box limits
  GetMeshBoundingBox: function(Mesh: TMesh): TBoundingBox; cdecl = nil;
  // Compute mesh tangents
  GenMeshTangents: procedure(Mesh: PMesh); cdecl = nil;
  // Compute mesh binormals
  GenMeshBinormals: procedure(Mesh: PMesh); cdecl = nil;

  { Mesh generation functions }

  // Generate polygonal mesh
  GenMeshPoly: function(sides: Integer; radius: Single): TMesh; cdecl = nil;
  // Generate plane mesh (with subdivisions)
  GenMeshPlane: function(width: Single; length: Single; resX: Integer; resZ: Integer): TMesh; cdecl = nil;
  // Generate cuboid mesh
  GenMeshCube: function(width: Single; height: Single; length: Single): TMesh; cdecl = nil;
  // Generate sphere mesh (standard sphere)
  GenMeshSphere: function(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl = nil;
  // Generate half-sphere mesh (no bottom cap)
  GenMeshHemiSphere: function(radius: Single; rings: Integer; slices: Integer): TMesh; cdecl = nil;
  // Generate cylinder mesh
  GenMeshCylinder: function(radius: Single; height: Single; slices: Integer): TMesh; cdecl = nil;
  // Generate cone/pyramid mesh
  GenMeshCone: function(radius: Single; height: Single; slices: Integer): TMesh; cdecl = nil;
  // Generate torus mesh
  GenMeshTorus: function(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl = nil;
  // Generate trefoil knot mesh
  GenMeshKnot: function(radius: Single; size: Single; radSeg: Integer; sides: Integer): TMesh; cdecl = nil;
  // Generate heightmap mesh from image data
  GenMeshHeightmap: function(heightmap: TImage; size: TVector3): TMesh; cdecl = nil;
  // Generate cubes-based map mesh from image data
  GenMeshCubicmap: function(cubicmap: TImage; cubeSize: TVector3): TMesh; cdecl = nil;

  { Material loading/unloading functions }

  // Load materials from model file
  LoadMaterials: function(const fileName: PUTF8Char; materialCount: PInteger): PMaterial; cdecl = nil;
  // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
  LoadMaterialDefault: function: TMaterial; cdecl = nil;
  // Unload material from GPU memory (VRAM)
  UnloadMaterial: procedure(material: TMaterial); cdecl = nil;
  // Set texture for a material map type (MAP_DIFFUSE, MAP_SPECULAR...)
  SetMaterialTexture: procedure(material: PMaterial; mapType: Integer; texture: TTexture2D); cdecl = nil;
  // Set material for a mesh
  SetModelMeshMaterial: procedure(model: PModel; meshId: Integer; materialId: Integer); cdecl = nil;

  { Model animations loading/unloading functions }

  // Load model animations from file
  LoadModelAnimations: function(const fileName: PUTF8Char; animsCount: PInteger): PModelAnimation; cdecl = nil;
  // Update model animation pose
  UpdateModelAnimation: procedure(model: TModel; anim: TModelAnimation; frame: Integer); cdecl = nil;
  // Unload animation data
  UnloadModelAnimation: procedure(anim: TModelAnimation); cdecl = nil;
  // Check model animation skeleton match
  IsModelAnimationValid: function(model: TModel; anim: TModelAnimation): Boolean; cdecl = nil;

  { Collision detection functions }

  // Check collision between two spheres
  CheckCollisionSpheres: function(centerA: TVector3; radiusA: Single; centerB: TVector3; radiusB: Single): Boolean; cdecl = nil;
  // Check collision between two bounding boxes
  CheckCollisionBoxes: function(box1: TBoundingBox; box2: TBoundingBox): Boolean; cdecl = nil;
  // Check collision between box and sphere
  CheckCollisionBoxSphere: function(box: TBoundingBox; center: TVector3; radius: Single): Boolean; cdecl = nil;
  // Get collision info between ray and sphere
  GetRayCollisionSphere: function(ray: TRay; center: TVector3; radius: Single): TRayCollision; cdecl = nil;
  // Get collision info between ray and box
  GetRayCollisionBox: function(ray: TRay; box: TBoundingBox): TRayCollision; cdecl = nil;
  // Get collision info between ray and model
  GetRayCollisionModel: function(ray: TRay; model: TModel): TRayCollision; cdecl = nil;
  // Get collision info between ray and mesh
  GetRayCollisionMesh: function(ray: TRay; mesh: TMesh; transform: TMatrix): TRayCollision; cdecl = nil;
  // Get collision info between ray and triangle
  GetRayCollisionTriangle: function(ray: TRay; p1: TVector3; p2: TVector3; p3: TVector3): TRayCollision; cdecl = nil;
  // Get collision info between ray and quad
  GetRayCollisionQuad: function(ray: TRay; p1: TVector3; p2: TVector3; p3: TVector3; p4: TVector3): TRayCollision;  cdecl = nil;

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------

  { Audio device management functions }

  // Initialize audio device and context
  InitAudioDevice: procedure; cdecl = nil;
  // Close the audio device and context
  CloseAudioDevice: procedure; cdecl = nil;
  // Check if audio device has been initialized successfully
  IsAudioDeviceReady: function: Boolean; cdecl = nil;
  // Set master volume (listener)
  SetMasterVolume: procedure(volume: Single); cdecl = nil;

  { Wave/Sound loading/unloading functions }

  // Load wave data from file
  LoadWave: function(const fileName: PUTF8Char): TWave; cdecl = nil;
  // Load wave from memory buffer, fileType refers to extension: i.e. "wav"
  LoadWaveFromMemory: function(const fileType: PUTF8Char; fileData: PByte; dataSize: Integer): TImage; cdecl = nil;
  // Load sound from file
  LoadSound: function(const fileName: PUTF8Char): TSound; cdecl = nil;
  // Load sound from wave data
  LoadSoundFromWave: function(wave: TWave): TSound; cdecl = nil;
  // Update sound buffer with new data
  UpdateSound: procedure(sound: TSound; const data: Pointer; sampleCount: Integer); cdecl = nil;
  // Unload wave data
  UnloadWave: procedure(wave: TWave); cdecl = nil;
  // Unload sound
  UnloadSound: procedure(sound: TSound); cdecl = nil;
  // Export wave data to file
  ExportWave: function(wave: TWave; const fileName: PUTF8Char): Boolean; cdecl = nil;
  // Export wave sample data to code (.h)
  ExportWaveAsCode: function(wave: TWave; const fileName: PUTF8Char): Boolean; cdecl = nil;

  { Wave/Sound management functions }

  // Play a sound
  PlaySound: procedure(Sound: TSound); cdecl = nil;
  // Stop playing a sound
  StopSound: procedure(Sound: TSound); cdecl = nil;
  // Pause a sound
  PauseSound: procedure(Sound: TSound); cdecl = nil;
  // Resume a paused sound
  ResumeSound: procedure(Sound: TSound); cdecl = nil;
  // Play a sound (using multichannel buffer pool)
  PlaySoundMulti: procedure(Sound: TSound); cdecl = nil;
  // Stop any sound playing (using multichannel buffer pool)
  StopSoundMulti: procedure; cdecl = nil;
  // Get number of sounds playing in the multichannel
  GetSoundsPlaying: function: Integer; cdecl = nil;
  // Check if a sound is currently playing
  IsSoundPlaying: function(Sound: TSound): Boolean; cdecl = nil;
  // Set volume for a sound (1.0 is max level)
  SetSoundVolume: procedure(Sound: TSound; volume: Single); cdecl = nil;
  // Set pitch for a sound (1.0 is base level)
  SetSoundPitch: procedure(Sound: TSound; pitch: Single); cdecl = nil;
  // Convert wave data to desired format
  WaveFormat: procedure(Wave: PWave; sampleRate: Integer; sampleSize: Integer; channels: Integer); cdecl = nil;
  // Copy a wave to a new wave
  WaveCopy: function(Wave: TWave): TWave; cdecl = nil;
  // Crop a wave to defined samples range
  WaveCrop: procedure(Wave: PWave; initSample: Integer; finalSample: Integer); cdecl = nil;
  // Get samples data from wave as a floats array
  LoadWaveSamples: function(Wave: TWave): PSingle; cdecl = nil;
  // Unload samples data loaded with LoadWaveSamples()
  UnloadWaveSamples: procedure(samples: PSingle); cdecl = nil;

  { Music management functions }

  // Load music stream from file
  LoadMusicStream: function(const FileName: PUTF8Char): TMusic; cdecl = nil;
  // Load music stream from data
  LoadMusicStreamFromMemory: function(fileType: PUTF8Char; data:PByte; dataSize: Integer): TMusic; cdecl = nil;
  // Unload music stream
  UnloadMusicStream: procedure(Music: TMusic); cdecl = nil;
  // Start music playing
  PlayMusicStream: procedure(Music: TMusic); cdecl = nil;
  // Check if music is playing
  IsMusicStreamPlaying:function(music: TMusic): Boolean; cdecl = nil;
  // Updates buffers for music streaming
  UpdateMusicStream: procedure(Music: TMusic); cdecl = nil;
  // Stop music playing
  StopMusicStream: procedure(Music: TMusic); cdecl = nil;
  // Pause music playing
  PauseMusicStream: procedure(Music: TMusic); cdecl = nil;
  // Resume playing paused music
  ResumeMusicStream: procedure(Music: TMusic); cdecl = nil;
  // Seek music to a position (in seconds)
  SeekMusicStream: procedure(music:TMusic; position: Single);
  // Set volume for music (1.0 is max level)
  SetMusicVolume: procedure(Music: TMusic; volume: Single); cdecl = nil;
  // Set pitch for a music (1.0 is base level)
  SetMusicPitch: procedure(Music: TMusic; pitch: Single); cdecl = nil;
  // Get music time length (in seconds)
  GetMusicTimeLength: function(Music: TMusic): Single; cdecl = nil;
  // Get current music time played (in seconds)
  GetMusicTimePlayed: function(Music: TMusic): Single; cdecl = nil;

  { AudioStream management functions }

  // Load audio stream (to stream raw audio pcm data)
  LoadAudioStream: function(SampleRate: Cardinal; SampleSize: Cardinal; Channels: Cardinal): TAudioStream; cdecl = nil;
  // Unload audio stream and free memory
  UnloadAudioStream: procedure(stream: TAudioStream); cdecl = nil;
  // Update audio stream buffers with data
  UpdateAudioStream: procedure(Stream: TAudioStream; const Data: Pointer; FrameCount: Integer); cdecl = nil;
  // Check if any audio stream buffers requires refill
  IsAudioStreamProcessed: function(Stream: TAudioStream): Boolean; cdecl = nil;
  // Play audio stream
  PlayAudioStream: procedure(stream: TAudioStream); cdecl = nil;
  // Pause audio stream
  PauseAudioStream: procedure(stream: TAudioStream); cdecl = nil;
  // Resume audio stream
  ResumeAudioStream: procedure(stream: TAudioStream); cdecl = nil;
  // Check if audio stream is playing
  IsAudioStreamPlaying: function(stream: TAudioStream): Boolean; cdecl = nil;
  // Stop audio stream
  StopAudioStream: procedure(stream: TAudioStream); cdecl = nil;
  // Set volume for audio stream (1.0 is max level)
  SetAudioStreamVolume: procedure(stream: TAudioStream; volume: Single); cdecl = nil;
  // Set pitch for audio stream (1.0 is base level)
  SetAudioStreamPitch: procedure(stream: TAudioStream; pitch: Single); cdecl = nil;
  // Default size for new audio streams
  SetAudioStreamBufferSizeDefault: procedure(size: Integer); cdecl = nil;

//------------------------------------------------------------------------------------
// Network (Module: network)
//------------------------------------------------------------------------------------

// IN PROGRESS: Check rnet.h for reference

{*
  Load library dynamically, use RayLibrary.Load to load it
*}

type

  { TmncRayLib }

  TmncRayLib = class(TmnLibrary)
  public
  protected
    procedure Link; override;
  end;

var
  RayLibrary: TmncRayLib = nil;

function Vector2Of(X, Y: Single): TVector2;
function RectangleOf(X, Y, W, H: Single): TRectangle;

implementation

function Vector2Of(X, Y: Single): TVector2;
begin
  Result := TVector2.Create(X, Y);
end;

function RectangleOf(X, Y, W, H: Single): TRectangle;
begin
  Result := TRectangle.Create(X, Y, W, H);
end;

function TColorHelper.SetAlpha(AAlpha: Byte): TColor;
begin
  Result := Self;
  Result.RGBA.Alpha := AAlpha;
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

procedure TColor.SetRGB(AColor: TColor);
begin
  RGBA.Red := AColor.RGBA.Red;
  RGBA.Green := AColor.RGBA.Green;
  RGBA.Blue := AColor.RGBA.Blue;
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

function TRGBAColorHelper.ReplaceAlpha(AAlpha: Byte): TColor;
begin
  Result := Self;
  Result.RGBA.Alpha := AAlpha;
end;

function TRGBAColorHelper.ReplaceRGB(AColor: TColor): TColor;
begin
  Result := Self;
  Result.RGBA.Red := AColor.RGBA.Red;
  Result.RGBA.Green := AColor.RGBA.Green;
  Result.RGBA.Blue := AColor.RGBA.Blue;
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

constructor TVector3.Create(AX, AY, AZ: Single);
begin
  X := AX;
  Y := AY;
  Z := AZ;
end;

{ TmncRayLib }

procedure TmncRayLib.Link;
begin
  RaiseError := True; //Raise error of one of this functions not exists
  GetAddress(@InitWindow, 'InitWindow');
  GetAddress(@WindowShouldClose, 'WindowShouldClose');
  GetAddress(@CloseWindow, 'CloseWindow');
  GetAddress(@IsWindowReady, 'IsWindowReady');
  GetAddress(@IsWindowFullscreen, 'IsWindowFullscreen');
  GetAddress(@IsWindowHidden, 'IsWindowHidden');
  GetAddress(@IsWindowMinimized, 'IsWindowMinimized');
  GetAddress(@IsWindowMaximized, 'IsWindowMaximized');
  GetAddress(@IsWindowResized, 'IsWindowResized');
  GetAddress(@IsWindowState, 'IsWindowState');
  GetAddress(@SetWindowState, 'SetWindowState');
  GetAddress(@ClearWindowState, 'ClearWindowState');
  GetAddress(@ToggleFullscreen, 'ToggleFullscreen');
  GetAddress(@MaximizeWindow, 'MaximizeWindow');
  GetAddress(@MinimizeWindow, 'MinimizeWindow');
  GetAddress(@RestoreWindow, 'RestoreWindow');
  GetAddress(@SetWindowIcon, 'SetWindowIcon');
  GetAddress(@SetWindowTitle, 'SetWindowTitle');
  GetAddress(@SetWindowPosition, 'SetWindowPosition');
  GetAddress(@SetWindowMonitor, 'SetWindowMonitor');
  GetAddress(@SetWindowMinSize, 'SetWindowMinSize');
  GetAddress(@SetWindowSize, 'SetWindowSize');
  GetAddress(@GetWindowHandle, 'GetWindowHandle');
  GetAddress(@GetScreenWidth, 'GetScreenWidth');
  GetAddress(@GetScreenHeight, 'GetScreenHeight');
  GetAddress(@GetMonitorCount, 'GetMonitorCount');
  GetAddress(@GetCurrentMonitor, 'GetCurrentMonitor');
  GetAddress(@GetMonitorPosition, 'GetMonitorPosition');
  GetAddress(@GetMonitorWidth, 'GetMonitorWidth');
  GetAddress(@GetMonitorHeight, 'GetMonitorHeight');
  GetAddress(@GetMonitorPhysicalWidth, 'GetMonitorPhysicalWidth');
  GetAddress(@GetMonitorPhysicalHeight, 'GetMonitorPhysicalHeight');
  GetAddress(@GetMonitorRefreshRate, 'GetMonitorRefreshRate');
  GetAddress(@GetWindowPosition, 'GetWindowPosition');
  GetAddress(@GetMonitorName, 'GetMonitorName');
  GetAddress(@GetWindowScaleDPI, 'GetWindowScaleDPI');
  GetAddress(@SetClipboardText, 'SetClipboardText');
  GetAddress(@GetClipboardText, 'GetClipboardText');
  GetAddress(@SwapScreenBuffer, 'SwapScreenBuffer');
  GetAddress(@PollInputEvents, 'PollInputEvents');
  GetAddress(@WaitTime, 'WaitTime');
  GetAddress(@ShowCursor, 'ShowCursor');
  GetAddress(@HideCursor, 'HideCursor');
  GetAddress(@IsCursorHidden, 'IsCursorHidden');
  GetAddress(@EnableCursor, 'EnableCursor');
  GetAddress(@DisableCursor, 'DisableCursor');
  GetAddress(@IsCursorOnScreen, 'IsCursorOnScreen');
  GetAddress(@ClearBackground, 'ClearBackground');
  GetAddress(@BeginDrawing, 'BeginDrawing');
  GetAddress(@EndDrawing, 'EndDrawing');
  GetAddress(@BeginMode2D, 'BeginMode2D');
  GetAddress(@EndMode2D, 'EndMode2D');
  GetAddress(@BeginMode3D, 'BeginMode3D');
  GetAddress(@EndMode3D, 'EndMode3D');
  GetAddress(@BeginTextureMode, 'BeginTextureMode');
  GetAddress(@EndTextureMode, 'EndTextureMode');
  GetAddress(@BeginShaderMode, 'BeginShaderMode');
  GetAddress(@EndShaderMode, 'EndShaderMode');
  GetAddress(@BeginBlendMode, 'BeginBlendMode');
  GetAddress(@EndBlendMode, 'EndBlendMode');
  GetAddress(@BeginScissorMode, 'BeginScissorMode');
  GetAddress(@EndScissorMode, 'EndScissorMode');
  GetAddress(@LoadVrStereoConfig, 'LoadVrStereoConfig');
  GetAddress(@UnloadVrStereoConfig, 'UnloadVrStereoConfig');
  GetAddress(@LoadShader, 'LoadShader');
  GetAddress(@LoadShaderFromMemory, 'LoadShaderFromMemory');
  GetAddress(@GetShaderLocation, 'GetShaderLocation');
  GetAddress(@GetShaderLocationAttrib, 'GetShaderLocationAttrib');
  GetAddress(@SetShaderValue, 'SetShaderValue');
  GetAddress(@SetShaderValueV, 'SetShaderValueV');
  GetAddress(@SetShaderValueMatrix, 'SetShaderValueMatrix');
  GetAddress(@SetShaderValueTexture, 'SetShaderValueTexture');
  GetAddress(@UnloadShader, 'UnloadShader');
  GetAddress(@GetMouseRay, 'GetMouseRay');
  GetAddress(@GetCameraMatrix, 'GetCameraMatrix');
  GetAddress(@GetCameraMatrix2D, 'GetCameraMatrix2D');
  GetAddress(@GetWorldToScreen, 'GetWorldToScreen');
  GetAddress(@GetWorldToScreenEx, 'GetWorldToScreenEx');
  GetAddress(@GetWorldToScreen2D, 'GetWorldToScreen2D');
  GetAddress(@GetScreenToWorld2D, 'GetScreenToWorld2D');
  GetAddress(@SetTargetFPS, 'SetTargetFPS');
  GetAddress(@GetFPS, 'GetFPS');
  GetAddress(@GetFrameTime, 'GetFrameTime');
  GetAddress(@GetTime, 'GetTime');

  GetAddress(@GetRandomValue, 'GetRandomValue');
  GetAddress(@SetRandomSeed, 'SetRandomSeed');
  GetAddress(@TakeScreenshot, 'TakeScreenshot');
  GetAddress(@SetConfigFlags, 'SetConfigFlags');
  GetAddress(@TraceLog, 'TraceLog');
  GetAddress(@SetTraceLogLevel, 'SetTraceLogLevel');
  GetAddress(@MemAlloc, 'MemAlloc');
  GetAddress(@MemRealloc, 'MemRealloc');
  GetAddress(@MemFree, 'MemFree');

  GetAddress(@SetTraceLogCallback, 'SetTraceLogCallback');
  GetAddress(@SetLoadFileDataCallback, 'SetLoadFileDataCallback');
  GetAddress(@SetSaveFileDataCallback, 'SetSaveFileDataCallback');
  GetAddress(@SetLoadFileTextCallback, 'SetLoadFileTextCallback');
  GetAddress(@SetSaveFileTextCallback, 'SetSaveFileTextCallback');

  GetAddress(@ColorToInt, 'ColorToInt');
  GetAddress(@ColorNormalize, 'ColorNormalize');
  GetAddress(@ColorFromNormalized, 'ColorFromNormalized');
  GetAddress(@ColorToHSV, 'ColorToHSV');
  GetAddress(@ColorFromHSV, 'ColorFromHSV');
  GetAddress(@ColorAlpha, 'ColorAlpha');
  GetAddress(@ColorAlphaBlend, 'ColorAlphaBlend');
  GetAddress(@GetColor, 'GetColor');
  GetAddress(@Fade, 'Fade');
  GetAddress(@LoadFileData, 'LoadFileData');
  GetAddress(@UnloadFileData, 'UnloadFileData');
  GetAddress(@SaveFileData, 'SaveFileData');
  GetAddress(@LoadFileText, 'LoadFileText');
  GetAddress(@UnloadFileText, 'UnloadFileText');
  GetAddress(@SaveFileText, 'SaveFileText');
  GetAddress(@FileExists, 'FileExists');
  GetAddress(@IsFileExtension, 'IsFileExtension');
  GetAddress(@DirectoryExists, 'DirectoryExists');
  GetAddress(@GetFileExtension, 'GetFileExtension');
  GetAddress(@GetFileName, 'GetFileName');
  GetAddress(@GetFileNameWithoutExt, 'GetFileNameWithoutExt');
  GetAddress(@GetDirectoryPath, 'GetDirectoryPath');
  GetAddress(@GetPrevDirectoryPath, 'GetPrevDirectoryPath');
  GetAddress(@GetWorkingDirectory, 'GetWorkingDirectory');
  GetAddress(@GetDirectoryFiles, 'GetDirectoryFiles');
  GetAddress(@ClearDirectoryFiles, 'ClearDirectoryFiles');
  GetAddress(@ChangeDirectory, 'ChangeDirectory');
  GetAddress(@IsFileDropped, 'IsFileDropped');
  GetAddress(@GetDroppedFiles, 'GetDroppedFiles');
  GetAddress(@ClearDroppedFiles, 'ClearDroppedFiles');
  GetAddress(@GetFileModTime, 'GetFileModTime');
  GetAddress(@CompressData, 'CompressData');
  GetAddress(@DecompressData, 'DecompressData');
  GetAddress(@EncodeDataBase64, 'EncodeDataBase64');
  GetAddress(@DecodeDataBase64, 'DecodeDataBase64');
  GetAddress(@SaveStorageValue, 'SaveStorageValue');
  GetAddress(@LoadStorageValue, 'LoadStorageValue');
  GetAddress(@OpenURL, 'OpenURL');
  GetAddress(@IsKeyPressed, 'IsKeyPressed');
  GetAddress(@IsKeyDown, 'IsKeyDown');
  GetAddress(@IsKeyReleased, 'IsKeyReleased');
  GetAddress(@IsKeyUp, 'IsKeyUp');
  GetAddress(@SetExitKey, 'SetExitKey');
  GetAddress(@GetKeyPressed, 'GetKeyPressed');
  GetAddress(@GetCharPressed, 'GetCharPressed');
  GetAddress(@IsGamepadAvailable, 'IsGamepadAvailable');
  GetAddress(@GetGamepadName, 'GetGamepadName');
  GetAddress(@IsGamepadButtonPressed, 'IsGamepadButtonPressed');
  GetAddress(@IsGamepadButtonDown, 'IsGamepadButtonDown');
  GetAddress(@IsGamepadButtonReleased, 'IsGamepadButtonReleased');
  GetAddress(@IsGamepadButtonUp, 'IsGamepadButtonUp');
  GetAddress(@GetGamepadButtonPressed, 'GetGamepadButtonPressed');
  GetAddress(@GetGamepadAxisCount, 'GetGamepadAxisCount');
  GetAddress(@GetGamepadAxisMovement, 'GetGamepadAxisMovement');
  GetAddress(@SetGamepadMappings, 'SetGamepadMappings');
  GetAddress(@IsMouseButtonPressed, 'IsMouseButtonPressed');
  GetAddress(@IsMouseButtonDown, 'IsMouseButtonDown');
  GetAddress(@IsMouseButtonReleased, 'IsMouseButtonReleased');
  GetAddress(@IsMouseButtonUp, 'IsMouseButtonUp');
  GetAddress(@GetMouseX, 'GetMouseX');
  GetAddress(@GetMouseY, 'GetMouseY');
  GetAddress(@GetMousePosition, 'GetMousePosition');
  GetAddress(@GetMouseDelta, 'GetMouseDelta');
  GetAddress(@SetMousePosition, 'SetMousePosition');
  GetAddress(@SetMouseOffset, 'SetMouseOffset');
  GetAddress(@SetMouseScale, 'SetMouseScale');
  GetAddress(@GetMouseWheelMove, 'GetMouseWheelMove');
  GetAddress(@SetMouseCursor, 'SetMouseCursor');
  GetAddress(@GetTouchX, 'GetTouchX');
  GetAddress(@GetTouchY, 'GetTouchY');
  GetAddress(@GetTouchPosition, 'GetTouchPosition');
  GetAddress(@GetTouchPointId, 'GetTouchPointId');
  GetAddress(@GetTouchPointCount, 'GetTouchPointCount');
  GetAddress(@SetGesturesEnabled, 'SetGesturesEnabled');
  GetAddress(@IsGestureDetected, 'IsGestureDetected');
  GetAddress(@GetGestureDetected, 'GetGestureDetected');
  GetAddress(@GetGestureHoldDuration, 'GetGestureHoldDuration');
  GetAddress(@GetGestureDragVector, 'GetGestureDragVector');
  GetAddress(@GetGestureDragAngle, 'GetGestureDragAngle');
  GetAddress(@GetGesturePinchVector, 'GetGesturePinchVector');
  GetAddress(@GetGesturePinchAngle, 'GetGesturePinchAngle');
  GetAddress(@SetCameraMode, 'SetCameraMode');
  GetAddress(@UpdateCamera, 'UpdateCamera');
  GetAddress(@SetCameraPanControl, 'SetCameraPanControl');
  GetAddress(@SetCameraAltControl, 'SetCameraAltControl');
  GetAddress(@SetCameraSmoothZoomControl, 'SetCameraSmoothZoomControl');
  GetAddress(@SetCameraMoveControls, 'SetCameraMoveControls');
  GetAddress(@SetShapesTexture, 'SetShapesTexture');
  GetAddress(@DrawPixel, 'DrawPixel');
  GetAddress(@DrawPixelV, 'DrawPixelV');
  GetAddress(@DrawLine, 'DrawLine');
  GetAddress(@DrawLineV, 'DrawLineV');
  GetAddress(@DrawLineEx, 'DrawLineEx');
  GetAddress(@DrawLineBezier, 'DrawLineBezier');
  GetAddress(@DrawLineBezierQuad, 'DrawLineBezierQuad');
  GetAddress(@DrawLineBezierCubic, 'DrawLineBezierCubic');
  GetAddress(@DrawCircle, 'DrawCircle');
  GetAddress(@DrawCircleSector, 'DrawCircleSector');
  GetAddress(@DrawCircleSectorLines, 'DrawCircleSectorLines');
  GetAddress(@DrawCircleGradient, 'DrawCircleGradient');
  GetAddress(@DrawCircleV, 'DrawCircleV');
  GetAddress(@DrawCircleLines, 'DrawCircleLines');
  GetAddress(@DrawEllipse, 'DrawEllipse');
  GetAddress(@DrawEllipseLines, 'DrawEllipseLines');
  GetAddress(@DrawRing, 'DrawRing');
  GetAddress(@DrawRingLines, 'DrawRingLines');
  GetAddress(@DrawRectangle, 'DrawRectangle');
  GetAddress(@DrawRectangleV, 'DrawRectangleV');
  GetAddress(@DrawRectangleRec, 'DrawRectangleRec');
  GetAddress(@DrawRectanglePro, 'DrawRectanglePro');
  GetAddress(@DrawRectangleGradientV, 'DrawRectangleGradientV');
  GetAddress(@DrawRectangleGradientH, 'DrawRectangleGradientH');
  GetAddress(@DrawRectangleGradientEx, 'DrawRectangleGradientEx');
  GetAddress(@DrawRectangleLines, 'DrawRectangleLines');
  GetAddress(@DrawRectangleLinesEx, 'DrawRectangleLinesEx');
  GetAddress(@DrawRectangleRounded, 'DrawRectangleRounded');
  GetAddress(@DrawRectangleRoundedLines, 'DrawRectangleRoundedLines');
  GetAddress(@DrawTriangle, 'DrawTriangle');
  GetAddress(@DrawTriangleLines, 'DrawTriangleLines');
  GetAddress(@DrawTriangleFan, 'DrawTriangleFan');
  GetAddress(@DrawTriangleStrip, 'DrawTriangleStrip');
  GetAddress(@DrawPoly, 'DrawPoly');
  GetAddress(@DrawPolyLines, 'DrawPolyLines');
  GetAddress(@DrawPolyLinesEx, 'DrawPolyLinesEx');
  GetAddress(@CheckCollisionRecs, 'CheckCollisionRecs');
  GetAddress(@CheckCollisionCircles, 'CheckCollisionCircles');
  GetAddress(@CheckCollisionCircleRec, 'CheckCollisionCircleRec');
  GetAddress(@CheckCollisionPointRec, 'CheckCollisionPointRec');
  GetAddress(@CheckCollisionPointCircle, 'CheckCollisionPointCircle');
  GetAddress(@CheckCollisionPointTriangle, 'CheckCollisionPointTriangle');
  GetAddress(@CheckCollisionLines, 'CheckCollisionLines');
  GetAddress(@CheckCollisionPointLine, 'CheckCollisionPointLine');
  GetAddress(@GetCollisionRec, 'GetCollisionRec');
  GetAddress(@LoadImage, 'LoadImage');
  GetAddress(@LoadImageRaw, 'LoadImageRaw');
  GetAddress(@LoadImageAnim, 'LoadImageAnim');
  GetAddress(@LoadImageFromMemory, 'LoadImageFromMemory');
  GetAddress(@LoadImageFromTexture, 'LoadImageFromTexture');
  GetAddress(@LoadImageFromScreen, 'LoadImageFromScreen');
  GetAddress(@UnloadImage, 'UnloadImage');
  GetAddress(@ExportImage, 'ExportImage');
  GetAddress(@ExportImageAsCode, 'ExportImageAsCode');
  GetAddress(@LoadImageColors, 'LoadImageColors');
  GetAddress(@LoadImagePalette, 'LoadImagePalette');
  GetAddress(@UnloadImageColors, 'UnloadImageColors');
  GetAddress(@UnloadImagePalette, 'UnloadImagePalette');
  GetAddress(@GenImageColor, 'GenImageColor');
  GetAddress(@GenImageGradientV, 'GenImageGradientV');
  GetAddress(@GenImageGradientH, 'GenImageGradientH');
  GetAddress(@GenImageGradientRadial, 'GenImageGradientRadial');
  GetAddress(@GenImageChecked, 'GenImageChecked');
  GetAddress(@GenImageWhiteNoise, 'GenImageWhiteNoise');
  GetAddress(@GenImageCellular, 'GenImageCellular');
  GetAddress(@ImageCopy, 'ImageCopy');
  GetAddress(@ImageFromImage, 'ImageFromImage');
  GetAddress(@ImageText, 'ImageText');
  GetAddress(@ImageTextEx, 'ImageTextEx');
  GetAddress(@ImageToPOT, 'ImageToPOT');
  GetAddress(@ImageFormat, 'ImageFormat');
  GetAddress(@ImageCrop, 'ImageCrop');
  GetAddress(@ImageAlphaCrop, 'ImageAlphaCrop');
  GetAddress(@ImageAlphaClear, 'ImageAlphaClear');
  GetAddress(@ImageAlphaMask, 'ImageAlphaMask');
  GetAddress(@ImageAlphaPremultiply, 'ImageAlphaPremultiply');
  GetAddress(@ImageResize, 'ImageResize');
  GetAddress(@ImageResizeNN, 'ImageResizeNN');
  GetAddress(@ImageResizeCanvas, 'ImageResizeCanvas');
  GetAddress(@ImageMipmaps, 'ImageMipmaps');
  GetAddress(@ImageDither, 'ImageDither');
  GetAddress(@ImageFlipVertical, 'ImageFlipVertical');
  GetAddress(@ImageFlipHorizontal, 'ImageFlipHorizontal');
  GetAddress(@ImageRotateCW, 'ImageRotateCW');
  GetAddress(@ImageRotateCCW, 'ImageRotateCCW');
  GetAddress(@ImageColorTint, 'ImageColorTint');
  GetAddress(@ImageColorInvert, 'ImageColorInvert');
  GetAddress(@ImageColorGrayscale, 'ImageColorGrayscale');
  GetAddress(@ImageColorContrast, 'ImageColorContrast');
  GetAddress(@ImageColorBrightness, 'ImageColorBrightness');
  GetAddress(@ImageColorReplace, 'ImageColorReplace');
  GetAddress(@GetImageAlphaBorder, 'GetImageAlphaBorder');
  GetAddress(@GetImageColor, 'GetImageColor');
  GetAddress(@ImageClearBackground, 'ImageClearBackground');
  GetAddress(@ImageDrawPixel, 'ImageDrawPixel');
  GetAddress(@ImageDrawPixelV, 'ImageDrawPixelV');
  GetAddress(@ImageDrawLine, 'ImageDrawLine');
  GetAddress(@ImageDrawLineV, 'ImageDrawLineV');
  GetAddress(@ImageDrawCircle, 'ImageDrawCircle');
  GetAddress(@ImageDrawCircleV, 'ImageDrawCircleV');
  GetAddress(@ImageDrawRectangle, 'ImageDrawRectangle');
  GetAddress(@ImageDrawRectangleV, 'ImageDrawRectangleV');
  GetAddress(@ImageDrawRectangleRec, 'ImageDrawRectangleRec');
  GetAddress(@ImageDrawRectangleLines, 'ImageDrawRectangleLines');
  GetAddress(@ImageDraw, 'ImageDraw');
  GetAddress(@ImageDrawText, 'ImageDrawText');
  GetAddress(@ImageDrawTextEx, 'ImageDrawTextEx');
  GetAddress(@LoadTexture, 'LoadTexture');
  GetAddress(@LoadTextureFromImage, 'LoadTextureFromImage');
  GetAddress(@LoadTextureCubemap, 'LoadTextureCubemap');
  GetAddress(@LoadRenderTexture, 'LoadRenderTexture');
  GetAddress(@UnloadTexture, 'UnloadTexture');
  GetAddress(@UnloadRenderTexture, 'UnloadRenderTexture');
  GetAddress(@UpdateTexture, 'UpdateTexture');
  GetAddress(@UpdateTextureRec, 'UpdateTextureRec');
  GetAddress(@GenTextureMipmaps, 'GenTextureMipmaps');
  GetAddress(@SetTextureFilter, 'SetTextureFilter');
  GetAddress(@SetTextureWrap, 'SetTextureWrap');
  GetAddress(@DrawTexture, 'DrawTexture');
  GetAddress(@DrawTextureV, 'DrawTextureV');
  GetAddress(@DrawTextureEx, 'DrawTextureEx');
  GetAddress(@DrawTextureRec, 'DrawTextureRec');
  DrawTextureQuad := GetAddress('DrawTextureQuad');
  DrawTextureTiled := GetAddress('DrawTextureTiled');
  DrawTexturePro := GetAddress('DrawTexturePro');
  DrawTextureNPatch := GetAddress('DrawTextureNPatch');
  GetAddress(@DrawTexturePoly, 'DrawTexturePoly');
  GetAddress(@GetPixelColor, 'GetPixelColor');
  GetAddress(@SetPixelColor, 'SetPixelColor');
  GetAddress(@GetPixelDataSize, 'GetPixelDataSize');
  GetAddress(@GetFontDefault, 'GetFontDefault');
  GetAddress(@LoadFont, 'LoadFont');
  GetAddress(@LoadFontEx, 'LoadFontEx');
  GetAddress(@LoadFontFromImage, 'LoadFontFromImage');
  GetAddress(@LoadFontFromMemory, 'LoadFontFromMemory');
  GetAddress(@LoadFontData, 'LoadFontData');
  GetAddress(@GenImageFontAtlas, 'GenImageFontAtlas');
  GetAddress(@UnloadFontData, 'UnloadFontData');
  GetAddress(@UnloadFont, 'UnloadFont');
  GetAddress(@DrawFPS, 'DrawFPS');
  GetAddress(@DrawText, 'DrawText');
  GetAddress(@DrawTextEx, 'DrawTextEx');
  GetAddress(@DrawTextPro, 'DrawTextPro');
  GetAddress(@DrawTextCodepoint, 'DrawTextCodepoint');
  GetAddress(@MeasureText, 'MeasureText');
  GetAddress(@MeasureTextEx, 'MeasureTextEx');
  GetAddress(@GetGlyphIndex, 'GetGlyphIndex');
  GetAddress(@GetGlyphInfo, 'GetGlyphInfo');
  GetAddress(@GetGlyphAtlasRec, 'GetGlyphAtlasRec');
  GetAddress(@LoadCodepoints, 'LoadCodepoints');
  GetAddress(@UnloadCodepoints, 'UnloadCodepoints');
  GetAddress(@GetCodepointCount, 'GetCodepointCount');
  GetAddress(@GetCodepoint, 'GetCodepoint');
  GetAddress(@CodepointToUTF8, 'CodepointToUTF8');
  GetAddress(@TextCodepointsToUTF8, 'TextCodepointsToUTF8');
  GetAddress(@TextCopy, 'TextCopy');
  GetAddress(@TextIsEqual, 'TextIsEqual');
  GetAddress(@TextLength, 'TextLength');
  GetAddress(@TextFormat, 'TextFormat');
  GetAddress(@TextSubtext, 'TextSubtext');
  GetAddress(@TextReplace, 'TextReplace');
  GetAddress(@TextInsert, 'TextInsert');
  GetAddress(@TextJoin, 'TextJoin');
  GetAddress(@TextSplit, 'TextSplit');
  GetAddress(@TextAppend, 'TextAppend');
  GetAddress(@TextFindIndex, 'TextFindIndex');
  GetAddress(@TextToUpper, 'TextToUpper');
  GetAddress(@TextToLower, 'TextToLower');
  GetAddress(@TextToPascal, 'TextToPascal');
  GetAddress(@TextToInteger, 'TextToInteger');

  GetAddress(@DrawLine3D, 'DrawLine3D');
  GetAddress(@DrawPoint3D, 'DrawPoint3D');
  GetAddress(@DrawCircle3D, 'DrawCircle3D');
  GetAddress(@DrawTriangle3D, 'DrawTriangle3D');
  GetAddress(@DrawTriangleStrip3D, 'DrawTriangleStrip3D');
  GetAddress(@DrawCube, 'DrawCube');
  GetAddress(@DrawCubeV, 'DrawCubeV');
  GetAddress(@DrawCubeWires, 'DrawCubeWires');
  GetAddress(@DrawCubeWiresV, 'DrawCubeWiresV');
  GetAddress(@DrawCubeTexture, 'DrawCubeTexture');
  GetAddress(@DrawCubeTextureRec, 'DrawCubeTextureRec');
  GetAddress(@DrawSphere, 'DrawSphere');
  GetAddress(@DrawSphereEx, 'DrawSphereEx');
  GetAddress(@DrawSphereWires, 'DrawSphereWires');
  GetAddress(@DrawCylinder, 'DrawCylinder');
  GetAddress(@DrawCylinderWires, 'DrawCylinderWires');
  GetAddress(@DrawCylinderWiresEx, 'DrawCylinderWiresEx');
  GetAddress(@DrawPlane, 'DrawPlane');
  GetAddress(@DrawRay, 'DrawRay');
  GetAddress(@DrawGrid, 'DrawGrid');

  GetAddress(@LoadModel, 'LoadModel');
  GetAddress(@LoadModelFromMesh, 'LoadModelFromMesh');
  GetAddress(@UnloadModel, 'UnloadModel');
  GetAddress(@UnloadModelKeepMeshes, 'UnloadModelKeepMeshes');
  GetAddress(@GetModelBoundingBox, 'GetModelBoundingBox');
  GetAddress(@DrawModel, 'DrawModel');
  GetAddress(@DrawModelEx, 'DrawModelEx');
  GetAddress(@DrawModelWires, 'DrawModelWires');
  GetAddress(@DrawModelWiresEx, 'DrawModelWiresEx');
  GetAddress(@DrawBoundingBox, 'DrawBoundingBox');
  GetAddress(@DrawBillboard, 'DrawBillboard');
  GetAddress(@DrawBillboardRec, 'DrawBillboardRec');
  GetAddress(@DrawBillboardPro, 'DrawBillboardPro');
  GetAddress(@UploadMesh, 'UploadMesh');
  GetAddress(@UpdateMeshBuffer, 'UpdateMeshBuffer');
  GetAddress(@UnloadMesh, 'UnloadMesh');
  GetAddress(@DrawMesh, 'DrawMesh');
  GetAddress(@DrawMeshInstanced, 'DrawMeshInstanced');
  GetAddress(@ExportMesh, 'ExportMesh');
  GetAddress(@GetMeshBoundingBox, 'GetMeshBoundingBox');
  GetAddress(@GenMeshTangents, 'GenMeshTangents');
  GetAddress(@GenMeshBinormals, 'GenMeshBinormals');

  GetAddress(@GenMeshPoly, 'GenMeshPoly');
  GetAddress(@GenMeshPlane, 'GenMeshPlane');
  GetAddress(@GenMeshCube, 'GenMeshCube');
  GetAddress(@GenMeshSphere, 'GenMeshSphere');
  GetAddress(@GenMeshHemiSphere, 'GenMeshHemiSphere');
  GetAddress(@GenMeshCylinder, 'GenMeshCylinder');
  GetAddress(@GenMeshCone, 'GenMeshCone');
  GetAddress(@GenMeshTorus, 'GenMeshTorus');
  GetAddress(@GenMeshKnot, 'GenMeshKnot');
  GetAddress(@GenMeshHeightmap, 'GenMeshHeightmap');
  GetAddress(@GenMeshCubicmap, 'GenMeshCubicmap');

  GetAddress(@LoadMaterials, 'LoadMaterials');
  GetAddress(@LoadMaterialDefault, 'LoadMaterialDefault');
  GetAddress(@UnloadMaterial, 'UnloadMaterial');
  GetAddress(@SetMaterialTexture, 'SetMaterialTexture');
  GetAddress(@SetModelMeshMaterial, 'SetModelMeshMaterial');

  GetAddress(@LoadModelAnimations, 'LoadModelAnimations');
  GetAddress(@UpdateModelAnimation, 'UpdateModelAnimation');
  GetAddress(@UnloadModelAnimation, 'UnloadModelAnimation');
  GetAddress(@IsModelAnimationValid, 'IsModelAnimationValid');

  GetAddress(@CheckCollisionSpheres, 'CheckCollisionSpheres');
  GetAddress(@CheckCollisionBoxes, 'CheckCollisionBoxes');
  GetAddress(@CheckCollisionBoxSphere, 'CheckCollisionBoxSphere');
  GetAddress(@GetRayCollisionSphere, 'GetRayCollisionSphere');
  GetAddress(@GetRayCollisionBox, 'GetRayCollisionBox');
  GetAddress(@GetRayCollisionMesh, 'GetRayCollisionMesh');
  GetAddress(@GetRayCollisionModel, 'GetRayCollisionModel');
  GetAddress(@GetRayCollisionTriangle, 'GetRayCollisionTriangle');
  GetAddress(@GetRayCollisionQuad, 'GetRayCollisionQuad');

  GetAddress(@InitAudioDevice, 'InitAudioDevice');
  GetAddress(@CloseAudioDevice, 'CloseAudioDevice');
  GetAddress(@IsAudioDeviceReady, 'IsAudioDeviceReady');
  GetAddress(@SetMasterVolume, 'SetMasterVolume');
  GetAddress(@LoadWave, 'LoadWave');
  GetAddress(@LoadWaveFromMemory, 'LoadWaveFromMemory');
  GetAddress(@LoadSound, 'LoadSound');
  GetAddress(@LoadSoundFromWave, 'LoadSoundFromWave');
  GetAddress(@UpdateSound, 'UpdateSound');
  GetAddress(@UnloadWave, 'UnloadWave');
  GetAddress(@UnloadSound, 'UnloadSound');
  GetAddress(@ExportWave, 'ExportWave');
  GetAddress(@ExportWaveAsCode, 'ExportWaveAsCode');
  GetAddress(@PlaySound, 'PlaySound');
  GetAddress(@StopSound, 'StopSound');
  GetAddress(@PauseSound, 'PauseSound');
  GetAddress(@ResumeSound, 'ResumeSound');
  GetAddress(@PlaySoundMulti, 'PlaySoundMulti');
  GetAddress(@StopSoundMulti, 'StopSoundMulti');
  GetAddress(@GetSoundsPlaying, 'GetSoundsPlaying');
  GetAddress(@IsSoundPlaying, 'IsSoundPlaying');
  GetAddress(@SetSoundVolume, 'SetSoundVolume');
  GetAddress(@SetSoundPitch, 'SetSoundPitch');
  GetAddress(@WaveFormat, 'WaveFormat');
  GetAddress(@WaveCopy, 'WaveCopy');
  GetAddress(@WaveCrop, 'WaveCrop');
  GetAddress(@LoadWaveSamples, 'LoadWaveSamples');
  GetAddress(@UnloadWaveSamples, 'UnloadWaveSamples');
  GetAddress(@LoadMusicStream, 'LoadMusicStream');
  GetAddress(@LoadMusicStreamFromMemory, 'LoadMusicStreamFromMemory');
  GetAddress(@UnloadMusicStream, 'UnloadMusicStream');
  GetAddress(@PlayMusicStream, 'PlayMusicStream');
  GetAddress(@IsMusicStreamPlaying, 'IsMusicStreamPlaying');
  GetAddress(@UpdateMusicStream, 'UpdateMusicStream');
  GetAddress(@StopMusicStream, 'StopMusicStream');
  GetAddress(@PauseMusicStream, 'PauseMusicStream');
  GetAddress(@ResumeMusicStream, 'ResumeMusicStream');
  GetAddress(@SeekMusicStream, 'SeekMusicStream');
  GetAddress(@SetMusicVolume, 'SetMusicVolume');
  GetAddress(@SetMusicPitch, 'SetMusicPitch');
  GetAddress(@GetMusicTimeLength, 'GetMusicTimeLength');
  GetAddress(@GetMusicTimePlayed, 'GetMusicTimePlayed');
  GetAddress(@LoadAudioStream, 'LoadAudioStream');
  GetAddress(@UnloadAudioStream, 'UnloadAudioStream');
  GetAddress(@UpdateAudioStream, 'UpdateAudioStream');
  GetAddress(@IsAudioStreamProcessed, 'IsAudioStreamProcessed');
  GetAddress(@PlayAudioStream, 'PlayAudioStream');
  GetAddress(@PauseAudioStream, 'PauseAudioStream');
  GetAddress(@ResumeAudioStream, 'ResumeAudioStream');
  GetAddress(@IsAudioStreamPlaying, 'IsAudioStreamPlaying');
  GetAddress(@StopAudioStream, 'StopAudioStream');
  GetAddress(@SetAudioStreamVolume, 'SetAudioStreamVolume');
  GetAddress(@SetAudioStreamPitch, 'SetAudioStreamPitch');
  GetAddress(@SetAudioStreamBufferSizeDefault, 'SetAudioStreamBufferSizeDefault');
end;

initialization
  RayLibrary := TmncRayLib.Create('raylib4');
finalization
  FreeAndNil(RayLibrary);
end.
