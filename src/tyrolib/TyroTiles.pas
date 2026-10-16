unit TyroTiles;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *  Native tilemap support.
 *
 *  A tileset is one texture that holds a grid of equally sized tiles; a tilemap
 *  is a rectangular grid of tile ids that refers into that tileset. The store
 *  (TTiles) owns the GPU textures, one per map, and hands out handles, exactly
 *  like TSprites does for sprites. Map data is mutated from the script thread and
 *  read from the main thread, so every access is serialized by the store lock.
 *
 *  Drawing is a single queued command (TDrawTileMapObject): the Lua map:draw()
 *  enqueues one command no matter how many tiles are visible, and the native
 *  DrawMap loops the visible range on the main thread. That keeps the draw queue
 *  from being flooded with one command per tile.
 *
 *  A queued draw captures a snapshot of the grid (see Capture) the moment the
 *  script calls map:draw(). Otherwise the main thread could sample the grid in
 *  the middle of the script's per-frame "clear then re-stamp every entity"
 *  sequence and draw a half-updated map (torn/missing sprites).
 *}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, Math, SyncObjs, Generics.Collections,
  mnClasses,
  RayLib, RayClasses,
  TyroScripts, TyroClasses;

const
  cTileMapInvalid: Integer = 0;

type
  { TTileMap }

  { One tilemap: the tileset texture it draws from, the geometry of that tileset
    (tile size, columns, spacing, margin) and the map grid itself. A tile id of 0
    means "empty"; ids are 1-based, so id 1 is the top-left tile of the sheet. }
  TTileMap = class(TTyroObject)
  public
    Handle: Integer;
    Name: string;
    Texture: TTexture2D;
    TileWidth: Integer;
    TileHeight: Integer;
    Columns: Integer;   // tiles per row in the tileset sheet
    Spacing: Integer;   // pixels between tiles in the sheet
    Margin: Integer;    // pixels from the sheet edge to the first tile
    Cols: Integer;      // map width in tiles
    Rows: Integer;      // map height in tiles
    Data: TArray<Integer>; // Cols*Rows tile ids, row-major, 0 = empty
    Solid: TDictionary<Integer, Boolean>; // tile ids treated as solid walls
    Visible: Boolean;
    X: Single;          // last drawn position
    Y: Single;
    Scale: Single;
    constructor Create(AHandle: Integer; const AName: string);
    destructor Destroy; override;
  end;

  { TTiles }

  { The store. Mirrors TSprites: a lock, a dictionary keyed by handle and an
    ever-increasing handle counter, so a handle a script still holds can never
    address a map of a later run. }
  TTiles = class(TObject)
  private
    FLock: TmnCriticalSection;
    FItems: TDictionary<Integer, TTileMap>;
    FNextHandle: Integer;
    function GetCount: integer;
    function GetItems(Index: Integer): TTileMap;
    //Unloads the GPU texture a map owns and resets it to unloaded.
    procedure FreeMapTexture(AMap: TTileMap);
  public
    constructor Create;
    destructor Destroy; override;
    //* Free every map and unload the GPU textures they own; the store is empty
    //* again (an engine rerun does this to leave no map of the finished run).
    procedure Clear;
    // Add a texture-less map, returns a handle
    function AddEmpty(const AName: string = ''): integer;
    function FindByName(const AName: string): integer;
    function Exists(Handle: integer): boolean;

    // Load (or replace) the tileset texture of an existing map. Must run on the
    // main thread: it uploads/releases a GPU texture. Returns success.
    function Load(Handle: integer; const AFileName: string;
      ATileWidth, ATileHeight, AColumns, ASpacing, AMargin: Integer): boolean;

    function GetTexture(Handle: integer): TTexture2D;
    function GetName(Handle: integer): string;
    procedure SetName(Handle: integer; const AName: string);

    function GetTileWidth(Handle: integer): integer;
    function GetTileHeight(Handle: integer): integer;
    function GetColumns(Handle: integer): integer;
    function GetSpacing(Handle: integer): integer;
    function GetMargin(Handle: integer): integer;
    function GetCols(Handle: integer): integer;
    function GetRows(Handle: integer): integer;
    // Full pixel size of the map (unscaled)
    function GetWidth(Handle: integer): integer;
    function GetHeight(Handle: integer): integer;

    procedure SetVisible(Handle: integer; AVisible: boolean);
    function GetVisible(Handle: integer): boolean;
    procedure SetPosition(Handle: integer; X, Y: single);
    procedure SetPositionComponent(Handle: integer; IsX: Boolean; Value: single);
    function GetX(Handle: integer): single;
    function GetY(Handle: integer): single;
    procedure SetScale(Handle: integer; AScale: single);
    function GetScale(Handle: integer): single;

    // Map grid
    procedure Resize(Handle: integer; ACols, ARows: integer);
    procedure Fill(Handle: integer; ATile: integer);
    procedure ClearMap(Handle: integer);
    procedure SetTile(Handle: integer; ACol, ARow, ATile: integer);
    function GetTile(Handle: integer; ACol, ARow: integer): integer;
    procedure GetData(Handle: integer; out AData: TArray<Integer>);

    // Atomic snapshot of the grid and its size, taken under the store lock. A
    // queued draw captures one so the main thread never samples a map that the
    // script is half-way through updating (clear + many set).
    procedure Capture(Handle: integer; out AData: TArray<Integer>; out ACols, ARows: integer);

    // Solid tiles
    procedure SetSolidTile(Handle: integer; ATile: integer; ASolid: boolean);
    function GetSolidTile(Handle: integer; ATile: integer): boolean;
    // True when the cell is outside the map or holds a tile marked solid.
    function SolidAt(Handle: integer; ACol, ARow: integer): boolean;

    // Draw a snapshot of one map (taken by Capture on the script thread) on the
    // main thread, inside Board.BeginDraw/EndDraw. The grid is read from the
    // snapshot, never from the live map, so a script update cannot tear the draw.
    procedure DrawMap(Handle: integer; const AData: TArray<Integer>; ACols, ARows: integer;
      ACanvas: TTyroCanvas; AX, AY, AScale: single; ATint: TColor);

    property Count: integer read GetCount;
    property Items[Index: Integer]: TTileMap read GetItems; default;
  end;

  { TLoadTileMapObject }

  { Loads a tileset into a map on the main thread (the upload needs the GL
    context) and returns whether it succeeded. }
  TLoadTileMapObject = class(TQueueObject)
  private
    FFileName: string;
    FHandle: integer;
    FTileWidth, FTileHeight, FColumns, FSpacing, FMargin: Integer;
    FResult: boolean;
  public
    constructor Create(AHandle: integer; const AFileName: string;
      ATileWidth, ATileHeight, AColumns, ASpacing, AMargin: Integer);
    procedure DoExecute; override;
    property HandleResult: boolean read FResult;
  end;

  { TDrawTileMapObject }

  { One queued tilemap draw. Captures a snapshot of the map grid at construction
    time (on the script thread) together with the transform, so the tile loop the
    main thread runs at execute time draws a complete frame even if the script has
    already started the next update. }
  TDrawTileMapObject = class(TDrawObject)
  private
    FTiles: TTiles;
    FHandle: integer;
    FData: TArray<Integer>;
    FCols, FRows: integer;
    FX, FY, FScale: single;
    FTint: TColor;
  public
    constructor Create(ATiles: TTiles; AHandle: integer; AX, AY, AScale: single;
      ATint: TColor; ACanvas: TTyroCanvas);
    procedure DoExecute; override;
  end;

implementation

uses
  TyroEngines;

{ TTileMap }

constructor TTileMap.Create(AHandle: Integer; const AName: string);
begin
  inherited Create;
  Handle := AHandle;
  Name := AName;
  Texture := Default(TTexture2D);
  TileWidth := 16;
  TileHeight := 16;
  Columns := 1;
  Spacing := 0;
  Margin := 0;
  Cols := 0;
  Rows := 0;
  Data := nil;
  Solid := TDictionary<Integer, Boolean>.Create;
  Visible := True;
  X := 0;
  Y := 0;
  Scale := 1.0;
end;

destructor TTileMap.Destroy;
begin
  Data := nil;
  Solid.Free;
  inherited;
end;

{ TTiles }

constructor TTiles.Create;
begin
  inherited Create;
  FLock := TmnCriticalSection.Create;
  FItems := TDictionary<Integer, TTileMap>.Create;
  FNextHandle := 1;
end;

destructor TTiles.Destroy;
begin
  Clear;
  FItems.Free;
  FLock.Free;
  inherited;
end;

procedure TTiles.FreeMapTexture(AMap: TTileMap);
begin
  if AMap = nil then
    Exit;
  if AMap.Texture.id > 0 then
    RayLib.UnloadTexture(AMap.Texture);
  AMap.Texture := Default(TTexture2D);
end;

procedure TTiles.Clear;
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    for Map in FItems.Values do
    begin
      FreeMapTexture(Map);
      Map.Free;
    end;
    FItems.Clear;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TTiles.GetItems(Index: Integer): TTileMap;
begin
  Result := FItems[Index];
end;

function TTiles.AddEmpty(const AName: string): integer;
begin
  FLock.Enter;
  try
    FItems.Add(FNextHandle, TTileMap.Create(FNextHandle, AName));
    Result := FNextHandle;
    Inc(FNextHandle);
  finally
    FLock.Leave;
  end;
end;

function TTiles.FindByName(const AName: string): integer;
var
  Map: TTileMap;
begin
  Result := cTileMapInvalid;
  if AName = '' then
    Exit;
  FLock.Enter;
  try
    for Map in FItems.Values do
      if Map.Name = AName then
      begin
        Result := Map.Handle;
        Exit;
      end;
  finally
    FLock.Leave;
  end;
end;

function TTiles.Exists(Handle: integer): boolean;
begin
  FLock.Enter;
  try
    Result := FItems.ContainsKey(Handle);
  finally
    FLock.Leave;
  end;
end;

function TTiles.Load(Handle: integer; const AFileName: string;
  ATileWidth, ATileHeight, AColumns, ASpacing, AMargin: Integer): boolean;
var
  Map: TTileMap;
  aTexture: TTexture2D;
begin
  Result := False;
  if ATileWidth < 1 then
    ATileWidth := 16;
  if ATileHeight < 1 then
    ATileHeight := ATileWidth;
  if ASpacing < 0 then
    ASpacing := 0;
  if AMargin < 0 then
    AMargin := 0;

  aTexture := RayLib.LoadTexture(PUTF8Char(UTF8String(AFileName)));
  if aTexture.id <= 0 then
  begin
    if IsConsole then
      WriteLn('Tileset not loaded: ' + AFileName);
    Exit;
  end;

  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
    begin
      RayLib.UnloadTexture(aTexture); //store did not take ownership
      Exit;
    end;
    if Map.Texture.id > 0 then
      RayLib.UnloadTexture(Map.Texture);
    Map.Texture := aTexture;
    Map.TileWidth := ATileWidth;
    Map.TileHeight := ATileHeight;
    Map.Spacing := ASpacing;
    Map.Margin := AMargin;
    if AColumns > 0 then
      Map.Columns := AColumns
    else
    begin
      // Derive the sheet width from the texture when the caller did not say.
      Map.Columns := (aTexture.width - 2 * AMargin + ASpacing) div (ATileWidth + ASpacing);
      if Map.Columns < 1 then
        Map.Columns := 1;
    end;
    Result := True;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetTexture(Handle: integer): TTexture2D;
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Texture
    else
      Result := Default(TTexture2D);
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetName(Handle: integer): string;
var
  Map: TTileMap;
begin
  Result := '';
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Name;
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.SetName(Handle: integer; const AName: string);
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Map.Name := AName;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetTileWidth(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.TileWidth;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetTileHeight(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.TileHeight;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetColumns(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Columns;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetSpacing(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Spacing;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetMargin(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Margin;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetCols(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Cols;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetRows(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Rows;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetWidth(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Cols * Map.TileWidth;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetHeight(Handle: integer): integer;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Rows * Map.TileHeight;
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.SetVisible(Handle: integer; AVisible: boolean);
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Map.Visible := AVisible;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetVisible(Handle: integer): boolean;
var
  Map: TTileMap;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Visible;
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.SetPosition(Handle: integer; X, Y: single);
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
    begin
      Map.X := X;
      Map.Y := Y;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.SetPositionComponent(Handle: integer; IsX: Boolean; Value: single);
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
    begin
      if IsX then
        Map.X := Value
      else
        Map.Y := Value;
    end;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetX(Handle: integer): single;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.X;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetY(Handle: integer): single;
var
  Map: TTileMap;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Y;
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.SetScale(Handle: integer; AScale: single);
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Map.Scale := AScale;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetScale(Handle: integer): single;
var
  Map: TTileMap;
begin
  Result := 1;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Scale;
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.Resize(Handle: integer; ACols, ARows: integer);
var
  Map: TTileMap;
  Old: TArray<Integer>;
  OldCols, OldRows, r, c, CopyCols, CopyRows: integer;
begin
  if ACols < 0 then
    ACols := 0;
  if ARows < 0 then
    ARows := 0;
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    Old := Map.Data;
    OldCols := Map.Cols;
    OldRows := Map.Rows;
    Map.Cols := ACols;
    Map.Rows := ARows;
    SetLength(Map.Data, ACols * ARows);
    // Keep the overlap so a resize does not throw away the map.
    CopyCols := Min(ACols, OldCols);
    CopyRows := Min(ARows, OldRows);
    for r := 0 to CopyRows - 1 do
      for c := 0 to CopyCols - 1 do
        Map.Data[r * ACols + c] := Old[r * OldCols + c];
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.Fill(Handle: integer; ATile: integer);
var
  Map: TTileMap;
  i: integer;
begin
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    for i := 0 to Length(Map.Data) - 1 do
      Map.Data[i] := ATile;
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.ClearMap(Handle: integer);
begin
  Fill(Handle, 0);
end;

procedure TTiles.SetTile(Handle: integer; ACol, ARow, ATile: integer);
var
  Map: TTileMap;
  idx: integer;
begin
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    if (ACol < 0) or (ARow < 0) or (ACol >= Map.Cols) or (ARow >= Map.Rows) then
      Exit;
    idx := ARow * Map.Cols + ACol;
    if (idx >= 0) and (idx < Length(Map.Data)) then
      Map.Data[idx] := ATile;
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetTile(Handle: integer; ACol, ARow: integer): integer;
var
  Map: TTileMap;
  idx: integer;
begin
  Result := 0;
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    if (ACol < 0) or (ARow < 0) or (ACol >= Map.Cols) or (ARow >= Map.Rows) then
      Exit;
    idx := ARow * Map.Cols + ACol;
    if (idx >= 0) and (idx < Length(Map.Data)) then
      Result := Map.Data[idx];
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.GetData(Handle: integer; out AData: TArray<Integer>);
var
  ACols, ARows: integer;
begin
  Capture(Handle, AData, ACols, ARows);
end;

procedure TTiles.Capture(Handle: integer; out AData: TArray<Integer>; out ACols, ARows: integer);
var
  Map: TTileMap;
  i: integer;
begin
  AData := nil;
  ACols := 0;
  ARows := 0;
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    ACols := Map.Cols;
    ARows := Map.Rows;
    SetLength(AData, Length(Map.Data));
    for i := 0 to Length(Map.Data) - 1 do
      AData[i] := Map.Data[i];
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.SetSolidTile(Handle: integer; ATile: integer; ASolid: boolean);
var
  Map: TTileMap;
begin
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    if ASolid then
      Map.Solid.AddOrSetValue(ATile, True)
    else
      Map.Solid.Remove(ATile);
  finally
    FLock.Leave;
  end;
end;

function TTiles.GetSolidTile(Handle: integer; ATile: integer): boolean;
var
  Map: TTileMap;
  v: Boolean;
begin
  Result := False;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Map) then
      Result := Map.Solid.TryGetValue(ATile, v);
  finally
    FLock.Leave;
  end;
end;

function TTiles.SolidAt(Handle: integer; ACol, ARow: integer): boolean;
var
  Map: TTileMap;
  idx: integer;
  v: Boolean;
begin
  // Outside the map is solid, so a player can never walk off the edge.
  Result := True;
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    if (ACol < 0) or (ARow < 0) or (ACol >= Map.Cols) or (ARow >= Map.Rows) then
      Exit;
    idx := ARow * Map.Cols + ACol;
    if (idx < 0) or (idx >= Length(Map.Data)) then
      Exit;
    Result := Map.Solid.TryGetValue(Map.Data[idx], v);
  finally
    FLock.Leave;
  end;
end;

procedure TTiles.DrawMap(Handle: integer; const AData: TArray<Integer>; ACols, ARows: integer;
  ACanvas: TTyroCanvas; AX, AY, AScale: single; ATint: TColor);
var
  Map: TTileMap;
  Tw, Th, OX, OY: single;
  C0, C1, R0, R1, Col, Row, Idx, Tile, SX, SY: integer;
  Src, Dst: TRectangle;
begin
  if ACanvas = nil then
    Exit;
  // The snapshot's own geometry decides the grid; the store only supplies the
  // tileset texture and its layout, which are fixed once the map is loaded.
  if (ACols <= 0) or (ARows <= 0) then
    Exit;
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Map) then
      Exit;
    if (not Map.Visible) or (Map.Texture.id <= 0) then
      Exit;
    if (Map.TileWidth <= 0) or (Map.TileHeight <= 0) or (Map.Columns <= 0) then
      Exit;
    if AScale <= 0 then
      AScale := 1;
    Tw := Map.TileWidth * AScale;
    Th := Map.TileHeight * AScale;
    // The canvas origin (0 for Board) plus the map's own position.
    OX := ACanvas.Origin.X + AX;
    OY := ACanvas.Origin.Y + AY;
    // Cull to the visible tile range.
    C0 := Floor((0 - AX) / Tw);
    if C0 < 0 then
      C0 := 0;
    R0 := Floor((0 - AY) / Th);
    if R0 < 0 then
      R0 := 0;
    C1 := Floor((ACanvas.Width - AX) / Tw) + 1;
    if C1 > ACols - 1 then
      C1 := ACols - 1;
    R1 := Floor((ACanvas.Height - AY) / Th) + 1;
    if R1 > ARows - 1 then
      R1 := ARows - 1;
    for Row := R0 to R1 do
    begin
      for Col := C0 to C1 do
      begin
        Idx := Row * ACols + Col;
        if (Idx < 0) or (Idx >= Length(AData)) then
          Continue;
        Tile := AData[Idx];
        if Tile <= 0 then
          Continue;
        Dec(Tile); // ids are 1-based
        SX := Map.Margin + (Tile mod Map.Columns) * (Map.TileWidth + Map.Spacing);
        SY := Map.Margin + (Tile div Map.Columns) * (Map.TileHeight + Map.Spacing);
        Src := RectangleOf(SX, SY, Map.TileWidth, Map.TileHeight);
        Dst := RectangleOf(OX + Col * Tw, OY + Row * Th, Tw, Th);
        RayLib.DrawTexturePro(Map.Texture, Src, Dst, Vector2Of(0, 0), 0, ATint);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

{ TLoadTileMapObject }

constructor TLoadTileMapObject.Create(AHandle: integer; const AFileName: string;
  ATileWidth, ATileHeight, AColumns, ASpacing, AMargin: Integer);
begin
  inherited Create;
  FHandle := AHandle;
  FFileName := AFileName;
  FTileWidth := ATileWidth;
  FTileHeight := ATileHeight;
  FColumns := AColumns;
  FSpacing := ASpacing;
  FMargin := AMargin;
  FResult := False;
end;

procedure TLoadTileMapObject.DoExecute;
begin
  if Engine.Tiles <> nil then
    FResult := Engine.Tiles.Load(FHandle, FFileName, FTileWidth, FTileHeight, FColumns, FSpacing, FMargin);
end;

{ TDrawTileMapObject }

constructor TDrawTileMapObject.Create(ATiles: TTiles; AHandle: integer;
  AX, AY, AScale: single; ATint: TColor; ACanvas: TTyroCanvas);
begin
  inherited Create(ACanvas);
  FTiles := ATiles;
  FHandle := AHandle;
  FData := nil;
  FCols := 0;
  FRows := 0;
  // Snapshot the grid now, on the script thread: the main thread must not read
  // a map that the script is midway through clearing/restamping.
  if FTiles <> nil then
    FTiles.Capture(AHandle, FData, FCols, FRows);
  FX := AX;
  FY := AY;
  FScale := AScale;
  FTint := ATint;
end;

procedure TDrawTileMapObject.DoExecute;
begin
  if FTiles = nil then
    Exit;
  FTiles.DrawMap(FHandle, FData, FCols, FRows, Canvas, FX, FY, FScale, FTint);
end;

end.
