unit TyroParallax;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *  Scrolling parallax backgrounds.
 *
 *  A parallax is a stack of layers, each a texture that repeats horizontally
 *  and scrolls at its own speed. The far layers (small speed) move slowly, a
 *  ground or foreground layer (speed 1) moves with the world, which is what
 *  gives a scene its depth. Layers are drawn slowest first, so a script can
 *  add them in any order and still get the far-to-near stacking right.
 *
 *  scripts:
 *    local bg = parallax.new('sky')
 *    bg.add('stars.png', 0.15)            -- far
 *    bg.add('hills.png', 0.5)             -- mid
 *    bg.addground('ground.png', 1.0)      -- anchored to the bottom
 *    bg.offset = player.x                 -- scroll with the world
 *
 *  The layer textures may be any width; the layer repeats to cover the screen
 *  width and wraps seamlessly, so one tile is enough to scroll forever.
 *}

{$ifdef FPC}
{$mode delphi}
{$H+}{$M+}
{$endif}

interface

uses
  Classes, SysUtils, Math, SyncObjs, Generics.Collections,
  mnClasses,
  RayLib, RayClasses,
  TyroClasses, TyroScripts;

type
  //Where a layer is anchored vertically: paTop measures Y from the top of the
  //canvas, paBottom measures it up from the bottom edge (so a ground layer
  //stays glued to the floor whatever the window height is).
  TParallaxAnchor = (paTop, paBottom);

const
  //A handle that never addresses a live parallax.
  cParallaxInvalid: Integer = 0;

type
  { TParallaxLayer }

  TParallaxLayer = class(TObject)
  public
    Texture: TTexture2D;
    Speed: Single;   //scroll multiplier: 0 = static, 1 = world speed
    Offset: Single;  //extra constant shift in pixels, independent of scroll
    Y: Single;       //offset from the layer's anchor edge
    Scale: Single;   //1 = native texture size
    Visible: Boolean;
    Anchor: TParallaxAnchor;
    constructor Create;
  end;

  { TParallax }

  TParallax = class(TTyroObject)
  public
    Handle: Integer;
    Name: string;
    Layers: TList<TParallaxLayer>;
    Offset: Single;      //master scroll position, in pixels
    GlobalSpeed: Single; //extra multiplier over every layer's own speed
    Visible: Boolean;
    Tile: Boolean;       //when True layers repeat; when False they scroll off
    constructor Create(AHandle: Integer; const AName: string);
    destructor Destroy; override;
    //Free every layer and unload its GPU texture.
    procedure ClearLayers;
    //Top edge of the highest ground-anchored layer (0 when there is none).
    function GroundY(AHeight: Integer): Single;
    //Draw every layer, far to near. Must run on the thread that owns the GL
    //context (the main loop).
    procedure Draw(AWidth, AHeight: Integer);
  end;

  { TParallaxes }

  //Store of the parallaxes a script created. Layer creation and drawing happen
  //on the main thread; the scroll properties (offset, visible, ...) may be
  //written from the script thread, so every access is guarded by one lock.
  TParallaxes = class(TObject)
  private
    FLock: TmnCriticalSection;
    FItems: TDictionary<Integer, TParallax>;
    FNextHandle: Integer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    //Free every parallax and its textures (an engine rerun drops the finished
    //run's backgrounds through this).
    procedure Clear;
    //Create an empty parallax and return its handle (main thread).
    function Add(const AName: string): Integer;
    function Find(const AName: string): Integer;
    function GetByHandle(Handle: Integer): TParallax;
    function GetName(Handle: Integer): string;

    //Script-thread safe property access.
    procedure SetOffset(Handle: Integer; AValue: Single);
    function GetOffset(Handle: Integer): Single;
    procedure AddOffset(Handle: Integer; ADelta: Single);
    procedure SetGlobalSpeed(Handle: Integer; AValue: Single);
    function GetGlobalSpeed(Handle: Integer): Single;
    procedure SetVisible(Handle: Integer; AValue: Boolean);
    function GetVisible(Handle: Integer): Boolean;
    procedure SetTile(Handle: Integer; AValue: Boolean);
    function GetTile(Handle: Integer): Boolean;
    function GetLayerCount(Handle: Integer): Integer;
    function GetGroundY(Handle: Integer; AHeight: Integer): Single;

    //Main-thread operations.
    procedure AddLayer(Handle: Integer; ATexture: TTexture2D; ASpeed, AY, AScale: Single; AAnchor: TParallaxAnchor);
    procedure ClearLayers(Handle: Integer);
    procedure Draw(AWidth, AHeight: Integer);

    property Count: Integer read GetCount;
  end;

  { TCreateParallaxObject }

  //Create a parallax on the main thread and hand its handle back to the script.
  TCreateParallaxObject = class(TQueueObject)
  private
    FName: string;
    FHandleResult: Integer;
  public
    constructor Create(const AName: string);
    procedure DoExecute; override;
    property HandleResult: Integer read FHandleResult;
  end;

  { TAddParallaxLayerObject }

  //Load a layer texture (GPU upload) on the main thread and append the layer.
  TAddParallaxLayerObject = class(TQueueObject)
  private
    FHandle: Integer;
    FFileName: string;
    FSpeed, FY, FScale: Single;
    FAnchor: TParallaxAnchor;
  public
    constructor Create(AHandle: Integer; const AFileName: string; ASpeed, AY, AScale: Single; AAnchor: TParallaxAnchor);
    procedure DoExecute; override;
  end;

  { TClearParallaxLayersObject }

  TClearParallaxLayersObject = class(TQueueObject)
  private
    FHandle: Integer;
  public
    constructor Create(AHandle: Integer);
    procedure DoExecute; override;
  end;

implementation

uses
  TyroEngines;

{ TParallaxLayer }

constructor TParallaxLayer.Create;
begin
  inherited Create;
  Texture := Default(TTexture2D);
  Speed := 1.0;
  Offset := 0;
  Y := 0;
  Scale := 1.0;
  Visible := True;
  Anchor := paTop;
end;

{ TParallax }

constructor TParallax.Create(AHandle: Integer; const AName: string);
begin
  inherited Create;
  Handle := AHandle;
  Name := AName;
  Layers := TList<TParallaxLayer>.Create;
  Offset := 0;
  GlobalSpeed := 1.0;
  Visible := True;
  Tile := True;
end;

destructor TParallax.Destroy;
begin
  ClearLayers;
  Layers.Free;
  inherited;
end;

procedure TParallax.ClearLayers;
var
  Layer: TParallaxLayer;
begin
  for Layer in Layers do
  begin
    if Layer.Texture.id > 0 then
      RayLib.UnloadTexture(Layer.Texture);
    Layer.Free;
  end;
  Layers.Clear;
end;

function TParallax.GroundY(AHeight: Integer): Single;
var
  i: Integer;
  Layer: TParallaxLayer;
  y: Single;
  Found: Boolean;
begin
  Result := 0;
  Found := False;
  for i := 0 to Layers.Count - 1 do
  begin
    Layer := Layers[i];
    if Layer.Anchor <> paBottom then
      Continue;
    y := AHeight - Layer.Texture.height * Layer.Scale + Layer.Y;
    if (not Found) or (y < Result) then
    begin
      Result := y;
      Found := True;
    end;
  end;
end;

procedure TParallax.Draw(AWidth, AHeight: Integer);
var
  i, j, k: Integer;
  order: array of Integer;
  Layer: TParallaxLayer;
  tw, th, disp, x, y, m: Single;
  Src, Dst: TRectangle;
  key: Integer;
begin
  if (not Visible) or (Layers.Count <= 0) then
    Exit;

  //Insertion-sort layer indices by speed so the slowest (farthest) is drawn
  //first and the fastest (nearest) last. The lists are tiny, so this beats
  //allocating a comparer every frame.
  SetLength(order, Layers.Count);
  for i := 0 to Layers.Count - 1 do
    order[i] := i;
  for i := 1 to Layers.Count - 1 do
  begin
    key := order[i];
    j := i - 1;
    while (j >= 0) and (Layers[order[j]].Speed > Layers[key].Speed) do
    begin
      order[j + 1] := order[j];
      Dec(j);
    end;
    order[j + 1] := key;
  end;

  for k := 0 to Layers.Count - 1 do
  begin
    Layer := Layers[order[k]];
    if (not Layer.Visible) or (Layer.Texture.id <= 0) then
      Continue;
    tw := Layer.Texture.width * Layer.Scale;
    th := Layer.Texture.height * Layer.Scale;
    if (tw <= 0) or (th <= 0) then
      Continue;

    if Layer.Anchor = paBottom then
      y := AHeight - th + Layer.Y
    else
      y := Layer.Y;

    disp := Offset * GlobalSpeed * Layer.Speed + Layer.Offset;

    Src := RectangleOf(0, 0, Layer.Texture.width, Layer.Texture.height);
    if Tile then
    begin
      //Start one tile to the left of the visible edge so a partially scrolled
      //tile is never clipped, then repeat until the screen is covered.
      m := disp - Floor(disp / tw) * tw; //remainder in [0, tw)
      x := -m;
      while x < AWidth do
      begin
        Dst := RectangleOf(x, y, tw, th);
        RayLib.DrawTexturePro(Layer.Texture, Src, Dst, Vector2Of(0, 0), 0, clWhite);
        x := x + tw;
      end;
    end
    else
    begin
      x := -disp;
      if (x > -tw) and (x < AWidth) then
      begin
        Dst := RectangleOf(x, y, tw, th);
        RayLib.DrawTexturePro(Layer.Texture, Src, Dst, Vector2Of(0, 0), 0, clWhite);
      end;
    end;
  end;
end;

{ TParallaxes }

constructor TParallaxes.Create;
begin
  inherited Create;
  FLock := TmnCriticalSection.Create;
  FItems := TDictionary<Integer, TParallax>.Create;
  FNextHandle := 1;
end;

destructor TParallaxes.Destroy;
begin
  Clear;
  FItems.Free;
  FLock.Free;
  inherited;
end;

function TParallaxes.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TParallaxes.Clear;
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    for Item in FItems.Values do
      Item.Free;
    FItems.Clear;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.Add(const AName: string): Integer;
begin
  FLock.Enter;
  try
    Result := FNextHandle;
    Inc(FNextHandle);
    FItems.Add(Result, TParallax.Create(Result, AName));
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.Find(const AName: string): Integer;
var
  Item: TParallax;
begin
  Result := 0;
  FLock.Enter;
  try
    for Item in FItems.Values do
      if Item.Name = AName then
      begin
        Result := Item.Handle;
        Break;
      end;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.GetByHandle(Handle: Integer): TParallax;
begin
  //No lock: the caller holds it, or is the main thread which never races with
  //itself over the dictionary.
  Result := nil;
  FItems.TryGetValue(Handle, Result);
end;

function TParallaxes.GetName(Handle: Integer): string;
var
  Item: TParallax;
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

procedure TParallaxes.SetOffset(Handle: Integer; AValue: Single);
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Offset := AValue;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.GetOffset(Handle: Integer): Single;
var
  Item: TParallax;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Offset;
  finally
    FLock.Leave;
  end;
end;

procedure TParallaxes.AddOffset(Handle: Integer; ADelta: Single);
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Offset := Item.Offset + ADelta;
  finally
    FLock.Leave;
  end;
end;

procedure TParallaxes.SetGlobalSpeed(Handle: Integer; AValue: Single);
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.GlobalSpeed := AValue;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.GetGlobalSpeed(Handle: Integer): Single;
var
  Item: TParallax;
begin
  Result := 1;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.GlobalSpeed;
  finally
    FLock.Leave;
  end;
end;

procedure TParallaxes.SetVisible(Handle: Integer; AValue: Boolean);
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Visible := AValue;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.GetVisible(Handle: Integer): Boolean;
var
  Item: TParallax;
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

procedure TParallaxes.SetTile(Handle: Integer; AValue: Boolean);
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.Tile := AValue;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.GetTile(Handle: Integer): Boolean;
var
  Item: TParallax;
begin
  Result := True;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Tile;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.GetLayerCount(Handle: Integer): Integer;
var
  Item: TParallax;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.Layers.Count;
  finally
    FLock.Leave;
  end;
end;

function TParallaxes.GetGroundY(Handle: Integer; AHeight: Integer): Single;
var
  Item: TParallax;
begin
  Result := 0;
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Result := Item.GroundY(AHeight);
  finally
    FLock.Leave;
  end;
end;

procedure TParallaxes.AddLayer(Handle: Integer; ATexture: TTexture2D; ASpeed, AY, AScale: Single; AAnchor: TParallaxAnchor);
var
  Item: TParallax;
  Layer: TParallaxLayer;
begin
  FLock.Enter;
  try
    if not FItems.TryGetValue(Handle, Item) then
    begin
      //Store is gone: the loader still owns the texture, drop it.
      if ATexture.id > 0 then
        RayLib.UnloadTexture(ATexture);
      Exit;
    end;
    Layer := TParallaxLayer.Create;
    Layer.Texture := ATexture;
    Layer.Speed := ASpeed;
    Layer.Y := AY;
    Layer.Scale := AScale;
    Layer.Anchor := AAnchor;
    Item.Layers.Add(Layer);
  finally
    FLock.Leave;
  end;
end;

procedure TParallaxes.ClearLayers(Handle: Integer);
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    if FItems.TryGetValue(Handle, Item) then
      Item.ClearLayers;
  finally
    FLock.Leave;
  end;
end;

procedure TParallaxes.Draw(AWidth, AHeight: Integer);
var
  Item: TParallax;
begin
  FLock.Enter;
  try
    for Item in FItems.Values do
      Item.Draw(AWidth, AHeight);
  finally
    FLock.Leave;
  end;
end;

{ TCreateParallaxObject }

constructor TCreateParallaxObject.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FHandleResult := 0;
end;

procedure TCreateParallaxObject.DoExecute;
begin
  FHandleResult := Engine.Parallaxes.Add(FName);
end;

{ TAddParallaxLayerObject }

constructor TAddParallaxLayerObject.Create(AHandle: Integer; const AFileName: string; ASpeed, AY, AScale: Single; AAnchor: TParallaxAnchor);
begin
  inherited Create;
  FHandle := AHandle;
  FFileName := AFileName;
  FSpeed := ASpeed;
  FY := AY;
  FScale := AScale;
  FAnchor := AAnchor;
end;

procedure TAddParallaxLayerObject.DoExecute;
var
  Texture: TTexture2D;
  FileName: string;
begin
  FileName := Res.GuessFileName(FFileName, Res.WorkPath);
  Texture := RayLib.LoadTexture(PUTF8Char(UTF8String(FileName)));
  if Texture.id <= 0 then
  begin
    if IsConsole then
      WriteLn('Parallax layer not loaded: ' + FFileName);
    Exit;
  end;
  Engine.Parallaxes.AddLayer(FHandle, Texture, FSpeed, FY, FScale, FAnchor);
end;

{ TClearParallaxLayersObject }

constructor TClearParallaxLayersObject.Create(AHandle: Integer);
begin
  inherited Create;
  FHandle := AHandle;
end;

procedure TClearParallaxLayersObject.DoExecute;
begin
  Engine.Parallaxes.ClearLayers(FHandle);
end;

end.
