unit TyroControls;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey 
 *
 *}

{$ifdef FPC}
{$mode delphi}
{$H+}{$M+}
{$endif}

interface

uses
  Classes, SysUtils, Types,
  mnUtils, mnClasses,
  {$ifdef FPC}
   LCLType, LazUTF8,
  {$endif}
  SyncObjs, //after LCLType
  RayLib, RayClasses,
  TyroClasses;

const
  cMainMargin = 32;
  cMinResizeSize = 16;

type
  {$ifdef FPC}
  TUTF8Char = LCLType.TUTF8Char;
  {$else}
  TUTF8Char = String[7]; //* ported from LCLType;
  {$endif}

  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TScrollbarType = (sbtHorizontal, sbtVertical);
  TScrollbarTypes = set of TScrollbarType;

  TScrollCode = (
    scrollTOP,
    scrollBOTTOM,
    scrollLINEDOWN,
    scrollLINEUP,
    scrollPAGEDOWN,
    scrollPAGEUP,
    scrollTHUMBPOSITION,
    scrollTHUMBTRACK,
    scrollENDSCROLL
  );

  TTyroControlStyle = (
    csClip,
    csOpaque
  );
  TTyroControlStyles = set of TTyroControlStyle;

  TTyroLayout = class;
  TTyroControl = class;
  TTyroWindow = class;

  TTyroLayoutState = (
    csCreating,
    csCreated,
    lsAligning,
    lsSizing,
    csDestroying
  );

  TTyroLayoutStates = set of TTyroLayoutState;

  TAlign = (alNone, alLeft, alTop, alRight, alBottom, alClient);
  TBorder = (brdNone, brdThin, brdThick, brdSizable);

  TTyroResizeSide = (rsLeft, rsRight, rsTop, rsBottom);
  TTyroResizeSides = set of TTyroResizeSide;

  TTyroControls = class;

  { TTyroLayout }

  TTyroLayout = class abstract(TmnNamedObject)
  private
    FBorder: TBorder;
    FMargin: Integer;
    FAlign: TAlign;
    FParent: TTyroLayout;
    FBoundsRect: TRect;
    FWindowRect: TRect;
    FState: TTyroLayoutStates;
    FControls: TTyroControls;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetAlign(AValue: TAlign);
    procedure SetBorder(AValue: TBorder);
    procedure SetHeight(AValue: Integer);
    procedure SetParent(AValue: TTyroLayout);
    procedure SetMargin(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected
    procedure SetBoundsRect(AValue: TRect);
    procedure SetWindowRect(AValue: TRect);

    procedure Resize;
    procedure Resized; virtual;

    procedure AddControl(AControl: TTyroLayout);
    procedure PaintWindow(ACanvas: TTyroCanvas); virtual;
    function BorderSize: Integer;
  public
    constructor Create(AParent: TTyroLayout); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Realign; virtual;
    procedure AlignControls; virtual;
    property Controls: TTyroControls read FControls;
    //WindowRect aligned rect, is Virtual changed by RealignControls of parent used paint control
    property WindowRect: TRect read FWindowRect;
    property State: TTyroLayoutStates read FState;
    property Align: TAlign read FAlign write SetAlign;
    property Parent: TTyroLayout read FParent write SetParent;
    property Margin: Integer read FMargin write SetMargin;
    property Border: TBorder read FBorder write SetBorder;
    //Real bounds control rect
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;


  TTyroControls = class(TmnNamedObjectList<TTyroLayout>)
  public
  end;

  { TTyroControl }

  TTyroControl = class abstract(TTyroLayout)
  private
    FBackColor: TColor;
    FWindow: TTyroWindow;
    FCanvas: TTyroCanvas;
    FVisible: Boolean;
    FResizing: Boolean;
    FResizeSides: TTyroResizeSides;
    FResizeStartRect: TRect;
    FResizeStartMouse: TVector2;
    FLastMouseX: Integer;
    FLastMouseY: Integer;
    function GetFocused: Boolean;
    procedure SetBackColor(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
    procedure SetWindow(AValue: TTyroWindow);
    procedure SetCanvas(AValue: TTyroCanvas);
    function GetClientLeft: Integer;
    function GetClientTop: Integer;
    procedure SetFocused(AValue: Boolean);
  protected
    Style: TTyroControlStyles;
    function GetClientRect: TRect;
    function GetClientWidth: Integer;
    function GetClientHeight: Integer;
    //* Edges which may be resized when hovering at the local point (X, Y).
    //* brdSizable honors the Align constraint: aligned controls only expose the
    //* single free edge, alClient exposes none, alNone exposes all four.
    function GetResizeSides(X, Y: Integer): TTyroResizeSides;
    procedure ApplyResize;
    procedure PaintBorder(ACanvas: TTyroCanvas);

    //* Make sure the own (transparent) texture buffer exists and matches the
    //* control size. Returns False when no buffer can be created.
    function PrepareCanvas: Boolean;
    procedure ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
    procedure SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
    procedure SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);
    procedure Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); virtual;

    procedure DoBorder(ACanvas: TTyroCanvas); virtual;
    procedure DoPaintBackground(ACanvas: TTyroCanvas); virtual;
    procedure DoPaint(ACanvas: TTyroCanvas); virtual;

    procedure Created; virtual;
    property Window: TTyroWindow read FWindow write SetWindow;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure Invalidate; virtual;

    procedure PaintWindow(ACanvas: TTyroCanvas); override;

    procedure FocusChanged; virtual;
    procedure Show;
    procedure Hide;

    //Move the control to the end of the parent's control list (drawn last, on
    //top) when it is not aligned
    procedure BringToFront;

    procedure KeyPress(var Key: TUTF8Char); virtual;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: TKeyboardKey; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseMove(Shift: TShiftState; x, y: integer); virtual;

    property Focused: Boolean read GetFocused write SetFocused;
    property ClientRect: TRect read GetClientRect;
    property ClientLeft: Integer read GetClientLeft;
    property ClientTop: Integer read GetClientTop;
    property ClientWidth: Integer read GetClientWidth;
    property ClientHeight: Integer read GetClientHeight;

    property BackColor: TColor read FBackColor write SetBackColor;

    property Visible: Boolean read FVisible write SetVisible;

    //* Own transparent texture buffer. The control content is painted into it
    //* every frame and the buffer is drawn (blitted) on top of the canvas.
    property Canvas: TTyroCanvas read FCanvas write SetCanvas;
  end;

  { TTyroPanel }

  TTyroPanel = class(TTyroControl)
  public
    constructor Create(AParent: TTyroLayout); override;
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  end;

  { TTyroButton }

  TTyroButton = class(TTyroControl)
  private
    FCaption: utf8string;
    FHover: Boolean;
    FDown: Boolean;
    FClicked: Boolean;
    FWasDown: Boolean;
    function IsMouseOver: Boolean;
    procedure CheckState;
    procedure SetCaption(AValue: utf8string);
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    property Caption: utf8string read FCaption write SetCaption;
    property Hover: Boolean read FHover;
    property Down: Boolean read FDown;
    property Clicked: Boolean read FClicked;
  end;

  { TTyroWindow }

  TTyroWindow = class abstract(TTyroLayout)
  private
    FCanvas: TTyroCanvas;
    FFocused: TTyroControl;
    FTitle: utf8string;
    procedure SetCanvas(AValue: TTyroCanvas);
    procedure SetFocused(AValue: TTyroControl);
    procedure SetTitle(AValue: utf8string);
  protected
    procedure PrepareCanvas; virtual;
    function CreateCanvas: TTyroCanvas; virtual; abstract;
  public
    Visible: Boolean;
    constructor Create(AParent: TTyroLayout); overload; override;
    constructor Create(AParent: TTyroLayout; AWidth, AHeight: Integer); overload;
    destructor Destroy; override;
    procedure Paint;
    property Canvas: TTyroCanvas read FCanvas write SetCanvas;
    property Title: utf8string read FTitle write SetTitle;
    property Focused: TTyroControl read FFocused write SetFocused;
  end;

  TTyroMainWindowOption = (moWindow, moOpaque, moShowFPS);
  TTyroMainWindowOptions= set of TTyroMainWindowOption;

  { TTyroMainWindow }

  TTyroMainWindow = class(TTyroWindow)
  private
    FFPS: Integer;
    FOptions: TTyroMainWindowOptions;
    FBackColor: TColor;
    //* The control which captured the mouse (e.g. dragging a sizable border).
    //* It keeps receiving MouseMove/MouseUp until the left button is released.
    FControlCapture: TTyroControl;
    function GetCanvasWidth: Integer;
    function GetCanvasHeight: Integer;
  protected
    FTextureMode: Boolean;
    IsTerminated: Boolean;
    FCanvasLock: TCriticalSection;
    Camera2D: TCamera2D;
    function CreateCanvas: TTyroCanvas; override;
    procedure Terminate; virtual;
  public
    constructor Create(AParent: TTyroLayout); overload; override;
    constructor Create; reintroduce; overload;
    destructor Destroy; override;

    //* TextureMode create texture with canvas
    procedure ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean = False); overload; virtual;
    procedure ShowWindow; overload;
    procedure SetFPS(FPS: Integer); virtual;
    procedure HideWindow; virtual;

    //* Resize the window (and canvas) to the given size; canvas is inset by margin + border
    procedure Resize(AWidth, AHeight: Integer); virtual;

    //* Before Show window
    procedure Init; virtual;
    //* After window initialized and other resource, load your resources here
    procedure Load; virtual;
    procedure Start; virtual;
    procedure Update; virtual;
    procedure PrepareDraw; virtual;
    procedure Draw; virtual;

    //When application exit, unload your resources
    procedure Unload; virtual;

    function Terminated: Boolean; virtual;
    procedure ProcessInput; virtual;

    procedure Run;
    procedure Shutdown; virtual;

    property CanvasLock: TCriticalSection read FCanvasLock;
    property Options: TTyroMainWindowOptions read FOptions write FOptions;
    property BackColor: TColor read FBackColor write FBackColor;
    property FPS: Integer read FFPS write SetFPS;
  end;

const
  cDefaultWindowWidth = 640;
  cDefaultWindowHeight = 480;

var
  Main: TTyroMainWindow = nil;

function Canvas: TTyroCanvas; inline;

implementation

{ Convert a Unicode codepoint to a UTF-8 encoded short string (TUTF8Char) }
function CodePointToUTF8(Ch: Integer): TUTF8Char;
begin
  if Ch < $80 then
    Result := TUTF8Char(Chr(Ch))
  else if Ch < $800 then
    Result := TUTF8Char(Chr($C0 or (Ch shr 6)) + Chr($80 or (Ch and $3F)))
  else if Ch < $10000 then
    Result := TUTF8Char(Chr($E0 or (Ch shr 12)) + Chr($80 or ((Ch shr 6) and $3F)) + Chr($80 or (Ch and $3F)))
  else
    Result := TUTF8Char('');
end;

{ TTyroMainWindow }

function Canvas: TTyroCanvas;
begin
  Result := Main.Canvas;
end;

constructor TTyroMainWindow.Create(AParent: TTyroLayout);
begin
  inherited;
  FControlCapture := nil;
  FOptions := [moWindow, moOpaque];
  RayLibrary.Load;
  Resources := TTyroResources.Create;
  Resources.WorkSpace := ExtractFilePath(ParamStr(0));
  FCanvasLock := TCriticalSection.Create;
  BoundsRect.Width := ScreenWidth;
  BoundsRect.Height := ScreenHeight;
  Margin := cMainMargin;
  FBackColor := clCornflowerBlue;
end;

constructor TTyroMainWindow.Create;
begin
  Create(nil);
end;

function TTyroMainWindow.CreateCanvas: TTyroCanvas;
begin
  Result := TTyroTextureCanvas.Create(GetCanvasWidth, GetCanvasHeight, FTextureMode);
end;

function TTyroMainWindow.GetCanvasWidth: Integer;
begin
  Result := Width - 2 * (BorderSize + Margin);
  if Result < 1 then
    Result := 1;
end;

function TTyroMainWindow.GetCanvasHeight: Integer;
begin
  Result := Height - 2 * (BorderSize + Margin);
  if Result < 1 then
    Result := 1;
end;

destructor TTyroMainWindow.Destroy;
begin
  FreeAndNil(FCanvasLock);
  inherited;
end;

function TTyroMainWindow.Terminated: Boolean;
begin
  Result := IsTerminated;
end;

procedure TTyroMainWindow.Load;
begin
end;

procedure TTyroMainWindow.Start;
begin
end;

procedure TTyroMainWindow.SetFPS(FPS: Integer);
begin
  FFPS := FPS;
  SetTargetFPS(FPS);
end;

procedure TTyroMainWindow.ShowWindow;
begin
  ShowWindow(cDefaultWindowWidth, cDefaultWindowHeight);
end;

procedure TTyroMainWindow.Shutdown;
begin

end;

procedure TTyroMainWindow.PrepareDraw;
begin

end;

procedure TTyroMainWindow.Draw;
begin
end;

procedure TTyroMainWindow.Run;
var
  tw: Integer;
begin
  Init;

  if not Visible and (moWindow in Options) then
  begin
    ShowWindow(cDefaultWindowWidth, cDefaultWindowHeight);
    SetFPS(FramePerSeconds);
  end;

  Resources.Load;

  Load;
  Start;
  if FPS = 0 then
    SetFPS(FramePerSeconds);
  repeat
    try
      CheckSynchronize;
      if WindowShouldClose() then
      begin
        Shutdown;
        Terminate;
      end
      else
      begin
        if Visible then
        begin
          if RayLib.IsWindowResized() then
            Resize(RayLib.GetScreenWidth(), RayLib.GetScreenHeight());
          PrepareDraw;
          RayLib.BeginDrawing();
          if moOpaque in Options then
            RayLib.ClearBackground(BackColor);

          try
            Camera2D.Target := Vector2Of(0, 0);
            Camera2D.Offset := Vector2Of(Margin, Margin);
            Camera2D.Zoom := 1;
            Camera2D.Rotation := 0;

            Canvas.BeginDraw;
            BeginMode2D(Camera2D);
            Draw;
            EndMode2D();
            Canvas.EndDraw;
            Canvas.PostDraw;

            Paint;

            if moShowFPS in Options then
            begin
              tw := RayLib.MeasureText('9999 FPS', 20) + 5;
              RayLib.DrawFPS(RayLib.GetScreenWidth - tw, 5);
            end;
          finally
            RayLib.EndDrawing();
          end;
        end;
      end;
       Update;
       RayUpdates.Update;
       ProcessInput;
    finally
    end;
  until Terminated;
  Unload;
  if Visible then
    RayLib.CloseWindow();
end;

procedure TTyroMainWindow.ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean);
begin
  FTextureMode := ATextureMode;
  if Visible then
  begin
    SetBoundsRect(Rect(0, 0, AWidth, AHeight));
    SetWindowSize(AWidth, AHeight);
  end
  else
  begin
    //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
    SetConfigFlags([FLAG_WINDOW_HIDDEN, FLAG_WINDOW_RESIZABLE]);
    SetBoundsRect(Rect(0, 0, AWidth, AHeight));
    InitWindow(AWidth, AHeight, PUTF8Char(Title));
    ClearWindowState([FLAG_WINDOW_HIDDEN]);
    ShowCursor();
    PrepareCanvas;
  end;
  Visible := True;
  Resize(AWidth, AHeight);
end;

procedure TTyroMainWindow.Resize(AWidth, AHeight: Integer);
begin
  if (AWidth <= 0) or (AHeight <= 0) then Exit;
  if (FWindowRect.Width = AWidth) and (FWindowRect.Height = AHeight)
     and (FCanvas <> nil) and (FCanvas.Width = GetCanvasWidth) and (FCanvas.Height = GetCanvasHeight) then
    Exit;
  SetWindowRect(Rect(0, 0, AWidth, AHeight));
  if FCanvas <> nil then
    FCanvas.Resize(GetCanvasWidth, GetCanvasHeight);
end;

procedure TTyroMainWindow.HideWindow;
begin
  if Visible then
  begin
    Visible := False;
    CloseWindow;
  end;
end;

procedure TTyroMainWindow.Init;
begin

end;

procedure TTyroMainWindow.Terminate;
begin
  IsTerminated := True;
end;

procedure TTyroMainWindow.Unload;
begin

end;

procedure TTyroMainWindow.Update;
begin

end;

{ TTyroLayout }

procedure TTyroLayout.SetAlign(AValue: TAlign);
begin
  if FAlign=AValue then Exit;
  FAlign := AValue;
  Realign;
end;

procedure TTyroLayout.SetBorder(AValue: TBorder);
begin
  if FBorder=AValue then Exit;
  FBorder:=AValue;
  Resize;
end;

function TTyroLayout.GetHeight: Integer;
begin
  Result := BoundsRect.Height;
end;

function TTyroLayout.GetWidth: Integer;
begin
  Result := BoundsRect.Width;
end;

procedure TTyroLayout.SetHeight(AValue: Integer);
begin
  FBoundsRect.Height := AValue;
  Resize;
end;

procedure TTyroLayout.SetParent(AValue: TTyroLayout);
begin
  if FParent = AValue then
    Exit;
  if FParent <> nil then
    FParent.Controls.Extract(Self);
  FParent :=AValue;
  if FParent <> nil then
    FParent.AddControl(Self);
end;

procedure TTyroLayout.AddControl(AControl: TTyroLayout);
begin
  Controls.Add(AControl);
  AControl.FParent := Self;
end;

procedure TTyroLayout.PaintWindow(ACanvas: TTyroCanvas);
begin
end;

function TTyroLayout.BorderSize: Integer;
begin
  case Border of
    brdNone: Result := 0;
    brdThin: Result := 1;
    brdThick: Result := 2;
    brdSizable: Result := 4;
  end;
end;

constructor TTyroLayout.Create(AParent: TTyroLayout);
begin
  inherited Create;
  FState := FState + [csCreating];
  FControls := TTyroControls.Create(True);
  SetParent(AParent);
end;

destructor TTyroLayout.Destroy;
var
  aControl: TTyroLayout;
begin
  aControl := Controls.Last;
  while aControl <> nil do
  begin
    aControl.FParent := nil;
    Controls.Extract(aControl);
    aControl.Free;
    aControl := Controls.Last;
  end;
  FreeAndNil(FControls);
  inherited;
end;

procedure TTyroLayout.AfterConstruction;
begin
  inherited;
  FState := FState + [csCreating];
end;

procedure TTyroLayout.Realign;
begin
  if (Align <> alNone) and (Parent <> nil) then
    Parent.AlignControls
  else
    FWindowRect := FBoundsRect;
end;

procedure TTyroLayout.AlignControls;
var
  aControl: TTyroLayout;
  aRect: TRect;
begin
  //* Align child controls within this parent's WindowRect.
  //* Each child's WindowRect is updated to position it according
  //* to its Align property.
  if FControls = nil then
    Exit;

  aRect := FWindowRect;

  for aControl in FControls do
  begin
    if aControl.Align <> alNone then
      aControl.FState := aControl.FState + [lsAligning];
    try
      case aControl.Align of
        alLeft:
        begin
          aControl.SetWindowRect(Rect(aRect.Left, aRect.Top, aRect.Left + aControl.WindowRect.Width, aRect.Bottom));
          aRect.Left := aRect.Left + aControl.WindowRect.Width;
        end;
        alTop:
        begin
          aControl.SetWindowRect(Rect(aRect.Left, aRect.Top, aRect.Right, aRect.Top + aControl.WindowRect.Height));
          aRect.Top := aRect.Top + aControl.WindowRect.Height;
        end;
        alRight:
        begin
          aControl.SetWindowRect(Rect(aRect.Right - aControl.WindowRect.Width, aRect.Top, aRect.Right, aRect.Bottom));
          aRect.Right := aRect.Right - aControl.WindowRect.Width;
        end;
        alBottom:
        begin
          aControl.SetWindowRect(Rect(aRect.Left, aRect.Bottom - aControl.WindowRect.Height, aRect.Right, aRect.Bottom));
          aRect.Bottom := aRect.Bottom - aControl.WindowRect.Height;
        end;
        alClient:
        begin
          aControl.SetWindowRect(aRect);
        end;
        alNone:
          aControl.SetWindowRect(aControl.BoundsRect);
      end;
    finally
      if aControl.Align <> alNone then
        aControl.FState := aControl.FState - [lsAligning];
    end;
  end;
end;

{ TTyroWindow }

constructor TTyroPanel.Create(AParent: TTyroLayout);
begin
  inherited;
  BoundsRect := Rect(0 ,0 , 100, 100);
end;

procedure TTyroPanel.DoPaint(ACanvas: TTyroCanvas);
begin
  inherited;
  ACanvas.DrawRectangle(ClientRect, ACanvas.PenColor, False);
end;

{ TTyroButton }

constructor TTyroButton.Create(AParent: TTyroLayout);
begin
  inherited;
  BoundsRect := Rect(0, 0 , 100, 32);
end;

procedure TTyroButton.SetCaption(AValue: utf8string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

function TTyroButton.IsMouseOver: Boolean;
var
  mp: TVector2;
begin
  Result := False;
  if (WindowRect.Width <= 0) or (WindowRect.Height <= 0) then
    Exit;
  mp := TVector2(RayLib.GetMousePosition);
  Result := (mp.X >= WindowRect.Left) and (mp.X <= WindowRect.Right) and
            (mp.Y >= WindowRect.Top) and (mp.Y <= WindowRect.Bottom);
end;

procedure TTyroButton.CheckState;
begin
  FHover := IsMouseOver;
  FDown := FHover and RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT);
  FClicked := FWasDown and FHover and (not FDown);
  FWasDown := FDown;
end;

procedure TTyroButton.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRectangle;
  body, border, foreground: TColor;
  roundness, lineThick: Single;
  tx, ty, tw, th: Single;
  v: TVector2;
begin
  inherited;
  CheckState;

  r := RectangleOf(ClientRect);
  if (r.Width <= 0) or (r.Height <= 0) then
    Exit;

  roundness := 8;
  lineThick := 2;
  if lineThick < 1 then
    lineThick := 1
  else if lineThick > 4 then
    lineThick := 4;

  if FDown then
    body := clGray
  else if FHover then
    body := clSkyBlue
  else
    body := clLightgray;

  border := clDarkGray;
  foreground := clBlack;
  r.X := r.X + 2;
  r.Y := r.Y + 2;
  r.Width := r.Width - 4;
  r.Height := r.Height - 4;

  RayLib.DrawRectangleRounded(r, 0.5, 15, body);
  RayLib.DrawRectangleRoundedLinesEx(r, 0.5, 15, 0.4, border);

  th := Resources.Font.Height;
  tw := RayLib.MeasureTextEx(Resources.Font.Data, PUTF8Char(FCaption), Resources.Font.Height, 0).x;
  tx := r.X + (r.Width - tw) / 2;
  ty := r.Y + (r.Height - th) / 2;
  if FDown then
    ty := ty + 0.1;
  ACanvas.DrawText(tx, ty, FCaption, foreground);
end;

{ TTyroControl }

procedure TTyroLayout.SetBoundsRect(AValue: TRect);
begin
  if FBoundsRect=AValue then
    Exit;
  FBoundsRect := AValue;
  Resize;
end;

procedure TTyroLayout.SetWindowRect(AValue: TRect);
begin
  FWindowRect := AValue;
  Resize;
end;

procedure TTyroLayout.Resize;
begin
  if not(lsAligning in State) then
    Realign;
  AlignControls;
  Resized;
end;

procedure TTyroLayout.Resized;
begin
end;

function GetCursorForSides(Sides: TTyroResizeSides): TMouseCursor;
begin
  if ((rsLeft in Sides) and (rsTop in Sides)) or
     ((rsRight in Sides) and (rsBottom in Sides)) then
    Result := MOUSE_CURSOR_RESIZE_NWSE
  else if ((rsRight in Sides) and (rsTop in Sides)) or
          ((rsLeft in Sides) and (rsBottom in Sides)) then
    Result := MOUSE_CURSOR_RESIZE_NESW
  else if (rsLeft in Sides) or (rsRight in Sides) then
    Result := MOUSE_CURSOR_RESIZE_EW
  else if (rsTop in Sides) or (rsBottom in Sides) then
    Result := MOUSE_CURSOR_RESIZE_NS
  else
    Result := MOUSE_CURSOR_DEFAULT;
end;

function TTyroControl.GetFocused: Boolean;
begin
  Result := (Window <> nil) and (Window.Focused = Self);
end;

procedure TTyroControl.SetBackColor(AValue: TColor);
begin
  if FBackColor=AValue then Exit;
  FBackColor:=AValue;
end;

procedure TTyroControl.SetFocused(AValue: Boolean);
begin
  if Window <> nil then
    Window.Focused := Self;
end;

procedure TTyroLayout.SetMargin(AValue: Integer);
begin
  if FMargin=AValue then Exit;
  FMargin:=AValue;
end;

procedure TTyroLayout.SetWidth(AValue: Integer);
begin
  FBoundsRect.Width := AValue;
  Resize;
end;

procedure TTyroControl.SetVisible(AValue: Boolean);
begin
  if FVisible =AValue then Exit;
  FVisible := AValue;
  Invalidate;
end;

procedure TTyroControl.SetWindow(AValue: TTyroWindow);
begin
  if FWindow =AValue then Exit;
  FWindow :=AValue;
end;

function TTyroControl.GetClientRect: TRect;
var
  lWidth, lHeight: Integer;
begin
  //* ClientRect is relative to this control's WindowRect origin.
  //* It is inset by BorderSize + Margin on each side.
  lWidth := WindowRect.Width;
  lHeight := WindowRect.Height;

  Result.Left := BorderSize + Margin;
  Result.Top := BorderSize + Margin;
  Result.Right := lWidth - BorderSize - Margin;
  Result.Bottom := lHeight - BorderSize - Margin;

  //* Guard against negative dimensions when the control is too small
  if Result.Right < Result.Left then
    Result.Right := Result.Left;
  if Result.Bottom < Result.Top then
    Result.Bottom := Result.Top;
end;

function TTyroControl.GetClientTop: Integer;
begin
  Result := ClientRect.Top;
end;

function TTyroControl.GetClientWidth: Integer;
begin
  Result := ClientRect.Width;
end;

function TTyroControl.GetClientHeight: Integer;
begin
  Result := ClientRect.Height;
end;

function TTyroControl.GetClientLeft: Integer;
begin
  Result := ClientRect.Left;
end;

procedure TTyroControl.ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
begin

end;

procedure TTyroControl.SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
begin

end;

procedure TTyroControl.SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);
begin

end;

procedure TTyroControl.Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
begin

end;

procedure TTyroControl.DoBorder(ACanvas: TTyroCanvas);
begin

end;

procedure TTyroControl.Invalidate;
begin

end;

procedure TTyroControl.PaintWindow(ACanvas: TTyroCanvas);
begin
  if Visible then
  begin
    if PrepareCanvas then
    begin
      //* Paint the control content into its own transparent texture buffer,
      //* then draw (blit) that buffer on top of the window canvas.
      Canvas.BeginDraw;
      Canvas.ClearBackground(clBlank);
      try
        PaintBorder(Canvas);
        if csClip in Style then
          RayLib.BeginScissorMode(ClientLeft, ClientTop, ClientWidth, ClientHeight);
        Canvas.SetOrigin(ClientLeft, ClientTop);
        try
          DoPaintBackground(Canvas);
          DoPaint(Canvas)
        finally
          Canvas.ResetOrigin;
          if csClip in Style then
            RayLib.EndScissorMode();
        end;
      finally
        Canvas.EndDraw;
      end;
      Canvas.PostDraw(WindowRect.Left, WindowRect.Top);
    end
    else
    begin
      //* No own texture could be created: paint directly as a fallback.
      ACanvas.SetOrigin(WindowRect.Left, WindowRect.Top);
      PaintBorder(ACanvas);
      ACanvas.ResetOrigin;
      if csClip in Style then
        RayLib.BeginScissorMode(WindowRect.Left + ClientLeft, WindowRect.Top + ClientTop, ClientWidth, ClientHeight);
      ACanvas.SetOrigin(WindowRect.Left + ClientLeft, WindowRect.Top + ClientTop);
      try
        DoPaintBackground(ACanvas);
        DoPaint(ACanvas)
      finally
        ACanvas.ResetOrigin;
        if csClip in Style then
          RayLib.EndScissorMode();
      end;
    end;
  end;
end;

function TTyroControl.PrepareCanvas: Boolean;
var
  w, h: Integer;
begin
  w := WindowRect.Width;
  h := WindowRect.Height;
  Result := (w >= 1) and (h >= 1);
  if not Result then
    Exit;
  if (FCanvas = nil) or (FCanvas.Width <> w) or (FCanvas.Height <> h) then
  begin
    FreeAndNil(FCanvas);
    FCanvas := TTyroTextureCanvas.Create(w, h, True);
  end;
end;

procedure TTyroControl.SetCanvas(AValue: TTyroCanvas);
begin
  if FCanvas = AValue then
    Exit;
  FreeAndNil(FCanvas);
  FCanvas := AValue;
end;

procedure TTyroControl.FocusChanged;
begin
end;

procedure TTyroControl.Show;
begin
  Visible := True;
end;

procedure TTyroControl.Hide;
begin
  Visible := False;
end;

procedure TTyroControl.BringToFront;
var
  I: Integer;
begin
  if (FParent = nil) or (FAlign <> alNone) then
    Exit;
  I := FParent.FControls.IndexOf(Self);
  if (I >= 0) and (I < FParent.FControls.Count - 1) then
  begin
    FParent.FControls.Move(I, FParent.FControls.Count - 1);
    Invalidate;
  end;
end;

procedure TTyroControl.DoPaintBackground(ACanvas: TTyroCanvas);
begin
  if csOpaque in Style then
    ACanvas.DrawRectangle(0, 0, ClientWidth, ClientHeight, BackColor, True);
end;

procedure TTyroControl.DoPaint(ACanvas: TTyroCanvas);
begin

end;

procedure TTyroControl.KeyPress(var Key: TUTF8Char);
begin

end;

procedure TTyroControl.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
begin

end;

procedure TTyroControl.KeyUp(var Key: TKeyboardKey; Shift: TShiftState);
begin

end;

procedure TTyroControl.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer);
var
  sides: TTyroResizeSides;
begin
  if (Button = mbLeft) and (Border = brdSizable) then
  begin
    sides := GetResizeSides(x, y);
    if sides <> [] then
    begin
      FResizing := True;
      FResizeSides := sides;
      if Align = alNone then
        FResizeStartRect := BoundsRect
      else
        FResizeStartRect := WindowRect;
      FResizeStartMouse := TVector2(RayLib.GetMousePosition);
    end;
  end;
end;

procedure TTyroControl.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin
  if FResizing then
  begin
    FResizing := False;
    FResizeSides := [];
    RayLib.SetMouseCursor(Ord(MOUSE_CURSOR_DEFAULT));
  end;
end;

procedure TTyroControl.MouseMove(Shift: TShiftState; x, y: integer);
var
  sides: TTyroResizeSides;
begin
  FLastMouseX := x;
  FLastMouseY := y;
  if FResizing then
  begin
    if ssLeft in Shift then
      ApplyResize
    else
    begin
      FResizing := False;
      FResizeSides := [];
      RayLib.SetMouseCursor(Ord(MOUSE_CURSOR_DEFAULT));
    end;
    RayLib.SetMouseCursor(Ord(GetCursorForSides(FResizeSides)));
  end
  else if Border = brdSizable then
  begin
    sides := GetResizeSides(x, y);
    if sides <> [] then
      RayLib.SetMouseCursor(Ord(GetCursorForSides(sides)));
  end;
end;

function TTyroControl.GetResizeSides(X, Y: Integer): TTyroResizeSides;
var
  bs, w, h: Integer;
begin
  Result := [];
  if Border <> brdSizable then
    Exit;
  w := WindowRect.Width;
  h := WindowRect.Height;
  if (w <= 0) or (h <= 0) then
    Exit;
  bs := BorderSize;

  case Align of
    alLeft:
      if X >= w - bs then
        Include(Result, rsRight);
    alRight:
      if X < bs then
        Include(Result, rsLeft);
    alTop:
      if Y >= h - bs then
        Include(Result, rsBottom);
    alBottom:
      if Y < bs then
        Include(Result, rsTop);
    alClient:
      ; // no resizable edges
  else // alNone
    if X < bs then
      Include(Result, rsLeft)
    else if X >= w - bs then
      Include(Result, rsRight);
    if Y < bs then
      Include(Result, rsTop)
    else if Y >= h - bs then
      Include(Result, rsBottom);
  end;
end;

procedure TTyroControl.ApplyResize;
var
  mp: TVector2;
  dx, dy: Integer;
  r: TRect;
  minSize: Integer;
begin
  minSize := cMinResizeSize;
  mp := TVector2(RayLib.GetMousePosition);
  dx := Round(mp.X) - Round(FResizeStartMouse.X);
  dy := Round(mp.Y) - Round(FResizeStartMouse.Y);
  r := FResizeStartRect;

  if rsRight in FResizeSides then
    r.Right := r.Right + dx;
  if rsLeft in FResizeSides then
    r.Left := r.Left + dx;
  if rsBottom in FResizeSides then
    r.Bottom := r.Bottom + dy;
  if rsTop in FResizeSides then
    r.Top := r.Top + dy;

  if (r.Right - r.Left) < minSize then
  begin
    if rsRight in FResizeSides then
      r.Right := r.Left + minSize
    else if rsLeft in FResizeSides then
      r.Left := r.Right - minSize;
  end;
  if (r.Bottom - r.Top) < minSize then
  begin
    if rsBottom in FResizeSides then
      r.Bottom := r.Top + minSize
    else if rsTop in FResizeSides then
      r.Top := r.Bottom - minSize;
  end;

  if Align = alNone then
    SetBoundsRect(r)
  else
    SetWindowRect(r);
  Invalidate;
end;

procedure TTyroControl.PaintBorder(ACanvas: TTyroCanvas);
var
  bs, w, h: Integer;
  baseColor, highlightColor: TColor;
  activeSides: TTyroResizeSides;
begin
  if Border = brdNone then
    Exit;
  bs := BorderSize;
  w := ClientRect.Width;
  h := ClientRect.Height;
  if (w <= 0) or (h <= 0) then
    Exit;

  baseColor := clDarkGray;

  RayLib.DrawRectangleLinesEx(RectangleOf(ClientRect.Left-bs, ClientRect.Top-bs, ClientRect.Width+bs*2, ClientRect.Height+bs*2), 2, clRed);
  //ACanvas.FillRectangle(0, h - bs, w, bs, baseColor);
  //ACanvas.FillRectangle(0, 0, bs, h, baseColor);
  //ACanvas.FillRectangle(w - bs, 0, bs, h, baseColor);
  exit;
  if Border = brdSizable then
  begin
    if FResizing then
      activeSides := FResizeSides
    else if (FLastMouseX >= 0) and (FLastMouseX < w) and
            (FLastMouseY >= 0) and (FLastMouseY < h) then
      activeSides := GetResizeSides(FLastMouseX, FLastMouseY)
    else
      activeSides := [];

    if activeSides <> [] then
    begin
      highlightColor := clLightgray;
      if rsRight in activeSides then
        ACanvas.FillRectangle(w - bs, 0, 1, h, highlightColor);
      if rsLeft in activeSides then
        ACanvas.FillRectangle(bs - 1, 0, 1, h, highlightColor);
      if rsBottom in activeSides then
        ACanvas.FillRectangle(0, h - bs, w, 1, highlightColor);
      if rsTop in activeSides then
        ACanvas.FillRectangle(0, bs - 1, w, 1, highlightColor);
    end;
  end;
end;

procedure TTyroControl.Created;
begin
end;

constructor TTyroControl.Create(AParent: TTyroLayout);
begin
  inherited;
  FState := FState + [csCreating];
  FParent := AParent;
  if (Parent is TTyroWindow) then
    FWindow := (Parent as TTyroWindow);
  FVisible := True;
  Created;
  FState := FState - [csCreating] + [csCreated];
end;

destructor TTyroControl.Destroy;
begin
  FState := FState - [csCreating, csCreated] + [csDestroying];
  if Parent <> nil then
    Parent := nil;
  FreeAndNil(FCanvas);
  inherited;
end;

{ TTyroWindow }

procedure TTyroWindow.SetTitle(AValue: utf8string);
begin
  if FTitle =AValue then Exit;
  FTitle :=AValue;
end;

procedure TTyroWindow.PrepareCanvas;
begin
  if FCanvas = nil then
    FCanvas := CreateCanvas;
end;

constructor TTyroWindow.Create(AParent: TTyroLayout; AWidth, AHeight: Integer);
begin
  Create(AParent);
  BoundsRect := Rect(0, 0, AWidth, AHeight);
end;

procedure TTyroWindow.SetFocused(AValue: TTyroControl);
begin
  if FFocused =AValue then
    Exit;
  if FFocused <> nil then
    FFocused.FocusChanged;
  FFocused :=AValue;
  if FFocused <> nil then
    FFocused.FocusChanged;
end;

procedure TTyroMainWindow.ProcessInput;
var
  Shift: TShiftState;
  Key: TKeyboardKey;
  ch: Integer;
  aChar: TUTF8Char;
  FocusedControl: TTyroControl;
  mp: TVector2;
  mx, my, x, y: Integer;
  i: Integer;
  aControl: TTyroControl;
begin
  // Build shift state from RayLib key queries
  Shift := [];
  if RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT) then
    Shift := Shift + [ssShift];
  if RayLib.IsKeyDown(KEY_LEFT_CONTROL) or RayLib.IsKeyDown(KEY_RIGHT_CONTROL) then
    Shift := Shift + [ssCtrl];
  if RayLib.IsKeyDown(KEY_LEFT_ALT) or RayLib.IsKeyDown(KEY_RIGHT_ALT) then
    Shift := Shift + [ssAlt];
  if RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    Shift := Shift + [ssLeft];
  if RayLib.IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
    Shift := Shift + [ssRight];

  //* Mouse routing: move is sent to the control under the cursor every frame;
  //* once a left press lands inside a control the pointer is captured to it
  //* (keeps tracking the cursor while dragging a sizable border) until release.
  RayLib.SetMouseCursor(Ord(MOUSE_CURSOR_DEFAULT));
  mp := TVector2(RayLib.GetMousePosition);
  mx := Round(mp.X);
  my := Round(mp.Y);

  if FControlCapture <> nil then
  begin
    x := mx - FControlCapture.WindowRect.Left;
    y := my - FControlCapture.WindowRect.Top;
    FControlCapture.MouseMove(Shift, x, y);
    if not (ssLeft in Shift) then
    begin
      x := mx - FControlCapture.WindowRect.Left;
      y := my - FControlCapture.WindowRect.Top;
      FControlCapture.MouseUp(mbLeft, Shift, x, y);
      FControlCapture := nil;
    end;
  end
  else
  begin
    for i := Controls.Count - 1 downto 0 do
    begin
      if Controls[i] is TTyroControl then
      begin
        aControl := TTyroControl(Controls[i]);
        if aControl.Visible and
           (mx >= aControl.WindowRect.Left) and (mx < aControl.WindowRect.Right) and
           (my >= aControl.WindowRect.Top) and (my < aControl.WindowRect.Bottom) then
        begin
          x := mx - aControl.WindowRect.Left;
          y := my - aControl.WindowRect.Top;
          aControl.MouseMove(Shift, x, y);
          if RayLib.IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
          begin
            aControl.MouseDown(mbLeft, Shift, x, y);
            FControlCapture := aControl;
          end;
          Break;
        end;
      end;
    end;
  end;

  if FFocused = nil then
    Exit;

  // Process key codes (function keys, arrows, etc.)
  Key := RayLib.GetKeyPressed;
  while Key <> KEY_NULL do
  begin
    case Key of
      KEY_LEFT_SHIFT, KEY_RIGHT_SHIFT, KEY_LEFT_CONTROL, KEY_RIGHT_CONTROL,
      KEY_LEFT_ALT, KEY_RIGHT_ALT:
        begin
          // Shift keys themselves - skip to avoid sending as regular key
        end;
    else
      FocusedControl := FFocused;
      if Assigned(FocusedControl) then
        FocusedControl.KeyDown(Key, Shift);
    end;
    Key := RayLib.GetKeyPressed;
  end;

  // Process character input (printable text, respecting modifiers for shortcuts)
  ch := RayLib.GetCharPressed;
  while ch > 0 do
  begin
    if ch >= 32 then
    begin
      // If Ctrl is held, treat as key shortcut (e.g. Ctrl+V) not text
      if not (ssCtrl in Shift) then
      begin
        FocusedControl := FFocused;
        if Assigned(FocusedControl) then
        begin
          aChar := CodePointToUTF8(ch);
          FocusedControl.KeyPress(aChar);
        end;
      end;
    end;
    ch := RayLib.GetCharPressed;
  end;
end;

procedure TTyroWindow.SetCanvas(AValue: TTyroCanvas);
begin
  if FCanvas =AValue then Exit;
  FCanvas :=AValue;
end;

constructor TTyroWindow.Create(AParent: TTyroLayout);
begin
  inherited;
end;

destructor TTyroWindow.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TTyroWindow.Paint;
var
  aControl: TTyroLayout;
begin
  if Visible then
  begin
    try
      for aControl in Controls do
      begin
        aControl.PaintWindow(Canvas);
      end;
    finally
    end;
  end;
end;

initialization
  Randomize;
finalization
  FreeAndNil(Main);
end.

