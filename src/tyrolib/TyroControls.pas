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
  cMarginSize = 32;

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
    csClip
  );
  TTyroControlStyles = set of TTyroControlStyle;

  TTyroControlState = (csCreating, csCreated, csDestroying);
  TTyroControlStates = set of TTyroControlState;

  TTyroLayout = class;
  TTyroControl = class;
  TTyroCustomWindow = class;

  TTyroControls = class(TmnObjectList<TTyroLayout>)
  public
  end;

  TAlign = (alNone, alLeft, alTop, alRight, alBottom, alClient);

  { TTyroLayout }

  TTyroLayout = class abstract(TObject)
  private
    FAlign: TAlign;
    FControls: TTyroControls;
    FParent: TTyroLayout;
    FBoundsRect: TRect;
    FWindowRect: TRect;
    procedure SetAlign(AValue: TAlign);
    procedure SetParent(AValue: TTyroLayout);
  protected
    procedure SetBoundsRect(AValue: TRect);
    procedure SetWindowRect(AValue: TRect);
    function GetWindowHeight: Integer;
    procedure SetWindowHeight(AValue: Integer);
    function GetWindowWidth: Integer;
    procedure SetWindowWidth(AValue: Integer);
    procedure SetWindowLeft(AValue: Integer);
    procedure SetWindowTop(AValue: Integer);
    procedure SetWindowBounds(Left, Top, Width, Height: Integer); virtual;

    procedure Resize;
    procedure Resized; virtual;

    procedure AddControl(AControl: TTyroLayout);
    procedure PaintWindow(ACanvas: TTyroCanvas); virtual;
  public
    constructor Create(AParent: TTyroLayout); virtual;
    destructor Destroy; override;
    procedure Realign; virtual;
    procedure AlignControls; virtual;
    property Controls: TTyroControls read FControls;
    property Align: TAlign read FAlign write SetAlign;
    property Parent: TTyroLayout read FParent write SetParent;
    //Real bounds
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    //WindowRect is Virtual changed by RealignControls of parent used paint control
    property WindowRect: TRect read FWindowRect write SetWindowRect;
  end;

  { TTyroControl }

  TTyroControl = class abstract(TTyroLayout)
  private
    FBorderColor: TColor;
    FBorderSize: Integer;
    FMargin: Integer;
    FWindow: TTyroCustomWindow;
    FVisible: Boolean;
    function GetFocused: Boolean;
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderSize(AValue: Integer);
    procedure SetFocused(AValue: Boolean);
    procedure SetMargin(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWindow(AValue: TTyroCustomWindow);
    function GetClientLeft: Integer;
    function GetClientTop: Integer;
  protected
    State: TTyroControlStates;
    Style: TTyroControlStyles;
    function GetClientRect: TRect;
    function GetClientWidth: Integer;
    function GetClientHeight: Integer;

    procedure ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
    procedure SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
    procedure SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);
    procedure Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); virtual;

    procedure DoPaintBackground(ACanvas: TTyroCanvas); virtual;
    procedure DoPaint(ACanvas: TTyroCanvas); virtual;

    procedure Created; virtual;
    property Window: TTyroCustomWindow read FWindow write SetWindow;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure Invalidate; virtual;

    procedure PaintWindow(ACanvas: TTyroCanvas); override;

    procedure FocusChanged; virtual;
    procedure Show;
    procedure Hide;

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
    property Margin: Integer read FMargin write SetMargin;
    property BorderSize: Integer read FBorderSize write SetBorderSize;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TTyroPanel }

  TTyroPanel = class(TTyroControl)
  public
    constructor Create(AParent: TTyroLayout); override;
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  end;

  { TTyroTexture }

  { TTyroTextureControl }

  TTyroTextureControl = class(TTyroControl) //Own a texture
  private
    FCanvas: TTyroCanvas;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure PaintWindow(ACanvas: TTyroCanvas); override;
    procedure Resized; override;
    property Canvas: TTyroCanvas read FCanvas write FCanvas;
  end;

  { TTyroCustomWindow }

  TTyroCustomWindow = class abstract(TTyroLayout)
  private
    FCanvas: TTyroCanvas;
    FFocused: TTyroControl;
    FTitle: utf8string;
    procedure SetCanvas(AValue: TTyroCanvas);
    procedure SetFocused(AValue: TTyroControl);
    procedure SetTitle(AValue: utf8string);
  protected
    Margin: Integer;
    procedure PrepareCanvas; virtual;
    function CreateCanvas: TTyroCanvas; virtual; abstract;
  public
    Visible: Boolean;
    constructor Create(AParent: TTyroLayout); override; overload;
    constructor Create(AParent: TTyroLayout; AWidth, AHeight: Integer); overload; overload;
    destructor Destroy; override;
    procedure Paint;
    property Canvas: TTyroCanvas read FCanvas write SetCanvas;
    property Title: utf8string read FTitle write SetTitle;
    property Focused: TTyroControl read FFocused write SetFocused;
  end;

  TTyroWindow = class(TTyroCustomWindow)
  protected
    function CreateCanvas: TTyroCanvas; override;
  public
  end;

  TTyroMainOption = (moWindow, moOpaque, moShowFPS);
  TTyroMainOptions= set of TTyroMainOption;

  { TTyroMain }

  TTyroMain = class(TTyroCustomWindow)
  private
    FFPS: Integer;
    FOptions: TTyroMainOptions;
    FMarginSize: Integer;
    //FMarginColor: TColor;
    procedure SetMarginSize(const Value: Integer);
    //procedure SetMarginColor(const Value: TColor);
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    FTextureMode: Boolean;
    IsTerminated: Boolean;
    FCanvasLock: TCriticalSection;
    Camera2D: TCamera2D;
    function CreateCanvas: TTyroCanvas; override;
    procedure Terminate; virtual;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;

    //* TextureMode create texture with canvas
    procedure ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean = False); overload; virtual;
    procedure ShowWindow; overload;
    procedure SetFPS(FPS: Integer); virtual;
    procedure HideWindow; virtual;

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

    //property MarginColor: TColor read FMarginColor write SetMarginColor;
    property MarginSize: Integer read FMarginSize write SetMarginSize;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property CanvasLock: TCriticalSection read FCanvasLock;
    property Options: TTyroMainOptions read FOptions write FOptions;
    property FPS: Integer read FFPS write SetFPS;
  end;

const
  cDefaultWindowWidth = 640;
  cDefaultWindowHeight = 480;

var
  Main: TTyroMain = nil;

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

{ TTyroMain }

function Canvas: TTyroCanvas;
begin
  Result := Main.Canvas;
end;

constructor TTyroMain.Create(AParent: TTyroLayout);
begin
  inherited;
  FOptions := [moWindow, moOpaque];
  RayLibrary.Load;
  Resources := TTyroResources.Create;
  Resources.WorkSpace := ExtractFilePath(ParamStr(0));
  FCanvasLock := TCriticalSection.Create;
  MarginSize := cMarginSize;
  //MarginColor := clCornflowerBlue;
end;

function TTyroMain.CreateCanvas: TTyroCanvas;
begin
  Result := TTyroTextureCanvas.Create(Width, Height, FTextureMode);
end;

destructor TTyroMain.Destroy;
begin
  FreeAndNil(FCanvasLock);
  inherited;
end;

function TTyroMain.Terminated: Boolean;
begin
  Result := IsTerminated;
end;

procedure TTyroMain.Load;
begin
end;

procedure TTyroMain.Start;
begin
end;

{procedure TTyroMain.SetMarginColor(const Value: TColor);
begin
  FMarginColor := Value;
end;}

procedure TTyroMain.SetMarginSize(const Value: Integer);
begin
  FMarginSize := Value;
end;

procedure TTyroMain.SetFPS(FPS: Integer);
begin
  FFPS := FPS;
  SetTargetFPS(FPS);
end;

procedure TTyroMain.ShowWindow;
begin
  ShowWindow(cDefaultWindowWidth, cDefaultWindowHeight);
end;

procedure TTyroMain.Shutdown;
begin

end;

procedure TTyroMain.PrepareDraw;
begin

end;

procedure TTyroMain.Draw;
begin
end;

function TTyroMain.GetHeight: Integer;
begin
  Result := WindowRect.Height - MarginSize * 2;
end;

function TTyroMain.GetWidth: Integer;
begin
  Result := WindowRect.Width - MarginSize * 2;
end;

procedure TTyroMain.Run;
var
  tw: Integer;
begin
  Init;

  if not Visible and (moWindow in Options) then
  begin
    ShowWindow(cDefaultWindowWidth, cDefaultWindowHeight);
    SetFPS(cFramePerSeconds);
  end;

  Resources.Load;

  Load;
  Start;
  if FPS = 0 then
    SetFPS(cFramePerSeconds);
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
          PrepareDraw;
          RayLib.BeginDrawing();
          if moOpaque in Options then
            Canvas.Clear;

          try
            Camera2D.Target := Vector2Of(0, 0);
            Camera2D.Offset := Vector2Of(MarginSize, MarginSize);
            Camera2D.Zoom := 1;
            Camera2D.Rotation := 0;

            Paint;
            Canvas.BeginDraw;
            BeginMode2D(Camera2D);
            Draw;
            EndMode2D();
            Canvas.EndDraw;
            Canvas.PostDraw;

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

procedure TTyroMain.ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean);
begin
  FTextureMode := ATextureMode;
  if Visible then
  begin
    SetWindowBounds(0, 0, AWidth, AHeight);
    SetWindowSize(AWidth, AHeight);
  end
  else
  begin
    //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
    SetConfigFlags([FLAG_WINDOW_HIDDEN]);
    SetWindowBounds(0, 0, AWidth, AHeight);
    InitWindow(AWidth, AHeight, PUTF8Char(Title));
    ClearWindowState([FLAG_WINDOW_HIDDEN]);
    ShowCursor();
    PrepareCanvas;
  end;
  Visible := True;
end;

procedure TTyroMain.HideWindow;
begin
  if Visible then
  begin
    Visible := False;
    CloseWindow;
  end;
end;

procedure TTyroMain.Init;
begin

end;

procedure TTyroMain.Terminate;
begin
  IsTerminated := True;
end;

procedure TTyroMain.Unload;
begin

end;

procedure TTyroMain.Update;
begin

end;

{ TTyroTextureControl }

constructor TTyroTextureControl.Create(AParent: TTyroLayout);
begin
  inherited Create(AParent);
  FCanvas := TTyroTextureCanvas.Create(ClientWidth, ClientHeight, True);
end;

destructor TTyroTextureControl.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TTyroTextureControl.Invalidate;
begin
  inherited Invalidate;
//  Paint(Canvas);
end;

procedure TTyroTextureControl.PaintWindow(ACanvas: TTyroCanvas);
begin
  inherited;
  Canvas.PostDraw;
end;

procedure TTyroTextureControl.Resized;
begin
  Canvas.Width := ClientWidth;
  Canvas.Height := ClientHeight;
  inherited;
end;

{ TTyroLayout }

procedure TTyroLayout.SetAlign(AValue: TAlign);
begin
  if FAlign=AValue then Exit;
  FAlign := AValue;
  Realign;
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

constructor TTyroLayout.Create(AParent: TTyroLayout);
begin
  inherited Create;
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

procedure TTyroLayout.Realign;
begin
  if Parent <> nil then
    Parent.AlignControls
  else
  begin
    FWindowRect := FBoundsRect;
  end;
end;

procedure TTyroLayout.AlignControls;
begin
  //TODO align child controls by change the WindowRect
end;

{ TTyroCustomWindow }

constructor TTyroPanel.Create(AParent: TTyroLayout);
begin
  inherited;
  FWindowRect.Right := 100;
  FWindowRect.Bottom := 100;
end;

procedure TTyroPanel.DoPaint(ACanvas: TTyroCanvas);
begin
  inherited;
  ACanvas.DrawRectangle(ClientRect, ACanvas.PenColor, False);
end;

{ TTyroControl }

procedure TTyroLayout.SetBoundsRect(AValue: TRect);
begin
  if FBoundsRect=AValue then Exit;
  FBoundsRect:=AValue;
  Resize;
end;

procedure TTyroLayout.SetWindowRect(AValue: TRect);
begin
  if FWindowRect = AValue then Exit;
  FWindowRect := AValue;
  FBoundsRect := AValue;
  Resize;
end;

function TTyroLayout.GetWindowHeight: Integer;
begin
  Result := FWindowRect.Height;
end;

function TTyroLayout.GetWindowWidth: Integer;
begin
  Result := FWindowRect.Width;
end;

procedure TTyroLayout.SetWindowHeight(AValue: Integer);
begin
  FWindowRect.Height := AValue;
  Resize;
end;

procedure TTyroLayout.SetWindowLeft(AValue: Integer);
begin
  FWindowRect.Left := AValue;
  FBoundsRect.Left := AValue;
  Resize;
end;

procedure TTyroLayout.SetWindowTop(AValue: Integer);
begin
  FWindowRect.Top := AValue;
  Resize;
end;

procedure TTyroLayout.SetWindowWidth(AValue: Integer);
begin
  FWindowRect.Width := AValue;
  Resize;
end;

procedure TTyroLayout.SetWindowBounds(Left, Top, Width, Height: Integer);
begin
  FWindowRect := Rect(Left, Top, Left + Width, Top + Height);
  Resize;
end;

procedure TTyroLayout.Resize;
begin
  if Parent <> nil then
    Parent.AlignControls;
  Resized;
end;

procedure TTyroLayout.Resized;
begin
end;

function TTyroControl.GetFocused: Boolean;
begin
  Result := (Window <> nil) and (Window.Focused = Self);
end;

procedure TTyroControl.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
end;

procedure TTyroControl.SetBorderSize(AValue: Integer);
begin
  if FBorderSize=AValue then Exit;
  FBorderSize:=AValue;
end;

procedure TTyroControl.SetFocused(AValue: Boolean);
begin
  if Window <> nil then
    Window.Focused := Self;
end;

procedure TTyroControl.SetMargin(AValue: Integer);
begin
  if FMargin=AValue then Exit;
  FMargin:=AValue;
end;

procedure TTyroControl.SetVisible(AValue: Boolean);
begin
  if FVisible =AValue then Exit;
  FVisible := AValue;
  Invalidate;
end;

procedure TTyroControl.SetWindow(AValue: TTyroCustomWindow);
begin
  if FWindow =AValue then Exit;
  FWindow :=AValue;
end;

function TTyroControl.GetClientRect: TRect;
begin
  //TODM Here we calc based it on margine and BorderSize
  Result := Rect(0, 0, WindowRect.Width, WindowRect.Height);
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

procedure TTyroControl.Invalidate;
begin

end;

procedure TTyroControl.PaintWindow(ACanvas: TTyroCanvas);
begin
  if Visible then
  begin
    if csClip in Style then
      RayLib.BeginScissorMode(FWindowRect.Left + ClientLeft, FWindowRect.Top + ClientTop, ClientWidth, ClientHeight);
    ACanvas.SetOrigin(FWindowRect.Left + ClientLeft, FWindowRect.Top + ClientTop);
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

procedure TTyroControl.DoPaintBackground(ACanvas: TTyroCanvas);
begin

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
begin

end;

procedure TTyroControl.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin

end;

procedure TTyroControl.MouseMove(Shift: TShiftState; x, y: integer);
begin

end;

procedure TTyroControl.Created;
begin
end;

constructor TTyroControl.Create(AParent: TTyroLayout);
begin
  inherited;
  State := State + [csCreating];
  FParent := AParent;
  if (Parent is TTyroCustomWindow) then
    FWindow := (Parent as TTyroCustomWindow);
  FVisible := True;
  Created;
  State := State - [csCreating] + [csCreated];
end;

destructor TTyroControl.Destroy;
begin
  State := State - [csCreating, csCreated] + [csDestroying];
  if Parent <> nil then
    Parent := nil;
  inherited;
end;

{ TTyroCustomWindow }

procedure TTyroCustomWindow.SetTitle(AValue: utf8string);
begin
  if FTitle =AValue then Exit;
  FTitle :=AValue;
end;

procedure TTyroCustomWindow.PrepareCanvas;
begin
  if FCanvas = nil then
    FCanvas := CreateCanvas;
end;

constructor TTyroCustomWindow.Create(AParent: TTyroLayout; AWidth, AHeight: Integer);
begin
  Create(AParent);
  FWindowRect := Rect(0, 0, AWidth, AHeight);
end;

procedure TTyroCustomWindow.SetFocused(AValue: TTyroControl);
begin
  if FFocused =AValue then Exit;
  if FFocused <> nil then
    FFocused.FocusChanged;
  FFocused :=AValue;
  if FFocused <> nil then
    FFocused.FocusChanged;
end;

procedure TTyroMain.ProcessInput;
var
  Shift: TShiftState;
  Key: TKeyboardKey;
  ch: Integer;
  aChar: TUTF8Char;
  FocusedControl: TTyroControl;
begin
  if FFocused = nil then
    Exit;

  // Build shift state from RayLib key queries
  Shift := [];
  if RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT) then
    Shift := Shift + [ssShift];
  if RayLib.IsKeyDown(KEY_LEFT_CONTROL) or RayLib.IsKeyDown(KEY_RIGHT_CONTROL) then
    Shift := Shift + [ssCtrl];
  if RayLib.IsKeyDown(KEY_LEFT_ALT) or RayLib.IsKeyDown(KEY_RIGHT_ALT) then
    Shift := Shift + [ssAlt];

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

procedure TTyroCustomWindow.SetCanvas(AValue: TTyroCanvas);
begin
  if FCanvas =AValue then Exit;
  FCanvas :=AValue;
end;

constructor TTyroCustomWindow.Create(AParent: TTyroLayout);
begin
  inherited;
end;

destructor TTyroCustomWindow.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TTyroCustomWindow.Paint;
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

{ TTyroWindow }

function TTyroWindow.CreateCanvas: TTyroCanvas;
begin
  Result := TTyroTextureCanvas.Create(WindowRect.Width, WindowRect.Height, True);
end;

initialization
  Randomize;
finalization
  FreeAndNil(Main);
end.

