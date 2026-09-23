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
  RayLib, RayClasses,
  TyroClasses;

const
  cMinResizeSize = 16;
  //* Thin scrollbars painted on the side of a control when csHScroll/csVScroll
  //* are in Style.
  cScrollSize = 7;        //* scrollbar thickness in pixels
  cScrollMinThumb = 8;    //* minimum thumb length in pixels

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
    csOpaque,
    csFocus, //Can focus
    csHScroll,
    csVScroll
  );
  TTyroControlStyles = set of TTyroControlStyle;

  { state of one scrollbar (range, page size and current position) }
  TTyroScrollInfo = record
    Min: Integer;
    Max: Integer;
    Page: Integer;
    Pos: Integer;
    Visible: Boolean;
  end;

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
    FVisible: Boolean;
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
    procedure SetVisible(AValue: Boolean); virtual;
    procedure SetBoundsRect(AValue: TRect);
    procedure SetWindowRect(AValue: TRect);

    procedure Resize;
    procedure Resized; virtual;

    procedure AddControl(AControl: TTyroLayout);
    procedure PaintWindow(ACanvas: TTyroCanvas); virtual;
    function BorderSize: Integer;
    function BorderRect: TRect;
  public
    constructor Create(AParent: TTyroLayout); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Realign; virtual;
    procedure AlignControls; virtual;
    property Controls: TTyroControls read FControls;
    procedure Update; virtual;
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
    property Visible: Boolean read FVisible write SetVisible;
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
    FResizing: Boolean;
    FResizeSides: TTyroResizeSides;
    FResizeStartRect: TRect;
    FResizeStartMouse: TVector2;
    FLastMouseX: Integer;
    FLastMouseY: Integer;
    //* Mouse-over/press/click state, tracked by CheckState every painted frame
    //* (buttons, checkboxes...). Read-only through Hover/Down/Clicked.
    FHover: Boolean;
    FDown: Boolean;
    FClicked: Boolean;
    FWasDown: Boolean;
    FHScroll: TTyroScrollInfo;
    FVScroll: TTyroScrollInfo;
    FHScrollHover: Boolean;
    FVScrollHover: Boolean;
    FHDrag: Boolean;
    FVDrag: Boolean;
    FHDragOfs: Integer;
    FVDragOfs: Integer;
    function GetFocused: Boolean;
    procedure SetBackColor(AValue: TColor);
    procedure SetWindow(AValue: TTyroWindow);
    procedure SetCanvas(AValue: TTyroCanvas);
    procedure SetFocused(AValue: Boolean);
  protected
    Style: TTyroControlStyles;
    function GetClientRect: TRect;
    //* Mouse-over/press/click state used by interactive controls. CheckState is
    //* called from DoPaint; Hover/Down/Clicked are readable after the call.
    function IsMouseOver: Boolean; virtual;
    procedure CheckState; virtual;
    function GetHover: Boolean; virtual;
    function GetDown: Boolean; virtual;
    function GetClicked: Boolean; virtual;
  public
    //* Text/caption of the control (buttons, labels, checkboxes, edits). The
    //* Lua controls table reads/writes it through these virtuals.
    function GetText: utf8string; virtual;
    procedure SetText(const AValue: utf8string); virtual;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(AValue: Boolean); virtual;
    //* Edges which may be resized when hovering at the local point (X, Y).
    //* brdSizable honors the Align constraint: aligned controls only expose the
    //* single free edge, alClient exposes none, alNone exposes all four.
    function GetResizeSides(X, Y: Integer): TTyroResizeSides;
    procedure ApplyResize;
    //* Make sure the own (transparent) texture buffer exists and matches the
    //* control size. Returns False when no buffer can be created.
    function PrepareCanvas: Boolean;

    //* Track/thumb rectangles are expressed in client-local coordinates; the
    //* caller maps window-local mouse points (X, Y) to them via ClientRect.Left.
    function ScrollTrackRect(Which: TScrollbarType): TRect;
    function ScrollThumbRect(Which: TScrollbarType): TRect;
    function ScrollThumbToPos(Which: TScrollbarType; AThumbStart: Integer): Integer;
    //* Which visible scrollbars contain the window-local point (X, Y).
    function HitScrollBar(X, Y: Integer): TScrollbarTypes;
    procedure PaintScrollBars(ACanvas: TTyroCanvas);
    procedure DrawScrollBar(ACanvas: TTyroCanvas; Which: TScrollbarType);

    procedure ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
    procedure SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
    procedure SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);
    procedure Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); virtual;

    procedure DoPaintBorder(ACanvas: TTyroCanvas); virtual;
    procedure DoPaintBackground(ACanvas: TTyroCanvas); virtual;
    procedure DoPaint(ACanvas: TTyroCanvas); virtual;

    procedure Created; override;
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

    property BackColor: TColor read FBackColor write SetBackColor;

    //* Shared text: caption for buttons/labels/checkboxes, edited text for edits.
    property Text: utf8string read GetText write SetText;
    property Checked: Boolean read GetChecked write SetChecked;
    property Hover: Boolean read GetHover;
    property Down: Boolean read GetDown;
    property Clicked: Boolean read GetClicked;

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
    procedure SetCaption(AValue: utf8string);
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    function GetText: utf8string; override;
    procedure SetText(const AValue: utf8string); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    property Caption: utf8string read FCaption write SetCaption;
  end;

  { TTyroLabel }

  TTyroLabel = class(TTyroControl)
  private
    FCaption: utf8string;
    procedure SetCaption(AValue: utf8string);
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    function GetText: utf8string; override;
    procedure SetText(const AValue: utf8string); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    property Caption: utf8string read FCaption write SetCaption;
  end;

  { TTyroCheckBox }

  TTyroCheckBox = class(TTyroControl)
  private
    FCaption: utf8string;
    FChecked: Boolean;
    procedure SetCaption(AValue: utf8string);
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    function GetText: utf8string; override;
    procedure SetText(const AValue: utf8string); override;
    function GetChecked: Boolean; override;
    procedure SetChecked(AValue: Boolean); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    property Caption: utf8string read FCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  { TTyroEdit }

  TTyroEdit = class(TTyroControl)
  private
    FText: utf8string;
    FCaretPos: Integer; //codepoint index into FText
    FSelStart: Integer; //selection anchor codepoint, -1 = none
    FSelEnd: Integer;   //selection end codepoint (exclusive), -1 = none
    FScrollPos: Integer;//horizontal scroll offset in pixels
    function PointToCaret(ALocalX: Integer): Integer;
    function TextWidth(const S: utf8string): Single;
    procedure DeleteSelection;
    procedure EnsureCaretVisible;
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    function GetText: utf8string; override;
    procedure SetText(const AValue: utf8string); override;
  public
    procedure FocusChanged; override;
    constructor Create(AParent: TTyroLayout); override;
    procedure KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: integer); override;
  end;

  { TTyroWindow }

  TTyroWindow = class abstract(TTyroLayout)
  private
    FCanvas: TTyroCanvas;
    FFocusedControl: TTyroControl;
    FTitle: utf8string;
    procedure SetCanvas(AValue: TTyroCanvas);
    procedure SetFocusedControl(AValue: TTyroControl);
    procedure SetTitle(AValue: utf8string);
  protected
    procedure PrepareCanvas; virtual;
    function CreateCanvas: TTyroCanvas; virtual; abstract;
  public
    constructor Create(AParent: TTyroLayout); overload; override;
    constructor Create(AParent: TTyroLayout; AWidth, AHeight: Integer); reintroduce; overload;
    destructor Destroy; override;
    procedure Paint;
    property Canvas: TTyroCanvas read FCanvas write SetCanvas;
    property Title: utf8string read FTitle write SetTitle;
    property FocusedControl: TTyroControl read FFocusedControl  write SetFocusedControl;
  end;

implementation

{ Codepoint helpers for UTF-8 strings (same pattern as TyroTerminal) }

function CPCount(const S: utf8string): Integer;
begin
  Result := UTF8Length(S);
end;

function CPSub(const S: utf8string; AStart, ACount: Integer): utf8string; //AStart is 0-based codepoint
begin
  if (ACount <= 0) or (AStart < 0) then
    Result := ''
  else
    Result := UTF8Copy(S, AStart + 1, ACount);
end;

function CPInsert(const S: utf8string; ACol: Integer; const AIns: utf8string): utf8string;
begin
  if AIns = '' then
    Result := S
  else
    Result := CPSub(S, 0, ACol) + AIns + CPSub(S, ACol, CPCount(S) - ACol);
end;

function CPDelete(const S: utf8string; ACol, ACount: Integer): utf8string;
begin
  Result := CPSub(S, 0, ACol) + CPSub(S, ACol + ACount, CPCount(S) - ACol - ACount);
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
    brdThin: Result := 1;
    brdThick: Result := 2;
    brdSizable: Result := 1;
    else
      Result := 0;
  end
end;

function TTyroLayout.BorderRect: TRect;
begin
  Result := WindowRect;
  //Result.Offset(-Result.Left, -Result.Top); //Because drawing will use Origin //NOPE
  Result.Inflate(-Margin, -Margin);
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

procedure TTyroLayout.Update;
begin
end;

{ TTyroWindow }

constructor TTyroPanel.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := [csOpaque];
  Border := brdSizable;
  BackColor := clGreen;
  BoundsRect := Rect(0 ,0 , 100, 100);
end;

procedure TTyroPanel.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRect;
begin
  inherited;
  r := ClientRect;
  //r.Inflate(-2,-2);
  ACanvas.DrawRectangle(r, BackColor, True);
end;

{ TTyroButton }

constructor TTyroButton.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := Style + [csFocus];
  BoundsRect := Rect(0, 0 , 100, 32);
end;

procedure TTyroButton.SetCaption(AValue: utf8string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

function TTyroButton.GetText: utf8string;
begin
  Result := FCaption;
end;

procedure TTyroButton.SetText(const AValue: utf8string);
begin
  Caption := AValue;
end;

procedure TTyroButton.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRectangle;
  body, border, foreground: TColor;
  tx, ty, tw, th: Single;
begin
  inherited;
  r := RectangleOf(ClientRect);
  if (r.Width <= 0) or (r.Height <= 0) then
    Exit;

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

{ TTyroLabel }

constructor TTyroLabel.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := [];
  Border := brdNone;
  BoundsRect := Rect(0, 0, 120, 24);
end;

procedure TTyroLabel.SetCaption(AValue: utf8string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

function TTyroLabel.GetText: utf8string;
begin
  Result := FCaption;
end;

procedure TTyroLabel.SetText(const AValue: utf8string);
begin
  Caption := AValue;
end;

procedure TTyroLabel.DoPaint(ACanvas: TTyroCanvas);
var
  th: Single;
begin
  inherited;
  th := Resources.Font.Height;
  ACanvas.DrawText(2, (ClientRect.Height - th) / 2, FCaption, ACanvas.PenColor);
end;

{ TTyroCheckBox }

constructor TTyroCheckBox.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := [csFocus];
  Border := brdNone;
  BoundsRect := Rect(0, 0, 120, 24);
end;

procedure TTyroCheckBox.SetCaption(AValue: utf8string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

function TTyroCheckBox.GetText: utf8string;
begin
  Result := FCaption;
end;

procedure TTyroCheckBox.SetText(const AValue: utf8string);
begin
  Caption := AValue;
end;

function TTyroCheckBox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TTyroCheckBox.SetChecked(AValue: Boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  Invalidate;
end;

procedure TTyroCheckBox.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRect;
  box: TRect;
  boxSize: Integer;
  th: Single;
begin
  inherited;
  r := ClientRect;

  boxSize := r.Height;
  if boxSize > 16 then
    boxSize := 16;
  box := Rect(r.Left, r.Top + (r.Height - boxSize) div 2, r.Left + boxSize, r.Top + (r.Height - boxSize) div 2 + boxSize);

  if FChecked then
    ACanvas.FillRectangle(box, clSkyBlue)
  else
    ACanvas.FillRectangle(box, clWhite);
  //ACanvas.DrawRect(box, 1, clDarkGray);
  {if FHover then
    ACanvas.DrawRect(Rect(box.Left - 1, box.Top - 1, box.Right + 1, box.Bottom + 1), 1, clSkyBlue);   }

  if FChecked then
  begin
    //check mark: two strokes inside the box
    ACanvas.DrawLine(box.Left + 3, box.Top + boxSize div 2, box.Left + boxSize div 2, box.Bottom - 3, ACanvas.PenColor);
    ACanvas.DrawLine(box.Left + boxSize div 2, box.Bottom - 3, box.Right - 2, box.Top + 3, ACanvas.PenColor);
  end;

  th := Resources.Font.Height;
  ACanvas.DrawText(box.Right + 6, r.Top + (r.Height - th) / 2, FCaption, ACanvas.PenColor);
end;

{ TTyroEdit }

constructor TTyroEdit.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := [csClip, csOpaque, csFocus];
  Border := brdNone;
  BackColor := clWhite;
  BoundsRect := Rect(0, 0, 140, 28);
end;

function TTyroEdit.GetText: utf8string;
begin
  Result := FText;
end;

procedure TTyroEdit.SetText(const AValue: utf8string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  if FCaretPos > CPCount(FText) then
    FCaretPos := CPCount(FText);
  FSelStart := -1;
  FSelEnd := -1;
  EnsureCaretVisible;
  Invalidate;
end;

function TTyroEdit.TextWidth(const S: utf8string): Single;
begin
  Result := RayLib.MeasureTextEx(Resources.Font.Data, PUTF8Char(S), Resources.Font.Height, 0).x;
end;

function TTyroEdit.PointToCaret(ALocalX: Integer): Integer;
var
  i, n: Integer;
begin
  ALocalX := ALocalX + FScrollPos;
  n := CPCount(FText);
  Result := n;
  for i := 0 to n - 1 do
  begin
    if TextWidth(CPSub(FText, 0, i + 1)) > ALocalX then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TTyroEdit.DeleteSelection;
var
  a, b: Integer;
begin
  if (FSelStart < 0) or (FSelEnd < 0) or (FSelStart = FSelEnd) then
    Exit;
  if FSelStart < FSelEnd then
  begin
    a := FSelStart;
    b := FSelEnd;
  end
  else
  begin
    a := FSelEnd;
    b := FSelStart;
  end;
  FText := CPDelete(FText, a, b - a);
  FCaretPos := a;
  FSelStart := -1;
  FSelEnd := -1;
  Invalidate;
end;

procedure TTyroEdit.EnsureCaretVisible;
var
  caretX, textW, w: Single;
  maxScroll: Integer;
begin
  caretX := TextWidth(CPSub(FText, 0, FCaretPos));
  textW := TextWidth(FText);
  w := ClientRect.Width;
  maxScroll := Round(textW) - Round(w) + 4;
  if maxScroll < 0 then
    maxScroll := 0;
  if caretX - FScrollPos > w - 4 then
    FScrollPos := Round(caretX) - Round(w) + 4
  else if caretX - FScrollPos < 2 then
    FScrollPos := Round(caretX) - 2;
  if FScrollPos < 0 then
    FScrollPos := 0;
  if FScrollPos > maxScroll then
    FScrollPos := maxScroll;
end;

procedure TTyroEdit.FocusChanged;
begin
  inherited;
  Invalidate;
end;

procedure TTyroEdit.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRect;
  th: Single;
  textColor: TColor;
  caretX, selX, selW: Integer;
  a, b: Integer;
  sel: Boolean;
begin
  inherited;
  r := ClientRect;

  th := Resources.Font.Height;

  //selection highlight under the text
  sel := (FSelStart >= 0) and (FSelEnd >= 0) and (FSelStart <> FSelEnd);
  if sel then
  begin
    if FSelStart < FSelEnd then
    begin
      a := FSelStart;
      b := FSelEnd;
    end
    else
    begin
      a := FSelEnd;
      b := FSelStart;
    end;
    selX := 2 + Round(TextWidth(CPSub(FText, 0, a))) - FScrollPos;
    selW := Round(TextWidth(CPSub(FText, a, b - a)));
    ACanvas.FillRectangle(selX + 1, 1, selW, r.Height - 2, clBlue.ReplaceAlpha(90));
  end;

  ACanvas.DrawText(2 - FScrollPos, (r.Height - th) / 2, FText, ACanvas.PenColor);

  if Focused and (Trunc(RayLib.GetTime() * 2) mod 2 = 0) then
  begin
    caretX := 2 + Round(TextWidth(CPSub(FText, 0, FCaretPos))) - FScrollPos;
    ACanvas.FillRectangle(caretX, 1, 1, r.Height - 2, clBlack);
  end;
end;

procedure TTyroEdit.KeyPress(var Key: TUTF8Char);
begin
  inherited;
  if Length(Key) = 0 then
    Exit;
  //skip single-byte control characters (character input is printable only anyway)
  if (Length(Key) = 1) and (Ord(Key[1]) < 32) then
    Exit;

  if (FSelStart >= 0) and (FSelEnd >= 0) and (FSelStart <> FSelEnd) then
    DeleteSelection;
  FText := CPInsert(FText, FCaretPos, Key);
  Inc(FCaretPos);
  EnsureCaretVisible;
  Invalidate;
end;

procedure TTyroEdit.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
var
  n, oldCaret: Integer;
begin
  inherited;
  n := CPCount(FText);

  if ssShift in Shift then
  begin
    //move the caret and extend/start the selection anchored at the old position
    case Key of
      KEY_LEFT:
      begin
        oldCaret := FCaretPos;
        if FCaretPos > 0 then
          Dec(FCaretPos);
        if FSelStart < 0 then
          FSelStart := oldCaret;
        FSelEnd := FCaretPos;
        Invalidate;
      end;
      KEY_RIGHT:
      begin
        oldCaret := FCaretPos;
        if FCaretPos < n then
          Inc(FCaretPos);
        if FSelStart < 0 then
          FSelStart := oldCaret;
        FSelEnd := FCaretPos;
        Invalidate;
      end;
      KEY_HOME:
      begin
        oldCaret := FCaretPos;
        FCaretPos := 0;
        if FSelStart < 0 then
          FSelStart := oldCaret;
        FSelEnd := FCaretPos;
        Invalidate;
      end;
      KEY_END:
      begin
        oldCaret := FCaretPos;
        FCaretPos := n;
        if FSelStart < 0 then
          FSelStart := oldCaret;
        FSelEnd := FCaretPos;
        Invalidate;
      end;
    end;
    EnsureCaretVisible;
    Exit;
  end;

  case Key of
    KEY_BACKSPACE:
    begin
      if (FSelStart >= 0) and (FSelEnd >= 0) and (FSelStart <> FSelEnd) then
        DeleteSelection
      else if FCaretPos > 0 then
      begin
        Dec(FCaretPos);
        FText := CPDelete(FText, FCaretPos, 1);
        Invalidate;
      end;
      EnsureCaretVisible;
    end;
    KEY_DELETE:
    begin
      if (FSelStart >= 0) and (FSelEnd >= 0) and (FSelStart <> FSelEnd) then
        DeleteSelection
      else if FCaretPos < n then
      begin
        FText := CPDelete(FText, FCaretPos, 1);
        Invalidate;
      end;
      EnsureCaretVisible;
    end;
    KEY_LEFT:
    begin
      if (FSelStart >= 0) and (FSelEnd >= 0) and (FSelStart <> FSelEnd) then
      begin
        if FSelStart < FSelEnd then
          FCaretPos := FSelStart
        else
          FCaretPos := FSelEnd;
      end
      else if FCaretPos > 0 then
        Dec(FCaretPos);
      FSelStart := -1;
      FSelEnd := -1;
      EnsureCaretVisible;
      Invalidate;
    end;
    KEY_RIGHT:
    begin
      if (FSelStart >= 0) and (FSelEnd >= 0) and (FSelStart <> FSelEnd) then
      begin
        if FSelStart > FSelEnd then
          FCaretPos := FSelStart
        else
          FCaretPos := FSelEnd;
      end
      else if FCaretPos < n then
        Inc(FCaretPos);
      FSelStart := -1;
      FSelEnd := -1;
      EnsureCaretVisible;
      Invalidate;
    end;
    KEY_HOME:
    begin
      FCaretPos := 0;
      FSelStart := -1;
      FSelEnd := -1;
      EnsureCaretVisible;
      Invalidate;
    end;
    KEY_END:
    begin
      FCaretPos := n;
      FSelStart := -1;
      FSelEnd := -1;
      EnsureCaretVisible;
      Invalidate;
    end;
  end;
end;

procedure TTyroEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    if not Focused then
      Focused := True;
    FCaretPos := PointToCaret(x);
    if ssShift in Shift then
      FSelEnd := FCaretPos
    else
    begin
      FSelStart := -1;
      FSelEnd := -1;
    end;
    EnsureCaretVisible;
    Invalidate;
  end;
end;

procedure TTyroEdit.MouseMove(Shift: TShiftState; x, y: integer);
begin
  inherited;
  if ssLeft in Shift then
  begin
    FCaretPos := PointToCaret(x);
    if FSelStart < 0 then
      FSelStart := FCaretPos;
    FSelEnd := FCaretPos;
    EnsureCaretVisible;
    Invalidate;
  end;
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
  Result := (Window <> nil) and (Window.FocusedControl = Self);
end;

procedure TTyroControl.SetBackColor(AValue: TColor);
begin
  if FBackColor=AValue then Exit;
  FBackColor:=AValue;
end;

procedure TTyroControl.SetFocused(AValue: Boolean);
begin
  //Honor the requested value: True steals the window focus (only focusable
  //controls, i.e. csFocus in Style), False releases it when we own it.
  if (Window <> nil) and (csFocus in Style) then
  begin
    if AValue then
      Window.FocusedControl := Self
    else if Window.FocusedControl = Self then
      Window.FocusedControl := nil;
  end;
end;

{ Mouse-over/press/click state, shared by interactive controls. CheckState is
  called from the base DoPaint every painted frame. }

function TTyroControl.IsMouseOver: Boolean;
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

procedure TTyroControl.CheckState;
begin
  FHover := IsMouseOver;
  FDown := FHover and RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT);
  FClicked := FWasDown and FHover and (not FDown);
  FWasDown := FDown;
end;

function TTyroControl.GetHover: Boolean;
begin
  Result := FHover;
end;

function TTyroControl.GetDown: Boolean;
begin
  Result := FDown;
end;

function TTyroControl.GetClicked: Boolean;
begin
  Result := FClicked;
end;

function TTyroControl.GetText: utf8string;
begin
  Result := '';
end;

procedure TTyroControl.SetText(const AValue: utf8string);
begin
  Invalidate;
end;

function TTyroControl.GetChecked: Boolean;
begin
  Result := False;
end;

procedure TTyroControl.SetChecked(AValue: Boolean);
begin
  Invalidate;
end;

procedure TTyroLayout.SetMargin(AValue: Integer);
begin
  if FMargin=AValue then Exit;
  FMargin:=AValue;
end;

procedure TTyroLayout.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TTyroLayout.SetWidth(AValue: Integer);
begin
  FBoundsRect.Width := AValue;
  Resize;
end;

procedure TTyroControl.SetWindow(AValue: TTyroWindow);
begin
  if FWindow =AValue then Exit;
  FWindow :=AValue;
end;

function TTyroControl.GetClientRect: TRect;
begin
  //* ClientRect is relative to this control's WindowRect origin.
  Result := WindowRect;
  Result.Inflate(-Margin - BorderSize, - Margin - BorderSize);
  Result.Offset(-Result.Left, -Result.Top); //Because drawing will use Origin
end;

procedure TTyroControl.SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
var
  Info: ^TTyroScrollInfo;
begin
  if Which = sbtVertical then
    Info := @FVScroll
  else
    Info := @FHScroll;
  if AMax < AMin then
    AMax := AMin;
  if APage < 1 then
    APage := 1;
  Info^.Min := AMin;
  Info^.Max := AMax;
  Info^.Page := APage;
  if Info^.Pos > AMax then
    Info^.Pos := AMax;
  if Info^.Pos < AMin then
    Info^.Pos := AMin;
  Invalidate;
end;

procedure TTyroControl.SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);
var
  Info: ^TTyroScrollInfo;
begin
  if Which = sbtVertical then
    Info := @FVScroll
  else
    Info := @FHScroll;
  if Info^.Min >= Info^.Max then
    AValue := Info^.Min
  else
  begin
    if AValue < Info^.Min then
      AValue := Info^.Min;
    if AValue > Info^.Max then
      AValue := Info^.Max;
  end;
  Info^.Pos := AValue;
  Info^.Visible := Visible;
  Invalidate;
end;

procedure TTyroControl.ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
begin
  if sbtVertical in Which then
  begin
    FVScroll.Visible := Visible;
    if Visible then
      Include(Style, csVScroll)
    else
      Exclude(Style, csVScroll);
  end;
  if sbtHorizontal in Which then
  begin
    FHScroll.Visible := Visible;
    if Visible then
      Include(Style, csHScroll)
    else
      Exclude(Style, csHScroll);
  end;
  Invalidate;
end;

procedure TTyroControl.Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
var
  Info: ^TTyroScrollInfo;
  Current: Integer;
begin
  if Witch = sbtVertical then
    Info := @FVScroll
  else
    Info := @FHScroll;
  Current := Info^.Pos;
  case ScrollCode of
    scrollTOP: Current := Info^.Min;
    scrollBOTTOM: Current := Info^.Max;
    scrollLINEDOWN: Inc(Current);
    scrollLINEUP: Dec(Current);
    scrollPAGEDOWN: Inc(Current, Info^.Page);
    scrollPAGEUP: Dec(Current, Info^.Page);
    scrollTHUMBPOSITION, scrollTHUMBTRACK: Current := Pos;
    scrollENDSCROLL: ;
  end;
  SetScrollPosition(Witch, Current, Info^.Visible);
end;

function TTyroControl.ScrollTrackRect(Which: TScrollbarType): TRect;
begin
  Result := Rect(0, 0, ClientRect.Width, ClientRect.Height);
  if Which = sbtVertical then
  begin
    if not (csVScroll in Style) or not FVScroll.Visible then
      Exit(Rect(-1, -1, -1, -1));
    Result.Left := Result.Right - cScrollSize;
    if (csHScroll in Style) and FHScroll.Visible then
      Dec(Result.Bottom, cScrollSize);
  end
  else
  begin
    if not (csHScroll in Style) or not FHScroll.Visible then
      Exit(Rect(-1, -1, -1, -1));
    Result.Top := Result.Bottom - cScrollSize;
    if (csVScroll in Style) and FVScroll.Visible then
      Dec(Result.Right, cScrollSize);
  end;
end;

function TTyroControl.ScrollThumbRect(Which: TScrollbarType): TRect;
var
  Track: TRect;
  Info: ^TTyroScrollInfo;
  TrackLen, Range, ThumbLen, ThumbOfs: Integer;
begin
  if Which = sbtVertical then
    Info := @FVScroll
  else
    Info := @FHScroll;
  Track := ScrollTrackRect(Which);
  if (Track.Right <= Track.Left) or (Track.Bottom <= Track.Top) then
    Exit(Track);
  if Which = sbtVertical then
    TrackLen := Track.Bottom - Track.Top
  else
    TrackLen := Track.Right - Track.Left;
  Range := Info^.Max - Info^.Min;
  if TrackLen <= 0 then
    Exit(Track);
  if Range <= 0 then
    Exit(Track); //nothing scrollable: full thumb
  ThumbLen := Round(TrackLen * Info^.Page / (Info^.Page + Range));
  if ThumbLen > TrackLen then
    ThumbLen := TrackLen;
  if ThumbLen < cScrollMinThumb then
    ThumbLen := cScrollMinThumb;
  ThumbOfs := Round((TrackLen - ThumbLen) * (Info^.Pos - Info^.Min) / Range);
  if ThumbOfs < 0 then
    ThumbOfs := 0;
  if ThumbOfs + ThumbLen > TrackLen then
    ThumbOfs := TrackLen - ThumbLen;
  if Which = sbtVertical then
    Result := Rect(Track.Left, Track.Top + ThumbOfs, Track.Right, Track.Top + ThumbOfs + ThumbLen)
  else
    Result := Rect(Track.Left + ThumbOfs, Track.Top, Track.Left + ThumbOfs + ThumbLen, Track.Bottom);
end;

function TTyroControl.ScrollThumbToPos(Which: TScrollbarType; AThumbStart: Integer): Integer;
var
  Track, Thumb: TRect;
  Info: ^TTyroScrollInfo;
  TrackLen, ThumbLen, Range: Integer;
begin
  if Which = sbtVertical then
    Info := @FVScroll
  else
    Info := @FHScroll;
  Track := ScrollTrackRect(Which);
  Thumb := ScrollThumbRect(Which);
  if Which = sbtVertical then
  begin
    TrackLen := Track.Bottom - Track.Top;
    ThumbLen := Thumb.Bottom - Thumb.Top;
  end
  else
  begin
    TrackLen := Track.Right - Track.Left;
    ThumbLen := Thumb.Right - Thumb.Left;
  end;
  Range := Info^.Max - Info^.Min;
  if (Range <= 0) or (TrackLen <= ThumbLen) then
    Exit(Info^.Min);
  if Which = sbtVertical then
    Result := Info^.Min + Round((AThumbStart - Track.Top) * Range / (TrackLen - ThumbLen))
  else
    Result := Info^.Min + Round((AThumbStart - Track.Left) * Range / (TrackLen - ThumbLen));
  if Result < Info^.Min then
    Result := Info^.Min;
  if Result > Info^.Max then
    Result := Info^.Max;
end;

function TTyroControl.HitScrollBar(X, Y: Integer): TScrollbarTypes;
var
  Ofs: Integer;
  P: TPoint;
begin
  Result := [];
  Ofs := Margin + BorderSize;
  P := Point(X - Ofs, Y - Ofs);
  if (csVScroll in Style) and FVScroll.Visible and PtInRect(ScrollTrackRect(sbtVertical), P) then
    Include(Result, sbtVertical);
  if (csHScroll in Style) and FHScroll.Visible and PtInRect(ScrollTrackRect(sbtHorizontal), P) then
    Include(Result, sbtHorizontal);
end;

procedure TTyroControl.DrawScrollBar(ACanvas: TTyroCanvas; Which: TScrollbarType);
var
  Track, Thumb: TRect;
  TrackColor, ThumbColor: TColor;
  Hover: Boolean;
begin
  Track := ScrollTrackRect(Which);
  if (Track.Right <= Track.Left) or (Track.Bottom <= Track.Top) then
    Exit;
  TrackColor := clDarkGray.ReplaceAlpha(140);
  ACanvas.DrawRectangle(Track, TrackColor, True);
  Thumb := ScrollThumbRect(Which);
  if (Thumb.Right <= Thumb.Left) or (Thumb.Bottom <= Thumb.Top) then
    Exit;
  if Which = sbtVertical then
    Hover := FVScrollHover
  else
    Hover := FHScrollHover;
  if Hover then
    ThumbColor := clLightgray
  else
    ThumbColor := clLightgray.ReplaceAlpha(170);
  ACanvas.DrawRectangle(Thumb, ThumbColor, True);
  ACanvas.DrawRect(Thumb, 1, clDarkGray);
end;

procedure TTyroControl.PaintScrollBars(ACanvas: TTyroCanvas);
begin
  if csVScroll in Style then
    DrawScrollBar(ACanvas, sbtVertical);
  if csHScroll in Style then
    DrawScrollBar(ACanvas, sbtHorizontal);
end;

procedure TTyroControl.Invalidate;
begin
end;

procedure TTyroControl.PaintWindow(ACanvas: TTyroCanvas);
var
  aClientRect: TRect;
begin
  if Visible then
  begin
    aClientRect := ClientRect;
    if (aClientRect.Width <= 0) or (aClientRect.Height <= 0) then
      exit;
    if PrepareCanvas then
    begin
      //* Paint the control content into its own transparent texture buffer,
      //* then draw (blit) that buffer on top of the window canvas.
      Canvas.BeginDraw;
      Canvas.ClearBackground(clBlank);
      try
        DoPaintBorder(Canvas);
        Canvas.SetOrigin(Margin + BorderSize, Margin + BorderSize);
        if csClip in Style then
          Canvas.BeginClip(aClientRect);
        try
          DoPaintBackground(Canvas);
          DoPaint(Canvas);
          PaintScrollBars(Canvas);
        finally
          Canvas.ResetOrigin;
          if csClip in Style then
            Canvas.EndClip;
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
      ACanvas.ResetOrigin;
      if csClip in Style then
        RayLib.BeginScissorMode(WindowRect.Left + aClientRect.Left, WindowRect.Top + aClientRect.Top, aClientRect.Width, aClientRect.Height);
      ACanvas.SetOrigin(WindowRect.Left + aClientRect.Left, WindowRect.Top + aClientRect.Top);
      try
        DoPaintBackground(ACanvas);
        DoPaint(ACanvas);
        PaintScrollBars(ACanvas);
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
    ACanvas.DrawRectangle(ClientRect, BackColor, True);
end;

procedure TTyroControl.DoPaint(ACanvas: TTyroCanvas);
begin
  //* Track hover/press/click state once per painted frame for every control.
  CheckState;
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
  Hit: TScrollbarTypes;
  Ofs, lx, ly: Integer;
  Thumb: TRect;
begin
  if (Button = mbLeft) then
  begin
    Hit := HitScrollBar(x, y);
    if (sbtVertical in Hit) and (FVScroll.Max > FVScroll.Min) then
    begin
      Ofs := Margin + BorderSize;
      lx := x - Ofs;
      ly := y - Ofs;
      Thumb := ScrollThumbRect(sbtVertical);
      if PtInRect(Thumb, Point(lx, ly)) then
      begin
        FVDrag := True;
        FVDragOfs := ly - Thumb.Top;
      end
      else if ly < Thumb.Top then
        Scroll(sbtVertical, scrollPAGEUP, FVScroll.Pos)
      else
        Scroll(sbtVertical, scrollPAGEDOWN, FVScroll.Pos);
      Exit;
    end
    else if (sbtHorizontal in Hit) and (FHScroll.Max > FHScroll.Min) then
    begin
      Ofs := Margin + BorderSize;
      lx := x - Ofs;
      ly := y - Ofs;
      Thumb := ScrollThumbRect(sbtHorizontal);
      if PtInRect(Thumb, Point(lx, ly)) then
      begin
        FHDrag := True;
        FHDragOfs := lx - Thumb.Left;
      end
      else if lx < Thumb.Left then
        Scroll(sbtHorizontal, scrollPAGEUP, FHScroll.Pos)
      else
        Scroll(sbtHorizontal, scrollPAGEDOWN, FHScroll.Pos);
      Exit;
    end;
  end;

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
  if FVDrag or FHDrag then
  begin
    if FVDrag then
      Scroll(sbtVertical, scrollENDSCROLL, FVScroll.Pos);
    if FHDrag then
      Scroll(sbtHorizontal, scrollENDSCROLL, FHScroll.Pos);
    FVDrag := False;
    FHDrag := False;
    Exit;
  end;
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
  Hit: TScrollbarTypes;
  Ofs, lx, ly: Integer;
begin
  FLastMouseX := x;
  FLastMouseY := y;

  Hit := HitScrollBar(x, y);
  FHScrollHover := sbtHorizontal in Hit;
  FVScrollHover := sbtVertical in Hit;

  if FVDrag then
  begin
    if ssLeft in Shift then
    begin
      Ofs := Margin + BorderSize;
      ly := y - Ofs;
      Scroll(sbtVertical, scrollTHUMBTRACK, ScrollThumbToPos(sbtVertical, ly - FVDragOfs));
    end
    else
      FVDrag := False;
    Exit;
  end;

  if FHDrag then
  begin
    if ssLeft in Shift then
    begin
      Ofs := Margin + BorderSize;
      lx := x - Ofs;
      Scroll(sbtHorizontal, scrollTHUMBTRACK, ScrollThumbToPos(sbtHorizontal, lx - FHDragOfs));
    end
    else
      FHDrag := False;
    Exit;
  end;

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

procedure TTyroControl.DoPaintBorder(ACanvas: TTyroCanvas);
var
  baseColor, highlightColor: TColor;
  activeSides: TTyroResizeSides;
  aRect: TRect;
begin
  if Border = brdNone then
    Exit;

  aRect := BorderRect;
  aRect.Right := aRect.Right + BorderSize;
  aRect.Bottom := aRect.Bottom + BorderSize;
  Canvas.DrawRect(aRect, BorderSize, clRed);
  exit;

  baseColor := clDarkGray;
  //ACanvas.FillRectangle(0, h - bs, w, bs, baseColor);
  //ACanvas.FillRectangle(0, 0, bs, h, baseColor);
  //ACanvas.FillRectangle(w - bs, 0, bs, h, baseColor);
  {
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
  end;}
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

procedure TTyroWindow.SetFocusedControl(AValue: TTyroControl);
begin
  if FFocusedControl =AValue then
    Exit;
  if FFocusedControl <> nil then
    FFocusedControl.FocusChanged;
  FFocusedControl :=AValue;
  if FFocusedControl <> nil then
    FFocusedControl.FocusChanged;
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
end.

