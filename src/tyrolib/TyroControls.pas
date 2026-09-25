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
    csRepeatKeys,
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
    csAligning,
    csSizing,
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
    procedure SetMargin(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetParent(AValue: TTyroLayout);
  protected
    procedure VisibleChanged; virtual;
    procedure SizeChanged; virtual;
    procedure ParentChanged; virtual;

    procedure SetBoundsRect(AValue: TRect);
    procedure SetWindowRect(AValue: TRect);

    procedure Resize;
    function CanAlign: Boolean;

    procedure AddControl(AControl: TTyroLayout);
    procedure PaintWindow(ACanvas: TTyroCanvas); virtual;
    function BorderSize: Integer;
    function BorderRect: TRect;
    function GetInnerRect: TRect; virtual;
    function GetClientRect: TRect; virtual;
  public
    constructor Create(AParent: TTyroLayout); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Show;
    procedure Hide; virtual;

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
    //DO NOT USE BoundsRect.Width and BoundsRect.Height or any member directly
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    //Inflate the rect with margin and border
    property InnerRect: TRect read GetInnerRect;
    //ClientRect always start from (0, 0)
    property ClientRect: TRect read GetClientRect;
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
    procedure SetCanvas(AValue: TTyroCanvas);
    procedure SetFocused(AValue: Boolean);
  protected
    Style: TTyroControlStyles;
    procedure ParentChanged; override;
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
    //* Edges which may be SizeChanged when hovering at the local point (X, Y).
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
    procedure Scroll(Which: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); virtual;

    procedure DoPaintBorder(ACanvas: TTyroCanvas); virtual;
    procedure DoPaintBackground(ACanvas: TTyroCanvas); virtual;
    procedure DoPaint(ACanvas: TTyroCanvas); virtual;

    procedure Created; override;
    property Window: TTyroWindow read FWindow;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure Invalidate; virtual;

    procedure PaintWindow(ACanvas: TTyroCanvas); override;

    procedure FocusChanged; virtual;

    //Move the control to the end of the parent's control list (drawn last, on
    //top) when it is not aligned
    procedure Hide; override;

    procedure SetFocus;
    procedure BringToFront;

    //* Return the top-most visible control at a window-coordinate point.
    //* Children are tested before their container and siblings are tested in
    //* reverse paint order.
    function MouseTargetAt(AX, AY: Integer): TTyroControl;

    procedure KeyPress(var Key: TUTF8Char); virtual;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: TKeyboardKey; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseMove(Shift: TShiftState; x, y: integer); virtual;

    property Focused: Boolean read GetFocused write SetFocused;

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
    FPlaceHolder: utf8string;
    FCaretPos: Integer; //codepoint index into FText
    FSelStart: Integer; //selection anchor codepoint, -1 = none
    FSelEnd: Integer;   //selection end codepoint (exclusive), -1 = none
    FScrollPos: Integer;//horizontal scroll offset in pixels
    function PointToCaret(ALocalX: Integer): Integer;
    function TextWidth(const S: utf8string): Single;
    procedure SetPlaceHolder(AValue: utf8string);
    procedure DeleteSelection;
    procedure EnsureCaretVisible;
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    function GetText: utf8string; override;
    procedure SetText(const AValue: utf8string); override;
    //* Placeholder painting, called only while the edit holds no text.
    //* Override it in a subclass to draw something else than the default
    //* prompt text at the left edge of the client area.
    procedure DoDrawPlaceHolder(ACanvas: TTyroCanvas); virtual;
  public
    procedure FocusChanged; override;
    constructor Create(AParent: TTyroLayout); override;
    procedure KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: integer); override;
    //* Text shown instead of the (empty) content. Empty (the default) paints
    //* nothing while the edit is empty.
    property PlaceHolder: utf8string read FPlaceHolder write SetPlaceHolder;
  end;

  { TTyroListBox }

  TTyroListBox = class(TTyroControl)
  private
    FItems: TStrings;
    FViewCount: Integer;
    FItemHeight: Integer;
    FCustomDraw: Boolean;
    FPlaceHolder: utf8string;
    FTopIndex: Integer;   //first visible item (vertical scroll offset)
    FItemIndex: Integer;  //selected item, -1 = none
    function GetRowHeight: Integer;
    function GetVisibleItems: Integer;
    function GetMaxTop: Integer;
    procedure SetItems(AValue: TStrings);
    procedure SetViewCount(AValue: Integer);
    procedure SetItemHeight(AValue: Integer);
    procedure SetCustomDraw(AValue: Boolean);
    procedure SetItemIndex(AValue: Integer);
    procedure SetPlaceHolder(AValue: utf8string);
    procedure AutoSizeHeight;
    procedure ClampTop;
    procedure UpdateScrollBars;
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    procedure SizeChanged; override;
    procedure Scroll(Which: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    //* Custom per-item painting, called for every visible item when CustomDraw
    //* is True. Override it in a subclass to paint AItemRect yourself; the
    //* default does nothing.
    procedure DoCustomDraw(ACanvas: TTyroCanvas; AIndex: Integer; AItemRect: TRect); virtual;
    //* Default per-item painting: prints the item text, the selected row is
    //* highlighted.
    procedure DoDrawItem(ACanvas: TTyroCanvas; AIndex: Integer; AItemRect: TRect); virtual;
    //* Placeholder painting, called only while the list box holds no items.
    //* Override it in a subclass to draw something else than the default text
    //* centered in the client area.
    procedure DoDrawPlaceHolder(ACanvas: TTyroCanvas); virtual;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure AddItem(const AText: utf8string);
    procedure DeleteItem(AIndex: Integer);
    procedure Clear;
    //* Item index from a client-local Y coordinate, -1 when no item is there.
    function ItemIndexAt(Y: Integer): Integer;
    //* Text items shown by the list box.
    property Items: TStrings read FItems write SetItems;
    //* Rows the box is sized to display. Setting it to N resizes the control
    //* height to N * RowHeight. 0 disables auto-sizing: the control keeps the
    //* size given by its BoundsRect.
    property ViewCount: Integer read FViewCount write SetViewCount;
    //* Row height in pixels (0 = derived from the loaded font).
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property RowHeight: Integer read GetRowHeight;
    //* When True each visible item is painted by DoCustomDraw instead of the
    //* default printed text.
    property CustomDraw: Boolean read FCustomDraw write SetCustomDraw;
    //* Selected item, -1 = none. Set by clicking on an item.
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    //* Text painted centered in the box when Items is empty. Empty (the
    //* default) paints nothing while the list is empty.
    property PlaceHolder: utf8string read FPlaceHolder write SetPlaceHolder;
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
var
  aRect: TRect;
begin
  aRect := BoundsRect;
  aRect.Height := AValue;
  SetBoundsRect(aRect);
end;

function IsSelfOrAncestor(ARoot, ALayout: TTyroLayout): Boolean;
var
  aCurrent: TTyroLayout;
begin
  Result := False;
  aCurrent := ARoot;
  while aCurrent <> nil do
  begin
    if aCurrent = ALayout then
      Exit(True);
    aCurrent := aCurrent.Parent;
  end;
end;

function FindWindow(ALayout: TTyroLayout): TTyroWindow;
var
  aCurrent: TTyroLayout;
begin
  aCurrent := ALayout;
  while aCurrent <> nil do
  begin
    if aCurrent is TTyroWindow then
      Exit(TTyroWindow(aCurrent));
    aCurrent := aCurrent.Parent;
  end;
  Result := nil;
end;

procedure TTyroLayout.SetParent(AValue: TTyroLayout);
var
  aOldParent: TTyroLayout;
begin
  //A control cannot become its own parent (directly or through its subtree).
  if (AValue = Self) then
    Exit;

  if FParent = AValue then
    Exit;

  aOldParent := FParent;
  if aOldParent <> nil then
    aOldParent.Controls.Extract(Self);

  FParent := AValue;
  if FParent <> nil then
    FParent.AddControl(Self);
  ParentChanged;
  FParent.AlignControls;
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

function TTyroLayout.GetClientRect: TRect;
begin
  Result := InnerRect;
  //* ClientRect is relative to this control's WindowRect origin.
  Result.Offset(-Result.Left, -Result.Top); //Because drawing will use Origin
end;

function TTyroLayout.GetInnerRect: TRect;
begin
  Result := WindowRect;
  Result.Inflate(-Margin - BorderSize, - Margin - BorderSize);
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
  FState := FState - [csCreating] + [csCreated];
  AlignControls;
end;

procedure TTyroLayout.Show;
begin
  Visible := True;
end;

procedure TTyroLayout.Hide;
begin
  Visible := False;
end;

procedure TTyroLayout.Realign;
begin
  //Only parented, aligned controls are repositioned by their parent.
  //Top-level (Parent = nil) and non-aligned controls keep the rect that was
  //set directly: SetWindowRect (e.g. ResizeWindow) must NOT be clobbered by
  //re-applying alignment here. Bounds->window syncing happens in SetBoundsRect.
  if (Align <> alNone) and (Parent <> nil) then
    Parent.AlignControls;
end;

procedure TTyroLayout.AlignControls;
var
  aControl: TTyroLayout;
  aCount: Integer;
  cr, tr, ir: TRect;
begin
  //* Align child controls within this parent's WindowRect.
  //* Alignment only changes each child's effective WindowRect. BoundsRect
  //* remains the control's original/preferred geometry.
  if not Visible or (FControls = nil) or (FControls.Count = 0) or (csCreating in State) or (csDestroying in State) or (csAligning in State) then
    Exit;

  aCount := 0;
  for aControl in FControls do
    if aControl.CanAlign then
    begin
      aControl.FState := aControl.FState + [csAligning];
      Inc(aCount);
    end;
  try

    if aCount = 0 then
      exit; //Finally will be called, dont worry

    WriteLn(ClassName + ': AlignControls');
    ir := InnerRect;
    cr := ClientRect;

    for aControl in FControls do
    begin
      if aControl.CanAlign then
      begin
        case aControl.Align of
          alLeft:
          begin
            tr := Rect(cr.Left, cr.Top, cr.Left + aControl.Width, cr.Bottom);
            tr.Offset(ir.Left, ir.Top);
            aControl.SetWindowRect(tr);
            cr.Left := cr.Left + aControl.Width;
          end;
          alTop:
          begin
            tr := Rect(cr.Left, cr.Top, cr.Right, cr.Top + aControl.Height);
            tr.Offset(ir.Left, ir.Top);
            aControl.SetWindowRect(tr);
            cr.Top := cr.Top + aControl.Height;
          end;
          alRight:
          begin
            tr := Rect(cr.Right - aControl.Width, cr.Top, cr.Right, cr.Bottom);
            tr.Offset(ir.Left, ir.Top);
            aControl.SetWindowRect(tr);
            cr.Right := cr.Right - aControl.Width;
          end;
          alBottom:
          begin
            tr := Rect(cr.Left, cr.Bottom - aControl.Height, cr.Right, cr.Bottom);
            tr.Offset(ir.Left, ir.Top);
            aControl.SetWindowRect(tr);
            cr.Bottom := cr.Bottom - aControl.Height;
          end;
          alClient:
          begin
              //Not here, in another loop
          end;
          alNone:
            aControl.SetWindowRect(aControl.BoundsRect);
        end;
      end;
    end;

    for aControl in FControls do
    begin
      if aControl.CanAlign and (aControl.Align = alClient) then
      begin
        tr := cr;
        tr.Offset(ir.Left, ir.Top);
        aControl.SetWindowRect(cr);
      end;
    end;

  finally
    for aControl in FControls do
      if aControl.CanAlign then
        aControl.FState := aControl.FState - [csAligning];
  end;

  for aControl in FControls do
    aControl.AlignControls;
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

procedure TTyroEdit.SetPlaceHolder(AValue: utf8string);
begin
  if FPlaceHolder = AValue then
    Exit;
  FPlaceHolder := AValue;
  Invalidate;
end;

procedure TTyroEdit.DoDrawPlaceHolder(ACanvas: TTyroCanvas);
var
  r: TRect;
  th: Single;
begin
  //Override in a subclass to paint the empty state yourself (see PlaceHolder).
  if (FPlaceHolder = '') or (Resources = nil) then
    Exit;
  r := ClientRect;
  if (r.Width <= 0) or (r.Height <= 0) then
    Exit;
  th := Resources.Font.Height;
  //Drawn at the text origin, so it lines up with the first typed character.
  ACanvas.DrawText(2, (r.Height - th) / 2, FPlaceHolder, clGray);
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

  if FText = '' then
    //* Empty: show the prompt instead of nothing, keeping the caret visible
    //* in front of it.
    DoDrawPlaceHolder(ACanvas)
  else
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

{ TTyroListBox }

constructor TTyroListBox.Create(AParent: TTyroLayout);
begin
  inherited;
  //Allocate the item list first: any property change below that resizes the
  //control (Border, BoundsRect) runs SizeChanged -> ClampTop -> GetMaxTop, which
  //reads FItems.
  FItems := TStringList.Create;
  Style := [csClip, csOpaque, csVScroll, csFocus];
  Border := brdThin;
  BackColor := clWhite;
  FViewCount := 0;
  FItemHeight := 0;
  FCustomDraw := False;
  FPlaceHolder := '';
  FTopIndex := 0;
  FItemIndex := -1;
  BoundsRect := Rect(0, 0, 160, 120);
end;

destructor TTyroListBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TTyroListBox.GetRowHeight: Integer;
begin
  if FItemHeight > 0 then
    Result := FItemHeight
  else if Resources <> nil then
    Result := Resources.Font.Height
  else
    Result := 0;
  if Result < 10 then
    Result := 20; //fallback before/beside a loaded font
end;

function TTyroListBox.GetVisibleItems: Integer;
begin
  if RowHeight <= 0 then
    Exit(1);
  Result := ClientRect.Height div RowHeight;
  if Result < 1 then
    Result := 1;
end;

function TTyroListBox.GetMaxTop: Integer;
begin
  Result := FItems.Count - GetVisibleItems;
  if Result < 0 then
    Result := 0;
end;

procedure TTyroListBox.SetItems(AValue: TStrings);
begin
  if AValue = FItems then
    Exit;
  FItems.Assign(AValue);
  if FItemIndex >= FItems.Count then
    FItemIndex := FItems.Count - 1;
  if FItemIndex < -1 then
    FItemIndex := -1;
  ClampTop;
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.SetViewCount(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FViewCount = AValue then
    Exit;
  FViewCount := AValue;
  AutoSizeHeight;
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.SetItemHeight(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 0;
  if FItemHeight = AValue then
    Exit;
  FItemHeight := AValue;
  AutoSizeHeight;
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.SetCustomDraw(AValue: Boolean);
begin
  if FCustomDraw = AValue then
    Exit;
  FCustomDraw := AValue;
  Invalidate;
end;

procedure TTyroListBox.SetItemIndex(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if AValue >= FItems.Count then
    AValue := FItems.Count - 1;
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  //keep the selected row visible
  if (FItemIndex >= 0) and (FItemIndex < FTopIndex) then
    FTopIndex := FItemIndex
  else if (FItemIndex >= 0) and (FItemIndex >= FTopIndex + GetVisibleItems) then
    FTopIndex := FItemIndex - GetVisibleItems + 1;
  ClampTop;
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.SetPlaceHolder(AValue: utf8string);
begin
  if FPlaceHolder = AValue then
    Exit;
  FPlaceHolder := AValue;
  Invalidate;
end;

function TTyroListBox.ItemIndexAt(Y: Integer): Integer;
begin
  Result := -1;
  if (RowHeight <= 0) or (FItems.Count <= 0) then
    Exit;
  Result := FTopIndex + (Y div RowHeight);
  if (Result < 0) or (Result >= FItems.Count) then
    Result := -1;
end;

procedure TTyroListBox.AddItem(const AText: utf8string);
begin
  FItems.Add(AText);
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.DeleteItem(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    Exit;
  FItems.Delete(AIndex);
  if FItemIndex >= FItems.Count then
    FItemIndex := FItems.Count - 1;
  if FItemIndex < -1 then
    FItemIndex := -1;
  ClampTop;
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.Clear;
begin
  FItems.Clear;
  FItemIndex := -1;
  FTopIndex := 0;
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.ClampTop;
begin
  if FTopIndex < 0 then
    FTopIndex := 0;
  if FTopIndex > GetMaxTop then
    FTopIndex := GetMaxTop;
end;

procedure TTyroListBox.AutoSizeHeight;
begin
  if FViewCount > 0 then
    Height := FViewCount * RowHeight;
end;

procedure TTyroListBox.UpdateScrollBars;
var
  vis, maxTop: Integer;
begin
  ClampTop;
  vis := GetVisibleItems;
  maxTop := GetMaxTop;
  if maxTop > 0 then
  begin
    ShowScrollBar([sbtVertical], True);
    SetScrollRange(sbtVertical, 0, maxTop, vis);
    SetScrollPosition(sbtVertical, FTopIndex, True);
  end
  else
    ShowScrollBar([sbtVertical], False);
end;

procedure TTyroListBox.SizeChanged;
begin
  inherited;
  ClampTop;
  UpdateScrollBars;
end;

procedure TTyroListBox.Scroll(Which: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
var
  maxTop: Integer;
begin
  if Which <> sbtVertical then
  begin
    inherited Scroll(Which, ScrollCode, Pos);
    Exit;
  end;
  maxTop := GetMaxTop;
  case ScrollCode of
    scrollTOP: FTopIndex := 0;
    scrollBOTTOM: FTopIndex := maxTop;
    scrollLINEDOWN: Inc(FTopIndex);
    scrollLINEUP: Dec(FTopIndex);
    scrollPAGEDOWN: Inc(FTopIndex, GetVisibleItems);
    scrollPAGEUP: Dec(FTopIndex, GetVisibleItems);
    scrollTHUMBPOSITION, scrollTHUMBTRACK: FTopIndex := Pos;
    scrollENDSCROLL: ;
  end;
  ClampTop;
  UpdateScrollBars;
  Invalidate;
end;

procedure TTyroListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer);
var
  i: Integer;
begin
  inherited;
  if (Button = mbLeft) and (HitScrollBar(x, y) = []) then
  begin
    i := ItemIndexAt(y - (Margin + BorderSize));
    if i >= 0 then
      ItemIndex := i;
  end;
end;

procedure TTyroListBox.DoDrawItem(ACanvas: TTyroCanvas; AIndex: Integer; AItemRect: TRect);
var
  th, ty: Single;
begin
  if AIndex = FItemIndex then
    ACanvas.FillRectangle(AItemRect, clSkyBlue);
  th := Resources.Font.Height;
  ty := AItemRect.Top + (AItemRect.Height - th) / 2;
  if AIndex = FItemIndex then
    ACanvas.DrawText(AItemRect.Left + 4, ty, FItems[AIndex], clBlack)
  else
    ACanvas.DrawText(AItemRect.Left + 4, ty, FItems[AIndex], ACanvas.PenColor);
end;

procedure TTyroListBox.DoCustomDraw(ACanvas: TTyroCanvas; AIndex: Integer; AItemRect: TRect);
begin
  //Override in a subclass to paint the item yourself (see CustomDraw).
end;

procedure TTyroListBox.DoDrawPlaceHolder(ACanvas: TTyroCanvas);
var
  r: TRect;
  tx, ty, tw, th: Single;
begin
  //Override in a subclass to paint the empty state yourself (see PlaceHolder).
  if (FPlaceHolder = '') or (Resources = nil) then
    Exit;
  r := ClientRect;
  if (r.Width <= 0) or (r.Height <= 0) then
    Exit;
  th := Resources.Font.Height;
  tw := RayLib.MeasureTextEx(Resources.Font.Data, PUTF8Char(FPlaceHolder), Resources.Font.Height, 0).x;
  tx := (r.Width - tw) / 2;
  if tx < 0 then
    tx := 0; //longer than the box: keep it left aligned instead of half clipped
  ty := (r.Height - th) / 2;
  ACanvas.DrawText(tx, ty, FPlaceHolder, clGray);
end;

procedure TTyroListBox.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRect;
  n, vis, top, i, hoverIdx: Integer;
  itemRect: TRect;
begin
  inherited;
  r := ClientRect;
  n := FItems.Count;
  if n <= 0 then
  begin
    //* Nothing to list: paint the placeholder centered in the box instead of
    //* leaving it blank.
    DoDrawPlaceHolder(ACanvas);
    Exit;
  end;
  if (r.Height <= 0) or (RowHeight <= 0) then
    Exit;

  vis := GetVisibleItems;
  top := FTopIndex;
  hoverIdx := -1;
  if Hover then
    hoverIdx := ItemIndexAt(FLastMouseY - (Margin + BorderSize));

  for i := 0 to vis - 1 do
  begin
    if top + i >= n then
      Break;
    itemRect := Rect(0, i * RowHeight, r.Width, (i + 1) * RowHeight);
    if FCustomDraw then
      DoCustomDraw(ACanvas, top + i, itemRect)
    else
    begin
      if hoverIdx = top + i then
        ACanvas.FillRectangle(itemRect, clLightGray.ReplaceAlpha(90));
      DoDrawItem(ACanvas, top + i, itemRect);
    end;
  end;
end;

{ TTyroControl }

procedure TTyroLayout.SetBoundsRect(AValue: TRect);
begin
  if Name = 'Main' then
    nothing;
  if FBoundsRect = AValue then
    Exit;
  FBoundsRect := AValue;
  if Align = alNone then
    FWindowRect := AValue; //non-aligned (top-level/dragged): keep both in sync
  //DoSetBounds(AValue);
  Resize;
end;

procedure TTyroLayout.SetWindowRect(AValue: TRect);
begin
  FWindowRect := AValue;
  Resize;
end;

procedure TTyroLayout.Resize;
begin
  if not(csAligning in State) then
    Realign;
  AlignControls;
  SizeChanged;
end;

function TTyroLayout.CanAlign: Boolean;
begin
  Result := Visible and (Align <> alNone) and not (csCreating in State) and not (csDestroying in State);
end;

procedure TTyroLayout.SizeChanged;
begin
end;

procedure TTyroLayout.ParentChanged;
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
  if FMargin = AValue then
    Exit;
  FMargin := AValue;  
end;

procedure TTyroLayout.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  VisibleChanged;
end;

procedure TTyroLayout.VisibleChanged;
begin
  if (Parent <> nil) then
    Parent.AlignControls
  else //Parent will call AlignControls of children, without parent we need to call it manually
    AlignControls
end;

procedure TTyroLayout.SetWidth(AValue: Integer);
var
  aRect: TRect;
begin
  aRect := BoundsRect;
  aRect.Width := AValue;
  SetBoundsRect(aRect);
end;

procedure TTyroControl.ParentChanged;
begin
  inherited;
  if FParent <> nil then
  begin
    if (Parent is TTyroWindow) then
      FWindow := (Parent as TTyroWindow)
    else if (Parent is TTyroControl) then
      FWindow := (Parent as TTyroControl).Window;
  end;
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

procedure TTyroControl.Scroll(Which: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
var
  Info: ^TTyroScrollInfo;
  Current: Integer;
begin
  if Which = sbtVertical then
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
  SetScrollPosition(Which, Current, Info^.Visible);
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
  aControl: TTyroLayout;
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
      {while aControl in Controls do
        aControl.PaintWindow(ACanvas);}
      Canvas.PostDraw(WindowRect.Left, WindowRect.Top);
    end
    else
    begin
      //* No own texture could be created: paint directly as a fallback.
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

procedure TTyroControl.Hide;
begin
  inherited;
  Focused := False;
end;

procedure TTyroControl.SetFocus;
begin
  Focused := True;
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

function TTyroControl.MouseTargetAt(AX, AY: Integer): TTyroControl;
var
  I: Integer;
begin
  Result := nil;
  if not Visible or not PtInRect(WindowRect, Point(AX, AY)) then
    Exit;

  //Later siblings are on top. Test a container's children before the container
  //itself so transparent panel backgrounds do not intercept their buttons.
  for I := Controls.Count - 1 downto 0 do
  begin
    if Controls[I] is TTyroControl then
    begin
      Result := TTyroControl(Controls[I]).MouseTargetAt(AX, AY);
      if Result <> nil then
        Exit;
    end;
  end;
  Result := Self;
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
    RayLib.SetMouseCursor(MOUSE_CURSOR_DEFAULT);
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
      RayLib.SetMouseCursor(MOUSE_CURSOR_DEFAULT);
    end;
    RayLib.SetMouseCursor(GetCursorForSides(FResizeSides));
  end
  else if Border = brdSizable then
  begin
    sides := GetResizeSides(x, y);
    if sides <> [] then
      RayLib.SetMouseCursor(GetCursorForSides(sides));
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
  w, h, bs: Integer;
begin
  if Border = brdNone then
    Exit;

  //The border is painted in the control's own texture buffer, whose origin is
  //its top-left corner (the client area starts at Margin+BorderSize, applied
  //by the caller after DoPaintBorder runs).
  w := ACanvas.Width;
  h := ACanvas.Height;
  bs := BorderSize;
  if (w <= 0) or (h <= 0) or (bs <= 0) then
    Exit;

  baseColor := clDarkGray;
  highlightColor := clLightGray;

  //Solid frame around the whole control.
  ACanvas.FillRectangle(Rect(0, 0, w, bs), baseColor);
  ACanvas.FillRectangle(Rect(0, h - bs, w, h), baseColor);
  ACanvas.FillRectangle(Rect(0, bs, bs, h - bs), baseColor);
  ACanvas.FillRectangle(Rect(w - bs, bs, w, h - bs), baseColor);

  //Brighten the side under the cursor (or the one being dragged) so a sizable
  //border advertises its resize handles.
  if Border = brdSizable then
  begin
    if FResizing then
      activeSides := FResizeSides
    else if (FLastMouseX >= 0) and (FLastMouseX < w) and
            (FLastMouseY >= 0) and (FLastMouseY < h) then
      activeSides := GetResizeSides(FLastMouseX, FLastMouseY)
    else
      activeSides := [];

    if rsLeft in activeSides then
      ACanvas.FillRectangle(Rect(0, 0, bs, h), highlightColor);
    if rsTop in activeSides then
      ACanvas.FillRectangle(Rect(0, 0, w, bs), highlightColor);
    if rsRight in activeSides then
      ACanvas.FillRectangle(Rect(w - bs, 0, w, h), highlightColor);
    if rsBottom in activeSides then
      ACanvas.FillRectangle(Rect(0, h - bs, w, h), highlightColor);
  end;
end;

procedure TTyroControl.Created;
begin
end;

constructor TTyroControl.Create(AParent: TTyroLayout);
begin
  inherited;
  FState := FState + [csCreating];
  FLastMouseX := -1;
  FLastMouseY := -1;
  FVisible := True;
  Created;
  //Do not clear csCreating here. The most-derived constructor has not returned
  //yet; TTyroLayout.AfterConstruction clears it only after full construction.
  SetParent(AParent);
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
  //Replacing the canvas must not leak the previous one (the control-level
  //SetCanvas already follows this rule).
  FreeAndNil(FCanvas);
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

