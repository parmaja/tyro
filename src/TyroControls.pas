unit TyroControls;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  mnUtils, mnClasses,
  TyroClasses, RayLib,
  LazUTF8, LCLType;

type
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

  TTyroControlState = (csCreating, csCreated, csDestroying);
  TTyroControlStates = set of TTyroControlState;

  TTyroContainer = class;
  TTyroControl = class;
  TTyroWindow = class;

  TTyroControls = class(specialize TmnObjectList<TTyroControl>)
  public
  end;

  { TTyroContainer }

  TTyroContainer = class abstract(TObject)
  private
    FControls: TTyroControls;
  protected
    procedure AddControl(AControl: TTyroControl);
  public
    constructor Create;
    destructor Destroy; override;
    property Controls: TTyroControls read FControls;
  end;

  TTyroSizable = class abstract(TTyroContainer)
  private
    FBoundsRect: TRect;
  protected
    procedure SetBoundsRect(AValue: TRect);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    procedure SetBounds(Left, Top, Width, Height: Integer); virtual;
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure Resize; virtual;
  public
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property Left: Integer read FBoundsRect.Left write SetLeft;
    property Top: Integer read FBoundsRect.Top write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

  { TTyroControl }

  TTyroControl = class abstract(TTyroSizable)
  private
    FAlpha: Byte;
    FBackColor: TColor;
    FPenColor: TColor;
    FWindow: TTyroWindow;
    FParent: TTyroContainer;
    FVisible: Boolean;
    function GetFocused: Boolean;
    procedure SetFocused(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetWindow(AValue: TTyroWindow);
    procedure SetParent(AValue: TTyroContainer);
  protected
    State: TTyroControlStates;
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
    property Window: TTyroWindow read FWindow write SetWindow;
  public
    constructor Create(AParent: TTyroContainer); virtual;
    destructor Destroy; override;
    procedure Invalidate; virtual;

    procedure Paint(ACanvas: TTyroCanvas);

    procedure FocusChanged; virtual;
    property Parent: TTyroContainer read FParent write SetParent;
    procedure Show;
    procedure Hide;

    procedure KeyPress(var Key: TUTF8Char); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: word; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseMove(Shift: TShiftState; x, y: integer); virtual;

    property Focused: Boolean read GetFocused write SetFocused;
    property ClientRect: TRect read GetClientRect;
    property ClientWidth: Integer read GetClientWidth;
    property ClientHeight: Integer read GetClientHeight;
    property Visible: Boolean read FVisible write SetVisible;

    property BackColor: TColor read FBackColor write FBackColor;
    property PenColor: TColor read FPenColor write FPenColor;
  end;

  { TTyroPanel }

  TTyroPanel = class(TTyroControl)
  public
    constructor Create(AParent: TTyroContainer); override;
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  end;

  { TTyroTexture }

  TTyroTexture = class(TTyroControl) //Own a texture
  private
    FCanvas: TTyroCanvas;
    procedure SetCanvas(AValue: TTyroCanvas);
  public
    //TODO
    constructor Create(AParent: TTyroContainer); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure Draw; virtual;
    procedure Resize; override;
    property Canvas: TTyroCanvas read FCanvas write SetCanvas;
  end;

  { TTyroWindow }

  TTyroWindow = class(TTyroSizable)
  private
    FCanvas: TTyroCanvas;
    FFocused: TTyroControl;
    FTitle: string;
    procedure SetCanvas(AValue: TTyroCanvas);
    procedure SetFocused(AValue: TTyroControl);
    procedure SetTitle(AValue: string);
  protected
    DefaultBackColor: TColor;
    Margin: Integer;
    procedure NeedCanvas;
  public
    Visible: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Paint;
    property Canvas: TTyroCanvas read FCanvas write SetCanvas;
    property Title: string read FTitle write SetTitle;
    property Focused: TTyroControl read FFocused write SetFocused;
  end;

implementation

{ TTyroTexture }

procedure TTyroTexture.SetCanvas(AValue: TTyroCanvas);
begin
  if FCanvas =AValue then Exit;
  FCanvas :=AValue;
end;

constructor TTyroTexture.Create(AParent: TTyroContainer);
begin
  inherited Create(AParent);
  FCanvas := TTyroCanvas.Create(Width, Height);
end;

destructor TTyroTexture.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TTyroTexture.Invalidate;
begin
  inherited Invalidate;
  Paint(Canvas);
end;

procedure TTyroTexture.Draw;
begin
  with Canvas.Texture do
    DrawTextureRec(Texture, TRectangle.Create(0, 0, texture.width, -texture.height), Vector2Of(0, 0), clWhite);
end;

procedure TTyroTexture.Resize;
begin
  Canvas.Width := Width;
  Canvas.Height := Height;
  inherited Resize;
end;

{ TTyroContainer }

procedure TTyroContainer.AddControl(AControl: TTyroControl);
begin
  Controls.Add(AControl);
end;

constructor TTyroContainer.Create;
begin
  inherited Create;
  FControls := TTyroControls.Create(True);
end;

destructor TTyroContainer.Destroy;
var
  aControl: TTyroControl;
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

{ TTyroWindow }

constructor TTyroPanel.Create(AParent: TTyroContainer);
begin
  inherited;
  FBoundsRect.Right := 100;
  FBoundsRect.Bottom := 100;
end;

procedure TTyroPanel.DoPaint(ACanvas: TTyroCanvas);
begin
  inherited;
  ACanvas.DrawRectangle(ClientRect, BackColor, True);
end;

{ TTyroControl }

procedure TTyroSizable.SetBoundsRect(AValue: TRect);
begin
  if FBoundsRect = AValue then Exit;
  FBoundsRect := AValue;
  Resize;
end;

function TTyroSizable.GetHeight: Integer;
begin
  Result := FBoundsRect.Height;
end;

function TTyroSizable.GetWidth: Integer;
begin
  Result := FBoundsRect.Width;
end;

procedure TTyroSizable.SetHeight(AValue: Integer);
begin
  FBoundsRect.Height := AValue;
  Resize;
end;

procedure TTyroSizable.SetLeft(AValue: Integer);
begin
  FBoundsRect.Left := AValue;
  Resize;
end;

procedure TTyroSizable.SetTop(AValue: Integer);
begin
  FBoundsRect.Top := AValue;
  Resize;
end;

procedure TTyroSizable.SetWidth(AValue: Integer);
begin
  FBoundsRect.Width := AValue;
  Resize;
end;

procedure TTyroSizable.SetBounds(Left, Top, Width, Height: Integer);
begin
  FBoundsRect := Rect(Left, Top, Left + Width, Top + Height);
end;

procedure TTyroSizable.Resize;
begin

end;

function TTyroControl.GetFocused: Boolean;
begin
  Result := (Window <> nil) and (Window.Focused = Self);
end;

procedure TTyroControl.SetFocused(AValue: Boolean);
begin
  if Window <> nil then
    Window.Focused := Self;
end;

procedure TTyroControl.SetVisible(AValue: Boolean);
begin
  if FVisible =AValue then Exit;
  FVisible :=AValue;
  Invalidate;
end;

procedure TTyroControl.SetWindow(AValue: TTyroWindow);
begin
  if FWindow =AValue then Exit;
  FWindow :=AValue;
end;

procedure TTyroControl.SetParent(AValue: TTyroContainer);
begin
  if FParent =AValue then
    Exit;
  if FParent <> nil then
    FParent.Controls.Extract(Self);
  FParent :=AValue;
  if FParent <> nil then
    FParent.AddControl(Self);
end;

function TTyroControl.GetClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TTyroControl.GetClientWidth: Integer;
begin
  Result := ClientRect.Width;
end;

function TTyroControl.GetClientHeight: Integer;
begin
  Result := ClientRect.Height;
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

procedure TTyroControl.Paint(ACanvas: TTyroCanvas);
var
  aAlpha: Byte;
begin
  if Visible then
  begin
    ACanvas.SetOrigin(Left, Top);
    try
      DoPaintBackground(ACanvas);
      DoPaint(ACanvas)
    finally
      ACanvas.ResetOrigin;
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

procedure TTyroControl.KeyDown(var Key: word; Shift: TShiftState);
begin

end;

procedure TTyroControl.KeyUp(var Key: word; Shift: TShiftState);
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

constructor TTyroControl.Create(AParent: TTyroContainer);
begin
  inherited Create;
  State := State + [csCreating];
  FParent := AParent;
  if Parent <> nil then
    Parent.AddControl(Self);
  if (Parent is TTyroWindow) then
    FWindow := (Parent as TTyroWindow);
  FVisible := True;
  FPenColor := clWhite;
  FBackColor := clBlack;
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

{ TTyroWindow }

procedure TTyroWindow.SetTitle(AValue: string);
begin
  if FTitle =AValue then Exit;
  FTitle :=AValue;
end;

procedure TTyroWindow.NeedCanvas;
begin
  if FCanvas = nil then
    FCanvas := TTyroCanvas.Create(Width, Height);
end;

procedure TTyroWindow.SetFocused(AValue: TTyroControl);
begin
  if FFocused =AValue then Exit;
  if FFocused <> nil then
    FFocused.FocusChanged;
  FFocused :=AValue;
  if FFocused <> nil then
    FFocused.FocusChanged;
end;

procedure TTyroWindow.SetCanvas(AValue: TTyroCanvas);
begin
  if FCanvas =AValue then Exit;
  FCanvas :=AValue;
end;

constructor TTyroWindow.Create;
begin
  inherited Create;
  DefaultBackColor := TColor.Create(220, 230, 240, 0);
end;

destructor TTyroWindow.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TTyroWindow.Paint;
var
  aControl: TTyroControl;
begin
  if Visible then
  begin
    try
      ClearBackground(DefaultBackColor);

      with Canvas.Texture do
        DrawTextureRec(Texture, TRectangle.Create(0, 0, texture.width, -texture.height), Vector2Of(0, 0), clWhite);

      for aControl in Controls do
      begin
        aControl.Paint(Canvas);
      end;

    finally
    end;
  end;
end;

end.

