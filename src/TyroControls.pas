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

  { TTyroControl }

  TTyroControl = class abstract(TTyroContainer)
  private
    FAlpha: Byte;
    FBoundsRect: TRect;
    FBackColor: TColor;
    FTextColor: TColor;
    FVisible: Boolean;
    FWindow: TTyroWindow;
    FParent: TTyroContainer;
    function GetHeight: Integer;
    function GetFocused: Boolean;
    function GetWidth: Integer;
    procedure SetAlpha(AValue: Byte);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetHeight(AValue: Integer);
    procedure SetFocused(AValue: Boolean);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    procedure SetWindow(AValue: TTyroWindow);
    procedure SetParent(AValue: TTyroContainer);
  protected
    IsCreated: Boolean;
    function GetClientRect: TRect;
    function GetClientWidth: Integer;
    function GetClientHeight: Integer;

    procedure ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
    procedure SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
    procedure SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);
    procedure Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); virtual;

    procedure SetBounds(Left, Top, Width, Height: Integer); virtual;
    procedure Resize; virtual;
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
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property ClientRect: TRect read GetClientRect;
    property Left: Integer read FBoundsRect.Left write SetLeft;
    property Top: Integer read FBoundsRect.Top write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ClientWidth: Integer read GetClientWidth;
    property ClientHeight: Integer read GetClientHeight;
    property Visible: Boolean read FVisible write SetVisible;
    property Alpha: Byte read FAlpha write SetAlpha;

    property BackColor: TColor read FBackColor write FBackColor;
    property TextColor: TColor read FTextColor write FTextColor;
  end;

  { TTyroPanel }

  TTyroPanel = class(TTyroControl)
  public
    constructor Create(AParent: TTyroContainer); override;
    procedure DoPaint(ACanvas: TTyroCanvas); override;

  end;

  { TTyroWindow }

  TTyroWindow = class(TTyroContainer)
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

procedure TTyroControl.SetBoundsRect(AValue: TRect);
begin
  if FBoundsRect = AValue then Exit;
  FBoundsRect := AValue;
  Resize;
end;

function TTyroControl.GetHeight: Integer;
begin
  Result := FBoundsRect.Height;
end;

function TTyroControl.GetFocused: Boolean;
begin
  Result := (Window <> nil) and (Window.Focused = Self);
end;

function TTyroControl.GetWidth: Integer;
begin
  Result := FBoundsRect.Width;
end;

procedure TTyroControl.SetAlpha(AValue: Byte);
begin
  if FAlpha =AValue then Exit;
  FAlpha :=AValue;
  Invalidate;
end;

procedure TTyroControl.SetHeight(AValue: Integer);
begin
  FBoundsRect.Height := AValue;
  Resize;
end;

procedure TTyroControl.SetFocused(AValue: Boolean);
begin
  if Window <> nil then
    Window.Focused := Self;
end;

procedure TTyroControl.SetLeft(AValue: Integer);
begin
  FBoundsRect.Left := AValue;
  Resize;
end;

procedure TTyroControl.SetTop(AValue: Integer);
begin
  FBoundsRect.Top := AValue;
  Resize;
end;

procedure TTyroControl.SetVisible(AValue: Boolean);
begin
  if FVisible =AValue then Exit;
  FVisible :=AValue;
  Invalidate;
end;

procedure TTyroControl.SetWidth(AValue: Integer);
begin
  FBoundsRect.Width := AValue;
  Resize;
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

procedure TTyroControl.SetBounds(Left, Top, Width, Height: Integer);
begin
  FBoundsRect := Rect(Left, Top, Left + Width, Top + Height);
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
    aAlpha := ACanvas.Alpha;
    ACanvas.Alpha := Alpha;
    try
      DoPaintBackground(ACanvas);
      DoPaint(ACanvas)
    finally
      ACanvas.ResetOrigin;
      ACanvas.Alpha := aAlpha;
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

procedure TTyroControl.Resize;
begin

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
  FParent := AParent;
  if Parent <> nil then
    Parent.AddControl(Self);
  if (Parent is TTyroWindow) then
    FWindow := (Parent as TTyroWindow);
  FVisible := True;
  FTextColor := clWhite;
  FBackColor := clBlack;
  Created;
  IsCreated := True;
end;

destructor TTyroControl.Destroy;
begin
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
    BeginDrawing;
    try
      ClearBackground(DefaultBackColor);

      {if Board <> nil then
      begin
        t := Board.LoadTexture;
        DrawTextureRec(t, TRectangle.Create(0, 0, t.width, t.height), TVector2.Create(0, 0), WHITE);
        UnloadTexture(t);
      end;}

      with Canvas.Texture do
        DrawTextureRec(texture, TRectangle.Create(0, 0, texture.width, -texture.height), Vector2Of(0, 0), clWhite);

      for aControl in Controls do
      begin
        aControl.Paint(Canvas);
      end;

    finally
      EndDrawing;
    end;
  end;
end;

end.

