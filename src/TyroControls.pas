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
  TyroClasses, RayLib3,
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

  TTyroControl = class;

  TTyroControls = class(specialize TmnObjectList<TTyroControl>)
  public
  end;

  { TTyroParentControl }

  TTyroParentControl = class abstract(TObject)
  private
    FControls: TTyroControls;
  public
    constructor Create;
    destructor Destroy; override;
    property Controls: TTyroControls read FControls;
  end;

  { TTyroControl }

  TTyroControl = class abstract(TTyroParentControl)
  private
    FAlpha: Byte;
    FParent: TTyroControl;
    FBoundsRect: TRect;
    FBackColor: TColor;
    FTextColor: TColor;
    FVisible: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetAlpha(AValue: Byte);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  protected
    IsCreated: Boolean;
    Focused: Boolean;
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
  public
    constructor Create(AParent: TTyroControl); virtual;
    destructor Destroy; override;
    procedure Invalidate; virtual;

    procedure Paint(ACanvas: TTyroCanvas);

    procedure KeyPress(var Key: TUTF8Char); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: word; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseMove(Shift: TShiftState; x, y: integer); virtual;

    property Parent: TTyroControl read FParent;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
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

  { TTyroWindow }

  { TTyroForm }

  TTyroForm = class(TTyroControl)
  public
    constructor Create(AParent: TTyroControl); override;
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  end;

implementation

{ TTyroParentControl }

constructor TTyroParentControl.Create;
begin
  inherited Create;
  FControls := TTyroControls.Create(True);
end;

destructor TTyroParentControl.Destroy;
begin
  FreeAndNil(FControls);
  inherited Destroy;
end;

{ TTyroWindow }

constructor TTyroForm.Create(AParent: TTyroControl);
begin
  inherited Create(AParent);
  FBoundsRect.Right := 100;
  FBoundsRect.Bottom := 100;
end;

procedure TTyroForm.DoPaint(ACanvas: TTyroCanvas);
begin
  inherited;
  ACanvas.DrawRect(BoundsRect, BackColor, True);
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

function TTyroControl.GetClientWidth: Integer;
begin
  Result := BoundsRect.Width;
end;

function TTyroControl.GetClientHeight: Integer;
begin
  Result := BoundsRect.Width;
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

constructor TTyroControl.Create(AParent: TTyroControl);
begin
  inherited Create;
  FParent := AParent;
  if Parent <> nil then
    Parent.Controls.Add(Self);
  FVisible := True;
  FTextColor := White;
  FBackColor := Black;
  Created;
  IsCreated := True;
end;

destructor TTyroControl.Destroy;
begin
  if Parent <> nil then
    Parent.Controls.Extract(Self);
  FParent := nil;
  inherited Destroy;
end;

end.

