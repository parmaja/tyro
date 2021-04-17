unit TyroControls;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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

  { TTyroControl }

  TTyroControl = class(TObject)
  private
    FBoundsRect: TRect;
    FBackColor: TColor;
    FTextColor: TColor;
  protected
    IsCreated: Boolean;
    Focused: Boolean;
    function ClientWidth: Integer;
    function ClientHeight: Integer;

    procedure ShowScrollBar(Which: TScrollbarTypes; Visible: Boolean);
    procedure SetScrollRange(Which: TScrollbarType; AMin, AMax: Integer; APage: Integer);
    procedure SetScrollPosition(Which: TScrollbarType; AValue: Integer; Visible: Boolean);

    procedure SetBounds(Left, Top, Width, Height: Integer); virtual;
    procedure Resize; virtual;

    procedure Invalidate; virtual;
    procedure Paint(ACanvas: TTyroCanvas); virtual;

    procedure KeyPress(var Key: TUTF8Char); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: word; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); virtual;
    procedure MouseMove(Shift: TShiftState; x, y: integer); virtual;

    procedure Created; virtual;
  public
    constructor Create(AParent: TTyroControl); virtual;
    destructor Destroy; override;
    property BoundsRect: TRect read FBoundsRect write FBoundsRect;
    property BackColor: TColor read FBackColor write FBackColor;
    property TextColor: TColor read FTextColor write FTextColor;
  end;

  { TTyroWindow }

  TTyroWindow = class(TTyroControl)
  public
    procedure Paint(ACanvas: TTyroCanvas); override;
  end;

implementation

{ TTyroWindow }

procedure TTyroWindow.Paint(ACanvas: TTyroCanvas);
begin
  inherited;
  ACanvas.DrawRect(BoundsRect, BackColor, False);
end;

{ TTyroControl }

function TTyroControl.ClientWidth: Integer;
begin
  Result := BoundsRect.Width;
end;

function TTyroControl.ClientHeight: Integer;
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

procedure TTyroControl.SetBounds(Left, Top, Width, Height: Integer);
begin
  FBoundsRect := Rect(Left, Top, Left + Width, Top + Height);
end;

procedure TTyroControl.Invalidate;
begin

end;

procedure TTyroControl.Paint(ACanvas: TTyroCanvas);
begin

end;

procedure TTyroControl.Resize;
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
  FTextColor := White;
  FBackColor := Black;
  Created;
  IsCreated := True;
end;

destructor TTyroControl.Destroy;
begin
  inherited Destroy;
end;

end.

