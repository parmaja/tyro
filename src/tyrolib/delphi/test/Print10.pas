unit Print10;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    belalhamed.
 *
 * //check: https://www.youtube.com/watch?v=HyK_Q5rrcr4
 *
 *}
interface

uses
  Classes, SysUtils,
  RayLib, Generics.Collections,
  TyroControls, TyroClasses;

type

  TMain = class(TTyroMain)
  public
    X, Y: Integer;
    Spacing: Integer;
    procedure Init; override;
    procedure Setup; override;
    procedure Draw; override;
    procedure Unload; override;
  end;

implementation

{ TMain }

procedure TMain.Draw;
begin
  inherited;
  Canvas.PenColor := clBlack;
//  Canvas.PenAlpha := 128;
  Canvas.DrawText(30, 30, 'Ready!', clRed);
  if Random(10)<5 then
    DrawLine(X, y, X+Spacing, Y+Spacing, Canvas.PenColor)
  else
    DrawLine(X, y+Spacing, X+Spacing, Y, Canvas.PenColor);

  Inc(X, Spacing);
  if x>Width then
  begin
    X := 0;
    Inc(Y, Spacing);
  end;
end;

procedure TMain.Init;
begin
  inherited;
  X := 0;
  Y := 0;
  Spacing := 10;
  MarginSize := 10;
  ShowWindow(400, 400, True);
end;

procedure TMain.Setup;
begin
  inherited;
  Randomize;
  SetFPS(10);
  //Options := Options - [moOpaque];
  Canvas.BackColor := clWhite;
  Canvas.PenColor := clBlack;
  //Options := Options + [moShowFPS];
end;

procedure TMain.Unload;
begin
  inherited;

end;

end.
