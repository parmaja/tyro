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
  Canvas.PenAlpha := 0;
  Canvas.PenColor := clBlack;
  Canvas.DrawText(30, 30, 'Ready!', clBlack);
  if Random(10)<5 then
    DrawLine(X, y, X+Spacing, Y+Spacing, clWhite)
  else
    DrawLine(X, y+Spacing, X+Spacing, Y, clWhite);

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
  Width := 400;
  Height := 400;
  ShowWindow(Width, Height);
end;

procedure TMain.Setup;
begin
  inherited;
  SetFPS(10);
  Options := Options - [moOpaque];
  Randomize;
  //Options := Options + [moShowFPS];
end;

procedure TMain.Unload;
begin
  inherited;

end;

end.
