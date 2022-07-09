unit Perceptron;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    belalhamed.
 *
 * //check: https://www.youtube.com/watch?v=ntKn5TPHHAk&list=PLRqwX-V7Uu6aCibgK1PTWWu9by6XFdCfh&index=2
 *
 *}
interface

uses
  Classes, SysUtils, Math,
  RayLib, Generics.Collections,
  Melodies,
  TyroControls, TyroClasses;

type

  TPoint = record
    X: Integer;
    Y: Integer;
    function Sign: Integer;
    function Color: TColor;
    function Data: TArray<Double>;
  end;

  TPoints = TArray<TPoint>;

  TPerceptron = class
  protected
    FWeights: TArray<Double>;

  public
    constructor Create;
    destructor Destroy; override;
    function Guess(vInp: TArray<Double>): Integer;
    procedure Train(vInp: TArray<Double>; vTarget: Integer);

  end;

  TMain = class(TTyroMain)
  public
    X, Y: Integer;
    Spacing: Integer;
    Perceptron: TPerceptron;
    Points: TPoints;
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
  var w := Perceptron.Guess([Random, Random]);
//  Canvas.PenAlpha := 128;

  DrawLine(0, 0, Canvas.Width, Canvas.Height, Canvas.PenColor);
  Canvas.DrawText(60, 60, w.ToString, Canvas.PenColor);

  for var p in Points do
  begin
    if RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    begin
      Perceptron.Train(p.Data, p.Sign);
    end;

    var g := Perceptron.Guess(p.Data);
    if g=p.Sign then
      DrawCircle(p.X, p.Y, 4, p.Color)
    else
      DrawCircle(p.X, p.Y, 4, clRed);
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

  Perceptron := TPerceptron.Create;

  SetLength(Points, 100);
  for var I := 0 to Length(Points)-1 do
  begin
    Points[i].X := Random(Canvas.Width);
    Points[i].Y := Random(Canvas.Height);
  end;

end;

procedure TMain.Setup;
begin
  inherited;
  Randomize;
  SetFPS(10);

  Canvas.BackColor := clWhite;
  Canvas.PenColor := clBlack;
end;

procedure TMain.Unload;
begin
  inherited;
  FreeAndNil(Perceptron);
end;

{ TPerceptron }

constructor TPerceptron.Create;
begin
  inherited;
  SetLength(FWeights, 2);
  for var i := 0 to Length(FWeights)-1 do
    FWeights[i] := Random * RandomFrom([-1, 1]);

end;

destructor TPerceptron.Destroy;
begin

  inherited;
end;

function TPerceptron.Guess(vInp: TArray<Double>): Integer;
begin
  var w := 0.0;
  for var i := 0 to Length(vInp)-1 do
    w := w + vInp[i] * FWeights[i];
  //Result := Sign(w)
  if w>=0 then
    Result := 1
  else
    Result := -1;
end;

procedure TPerceptron.Train(vInp: TArray<Double>; vTarget: Integer);
begin
  var g := Guess(vInp);
  var e := vTarget - g;
  //tune all weights
  for var I := 0 to Length(FWeights)-1 do
    FWeights[i] := FWeights[i] + e * vInp[i];
end;

{ TPoint }

function TPoint.Color: TColor;
begin
  if Sign=1 then
    Result := clGreen
  else
    Result := clBlue;

end;

function TPoint.Data: TArray<Double>;
begin
  Result := [X, Y];
end;

function TPoint.Sign: Integer;
begin
  if X>Y then
    Result := 1
  else
    Result := -1;
end;

end.
