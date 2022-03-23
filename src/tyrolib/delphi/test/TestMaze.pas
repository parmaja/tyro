unit TestMaze;

interface

uses
  Classes, SysUtils,
  RayLib, Generics.Collections,
  TyroControls, TyroClasses;

type
  TEdge = (egLeft, egTop, egRight, egBottom);
  TEdges = set of TEdge;

  TCell = class;

  TCells = TObjectList<TCell>;

  TCell = class
  public
    i: Integer;
    j: Integer;
    w: Integer;
    Edges: TEdges;
    FHit: Boolean;

    constructor Create(i, j, w: Integer);
    procedure Show;
    function Check: TCell;
  end;

var
  Cells:TCells;

  FWidth: Integer = 400;
  FHeight: Integer = 400;
  FCols: Integer = 10;
  FRows: Integer = 10;
  FCW: Integer;
  FCurrent: TCell = nil;
  FNext: TCell;
//  Stack: TCells;

implementation


{ TCell }

function GetIndex(I, J: Integer): Integer;
begin
  Result := i + j * FCols;
end;

function GetCell(I, J: Integer): TCell;
var
  idx: Integer;
begin
  idx := GetIndex(i, j);
  if (idx>=0) and (idx<Cells.Count) then
    Result := Cells[idx]
  else
    Result := nil;
end;

function TCell.Check: TCell;
var
  aCells: TCells;

  procedure _Add(I, J: Integer);
  var
    t: TCell;
  begin
    t := GetCell(i, j);
    if (t <> nil)and(not t.FHit) then
      aCells.Add(t);
  end;

begin
  aCells := TCells.Create;
  try
    aCells.OwnsObjects := False;

    _Add(i, j-1);
    _Add(i + 1, j);
    _Add(i, j+1);
    _Add(i-1, j);

    if aCells.Count = 0 then
      Result := nil
    else
      Result := aCells[Random(aCells.Count)];
  finally
    aCells.Free;
  end;
end;

constructor TCell.Create(i, j, w: Integer);
begin
  Self.i := i;
  Self.j := j;
  Self.w := w;
  Edges := [egLeft, egTop, egRight, egBottom];
  FHit := False;
end;

procedure TCell.Show;
var
  c: TColor;
  x, y: Integer;
begin
  with Canvas do
  begin
    x := i*w;
    y := j*w;

    PenColor := clWhite;

    if egTop in Edges then
      DrawLine(x, y, x+w, y);//top
    if egRight in Edges then
      DrawLine(x+w, y, x+w, y+w); //right
    if egBottom in Edges then
      DrawLine(x, y+w, x+w, y+w); //bottom
    if egLeft in Edges then
      DrawLine(x, y, x, y+w); //left


    if FHit then
    begin
      c := TColor.Create(100, 200, 77, 100);
      DrawRectangle(x, y, w, w, c, True);
    end;
  end;
end;

end.
