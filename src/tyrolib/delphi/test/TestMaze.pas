//check: https://www.youtube.com/watch?v=HyK_Q5rrcr4

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
    procedure RemoveWalls(vCell: TCell);
    function Check: TCell;
  end;

var
  Cells:TCells;

  FWidth: Integer = 400;
  FHeight: Integer = 400;
  FCols: Integer = 30;
  FRows: Integer = 30;
  FCW: Integer;
  FCurrent: TCell = nil;
  FNext: TCell;
  Stack: TStack<TCell>;
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

procedure TCell.RemoveWalls(vCell: TCell);
begin
  if (vCell.i-Self.i)=1 then
  begin
    Edges := Edges - [egRight];
    vCell.Edges := vCell.Edges - [egLeft];
  end
  else if (vCell.i-Self.i)=-1 then
  begin
    Edges := Edges - [egLeft];
    vCell.Edges := vCell.Edges - [egRight];
  end;

  if (vCell.j-Self.j)=1 then
  begin
    Edges := Edges - [egBottom];
    vCell.Edges := vCell.Edges - [egTop];
  end
  else if (vCell.j-Self.j)=-1 then
  begin
    Edges := Edges - [egTop];
    vCell.Edges := vCell.Edges - [egBottom];
  end;
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
      if Self=FCurrent then
        c := TColor.Create(200, 100, 77, 100)
      else
        c := TColor.Create(100, 200, 77, 100);

      DrawRectangle(x, y, w, w, c, True);
    end;
  end;
end;

end.
