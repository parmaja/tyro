unit TestMaze;
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
  TEdge = (egLeft, egTop, egRight, egBottom);
  TEdges = set of TEdge;

  TCell = class;

  TCells = TObjectList<TCell>;

  TCell = class
  public
    Row: Integer;
    Col: Integer;
    Width: Single;
    Edges: TEdges;
    FHit: Boolean;

    constructor Create(vRow, vCol: Integer; AWidth: Single);
    procedure Show;
    procedure RemoveWalls(vCell: TCell);
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
  Stack: TStack<TCell>;
//  Stack: TCells;

implementation


{ TCell }

function GetIndex(vRow, vCol: Integer): Integer;
begin
  Result := vCol + vRow * FCols;
end;

function GetCell(vRow, vCol: Integer): TCell;
var
  idx: Integer;
begin
  idx := GetIndex(vRow, vCol);
  if (idx>=0) and (idx<Cells.Count) then
    Result := Cells[idx]
  else
    Result := nil;
end;

function TCell.Check: TCell;
var
  aCells: TCells;

  procedure _Add(vRow, vCol: Integer);
  var
    t: TCell;
  begin
    t := GetCell(vRow, vCol);
    if (t <> nil)and(not t.FHit) then
      aCells.Add(t);
  end;

begin
  aCells := TCells.Create;
  try
    aCells.OwnsObjects := False;

    if Row>0 then _Add(Row-1, Col); //top
    if Col<FCols then _Add(Row, Col+1); //right
    if Row<FRows then _Add(Row + 1, Col); //bottom
    if Col>0 then _Add(Row, Col-1); //left

    if aCells.Count = 0 then
      Result := nil
    else
      Result := aCells[Random(aCells.Count)];
  finally
    aCells.Free;
  end;
end;

constructor TCell.Create(vRow, vCol: Integer; AWidth: Single);
begin
  Self.Row := vRow;
  Self.Col := vCol;
  Self.Width := AWidth;
  Edges := [egLeft, egTop, egRight, egBottom];
  FHit := False;
end;

procedure TCell.RemoveWalls(vCell: TCell);
begin
  if (vCell.Col-Self.Col)=1 then
  begin
    Edges := Edges - [egRight];
    vCell.Edges := vCell.Edges - [egLeft];
  end
  else if (vCell.Col-Self.Col)=-1 then
  begin
    Edges := Edges - [egLeft];
    vCell.Edges := vCell.Edges - [egRight];
  end;

  if (vCell.Row-Self.Row)=1 then
  begin
    Edges := Edges - [egBottom];
    vCell.Edges := vCell.Edges - [egTop];
  end
  else if (vCell.Row-Self.Row)=-1 then
  begin
    Edges := Edges - [egTop];
    vCell.Edges := vCell.Edges - [egBottom];
  end;
end;

procedure TCell.Show;
var
  c: TColor;
  x, y, w: Single;
begin
  with Canvas do
  begin
    w := Self.Width;
    x := Col*w;
    y := Row*w;

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
