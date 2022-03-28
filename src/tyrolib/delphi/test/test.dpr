program test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  RayLib,
  TyroClasses,
  TyroControls,
  Generics.Collections,
  TestMaze in 'TestMaze.pas';

type
  TMyMain = class(TTyroMain)
  public
    procedure Init; override;
    procedure Setup; override;
    procedure Draw; override;
    procedure Unload; override;
  end;

{ TMyMain }

procedure TMyMain.Draw;
begin
  inherited;
  Canvas.PenAlpha := 0;
  Canvas.PenColor := clBlack;
  Canvas.DrawText(30, 30, 'Ready!', clBlack);

  for var aCell in Cells do
    aCell.Show;

  FCurrent.FHit := True;
  FNext := FCurrent.Check;
  if FNext<>nil then
  begin
    FNext.FHit := True;
    Stack.Push(FCurrent);
    FCurrent.RemoveWalls(FNext);
    FCurrent := FNext;
  end
  else
  begin
    if Stack.Count<>0 then
      FCurrent := Stack.Pop;
  end;
end;

procedure TMyMain.Init;
var
  x: Integer;
  y: Integer;
  c: TCell;
begin
  inherited;
  FCW := FWidth div FCols;
  Cells := TObjectList<TCell>.Create;
  Stack := TStack<TCell>.Create;

  ShowWindow(FWidth, FHeight);
  for y:=0 to FRows-1 do
    for x:=0 to FCols-1 do
      Cells.Add(TCell.Create(x, y, FCW));

  FCurrent := Cells[0];
end;

procedure TMyMain.Setup;
begin
  inherited;
  SetTargetFPS(30);
  //Options := Options + [moShowFPS];

//  Canvas.BackColor := ;
end;

procedure TMyMain.Unload;
begin
  inherited;
  FreeAndNil(Cells);
  FreeAndNil(Stack);
end;

begin
  Randomize;
  Main := TMyMain.Create;
  try
    Main.Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
