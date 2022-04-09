program test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  RayLib,
  TyroClasses,
  TyroControls,
  Generics.Collections,
  Print10 in 'Print10.pas',
  TestMaze in 'TestMaze.pas';

begin
  Randomize;
  Main := TestMaze.TMain.Create;
  //Main := Print10.TMain.Create;
  try
    Main.Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
