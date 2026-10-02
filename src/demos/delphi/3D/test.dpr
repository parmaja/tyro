program test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  RayLib,
  TyroClasses,
  TyroControls,
  TyroEngines,
  T3D,
  Generics.Collections;

begin
  Randomize;
  Main := T3D.TMain.Create;
  try
    try
      Main.Run;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    FreeAndNil(Main);
  end;
end.
