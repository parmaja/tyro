program test;

{.$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  RayLib,
  TyroClasses, TyroControls;

type
  TMyMain = class(TTyroMain)
  public
    procedure Init; override;
    procedure Setup; override;
    procedure Draw; override;
  end;

var
  Main: TMyMain;

{ TMyMain }

procedure TMyMain.Draw;
begin
  inherited;
//  Canvas.Clear;
  Canvas.PenAlpha := 0;
  Canvas.PenColor := clBlack;
  Canvas.DrawText(30, 30, 'Ready!', clBlack);
  Canvas.DrawCircle(150, 150 , 50, clBlack, true);
end;

procedure TMyMain.Init;
begin
  inherited;
  //ShowWindow(400, 400);
end;

procedure TMyMain.Setup;
begin
  inherited;
  Options := Options + [moShowFPS];
//  Canvas.BackColor := ;
end;

begin
  Main := TMyMain.Create;
  try
    Main.Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
