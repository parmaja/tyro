program tyro;

{.$apptype console}

{$R *.res}

uses
  System.SysUtils, TyroApp;

begin
  Application := TTyroApplication.Create;
  Application.Title := 'Tyro';
  Application.Run;
  Application.Free;
end.
