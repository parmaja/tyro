program tyro;

{$ifopt D+}
{$apptype console}
{$endif}

{$R *.res}

uses
  System.SysUtils, TyroApp;

begin
  Application := TTyroApplication.Create;
  Application.Title := 'Tyro';
  Application.Run;
  Application.Free;
end.
