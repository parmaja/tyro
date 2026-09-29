program tyro;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *
}

{.$apptype console}

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

uses
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  TyroApp;

{$R *.res}

begin
  Application := TTyroApplication.Create;
  Application.Title := 'Tyro';
  Application.Run;
  Application.Free;
end.
