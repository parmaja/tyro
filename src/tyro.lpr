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

{$ifopt D+}
{.$apptype console}
{$endif}

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

uses
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  TyroApp, TyroEditors, Melodies, TyroLua, TyroScripts, TyroControls,
  TyroTerminal, TyroSounds;

{$R *.res}

var
  Engineless: Boolean;

begin
  Engineless := False;
  Application := TTyroApplication.Create;
  Application.Title := 'Tyro';
  Application.Run;
  Engineless := Application.Engineless;
  Application.Free;

  {$IFDEF MSWINDOWS}
  //A console-only run (--help, --fuse) never creates the engine, and some
  //engine-related unit finalizations fault when the engine is absent. The
  //console was already flushed and closed by CloseConsole, so exit directly
  //with the recorded exit code instead of running those finalizations.
  if Engineless then
    ExitProcess(ExitCode);
  {$ENDIF}
end.
