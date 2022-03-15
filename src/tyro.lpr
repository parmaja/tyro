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
 *  Fonts
 *   https://opengameart.org/content/the-collection-of-8-bit-fonts-for-grafx2
     http://www.pentacom.jp/pentacom/bitfontmaker2/gallery/?page=1&order=&
     http://orangetide.com/OLD/fonts/DOS/
 *}

{$mode objfpc}
{$H+}

//ref https://github.com/raysan5/raylib/blob/master/examples/textures/textures_mouse_painting.c

uses
  Windows,
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, CustApp, TyroClasses, RayLib, RayClasses,
  TyroEditors, mnLogs, TyroSounds, Melodies, TyroControls, TyroEngines,
  TyroLua, TyroPascal;  //Add all languages units here


type

  { TTyroApplication }

  TTyroApplication = class(TCustomApplication)
  private
    Files: TStringList;
  protected
    procedure SetTitle(const AValue: string); override;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TTyroApplication }

procedure TTyroApplication.SetTitle(const AValue: string);
begin
  inherited SetTitle(AValue);
  Main.Title := AValue;
end;

var
  OwnConsole: Boolean = False;
  OwnConsoleAllocated: Boolean = False;

function OpenConsole(Force: Boolean): Boolean;
begin
  if not IsConsole then
  begin
    if AttachConsole(ATTACH_PARENT_PROCESS) then
    begin
      OwnConsole := True;
      StdOutputHandle := THandle(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
      Assign(Output, '');
      Rewrite(Output);
      TextRec(Output).Handle := StdOutputHandle;

      StdErrorHandle := THandle(GetStdHandle(cardinal(STD_ERROR_HANDLE)));
      Assign(ErrOutput, '');
      Rewrite(ErrOutput);
      TextRec(ErrOutput).Handle := StdErrorHandle;

      IsConsole := True;
    end
    else if Force then
    begin
      OwnConsole := True;
      OwnConsoleAllocated := AllocConsole;

      StdOutputHandle := THandle(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
      Assign(Output, '');
      Rewrite(Output);
      TextRec(Output).Handle := StdOutputHandle;

      StdErrorHandle := THandle(GetStdHandle(cardinal(STD_ERROR_HANDLE)));
      Assign(ErrOutput, '');
      Rewrite(ErrOutput);
      TextRec(ErrOutput).Handle := StdErrorHandle;

      IsConsole := True;
    end;
  end;
  Result := IsConsole;
end;

procedure CloseConsole;
begin
  if OwnConsole then
  begin
    Flush(Output);
    Close(Output);
    Close(ErrOutput);
  end;
  if OwnConsoleAllocated then
    FreeConsole;
end;

//-w my_workpath ../demos/sin.lua

constructor TTyroApplication.Create(AOwner: TComponent);
var
  WorkPaths: TStringArray;
  err: string;
const
  cShortOptions = 'w:d:c';
  cLongOptions = 'workpath:debug:console';
begin
  inherited Create(AOwner);
  Files := TStringList.Create;
  err := CheckOptions(cShortOptions, cLongOptions);

  OpenConsole(HasOption('c', 'console'));
  if IsConsole then
    WriteLn('Starting');

  if err <> '' then
  begin
    if IsConsole then
      WriteLn(err);
    Terminate;
    exit;
  end;

  Main.Title := 'Tyro';

  //w workpath, d socket
  GetNonOptions(cShortOptions, ['workpath:', 'debug', 'console'], Files);
  if Files.Count > 0 then
    Main.FileName := Files[0];
  WorkPaths := GetOptionValues('w', 'workpath');
  if Length(WorkPaths) > 0  then
    Main.WorkSpace := WorkPaths[0]
  else
    Main.WorkSpace := Location;

  InstallConsoleLog;
  Main.Start;
end;

destructor TTyroApplication.Destroy;
begin
  FreeAndNil(Files);
  CloseConsole;
  inherited;
end;

procedure TTyroApplication.DoRun;
begin
  inherited;
  try
    CheckSynchronize;
    Main.Run;
    if not Main.Active then
    begin
      Main.Stop;
      Terminate;
    end;
  finally
  end;
end;

var
  Application: TTyroApplication;

{$R *.res}

begin
  Application := TTyroApplication.Create(nil);
  Application.Title :='Tyro';
  Application.Run;
  Application.Free;
end.
