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
  mnUtils,
  TyroEditors, mnLogs, TyroSounds, Melodies, TyroControls, TyroEngines,
  TyroLua, TyroPascal, TyroScripts;  //Add all languages units here

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

procedure PrintList;
var
  item: TScriptType;
begin
  WriteLn('Languages:');
  for item in Main.ScriptTypes do
  begin
    WriteLn(item.Title + ': ' + item.CollectExtentions);
  end;
end;

procedure PrintHelp;
begin
  WriteLn('Tyro to run script in graphical mode with media function, using RayLib library, for learning proramming languages students and kids.');
  WriteLn('usage: tyro [<script>] [--workpath=<workpath>] [<options>]');
  WriteLn('');
  WriteLn('--help -h              Show this help page');
  WriteLn('--console -c           Force to show command prompt');
  WriteLn('--debug -d             Run in debug mode');
  WriteLn('--lint -l              Lint to check errors only in script do not run');
  WriteLn('--main -m              Execute first script in main loop thread');
  WriteLn('--exit -x              Exit after finish execute');
  WriteLn('--show=true/false -s   Force to show main graphic window');
  WriteLn('--list                 List of programming language supported');
end;

//-w my_workpath ../demos/sin.lua

constructor TTyroApplication.Create(AOwner: TComponent);
var
  WorkPaths: TStringArray;
  err: string;
  RunConsole: Boolean;
const
  cShortOptions = 'w: m e l d c s: h t';
  cLongOptions = 'workpath: main execute lint debug console show: help list';
begin
  inherited Create(AOwner);
  Files := TStringList.Create;
  err := CheckOptions(cShortOptions, cLongOptions);

  RunConsole := HasOption('c', 'console');
  OpenConsole((err <> '') or RunConsole);

  if err <> '' then
  begin
    if IsConsole then
      WriteLn(err);
    PrintHelp;
    Terminate;
    exit;
  end;

  InstallConsoleLog;

  if HasOption('h', 'help') then
  begin
    PrintHelp;
    Terminate;
    exit;
  end;

  if HasOption(#0, 'list') then
  begin
    PrintList;
    Terminate;
    exit;
  end;

  Main.Title := 'Tyro';

  //w workpath, d socket
  GetNonOptions(cShortOptions, [], Files);
  if Files.Count > 0 then
    Main.RunFile := Files[0];
  WorkPaths := GetOptionValues('w', 'workpath');
  if Length(WorkPaths) > 0  then
    Main.WorkSpace := WorkPaths[0]
  else
    Main.WorkSpace := Location;

  if IsConsole then
  begin
    Write('Starting');
    Write(' ' + CollectStrings(Files));
    Write(' ' + CollectStrings(WorkPaths));
    WriteLn();
  end;
  Main.RunInMain := HasOption('m', 'main');
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
  Application.Title := 'Tyro';
  Application.Run;
  Application.Free;
end.
