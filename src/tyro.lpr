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
    Fonts
    https://opengameart.org/content/the-collection-of-8-bit-fonts-for-grafx2
     http://www.pentacom.jp/pentacom/bitfontmaker2/gallery/?page=1&order=&
     http://orangetide.com/OLD/fonts/DOS/


   For Sound
     https://sourceforge.net/projects/tralala/
     https://sourceforge.net/projects/mytralala/

   //ref https://github.com/raysan5/raylib/blob/master/examples/textures/textures_mouse_painting.c

   Console
    https://kriscode.blogspot.com/2018/02/console-vs-gui-application-in-op.html
 *}

{$mode objfpc}
{$modeswitch advancedrecords}
{$H+}

uses
  Windows,
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, CustApp, RayLib, mnUtils, Melodies,
  TyroControls, TyroClasses, TyroEditors, mnLogs, TyroEngines,
  TyroLua, TyroScripts, TyroTerminal, LuaClasses, LuaAPI;  //Add all languages units here

type

  { TTyroApplication }

  TTyroApplication = class(TCustomApplication)
  private
    MainOptions: TTyroMainOptions;
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

procedure PrintHelp;
begin
  WriteLn('Tyro to run script in graphical mode with media function, using RayLib library, for learning proramming languages students and kids.');
  WriteLn('usage: tyro [<script>] [--workpath=<workpath>] [<options>]');
  WriteLn('');
  WriteLn('--help -h              Show this help page');
  WriteLn('--console -c           Show command prompt');
  WriteLn('--terminal -t          Show terminal');
  WriteLn('--debug -d             Run in debug mode');
  WriteLn('--window -w            Show window');
end;

//-w my_workpath ../demos/sin.lua

// Syntax-check a script file with a fresh Lua state without executing it.
// Returns True when the chunk compiles (no window is opened). Errors are
// written to the console; a nonzero process exit status is left to the caller.
function LintLua(const AFileName: string): Boolean;
var
  L: Plua_State;
  Text: TStringList;
  Msg: string;
begin
  Result := False;
  L := nil;
  Text := nil;
  try
    Text := TStringList.Create;
    try
      Text.LoadFromFile(AFileName);
      L := LuaAPI.luaL_newstate;
      if L = nil then
      begin
        WriteLn('Unable to create Lua state');
        Exit;
      end;
      // Compile-only check: opening the standard libraries is not required.
      if LuaAPI.luaL_loadstring(L, PUTF8Char(Text.Text)) <> 0 then
      begin
        Msg := LuaAPI.lua_tostring(L, -1);
        LuaAPI.lua_pop(L, 1);
        WriteLn(AFileName + ': ' + Msg);
        Exit;
      end;
      WriteLn(AFileName + ': syntax OK');
      Result := True;
    except
      on E: Exception do
      begin
        if IsConsole then
          WriteLn(AFileName + ': ' + E.ClassName + ': ' + E.Message);
      end;
    end;
  finally
    if L <> nil then
      LuaAPI.lua_close(L);
    if Text <> nil then
      Text.Free;
  end;
end;

constructor TTyroApplication.Create(AOwner: TComponent);
var
  WorkPaths: TStringArray;
  err: string;
  RunConsole: Boolean;
const
  cShortOptions = 'p:dcwh';
  cLongOptions = 'workpath: debug console window help';
begin
  inherited Create(AOwner);
  Files := TStringList.Create;
  err := CheckOptions(cShortOptions, cLongOptions);

  RunConsole := HasOption(#0, 'console') or HasOption('c', '');
  OpenConsole((err <> '') or RunConsole);

  if err <> '' then
  begin
    if IsConsole then
      WriteLn(err);
    PrintHelp;
    ExitCode := 2;
    Terminate;
    exit;
  end;

  if RunConsole then
    InstallConsoleLog;

  if HasOption(#0, 'debug') or HasOption('d', '') then
    IsDebug := True;

  if HasOption(#0, 'help') or HasOption('h', '') then
  begin
    PrintHelp;
    Terminate;
    exit;
  end;

  Main.Title := 'Tyro';

  //w workpath, d socket
  GetNonOptions(cShortOptions, ['workpath:', 'debug', 'console', 'window', 'help', 'list'], Files);
  if Files.Count > 0 then
    Main.RunFile := Files[0];
  WorkPaths := GetOptionValues('p', 'workpath');
  if Length(WorkPaths) > 0  then
    Resources.WorkSpace := WorkPaths[0]
  else
    Resources.WorkSpace := Location;

  if IsConsole then
  begin
    Write('Starting');
    Write(' ' + CollectStrings(Files));
    Write(' ' + CollectStrings(WorkPaths));
    WriteLn();
  end;
  Main.RunInMain := HasOption(#0, 'main') or HasOption('m', '');
  // --main is retained for command-line compatibility. Raylib and control
  // calls must remain on the application thread, so scripts still use the
  // worker plus its main-thread dispatch queue.
  if HasOption(#0, 'window') or HasOption('w', '') then
    MainOptions := MainOptions + [moMainWindow];
  if HasOption(#0, 'terminal') or HasOption('t', '') then
    MainOptions := MainOptions + [moTerminal];
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
  if Terminated then
    Exit;
  try
    try
      Main.Run(MainOptions);
    except
      on E: Exception do
      begin
        if IsConsole then
          WriteLn('EX: ' + E.ClassName + ': ' + E.Message + ' @' + IntToHex(NativeUInt(ExceptAddr), 16));
        ExitCode := 1;
      end;
    end;
  finally
    // A failed script under --exit/--execute becomes a nonzero exit status.
    // Interactive sessions only report through the console and keep running.
    if (ExitCode = 0) and Main.ScriptFailed then
      ExitCode := 1;
    Terminate;
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
