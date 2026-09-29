unit TyroApp;
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

{$ifdef FPC}
{$mode delphi}
{$modeswitch advancedrecords}
{$endif}
{$H+}

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  SysUtils, Classes, RayLib, mnUtils, mnConfigs,
  Melodies, TyroControls, TyroClasses, mnLogs, TyroEngines,
  LuaAPI;  //Add all languages units here

type

  { TTyroApplication }

  TTyroApplication = class(TObject)
  private
    FTitle: string;
    FLocation: string;
    FArguments: TConfFile;
    FTerminated: Boolean;
    MainOptions: TTyroMainOptions;
  protected
    procedure SetTitle(const AValue: string);
    procedure DoRun;
  public
    RunConsole: Boolean;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Run;
    procedure Terminate;
    property Location: string read FLocation;
    property Arguments: TConfFile read FArguments;
    property Terminated: Boolean read FTerminated;
    property Title: string read FTitle write SetTitle;
  end;

var
  Application: TTyroApplication = nil;

implementation

{ TTyroApplication }

procedure TTyroApplication.SetTitle(const AValue: string);
begin
  FTitle := AValue;;
  Main.Title := AValue;
end;

{$ifdef MSWINDOWS}
var
  OwnConsole: Boolean = False;
  OwnConsoleAllocated: Boolean = False;
{$ifdef FPC}
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
{$else}
function OpenConsole(Force: Boolean): Boolean;
var
  StdOutputHandle: THandle;
  StdErrorHandle: THandle;
  ConsoleWindow: THandle;
begin
  // IsConsole is read-only in Delphi. We check both the system flag and our own tracking variable.
  if not (IsConsole or OwnConsole) then
  begin
    if AttachConsole(ATTACH_PARENT_PROCESS) then
    begin
      WriteLn('AttachConsole');
      OwnConsole := True;

      StdOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
      AssignFile(Output, '');
      Rewrite(Output);
      TTextRec(Output).Handle := StdOutputHandle;

      StdErrorHandle := GetStdHandle(STD_ERROR_HANDLE);
      AssignFile(ErrOutput, '');
      Rewrite(ErrOutput);
      TTextRec(ErrOutput).Handle := StdErrorHandle;
      WriteLn('AttachConsole2');
//      ConsoleWindow := GetConsoleWindow();
//      ShowWindow(ConsoleWindow, SW_SHOW)
    end
    else if Force then
    begin
      OwnConsole := True;
      OwnConsoleAllocated := AllocConsole;

      StdOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
      AssignFile(Output, '');
      Rewrite(Output);
      TTextRec(Output).Handle := StdOutputHandle;

      StdErrorHandle := GetStdHandle(STD_ERROR_HANDLE);
      AssignFile(ErrOutput, '');
      Rewrite(ErrOutput);
      TTextRec(ErrOutput).Handle := StdErrorHandle;
    end;
  end;

  Result := IsConsole or OwnConsole;
end;

procedure CloseConsole;
begin
  if OwnConsole then
  begin
    Flush(Output);
    CloseFile(Output);
    CloseFile(ErrOutput);
    OwnConsole := False; // Reset state
  end;

  if OwnConsoleAllocated then
  begin
    FreeConsole;
    OwnConsoleAllocated := False; // Reset state
  end;
end;
{$endif}

{$endif}

procedure PrintHelp;
begin
  WriteLn('Tyro to run script in graphical mode with media function, using RayLib library, for learning proramming languages students and kids.');
  WriteLn('usage: tyro [<script>] [--workpath=<workpath>] [<options>]');
  WriteLn('');
  WriteLn('--help -h              Show this help page');
  WriteLn('--console -c           Show command prompt');
  WriteLn('--terminal -t          Show terminal');
  WriteLn('--debug -d             Run in debug mode');
  //WriteLn('--window -w            Show window');
end;

constructor TTyroApplication.Create;
begin
  inherited;
  FLocation := ExtractFilePath(ParamStr(0));
  FArguments := ParseArguments([]);

  RunConsole := Arguments.ReadSwitch('-console') or Arguments.ReadSwitch('-c');
  OpenConsole(RunConsole);
  RunConsole := RunConsole or IsConsole;

  if RunConsole then
    InstallConsoleLog;

  if Arguments.ReadSwitch('-debug') or Arguments.ReadSwitch('-d') then
    IsDebug := True;

  if Arguments.ReadSwitch('-help') or Arguments.ReadSwitch('-h') then
  begin
    PrintHelp;
    Terminate;
    exit;
  end;

  Main.Title := 'Tyro';

  Main.ScriptFile := Arguments.ReadString(''); //File come without switch name
  Resources.WorkPath := Arguments.ReadPath('-workpath', Location);

  if RunConsole then
    WriteLn('Starting');
  if Arguments.ReadSwitch('-window', true) or Arguments.ReadSwitch('-w', true) then
    MainOptions := MainOptions + [moMainWindow];
  if Arguments.ReadSwitch('-terminal') or Arguments.ReadSwitch('-t') then
    MainOptions := MainOptions + [moTerminal];
end;

destructor TTyroApplication.Destroy;
begin
  CloseConsole;
  FreeAndNil(FArguments);
  inherited;
end;

procedure TTyroApplication.Run;
begin
  DoRun;
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
        if RunConsole then
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

procedure TTyroApplication.Terminate;
begin
  FTerminated := True;
end;

end.

