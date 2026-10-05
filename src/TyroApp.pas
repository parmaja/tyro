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
  TyroLua;

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
  if Main <> nil then
    Main.Title := AValue;
end;

{$ifdef MSWINDOWS}
var
  OwnConsole: Boolean = False;
  OwnConsoleAllocated: Boolean = False;

function OpenConsole(Force: Boolean): Boolean;

  procedure SetTextHandle(var T:Text; H: THandle);
  begin
    {$ifdef FPC}
    TextRec(T).Handle := H;
    {$else}
    TTextRec(T).Handle := H;
    {$endif}
  end;

  procedure AssignHandles;
  var
    aStdInputHandle: THandle;
    aStdOutputHandle: THandle;
    aStdErrorHandle: THandle;
  begin
    aStdInputHandle := THandle(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
    {$ifdef FPC}
    StdInputHandle := aStdInputHandle;
    {$endif}
    Assign(Input, 'CONOUT$'); //'CONOUT$'
    Reset(Input);
    SetTextHandle(Input, aStdInputHandle);

    aStdOutputHandle := THandle(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
    {$ifdef FPC}
    StdOutputHandle := aStdOutputHandle;
    {$endif}
//  SetStdHandle(STD_OUTPUT_HANDLE, CreateFile('CONOUT$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0));
    Assign(Output, ''); //'CONOUT$'
    Rewrite(Output);
    SetTextHandle(Output, aStdOutputHandle);

    aStdErrorHandle := THandle(GetStdHandle(cardinal(STD_ERROR_HANDLE)));
    {$ifdef FPC}
    StdErrorHandle := aStdErrorHandle;
    {$endif}
//  SetStdHandle(STD_ERROR_HANDLE, CreateFile('CONOUT$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0));
    Assign(ErrOutput, ''); //'CONOUT$'
    Rewrite(ErrOutput);
    SetTextHandle(ErrOutput, aStdErrorHandle);

    {$ifdef FPC}
    IsConsole := True;
    {$endif}
  end;
var
  ConsoleWindow: THandle;
  IsVisible: Boolean;
begin
  if IsConsole then
    exit(True);

  if AttachConsole(ATTACH_PARENT_PROCESS) then
  begin
    ConsoleWindow := GetConsoleWindow;
    //Delphi is debugging Win64 with hidden console :(
    IsVisible :=  (ConsoleWindow <> 0) and IsWindowVisible(ConsoleWindow);
    if IsVisible then
    begin
      OwnConsole := True;
      AssignHandles;
      Result := True;
      exit;
    end
    else if Force then
      FreeConsole;
  end;
  if Force then
  begin
    OwnConsole := True;
    OwnConsoleAllocated := AllocConsole;
    AssignHandles;
    Result := True;
  end
  else
    Result := False;
end;

procedure CloseConsole;
begin
  if OwnConsole then
  begin
    Flush(Output);
    CloseFile(Output);
    CloseFile(ErrOutput);
    OwnConsole := False;
  end;

  if OwnConsoleAllocated then
  begin
    FreeConsole;
    OwnConsoleAllocated := False; // Reset state
  end;
end;
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
var
  LogLevel: TLogLevel;
  s: string;
  ScriptFile: string;
begin
  inherited;
  FLocation := ExtractFilePath(ParamStr(0));
  FArguments := ParseArguments([]);

  RunConsole := Arguments.ReadSwitch('-console') or Arguments.ReadSwitch('-c');
  OpenConsole(RunConsole);
  RunConsole := RunConsole or IsConsole;
  if RunConsole then
  begin
    WriteLn('Tyro version 0.1');
    InstallConsoleLog;
  end;

  LogLevel := lglInfo;
  if (Arguments.ReadSwitch('-log') or Arguments.ReadSwitch('-loglevel')) then
  begin
    s := Arguments.ReadString('-loglevel');
    if s <> '' then
      LogLevel := StrToLogLevel(s);
  end;
  Log.Install(LogLevel, TTyroConsoleLog.Create);

  if Arguments.ReadSwitch('-debug') or Arguments.ReadSwitch('-d') then
    IsDebug := True;

  if Arguments.ReadSwitch('-help') or Arguments.ReadSwitch('-h') then
  begin
    PrintHelp;
    Terminate;
    exit;
  end;

  Main := TTyroMain.Create;

  Main.Title := Title;

  Res.WorkPath := Arguments.ReadPath('-workpath', Location);

  ScriptFile := CorrectPath(ExpandToPath(Arguments.ReadString(''), Res.WorkPath)); //File come without switch name

  if SysUtils.FileExists(ScriptFile) or SysUtils.DirectoryExists(ScriptFile)  then
    Main.ScriptFile := ScriptFile;

  if (Main.ScriptFile = '') or SysUtils.DirectoryExists(ScriptFile) or Arguments.ReadSwitch('-window', true) or Arguments.ReadSwitch('-w', true) then
    MainOptions := MainOptions + [moMainWindow];
  if Arguments.ReadSwitch('-terminal') or Arguments.ReadSwitch('-t') then
    MainOptions := MainOptions + [moShowTerminal];
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

