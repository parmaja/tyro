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
  SysUtils, Classes, RayLib, mnUtils, mnConfigs, StrUtils,
  Melodies, TyroControls, TyroClasses, mnLogs, TyroEngines,
  TyroLua, TyroFuse;

type

  { TTyroApplication }

  TTyroApplication = class(TObject)
  private
    FTitle: string;
    FLocation: string;
    FArguments: TConfFile;
    FTerminated: Boolean;
    MainOptions: TTyroEngineOptions;
    procedure DoFuse(const APathTo, AZipFile: string);
  protected
    procedure SetTitle(const AValue: string);
    procedure DoRun;
  public
    RunConsole: Boolean;
    //* True when the process handled a console-only command (--help, --fuse)
    //* and never created the engine; such runs must exit without running the
    //* engine-related unit finalizations.
    Engineless: Boolean;

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
  if Engine <> nil then
    Engine.Title := AValue;
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
  begin
    exit(True);
  end;

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
    begin
      FreeConsole;
    end;
  end;
  if Force then
  begin
    OwnConsole := True;
    OwnConsoleAllocated := AllocConsole;
    AssignHandles;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
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
  WriteLn('       tyro --fuse <path> <zip>');
  WriteLn('');
  WriteLn('--help -h              Show this help page');
  WriteLn('--console -c           Show command prompt');
  WriteLn('--terminal -t          Show terminal');
  WriteLn('--debug -d             Run in debug mode');
  WriteLn('--exit[:N]             Exit after N seconds, or when the script ends');
  WriteLn('--screenshot:N         Save a screenshot after N seconds');
  WriteLn('--fuse <path> <zip>    Pack <path> with engine files into a fuse zip package');
  //WriteLn('--window -w            Show window');
end;

//* Look up a command line switch that may carry a numeric value, accepting
//* both the "-name:value" and "-name=value" forms (and the --name and /name
//* spellings). Returns True when the switch is present; AValue is '' for a
//* bare switch. The mini library argument parser only splits on '=', so the
//* ':' form used by --exit:N/--screenshot:N is handled here.
function ReadValueSwitch(const AName: string; out AValue: string): Boolean;
var
  i: Integer;
  S, N: string;
begin
  Result := False;
  AValue := '';
  for i := 1 to ParamCount do
  begin
    S := ParamStr(i);
    N := S;
    if StartsText('/', N) then
      N := '-' + Copy(N, 2, MaxInt)
    else if StartsText('--', N) then
      N := Copy(N, 2, MaxInt);
    if SameText(N, AName) then
      Exit(True);
    if StartsText(AName + ':', N) or StartsText(AName + '=', N) then
    begin
      AValue := Copy(N, Length(AName) + 2, MaxInt);
      Exit(True);
    end;
  end;
end;

//* Looks up the --fuse switch and its two arguments. The mini library
//* argument parser keeps a single valueless positional argument while fuse
//* takes two, so they are read directly from the command line. Both the
//* "--fuse <path> <zip>" and "--fuse=<path> <zip>" forms are accepted.
//* Returns True when the switch is present; the outputs stay '' when an
//* argument is missing, the caller reports the usage error.
function ReadFuseArguments(out APathTo, AZipFile: string): Boolean;
var
  i: Integer;
  S, N: string;
begin
  Result := False;
  APathTo := '';
  AZipFile := '';
  for i := 1 to ParamCount do
  begin
    S := ParamStr(i);
    N := S;
    if StartsText('/', N) then
      N := '-' + Copy(N, 2, MaxInt)
    else if StartsText('--', N) then
      N := Copy(N, 2, MaxInt);
    if SameText(N, '-fuse') then
    begin
      Result := True;
      if i + 1 <= ParamCount then
        APathTo := ParamStr(i + 1);
      if i + 2 <= ParamCount then
      begin
        AZipFile := ParamStr(i + 2);
        AZipFile := SysUtils.ChangeFileExt(AZipFile, '.zip');
      end;
      exit;
    end;
    if StartsText('-fuse=', N) or StartsText('-fuse:', N) then
    begin
      Result := True;
      APathTo := Copy(N, Length('-fuse') + 2, MaxInt);
      if i + 1 <= ParamCount then
        AZipFile := ParamStr(i + 1);
      exit;
    end;
  end;
end;

//* Packs the game given by --fuse together with the engine files listed in
//* release.conf (next to tyro.exe) into the target zip, reports to the
//* console and sets a failing exit code when the package was not written.
procedure TTyroApplication.DoFuse(const APathTo, AZipFile: string);
var
  aConfFile: string;
  aMessage: string;
begin
  aConfFile := FLocation + 'release.conf';
  if not SysUtils.FileExists(aConfFile) then
    aConfFile := 'release.conf';
  WriteLn('fusing ' + APathTo + ' into ' + AZipFile);
  if FusePackage(APathTo, AZipFile, aConfFile, aMessage) then
  begin
    if aMessage <> '' then
      WriteLn(aMessage);
    WriteLn('done: ' + AZipFile);
  end
  else
  begin
    WriteLn('fuse failed: ' + aMessage);
    ExitCode := 1;
  end;
end;

constructor TTyroApplication.Create;
var
  LogLevel: TLogLevel;
  s: string;
  ScriptFile: string;
  i: Integer;
  aFuse: Boolean;
  aFusePath: string;
  aFuseZip: string;
  aHelp: Boolean;
begin
  inherited;
  FLocation := ExtractFilePath(ParamStr(0));
  FArguments := ParseArguments([]);

  //--fuse packs a game with the engine into a distributable zip, it needs
  //the console for its report and quits without creating the engine.
  aFuse := ReadFuseArguments(aFusePath, aFuseZip);

  //Help prints to the console too; without this it would write to the
  //never-opened GUI stdout and fail.
  aHelp := Arguments.ReadSwitch('-help') or Arguments.ReadSwitch('-h');

  RunConsole := Arguments.ReadSwitch('-console') or Arguments.ReadSwitch('-c') or aFuse or aHelp;
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

  if aHelp then
  begin
    PrintHelp;
    Engineless := True;
    Terminate;
    exit;
  end;

  if aFuse then
  begin
    if (aFusePath = '') or (aFuseZip = '') then
    begin
      WriteLn('usage: tyro --fuse <path> <zip>');
      ExitCode := 1;
    end
    else
      DoFuse(aFusePath, aFuseZip);
    Engineless := True;
    Terminate;
    exit;
  end;

  Engine := TTyroEngine.Create;

  Engine.Title := Title;

  ScriptFile := CorrectPath(ExpandToPath(Arguments.ReadString(''), Res.WorkPath)); //File come without switch name

  if (ScriptFile <> '') and (SysUtils.FileExists(ScriptFile) or SysUtils.DirectoryExists(ScriptFile)) then
    Engine.ScriptFile := ScriptFile;

  if (Engine.ScriptFile = '') or SysUtils.DirectoryExists(ScriptFile) or Arguments.ReadSwitch('-window', true) or Arguments.ReadSwitch('-w', true) then
    MainOptions := MainOptions + [moMainWindow];
  if Arguments.ReadSwitch('-terminal') or Arguments.ReadSwitch('-t') then
    MainOptions := MainOptions + [moShowTerminal];

  //--exit:N ends the session N seconds into the run; a bare --exit ends it as
  //soon as the main script finishes. Either way the process quits by itself
  //instead of waiting for the window to be closed.
  if ReadValueSwitch('-exit', s) then
  begin
    i := StrToIntDef(s, 0);
    if i > 0 then
      Engine.ExitAfterSeconds := i
    else
      Engine.ExitAfterScript := True;
  end;

  //--screenshot:N captures the screen N seconds in. Drawing, and therefore the
  //capture, needs a visible window, so ask for one.
  if ReadValueSwitch('-screenshot', s) then
  begin
    i := StrToIntDef(s, 0);
    if i > 0 then
    begin
      Engine.ScreenshotAfterSeconds := i;
      MainOptions := MainOptions + [moMainWindow];
    end;
  end;
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
      Engine.Run(MainOptions);
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
    if (ExitCode = 0) and Engine.ScriptFailed then
      ExitCode := 1;
  end;
end;

procedure TTyroApplication.Terminate;
begin
  FTerminated := True;
end;

end.

