program test_rerun_editor;

{ Headless test for the two keyboard round trips of TTyroMain:
    F5  reruns the script that is loaded
    F2  shows the editor on that script's source, and hiding the editor saves
        the source back and reruns it, exactly like F5

  Build and run:
    lazbuild --build-mode=Debug tests\test_rerun_editor.lpi
    bin\test_rerun_editor.exe [tests directory]

  rerun_target.lua appends one line per run to rerun_target.log, so the test can
  count exactly how often the engine started the loaded script and which source it
  ran. The shortcuts themselves are one line each in TTyroMain.ProcessInput; what
  they call (RunScriptThread, ShowEditor, HideEditor) is driven here through the
  same entry points, on a real TTyroMain with no window.

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  mnLogs, mnUtils, 
  TyroClasses, TyroScripts, TyroControls, TyroLua, TyroEngines;

const
  cScriptName = 'rerun_target.lua';
  cLogName = 'rerun_target.log';

type
  { TTestMain }

  { The script template, the worker and the command executor live in protected and
    private sections of TTyroMain, so the test reaches them through a descendant,
    which is the supported way in. }

  TTestMain = class(TTyroMain)
  public
    //True while a worker exists (running or finished, not yet released)
    function HasRun: Boolean;
    //True when there is nothing left to wait for
    function RunDone: Boolean;
    //A copy of the loaded script source (the editable template)
    function TemplateLines: TStringList;
    //A copy of what the editor shows
    function EditorLines: TStringList;
    procedure Command(const ACommand: string);
    //Wait for the worker to publish completion, pumping its callbacks meanwhile
    procedure WaitRun;
  end;

function TTestMain.HasRun: Boolean;
begin
  Result := FScriptThread <> nil;
end;

function TTestMain.RunDone: Boolean;
begin
  Result := (FScriptThread = nil) or FScriptThread.Completed;
end;

function TTestMain.TemplateLines: TStringList;
begin
  Result := TStringList.Create;
  if FScriptMain <> nil then
    Result.Assign(FScriptMain.Source);
end;

function TTestMain.EditorLines: TStringList;
begin
  Result := TStringList.Create;
  if Editor <> nil then
    Editor.SaveSource(Result);
end;

procedure TTestMain.Command(const ACommand: string);
begin
  ExecuteCommand(ACommand);
end;

procedure TTestMain.WaitRun;
var
  Waited: Integer;
begin
  Waited := 0;
  while (not RunDone) and (Waited < 10000) do
  begin
    CheckSynchronize(10);
    Inc(Waited, 10);
  end;
end;

var
  aMain: TTestMain;
  aTestDir, aLogFile: string;
  aFile, aRun, aTemplate, aEditor: TStringList;
  Tests, Failures: Integer;

procedure Check(const AName: string; AOk: Boolean; const ADetail: string = '');
begin
  Inc(Tests);
  if AOk then
    WriteLn('  PASS  ', AName)
  else
  begin
    Inc(Failures);
    WriteLn('  FAIL  ', AName, '  <', ADetail, '>');
  end;
end;

//Two lists hold the same text when they hold the same source line by line
function SameLines(A, B: TStringList): Boolean;
var
  I: Integer;
begin
  Result := (A <> nil) and (B <> nil) and (A.Count = B.Count);
  if not Result then
    Exit;
  for I := 0 to A.Count - 1 do
    if A[I] <> B[I] then
    begin
      Result := False;
      Exit;
    end;
end;

function LinesOf(const AText: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := AText;
end;

//What the log holds after the runs so far
function ReadLog(const AFileName: string): TStringList;
begin
  Result := TStringList.Create;
  if not FileExists(AFileName) then
    Exit;
  try
    Result.LoadFromFile(AFileName);
  except
    on E: Exception do
    begin
      Result.Clear;
      Result.Add('<unreadable: ' + E.Message + '>');
    end;
  end;
end;

//'v1' / 'v2' as they appear in the Lua source
function Quoted(const AText: string): string;
begin
  Result := Chr(39) + AText + Chr(39);
end;

//The source with another tag, i.e. what a user would type into the editor
function Tagged(const ATag: string): TStringList;
begin
  Result := LinesOf(StringReplace(aFile.Text, Quoted('v1'), Quoted(ATag),
    [rfReplaceAll]));
end;

//"3 line(s), last is <tag>" for a failure message
function Logged(const ALines: TStringList): string;
begin
  Result := IntToStr(ALines.Count) + ' line(s)';
  if ALines.Count > 0 then
    Result := Result + ', last is ' + ALines[ALines.Count - 1];
end;

procedure FreeAll;
begin
  FreeAndNil(aRun);
  FreeAndNil(aTemplate);
  FreeAndNil(aEditor);
  FreeAndNil(aFile);
end;

begin
  aTestDir := IncludePathDelimiter(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'tests');
  if ParamCount > 0 then
    aTestDir := ParamStr(1);
  aTestDir := IncludePathDelimiter(ExcludeTrailingPathDelimiter(ExpandFileName(aTestDir)));
  aLogFile := aTestDir + cLogName;

  if not FileExists(aTestDir + cScriptName) then
  begin
    WriteLn('Cannot find ', cScriptName, ' in ', aTestDir);
    Halt(2);
  end;

  WriteLn('F5 rerun / F2 editor round trip');
  WriteLn('  tests in ', aTestDir);
  WriteLn;

  //The script writes its log next to itself
  SetCurrentDir(aTestDir);
  DeleteFile(aLogFile);

  aFile := TStringList.Create;
  aFile.LoadFromFile(aTestDir + cScriptName);

  aMain := TTestMain.Create;
  try
    Res.WorkPath := aTestDir;

    //1. load: the script becomes the editable template and stays unstarted
    aMain.Command('load ' + cScriptName);
    aTemplate := aMain.TemplateLines;
    Check('load reads the script source', SameLines(aTemplate, aFile));
    Check('load leaves the script not started', not aMain.HasRun);

    //2. run: the first execution, the same call F5 makes
    aMain.Command('run');
    Check('run starts a worker', aMain.HasRun);
    aMain.WaitRun;
    aRun := ReadLog(aLogFile);
    Check('run executes the loaded script once', aRun.Count = 1, Logged(aRun));

    //3. F5: the loaded script runs again, on a fresh worker
    aMain.Command('run');
    aMain.WaitRun;
    aRun := ReadLog(aLogFile);
    Check('F5 reruns the loaded script', aRun.Count = 2, Logged(aRun));

    //4. F2: the editor opens on the source of the loaded script, the run stops
    aMain.ShowEditor;
    aEditor := aMain.EditorLines;
    Check('F2 shows the editor', aMain.Editor.Visible);
    Check('F2 stops the running script', not aMain.HasRun);
    Check('F2 takes the source from the loaded script', SameLines(aEditor, aFile));

    //5. edit the buffer and hide the editor: the source is saved back and rerun
    aMain.Editor.LoadSource(Tagged('v2'));
    aMain.HideEditor;
    aMain.WaitRun;
    Check('hiding the editor hides it', not aMain.Editor.Visible);
    FreeAndNil(aTemplate);
    aTemplate := aMain.TemplateLines;
    Check('hiding the editor saves the buffer back to the script',
      SameLines(aTemplate, Tagged('v2')));
    aRun := ReadLog(aLogFile);
    Check('hiding the editor reruns the edited script',
      (aRun.Count = 3) and (aRun[2] = 'v2'), Logged(aRun));

    //6. F5 while the editor is open runs the buffer, not the older source
    aMain.ShowEditor;
    aMain.Editor.LoadSource(Tagged('v3'));
    aMain.Command('run');
    aMain.WaitRun;
    aRun := ReadLog(aLogFile);
    Check('F5 with the editor open runs the buffer',
      (aRun.Count = 4) and (aRun[3] = 'v3'), Logged(aRun));
    aMain.HideEditor;
    aMain.WaitRun;
    aRun := ReadLog(aLogFile);
    Check('hiding the editor runs that same buffer',
      (aRun.Count = 5) and (aRun[4] = 'v3'), Logged(aRun));
  finally
    FreeAndNil(aMain);
    FreeAll;
  end;

  WriteLn;
  if Failures = 0 then
    WriteLn('OK: ', Tests, ' checks passed (log: ', aLogFile, ')')
  else
    WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks (log: ', aLogFile, ')');
  Halt(Failures);
end.