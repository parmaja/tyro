program test_rerun_editor;

{ Headless test for the two keyboard round trips of TTyroMain:
    F5  reruns the script that is loaded
    F2  shows the editor on that script's source, and hiding the editor saves
        the source back and reruns it, exactly like F5
  and for what a rerun has to clean up: the controls and the sprites the finished
  run left behind (only the console, output, editor and file picker survive) and
  the pixels it left on the board and window canvases.

  Build and run:
    lazbuild --build-mode=Debug tests\test_rerun_editor.lpi
    bin\test_rerun_editor.exe [tests directory]

  rerun_target.lua appends one line per run to rerun_target.log, so the test can
  count exactly how often the engine started the loaded script and which source it
  ran, and it leaves one focused control and one named sprite per run. The
  shortcuts themselves are one line each in TTyroMain.ProcessInput; what they call
  (RunScriptThread, ShowEditor, HideEditor) is driven here through the same entry
  points, on a real TTyroMain with no window.

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
  RayLib,
  TyroClasses, TyroScripts, TyroControls, TyroLua, TyroEngines;

const
  cScriptName = 'rerun_target.lua';
  cLogName = 'rerun_target.log';

type
  { TFakeCanvas }

  { A canvas that records what the engine does to it instead of drawing. Headless
    there is no graphics context, and what matters here is the order of the calls,
    not their pixels: a texture canvas only receives its clear between BeginDraw
    and EndDraw. ClearBackground is the virtual that TTyroCanvas.Clear ends up in,
    and it deliberately does not call inherited, which would reach for the real
    framebuffer. }

  TFakeCanvas = class(TTyroCanvas)
  public
    Steps: string;
    procedure BeginDraw; override;
    procedure EndDraw; override;
    procedure ClearBackground(const AColor: TColor); override;
  end;

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
    //The names of the window children the engine did not create itself, i.e. the
    //controls a script run left behind
    function ScriptControlNames: TStringList;
    //True while the console, output, editor and file picker are still parented to
    //the window, i.e. a rerun left the controls of the engine alone
    function EngineControlsIntact: Boolean;
    //The first control a script left in the window, nil when there is none
    function FirstScriptControl: TTyroControl;
    //Drop the script controls the way a rerun does
    procedure ClearControls;
    procedure Command(const ACommand: string);
    //Wait for the worker to publish completion, pumping its callbacks meanwhile
    procedure WaitRun;
  end;

procedure TFakeCanvas.BeginDraw;
begin
  Steps := Steps + 'B';
end;

procedure TFakeCanvas.EndDraw;
begin
  Steps := Steps + 'E';
end;

procedure TFakeCanvas.ClearBackground(const AColor: TColor);
begin
  Steps := Steps + 'C';
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

function TTestMain.ScriptControlNames: TStringList;
var
  i: Integer;
  aControl: TTyroLayout;
begin
  Result := TStringList.Create;
  for i := 0 to Controls.Count - 1 do
  begin
    aControl := Controls[i];
    if (aControl <> Console) and (aControl <> Output) and (aControl <> Editor) and (aControl <> FFileList) then
      Result.Add(aControl.Name);
  end;
end;

function TTestMain.EngineControlsIntact: Boolean;
begin
  Result := (Console.Parent = Self) and (Output.Parent = Self) and
    (Editor.Parent = Self) and (FFileList.Parent = Self);
end;

function TTestMain.FirstScriptControl: TTyroControl;
var
  i: Integer;
  aControl: TTyroLayout;
begin
  Result := nil;
  for i := 0 to Controls.Count - 1 do
  begin
    aControl := Controls[i];
    if (aControl is TTyroControl) and not
      ((aControl = Console) or (aControl = Output) or (aControl = Editor) or (aControl = FFileList)) then
    begin
      Result := TTyroControl(aControl);
      Exit;
    end;
  end;
end;

procedure TTestMain.ClearControls;
begin
  ClearScriptControls;
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
  aFile, aRun, aTemplate, aEditor, aNames: TStringList;
  aBoard, aWindow: TFakeCanvas;
  aCtrl: TTyroControl;
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
  if not SysUtils.FileExists(AFileName) then
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

//"2 [ctrlv1, ctrlv2]" for a failure message: the count first, because a control
//without a name would print as nothing at all
function Named(const ALines: TStringList): string;
begin
  Result := IntToStr(ALines.Count) + ' [' + ALines.CommaText + ']';
end;

procedure FreeAll;
begin
  FreeAndNil(aRun);
  FreeAndNil(aTemplate);
  FreeAndNil(aEditor);
  FreeAndNil(aNames);
  FreeAndNil(aFile);
end;

begin
  aTestDir := IncludePathDelimiter(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'tests');
  if ParamCount > 0 then
    aTestDir := ParamStr(1);
  aTestDir := IncludePathDelimiter(ExcludeTrailingPathDelimiter(ExpandFileName(aTestDir)));
  aLogFile := aTestDir + cLogName;

  if not SysUtils.FileExists(aTestDir + cScriptName) then
  begin
    WriteLn('Cannot find ', cScriptName, ' in ', aTestDir);
    Halt(2);
  end;

  WriteLn('F5 rerun / F2 editor round trip');
  WriteLn('  tests in ', aTestDir);
  WriteLn;

  //The script writes its log next to itself
  SetCurrentDir(aTestDir);
  SysUtils.DeleteFile(aLogFile);

  aFile := TStringList.Create;
  aFile.LoadFromFile(aTestDir + cScriptName);

  aMain := TTestMain.Create;
  //The engine reaches the window through the global Main (TTyroApplication
  //publishes it there), and that is also what a script uses to parent the
  //controls it creates.
  Main := aMain;
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
    FreeAndNil(aNames);
    aNames := aMain.ScriptControlNames;
    Check('the run leaves its control in the window', SameLines(aNames, LinesOf('ctrlv1')),
      Named(aNames));
    Check('the run focus lands on that control',
      (aMain.FocusedControl <> nil) and (aMain.FocusedControl.Name = 'ctrlv1'));
    Check('the run leaves its sprite in the store', aMain.Sprites.Count = 1,
      IntToStr(aMain.Sprites.Count));

    //3. F5: the loaded script runs again, on a fresh worker
    aMain.Command('run');
    aMain.WaitRun;
    aRun := ReadLog(aLogFile);
    Check('F5 reruns the loaded script', aRun.Count = 2, Logged(aRun));
    FreeAndNil(aNames);
    aNames := aMain.ScriptControlNames;
    //One control, not two: the one the finished run left is gone and only the one
    //the rerun just created is left.
    Check('F5 drops the controls of the finished run', SameLines(aNames, LinesOf('ctrlv1')),
      Named(aNames));
    Check('F5 leaves the engine controls alone', aMain.EngineControlsIntact);
    //One sprite, not two, and it belongs to the run that just happened
    Check('F5 drops the sprites of the finished run',
      (aMain.Sprites.Count = 1) and (aMain.Sprites.FindByName('spv1') > 0),
      IntToStr(aMain.Sprites.Count));

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
    FreeAndNil(aNames);
    aNames := aMain.ScriptControlNames;
    Check('the rerun after editing left only the new control',
      SameLines(aNames, LinesOf('ctrlv2')), Named(aNames));
    //The sprite of the previous tag is gone, not merely outnumbered
    Check('the rerun after editing left only the new sprite',
      (aMain.Sprites.Count = 1) and (aMain.Sprites.FindByName('spv1') = 0) and
      (aMain.Sprites.FindByName('spv2') > 0), IntToStr(aMain.Sprites.Count));

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

    //7. a rerun wipes both canvases, inside BeginDraw/EndDraw
    aBoard := TFakeCanvas.Create(8, 8);
    aWindow := TFakeCanvas.Create(8, 8);
    aMain.Board := aBoard;
    aMain.Canvas := aWindow;
    aMain.Command('run');
    aMain.WaitRun;
    //'BCE': bind the render texture, clear it, release it again. A clear outside
    //that pair would wipe the window backbuffer and leave the canvas as it was.
    Check('a rerun clears the board canvas', aBoard.Steps = 'BCE', aBoard.Steps);
    Check('a rerun clears the window canvas', aWindow.Steps = 'BCE', aWindow.Steps);
    Check('a rerun leaves the engine controls alone', aMain.EngineControlsIntact);

    //8. who keeps the keyboard focus when the script controls go away
    aMain.ShowConsole;
    Check('the console takes the focus', aMain.FocusedControl = aMain.Console);
    aMain.ClearControls;
    Check('a rerun keeps the focus on an engine control',
      aMain.FocusedControl = aMain.Console);
    aMain.HideConsole;
    //A control of the script that holds the focus goes with it: the engine has to
    //drop the focus before the control is freed, since releasing it touches the
    //control being replaced.
    aMain.Command('run');
    aMain.WaitRun;
    aCtrl := aMain.FirstScriptControl;
    Check('the finished run left a control to focus', aCtrl <> nil);
    if aCtrl <> nil then
    begin
      aMain.FocusedControl := aCtrl;
      aMain.ClearControls;
      Check('a rerun takes the focus with the control', aMain.FocusedControl = nil);
    end;
  finally
    //The fakes are owned by the window now (its Canvas and its Board), so they go
    // with it.
    Main := nil;
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