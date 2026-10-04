program test_terminal_tab_complete;

{ Headless test for the TAB completion of TTyroTerminal
  (src\tyrolib\TyroTerminal.pas).

  While the caret is inside the first word of the line, TAB completes the builtin
  command names. Once the caret left it, TAB completes the files of the workspace
  (Res.WorkPath) that match one of FileMasks. A second TAB walks to the next
  candidate and cycles back to the first after the last one. A word with no
  candidate at all still inserts a plain tab, so the key is never dead.

  Everything is pressed through the same KeyDown/KeyPress a window hands to a
  focused control, and the command line is read back through the file the
  terminal saves its echo to (SaveToFile), which is the only way out of the
  output area in a program with no window.

  Build and run:
    lazbuild --build-mode=Debug tests\test_terminal_tab_complete.lpi
    bin\test_terminal_tab_complete.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}
{$codepage utf8}   //so a string handed to a control keeps the bytes it was given

uses
  Classes, SysUtils,
  RayLib,
  TyroClasses, TyroControls, TyroTerminal;

type
  { The completion reads Res.WorkPath, so the checks drive the workspace through
    a folder of their own instead of whatever folder the test runs in. }
  TWorkspace = class
  private
    FDir: string;
    Files: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(const AName: string);
    { makes this folder the workspace the terminal completes from }
    procedure Activate;
    { writes the files out, so the completion finds them on disk }
    procedure Flush;
    property Dir: string read FDir;
  end;

constructor TWorkspace.Create;
begin
  inherited;
  //GetTempDir already ends in a delimiter on some platforms and not on others,
  //so the folder is built through one that always adds it
  FDir := SysUtils.IncludeTrailingPathDelimiter(GetTempDir) + 'tyro_tab_complete' + PathDelim;
  if not SysUtils.DirectoryExists(FDir) then
    SysUtils.CreateDir(FDir);
  Files := TStringList.Create;
  Files.Sorted := True;
end;

destructor TWorkspace.Destroy;
var
  f: string;
begin
  for f in Files do
    DeleteFile(FDir + f);
  Files.Free;
  inherited Destroy;
end;

procedure TWorkspace.AddFile(const AName: string);
begin
  if Files.IndexOf(AName) < 0 then
    Files.Add(AName);
end;

procedure TWorkspace.Activate;
begin
  Res.WorkPath := FDir;
end;

procedure TWorkspace.Flush;
var
  f: string;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Add('-- created by the tab completion test');
    for f in Files do
    begin
      L.SaveToFile(FDir + f);
    end;
  finally
    L.Free;
  end;
end;

var
  aTerminal: TTyroTerminal;
  aWork: TWorkspace;
  tmpFile: string;
  Tests, Failures: Integer;

{ A string built from its bytes and nothing else, so the bytes of a character
  reach the control the way a keyboard would hand them over on a machine whose
  codepage is not UTF-8. }

function CharOf(const S: utf8string): TUTF8Char;
var
  i: Integer;
begin
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Move(S[i], Result[i], 1);
end;

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

{ A key, the way a user presses one. KeyDown takes the key by reference because
  a handler swallows it by writing KEY_NULL into it. }

procedure Key(AKey: TKeyboardKey; AShift: TShiftState = []);
var
  aKeyCode: TKeyboardKey;
begin
  aKeyCode := AKey;
  aTerminal.KeyDown(aKeyCode, AShift);
end;

procedure TypeChar(const AChar: utf8string);
var
  c: TUTF8Char;
begin
  c := CharOf(AChar);
  aTerminal.KeyPress(c);
end;

procedure TypeStr(const AChars: string);
var
  i: Integer;
begin
  for i := 1 to Length(AChars) do
    TypeChar(AChars[i]);
end;

procedure Tab(ATimes: Integer = 1);
var
  i: Integer;
begin
  for i := 1 to ATimes do
    Key(KEY_TAB);
end;

{ The command line as the terminal holds it right now: SubmitInput is not public,
  so the line is taken from the echo of the Enter that follows it. }

function TypedLine: utf8string;
var
  aSaved: TStringList;
begin
  Key(KEY_ENTER);
  //SaveToFile is the only way out of the output area
  aTerminal.SaveToFile(tmpFile);
  aSaved := TStringList.Create;
  try
    aSaved.LoadFromFile(tmpFile);
    //the echo is the last line that carries text, the ones after it are the
    //empty lines the line breaks opened
    Result := '';
    while aSaved.Count > 0 do
    begin
      if aSaved[aSaved.Count - 1] <> '' then
      begin
        Result := aSaved[aSaved.Count - 1];
        Break;
      end;
      aSaved.Delete(aSaved.Count - 1);
    end;
  finally
    aSaved.Free;
  end;
end;

{ Types a line, presses TAB ACount times and returns what came out. Every check
  starts from a fresh read, so the checks cannot influence each other. }

function Completed(const ATyped: string; ACount: Integer = 1): utf8string;
begin
  aTerminal.StartRead('>');
  TypeStr(ATyped);
  Tab(ACount);
  Result := TypedLine;
end;

var
  First, Second, Third: utf8string;
begin
  Tests := 0;
  Failures := 0;
  tmpFile := GetTempDir + 'test_terminal_tab_complete.txt';

  //The controls read their font metrics through the global resources, the same
  //global the engine fills in, and need nothing else: no window, no canvas.
  RayLibrary.Load;
  Res := TTyroResources.Create;

  aWork := TWorkspace.Create;
  aTerminal := TTyroTerminal.Create(nil);
  try
    aTerminal.BoundsRect := Rect(0, 0, 400, 300);
    aWork.Activate;
    //Four scripts whose names share a prefix, so the cycling is visible, and one
    //that does not, so the filtering can be told apart from the cycling.
    aWork.AddFile('alpha.tyro');
    aWork.AddFile('alpine.tyro');
    aWork.AddFile('alt.tyro');
    aWork.AddFile('hello.tyro');
    aWork.AddFile('zeta.lua');
    aWork.Flush;

    WriteLn('TAB completion of TTyroTerminal');
    WriteLn;
    WriteLn('command names');

    //An empty first word offers every command, and the first one alphabetically
    Check('an empty line completes a command name',
      Completed('', 1) = 'clear', Completed('', 1));

    Check('a command name prefix completes to the command',
      Completed('he', 1) = 'help', Completed('he', 1));

    Check('a command prefix with several matches completes to the first',
      Completed('cl', 1) = 'clear', Completed('cl', 1));

    Check('TAB again walks to the next command',
      Completed('cl', 2) = 'cls', Completed('cl', 2));

    Check('a third TAB cycles back to the first command',
      Completed('cl', 3) = 'clear', Completed('cl', 3));

    //The prefix the user typed is kept, only the word it stands for is replaced
    Check('the completion keeps the text around the first word',
      Completed('lo x', 0) = 'lo x', Completed('lo x', 0));

    Check('a first word that matches no command inserts a plain tab',
      Completed('zz', 1) = 'zz    ', Completed('zz', 1));

    WriteLn;
    WriteLn('files of the workspace');

    //Once the caret left the first word, the files of the workspace are offered.
    //'hello.tyro' and the command 'help' share their prefix, so the two checks
    //below tell the two sources apart: the same typed letters, completed to the
    //command in the first word and to the file as an argument.
    Check('a prefix shared by a command and a file completes to the command',
      Completed('he', 1) = 'help', Completed('he', 1));

    Check('an argument is completed from the files of the workspace',
      Completed('load he', 1) = 'load hello.tyro', Completed('load he', 1));

    First := Completed('load al', 1);
    Second := Completed('load al', 2);
    Third := Completed('load al', 3);
    Check('the file candidates come in alphabetical order',
      (First = 'load alpha.tyro') and (Second = 'load alpine.tyro') and
      (Third = 'load alt.tyro'), First + '|' + Second + '|' + Third);

    Check('TAB again cycles through the files and wraps around',
      Completed('load al', 4) = 'load alpha.tyro', Completed('load al', 4));

    //Only the file that matches the prefix is offered
    Check('a file prefix filters the workspace',
      Completed('load ze', 1) = 'load zeta.lua', Completed('load ze', 1));

    //A typed character invalidates the running list: the candidates are built
    //again from the text now under the caret, so TAB rebuilds and does not walk
    //on to the candidate that was next. Nothing matches "alpine.tyrox" any more,
    //and TAB falls back to a plain tab rather than to "alt.tyro".
    aTerminal.StartRead('>');
    TypeStr('load al');
    Tab(2);                  //"load alpine.tyro", the second of three
    TypeStr('x');            //"load alpine.tyrox"
    Tab(1);
    Check('a typed character rebuilds the completion instead of walking it',
      TypedLine = 'load alpine.tyrox    ', TypedLine);

    //A caret that is no longer at the end of the candidate starts a new one:
    //the word under it is completed against the workspace on its own, and only
    //"alpha.tyro" matches it, so the line stays as it is
    aTerminal.StartRead('>');
    TypeStr('load al');
    Tab(1);                  //"load alpha.tyro"
    Key(KEY_LEFT);           //the caret is now inside the completed word
    Tab(1);
    Check('moving the caret off the end of a completion starts a new one',
      TypedLine = 'load alpha.tyro', TypedLine);

    //A word that matches nothing is left alone by the completion: TAB is still
    //a tab, so a script indented with them keeps working
    Check('an argument with no candidate inserts a plain tab',
      Completed('load zz', 1) = 'load zz    ', Completed('load zz', 1));

    //The workspace is read when TAB is pressed, not when the terminal is made
    aWork.AddFile('amber.tyro');
    aWork.Flush;
    Check('a file created after the terminal was made is offered',
      Completed('load am', 1) = 'load amber.tyro', Completed('load am', 1));

    WriteLn;
    WriteLn('unchanged behaviour');

    //CTRL+TAB was never the completion, it is left alone
    aTerminal.StartRead('>');
    TypeStr('ab');
    Key(KEY_TAB, [ssCtrl]);
    Check('CTRL+TAB does not complete', TypedLine = 'ab', TypedLine);

    aTerminal.StopRead;
    if Failures = 0 then
      WriteLn('OK: ', Tests, ' checks passed')
    else
      WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks');
    Halt(Failures);
  finally
    aTerminal.Free;
    aWork.Free;
    FreeAndNil(Res);
    SysUtils.DeleteFile(tmpFile);
  end;
end.