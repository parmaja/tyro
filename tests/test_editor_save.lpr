program test_editor_save;

{ Headless test for saving what TyroEditor holds back to the file the script was
  loaded from - what CTRL+S does while the editor is showing (src\TyroEngines.pas,
  TTyroMain.EditorSave).

  Saving is two steps and the file is named by two properties of the script:
  the editor buffer is copied back into the source of the script it edits, and
  that source is then written to TTyroScript.Path + TTyroScript.FileName. Those
  two are what LoadFile split the name it was given into, so this checks that
  the name survives the split and comes back whole, that the buffer the editor
  holds is what reaches the file, and that a script with no file behind it has
  no name a save could be pointed at.

  What is not covered here is TTyroMain itself, which needs a window: the
  handler is these steps in order, and each of them is checked below on its own.

  Known and left as it is: the editor holds lines without their breaks, and
  SaveToFile writes the break of the platform, so a file that arrived with LF
  endings is saved back with CRLF. Section 4 says so out loud rather than
  hiding it.

  Build and run:
    lazbuild --build-mode=Debug tests\test_editor_save.lpi
    bin\test_editor_save.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  RayLib,
  TyroClasses, TyroControls, TyroEditors, TyroScripts;

type
  { TTyroScript is abstract over one method, Run, which a test never calls:
    this is about the name and the source of a script, not about running it. }

  TTestScript = class(TTyroScript)
  protected
    procedure Run; override;
  end;

procedure TTestScript.Run;
begin
end;

var
  aEditor: TyroEditor;
  aScript: TTestScript;
  aList: TStringList;
  srcFile: string;
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

{ A key, the way a user presses one. KeyDown takes the key by reference because
  a handler swallows it by writing KEY_NULL into it. }

procedure Key(AKey: TKeyboardKey; AShift: TShiftState = []);
var
  aKeyCode: TKeyboardKey;
begin
  aKeyCode := AKey;
  aEditor.KeyDown(aKeyCode, AShift);
end;

procedure TypeChar(const AChar: utf8string);
var
  c: TUTF8Char;
begin
  c := AChar;
  aEditor.KeyPress(c);
end;

//a source as one string, '|' between the lines, to compare in a check
function SourceOf(aSource: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to aSource.Count - 1 do
  begin
    if i > 0 then
      Result := Result + '|';
    Result := Result + aSource[i];
  end;
end;

//the bytes of a string as numbers, for a failure a console cannot print
function ByteText(const S: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + IntToStr(Byte(S[i])) + ' ';
end;

{ What the editor holds, one '|' between the lines, read the way EditorSave
  reads it: copied out through SaveSource and then compared. }

function EditorText: string;
var
  aSaved: TStringList;
begin
  aSaved := TStringList.Create;
  try
    aEditor.SaveSource(aSaved);
    Result := SourceOf(aSaved);
  finally
    aSaved.Free;
  end;
end;

{ The bytes of a file as they stand on the disk, line breaks and all: what a
  TStringList reads back has already had them taken out. }

function FileBytes(const AFileName: string): string;
var
  fs: TFileStream;
begin
  Result := '';
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, fs.Size);
    if fs.Size > 0 then
      fs.ReadBuffer(Result[1], fs.Size);
  finally
    fs.Free;
  end;
end;

//a file of exactly these bytes, breaks included
procedure WriteBytes(const AFileName: string; const ABytes: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    if ABytes <> '' then
      fs.WriteBuffer(ABytes[1], Length(ABytes));
  finally
    fs.Free;
  end;
end;

//a fresh read of the file the save wrote, through the same LoadFile the engine
//loads a script with
function SavedText: string;
var
  aFresh: TTestScript;
begin
  aFresh := TTestScript.Create;
  try
    aFresh.LoadFile(srcFile);
    Result := SourceOf(aFresh.Source);
  finally
    aFresh.Free;
  end;
end;

//the two steps EditorSave takes, over the path it names the file by
procedure SaveToScriptFile;
begin
  aEditor.SaveSource(aScript.Source);
  aScript.Source.SaveToFile(aScript.Path + aScript.FileName);
end;

begin
  Tests := 0;
  Failures := 0;
  //The save joins Path and FileName back together, so the file has to sit
  //somewhere with a directory part and a name part to join.
  srcFile := GetTempDir + 'test_editor_save.lua';
  SysUtils.DeleteFile(srcFile);

  //The controls read their font metrics through the global resources, the same
  //global the engine fills in, and need nothing else: no window, no canvas.
  RayLibrary.Load;
  Res := TTyroResources.Create;
  aEditor := TyroEditor.Create(nil);
  aScript := TTestScript.Create;
  try
    aEditor.BoundsRect := Rect(0, 0, 400, 300);

    WriteLn('Saving the editor back to the file of the script');
    WriteLn;

    //1. the name of the script survives being split in two
    WriteBytes(srcFile, '-- one' + LineEnding + 'print("hello")' + LineEnding);
    aScript.LoadFile(srcFile);
    Check('loading a script keeps the name of the file', aScript.FileName = ExtractFileName(srcFile), aScript.FileName);
    Check('and the directory it sits in', aScript.Path = ExtractFilePath(srcFile), aScript.Path);
    Check('Path ends in a separator, so joining needs nothing between the two',
      (Length(aScript.Path) > 0) and (aScript.Path[Length(aScript.Path)] = PathDelim), aScript.Path);
    Check('and the two of them name the file that was loaded',
      aScript.Path + aScript.FileName = srcFile, aScript.Path + aScript.FileName);

    //2. what the editor holds reaches that file, through the script it edits
    aEditor.LoadSource(aScript.Source);
    Check('the editor opens on the source of the script',
      EditorText = '-- one|print("hello")', EditorText);
    Check('an editor nobody touched is not marked modified', not aEditor.Modified);

    Key(KEY_END);
    TypeChar('!');                 //'-- one!' / 'print("hello")'
    Check('one typed character marks the editor modified', aEditor.Modified);
    SaveToScriptFile;
    Check('saving takes the mark away again, so the star leaves the status bar',
      not aEditor.Modified);
    Check('the typed character is in the source of the script',
      SourceOf(aScript.Source) = '-- one!|print("hello")', SourceOf(aScript.Source));
    Check('and in the file that script was loaded from',
      SavedText = '-- one!|print("hello")', SavedText);

    //Saving again writes the same lines: the editor is the whole of the buffer,
    //so a second save cannot grow the file.
    SaveToScriptFile;
    Check('saving a second time writes the same file again',
      SavedText = '-- one!|print("hello")', SavedText);

    //3. a line break typed in the editor is a line of its own in the file
    aEditor.LoadSource(aScript.Source);
    Key(KEY_END, [ssCtrl]);       //the caret opens at the start of the buffer
    Key(KEY_ENTER);
    TypeChar('x');
    SaveToScriptFile;
    Check('a line break typed in the editor reaches the file as a line',
      SavedText = '-- one!|print("hello")|x', SavedText);

    //4. what the file keeps of the breaks it came with. The editor holds lines
    //without them and SaveToFile writes the break of the platform, so LF
    //endings do not survive a save. The lines are unaffected; this is only said
    //out loud, and nothing else here depends on it.
    WriteBytes(srcFile, 'a' + #10 + 'b' + #10);        //LF endings
    aScript.LoadFile(srcFile);
    Check('a file with LF endings is read as the same two lines',
      SourceOf(aScript.Source) = 'a|b', SourceOf(aScript.Source));
    aEditor.LoadSource(aScript.Source);
    SaveToScriptFile;
    Check('and they are written back as the same two lines',
      SavedText = 'a|b', SavedText);
    Check('with the break of the platform instead of the one it came with',
      Pos('a' + LineEnding + 'b', FileBytes(srcFile)) > 0, ByteText(FileBytes(srcFile)));

    //5. a script with no file behind it has no name a save could be pointed at,
    //which is the case the handler refuses on
    FreeAndNil(aScript);
    aScript := TTestScript.Create;
    Check('a script that was never loaded has no file name', aScript.FileName = '', aScript.FileName);
    Check('and no path either', aScript.Path = '', aScript.Path);
    Check('so there is no name at all for a save to write to',
      (aScript.Path + aScript.FileName) = '', '<' + aScript.Path + aScript.FileName + '>');
    Check('the file of the last save is still the one that was written',
      SavedText = 'a|b', SavedText);
  finally
    FreeAndNil(aScript);
    aEditor.Free;
    FreeAndNil(Res);
    SysUtils.DeleteFile(srcFile);
  end;

  WriteLn;
  if Failures = 0 then
    WriteLn('OK: ', Tests, ' checks passed')
  else
    WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks');
  Halt(Failures);
end.