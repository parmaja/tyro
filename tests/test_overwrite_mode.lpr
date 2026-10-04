program test_overwrite_mode;

{ Headless test for the Insert/overwrite character mode of the three text
  inputs of Tyro:
      TTyroEdit        src\tyrolib\TyroControls.pas
      TTyroTerminal    src\tyrolib\TyroTerminal.pas
      TyroEditor       src\tyrolib\TyroEditors.pas

  The Insert key toggles between the two modes. Insert pushes the text to the
  right of the caret, overwrite replaces the character the caret stands on; at
  the end of the line there is nothing to replace, so both append there. A
  selection is replaced as a whole in either mode, and CTRL+INSERT (copy) and
  SHIFT+INSERT (paste) keep their traditional console meaning.

  Every check presses keys through the same KeyDown/KeyPress a window hands to a
  focused control, and reads the result back through the public accessors. The
  painting of the caret (an underline while overwriting) needs a real window and
  is not covered here.

  Characters of more than one byte are only checked for TTyroEdit, which keeps
  its text as a utf8string from end to end. TTyroEditor and TTyroTerminal hold
  theirs in a plain string and hand it to UTF8Copy/UTF8Length, which take a
  UTF8String, so on a machine whose codepage is not UTF-8 those two lose the
  bytes of a character they cannot represent - in insert mode just as much as in
  overwrite mode. That is a matter of the string types those two units use, not
  of this mode, and it is left as it is here.

  Build and run:
    lazbuild --build-mode=Debug tests\test_overwrite_mode.lpi
    bin\test_overwrite_mode.exe

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
  TyroClasses, TyroControls, TyroEditors, TyroTerminal;

{ A string built from its bytes and nothing else. Every other way of writing
  the bytes into a string (a literal, Chr, or a concatenation with a literal)
  runs them through the codepage of the machine, which is not UTF-8 here and
  would hand back other bytes than the ones asked for. }

function BytesOf(const ABytes: array of Byte): utf8string;
var
  i: Integer;
begin
  SetLength(Result, Length(ABytes));
  for i := 0 to High(ABytes) do
    Move(ABytes[i], Result[i + 1], 1);   //the byte itself, not a character
end;

{ The same bytes under the type the controls take a typed character in.
  TUTF8Char carries the codepage of the machine, which is not UTF-8 here, so
  assigning a utf8string to one of them runs the bytes through that codepage and
  changes them - 'C3 A9' would come out as three bytes of something else.
  Copying the bytes one by one is the only way to hand a control the character
  it was given, and it costs nothing for a single ASCII byte. }

function CharOf(const S: utf8string): TUTF8Char;
var
  i: Integer;
begin
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Move(S[i], Result[i], 1);
end;

//e-acute: two UTF-8 bytes, so one character
function MakeEAcute: utf8string;
begin
  Result := BytesOf([$C3, $A9]);
end;

type
  TTestEditor = class(TyroEditor)
  end;

var
  aEdit: TTyroEdit;
  aTerminal: TTyroTerminal;
  aEditor: TTestEditor;
  aList: TStringList;
  tmpFile: string;
  Tests, Failures: Integer;

procedure Key(aTarget: TTyroControl; AKey: TKeyboardKey; AShift: TShiftState = []); forward;

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

procedure Key(aTarget: TTyroControl; AKey: TKeyboardKey; AShift: TShiftState = []);
var
  aKeyCode: TKeyboardKey;
begin
  aKeyCode := AKey;
  aTarget.KeyDown(aKeyCode, AShift);
end;

{ Every control starts in insert mode, and the Insert key leaves it wherever
  the last check left it - the mode belongs to the control, not to a line of
  text. Each check therefore states the mode it needs, so the order of the
  checks cannot change the result. }

//the mode each of the three controls currently reports
function ModeOf(aTarget: TTyroControl): Boolean;
begin
  if aTarget is TTyroEdit then
    Result := TTyroEdit(aTarget).OverwriteMode
  else if aTarget is TTyroTerminal then
    Result := TTyroTerminal(aTarget).OverwriteMode
  else
    Result := TyroEditor(aTarget).OverwriteMode;
end;

procedure SetMode(aTarget: TTyroControl; AOverwrite: Boolean);
begin
  if ModeOf(aTarget) = AOverwrite then
    Exit;
  Key(aTarget, KEY_INSERT);
  if ModeOf(aTarget) <> AOverwrite then
    WriteLn('  NOTE  ', aTarget.ClassName, ' refused to change mode, the checks after it are meaningless');
end;

procedure TypeChar(aTarget: TTyroControl; const AChar: utf8string);
var
  c: TUTF8Char;
begin
  c := CharOf(AChar);      //byte for byte, whatever the codepage of the machine
  aTarget.KeyPress(c);
end;

{ Several characters one after the other, the way they arrive from a keyboard.
  Only the single-byte characters the checks type are handled here; the
  multi-byte one goes through KeyPress directly. }

procedure TypeStr(aTarget: TTyroControl; const AChars: string);
var
  i: Integer;
begin
  for i := 1 to Length(AChars) do
    TypeChar(aTarget, AChars[i]);
end;

procedure CheckEditText(const AName: string; aTarget: TTyroEdit; const AExpected: utf8string);
var
  got: utf8string;
begin
  got := aTarget.Text;
  Check(AName, got = AExpected, got);
end;

{ The bytes of a string as numbers, for a failure a console cannot print }

function ByteText(const S: utf8string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + IntToStr(Byte(S[i])) + ' ';
end;

{ What the terminal echoed, read back through the file it saves its output to.
  SaveToFile is the only way out of the output area, which is what makes the
  command line readable in a program with no window. }

function TerminalOutput: utf8string;
var
  aSaved: TStringList;
begin
  aSaved := TStringList.Create;
  try
    aTerminal.SaveToFile(tmpFile);
    aSaved.LoadFromFile(tmpFile);
    //Every echoed line is followed by the empty line its line break opened,
    //so the echo itself is the last line that carries text.
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

{ A command line, as it is echoed back after Enter }

function Submitted(aTarget: TTyroTerminal; const ATyped: utf8string): utf8string;
begin
  aTarget.StartRead('>');
  TypeChar(aTarget, ATyped);
  Key(aTarget, KEY_ENTER);
  Result := TerminalOutput;
end;

var
  ch: TUTF8Char;
  eAcute, baseline: utf8string;
begin
  Tests := 0;
  Failures := 0;
  tmpFile := GetTempDir + 'test_overwrite_mode.txt';
  eAcute := MakeEAcute;

  //The controls read their font metrics through the global resources, the same
  //global the engine fills in, and need nothing else: no window, no canvas.
  RayLibrary.Load;
  Res := TTyroResources.Create;
  aEdit := TTyroEdit.Create(nil);
  aTerminal := TTyroTerminal.Create(nil);
  aEditor := TTestEditor.Create(nil);
  try
    aEditor.BoundsRect := Rect(0, 0, 400, 300);
    aTerminal.BoundsRect := Rect(0, 0, 400, 300);

WriteLn('Insert / overwrite mode');
    WriteLn;
    WriteLn('TTyroEdit');

    SetMode(aEdit, False);
    Check('the edit starts in insert mode', not aEdit.OverwriteMode);
    aEdit.Text := 'abc def';
    Key(aEdit, KEY_HOME);
    TypeChar(aEdit, 'X');
    CheckEditText('insert mode pushes the text to the right of the caret', aEdit, 'Xabc def');

    aEdit.Text := 'abc def';
    Key(aEdit, KEY_HOME);
    Key(aEdit, KEY_INSERT);
    Check('the Insert key turns overwrite on', aEdit.OverwriteMode);
    TypeChar(aEdit, 'X');
    CheckEditText('overwrite mode replaces the character under the caret', aEdit, 'Xbc def');

    TypeChar(aEdit, 'Y');
    CheckEditText('overwrite walks to the right one character at a time', aEdit, 'XYc def');

    Key(aEdit, KEY_INSERT);
    Check('the Insert key turns overwrite off again', not aEdit.OverwriteMode);
    TypeChar(aEdit, 'Z');
    CheckEditText('insert mode pushes the text again after the second press', aEdit, 'XYZc def');

    //At the end of the line both modes append: there is nothing to replace
    aEdit.Text := 'abc';
    Key(aEdit, KEY_END);
    SetMode(aEdit, True);
    TypeChar(aEdit, 'd');
    CheckEditText('overwrite at the end of the text appends', aEdit, 'abcd');

    SetMode(aEdit, False);
    Key(aEdit, KEY_END);
    TypeChar(aEdit, 'e');
    CheckEditText('insert at the end of the text appends', aEdit, 'abcde');

    //A selection is replaced as a whole, whatever the mode
    aEdit.Text := 'abcdef';
    Key(aEdit, KEY_HOME);
    Key(aEdit, KEY_RIGHT, [ssShift]);
    Key(aEdit, KEY_RIGHT, [ssShift]);
    SetMode(aEdit, True);
    TypeChar(aEdit, 'Z');
    CheckEditText('a character typed over a selection replaces the selection only', aEdit, 'Zcdef');

    //Overwrite counts codepoints, not bytes: e-acute is two bytes and still one
    //character, so it is replaced whole and never split in half. Every byte here
    //is built and read back by hand - the codepage of the machine is not UTF-8,
    //so a character spelled out in this file would not be the bytes that went in.
    aEdit.Text := BytesOf([$61, $C3, $A9, $62]);       //a + e-acute + b
    baseline := aEdit.Text;
    Check('the text handed to the edit survives it whole',
      (Length(baseline) = 4) and (Byte(baseline[2]) = $C3) and (Byte(baseline[3]) = $A9),
      ByteText(baseline));
    Key(aEdit, KEY_HOME);
    Key(aEdit, KEY_RIGHT);                            //onto the multi-byte character
    SetMode(aEdit, True);
    ch := CharOf(eAcute);
    aEdit.KeyPress(ch);                 //the two-byte character, byte for byte
    Check('overwrite replaces a whole multi-byte character, byte for byte',
      (aEdit.Text = baseline) and (Length(aEdit.Text) = 4), ByteText(aEdit.Text));
    //The caret is now on the last character, so a one-byte character replaces it
    TypeChar(aEdit, 'z');
    Check('the caret moved one codepoint past the multi-byte character',
      (Length(aEdit.Text) = 4) and (Byte(aEdit.Text[1]) = $61) and
      (Byte(aEdit.Text[2]) = $C3) and (Byte(aEdit.Text[3]) = $A9) and
      (Byte(aEdit.Text[4]) = Ord('z')), ByteText(aEdit.Text));

    //CTRL+INSERT keeps its traditional meaning: it copies, it does not toggle
    SetMode(aEdit, True);
    aEdit.Text := 'abcdef';
    Key(aEdit, KEY_HOME);
    Key(aEdit, KEY_RIGHT, [ssShift]);
    Key(aEdit, KEY_RIGHT, [ssShift]);
    Key(aEdit, KEY_INSERT, [ssCtrl]);
    Check('CTRL+INSERT copies instead of toggling the mode', aEdit.OverwriteMode);
    TypeChar(aEdit, 'Z');
    CheckEditText('the copy left the text and the mode alone', aEdit, 'Zcdef');

    WriteLn;
    WriteLn('TTyroTerminal');

    SetMode(aTerminal, False);
    Check('the terminal starts in insert mode', not aTerminal.OverwriteMode);
    Check('the terminal echoes a typed command line', Submitted(aTerminal, 'help') = 'help');

    //The terminal only reads keys while a read is open, so that is where the
    //mode is switched - a console with no prompt has no line to write on
    aTerminal.StartRead('>');
    Key(aTerminal, KEY_INSERT);
    Check('the Insert key turns overwrite on in the terminal', aTerminal.OverwriteMode);
    TypeStr(aTerminal, 'ab');
    Key(aTerminal, KEY_HOME);         //back onto the first character
    TypeChar(aTerminal, 'X');
    Key(aTerminal, KEY_ENTER);
    Check('overwrite replaces the character under the caret in the terminal',
      TerminalOutput = 'Xb', TerminalOutput);

    aTerminal.StartRead('>');
    Key(aTerminal, KEY_INSERT);
    Check('the Insert key turns overwrite off again in the terminal',
      not aTerminal.OverwriteMode);
    TypeStr(aTerminal, 'ab');
    Key(aTerminal, KEY_HOME);
    TypeChar(aTerminal, 'X');
    Key(aTerminal, KEY_ENTER);
    Check('insert mode pushes the text again after the second press',
      TerminalOutput = 'Xab', TerminalOutput);

    //Nothing to overwrite at the end of the line: the character is appended
    aTerminal.StartRead('>');
    Key(aTerminal, KEY_INSERT);
    TypeStr(aTerminal, 'ab');
    Key(aTerminal, KEY_END);
    TypeChar(aTerminal, 'c');
    Key(aTerminal, KEY_ENTER);
    Check('overwrite at the end of the command line appends', TerminalOutput = 'abc',
      TerminalOutput);

    //A selection is replaced as a whole, whatever the mode
    aTerminal.StartRead('>');
    TypeStr(aTerminal, 'abc');
    Key(aTerminal, KEY_HOME);
    Key(aTerminal, KEY_RIGHT, [ssShift]);
    Key(aTerminal, KEY_RIGHT, [ssShift]);
    TypeChar(aTerminal, 'Z');
    Key(aTerminal, KEY_ENTER);
    Check('a character typed over a selection replaces the selection only',
      TerminalOutput = 'Zc', TerminalOutput);
    aTerminal.StartRead('>');
    Key(aTerminal, KEY_INSERT);       //back to insert for the next check
    aTerminal.StopRead;

    WriteLn;
    WriteLn('TyroEditor');

    aList := TStringList.Create;
    try
      SetMode(aEditor, False);
      Check('the editor starts in insert mode', not aEditor.OverwriteMode);
      aList.Add('abcdef');
      aEditor.LoadSource(aList);
      Key(aEditor, KEY_HOME);
      TypeChar(aEditor, 'X');
      aEditor.SaveSource(aList);
      Check('insert mode pushes the text to the right of the caret',
        aList[0] = 'Xabcdef', aList[0]);

      Key(aEditor, KEY_INSERT);
      Check('the Insert key turns overwrite on', aEditor.OverwriteMode);
      aList.Clear;
      aList.Add('abcdef');
      aEditor.LoadSource(aList);
      Key(aEditor, KEY_HOME);
      TypeChar(aEditor, 'X');
      TypeChar(aEditor, 'Y');
      aEditor.SaveSource(aList);
      Check('overwrite replaces the character under the caret', aList[0] = 'XYcdef', aList[0]);

      //The caret kept walking, so the third character goes too
      TypeChar(aEditor, 'Z');
      aEditor.SaveSource(aList);
      Check('overwrite walks to the right one character at a time', aList[0] = 'XYZdef', aList[0]);

      //At the end of a line the character is appended
      Key(aEditor, KEY_END);
      TypeChar(aEditor, '!');
      aEditor.SaveSource(aList);
      Check('overwrite at the end of the line appends', aList[0] = 'XYZdef!', aList[0]);

      Key(aEditor, KEY_INSERT);
      Check('the Insert key turns overwrite off again', not aEditor.OverwriteMode);
      TypeChar(aEditor, '?');
      aEditor.SaveSource(aList);
      Check('insert mode pushes the text again after the second press',
        aList[0] = 'XYZdef!?', aList[0]);

      //A selection is replaced as a whole, whatever the mode
      SetMode(aEditor, True);
      aList.Clear;
      aList.Add('abcdef');
      aEditor.LoadSource(aList);
      Key(aEditor, KEY_HOME);
      Key(aEditor, KEY_RIGHT, [ssShift]);
      Key(aEditor, KEY_RIGHT, [ssShift]);
      TypeChar(aEditor, 'Z');
      aEditor.SaveSource(aList);
      Check('a character typed over a selection replaces the selection only',
        aList[0] = 'Zcdef', aList[0]);

      //The mode belongs to the control, not to the text in it, so loading
      //another buffer keeps it as it was
      aList.Clear;
      aList.Add('12345');
      aEditor.LoadSource(aList);
      Key(aEditor, KEY_HOME);
      TypeChar(aEditor, 'x');
      aEditor.SaveSource(aList);
      Check('loading another buffer keeps the mode', aList[0] = 'x2345', aList[0]);

      //Enter breaks the line as usual, whatever the mode, and the character then
      //goes to the empty line that was opened
      SetMode(aEditor, True);
      aList.Clear;
      aList.Add('ab');
      aEditor.LoadSource(aList);
      Key(aEditor, KEY_END);
      Key(aEditor, KEY_ENTER);
      TypeChar(aEditor, 'X');
      aEditor.SaveSource(aList);
      Check('enter breaks the line in overwrite mode too, and the character follows',
        (aList.Count = 2) and (aList[0] = 'ab') and (aList[1] = 'X'),
        IntToStr(aList.Count) + ':' + aList[0] + '|' + aList[1]);

      SetMode(aEditor, False);
    finally
      aList.Free;
    end;
  finally
    aEditor.Free;
    aTerminal.Free;
    aEdit.Free;
    FreeAndNil(Res);
    SysUtils.DeleteFile(tmpFile);
  end;

  WriteLn;
  if Failures = 0 then
    WriteLn('OK: ', Tests, ' checks passed')
  else
    WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks');
  Halt(Failures);
end.
