program test_editor_shift_select;

{ Headless test for the shift selections of TyroEditor (src/tyrolib\TyroEditors.pas):
    shift+left / shift+right           character by character
    shift+up / shift+down              line by line
    shift+home / shift+end             to the start and to the end of the line
    shift+ctrl+left / shift+ctrl+right word by word
    shift+page up / shift+page down    over a page
    ctrl+shift+home / ctrl+shift+end   over the whole buffer
  and the two rules the rest of them rest on: a plain move drops the selection
  and leaves the caret ready to anchor the next one, and a shift that grows back
  to nothing selects nothing at all.

  The selection itself is private, so every check ends the selection the way a
  user does - with Delete, Backspace, Enter or a typed character - and reads
  what is left in the buffer through the public SaveSource. Cutting pins both
  edges of the range at once, and because a Delete with no selection cuts a
  single character instead, the same check tells "the selection collapsed" apart
  from "the selection is still there".

  Not covered here is the painting itself, which needs a real window; what the
  painter covers is checked through LineSelection, and the syntax runs and the
  caret geometry are untouched by the keys under test.

  Build and run:
    lazbuild --build-mode=Debug tests\test_editor_shift_select.lpi
    bin\test_editor_shift_select.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils, Types,
  RayLib,
  TyroClasses, TyroControls, TyroEditors;

const
  cBufferHeight = 300;   //the client a page is counted over, whatever the font is
  cPageLines = 200;      //more lines than fit on a page, so a page move cannot clamp
  cCell = 8;             //the cell size the editor falls back to with no font loaded

type
  { The mouse half of the selection is protected, and a test cannot hand a
    TyroEditor a mouse: raylib owns that state. A descendant can reach the two
    handlers and call them with the client coordinates a mouse would have, and
    can ask which columns a line has selected - that range is what the painter
    covers and what the eye reads as "selected", and it has to be checked
    somewhere other than in a window that cannot be opened here. }

  TTestEditor = class(TyroEditor)
  public
    procedure Press(aX, aY: Integer; AExtend: Boolean);
    procedure Drag(aX, aY: Integer);
    function RangeOf(ALine: Integer): string;
  end;

procedure TTestEditor.Press(aX, aY: Integer; AExtend: Boolean);
begin
  SelectAtPress(aX, aY, AExtend);
end;

procedure TTestEditor.Drag(aX, aY: Integer);
begin
  SelectAtDrag(aX, aY);
end;

{ The columns a line has selected, as 'from-to' over the half-open range the
  painter fills, or '-' where nothing is selected on that line. The two columns
  of a caret are both a from and a to, so a selection that has collapsed reads
  as nothing at all. }

function TTestEditor.RangeOf(ALine: Integer): string;
var
  lFrom, lTo: Integer;
begin
  LineSelection(ALine, lFrom, lTo);
  if (lFrom < 0) or (lTo <= lFrom) then
    Result := '-'
  else
    Result := IntToStr(lFrom) + '-' + IntToStr(lTo);
end;

var
  aEditor: TTestEditor;
  aList: TStringList;
  s, full: string;
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

{ What the editor holds now, one '|' between the lines. Read once per check:
  a check compares against the buffer it just cut, not against one that a later
  argument has since changed. }

function Buffer: string;
var
  aSaved: TStringList;
  i: Integer;
begin
  aSaved := TStringList.Create;
  try
    aEditor.SaveSource(aSaved);
    Result := '';
    for i := 0 to aSaved.Count - 1 do
    begin
      if i > 0 then
        Result := Result + '|';
      Result := Result + aSaved[i];
    end;
  finally
    aSaved.Free;
  end;
end;

//How many lines a Buffer() string holds
function LineCountOf(const AText: string): Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 1 to Length(AText) do
    if AText[i] = '|' then
      Inc(Result);
end;

//The three lines every check starts from, caret on the first character
procedure Setup;
begin
  aList := TStringList.Create;
  try
    aList.Add('abc def');
    aList.Add('ghi jkl');
    aList.Add('mno pqr');
    aEditor.LoadSource(aList);
  finally
    aList.Free;
  end;
end;

{ Where the mouse has to be for a caret on that line and column. The editor
  counts a column per cell from the right edge of the gutter, and the gutter is
  as wide as the line numbers plus two cells, so this mirrors that; no font is
  loaded here, so a cell is cCell wide and cCell high. }

function MouseX(ACol: Integer): Integer;
begin
  Result := (Length(IntToStr(aEditor.LineCount)) + 2) * cCell + ACol * cCell + 1;
end;

function MouseY(ALine: Integer): Integer;
begin
  Result := ALine * cCell + 1;
end;

{ A press, and then the drag of the same frame, the way Update hands them over
  when a button goes down: the button is both pressed and down on that frame. }

procedure Click(ALine, ACol: Integer; AExtend: Boolean);
begin
  aEditor.Press(MouseX(ACol), MouseY(ALine), AExtend);
  aEditor.Drag(MouseX(ACol), MouseY(ALine));
end;

//Lines of different lengths, for the column a move remembers
procedure SetupRagged;
begin
  aList := TStringList.Create;
  try
    aList.Add('abcdefghij');
    aList.Add('ab');
    aList.Add('abcdefghij');
    aEditor.LoadSource(aList);
  finally
    aList.Free;
  end;
end;

//ALines numbered lines, the buffer a page is measured over
procedure SetupLong(ALines: Integer);
var
  i: Integer;
begin
  aList := TStringList.Create;
  try
    for i := 1 to ALines do
      aList.Add('line ' + IntToStr(i));
    aEditor.LoadSource(aList);
  finally
    aList.Free;
  end;
end;

//One more numbered line than the buffer holds now
procedure NextSetupLong;
var
  aSaved: TStringList;
begin
  aSaved := TStringList.Create;
  try
    aEditor.SaveSource(aSaved);
    SetupLong(aSaved.Count + 1);
  finally
    aSaved.Free;
  end;
end;

//Cuts the selection and asks what is left of the buffer
procedure CheckCut(const AName, AExpected: string);
begin
  s := Buffer;
  Check(AName, s = AExpected, s);
end;

begin
  Tests := 0;
  Failures := 0;

  //The editor reads its font metrics through the global resources, the same
  //global the engine fills in, and needs nothing else: no window, no canvas.
  //Raylib itself is only loaded for its entry points, which the sound unit
  //calls on the way out whatever the program did with audio.
  RayLibrary.Load;
  Res := TTyroResources.Create;
  aEditor := TTestEditor.Create(nil);
  try
    aEditor.BoundsRect := Rect(0, 0, 400, cBufferHeight);

    WriteLn('TyroEditor shift selections');
    WriteLn;

    //1. shift+right and shift+left walk the selection one character at a time
    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+right selects the characters it walks over',
      ' def|ghi jkl|mno pqr');

    Setup;
    Key(KEY_END);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+left selects back towards the start of the line',
      'abc d|ghi jkl|mno pqr');

    Setup;
    Key(KEY_END);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+left walks back across the start of the line into the one above',
      '|ghi jkl|mno pqr');

    //2. shift+end and shift+home
    Setup;
    Key(KEY_RIGHT);
    Key(KEY_RIGHT);
    Key(KEY_END, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+end selects from the caret to the end of the line',
      'ab|ghi jkl|mno pqr');

    Setup;
    Key(KEY_END, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+end from the start of the line takes the whole line',
      '|ghi jkl|mno pqr');

    Setup;
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_END);
    Key(KEY_HOME, [ssShift]);
    Key(KEY_HOME, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+home walks back to the first character of the line',
      '|ghi jkl|mno pqr');

    Setup;
    Key(KEY_END);
    Key(KEY_HOME, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+home from the first character of the line takes the whole line',
      '|ghi jkl|mno pqr');

    //3. shift+down and shift+up, which the caret column survives
    Setup;
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+down selects into the next line and swallows the break',
      'ghi jkl|mno pqr');

    Setup;
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+down keeps growing past the last line, keeping the column',
      'no pqr');

    Setup;
    Key(KEY_DOWN);
    Key(KEY_UP, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+up selects back into the previous line',
      'ghi jkl|mno pqr');

    SetupRagged;
    Key(KEY_END);
    Key(KEY_DOWN);              //the short line takes the caret back to its end
    Key(KEY_UP, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+up gives the wanted column back on a line that was too short',
      'abcdefghij|abcdefghij');

    //4. a plain move drops the selection, and the caret is the next anchor
    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_LEFT);              //no shift: the selection goes, the caret stays put
    Key(KEY_LEFT);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('a plain move drops the selection and re-anchors at the caret',
      'adef|ghi jkl|mno pqr');

    //5. a selection that grows back to nothing selects nothing
    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('a selection that shrank back to the anchor selects nothing',
      'bc def|ghi jkl|mno pqr');

    //6. shift+ctrl+left and shift+ctrl+right, word by word
    Setup;
    Key(KEY_RIGHT, [ssCtrl, ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+ctrl+right selects up to the start of the next word',
      'def|ghi jkl|mno pqr');

    Setup;
    Key(KEY_END);
    Key(KEY_LEFT, [ssCtrl, ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+ctrl+left selects back up to the start of the word',
      'abc |ghi jkl|mno pqr');

    //7. shift+page down and shift+page up
    Setup;
    Key(KEY_PAGE_DOWN, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+page down selects a page, clamped at the last line',
      'mno pqr');

    Setup;
    Key(KEY_END, [ssCtrl]);
    Key(KEY_HOME);
    Key(KEY_PAGE_UP, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+page up selects a page back, clamped at the first line',
      'mno pqr');

    //On a buffer longer than a page the page has to be a page and not a line,
    //without pinning its size: the rest is a tail of the original and more than
    //one line of it is gone.
    SetupLong(cPageLines);
    s := Buffer;
    full := s;
    Key(KEY_PAGE_DOWN, [ssShift]);
    Key(KEY_DELETE);
    s := Buffer;
    Check('shift+page down selects over a whole page',
      (Pos(s, full) > 1) and (LineCountOf(s) < cPageLines - 1), s);

    NextSetupLong;
    s := Buffer;
    full := s;
    Key(KEY_END, [ssCtrl]);
    Key(KEY_PAGE_UP, [ssShift]);
    Key(KEY_DELETE);
    s := Buffer;
    Check('shift+page up selects over a whole page back',
      (Copy(s, 1, 6) = 'line 1') and (LineCountOf(s) < cPageLines - 1), s);

    //8. ctrl+shift+home and ctrl+shift+end
    Setup;
    Key(KEY_END, [ssCtrl]);
    Key(KEY_HOME, [ssCtrl, ssShift]);
    Key(KEY_DELETE);
    CheckCut('ctrl+shift+home selects from the caret to the start of the buffer',
      '');

    Setup;
    Key(KEY_END, [ssCtrl, ssShift]);
    Key(KEY_DELETE);
    CheckCut('ctrl+shift+end selects from the caret to the end of the buffer',
      '');

    Setup;
    Key(KEY_END);
    Key(KEY_END, [ssCtrl, ssShift]);
    Key(KEY_DELETE);
    CheckCut('ctrl+shift+end keeps the column the caret is on', 'abc def');

    //9. a selection made elsewhere grows and shrinks the same way
    Setup;
    Key(KEY_A, [ssCtrl]);      //select all: anchor at the start, caret at the end
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('shift+left shrinks a selection made elsewhere by one character',
      'r');

    Setup;
    Key(KEY_A, [ssCtrl]);
    Key(KEY_LEFT, [ssCtrl]);   //no shift: collapses back to the end of the buffer
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_DELETE);
    CheckCut('ctrl+left collapses the selection and re-anchors at the caret',
      'abc def|ghi jkl|mnopqr');

    //10. typing, backspace and enter go over a selection instead of around it
    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    TypeChar('#');
    CheckCut('a typed character replaces the selection', '# def|ghi jkl|mno pqr');

    Setup;
    Key(KEY_DOWN, [ssShift]);
    TypeChar('#');
    CheckCut('a typed character replaces a selection across lines',
      '#ghi jkl|mno pqr');

    Setup;
    Key(KEY_END, [ssShift]);
    Key(KEY_BACKSPACE);
    CheckCut('backspace cuts the selection when there is one', '|ghi jkl|mno pqr');

    Setup;
    Key(KEY_END, [ssShift]);
    Key(KEY_ENTER);
    CheckCut('enter cuts the selection when there is one', '||ghi jkl|mno pqr');

    Setup;
    Key(KEY_END);
    Key(KEY_BACKSPACE);
    CheckCut('backspace cuts one character when there is no selection',
      'abc de|ghi jkl|mno pqr');

    //12. undo brings the buffer back, and with it no selection over that buffer
    Setup;
    TypeChar('!');              //something to undo, and an undo step to undo it
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_Z, [ssCtrl]);       //the selection was over the text that just went
    Key(KEY_DELETE);
    CheckCut('undo leaves no selection behind over the text it undid',
      'abc ef|ghi jkl|mno pqr');

    //11. a selection counts columns, not bytes: a tab is one of them
    aList := TStringList.Create;
    try
      aList.Add('a' + #9 + 'b');
      aEditor.LoadSource(aList);
    finally
      aList.Free;
    end;
    Key(KEY_END, [ssShift]);
    Key(KEY_END, [ssShift]);   //already at the end: the selection stops growing
    Key(KEY_DELETE);
    CheckCut('shift+end on a line with a tab selects to its end', '');

    //13. the mouse: a press without shift leaves nothing selected, a drag selects
    //from where the press landed, and a press with shift extends from the anchor
    Setup;
    Key(KEY_END, [ssShift]);
    Click(2, 3, False);
    Key(KEY_DELETE);
    CheckCut('a click without shift drops the selection',
      'abc def|ghi jkl|mnopqr');

    Setup;
    Key(KEY_END, [ssShift]);
    aEditor.Press(MouseX(1), MouseY(1), False);
    Key(KEY_DELETE);
    CheckCut('a click without a drag selects nothing at all',
      'abc def|gi jkl|mno pqr');

    Setup;
    aEditor.Press(MouseX(2), MouseY(0), False);
    aEditor.Drag(MouseX(5), MouseY(0));
    Key(KEY_DELETE);
    CheckCut('a drag selects from where the press landed',
      'abef|ghi jkl|mno pqr');

    Setup;
    aEditor.Press(MouseX(0), MouseY(0), False);
    aEditor.Drag(MouseX(7), MouseY(0));
    Key(KEY_DELETE);
    CheckCut('a drag to the end of the line selects the line',
      '|ghi jkl|mno pqr');

    Setup;
    aEditor.Press(MouseX(1), MouseY(0), False);
    aEditor.Drag(MouseX(1), MouseY(0));
    aEditor.Drag(MouseX(4), MouseY(1));
    Key(KEY_DELETE);
    CheckCut('a drag down a line selects across the break',
      'ajkl|mno pqr');

    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Click(1, 4, True);
    Key(KEY_DELETE);
    CheckCut('a click with shift extends the selection from the anchor',
      'jkl|mno pqr');

    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Click(1, 0, True);
    Key(KEY_DELETE);
    CheckCut('a click with shift backwards extends over the lines between',
      'ghi jkl|mno pqr');

    Setup;
    Key(KEY_END, [ssShift]);
    Click(0, 7, True);
    Key(KEY_DELETE);
    CheckCut('a click with shift on the anchor of the selection keeps it',
      '|ghi jkl|mno pqr');

    //14. what the painter covers: shift+left and shift+right move the range one
    //character at a time, and a selection kept on one line ends at the caret
    //rather than running on to the end of the line
    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Check('one shift+right covers one character', aEditor.RangeOf(0) = '0-1', aEditor.RangeOf(0));
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Check('shift+right grows the range one character at a time',
      aEditor.RangeOf(0) = '0-3', aEditor.RangeOf(0));

    Setup;
    Key(KEY_END);
    Key(KEY_LEFT, [ssShift]);
    Check('one shift+left from the end of a line covers one character',
      aEditor.RangeOf(0) = '6-7', aEditor.RangeOf(0));
    Key(KEY_LEFT, [ssShift]);
    Check('shift+left grows the range back towards the start of the line',
      aEditor.RangeOf(0) = '5-7', aEditor.RangeOf(0));

    Setup;
    Key(KEY_RIGHT);
    Key(KEY_RIGHT);
    Key(KEY_RIGHT);
    Key(KEY_LEFT, [ssShift]);
    Check('the range stops at the caret when it walks back from the middle',
      aEditor.RangeOf(0) = '2-3', aEditor.RangeOf(0));
    Key(KEY_LEFT, [ssShift]);
    Check('and it keeps stopping at the caret as it walks back further',
      aEditor.RangeOf(0) = '1-3', aEditor.RangeOf(0));

    Setup;
    Key(KEY_END);
    Key(KEY_HOME);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Check('a range on one line never reaches past the caret',
      aEditor.RangeOf(0) = '0-2', aEditor.RangeOf(0));
    Check('a line outside the selection is left alone', aEditor.RangeOf(1) = '-', aEditor.RangeOf(1));

    //a selection over more than one line runs to the end of the first line and
    //from the start of the last one, so those two ends stay where they belong
    Setup;
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Check('the first line of a selection over lines runs to its end',
      aEditor.RangeOf(0) = '0-7', aEditor.RangeOf(0));
    Check('the last line of a selection over lines runs from its start',
      aEditor.RangeOf(1) = '0-1', aEditor.RangeOf(1));
    Check('a line the selection does not reach is left alone',
      aEditor.RangeOf(2) = '-', aEditor.RangeOf(2));

    Setup;
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Check('the line in the middle of a selection over lines is covered whole',
      aEditor.RangeOf(1) = '0-7', aEditor.RangeOf(1));
    Check('and the last line still stops at the caret',
      aEditor.RangeOf(2) = '0-1', aEditor.RangeOf(2));

    Setup;
    Key(KEY_END);
    Key(KEY_DOWN, [ssShift]);
    Key(KEY_DOWN, [ssShift]);
    Check('a range walking down from the end of a line leaves the whole line covered',
      (aEditor.RangeOf(0) = '-') and (aEditor.RangeOf(1) = '0-7') and (aEditor.RangeOf(2) = '0-7'),
      aEditor.RangeOf(0) + ' ' + aEditor.RangeOf(1) + ' ' + aEditor.RangeOf(2));

    //and the range is gone again with the selection itself
    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_LEFT);              //no shift: the selection goes with it
    Check('a plain move takes the painted range with it', aEditor.RangeOf(0) = '-', aEditor.RangeOf(0));

    Setup;
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_RIGHT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Key(KEY_LEFT, [ssShift]);
    Check('a range that shrank back to nothing covers nothing',
      aEditor.RangeOf(0) = '-', aEditor.RangeOf(0));
  finally
    aEditor.Free;
    FreeAndNil(Res);
  end;

  WriteLn;
  if Failures = 0 then
    WriteLn('OK: ', Tests, ' checks passed')
  else
    WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks');
  Halt(Failures);
end.