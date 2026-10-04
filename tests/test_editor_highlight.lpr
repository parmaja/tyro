program test_editor_highlight;

{ Headless test for the Lua syntax highlighting of TyroEditor (src/tyrolib\TyroEditors.pas).

  RebuildRuns measures every line into runs of same-colored characters and
  DrawLine draws each run from the column the run starts on, so what is under
  test is where those runs fall. They are read one column at a time back
  through GetRuns, without a window, a font or a canvas:

    keywords, api names, numbers, comments and long strings  keep their colors
    a quoted string  runs from its opening quote to its closing quote and no
                     further: a quote behind a backslash closes nothing, a
                     backslash behind a backslash is a backslash, and a line
                     whose string is never closed keeps its color to the end of
                     the line
    a quote inside a comment  opens nothing

  The line of the bug report this test came from is checked first, with its
  runs printed, because a string that does not stop at its closing quote is
  what that line showed: the color of "button" ran on over "Hi", over the
  numbers and over "btn1" instead of ending at the quote after the word.

  Build and run:
    lazbuild --build-mode=Debug tests\test_editor_highlight.lpi
    bin\test_editor_highlight.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  mnUtils,
  RayLib,
  TyroClasses, TyroControls, TyroEditors;

type
  { The runs are private, so a test cannot ask the editor for them; a
    descendant reaches GetRuns, which hands them over as they are measured. }

  TTestEditor = class(TyroEditor)
  public
    function RunsOf(ALine: Integer): TEditorRuns;
  end;

function TTestEditor.RunsOf(ALine: Integer): TEditorRuns;
begin
  Result := GetRuns(ALine);
end;

var
  aEditor: TTestEditor;
  aBuf: TStringList;      //the buffer the editor holds, kept to name the columns
  aRuns: TEditorRuns;     //the runs of the line in hand
  aRunLine: Integer;      //which line of the buffer those runs are
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

{ The color constants of raylib are TRGBAColor while the runs hold a TColor,
  which wraps one, so a check names the color it expects and a failure is told
  what the highlighter painted instead. }

function IsColor(C: TColor; AColor: TRGBAColor): Boolean;
begin
  Result := (C.RGBA.Red = AColor.Red) and (C.RGBA.Green = AColor.Green) and
    (C.RGBA.Blue = AColor.Blue) and (C.RGBA.Alpha = AColor.Alpha);
end;

//A color as the highlighter means it
function ColorName(C: TColor): string;
begin
  if IsColor(C, clYellow) then
    Result := 'keyword'
  else if IsColor(C, clGreen) then
    Result := 'string'
  else if IsColor(C, clOrange) then
    Result := 'number'
  else if IsColor(C, clSkyBlue) then
    Result := 'api'
  else if IsColor(C, clGray) then
    Result := 'comment'
  else if IsColor(C, clLightgray) then
    Result := 'text'
  else if IsColor(C, clBlack) then
    Result := 'nothing'
  else
    Result := 'other';
end;

//A buffer to measure, from one line or from several
procedure Setup(const ALines: array of string);
var
  i: Integer;
begin
  aBuf.Clear;
  for i := 0 to High(ALines) do
    aBuf.Add(ALines[i]);
  aEditor.LoadSource(aBuf);
  aRunLine := 0;
  aRuns := aEditor.RunsOf(aRunLine);
end;

//The runs of another line of the buffer in hand
procedure Line(ALine: Integer);
begin
  aRunLine := ALine;
  aRuns := aEditor.RunsOf(aRunLine);
end;

//Where a word stands in a line, in the columns the runs are counted in
function ColOf(ALine: Integer; const AWord: string): Integer;
begin
  Result := Pos(AWord, aBuf[ALine]) - 1;
end;

//The color the line in hand is painted with at that column, black past its end
function ColorAt(ACol: Integer): TColor;
var
  i: Integer;
begin
  Result := clBlack;
  for i := 0 to High(aRuns) do
    if (ACol >= aRuns[i].Start) and (ACol < aRuns[i].Start + aRuns[i].Count) then
    begin
      Result := aRuns[i].Color;
      Exit;
    end;
end;

//The last column of the line in hand
function LastCol: Integer;
begin
  Result := UTF8Length(aBuf[aRunLine]) - 1;
end;

//The color that word of that line is painted with
function ColorOf(ALine: Integer; const AWord: string): TColor;
begin
  Result := ColorAt(ColOf(ALine, AWord));
end;

//Every run of the line in hand, for a look at what the editor painted
procedure DumpRuns(const ATitle: string);
var
  i: Integer;
begin
  WriteLn('  ', ATitle);
  WriteLn('      line ', aRunLine + 1, ': ', aBuf[aRunLine]);
  for i := 0 to High(aRuns) do
    WriteLn('        cols ', aRuns[i].Start, '..', aRuns[i].Start + aRuns[i].Count - 1,
      '  ', ColorName(aRuns[i].Color):8, ' "',
      UTF8SubStr(aBuf[aRunLine], aRuns[i].Start, aRuns[i].Count), '"');
end;

//AQuoted as the line holds it, both quotes in all: the opening quote has to
//start the color, the closing quote has to be the last of it, and whatever
//stands behind that has to be plain text again - unless the string ends the
//line, where there is nothing behind it to be painted
procedure CheckQuoted(const AName, AQuoted: string);
var
  w: Integer;
begin
  w := ColOf(aRunLine, AQuoted);
  Check(AName + ': ' + AQuoted + ' is a string from its opening quote',
    IsColor(ColorAt(w), clGreen), ColorName(ColorAt(w)));
  Check(AName + ': ' + AQuoted + ' is a string up to its closing quote',
    IsColor(ColorAt(w + Length(AQuoted) - 1), clGreen),
    ColorName(ColorAt(w + Length(AQuoted) - 1)));
  if w + Length(AQuoted) <= LastCol then
    Check(AName + ': what follows ' + AQuoted + ' is text again',
      IsColor(ColorAt(w + Length(AQuoted)), clLightgray),
      ColorName(ColorAt(w + Length(AQuoted))));
end;

//AWord of the line in hand, painted the one color
procedure CheckColored(const AName, AWord: string; AColor: TRGBAColor);
begin
  Check(AName, IsColor(ColorOf(aRunLine, AWord), AColor),
    ColorName(ColorOf(aRunLine, AWord)));
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
  aBuf := TStringList.Create;
  aEditor := TTestEditor.Create(nil);
  try
    aEditor.BoundsRect := Rect(0, 0, 400, 300);

    WriteLn('TyroEditor syntax highlighting');
    WriteLn;

    //1. the line of the bug report: every string of it ends at its own quote
    Setup(['local b = controls.new("button", "Hi", 10, 10, 120, 40, "btn1")']);
    DumpRuns('the reported line');
    CheckColored('the reported line: local is a keyword', 'local', clYellow);
    CheckColored('the reported line: new is an api name', 'new', clSkyBlue);
    CheckColored('the reported line: a number is a number', '120', clOrange);
    CheckQuoted('the reported line', '"button"');
    CheckQuoted('the reported line', '"Hi"');
    CheckQuoted('the reported line', '"btn1"');
    Check('the reported line: the parenthesis behind the last string is text',
      IsColor(ColorAt(LastCol), clLightgray), ColorName(ColorAt(LastCol)));

    //2. a quote behind a backslash closes nothing
    Setup(['x = "a\"b" + 1']);
    DumpRuns('a quote behind a backslash');
    CheckColored('an escaped quote keeps the string open over what is behind it',
      'b', clGreen);
    CheckColored('a number behind an escaped quote is still a number', '1', clOrange);
    Check('an escaped quote still lets the closing quote close the string',
      IsColor(ColorAt(ColOf(0, 'b') + 2), clLightgray),
      ColorName(ColorAt(ColOf(0, 'b') + 2)));

    //3. a backslash behind a backslash is a backslash, not an escape
    Setup(['x = "a\\" + 2']);
    DumpRuns('a backslash behind a backslash');
    CheckColored('a doubled backslash does not swallow the closing quote', '+', clLightgray);
    CheckColored('a doubled backslash ends the string at the quote', '2', clOrange);

    //4. a string that holds nothing, and a single quoted one
    Setup(['x = "" .. ''a''']);
    DumpRuns('an empty string and a single quoted one');
    CheckQuoted('an empty string is a string of its two quotes', '""');
    CheckColored('a single quoted string is a string', '''a''', clGreen);
    CheckColored('the text between two strings is text', '..', clLightgray);

    //5. two strings on one line, the second one behind the first
    Setup(['x = "a" .. "b"']);
    DumpRuns('two strings on one line');
    CheckQuoted('the second string of a line', '"b"');
    CheckColored('the first string of a line does not run into the second', '..', clLightgray);

    //6. a string nobody closes keeps its color to the end of the line
    Setup(['x = "abc']);
    DumpRuns('a string with no closing quote');
    Check('an unclosed string runs to the end of the line',
      IsColor(ColorAt(LastCol), clGreen), ColorName(ColorAt(LastCol)));

    //7. a quote inside a comment opens nothing
    Setup(['-- "not a string" + 1']);
    DumpRuns('a quote inside a comment');
    CheckColored('a comment is a comment over a quote', '--', clGray);
    Check('a comment runs to the end of the line, quote or no quote',
      IsColor(ColorAt(LastCol), clGray), ColorName(ColorAt(LastCol)));

    //8. long strings, closed on their own line and left open over the break
    Setup(['x = [[a]] + 1']);
    DumpRuns('a long string closed on its own line');
    CheckColored('a long string is a string', '[[', clGreen);
    CheckColored('a long string ends at its closer', '+', clLightgray);
    CheckColored('a number behind a long string is a number', '1', clOrange);

    Setup(['x = [[a', 'b]] + 1']);
    DumpRuns('a long string left open over the break, the line it opened on');
    Check('a long string open at the end of a line runs to the end of it',
      IsColor(ColorAt(LastCol), clGreen), ColorName(ColorAt(LastCol)));
    Line(1);
    DumpRuns('a long string left open over the break, the line it closes on');
    Check('the line it closes on is a string up to its closer',
      IsColor(ColorAt(ColOf(1, ']]') + 1), clGreen),
      ColorName(ColorAt(ColOf(1, ']]') + 1)));
    Check('the text behind the closer is text again',
      IsColor(ColorAt(ColOf(1, ']]') + 3), clLightgray),
      ColorName(ColorAt(ColOf(1, ']]') + 3)));
    CheckColored('a number behind the closer is a number', '1', clOrange);

    //9. the words of a plain line keep the colors they had
    Setup(['local function f(n) return n * 2 end']);
    DumpRuns('a plain line of Lua');
    CheckColored('local is a keyword', 'local', clYellow);
    CheckColored('function is a keyword', 'function', clYellow);
    CheckColored('return is a keyword', 'return', clYellow);
    CheckColored('end is a keyword', 'end', clYellow);
    CheckColored('a name of your own is text', 'f(', clLightgray);
    CheckColored('a number is a number', '2', clOrange);
  finally
    aBuf.Free;
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
