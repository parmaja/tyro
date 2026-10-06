unit TyroTerminal;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *  TTyroTerminal - a terminal style control that replaces the old console:
 *    - scrollable output/history area on top (selectable and copyable)
 *    - a single editable command line pinned at the bottom
 *    - the current command word is syntax highlighted when it matches a
 *      registered builtin command
 *    - TAB completes the word under the caret: the builtin command names while
 *      the caret is inside the first word, otherwise the files of the workspace
 *      (WorkPath). TAB again walks to the next candidate, cycling at the end
 *    - up/down arrows browse the command history
 *    - holding a key auto-repeats it (text, backspace, arrows, history)
 *    - console.read() (Lua) works through StartRead/StopRead
 *
 *  TTyroOutput - the small script output panel (moved here from the old
 *  console unit).
 *
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, SyncObjs, Types,
  mnUtils,
  RayLib, RayClasses,
  TyroClasses, TyroControls;

const
  { Default character dimensions }
  CDefaultCharHeight = 8;
  CDefaultCharWidth = 8;

  { Caret timing (in seconds) }
  CCaretBlinkInterval = 0.5;
  { Minimum dim factor (0.0 = fully dimmed, 1.0 = full brightness) }
  CCaretMinDim = 0.2;

  { Default values }
  CDefaultLineCount = 1000;
  CDefaultHistoryCount = 200;

type
  TTyroTerminal = class;

  { EOnTerminalInput - Event handler for terminal input completion }
  EOnTerminalInput = procedure(ATerminal: TTyroTerminal; Input: utf8string) of object;

  { EOnTerminalInputChange - Event handler for input buffer changes }
  EOnTerminalInputChange = procedure(ATerminal: TTyroTerminal; const InputData: utf8string) of object;

  { one run of same-colored characters (columns are UTF-8 codepoints) }
  TTermRun = record
    Start: Integer;
    Count: Integer;
    Color: TColor;
  end;
  TTermRuns = array of TTermRun;

  { TTyroTerminal }

  TTyroTerminal = class(TTyroControl)
  private
    FLines: TStringList;              // output/history lines (newest at the end)
    FCharWidth: Integer;
    FCharHeight: Integer;
    FHighlightColor: TColor;          // builtin command word + prompt
    FSelectionColor: TColor;          // selection background (selected text is inverted)
    FMaxLines: Integer;
    FScrollBack: Integer;             // 0 = stick to bottom, >0 = lines scrolled up

    FInputOn: Boolean;                // true when we accept a command line
    FPrompt: utf8string;
    FInputBuffer: utf8string;             // the current command line (UTF-8)
    FInputPos: Integer;               // caret position in codepoints
    FInputScroll: Integer;            // first visible codepoint of the input line
    FInputSelStart: Integer;          // -1 = no selection
    FInputSelEnd: Integer;
    FInputSelAnchor: Integer;         // the fixed end of the selection, -1 = none
    FPasswordMode: Boolean;
    FPasswordChar: TUTF8Char;
    FOverwrite: Boolean;              // true = typing replaces the character under the caret

    FHistory: TStringList;            // previously submitted command lines
    FHistoryPos: Integer;             // -1 = not browsing history

    FCommandNames: TStringList;       // builtin command names highlighted in the input

    { Tab completion: the candidates of the word under the caret, the one the
      last TAB inserted, and where that one lives in the input line. The caret
      sitting right after it is what tells a second TAB ("next candidate") from
      a fresh completion. }
    FCompletion: TStringList;
    FCompletionIndex: Integer;        // -1 = no completion running
    FCompletionStart: Integer;        // codepoint of the first char of the completed word
    FCompletionEnd: Integer;          // codepoint right after the inserted candidate
    FCompletionCurrent: utf8string;   // exactly what the last TAB inserted
    FFileMasks: TStringList;          // masks of the files offered as arguments

    FCaretTimer: Double;
    FCaretDim: Double;
    FCaretVisible: Boolean;

    FOnInput: EOnTerminalInput;
    FOnInputChange: EOnTerminalInputChange;
    FOnAny: EOnTerminalInputChange;

    // Output (history) selection, anchored on absolute line/col
    FSelActive: Boolean;
    FSelAnchorLine: Integer;
    FSelAnchorCol: Integer;
    FSelCurLine: Integer;
    FSelCurCol: Integer;
    FMouseButtonDown: Boolean;

    function GetLineCount: Integer;
    function GetVisibleLines: Integer;
    function GetPromptX: Integer;
    function IsCommandName(const AWord: utf8string): Boolean;
    procedure SplitFirstWord(const S: utf8string; out ALeading, AWord, ARest: utf8string);
    procedure BuildInputRuns(out ARuns: TTermRuns);
    function CharAtPixel(AX: Integer): Integer;
    procedure UpdateInputScroll;

    procedure ProcessChar(var Key: TUTF8Char);
    procedure ProcessKey(var Key: TKeyboardKey; Shift: TShiftState);
    procedure SetOverwriteMode(AValue: Boolean);

    procedure AddLine(const ALine: utf8string);
    procedure AppendText(const S: utf8string);
    procedure TrimLines;
    procedure ClampScroll;
    procedure UpdateScrollBars;

    procedure ClearInputSelection;
    procedure SetInputSelection(AAnchor, ACaret: Integer);
    function SelectionAnchor: Integer;
    function InputSelectionText: utf8string;
    procedure DeleteInputSelection;
    procedure SelectInputAll;

    procedure PlaceInputCaretAt(AX: Integer);
    procedure SetSelectionAnchor(AX, AY: Integer);
    procedure ExtendSelectionTo(AX, AY: Integer);
    function OutputSelectionText: utf8string;
    procedure CopySelection;
    procedure PasteClipboard;

    procedure HistoryUp;
    procedure HistoryDown;
    procedure SubmitInput;

    function HasPrefix(const AText, APrefix: utf8string): Boolean;
    function WordAtCaret(out AStart: Integer): utf8string;
    function IsFirstWord: Boolean;
    procedure BuildCompletion(const APrefix: utf8string; ACommandPos: Boolean);
    function ApplyCompletion: Boolean;
    procedure CompleteInput;
    procedure InsertTabSpaces;
    procedure ResetCompletion;

    procedure DrawOutputLine(ACanvas: TTyroCanvas; ALine, AY: Integer);
    procedure DrawInputLine(ACanvas: TTyroCanvas; AY: Integer);
    procedure DrawCaret(ACanvas: TTyroCanvas; AY: Integer);

    procedure SetCaretVisible(AValue: Boolean);
  protected
    procedure UpdateSizes;
    procedure SizeChanged; override;
    procedure Scroll(Which: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;

    procedure Update; override; //key auto-repeat + caret blink + mouse interaction
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    procedure KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;

    procedure Clear;
    procedure Write(s: utf8string);
    procedure Writeln(s: utf8string);

    procedure StartRead(const Desc: utf8string);
    procedure StopRead;
    procedure SaveToFile(AFileName: utf8string);

    { All colors are control properties; code only reads these }
    property HighlightColor: TColor read FHighlightColor write FHighlightColor;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor;
    property CharWidth: Integer read FCharWidth write FCharWidth;
    property CharHeight: Integer read FCharHeight write FCharHeight;
    property MaxLines: Integer read FMaxLines write FMaxLines;

    property CaretVisible: Boolean read FCaretVisible write SetCaretVisible;
    property PasswordChar: TUTF8Char read FPasswordChar write FPasswordChar;
    {* Overwrite mode: a typed character replaces the one under the caret
     instead of pushing the line to the right of it. The Insert key toggles
     it, and the caret is drawn as an underline while it is on. }
    property OverwriteMode: Boolean read FOverwrite write SetOverwriteMode;

    property OnInput: EOnTerminalInput read FOnInput write FOnInput;
    property OnInputChange: EOnTerminalInputChange read FOnInputChange write FOnInputChange;
    property OnAny: EOnTerminalInputChange read FOnAny write FOnAny;

    property LineCount: Integer read GetLineCount;
    property CommandNames: TStringList read FCommandNames;

    {* Masks of the files TAB offers once the caret left the command word.
     Defaults to the script extensions; set it to '*.*' to complete any file of
     the workspace. }
    property FileMasks: TStringList read FFileMasks;
  end;

  { TTyroOutput }

  { Simple output-only control. It catches the script output (log, print,
    println) and displays the tail of the line buffer. It is positioned,
    shown and hidden from Lua (see the "output" table). }

  TTyroOutput = class(TTyroControl)
  private
    FLines: TStringList;
    FLock: TCriticalSection; //guards FLines (log() writes from the script thread)
    FMaxLines: Integer;
    procedure TrimLines;
    function GetLineCount: Integer;
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure Write(S: utf8string);
    procedure Writeln(S: utf8string);
    procedure Clear;
    property MaxLines: Integer read FMaxLines write FMaxLines default 500;
    property LineCount: Integer read GetLineCount;
  end;

implementation

{ TTyroTerminal }

constructor TTyroTerminal.Create(AParent: TTyroLayout);
var
  Builtin: array of utf8string;
  i: Integer;
begin
  inherited;
  //csFocus lets the console receive keyboard input when it is shown/focused,
  //csRepeatKeys replays a key (text, backspace, arrows, history) while it is
  //held down.
  Style := [csClip, csOpaque, csVScroll, csFocus, csRepeatKeys];
  Color := clLightGray;
  BackColor := clDarkGray;

  FCharWidth := CDefaultCharWidth;
  FCharHeight := CDefaultCharHeight;
  FMaxLines := CDefaultLineCount;
  FHighlightColor := clYellow;
  FSelectionColor := clWhite;
  FScrollBack := 0;

  FLines := TStringList.Create;

  FPrompt := '';
  FInputBuffer := '';
  FInputPos := 0;
  FInputScroll := 0;
  FInputSelStart := -1;
  FInputSelEnd := -1;
  FInputSelAnchor := -1;
  FPasswordMode := False;
  FPasswordChar := '*';
  FOverwrite := False;

  FHistory := TStringList.Create;
  FHistoryPos := -1;

  FCommandNames := TStringList.Create;
  Builtin := ['help', 'list', 'ls', 'dir', 'clear', 'cls', 'exit', 'quit', 'q',
    'stop', 'load', 'state', 'run', 'edit'];
  for i := 0 to High(Builtin) do
    FCommandNames.Add(Builtin[i]);

  //what TAB offers as arguments: the script files of the workspace
  FFileMasks := TStringList.Create;
  FFileMasks.Add('*.tyro');
  FFileMasks.Add('*.lua');

  FCompletion := nil;
  FCompletionIndex := -1;
  FCompletionStart := 0;
  FCompletionEnd := 0;
  FCompletionCurrent := '';

  FCaretTimer := 0;
  FCaretDim := 1;
  FCaretVisible := True;

  FSelActive := False;
  FMouseButtonDown := False;

  SetBoundsRect(Rect(0, 0, 200, 200));
end;

destructor TTyroTerminal.Destroy;
begin
  FreeAndNil(FCompletion);
  FreeAndNil(FFileMasks);
  FreeAndNil(FCommandNames);
  FreeAndNil(FHistory);
  FreeAndNil(FLines);
  inherited Destroy;
end;

procedure TTyroTerminal.UpdateSizes;
begin
  if Res.Font.Width > 0 then
    FCharWidth := Res.Font.Width
  else
    FCharWidth := CDefaultCharWidth;
  if Res.Font.Height > 0 then
    FCharHeight := Res.Font.Height
  else
    FCharHeight := CDefaultCharHeight;
end;

procedure TTyroTerminal.SizeChanged;
begin
  inherited;
  ClampScroll;
  UpdateScrollBars;
  Invalidate;
end;

function TTyroTerminal.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TTyroTerminal.GetVisibleLines: Integer;
begin
  if FCharHeight <= 0 then
    Exit(1);
  Result := ClientRect.Height div FCharHeight;
  if Result < 1 then
    Result := 1;
end;

function TTyroTerminal.IsCommandName(const AWord: utf8string): Boolean;
var
  i: Integer;
begin
  if AWord = '' then
    Exit(False);
  for i := 0 to FCommandNames.Count - 1 do
    if SameText(FCommandNames[i], AWord) then
      Exit(True);
  Result := False;
end;

procedure TTyroTerminal.SplitFirstWord(const S: utf8string; out ALeading, AWord, ARest: utf8string);
var
  L, P: Integer;
  Ch: utf8string;
begin
  ALeading := '';
  AWord := '';
  ARest := '';
  L := UTF8Length(S);
  P := 0;
  while P < L do
  begin
    Ch := UTF8SubStr(S, P, 1);
    if (Ch = ' ') or (Ch = #9) then
      Inc(P)
    else
      Break;
  end;
  ALeading := UTF8SubStr(S, 0, P);
  while P < L do
  begin
    Ch := UTF8SubStr(S, P, 1);
    if (Ch = ' ') or (Ch = #9) then
      Break;
    Inc(P);
  end;
  AWord := UTF8SubStr(S, UTF8Length(ALeading), P - UTF8Length(ALeading));
  ARest := UTF8SubStr(S, P, L - P);
end;

procedure TTyroTerminal.BuildInputRuns(out ARuns: TTermRuns);
var
  Leading, Word, Rest: utf8string;
  L: Integer;
begin
  ARuns := nil;
  if FPasswordMode then
  begin
    L := UTF8Length(FInputBuffer);
    if L > 0 then
    begin
      SetLength(ARuns, 1);
      ARuns[0].Start := 0;
      ARuns[0].Count := L;
      ARuns[0].Color := Color;
    end;
    Exit;
  end;
  if FInputBuffer = '' then
    Exit;
  SplitFirstWord(FInputBuffer, Leading, Word, Rest);
  if Leading <> '' then
  begin
    SetLength(ARuns, Length(ARuns) + 1);
    ARuns[High(ARuns)].Start := 0;
    ARuns[High(ARuns)].Count := UTF8Length(Leading);
    ARuns[High(ARuns)].Color := Color;
  end;
  if Word <> '' then
  begin
    SetLength(ARuns, Length(ARuns) + 1);
    ARuns[High(ARuns)].Start := UTF8Length(Leading);
    ARuns[High(ARuns)].Count := UTF8Length(Word);
    if IsCommandName(Word) then
      ARuns[High(ARuns)].Color := FHighlightColor
    else
      ARuns[High(ARuns)].Color := Color;
  end;
  if Rest <> '' then
  begin
    SetLength(ARuns, Length(ARuns) + 1);
    ARuns[High(ARuns)].Start := UTF8Length(Leading) + UTF8Length(Word);
    ARuns[High(ARuns)].Count := UTF8Length(Rest);
    ARuns[High(ARuns)].Color := Color;
  end;
end;

function TTyroTerminal.GetPromptX: Integer;
begin
  Result := UTF8Length(FPrompt) * FCharWidth;
  if (FPrompt <> '') and (UTF8SubStr(FPrompt, UTF8Length(FPrompt) - 1, 1) <> ' ') then
    Result := Result + FCharWidth; //a trailing space after the prompt
end;

function TTyroTerminal.CharAtPixel(AX: Integer): Integer;
var
  L: Integer;
begin
  L := UTF8Length(FInputBuffer);
  Result := FInputScroll + ((AX - GetPromptX) div FCharWidth);
  if Result < FInputScroll then
    Result := FInputScroll;
  if Result > L then
    Result := L;
  if Result < 0 then
    Result := 0;
end;

procedure TTyroTerminal.UpdateInputScroll;
var
  VisibleCols: Integer;
  v: Integer;
begin
  VisibleCols := (ClientRect.Width - GetPromptX) div FCharWidth;
  if VisibleCols < 1 then
    VisibleCols := 1;
  if FInputPos < FInputScroll then
    FInputScroll := FInputPos;
  v := FInputPos - FInputScroll;
  if v >= VisibleCols then
    FInputScroll := FInputPos - VisibleCols + 1;
  if FInputScroll < 0 then
    FInputScroll := 0;
end;

procedure TTyroTerminal.AddLine(const ALine: utf8string);
begin
  FLines.Add(ALine);
end;

procedure TTyroTerminal.AppendText(const S: utf8string);
var
  P, l: Integer;
begin
  if S = '' then
    Exit;
  if FLines.Count = 0 then
    AddLine('');
  P := 1;
  while P <= Length(S) do
  begin
    l := 1;
    case S[P] of
      #13:
      begin
        //ignore CR
      end;
      #10:
      begin
        AddLine('');
      end;
      #9:
      begin
        FLines[FLines.Count - 1] := FLines[FLines.Count - 1] + '    ';
      end;
      else
      begin
        l := UTF8CodepointSize(@S[P]);
        FLines[FLines.Count - 1] := FLines[FLines.Count - 1] + Copy(S, P, l);
      end;
    end;
    Inc(P, l);
  end;
end;

procedure TTyroTerminal.TrimLines;
begin
  while FLines.Count > FMaxLines do
    FLines.Delete(0);
end;

procedure TTyroTerminal.ClampScroll;
var
  MaxScroll: Integer;
  vis: Integer;
begin
  vis := GetVisibleLines - 1;
  if vis < 0 then
    vis := 0;
  MaxScroll := FLines.Count - vis;
  if MaxScroll < 0 then
    MaxScroll := 0;
  if FScrollBack < 0 then
    FScrollBack := 0;
  if FScrollBack > MaxScroll then
    FScrollBack := MaxScroll;
end;

procedure TTyroTerminal.UpdateScrollBars;
var
  vis, MaxScroll: Integer;
begin
  vis := GetVisibleLines;
  MaxScroll := FLines.Count - vis;
  if MaxScroll < 0 then
    MaxScroll := 0;
  if MaxScroll > 0 then
  begin
    ShowScrollBar([sbtVertical], True);
    SetScrollRange(sbtVertical, 0, MaxScroll, vis);
    SetScrollPosition(sbtVertical, FScrollBack, True);
  end
  else
    ShowScrollBar([sbtVertical], False);
end;

procedure TTyroTerminal.Scroll(Which: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
var
  vis, MaxScroll: Integer;
begin
  if Which <> sbtVertical then
  begin
    inherited Scroll(Which, ScrollCode, Pos);
    Exit;
  end;
  vis := GetVisibleLines;
  MaxScroll := FLines.Count - vis;
  if MaxScroll < 0 then
    MaxScroll := 0;
  case ScrollCode of
    scrollTOP: FScrollBack := 0;
    scrollBOTTOM: FScrollBack := MaxScroll;
    scrollLINEDOWN: Inc(FScrollBack);
    scrollLINEUP: Dec(FScrollBack);
    scrollPAGEDOWN: Inc(FScrollBack, vis);
    scrollPAGEUP: Dec(FScrollBack, vis);
    scrollTHUMBPOSITION, scrollTHUMBTRACK: FScrollBack := Pos;
    scrollENDSCROLL: ;
  end;
  ClampScroll;
  UpdateScrollBars;
  Invalidate;
end;

{ Drops the running completion, so the next TAB builds a fresh list from
  whatever the input line holds now. }
procedure TTyroTerminal.ResetCompletion;
begin
  FreeAndNil(FCompletion);
  FCompletionIndex := -1;
  FCompletionStart := 0;
  FCompletionEnd := 0;
  FCompletionCurrent := '';
end;

procedure TTyroTerminal.Clear;
begin
  FLines.Clear;
  FScrollBack := 0;
  ResetCompletion;
  FInputBuffer := '';
  FInputPos := 0;
  FInputScroll := 0;
  ClearInputSelection;
  ClearKeyRepeat;
  FSelActive := False;
  Invalidate;
end;

procedure TTyroTerminal.Write(s: utf8string);
begin
  AppendText(s);
  ClampScroll;
  Invalidate;
end;

procedure TTyroTerminal.Writeln(s: utf8string);
begin
  Write(s);
  Write(#10);
end;

procedure TTyroTerminal.StartRead(const Desc: utf8string);
begin
  FPrompt := Desc;
  FInputBuffer := '';
  FInputPos := 0;
  FInputScroll := 0;
  ResetCompletion;
  ClearInputSelection;
  ClearKeyRepeat;
  FInputOn := True;
  FHistoryPos := -1;
  Invalidate;
end;

procedure TTyroTerminal.StopRead;
begin
  FInputOn := False;
  ClearKeyRepeat;
  Invalidate;
end;

procedure TTyroTerminal.SaveToFile(AFileName: utf8string);
var
  Txt: System.Text;
  i: Integer;
begin
  AssignFile(Txt, AFileName);
  Rewrite(Txt);
  for i := 0 to FLines.Count - 1 do
    system.Writeln(Txt, FLines[i]);
  CloseFile(Txt);
end;

{ selection }

procedure TTyroTerminal.ClearInputSelection;
begin
  FInputSelStart := -1;
  FInputSelEnd := -1;
  FInputSelAnchor := -1;
end;

{ Selects from the anchor to the caret (both in codepoints) and moves the caret
  there, clamped to the input line. The anchor is remembered, so a following
  shift+arrow/shift+home/shift+end keeps growing the same selection in either
  direction. A zero-length selection clears it. }
procedure TTyroTerminal.SetInputSelection(AAnchor, ACaret: Integer);
var
  L: Integer;
begin
  L := UTF8Length(FInputBuffer);
  if AAnchor < 0 then
    AAnchor := 0;
  if AAnchor > L then
    AAnchor := L;
  if ACaret < 0 then
    ACaret := 0;
  if ACaret > L then
    ACaret := L;
  FInputSelAnchor := AAnchor;
  FInputPos := ACaret;
  if ACaret < AAnchor then
  begin
    FInputSelStart := ACaret;
    FInputSelEnd := AAnchor;
  end
  else
  begin
    FInputSelStart := AAnchor;
    FInputSelEnd := ACaret;
  end;
  if FInputSelStart = FInputSelEnd then
    ClearInputSelection;
  UpdateInputScroll;
end;

{ The end the caret moves away from: the selection anchor while a selection
  exists, the caret itself otherwise. }
function TTyroTerminal.SelectionAnchor: Integer;
begin
  if FInputSelStart >= 0 then
    Result := FInputSelAnchor
  else
    Result := FInputPos;
end;

function TTyroTerminal.InputSelectionText: utf8string;
begin
  Result := '';
  if FInputSelStart < 0 then
    Exit;
  if FPasswordMode then
    Result := StringOfChar(Char(FPasswordChar[1]), FInputSelEnd - FInputSelStart)
  else
    Result := UTF8SubStr(FInputBuffer, FInputSelStart, FInputSelEnd - FInputSelStart);
end;

procedure TTyroTerminal.DeleteInputSelection;
begin
  if FInputSelStart < 0 then
    Exit;
  FInputBuffer := UTF8Delete(FInputBuffer, FInputSelStart, FInputSelEnd - FInputSelStart);
  FInputPos := FInputSelStart;
  ClearInputSelection;
end;

procedure TTyroTerminal.SelectInputAll;
begin
  SetInputSelection(0, UTF8Length(FInputBuffer));
end;

procedure TTyroTerminal.PlaceInputCaretAt(AX: Integer);
begin
  ResetCompletion;       //the caret moved: the next TAB completes the new word
  FInputPos := CharAtPixel(AX);
  if FInputPos < 0 then
    FInputPos := 0;
  UpdateInputScroll;
end;

procedure TTyroTerminal.SetSelectionAnchor(AX, AY: Integer);
var
  Line, Col, visible: Integer;
begin
  visible := GetVisibleLines - 1;
  Line := 0;
  Col := 0;
  if (visible > 0) and (FLines.Count > 0) then
  begin
    // map output row to the visible line index
    Line := FScrollBack;
    // the row clicked: rows [0..visible-1] map to lines [top..bottom]
    // bottom = FLines.Count-1-FScrollBack
    if AY >= 0 then
      Line := (FLines.Count - 1 - FScrollBack) - (visible - 1 - (AY div FCharHeight));
    if Line < 0 then
      Line := 0;
    if Line >= FLines.Count then
      Line := FLines.Count - 1;
    Col := AX div FCharWidth;
    if Col < 0 then
      Col := 0;
    if Col > UTF8Length(FLines[Line]) then
      Col := UTF8Length(FLines[Line]);
  end;
  FSelAnchorLine := Line;
  FSelAnchorCol := Col;
  FSelCurLine := Line;
  FSelCurCol := Col;
  FSelActive := True;
end;

procedure TTyroTerminal.ExtendSelectionTo(AX, AY: Integer);
var
  Line, Col, visible: Integer;
begin
  visible := GetVisibleLines - 1;
  Line := FSelCurLine;
  Col := FSelCurCol;
  if (visible > 0) and (FLines.Count > 0) then
  begin
    Line := (FLines.Count - 1 - FScrollBack) - (visible - 1 - (AY div FCharHeight));
    if Line < 0 then
      Line := 0;
    if Line >= FLines.Count then
      Line := FLines.Count - 1;
    Col := AX div FCharWidth;
    if Col < 0 then
      Col := 0;
    if Col > UTF8Length(FLines[Line]) then
      Col := UTF8Length(FLines[Line]);
  end;
  FSelCurLine := Line;
  FSelCurCol := Col;
end;

function TTyroTerminal.OutputSelectionText: utf8string;
var
  l1, c1, l2, c2, i: Integer;
begin
  Result := '';
  if not FSelActive then
    Exit;
  //normalize
  if (FSelAnchorLine > FSelCurLine) or ((FSelAnchorLine = FSelCurLine) and (FSelAnchorCol > FSelCurCol)) then
  begin
    l1 := FSelCurLine; c1 := FSelCurCol;
    l2 := FSelAnchorLine; c2 := FSelAnchorCol;
  end
  else
  begin
    l1 := FSelAnchorLine; c1 := FSelAnchorCol;
    l2 := FSelCurLine; c2 := FSelCurCol;
  end;
  if l1 >= FLines.Count then
    Exit;
  if l2 >= FLines.Count then
    l2 := FLines.Count - 1;
  for i := l1 to l2 do
  begin
    if i > l1 then
      Result := Result + #13#10;
    if i = l1 then
      Result := Result + UTF8SubStr(FLines[i], c1, UTF8Length(FLines[i]) - c1)
    else if i = l2 then
      Result := Result + UTF8SubStr(FLines[i], 0, c2)
    else
      Result := Result + FLines[i];
  end;
end;

procedure TTyroTerminal.CopySelection;
begin
  if FSelActive then
    RayLib.SetClipboardText(PUTF8Char(OutputSelectionText))
  else if FInputSelStart >= 0 then
    RayLib.SetClipboardText(PUTF8Char(InputSelectionText));
end;

{ Inserts the clipboard at the caret, replacing the input selection. Line ends
  are dropped: the command line is a single line. }
procedure TTyroTerminal.PasteClipboard;
var
  P: PUTF8Char;
  S: utf8string;
begin
  P := RayLib.GetClipboardText;
  S := '';
  if P <> nil then
    S := PUTF8Char(P);
  S := StringReplace(S, #13, '', [rfReplaceAll]);
  S := StringReplace(S, #10, '', [rfReplaceAll]);
  if S = '' then
    Exit;
  if FInputSelStart >= 0 then
    DeleteInputSelection;
  ResetCompletion;       //the pasted text replaces what a completion covered
  FInputBuffer := UTF8Insert(FInputBuffer, FInputPos, S);
  Inc(FInputPos, UTF8Length(S));
  UpdateInputScroll;
  if Assigned(FOnInputChange) then
    FOnInputChange(Self, FInputBuffer);
  if Assigned(FOnAny) then
    FOnAny(Self, FInputBuffer);
  Invalidate;
end;

{ history }

procedure TTyroTerminal.HistoryUp;
begin
  if FHistory.Count = 0 then
    Exit;
  ResetCompletion;
  if FHistoryPos < 0 then
    FHistoryPos := FHistory.Count - 1
  else if FHistoryPos > 0 then
    Dec(FHistoryPos);
  FInputBuffer := FHistory[FHistoryPos];
  FInputPos := UTF8Length(FInputBuffer);
  UpdateInputScroll;
  ClearInputSelection;
  Invalidate;
end;

procedure TTyroTerminal.HistoryDown;
begin
  if FHistoryPos < 0 then
    Exit;
  ResetCompletion;
  Inc(FHistoryPos);
  if FHistoryPos >= FHistory.Count then
  begin
    FHistoryPos := -1;
    FInputBuffer := '';
  end
  else
    FInputBuffer := FHistory[FHistoryPos];
  FInputPos := UTF8Length(FInputBuffer);
  UpdateInputScroll;
  ClearInputSelection;
  Invalidate;
end;

procedure TTyroTerminal.SubmitInput;
var
  s: utf8string;
begin
  if not FInputOn then
    Exit;
  s := FInputBuffer;
  if FPasswordMode then
  begin
    Writeln(''); //do not echo passwords
  end
  else
    Writeln(s);
  if s <> '' then
  begin
    FHistory.Add(s);
    while FHistory.Count > CDefaultHistoryCount do
      FHistory.Delete(0);
  end;
  FHistoryPos := -1;
  FInputBuffer := '';
  FInputPos := 0;
  FInputScroll := 0;
  ResetCompletion;
  ClearInputSelection;
  ClearKeyRepeat;
  FInputOn := False;
  if Assigned(FOnInput) then
    FOnInput(Self, s);
  Invalidate;
end;

{ tab completion }

{ Case-insensitive prefix test. Working on the codepoints, not on bytes, so a
  multi-byte prefix is compared the way the user sees it. }
function TTyroTerminal.HasPrefix(const AText, APrefix: utf8string): Boolean;
begin
  if APrefix = '' then
    Exit(True);
  if UTF8Length(AText) < UTF8Length(APrefix) then
    Exit(False);
  Result := SameText(AText, APrefix) or
    SameText(UTF8SubStr(AText, 0, UTF8Length(APrefix)), APrefix);
end;

{ The word the caret sits in or next to, and where it starts. Scanning stops at
  a space or a tab, so a caret between two words completes the one it touches
  (the right one first), and a caret next to a space completes the word before
  it. An empty result means there is no word to complete. }
function TTyroTerminal.WordAtCaret(out AStart: Integer): utf8string;
var
  L, Start, Stop: Integer;
  Ch: utf8string;
begin
  Result := '';
  AStart := 0;
  L := UTF8Length(FInputBuffer);
  //back up to the first character of the word the caret touches: a caret inside
  //a word or right after it belongs to that word, a caret in a run of spaces
  //belongs to the word it faces (the one to its right)
  Start := FInputPos;
  while Start > 0 do
  begin
    Ch := UTF8SubStr(FInputBuffer, Start - 1, 1);
    if (Ch = ' ') or (Ch = #9) then
      Break;
    Dec(Start);
  end;
  if Start > L then
    Start := L;
  //and forward to its last character
  Stop := Start;
  while Stop < L do
  begin
    Ch := UTF8SubStr(FInputBuffer, Stop, 1);
    if (Ch = ' ') or (Ch = #9) then
      Break;
    Inc(Stop);
  end;
  AStart := Start;
  Result := UTF8SubStr(FInputBuffer, Start, Stop - Start);
end;

{ True when the caret belongs to the command word (the first word of the line)
  rather than to an argument. An empty first word still counts as the command:
  that is where the command names are offered. }
function TTyroTerminal.IsFirstWord: Boolean;
var
  Leading, Word, Rest: utf8string;
  WordEnd: Integer;
begin
  SplitFirstWord(FInputBuffer, Leading, Word, Rest);
  //one past the last char of the command word, so a caret right after it counts
  WordEnd := UTF8Length(Leading) + UTF8Length(Word);
  Result := (Word = '') or (FInputPos <= WordEnd);
end;

{ Fills FCompletion with the candidates that start with APrefix: the command
  names for the first word, otherwise the files of the workspace that match one
  of FFileMasks. }
procedure TTyroTerminal.BuildCompletion(const APrefix: utf8string; ACommandPos: Boolean);
var
  i, Mask: Integer;
  sr: TSearchRec;
  DirPath: utf8string;
begin
  ResetCompletion;
  FCompletion := TStringList.Create;
  FCompletion.Sorted := True;   //cycle in a stable, alphabetical order
  FCompletion.Duplicates := dupIgnore;
  if ACommandPos then
  begin
    for i := 0 to FCommandNames.Count - 1 do
      if HasPrefix(FCommandNames[i], APrefix) then
        FCompletion.Add(FCommandNames[i]);
  end
  else
  begin
    DirPath := IncludePathDelimiter(Res.WorkPath);
    if DirPath = PathDelim then
      DirPath := PathDelim
    else if DirPath = '' then
      Exit;
    for Mask := 0 to FFileMasks.Count - 1 do
      if (FFileMasks[Mask] <> '') and
        (SysUtils.FindFirst(DirPath + FFileMasks[Mask], faAnyFile, sr) = 0) then
      begin
        try
          repeat
            if ((sr.Attr and faDirectory) = 0) and HasPrefix(sr.Name, APrefix) then
              FCompletion.Add(sr.Name);
          until FindNext(sr) <> 0;
        finally
          FindClose(sr);
        end;
      end;
  end;
end;

{ Replaces the word the completion covers with the current candidate. The span
  grows and shrinks with the candidate, so cycling back and forth restores the
  original text exactly. }
function TTyroTerminal.ApplyCompletion: Boolean;
var
  Word: utf8string;
begin
  Result := False;
  if (FCompletion = nil) or (FCompletionIndex < 0) or
    (FCompletionIndex >= FCompletion.Count) then
    Exit;
  Word := UTF8SubStr(FInputBuffer, FCompletionStart,
    FCompletionEnd - FCompletionStart);
  //keep the span identical when the candidate already is the typed word
  if Word = FCompletion[FCompletionIndex] then
    Exit;
  FInputBuffer := UTF8SubStr(FInputBuffer, 0, FCompletionStart) +
    FCompletion[FCompletionIndex] +
    UTF8SubStr(FInputBuffer, FCompletionEnd, UTF8Length(FInputBuffer) - FCompletionEnd);
  FCompletionCurrent := FCompletion[FCompletionIndex];
  FInputPos := FCompletionStart + UTF8Length(FCompletionCurrent);
  //the covered span follows the candidate, so the next TAB knows it is still
  //looking at a completion and not at a word the user typed
  FCompletionEnd := FInputPos;
  UpdateInputScroll;
  Result := True;
end;

{ TAB: completes the word under the caret, or moves to the next candidate when
  the last one is still in place and untouched. A password prompt completes
  nothing, and a word with no candidate at all falls back to a plain tab so the
  key is never dead. }
procedure TTyroTerminal.CompleteInput;
var
  Prefix: utf8string;
  Start: Integer;
  CommandPos: Boolean;
begin
  if FPasswordMode then
    Exit;
  //a completion still standing and unedited cycles to the next candidate
  if (FCompletion <> nil) and (FCompletionIndex >= 0) and
    (FInputPos = FCompletionEnd) and
    (UTF8SubStr(FInputBuffer, FCompletionStart, UTF8Length(FCompletionCurrent)) = FCompletionCurrent) then
  begin
    Inc(FCompletionIndex);
    if FCompletionIndex >= FCompletion.Count then
      FCompletionIndex := 0; //cycle back to the first
    if not ApplyCompletion then
      Exit;
  end
  else
  begin
    Prefix := WordAtCaret(Start);
    //the first word is the command itself, everything after it is an argument
    CommandPos := IsFirstWord;
    BuildCompletion(Prefix, CommandPos);
    if FCompletion.Count = 0 then
    begin
      InsertTabSpaces;
      Exit;
    end;
    FCompletionIndex := 0;
    FCompletionStart := Start;
    FCompletionEnd := Start + UTF8Length(Prefix);
    if not ApplyCompletion then
      Exit;
  end;
  ClearInputSelection;
  if Assigned(FOnInputChange) then
    FOnInputChange(Self, FInputBuffer);
  if Assigned(FOnAny) then
    FOnAny(Self, FInputBuffer);
  Invalidate;
end;

procedure TTyroTerminal.InsertTabSpaces;
begin
  if FInputSelStart >= 0 then
    DeleteInputSelection;
  FInputBuffer := UTF8Insert(FInputBuffer, FInputPos, '    ');
  Inc(FInputPos, 4);
  UpdateInputScroll;
  if Assigned(FOnInputChange) then
    FOnInputChange(Self, FInputBuffer);
  if Assigned(FOnAny) then
    FOnAny(Self, FInputBuffer);
  Invalidate;
end;

{ input }

procedure TTyroTerminal.KeyPress(var Key: TUTF8Char);
var
  S: utf8string;
begin
  if not FInputOn then
    Exit;
  S := CharOf(Key);      //the bytes as they came, not through the codepage of the machine
  if S = '' then
    Exit;
  //arm auto-repeat for this character; the same physical key is also reported
  //as a key press (without a character) right before it
  TrackKeyRepeat(CharToKey(S), S);
  ProcessChar(Key);
end;

procedure TTyroTerminal.SetOverwriteMode(AValue: Boolean);
begin
  if FOverwrite = AValue then
    Exit;
  FOverwrite := AValue;
  Invalidate;
end;

procedure TTyroTerminal.ProcessChar(var Key: TUTF8Char);
var
  S: utf8string;
  HadSelection: Boolean;
  L, N: Integer;
begin
  if not FInputOn then
    Exit;
  S := CharOf(Key);      //the bytes as they came, not through the codepage of the machine
  if S = '' then
    Exit;
  ResetCompletion;       //the typed char invalidates a running completion
  HadSelection := FInputSelStart >= 0;
  if HadSelection then
    DeleteInputSelection;
  L := UTF8Length(FInputBuffer);
  N := UTF8Length(S);
  { Overwrite mode (the Insert key) replaces the character under the caret
    instead of pushing the rest of the line to the right. At the end of the line
    there is nothing to replace, so the character is appended as usual - and so
    is one typed over a selection, which has already swallowed the characters it
    covered. }
  if FOverwrite and (not HadSelection) and (FInputPos < L) then
    FInputBuffer := UTF8SubStr(FInputBuffer, 0, FInputPos) + S +
      UTF8SubStr(FInputBuffer, FInputPos + 1, L - FInputPos - 1)
  else
    FInputBuffer := UTF8Insert(FInputBuffer, FInputPos, S);
  Inc(FInputPos, N);
  UpdateInputScroll;
  if Assigned(FOnInputChange) then
    FOnInputChange(Self, FInputBuffer);
  if Assigned(FOnAny) then
    FOnAny(Self, FInputBuffer);
  Invalidate;
  Key := '';
end;

procedure TTyroTerminal.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
begin
  if not FInputOn then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;

  //Ctrl/Alt combinations are one-shot shortcuts (copy/paste/select-all) and
  //must not repeat; a character typed with AltGr still arms the repeat through
  //KeyPress, which arrives right after this key press
  if (ssCtrl in Shift) or (ssAlt in Shift) then
    ClearKeyRepeat
  else
    TrackKeyRepeat(Key, '');

  ProcessKey(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TTyroTerminal.ProcessKey(var Key: TKeyboardKey; Shift: TShiftState);
begin
  case Key of
    KEY_ENTER:
    begin
      SubmitInput;
      Key := KEY_NULL;
    end;
    KEY_KP_ENTER:
    begin
      SubmitInput;
      Key := KEY_NULL;
    end;
    KEY_ESCAPE:
    begin
      if FInputBuffer <> '' then
      begin
        FInputBuffer := '';
        FInputPos := 0;
        FInputScroll := 0;
        ClearInputSelection;
        Invalidate;
        Key := KEY_NULL;
      end
      else
        Key := KEY_NULL;
    end;
    KEY_TAB:
    begin
      if not (ssCtrl in Shift) then
      begin
        CompleteInput;
        Key := KEY_NULL;
      end;
    end;
    KEY_BACKSPACE:
    begin
      ResetCompletion;
      if FInputSelStart >= 0 then
        DeleteInputSelection
      else if FInputPos > 0 then
      begin
        Dec(FInputPos);
        FInputBuffer := UTF8Delete(FInputBuffer, FInputPos, 1);
        UpdateInputScroll;
      end;
      if Assigned(FOnInputChange) then
        FOnInputChange(Self, FInputBuffer);
      if Assigned(FOnAny) then
        FOnAny(Self, FInputBuffer);
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_DELETE:
    begin
      ResetCompletion;
      if FInputSelStart >= 0 then
        DeleteInputSelection
      else if FInputPos < UTF8Length(FInputBuffer) then
      begin
        FInputBuffer := UTF8Delete(FInputBuffer, FInputPos, 1);
        UpdateInputScroll;
      end;
      if Assigned(FOnInputChange) then
        FOnInputChange(Self, FInputBuffer);
      if Assigned(FOnAny) then
        FOnAny(Self, FInputBuffer);
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_LEFT:
    begin
      if ssShift in Shift then
      begin
        //the anchor stays where the selection started, the caret walks away
        //from it, so repeated presses grow the selection in both directions
        SetInputSelection(SelectionAnchor, FInputPos - 1);
      end
      else
      begin
        if FInputSelStart >= 0 then
          FInputPos := FInputSelStart //jump to the low edge of the selection
        else if FInputPos > 0 then
          Dec(FInputPos);
        ClearInputSelection;
        UpdateInputScroll;
      end;
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_RIGHT:
    begin
      if ssShift in Shift then
      begin
        SetInputSelection(SelectionAnchor, FInputPos + 1);
      end
      else
      begin
        if FInputSelStart >= 0 then
          FInputPos := FInputSelEnd //jump to the high edge of the selection
        else if FInputPos < UTF8Length(FInputBuffer) then
          Inc(FInputPos);
        ClearInputSelection;
        UpdateInputScroll;
      end;
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_HOME:
    begin
      if ssShift in Shift then
        SetInputSelection(SelectionAnchor, 0) //select back to the start
      else
      begin
        FInputPos := 0;
        ClearInputSelection;
        UpdateInputScroll;
      end;
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_END:
    begin
      if ssShift in Shift then
        SetInputSelection(SelectionAnchor, UTF8Length(FInputBuffer)) //select to the end
      else
      begin
        FInputPos := UTF8Length(FInputBuffer);
        ClearInputSelection;
        UpdateInputScroll;
      end;
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_UP:
    begin
      HistoryUp;
      Key := KEY_NULL;
    end;
    KEY_DOWN:
    begin
      HistoryDown;
      Key := KEY_NULL;
    end;
    KEY_C:
    begin
      if ssCtrl in Shift then
      begin
        CopySelection;
        Key := KEY_NULL;
      end;
    end;
    KEY_A:
    begin
      if ssCtrl in Shift then
      begin
        SelectInputAll;
        Key := KEY_NULL;
      end;
    end;
    KEY_V:
    begin
      if ssCtrl in Shift then
      begin
        PasteClipboard;
        Key := KEY_NULL;
      end;
    end;
    KEY_INSERT:
    begin
      //the traditional console keys: CTRL+INSERT copies, SHIFT+INSERT pastes
      if ssCtrl in Shift then
      begin
        CopySelection;
        Key := KEY_NULL;
      end
      else if ssShift in Shift then
      begin
        PasteClipboard;
        Key := KEY_NULL;
      end
      else
      begin
        //a plain INSERT switches between insert and overwrite mode
        FOverwrite := not FOverwrite;
        Key := KEY_NULL;
        Invalidate;
      end;
    end;
  else
    ;
  end;
end;

{ paint }

procedure TTyroTerminal.DrawOutputLine(ACanvas: TTyroCanvas; ALine, AY: Integer);
var
  S: utf8string;
  c: TColor;
  l1, c1, l2, c2, selStart, selEnd, L: Integer;
  Prefix, SelText, Suffix: utf8string;
begin
  if (ALine < 0) or (ALine >= FLines.Count) then
    Exit;
  S := FLines[ALine];
  c := Color;

  selStart := -1;
  selEnd := -1;
  if FSelActive then
  begin
    if (FSelAnchorLine > FSelCurLine) or ((FSelAnchorLine = FSelCurLine) and (FSelAnchorCol > FSelCurCol)) then
    begin
      l1 := FSelCurLine; c1 := FSelCurCol;
      l2 := FSelAnchorLine; c2 := FSelAnchorCol;
    end
    else
    begin
      l1 := FSelAnchorLine; c1 := FSelAnchorCol;
      l2 := FSelCurLine; c2 := FSelCurCol;
    end;
    if (ALine >= l1) and (ALine <= l2) and (l1 <= l2) then
    begin
      if ALine = l1 then
        selStart := c1
      else
        selStart := 0;
      if ALine = l2 then
        selEnd := c2
      else
        selEnd := UTF8Length(S);
      if selEnd < selStart then
        selEnd := selStart;
    end;
  end;

  if S = '' then
    Exit;

  L := UTF8Length(S);
  if (selStart >= 0) and (selEnd > selStart) then
  begin
    Prefix := UTF8SubStr(S, 0, selStart);
    SelText := UTF8SubStr(S, selStart, selEnd - selStart);
    Suffix := UTF8SubStr(S, selEnd, L - selEnd);
    if Prefix <> '' then
      ACanvas.DrawText(0, AY, Prefix, c);
    ACanvas.DrawRectangle(selStart * FCharWidth, AY, (selEnd - selStart) * FCharWidth, FCharHeight, FSelectionColor, True);
    if SelText <> '' then
      ACanvas.DrawText(selStart * FCharWidth, AY, SelText, BackColor);
    if Suffix <> '' then
      ACanvas.DrawText(selEnd * FCharWidth, AY, Suffix, c);
  end
  else
    ACanvas.DrawText(0, AY, S, c);
end;

procedure TTyroTerminal.DrawInputLine(ACanvas: TTyroCanvas; AY: Integer);
var
  Runs: TTermRuns;
  promptX, L, maxVis, vStart, vEnd, i, rStart, rEnd, dStart, cnt: Integer;
  disp: utf8string;
  x: Integer;
  Sc, sE: Integer;
begin
  ACanvas.DrawRectangle(0, AY, ClientRect.Width, FCharHeight, BackColor, True);
  if (ClientRect.Width <= 0) or (FCharWidth <= 0) then
    Exit;

  promptX := 0;
  if FPrompt <> '' then
  begin
    disp := FPrompt;
    if UTF8SubStr(disp, UTF8Length(disp) - 1, 1) <> ' ' then
      disp := disp + ' ';
    ACanvas.DrawText(0, AY, disp, FHighlightColor);
    promptX := UTF8Length(disp) * FCharWidth;
  end;

  if FInputBuffer = '' then
    Exit;
  L := UTF8Length(FInputBuffer);
  maxVis := (ClientRect.Width - promptX) div FCharWidth;
  if maxVis < 1 then
    maxVis := 1;
  vStart := FInputScroll;
  vEnd := vStart + maxVis;
  if vEnd > L then
    vEnd := L;

  BuildInputRuns(Runs);
  for i := 0 to High(Runs) do
  begin
    rStart := Runs[i].Start;
    rEnd := rStart + Runs[i].Count;
    if rEnd <= vStart then
      Continue;
    if rStart >= vEnd then
      Break;
    dStart := rStart;
    if dStart < vStart then
      dStart := vStart;
    cnt := rEnd - dStart;
    if cnt > vEnd - dStart then
      cnt := vEnd - dStart;
    if cnt <= 0 then
      Continue;
    x := promptX + (dStart - FInputScroll) * FCharWidth;
    if FPasswordMode then
      ACanvas.DrawText(x, AY, StringOfChar(Char(FPasswordChar[1]), cnt), Runs[i].Color)
    else
      ACanvas.DrawText(x, AY, UTF8SubStr(FInputBuffer, dStart, cnt), Runs[i].Color);
  end;

  //selection background in the input line
  if FInputSelStart >= 0 then
  begin
    Sc := FInputSelStart;
    sE := FInputSelEnd;
    if Sc < vStart then
      Sc := vStart;
    if sE > vEnd then
      sE := vEnd;
    if sE > Sc then
    begin
      x := promptX + (Sc - FInputScroll) * FCharWidth;
      ACanvas.DrawRectangle(x, AY, (sE - Sc) * FCharWidth, FCharHeight, FSelectionColor, True);
      if FPasswordMode then
        ACanvas.DrawText(x, AY, StringOfChar(Char(FPasswordChar[1]), sE - Sc), BackColor)
      else
        ACanvas.DrawText(x, AY, UTF8SubStr(FInputBuffer, Sc, sE - Sc), BackColor);
    end;
  end;
end;

procedure TTyroTerminal.DrawCaret(ACanvas: TTyroCanvas; AY: Integer);
var
  x: Integer;
  col: TColor;
begin
  if not (FInputOn and Focused and FCaretVisible) then
    Exit;
  if (FInputPos < FInputScroll) or (FInputPos > FInputScroll + (ClientRect.Width - GetPromptX) div FCharWidth) then
    Exit;
  x := GetPromptX + (FInputPos - FInputScroll) * FCharWidth;
  if x < 0 then
    x := 0;
  if x >= ClientRect.Width then
    Exit;
  col := Color.SetAlpha(Round(Color.RGBA.Alpha * FCaretDim));
  if FOverwrite then
    //overwrite is shown by an underline, the sign every other editor uses
    ACanvas.FillRect(x, AY + FCharHeight - 2, x + FCharWidth, AY + FCharHeight, col)
  else
    ACanvas.FillRect(x, AY, x + 2, AY + FCharHeight, col);
end;

procedure TTyroTerminal.DoPaint(ACanvas: TTyroCanvas);
var
  vis, outRows, i, y, bottom, top, Line: Integer;
begin
  inherited;
  UpdateSizes;
  vis := GetVisibleLines;
  outRows := vis - 1;
  if outRows < 0 then
    outRows := 0;

  if FLines.Count > 0 then
  begin
    bottom := FLines.Count - 1 - FScrollBack;
    if bottom < 0 then
      bottom := 0;
    top := bottom - outRows + 1;
    if top < 0 then
      top := 0;
    y := 0;
    for i := top to bottom do
    begin
      if y + FCharHeight <= ClientRect.Height then
        DrawOutputLine(ACanvas, i, y);
      Inc(y, FCharHeight);
    end;
  end
  else if outRows > 0 then
  begin
    bottom := 0;
    top := 0;
  end;

  if FInputOn then
  begin
    Line := (vis - 1) * FCharHeight;
    if Line < ClientRect.Height then
    begin
      DrawInputLine(ACanvas, Line);
      DrawCaret(ACanvas, Line);
    end;
  end;
end;

procedure TTyroTerminal.SetCaretVisible(AValue: Boolean);
begin
  if FCaretVisible = AValue then
    Exit;
  FCaretVisible := AValue;
  Invalidate;
end;

procedure TTyroTerminal.Update;
var
  mp: TVector2;
  lx, ly: Integer;
  visRows, inputTop: Integer;
  wheel: Single;
  Hovering: Boolean;
begin
  UpdateSizes;
  if not Visible then
  begin
    ClearKeyRepeat; //a hidden terminal must not keep replaying a held key
    Exit;
  end;
  //replay a held key (csRepeatKeys) only while a read accepts input
  if FInputOn then
    inherited
  else
    ClearKeyRepeat;
  UpdateScrollBars;

  // caret blink
  if Focused then
  begin
    FCaretTimer := FCaretTimer + RayLib.GetFrameTime();
    if FCaretTimer >= CCaretBlinkInterval then
      FCaretTimer := FCaretTimer - CCaretBlinkInterval;
    FCaretDim := Sin(FCaretTimer / CCaretBlinkInterval * 2 * Pi);
    FCaretDim := CCaretMinDim + (FCaretDim + 1) / 2 * (1 - CCaretMinDim);
    FCaretVisible := True;
    Invalidate;
  end
  else
  begin
    FCaretTimer := 0;
    FCaretDim := 1;
    FCaretVisible := True;
  end;

  visRows := GetVisibleLines;
  inputTop := (visRows - 1) * FCharHeight;

  mp := RayLib.GetMousePosition;
  lx := Round(mp.X) - WindowRect.Left - ClientRect.Left;
  ly := Round(mp.Y) - WindowRect.Top - ClientRect.Top;
  Hovering := (lx >= 0) and (ly >= 0) and (lx < ClientRect.Width) and (ly <= ClientRect.Height);

  if RayLib.IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
  begin
    if Hovering and (HitScrollBar(lx, ly) = []) then
    begin
      Focused := True;
      FMouseButtonDown := True;
      if (ly < inputTop) and (visRows > 1) then
      begin
        FSelActive := False;
        SetSelectionAnchor(lx, ly);
      end
      else if FInputOn then
      begin
        //drop any selection and arm a drag selection at the click point
        SetInputSelection(CharAtPixel(lx), CharAtPixel(lx));
        FSelActive := False;
      end
      else
      begin
        FSelActive := False;
      end;
      Invalidate;
    end;
  end;

  if FMouseButtonDown and RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
  begin
    if Hovering then
    begin
      if (ly < inputTop) and (visRows > 1) then
        ExtendSelectionTo(lx, ly)
      else if FInputOn and (ly >= inputTop) then
      begin
        //drag: grow the selection away from the point the drag started at,
        //which is the caret itself while nothing is selected yet
        if FInputSelStart >= 0 then
          SetInputSelection(FInputSelAnchor, CharAtPixel(lx))
        else if CharAtPixel(lx) <> FInputPos then
          SetInputSelection(FInputPos, CharAtPixel(lx))
        else
          PlaceInputCaretAt(lx);
        if FSelActive then
          FSelActive := False;
      end;
      Invalidate;
    end;
  end;

  if not RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    FMouseButtonDown := False;

  wheel := RayLib.GetMouseWheelMove;
  if (wheel <> 0) and Hovering then
  begin
    FScrollBack := FScrollBack + Trunc(wheel);
    ClampScroll;
    Invalidate;
  end;
end;

{ TTyroOutput }

constructor TTyroOutput.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := [csClip];
  FMaxLines := 500;
  FLines := TStringList.Create;
  FLock := TCriticalSection.Create;
  BackColor := clBlack.ReplaceAlpha(0); //transparent by default
  Color := clBlack; //contrasts with the light window backcolor
  SetBoundsRect(Rect(0, 0, 480, 240));
end;

destructor TTyroOutput.Destroy;
begin
  FreeAndNil(FLines);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TTyroOutput.GetLineCount: Integer;
begin
  FLock.Enter;
  try
    Result := FLines.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TTyroOutput.TrimLines;
begin
  while FLines.Count > FMaxLines do
    FLines.Delete(0);
end;

procedure TTyroOutput.Write(S: utf8string);
begin
  if S = '' then
    Exit;
  FLock.Enter;
  try
    if FLines.Count = 0 then
      FLines.Add('');
    FLines[FLines.Count - 1] := FLines[FLines.Count - 1] + S;
    TrimLines;
  finally
    FLock.Leave;
  end;
end;

procedure TTyroOutput.Writeln(S: utf8string);
begin
  FLock.Enter;
  try
    if S <> '' then
    begin
      if FLines.Count = 0 then
        FLines.Add('');
      FLines[FLines.Count - 1] := FLines[FLines.Count - 1] + S;
    end;
    FLines.Add(''); //open the next line
    TrimLines;
  finally
    FLock.Leave;
  end;
end;

procedure TTyroOutput.Clear;
begin
  FLock.Enter;
  try
    FLines.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TTyroOutput.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRect;
  ch: Integer;
  i, vis, start, y: Integer;
begin
  inherited;
  r := ClientRect;
  ch := Res.Font.Height;
  if ch <= 0 then
    ch := CDefaultCharHeight;
  if ch <= 0 then
    Exit;
  vis := r.Height div ch;
  if vis < 1 then
    vis := 1;
  FLock.Enter;
  try
    start := FLines.Count - vis;
    if start < 0 then
      start := 0;
    y := r.Top;
    for i := start to FLines.Count - 1 do
    begin
      ACanvas.DrawText(r.Left, y, FLines[i], Color);
      Inc(y, ch);
    end;
  finally
    FLock.Leave;
  end;
end;

end.
