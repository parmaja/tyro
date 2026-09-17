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
 *    - up/down arrows browse the command history
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
  Classes, SysUtils, SyncObjs,
  LazUTF8,
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
  EOnTerminalInput = procedure(ATerminal: TTyroTerminal; Input: string) of object;

  { EOnTerminalInputChange - Event handler for input buffer changes }
  EOnTerminalInputChange = procedure(ATerminal: TTyroTerminal; const InputData: string) of object;

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
    FTextColor: TColor;               // normal text (output lines, input line, caret)
    FHighlightColor: TColor;          // builtin command word + prompt
    FSelectionColor: TColor;          // selection background (selected text is inverted)
    FMaxLines: Integer;
    FScrollBack: Integer;             // 0 = stick to bottom, >0 = lines scrolled up

    FInputOn: Boolean;                // true when we accept a command line
    FPrompt: string;
    FInputBuffer: string;             // the current command line (UTF-8)
    FInputPos: Integer;               // caret position in codepoints
    FInputScroll: Integer;            // first visible codepoint of the input line
    FInputSelStart: Integer;          // -1 = no selection
    FInputSelEnd: Integer;
    FPasswordMode: Boolean;
    FPasswordChar: TUTF8Char;

    FHistory: TStringList;            // previously submitted command lines
    FHistoryPos: Integer;             // -1 = not browsing history

    FCommandNames: TStringList;       // builtin command names highlighted in the input

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
    function IsCommandName(const AWord: string): Boolean;
    procedure SplitFirstWord(const S: string; out ALeading, AWord, ARest: string);
    procedure BuildInputRuns(out ARuns: TTermRuns);
    function CharAtPixel(AX: Integer): Integer;
    procedure UpdateInputScroll;

    procedure AddLine(const ALine: string);
    procedure AppendText(const S: string);
    procedure TrimLines;
    procedure ClampScroll;
    procedure UpdateScrollBars;

    procedure ClearInputSelection;
    procedure SetInputSelection(AStart, AEnd: Integer);
    function InputSelectionText: string;
    procedure DeleteInputSelection;
    procedure SelectInputAll;

    procedure PlaceInputCaretAt(AX: Integer);
    procedure SetSelectionAnchor(AX, AY: Integer);
    procedure ExtendSelectionTo(AX, AY: Integer);
    function OutputSelectionText: string;
    procedure CopySelection;

    procedure HistoryUp;
    procedure HistoryDown;
    procedure SubmitInput;

    procedure DrawOutputLine(ACanvas: TTyroCanvas; ALine, AY: Integer);
    procedure DrawInputLine(ACanvas: TTyroCanvas; AY: Integer);
    procedure DrawCaret(ACanvas: TTyroCanvas; AY: Integer);

    procedure SetCaretVisible(AValue: Boolean);
  protected
    procedure UpdateSizes;
    procedure Resized; override;
    procedure Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;

    procedure Update; override; //caret blink + mouse interaction (called from the main loop)
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    procedure KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;

    procedure Clear;
    procedure Write(s: string);
    procedure Writeln(s: string);

    procedure StartRead(const Desc: string);
    procedure StopRead;
    procedure SaveToFile(AFileName: string);

    { All colors are control properties; code only reads these }
    property TextColor: TColor read FTextColor write FTextColor;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor;
    property CharWidth: Integer read FCharWidth write FCharWidth;
    property CharHeight: Integer read FCharHeight write FCharHeight;
    property MaxLines: Integer read FMaxLines write FMaxLines;

    property CaretVisible: Boolean read FCaretVisible write SetCaretVisible;
    property PasswordChar: TUTF8Char read FPasswordChar write FPasswordChar;

    property OnInput: EOnTerminalInput read FOnInput write FOnInput;
    property OnInputChange: EOnTerminalInputChange read FOnInputChange write FOnInputChange;
    property OnAny: EOnTerminalInputChange read FOnAny write FOnAny;

    property LineCount: Integer read GetLineCount;
    property CommandNames: TStringList read FCommandNames;
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
    FBackColor: TColor;
    FTextColor: TColor;
    procedure TrimLines;
    function GetLineCount: Integer;
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure Write(S: string);
    procedure Writeln(S: string);
    procedure Clear;
    property MaxLines: Integer read FMaxLines write FMaxLines default 500;
    property LineCount: Integer read GetLineCount;
    property BackColor: TColor read FBackColor write FBackColor;
    property TextColor: TColor read FTextColor write FTextColor;
  end;

implementation

{ Codepoint helpers for UTF-8 strings }

function CPCount(const S: string): Integer;
begin
  Result := UTF8Length(S);
end;

function CPSub(const S: string; AStart, ACount: Integer): string; //AStart is 0-based codepoint
begin
  if (ACount <= 0) or (AStart < 0) then
    Result := ''
  else
    Result := UTF8Copy(S, AStart + 1, ACount);
end;

function CPInsert(const S: string; ACol: Integer; const AIns: string): string;
begin
  if AIns = '' then
    Result := S
  else
    Result := CPSub(S, 0, ACol) + AIns + CPSub(S, ACol, CPCount(S) - ACol);
end;

function CPDelete(const S: string; ACol, ACount: Integer): string;
begin
  Result := CPSub(S, 0, ACol) + CPSub(S, ACol + ACount, CPCount(S) - ACol - ACount);
end;

{ TTyroTerminal }

constructor TTyroTerminal.Create(AParent: TTyroLayout);
var
  Builtin: array of string;
  i: Integer;
begin
  inherited;
  Style := [csClip, csOpaque, csVScroll];
  BackColor := clDarkGray;

  FCharWidth := CDefaultCharWidth;
  FCharHeight := CDefaultCharHeight;
  FMaxLines := CDefaultLineCount;
  FTextColor := clLightGray;
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
  FPasswordMode := False;
  FPasswordChar := '*';

  FHistory := TStringList.Create;
  FHistoryPos := -1;

  FCommandNames := TStringList.Create;
  Builtin := ['help', 'list', 'ls', 'dir', 'clear', 'cls', 'exit', 'quit', 'q',
    'stop', 'load', 'state', 'run', 'edit'];
  for i := 0 to High(Builtin) do
    FCommandNames.Add(Builtin[i]);

  FCaretTimer := 0;
  FCaretDim := 1;
  FCaretVisible := True;

  FSelActive := False;
  FMouseButtonDown := False;

  SetBoundsRect(Rect(0, 0, 200, 200));
end;

destructor TTyroTerminal.Destroy;
begin
  FreeAndNil(FCommandNames);
  FreeAndNil(FHistory);
  FreeAndNil(FLines);
  inherited Destroy;
end;

procedure TTyroTerminal.UpdateSizes;
begin
  if Resources.Font.Width > 0 then
    FCharWidth := Resources.Font.Width
  else
    FCharWidth := CDefaultCharWidth;
  if Resources.Font.Height > 0 then
    FCharHeight := Resources.Font.Height
  else
    FCharHeight := CDefaultCharHeight;
end;

procedure TTyroTerminal.Resized;
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

function TTyroTerminal.IsCommandName(const AWord: string): Boolean;
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

procedure TTyroTerminal.SplitFirstWord(const S: string; out ALeading, AWord, ARest: string);
var
  L, P: Integer;
  Ch: string;
begin
  ALeading := '';
  AWord := '';
  ARest := '';
  L := CPCount(S);
  P := 0;
  while P < L do
  begin
    Ch := CPSub(S, P, 1);
    if (Ch = ' ') or (Ch = #9) then
      Inc(P)
    else
      Break;
  end;
  ALeading := CPSub(S, 0, P);
  while P < L do
  begin
    Ch := CPSub(S, P, 1);
    if (Ch = ' ') or (Ch = #9) then
      Break;
    Inc(P);
  end;
  AWord := CPSub(S, CPCount(ALeading), P - CPCount(ALeading));
  ARest := CPSub(S, P, L - P);
end;

procedure TTyroTerminal.BuildInputRuns(out ARuns: TTermRuns);
var
  Leading, Word, Rest: string;
  L: Integer;
begin
  ARuns := nil;
  if FPasswordMode then
  begin
    L := CPCount(FInputBuffer);
    if L > 0 then
    begin
      SetLength(ARuns, 1);
      ARuns[0].Start := 0;
      ARuns[0].Count := L;
      ARuns[0].Color := FTextColor;
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
    ARuns[High(ARuns)].Count := CPCount(Leading);
    ARuns[High(ARuns)].Color := FTextColor;
  end;
  if Word <> '' then
  begin
    SetLength(ARuns, Length(ARuns) + 1);
    ARuns[High(ARuns)].Start := CPCount(Leading);
    ARuns[High(ARuns)].Count := CPCount(Word);
    if IsCommandName(Word) then
      ARuns[High(ARuns)].Color := FHighlightColor
    else
      ARuns[High(ARuns)].Color := FTextColor;
  end;
  if Rest <> '' then
  begin
    SetLength(ARuns, Length(ARuns) + 1);
    ARuns[High(ARuns)].Start := CPCount(Leading) + CPCount(Word);
    ARuns[High(ARuns)].Count := CPCount(Rest);
    ARuns[High(ARuns)].Color := FTextColor;
  end;
end;

function TTyroTerminal.GetPromptX: Integer;
begin
  Result := CPCount(FPrompt) * FCharWidth;
  if (FPrompt <> '') and (CPSub(FPrompt, CPCount(FPrompt) - 1, 1) <> ' ') then
    Result := Result + FCharWidth; //a trailing space after the prompt
end;

function TTyroTerminal.CharAtPixel(AX: Integer): Integer;
var
  L: Integer;
begin
  L := CPCount(FInputBuffer);
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

procedure TTyroTerminal.AddLine(const ALine: string);
begin
  FLines.Add(ALine);
end;

procedure TTyroTerminal.AppendText(const S: string);
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

procedure TTyroTerminal.Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
var
  vis, MaxScroll: Integer;
begin
  if Witch <> sbtVertical then
  begin
    inherited Scroll(Witch, ScrollCode, Pos);
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

procedure TTyroTerminal.Clear;
begin
  FLines.Clear;
  FScrollBack := 0;
  FInputBuffer := '';
  FInputPos := 0;
  FInputScroll := 0;
  ClearInputSelection;
  FSelActive := False;
  Invalidate;
end;

procedure TTyroTerminal.Write(s: string);
begin
  AppendText(s);
  ClampScroll;
  Invalidate;
end;

procedure TTyroTerminal.Writeln(s: string);
begin
  Write(s);
  Write(#10);
end;

procedure TTyroTerminal.StartRead(const Desc: string);
begin
  FPrompt := Desc;
  FInputBuffer := '';
  FInputPos := 0;
  FInputScroll := 0;
  ClearInputSelection;
  FInputOn := True;
  FHistoryPos := -1;
  Invalidate;
end;

procedure TTyroTerminal.StopRead;
begin
  FInputOn := False;
  Invalidate;
end;

procedure TTyroTerminal.SaveToFile(AFileName: string);
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
end;

procedure TTyroTerminal.SetInputSelection(AStart, AEnd: Integer);
begin
  if AStart < 0 then
    AStart := 0;
  if AEnd < AStart then
  begin
    FInputSelStart := AEnd;
    FInputSelEnd := AStart;
  end
  else
  begin
    FInputSelStart := AStart;
    FInputSelEnd := AEnd;
  end;
  if FInputSelStart = FInputSelEnd then
  begin
    FInputSelStart := -1;
    FInputSelEnd := -1;
  end;
end;

function TTyroTerminal.InputSelectionText: string;
begin
  Result := '';
  if FInputSelStart < 0 then
    Exit;
  if FPasswordMode then
    Result := StringOfChar(Char(FPasswordChar[1]), FInputSelEnd - FInputSelStart)
  else
    Result := CPSub(FInputBuffer, FInputSelStart, FInputSelEnd - FInputSelStart);
end;

procedure TTyroTerminal.DeleteInputSelection;
begin
  if FInputSelStart < 0 then
    Exit;
  FInputBuffer := CPDelete(FInputBuffer, FInputSelStart, FInputSelEnd - FInputSelStart);
  FInputPos := FInputSelStart;
  ClearInputSelection;
end;

procedure TTyroTerminal.SelectInputAll;
begin
  SetInputSelection(0, CPCount(FInputBuffer));
  FInputPos := CPCount(FInputBuffer);
  UpdateInputScroll;
end;

procedure TTyroTerminal.PlaceInputCaretAt(AX: Integer);
begin
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
    if Col > CPCount(FLines[Line]) then
      Col := CPCount(FLines[Line]);
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
    if Col > CPCount(FLines[Line]) then
      Col := CPCount(FLines[Line]);
  end;
  FSelCurLine := Line;
  FSelCurCol := Col;
end;

function TTyroTerminal.OutputSelectionText: string;
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
      Result := Result + CPSub(FLines[i], c1, CPCount(FLines[i]) - c1)
    else if i = l2 then
      Result := Result + CPSub(FLines[i], 0, c2)
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

{ history }

procedure TTyroTerminal.HistoryUp;
begin
  if FHistory.Count = 0 then
    Exit;
  if FHistoryPos < 0 then
    FHistoryPos := FHistory.Count - 1
  else if FHistoryPos > 0 then
    Dec(FHistoryPos);
  FInputBuffer := FHistory[FHistoryPos];
  FInputPos := CPCount(FInputBuffer);
  UpdateInputScroll;
  ClearInputSelection;
  Invalidate;
end;

procedure TTyroTerminal.HistoryDown;
begin
  if FHistoryPos < 0 then
    Exit;
  Inc(FHistoryPos);
  if FHistoryPos >= FHistory.Count then
  begin
    FHistoryPos := -1;
    FInputBuffer := '';
  end
  else
    FInputBuffer := FHistory[FHistoryPos];
  FInputPos := CPCount(FInputBuffer);
  UpdateInputScroll;
  ClearInputSelection;
  Invalidate;
end;

procedure TTyroTerminal.SubmitInput;
var
  s: string;
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
  ClearInputSelection;
  FInputOn := False;
  if Assigned(FOnInput) then
    FOnInput(Self, s);
  Invalidate;
end;

{ input }

procedure TTyroTerminal.KeyPress(var Key: TUTF8Char);
var
  S: string;
begin
  if not FInputOn then
    Exit;
  S := Key;
  if S = '' then
    Exit;
  if FPasswordMode then
  begin
    if FInputSelStart >= 0 then
      DeleteInputSelection;
    FInputBuffer := CPInsert(FInputBuffer, FInputPos, S);
    Inc(FInputPos);
    UpdateInputScroll;
  end
  else
  begin
    if FInputSelStart >= 0 then
      DeleteInputSelection;
    FInputBuffer := CPInsert(FInputBuffer, FInputPos, S);
    Inc(FInputPos);
    UpdateInputScroll;
  end;
  if Assigned(FOnInputChange) then
    FOnInputChange(Self, FInputBuffer);
  if Assigned(FOnAny) then
    FOnAny(Self, FInputBuffer);
  Invalidate;
  Key := '';
end;

procedure TTyroTerminal.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
var
  P: PUTF8Char;
  S: string;
begin
  if not FInputOn then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;

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
        if FInputSelStart >= 0 then
          DeleteInputSelection;
        FInputBuffer := CPInsert(FInputBuffer, FInputPos, '    ');
        Inc(FInputPos, 4);
        UpdateInputScroll;
        if Assigned(FOnInputChange) then
          FOnInputChange(Self, FInputBuffer);
        if Assigned(FOnAny) then
          FOnAny(Self, FInputBuffer);
        Invalidate;
        Key := KEY_NULL;
      end;
    end;
    KEY_BACKSPACE:
    begin
      if FInputSelStart >= 0 then
        DeleteInputSelection
      else if FInputPos > 0 then
      begin
        Dec(FInputPos);
        FInputBuffer := CPDelete(FInputBuffer, FInputPos, 1);
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
      if FInputSelStart >= 0 then
        DeleteInputSelection
      else if FInputPos < CPCount(FInputBuffer) then
      begin
        FInputBuffer := CPDelete(FInputBuffer, FInputPos, 1);
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
      if FInputPos > 0 then
      begin
        Dec(FInputPos);
        UpdateInputScroll;
      end;
      if ssShift in Shift then
        SetInputSelection(FInputPos, FInputPos + 1)
      else
        ClearInputSelection;
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_RIGHT:
    begin
      if FInputPos < CPCount(FInputBuffer) then
      begin
        Inc(FInputPos);
        UpdateInputScroll;
      end;
      if ssShift in Shift then
        SetInputSelection(FInputPos - 1, FInputPos)
      else
        ClearInputSelection;
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_HOME:
    begin
      FInputPos := 0;
      UpdateInputScroll;
      if not (ssShift in Shift) then
        ClearInputSelection;
      Invalidate;
      Key := KEY_NULL;
    end;
    KEY_END:
    begin
      FInputPos := CPCount(FInputBuffer);
      UpdateInputScroll;
      if not (ssShift in Shift) then
        ClearInputSelection;
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
        P := RayLib.GetClipboardText;
        S := '';
        if P <> nil then
          S := PUTF8Char(P);
        S := StringReplace(S, #13, '', [rfReplaceAll]);
        S := StringReplace(S, #10, '', [rfReplaceAll]);
        if S <> '' then
        begin
          if FInputSelStart >= 0 then
            DeleteInputSelection;
          FInputBuffer := CPInsert(FInputBuffer, FInputPos, S);
          Inc(FInputPos, CPCount(S));
          UpdateInputScroll;
          if Assigned(FOnInputChange) then
            FOnInputChange(Self, FInputBuffer);
          if Assigned(FOnAny) then
            FOnAny(Self, FInputBuffer);
          Invalidate;
        end;
        Key := KEY_NULL;
      end;
    end;
  else
    ;
  end;
  inherited KeyDown(Key, Shift);
end;

{ paint }

procedure TTyroTerminal.DrawOutputLine(ACanvas: TTyroCanvas; ALine, AY: Integer);
var
  S: string;
  c: TColor;
  l1, c1, l2, c2, selStart, selEnd, L: Integer;
  Prefix, SelText, Suffix: string;
begin
  if (ALine < 0) or (ALine >= FLines.Count) then
    Exit;
  S := FLines[ALine];
  c := FTextColor;

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
        selEnd := CPCount(S);
      if selEnd < selStart then
        selEnd := selStart;
    end;
  end;

  if S = '' then
    Exit;

  L := CPCount(S);
  if (selStart >= 0) and (selEnd > selStart) then
  begin
    Prefix := CPSub(S, 0, selStart);
    SelText := CPSub(S, selStart, selEnd - selStart);
    Suffix := CPSub(S, selEnd, L - selEnd);
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
  disp: string;
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
    if CPSub(disp, CPCount(disp) - 1, 1) <> ' ' then
      disp := disp + ' ';
    ACanvas.DrawText(0, AY, disp, FHighlightColor);
    promptX := CPCount(disp) * FCharWidth;
  end;

  if FInputBuffer = '' then
    Exit;
  L := CPCount(FInputBuffer);
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
      ACanvas.DrawText(x, AY, CPSub(FInputBuffer, dStart, cnt), Runs[i].Color);
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
        ACanvas.DrawText(x, AY, CPSub(FInputBuffer, Sc, sE - Sc), BackColor);
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
  col := FTextColor.SetAlpha(Round(FTextColor.RGBA.Alpha * FCaretDim));
  ACanvas.FillRect(x, AY, x + 2, AY + FCharHeight, col);
end;

procedure TTyroTerminal.DoPaint(ACanvas: TTyroCanvas);
var
  vis, outRows, i, y, bottom, top, Line: Integer;
begin
  inherited;
  UpdateSizes;
  ACanvas.DrawRectangle(ClientRect, BackColor, True);

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
    Exit;
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
        PlaceInputCaretAt(lx);
        ClearInputSelection;
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
  FBackColor := clBlack.ReplaceAlpha(0); //transparent by default
  FTextColor := clBlack; //contrasts with the light window backcolor
  FLines := TStringList.Create;
  FLock := TCriticalSection.Create;
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

procedure TTyroOutput.Write(S: string);
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

procedure TTyroOutput.Writeln(S: string);
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
  if (r.Width <= 0) or (r.Height <= 0) then
    Exit;
  if FBackColor.RGBA.Alpha > 0 then
    ACanvas.DrawRectangle(r.Left, r.Top, r.Width, r.Height, FBackColor, True);
  ch := Resources.Font.Height;
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
      ACanvas.DrawText(r.Left, y, FLines[i], FTextColor);
      Inc(y, ch);
    end;
  finally
    FLock.Leave;
  end;
end;

end.
