unit TyroEditors;

{**
 *  This file is part of the "Tyro"
 *
 *  @license   MIT
 *
 *  @author    Zaher Dirkey
 *
 *  In-project script editor with simple Lua syntax highlighting.
 *  Opened with F2 or the "edit" console command.
 *}

{$MODE DELPHI}{$H+}{$M+}

interface

uses
  Classes, SysUtils, Types, StrUtils,
  LCLType, LazUTF8,
  RayLib, RayClasses,
  TyroClasses, TyroControls;

const
  cEditorTabWidth = 4;
  cEditorMaxUndo = 100;
  cEditorCaretBlink = 0.5;
  cEditorRepeatDelay = 0.5;      //seconds before a held key starts repeating
  cEditorRepeatInterval = 0.03;  //seconds between repeats

  cEditorCaretLineColor = $FF141414;  //subtle highlight for the caret line

type
  TEditorCloseEvent = procedure(Sender: TObject) of object;
  TEditorSaveEvent = procedure(Sender: TObject) of object;

  { one run of same-colored characters in a line (columns are UTF-8 codepoints) }
  TEditorRun = record
    Start: Integer;
    Count: Integer;
    Color: TColor;
  end;

  TEditorRuns = array of TEditorRun;

  TKState = (stNone, stComment, stString);

  TyroEditor = class(TTyroControl)
  private
    FLines: TStringList;
    FRuns: array of TEditorRuns;
    FCharWidth: Integer;
    FCharHeight: Integer;
    FTabWidth: Integer;
    FCaretLine: Integer;
    FCaretCol: Integer;
    FDesiredCol: Integer;
    FTopLine: Integer;
    FLeftCol: Integer;
    FModified: Boolean;
    FCaretTimer: Double;
    FCaretDim: Double;
    FCaretVisible: Boolean;
    FAnchorLine: Integer;
    FAnchorCol: Integer;
    FSelecting: Boolean;
    FMouseDown: Boolean;
    FUndo: TStringList;
    FRedo: TStringList;
    FFileName: string;
    FOnClose: TEditorCloseEvent;
    FOnSave: TEditorSaveEvent;
    FRepeatKey: TKeyboardKey;
    FRepeatTimer: Double;
    function GetLineCount: Integer;
    function GetLineLength(ALine: Integer): Integer;
    function GetGutterWidth: Integer;
    function GetVisibleLines: Integer;
    function GetStatusHeight: Integer;

    procedure SetCaret(ALine, ACol: Integer);
    function IsSelecting: Boolean;
    procedure GetSelection(out ALine1, ACol1, ALine2, ACol2: Integer);
    function SelectedText: string;
    procedure ClearSelection;
    procedure SelectAll;
    function ColToPixel(ALine, ACol: Integer): Integer;
    procedure PlaceCaretAt(aX, aY: Integer);
    procedure ScrollCaretVisible;

    procedure PushUndo;
    procedure UndoMove;
    procedure RedoMove;
    procedure ModifyDone;

    procedure BeginEdit;
    procedure InsertSingleChar(const AChar: string);
    procedure InsertText(const S: string);
    procedure DeleteCharAt(ALine, ACol: Integer);
    procedure DeleteAtCaret;
    procedure BackspaceAtCaret;
    procedure DeleteSelected;

    procedure MoveLeft(Extend: Boolean);
    procedure MoveRight(Extend: Boolean);
    procedure MoveWord(Forward: Boolean; Extend: Boolean);
    procedure MoveUp(Extend: Boolean);
    procedure MoveDown(Extend: Boolean);
    procedure MoveHome(Extend: Boolean);
    procedure MoveEnd(Extend: Boolean);
    procedure MovePage(Forward: Boolean; Extend: Boolean);

    procedure CopySelection;
    procedure CutSelection;
    procedure PasteText;

    procedure RebuildRuns;
    function IsKeyword(const AWord: string): Boolean;
    function IsApiName(const AWord: string): Boolean;
    procedure UpdateSizes;
    procedure UpdateScrollBars;
    function GetMaxCol: Integer;
    procedure ProcessKey(var Key: TKeyboardKey; Shift: TShiftState);
    procedure UpdateKeyRepeat;

    procedure DrawLine(ACanvas: TTyroCanvas; ALine: Integer; aY: Integer; aGutterWidth: Integer);
    procedure DrawStatus(ACanvas: TTyroCanvas);
    procedure DrawCaret(ACanvas: TTyroCanvas);
  protected
    procedure Resized; override;
    procedure Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;

    procedure Update; //caret blink and mouse (called from the main loop)
    procedure DoPaintBackground(ACanvas: TTyroCanvas); override;
    procedure DoPaint(ACanvas: TTyroCanvas); override;
    procedure KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;

    procedure LoadSource(ASource: TStringList);
    procedure SaveSource(ASource: TStringList);
    procedure Close;

    property FileName: string read FFileName write FFileName;
    property Modified: Boolean read FModified;
    property LineCount: Integer read GetLineCount;
    property OnClose: TEditorCloseEvent read FOnClose write FOnClose;
    property OnSave: TEditorSaveEvent read FOnSave write FOnSave;
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

function CPSizeAt(const S: string; ACol: Integer): Integer; //bytes of the char at codepoint ACol
var
  P, I, L: Integer;
begin
  Result := 0;
  L := Length(S);
  if (ACol < 0) or (S = '') then
    Exit;
  P := 1;
  I := 0;
  while (I < ACol) and (P <= L) do
  begin
    Inc(P, UTF8CodepointSize(@S[P]));
    Inc(I);
  end;
  if P <= L then
    Result := UTF8CodepointSize(@S[P]);
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

function ExpandPrefix(const S: string; AMaxCol: Integer): string; //expand tabs for the first AMaxCol codepoints
var
  P, I, L, Col: Integer;
  Ch: string;
begin
  Result := '';
  L := Length(S);
  P := 1;
  I := 0;
  Col := 0;
  while (P <= L) and (I < AMaxCol) do
  begin
    Ch := UTF8Copy(S, I + 1, 1);
    if Ch = #9 then
    begin
      repeat
        Result := Result + ' ';
        Inc(Col);
      until (Col mod cEditorTabWidth) = 0;
    end
    else
    begin
      Result := Result + Ch;
      Inc(Col);
    end;
    Inc(P, UTF8CodepointSize(@S[P]));
    Inc(I);
  end;
end;

{ TyroEditor }

constructor TyroEditor.Create(AParent: TTyroLayout);
begin
  inherited;
  Style := [csClip, csOpaque, csHScroll, csVScroll];
  FLines := TStringList.Create;
  FLines.Add('');
  FUndo := TStringList.Create;
  FRedo := TStringList.Create;
  FTabWidth := cEditorTabWidth;
  FCharWidth := 8;
  FCharHeight := 8;
  FCaretLine := 0;
  FCaretCol := 0;
  FDesiredCol := 0;
  FTopLine := 0;
  FLeftCol := 0;
  FModified := False;
  FCaretTimer := 0;
  FCaretDim := 1;
  FCaretVisible := True;
  FAnchorLine := 0;
  FAnchorCol := 0;
  FSelecting := False;
  FMouseDown := False;
  FRepeatKey := KEY_NULL;
  FRepeatTimer := 0;
  FFileName := '';
  BackColor := clDarkGray;
  SetBoundsRect(Rect(0, 0, 400, 300));
  RebuildRuns;
end;

destructor TyroEditor.Destroy;
begin
  FreeAndNil(FUndo);
  FreeAndNil(FRedo);
  FreeAndNil(FLines);
  inherited Destroy;
end;

function TyroEditor.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TyroEditor.GetLineLength(ALine: Integer): Integer;
begin
  if (ALine >= 0) and (ALine < FLines.Count) then
    Result := CPCount(FLines[ALine])
  else
    Result := 0;
end;

function TyroEditor.GetGutterWidth: Integer;
begin
  Result := (Length(IntToStr(FLines.Count)) + 2) * FCharWidth;
end;

function TyroEditor.GetStatusHeight: Integer;
begin
  Result := FCharHeight;
end;

function TyroEditor.GetVisibleLines: Integer;
var
  h: Integer;
begin
  h := ClientRect.Height - GetStatusHeight - FCharHeight;
  if h < FCharHeight then
    h := FCharHeight;
  Result := h div FCharHeight;
  if Result < 1 then
    Result := 1;
end;

procedure TyroEditor.UpdateSizes;
begin
  if Resources.Font.Width > 0 then
    FCharWidth := Resources.Font.Width
  else
    FCharWidth := 8;
  if Resources.Font.Height > 0 then
    FCharHeight := Resources.Font.Height
  else
    FCharHeight := 8;
end;

function TyroEditor.GetMaxCol: Integer;
var
  I, L: Integer;
begin
  Result := 0;
  for I := 0 to FLines.Count - 1 do
  begin
    L := CPCount(FLines[I]);
    if L > Result then
      Result := L;
  end;
end;

procedure TyroEditor.UpdateScrollBars;
var
  vis, cols, maxV, maxH: Integer;
begin
  UpdateSizes;
  vis := GetVisibleLines;
  cols := (ClientRect.Width - GetGutterWidth) div FCharWidth;
  if cols < 1 then
    cols := 1;
  maxV := FLines.Count - vis;
  if maxV < 0 then
    maxV := 0;
  maxH := GetMaxCol - cols;
  if maxH < 0 then
    maxH := 0;

  if maxV > 0 then
  begin
    ShowScrollBar([sbtVertical], True);
    SetScrollRange(sbtVertical, 0, maxV, vis);
    SetScrollPosition(sbtVertical, FTopLine, True);
  end
  else
    ShowScrollBar([sbtVertical], False);

  if maxH > 0 then
  begin
    ShowScrollBar([sbtHorizontal], True);
    SetScrollRange(sbtHorizontal, 0, maxH, cols);
    SetScrollPosition(sbtHorizontal, FLeftCol, True);
  end
  else
    ShowScrollBar([sbtHorizontal], False);
end;

procedure TyroEditor.Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
var
  vis, cols, maxV, maxH: Integer;
  v: Integer;
begin
  UpdateSizes;
  vis := GetVisibleLines;
  cols := (ClientRect.Width - GetGutterWidth) div FCharWidth;
  if cols < 1 then
    cols := 1;
  maxV := FLines.Count - vis;
  if maxV < 0 then
    maxV := 0;
  maxH := GetMaxCol - cols;
  if maxH < 0 then
    maxH := 0;

  if Witch = sbtVertical then
  begin
    v := FTopLine;
    case ScrollCode of
      scrollTOP: v := 0;
      scrollBOTTOM: v := maxV;
      scrollLINEDOWN: Inc(v);
      scrollLINEUP: Dec(v);
      scrollPAGEDOWN: Inc(v, vis);
      scrollPAGEUP: Dec(v, vis);
      scrollTHUMBPOSITION, scrollTHUMBTRACK: v := Pos;
      scrollENDSCROLL: ;
    end;
    if v < 0 then
      v := 0;
    if v > maxV then
      v := maxV;
    FTopLine := v;
  end
  else
  begin
    v := FLeftCol;
    case ScrollCode of
      scrollTOP: v := 0;
      scrollBOTTOM: v := maxH;
      scrollLINEDOWN: Inc(v);
      scrollLINEUP: Dec(v);
      scrollPAGEDOWN: Inc(v, cols);
      scrollPAGEUP: Dec(v, cols);
      scrollTHUMBPOSITION, scrollTHUMBTRACK: v := Pos;
      scrollENDSCROLL: ;
    end;
    if v < 0 then
      v := 0;
    if v > maxH then
      v := maxH;
    FLeftCol := v;
  end;
  UpdateScrollBars;
  Invalidate;
end;

procedure TyroEditor.Resized;
begin
  inherited;
  ScrollCaretVisible;
  UpdateScrollBars;
  Invalidate;
end;

procedure TyroEditor.SetCaret(ALine, ACol: Integer);
begin
  FCaretLine := ALine;
  FCaretCol := ACol;
  FDesiredCol := ACol;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.ClearSelection;
begin
  FSelecting := False;
end;

function TyroEditor.IsSelecting: Boolean;
begin
  Result := FSelecting and not ((FAnchorLine = FCaretLine) and (FAnchorCol = FCaretCol));
end;

procedure TyroEditor.GetSelection(out ALine1, ACol1, ALine2, ACol2: Integer);
var
  Worse: Boolean;
begin
  Worse := (FAnchorLine > FCaretLine) or ((FAnchorLine = FCaretLine) and (FAnchorCol > FCaretCol));
  if not Worse then
  begin
    ALine1 := FAnchorLine; ACol1 := FAnchorCol;
    ALine2 := FCaretLine; ACol2 := FCaretCol;
  end
  else
  begin
    ALine1 := FCaretLine; ACol1 := FCaretCol;
    ALine2 := FAnchorLine; ACol2 := FAnchorCol;
  end;
end;

function TyroEditor.SelectedText: string;
var
  l1, c1, l2, c2, I: Integer;
begin
  Result := '';
  if not IsSelecting then
    Exit;
  GetSelection(l1, c1, l2, c2);
  for I := l1 to l2 do
  begin
    if I > l1 then
      Result := Result + #13#10;
    if I = l1 then
      Result := Result + CPSub(FLines[I], c1, CPCount(FLines[I]) - c1)
    else if I = l2 then
      Result := Result + CPSub(FLines[I], 0, c2)
    else
      Result := Result + FLines[I];
  end;
end;

procedure TyroEditor.SelectAll;
begin
  FAnchorLine := 0;
  FAnchorCol := 0;
  FCaretLine := FLines.Count - 1;
  FCaretCol := CPCount(FLines[FLines.Count - 1]);
  FSelecting := True;
  FDesiredCol := FCaretCol;
  Invalidate;
end;

function TyroEditor.ColToPixel(ALine, ACol: Integer): Integer;
var
  P, L, Col: Integer;
  Ch: string;
begin
  Result := 0;
  if (ALine < 0) or (ALine >= FLines.Count) then
    Exit;
  L := Length(FLines[ALine]);
  P := 1;
  Col := 0;
  while (P <= L) and (Col < ACol) do
  begin
    Ch := UTF8Copy(FLines[ALine], Col + 1, 1);
    if Ch = #9 then
      Result := ((Result div FCharWidth) div FTabWidth + 1) * FTabWidth * FCharWidth
    else
      Inc(Result, FCharWidth);
    Inc(P, UTF8CodepointSize(@FLines[ALine][P]));
    Inc(Col);
  end;
end;

procedure TyroEditor.PlaceCaretAt(aX, aY: Integer);
var
  Line, Col, gx: Integer;
begin
  Line := FTopLine + (aY div FCharHeight);
  if Line < 0 then
    Line := 0;
  if Line >= FLines.Count then
    Line := FLines.Count - 1;
  gx := aX - GetGutterWidth;
  if gx < 0 then
    gx := 0;
  Col := FLeftCol + (gx div FCharWidth);
  if Col > CPCount(FLines[Line]) then
    Col := CPCount(FLines[Line]);
  FCaretLine := Line;
  FCaretCol := Col;
  FDesiredCol := Col;
  Invalidate;
end;

procedure TyroEditor.ScrollCaretVisible;
var
  visible: Integer;
  cols: Integer;
begin
  if FCharHeight <= 0 then
    Exit;
  visible := GetVisibleLines;
  if FCaretLine < FTopLine then
    FTopLine := FCaretLine;
  if FCaretLine >= FTopLine + visible then
    FTopLine := FCaretLine - visible + 1;
  if FTopLine < 0 then
    FTopLine := 0;
  cols := (ClientRect.Width - GetGutterWidth) div FCharWidth;
  if cols < 1 then
    cols := 1;
  if FCaretCol < FLeftCol then
    FLeftCol := FCaretCol;
  if FCaretCol >= FLeftCol + cols then
    FLeftCol := FCaretCol - cols + 1;
  if FLeftCol < 0 then
    FLeftCol := 0;
end;

procedure TyroEditor.PushUndo;
begin
  if FUndo.Count >= cEditorMaxUndo then
    FUndo.Delete(0);
  FUndo.Add(FLines.Text);
  FRedo.Clear;
end;

procedure TyroEditor.BeginEdit;
begin
  PushUndo;
end;

procedure TyroEditor.UndoMove;
var
  S: string;
begin
  if FUndo.Count = 0 then
    Exit;
  FRedo.Add(FLines.Text);
  S := FUndo[FUndo.Count - 1];
  FUndo.Delete(FUndo.Count - 1);
  FLines.Text := S;
  if FLines.Count = 0 then
    FLines.Add('');
  if FCaretLine >= FLines.Count then
    FCaretLine := FLines.Count - 1;
  if FCaretCol > GetLineLength(FCaretLine) then
    FCaretCol := GetLineLength(FCaretLine);
  FDesiredCol := FCaretCol;
  FModified := True;
  RebuildRuns;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.RedoMove;
var
  S: string;
begin
  if FRedo.Count = 0 then
    Exit;
  FUndo.Add(FLines.Text);
  S := FRedo[FRedo.Count - 1];
  FRedo.Delete(FRedo.Count - 1);
  FLines.Text := S;
  if FLines.Count = 0 then
    FLines.Add('');
  if FCaretLine >= FLines.Count then
    FCaretLine := FLines.Count - 1;
  if FCaretCol > GetLineLength(FCaretLine) then
    FCaretCol := GetLineLength(FCaretLine);
  FDesiredCol := FCaretCol;
  FModified := True;
  RebuildRuns;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.ModifyDone;
begin
  FModified := True;
  RebuildRuns;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.InsertSingleChar(const AChar: string);
begin
  FLines[FCaretLine] := CPInsert(FLines[FCaretLine], FCaretCol, AChar);
  Inc(FCaretCol, CPCount(AChar));
  FDesiredCol := FCaretCol;
end;

procedure TyroEditor.InsertText(const S: string);
var
  T, Parts: string;
  Segments: array of string;
  I, L, Col: Integer;
begin
  if S = '' then
    Exit;
  T := StringReplace(S, #13#10, #10, [rfReplaceAll]);
  T := StringReplace(T, #13, #10, [rfReplaceAll]);
  //split preserving tractions
  Parts := T;
  SetLength(Segments, 0);
  while Parts <> '' do
  begin
    I := Pos(#10, Parts);
    if I > 0 then
    begin
      SetLength(Segments, Length(Segments) + 1);
      Segments[High(Segments)] := Copy(Parts, 1, I - 1);
      Delete(Parts, 1, I);
    end
    else
    begin
      SetLength(Segments, Length(Segments) + 1);
      Segments[High(Segments)] := Parts;
      Parts := '';
    end;
  end;
  if Length(Segments) = 0 then
    Exit;
  L := FCaretLine;
  Col := FCaretCol;
  for I := 0 to High(Segments) do
  begin
    if I = 0 then
      FLines[L] := CPInsert(FLines[L], Col, Segments[I])
    else
    begin
      Inc(L);
      FLines.Insert(L, Segments[I]);
    end;
    Col := CPCount(Segments[I]);
  end;
  FCaretLine := L;
  FCaretCol := CPCount(Segments[High(Segments)]);
  FDesiredCol := FCaretCol;
end;

procedure TyroEditor.DeleteCharAt(ALine, ACol: Integer);
begin
  FLines[ALine] := CPDelete(FLines[ALine], ACol, 1);
end;

procedure TyroEditor.DeleteSelected;
var
  l1, c1, l2, c2: Integer;
begin
  if not IsSelecting then
    Exit;
  GetSelection(l1, c1, l2, c2);
  if l1 = l2 then
    FLines[l1] := CPDelete(FLines[l1], c1, c2 - c1)
  else
  begin
    FLines[l1] := CPSub(FLines[l1], 0, c1) + CPSub(FLines[l2], c2, CPCount(FLines[l2]) - c2);
    while l2 > l1 do
    begin
      FLines.Delete(l2);
      Dec(l2);
    end;
  end;
  FCaretLine := l1;
  FCaretCol := c1;
  FDesiredCol := c1;
  ClearSelection;
end;

procedure TyroEditor.DeleteAtCaret;
begin
  if IsSelecting then
  begin
    DeleteSelected;
    Exit;
  end;
  if FCaretCol < GetLineLength(FCaretLine) then
    DeleteCharAt(FCaretLine, FCaretCol)
  else if FCaretLine < FLines.Count - 1 then
  begin
    FLines[FCaretLine] := FLines[FCaretLine] + FLines[FCaretLine + 1];
    FLines.Delete(FCaretLine + 1);
  end;
end;

procedure TyroEditor.BackspaceAtCaret;
begin
  if IsSelecting then
  begin
    DeleteSelected;
    Exit;
  end;
  if FCaretCol > 0 then
  begin
    Dec(FCaretCol);
    DeleteCharAt(FCaretLine, FCaretCol);
    FDesiredCol := FCaretCol;
  end
  else if FCaretLine > 0 then
  begin
    FLines[FCaretLine - 1] := FLines[FCaretLine - 1] + FLines[FCaretLine];
    FLines.Delete(FCaretLine);
    Dec(FCaretLine);
    FCaretCol := CPCount(FLines[FCaretLine]);
    FDesiredCol := FCaretCol;
  end;
end;

procedure TyroEditor.MoveLeft(Extend: Boolean);
begin
  if not Extend then
    ClearSelection;
  if FCaretCol > 0 then
    Dec(FCaretCol)
  else if FCaretLine > 0 then
  begin
    Dec(FCaretLine);
    FCaretCol := CPCount(FLines[FCaretLine]);
  end;
  FDesiredCol := FCaretCol;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.MoveRight(Extend: Boolean);
begin
  if not Extend then
    ClearSelection;
  if FCaretCol < GetLineLength(FCaretLine) then
    Inc(FCaretCol)
  else if FCaretLine < FLines.Count - 1 then
  begin
    Inc(FCaretLine);
    FCaretCol := 0;
  end;
  FDesiredCol := FCaretCol;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.MoveWord(Forward: Boolean; Extend: Boolean);
var
  L, Col: Integer;
  IsW: Boolean;
  function CharIsWord(AL, AC: Integer): Boolean;
  var
    b: Byte;
    S: string;
  begin
    if (AC < 0) or (AC >= CPCount(FLines[AL])) then
      Result := False
    else
    begin
      S := CPSub(FLines[AL], AC, 1);
      if S = '' then
        Result := False
      else
      begin
        b := Byte(S[1]);
        Result := ((b >= 65) and (b <= 90)) or ((b >= 97) and (b <= 122)) or (b = 95) or ((b >= 48) and (b <= 57));
      end;
    end;
  end;
begin
  if not Extend then
    ClearSelection;
  L := FCaretLine;
  Col := FCaretCol;
  if Forward then
  begin
    while True do
    begin
      if Col < GetLineLength(L) then
        Inc(Col)
      else if L < FLines.Count - 1 then
      begin
        Inc(L);
        Col := 0;
      end
      else
        Break;
      IsW := CharIsWord(L, Col - 1);
      if not IsW then
      begin
        if (Col < GetLineLength(L)) and CharIsWord(L, Col) then
          Break
        else if L < FLines.Count - 1 then
        begin
          Inc(L);
          Col := 0;
        end
        else
          Break;
      end;
    end;
  end
  else
  begin
    while True do
    begin
      if Col > 0 then
        Dec(Col)
      else if L > 0 then
      begin
        Dec(L);
        Col := GetLineLength(L);
      end
      else
        Break;
      if Col = 0 then
        Break;
      if not CharIsWord(L, Col - 1) then
      begin
        if (Col > 0) and CharIsWord(L, Col) then
          Break;
      end;
    end;
  end;
  FCaretLine := L;
  FCaretCol := Col;
  FDesiredCol := Col;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.MoveUp(Extend: Boolean);
var
  L, Col: Integer;
begin
  if not Extend then
    ClearSelection;
  L := FCaretLine;
  if L > 0 then
    Dec(L);
  Col := FDesiredCol;
  if Col > GetLineLength(L) then
    Col := GetLineLength(L);
  FCaretLine := L;
  FCaretCol := Col;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.MoveDown(Extend: Boolean);
var
  L, Col: Integer;
begin
  if not Extend then
    ClearSelection;
  L := FCaretLine;
  if L < FLines.Count - 1 then
    Inc(L);
  Col := FDesiredCol;
  if Col > GetLineLength(L) then
    Col := GetLineLength(L);
  FCaretLine := L;
  FCaretCol := Col;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.MoveHome(Extend: Boolean);
var
  I: Integer;
  W: Integer;
begin
  if not Extend then
    ClearSelection;
  I := 0;
  W := GetLineLength(FCaretLine);
  if (FCaretCol > 0) and (W > 0) then
  begin
    while (I < W) and (CPSub(FLines[FCaretLine], I, 1) = ' ') do
      Inc(I);
    if I >= W then
      I := 0
    else if FCaretCol = I then
      I := 0;
  end;
  FCaretCol := I;
  FDesiredCol := I;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.MoveEnd(Extend: Boolean);
begin
  if not Extend then
    ClearSelection;
  FCaretCol := GetLineLength(FCaretLine);
  FDesiredCol := FCaretCol;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.MovePage(Forward: Boolean; Extend: Boolean);
var
  L, Step: Integer;
begin
  if not Extend then
    ClearSelection;
  Step := GetVisibleLines - 1;
  if Step < 1 then
    Step := 1;
  L := FCaretLine;
  if Forward then
    L := L + Step
  else
    L := L - Step;
  if L < 0 then
    L := 0;
  if L >= FLines.Count then
    L := FLines.Count - 1;
  FCaretLine := L;
  if FCaretCol > GetLineLength(L) then
    FCaretCol := GetLineLength(L);
  FDesiredCol := FCaretCol;
  ScrollCaretVisible;
  Invalidate;
end;

procedure TyroEditor.CopySelection;
begin
  if IsSelecting then
    RayLib.SetClipboardText(PUTF8Char(SelectedText));
end;

procedure TyroEditor.CutSelection;
begin
  if IsSelecting then
  begin
    CopySelection;
    BeginEdit;
    DeleteSelected;
    ModifyDone;
  end;
end;

procedure TyroEditor.PasteText;
var
  P: PUTF8Char;
  S: string;
begin
  P := RayLib.GetClipboardText;
  S := '';
  if P <> nil then
    S := PUTF8Char(P);
  if S = '' then
    Exit;
  BeginEdit;
  if IsSelecting then
    DeleteSelected;
  InsertText(S);
  ModifyDone;
end;

function TyroEditor.IsKeyword(const AWord: string): Boolean;
const
  Words: array[0..21] of string = ('and', 'break', 'do', 'else', 'elseif', 'end',
    'false', 'for', 'function', 'goto', 'if', 'in', 'local', 'nil', 'not', 'or',
    'repeat', 'return', 'then', 'true', 'until', 'while');
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(Words) do
    if Words[I] = AWord then
    begin
      Result := True;
      Exit;
    end;
end;

function TyroEditor.IsApiName(const AWord: string): Boolean;
const
  Words: array[0..38] of string = ('version', 'log', 'sleep', 'print', 'println',
    'iskeypressed', 'iskeydown', 'mousex', 'mousey', 'ismousepressed', 'frametime',
    'time', 'rand', 'window', 'console', 'canvas', 'shader', 'font', 'music',
    'sprites', 'buttons', 'new', 'find', 'caption', 'border', 'clear', 'text',
    'circle', 'rectangle', 'line', 'point', 'load', 'show', 'read', 'play',
    'beep', 'sound', 'mml', 'hide');
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(Words) do
    if SameText(Words[I], AWord) then
    begin
      Result := True;
      Exit;
    end;
end;

{ Rebuild the syntax run cache for every line }

procedure TyroEditor.RebuildRuns;
var
  I: Integer;
  L: string;
  Runs: TEditorRuns;
  P, Ln: Integer;
  CurCol: Integer;
  RunStart: Integer;
  Block: TKState;
  BlockEq: Integer;
  N, K, Q: Integer;
  OpenerLen: Integer;
  Closer: string;

  procedure EmitRun(AColor: TColor);
  var
    Idx: Integer;
  begin
    if RunStart >= CurCol then
      Exit;
    Idx := High(Runs);
    if (Idx >= 0) and (Runs[Idx].Start + Runs[Idx].Count = RunStart) and (Runs[Idx].Color.Value = AColor.Value) then
      Runs[Idx].Count := Runs[Idx].Count + (CurCol - RunStart)
    else
    begin
      SetLength(Runs, Length(Runs) + 1);
      Runs[High(Runs)].Start := RunStart;
      Runs[High(Runs)].Count := CurCol - RunStart;
      Runs[High(Runs)].Color := AColor;
    end;
    RunStart := CurCol;
  end;

  procedure Advance(ABytes: Integer);
  begin
    CurCol := CurCol + UTF8Length(Copy(L, P, ABytes));
    Inc(P, ABytes);
  end;

  function LongOpenLen(APos: Integer): Integer;
  var
    C: Integer;
  begin
    Result := 0;
    if (APos > Ln) or (L[APos] <> '[') then
      Exit;
    C := APos + 1;
    while (C <= Ln) and (L[C] = '=') do
      Inc(C);
    if (C <= Ln) and (L[C] = '[') then
      Result := C + 1 - APos;
  end;

begin
  Block := stNone;
  BlockEq := 0;
  SetLength(FRuns, FLines.Count);
  for I := 0 to FLines.Count - 1 do
  begin
    L := FLines[I];
    Ln := Length(L);
    if Ln = 0 then
    begin
      FRuns[I] := nil;
      Continue;
    end;
    Runs := nil;
    P := 1;
    CurCol := 0;
    RunStart := 0;
    while P <= Ln do
    begin
      if Block <> stNone then
      begin
        if Block = stComment then
          Closer := ']]'
        else
          Closer := ']' + StringOfChar('=', BlockEq) + ']';
        Q := PosEx(Closer, L, P);
        if Q > 0 then
        begin
          Advance(Q + Length(Closer) - P);
          if Block = stComment then
            EmitRun(clGray)
          else
            EmitRun(clGreen);
          Block := stNone;
        end
        else
        begin
          Advance(Ln - P + 1);
          if Block = stComment then
            EmitRun(clGray)
          else
            EmitRun(clGreen);
        end;
        Continue;
      end;

      N := UTF8CodepointSize(@L[P]);
      case L[P] of
        '-':
          if (P + 1 <= Ln) and (L[P + 1] = '-') then
          begin
            OpenerLen := 0;
            if (P + 2 <= Ln) and (L[P + 2] = '[') then
              OpenerLen := LongOpenLen(P + 2);
            if OpenerLen > 0 then
            begin
              Advance(2 + OpenerLen); //"--" + opener
              Block := stComment;
              BlockEq := OpenerLen - 2;
              Q := PosEx(']]', L, P);
              if Q > 0 then
              begin
                Advance(Q + 2 - P);
                EmitRun(clGray);
                Block := stNone;
              end
              else
              begin
                Advance(Ln - P + 1);
                EmitRun(clGray);
              end;
            end
            else
            begin
              //plain line comment "--" to end of the line
              Advance(2);
              Advance(Ln - P + 1);
              EmitRun(clGray);
            end;
          end
          else
          begin
            Advance(N);
            EmitRun(clLightgray);
          end;
        '[':
          begin
            OpenerLen := LongOpenLen(P);
            if OpenerLen > 0 then
            begin
              Advance(OpenerLen);
              Block := stString;
              BlockEq := OpenerLen - 2;
              Closer := ']' + StringOfChar('=', BlockEq) + ']';
              Q := PosEx(Closer, L, P);
              if Q > 0 then
              begin
                Advance(Q + Length(Closer) - P);
                EmitRun(clGreen);
                Block := stNone;
              end
              else
              begin
                Advance(Ln - P + 1);
                EmitRun(clGreen);
              end;
            end
            else
            begin
              Advance(N);
              EmitRun(clLightgray);
            end;
          end;
        '''', '"':
          begin
            Advance(N);
            while P <= Ln do
            begin
              if L[P] = '\' then
              begin
                Advance(1);
                if P <= Ln then
                  Advance(UTF8CodepointSize(@L[P]));
              end
              else if L[P] = L[P - 1] then
              begin
                Advance(UTF8CodepointSize(@L[P]));
                Break;
              end
              else
                Advance(UTF8CodepointSize(@L[P]));
            end;
            EmitRun(clGreen);
          end;
        '0'..'9':
          begin
            if (L[P] = '0') and (P + 1 <= Ln) and (L[P + 1] in ['x', 'X']) then
            begin
              Advance(2);
              while (P <= Ln) and (L[P] in ['0'..'9', 'a'..'f', 'A'..'F']) do
                Advance(1);
            end
            else
            begin
              while (P <= Ln) and (L[P] in ['0'..'9']) do
                Advance(1);
              if (P + 1 <= Ln) and (L[P] = '.') and (L[P + 1] in ['0'..'9']) then
              begin
                Advance(1);
                while (P <= Ln) and (L[P] in ['0'..'9']) do
                  Advance(1);
              end;
              if (P <= Ln) and (L[P] in ['e', 'E']) then
              begin
                if (P + 1 <= Ln) and
                   ((L[P + 1] in ['0'..'9']) or ((L[P + 1] in ['+', '-']) and (P + 2 <= Ln) and (L[P + 2] in ['0'..'9']))) then
                begin
                  Advance(1);
                  if (P <= Ln) and (L[P] in ['+', '-']) then
                    Advance(1);
                  while (P <= Ln) and (L[P] in ['0'..'9']) do
                    Advance(1);
                end;
              end;
            end;
            EmitRun(clOrange);
          end;
        'A'..'Z', 'a'..'z', '_':
          begin
            K := P;
            while (P <= Ln) and (L[P] in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) do
              Advance(1);
            if IsKeyword(Copy(L, K, P - K)) then
              EmitRun(clYellow)
            else if IsApiName(Copy(L, K, P - K)) then
              EmitRun(clSkyBlue)
            else
              EmitRun(clLightgray);
          end;
      else
        Advance(N);
        EmitRun(clLightgray);
      end;
    end;
    FRuns[I] := Runs;
  end;
end;

procedure TyroEditor.DrawLine(ACanvas: TTyroCanvas; ALine: Integer; aY: Integer; aGutterWidth: Integer);
var
  Runs: TEditorRuns;
  I, From, ToCol: Integer;
  x, x1, x2: Integer;
  textStart, cols: Integer;
  Gov: string;
  glyph: string;
  SelFrom, SelTo, l1, c1, l2, c2: Integer;
begin
  if (ALine < 0) or (ALine >= FLines.Count) then
    Exit;
  textStart := aGutterWidth;
  cols := (ClientRect.Width - textStart) div FCharWidth;
  if cols < 1 then
    cols := 1;

  //gutter number
  Gov := IntToStr(ALine + 1);
  x := aGutterWidth - Length(Gov) * FCharWidth - 2;
  if x < 0 then
    x := 0;
  ACanvas.DrawText(x, aY, Gov, clDarkGray);

  //draw text runs clipped to the visible range
  Runs := FRuns[ALine];
  for I := 0 to High(Runs) do
  begin
    From := Runs[I].Start;
    if From < FLeftCol then
      From := FLeftCol;
    ToCol := Runs[I].Start + Runs[I].Count;
    if ToCol > FLeftCol + cols then
      ToCol := FLeftCol + cols;
    if ToCol <= From then
      Continue;
    x := textStart + (From - FLeftCol) * FCharWidth;
    glyph := CPSub(FLines[ALine], From, ToCol - From);
    ACanvas.DrawText(x, aY, glyph, Runs[I].Color);
  end;

  //selection marker bar + redraw of the selected text
  SelFrom := -1;
  SelTo := -1;
  if IsSelecting then
  begin
    GetSelection(l1, c1, l2, c2);
    if ALine = l1 then
    begin
      SelFrom := c1;
      SelTo := CPCount(FLines[ALine]);
    end
    else if ALine = l2 then
    begin
      SelFrom := 0;
      SelTo := c2;
    end
    else if (ALine > l1) and (ALine < l2) then
    begin
      SelFrom := 0;
      SelTo := CPCount(FLines[ALine]);
    end;
    if (SelFrom >= 0) and (SelTo > SelFrom) then
    begin
      x1 := textStart + (SelFrom - FLeftCol) * FCharWidth;
      if x1 < textStart then
        x1 := textStart;
      x2 := textStart + (SelTo - FLeftCol) * FCharWidth;
      if x2 > ClientRect.Width then
        x2 := ClientRect.Width;
      if x2 > x1 then
      begin
        ACanvas.DrawRectangle(x1, aY, x2 - x1, FCharHeight, clDarkGray, True);
        for I := 0 to High(Runs) do
        begin
          From := Runs[I].Start;
          if From < SelFrom then
            From := SelFrom;
          ToCol := Runs[I].Start + Runs[I].Count;
          if ToCol > SelTo then
            ToCol := SelTo;
          if ToCol <= From then
            Continue;
          x := textStart + (From - FLeftCol) * FCharWidth;
          glyph := CPSub(FLines[ALine], From, ToCol - From);
          ACanvas.DrawText(x, aY, glyph, clWhite);
        end;
      end;
    end;
  end;
end;

procedure TyroEditor.DrawStatus(ACanvas: TTyroCanvas);
var
  tx, rx: string;
  sbY: Integer;
  rx2, tx2: string;
begin
  sbY := ClientRect.Height - GetStatusHeight;
  ACanvas.DrawRectangle(0, sbY, ClientRect.Width, GetStatusHeight, clBlack, True);
  tx := ' ' + FFileName;
  if FModified then
    tx := tx + ' *';
  if tx = ' ' then
    tx := ' Untitled';
  ACanvas.DrawText(0, sbY, tx, clLightgray);

  rx := Format('Ln %d, Col %d   [F2/Esc: close]', [FCaretLine + 1, FCaretCol + 1]);
  if (csHScroll in Style) or (csVScroll in Style) then
    ACanvas.DrawText(ClientRect.Width - UTF8Length(rx) * FCharWidth - cScrollSize, sbY, rx, clLightgray)
  else
    ACanvas.DrawText(ClientRect.Width - UTF8Length(rx) * FCharWidth, sbY, rx, clLightgray);
end;

procedure TyroEditor.DrawCaret(ACanvas: TTyroCanvas);
var
  x, y, px: Integer;
  col: TColor;
begin
  if not Visible or not Focused or not FCaretVisible then
    Exit;
  if FCaretLine < FTopLine then
    Exit;
  y := (FCaretLine - FTopLine) * FCharHeight;
  if y + FCharHeight > ClientRect.Height - GetStatusHeight then
    Exit;
  px := ColToPixel(FCaretLine, FCaretCol) - FLeftCol * FCharWidth;
  x := GetGutterWidth + px;
  if x < GetGutterWidth then
    x := GetGutterWidth;
  if x >= ClientRect.Width then
    Exit;
  col := clWhite.ReplaceAlpha(Round(255 * FCaretDim));
  ACanvas.DrawRectangle(x, y, 2, FCharHeight, col, True);
end;

procedure TyroEditor.DoPaintBackground(ACanvas: TTyroCanvas);
begin
  inherited;
end;

procedure TyroEditor.DoPaint(ACanvas: TTyroCanvas);
var
  I, Y: Integer;
  gW: Integer;
begin
  inherited;
  UpdateSizes;
  if (FRuns = nil) or (Length(FRuns) <> FLines.Count) then
    RebuildRuns;
  gW := GetGutterWidth;

  //caret line highlight
  if (FCaretLine >= FTopLine) and (FCaretLine < FLines.Count) then
  begin
    Y := (FCaretLine - FTopLine) * FCharHeight;
    if Y + FCharHeight <= ClientRect.Height - GetStatusHeight then
      ACanvas.DrawRectangle(0, Y, ClientRect.Width, FCharHeight, TColor.CreateRGBA(cEditorCaretLineColor), True);
  end;

  Y := 0;
  for I := FTopLine to FLines.Count - 1 do
  begin
    if Y + FCharHeight > ClientRect.Height - GetStatusHeight then
      Break;
    DrawLine(ACanvas, I, Y, gW);
    Inc(Y, FCharHeight);
  end;

  DrawStatus(ACanvas);
  DrawCaret(ACanvas);
end;

procedure TyroEditor.Update;
var
  mp: TVector2;
  lx, ly: Integer;
  wheel: Single;
begin
  if Visible and Focused then
  begin
    UpdateSizes;
    UpdateScrollBars;
    FCaretTimer := FCaretTimer + RayLib.GetFrameTime();
    if FCaretTimer >= cEditorCaretBlink then
      FCaretTimer := FCaretTimer - cEditorCaretBlink;
    FCaretDim := Sin(FCaretTimer / cEditorCaretBlink * 2 * Pi);
    FCaretDim := 0.3 + (FCaretDim + 1) / 2 * 0.7;
    FCaretVisible := True;
    Invalidate;
    UpdateKeyRepeat;

    mp := RayLib.GetMousePosition;
    lx := Round(mp.X) - WindowRect.Left - ClientRect.Left;
    ly := Round(mp.Y) - WindowRect.Top - ClientRect.Top;
    if RayLib.IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
    begin
      if (lx >= 0) and (ly >= 0) and (lx < ClientRect.Width) and (ly < ClientRect.Height)
         and (HitScrollBar(lx, ly) = []) then
      begin
        PlaceCaretAt(lx, ly);
        FAnchorLine := FCaretLine;
        FAnchorCol := FCaretCol;
        ClearSelection;
        FMouseDown := True;
        Invalidate;
      end;
    end;
    if FMouseDown and RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    begin
      if (lx >= 0) and (ly >= 0) and (lx < ClientRect.Width) and (ly < ClientRect.Height) then
      begin
        PlaceCaretAt(lx, ly);
        FSelecting := True;
        Invalidate;
      end;
    end;
    if not RayLib.IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
      FMouseDown := False;

    wheel := RayLib.GetMouseWheelMove;
    if wheel <> 0 then
    begin
      if RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT) then
      begin
        FLeftCol := FLeftCol - Trunc(wheel);
        if FLeftCol < 0 then
          FLeftCol := 0;
      end
      else
      begin
        FTopLine := FTopLine - Trunc(wheel);
        if FTopLine < 0 then
          FTopLine := 0;
        if FTopLine > FLines.Count - 1 then
          FTopLine := FLines.Count - 1;
      end;
      Invalidate;
    end;
  end
  else
  begin
    FRepeatKey := KEY_NULL;
    FCaretTimer := 0;
    FCaretDim := 1;
    FCaretVisible := True;
  end;
end;

procedure TyroEditor.KeyPress(var Key: TUTF8Char);
var
  S: string;
begin
  S := Key;
  if S = '' then
    Exit;
  BeginEdit;
  if IsSelecting then
    DeleteSelected;
  InsertSingleChar(S);
  ModifyDone;
  Key := '';
end;

procedure TyroEditor.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
begin
  //remember the key for auto-repeat while it stays held down (no Ctrl, no Insert)
  if not (ssCtrl in Shift) and (Key <> KEY_INSERT) and (Key <> KEY_ESCAPE) then
  begin
    FRepeatKey := Key;
    FRepeatTimer := cEditorRepeatDelay;
  end;
  ProcessKey(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TyroEditor.ProcessKey(var Key: TKeyboardKey; Shift: TShiftState);
var
  Extend: Boolean;
begin
  Extend := ssShift in Shift;
  case Key of
    KEY_ESCAPE:
      begin
        Close;
        Key := KEY_NULL;
      end;
    KEY_ENTER:
      begin
        BeginEdit;
        if IsSelecting then
          DeleteSelected;
        InsertText(#10);
        ModifyDone;
        Key := KEY_NULL;
      end;
    KEY_TAB:
      begin
        BeginEdit;
        if IsSelecting then
          DeleteSelected;
        if ssShift in Shift then
        begin
          //Shift+Tab: un-indent (remove up to TabWidth leading spaces)
          while (FCaretCol > 0) and (FCaretCol mod FTabWidth <> 0) and (CPSub(FLines[FCaretLine], FCaretCol - 1, 1) = ' ') do
          begin
            Dec(FCaretCol);
            DeleteCharAt(FCaretLine, FCaretCol);
          end;
          FDesiredCol := FCaretCol;
        end
        else
          InsertText(StringOfChar(' ', FTabWidth - (FCaretCol mod FTabWidth)));
        ModifyDone;
        Key := KEY_NULL;
      end;
    KEY_BACKSPACE:
      begin
        BeginEdit;
        BackspaceAtCaret;
        ModifyDone;
        Key := KEY_NULL;
      end;
    KEY_DELETE:
      begin
        BeginEdit;
        DeleteAtCaret;
        ModifyDone;
        Key := KEY_NULL;
      end;
    KEY_LEFT:
      if ssCtrl in Shift then
        MoveWord(False, Extend)
      else
        MoveLeft(Extend);
    KEY_RIGHT:
      if ssCtrl in Shift then
        MoveWord(True, Extend)
      else
        MoveRight(Extend);
    KEY_UP:
      MoveUp(Extend);
    KEY_DOWN:
      MoveDown(Extend);
    KEY_HOME:
      MoveHome(Extend);
    KEY_END:
      MoveEnd(Extend);
    KEY_PAGE_UP:
      MovePage(False, Extend);
    KEY_PAGE_DOWN:
      MovePage(True, Extend);
    KEY_INSERT:
      Key := KEY_NULL;
    KEY_C:
      if ssCtrl in Shift then
      begin
        CopySelection;
        Key := KEY_NULL;
      end;
    KEY_V:
      if ssCtrl in Shift then
      begin
        PasteText;
        Key := KEY_NULL;
      end;
    KEY_X:
      if ssCtrl in Shift then
      begin
        CutSelection;
        Key := KEY_NULL;
      end;
    KEY_A:
      if ssCtrl in Shift then
      begin
        SelectAll;
        Key := KEY_NULL;
      end;
    KEY_Z:
      if ssCtrl in Shift then
      begin
        UndoMove;
        Key := KEY_NULL;
      end;
    KEY_Y:
      if ssCtrl in Shift then
      begin
        RedoMove;
        Key := KEY_NULL;
      end;
    KEY_S:
      if ssCtrl in Shift then
      begin
        if Assigned(FOnSave) then
          FOnSave(Self);
        Key := KEY_NULL;
      end;
  end;
end;

procedure TyroEditor.UpdateKeyRepeat;
var
  dt: Double;
  Key: TKeyboardKey;
  Shift: TShiftState;
begin
  if FRepeatKey = KEY_NULL then
    Exit;
  if not RayLib.IsKeyDown(FRepeatKey) then
  begin
    FRepeatKey := KEY_NULL;
    Exit;
  end;
  dt := RayLib.GetFrameTime();
  FRepeatTimer := FRepeatTimer - dt;
  if FRepeatTimer > 0 then
    Exit;
  FRepeatTimer := FRepeatTimer + cEditorRepeatInterval;
  //rebuild the shift state live so Shift+arrow keeps extending on repeat
  Shift := [];
  if RayLib.IsKeyDown(KEY_LEFT_SHIFT) or RayLib.IsKeyDown(KEY_RIGHT_SHIFT) then
    Shift := Shift + [ssShift];
  Key := FRepeatKey;
  ProcessKey(Key, Shift);
end;

procedure TyroEditor.LoadSource(ASource: TStringList);
begin
  FLines.Assign(ASource);
  if FLines.Count = 0 then
    FLines.Add('');
  FCaretLine := 0;
  FCaretCol := 0;
  FDesiredCol := 0;
  FTopLine := 0;
  FLeftCol := 0;
  FModified := False;
  FSelecting := False;
  FUndo.Clear;
  FRedo.Clear;
  RebuildRuns;
  Invalidate;
end;

procedure TyroEditor.SaveSource(ASource: TStringList);
begin
  ASource.Assign(FLines);
  FModified := False;
  Invalidate;
end;

procedure TyroEditor.Close;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
  Visible := False;
end;

end.
