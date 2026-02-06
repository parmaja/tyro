unit TyroConsoles;
{**
 *  This file is part of the "Tyro", based on CMDLine (Julian Schutsch)
 *
 * @license   MIT/LGPL idk
 *
 * @author    Julian Schutsch, Zaher Dirkey
 *
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}{$M+}

{ Copyright (C) 2007 Julian Schutsch

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU Lesser General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
  details.

  A copy of the GNU Lesser General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/lgpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
interface

uses
  Classes, SysUtils, Contnrs,
  LazUTF8, LCLType,
  RayLib,
  TyroClasses, TyroControls;

const
  SB_HORZ = 0;
  SB_Vert = 1;

type

  TCaretType = (cartLine, cartSubBar, cartBigBar, cartUser);
  TEscapeCodeType = (esctConsole, esctAnsi, esctNone);
  TEscapeMode = (escmNone, escmOperation, escmData2, escmData1,
    escmAnsiOperation, escmAnsiSquare);
  TCharAttrib = (charaUnderline, charaItalic, charaBold, charaBlink);

  TTyroConsole = class;

  TColorstring = class;
  TColorStrings = class;

  EOnConsoleInput = procedure(AConsole: TTyroConsole; Input: string) of object;
  EOnConsoleInputChange = procedure(AConsole: TTyroConsole; InputData: TColorstring) of object;

  { TTyroConsole }

  TTyroConsole = class(TTyroControl)
  private
    //FCaretTimer: TTimer;
    FCaretVisible: boolean;
    FLineCount: Integer;
    FLines: TColorStrings;
    FLineHeights: array of Integer;
    FLineHeightSum: array of Integer;
    FTopLine: Integer;
    FPageHeight: Integer;
    FVisibleLines: Integer;
    FVSBVisible: boolean;
    FVSBPos: Integer;
    //FVSBWidth:  Integer;
    FCaretX:    Integer;
    FOutX, FOutY: Integer;
    FInputX, FInputY: Integer;
    FInputPos: Integer;
    FCharHeight: Integer;
    FCharWidth: Integer;
    FLineOfTopLine: Integer;
    FVisibleLineCount: Integer;
    FInput: Boolean;
    FInputBuffer: TColorstring;
    FInputVisible: Boolean;
    FInputMinPos: Integer;
    FUTF8InputMinPos: Integer;
    FOnInput:   EOnConsoleInput;
    FOnAny:     EOnConsoleInputChange;
    FOnInputChange: EOnConsoleInputChange;
    FBackGroundColor: TColor;
    FCurrentColor: TColor;
    FCurrentBackGround: TColor;
    FPassWordChar: TUTF8Char;
    FInputIsPassWord: Boolean;
    FHistory: TColorstrings;
    FHistoryLength: Integer;
    FHistoryMax: Integer;
    FHistoryPos: Integer;
    FInputColor: TColor;
    FInputBackground: TColor;
    FInputSelColor: TColor;
    FInputSelBackGround: TColor;
    FMouseDown: Boolean;
    FSelStart, FSelEnd: Integer;
    FMouseDownInputPos: Integer;
    FCurrentString: string;
    FCaretColor: TColor;
    FCaretType: TCaretType;
    FCaretWidth: Integer;
    FCaretHeight: Integer;
    FCaretYShift: Integer;
    FTabWidth:  Integer;
    FGraphicCharWidth: Integer;//Delete
    FEscapeCodeType: TEscapeCodeType;
    FEscapeMode: TEscapeMode;
    FEscapeData: string;
    FStringBuffer: TStringList;
    FAutoFollow: Boolean;
    FCurrentAttrib: TCharAttrib;
    FInputAttrib: TCharAttrib;
    FWriteInput: Boolean;
    procedure CaretTimerExecute(Sender: TObject);
    procedure SetLineCount(c: Integer);
    procedure SetTopLine(Nr: Integer);
    procedure AdjustScrollBars(const Recalc:Boolean=False);
    function AdjustLineHeight(i: Integer; const Recalc:Boolean = False): Integer;
    procedure MakeInputVisible;
    procedure MakeOutVisible;
    procedure SetBackGroundColor(c: Tcolor);
    function GetSystemMetricsGapSize(const Index: Integer): Integer;
    procedure ScrollBarRange(Which: TScrollbarType; aRange, aPage: Integer);
    procedure ScrollBarPosition(Which: TScrollbarType; Value: Integer);
    function UpdateLineHeights(const Recalc:Boolean=False): Integer;
    procedure TranslateScrollBarPosition;
    procedure ScrollUp;
    procedure SetHistoryMax(v: Integer);
    procedure InsertHistory;
    procedure SetHistoryPos(v: Integer);
    function GetHistory(i: Integer): string;
    procedure DeleteHistoryEntry(i: Integer);
    procedure MakeFirstHistoryEntry(i: Integer);
    function MoveInputCaretTo(x, y: Integer; chl: Boolean): Boolean;
    procedure SetSelection(Start, Stop: Integer);
    procedure LeftSelection(Start, Stop: Integer);
    procedure RightSelection(Start, Stop: Integer);
    procedure DeleteSelected;
    procedure SetOutY(v: Integer);
    procedure IntWrite;
    procedure MultiWrite;
    procedure SetCaretType(ACaretType: TCaretType);
    procedure SetCaretWidth(AValue: Integer);
    procedure SetCaretHeight(AValue: Integer);
    procedure SetCaretYShift(AValue: Integer);
    procedure SetTabWidth(AValue: Integer);
    //function GetCaretInterval: Integer;
    //procedure SetCaretInterval(AValue: Integer);

  protected
    procedure Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer); override;

  public
    constructor Create(AParent: TTyroContainer); override;
    destructor Destroy; override;

    procedure DoPaint(ACanvas: TTyroCanvas); override;
    procedure Resize; override;
    procedure KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: TKeyboardKey; Shift: TShiftState); override;
    procedure KeyUp(var Key: TKeyboardKey; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: Integer); override;

    procedure SaveToFile(AFileName: string);
    function HistoryHas(s: string): Boolean;
    function HistoryIndexOf(s: string): Integer;
    procedure ClearHistory;
    procedure TextColor(C: TColor);
    procedure TextBackground(C: TColor);
    procedure TextColors(FC, BC: TColor);
    procedure Write(s: string);
    procedure Writeln(s: string);
    procedure Clear;
    procedure StartRead(DFC, DBC: TColor; const Desc: string; IFC, IBC: TColor);
    procedure StartReadPassWord(DFC, DBC: TColor; const Desc: string; IFC, IBC: TColor);
    procedure StopRead;
    procedure CopyToClipBoard;
    procedure PasteFromClipBoard;
    procedure CutToClipBoard;
    procedure ClearLine;
    property OutX: Integer Read FOutX Write FOutX;
    property OutY: Integer Read FOutY Write SetOutY;
    property TopLine: Integer Read FTopLine Write SetTopLine;
    property History[i: Integer]: string Read GetHistory;
    property InputPos: Integer Read FInputPos;
    function HistoryCount: Integer;
  public
    property CharHeight: Integer read FCharHeight write FCharHeight;
    property CharWidth: Integer read FCharWidth write FCharWidth;

    property CaretColor: TColor Read FCaretColor Write FCaretColor;
    property CaretType: TCaretType Read FCaretType Write SetCaretType;
    property CaretWidth: Integer Read FCaretWidth Write SetCaretWidth;
    property CaretHeight: Integer Read FCaretHeight Write SetCaretHeight;
    property CaretYShift: Integer Read FCaretYShift Write SetCaretYShift;
    property OnInput: EOnConsoleInput Read FOnInput Write FOnInput;
    property OnInputChange: EOnConsoleInputChange Read FOnInputChange Write FOnInputChange;
    property OnAny: EOnConsoleInputChange Read FOnAny Write FOnAny;
    property LineCount: Integer Read FLineCount Write SetLineCount;
    property BackGroundColor: TColor Read FBackgroundColor Write SetBackGroundColor;
    property TabWidth: Integer Read FTabWidth Write SetTabWidth;
    property PassWordChar: TUTF8Char Read FPassWordChar Write FPassWordChar;
    property HistoryMax: Integer Read FHistoryMax Write SetHistoryMax;
    property InputSelColor: TColor Read FInputSelColor Write FInputSelColor;
    property InputSelBackGround: TColor Read FInputSelBackGround write FInputSelBackGround;
    //property CaretInterval: Integer Read GetCaretInterval Write SetCaretInterval;
    property EscapeCodeType: TEscapeCodeType Read FEscapeCodeType Write FEscapeCodeType;
    property GraphicalCharacterWidth: Integer Read FGraphicCharWidth Write FGraphicCharWidth;
    property AutoFollow: Boolean Read FAutoFollow Write FAutoFollow default True;
    property WriteInput:Boolean read FWriteInput write FWriteInput default True;
  end;

  TColorChar = packed record
    FChar:      TUTF8Char;
    FSumWidth:  Integer;
    FWordStart: Integer;
    FFrontColor: TColor;
    FBackColor: TColor;
    FAttrib:    TCharAttrib;
  end;

  { TColorString }

  TColorString = class(TObject)
  private
    FChars: packed array of TColorChar;
    FSumWidth: Integer;
    FPassWordStart: Integer;
    FPassWordChar: TUTF8Char;
    FStoredLineCount:Integer;
    FDefaultBackGround: TColor;
    FConsole: TTyroConsole;
    function GetCharWidth: Integer;
    function GetTabWidth: Integer;
    procedure MinimumLength(V: Integer; FC, BC: TColor);
    procedure MaximumLength(V: Integer);
    procedure UpdateSum;
    procedure UpdateAll;
  public
    constructor Create(AConsole: TTyroConsole);
    destructor Destroy; override;
    procedure Clear;
    procedure OverWrite(S: string; Pos: Integer; FC, BC: TColor; Attrib: TCharAttrib);
    procedure OverWriteChar(s: TUTF8Char; Pos, ADefWidth: Integer; FC, BC: TColor; Attrib: TCharAttrib);
    procedure OverWrite(S: TColorString; Pos: Integer);
    procedure OverWritePW(S: TColorString; PWS, Pos: Integer; PWC: string);
    procedure PartOverWrite(S: TColorString; Start, Stop, Pos: Integer);
    procedure LineOutAndFill(ACanvas: TTyroCanvas;
      AX, AY, ALeftX, AWrapWidth, ACH, ACB, ACaretPos: Integer;
      ABC, ACC: TColor; ACaretHeight, ACaretWidth, ACaretYShift: Integer;
      ADrawCaret: Boolean);
    function GetString: string;
    function GetPartString(Start, Stop: Integer): string;
    procedure Delete(Index: Integer);
    procedure Delete(Index, Len: Integer);
    procedure Insert(Index: Integer; C: string; FC, BC: TColor; Attrib: TCharAttrib);
    procedure BColorBlock(StartPos, EndPos: Integer; C: TColor);
    procedure ColorBlock(StartPos, EndPos: Integer; FC, BC: TColor);
    function LineCount(AWrapWidth, ACaretPos, ACaretWidth: Integer): Integer;
    function GetCount: Integer;
    function GetLineOfCaret(AWrapWidth, ACaretPos, ACaretWidth: Integer): Integer;
    function GetCharPosition(AWrapWidth, ALine, AXPos: Integer): Integer;
  public
    property TabWidth: Integer Read GetTabWidth;
    property PassWordChar: TUTF8Char Read FPassWordChar Write FPassWordChar;
    property PassWordStart: Integer Read FPassWordStart Write FPassWordStart;
    property Count: Integer Read GetCount;
    property DefaultBackGround: TColor Read FDefaultBackground Write FDefaultBackground;
    property CharWidth: Integer read GetCharWidth;
  end;

  { TColorStrings }

  TColorStrings = class(TObjectList)
  private
    FConsole: TTyroConsole;
    function GetItems(Index: Integer): TColorString;
    function GetRequire(Index: Integer): TColorString;
    procedure SetItems(Index: Integer; AValue: TColorString);
  public
    constructor Create(AConsole: TTyroConsole);
    property Require[Index: Integer]: TColorString read GetRequire;
    property Items[Index: Integer]: TColorString read GetItems write SetItems; default;
  end;

implementation

var
  AnsiColors: array[0..7] of TColor;

{ TColorStrings }

function TColorStrings.GetItems(Index: Integer): TColorString;
begin
  Result := inherited Items[Index] as TColorString;
end;

function TColorStrings.GetRequire(Index: Integer): TColorString;
begin
  if Index >= Count then
    Count := Index + 1;
  if Items[Index] = nil then
    Items[Index] := TColorString.Create(FConsole);

  Result := Items[Index];
end;

procedure TColorStrings.SetItems(Index: Integer; AValue: TColorString);
begin
  inherited Items[Index] := AValue;
end;

constructor TColorStrings.Create(AConsole: TTyroConsole);
begin
  inherited Create;
  FConsole := AConsole;
end;

procedure TTyroConsole.SaveToFile(AFileName: string);
var
  Txt: System.Text;
  i:   Integer;
begin
  AssignFile(Txt, AFileName);
  Rewrite(Txt);
  for i := 0 to LineCount - 1 do
  begin
    with FLines[i] do
    begin
      system.Writeln(Txt, GetString);
    end;
  end;
  CloseFile(Txt);
end;

procedure TColorString.UpdateAll;
begin
  UpdateSum;
end;

procedure TColorString.UpdateSum;
begin
  FSumWidth := Length(FChars) * CharWidth;
  FStoredLineCount:=-1;
end;

function TColorString.GetCount: Integer;
begin
  Result := System.Length(FChars);
end;

procedure TTyroConsole.SetTabWidth(AValue: Integer);
begin
  FTabWidth := AValue;
  UpdateLineHeights;
  Invalidate;
end;

procedure TTyroConsole.SetCaretWidth(AValue: Integer);
begin
  FCaretWidth := AValue;
  FCaretType  := cartUser;
end;

procedure TTyroConsole.SetCaretHeight(AValue: Integer);
begin
  FCaretHeight := AValue;
  FCaretType   := cartUser;
end;

procedure TTyroConsole.SetCaretYShift(AValue: Integer);
begin
  FCaretYShift := AValue;
  FCaretType   := cartUser;
end;

procedure TTyroConsole.SetCaretType(ACaretType: TCaretType);
begin
  case ACaretType of
    cartLine:
    begin
      FCaretWidth := 1;
      FCaretYShift := 3;
    end;
    cartSubBar:
    begin
      FCaretWidth  := -1;
      FCaretHeight := 3;
      FCaretYShift := 0;
    end;
    cartBigBar:
    begin
      FCaretWidth := -1;
      FCaretYShift := 3;
    end;
  end;
  Invalidate;
  FCaretType := ACaretType;
end;

// TOdo : Use string buffer instead of string (speed improvement expected)
procedure TColorString.LineOutAndFill(ACanvas: TTyroCanvas;
  AX, AY, ALeftX, AWrapWidth, ACH, ACB, ACaretPos: Integer; ABC, ACC: TColor;
  ACaretHeight, ACaretWidth, ACaretYShift: Integer; ADrawCaret: Boolean);
var
  LineStart         : Integer;
  LineEnd           : Integer;
  MidWidth          : Integer;
  LineStartSumWidth : Integer;
  x                 : Integer;
  LastLineSumWidth  : Integer;
  ACHH              : Integer;
  ACBH              : Integer;
  SAX               : Integer;
  SAY               : Integer;

  procedure DrawLine;
  var
    SameColor: string;
    SameForeColor: TColor;
    SameBackColor: TColor;
    SameColorX: Integer;
    SameColorWidth: Integer;
    LP:     Integer;
    CaretX: Integer;
    CaretW: Integer;
    CW:     Integer;
    xp:     Integer;
  begin
    if (AY <= -ACH) and (AY > ACanvas.Height) then
    begin
      Inc(AY, ACH);
      Ax := ALeftx;
      Exit;
    end;
    SameColor := '';
    SameForeColor := clBlack;
    SameColorX := 0;
    SameColorWidth := 0;
    LP     := LineStart;
    CaretX := -1;
    while LineStart <> LineEnd + 1 do
    begin
      with FChars[LineStart] do
      begin
        CW := CharWidth;
        if FChar = #9 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.PenColor  := SameForeColor;
            ACanvas.DrawText(SameColorX, AY, SameColor, SameForeColor);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
        end
        else
        if FChar[1] = #27 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.PenColor  := SameForeColor;
            ACanvas.DrawText(SameColorX, AY, SameColor, SameForeColor);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
          case FChar[2] of
            #9:
            begin
              case FChar[3] of
                #46:
                begin
                  ACanvas.PenColor   := FFrontColor;
                  //ACanvas.Pen.Style   := psDash;
                  xp := SameColorX;
                  if xp mod 2 <> 0 then
                    Inc(xp);
                  while xp < SameColorX + CharWidth do
                  begin
                    ACanvas.DrawPixel(xp, AY + ACH - 3, FFrontColor);
                    Inc(xp, 2);
                  end;
                end;
                #196:
                begin
                  ACanvas.PenColor   := FFrontColor;
                  //ACanvas.PenStyle   := psSolid;
                  ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + CharWidth, AY + ACHH, FFrontColor);
                end;
              end;
            end;
            #10:
            begin
              CW := AWrapWidth - SameColorX;
              case FChar[3] of
                #179:
                begin
                  ACanvas.PenColor   := FFrontColor;
                  ACanvas.DrawLine(SameColorX + CW - ACBH, AY, SameColorX +
                    CW - ACBH, AY + ACH, FFrontColor);
                end;
                #180:
                begin
                  ACanvas.DrawLine(SameColorX + CW - ACBH, AY, SameColorX +
                    CW - ACBH, AY + ACH, FFrontColor);
                  ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + CW - ACBH, AY + ACHH, FFrontColor);
                end;
                #191:
                begin
                  ACanvas.PenColor   := FFrontColor;
                  ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + CW - ACBH, AY + ACHH, FFrontColor);
                  ACanvas.DrawLine(SameColorX + CW - ACBH, AY + ACHH, SameColorX + CW - ACBH, AY + ACH, FFrontColor);
                end;
                #196:
                begin
                  ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + CW, AY + ACHH, FFrontColor);
                end;
                #205:
                begin
                  ACanvas.DrawLine(SameColorX, AY + ACHH - 1, SameColorX +
                    CW, AY + ACHH - 1, FFrontColor);
                  ACanvas.DrawLine(SameColorX, AY + ACHH + 1, SameColorX +
                    CW, AY + ACHH + 1, FFrontColor);
                end;
                #217:
                begin
                  ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + CW - ACBH, AY + ACHH, FFrontColor);
                  ACanvas.DrawLine(SameColorX + CW - ACBH, AY + ACHH, SameColorX + CW - ACBH, AY - 1, FFrontColor);
                end;
              end;
            end;
            #32, #33:
            begin
            end;
            #46, #47:
            begin
              xp := SameColorX;
              if xp mod 2 <> 0 then
                Inc(xp);
              while xp < SameColorX + CharWidth do
              begin
                ACanvas.DrawPixel(xp, AY + ACH - 3, FFrontColor);
                Inc(xp, 2);
              end;
            end;
            #196, #197:
            begin
              ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + CharWidth, AY + ACHH, FFrontColor);
            end;
            #179:
            begin
              ACanvas.DrawLine(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACH, FFrontColor);
            end;
            #193:
            begin
              ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + ACB, AY + ACHH, FFrontColor);
              ACanvas.DrawLine(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACHH, FFrontColor);
            end;
            #194:
            begin
              ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + ACB, AY + ACHH, FFrontColor);
              ACanvas.DrawLine(SameColorX + ACBH, AY + ACHH, SameColorX + ACBH, AY + ACH, FFrontColor);
            end;
            #198:
            begin
              ACanvas.DrawLine(SameColorX, AY + ACHH, SameColorX + ACB, AY + ACHH, FFrontColor);
              ACanvas.DrawLine(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACH, FFrontColor);
            end;
            #195:
            begin
              ACanvas.DrawLine(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACH, FFrontColor);
              ACanvas.DrawLine(SameColorX + ACBH, AY + ACHH, SameColorX + ACB, AY + ACHH, FFrontColor);
            end;
            #217:
            begin
              ACanvas.DrawLine(SameColorX + ACBH, AY, SameColorX + ACBH, AY + ACHH, FFrontColor);
              ACanvas.DrawLine(SameColorX + ACBH, AY + ACHH, SameColorX + ACB, AY + ACHH, FFrontColor);
            end;
            #218:
            begin
              ACanvas.DrawLine(SameColorX + ACBH, AY + ACH, SameColorX + ACBH, AY + ACHH, FFrontColor);
              ACanvas.DrawLine(SameColorX + ACBH, AY + ACHH, SameColorX + ACB, AY + ACHH, FFrontColor);
            end;
          end;
        end
        else
        if SameColor = '' then
        begin
          if (LP >= FPassWordStart) then
          begin
            SameColor      := FPassWordChar;
            SameColorWidth := CharWidth;
          end
          else
          begin
            SameColor      := FChar;
            SameColorWidth := CharWidth;
          end;
          SameColorX    := AX;
          SameForeColor := FFrontColor;
          SameBackColor := FBackColor;
        end
        else
        begin
          if (SameForeColor = FFrontColor) and (SameBackColor = FBackColor) then
          begin
            if (LP >= FPassWordStart) then
            begin
              SameColor := SameColor + FPassWordChar;
              Inc(SameColorWidth, CharWidth);
            end
            else
            begin
              SameColor := SameColor + FChar;
              Inc(SameColorWidth, CharWidth);
            end;
          end
          else
          begin
            ACanvas.PenColor  := SameForeColor;
            ACanvas.DrawText(SameColorX, AY, SameColor, SameForeColor);
            if (LP >= FPassWordStart) then
            begin
              SameColor      := FPassWordChar;
              SameColorWidth := CharWidth;
            end
            else
            begin
              SameColor      := FChar;
              SameColorWidth := CharWidth;
            end;
            SameForeColor := FFrontColor;
            SameBackColor := FBackColor;
            SameColorX    := AX;
          end;
        end;
        if LP = ACaretPos then
        begin
          CaretX := AX;
          CaretW := CharWidth;
        end;
        Inc(AX, CW);
        Inc(LP);
      end;
      Inc(LineStart);
    end;
    if SameColor <> '' then
    begin
      ACanvas.PenColor  := SameForeColor;
      ACanvas.DrawText(SameColorX, AY, SameColor, SameForeColor);
    end;
    AX := ALeftX;
    Inc(AY, ACH);
    if ADrawCaret and (CaretX >= 0) then
    begin
      if ACaretWidth >= 0 then
        CaretW := ACaretWidth;
      ACanvas.DrawRect(CaretX, AY - ACaretHeight - ACaretYShift, CaretX + CaretW, AY - ACaretYShift, ACC, True);
    end;
  end;

  procedure DrawBack;
  var
    SameColor: string;
    SameForeColor: TColor;
    SameBackColor: TColor;
    SameColorX: Integer;
    SameColorWidth: Integer;
    LP:     Integer;
    CW:     Integer;
  begin
    if (AY <= -ACH) and (AY > ACanvas.Height) then
    begin
      Inc(AY, ACH);
      Ax := ALeftx;
      Exit;
    end;
    SameColor := '';
    SameBackColor := clBlack;
    SameColorX := 0;
    SameColorWidth := 0;
    LP     := LineStart;
    while LineStart <> LineEnd + 1 do
    begin
      with FChars[LineStart] do
      begin
        CW := CharWidth;
        if FChar = #9 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.DrawRect(SameColorX, AY, SameColorX + SameColorWidth, Ay + ACH, SameBackColor, True);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
          ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
        end
        else
        if FChar[1] = #27 then
        begin
          if SameColor <> '' then
          begin
            ACanvas.DrawRect(SameColorX, AY, SameColorX + SameColorWidth, Ay + ACH, SameBackColor, True);
            Inc(SameColorX, SameColorWidth);
            SameColor := '';
          end
          else
            SameColorX := AX;
          case FChar[2] of
            #9:
            begin
              case FChar[3] of
                #46:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
                end;
                #196:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
                end;
              end;
            end;
            #10:
            begin
              CW := AWrapWidth - SameColorX;
              case FChar[3] of
                #179:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CW, AY + ACH, FBackColor, True);
                end;
                #180:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CW, AY + ACH, FBackColor, True);
                end;
                #191:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CW, AY + ACH, FBackColor, True);
                end;
                #196:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CW, AY + ACH, FBackColor, True);
                end;
                #205:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CW, AY + ACH, FBackColor, True);
                end;
                #217:
                begin
                  ACanvas.DrawRect(SameColorX, AY, SameColorX + CW, AY + ACH, FBackColor, True);
                end;
              end;
            end;
            #32, #33:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #46, #47:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #196, #197:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #179:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #193:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #194:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #198:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #195:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #217:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
            #218:
            begin
              ACanvas.DrawRect(SameColorX, AY, SameColorX + CharWidth, AY + ACH, FBackColor, True);
            end;
          end;
        end
        else
        if SameColor = '' then
        begin
          if (LP >= FPassWordStart) then
          begin
            SameColor      := FPassWordChar;
            SameColorWidth := CharWidth;
          end
          else
          begin
            SameColor      := FChar;
            SameColorWidth := CharWidth;
          end;
          SameColorX    := AX;
          SameForeColor := FFrontColor;
          SameBackColor := FBackColor;
        end
        else
        begin
          if (SameForeColor = FFrontColor) and (SameBackColor = FBackColor) then
          begin
            if (LP >= FPassWordStart) then
            begin
              SameColor := SameColor + FPassWordChar;
              Inc(SameColorWidth, CharWidth);
            end
            else
            begin
              SameColor := SameColor + FChar;
              Inc(SameColorWidth, CharWidth);
            end;
          end
          else
          begin
            ACanvas.DrawRect(SameColorX, Ay, SameColorX + SameColorWidth, Ay + ACH, SameBackColor, True);
            if (LP >= FPassWordStart) then
            begin
              SameColor      := FPassWordChar;
              SameColorWidth := CharWidth;
            end
            else
            begin
              SameColor      := FChar;
              SameColorWidth := CharWidth;
            end;
            SameForeColor := FFrontColor;
            SameBackColor := FBackColor;
            SameColorX    := AX;
          end;
        end;
        Inc(AX, CW);
        Inc(LP);
      end;
      Inc(LineStart);
    end;
    if SameColor <> '' then
    begin
      ACanvas.DrawRect(SameColorX, Ay, SameColorX + SameColorWidth, Ay + ACH, SameBackColor, True);
    end;
    ACanvas.DrawRect(AX, AY, AWrapWidth, AY + ACH, SameBackColor, True);
    AX := ALeftX;
    Inc(AY, ACH);
  end;

begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    ACanvas.DrawRect(AX, AY, AWrapWidth, AY + ACH, ABC, True);
    Exit;
  end;
  ACHH     := ACH div 2;
  ACBH     := ACB div 2;
  SAX:=AX;
  SAY:=AY;
  MidWidth := FSumWidth div System.Length(FChars);
  // Draw background
  LineStart         := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth  := 0;
  x                 := 0;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LineEnd := x;
    DrawBack;
    LastLineSumWidth  := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart         := x + 1;
  end;
  // Draw foreground
  LineStart         := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth  := 0;
  x                 := 0;
  AX:=SAX;
  AY:=SAY;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LineEnd := x;
    DrawLine;
    LastLineSumWidth  := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart         := x + 1;
  end;
  // Draw Caret
  if ACaretPos >= LineStart then
  begin
    if ACaretWidth >= 0 then
      x := ACaretWidth
    else
      x := CharWidth;
    AX := LineStartSumWidth - LastLineSumWidth + (ACaretPos - LineStart) * x;
    if Ax + x > AWrapWidth then
    begin
      Ax := 0;
      ACanvas.DrawRect(0, AY, AWrapWidth, AY + ACH, ABC, True);
      Inc(Ay, ACH);
    end;
    if ADrawCaret then
    begin
      ACanvas.DrawRect(AX, AY - ACaretHeight - ACaretYShift, AX + x, AY - ACaretYShift, ACC, True);
    end;
  end;
end;

function TColorString.GetCharPosition(AWrapWidth, ALine, AXPos: Integer): Integer;
var
  x, MidWidth, LineStart, LineStartSumWidth, LastLineSumWidth, LastLineStart: Integer;
begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    Result := 0;
    Exit;
  end;
  MidWidth := FSumWidth div System.Length(FChars);
  if MidWidth = 0 then
  begin
    Result := 0;
    Exit;
  end;
  LineStart := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth := 0;
  LastLineStart    := 0;
  x := 0;
  while (LineStart < System.Length(FChars)) and (ALine >= 0) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LastLineSumWidth := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LastLineStart := LineStart;
    LineStart := x + 1;
    Dec(ALine);
  end;
  Result := LastLineStart;
  while (Result < LineStart) and (FChars[Result].FSumWidth -
      LastLineSumWidth <= AXPos) do
    Inc(Result);
end;

function TColorString.GetLineOfCaret(AWrapWidth, ACaretPos, ACaretWidth:
  Integer): Integer;
var
  x, MidWidth, LineStart, LineStartSumWidth, LastLineSumWidth: Integer;
begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    Result := 0;
    Exit;
  end;
  MidWidth := FSumWidth div System.Length(FChars);
  if MidWidth = 0 then
  begin
    Result := 0;
    Exit;
  end;
  LineStart := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth := 0;
  Result := 0;
  x := 0;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <
        AWrapWidth) do
      Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LastLineSumWidth := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart := x + 1;
    if ACaretPos < x then
      Exit;
    Inc(Result);
  end;
  if ACaretWidth >= 0 then x := ACaretWidth else x := CharWidth;
  if (ACaretPos > LineStart) or (LineStartSumWidth - LastLineSumWidth +
    (ACaretPos - LineStart) * x + x <= AWrapWidth) then
    Dec(Result);
end;

function TColorString.LineCount(AWrapWidth, ACaretPos, ACaretWidth: Integer): Integer;
var
  x: Integer;
  MidWidth: Integer;
  LineStart: Integer;
  LineStartSumWidth: Integer;
  LastLineSumWidth: Integer;
begin
  if AWrapWidth < 0 then
    AWrapWidth := 0;
  if System.Length(FChars) = 0 then
  begin
    Result := 1;
    Exit;
  end;
  MidWidth := FSumWidth div System.Length(FChars);
  if MidWidth = 0 then
  begin
    Result := 1;
    Exit;
  end;
  LineStart := 0;
  LineStartSumWidth := 0;
  LastLineSumWidth := 0;
  Result := 0;
  x := 0;
  while LineStart < System.Length(FChars) do
  begin
    x := LineStart + AWrapWidth div MidWidth;
    if x > High(FChars) then
      x := High(FChars);
    while (x < High(FChars)) and (FChars[x].FSumWidth - LineStartSumWidth <AWrapWidth) do Inc(x);
    while (x > LineStart) and (FChars[x].FSumWidth - LineStartSumWidth >= AWrapWidth) do
      with FChars[x] do
        if (FChar <> ' ') and (FWordStart > LineStart) then
          x := FWordStart - 1
        else
          Dec(x);
    LastLineSumWidth := LineStartSumWidth;
    LineStartSumWidth := FChars[x].FSumWidth;
    LineStart := x + 1;
    Inc(Result);
  end;
  if ACaretWidth >= 0 then
    x := ACaretWidth
  else
    x := CharWidth;
  if (ACaretPos >= LineStart) and (LineStartSumWidth - LastLineSumWidth +
    (ACaretPos - LineStart) * x + x > AWrapWidth) then
    Inc(Result);
  if Result=0 then Inc(Result);
end;

constructor TColorString.Create(AConsole: TTyroConsole);
begin
  inherited Create;
  FConsole := AConsole;
  FPassWordStart := MaxInt;
  DefaultBackGround := FConsole.FBackGroundColor;
  FStoredLineCount:= -1;
end;

procedure TColorString.BColorBlock(StartPos, EndPos: Integer; C: TColor);
var
  i: Integer;
begin
  if StartPos < 0 then
    StartPos := 0;
  if EndPos > High(FChars) then
    EndPos := High(FChars);
  for i := StartPos to EndPos do
    FChars[i].FBackColor := C;
end;

procedure TColorString.ColorBlock(StartPos, EndPos: Integer; FC, BC: TColor);
var
  i: Integer;
begin
  if StartPos < 0 then
    StartPos := 0;
  if EndPos > High(FChars) then
    EndPos := High(FChars);
  for i := StartPos to EndPos do
  begin
    FChars[i].FFrontColor := FC;
    FChars[i].FBackColor  := BC;
  end;
end;

procedure TColorString.Insert(Index: Integer; C: string; FC, BC: TColor; Attrib: TCharAttrib);
var
  i:      Integer;
  l:      Integer;
  Pp:     Integer;
  OldLen: Integer;
  SLen:   Integer;
begin
  OldLen := System.Length(FChars);
  SLen   := UTF8Length(C);
  if OldLen < Index then
    MinimumLength(Index + SLen, FC, BC)
  else
  begin
    MinimumLength(SLen + OldLen, FC, BC);
    for i := OldLen - 1 downto Index do
      FChars[i + SLen] := FChars[i];
  end;
  pp := 1;
  for i := 0 to SLen - 1 do
  begin
    l := UTF8CodepointSize(@C[Pp]);
    with FChars[Index + i] do
    begin
      FChar := Copy(C, Pp, l);
      FFrontColor := FC;
      FBackColor := BC;
      FAttrib    := Attrib;
    end;
    Inc(pp, l);
  end;
  UpdateSum;
end;

procedure TColorString.Delete(Index, Len: Integer);
var
  i: Integer;
begin
  if (Len = 0) or (Index >= System.Length(FChars)) then
    Exit;
  if Index + Len > System.Length(FChars) then
    Len := System.Length(FChars) - Index;
  for i := Index to System.Length(FChars) - Len - 1 do
    FChars[i] := FChars[i + Len];
  SetLength(FChars, System.Length(FChars) - Len);
  UpdateSum;
end;

procedure TColorString.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index >= System.Length(FChars)) then
    Exit;
  for i := Index to System.Length(FChars) - 2 do
    FChars[i] := FChars[i + 1];
  SetLength(FChars, System.Length(FChars) - 1);
  UpdateSum;
end;

function TColorString.GetPartString(Start, Stop: Integer): string;
var
  i, n: Integer;
  Len:  Integer;
begin
  if Start < 0 then
    Start := 0;
  if Stop > High(FChars) then
    Stop := High(FChars);
  Len    := 0;
  for i := Start to Stop do
    Inc(Len, System.Length(FChars[i].FChar));
  SetLength(Result, Len);
  Len := 1;
  for i := Start to Stop do
  begin
    with FChars[i] do
    begin
      for n := 1 to System.Length(FChar) do
      begin
        Result[Len] := FChar[n];
        Inc(Len);
      end;
    end;
  end;
end;

function TColorString.GetString: string;
var
  i, n: Integer;
  Len:  Integer;
begin
  Len := 0;
  for i := 0 to High(FChars) do
    Inc(Len, System.Length(FChars[i].FChar));
  SetLength(Result, Len);
  Len := 1;
  for i := 0 to High(FChars) do
  begin
    with FChars[i] do
    begin
      for n := 1 to System.Length(FChar) do
      begin
        Result[Len] := FChar[n];
        Inc(Len);
      end;
    end;
  end;
end;

procedure TColorString.OverWritePW(S: TColorString; PWS, Pos: Integer; PWC: string);
var
  i: Integer;
  CPassWordStart: Integer;
begin
  MinimumLength(Pos + S.Count, clLightgray, S.FDefaultBackGround);
  CPassWordStart := PWS;
  for i := 0 to S.Count- 1 do
  begin
    FChars[i + Pos] := S.FChars[i];
    if CPassWordStart <= 0 then
      FChars[i + Pos].FChar := PWC;
    Dec(CPassWordStart);
  end;
  UpdateSum;
end;

procedure TColorString.OverWrite(S: TColorString; Pos: Integer);
var
  i: Integer;
begin
  MinimumLength(Pos + S.Count, clLightgray, S.FDefaultBackGround);
  for i := 0 to S.Count- 1 do
    FChars[i + Pos] := S.FChars[i];
  UpdateSum;
end;

procedure TColorString.PartOverWrite(S: TColorString; Start, Stop, Pos: Integer);
var
  i: Integer;
begin
  MinimumLength(Pos + Stop - Start, clLightgray, S.FDefaultBackGround);
  for i := 0 to Stop - Start - 1 do
    FChars[i + Pos] := S.FChars[i + Start];
  UpdateSum;
end;

procedure TColorString.OverWrite(S: string; Pos: Integer; FC, BC: TColor; Attrib: TCharAttrib);
var
  i, Pp, l: Integer;
begin
  MinimumLength(Pos + UTF8Length(S), FC, BC);
  Pp := 1;
  for i := 0 to UTF8Length(S) - 1 do
  begin
    l := UTF8CodepointSize(@s[Pp]);
    with FChars[i + Pos] do
    begin
      FChar      := Copy(S, Pp, l);
      FFrontColor := FC;
      FBackColor := BC;
      FAttrib    := Attrib;
    end;
    Inc(Pp, l);
  end;
  UpdateSum;
end;

procedure TColorString.OverWriteChar(s: TUTF8Char; Pos, ADefWidth: Integer; FC, BC: TColor; Attrib: TCharAttrib);
begin
  MinimumLength(Pos + 1, FC, BC);
  with FChars[Pos] do
  begin
    FChar      := s;
    FFrontColor := FC;
    FBackColor := BC;
    FAttrib    := Attrib;
  end;
  UpdateSum;
end;

procedure TColorString.MinimumLength(V: Integer; FC, BC: TColor);
var
  OldLen, i: Integer;
begin
  if System.Length(FChars) < V then
  begin
    OldLen := System.Length(FChars);
    SetLength(FChars, V);
    for i := OldLen to High(FChars) do
    begin
      with FChars[i] do
      begin
        FChar      := ' ';
        FFrontColor := FC;
        FBackColor := BC;
      end;
    end;
  end;
end;

function TColorString.GetCharWidth: Integer;
begin
  Result := FConsole.CharWidth;
end;

function TColorString.GetTabWidth: Integer;
begin
  Result := FConsole.TabWidth;
end;

procedure TColorString.MaximumLength(V: Integer);
begin
  if System.Length(FChars) > V then
    SetLength(FChars, V);
end;

procedure TColorString.Clear;
begin
  FStoredLineCount:=-1;
  FChars := nil;
end;

procedure TTyroConsole.ClearLine;
begin
  if FLines[FOutY].Count <> 0 then
  begin
    FLines[FOutY] := nil;
    FOutX := 0;
    if FInput then
      FInputY := FOutY;
    Invalidate;
  end;
end;

{function TTyroConsole.GetCaretInterval: Integer;
begin
  Result := FCaretTimer.Interval;
end;

procedure TTyroConsole.SetCaretInterval(AValue: Integer);
begin
  FCaretTimer.Interval := AValue;
end;}

procedure TTyroConsole.MultiWrite;
var
  DoWrite: Boolean;
begin
  repeat
    DoWrite := FStringBuffer.Count <> 0;
    if DoWrite then
    begin
      FCurrentString := FStringBuffer[0];
      FStringBuffer.Delete(0);
    end;
    if DoWrite then
      IntWrite;
  until not DoWrite;
end;

procedure TTyroConsole.Write(s: string);
begin
  MultiWrite;
  FCurrentString := S;
  IntWrite;
end;

function TTyroConsole.HistoryIndexOf(s: string): Integer;
begin
  for Result := 0 to HistoryCount - 1 do
    if History[Result] = s then
      Exit;
  Result := -1;
end;

function TTyroConsole.HistoryHas(s: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to HistoryCount - 1 do
    if History[i] = s then
      Exit;
  Result := False;
end;

function TTyroConsole.HistoryCount: Integer;
begin
  HistoryCount := FHistoryLength - Ord(FInput);
end;

function TTyroConsole.GetHistory(i: Integer): string;
begin
  Inc(i, Ord(FInput));
  if (i >= 0) and (i < FHistoryLength) then
    GetHistory := FHistory[i].Getstring
  else
    GetHistory := '';
end;

procedure TTyroConsole.ClearHistory;
begin
  FHistoryLength := Ord(FInput);
  FHistoryPos    := 0;
end;

procedure TTyroConsole.SetHistoryMax(v: Integer);
var
  i: Integer;
begin
  if v < 1 then
    v := 1;
  if v <> FHistoryMax then
  begin
    if FHistoryLength > v then
      FHistoryLength := v;
    for i := v to FHistoryMax - 1 do
      FHistory[i].Free;
    FHistory.Count := v;
    for i := FHistoryMax to v - 1 do
      FHistory[i] := TColorString.Create(Self);
    FHistoryMax   := v;
  end;
end;

procedure TTyroConsole.LeftSelection(Start, Stop: Integer);
begin
  if FSelStart = -1 then
  begin
    SetSelection(Start, Stop);
  end
  else
  begin
    if FSelStart = Start then
      SetSelection(-1, 0)
    else
    begin
      if FSelStart < Start then
      begin
        SetSelection(FSelStart, Start);
      end
      else
        SetSelection(Start, FSelEnd + 1);
    end;
  end;
end;

procedure TTyroConsole.RightSelection(Start, Stop: Integer);
begin
  if FSelStart = -1 then
  begin
    SetSelection(Start, Stop);
  end
  else
  begin
    if FSelEnd + 1 = Stop then
      SetSelection(-1, 0)
    else
    begin
      if FSelstart < Start then
      begin
        SetSelection(FSelStart, Stop);
      end
      else
        SetSelection(Stop, FSelEnd + 1);
    end;
  end;
end;

procedure TTyroConsole.SetSelection(Start, Stop: Integer);
begin
  if FSelStart <> -1 then
    FInputBuffer.ColorBlock(FSelStart, FSelEnd, FInputColor, FInputBackGround);
  if Start = Stop then
    FSelStart := -1
  else
  begin
    if Start < Stop then
    begin
      FSelStart := Start;
      FSelEnd   := Stop - 1;
    end
    else
    begin
      FSelStart := Stop;
      FSelEnd   := Start - 1;
    end;
  end;
  if FSelStart <> -1 then
    FInputBuffer.ColorBlock(FSelStart, FSelEnd, FInputSelColor, FInputSelBackGround);
end;

procedure TTyroConsole.CopyToClipBoard;
begin
  if FSelStart <> -1 then
  begin
    //ClipBoard.AsText := FInputBuffer.GetPartstring(FSelStart, FSelEnd);
  end;
end;

procedure TTyroConsole.PasteFromClipBoard;
var
  s:     WideString;
  l, Pp: Integer;
begin
  {if ClipBoard.HasFormat(CF_TEXT) then
  begin
    s  := ClipBoard.AsText;
    Pp := 1;
    while pp <= Length(s) do
    begin
      l := UTF8CharacterLength(@S[Pp]);
      if (l = 1) and (byte(S[Pp]) < 32) then
        Delete(s, Pp, 1)
      else
        Inc(Pp, l);
    end;
    FInputBuffer.Insert(InputPos, s, FInputColor, FInputBackGround, FInputAttrib);
    Inc(FInputPos, UTF8Length(s));
    FCaretX := FInputX + InputPos;
    AdjustScrollBars;
    MakeInputVisible;
    FHistoryPos := 0;
    if Assigned(FOnInputChange) then
      FOnInputChange(Self, FInputBuffer);
    if Assigned(FOnAny) then
      FOnAny(Self, FInputBuffer);
  end;}
end;

procedure TTyroConsole.DeleteSelected;
begin
  if FSelStart <> -1 then
  begin
    FInputBuffer.Delete(FSelStart, FSelEnd - FSelStart + 1);
    FInputPos := FSelStart;
    FCaretX   := FInputX + FInputPos;
    FSelStart := -1;
  end;
end;

procedure TTyroConsole.CutToClipBoard;
begin
  if FSelStart <> -1 then
  begin
    //ClipBoard.AsText := FInputBuffer.GetPartstring(FSelStart, FSelEnd);
    DeleteSelected;
  end;
end;

procedure TTyroConsole.MouseMove(Shift: TShiftState; x, y: Integer);
begin
  if FMouseDown then
  begin
    if MoveInputCaretTo(x, y, False) then
      SetSelection(FMouseDownInputPos, FInputPos);
  end;
  inherited MouseMove(Shift,x,y);
end;

function TTyroConsole.MoveInputCaretTo(x, y: Integer; chl: Boolean): Boolean;
var
  h, sl, q: Integer;
begin
  if not FInput then
    Exit;
  y  := y div FCharHeight;
  h  := FLineHeightSum[FTopLine] + FLineOfTopLine + y;
  sl := FTopLine;
  while (sl < FLineCount - 1) and (FLineHeightSum[sl + 1] <= h) do
    Inc(sl);
  if (sl = FInputY) or (not chl) then
  begin
    Dec(h, FLineHeightSum[FInputY]);
    q := FInputBuffer.GetCharPosition(ClientWidth, h, x);
    if (q < FInputMinPos) then
      q := FInputMinPos;
    if (q - FInputX > FInputBuffer.Count) then
      q     := FInputBuffer.Count- FInputX;
    FCaretX := q;
    FInputPos := FCaretX - FInputX;
    if Assigned(FOnAny) then
      FOnAny(Self, FInputBuffer);
    Invalidate;
    Result := True;
  end
  else
    Result := False;
end;

procedure TTyroConsole.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
begin
  MoveInputCaretTo(x, y, True);
  FMouseDown := True;
  SetSelection(-1, 0);
  FMouseDownInputPos := FInputPos;
  Invalidate;
  inherited MouseDown(Button,Shift,x,y);
end;

procedure TTyroConsole.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
begin
  FMouseDown := False;
  inherited MouseUp(Button,Shift,x,y);
end;

destructor TColorString.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTyroConsole.ScrollUp;
begin
  FLines.Delete(0);
end;

procedure TTyroConsole.TextColors(FC, BC: TColor);
begin
  FCurrentColor      := FC;
  FCurrentBackGround := BC;
end;

procedure TTyroConsole.TextColor(C: TColor);
begin
  FCurrentColor := C;
end;

procedure TTyroConsole.TextBackground(C: TColor);
begin
  FCurrentBackGround := C;
end;

procedure TTyroConsole.TranslateScrollBarPosition;
var
  GLine, Line: Integer;
  He: Integer;
begin
  if (FLineOfTopLine < FLineHeights[FTopLine]) and
    (FLineHeightSum[FTopLine] + FLineOfTopLine = FVSBPos) then
    exit;
  UpdateLineHeights;
  Line  := 0;
  GLine := 0;
  He    := FLineHeights[Line];
  while (Line < LineCount - 1) and (Gline + He <= FVSBPos) do
  begin
    Inc(Line);
    Inc(Gline, He);
    He := FLineHeights[Line];
  end;
  FTopLine := Line;
  FLineOfTopLine := FVSBPos - GLine;
  Invalidate;
end;

procedure TTyroConsole.Scroll(Witch: TScrollbarType; ScrollCode: TScrollCode; Pos: Integer);
var
  CurrentPos: Integer;
begin
  CurrentPos := FLineHeightSum[FTopLine] + FLineOfTopLine;
  case ScrollCode of
    scrollTOP: CurrentPos    := 0;
    scrollBOTTOM: CurrentPos := FVisibleLineCount - FPageHeight;
    scrollLINEDOWN: Inc(CurrentPos);
    scrollLINEUP: Dec(CurrentPos);
    scrollPAGEDOWN: Inc(CurrentPos, FPageHeight);
    scrollPAGEUP: Dec(CurrentPos, FPageHeight);
    scrollTHUMBPOSITION: CurrentPos := Pos;
    scrollTHUMBTRACK: CurrentPos    := Pos;
    scrollENDSCROLL: Exit;
  end;

  if CurrentPos < 0 then
    CurrentPos := 0
  else if Currentpos > FVisibleLineCount - FPageHeight then
    CurrentPos := FVisibleLineCount - FPageHeight;
 {$IFNDEF LCLGTK}
  //ScrollBarPosition(sbtVertical, CurrentPos);
 {$ENDIF}

  FVSBPos := CurrentPos;
  TranslateScrollBarPosition;
end;

procedure TTyroConsole.ScrollBarRange(Which: TScrollbarType; aRange, aPage: Integer);
begin
  SetScrollRange(Which, 0, aRange, aPage);
end;

procedure TTyroConsole.ScrollBarPosition(Which: TScrollbarType; Value: Integer);
begin
  SetScrollPosition(Which, Value, FVSbVisible);
end;

function TTyroConsole.GetSystemMetricsGapSize(const Index: Integer): Integer;
begin
 {$ifdef LCLWIN32}
  Result := 0;
 {$else}
  Result := 3;
 {$endif}
end;

procedure TTyroConsole.SetBackGroundColor(c: Tcolor);
begin
  if c <> FBackGroundColor then
  begin
    FBackGroundColor := c;
    Invalidate;
  end;
end;

// Still a Bug: Try having a cmdline with more lines than fit on screen : update doesn't work anymore...

procedure TTyroConsole.MakeInputVisible;
var
  y: Integer;
begin
  if not FAutoFollow then
  begin
    Exit;
  end;
  UpdateLineHeights;
  y := FLineHeightSum[FInputY] + FInputBuffer.GetLineOfCaret(ClientWidth, FCaretX, FCaretWidth);
  if y >= FLineHeightSum[FTopLine] + FLineOfTopLine + FPageHeight - 1 then
  begin
    while y >= FLineHeightSum[FTopLine] + FLineHeights[FTopLine] + FPageHeight - 1 do
    begin
      Inc(FTopLine);
    end;
    FLineOfTopLine := y - (FLineHeightSum[FTopLine] + FPageHeight) + 1;
  end
  else if y < FLineHeightSum[FTopLine] + FLineOfTopLine then
  begin
    FLineOfTopLine := 0;
    while y < FLineHeightSum[FTopLine] do
    begin
      Dec(FTopLine);
    end;
    FLineOfTopLine := y - FLineHeightSum[FTopLine];
  end;
  y := FLineHeightSUm[FTopLine] + FLineOfTopLine;
  if y <> FVSBPos then
  begin
    FVSBPos := y;
    ScrollBarPosition(sbtVertical, y);
  end;
end;

procedure TTyroConsole.MakeOutVisible;
var
  y: Integer;
begin
  if not FAutoFollow then
    Exit;
  UpdateLineHeights;
  y := FLineHeightSum[FOutY] + FLines.Require[FOutY].GetLineOfCaret(ClientWidth,
    FOutX, FCaretWidth);
  if y >= FLineHeightSum[FTopLine] + FLineOfTopLine + FPageHeight then
  begin
    while y >= FLineHeightSum[FTopLine] + FLineHeights[FTopLine] + FPageHeight - 1 do
      Inc(FTopLine);
    FLineOfTopLine := y - (FLineHeightSum[FTopLine] + FPageHeight) + 1;
  end
  else if y < FLineHeightSum[FTopLine] + FLineOfTopLine then
  begin
    FLineOfTopLine := 0;
    while y < FLineHeightSum[FTopLine] do
      Dec(FTopLine);
    FLineOfTopLine := y - FLineHeightSum[FTopLine];
  end;
  y := FLineHeightSUm[FTopLine] + FLineOfTopLine;
  if y <> FVSBPos then
  begin
    FVSBPos := y;
    ScrollBarPosition(sbtVertical, y);
  end;
end;

procedure TTyroConsole.SetHistoryPos(v: Integer);
begin
  if FInputIsPassWord then
    Exit;
  if v < 0 then
    v := FHistoryLength - 1
  else if v >= FHistoryLength then
    v := 0;
  if v <> FHistoryPos then
  begin
    if FHistoryPos = 0 then
    begin
      FHistory[0].Clear;
      FHistory[0].PartOverWrite(FInputBuffer, FInputMinPos, FInputBuffer.Count, 0);
    end;
    FInputBuffer.MaximumLength(FInputMinPos + FHistory[v].Count);
    FInputBuffer.OverWrite(FHistory[v], FInputMinPos);
    FInputPos := FInputBuffer.Count;
    FCaretX   := FInputX + FInputPos;
    FHistoryPos := v;
  end;
  if Assigned(FOnInputChange) then
    FOnInputChange(Self, FInputBuffer);
  MakeInputVisible;
  AdjustLineHeight(FInputY);
  AdjustScrollBars;
  Invalidate;
end;

procedure TTyroConsole.KeyPress(var Key: TUTF8Char);
begin
  if not FInput then
    Exit;
  if key >= #32 then
  begin
    if FSelStart <> -1 then
      DeleteSelected;
    FInputBuffer.Insert(FInputPos, key, FInputColor, FInputBackGround, FCurrentAttrib);
    Inc(FInputPos);
    FCaretX     := FInputX + FInputPos;
    FHistoryPos := 0;
    if assigned(FOnInputChange) then
      FOnInputChange(Self, FInputBuffer);
  end;
  if Assigned(OnAny) then
    OnAny(Self, FInputBuffer);
  AdjustScrollBars;
  MakeInputVisible;
  if FInputVisible then
    Invalidate;
  inherited;
end;

procedure TTyroConsole.KeyUp(var Key: TKeyboardKey; Shift: TShiftState);
begin
  inherited KeyUp(key, shift);
  key:= KEY_NULL;
end;

procedure TTyroConsole.KeyDown(var Key: TKeyboardKey; Shift: TShiftState);
var
  s: string;
  i: Integer;
begin
  if not FInput then
    Exit;
  case Key of
    Key_END:
    begin
      key := KEY_NULL;
      if (not (ssAlt in Shift)) and FInput and (FInputPos <> FInputBuffer.Count) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          RightSelection(FInputPos, FInputBuffer.Count);
        FInputPos := FInputBuffer.Count;
        FCaretX   := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    Key_HOME:
    begin
      key := KEY_NULL;
      if (not (ssAlt in Shift)) and FInput and (FInputPos <> FInputMinPos) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          LeftSelection(FInputMinPos, FInputPos);
        FInputPos := FInputMinPos;
        FCaretX   := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    Key_LEFT:
    begin
      key:=KEY_NULL;
      if (not (ssAlt in Shift)) and (FInput and (FInputPos > FInputMinPos)) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          LeftSelection(FInputPos - 1, FInputPos);
        Dec(FInputPos);
        FCaretX := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    Key_UP:
    begin
      key:=KEY_NULL;
      if (not (ssAlt in Shift)) and FInput then
      begin
        SetSelection(-1, 0);
        SetHistoryPos(FHistoryPos + 1);
      end;
    end;
    Key_DOWN:
    begin
      key:=KEY_NULL;
      if (not (ssAlt in Shift)) and FInput then
      begin
        SetSelection(-1, 0);
        SetHistoryPos(FHistoryPos - 1);
      end;
    end;
    Key_RIGHT:
    begin
      key:=KEY_NULL;
      if (not (ssAlt in Shift)) and FInput and (FInputPos < FInputBuffer.Count) then
      begin
        if not (ssShift in Shift) then
          SetSelection(-1, 0)
        else
          RightSelection(FInputPos, FInputPos + 1);
        Inc(FInputPos);
        FCaretX := FInputX + FInputPos;
        MakeInputVisible;
        Invalidate;
      end;
    end;
    Key_DELETE:
    begin
      if FInput then
      begin
        if FSelStart <> -1 then
          DeleteSelected
        else
          FInputBuffer.Delete(FInputPos);
        FHistoryPos := 0;
        if assigned(FOnInputChange) then
          FOnInputChange(Self, FInputBuffer);
        MakeInputVisible;
        AdjustLineHeight(FInputY);
        AdjustScrollBars;
      end;
    end;
    KEY_ENTER:
    begin
      if FInput then
      begin
        s := FInputBuffer.GetString;
        s := Copy(s, FUTF8InputMinPos + 1, Length(s));
        if (FHistoryPos = 0) then
        begin
          if (FInputBuffer.Count = FInputMinPos) or FInputIsPassWord then
          begin
            DeleteHistoryEntry(0);
          end
          else
          begin
            i := HistoryIndexOf(s);
            if i >= 0 then
            begin
              DeleteHistoryEntry(0);
              MakeFirstHistoryEntry(i);
            end
            else
            begin
              FHistory[0].Clear;
              FHistory[0].PartOverWrite(FInputBuffer, FInputMinPos,
                FInputBuffer.Count , 0);
            end;
          end;
        end
        else
        begin
          DeleteHistoryEntry(0);
          MakeFirstHistoryEntry(FHistoryPos);
        end;
        FInput := False;
        if FWriteInput then
        begin
          if FLines[FOutY].Count <> 0 then
          begin
            if FOutY >= FLineCount - 1 then
            begin
              ScrollUp;
              Dec(FOutY);
              FInputY := FOutY;
              AdjustLineHeight(FOutY);
              UpdateLineHeights;
              TranslateScrollBarPosition;
            end;
            FLines[FOutY + 1].Clear;
            FLines[FOutY + 1].OverWrite(FLines[FOutY], 0);
            FLines[FOutY].Clear;
            if FInputIsPassWord then
              FLines[FOutY].OverWritePW(FInputBuffer, FInputMinPos, FInputX, FPassWordChar)
            else
              FLines[FOutY].OverWrite(FInputBuffer, FInputX);
          end
          else
          begin
            if FInputIsPassWord then
              FLines[FOutY].OverWritePW(FInputBuffer, FInputMinPos, FInputX, FPassWordChar)
            else
              FLines[FOutY].OverWrite(FInputBuffer, FInputX);
          end;
          Inc(FOutY);
          if FOutY >= FLineCount then
          begin
            ScrollUp;
            Dec(FOutY);
            FInputY := FOutY;
            AdjustLineHeight(FOutY);
            UpdateLineHeights;
            TranslateScrollBarPosition;
          end;
          FOutX   := 0;
          FCaretX := 0;
        end;
        FInputBuffer.Clear;
        if Assigned(OnInput) then
          OnInput(Self, s);
        if Assigned(OnAny) then
          OnAny(Self, FInputBuffer);
        AdjustScrollBars;
        Invalidate;
      end;
    end;
    KEY_BACKSPACE:
    begin
      if FInput then
      begin
        if FSelStart <> -1 then
          DeleteSelected
        else
        begin
          if (FInputPos > FInputMinPos) then
          begin
            Dec(FInputPos);
            FInputBuffer.Delete(FInputPos);
            FCaretX := FInputX + FInputPos;
          end;
        end;
        FHistoryPos := 0;
        if assigned(FOnInputChange) then
          FOnInputChange(Self, FInputBuffer);
        if Assigned(OnAny) then
          OnAny(Self, FInputBuffer);
        AdjustScrollBars;
        MakeInputVisible;
        if FInputVisible then
          Invalidate;
      end;
    end;
    Key_C:
    begin
      if (FInput) and (ssCtrl in Shift) then
        CopyToClipBoard;
    end;
    Key_V:
    begin
      if (FInput) and (ssCtrl in Shift) then
        PasteFromClipBoard;
    end;
    Key_X:
    begin
      if (FInput) and (ssCtrl in Shift) then
        CutToClipBoard;
    end;
    Key_A:
    begin
      if (FInput) and (ssCtrl in Shift) then
      begin
        SetSelection(FInputMinPos, FInputBuffer.Count);
        FInputPos := FInputBuffer.Count;
        MakeInputVisible;
        if FInputVisible then
          Invalidate;
      end;
    end;
  end;
  if Assigned(OnAny) then
    OnAny(Self, FInputBuffer);
  inherited KeyDown(Key,Shift);
end;

procedure TTyroConsole.InsertHistory;
var
  i: Integer;
  t: TColorString;
begin
  t := FHistory[FHistoryMax - 1];
  for i := FHistoryMax - 2 downto 0 do
  begin
    FHistory[i + 1] := FHistory[i];
  end;
  FHistory[0] := t;
  FHistoryPos := 0;
  if FHistoryLength < FHistoryMax then
    Inc(FHistoryLength);
end;

procedure TTyroConsole.StartRead(DFC, DBC: TColor; const Desc: string; IFC, IBC: TColor);
var
  Pp, i, l: Integer;
begin
  Inc(FCaretX, UTF8Length(Desc));
  FInputX := 0;
  if FLines[FOutY].Count = 0 then
    FInputY := FOutY
  else
    FInputY := FOutY + 1;
  FInputVisible := True;
  FInput := True;
  FUTF8InputMinPos := Length(Desc);
  i      := 0;
  Pp     := 1;
  while Pp <= Length(Desc) do
  begin
    if Desc[Pp] = #27 then
    begin
      if Pp + 1 > Length(Desc) then
        Break;
      case Desc[Pp + 1] of
        #9, #10, #32, #46, #196:
        begin
          if Pp + 2 > Length(Desc) then
            Break; //Incomplete Escape Seq...ignore
          l := 3;
        end;
        #33, #47, #197:
        begin
          if Pp + 3 > Length(Desc) then
            Break; //Incomplete Escape Seq...ignore
          l := 4;
        end;
        else
        begin
          l := 2;
        end;
      end;
    end
    else
      l := UTF8CodepointSize(@Desc[PP]);
    FInputBuffer.OverWrite(Copy(Desc, Pp, l), i, DFC, DBC, FCurrentAttrib);
    Inc(i);
    Inc(Pp, l);
  end;
  FInputPos    := i;
  FInputMinPos := i;
  // FInputBuffer.OverWrite(Desc,0,DFC,DBC);
  FInputIsPassWord := False;
  FInputColor  := IFC;
  FInputBackground := IBC;
  FInputBuffer.PassWordStart := MaxInt;
  InsertHistory;
  MakeInputVisible;
end;

procedure TTyroConsole.StartReadPassWord(DFC, DBC: TColor; const Desc: string;
  IFC, IBC: TColor);
begin
  StartRead(DFC, DBC, Desc, IFC, IBC);
  FInputBuffer.PassWordStart := UTF8Length(Desc);
  FInputBuffer.PassWordChar := FPassWordChar;
  FInputIsPassWord := True;
end;

procedure TTyroConsole.StopRead;
begin
  FInput := False;
end;

procedure TTyroConsole.DeleteHistoryEntry(i: Integer);
var
  j:    Integer;
  Temp: TColorString;
begin
  Temp := FHistory[i];
  for j := i to FHistoryLength - 2 do
    FHistory[j] := FHistory[j + 1];
  FHistory[FHistoryLength - 1] := Temp;
  Dec(FHistoryLength);
  if FHistoryPos >= i then
    Dec(FHistoryPos);
end;

procedure TTyroConsole.MakeFirstHistoryEntry(i: Integer);
var
  Temp: TColorString;
begin
  if FHistoryPos <> 0 then
  begin
    Temp := FHistory[i];
    for i := i - 1 downto 0 do
      FHistory[i + 1] := FHistory[i];
    FHistory[0] := Temp;
  end;
end;

procedure TTyroConsole.Clear;
var
  i: Integer;
begin
  Flines.Clear;
  FCaretX := 0;
  FInputY := 0;
  FOutX   := 0;
  FOutY   := 0;
  if FInput then
    FInputY := 0;
  Invalidate;
end;

procedure TTyroConsole.Writeln(s: string);
begin
  Write(s + #13#10);
end;

procedure TTyroConsole.IntWrite;
var
  Pp:     Integer;
  l:      Integer;
  s:      string;
  EscPos: Integer;
  EscSubMode: Integer;
begin
  S    := FCurrentString;
  Pp   := 1;
  while Pp <= Length(S) do
  begin
    l := 1;
    case FEscapeMode of
      escmNone:
      begin
        if S[Pp] = #27 then
        begin
          case FEscapeCodeType of
            esctConsole:
            begin
              FEscapeMode := escmOperation;
              FEscapeData := '';
            end;
            esctAnsi:
            begin
              FEscapeMode := escmAnsiOperation;
              FEscapeData := '';
            end;
            esctNone:
            begin
              // Simply ignore it
            end;
          end;
        end
        else
        begin
          l := UTF8CodepointSize(@S[Pp]);
          if l = 1 then
          begin
            case s[Pp] of
              #13: FOutX := 0;
              #10:
              begin
                AdjustLineHeight(FOutY);
                if FLines.Require[FOutY].Count = 0 then
                  FLines.Require[FOutY].DefaultBackGround := FCurrentBackGround;
                Inc(FOutY);
                if FOutY >= FLineCount then
                begin
                  ScrollUp;
                  Dec(FOutY);
                  AdjustLineHeight(FOutY);
                  UpdateLineHeights;
                  TranslateScrollBarPosition;
                end;
              end;
              else
              begin
                FLines.Require[FOutY].OverWrite(s[Pp], FOutX, FCurrentColor, FCurrentBackGround,
                  FCurrentAttrib);
                Inc(FOutX);
              end;
            end;
          end
          else
          begin
            FLines[FOutY].OverWrite(Copy(s, Pp, l), FOutX, FCurrentColor,
              FCurrentBackGround, FCurrentAttrib);
            Inc(FOutX);
          end;
        end;
      end;
      escmOperation:
      begin
        case S[Pp] of
          #9, #10, #32, #46, #196:
          begin
            FEscapeData := S[Pp];
            FEscapeMode := escmData1;
          end;
          #33, #47, #197:
          begin
            FEscapeData := S[Pp];
            FEscapeMode := escmData2;
          end;
          else
          begin
            FLines[FOutY].OverWriteChar(#27 + S[Pp], FOutX, FGraphicCharWidth,
              FCurrentColor, FCurrentBackGround, FCurrentAttrib);
            Inc(FOutX);
            FEscapeMode := escmNone;
          end;
        end;
      end;
      escmData1:
      begin
        FLines[FOutY].OverWriteChar(#27 + FEscapeData + S[Pp], FOutX, FGraphicCharWidth,
          FCurrentColor, FCurrentBackGround, FCurrentAttrib);
        Inc(FOutX);
        FEscapeMode := escmNone;
      end;
      escmData2:
      begin
        FEscapeData := FEscapeData + S[Pp];
        FEscapeMode := escmData1;
      end;
      escmAnsiOperation:
      begin
        case S[Pp] of
          '[': FEscapeMode := escmAnsiSquare;
          else
            FEscapeMode := escmNone;
        end;
      end;
      escmAnsiSquare:
      begin
        case S[Pp] of
          'm':
          begin
            EscPos     := 1;
            EscSubMode := 0;
            while EscPos <= Length(FEscapeData) do
            begin
              case EscSubMode of
                0:
                begin
                  case FEscapeData[EscPos] of
                    '0':
                    begin
                      // No Reset Values know here...just assume
                      FCurrentColor      := clLightgray;
                      FCurrentBackGround := clBlack;
                    end;
                    '7':
                    begin
                      // Reverse? What now...
                    end;
                    '3': EscSubMode := 3;
                    '4': EscSubMode := 4;
                  end;
                end;
                1:
                begin
                  // Just collect the expected ";", not sure what to do if it isn't there...
                  EscSubMode := 0;
                end;
                3:
                begin
                  if FEscapeData[EscPos] in ['0'..'7'] then
                    FCurrentColor := AnsiColors[StrToInt(FEscapeData[EscPos])];
                  EscSubMode      := 1;
                end;
                4:
                begin
                  if FEscapeData[EscPos] in ['0'..'7'] then
                    FCurrentBackGround := AnsiColors[StrToInt(FEscapeData[EscPos])];
                  EscSubMode := 1;
                end;
              end;
              Inc(EscPos);
            end;
            FEscapeMode := escmNone;
          end;
          else
          begin
            FEscapeData := FEscapeData + S[Pp];
          end;
        end;
      end;
    end;
    Inc(Pp, l);
  end;
  if FInput then
  begin
    if FLines[FOutY].Count = 0 then
    begin
      if (FInputY <> FOutY) then
        FInputY := FOutY;
    end
    else
    begin
      if FInputY <> FOutY + 1 then
        FInputY := FOutY + 1;
    end;
    if FInputY >= FLineCount then
    begin
      ScrollUp;
      Dec(FOutY);
      Dec(FInputY);
      FInputY := FOutY;
      AdjustLineHeight(FOutY);
      UpdateLineHeights;
      TranslateScrollBarPosition;
    end;
    MakeInputVisible;
  end
  else
    MakeOutVisible;
  AdjustLineHeight(FOutY);
  if not FInput then
    FCaretX := FOutX;
  AdjustScrollBars;
end;

procedure TTyroConsole.SetOutY(v: Integer);
begin
  if v > FLineCount - 1 then
    v   := FLineCount - 1;
  FOutY := v;
end;

procedure TTyroConsole.Resize;
begin
  inherited Resize;
  AdjustScrollBars(True);
end;

function TTyroConsole.AdjustLineHeight(i: Integer; const Recalc: Boolean): Integer;
var
  LineC:  Integer;
  LineC2: Integer;
begin
  if (i < FLines.Count) and (FLines[i] <> nil) then
  begin
    with FLines[i] do
    begin
      if (not Recalc) and (FStoredLineCount>=0) then
        LineC := FStoredLineCount
      else
      begin
        LineC := LineCount(ClientWidth, -1, FCaretWidth);
        FStoredLineCount:=LineC;
      end;
    end;
  end
  else
    LineC := 0;

  if (FInputY = i) then
  begin
    with FInputBuffer do
    begin
      if (not Recalc) and (FStoredLineCount>=0) then
        LineC2 := FStoredLineCount
      else
      begin
        LineC2 := LineCount(ClientWidth, FCaretX, FCaretWidth);
        FStoredLineCount:=LineC2;
      end;
    end;

    if LineC2 > LineC then
      LineC := LineC2;
  end;
  Result  := LineC;
  FLineHeights[i] := Result;
end;

function TTyroConsole.UpdateLineHeights(const Recalc:Boolean=False): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FLineCount - 1 do
  begin
    FLineHeightSum[i] := Result;
    Inc(Result, AdjustLineHeight(i, Recalc));
  end;
end;

procedure TTyroConsole.AdjustScrollBars(const Recalc: Boolean);
var
  LH: Integer;
begin
  FPageHeight   := ClientHeight div FCharHeight;
  FVisibleLines := FPageHeight + Ord(ClientHeight mod FCharHeight <> 0);
  LH            := UpdateLineHeights(Recalc);
  if LH <> FVisibleLineCount then
  begin
    FVisibleLineCount := LH;
    if FVisibleLineCount <= FVSBPos + FPageHeight then
    begin
      FVSBPos := FVisibleLineCount - FPageHeight;
      if FVSBPos < 0 then FVSBPos := 0;
      ScrollBarPosition(sbtVertical, FVSBPos);
      TranslateScrollBarPosition;
    end;
  end;
  if FVisibleLineCount < FPageHeight then
  begin
    ScrollBarPosition(sbtVertical, 0);
    ScrollBarRange(sbtVertical, 0, FPageHeight);
    ShowScrollBar([sbtVertical], True); { Disable the Scrollbar ! }
  end
  else
  begin
    ScrollBarRange(sbtVertical, FVisibleLineCount, FPageHeight);
    ShowScrollBar([sbtVertical], True);
  end;
  Invalidate;
end;

procedure TTyroConsole.SetTopLine(Nr: Integer);
begin
  if Nr <> FTopLine then
  begin
    FTopLine := Nr;
    AdjustScrollBars;
  end;
end;

procedure TTyroConsole.SetLineCount(c: Integer);
var
  i: Integer;
begin
  if c < 1 then
    c := 1;
  if c <> FLineCount then
  begin
    for i := 0 to FLineCount - 1 do
      FLines[i].Free;
    FLineCount := c;
    SetLength(FLineHeights, FLineCount);
    SetLength(FLineHeightSum, FLineCount);
    AdjustScrollBars;
  end;
end;

procedure TTyroConsole.DoPaint(ACanvas: TTyroCanvas);
var
  y, t : Integer;
  m : Integer;
  CurrentLine : Integer;
begin
  inherited;
  with ACanvas do
  begin
    m := FVisibleLines - 1;
    y := -FLineOfTopLine;
    CurrentLine := FTopLine;
    while (y <= m) and (CurrentLine < FLines.Count){ and (CurrentLine < Length(FLines))} do
    begin
      FLines[CurrentLine].LineOutAndFill(ACanvas, 0, y * FCharHeight, 0,
        ClientWidth, FCharHeight, FGraphicCharWidth, -1, FBackGroundColor, FCaretColor,
        FCaretHeight, FCaretWidth, FCaretYShift, False);
      if (FInput) and (FInputY = CurrentLine) then
      begin
        if FInputIsPassWord then
        begin
          FInputBuffer.LineOutAndFill(ACanvas, 0, y * FCharHeight, 0, ClientWidth,
            FCharHeight, FGraphicCharWidth, FCaretX, FBackGroundColor, FCaretColor,
            FCaretHeight, FCaretWidth, FCaretYShift, FCaretVisible and Focused);
        end
        else
        begin
          FInputBuffer.LineOutAndFill(ACanvas, 0, y * FCharHeight, 0, ClientWidth,
            FCharHeight, FGraphicCharWidth, FCaretX, FBackGroundColor, FCaretColor,
            FCaretHeight, FCaretWidth, FCaretYShift, FCaretVisible and Focused);
        end;
      end;
      Inc(y, FLineHeights[CurrentLine]);
      Inc(CurrentLine);
    end;
    y := y * FCharHeight;
    if y < ClientHeight then
    begin
      ACanvas.DrawRect(0, y, ClientWidth, ClientHeight, FBackGroundColor, True);
    end;
  end;
end;

procedure TTyroConsole.CaretTimerExecute(Sender: TObject);
begin
  if Focused then
  begin
    if not Assigned(WakeMainThread) then
      MultiWrite;
    FCaretVisible := not FCaretVisible;
    Invalidate;
  end;
end;

constructor TTyroConsole.Create(AParent: TTyroContainer);
var
  i: Integer;
begin
  inherited;
  FStringBuffer     := TStringList.Create;
  FCharHeight := 8;
  FCharWidth := 8;
  FSelStart         := -1;
  FLineCount        := 1000;
  FInputVisible     := False;
  FWriteInput       := True;
  FBackGroundColor  := clBlack;
  FGraphicCharWidth := 10;
  FInputBuffer      := TColorString.Create(Self);
  FEscapeCodeType   := esctConsole;
  FAutoFollow       := True;
  FLines := TColorStrings.Create(Self);// FLineCount;

  SetLength(FLineHeights, FLineCount);
  SetLength(FLineHeightSum, FLineCount);
  FTabWidth := 60;
{  FCaretTimer          := TTimer.Create(self);
  FCaretTimer.Interval := 500;
  FCaretTimer.OnTimer  := @carettimerexecute;
  FCaretTimer.Enabled  := True;}
  FCaretVisible        := True;
  FVSBVisible          := True;
  FCurrentColor        := clLightgray;
  FCurrentBackground   := clBlack;
  FCaretColor          := clWhite;
  FCaretType           := cartLine;
  FCaretWidth          := 1;
  FCaretHeight         := -1;
  FCaretYShift         := 3;
  FInputSelBackground  := clWhite;
  FInputSelColor       := clBlue;
  FHistoryMax          := 10;
  FHistoryLength       := 0;
  SetWindowBounds(0, 0, 200, 200);
  FHistory := TColorStrings.Create(Self);

  if FCaretHeight = -1 then
    FCaretHeight := FCharHeight;
  AdjustScrollBars;

  for i:=0 to FLines.Count-1 do
  begin
    FLines[i].UpdateAll;
  end;
  FInputBuffer.UpdateAll;
  Invalidate;
end;

destructor TTyroConsole.Destroy;
var i : Integer;
begin
  //FCaretTimer.Enabled := False;
  FStringBuffer.Free;
  FreeAndNil(FLines);
  FreeAndNil(FHistory);
  FInputBuffer.Free;
  inherited Destroy;
end;

procedure InitColors;
begin
  AnsiColors[0] := clBlack;
  AnsiColors[1] := clRed;
  AnsiColors[2] := clGreen;
  AnsiColors[3] := clYellow;
  AnsiColors[4] := clBlue;
  AnsiColors[5] := clPurple;//Fuchsia;
  AnsiColors[6] := clSkyBlue;//Aqua;
  AnsiColors[7] := clWhite;
end;

initialization
  InitColors;
end.

