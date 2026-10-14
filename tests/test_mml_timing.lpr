program test_mml_timing;

{ Headless test for the MML note durations in tyrolib\Melodies.pas.

  It drives the real TMelodyChannel parser (a descendant captures every
  SetSound call) and compares each note/rest duration against an independent
  music-theory calculation:

      seconds = (60 / tempo) * (4 / noteValue) * dotFactor

  where noteValue is the number written after the note (4 = quarter note,
  8 = eighth, 1 = whole, ...) and dotFactor is 1 + 1/2 + 1/4 + ... for dots.
  A quarter note at 120 BPM must be exactly 0.5s, a whole note at 60 BPM
  exactly 4s, and so on.

  The second half extracts the real 'WilliamTell' MML from demos\music.tyro
  and reports the parsed total against the theory total, listing any note
  whose parsed duration does not match.

  Build and run:
    lazbuild --build-mode=Debug tests\test_mml_timing.lpi
    bin\test_mml_timing.exe

  Exit status: 0 when everything matched, otherwise the number of mismatches. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}
{$M+}

uses
  SysUtils, Classes, StrUtils, Math,
  Melodies;

type
  TSoundEvent = record
    Freq: Single;
    Duration: Single; // the sounding part
    Rest: Single;     // the silent gap after it
  end;

  { TProbeChannel - a real TMelodyChannel that records every SetSound call. }
  TProbeChannel = class(TMelodyChannel)
  public
    Events: array of TSoundEvent;
    procedure SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single); override;
    function ParseNext: Boolean;
    procedure ResetEvents;
  end;

procedure TProbeChannel.SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single);
var
  k: Integer;
begin
  k := Length(Events);
  SetLength(Events, k + 1);
  Events[k].Freq := Frequency;
  Events[k].Duration := Duration;
  Events[k].Rest := Rest;
  inherited;
end;

function TProbeChannel.ParseNext: Boolean;
begin
  Result := Next;
end;

procedure TProbeChannel.ResetEvents;
begin
  SetLength(Events, 0);
end;

var
  Failures: Integer;

procedure Fail(const Msg: string);
begin
  Inc(Failures);
  WriteLn('FAIL: ', Msg);
end;

// Parse an MML string with the real parser. Returns the captured events.
function ParseActual(const MML: string): TArray<TSoundEvent>;
var
  Ch: TProbeChannel;
begin
  Ch := TProbeChannel.Create(nil);
  try
    Ch.ResetEvents;
    Ch.Notes := MML;
    Ch.Prepare;
    while Ch.ParseNext do
      ;
    Result := Copy(Ch.Events);
  finally
    Ch.Free;
  end;
end;

// Total scheduled seconds of one event (sound + trailing gap).
function EventTotal(const E: TSoundEvent): Double;
begin
  Result := E.Duration + E.Rest;
end;

{ --- independent reference parser ------------------------------------------ }

function ReadNum(const S: string; var i: Integer; const Def: Double): Double;
var
  j: Integer;
  tmp: string;
begin
  j := i;
  tmp := '';
  if (j <= Length(S)) and ((S[j] = '-') or (S[j] = '+')) then
  begin
    tmp := S[j];
    Inc(j);
  end;
  while (j <= Length(S)) and (S[j] >= '0') and (S[j] <= '9') do
  begin
    tmp := tmp + S[j];
    Inc(j);
  end;
  if (tmp = '') or (tmp = '-') or (tmp = '+') then
    Result := Def
  else
    Result := StrToFloat(tmp);
  i := j;
end;

// Reference: list of scheduled seconds for every note/rest, derived from
// music theory only, not from the implementation.
function RefParse(const S: string): TArray<Double>;
var
  i: Integer;
  tempo, noteLen, dur, dotAdd, by: Double;
  list: array of Double;

  procedure Add(v: Double);
  begin
    SetLength(list, Length(list) + 1);
    list[High(list)] := v;
  end;

begin
  tempo := 120;
  noteLen := 4;
  i := 1;
  while i <= Length(S) do
  begin
    if (S[i] = ' ') or (S[i] = #9) or (S[i] = #10) or (S[i] = #13) then
      Inc(i)
    else if S[i] = '#' then
    begin
      while (i <= Length(S)) and (S[i] <> #13) and (S[i] <> #10) do
        Inc(i);
    end
    else if S[i] = '!' then
    begin
      tempo := 120;
      noteLen := 4;
      Inc(i);
    end
    else if (S[i] >= 'a') and (S[i] <= 'g') then
    begin
      Inc(i);
      if (i <= Length(S)) and (S[i] = '#') then
        Inc(i);
      if (i <= Length(S)) and ((S[i] = '+') or (S[i] = '-')) then
        Inc(i);
      dur := ReadNum(S, i, noteLen);
      if (i <= Length(S)) and (S[i] = ',') then
      begin
        Inc(i);
        ReadNum(S, i, 0);
      end;
      dotAdd := 0;
      by := 0.5;
      while (i <= Length(S)) and (S[i] = '.') do
      begin
        dotAdd := dotAdd + by;
        by := by / 2;
        Inc(i);
      end;
      if (i <= Length(S)) and (S[i] = '&') then
        Inc(i);
      Add((60 / tempo) * (4 / dur) * (1 + dotAdd));
    end
    else if (S[i] = 'p') or (S[i] = 'r') then
    begin
      Inc(i);
      dur := ReadNum(S, i, noteLen);
      dotAdd := 0;
      by := 0.5;
      while (i <= Length(S)) and (S[i] = '.') do
      begin
        dotAdd := dotAdd + by;
        by := by / 2;
        Inc(i);
      end;
      if (i <= Length(S)) and (S[i] = '&') then
        Inc(i);
      Add((60 / tempo) * (4 / dur) * (1 + dotAdd));
    end
    else if S[i] = 'l' then
    begin
      Inc(i);
      dur := ReadNum(S, i, 0);
      dotAdd := 0;
      by := 0.5;
      while (i <= Length(S)) and (S[i] = '.') do
      begin
        dotAdd := dotAdd + by;
        by := by / 2;
        Inc(i);
      end;
      noteLen := dur + dotAdd;
    end
    else if S[i] = 't' then
    begin
      Inc(i);
      tempo := ReadNum(S, i, tempo);
    end
    else if S[i] = 'o' then
    begin
      Inc(i);
      ReadNum(S, i, 0);
    end
    else if (S[i] = '<') or (S[i] = '>') then
    begin
      Inc(i);
      if (i <= Length(S)) and (S[i] >= '0') and (S[i] <= '9') then
        Inc(i);
    end
    else if S[i] = 's' then
    begin
      Inc(i);
      ReadNum(S, i, -1);
    end
    else if S[i] = 'v' then
    begin
      Inc(i);
      ReadNum(S, i, 0);
    end
    else if S[i] = 'i' then
    begin
      Inc(i);
      if (i <= Length(S)) and (S[i] = '[') then
        while (i <= Length(S)) and (S[i] <> ']') do
          Inc(i);
      if (i <= Length(S)) and (S[i] = ']') then
        Inc(i);
    end
    else if S[i] = 'm' then
    begin
      Inc(i);
      if i <= Length(S) then
      begin
        case S[i] of
          'r':
            begin
              tempo := 120;
              noteLen := 4;
            end;
          'x':
            begin
              Inc(i);
              Break;
            end;
        end;
        Inc(i);
      end;
    end
    else if S[i] = 'n' then
    begin
      Inc(i);
      ReadNum(S, i, -1);
      Add((60 / tempo) * (4 / noteLen));
    end
    else if S[i] = 'q' then
    begin
      Inc(i);
      ReadNum(S, i, -1);
      Add((60 / tempo) * (4 / noteLen));
    end
    else if S[i] = ';' then
    begin
      Inc(i);
      Break;
    end
    else
      Inc(i); // unknown, skip
  end;
  Result := list;
end;

{ --- helpers ---------------------------------------------------------------- }

function LoadText(const FileName: string): string;
var
  fs: TFileStream;
  n: Int64;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    n := fs.Size;
    SetLength(Result, n);
    if n > 0 then
      fs.ReadBuffer(Result[1], n);
  finally
    fs.Free;
  end;
end;

// Pull the text between [[ and ]] that follows 'Name ='
function ExtractSong(const Text, Name: string): string;
var
  p, q: Integer;
begin
  Result := '';
  p := Pos(Name, Text);
  if p = 0 then
    Exit;
  p := PosEx('[[', Text, p);
  if p = 0 then
    Exit;
  p := p + 2;
  q := PosEx(']]', Text, p);
  if q = 0 then
    Exit;
  Result := Copy(Text, p, q - p);
end;

function FindDemoFile: string;
var
  Dir: string;
  cand: array[0..3] of string;
  k: Integer;
begin
  Dir := ExtractFilePath(ParamStr(0));
  cand[0] := Dir + 'demos\music.tyro';
  cand[1] := Dir + '..\demos\music.tyro';
  cand[2] := 'demos\music.tyro';
  cand[3] := '..\demos\music.tyro';
  Result := '';
  for k := 0 to High(cand) do
    if FileExists(cand[k]) then
      Exit(cand[k]);
end;

const
  cTol = 1E-4;

function Near(a, b: Double): Boolean;
begin
  Result := Abs(a - b) <= cTol;
end;

{ --- checks ----------------------------------------------------------------- }

procedure CheckSimple;
var
  E: TArray<TSoundEvent>;
  T: Double;

  procedure One(const MML: string; Expected: Double; const What: string);
  var
    Ev: TArray<TSoundEvent>;
    Tot: Double;
    k: Integer;
  begin
    Ev := ParseActual(MML);
    Tot := 0;
    for k := 0 to High(Ev) do
      Tot := Tot + EventTotal(Ev[k]);
    if not Near(Tot, Expected) then
      Fail(Format('%s: "%s" gave %.6fs, expected %.6fs', [What, MML, Tot, Expected]));
  end;

begin
  WriteLn('theory: simple, hand checked notes');
  One('t120 c4', 0.5, 'quarter @120');
  One('t120 c8', 0.25, 'eighth @120');
  One('t120 c2', 1.0, 'half @120');
  One('t120 c1', 2.0, 'whole @120');
  One('t60 c4', 1.0, 'quarter @60');
  One('t240 c4', 0.25, 'quarter @240');
  One('t120 c4.', 0.75, 'dotted quarter @120');
  One('t120 c4..', 0.875, 'double dotted quarter @120');
  One('t120 c4 c8 c8', 1.0, 'three notes sum');
  One('t120 l8 c d e', 0.75, 'l8 default length x3');
  One('t120 l4 c l8 d e', 0.5 + 0.25 + 0.25, 'l changes mid song');
  One('t120 r4', 0.5, 'quarter rest');
  One('t120 c4 c4', 1.0, 'two quarters');
  One('t120 i[GrandPiano] c4', 0.5, 'instrument then note');
  One('mfl16 t155 c', (60 / 155) * (4 / 16), 'WilliamTell header l16');
  // staccato must keep the same total note length, just split it
  begin
    E := ParseActual('t120 ms c4');
    T := 0;
    if Length(E) = 1 then
      T := EventTotal(E[0]);
    if not Near(T, 0.5) then
      Fail(Format('staccato: "t120 ms c4" total %.6fs, expected 0.5s', [T]));
    if Length(E) = 1 then
      if not Near(E[0].Duration, 0.375) then
        Fail(Format('staccato: sound %.6fs, expected 0.375s', [E[0].Duration]));
  end;
  // legato has no rest
  begin
    E := ParseActual('t120 ml c4');
    if (Length(E) = 1) and not Near(E[0].Rest, 0) then
      Fail('legato: expected zero rest');
  end;
  // normal mode removes 1/8 as a rest
  begin
    E := ParseActual('t120 mn c4');
    if (Length(E) = 1) and not Near(E[0].Rest, 0.0625) then
      Fail(Format('normal: rest %.6fs, expected 0.0625s', [E[0].Rest]));
  end;
end;

procedure CheckWilliamTell;
var
  Text, MML: string;
  Act: TArray<TSoundEvent>;
  Ref: TArray<Double>;
  k, Bad, Shown: Integer;
  ActTotal, RefTotal: Double;
begin
  WriteLn;
  WriteLn('WilliamTell from demos\music.tyro');
  Text := LoadText(FindDemoFile);
  if Text = '' then
  begin
    Fail('could not read demos\music.tyro');
    Exit;
  end;
  MML := ExtractSong(Text, 'WilliamTell');
  if MML = '' then
  begin
    Fail('could not extract WilliamTell');
    Exit;
  end;

  Act := ParseActual(MML);
  Ref := RefParse(MML);

  WriteLn(Format('  notes parsed: actual %d, reference %d', [Length(Act), Length(Ref)]));
  if Length(Act) <> Length(Ref) then
    Fail(Format('note count differs: actual %d, reference %d', [Length(Act), Length(Ref)]));

  ActTotal := 0;
  for k := 0 to High(Act) do
    ActTotal := ActTotal + EventTotal(Act[k]);
  RefTotal := 0;
  for k := 0 to High(Ref) do
    RefTotal := RefTotal + Ref[k];

  Bad := 0;
  Shown := 0;
  for k := 0 to Min(High(Act), High(Ref)) do
  begin
    if not Near(EventTotal(Act[k]), Ref[k]) then
    begin
      Inc(Bad);
      if Shown < 12 then
      begin
        WriteLn(Format('    note #%d: parsed %.6fs, theory %.6fs (freq %.1f)',
          [k + 1, EventTotal(Act[k]), Ref[k], Act[k].Freq]));
        Inc(Shown);
      end;
    end;
  end;

  WriteLn(Format('  total: parsed %.3fs (%.2f min), theory %.3fs (%.2f min)',
    [ActTotal, ActTotal / 60, RefTotal, RefTotal / 60]));
  if Bad > 0 then
    Fail(Format('%d of %d notes mismatch the theory length', [Bad, Length(Ref)]))
  else
    WriteLn('  every note duration matches music theory');
end;

begin
  Failures := 0;
  CheckSimple;
  CheckWilliamTell;
  WriteLn;
  if Failures = 0 then
    WriteLn('OK: all MML timing checks passed')
  else
    WriteLn(Failures, ' check(s) failed');
  Halt(Failures);
end.
