program test_waveforms;

{ Headless test for the waveform generators of tyrolib\TyroSounds.pas.

  Two things are checked:

    phase   the period based waveforms (sine, square, saw, triangle, pulse,
            rect, dsin) must follow the exact frequency. This is the
            regression for the old '(Index mod Round(period))' phase, which
            quantized the pitch (440 Hz played as 441 Hz) and put a step in
            the sine at every period boundary, and for the old sine that mixed
            a rounded period with an unrounded one and so jumped phase every
            cycle.

    range   every tonal waveform must stay inside +-1, so a sample cannot wrap
            a SmallInt when the generator scales it by the amplitude. The
            white 'Noise' is skipped because it calls Rayleigh's RNG, the rest
            are pure math.

  Build and run:
    lazbuild --build-mode=Debug tests\test_waveforms.lpi
    bin\test_waveforms.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  SysUtils, Math,
  RayLib,
  TyroSounds;

const
  cSampleRate = 44100;
  cFreqs: array[0..2] of Single = (110, 440, 3000);
  cLastIndex = 8192; //a couple of seconds at the low test frequency

type
  TRefFunction = function(Pos: Double): Double;

var
  Failures: Integer;

procedure Fail(const Msg: string);
begin
  Inc(Failures);
  WriteLn('FAIL: ', Msg);
end;

// Exact phase, 0..1, computed in Double on purpose and independently from the
// implementation, so the test does not just mirror the code under test.
function RefPosition(Index: Integer; Frequency: Single): Double;
var
  p: Double;
begin
  p := Frequency; //widen before multiplying by the (possibly large) Index
  Result := Frac(p * Index / cSampleRate);
end;

{ --- reference shapes ------------------------------------------------------ }

function RefSin(Pos: Double): Double;
begin
  Result := Sin(2 * Pi * Pos);
end;

function RefSquare(Pos: Double): Double;
begin
  if Pos < 0.5 then
    Result := 1
  else
    Result := -1;
end;

function RefSaw(Pos: Double): Double;
begin
  Result := (Pos * 2) - 1;
end;

function RefTri(Pos: Double): Double;
begin
  if Pos < 0.5 then
    Result := (Pos * 4) - 1
  else
    Result := 3 - (Pos * 4);
end;

function RefPulse25(Pos: Double): Double;
begin
  if Pos < 0.25 then
    Result := 1
  else
    Result := -1;
end;

function RefPulse125(Pos: Double): Double;
begin
  if Pos < 0.125 then
    Result := 1
  else
    Result := -1;
end;

function RefRect(Pos: Double): Double;
begin
  Result := Abs(Sin(2 * Pi * Pos));
end;

function RefDsin(Pos: Double): Double;
var
  s: Double;
begin
  s := Sin(2 * Pi * Pos);
  Result := s * Abs(s);
end;

{ --- checks ---------------------------------------------------------------- }

procedure CheckPhase(const Name: string; Proc: TWaveformProc; Ref: TRefFunction);
var
  fi, i, Bad: Integer;
  f, v: Single;
  e: Double;
begin
  for fi := 0 to High(cFreqs) do
  begin
    f := cFreqs[fi];
    Bad := 0;
    for i := 0 to cLastIndex do
    begin
      e := Ref(RefPosition(i, f));
      v := Proc(i, cSampleRate, f);
      if Abs(v - e) > 1E-3 then
        Inc(Bad);
    end;
    if Bad > 0 then
      Fail(Format('%s: %d/%d samples off the exact phase at %g Hz',
        [Name, Bad, cLastIndex + 1, f]));
  end;
end;

procedure CheckRange(const Name: string; Proc: TWaveformProc);
var
  fi, i: Integer;
  f, v, Peak: Single;
begin
  for fi := 0 to High(cFreqs) do
  begin
    f := cFreqs[fi];
    Peak := 0;
    for i := 0 to cLastIndex do
    begin
      v := Proc(i, cSampleRate, f);
      if IsNan(v) or IsInfinite(v) then
      begin
        Fail(Format('%s: not finite at i=%d, %g Hz', [Name, i, f]));
        Exit;
      end;
      Peak := Max(Peak, Abs(v));
    end;
    if Peak > 1.0001 then
      Fail(Format('%s: peak %g exceeds 1 at %g Hz', [Name, Peak, f]));
    if Peak < 0.02 then
      Fail(Format('%s: silent (peak %g) at %g Hz', [Name, Peak, f]));
  end;
end;

var
  i: Integer;
  w: TWaveForm;

begin
  Failures := 0;

  //TyroSounds pulls in RayClasses, whose finalization asks raylib whether the
  //audio device is ready. Loading the library keeps that call valid; the
  //RayLib unit frees it again, after RayClasses is done with it.
  if not RayLibrary.IsLoaded then
    RayLibrary.Load;

  WriteLn('phase: exact frequency is followed');
  CheckPhase('Sin', @Sin_Waveform, RefSin);
  CheckPhase('Square', @Square_Waveform, RefSquare);
  CheckPhase('Saw', @Saw_Waveform, RefSaw);
  CheckPhase('Tri', @Tri_Waveform, RefTri);
  CheckPhase('Pulse25', @Pulse25_Waveform, RefPulse25);
  CheckPhase('Pulse125', @Pulse125_Waveform, RefPulse125);
  CheckPhase('Rect', @Rect_Waveform, RefRect);
  CheckPhase('Dsin', @Dsin_Waveform, RefDsin);

  WriteLn('range: every waveform stays inside +-1');
  for i := 0 to WaveForms.Count - 1 do
  begin
    w := WaveForms[i];
    if SameText(w.Name, 'Noise') then //white noise uses Rayleigh's RNG
      Continue;
    CheckRange(w.Name, w.Proc);
  end;

  if Failures = 0 then
    WriteLn('OK: all waveform checks passed')
  else
    WriteLn(Failures, ' check(s) failed');
  Halt(Failures);
end.
