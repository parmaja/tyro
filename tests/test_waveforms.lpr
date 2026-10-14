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

    piano   the piano must die away, must lose its brightness while it dies,
            and must not put energy an octave below the written note. This is
            the regression for the old (sin f - sin 2f - sin f/2) generator
            with exp(-log10(50)*t/3), which stayed at ~48% after 1.5s, kept
            the same brightness all the way, and had the f/2 subharmonic as
            loud as the fundamental.

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

{ --- piano like decay -------------------------------------------------------
  A window of cWindow samples, the length is an integer number of cycles at
  every test frequency, so sines of different frequencies stay orthogonal. }

const
  cWindow = 4410;              //100ms
  cNoteLen = Round(1.5 * cSampleRate);
  cPianoFreq = 440;

function WindowRms(Proc: TWaveformProc; Freq: Single; StartIndex: Integer): Double;
var
  i: Integer;
  v: Double;
begin
  Result := 0;
  for i := StartIndex to StartIndex + cWindow - 1 do
  begin
    v := Proc(i, cSampleRate, Freq);
    Result := Result + v * v;
  end;
  Result := Sqrt(Result / cWindow);
end;

//Correlation of the generator with a plain sine of TestFreq, -1..1.
function WindowCorr(Proc: TWaveformProc; Freq, TestFreq: Single; StartIndex: Integer): Double;
var
  i: Integer;
  v, x, sv, sx: Double;
begin
  sv := 0;
  sx := 0;
  for i := StartIndex to StartIndex + cWindow - 1 do
  begin
    v := Proc(i, cSampleRate, Freq);
    x := Sin(2 * Pi * TestFreq * i / cSampleRate);
    sv := sv + v * x;
    sx := sx + v * v;
  end;
  if sx <= 0 then
    Result := 0
  else
    Result := sv / Sqrt(sx * (cWindow / 2));
end;

//Amplitude of the partial closest to Harmonic*f, 0..1. A piano string partial
//is inharmonic (a little sharp of k*f) and the generator may use any amount,
//so scan +-1% in 0.1% steps and keep the strongest, phase independent.
function PartialAmp(Proc: TWaveformProc; Freq: Single; Harmonic, StartIndex: Integer): Double;
var
  si, i: Integer;
  TestFreq, v, x, y, sv, ss, sc, m, best: Double;
begin
  best := 0;
  for si := 0 to 20 do
  begin
    TestFreq := Freq * Harmonic * (0.99 + si * 0.001);
    sv := 0;
    ss := 0;
    sc := 0;
    for i := StartIndex to StartIndex + cWindow - 1 do
    begin
      v := Proc(i, cSampleRate, Freq);
      x := Sin(2 * Pi * TestFreq * i / cSampleRate);
      y := Cos(2 * Pi * TestFreq * i / cSampleRate);
      sv := sv + v * v;
      ss := ss + v * x;
      sc := sc + v * y;
    end;
    if sv > 0 then
      m := Sqrt(ss * ss + sc * sc) / Sqrt(sv * (cWindow / 2))
    else
      m := 0;
    best := Max(best, m);
  end;
  Result := best;
end;

procedure CheckPiano;
var
  Early, Late: Double;
begin
  //decay: the note must be well below its start after 1.5s
  Early := WindowRms(@Piano_Waveform, cPianoFreq, 1000);
  Late := WindowRms(@Piano_Waveform, cPianoFreq, cNoteLen - cWindow);
  if Early < 0.1 then
    Fail(Format('Piano: nearly silent at the start (rms %g)', [Early]));
  if Late > Early * 0.35 then
    Fail(Format('Piano: does not decay, rms %g late vs %g early (ratio %g)',
      [Late, Early, Late / Early]));

  //brightness: the upper partials must die faster than the fundamental.
  //Amplitude of the 6th partial early and late: with the old shared envelope
  //the two were the same.
  Early := PartialAmp(@Piano_Waveform, cPianoFreq, 6, 1000);
  Late := PartialAmp(@Piano_Waveform, cPianoFreq, 6, cNoteLen - cWindow);
  if Early < 0.015 then
    Fail(Format('Piano: no upper partials at the start (6th amp %g)', [Early]));
  if Late > Early * 0.5 then
    Fail(Format('Piano: keeps its brightness, 6th amp %g late vs %g early',
      [Late, Early]));

  //pitch: an f/2 subharmonic would read as a note one octave too low
  Early := WindowCorr(@Piano_Waveform, cPianoFreq, cPianoFreq, 1000);
  Late := WindowCorr(@Piano_Waveform, cPianoFreq, cPianoFreq / 2, 1000);
  if Early < 0.6 then
    Fail(Format('Piano: weak fundamental, correlation %g', [Early]));
  if Abs(Late) > 0.3 then
    Fail(Format('Piano: subharmonic at half the frequency, correlation %g', [Late]));
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

  WriteLn('piano: decays, loses brightness, no subharmonic');
  CheckPiano;

  if Failures = 0 then
    WriteLn('OK: all waveform checks passed')
  else
    WriteLn(Failures, ' check(s) failed');
  Halt(Failures);
end.
