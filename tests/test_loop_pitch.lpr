program test_loop_pitch;

{ Headless test for the loop voice of tyrolib\TyroSounds.pas, the code behind
  sound.loopPlay()/sound.loopPitch()/sound.loopStop().

  A loop renders its waveform at the frequency it was started with and then
  walks a source cursor over that stream at Rate = wanted/base, interpolating
  between the two neighbouring source samples. Five things have to hold:

    identity  at Rate = 1 the output must still be the generator's own
              sequence, so a plain sound.loopPlay did not change at all.

              This is the regression for the previous implementation, which
              evaluated the waveform at Round(cursor) directly: it was nearest
              neighbour resampling, so it stepped by two source indices for any
              Rate above 1 (the 'glide' check below) and it never reached the
              waveforms that ignore the Index argument at all.

    pitch     for a periodic waveform the output frequency must be base * Rate,
              at 1, above and below the start pitch.

    retune    Engine and Noise build their sound from their own state and from
              the Frequency argument, they ignore Index, so the old read at
              Round(cursor) gave them the unchanged BaseFrequency once per
              output sample no matter what Rate was: sound.loopPitch did
              literally nothing for the River Raid plane engine. They now have
              to get brighter (their spectrum moves up) as Rate goes up.

              White is not checked: white noise is scale invariant, resampling
              it sounds exactly the same at any rate.

    glide     changing Rate while playing must only change the slope of the
              cursor. Consecutive samples stay inside the slope the new rate
              allows, where nearest neighbour overshoots it by ~2x.

    wrap      a loop can play forever, so the source cursor is wrapped back
              before it can reach the top of an Integer; the app builds with
              {$R+}{$Q+}, so the overflow would stop the audio thread after
              ~6.7 hours of looping. WrapAt is a whole number of base cycles
              and SrcPos moves with SrcIndex, so a wrapped sine must render
              the same samples as one that never wraps.

  The envelope is checked too, since the fade in/out is what keeps a retrigged
  or retuned loop from clicking.

  Build and run:
    lazbuild --build-mode=Debug tests\test_loop_pitch.lpi
    bin\test_loop_pitch.exe

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
  cBaseFreq = 300;           //the River Raid engine drone range
  cSecond = cSampleRate;

type
  TFloats = array of Single;

var
  Failures: Integer;

procedure Fail(const Msg: string);
begin
  Inc(Failures);
  WriteLn('FAIL: ', Msg);
end;

//A loop voice as TTyroLoopSound.Create builds it, but with the fade already
//open so the checks read raw samples, except where the fade itself is checked.
function MakeSignal(Wave: TWaveformProc; Rate: Single): TLoopSignal;
begin
  Result.Active := True;
  Result.Wave := Wave;
  Result.SampleRate := cSampleRate;
  Result.BaseFrequency := cBaseFreq;
  Result.Rate := Rate;
  Result.Gain := 1;
  Result.TargetGain := 1;
  Result.GainStep := 1;
  Result.Reset;
end;

function Render(var Signal: TLoopSignal; Count: Integer): TFloats;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Signal.NextSample;
end;

{ --- measurements ----------------------------------------------------------- }

//Frequency from upward zero crossings, the crossing point is interpolated
//between the two samples so a partial sample of error does not accumulate.
function UpCrossFrequency(const Buf: TFloats): Double;
var
  i, Count: Integer;
  First, Last, t: Double;
begin
  Result := 0;
  Count := 0;
  First := 0;
  Last := 0;
  for i := 0 to High(Buf) - 1 do
    if (Buf[i] <= 0) and (Buf[i + 1] > 0) then
    begin
      t := i + (-Buf[i]) / (Buf[i + 1] - Buf[i]);
      if Count = 0 then
        First := t;
      Last := t;
      Inc(Count);
    end;
  if Count < 2 then
    Exit;
  Result := (Count - 1) * cSampleRate / (Last - First);
end;

//Energy of the first difference over the energy of the signal: it rises with
//the centroid of the spectrum, so a lowpassed noise getting brighter moves it
//up. Robust enough to compare two windows of the same generator.
function SpectralTilt(const Buf: TFloats): Double;
var
  i: Integer;
  d, Num, Den: Double;
begin
  Num := 0;
  Den := 0;
  for i := 1 to High(Buf) do
  begin
    d := Buf[i] - Buf[i - 1];
    Num := Num + d * d;
    Den := Den + Buf[i] * Buf[i];
  end;
  if Den <= 0 then
    Result := 0
  else
    Result := Num / Den;
end;

function MaxDelta(const Buf: TFloats): Double;
var
  i: Integer;
  d: Double;
begin
  Result := 0;
  for i := 1 to High(Buf) do
  begin
    d := Abs(Buf[i] - Buf[i - 1]);
    if d > Result then
      Result := d;
  end;
end;

function Peak(const Buf: TFloats): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Buf) do
    if Abs(Buf[i]) > Result then
      Result := Abs(Buf[i]);
end;

{ --- checks ----------------------------------------------------------------- }

procedure CheckIdentity;
var
  Sig: TLoopSignal;
  Buf: TFloats;
  i, Bad: Integer;
  e: Double;
begin
  Sig := MakeSignal(@Sin_Waveform, 1);
  Buf := Render(Sig, 4096);
  Bad := 0;
  for i := 0 to High(Buf) do
  begin
    e := Sin(2 * Pi * cBaseFreq * i / cSampleRate);
    if Abs(Buf[i] - e) > 1E-6 then
      Inc(Bad);
  end;
  if Bad > 0 then
    Fail(Format('identity: %d/%d samples differ from the generator at Rate=1',
      [Bad, Length(Buf)]))
  else
    WriteLn(Format('identity: Rate=1 is the generator''s own sequence (%d samples, peak %.4f)',
      [Length(Buf), Peak(Buf)]));
end;

procedure CheckPitch(const Name: string; Proc: TWaveformProc);
const
  cRates: array[0..2] of Single = (1, 4 / 3, 0.5);
var
  Sig: TLoopSignal;
  Buf: TFloats;
  fi: Integer;
  target, measured: Double;
begin
  for fi := 0 to High(cRates) do
  begin
    Sig := MakeSignal(Proc, cRates[fi]);
    //half a second: enough cycles at 150 Hz for the crossings to be exact
    Buf := Render(Sig, cSecond div 2);
    target := cBaseFreq * cRates[fi];
    measured := UpCrossFrequency(Buf);
    if measured <= 0 then
      Fail(Format('pitch: %s produced no upward crossing at Rate=%g',
        [Name, cRates[fi]]))
    else if Abs(measured - target) / target > 0.01 then
      Fail(Format('pitch: %s at Rate=%g plays %.1f Hz, expected %.1f Hz',
        [Name, cRates[fi], measured, target]))
    else
      WriteLn(Format('pitch:   %-7s Rate %.3f -> %6.1f Hz (target %6.1f)',
        [Name, cRates[fi], measured, target]));
  end;
end;

procedure CheckRetune(const Name: string; Proc: TWaveformProc);
var
  Slow, Fast: TLoopSignal;
  SlowBuf, FastBuf: TFloats;
  TiltSlow, TiltFast, Ratio: Double;
begin
  Slow := MakeSignal(Proc, 0.5);
  SlowBuf := Render(Slow, cSecond);
  Fast := MakeSignal(Proc, 2.0);
  FastBuf := Render(Fast, cSecond);

  TiltSlow := SpectralTilt(SlowBuf);
  TiltFast := SpectralTilt(FastBuf);
  if (TiltSlow <= 0) or (TiltFast <= 0) then
  begin
    Fail(Format('retune: %s is silent, tilt %.6f / %.6f', [Name, TiltSlow, TiltFast]));
    Exit;
  end;
  Ratio := TiltFast / TiltSlow;
  //the spectrum moves with Rate: (2.0 / 0.5)^2 = 16 for a signal this low
  //passed, anything at or below 1 means the rate never reached the generator.
  if Ratio < 1.5 then
    Fail(Format('retune: %s ignores Rate, high/low tilt ratio %.3f (need > 1.5)',
      [Name, Ratio]))
  else
    WriteLn(Format('retune: %-7s tilt %.5f -> %.5f at Rate 0.5 -> 2 (x%.1f)',
      [Name, TiltSlow, TiltFast, Ratio]));
end;

procedure CheckGlide;
var
  Sig: TLoopSignal;
  Buf: TFloats;
  Rate, Slope, Limit, d: Double;
begin
  //Start a plain loop, then retune it while it plays, the way sound.loopPitch
  //does.
  Sig := MakeSignal(@Sin_Waveform, 1);
  Buf := Render(Sig, 4410);

  Rate := 1.1;
  Sig.Rate := Rate;
  Buf := Render(Sig, 4410);

  //Slope of a sine of BaseFrequency at Rate, with 50% headroom for the
  //interpolation between two source samples.
  Slope := Rate * 2 * Pi * cBaseFreq / cSampleRate;
  Limit := Slope * 1.5;
  d := MaxDelta(Buf);
  if d > Limit then
    Fail(Format('glide: step of %.4f after retune to Rate %g exceeds the %.4f the new rate allows (phase jump)',
      [d, Rate, Limit]))
  else
    WriteLn(Format('glide:   max step %.4f after retune, limit %.4f', [d, Limit]));
end;

procedure CheckWrap;
const
  cWrapLimit = 1 shl 30;
var
  Sig, Ref, Noise: TLoopSignal;
  Buf, RefBuf, NoiseBuf: TFloats;
  i, Bad, Period, Wraps: Integer;
begin
  //Reset must size the wrap as a whole number of base cycles and keep it
  //inside the range the cursor has to stay in.
  Sig := MakeSignal(@Sin_Waveform, 1);
  Period := Round(cSampleRate / cBaseFreq);
  if (Sig.WrapAt <= 0) or (Sig.WrapAt > cWrapLimit) then
    Fail(Format('wrap: WrapAt %d is outside 1..%d', [Sig.WrapAt, cWrapLimit]))
  else if (Sig.WrapAt mod Period) <> 0 then
    Fail(Format('wrap: WrapAt %d is not a whole number of %d sample cycles',
      [Sig.WrapAt, Period]));

  //Wrapping on a whole number of cycles must be inaudible: the wrapping voice
  //has to render the same samples as one that never wraps.
  Ref := MakeSignal(@Sin_Waveform, 1);
  RefBuf := Render(Ref, 4096);

  Sig := MakeSignal(@Sin_Waveform, 1);
  Sig.WrapAt := Period * 4; //wrap far more often than the real limit
  Buf := Render(Sig, 4096);
  Bad := 0;
  for i := 0 to High(Buf) do
    if Abs(Buf[i] - RefBuf[i]) > 1E-5 then
      Inc(Bad);
  Wraps := Length(Buf) div (Period * 4);

  //And a wrap wild enough to catch a stateful generator out every few samples
  //must leave it inside +-1 (Engine ignores Index, so nothing of it is lost).
  Noise := MakeSignal(@Engine_Waveform, 1);
  Noise.WrapAt := 3;
  NoiseBuf := Render(Noise, 4096);

  if Bad > 0 then
    Fail(Format('wrap: %d/%d samples differ across %d wraps, the phase moved',
      [Bad, Length(Buf), Wraps]))
  else if Peak(NoiseBuf) > 1.0001 then
    Fail(Format('wrap: Engine peak %g while wrapping every 3 samples', [Peak(NoiseBuf)]))
  else
    WriteLn(Format('wrap:   %d wraps, sine unchanged, Engine stays inside +-1', [Wraps]));
end;

procedure CheckFade;
var
  Sig: TLoopSignal;
  Buf: TFloats;
  Ramp: Integer;
  Before: Integer;
begin
  Before := Failures;
  //The attack the constructor installs: 8 ms from silence to full.
  Sig := MakeSignal(@Sin_Waveform, 1);
  Sig.Gain := 0;
  Sig.TargetGain := 1;
  Sig.GainStep := 1 / (0.008 * cSampleRate);
  Ramp := Ceil(0.008 * cSampleRate);

  Buf := Render(Sig, Ramp * 4);
  if Buf[0] <> 0 then
    Fail(Format('fade: first sample is %g, expected 0 while the voice is silent', [Buf[0]]));
  if Sig.Gain < 1 then
    Fail(Format('fade: attack did not finish after %d samples, gain %g', [Ramp, Sig.Gain]));
  if Peak(Buf) > 1.0001 then
    Fail(Format('fade: peak %g during the attack exceeds 1', [Peak(Buf)]));

  //and the release StopLoop ramps down with. The ramp itself is meant to be
  //audible, it starts at full gain; what must be silent is the voice after it.
  Sig.TargetGain := 0;
  Buf := Render(Sig, Ramp + 64);
  if Sig.Gain > 0 then
    Fail(Format('fade: release did not reach 0, gain %g', [Sig.Gain]));
  Buf := Render(Sig, 128);
  if Peak(Buf) > 0.0001 then
    Fail(Format('fade: tail peak %g after the release, expected silence', [Peak(Buf)]));
  if Failures = Before then
    WriteLn('fade:    silent start, 8 ms attack, release to silence');
end;

procedure CheckInactive;
var
  Sig: TLoopSignal;
  Buf: TFloats;
begin
  Sig := MakeSignal(@Sin_Waveform, 1);
  Sig.Active := False;
  Buf := Render(Sig, 256);
  if Peak(Buf) <> 0 then
    Fail(Format('inactive voice produced audio, peak %g', [Peak(Buf)]))
  else
    WriteLn('silent:  an inactive voice renders zeros');
end;

begin
  Failures := 0;

  //TyroSounds pulls in RayClasses, whose finalization asks raylib whether the
  //audio device is ready. Loading the library keeps that call valid; the
  //RayLib unit frees it again, after RayClasses is done with it.
  if not RayLibrary.IsLoaded then
    RayLibrary.Load;

  WriteLn('loop voice: Rate must reach every waveform');
  CheckIdentity;
  CheckInactive;

  WriteLn('pitch: base * Rate, above and below the start pitch');
  CheckPitch('Sin', @Sin_Waveform);
  CheckPitch('Square', @Square_Waveform);
  CheckPitch('Saw', @Saw_Waveform);
  CheckPitch('Tri', @Tri_Waveform);
  CheckPitch('Pulse25', @Pulse25_Waveform);
  CheckPitch('Dsin', @Dsin_Waveform);
  //Organ is not checked: its 4th harmonic puts a second upward crossing in
  //every period, so a crossing count reads twice the frequency for it.
  CheckPitch('FM', @FM_Waveform);

  WriteLn('retune: the stateful generators have to follow Rate too');
  CheckRetune('Engine', @Engine_Waveform);
  CheckRetune('Noise', @Noise_Waveform);
  //index driven noise, kept as a control: it always worked
  CheckRetune('Pink', @PinkNoise_Waveform);

  CheckGlide;
  CheckWrap;
  CheckFade;

  if Failures = 0 then
    WriteLn('OK: all loop pitch checks passed')
  else
    WriteLn(Failures, ' check(s) failed');
  Halt(Failures);
end.
