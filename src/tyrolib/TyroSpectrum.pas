unit TyroSpectrum;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *  Stereo spectrum analyzer control (Winamp-like): the registered audio
 *  processor callback captures the frames raylib feeds to a music/radio
 *  stream, an FFT splits them into frequency bars and a panel paints the
 *  left/right channels mirrored around the center.
 *
 *  The stream drives the analyzer: TRadioPlayer attaches the registered
 *  SpectrumProcessor() to the live music stream (AttachAudioStreamProcessor)
 *  and detaches it before every unload, so the analyzer only reacts to audio
 *  that is actually playing through that stream.
 *}

{$ifdef FPC}
{$mode delphi}
{$H+}{$M+}
{$endif}

interface

uses
  Classes, SysUtils, Types, SyncObjs, Math,
  RayLib, RayClasses,
  TyroClasses, TyroControls;

const
  cSpectrumFFTSize = 1024;       //power of two, ~21ms window at 48KHz
  cSpectrumRingFrames = 2048;    //capture ring (frames), must be >= FFT size
  cSpectrumDefaultBars = 32;
  cSpectrumBarsMin = 8;
  cSpectrumBarsMax = 128;
  cSpectrumMinFreq = 40;         //lowest analyzed frequency (Hz)
  cSpectrumFloorDb = -50;        //silence level (dB) -> bar height 0
  cSpectrumRangeDb = 42;         //dB range from silence to full scale

type
  TTyroSpectrumUpdate = class;

  { TTyroSpectrum }

  TTyroSpectrum = class(TTyroControl)
  private
    procedure DrawBar(ACanvas: TTyroCanvas; X, AWidth, AMaxHeight, ABaseY: Single; ABar: Integer; AChannel: Integer);
  protected
    procedure DoPaint(ACanvas: TTyroCanvas); override;
  public
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure PaintWindow(ACanvas: TTyroCanvas); override;
  end;

  { TTyroSpectrumUpdate }

  TTyroSpectrumUpdate = class(TRayUpdate)
  private
    FLock: TCriticalSection;
    FAttachedStream: TAudioStream;
    FAttached: Boolean;
    FSampleRate: Single;
    FActive: Boolean;
    FPanel: TTyroSpectrum;
    FVisible: Boolean;
    FBars: Integer;
    FRequestedBars: Integer;
    //capture ring, interleaved stereo f32 (audio thread writes, main thread reads)
    FRing: array of Single;
    FRingFrames: Integer;
    FWriteIndex: Integer;
    //analysis workspace (main thread)
    FHann: array of Single;
    FWindowL, FWindowR: array of Single;
    FReal: array[0..1] of array of Single;
    FImag: array[0..1] of array of Single;
    FBar: array[0..1] of array of Single;
    FPeak: array[0..1] of array of Single;
    FBandBins: array of Integer;
    procedure AllocateRing;
    procedure ComputeBands;
    procedure Capture(const AData: Pointer; AFrames: Integer);
    procedure ReadWindow;
    procedure FFT(var Re, Im: array of Single);
    procedure AnalyzeChannel(AChannel: Integer);
    procedure EnsurePanel;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Attach(const AStream: TAudioStream);
    procedure Detach;
    procedure Update; override;

    procedure Show(ALeft, ATop, AWidth, AHeight: Integer);
    procedure Hide;

    function BarLevel(ABar, AChannel: Integer): Single;
    function PeakLevel(ABar, AChannel: Integer): Single;

    property Active: Boolean read FActive;
    property Visible: Boolean read FVisible;
    property Bars: Integer read FBars;
    property RequestedBars: Integer read FRequestedBars write FRequestedBars;
  end;

  //Registered into a raylib audio stream: the stream calls this for every
  //block pushed to the device (f32, interleaved stereo, device sample rate).
  procedure SpectrumProcessor(var bufferData; frames: Cardinal); cdecl;

var
  Spectrum: TTyroSpectrumUpdate = nil;

implementation

uses
  TyroEngines;

{ TTyroSpectrum }

constructor TTyroSpectrum.Create(AParent: TTyroLayout);
begin
  inherited;
  Name := 'Spectrum';
  Style := [csOpaque];
  //Border := brdSizable;
  BackColor := clNearBlack;
  SetBoundsRect(Rect(0, 0, 500, 500));
end;

destructor TTyroSpectrum.Destroy;
begin
  inherited Destroy;
end;

procedure TTyroSpectrum.PaintWindow(ACanvas: TTyroCanvas);
begin
  inherited PaintWindow(ACanvas);
end;

procedure TTyroSpectrum.DrawBar(ACanvas: TTyroCanvas; X, AWidth,
  AMaxHeight, ABaseY: Single; ABar: Integer; AChannel: Integer);
var
  Level, Peak: Single;
  BarHeight, PeakY: Single;
  Color: TColor;
begin
  if (AWidth <= 0) or (AMaxHeight <= 0) then
    Exit;
  Level := Spectrum.BarLevel(ABar, AChannel);
  Peak := Spectrum.PeakLevel(ABar, AChannel);
  BarHeight := AMaxHeight * Level;
  if BarHeight < 1 then
    BarHeight := 0
  else if BarHeight > AMaxHeight then
    BarHeight := AMaxHeight;

  if Level > 0.85 then
    Color := clRed
  else if Level > 0.55 then
    Color := clYellow
  else
    Color := clGreen;
  if BarHeight > 0 then
    ACanvas.DrawRectangle(X, ABaseY - BarHeight, AWidth, BarHeight, Color, True);

  PeakY := ABaseY - AMaxHeight * Peak - 1;
  if PeakY < 0 then
    PeakY := 0;
  ACanvas.DrawRectangle(X, PeakY, AWidth, 1.5, clWhite, True);
end;

procedure TTyroSpectrum.DoPaint(ACanvas: TTyroCanvas);
var
  r: TRect;
  Margin, Gap, HalfWidth, BarWidth, MaxHeight, BaseY, X: Single;
  Bars, b, i: Integer;
  LineY: Single;
begin
  inherited;
  r := ClientRect;
  if (r.Width <= 0) or (r.Height <= 0) then
    Exit;
  Bars := Spectrum.Bars;
  if Bars < cSpectrumBarsMin then
    Bars := cSpectrumBarsMin;

  Margin := 3;
  Gap := 2;
  HalfWidth := (r.Width - 2 * Margin - Gap) * 0.5;
  BarWidth := (HalfWidth - (Bars - 1) * Gap) / Bars;
  if BarWidth < 1 then
    BarWidth := 1;
  MaxHeight := r.Height - 2 * Margin;
  BaseY := r.Bottom - Margin;

  //faint horizontal grid lines
  for i := 1 to 3 do
  begin
    LineY := BaseY - MaxHeight * i * 0.25;
    if LineY > r.Top then
      ACanvas.DrawLineF(r.Left + Margin, LineY, r.Right - Margin, LineY, clDarkGray.ReplaceAlpha(70));
  end;
  //center divider
  ACanvas.DrawLineF(r.Left + Margin, r.Top + Margin, r.Left + Margin, BaseY, clDarkGray.ReplaceAlpha(120));

  for b := 0 to Bars - 1 do
  begin
    //left channel: mirrored on the left half, growing toward the center
    X := r.Left + Margin + b * (BarWidth + Gap);
    DrawBar(ACanvas, X, BarWidth, MaxHeight, BaseY, b, 0);
    //right channel: mirrored on the right half
    X := r.Right - Margin - (b + 1) * BarWidth - b * Gap;
    DrawBar(ACanvas, X, BarWidth, MaxHeight, BaseY, b, 1);
  end;
end;

{ TTyroSpectrumUpdate }

constructor TTyroSpectrumUpdate.Create;
var
  i: Integer;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FSampleRate := 48000;
  FBars := cSpectrumDefaultBars;
  FRequestedBars := cSpectrumDefaultBars;
  SetLength(FHann, cSpectrumFFTSize);
  for i := 0 to cSpectrumFFTSize - 1 do
    FHann[i] := 0.5 * (1 - Cos(2 * Pi * i / (cSpectrumFFTSize - 1)));
  SetLength(FWindowL, cSpectrumFFTSize);
  SetLength(FWindowR, cSpectrumFFTSize);
  SetLength(FReal[0], cSpectrumFFTSize);
  SetLength(FReal[1], cSpectrumFFTSize);
  SetLength(FImag[0], cSpectrumFFTSize);
  SetLength(FImag[1], cSpectrumFFTSize);
  for i := 0 to 1 do
  begin
    SetLength(FBar[i], FBars);
    SetLength(FPeak[i], FBars);
  end;
  ComputeBands;
end;

destructor TTyroSpectrumUpdate.Destroy;
begin
  Detach;
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TTyroSpectrumUpdate.AllocateRing;
begin
  FRingFrames := cSpectrumRingFrames;
  SetLength(FRing, FRingFrames * 2);
  FWriteIndex := 0;
end;

procedure TTyroSpectrumUpdate.ComputeBands;
var
  b: Integer;
  Ratio, f1: Single;
begin
  if (FBars < 1) or (FSampleRate <= cSpectrumMinFreq * 2) then
    Exit;
  SetLength(FBandBins, FBars + 1);
  Ratio := (FSampleRate * 0.5) / cSpectrumMinFreq;
  for b := 0 to FBars - 1 do
  begin
    FBandBins[b] := Floor(cSpectrumMinFreq * Power(Ratio, b / FBars) / FSampleRate * cSpectrumFFTSize);
    f1 := cSpectrumMinFreq * Power(Ratio, (b + 1) / FBars);
    FBandBins[b + 1] := Ceil(f1 / FSampleRate * cSpectrumFFTSize);
  end;
  for b := 0 to FBars do
  begin
    if FBandBins[b] < 1 then
      FBandBins[b] := 1;
    if FBandBins[b] > cSpectrumFFTSize div 2 - 1 then
      FBandBins[b] := cSpectrumFFTSize div 2 - 1;
  end;
  for b := 1 to FBars do
    if FBandBins[b] < FBandBins[b - 1] then
      FBandBins[b] := FBandBins[b - 1];
end;

procedure TTyroSpectrumUpdate.Attach(const AStream: TAudioStream);
begin
  Detach;
  FSampleRate := AStream.SampleRate;
  if (FSampleRate < 8000) or (FSampleRate > 192000) then
    FSampleRate := 48000;
  ComputeBands;
  AllocateRing;
  AttachAudioStreamProcessor(AStream, @SpectrumProcessor);
  FAttachedStream := AStream;
  FAttached := True;
end;

procedure TTyroSpectrumUpdate.Detach;
begin
  if FAttached then
  begin
    //DetachAudioStreamProcessor is safe to call while the audio thread is
    //mixing: both take the same internal audio system lock, so once it returns
    //the processor is no longer invoked and the ring can be freed.
    DetachAudioStreamProcessor(FAttachedStream, @SpectrumProcessor);
    FAttached := False;
    FLock.Enter;
    try
      FRing := nil;
      FWriteIndex := 0;
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TTyroSpectrumUpdate.Capture(const AData: Pointer; AFrames: Integer);
var
  S: PSingle;
  i, n: Integer;
begin
  if (AData = nil) or (AFrames <= 0) then
    Exit;
  S := AData;
  n := AFrames;
  FLock.Enter;
  try
    if FRing = nil then
      Exit;
    if n >= FRingFrames then
    begin
      //block larger than the ring: keep only the latest ring-sized portion
      Inc(S, (n - FRingFrames) * 2);
      n := FRingFrames;
      FWriteIndex := 0;
    end;
    if FWriteIndex + n > FRingFrames then
    begin
      i := FRingFrames - FWriteIndex;
      Move(S^, FRing[FWriteIndex * 2], i * 2 * SizeOf(Single));
      Move((S + i * 2)^, FRing[0], (n - i) * 2 * SizeOf(Single));
      FWriteIndex := n - i;
    end
    else
    begin
      Move(S^, FRing[FWriteIndex * 2], n * 2 * SizeOf(Single));
      Inc(FWriteIndex, n);
      if FWriteIndex >= FRingFrames then
        Dec(FWriteIndex, FRingFrames);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TTyroSpectrumUpdate.ReadWindow;
var
  Start, i, j: Integer;
begin
  FLock.Enter;
  try
    if FRing = nil then
      Exit;
    Start := FWriteIndex - cSpectrumFFTSize;
    if Start < 0 then
      Inc(Start, FRingFrames);
    for i := 0 to cSpectrumFFTSize - 1 do
    begin
      j := ((Start + i) mod FRingFrames) * 2;
      FWindowL[i] := FRing[j];
      FWindowR[i] := FRing[j + 1];
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TTyroSpectrumUpdate.FFT(var Re, Im: array of Single);
var
  i, j, k, Len, Step: Integer;
  c, s, temp, twr, twi, tr, ti: Single;
begin
  //bit-reversal permutation
  j := 0;
  for i := 1 to cSpectrumFFTSize - 1 do
  begin
    Step := cSpectrumFFTSize shr 1;
    while (j and Step) <> 0 do
    begin
      j := j xor Step;
      Step := Step shr 1;
    end;
    j := j xor Step;
    if i < j then
    begin
      temp := Re[i]; Re[i] := Re[j]; Re[j] := temp;
      temp := Im[i]; Im[i] := Im[j]; Im[j] := temp;
    end;
  end;

  Len := 2;
  while Len <= cSpectrumFFTSize do
  begin
    Step := Len shr 1;
    c := Cos(-2 * Pi / Len);
    s := Sin(-2 * Pi / Len);
    i := 0;
    while i < cSpectrumFFTSize do
    begin
      twr := 1;
      twi := 0;
      for k := 0 to Step - 1 do
      begin
        tr := Re[i + k + Step] * twr - Im[i + k + Step] * twi;
        ti := Re[i + k + Step] * twi + Im[i + k + Step] * twr;
        Re[i + k + Step] := Re[i + k] - tr;
        Im[i + k + Step] := Im[i + k] - ti;
        Re[i + k] := Re[i + k] + tr;
        Im[i + k] := Im[i + k] + ti;
        temp := twr * c - twi * s;
        twi := twr * s + twi * c;
        twr := temp;
      end;
      Inc(i, Len);
    end;
    Len := Len * 2;
  end;
end;

procedure TTyroSpectrumUpdate.AnalyzeChannel(AChannel: Integer);
var
  b, i, Bin, BinEnd, Count: Integer;
  Amp, Level, Abs, Db: Single;
begin
  for b := 0 to FBars - 1 do
  begin
    Amp := 0;
    Count := 0;
    Bin := FBandBins[b];
    BinEnd := FBandBins[b + 1];
    for i := Bin to BinEnd do
    begin
      Abs := Sqrt(FReal[AChannel, i] * FReal[AChannel, i] + FImag[AChannel, i] * FImag[AChannel, i]);
      Amp := Amp + Abs;
      Inc(Count);
    end;
    if Count > 0 then
      Amp := (Amp / Count) * (4.0 / cSpectrumFFTSize); //single-sided amplitude (Hann gain ~0.5)
    if Amp <= 0 then
      Level := 0
    else
    begin
      Db := 20 * Log10(Amp);
      Level := (Db - cSpectrumFloorDb) / cSpectrumRangeDb;
      if Level < 0 then
        Level := 0
      else if Level > 1 then
        Level := 1;
    end;
    //fast attack, slow release
    if Level > FBar[AChannel, b] then
      FBar[AChannel, b] := FBar[AChannel, b] + (Level - FBar[AChannel, b]) * 0.7
    else
      FBar[AChannel, b] := FBar[AChannel, b] + (Level - FBar[AChannel, b]) * 0.12;
    if Level > FPeak[AChannel, b] then
      FPeak[AChannel, b] := Level
    else
      FPeak[AChannel, b] := FPeak[AChannel, b] * 0.965;
    if FPeak[AChannel, b] < FBar[AChannel, b] then
      FPeak[AChannel, b] := FBar[AChannel, b];
  end;
end;

procedure TTyroSpectrumUpdate.EnsurePanel;
begin
  if FPanel = nil then
  begin
    FPanel := TTyroSpectrum.Create(Main);
    FPanel.Show;
  end;
end;

procedure TTyroSpectrumUpdate.Show(ALeft, ATop, AWidth, AHeight: Integer);
begin
  EnsurePanel;
  FPanel.BoundsRect := Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
  FPanel.Show;
  FVisible := True;
end;

procedure TTyroSpectrumUpdate.Hide;
begin
  if FPanel <> nil then
    FPanel.Hide;
  FVisible := False;
end;

procedure TTyroSpectrumUpdate.Update;
var
  i, ch: Integer;
begin
  if (FRequestedBars <> FBars) and (FRequestedBars >= cSpectrumBarsMin) and (FRequestedBars <= cSpectrumBarsMax) then
  begin
    FBars := FRequestedBars;
    for ch := 0 to 1 do
    begin
      SetLength(FBar[ch], FBars);
      SetLength(FPeak[ch], FBars);
    end;
    ComputeBands;
  end;

  FActive := FAttached;

  if not FAttached then
  begin
    //no stream: let the bars decay to zero
    for ch := 0 to 1 do
      for i := 0 to FBars - 1 do
      begin
        FBar[ch, i] := FBar[ch, i] * 0.9;
        FPeak[ch, i] := FPeak[ch, i] * 0.92;
      end;
    Exit;
  end;

  ReadWindow;
  for ch := 0 to 1 do
  begin
    for i := 0 to cSpectrumFFTSize - 1 do
    begin
      if ch = 0 then
        FReal[0, i] := FWindowL[i] * FHann[i]
      else
        FReal[1, i] := FWindowR[i] * FHann[i];
      FImag[ch, i] := 0;
    end;
    FFT(FReal[ch], FImag[ch]);
    AnalyzeChannel(ch);
  end;
end;

function TTyroSpectrumUpdate.BarLevel(ABar, AChannel: Integer): Single;
begin
  if (ABar >= 0) and (ABar < FBars) and (AChannel >= 0) and (AChannel <= 1) then
    Result := FBar[AChannel, ABar]
  else
    Result := 0;
end;

function TTyroSpectrumUpdate.PeakLevel(ABar, AChannel: Integer): Single;
begin
  if (ABar >= 0) and (ABar < FBars) and (AChannel >= 0) and (AChannel <= 1) then
    Result := FPeak[AChannel, ABar]
  else
    Result := 0;
end;

{ SpectrumProcessor }

procedure SpectrumProcessor(var bufferData; frames: Cardinal); cdecl;
begin
  if Spectrum <> nil then
    Spectrum.Capture(@bufferData, Integer(frames));
end;

initialization
  Spectrum := TTyroSpectrumUpdate.Create;
  RayUpdates.Add(Spectrum);

finalization
  if Spectrum <> nil then
  begin
    RayUpdates.Remove(Spectrum);
    FreeAndNil(Spectrum);
  end;
end.
