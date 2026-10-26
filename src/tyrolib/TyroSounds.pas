unit TyroSounds;
{$IFDEF FPC}
{$MODE delphi}
{$else}
{$POINTERMATH ON}
{$ENDIF}

{$M+}{$H+}
{**
 * This file is part of the 'Tyro'
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey , zaher, zaherdirkey
 *
 *}

{////////////////////////////////////////////////////////////////////////////-
*  	Music Macro Language
*   https://en.wikipedia.org/wiki/Music_Macro_Language
*
*   This file is used part of the 'Tyro'
*   @license   The MIT License (MIT) Included in this distribution
*   @author    Zaher Dirkey <zaherdirkey at yahoo dot com>
//////////////////////////////////////////////////////////////////////////////-
*  Look at this site, it have many of songs
*  https://archeagemmllibrary.com/
*	 usefull refereces
*
*  http://web.mit.edu/18.06/www/Essays/linear-algebra-and-music.pdf
*
* Editor
* 3ML https://wiki.mabinogiworld.com/view/User:LexisMikaya/MML_101_Guide
//////////////////////////////////////////////////////////////////////////////-}

interface

uses
  Classes, SysUtils, mnClasses, mnUtils, Math,
  Melodies,
  RayLib, RayClasses;

{$define FADE} //fadein fadeout generated sound to reduce tick at the end of sound

type

  TWaveformProc = function(Index: Integer; SampleRate: Integer; Frequency: Single): Single;

  { TWaveForm }

  TWaveForm = class(TmnNamedObject)
  public
    Proc: TWaveformProc;
  end;

  { TWaveForms }

  TWaveForms = Class(TmnNamedObjectList<TWaveForm>)
  public
    procedure Add(Name: string; Proc: TWaveformProc);
  end;

  { TTyroRayWave }

  TTyroRayWave = class(TRaySound)
  protected
  public
    procedure Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single = 100; SampleRate: Integer = 44100; BitRate: Integer = 16);
  end;

  { TTyroFileSound }

  { A one-shot sound effect loaded from an audio file (raylib LoadSound,
    .wav). It subclasses TTyroRayWave without generating anything, so
    PlayingWaveforms and its UpdateWaveforms poll - which cast their entries
    to TTyroRayWave and ask IsPlaying - reclaim it exactly like a generated
    waveform. }
  TTyroFileSound = class(TTyroRayWave)
  private
    FFileName: string;
    FIsAlias: Boolean;
  public
    constructor CreateFromFile(const AFileName: string);
    //A cheap second voice over a cached sound (raylib LoadSoundAlias): it
    //shares the sample data, so several copies of one effect can overlap and
    //the cached original is never touched by the one-shot.
    constructor CreateAlias(Source: TTyroFileSound);
    destructor Destroy; override;
    function Valid: Boolean;
    procedure SetVolume(Volume: Single);
    property FileName: string read FFileName;
    property IsAlias: Boolean read FIsAlias;
  end;

  { TTyroFileSounds }

  { The preload cache behind sound.load/sound.unload: decoded sound files kept
    resident and found by their resolved file name (case-insensitive). The list
    owns its entries, so dropping one unloads the raylib sound. sound.play()
    looks here first and plays a cheap alias when the file was preloaded, but it
    never adds to the cache itself; only sound.load() does. }
  TTyroFileSounds = class(TmnObjectList<TTyroFileSound>)
  public
    function Find(const AFileName: string): TTyroFileSound;
    function Load(const AFileName: string): TTyroFileSound;
    procedure Unload(const AFileName: string);
  end;

  { TTyroRayAudio }

  TTyroRayAudio = class(TRayAudio)
  protected
  public
    procedure Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single = 100; SampleRate: Integer = 44100; BitRate: Integer = 16);
  end;

  TTyroMelodySound = TTyroRayWave;

  { TRayMelodyChannel }

  TRayMelodyChannel = class(TMelodyChannel)
  public
    Amplitude: Single;
    SampleRate: Integer;
    BitRate: Integer;
    Sound: TTyroMelodySound; //currently playing
    PendingSound: TTyroMelodySound; //generated and waiting for its turn
    SpareSound: TTyroMelodySound; //retired, still draining; reused for the next generation
    Waveform: TWaveForm;
    procedure SetInstrument(Instrument: String); override;
    procedure SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single); override;
    function PlaySound: Boolean; override;
    function IsPlaying: Boolean; override;
    function StopSound: Boolean; override;

    procedure Prepare; override;
    procedure Update; override;
    procedure Unprepare; override;
    constructor Create(AMelody: TMelody); override;
    destructor Destroy; override;
  end;

  { TRayMelody }

  TRayMelody = class(TMelody)
  private
  protected
    procedure BeforePlay; override;
    procedure AfterPlay; override;
    function CheckTerminated: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateChannel: TMelodyChannel; override;
  end;

  { TLoopSignal }

  { The state of the looping voice, shared between the audio thread (reads it
    in TTyroLoopSound's callback) and the main thread (writes the pitch/volume
    targets). Plain values only, no object reference, so the callback keeps
    reading safely even in the instant after the voice that owns the stream is
    freed.

    The waveform is rendered at BaseFrequency into a two sample history
    (S0/S1) and the source cursor SrcPos walks that history at `Rate` samples
    per output sample, interpolating between the two neighbours. That is a
    resample, not a re-evaluate: every waveform retunes with Rate, including
    the stateful ones (Engine, Noise) that ignore the Index argument, and a
    pitch change only changes the slope of the cursor, so it never jumps phase.

    Reset/NextSample are the render core; keeping them on the record lets a
    headless test drive the exact code the callback runs. SrcIndex/SrcPos are
    wrapped back at WrapAt, a whole number of base cycles, so a loop can keep
    playing without the source cursor ever overflowing. }
  TLoopSignal = record
    Active: Boolean;
    Wave: TWaveformProc;
    SampleRate: Integer;
    BaseFrequency: Single;   //frequency the waveform is generated at
    Rate: Single;            //source cursor advance per output sample
    SrcIndex: Integer;       //integer part of the source cursor
    SrcPos: Double;          //fractional source cursor
    S0, S1: Single;          //source samples at SrcIndex and SrcIndex + 1
    WrapAt: Integer;         //source index the cursor wraps back from
    Gain: Single;            //envelope, ramps to avoid clicks
    TargetGain: Single;
    GainStep: Single;
    //(Re)seed the source history at sample 0. Does not touch Rate or Gain.
    procedure Reset;
    //One mono output sample; ramps Gain toward TargetGain first.
    function NextSample: Single;
  end;

  { TTyroLoopSound }

  { A continuously looping generated tone. RayLib has no loop for a decoded
    Sound/Music, and looping a buffer leaves a small silent gap at the wrap
    point. Instead this voice drives an AudioStream through a callback that
    generates samples on demand, so the tone is genuinely gapless. See
    TLoopSignal for how the pitch (sound.loopPitch) is applied. }
  TTyroLoopSound = class(TObject)
  private
    FStream: TAudioStream;
    FBaseFrequency: Single;
    FStopping: Boolean;
    FFinished: Boolean;
    FValid: Boolean;
  public
    constructor Create(Proc: TWaveformProc; Frequency, Volume: Single);
    destructor Destroy; override;
    procedure Update;
    procedure SetPitch(Frequency: Single);
    procedure SetVolume(Volume: Single);
    procedure Stop;
    property Valid: Boolean read FValid;
    property Finished: Boolean read FFinished;
  end;

var
  WaveForms: TWaveForms = nil;
  SoundCache: TTyroFileSounds = nil;

procedure PlayMML(const Song: TmmlSong);

procedure PlayWaveform(Proc: TWaveformProc; Freq, Duration: Single); overload;
procedure PlayWaveform(WaveformName: string; Freq, Duration: Single); overload;
//One-shot SFX from an audio file (raylib LoadSound: .wav) - sound.play().
//If the file was preloaded with LoadSoundFile it is played as an alias of the
//cached sound; otherwise it is decoded and reclaimed when it finishes.
procedure PlaySoundFile(const AFileName: string; AVolume: Single = 1.0);
//Preload a file into the permanent sound cache - sound.load().
procedure LoadSoundFile(const AFileName: string);
//Drop a preloaded file from the sound cache - sound.unload().
procedure UnloadSoundFile(const AFileName: string);

procedure UpdateMelodies;
procedure UpdateWaveforms;
procedure ShutdownMelodies;
procedure ShutdownWaveforms;

//A single looping voice, started/stopped by sound.loopPlay()/sound.stop().
procedure StartLoop(Proc: TWaveformProc; Frequency, Volume: Single); overload;
procedure StartLoop(WaveformName: string; Frequency, Volume: Single); overload;
procedure StopLoop;
procedure SetLoopPitch(Frequency: Single);
procedure SetLoopVolume(Volume: Single);
procedure UpdateLoopSounds;

function LFSRNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function TrueLFSRNoise(Index, SampleRate: Integer; Frequency: Single): Single;
function HashNoise(Index: Integer): Single;

function WhiteNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function PlaneNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Engine_Waveform(Index: Integer; SampleRate: Integer; Frequency: Single): Single;
function Noise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Sin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Piano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function GrandPiano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function OldPiano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Square_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Saw_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Tri_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Pulse25_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Pulse125_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Rect_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Dsin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Organ_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Bell_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Pluck_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Flute_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function FM_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function PinkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function DarkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Pew_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Explode_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Fire_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Boing_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Siren_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Bubble_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Kick_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Snare_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function HiHat_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Coin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Cash_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Phaser_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Zap_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Laser_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Chime_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Sweep_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
function Bee_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;

implementation

var
  PlayingMelodies: TList = nil;
  PlayingWaveforms: TList = nil;
  LoopSound: TTyroLoopSound = nil;

{ TTyroRayAudio }

procedure TTyroRayAudio.Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single; SampleRate: Integer; BitRate: Integer);
var
  aData: array of SmallInt;
  SampleCount, SampleSize: Integer;
  i: Integer;
  s: Int64;
  v: Smallint;
  {$ifdef FADE}
  WaveSamples: Integer;
  Starting, Ending: Integer;
  Delta: Single;
  {$endif}
begin
  if (Duration <= 0) or (SampleRate <= 0) or not Assigned(Proc) then
    Exit;
  aData := nil;
  SampleCount := Round(Duration * SampleRate);
  SampleSize := Sizeof(Smallint) * 8; // I use 16 bit only
  SetLength(aData, SampleCount);
  if Frequency <> 0 then
  begin
    Amplitude := Amplitude * ((Power(2, SampleSize) / 2) - 1) / 100;
    {$ifdef FADE}
    //Guard degenerate frequencies: round(Frequency)=0 would be a div-by-zero
    //and WaveSamples<=0 would make Delta = 100/0 = Inf (crash on Round(Inf)).
    if round(Frequency) > 0 then
      WaveSamples := SampleRate div round(Frequency)
    else
      WaveSamples := SampleRate;
    if WaveSamples <= 0 then
      WaveSamples := 1;
    Starting := WaveSamples * 3;
    //Keep the fade window inside the sound, a note shorter than 3 periods
    //would otherwise fade in over its whole length and be nearly silent.
    if Starting > SampleCount div 2 then
      Starting := SampleCount div 2;
    if Starting <= 0 then
      Starting := 1;
    Ending := SampleCount - Starting;
    Delta := 100 / Starting;
    {$endif}
    for i := 0 to SampleCount -1 do
    begin
      s := Round(Proc(i, SampleRate, Frequency) * Amplitude);
      //A waveform may overshoot +-1 a little, a wrapped SmallInt would click.
      if s > High(SmallInt) then
        s := High(SmallInt)
      else if s < Low(SmallInt) then
        s := Low(SmallInt);
      v := s;
      {$ifdef FADE}
      if i < Starting then
        v := Round(v * i * Delta / 100);
      if i > Ending then
        v := Round(v * (SampleCount - i) * Delta / 100);
      {$endif}
      aData[i] := v;
    end;
  end
  else
  begin
    for i := 0 to Length(aData) -1 do
      aData[i] := 0;
  end;
  if SampleCount > 0 then
    UpdateData(@aData[0], SampleCount);
end;

{ TTyroRayWave }

procedure TTyroRayWave.Generate(Proc: TWaveformProc; Frequency, Duration: Single; Amplitude: Single; SampleRate: Integer; BitRate: Integer);
var
  Wave: TWave;
  i: Integer;
  s: Int64;
  v: Smallint;
  {$ifdef FADE}
  WaveSamples: Integer;
  Starting, Ending: Integer;
  Delta: Single;
  {$endif}
  //aData: array of SmallInt;
  aData: PSmallInt;
begin
  if (Duration <= 0) or (SampleRate <= 0) or not Assigned(Proc) then
    Exit;
  Wave := Default(TWave);
  Wave.FrameCount := Round(Duration * SampleRate);
  Wave.SampleRate := SampleRate; // By default 44100 Hz
  Wave.SampleSize := Sizeof(Smallint) * 8; // I use 16 bit only
  Wave.Channels := 1;                  // By default 1 channel (mono)
  //aData := nil;
  //SetLength(aData, Wave.FrameCount);
  if Wave.FrameCount = 0 then
    Exit;
  aData := RayLib.MemAlloc(Wave.FrameCount * SizeOf(SmallInt));
  if aData = nil then
    raise Exception.Create('Unable to allocate generated waveform');
  if Frequency <> 0 then
  begin
    Amplitude := Amplitude * ((Power(2, Wave.SampleSize) / 2) - 1) / 100;

    {$ifdef FADE}
    //Guard degenerate frequencies: round(Frequency)=0 would be a div-by-zero
    //and WaveSamples<=0 would make Delta = 100/0 = Inf (crash on Round(Inf)).
    if round(Frequency) > 0 then
      WaveSamples := SampleRate div round(Frequency)
    else
      WaveSamples := SampleRate;
    if WaveSamples <= 0 then
      WaveSamples := 1;
    Starting := WaveSamples * 3;
    //Keep the fade window inside the sound, a note shorter than 3 periods
    //would otherwise fade in over its whole length and be nearly silent.
    if Starting > Integer(Wave.FrameCount) div 2 then
      Starting := Integer(Wave.FrameCount) div 2;
    if Starting <= 0 then
      Starting := 1;
    Ending := Integer(Wave.FrameCount) - Starting;
    Delta := 100 / Starting;
    {$endif}

    for i := 0 to Integer(Wave.FrameCount) -1 do
    begin
      s := Round(Proc(i, SampleRate, Frequency) * Amplitude);
      //A waveform may overshoot +-1 a little, a wrapped SmallInt would click.
      if s > High(SmallInt) then
        s := High(SmallInt)
      else if s < Low(SmallInt) then
        s := Low(SmallInt);
      v := s;
      {$ifdef FADE}
      if i < Starting then
        v := Round(v * i * Delta / 100);
      if i > Ending then
        v := Round(v * (Integer(Wave.FrameCount) - i) * Delta / 100);
      {$endif}
      aData[i] := v;
    end;
  end
  else
    for i := 0 to Integer(Wave.FrameCount) - 1 do
      aData[i] := 0;
  Wave.Data := aData;
  //ExportWave(Wave, PChar('c:\temp\'+IntTOStr(round(Frequency))+'.wav'));
  if Sound.FrameCount <> 0 then
    UnloadSound(Sound);
  Sound := Default(TSound);
  Sound := LoadSoundFromWave(Wave);
  //RayLib.MemFree(aData);
  //aData := nil;
  //Wave.Data := nil;
  UnloadWave(Wave);
  //aData := nil;
  if Wave.Data <> nil then //maybe move it to generate
  begin
    //RayLib.MemFree(Wave.Data); //* no it is our memory
    //Wave.Data := nil;
    //UnloadWave(Wave);
  end;
end;

{ TTyroFileSound }

constructor TTyroFileSound.CreateFromFile(const AFileName: string);
begin
  inherited Create(False);
  //TRaySound.Destroy only unloads a Sound with FrameCount > 0, so a failed
  //LoadSound (FrameCount 0) is freed safely by the inherited destructor.
  Sound := RayLib.LoadSound(PUTF8Char(AFileName));
  FFileName := AFileName;
end;

constructor TTyroFileSound.CreateAlias(Source: TTyroFileSound);
begin
  inherited Create(False);
  FIsAlias := True;
  if Source <> nil then
  begin
    FFileName := Source.FileName;
    //LoadSoundAlias shares the source's sample data; an alias is unloaded with
    //UnloadSoundAlias (see Destroy), never UnloadSound, so the source survives.
    if Assigned(RayLib.LoadSoundAlias) and (Source.Sound.FrameCount > 0) then
      Sound := LoadSoundAlias(Source.Sound);
  end;
end;

destructor TTyroFileSound.Destroy;
begin
  if FIsAlias then
  begin
    //An alias owns no data, so release it with UnloadSoundAlias and hand a
    //cleared Sound to the inherited destructor (which then does nothing).
    if Sound.FrameCount > 0 then
    begin
      StopSound(Sound);
      if Assigned(RayLib.UnloadSoundAlias) then
        UnloadSoundAlias(Sound)
      else
        UnloadSound(Sound);
      Sound := Default(TSound);
    end;
  end;
  inherited;
end;

function TTyroFileSound.Valid: Boolean;
begin
  Result := Sound.FrameCount > 0;
end;

procedure TTyroFileSound.SetVolume(Volume: Single);
begin
  SetSoundVolume(Sound, Volume);
end;

{ TTyroFileSounds }

function TTyroFileSounds.Find(const AFileName: string): TTyroFileSound;
var
  I: Integer;
begin
  Result := nil;
  if AFileName = '' then
    Exit;
  for I := 0 to Count - 1 do
    if SameText(Items[I].FileName, AFileName) then
    begin
      Result := Items[I];
      Exit;
    end;
end;

function TTyroFileSounds.Load(const AFileName: string): TTyroFileSound;
var
  Item: TTyroFileSound;
begin
  Result := Find(AFileName);
  if Result <> nil then
    Exit;
  //Same forgiving contract as PlaySoundFile: a missing or undecodable file is
  //ignored and the caller gets nil instead of an exception.
  if (AFileName = '') or not SysUtils.FileExists(AFileName) then
    Exit;
  RayLibSound.Open;
  Item := TTyroFileSound.CreateFromFile(AFileName);
  if not Item.Valid then
  begin
    Item.Free;
    Exit;
  end;
  Add(Item);
  Result := Item;
end;

procedure TTyroFileSounds.Unload(const AFileName: string);
var
  Item: TTyroFileSound;
  Index: Integer;
begin
  Item := Find(AFileName);
  if Item = nil then
    Exit;
  Index := IndexOf(Item);
  if Index >= 0 then
    Delete(Index); //the list owns the sound, so this unloads it
end;

procedure LoadSoundFile(const AFileName: string);
begin
  if SoundCache = nil then
    Exit;
  SoundCache.Load(AFileName);
end;

procedure UnloadSoundFile(const AFileName: string);
begin
  if SoundCache = nil then
    Exit;
  SoundCache.Unload(AFileName);
end;

procedure PlaySoundFile(const AFileName: string; AVolume: Single);
var
  Master: TTyroFileSound;
  Effect: TTyroFileSound;
begin
  //Same forgiving contract as PlayWaveform: a missing or undecodable file is
  //ignored instead of raising, so a typo in a game script cannot kill the run.
  if (AFileName = '') or not SysUtils.FileExists(AFileName) then
    Exit;
  if PlayingWaveforms = nil then
    Exit;
  RayLibSound.Open;
  //Only sound.load() puts a sound in the cache; sound.play never does. When the
  //file was preloaded, play a cheap alias of the cached sound (it shares the
  //sample data and survives the one-shot). Otherwise decode it here and let the
  //one-shot be reclaimed when it finishes, exactly as before.
  Master := nil;
  if SoundCache <> nil then
    Master := SoundCache.Find(AFileName);
  if Master <> nil then
  begin
    Effect := TTyroFileSound.CreateAlias(Master);
    try
      if Effect.Valid then
      begin
        if AVolume <> 1 then
          Effect.SetVolume(AVolume);
        Effect.Play;
        PlayingWaveforms.Add(Effect);
        Effect := nil;
      end
      else
      begin
        //No alias support in the loaded raylib: fall back to the cached sound.
        if AVolume <> 1 then
          Master.SetVolume(AVolume);
        Master.Play;
      end;
    finally
      Effect.Free;
    end;
  end
  else
  begin
    Effect := TTyroFileSound.CreateFromFile(AFileName);
    try
      if not Effect.Valid then
        Exit;
      if AVolume <> 1 then
        Effect.SetVolume(AVolume);
      Effect.Play;
      PlayingWaveforms.Add(Effect);
      Effect := nil;
    finally
      Effect.Free;
    end;
  end;
end;

{ TWaveForms }

procedure TWaveForms.Add(Name: string; Proc: TWaveformProc);
var
  WaveForm: TWaveForm;
begin
  WaveForm := TWaveForm.Create;
  WaveForm.Proc := Proc;
  WaveForm.Name := Name;
  inherited Add(WaveForm);
end;

{ TRayMelodyChannel }

procedure TRayMelodyChannel.SetInstrument(Instrument: String);
begin
  inherited;
  if Instrument = '' then
    Waveform := WaveForms[0]
  else
  begin
    Waveform := WaveForms.Find(Instrument);
    if Waveform = nil then
      raise EMelodyException.Create('Waveform not exists ' + Instrument);
  end;
end;

procedure TRayMelodyChannel.SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single);
begin
  inherited;
  if Waveform = nil then
    raise EMelodyException.Create('Waveform not defined');
  //Generate into the free buffer, not the one currently playing, so preparing a
  //note ahead of time never cuts the sound that is already on the air.
  //With very short notes the recycled buffer could still be draining, stop it
  //first so its wave is never freed while the audio thread is reading it.
  if PendingSound.IsPlaying then
    PendingSound.Stop;
  PendingSound.Generate(Waveform.Proc, Frequency, Duration, Amplitude, SampleRate, BitRate);
end;

function TRayMelodyChannel.PlaySound: Boolean;
var
  tmp: TTyroMelodySound;
begin
  inherited;
  //Rotate the three buffers: the freshly generated note becomes current, the
  //finished one is retired (it may still be draining in the audio device), and
  //the oldest, long drained buffer is handed back for the next generation.
  tmp := SpareSound;
  SpareSound := Sound;
  Sound := PendingSound;
  PendingSound := tmp;
  Sound.Play;
  Result := True;
end;

function TRayMelodyChannel.IsPlaying: Boolean;
begin
  Result := Sound.IsPlaying;
end;

function TRayMelodyChannel.StopSound: Boolean;
begin
  inherited;
  Result := Sound.IsPlaying;
  Sound.Stop;
end;

procedure TRayMelodyChannel.Prepare;
begin
  inherited Prepare;
  Amplitude := 100;
end;

procedure TRayMelodyChannel.Update;
begin
  inherited Update;
  Sound.Update;
end;

procedure TRayMelodyChannel.Unprepare;
begin
  inherited;
end;

constructor TRayMelodyChannel.Create(AMelody: TMelody);
begin
  inherited;
  SampleRate := cDefaultSampleRate;
  BitRate := 16;
  Sound := TTyroMelodySound.Create(False);
  PendingSound := TTyroMelodySound.Create(False);
  SpareSound := TTyroMelodySound.Create(False);
  SetInstrument('');
end;

destructor TRayMelodyChannel.Destroy;
begin
  FreeAndNil(Sound);
  FreeAndNil(PendingSound);
  FreeAndNil(SpareSound);
  inherited;
end;

{ TMelody }

procedure TRayMelody.BeforePlay;
begin
  inherited;
  RayLibSound.Open;
end;

procedure TRayMelody.AfterPlay;
begin
  inherited AfterPlay;
  RayLibSound.Close;
end;

function TRayMelody.CheckTerminated: Boolean;
begin
  Result := inherited CheckTerminated;
  //Sleep(1);
end;

constructor TRayMelody.Create;
begin
  inherited Create;
end;

destructor TRayMelody.Destroy;
begin
  inherited Destroy;
end;

function TRayMelody.CreateChannel: TMelodyChannel;
begin
  Result := TRayMelodyChannel.Create(Self);
end;

procedure PlayMML(const Song: TmmlSong);
var
  Melody: TRayMelody;
begin
  if Length(Song) = 0 then
    Exit;
  if PlayingMelodies = nil then
    Exit;
  Melody := TRayMelody.Create;
  try
    Melody.BeginPlay(Song);
    if Melody.Playing then
    begin
      PlayingMelodies.Add(Melody);
      Melody := nil;
    end;
  finally
    Melody.Free;
  end;
end;

procedure PlayWaveform(Proc: TWaveformProc; Freq, Duration: Single);
var
  Wave: TTyroRayWave;
begin
  if (Duration <= 0) or (Freq < 0) or not Assigned(Proc) then
    Exit;
  if PlayingWaveforms = nil then
    Exit;
  RayLibSound.Open;
  Wave := TTyroRayWave.Create;
  try
    // Lua's music.sound duration is expressed in milliseconds.
    Wave.Generate(Proc, Freq, Duration / 1000, 100);
    Wave.Play;
    PlayingWaveforms.Add(Wave);
    Wave := nil;
  finally
    Wave.Free;
  end;
end;

procedure PlayWaveform(WaveformName: string; Freq, Duration: Single); overload;
var
  wf: TWaveForm;
  wp: TWaveformProc;
begin
  wf :=WaveForms.Find(WaveformName);
  if wf = nil then
    wp := @Sin_Waveform
  else
    wp := wf.Proc;
  PlayWaveform(wp, Freq, Duration);
end;

procedure UpdateMelodies;
var
  I: Integer;
  Melody: TRayMelody;
begin
  if PlayingMelodies = nil then
    Exit;
  for I := PlayingMelodies.Count - 1 downto 0 do
  begin
    Melody := TRayMelody(PlayingMelodies[I]);
    if not Melody.UpdatePlay then
    begin
      PlayingMelodies.Delete(I);
      Melody.Free;
    end;
  end;
end;

procedure UpdateWaveforms;
var
  I: Integer;
  Wave: TTyroRayWave;
begin
  if PlayingWaveforms <> nil then
  begin
    for I := PlayingWaveforms.Count - 1 downto 0 do
    begin
      Wave := TTyroRayWave(PlayingWaveforms[I]);
      if not Wave.IsPlaying then
      begin
        PlayingWaveforms.Delete(I);
        Wave.Free;
      end;
    end;
  end;
  //The looping voice is independent of the one-shot waveforms; it is kept
  //streaming here so sound.loopPlay() plays without gaps.
  UpdateLoopSounds;
end;

procedure ShutdownWaveforms;
var
  I: Integer;
begin
  FreeAndNil(LoopSound);
  //Free the one-shot voices (aliases share the cached sounds' data) before the
  //cache itself, so no alias is left pointing at unloaded sample data.
  if PlayingWaveforms <> nil then
  begin
    for I := PlayingWaveforms.Count - 1 downto 0 do
      TObject(PlayingWaveforms[I]).Free;
    PlayingWaveforms.Clear;
  end;
  FreeAndNil(SoundCache);
end;

procedure ShutdownMelodies;
var
  I: Integer;
begin
  if PlayingMelodies = nil then
    Exit;
  for I := PlayingMelodies.Count - 1 downto 0 do
    TObject(PlayingMelodies[I]).Free;
  PlayingMelodies.Clear;
end;

{ TLoopSignal }

//Was: evaluate the waveform straight at the rounded cursor. That dropped every
//waveform that ignores Index (Engine, Noise, White) out of pitch control
//completely, and resampled the rest with nearest neighbour, which aliases when
//Rate is not 1. The two sample history below resamples properly instead.

const
  //A loop can play for days, so the cursor is wrapped back before it can reach
  //the top of an Integer. At 44.1 kHz this is about 6.7 hours of looping; the
  //application is built with {$R+}{$Q+}, so without it the voice would raise
  //ERangeError on the audio thread instead of wrapping.
  cLoopWrapLimit = 1 shl 30;

procedure TLoopSignal.Reset;
var
  Period: Integer;
begin
  SrcIndex := 0;
  SrcPos := 0;
  //Wrap as a whole number of cycles of the base waveform, so neither the
  //resampler (SrcPos moves with SrcIndex, the interpolation fraction is
  //untouched) nor the phase of a periodic waveform moves at the wrap.
  //Period is clamped to the wrap limit: below ~0.04 Hz a cycle no longer fits,
  //and then the wrap is just a plain sample count.
  if (SampleRate > 0) and (BaseFrequency >= SampleRate / cLoopWrapLimit) then
    Period := Round(SampleRate / BaseFrequency)
  else
    Period := cLoopWrapLimit;
  if Period < 1 then
    Period := 1;
  WrapAt := (cLoopWrapLimit div Period) * Period;
  if Assigned(Wave) and (SampleRate > 0) then
  begin
    S0 := Wave(0, SampleRate, BaseFrequency);
    S1 := Wave(1, SampleRate, BaseFrequency);
  end
  else
  begin
    S0 := 0;
    S1 := 0;
  end;
end;

function TLoopSignal.NextSample: Single;
var
  v, fr: Single;
begin
  if not Active or not Assigned(Wave) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  if Gain < TargetGain then
  begin
    Gain := Gain + GainStep;
    if Gain > TargetGain then
      Gain := TargetGain;
  end
  else if Gain > TargetGain then
  begin
    Gain := Gain - GainStep;
    if Gain < TargetGain then
      Gain := TargetGain;
  end;
  //Consume every source sample the cursor walked past, so SrcPos always ends
  //up inside [SrcIndex, SrcIndex + 1). Generating ahead by one sample is what
  //makes the interpolation below possible; at Rate = 1 it costs exactly the one
  //call per output sample the old code made, and emits the same values.
  while SrcPos >= SrcIndex + 1 do
  begin
    S0 := S1;
    SrcIndex := SrcIndex + 1;
    S1 := Wave(SrcIndex + 1, SampleRate, BaseFrequency);
  end;
  //Wrap the cursor back before it can reach the top of an Integer. SrcIndex
  //and SrcPos move together, so the interpolation fraction, and therefore the
  //output, is unchanged by it.
  if SrcIndex >= WrapAt then
  begin
    SrcIndex := SrcIndex - WrapAt;
    SrcPos := SrcPos - WrapAt;
  end;
  fr := SrcPos - SrcIndex;
  v := S0 + (S1 - S0) * fr;
  SrcPos := SrcPos + Rate;
  Result := v * Gain;
end;

var
  LoopSignal: TLoopSignal;

//Runs on the audio thread. Fills `Frames` mono float samples from the shared
//render core, so a pitch change is a continuous resample of what is already
//playing: no gap at the loop point, no phase jump when the pitch moves.
procedure LoopAudioCallback(var Buffer; Frames: Cardinal); cdecl;
var
  Out32: PSingle;
  I, N: Integer;
begin
  Out32 := PSingle(@Buffer);
  N := Integer(Frames);
  for I := 0 to N - 1 do
    Out32[I] := LoopSignal.NextSample;
end;

{ TTyroLoopSound }

constructor TTyroLoopSound.Create(Proc: TWaveformProc; Frequency, Volume: Single);
begin
  inherited Create;
  FValid := False;
  FStopping := False;
  FFinished := False;
  if not Assigned(Proc) then
    Exit;
  if Frequency < 1 then
    Frequency := 1;
  FBaseFrequency := Frequency;
  RayLibSound.Open;
  //32 bit float mono: our callback feeds it, so playback never underruns at a
  //loop point the way a decoded Music stream does.
  FStream := LoadAudioStream(cDefaultSampleRate, 32, 1);
  if FStream.Buffer = nil then
    Exit;
  //Publish the signal before the stream starts so the first callback sees it.
  LoopSignal.Active := True;
  LoopSignal.Wave := Proc;
  LoopSignal.SampleRate := cDefaultSampleRate;
  LoopSignal.BaseFrequency := FBaseFrequency;
  LoopSignal.Rate := 1;
  LoopSignal.Gain := 0;
  LoopSignal.TargetGain := 1;
  LoopSignal.GainStep := 1 / (0.008 * cDefaultSampleRate); //8 ms attack
  LoopSignal.Reset;
  SetAudioStreamVolume(FStream, Volume);
  SetAudioStreamCallback(FStream, LoopAudioCallback);
  PlayAudioStream(FStream);
  FValid := True;
end;

destructor TTyroLoopSound.Destroy;
begin
  if FStream.Buffer <> nil then
  begin
    LoopSignal.Active := False;
    StopAudioStream(FStream);
    SetAudioStreamCallback(FStream, nil);
    UnloadAudioStream(FStream);
    FStream := Default(TAudioStream);
  end;
  inherited;
end;

procedure TTyroLoopSound.Update;
begin
  if not FValid then
    Exit;
  //The fade-out has completed; the caller can now free this voice.
  if FStopping and (LoopSignal.Gain <= 0.0001) then
    FFinished := True;
end;

procedure TTyroLoopSound.SetPitch(Frequency: Single);
begin
  if not FValid then
    Exit;
  if Frequency < 1 then
    Frequency := 1;
  LoopSignal.Rate := Frequency / FBaseFrequency;
end;

procedure TTyroLoopSound.SetVolume(Volume: Single);
begin
  if not FValid then
    Exit;
  if Volume < 0 then
    Volume := 0
  else if Volume > 1 then
    Volume := 1;
  SetAudioStreamVolume(FStream, Volume);
end;

procedure TTyroLoopSound.Stop;
begin
  if not FValid then
    Exit;
  //Ramp down instead of cutting the stream, then let Update free us.
  FStopping := True;
  LoopSignal.TargetGain := 0;
end;

procedure StartLoop(Proc: TWaveformProc; Frequency, Volume: Single);
var
  Loop: TTyroLoopSound;
begin
  //Replace any current voice outright (a retrigger is allowed to click).
  FreeAndNil(LoopSound);
  Loop := TTyroLoopSound.Create(Proc, Frequency, Volume);
  if Loop.Valid then
    LoopSound := Loop
  else
    Loop.Free;
end;

procedure StartLoop(WaveformName: string; Frequency, Volume: Single);
var
  wf: TWaveForm;
begin
  if WaveformName = '' then
    StartLoop(@Sin_Waveform, Frequency, Volume)
  else
  begin
    wf := WaveForms.Find(WaveformName);
    if wf = nil then
      StartLoop(@Sin_Waveform, Frequency, Volume)
    else
      StartLoop(wf.Proc, Frequency, Volume);
  end;
end;

procedure StopLoop;
begin
  //Fade out; UpdateLoopSounds frees the voice once the envelope reaches zero.
  if LoopSound <> nil then
    LoopSound.Stop;
end;

procedure SetLoopPitch(Frequency: Single);
begin
  if LoopSound <> nil then
    LoopSound.SetPitch(Frequency);
end;

procedure SetLoopVolume(Volume: Single);
begin
  if LoopSound <> nil then
    LoopSound.SetVolume(Volume);
end;

procedure UpdateLoopSounds;
begin
  if LoopSound = nil then
    Exit;
  LoopSound.Update;
  if LoopSound.Finished then
    FreeAndNil(LoopSound);
end;

function LFSRNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Period, T: Integer;
  LFSR: Word;
  Bit: Word;
  i: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := GetRandomValue(-100,+100) / 100;
    Exit;
  end;
  Period := Round(SampleRate / Frequency);
  if Period < 1 then Period := 1;
  T := Index div Period;
  // seed LFSR based on period; the multiply wraps on purpose, so keep it in
  // UInt64 and take the low word instead of overflowing a 32 bit integer.
  LFSR := Word((UInt64(T) * 131542391 + 1009) and $FFFF);
  if LFSR = 0 then LFSR := $ACE1;
  // advance LFSR a few steps to get variation
  for i := 0 to (T mod 8) do
  begin
    Bit := ((LFSR shr 0) xor (LFSR shr 2) xor (LFSR shr 3) xor (LFSR shr 5)) and 1;
    LFSR := (LFSR shr 1) or (Bit shl 15);
  end;
  // output bit as -1 or 1 for harsher tone
  if (LFSR and 1) = 1 then
    Result := 1.0
  else
    Result := -1.0;
end;

var
  // Global/Class state variables
  LFSR_State: Word = $ACE1; // Non-zero seed
  LFSR_Phase: Single = 0.0;
  LFSR_HoldValue: Single = 0.0;

function TrueLFSRNoise(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Bit: Word;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := GetRandomValue(-100,+100) / 100;
    Exit;
  end;

  // Advance the phase based on frequency
  LFSR_Phase := LFSR_Phase + (Frequency / SampleRate);

  // Every time phase wraps 1.0, advance the LFSR state and grab a new sample
  while LFSR_Phase >= 1.0 do
  begin
    LFSR_Phase := LFSR_Phase - 1.0;

    // 16-bit maximal length LFSR (taps at 15, 13, 12, 10)
    Bit := ((LFSR_State shr 0) xor (LFSR_State shr 2) xor
            (LFSR_State shr 3) xor (LFSR_State shr 5)) and 1;

    LFSR_State := (LFSR_State shr 1) or (Bit shl 15);

    // Output as 8-bit style noise (smoother than 1-bit, grittier than 16-bit)
    LFSR_HoldValue := ((LFSR_State and $FF) - 128) / 128.0;
  end;

  // Output the held value (this creates the "pitched" effect)
  Result := LFSR_HoldValue;
end;

function Noise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
begin
  // LFSR-based pitched noise (periodic like NES noise)
  Result := TrueLFSRNoise(Index, SampleRate, Frequency);
end;

function PlaneNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Period, T: Integer;
  Hash: UInt64;
  NoiseVal: Single;
  PropPhase: Single;
  PropVal: Single;
begin
  // 1. Generate the Engine Noise (Pitched)
  Period := Round(SampleRate / Frequency);
  if Period < 1 then Period := 1;

  T := Index div Period;

  // High-quality integer hash to scramble T. Kept in UInt64 and masked back to
  // 32 bits at every step that can wrap, for the same reason HashNoise is: the
  // shift below and the multiply after it overflow on purpose, and with the
  // {$Q+} this application is built with a UInt32 operation raises ERangeError
  // instead of wrapping. The masked result is bit for bit what a 32 bit
  // wraparound would give.
  Hash := Cardinal(T);
  Hash := (Hash xor 61) xor (Hash shr 16);
  Hash := (Hash + ((Hash shl 3) and $FFFFFFFF)) and $FFFFFFFF;
  Hash := Hash xor (Hash shr 4);
  Hash := (Hash * $27d4eb2d) and $FFFFFFFF;
  Hash := Hash xor (Hash shr 15);

  // Output harsh 1-bit noise for that raspy 8-bit engine texture
  if (Hash and 1) = 1 then
    NoiseVal := 1.0
  else
    NoiseVal := -1.0;

  // 2. Generate the Propeller "Chug" (Amplitude Modulation)
  // The propeller spins at 1/4th the speed of the noise frequency.
  // (e.g., if Frequency is 120Hz, the propeller chops at 30Hz)
  PropPhase := Frac((Index / SampleRate) * (Frequency * 0.25));

  // Create a pulse wave. It's "ON" 40% of the time, and "Muffled" 60%.
  if PropPhase < 0.4 then
    PropVal := 1.0        // Loud
  else
    PropVal := 0.2;       // Muffled (but not totally silent)

  // 3. Combine them
  Result := NoiseVal * PropVal;
end;

var
  LP1: Single = 0.0;
  LP2: Single = 0.0;
  LP3: Single = 0.0;  // cascaded lowpass states for the roar
  WobLP: Single = 0.0;          // lowpass for the slow wobble

function Engine_Waveform(Index: Integer; SampleRate: Integer; Frequency: Single): Single;
const
  RoarLevel   = 0.9;    // noise rumble volume
  WobbleDepth = 0.25;   // how much roar throbs
var
  dt, k: Single;
  RoarCutoff, WobbleRate: Single;
  Roar, Wobble, n: Single;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
    Exit(0);

  dt := 1.0 / SampleRate;

  // Roar cutoff scales with RPM so higher revs sound brighter/harsher
  RoarCutoff := 250.0 + Frequency * 1.6;
  if RoarCutoff > 2800.0 then
    RoarCutoff := 2800.0;
  if RoarCutoff < 150.0 then
    RoarCutoff := 150.0;

  // Wobble/throb rate scales with RPM
  WobbleRate := Frequency * 0.06;
  if WobbleRate > 60.0 then WobbleRate := 60.0;
  if WobbleRate < 6.0 then WobbleRate := 6.0;

  // Roar: white noise through 3 cascaded one-pole lowpasses
  n := GetRandomValue(-100, +100) / 200;   // +/-0.5
  k := 1 - Exp(-2 * Pi * RoarCutoff * dt);
  LP1 := LP1 + k * (n - LP1);
  LP2 := LP2 + k * (LP1 - LP2);
  LP3 := LP3 + k * (LP2 - LP3);
  Roar := LP3 * 4.0;

  // Slow wobble driven by filtered noise, rate follows RPM
  n := GetRandomValue(-100, +100) / 200;
  k := 1 - Exp(-2 * Pi * WobbleRate * dt);
  WobLP := WobLP + k * (n - WobLP);
  Wobble := 1.0 - WobbleDepth * (WobLP * 2.0);

  Result := RoarLevel * Roar * Wobble;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

function WhiteNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
begin
  Result := GetRandomValue(-100,+100) / 100;
end;

function Sin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
begin
  //The old version mixed 'Index mod Round(period)' with '/ period' (unrounded),
  //which made the phase jump every cycle and caused an audible glitch.
  //A sine is naturally periodic, no wrapping is needed at all.
  if (Frequency <= 0) or (SampleRate <= 0) then
    Result := 0
  else
    Result := Sin(2 * Pi * Frequency * Index / SampleRate);
  //WriteLn('Sample = ' + FloatTOStr(Sample) + ' WaveSamples = '+ FloatToStr(wavesamples) + ' Result: ' + FloatToStr(Result));
end;

//Additive piano: inharmonic partials, per-partial decay, hammer transient.
//The old version was (sin f - sin 2f - sin f/2) with one shared envelope:
//  - exp(-log10(50)*t/3) only fell to 18% after 3s (log10, not ln), so notes
//    sounded sustained like an organ instead of dying.
//  - the f/2 term is a subharmonic, pianos do not have one, and it was as loud
//    as the fundamental so the pitch read an octave low and muddy.
//  - one envelope for every partial kept the timbre static; a real piano loses
//    its brightness first because the upper partials decay faster.
function Piano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  Inharmonicity = 0.0004; //string stiffness, partials sit slightly sharp of k*f
  PartialCount = 8;
var
  t, Rate, Sum, Norm, Amp, Decay, Fk, Attack: Double;
  k: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Rate := 0.7 + Frequency / 880; //higher notes die quicker
  Attack := 1 - Exp(-t * 400);   //~2.5ms hammer attack
  Sum := 0;
  Norm := 0;
  for k := 1 to PartialCount do
  begin
    Fk := Frequency * k * Sqrt(1 + Inharmonicity * k * k);
    if Fk >= SampleRate / 2 then
      Break;
    Amp := 1 / (k * Sqrt(k));                      //rolls off faster than 1/k
    Decay := Exp(-Rate * (0.6 + 0.5 * k) * t);     //brightness dies first
    Sum := Sum + Amp * Sin(2 * Pi * Fk * Index / SampleRate) * Decay;
    Norm := Norm + Amp;
  end;
  if Norm > 0 then
    Sum := Sum / Norm;
  //Hammer noise at the start of the note
  Result := (Sum + HashNoise(Index) * Exp(-t * 150) * 0.12) * Attack;
end;

//The original piano generator, kept under a separate name so it can be A/B
//compared with the additive one above. It is a fixed (sin f - sin 2f - sin f/2)
//wave times a single envelope: exp(-log10(50)*t/3) is log10, not ln, so it only
//falls to ~18% after 3s, and the f/2 term is a subharmonic pianos do not have.
//ref: http://web.mit.edu/6.02/www/s2007/lab2.pdf
function OldPiano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  a, b,
  Sample, Fade: Single;
begin
//https://stackoverflow.com/questions/20037947/fade-out-function-of-audio-between-samplerate-changes
//  if not connected then
  fade := exp(-log10(50) * index / SampleRate / 3); //fadeout
  sample := sin(index * (2 * pi) * frequency / SampleRate);
  a := sin(index * (2 * pi) * frequency * 2 / SampleRate);
  b := sin(index * (2 * pi) * frequency / 2 / SampleRate);
  sample := (sample - a - b) / 3;
  Result := sample * fade;
end;

//Grand piano, a fuller Piano: 16 partials rolling off at 1/k instead of
//1/k^1.5 so it is brighter, each partial with its own decay (the bass rings
//longer than the treble), every partial shared between two strings detuned by
//~1.4 cents so the note beats like a real unison, and a hammer noise burst.
//Sum/Norm is normalized by the sum of the amplitudes, so it stays inside +-1
//by construction, and the Gain*Sum + Noise*hammer result is measured against
//+-1 by test_waveforms at 110/440/3000 Hz over a whole 1.5s note. Gain brings
//the level next to Piano so an A/B does not just compare loudness.
function GrandPiano_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  Inharmonicity = 0.00025; //stiff strings, partials sit slightly sharp of k*f
  PartialCount = 16;
  Unison = 0.0008;         //detune of the second string, ~1.4 cents
  Gain = 1.3;              //makeup to match the Piano level
  Noise = 0.1;             //hammer noise on top of the gain
var
  t, Rate, Sum, Norm, Amp, Decay, Fk, Attack, Hammer, Beat: Double;
  k: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Rate := 0.55 + Frequency / 700; //bass rings longer than treble
  Attack := 1 - Exp(-t * 500);    //~2ms hammer attack
  Hammer := Exp(-t * 250);        //hammer noise over the first ~4ms
  Sum := 0;
  Norm := 0;
  for k := 1 to PartialCount do
  begin
    Fk := Frequency * k * Sqrt(1 + Inharmonicity * k * k);
    if Fk >= SampleRate / 2 then
      Break;
    Amp := 1 / k;
    Decay := Exp(-Rate * (0.5 + 0.55 * k) * t);
    //(sin a + sin b) / 2 for two unison strings = sin * cos of their beating
    Beat := Cos(2 * Pi * Fk * Unison * t);
    Sum := Sum + Amp * Sin(2 * Pi * Fk * Index / SampleRate) * Decay * Beat;
    Norm := Norm + Amp;
  end;
  if Norm > 0 then
    Sum := Sum / Norm;
  Result := (Gain * Sum + Noise * Hammer * HashNoise(Index)) * Attack;
end;

// Where in the wave Index stands, 0..1. Returns -1 only for a frequency or
// sample rate that has no meaningful period.
function WavePosition(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  //Was '(Index mod Round(SampleRate/Frequency)) / Round(...)' which quantized
  //the pitch (440 Hz became 441 Hz). Frac() keeps the exact frequency.
  Result := -1;
  if (Frequency <= 0) or (SampleRate <= 0) then
    Exit;
  Phase := Frequency; //widen to Double before multiplying by Index
  Result := Frac(Phase * Index / SampleRate);
end;

function Square_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.5 then
    Result := 1
  else
    Result := -1;
end;

function Saw_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else
    Result := (Pos * 2) - 1;
end;

function Tri_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.5 then
    Result := (Pos * 4) - 1
  else
    Result := 3 - (Pos * 4);
end;

{ --- deterministic noise helpers -------------------------------------------
  A waveform proc has no memory between calls, so the noise layers derive their
  randomness from the index. Same index always gives the same value, which also
  makes an MML note render the same again on every play. }

function HashNoise(Index: Integer): Single;
var
  h: UInt64;
begin
  //A 32 bit wrap is intended here. Kept in UInt64 and masked so it wraps the
  //same way but does not trip the overflow/range checks the application is
  //built with ({$Q+}, {$R+}), which used to raise ERangeError on this line.
  h := (UInt64(Cardinal(Index)) * 1664525 + 1013904223) and $FFFFFFFF;
  h := h xor (h shr 13);
  h := (h * 1274126177) and $FFFFFFFF;
  Result := (h and $7FFFFFFF) / 1073741824 - 1;
end;

{ --- new waveforms --------------------------------------------------------- }

// NES/Game Boy style pulse, 25% duty (thinner than Square)
function Pulse25_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.25 then
    Result := 1
  else
    Result := -1;
end;

// Even thinner 12.5% duty pulse, very nasal/chip-like
function Pulse125_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else if Pos < 0.125 then
    Result := 1
  else
    Result := -1;
end;

// Rectified sine, Abs(Sin), hollow organ-ish tone
function Rect_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else
    Result := Abs(Sin(2 * Pi * Pos));
end;

// Sine shaped by its own absolute value, smoother than square with a little grit
function Dsin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Pos: Single;
  s: Single;
begin
  Pos := WavePosition(Index, SampleRate, Frequency);
  if Pos < 0 then
    Result := 0
  else
  begin
    s := Sin(2 * Pi * Pos);
    Result := s * Abs(s);
  end;
end;

// Additive harmonics, Hammond-style organ. Normalized by the sum of weights.
function Organ_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  Result := (Sin(Phase) + 0.6 * Sin(2 * Phase) + 0.4 * Sin(3 * Phase) + 0.3 * Sin(4 * Phase)) / 2.3;
end;

// Inharmonic partials each with its own decay, struck-bell character.
function Bell_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase, t: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  t := Index / SampleRate;
  Result := (Sin(Phase) * Exp(-1.5 * t)
           + 0.6 * Sin(2.76 * Phase) * Exp(-2.8 * t)
           + 0.4 * Sin(5.40 * Phase) * Exp(-4.2 * t)
           + 0.25 * Sin(8.93 * Phase) * Exp(-6.0 * t)) / 2.25;
end;

// Karplus-Strong flavored pluck: harmonics decay faster the higher they are,
// which is what makes a plucked string lose its brightness over time.
function Pluck_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  f, MaxH, k: Integer;
  Sum, Norm, PhaseStep, t, Decay: Double;
begin
  Result := 0;
  if (Frequency <= 0) or (SampleRate <= 0) then
    Exit;
  f := Round(Frequency);
  if f < 1 then
    f := 1;
  //Only harmonics below Nyquist can be heard, cap the count for speed.
  MaxH := (SampleRate div 2) div f;
  if MaxH > 24 then
    MaxH := 24;
  if MaxH < 1 then
  begin
    Result := Sin(2 * Pi * Frequency * Index / SampleRate);
    Exit;
  end;
  PhaseStep := 2 * Pi * Frequency / SampleRate;
  t := Index / SampleRate;
  Sum := 0;
  Norm := 0;
  for k := 1 to MaxH do
  begin
    Decay := Exp(-t * k * 3.0);
    Sum := Sum + Sin(PhaseStep * k * Index) * Decay / k;
    Norm := Norm + 1 / k;
  end;
  Result := Sum / Norm;
end;

// Mostly sine with soft upper harmonics plus a breath of noise.
function Flute_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  Result := (Sin(Phase) + 0.15 * Sin(2 * Phase) + 0.05 * Sin(3 * Phase)) / 1.3
          + HashNoise(Index) * 0.02;
end;

// Two operator FM, one function covers metal/brass/electric-piano flavors.
function FM_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  Phase: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  Phase := 2 * Pi * Frequency * Index / SampleRate;
  Result := Sin(Phase + 2.0 * Sin(2 * Phase));
end;

// Voss style pink noise: one random layer per octave, held for 2^k samples.
function PinkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  k: Integer;
  Sum: Single;
begin
  Sum := 0;
  for k := 0 to 6 do
    Sum := Sum + HashNoise(Index shr k) / (k + 1);
  Result := Sum / 2.593; //sum of 1/(k+1) for k = 0..6
end;

// Brown-ish noise: octave layers with more weight on the slow (low frequency)
// ones. Summing the weights stays just under 2, so the output cannot exceed 1.
function DarkNoise_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  k: Integer;
  Sum, Weight: Double;
begin
  Sum := 0;
  Weight := 1;
  for k := 0 to 7 do
  begin
    Sum := Sum + HashNoise(Index shr k) * Weight;
    Weight := Weight / 2;
  end;
  Result := Sum / 2;
end;

// Missile launch "pew": a fast descending chirp (the rocket motor kicking in)
// layered with a noise whoosh that dies away, so a short note reads as a launch
// rather than a plain laser blip. The sweep is expressed as an integrated
// phase (t + 3*tau*(1 - exp(-t/tau))) so it stays sample-accurate and
// deterministic; Frequency is the pitch the sweep settles on.
function Pew_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SweepDepth = 3.0;   //starts SweepDepth+1 times higher, drops to the base pitch
  SweepTau = 0.08;    //how fast the pitch falls, seconds
  DecayTau = 0.18;    //overall amplitude decay, seconds
  Whoosh = 0.45;      //noise level mixed under the chirp
var
  t, Phase, Sweep, Noise, Envelope: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  //Instantaneous frequency f(t) = Frequency * (1 + SweepDepth*exp(-t/SweepTau)),
  //integrated to get the phase; the exp integral is analytic, no per-sample
  //state needed.
  Phase := 2 * Pi * Frequency * (t + SweepDepth * SweepTau * (1 - Exp(-t / SweepTau)));
  //Soft 2ms onset keeps the transient from clicking, then a rocket-motor decay.
  Envelope := (1 - Exp(-t * 500)) * Exp(-t / DecayTau);
  Sweep := Sin(Phase);
  //Deterministic noise, brighter right after launch and gone with the whoosh.
  Noise := (HashNoise(Index) * 0.7 + HashNoise(Index div 3) * 0.3) * Exp(-t / (SweepTau * 1.5));
  Result := (Sweep + Noise * Whoosh) * Envelope;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Enemy explosion "Trrrr": a broadband bang that dies in tens of milliseconds,
// followed by a dark rumble tail chopped by a fast stutter gate, so the decay
// rattles instead of fading smoothly. Frequency only tunes the rattle rate
// (higher note = faster chop), the blast itself is unpitched. Everything is
// derived from HashNoise, so the same note always renders the same explosion.
function Explode_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  BlastTau = 0.035;   //initial bang decay, seconds
  TailTau = 0.32;     //rumble tail decay, seconds
  GateOn = 1.0;       //stutter pass level
  GateOff = 0.22;     //stutter chop level, low but not silence
var
  t, RattlePhase, Gate, Blast, Tail, Rumble, Weight: Double;
  k: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  //The bang: full spectrum noise burst, the loudest part of the sound.
  Blast := HashNoise(Index) * Exp(-t / BlastTau);
  //The tail: octave layers of hashed noise (DarkNoise style), slow and dark
  //like debris and smoke after the blast.
  Rumble := 0;
  Weight := 1;
  for k := 0 to 5 do
  begin
    Rumble := Rumble + HashNoise(Index shr k) * Weight;
    Weight := Weight / 2;
  end;
  Rumble := Rumble / 1.96875; //sum of the weights, keeps it inside +-1
  Tail := Rumble * Exp(-t / TailTau);
  //Trrrr: chop the tail with a square gate. Frac of real time, so the stutter
  //rate is exact and the gate does not drift with the sample count.
  RattlePhase := Frac(t * (Frequency * 0.5 + 8));
  if RattlePhase < 0.4 then
    Gate := GateOn
  else
    Gate := GateOff;
  Result := Blast + Tail * Gate;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Fire crackle: a bed of hashed noise for the hiss and the low roar, with
// short noise pops on top for the crackles. Frequency controls the crackle
// rate (higher note = faster, denser pops), so a game can retune the ambience
// as the scene gets hotter. Everything comes from HashNoise, so it renders the
// same again on every play and is safe to loop.
function Fire_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  HissLevel = 0.52;    //continuous bed of the fire
  PopLevel = 0.62;     //crackles on top
  PopThreshold = 0.42;
var
  t, Hiss, Rumble, Pop, Phase: Double;
  k, Period, Block: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;

  //Three octaves of hashed noise: fine hiss, a mid layer and a slow
  //breathing one. The swell keeps it from sounding like radio static.
  Hiss := (HashNoise(Index) * 0.45
         + HashNoise(Index shr 2) * 0.33
         + HashNoise(Index shr 6) * 0.22) * HissLevel;
  Hiss := Hiss * (0.72 + 0.28 * Sin(2 * Pi * 0.55 * t));

  //Low rumble of the burning building.
  Rumble := Sin(2 * Pi * Frequency * 0.21 * t) * 0.10
          * (0.55 + 0.45 * Sin(2 * Pi * Frequency * 0.028 * t));

  //Crackles: each short block may fire one decaying noise burst. Two layers
  //at different block lengths keep the pattern from sounding metronomic.
  Period := Round(SampleRate / (Frequency * 2.5));
  if Period < 2 then
    Period := 2;
  Pop := 0;
  for k := 0 to 1 do
  begin
    Block := Period * (k + 1);
    if HashNoise(Index div Block + k * 1013) > PopThreshold then
    begin
      Phase := (Index mod Block) / Block;
      Pop := Pop + HashNoise(Index xor (k * 7919)) * (1 - Phase) * (1 - Phase);
    end;
  end;
  Pop := Pop * PopLevel;

  Result := Hiss + Rumble + Pop;
  if Result > 1.0 then
    Result := 1.0
  else if Result < -1.0 then
    Result := -1.0;
end;

// Springy net bounce "boing": the pitch rises quickly from 70% of the tuned
// note to 170% (the closed-form phase integral keeps it sample accurate),
// with a little decaying vibrato and a fast attack / body decay envelope.
// Used when a jumper lands on the life net.
function Boing_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SweepDepth = 1.0;
  SweepTau = 0.035;
  DecayTau = 0.16;
  VibRate = 22.0;
  VibDepth = 0.10;
var
  t, Phase, Env, Vib: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Phase := 2 * Pi * Frequency *
    (0.7 * t + SweepDepth * (t - SweepTau * (1 - Exp(-t / SweepTau))));
  Vib := VibDepth * Sin(2 * Pi * VibRate * t) * Exp(-t / 0.10);
  Env := (1 - Exp(-t * 800)) * Exp(-t / DecayTau);
  Result := Sin(Phase + Vib) * Env;
end;

// Fire engine siren: a sine wailing between 55% and 145% of the tuned note.
// The phase is the closed-form integral of that sweep, so the tone glides
// instead of stepping. Frequency is the centre pitch, low notes are a big
// engine and high ones a hand siren.
function Siren_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  WailRate = 1.6;    //wails per second
  WailDepth = 0.45;
var
  t, w, Phase: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  w := 2 * Pi * WailRate;
  //integral of (1 + WailDepth * sin(w*t))
  Phase := 2 * Pi * Frequency * (t - (WailDepth / w) * Cos(w * t));
  Result := Sin(Phase) * 0.75;
end;

// A single water bubble surfacing: the classic "bloop". The pitch glides up
// from 55% of the tuned note to the note itself in about 20 ms (closed-form
// phase integral, so the rise is smooth and sample accurate), while a short
// high ping adds the little resonant tick a bubble makes as it leaves the
// water. The envelope is a fast soft attack and a ~65 ms decay, so one note
// reads as one bubble. Frequency is the final pitch: low notes are big lazy
// bubbles, high notes are small fizzy ones.
function Bubble_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  RiseStart = 0.45;   //pitch starts 45% below the tuned note
  RiseTau = 0.018;    //seconds the upward glide lasts
  DecayTau = 0.065;   //amplitude decay of the body
  PingLevel = 0.18;   //high resonant tick on top
  PingTau = 0.012;
var
  t, Phase, Env, Ping: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  //f(t) = F * (1 - RiseStart * exp(-t/RiseTau)), integrated in closed form.
  Phase := 2 * Pi * Frequency *
    (t - RiseStart * RiseTau * (1 - Exp(-t / RiseTau)));
  Env := (1 - Exp(-t * 1200)) * Exp(-t / DecayTau);
  Ping := Sin(2 * Pi * Frequency * 2.6 * t) * Exp(-t / PingTau) * PingLevel;
  Result := Sin(Phase) * Env + Ping;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Bass drum: a sine whose pitch drops from ~3x the tuned note down to it in
// the first 30 ms (the classic "boom"), plus a short noise click for the
// beater attack. Frequency is the drum pitch (a 60 Hz note is a deep kick,
// 120 a tight one). The phase is the closed-form integral of the pitch curve
// (F * (t + depth * tau * (1 - e^(-t/tau)))), so the sweep never jumps.
function Kick_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SweepDepth = 2.2;  //pitch starts at (1 + depth) x Frequency
  SweepTau = 0.030;  //seconds the pitch glide lasts
  BodyTau = 0.120;   //amplitude decay of the body
  ClickTau = 0.002;  //beater click length
var
  t, Phase, Body, Click: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Phase := 2 * Pi * Frequency *
    (t + SweepDepth * SweepTau * (1 - Exp(-t / SweepTau)));
  Body := Sin(Phase) * Exp(-t / BodyTau);
  Click := HashNoise(Index) * Exp(-t / ClickTau) * 0.3;
  Result := Body + Click;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Snare drum: the shell tone (two detuned sines at the tuned pitch, the beat
// between them gives the skin ring) under a broadband noise burst from the
// snare wires. Both decay in tens of milliseconds; Frequency tunes the shell
// note (music.sound('Snare', 200, 300)).
function Snare_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  BodyTau = 0.045;   //shell tone decay
  NoiseTau = 0.080;  //snare-wire noise decay
  Detune = 1.51;     //second partial ratio
var
  t, Tone, Noise: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Tone := (Sin(2 * Pi * Frequency * t) + Sin(2 * Pi * Frequency * Detune * t))
    * 0.5 * Exp(-t / BodyTau);
  Noise := HashNoise(Index) * Exp(-t / NoiseTau);
  Result := Tone * 0.6 + Noise * 0.8;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Closed hi-hat: an inharmonic metallic ring (the classic cymbal partial
// stack) under a very short bright noise burst for the stick attack; gone in
// ~50 ms. Frequency tunes the ring (music.sound('HiHat', 500, 80)).
function HiHat_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  RingTau = 0.030;   //metallic ring decay
  NoiseTau = 0.012;  //stick-click noise decay
  Ratios: array[0..5] of Double = (1.0, 1.51, 1.89, 2.31, 2.66, 3.15);
var
  t, Ring, Noise: Double;
  k: Integer;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Ring := 0;
  for k := 0 to High(Ratios) do
    Ring := Ring + Sin(2 * Pi * Frequency * Ratios[k] * t);
  Ring := Ring / Length(Ratios) * Exp(-t / RingTau);
  Noise := HashNoise(Index) * Exp(-t / NoiseTau) * 0.6;
  Result := Ring + Noise;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Coin collect sound like Sonic games - classic bleep with rising pitch
function Coin_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SweepTau = 0.015;
var
  t, Phase, Env: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  // Pitch rises quickly from lower to higher
  Phase := 2 * Pi * Frequency * (1.0 + 0.5 * (1 - Exp(-t / SweepTau))) * t;
  Env := (1 - Exp(-t * 2000)) * Exp(-t / 0.25);
  // Two sine waves for a richer tone
  Result := (Sin(Phase) * 0.7 + Sin(Phase * 2) * 0.3) * Env;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Cash drawer: the drawer is pulled open with a mechanical rattle, hits the
// end of its travel, then the register chimes twice ("ka-ching"). Frequency
// tunes the register bell; the mechanical parts are derived from HashNoise, so
// the same note renders exactly the same sound on every play.
function Cash_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SlideTau = 0.11;      //drawer slide hiss decay, seconds
  StopDelay = 0.135;    //when the drawer reaches the end of its travel
  StopTau = 0.045;      //mechanical stop thump decay
  BellDelay1 = 0.055;   //first chime, while the drawer is still moving
  BellDelay2 = 0.190;   //second chime, the "ching"
  BellTau = 0.30;       //chime ring decay
  BellLevel = 0.46;     //chime level against the mechanical noise
var
  t, d, Slide, Rattle, StopThump, Bell, Env, Phase: Double;
  Strike: Integer;
begin
  Result := 0;
  if (Frequency <= 0) or (SampleRate <= 0) then
    Exit;
  t := Index / SampleRate;
  //Drawer on its runners: bright noise with a darker layer under it. A fast
  //gate turns the hiss into the rattle of the drawer rolling out.
  Slide := HashNoise(Index) * 0.5 + HashNoise(Index shr 3) * 0.5;
  Rattle := Frac(t * (Frequency * 0.04 + 18));
  if Rattle < 0.55 then
    Rattle := 1.0
  else
    Rattle := 0.35;
  Slide := Slide * (1 - Exp(-t * 220)) * Exp(-t / SlideTau) * Rattle;
  //The drawer hitting the stop: a low knock, not a musical note.
  StopThump := 0;
  d := t - StopDelay;
  if d >= 0 then
    StopThump := Sin(2 * Pi * Frequency * 0.5 * d)
      * Exp(-d / StopTau) * (1 - Exp(-d * 3000)) * 0.55;
  //Two strikes of the register bell. The partials are deliberately inharmonic
  //so it reads as struck metal instead of an organ chord.
  Bell := 0;
  for Strike := 0 to 1 do
  begin
    if Strike = 0 then
      d := t - BellDelay1
    else
      d := t - BellDelay2;
    if d >= 0 then
    begin
      Phase := 2 * Pi * Frequency * 2.0 * d; //bell an octave above the note
      Env := (1 - Exp(-d * 2500)) * Exp(-d / BellTau);
      Bell := Bell + (Sin(Phase) * 0.6
        + Sin(Phase * 1.51) * 0.3
        + Sin(Phase * 2.37) * 0.15) * Env;
    end;
  end;
  Bell := Bell * BellLevel;
  Result := Slide * 0.55 + StopThump + Bell;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Sci-fi phaser: the beam fires high and drops to the tuned note, warbling
// fast as it goes. The sweep is a closed-form phase integral so it glides; the
// warble is phase modulation, and a detuned second oscillator plus ring
// modulation give the metallic "phase" edge. A short noise crack adds the
// energy discharge. Frequency is the pitch the beam settles on: low notes are
// an alien stun gun, high ones a starship rifle.
function Phaser_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SweepDepth = 2.6;    //pitch starts 3.6x above the tuned note
  SweepTau = 0.10;     //pitch fall time, seconds
  DecayTau = 0.35;     //beam decay, seconds
  WarbleRate = 27.0;   //fast flutter, Hz
  WarbleDepth = 0.55;  //flutter depth, radians
  TremoloRate = 16.0;
var
  t, Phase, Warble, Tremolo, Tone, Detune, Ring, Noise, Env: Double;
begin
  Result := 0;
  if (Frequency <= 0) or (SampleRate <= 0) then
    Exit;
  t := Index / SampleRate;
  //f(t) = F * (1 + SweepDepth*exp(-t/SweepTau)), integrated analytically.
  Phase := 2 * Pi * Frequency * (t + SweepDepth * SweepTau * (1 - Exp(-t / SweepTau)));
  Warble := WarbleDepth * Sin(2 * Pi * WarbleRate * t) * Exp(-t / 0.22);
  Tremolo := 0.65 + 0.35 * Sin(2 * Pi * TremoloRate * t);
  Tone := Sin(Phase + Warble);
  Detune := Sin(Phase * 1.51 - Warble * 0.6);
  Ring := Sin(Phase * 2.73 + Warble * 0.3);
  //The beam's energy discharge, brightest at the muzzle flash.
  Noise := HashNoise(Index) * Exp(-t / 0.045);
  Env := (1 - Exp(-t * 900)) * Exp(-t / DecayTau);
  Result := (Tone * 0.55 + Detune * 0.25 + Tone * Ring * 0.28
    + Noise * 0.18) * Tremolo * Env;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

function Zap_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SweepDepth = 4.0;
  SweepTau = 0.025;
  DecayTau = 0.08;
var
  t, Phase, Tone, Noise, Env: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Phase := 2 * Pi * Frequency * (t + SweepDepth * SweepTau * (1 - Exp(-t / SweepTau)));
  Tone := Sin(Phase) * 0.6 + Sin(Phase * 3) * 0.25 + Sin(Phase * 5) * 0.15;
  Noise := HashNoise(Index) * Exp(-t / 0.035);
  Env := (1 - Exp(-t * 2500)) * Exp(-t / DecayTau);
  Result := (Tone * 0.75 + Noise * 0.25) * Env;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

function Laser_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  SweepDepth = 3.2;
  SweepTau = 0.04;
  DecayTau = 0.11;
var
  t, Phase, Tone, Env: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Phase := 2 * Pi * Frequency * (t + SweepDepth * SweepTau * (1 - Exp(-t / SweepTau)));
  Tone := Sin(Phase) * 0.75 - Sin(Phase * 3) * 0.2 + Sin(Phase * 5) * 0.05;
  Env := (1 - Exp(-t * 3000)) * Exp(-t / DecayTau);
  Result := Tone * Env;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

function Chime_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  t, Shimmer, Part1, Part2, Part3, Env1, Env2, Env3, Onset: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Onset := 1 - Exp(-t * 1200);
  Env1 := Exp(-t / 0.35);
  Env2 := Exp(-t / 0.20);
  Env3 := Exp(-t / 0.12);
  Shimmer := 1.0 + 0.15 * Sin(2 * Pi * 8.0 * t);
  Part1 := Sin(2 * Pi * Frequency * t) * Env1;
  Part2 := Sin(2 * Pi * Frequency * 2.756 * t) * Env2 * 0.45;
  Part3 := Sin(2 * Pi * Frequency * 5.404 * t) * Env3 * 0.20;
  Result := (Part1 + Part2 + Part3) * Shimmer * Onset * 0.85;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

function Sweep_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
const
  RiseDepth = 1.8;
  RiseTau = 0.07;
  DecayTau = 0.22;
var
  t, Phase, Tone, Env: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  Phase := 2 * Pi * Frequency * ((0.6 + RiseDepth) * t - RiseDepth * RiseTau * (1 - Exp(-t / RiseTau)));
  Tone := Sin(Phase) * 0.75 + Sin(Phase * 2) * 0.25;
  Env := (1 - Exp(-t * 2000)) * Exp(-t / DecayTau);
  Result := Tone * Env;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

function Bee_Waveform(Index, SampleRate: Integer; Frequency: Single): Single;
var
  t, Phase, Vibrato, Harm1, Harm2, Harm3, Harm4, Harm5, Harm6, Tone, Air, Wobble, Env: Double;
begin
  if (Frequency <= 0) or (SampleRate <= 0) then
  begin
    Result := 0;
    Exit;
  end;
  t := Index / SampleRate;
  // Natural wingbeat micro-flutter / frequency tremor
  Vibrato := 0.035 * Sin(2 * Pi * 18.0 * t) + 0.015 * Sin(2 * Pi * 6.5 * t);
  Phase := 2 * Pi * Frequency * t + Vibrato;

  // Rich harmonic spectrum characteristic of insect wing stroke aerodynamics
  Harm1 := Sin(Phase) * 0.36;
  Harm2 := Sin(Phase * 2.0) * 0.28;
  Harm3 := Sin(Phase * 3.0) * 0.18;
  Harm4 := Sin(Phase * 4.0) * 0.12;
  Harm5 := Sin(Phase * 5.0) * 0.08;
  Harm6 := Sin(Phase * 6.0) * 0.06;
  Tone := Harm1 + Harm2 + Harm3 + Harm4 + Harm5 + Harm6;

  // Air vortex turbulence shed by wingtips (modulated by wingstroke phase)
  Air := HashNoise(Index) * (0.5 + 0.5 * Sin(Phase * 2.0)) * 0.08;

  // Body wobble / flight amplitude modulation
  Wobble := 0.85 + 0.15 * Sin(2 * Pi * 22.0 * t);

  // Smooth onset to avoid click at start
  Env := 1.0 - Exp(-t * 800.0);

  Result := (Tone * 0.78 + Air) * Wobble * Env;
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

initialization
  WaveForms := TWaveForms.Create;
  //Default one is the first one
  Waveforms.Add('Sin', @Sin_Waveform);
  Waveforms.Add('Piano', @Piano_Waveform);
  Waveforms.Add('GrandPiano', @GrandPiano_Waveform);
  Waveforms.Add('OldPiano', @OldPiano_Waveform);
  Waveforms.Add('Square', @Square_Waveform);
  Waveforms.Add('Pulse25', @Pulse25_Waveform);
  Waveforms.Add('Pulse125', @Pulse125_Waveform);
  Waveforms.Add('Saw', @Saw_Waveform);
  Waveforms.Add('Tri', @Tri_Waveform);
  Waveforms.Add('Rect', @Rect_Waveform);
  Waveforms.Add('Dsin', @Dsin_Waveform);
  Waveforms.Add('Organ', @Organ_Waveform);
  Waveforms.Add('Bell', @Bell_Waveform);
  Waveforms.Add('Pluck', @Pluck_Waveform);
  Waveforms.Add('Flute', @Flute_Waveform);
  Waveforms.Add('FM', @FM_Waveform);

  //Noises
  Waveforms.Add('Noise', @Noise_Waveform); //Pitched Noise
  Waveforms.Add('White', @WhiteNoise_Waveform);
  Waveforms.Add('Pink', @PinkNoise_Waveform);
  Waveforms.Add('Dark', @DarkNoise_Waveform);

  Waveforms.Add('Plane', @PlaneNoise_Waveform);
  Waveforms.Add('Engine', @Engine_Waveform);
  Waveforms.Add('Pew', @Pew_Waveform); //missile launch chirp
  Waveforms.Add('Explode', @Explode_Waveform); //enemy explosion rumble
  Waveforms.Add('Fire', @Fire_Waveform); //crackling fire bed, loopable
  Waveforms.Add('Boing', @Boing_Waveform); //springy life-net bounce
  Waveforms.Add('Siren', @Siren_Waveform); //wailing fire engine siren
  Waveforms.Add('Bubble', @Bubble_Waveform); //single water bubble "bloop"

  //Drums
  Waveforms.Add('Kick', @Kick_Waveform);
  Waveforms.Add('Snare', @Snare_Waveform);
  Waveforms.Add('HiHat', @HiHat_Waveform);
  Waveforms.Add('Coin', @Coin_Waveform);

  //Cash register and weapons
  Waveforms.Add('Cash', @Cash_Waveform); //cash drawer sliding open + register chime
  Waveforms.Add('Phaser', @Phaser_Waveform); //sci-fi gun phaser beam
  Waveforms.Add('Zap', @Zap_Waveform); //electric spark discharge
  Waveforms.Add('Laser', @Laser_Waveform); //sci-fi cutter laser
  Waveforms.Add('Chime', @Chime_Waveform); //harmonic crystalline chime
  Waveforms.Add('Sweep', @Sweep_Waveform); //ascending sci-fi sweep
  Waveforms.Add('Bee', @Bee_Waveform); //insect wing buzz (bee/fly)

  PlayingMelodies := TList.Create;
  PlayingWaveforms := TList.Create;
  SoundCache := TTyroFileSounds.Create;
finalization
  ShutdownMelodies;
  ShutdownWaveforms;

  FreeAndNil(PlayingMelodies);
  FreeAndNil(PlayingWaveforms);

  FreeAndNil(WaveForms);
end.

