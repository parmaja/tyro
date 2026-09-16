unit TyroRadio;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *  Live internet radio player.
 *  Tricky part: the audio comes in a never-ending stream, so we cannot load the
 *  whole thing into a Music. We buffer into a bounded ring (the IceCast client
 *  buffer), snapshot it into a Music when enough is buffered, and when the
 *  decoder is about to overtake the snapshot we load a fresh one that keeps an
 *  overlap of already-decoded bytes and seek to the overlap point.
 *}

{$ifdef FPC}
{$mode delphi}
{$H+}{$M+}
{$endif}

interface

uses
  Classes, SysUtils,
  RayLib, RayClasses,
  IceCastClients,
  TyroSpectrum;

type
  TRadioPlayerState = (
    rpIdle,       //nothing loaded, no client
    rpBuffering,  //connected, filling the buffer
    rpPlaying,
    rpPaused,
    rpStopped,    //stopped by user or stream ended
    rpError
  );

  { TRadioPlayer }

  TRadioPlayer = class(TRayUpdate)
  private
    FClient: TmnIceCastClient;
    FSnapshot: TMemoryStream; //last snapshot handed to raylib (kept for bookkeeping)
    FMusic: TMusic;
    FMusicLoaded: Boolean;
    FBytesPerSec: Single; //estimate: bytes / second of decoded audio
    FVolume: Single;
    FURL: string;
    FState: TRadioPlayerState;
    FError: string;
    FUserPlaying: Boolean; //user wants playback; Pause clears it until Resume
    FSeekBytes: Int64; //bytes into the new snapshot where we seek on reload
    FPrerollBytes: Int64; //buffer bytes needed before first playback
    FPreloadBytes: Int64; //reload the snapshot when less than this remains
    FOverlapBytes: Int64; //bytes of already-decoded audio kept on reload
    procedure LoadFromSnapshot(AFirst: Boolean);
    function GuessFileType: string;
    procedure DoError(const AMessage: string);
    procedure StreamEnded;
    procedure SetVolume(AVolume: Single);
    function GetTitle: string;
    function GetStation: string;
    function GetGenre: string;
    function GetBitrate: string;
    function GetPlaying: Boolean;
    function GetBuffered: Int64;
    function GetStateString: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Play(const AURL: string);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure Update; override;

    property State: TRadioPlayerState read FState;
    property StateString: string read GetStateString;
    property Error: string read FError;
    property Title: string read GetTitle;
    property Station: string read GetStation;
    property Genre: string read GetGenre;
    property Bitrate: string read GetBitrate;
    property URL: string read FURL;
    property Playing: Boolean read GetPlaying;
    property Buffered: Int64 read GetBuffered;
    property Volume: Single read FVolume write SetVolume;
  end;

var
  RadioPlayer: TRadioPlayer = nil;

implementation

const
  cDefaultPreroll = 128 * 1024; //wait ~2-4s before the first playback starts
  cDefaultPreload = 64 * 1024;  //reload when less than ~1-2s remains
  cDefaultOverlap = 64 * 1024;  //keep an overlap of already-decoded audio on reload

{ TRadioPlayer }

constructor TRadioPlayer.Create;
begin
  inherited Create;
  FVolume := 1;
  FState := rpIdle;
  FPrerollBytes := cDefaultPreroll;
  FPreloadBytes := cDefaultPreload;
  FOverlapBytes := cDefaultOverlap;
  RayUpdates.Add(Self);
end;

destructor TRadioPlayer.Destroy;
begin
  Stop;
  RayUpdates.Remove(Self);
  inherited Destroy;
end;

function TRadioPlayer.GuessFileType: string;
var
  s: string;
begin
  s := LowerCase(FClient.ContentType);
  if Pos('ogg', s) > 0 then
    Exit('.ogg');
  if Pos('flac', s) > 0 then
    Exit('.flac');
  if Pos('mpeg', s) > 0 then
    Exit('.mp3');
  if Pos('aac', s) > 0 then
    Exit('.mp3'); //raylib cannot decode aac; it will fail gracefully
  Result := '.mp3';
end;

procedure TRadioPlayer.Play(const AURL: string);
begin
  Stop;
  FURL := AURL;
  FError := '';
  FState := rpBuffering;
  FUserPlaying := True;
  FClient := TmnIceCastClient.Create;
  FClient.Open(AURL);
end;

procedure TRadioPlayer.Pause;
begin
  FUserPlaying := False;
  if FMusicLoaded then
    PauseMusicStream(FMusic);
  if FMusicLoaded then
    FState := rpPaused;
end;

procedure TRadioPlayer.Resume;
begin
  if FState = rpError then
    Exit; //must Play() again
  FUserPlaying := True;
  if FMusicLoaded then
  begin
    if not IsMusicStreamPlaying(FMusic) then
      ResumeMusicStream(FMusic);
    FState := rpPlaying;
  end
  else
    FState := rpBuffering;
end;

procedure TRadioPlayer.Stop;
begin
  FUserPlaying := False;
  if FMusicLoaded then
  begin
    Spectrum.Detach; //stop feeding the analyzer before the stream is freed
    StopMusicStream(FMusic);
    UnloadMusicStream(FMusic);
    FMusicLoaded := False;
  end;
  FSnapshot.Free;
  FSnapshot := nil;
  if FClient <> nil then
  begin
    FClient.Close;
    FreeAndNil(FClient);
  end;
  FState := rpStopped;
end;

procedure TRadioPlayer.SetVolume(AVolume: Single);
begin
  FVolume := AVolume;
  if FMusicLoaded then
    SetMusicVolume(FMusic, FVolume);
end;

function TRadioPlayer.GetTitle: string;
begin
  if FClient <> nil then
    Result := FClient.Title
  else
    Result := '';
end;

function TRadioPlayer.GetStation: string;
begin
  if FClient <> nil then
    Result := FClient.StationName
  else
    Result := '';
end;

function TRadioPlayer.GetGenre: string;
begin
  if FClient <> nil then
    Result := FClient.Genre
  else
    Result := '';
end;

function TRadioPlayer.GetBitrate: string;
begin
  if FClient <> nil then
    Result := FClient.Bitrate
  else
    Result := '';
end;

function TRadioPlayer.GetPlaying: Boolean;
begin
  Result := FMusicLoaded and FUserPlaying;
end;

function TRadioPlayer.GetBuffered: Int64;
begin
  if FClient <> nil then
    Result := FClient.BufferSize
  else
    Result := 0;
end;

function TRadioPlayer.GetStateString: string;
begin
  case FState of
    rpIdle: Result := 'idle';
    rpBuffering: Result := 'buffering';
    rpPlaying: Result := 'playing';
    rpPaused: Result := 'paused';
    rpStopped: Result := 'stopped';
    rpError: Result := 'error';
  end;
end;

procedure TRadioPlayer.DoError(const AMessage: string);
begin
  FError := AMessage;
  FState := rpError;
  FUserPlaying := False;
  if FMusicLoaded then
  begin
    Spectrum.Detach; //stop feeding the analyzer before the stream is freed
    StopMusicStream(FMusic);
    UnloadMusicStream(FMusic);
    FMusicLoaded := False;
  end;
  FSnapshot.Free;
  FSnapshot := nil;
  FClient.Close;
  FreeAndNil(FClient);
end;

procedure TRadioPlayer.StreamEnded;
begin
  FUserPlaying := False;
  if FMusicLoaded then
  begin
    Spectrum.Detach; //stop feeding the analyzer before the stream is freed
    StopMusicStream(FMusic);
    UnloadMusicStream(FMusic);
    FMusicLoaded := False;
  end;
  FSnapshot.Free;
  FSnapshot := nil;
  FClient.Close;
  FreeAndNil(FClient);
  FState := rpStopped;
end;

// Snapshot the client buffer into a raylib Music.
// AFirst: first playback of the stream (no trimming, no seeking).
// Otherwise trim the client buffer, keeping an overlap of already-decoded bytes,
// load a fresh snapshot of the tail and seek to the overlap point.
procedure TRadioPlayer.LoadFromSnapshot(AFirst: Boolean);
var
  aBuf, aSnap: TMemoryStream;
  aPlayed: Single;
  aConsumed, aKeepBytes, aKeep: Int64;
  aSeek, aLen, aBP: Single;
  aNewMusic: TMusic;
  aExt: string;
begin
  if (FClient = nil) or (FClient.BufferSize = 0) then
    Exit;
  aBuf := FClient.Buffer;
  FSeekBytes := 0;
  FClient.Lock.Enter;
  try
    if (not AFirst) and FMusicLoaded and (FBytesPerSec > 0) then
    begin
      aPlayed := GetMusicTimePlayed(FMusic);
      aConsumed := Round(FBytesPerSec * aPlayed);
      if aConsumed < 0 then
        aConsumed := 0;
      if aConsumed > aBuf.Size then
        aConsumed := aBuf.Size;
      aKeepBytes := FOverlapBytes;
      if aKeepBytes > aConsumed then
        aKeepBytes := aConsumed;
      aKeep := aBuf.Size - (aConsumed - aKeepBytes); //tail to keep
      if aKeep > aBuf.Size then
        aKeep := aBuf.Size;
      if aKeep < 1 then
        aKeep := aBuf.Size;
      //trim the head of the shared buffer: move the tail to the front
      if aKeep < aBuf.Size then
      begin
        Move((PByte(aBuf.Memory) + (aBuf.Size - aKeep))^, aBuf.Memory^, aKeep);
        aBuf.SetSize(aKeep);
        aBuf.Position := 0;
      end;
      FSeekBytes := aKeepBytes;
    end;
    aSnap := TMemoryStream.Create;
    aBuf.Position := 0;
    aSnap.CopyFrom(aBuf, aBuf.Size);
  finally
    FClient.Lock.Leave;
  end;
  try
    if aSnap.Size = 0 then
      Exit;
    RayLibSound.Open;
    aExt := GuessFileType;
    aNewMusic := LoadMusicStreamFromMemory(PUTF8Char(aExt), aSnap.Memory, Integer(aSnap.Size));
    if aNewMusic.CtxType = 0 then
    begin
      DoError('Cannot decode audio format: ' + FClient.ContentType);
      Exit;
    end;
    SetMusicVolume(aNewMusic, FVolume);
    aLen := GetMusicTimeLength(aNewMusic);
    if aLen > 0 then
      aBP := aSnap.Size / aLen
    else
      aBP := 0;
    if (not AFirst) and (FSeekBytes > 0) and (aBP > 0) then
    begin
      aSeek := FSeekBytes / aBP;
      if aSeek > aLen * 0.9 then
        aSeek := aLen * 0.9;
      if aSeek < 0 then
        aSeek := 0;
      SeekMusicStream(aNewMusic, aSeek);
    end;
    //commit the new snapshot; drop the old music
    if FMusicLoaded then
    begin
      Spectrum.Detach; //stop feeding the analyzer before the stream is freed
      StopMusicStream(FMusic);
      UnloadMusicStream(FMusic);
    end;
    FMusic := aNewMusic;
    FBytesPerSec := aBP;
    FMusicLoaded := True;
    Spectrum.Attach(FMusic.Stream); //feed the analyzer from the live stream
    FSnapshot.Free;
    FSnapshot := aSnap;
    aSnap := nil;
    if FUserPlaying then
    begin
      FState := rpPlaying;
      UpdateMusicStream(FMusic);
      PlayMusicStream(FMusic);
    end
    else
      FState := rpPaused;
  finally
    aSnap.Free;
  end;
end;

procedure TRadioPlayer.Update;
var
  aState: TmnIceCastState;
  aPlayed, aRemaining: Single;
  aNewData: Boolean;
begin
  if FClient = nil then
    Exit;
  aState := FClient.State;
  case aState of
    icError:
    begin
      DoError(FClient.Error);
      Exit;
    end;
    icStopped:
    begin
      if FClient.StreamEnded then
        StreamEnded
      else
        DoError('Connection closed unexpectedly');
      Exit;
    end;
  end;
  if not FMusicLoaded then
  begin
    if (aState = icReady) and (FClient.BufferSize >= FPrerollBytes) then
      LoadFromSnapshot(True);
  end
  else if FUserPlaying then
  begin
    UpdateMusicStream(FMusic);
    aPlayed := GetMusicTimePlayed(FMusic);
    aRemaining := FSnapshot.Size - FBytesPerSec * aPlayed;
    if (aState = icReady) and (aRemaining < FPreloadBytes) then
    begin
      aNewData := False;
      FClient.Lock.Enter;
      try
        if FClient.BufferSize > FSnapshot.Size then
          aNewData := True;
      finally
        FClient.Lock.Leave;
      end;
      if aNewData then
        LoadFromSnapshot(False)
      else if aPlayed >= GetMusicTimeLength(FMusic) - 0.1 then
        StreamEnded; //no new data and the decoder finished the snapshot
    end;
  end;
end;

initialization
  RadioPlayer := TRadioPlayer.Create;

finalization
  FreeAndNil(RadioPlayer);

end.