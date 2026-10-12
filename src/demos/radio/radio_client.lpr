program radio_client;
{*******************************************************************************
*
*   Tyro - internet radio client demo
*
*   RayLib + MiniLib mnIceCasts, no TyroLib.
*
*   The point of this demo is to exercise TmnIceCastClient end to end so the
*   bugs in mnIceCasts can be reproduced and fixed:
*     - HTTP request / response parsing (ICY and HTTP status lines)
*     - Icy-* headers (name, genre, bitrate, url) and Icy-MetaInt metadata
*     - Transfer-Encoding: chunked bodies
*     - HTTP redirects (Location)
*     - reconnect after a dropped connection
*     - the bounded audio buffer consumed by the player
*
*   The audio itself is decoded by RayLib's audio backend (WAV/OGG/MP3/FLAC/
*   QOA/XM/MOD). The client buffer is a never ending stream, so the player
*   snapshots it into a Music, plays it, and reloads a new snapshot with a
*   small overlap when the decoder is close to catching up. AAC/ADTS streams
*   are downloaded fine but cannot be decoded by raylib and are reported as
*   such.
*
*   Keys:
*     Enter           play the URL in the box
*     Space / P       pause / resume
*     S               stop
*     Up / Down       volume
*     D               dump the current raw buffer to radio_dump_NNN.bin
*     Esc / close     quit
*
*******************************************************************************}

{$mode delphi}{$H+}
{$ifdef mswindows}{$apptype console}{$endif}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  SysUtils, Classes,
  RayLib,
  mnIceCasts;

const
  cScreenWidth = 900;
  cScreenHeight = 560;

  //a public MP3 station that speaks ICY and sends StreamTitle metadata
  cDefaultURL = 'http://ice1.somafm.com/groovesalad-128-mp3';

  //buffer water marks (bytes)
  cPrerollBytes = 64 * 1024; //wait for this much before the first playback
  cPreloadBytes = 32 * 1024; //reload the snapshot when less than this remains
  cOverlapBytes = 32 * 1024; //keep this much already-decoded audio on reload

type
  TRadioState = (rsIdle, rsBuffering, rsReconnecting, rsPlaying, rsPaused, rsStopped, rsError);

  { TRadio }

  TRadio = class(TObject)
  private
    FClient: TmnIceCastClient;
    FSnapshot: TMemoryStream;
    FMusic: TMusic;
    FMusicLoaded: Boolean;
    FBytesPerSec: Single;
    FVolume: Single;
    FURL: string;
    FState: TRadioState;
    FError: string;
    FUserPlaying: Boolean;
    FStarted: Boolean; //saw the connection progress past icStopped
    FSeekBytes: Int64;
    FLastState: TmnIceCastState;
    FLastTitle: string;
    FDumpNo: Integer;
    FLastDump: string;
    function GuessFileType: string;
    procedure DoError(const AMessage: string);
    procedure StreamEnded;
    procedure LoadFromSnapshot(AFirst: Boolean);
    procedure SetVolume(AValue: Single);
    function GetTitle: string;
    function GetStation: string;
    function GetGenre: string;
    function GetBitrate: string;
    function GetContentType: string;
    function GetConnectedTo: string;
    function GetBuffered: Int64;
    function GetSnapshotSize: Int64;
    function GetPlaying: Boolean;
    function GetStateName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Play(const AURL: string);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure Update;
    procedure DumpBuffer;

    property State: TRadioState read FState;
    property StateName: string read GetStateName;
    property Error: string read FError;
    property Title: string read GetTitle;
    property Station: string read GetStation;
    property Genre: string read GetGenre;
    property Bitrate: string read GetBitrate;
    property ContentType: string read GetContentType;
    property ConnectedTo: string read GetConnectedTo;
    property Buffered: Int64 read GetBuffered;
    property SnapshotSize: Int64 read GetSnapshotSize;
    property BytesPerSec: Single read FBytesPerSec;
    property Playing: Boolean read GetPlaying;
    property MusicLoaded: Boolean read FMusicLoaded;
    property Volume: Single read FVolume write SetVolume;
    property URL: string read FURL;
    property LastDump: string read FLastDump;
  end;

const
  cDumpPrefix = 'radio_dump_';

{ colours and small helpers }

function C(ARed, AGreen, ABlue: Byte): TColor; overload;
begin
  Result := TColor.Create(ARed, AGreen, ABlue, 255);
end;

function C(ARed, AGreen, ABlue, AAlpha: Byte): TColor; overload;
begin
  Result := TColor.Create(ARed, AGreen, ABlue, AAlpha);
end;

procedure Txt(const S: string; X, Y, Size: Integer; Color: TColor);
begin
  DrawText(PUTF8Char(UTF8String(S)), X, Y, Size, Color);
end;

function Cut(const S: string; AMax: Integer): string;
begin
  if Length(S) > AMax then
    Result := Copy(S, 1, AMax - 3) + '...'
  else
    Result := S;
end;

function IceStateName(AState: TmnIceCastState): string;
begin
  case AState of
    icStopped: Result := 'stopped';
    icConnecting: Result := 'connecting';
    icOpening: Result := 'opening';
    icReady: Result := 'ready';
    icReconnecting: Result := 'reconnecting';
    icError: Result := 'error';
  else
    Result := '?';
  end;
end;

{ TRadio }

constructor TRadio.Create;
begin
  inherited Create;
  FVolume := 0.8;
  FState := rsIdle;
  FLastState := icStopped;
end;

destructor TRadio.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TRadio.GuessFileType: string;
var
  S: string;
begin
  S := LowerCase(FClient.ContentType);
  if Pos('ogg', S) > 0 then
    Exit('.ogg');
  if Pos('flac', S) > 0 then
    Exit('.flac');
  if Pos('mpeg', S) > 0 then
    Exit('.mp3');
  if Pos('aac', S) > 0 then
    Exit('.aac');
  Result := '.mp3';
end;

procedure TRadio.Play(const AURL: string);
begin
  Stop;
  FState := rsBuffering;
  FUserPlaying := True;
  FStarted := False;
  FLastTitle := '';
  FURL := AURL;
  FError := '';
  FClient := TmnIceCastClient.Create;
  FClient.MaxAttempts := 5;
  FClient.ReconnectDelay := 1000;
  FClient.Open(AURL);
  WriteLn('[radio] playing ', AURL);
end;

procedure TRadio.Pause;
begin
  FUserPlaying := False;
  if FMusicLoaded then
  begin
    PauseMusicStream(FMusic);
    FState := rsPaused;
  end;
end;

procedure TRadio.Resume;
begin
  if FState = rsError then
    Exit;
  FUserPlaying := True;
  if FMusicLoaded then
  begin
    if not IsMusicStreamPlaying(FMusic) then
      ResumeMusicStream(FMusic);
    FState := rsPlaying;
  end
  else
    FState := rsBuffering;
end;

procedure TRadio.Stop;
var
  aClient: TmnIceCastClient;
begin
  FUserPlaying := False;
  if FMusicLoaded then
  begin
    StopMusicStream(FMusic);
    UnloadMusicStream(FMusic);
    FMusic := Default(TMusic);
    FMusicLoaded := False;
  end;
  FreeAndNil(FSnapshot);
  aClient := FClient;
  FClient := nil;
  if aClient <> nil then
  begin
    aClient.Close;
    aClient.Free;
  end;
  FState := rsStopped;
  FError := '';
  FStarted := False;
  FLastState := icStopped;
end;

procedure TRadio.DoError(const AMessage: string);
var
  aClient: TmnIceCastClient;
begin
  WriteLn('[radio] error: ', AMessage);
  FError := AMessage;
  FState := rsError;
  FUserPlaying := False;
  if FMusicLoaded then
  begin
    StopMusicStream(FMusic);
    UnloadMusicStream(FMusic);
    FMusic := Default(TMusic);
    FMusicLoaded := False;
  end;
  FreeAndNil(FSnapshot);
  aClient := FClient;
  FClient := nil;
  if aClient <> nil then
  begin
    aClient.Close;
    aClient.Free;
  end;
end;

procedure TRadio.StreamEnded;
var
  aClient: TmnIceCastClient;
begin
  WriteLn('[radio] stream ended');
  FUserPlaying := False;
  if FMusicLoaded then
  begin
    StopMusicStream(FMusic);
    UnloadMusicStream(FMusic);
    FMusic := Default(TMusic);
    FMusicLoaded := False;
  end;
  FreeAndNil(FSnapshot);
  aClient := FClient;
  FClient := nil;
  if aClient <> nil then
  begin
    aClient.Close;
    aClient.Free;
  end;
  FState := rsStopped;
end;

procedure TRadio.SetVolume(AValue: Single);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue > 1 then
    AValue := 1;
  FVolume := AValue;
  if FMusicLoaded then
    SetMusicVolume(FMusic, FVolume);
end;

function TRadio.GetTitle: string;
begin
  if FClient <> nil then Result := FClient.Title else Result := '';
end;

function TRadio.GetStation: string;
begin
  if FClient <> nil then Result := FClient.StationName else Result := '';
end;

function TRadio.GetGenre: string;
begin
  if FClient <> nil then Result := FClient.Genre else Result := '';
end;

function TRadio.GetBitrate: string;
begin
  if FClient <> nil then Result := FClient.Bitrate else Result := '';
end;

function TRadio.GetContentType: string;
begin
  if FClient <> nil then Result := FClient.ContentType else Result := '';
end;

function TRadio.GetConnectedTo: string;
begin
  if FClient <> nil then Result := FClient.ConnectedTo else Result := '';
end;

function TRadio.GetBuffered: Int64;
begin
  if FClient <> nil then Result := FClient.BufferSize else Result := 0;
end;

function TRadio.GetSnapshotSize: Int64;
begin
  if FSnapshot <> nil then Result := FSnapshot.Size else Result := 0;
end;

function TRadio.GetPlaying: Boolean;
begin
  Result := FMusicLoaded and FUserPlaying;
end;

function TRadio.GetStateName: string;
begin
  case FState of
    rsIdle: Result := 'idle';
    rsBuffering: Result := 'buffering';
    rsReconnecting: Result := 'reconnecting';
    rsPlaying: Result := 'playing';
    rsPaused: Result := 'paused';
    rsStopped: Result := 'stopped';
    rsError: Result := 'error';
  else
    Result := '?';
  end;
end;

procedure TRadio.DumpBuffer;
var
  aBuf: TMemoryStream;
  aFile: TFileStream;
  aName: string;
begin
  if FClient = nil then
    Exit;
  aBuf := FClient.Buffer;
  if aBuf = nil then
    Exit;
  Inc(FDumpNo);
  aName := Format('%s%.3d.bin', [cDumpPrefix, FDumpNo]);
  FClient.Lock.Enter;
  try
    aFile := TFileStream.Create(aName, fmCreate);
    try
      aBuf.Position := 0;
      aFile.CopyFrom(aBuf, aBuf.Size);
    finally
      aFile.Free;
    end;
  finally
    FClient.Lock.Leave;
  end;
  FLastDump := aName;
  WriteLn('[radio] dumped ', aBuf.Size, ' bytes to ', aName);
end;

//Snapshot the client buffer into a RayLib Music.
//AFirst: first playback (no trimming, no seeking).
//Otherwise trim the shared buffer keeping an overlap of already-decoded audio,
//load a fresh snapshot of the tail and seek to the overlap point so playback
//continues seamlessly.
procedure TRadio.LoadFromSnapshot(AFirst: Boolean);
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
      aKeepBytes := cOverlapBytes;
      if aKeepBytes > aConsumed then
        aKeepBytes := aConsumed;
      aKeep := aBuf.Size - (aConsumed - aKeepBytes);
      if aKeep > aBuf.Size then
        aKeep := aBuf.Size;
      if aKeep < 1 then
        aKeep := aBuf.Size;
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
    aExt := GuessFileType;
    if SameText(aExt, '.aac') then
    begin
      DoError('AAC is not supported: raylib cannot decode ' + FClient.ContentType +
        ' streams, use an MP3/OGG station');
      Exit;
    end;
    aNewMusic := LoadMusicStreamFromMemory(PUTF8Char(UTF8String(aExt)), aSnap.Memory, Integer(aSnap.Size));
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
    if FMusicLoaded then
    begin
      StopMusicStream(FMusic);
      UnloadMusicStream(FMusic);
    end;
    FMusic := aNewMusic;
    FBytesPerSec := aBP;
    FMusicLoaded := True;
    FSnapshot.Free;
    FSnapshot := aSnap;
    aSnap := nil;
    if FUserPlaying then
    begin
      FState := rsPlaying;
      UpdateMusicStream(FMusic);
      PlayMusicStream(FMusic);
    end
    else
      FState := rsPaused;
  finally
    aSnap.Free;
  end;
end;

procedure TRadio.Update;
var
  aState: TmnIceCastState;
  aPlayed, aRemaining: Single;
  aNewData: Boolean;
begin
  if FClient = nil then
    Exit;
  aState := FClient.State;
  if aState <> FLastState then
  begin
    WriteLn('[icecast] ', IceStateName(FLastState), ' -> ', IceStateName(aState),
      '  buffer=', IntToStr(FClient.BufferSize div 1024), ' KB');
    FLastState := aState;
  end;
  if aState <> icStopped then
    FStarted := True;
  if FClient.Title <> FLastTitle then
  begin
    FLastTitle := FClient.Title;
    if FLastTitle <> '' then
      WriteLn('[icecast] title: ', FLastTitle);
  end;
  case aState of
    icError:
      begin
        DoError(FClient.Error);
        Exit;
      end;
    icStopped:
      begin
        //ignore the tiny window between Open() and Prepare() running in the
        //worker thread, where the freshly created connection is still icStopped
        if not FStarted then
          Exit;
        if FClient.StreamEnded then
          StreamEnded
        else
          DoError('Connection closed unexpectedly');
        Exit;
      end;
  end;
  //While reconnecting keep whatever is buffered playing and do not end the
  //stream; only a failed reconnect (icError) stops playback.
  if aState = icReconnecting then
  begin
    if FUserPlaying then
      FState := rsReconnecting;
    if FMusicLoaded then
      UpdateMusicStream(FMusic);
    Exit;
  end;
  if not FMusicLoaded then
  begin
    if (aState = icReady) and (FClient.BufferSize >= cPrerollBytes) then
      LoadFromSnapshot(True);
  end
  else if FUserPlaying then
  begin
    UpdateMusicStream(FMusic);
    aPlayed := GetMusicTimePlayed(FMusic);
    aRemaining := FSnapshot.Size - FBytesPerSec * aPlayed;
    if (aState = icReady) and (aRemaining < cPreloadBytes) then
    begin
      //only reload when the client actually has more data than the snapshot
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
    if (FState <> rsError) and (aState = icReady) and FUserPlaying then
      FState := rsPlaying;
  end;
end;

{ the demo UI }

var
  Radio: TRadio;

  EditURL: string;
  URLFocus: Boolean;
  URLRect: TRectangle;
  PlayRect, PauseRect, StopRect, DumpRect: TRectangle;
  VolRect: TRectangle;
  FrameNo: Integer;

procedure InitUI;
begin
  EditURL := cDefaultURL;
  URLFocus := False;
  FrameNo := 0;
  URLRect := RectangleOf(20, 52, 600, 36);
  PlayRect := RectangleOf(632, 52, 72, 36);
  PauseRect := RectangleOf(712, 52, 84, 36);
  StopRect := RectangleOf(804, 52, 76, 36);
  DumpRect := RectangleOf(20, 492, 110, 28);
  VolRect := RectangleOf(120, 112, 320, 16);
end;

procedure HandleInput;
var
  M: TVector2;
  Ch: Integer;
begin
  M := GetMousePosition;

  if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
  begin
    if CheckCollisionPointRec(M, PlayRect) then
    begin
      URLFocus := False;
      Radio.Play(EditURL);
    end
    else if CheckCollisionPointRec(M, PauseRect) then
    begin
      URLFocus := False;
      if Radio.Playing then
        Radio.Pause
      else
        Radio.Resume;
    end
    else if CheckCollisionPointRec(M, StopRect) then
    begin
      URLFocus := False;
      Radio.Stop;
    end
    else if CheckCollisionPointRec(M, DumpRect) then
    begin
      URLFocus := False;
      Radio.DumpBuffer;
    end
    else if CheckCollisionPointRec(M, URLRect) then
      URLFocus := True
    else
      URLFocus := False;
  end;

  if IsMouseButtonDown(MOUSE_BUTTON_LEFT) and CheckCollisionPointRec(M, VolRect) then
  begin
    if VolRect.width > 0 then
      Radio.Volume := (M.x - VolRect.x) / VolRect.width;
  end;

  if URLFocus then
  begin
    while True do
    begin
      Ch := GetCharPressed;
      if Ch = 0 then
        Break;
      if (Ch >= 32) and (Ch < 127) and (Length(EditURL) < 500) then
        EditURL := EditURL + Chr(Ch);
    end;
    if IsKeyPressed(KEY_BACKSPACE) and (Length(EditURL) > 0) then
      Delete(EditURL, Length(EditURL), 1);
  end;

  if IsKeyPressed(KEY_ENTER) then
  begin
    URLFocus := False;
    Radio.Play(EditURL);
  end
  else if not URLFocus then
  begin
    if IsKeyPressed(KEY_SPACE) or IsKeyPressed(KEY_P) then
    begin
      if Radio.Playing then
        Radio.Pause
      else
        Radio.Resume;
    end;
    if IsKeyPressed(KEY_S) then
      Radio.Stop;
    if IsKeyPressed(KEY_D) then
      Radio.DumpBuffer;
    if IsKeyDown(KEY_UP) then
      Radio.Volume := Radio.Volume + 0.02;
    if IsKeyDown(KEY_DOWN) then
      Radio.Volume := Radio.Volume - 0.02;
  end;
end;

procedure DrawButton(const R: TRectangle; const ALabel: string);
var
  Hover: Boolean;
  Bg, Fg: TColor;
  TW: Integer;
begin
  Hover := CheckCollisionPointRec(GetMousePosition, R);
  if Hover then
    Bg := C(64, 108, 160)
  else
    Bg := C(52, 58, 70);
  Fg := C(228, 232, 238);
  DrawRectangleRec(R, Bg);
  DrawRectangleLinesEx(R, 1.5, C(84, 92, 108));
  TW := MeasureText(PUTF8Char(UTF8String(ALabel)), 18);
  Txt(ALabel, Round(R.x + (R.width - TW) / 2), Round(R.y + (R.height - 18) / 2), 18, Fg);
end;

procedure DrawUI;
var
  Buf, Snap: Int64;
  Bps: Single;
  Secs: Single;
  InfoY: Integer;

  procedure Line(const AName, AValue: string; AColor: TColor);
  begin
    Txt(AName, 40, InfoY, 18, C(140, 148, 160));
    Txt(Cut(AValue, 90), 190, InfoY, 18, AColor);
    Inc(InfoY, 26);
  end;

begin
  BeginDrawing;
  ClearBackground(C(24, 26, 32));

  Txt('Tyro internet radio  -  RayLib + MiniLib mnIceCasts', 20, 16, 22, C(232, 236, 242));
  Txt('Esc to quit', 20 + MeasureText('Tyro internet radio  -  RayLib + MiniLib mnIceCasts', 22) + 16, 22, 14, C(120, 128, 140));

  //URL box
  DrawRectangleRec(URLRect, C(17, 18, 23));
  DrawRectangleLinesEx(URLRect, 1.5, C(92, 100, 116));
  Txt(Cut(EditURL, 66), Round(URLRect.x) + 8, Round(URLRect.y) + 9, 18, C(220, 224, 230));
  if URLFocus and ((FrameNo div 30) mod 2 = 0) then
  begin
    DrawRectangle(Round(URLRect.x) + 8 + MeasureText(PUTF8Char(UTF8String(Cut(EditURL, 66))), 18),
      Round(URLRect.y) + 7, 2, 20, C(220, 224, 230));
  end;

  DrawButton(PlayRect, 'Play');
  if Radio.Playing then
    DrawButton(PauseRect, 'Pause')
  else
    DrawButton(PauseRect, 'Resume');
  DrawButton(StopRect, 'Stop');

  //volume
  Txt('Volume', 40, 110, 18, C(140, 148, 160));
  DrawRectangleRec(VolRect, C(17, 18, 23));
  DrawRectangleLinesEx(VolRect, 1.0, C(92, 100, 116));
  DrawRectangle(Round(VolRect.x), Round(VolRect.y), Round(VolRect.width * Radio.Volume), Round(VolRect.height), C(64, 108, 160));
  DrawRectangle(Round(VolRect.x + VolRect.width * Radio.Volume) - 2, Round(VolRect.y) - 4, 4, Round(VolRect.height) + 8, C(200, 206, 214));
  Txt(IntToStr(Round(Radio.Volume * 100)) + '%', Round(VolRect.x + VolRect.width) + 12, 110, 18, C(200, 206, 214));

  //info panel
  DrawRectangleRec(RectangleOf(20, 150, 860, 320), C(33, 36, 44));
  DrawRectangleLinesEx(RectangleOf(20, 150, 860, 320), 1.5, C(70, 76, 90));

  InfoY := 164;
  Line('State', Radio.StateName, C(150, 210, 255));
  Line('Station', Radio.Station, C(224, 228, 234));
  Line('Now playing', Radio.Title, C(160, 240, 160));
  Line('Genre', Radio.Genre, C(224, 228, 234));
  Line('Bitrate', Radio.Bitrate, C(224, 228, 234));
  Line('Content-Type', Radio.ContentType, C(224, 228, 234));
  Line('Connected to', Radio.ConnectedTo, C(200, 206, 214));

  Buf := Radio.Buffered;
  Snap := Radio.SnapshotSize;
  Bps := Radio.BytesPerSec;
  if Bps > 0 then
    Secs := Buf / Bps
  else
    Secs := 0;
  Line('Buffer', Format('%d KB buffered / %d KB snapshot / %.1f s', [Buf div 1024, Snap div 1024, Secs]), C(224, 228, 234));
  if Radio.LastDump <> '' then
    Line('Last dump', Radio.LastDump, C(200, 206, 214));

  if Radio.Error <> '' then
    Txt('Error: ' + Cut(Radio.Error, 100), 40, 430, 18, C(240, 120, 120));

  //dump button + help
  DrawButton(DumpRect, 'Save dump');
  Txt('Enter play   Space/P pause   S stop   Up/Down volume   D dump   click URL to edit',
    20, 528, 16, C(120, 128, 140));

  EndDrawing;
end;

begin
  InitLibrary;
  InitWindow(cScreenWidth, cScreenHeight, 'Tyro Radio - RayLib + mnIceCasts');
  InitAudioDevice;
  SetTargetFPS(60);

  Radio := TRadio.Create;
  InitUI;

  WriteLn('[radio] default station: ', cDefaultURL);
  Radio.Play(cDefaultURL);

  while not WindowShouldClose do
  begin
    Inc(FrameNo);
    HandleInput;
    Radio.Update;
    DrawUI;
  end;

  Radio.Free;
  Radio := nil;

  CloseAudioDevice;
  CloseWindow;
end.
