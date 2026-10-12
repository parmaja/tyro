program icecast_probe;
{*******************************************************************************
*
*  Console probe for MiniLib mnIceCasts.
*
*  Opens a station URL, prints the response headers and watches the state and
*  the ICY StreamTitle metadata. Used to debug mnIceCasts without the RayLib
*  demo.
*
*  usage: icecast_probe <url> [seconds]
*
*******************************************************************************}
{$mode delphi}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  SysUtils, Classes, DateUtils,
  mnIceCasts;

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

var
  Client: TmnIceCastClient;
  URL: string;
  Seconds: Integer;
  Start, NowT: TDateTime;
  LastState: TmnIceCastState;
  LastTitle, LastHeaderDump: string;
  PrintHeaders: Boolean;
  Hdrs: TStringList;
  FinalSize: Int64;
begin
  if ParamCount < 1 then
  begin
    WriteLn('usage: icecast_probe <url> [seconds]');
    Halt(1);
  end;
  URL := ParamStr(1);
  Seconds := StrToIntDef(ParamStr(2), 15);

  Client := TmnIceCastClient.Create;
  Client.MaxAttempts := 3;
  Client.ReconnectDelay := 500;
  WriteLn('opening ', URL);
  Client.Open(URL);

  LastState := icStopped;
  LastTitle := '';
  PrintHeaders := False;
  Hdrs := nil;
  Start := Now;
  try
    repeat
      NowT := Now;
      if Client.State <> LastState then
      begin
        LastState := Client.State;
        WriteLn(Format('[%6.2f] state=%s buffer=%dKB', [MilliSecondsBetween(NowT, Start) / 1000,
          IceStateName(LastState), Client.BufferSize div 1024]));
      end;

      if (Client.State = icReady) and (not PrintHeaders) then
      begin
        PrintHeaders := True;
        WriteLn('  content-type : ', Client.ContentType);
        WriteLn('  connected-to : ', Client.ConnectedTo);
        WriteLn('  station name : ', Client.StationName);
        WriteLn('  genre        : ', Client.Genre);
        WriteLn('  bitrate      : ', Client.Bitrate);
        WriteLn('  station url  : ', Client.StationURL);
        WriteLn('  response     : ', Client.ResponseCode);
        Hdrs := Client.Headers;
        if Hdrs <> nil then
        begin
          LastHeaderDump := Hdrs.Text;
          WriteLn('  headers:');
          WriteLn(LastHeaderDump);
        end;
      end;

      if Client.Title <> LastTitle then
      begin
        LastTitle := Client.Title;
        WriteLn(Format('[%6.2f] title=%s', [MilliSecondsBetween(NowT, Start) / 1000, LastTitle]));
      end;

      if Client.State = icError then
      begin
        WriteLn('error: ', Client.Error);
        Break;
      end;
      Sleep(50);
    until MilliSecondsBetween(Now, Start) > Seconds * 1000;
    FinalSize := Client.BufferSize;
  finally
    Client.Close;
    Client.Free;
  end;
  WriteLn('done, last buffer size ', FinalSize);
end.
