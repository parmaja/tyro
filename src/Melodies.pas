unit Melodies;

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{**
 * This file is part of the 'Tyro'
 * @description MML parser to play music language in a string
 * @license   MIT
 * @author    Zaher Dirkey , zaher, zaherdirkey
 *
 *}

{////////////////////////////////////////////////////////////////////////////-
*    Music Macro Language
*   https://en.wikipedia.org/wiki/Music_Macro_Language
*
*   This file is used part of the 'Tyro'
*   @license   The MIT License (MIT) Included in this distribution
*   @author    Zaher Dirkey <zaherdirkey at yahoo dot com>
//////////////////////////////////////////////////////////////////////////////-
*  Look at this site, it have many of songs
*  https://archeagemmllibrary.com/
*   usefull refereces
*
*  http://web.mit.edu/18.06/www/Essays/linear-algebra-and-music.pdf
//////////////////////////////////////////////////////////////////////////////-}

interface

uses
  Classes, SysUtils, mnClasses, Math;

type

  { EMelodyException }

  EMelodyException = class(Exception)
  public
    constructor Create(Msg: String; Line, CharIndex: Integer); overload;
  end;

  TmmlNotes = Utf8String;

  TMelody = class;

  { TMelodyChannel }

  TMelodyChannel = class(TmnNamedObject)
  private
    FVolume: Single;
  private
    Chr: Char;
    Line, Current: Integer; //For Parser
  protected
    Melody: TMelody;
  public
    ID: Integer;
    Finished: Boolean;
    Notes: TmmlNotes;
    Tempo: Integer;
    Octave: Integer;
    NoteLength: Single;
    SubSequent: Integer;
    ShiftOctave: Integer;
    Instrument: String;
    Expired: Longint;
    SoundDuration: Single;
    constructor Create(AMelody: TMelody); virtual;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    procedure SetInstrument(Instrument: String); virtual;
    procedure SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single); virtual;
    function PlaySound: Boolean; virtual;
    function StopSound: Boolean; virtual;
    function Next: Boolean;
    property Volume: Single read FVolume write FVolume;
  end;

  { TMelodyChannels }

  TMelodyChannels = class(TmnNamedObjectList<TMelodyChannel>)
  end;

  { TMelody }

  TMelody = class(TObject)
  private
  protected
    procedure BeforePlay; virtual;
    procedure AfterPlay; virtual;
  public
    Terminated: Boolean;
    constructor Create; virtual;
    destructor Destroy; override;

    function CreateChannel: TMelodyChannel; virtual;
    procedure Play(Song: TArray<TmmlNotes>);
  end;

//    http://www.headchant.com/2011/11/01/sound-synthesis-with-love-part-ii-sine-waves/
//      https://stackoverflow.com/questions/11355353/how-can-i-convert-qbasic-play-commands-to-something-more-contemporary
//ref:  http://www.phy.mtu.edu/~suits/notefreqs.html
//     http://wiki.mabinogiworld.com/view/User:LexisMikaya/MML_101_Guide
//ref:  The calculation of freq: http://www.phy.mtu.edu/~suits/NoteFreqCalcs.html
// Between C4 and C4# is one step so C4 freq * (2^1/12) = 262 * 1.059463
// Between C4 and D4 is 2 steps = so C4 freq * (2^1/12) = 262 * 1.059463 ^ 2

//?    f = 440 * 2^((n-49)/12)

//////////////////////////////////////////////////////////////////////////////-
//ref: http://www.qb64.net/wiki/index.php?title=SOUND
  {          The Seven Music Octaves

       Note     Frequency      Note     Frequency      Note      Frequency
     1* D#1 ...... 39           G3 ....... 196          A#5 ...... 932
        E1 ....... 41           G#3 ...... 208          B5 ....... 988
        F1 ....... 44           A3 ....... 220       6* C6 ....... 1047
        F#1 ...... 46           A#3 ...... 233          C#6 ...... 1109
        G1 ....... 49           B3 ....... 247          D6 ....... 1175
        G#1 ...... 51        4* C4 ....... 262          D#6 ...... 1245
        A1 ....... 55           C#4 ...... 277          E6 ....... 1318
        A#1 ...... 58           D4 ....... 294          F6 ....... 1397
        B1 ....... 62           D#4 ...... 311          F#6 ...... 1480
     2* C2 ....... 65           E4 ....... 330          G6 ....... 1568
        C#2 ...... 69           F4 ....... 349          G# ....... 1661
        D2 ....... 73           F#4 ...... 370          A6 ....... 1760
        D#2 ...... 78           G4 ....... 392          A#6 ...... 1865
        E2 ....... 82           G#4 ...... 415          B6 ....... 1976
        F2 ....... 87           A4 ....... 440       7* C7 ....... 2093
        F#2 ...... 92           A# ....... 466          C#7 ...... 2217
        G2 ....... 98           B4 ....... 494          D7 ....... 2349
        G#2 ...... 104       5* C5 ....... 523          D#7 ...... 2489
        A2 ....... 110          C#5 ...... 554          E7 ....... 2637
        A#2 ...... 117          D5 ....... 587          F7 ....... 2794
        B2 ....... 123          D#5 ...... 622          F#7 ...... 2960
     3* C3 ....... 131          E5 ....... 659          G7 ....... 3136
        C#3 ...... 139          F5 ....... 698          G#7 ...... 3322
        D3 ....... 147          F#5 ...... 740          A7 ....... 3520
        D#3 ...... 156          G5 ....... 784          A#7 ...... 3729
        E3 ....... 165          G#5 ...... 831          B7 ....... 3951
        F3 ....... 175          A5 ....... 880       8* C8 ....... 4186
        F#3 ...... 185
                               # denotes sharp
  //////////////////////////////////////////////////////////////////////////-}

const
  Scores: TArray<String> = [
    'c',
    'c#',
    'd',
    'd#',
    'e',
    'f',
    'f#',
    'g',
    'g#',
    'a',
    'a#',
    'b'
    ];

var
  //initialized in initialization
  BaseNumber: Single;
  BaseOctave: Integer;
  BaseNoteFreqC4: Single; // yes i am using C4 not A4
  BaseNoteIndex: Integer;
  BaseDuration: Single;
  BaseTempo: Single;

implementation

function CharToStr(C: Char):String;
begin
  if C <= #32 then
    Result := '#' + IntToStr(Ord(C))
  else
    Result := C;
end;

{ EMelodyException }

constructor EMelodyException.Create(Msg: String; Line, CharIndex: Integer);
begin
  inherited Create(Msg + '[' + IntToStr(Line) + ',' + IntToStr(CharIndex) + ']');
end;

{ TMelodyChannel }

constructor TMelodyChannel.Create(AMelody: TMelody);
begin
  inherited Create;
  Melody := AMelody;
  FVolume := 100;
end;

procedure TMelodyChannel.Prepare;
begin
  Notes := LowerCase(Notes);

  //Current values by default
  Tempo := 120;
  octave := 4;
  ShiftOctave := 0;
  NoteLength := 4; //note length
  SubSequent := 0; // 0 = legato 1 = normal 2 = staccato

  Line := 1; //Line for error messages
  Current := 1;

  if (Current <= Length(Notes)) then
    Chr := Notes[Current];
end;

procedure TMelodyChannel.Unprepare;
begin
end;

procedure TMelodyChannel.SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single);
begin
  SoundDuration := Duration + Rest;
  //WriteLn(Format('Frequency %f, Duration %f, Rest %f ', [Frequency, Duration, Rest]));
end;

function TMelodyChannel.PlaySound: Boolean;
begin
  Result := False;
end;

function TMelodyChannel.StopSound: Boolean;
begin
  Result := False;
end;

procedure TMelodyChannel.SetInstrument(Instrument: String);
begin
  WriteLn('SetInstrument(' + Instrument + ')');
end;

function TMelodyChannel.Next: Boolean;

  function IndexOfScore(Score: String): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to Length(Scores) - 1 do
    begin
      if SameText(Scores[i], Score) then
      begin
        Result := i;
        break;
      end;
    end;
  end;

  //playnote(char, number[-1,0,+1], number[1..16], number[1..2])
  //playnote('c#', 1, 0, 0)
  //playnote('r', 1)
  //playnote(20, 1) //by number
  function PlayNote(Note: String; Duration: Single; Offset: Integer = 0; Increase: Single = 0; Connected: Boolean = False): Boolean; overload;
  var
    f: Integer;
    r: Single;
    d: Single;
    index: Integer;
  begin
    f := 0;
    if (Note = 'r') or (Note = 'p') then
      f := 0
    else if TryStrToInt(Note, index) then
      f := floor((BaseNoteIndex + ShiftOctave) * Power(BaseNumber, Index))
    else
    begin
      index := IndexOfScore(Note);
      if index < 0 then
        raise Exception.Create('We dont have it in music:' + Note);// ,line, self.pos)
      //calc index using current octave
      index := ((octave + ShiftOctave) - BaseOctave) * 12 + index + offset;
      f := floor(BaseNoteFreqC4 * power(BaseNumber, index));
    end;
    //ref: https://music.stackexchange.com/questions/24140/how-can-i-find-the-length-in-seconds-of-a-quarter-note-crotchet-if-i-have-a-te
    //     http://www.sengpielaudio.com/calculator-bpmtempotime.htm
    //4 seconds for tempo = 60 beat per second, so what if tempo 120 and 2 for duration
    d := (BaseDuration / Duration) * (BaseTempo / Tempo) * (1 + Increase);
    r := 0; //legato
    if not Connected then //only if not connected to the next note
    begin
      if SubSequent = 1 then //normal
      begin
        r := d / 8;
        d := d - r;
      end
      else if SubSequent = 2 then //staccato
      begin
        r := d / 4;
        d := d - r;
      end;
    end;

    SetSound(f, d, r, Connected, Volume);
    Result := True;
  end;

  function PlayNote(Note: Integer; Duration: Single; Offset: Integer = 0; Increase: Single = 0; Connected: Boolean = False): Boolean; overload;
  begin
    Result := PlayNote(IntToStr(Note), Duration, Offset, Increase, Connected);
  end;

  procedure Reset;
  begin
    Tempo := 120;
    Octave := 4;
    ShiftOctave := 0;
    SetInstrument('');//To default
    NoteLength := 4; //note length
    SubSequent := 0; // 0 = legato 1 = normal 2 = staccato
  end;

  function Step: Boolean;
  begin
    Current := Current + 1;
    if (Current > Length(Notes)) then
    begin
      Chr := #0;
      Result := False;
    end
    else
    begin
      Chr := Notes[Current];
      Result := True;
    end;
  end;

  procedure Restart;
  begin
    Reset;
    Line := 1; //line for error messages
    Current := 0; //zero because we do step()
    Step;
  end;

  function ScanNumber(MaxChar: Integer = 0; Default: Single = 0): Single;
  var
    r: String;
  begin
    r := '';
    while Current <= Length(Notes) do
    begin
      if ((Chr >= '0') and (Chr <= '9')) or (Chr = '-') or (Chr = '+') or (Chr = '.') then
        r := r + Chr
      else
        break;

      Step;

      if (MaxChar <> 0) and (Length(r) >= MaxChar) then
        break;
    end;

    if Length(r) > 0 then
      Result := StrToFloat(r)
    else
      Result := Default;
  end;

  function Check(c: Char; t: array of Char): Boolean;
  var
    v: Char;
  begin
    for v in t do
    begin
      if c = v then
      begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;

  function Scan(t: array of Char): String;
  var
    r: String;
  begin
    r := '';
    while Current <= Length(Notes) do
    begin
      if Check(Chr, t) then
        r := r + Chr
      else
        break;
      Step;
    end;
    Result := r;
  end;

  function ScanTo(c: Char): String;
  begin
    Result := '';
    while Current <= Length(Notes) do
    begin
      if Chr = c then
      begin
        Step;
        Break;
      end;
      Result := Result + Chr;
      Step;
    end;
  end;

  procedure ScanEol;
  begin
    while Current <= Length(Notes) do
    begin
      if (Chr = #13) then
        Break;
      Step;
    end;
  end;

var
  Note: String;
  Offset: Integer;

  Duration: Single;
  Connected: Boolean;
  Increase, By: Single;
  Number: Integer;
  l: Single;
  S: String;
begin
  Result := False;
  while Current <= Length(Notes) do
  begin
    if Chr = '#' then
    begin
      Step;
      ScanEol;
    end
    else if Check(Chr, [' ', #9,  #10]) then //#13
      Step
    else if Chr = #13 then
    begin
      Line := Line + 1; //line only for error messages
      Step;
    end
    else if Chr = '!' then
    begin
      Reset;
      Step;
    end
    else if (Chr >= 'a') and (Chr <= 'g') then
    begin
      Note := Chr;
      Step;

      if Chr = '#' then
      begin
        Note := Note + '#';
        Step;
      end;

      Offset := 0;
      if Chr = '+' then
      begin
        offset := 1;
        Step;
      end
      else if (Chr = '-') then
      begin
        Offset := -1;
        Step;
      end;

      Duration := ScanNumber(0, NoteLength);
      if Duration > 96 then
        raise EMelodyException.Create('Length should be less or equal 96: your length is: ' + FloatToStr(Duration), Line, Current);

      if Chr = ',' then
      begin
        Offset := Offset + (Round(ScanNumber(0, Octave)) - Octave);
        Step;
      end;

      Increase := 0;
      By := 0.5;
      if (Chr = '.') then
      begin
        repeat
          Increase := Increase + By; //not sure about next dot
          By := By / 2;
        until not Step or (Chr <> '.');
      end;

      Connected := False;
      if (Chr = '&') then  //trying to use it, but i think i cant
      begin
        Step;
        Connected := True;
      end;
      Result := PlayNote(Note, Duration, Offset, Increase, Connected);
      exit;
    end
    else if Chr = 'n' then
    begin
      Step;
      Number := Round(ScanNumber(2, -1));
      if Number = -1 then
        raise EMelodyException.Create('"n" command need a number', Line, Current);
      Result := PlayNote(Number, NoteLength);
      exit;
    end
    else if Chr = 'q' then //by frequency
    begin
      Step;
      Number := Round(ScanNumber(0, -1));
      if Number = -1 then
        raise EMelodyException.Create('"q" command need a number', Line, Current);
      Result := playnote(number, NoteLength);
      exit;
    end
    else if Chr = 't' then
    begin
      Step;
      Tempo := Round(ScanNumber(0, Tempo));
    end
    else if Chr = 'l' then
    begin
      Step;
      l := ScanNumber;
      if l > 96 then
        raise EMelodyException.Create('"l" command, length should be less or equal 96: your length is:' + FloatToStr(l), Line, Current);
      Increase := 0;
      By := 0.5;
      if (Chr = '.') then
      begin
        repeat
          increase := increase + by; //not sure about next dot
          by := by / 2;
        until not Step or (Chr <> '.');
      end;

      NoteLength := l + Increase;
    end
    else if (Chr = 'p') or (Chr = 'r') then //rest or pause
    begin
      Step;
      Duration := ScanNumber(0, NoteLength);
      Increase := 0;
      By := 0.5;
      if Chr = '.' then
      begin
        repeat
          increase := increase + By; //not sure about next dot
          By := By / 2;
        until not Step or (Chr <> '.');
      end;
      if (Chr = '&') then  //skip it
        Step;
      Result := PlayNote('r', Duration, 0, Increase, True);
      exit;
    end
    else if Chr = 'o' then
    begin
      Step;
      Octave := Round(ScanNumber);
    end
    else if Chr = '>' then
    begin
      Step;
      Number := Round(ScanNumber(1, 1));
      Octave := Octave + Number;
    end
    else if Chr = '<' then
    begin
      Step;
      Number := Round(ScanNumber(1, 1));
      Octave := self.octave - Number;
    end
    else if Chr = 's' then //shift octave, special for compatibility with some devices
    begin
      Step;
      Number := Round(ScanNumber(0, -1));
      if Number >= 0 then
        ShiftOctave := ShiftOctave + Number
      else
        ShiftOctave := 0;
    end
    else if Chr = ',' then
    begin
      Step;
      raise EMelodyException.Create('"," not supported yet, it used to split song to multiple channels, use play(note1, note2)', Line, Current);
    end
    else if Chr = ';' then //stop
    begin
      Step;
      Result := False; //finish it
      exit;
    end
    else if Chr = 'v' then
    begin
      Step;
      Volume := ScanNumber;
    end
    else if Chr = 'i' then //set a instrument
    begin
      Step;
      if Chr = '[' then
      begin
        Step;
        S := ScanTo(']'); //by name
        SetInstrument(S);
      end
      else
        SetInstrument(''); //Default
    end
    else if Chr = 'm' then
    begin
      Step;
      if Chr = 'l' then //legato
      begin
        Step;
        self.subsequent := 0;
      end
      else if Chr = 'n' then //normal
      begin
        Step;
        Subsequent := 1;
      end
      else if Chr = 's' then //staccato
      begin
        Step;
        SubSequent := 2;
      end
      else if Chr = 'r' then //repeat it
      begin
        Step;
        //Number := Round(ScanNumber); //todo
        Restart;
        //Restart(number);
      end
      else if Chr = 'x' then //exit
      begin
        Step;
        exit(False);
      end
      else if Chr = 'f' then //just for compatibility
        Step
      else if Chr = 'b' then
        Step
      else
      begin
        Step;
        raise EMelodyException.Create('Illegal subcommand for M' + CharToStr(Chr), Line, Current);
      end;
    end
    else
      raise EMelodyException.Create('Can not recognize: ' + CharToStr(Chr), Line, Current);
  end;
end;

{ TMelody }

procedure TMelody.BeforePlay;
begin

end;

procedure TMelody.AfterPlay;
begin

end;

constructor TMelody.Create;
begin
  inherited Create;
end;

destructor TMelody.Destroy;
begin
  inherited Destroy;
end;

function TMelody.CreateChannel: TMelodyChannel;
begin
  Result := TMelodyChannel.Create(Self);
end;

procedure TMelody.Play(Song: TArray<TmmlNotes>);
var
  Channels: TMelodyChannels;
  Channel: TMelodyChannel;
  mml: TmmlNotes;
  Index: Integer;
  Busy: Boolean;//At least one of channel is playing
begin
  BeforePlay;
  Channels := TMelodyChannels.Create;
  try
    for mml in Song do
    begin
      Channel := CreateChannel;
      Channel.Volume := 100;
      Channel.Finished := False;
      Channel.Name := 'notdefined';
      Channel.ID := Channels.Add(Channel);
      Channel.Name := IntToStr(Channel.ID);
      Channel.Notes := mml;
      Channel.Prepare;

      Index := 0;
      Busy := False;
      while True do
      begin
        if Terminated then
          break;
        Channel := Channels[Index];
        if not Channel.Finished then
        begin
          if (Channel.Expired > 0) and (Channel.Expired > GetTickCount64) then //Still waiting to finish playing
            Busy := True
          else
          begin
            Channel.StopSound;//no if we like to make some waves say playing
            if Channel.Next then //SetSound will be in Next function
            begin
              //WriteLn(ch.name, 'n, freq Hz, len ms, rest ms', ch.pos, ch.sound.pitch, math.floor(ch.sound.length * 100), math.floor(ch.sound.rest * 100))
              Channel.Expired := Round(Channel.SoundDuration * 1000) + GetTickCount64 + 1;
              if not Channel.PlaySound then //Assuming can't play it, idk why
                break;
              Busy := True;
            end
            else
            begin
              Channel.Finished := True;
              Channel.Unprepare;
            end;
          end;
        end;
        index := index + 1;
        if index >= Channels.Count then
        begin
          index := 0;
          if not Busy then
            break
          else
            busy := False;
        end;
      end;
    end;
  finally
    Channels.Free;
    AfterPlay;
  end;
end;

initialization
  BaseNumber := power(2, 1 / 12);
  BaseOctave := 4;
  BaseNoteFreqC4 := 261.63; // yes i am using C4 not A4
  BaseNoteIndex := 39;
  BaseDuration := 4;
  BaseTempo := 60;
end.
