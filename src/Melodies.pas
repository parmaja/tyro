unit Melodies;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{**
 * This file is part of the 'Tyro'
 * @description Simple Wave generator using RayLib
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
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
//////////////////////////////////////////////////////////////////////////////-}

interface

uses
  Classes, SysUtils, mnClasses, mnUtils, Math;

type

  { EMelodyException }

  EMelodyException = class(Exception)
  public
    constructor Create(Msg: string; Line, CharIndex: Integer);
  end;

  TmmlNotes = Utf8String;

  TMelody = class;

  { TMelodyChannel }

  TMelodyChannel = class(TmnNamedObject)
  private
    FVolume: Single;
  public
    Melody: TMelody;
    ID: Integer;
    Finished: Boolean;
    Source: TObject;//
    Chr: Char;
    Line, Current: Integer;
    Notes: TmmlNotes;
    Tempo: Integer;
    Octave: Integer;
    NoteLength: Single;
    SubSequent: Integer;
    ShiftOctave: Integer;
    Instrument: String;
    Expired: LongInt;
    SoundLength: Integer;
    SoundRest: Integer;
    constructor Create(AMelody: TMelody);
    procedure Prepare(ANotes: TmmlNotes);
    procedure SetWaveform(Instrument: String); virtual;
    procedure SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single); virtual;
    function PlaySound: Boolean; virtual;
    function Next: Boolean;
    property Volume: Single read FVolume write FVolume;
  end;

  { TMelodyChannels }

  TMelodyChannels = Class(TmnNamedObjectList<TMelodyChannel>)
  end;

  { TMelody }

  TMelody = class(TObject)
  private
  public
    Terminated: Boolean;
    constructor Create;
    destructor Destroy; override;

    procedure Play(Instrument: String; Song: TArray<TmmlNotes>);
  end;

  //		http://www.headchant.com/2011/11/01/sound-synthesis-with-love-part-ii-sine-waves/
  //      https://stackoverflow.com/questions/11355353/how-can-i-convert-qbasic-play-commands-to-something-more-contemporary
  //ref:  http://www.phy.mtu.edu/~suits/notefreqs.html
  // 		http://wiki.mabinogiworld.com/view/User:LexisMikaya/MML_101_Guide
  //ref:	The calculation of freq: http://www.phy.mtu.edu/~suits/NoteFreqCalcs.html
  // Between C4 and C4# is one step so C4 freq * (2^1/12) = 262 * 1.059463
  // Between C4 and D4 is 2 steps = so C4 freq * (2^1/12) = 262 * 1.059463 ^ 2

  //?    f = 440 * 2^((n-49)/12)

  //////////////////////////////////////////////////////////////////////////////-
  //ref: http://www.qb64.net/wiki/index.php?title=SOUND
  {					The Seven Music Octaves

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
  baseNumber : Single;
  baseOctave : Integer;
  baseNoteC4 : Single; // yes i am using C4 not A4
  baseNote   : Single;
  baseLength : Integer;
  baseTempo  : Integer;

implementation

{ EMelodyException }

constructor EMelodyException.Create(Msg: string; Line, CharIndex: Integer);
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

procedure TMelodyChannel.Prepare(ANotes: TmmlNotes);
begin
  Notes := LowerCase(ANotes);

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

procedure TMelodyChannel.SetSound(Frequency, Duration, Rest: Single; Connected: Boolean; Volume: Single);
begin
  WriteLn(Format('Frequency %d, Duration %d, Rest %d ', [Frequency, Duration, Rest]));
end;

function TMelodyChannel.PlaySound: Boolean;
begin
  Result := False;
end;

procedure TMelodyChannel.SetWaveform(Instrument: String);
begin
  WriteLn('SetWaveForm('+Instrument+')');
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
    l: Single;
    index: Integer;
  begin
    f := 0;
    if (Note = 'r') or (Note = 'p') then
      f := 0
    else if TryStrToInt(Note, index) then
      f := floor((baseNote + ShiftOctave) * Power(baseNumber, index))
    else
    begin
      index := IndexOfScore(Note);
      if index < 0 then
        raise Exception.Create('We dont have it in music:' + Note);// ,line, self.pos)
      //calc index using current octave
      index := ((octave + ShiftOctave)- baseOctave) * 12 + index + offset;
      f := floor(baseNoteC4 * power(baseNumber, index));
    end;
    //ref: https://music.stackexchange.com/questions/24140/how-can-i-find-the-length-in-seconds-of-a-quarter-note-crotchet-if-i-have-a-te
    //     http://www.sengpielaudio.com/calculator-bpmtempotime.htm
    //4 seconds for tempo = 60 beat per second, so what if tempo 120 and 2 for duration
    l := (baseLength / duration) * (baseTempo / tempo) * (1 + increase);
    r := 0; //legato
    if not Connected then //only if not connected to the next note
    begin
        if SubSequent = 1 then //normal
        begin
            r := l / 8;
            l := l - r;
        end
        else if SubSequent = 2 then //staccato
        begin
            r := l / 4;
            l := l - r;
        end
    end;

    SetSound(f, l, r, Connected, Volume);
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
    SetWaveForm('');//To default
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
    end
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
    r: string;
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

  function Scan(t: array of Char): string;
  var
    r: string;
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

  function ScanTo(c: Char): string;
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
    end
  end;

  procedure ScanEol;
  begin
    while Current <= Length(Notes) do
    begin
      if (Chr = '\n') then
        Break;
      Step;
    end
  end;

var
  Note: string;
  Offset: Integer;

  Duration: Single;
  Connected: Boolean;
  Increase, By: Single;
  Number: Integer;
  l: Single;
  S: string;
begin
  while Current<= Length(Notes) do
  begin
      if Chr = '#' then
      begin
        Step;
        ScanEol;
      end
      else if Check(Chr, [' ', #9, #13]) then
        Step
      else if Chr = '\n' then
      begin
        Line := Line + 1; //line only for error messages
        Step;
      end
      else if Chr = '!' then
      begin
        Reset;
        Step;
      end
      else if (Chr >= 'a') and (Chr <='g') then
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
            Connected := true;
          end;
          Result := PlayNote(Note, Duration, Offset, Increase, Connected);
      end
      else if Chr = 'n' then
      begin
          Step;
          Number := Round(ScanNumber(2, -1));
          if Number = -1 then
              raise EMelodyException.Create('n command need a number', Line, Current);
          Result := PlayNote(Number, NoteLength);
      end
      else if Chr = 'q' then //by frequency
      begin
          Step;
          Number := Round(ScanNumber(0, -1));
          if Number = -1 then
            raise EMelodyException.Create('q command need a number', Line, Current);
          Result := playnote(number, NoteLength);
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
          raise EMelodyException.Create('Length should be less or equal 96: your length is:' + FloatToStr(l), Line, Current);
        Increase := 0;
        By := 0.5;
        if (Chr = '.') then
        begin
          repeat
            increase := increase + by; //not sure about next dot
            by := by / 2;
          until not Step or (Chr <> '.')
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
            until not Step or (Chr <> '.')
          end;
          if (Chr = '&') then  //skip it
            Step;
          Result := PlayNote('r', Duration, 0, Increase, True);
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
        raise EMelodyException.Create('command , not supported, it used to split song to multiple channels, use play(note1, note2)', Line, Current);
      end
      else if Chr = ';' then //stop
      begin
        Step;
        Result := false; //finish it
        exit;
      end
      else if Chr = 'v' then
      begin
        Step;
        Volume := ScanNumber;
      end
      else if Chr = 'w' then //set a waveform
      begin
          Step;
          if Chr = '[' then
          begin
              Step;
              S := ScanTo(']'); //by name
              SetWaveform(S);
          end
          else
          begin
              Number := Round(ScanNumber);
              //SetWaveForm(Number);
          end
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
          else if Chr='r' then //repeat it
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
            raise EMelodyException.Create('Illegal subcommand for M' + Chr, Line, Current)
          end
      end
      else
          raise EMelodyException.Create('Can not recognize: ' + Chr, Line, Current)
  end
end;

{ TMelody }

constructor TMelody.Create;
begin
  inherited Create;
end;

destructor TMelody.Destroy;
begin
  inherited Destroy;
end;

procedure TMelody.Play(Instrument: String; Song: TArray<TmmlNotes>);
var
  Channels: TMelodyChannels;
  Channel: TMelodyChannel;
  mml: TmmlNotes;
  Index: Integer;
  Count: Integer;
  Busy: Boolean;
begin
  Channels := TMelodyChannels.Create;
  try
    for mml in Song do
    begin
      Channel := TMelodyChannel.Create(Self);
      Channel.Volume := 100;
      Channel.Source := nil;
      Channel.Finished := false;
      Channel.Name := 'notdefined';
      Channel.ID :=  Channels.Add(Channel);
      Channel.Name := IntToStr(Channel.ID);
      Channel.Prepare(mml);

      Index := 0;
      Busy := False;
      Count := Channels.Count;
      while true do
      begin
        if Terminated then
          break;
        Channel := Channels[Index];
        if not Channel.Finished then
        begin
          if (Channel.Expired > 0) and (Channel.Expired > GetTickCount64) then
              Busy := true
          else if Channel.Next then
          begin
              //WriteLn(ch.name, 'n, freq Hz, len ms, rest ms', ch.pos, ch.sound.pitch, math.floor(ch.sound.length * 100), math.floor(ch.sound.rest * 100))
              Channel.Expired := GetTickCount64 + Channel.SoundLength + Channel.SoundRest;
              if not Channel.PlaySound then
                break;
              Busy := True;
          end
          else
          begin
            Channel.finished := True;
            FreeAndNil(Channel.Source);
          end
        end;
        index := index + 1;
        if index > count then
        begin
            index := 1;
            if not Busy then
                break
            else
                busy := False;
        end;
      end;
    end;
  finally
    Channels.Free;
  end;
end;

initialization
  baseNumber := power(2, 1/12);
  baseOctave := 4;
  baseNoteC4 := 261.63; // yes i am using C4 not A4
  baseNote   := 39;
  baseLength := 4;
  baseTempo  := 60;
end.

