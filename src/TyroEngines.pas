unit TyroEngines;
{$MODE DELPHI} {$H+}
 {**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey 
 *
 *}

interface

uses
  Classes, SysUtils, SyncObjs,
  mnLogs, mnUtils,
//  FPCanvas, FPImage,
  RayLib, RayClasses, TyroScripts,
  TyroClasses, TyroControls, TyroConsoles,
  mnClasses;

const
  TyroVersion = 1;
  TyroVersionString = '0.1';

//  sPromptChar = '>';
  sPromptChar = '›';
  sPromptDOT: UTF8string = #$25CF;


type

  TProcedureObject = procedure(Params: TStrings) of object;

  TConsoleCommand = class(TmnNamedObject)
  public
    Alts: TStringArray;
    Proc: TProcedureObject;
    Note: string;
    SyncIt: Boolean;
    procedure Execute;
  end;

  TExecuteCommand = class(TObject)
  public
    Command: TConsoleCommand;
    Params: TStrings;
    procedure Execute;
  end;

  { TConsoleCommands }

  TConsoleCommands = class(TmnNamedObjectList<TConsoleCommand>)
  public
    function Add(Sync: Boolean; Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string = ''): TConsoleCommand; overload;
    function Add(Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string = ''): TConsoleCommand; overload;
    function Find(const Name: string): TConsoleCommand; overload;
    function Execute(Name: UTF8String; Params: TStrings = nil): Boolean; overload;
  end;

  TRunHow = (
    runLint,
    runCompile,
    //runLink,
    runExecute
  );

  { TTyroEngine }

  TConsoleReadEvent = procedure(AConsole: TTyroConsole; AInput: string) of object;

  TTyroEngine = class(TTyroMain)
  private
    //FBoard: TTyroImage;
    function GetActive: Boolean;

    procedure Help_Command(Params: TStrings);
    procedure Dir_Command(Params: TStrings);
    procedure Clear_Command(Params: TStrings);
    procedure Exit_Command(Params: TStrings);
  protected
    FQueue: TQueueObjects;
    FScriptThread: TTyroScriptThread;
    FScriptMain: TTyroScript; //only if we have main loop
    FScriptTypes: TScriptTypes;
    FReadCallback: TConsoleReadEvent;
  protected
    Commands: TConsoleCommands;
    procedure ConsoleInput(AConsole: TTyroConsole; AInput: string);
    procedure ExecuteCommand(ACommand: string);
    procedure RegisterCommands;
  public
    procedure StartConsoleRead;
    procedure StartConsoleReadEx(ACallback: TConsoleReadEvent);
  public
    RunInMain: Boolean;
    Running: Boolean;
    How: TRunHow;
    RunFile: string;//that to run in script
    Console: TTyroConsole;
    Graphic: TTyroCanvas;
    constructor Create;
    destructor Destroy; override;
    procedure Stop; //and wait
    procedure Init; override;
    procedure Terminate; override;
    procedure ProcessQueue;
    procedure Load; override;
    procedure Shutdown; override;
    procedure PrepareDraw; override;
    procedure Draw; override;
    procedure Update; override;
    procedure ProcessInput; override;
    //property Board: TTyroImage read FBoard;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtentions: TStringArray; AScriptClass: TTyroScriptClass);

     procedure ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean = False); override;
     procedure ShowConsole(AX, AY, AWidth, AHeight: Integer);
     procedure HideConsole;

    property Queue: TQueueObjects read FQueue;
    property ScriptTypes: TScriptTypes read FScriptTypes;

  end;
{
  function IntToFPColor(I: Integer): TFPColor;
  function FPColorToInt(C: TFPColor): Integer;
  function RayColorOf(Color: TFPColor): TRGBAColor;
}
var
  Main : TTyroEngine = nil;

implementation

{
function IntToFPColor(I: Integer): TFPColor;
begin
  Result.Alpha := I and $ff;
  Result.Alpha := Result.Alpha + (Result.Alpha shl 8);

  I := I shr 8;
  Result.Blue := I and $ff;
  Result.Blue := Result.Blue + (Result.Blue shl 8);
  I := I shr 8;
  Result.Green := I and $ff;
  Result.Green := Result.Green + (Result.Green shl 8);
  I := I shr 8;
  Result.Red := I and $ff;
  Result.Red := Result.Red + (Result.Red shl 8);
end;

function FPColorToInt(C: TFPColor): Integer;
begin
  Result := hi(C.Red);
  Result := Result shl 8;
  Result := Result or hi(C.Green);
  Result := Result shl 8;
  Result := Result or hi(C.Blue);
  Result := Result shl 8;
  Result := Result or hi(C.Alpha);
end;

function RayColorOf(Color: TFPColor): TRGBAColor;
begin
  Result.Alpha := hi(Color.Alpha);
  Result.Red := hi(Color.Red);
  Result.Green := hi(Color.Green);
  Result.Blue := hi(Color.Blue);
end;
}

{ TTyroEngine }

function TTyroEngine.GetActive: Boolean;
begin
  Result := Running or ((FScriptThread <> nil) and FScriptThread.Active) or ((FScriptMain <> nil) and (FScriptMain.Active));
end;

procedure TTyroEngine.ProcessQueue;
var
  p: TQueueObject;
  c: Integer;
  fpd: Double;
  ft, ft2: Double;
begin
  if Graphic <> nil then
  begin
    CanvasLock.Enter;
    try
      ft := GetTime();
      fpd := (1 / cFramePerSeconds);
      Graphic.BeginDraw;
      c := 0;
      while Queue.Count > 0 do
      begin
        Lock.Enter;
        try
          p := Queue.Extract(Queue[0]);
        finally
          Lock.Leave;
        end;
        p.Execute;
        p.Free;
        Inc(c);
        ft2 := GetTime() - ft;
        if ft2 >= fpd then
        begin
          break;
        end;
      end;
      Graphic.EndDraw;
    finally
      CanvasLock.Leave;
    end;
  end;
end;

procedure TTyroEngine.Load;
begin
  inherited;
  if (FScriptThread <> nil) and not FScriptThread.Started then
    FScriptThread.Start;

  //if (FScriptMain <> nil) and not FScriptMain.Started then
  if (FScriptMain <> nil) then
    FScriptMain.Start;
  Options := Options + [moShowFPS];
end;

procedure TTyroEngine.Shutdown;
begin
  Running := False;
  if (FScriptThread <> nil) then
    FScriptThread.Terminate;
  if (FScriptMain <> nil) then
    FScriptMain.Terminate;
end;

procedure TTyroEngine.PrepareDraw;
begin
  ProcessQueue;
end;

constructor TTyroEngine.Create;
begin
  inherited Create;
  Margin := 10;
  //SetTraceLog(LOG_DEBUG or LOG_INFO or LOG_WARNING);
  SetTraceLogLevel([LOG_ERROR, LOG_FATAL]);
  FQueue := TQueueObjects.Create(True);
  FScriptTypes := TScriptTypes.Create(true);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  //TTyroPanel.Create(Self);

  Console := TTyroConsole.Create(Self);
  Console.WindowRect := Rect(Margin, Margin , 100, 100);
  Console.Visible := False;
  Console.Focused := True;
  Console.Visible := False;
  Console.Focused := True;
  Console.OnInput := ConsoleInput;
  Commands := TConsoleCommands.Create();
  RegisterCommands;
end;

destructor TTyroEngine.Destroy;
begin
  //Stop;
  FreeAndNil(Graphic);
  FreeAndNil(FQueue);
  FreeAndNil(FScriptTypes);
  FreeAndNil(Commands);
  inherited;
end;

procedure TTyroEngine.Init;
var
  aScriptType: TScriptType;
  aScript: TTyroScript;
begin
  Running := True;
  ShowWindow(ScreenWidth, ScreenHeight); //with option to show window /w
  if RunFile <> '' then
  begin
    Log.WriteLn('File: ' + RunFile);
    aScriptType := ScriptTypes.FindByExtension(ExtractFileExt(RunFile));
    if aScriptType <> nil then
    begin
      aScript := aScriptType.ScriptClass.Create;
      if SysUtils.FileExists(RunFile) then
      begin
        if LeftStr(RunFile, 1) = '.' then
          RunFile := ExpandFileName(Resources.WorkSpace + RunFile);
        aScript.ScriptPath := ExtractFilePath(RunFile);
        aScript.LoadFile(RunFile);
        Resources.CurrentDirectory := ExtractFilePath(RunFile);
        if RunInMain then
          FScriptMain := aScript
        else
          FScriptThread := TTyroScriptThread.Create(aScript);
      end;
    end
    else
      Log.WriteLn('Type of file not found: ' + RunFile);
  end;
end;

procedure TTyroEngine.Draw;
begin
  if Graphic <> nil then
    Graphic.PostDraw;
  ThreadSwitch; //Yield
end;

procedure TTyroEngine.Update;
begin
  inherited;
  // Update console (handles caret blinking internally)
  if Console <> nil then
    Console.Update;
  ThreadSwitch; //Yield
  if not Active then
    Terminate;
end;

procedure TTyroEngine.ProcessInput;
begin
  inherited;
  // Handle ESC to hide console when it's active and focused
  if (Console.Visible) and Console.Focused and RayLib.IsKeyPressed(KEY_ESCAPE) then
  begin
    HideConsole;
  end;
end;

procedure TTyroEngine.RegisterLanguage(ATitle: string; AExtentions: TStringArray; AScriptClass: TTyroScriptClass);
var
  Item: TScriptType;
begin
  Item := TScriptType.Create;
  Item.Title := ATitle;
  Item.Extentions := AExtentions;
  Item.ScriptClass := AScriptClass;
  FScriptTypes.Add(Item);
end;

procedure TTyroEngine.ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean);
begin
  inherited;
  if AWidth = 0 then
    raise exception.Create('Screen width can not be 0');
  if AHeight = 0 then
    raise exception.Create('Screen height can not be 0');
  Graphic := TTyroTextureCanvas.Create(AWidth, AHeight, True);
  //Console.BoundsRect := Rect(Margin, Margin , 50, 50);
  //Console.WindowRect := Rect(Margin, Margin , AWidth - Margin, AHeight - Margin);
end;

procedure TTyroEngine.Stop;
begin
  Running := False;
  if FScriptThread <> nil then
  begin
    FScriptThread.Terminate;
    FScriptThread.WaitFor;
    FreeAndNil(FScriptThread);
  end;

  if FScriptMain <> nil then
  begin
    FScriptMain.Stop;
    FreeAndNil(FScriptMain);
  end;
end;

procedure TTyroEngine.Terminate;
begin
  HideWindow;
  Stop;
  inherited;
end;

procedure TTyroEngine.ShowConsole(AX, AY, AWidth, AHeight: Integer);
begin
  Console.CharWidth := Resources.Font.Width;
  Console.CharHeight := Resources.Font.Height;
  if (AWidth <= 0) or (AHeight <= 0) then
  begin
    AWidth := 80;
    AHeight := 25;
  end;
  Console.WindowRect := Rect(AX, AY, AX + AWidth, AY + AHeight);
  Console.Show;
  //StartConsoleRead;
end;

procedure TTyroEngine.HideConsole;
begin
  Console.StopRead;
  Console.Hide;
end;

procedure TTyroEngine.ConsoleInput(AConsole: TTyroConsole; AInput: string);
begin
  // If a script callback is set, route input to it (console.read())
  if Assigned(FReadCallback) then
  begin
    FReadCallback(AConsole, AInput);
    Exit;
  end;

  // Echo a newline after user input for readability
  Console.Writeln('');
  // Execute the typed command
  ExecuteCommand(AInput);
  // Re-arm the console for the next line of input
  if Console.Visible then
    StartConsoleRead;
end;

procedure TTyroEngine.ExecuteCommand(ACommand: string);
var
  Params: TStringList;
begin
  ACommand := Trim(ACommand);
  if ACommand <> '' then
  begin
    Params := TStringList.Create;
    ParseArguments(ACommand, Params, ['-', '/']);
    if (Params.Count>0) then
    try
      ACommand := Params[0];
      Params.Delete(0);
//          Write(#8);
      if not Commands.Execute(ACommand, Params) then
      begin
        Console.Writeln('Unknown command: ' + ACommand);
        Console.Writeln('Type "help" for available commands.');
        Console.Writeln('');
      end;
    finally
      FreeAndNil(Params);
    end;
  end;
end;

procedure TTyroEngine.RegisterCommands;
begin
  Commands.Add('Help', ['?'], Help_Command, 'Show help');
  Commands.Add('dir', ['ls'], Dir_Command, 'Show current directory');
  Commands.Add('clear', ['cls'], Clear_Command, 'List files in current directory');
  Commands.Add('exit', ['quit', 'q'], Exit_Command, 'Hide console and stop');
end;

procedure TTyroEngine.StartConsoleRead;
begin
  // Clear any script callback so built-in commands are executed
  FReadCallback := nil;
  Console.OnInput := ConsoleInput;
  Console.StartRead(clBlack, clBlack, '> ', clLightGray, clBlack);
end;

procedure TTyroEngine.StartConsoleReadEx(ACallback: TConsoleReadEvent);
begin
  FReadCallback := ACallback;
  Console.OnInput := ACallback;
  Console.StartRead(clBlack, clBlack, '> ', clLightGray, clBlack);
end;

procedure TTyroEngine.Help_Command(Params: TStrings);
var
  Command: TConsoleCommand;
begin
  Console.Writeln('Available commands:');
  for Command in Commands do
    if Command.Note <> '' then
      Console.Writeln(' '+sPromptDOT+' '+ Command.Name + ' : '+ Command.Note)
    else
      Console.Writeln(' '+sPromptDOT+' '+ Command.Name);
  Console.Writeln('');
end;

procedure TTyroEngine.Dir_Command(Params: TStrings);
var
  DirPath: string;
  sr: TSearchRec;
begin
  Console.Writeln('Directory: ' + Resources.CurrentDirectory);
  DirPath := ExcludeTrailingPathDelimiter(Resources.CurrentDirectory);
  if FindFirst(DirPath + PathDelim + '*.*', faAnyFile, sr) = 0 then
  begin
    try
      repeat
        Console.Writeln('  ' + sr.Name);
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end
  else
  begin
    Console.Writeln('  (empty)');
  end;
  Console.Writeln('');
end;

procedure TTyroEngine.Clear_Command(Params: TStrings);
begin
  Console.Clear;
end;

procedure TTyroEngine.Exit_Command(Params: TStrings);
begin
  HideConsole;
  Stop;
  Terminate;
end;

{ TConsoleCommand }

procedure TConsoleCommand.Execute;
begin
end;

{ TExecuteCommand }

procedure TExecuteCommand.Execute;
begin
  Command.Proc(Params);
  Free;
end;

{ TConsoleCommands }

function TConsoleCommands.Add(Sync: Boolean; Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string): TConsoleCommand;
begin
  Result := TConsoleCommand.Create;
  Result.Name := Name;
  Result.Alts := Alts;
  Result.Proc := proc;
  Result.Note := ANote;
  Result.SyncIt := Sync;
  inherited Add(Result);
end;

function TConsoleCommands.Add(Name: UTF8String; Alts: TStringArray; Proc: TProcedureObject; ANote: string): TConsoleCommand;
begin
  Result := Add(False, Name, Alts, Proc, ANote);
end;

function TConsoleCommands.Find(const Name: string): TConsoleCommand;
var
  i: integer;
begin
	if Name <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Name, Items[i].Name) or IsStrInArray(Name, Items[i].Alts) then
        exit(Items[i]);
    end;
  Result := nil;
end;

function TConsoleCommands.Execute(Name: UTF8String; Params: TStrings): Boolean;
var
  Command: TConsoleCommand;
  Exec :TExecuteCommand;
begin
  Command := Find(Name);
  Result := Command <> nil;
  if Result then
  begin
    Exec := TExecuteCommand.Create;
    Exec.Command := Command;
    Exec.Params := Params;
    Exec.Execute
  end;
end;

initialization
  Main := TTyroEngine.Create;
finalization
  FreeAndNil(Main);
end.


