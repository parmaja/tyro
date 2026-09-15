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
  mnLogs, mnUtils, mnConfigs,
  RayLib, RayClasses, TyroScripts,
  TyroClasses, TyroControls, TyroConsoles,
  TyroSprites, TyroPhysics,
  TyroEditors,
  mnClasses;

const
  TyroVersion: Double = 0.1;
  TyroVersionString = '0.1';

  sPromptChar: UTF8string = '>';
  sPromptDOT: UTF8string = '*';

var
  // Debug switch: when True, DBG messages are written to the console.
  // Off by default (globals are zero-initialized) to keep the output clean.
  IsDebug: Boolean = False;

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

  { TTyroMain }

  TConsoleReadEvent = procedure(AConsole: TTyroConsole; AInput: string) of object;

  TTyroMain = class(TTyroMainWindow)
  private
    //FBoard: TTyroImage;
    function GetActive: Boolean;

    procedure Help_Command(Params: TStrings);
    procedure Dir_Command(Params: TStrings);
    procedure Clear_Command(Params: TStrings);
    procedure Exit_Command(Params: TStrings);
    procedure Load_Command(Params: TStrings);
    procedure State_Command(Params: TStrings);
    procedure Run_Command(Params: TStrings);
    procedure Stop_Command(Params: TStrings);
    procedure Edit_Command(Params: TStrings);
    procedure EditorClosed(Sender: TObject);
    procedure EditorSave(Sender: TObject);
    procedure ReloadAndRunScript;
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
    Output: TTyroOutput;
    Editor: TyroEditor;
    Graphic: TTyroCanvas;
    Sprites: TSprites;
    Physics: TPhysics;
    constructor Create(AParent: TTyroLayout); override;
    destructor Destroy; override;
    procedure Stop; //and wait
    procedure Init; override;
    procedure Terminate; override;
    procedure ProcessQueue;
    procedure Start; override;
    procedure LoadConfig;
    procedure Unload; override;
    procedure Shutdown; override;
    procedure PrepareDraw; override;
    procedure Draw; override;
    procedure Update; override;
    procedure ProcessInput; override;
    //property Board: TTyroImage read FBoard;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtentions: TStringArray; AScriptClass: TTyroScriptClass);

     procedure ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean = False); override;
     procedure ShowConsole(AX, AY, AWidth, AHeight: Integer); overload;
     procedure ShowConsole; overload;
     procedure HideConsole;
     procedure ToggleConsole;
     procedure ToggleOutput;
     procedure ShowEditor;
     procedure HideEditor;
     procedure ToggleEditor;
     procedure Resize(AWidth, AHeight: Integer); override;

    property Queue: TQueueObjects read FQueue;
    property ScriptTypes: TScriptTypes read FScriptTypes;

  end;
{
  function IntToFPColor(I: Integer): TFPColor;
  function FPColorToInt(C: TFPColor): Integer;
  function RayColorOf(Color: TFPColor): TRGBAColor;
}
var
  Main : TTyroMain = nil;

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

{ TTyroMain }

function TTyroMain.GetActive: Boolean;
begin
  Result := Running or ((FScriptThread <> nil) and FScriptThread.Active) or ((FScriptMain <> nil) and (FScriptMain.Active));
end;

procedure TTyroMain.ProcessQueue;
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
      fpd := (1 / FramePerSeconds);
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

procedure TTyroMain.Start;
begin
  inherited;
  LoadConfig;
  if (FScriptThread <> nil) and not FScriptThread.Started then
    FScriptThread.Start;

  //if (FScriptMain <> nil) and not FScriptMain.Started then
  if (FScriptMain <> nil) then
    FScriptMain.Start;
end;

procedure TTyroMain.LoadConfig;
var
  aColor: string;
begin
  with Resources do
  begin
    FPS := Config.ReadInteger('fps', FramePerSeconds);
    FramePerSeconds := FPS;
    IsDebug := Config.ReadBool('debug', IsDebug);
    aColor := Config.ReadString('backcolor', '');
    if aColor <> '' then
      BackColor := StrToColor(aColor);

    if Config.Sections.ReadBool('show', 'fps', True) then
      Options := Options + [moShowFPS]
    else
      Options := Options - [moShowFPS];

    if Config.Sections.ReadBool('show', 'console', False) then
    begin
      ShowConsole(0, 0, 0, 0);
      StartConsoleRead;
    end;

    if Config.Sections.ReadBool('show', 'log', False) then
      Output.Show;
  end;
end;

procedure TTyroMain.Unload;
begin
  inherited;
end;

procedure TTyroMain.Shutdown;
begin
  Running := False;
  if (FScriptThread <> nil) then
    FScriptThread.Terminate;
  if (FScriptMain <> nil) then
    FScriptMain.Terminate;
end;

procedure TTyroMain.PrepareDraw;
begin
  ProcessQueue;
end;

constructor TTyroMain.Create(AParent: TTyroLayout);
begin
  inherited;
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
  Console.BoundsRect := Rect(Margin, Margin , 100, 100);
  Console.Border:= brdSizable;
  Console.BackColor := clDarkGray;
  Console.Visible := False;
  Console.Focused := True;
  Console.Visible := False;
  Console.Focused := True;
  Console.OnInput := ConsoleInput;
  Console.Margin:= 5;
  Console.Align:= alBottom;
  Output := TTyroOutput.Create(Self);
  Output.BoundsRect := Rect(Margin, Margin, 480, 240);
  Output.Visible := False;
  Editor := TyroEditor.Create(Self);
  Editor.BoundsRect := Rect(0, 0, 200, 200);
  Editor.Visible := False;
  Editor.OnClose := EditorClosed;
  Editor.OnSave := EditorSave;
  Sprites := TSprites.Create;
  Physics := TPhysics.Create(Sprites);
  Commands := TConsoleCommands.Create();
  RegisterCommands;
end;

   destructor TTyroMain.Destroy;
begin
  //Stop;
  FreeAndNil(Physics);
  FreeAndNil(Sprites);
  FreeAndNil(Graphic);
  FreeAndNil(FQueue);
  FreeAndNil(FScriptTypes);
  FreeAndNil(Commands);
  inherited;
end;

procedure TTyroMain.Init;
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

   procedure TTyroMain.Draw;
begin
  if Graphic <> nil then
  begin
    // Canvas layer sits at the bottom: blit the legacy Graphic first so its
    // opaque background does not cover the sprites drawn on top of it.
    Graphic.PostDraw;
    try
      Sprites.DrawAll;
    except
      on E: Exception do
      begin
        if IsConsole then WriteLn('EX-DRAW: ' + E.ClassName + ': ' + E.Message);
        raise;
      end;
    end;
    // scripted on_draw() overlays run last so they stay above the texture
    // and the legacy Graphic layer
    Sprites.DrawScripts;
  end;
  ThreadSwitch; //Yield
end;

procedure TTyroMain.Update;
var
  Scripted: array[0..1023] of TCollisionEvent;
  Enough: Integer;
  I: Integer;
  ev: TCollisionEvent;
  AState: string;
begin
  inherited;
  try
    if Physics <> nil then
      Physics.Step(RayLib.GetFrameTime());
  except
    on E: Exception do
    begin
      if IsConsole then
      begin
        WriteLn('EX-STEP: ' + E.ClassName + ': ' + E.Message + ' @' + IntToHex(NativeUInt(ExceptAddr), 16));
      end;
      raise;
    end;
  end;
  // Fire per-sprite on_collide handlers (main thread) for events touching scripted sprites
  try
    if Physics <> nil then
    begin
      Physics.SplitScriptedEvents;
      Physics.PollScripted(Scripted, Enough);
      for I := 0 to Enough - 1 do
      begin
        ev := Scripted[I];
        if ev.State = csBegin then
          AState := 'enter'
        else
          AState := 'leave';
        if Sprites.HasScript(ev.HandleA) then
          Sprites.GetScript(ev.HandleA).OnCollide(ev.HandleB, AState);
        if Sprites.HasScript(ev.HandleB) then
          Sprites.GetScript(ev.HandleB).OnCollide(ev.HandleA, AState);
      end;
    end;
    if Sprites <> nil then
      Sprites.UpdateScripts;
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EX-SCRIPT: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
  // Update console (handles caret blinking internally)
  try
    if Console <> nil then
      Console.Update;
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EX-CONSOLE: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
  // Update editor (caret blink and mouse interaction)
  try
    if Editor <> nil then
      Editor.Update;
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EX-EDITOR: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
  ThreadSwitch; //Yield
  {if not Active then
    Terminate;}
end;

procedure TTyroMain.ProcessInput;
begin
  inherited;
  // F2 toggles the script editor
  if RayLib.IsKeyPressed(KEY_F2) then
    ToggleEditor;
  // F7 toggles the output control
  if RayLib.IsKeyPressed(KEY_F7) then
    ToggleOutput;
  // F8 toggles the console
  if RayLib.IsKeyPressed(KEY_F8) then
    ToggleConsole;
  // Handle ESC to hide console when it's active and focused
  if (Console.Visible) and Console.Focused and RayLib.IsKeyPressed(KEY_ESCAPE) then
  begin
    HideConsole;
  end;
end;

procedure TTyroMain.RegisterLanguage(ATitle: string; AExtentions: TStringArray; AScriptClass: TTyroScriptClass);
var
  Item: TScriptType;
begin
  Item := TScriptType.Create;
  Item.Title := ATitle;
  Item.Extentions := AExtentions;
  Item.ScriptClass := AScriptClass;
  FScriptTypes.Add(Item);
end;

procedure TTyroMain.ShowWindow(AWidth, AHeight: Integer; ATextureMode: Boolean);
begin
  inherited;
  if AWidth = 0 then
    raise exception.Create('Screen width can not be 0');
  if AHeight = 0 then
    raise exception.Create('Screen height can not be 0');
  Graphic := TTyroTextureCanvas.Create(AWidth - 2 * (BorderSize + Margin), AHeight - 2 * (BorderSize + Margin), True);
  //Console.BoundsRect := Rect(Margin, Margin , 50, 50);
  //Console.WindowRect := Rect(Margin, Margin , AWidth - Margin, AHeight - Margin);
end;

procedure TTyroMain.Resize(AWidth, AHeight: Integer);
begin
  inherited Resize(AWidth, AHeight);
  if Graphic <> nil then
    Graphic.Resize(AWidth - 2 * (BorderSize + Margin), AHeight - 2 * (BorderSize + Margin));
  if (Editor <> nil) and Editor.Visible then
    Editor.BoundsRect := Rect(0, 0, AWidth - 2 * (BorderSize + Margin), AHeight - 2 * (BorderSize + Margin));
end;

procedure TTyroMain.Stop;
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

procedure TTyroMain.Terminate;
begin
  HideWindow;
  Stop;
  inherited;
end;

procedure TTyroMain.ShowConsole(AX, AY, AWidth, AHeight: Integer);
begin
  if (AWidth <= 0) or (AHeight <= 0) then
  begin
    AWidth := 80;
    AHeight := 25;
  end;
  Console.BoundsRect := Rect(AX, AY, AX + AWidth, AY + AHeight);
  ShowConsole;
end;

procedure TTyroMain.ShowConsole;
begin
  Console.CharWidth := Resources.Font.Width;
  Console.CharHeight := Resources.Font.Height;
  Console.Show;
  StartConsoleRead;
end;

procedure TTyroMain.HideConsole;
begin
  Console.StopRead;
  Console.Hide;
end;

procedure TTyroMain.ToggleConsole;
begin
  if Console.Visible then
    HideConsole
  else
    ShowConsole;
end;

procedure TTyroMain.ToggleOutput;
begin
  Output.Visible := not Output.Visible;
  if Output.Visible then
    Output.BringToFront;
end;

procedure TTyroMain.ShowEditor;
var
  w, h: Integer;
begin
  if FScriptThread = nil then
  begin
    Log.Writeln('No script loaded. Use "load <script>" first.');
    Exit;
  end;
  //stop the current run so the edited source is not being executed
  Editor.FileName := FScriptThread.Script.FileName;
  Editor.LoadSource(FScriptThread.Script.Source);
  Editor.BoundsRect := Rect(0, 0, Width, Height);
  Editor.Margin:= 10;
  Editor.BackColor:= clBlack;
  Editor.Visible := True;
  Editor.Focused := True;
end;

procedure TTyroMain.HideEditor;
begin
  if Editor.Visible then
  begin
    if FScriptMain <> nil then
    begin
      Editor.SaveSource(FScriptMain.Source);
      ReloadAndRunScript;
    end;
    Editor.Visible := False;
    if Console.Visible then
    begin
      Console.Focused := True;
      StartConsoleRead;
    end;
  end;
end;

procedure TTyroMain.ToggleEditor;
begin
  if Editor.Visible then
    HideEditor
  else
    ShowEditor;
end;

procedure TTyroMain.EditorClosed(Sender: TObject);
begin
  HideEditor;
end;

procedure TTyroMain.EditorSave(Sender: TObject);
begin
  if FScriptMain <> nil then
  begin
    Editor.SaveSource(FScriptMain.Source);
    ReloadAndRunScript;
  end;
end;

//Save the edited source to disk, reload the script from it, and run it again
procedure TTyroMain.ReloadAndRunScript;
var
  aFileName: string;
begin
  if FScriptMain = nil then
    Exit;
  FScriptMain.Stop;
  aFileName := IncludePathDelimiter(FScriptMain.Path) + FScriptMain.FileName;
  try
    FScriptMain.Source.SaveToFile(aFileName);
  except
    on E: Exception do
    begin
      if IsConsole then WriteLn('EDITOR-SAVE: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
  FScriptMain.LoadFile(aFileName);
  FScriptMain.Start;
end;

procedure TTyroMain.Edit_Command(Params: TStrings);
begin
  ShowEditor;
end;

procedure TTyroMain.ConsoleInput(AConsole: TTyroConsole; AInput: string);
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

procedure TTyroMain.ExecuteCommand(ACommand: string);
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

procedure TTyroMain.RegisterCommands;
begin
  Commands.Add('help', ['?'], Help_Command, 'Show help');
  Commands.Add('list', ['ls'], Dir_Command, 'Show current directory');
  Commands.Add('clear', ['cls'], Clear_Command, 'List files in current directory');
  Commands.Add('exit', ['quit', 'q'], Exit_Command, 'Hide console and stop');
  Commands.Add('stop', [], Stop_Command, 'Stop current script');
  Commands.Add('load', [], Load_Command, 'Load script name from current directory');
  Commands.Add('state', [], State_Command, 'State of current directory');
  Commands.Add('run', [], Run_Command, 'Run current loaded script');
  Commands.Add('edit', [], Edit_Command, 'Edit the current loaded script (F2)');
end;

procedure TTyroMain.StartConsoleRead;
begin
  // Clear any script callback so built-in commands are executed
  FReadCallback := nil;
  Console.OnInput := ConsoleInput;
  Console.StartRead(clBlack, clBlack, sPromptChar, clLightGray, clBlack);
end;

procedure TTyroMain.StartConsoleReadEx(ACallback: TConsoleReadEvent);
begin
  FReadCallback := ACallback;
  Console.OnInput := ACallback;
  Console.StartRead(clBlack, clBlack, sPromptChar, clLightGray, clBlack);
end;

procedure TTyroMain.Help_Command(Params: TStrings);
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

procedure TTyroMain.Dir_Command(Params: TStrings);
var
  DirPath: string;
  sr: TSearchRec;
  aFile: string;
begin
  Console.Writeln('Directory: ' + Resources.CurrentDirectory);
  DirPath := ExcludeTrailingPathDelimiter(Resources.CurrentDirectory);
  if Params.Count > 0 then
    aFile := Params[0]
  else
      aFile := '*.*';
  if FindFirst(DirPath + PathDelim + aFile, faAnyFile, sr) = 0 then
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

procedure TTyroMain.Clear_Command(Params: TStrings);
begin
  Console.Clear;
end;

procedure TTyroMain.Exit_Command(Params: TStrings);
begin
  HideConsole;
  Stop;
  Terminate;
end;

procedure TTyroMain.Run_Command(Params: TStrings);
begin
  if (FScriptMain <> nil) then
  begin
    FScriptMain.Start;
  end
  else
  begin
    Console.Writeln('No script loaded. Use "load <script>" to load a script first.');
  end;
end;

procedure TTyroMain.Stop_Command(Params: TStrings);
begin
  Stop;
end;

procedure TTyroMain.Load_Command(Params: TStrings);
var
  aFile, aFileName: string;
  aScriptType: TScriptType;
  aScript: TTyroScript;
begin
  if (Params.Count = 0) then
  begin
    Console.Writeln('Usage: load <script_name>');
    Exit;
  end;

  aFile := Params[0];
  aScriptType := ScriptTypes.FindByExtension(ExtractFileExt(aFile));
  aFileName := IncludePathDelimiter(Resources.CurrentDirectory) + aFile;

  if SysUtils.FileExists(aFileName) then
  begin
    if aScriptType = nil then
    begin
      Console.Writeln('Unknown script type for: ' + aFile);
      Exit;
    end;
    aScript := aScriptType.ScriptClass.Create;
    aScript.LoadFile(aFileName);
    Console.Writeln('Loaded: ' + aFile);
  end
  else
  begin
    Console.Writeln('Script not found: ' + aFile);
    FreeAndNil(aScript);
    Exit;
  end;

  // Stop previous script if running
  if (FScriptMain <> nil) then
  begin
    FScriptMain.Stop;
    FreeAndNil(FScriptMain);
  end;
  Stop;
  FScriptMain := aScript;
  //Start;
  //FScriptMain.RUNINMAIN := True;
end;

procedure TTyroMain.State_Command(Params: TStrings);
begin
  if Active and (FScriptMain <> nil) then
    Console.Writeln(FScriptMain.FileName + ' is running');
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
  Main := TTyroMain.Create(nil);
finalization
  FreeAndNil(Main);
end.


