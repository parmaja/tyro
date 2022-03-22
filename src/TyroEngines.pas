unit TyroEngines;
{$mode ObjFPC}{$H+}
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
  fgl,
  mnLogs, FPCanvas, FPImage,
  RayLib, RayClasses, TyroScripts,
  TyroClasses, TyroControls, TyroConsoles;

const
  TyroVersion = 1;
  TyroVersionString = '0.1';

type
  TRunHow = (
    runLint,
    runCompile,
    //runLink,
    runExecute
  );

  { TTyroMain }

  { TTyroEngine }

  TTyroEngine = class(TyroControls.TTyroMain)
  private
    //FBoard: TTyroImage;
    function GetActive: Boolean;
  protected
    FQueue: TQueueObjects;
    FScriptThread: TTyroScriptThread;
    FScriptMain: TTyroScript; //only if we have main loop
    FScriptTypes: TScriptTypes;
    FCanvasLock: TCriticalSection;
  protected
  public
    RunInMain: Boolean;
    Running: Boolean;
    How: TRunHow;
    RunFile: string;//that to run in script
    Console: TTyroConsole;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Terminate; override;
    procedure ProcessQueue;
    procedure Setup; override;
    procedure Draw; override;
    procedure Loop; override;
    //property Board: TTyroImage read FBoard;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtentions: TStringArray; AScriptClass: TTyroScriptClass);

    procedure ShowWindow(AWidth, AHeight: Integer);
    procedure HideWindow;

    procedure ShowConsole(AWidth, AHeight: Integer);
    procedure HideConsole;

    property Queue: TQueueObjects read FQueue;
    property ScriptTypes: TScriptTypes read FScriptTypes;

    property CanvasLock: TCriticalSection read FCanvasLock;
  end;

  function IntToFPColor(I: Integer): TFPColor;
  function FPColorToInt(C: TFPColor): Integer;
  function RayColorOf(Color: TFPColor): TRGBAColor;

var
  Main : TTyroEngine = nil;

implementation

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
  if Canvas <> nil then
  begin
    CanvasLock.Enter;
    try
      ft := GetTime();
      fpd := (1 / cFramePerSeconds);
      Canvas.BeginDraw;
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
      Canvas.EndDraw;
    finally
      CanvasLock.Leave;
    end;
  end;
end;

procedure TTyroEngine.Setup;
begin
  inherited;
  if (FScriptThread <> nil) and not FScriptThread.Started then
    FScriptThread.Start;

  //if (FScriptMain <> nil) and not FScriptMain.Started then
  if (FScriptMain <> nil) then
    FScriptMain.Start;
end;

constructor TTyroEngine.Create;
begin
  RayLibrary.Load;
  inherited Create;
  Margin := 10;
  FCanvasLock := TCriticalSection.Create;
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
  Console.CharHeight := 16;
  Console.CharWidth := 16;
  Console.Visible := False;
  Console.Focused := True;
end;

destructor TTyroEngine.Destroy;
begin
  //Stop;
  HideWindow;
  //FreeAndNil(FBoard);
  FreeAndNil(FQueue);
  FreeAndNil(FScriptTypes);
  FreeAndNil(FCanvasLock);
  inherited Destroy;
end;

procedure TTyroEngine.Start;
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
          RunFile := ExpandFileName(WorkSpace + RunFile);
        aScript.AssetsFolder := ExtractFilePath(RunFile);
        aScript.LoadFile(RunFile);
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
var
  t: TTexture2D;
  //im: TImage;
begin  //
  if Visible then
  begin
    if WindowShouldClose() then
    begin
      Running := False;
      if (FScriptThread <> nil) then
        FScriptThread.Terminate;
      if (FScriptMain <> nil) then
        FScriptMain.Stop;
    end;

    ProcessQueue;

    BeginDrawing;
    CanvasLock.Enter;
    try
      Paint;
      ThreadSwitch; //Yield
    finally
      CanvasLock.Leave;
    end;
    ThreadSwitch; //Yield
    EndDrawing;
    RayUpdates.Update;
  end;
end;

procedure TTyroEngine.Loop;
begin
  inherited;
  if not Active then
    Terminate;
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
  Stop;
  inherited;
end;

procedure TTyroEngine.ShowWindow(AWidth, AHeight: Integer);
begin
  if Visible then
  begin
    SetBounds(0, 0, AWidth, AHeight);
    SetWindowSize(AWidth, AHeight);
  end
  else
  begin
    //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
    SetBounds(0, 0, AWidth, AHeight);
    InitWindow(AWidth, AHeight, PChar(Title));
    SetTargetFPS(cFramePerSeconds);
    ShowCursor();
    NeedCanvas;
  end;
  //Console.BoundsRect := Rect(Margin, Margin , 50, 50);
  Console.BoundsRect := Rect(Margin, Margin , AWidth - Margin, AHeight - Margin);
  Visible := True;
end;

procedure TTyroEngine.HideWindow;
begin
  if Visible then
    CloseWindow;
end;

procedure TTyroEngine.ShowConsole(AWidth, AHeight: Integer);
begin
  Console.Show;
end;

procedure TTyroEngine.HideConsole;
begin
  Console.Hide;
end;

initialization
  Main := TTyroEngine.Create;
finalization
  FreeAndNil(Main);
end.


