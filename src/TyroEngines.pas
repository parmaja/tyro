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
  Classes, SysUtils, SyncObjs, fgl,
  mnLogs,
  RayLib, RayClasses, TyroClasses, TyroControls, TyroConsoles;

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

  TTyroMain = class(TTyroWindow)
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
    WorkSpace: string;
    Console: TTyroConsole;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure ProcessQueue;
    procedure Run;
    procedure Loop; virtual;
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

var
  Main : TTyroMain = nil;

implementation


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

constructor TTyroMain.Create;
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

destructor TTyroMain.Destroy;
begin
  //Stop;
  HideWindow;
  //FreeAndNil(FBoard);
  FreeAndNil(FQueue);
  FreeAndNil(FScriptTypes);
  FreeAndNil(FCanvasLock);
  inherited Destroy;
end;

procedure TTyroMain.Start;
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

procedure TTyroMain.Run;
var
  t: TTexture2D;
  //im: TImage;
begin
  if (FScriptThread <> nil) and not FScriptThread.Started then
    FScriptThread.Start;

  //if (FScriptMain <> nil) and not FScriptMain.Started then
  if (FScriptMain <> nil) then
    FScriptMain.Start;

  //
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
    Loop;
    ThreadSwitch; //Yield
    EndDrawing;
    RayUpdates.Update;
  end;
end;

procedure TTyroMain.Loop;
begin
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

procedure TTyroMain.ShowWindow(AWidth, AHeight: Integer);
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

procedure TTyroMain.HideWindow;
begin
  if Visible then
    CloseWindow;
end;

procedure TTyroMain.ShowConsole(AWidth, AHeight: Integer);
begin
  Console.Show;
end;

procedure TTyroMain.HideConsole;
begin
  Console.Hide;
end;

end.

