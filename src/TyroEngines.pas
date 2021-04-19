unit TyroEngines;
{$mode ObjFPC}{$H+}
 {**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

interface

uses
  Classes, SysUtils, SyncObjs, fgl,
  RayLib, RayClasses, TyroClasses, TyroControls, TyroConsoles;

type

  { TTyroMain }

  TTyroMain = class(TTyroWindow)
  private
    //FBoard: TTyroImage;
    function GetActive: Boolean;
  protected
    FQueue: TQueueObjects;
    FScript: TTyroScript;
    FScriptTypes: TScriptTypes;
    FCanvasLock: TCriticalSection;
  protected
  public
    Running: Boolean;
    FileName: string;//that to run in script
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

    procedure RegisterLanguage(ATitle: string; AExtention: string; AScriptClass: TTyroScriptClass);

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
  Result := Running or ((FScript <> nil) and FScript.Active);
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
  Console.Alpha := 128;
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
  ScriptType: TScriptType;
begin
  Running := True;
//ShowWindow(ScreenWidth, ScreenHeight); with option to show window /w
  if FileName <> '' then
  begin
    ScriptType := ScriptTypes.FindByExtension(ExtractFileExt(FileName));
    if ScriptType <> nil then
    begin
      FScript := ScriptType.ScriptClass.Create;
      if SysUtils.FileExists(FileName) then
      begin
        if LeftStr(FileName, 1) = '.' then
          FileName := ExpandFileName(WorkSpace + FileName);
        FScript.AssetsFolder := ExtractFilePath(FileName);
        FScript.LoadFile(FileName);
      end;
    end
    else
      Log('Type of file not found: ' + FileName);
  end;
end;

procedure TTyroMain.Run;
var
  t: TTexture2D;
  //im: TImage;
begin
  if (FScript <> nil) and FScript.Suspended then
    FScript.Start;

  if Visible then
  begin
    if WindowShouldClose() then
    begin
      Running := False;
      if (FScript <> nil) then
        FScript.Terminate;
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

procedure TTyroMain.RegisterLanguage(ATitle: string; AExtention: string; AScriptClass: TTyroScriptClass);
var
  Item: TScriptType;
begin
  Item := TScriptType.Create;
  Item.Title := ATitle;
  Item.Extention := AExtention;
  Item.ScriptClass := AScriptClass;
  FScriptTypes.Add(Item);
end;

procedure TTyroMain.Stop;
begin
  Running := False;
  if FScript <> nil then
  begin
    FScript.Terminate;
    FScript.WaitFor;
    FreeAndNil(FScript);
  end;
end;

procedure TTyroMain.ShowWindow(AWidth, AHeight: Integer);
begin
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  SetBounds(0, 0, AWidth, AHeight);
  InitWindow(AWidth, AHeight, PChar(Title));
  Visible := True;
  SetTargetFPS(cFramePerSeconds);
  ShowCursor();
  NeedCanvas;
  //Console.BoundsRect := Rect(Margin, Margin , 50, 50);
  Console.BoundsRect := Rect(Margin, Margin , AWidth - Margin, AHeight - Margin);
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

