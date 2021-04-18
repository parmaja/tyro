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
  RayLib3, RayClasses, TyroClasses, TyroControls, TyroConsoles;

type

  { TTyroWindow }

  TTyroWindow = class(TTyroContainer)
  private
    FCanvas: TTyroCanvas;
    FTitle: string;
    procedure SetTitle(AValue: string);
  protected
    DefaultBackColor: TColor;
    WindowVisible: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Paint;
    procedure ShowWindow(W, H: Integer);
    procedure HideWindow;
    property Canvas: TTyroCanvas read FCanvas;
    property Title: string read FTitle write SetTitle;
  end;

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
    procedure Loop;
    //property Board: TTyroImage read FBoard;
    property Active: Boolean read GetActive;

    procedure RegisterLanguage(ATitle: string; AExtention: string; AScriptClass: TTyroScriptClass);

    property Queue: TQueueObjects read FQueue;
    property ScriptTypes: TScriptTypes read FScriptTypes;

    property CanvasLock: TCriticalSection read FCanvasLock;
  end;

var
  Main : TTyroMain = nil;

implementation

{ TTyroWindow }

procedure TTyroWindow.SetTitle(AValue: string);
begin
  if FTitle =AValue then Exit;
  FTitle :=AValue;
end;

constructor TTyroWindow.Create;
begin
  inherited Create;
  DefaultBackColor := TColor.Create(220, 230, 240, 0);
end;

destructor TTyroWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TTyroWindow.Paint;
var
  aControl: TTyroControl;
begin
  BeginDrawing;
  try
    ClearBackground(DefaultBackColor);

    {if Board <> nil then
    begin
      t := Board.LoadTexture;
      DrawTextureRec(t, TRectangle.Create(0, 0, t.width, t.height), TVector2.Create(0, 0), WHITE);
      UnloadTexture(t);
    end;}

    with Canvas.Texture do
      DrawTextureRec(texture, TRectangle.Create(0, 0, texture.width, -texture.height), Vector2Of(0, 0), WHITE);

    for aControl in Controls do
    begin
      aControl.Paint(Canvas);
    end;

  finally
    EndDrawing;
  end;
end;

procedure TTyroWindow.ShowWindow(W, H: Integer);
begin
  //SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(W, H, PChar(Title));
  SetTargetFPS(cFramePerSeconds);
  ShowCursor();
  WindowVisible := True;
  FCanvas := TTyroCanvas.Create(W, H);
  //FBoard := TTyroImage.Create(W, H);
end;

procedure TTyroWindow.HideWindow;
begin
  if WindowVisible then
    CloseWindow;
end;

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
  RayLib.Load;
  inherited Create;
  FCanvasLock := TCriticalSection.Create;
  //SetTraceLog(LOG_DEBUG or LOG_INFO or LOG_WARNING);
  SetTraceLogLevel([LOG_ERROR, LOG_FATAL]);
  FQueue := TQueueObjects.Create(True);
  FScriptTypes := TScriptTypes.Create(true);
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  //Controls.Add(TTyroForm.Create(nil));

  Console := TTyroConsole.Create(nil);
  Console.BoundsRect := Rect(10, 10 , 10 + 20 * 8, 10 + 20 * 8);
  //Console.Visible := False;
  Console.Alpha := 128;
  Controls.Add(Console);
end;

destructor TTyroMain.Destroy;
begin
  //Stop;
  HideWindow;
  FreeAndNil(FCanvas);
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

  if WindowVisible then
  begin
    if WindowShouldClose() then
    begin
      Running := False;
      if (FScript <> nil) then
        FScript.Terminate;
    end;

    ProcessQueue;

    CanvasLock.Enter;
    try
      Paint;
      EndDrawing;
      ThreadSwitch; //Yield
    finally
      CanvasLock.Leave;
    end;
    BeginDrawing;
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

end.

