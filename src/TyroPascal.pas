unit TyroPascal;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils,
  RayLib, RayClasses, //remove it
  uPSCompiler, uPSComponent, uPSRuntime, uPSC_classes, uPSR_classes, uPSC_std, uPSR_std,
  TyroSounds, TyroClasses, Melodies, TyroEngines;

type
  TPasScript = class;

  { TPasObject }

  TPasObject = class abstract(TObject)
  private
    FScript: TPasScript;
  protected
    procedure Created; virtual;
  protected
  public
    constructor Create(AScript: TPasScript); virtual;
  end;

  { TPasCanvas }

  TPasCanvas = class(TPasObject)
  protected
  public
    constructor Create(AScript: TPasScript); override;
  end;

  { TPasConsole }

  TPasConsole = class(TPasObject)
  protected
  public
    constructor Create(AScript: TPasScript); override;
  end;

  { TPasColors }

  TPasColors = class(TPasObject)
  protected
    type
      TPasColor = record
        Name: string;
        Color: TColor;
      end;
    var
      Colors: array of TPasColor;
    procedure Created; override;
  public
    procedure AddColor(Name: string; AColor: TColor);
  end;

  { TPasScript }

  TPasScript = class(TTyroScript)
  private
  protected
    CompExec: TPSScript;

    FQueueObject: TQueueObject;
    PasCanvas: TPasCanvas;
    PasConsole: TPasConsole;
    PasColors: TPasColors;

    procedure Compile(script: string);
    procedure CompileRun(Script: string);

    procedure OnCompile(Sender: TPSScript); virtual;
    procedure OnExecute(Sender: TPSScript); virtual;
    procedure OnCompImport(Sender: TObject; x: TIFPSPascalCompiler); virtual;
    procedure OnExecImport(Sender: TObject; se: TIFPSExec; x: TIFPSRuntimeClassImporter); virtual;

    procedure ExecuteQueueObject;
    procedure DoError(S: string);
    procedure Run; override;
  protected
    procedure AddQueueObject(AQueueObject: TQueueObject); override;
    //canvas functions
    function Clear_func: Integer; cdecl;
    function Window_func(w, h: integer): Integer;
    function ShowConsole_func(w, h: integer): Integer;
    function DrawText_func(x, y: Integer; s: string): Integer;
    function DrawCircle_func(x, y, r: integer; f: Boolean): Integer;
    function DrawRectangle_func(x, y, w, h: integer; f: Boolean): Integer;
    function DrawLine_func(x1, y1, x2, y2: integer): Integer; overload;
    function DrawLine_func(x1, y1: integer): Integer; overload;
    function DrawPoint_func(x, y: integer): Integer;
    //global functions
    function Print_func(s: string): Integer;

    function Beep_func: Integer;
    function PlaySound_func(Freq, Period: integer): Integer;
    function PlayMusic_func(s: string): Integer; cdecl;
    function PlayMML_func(Songs: TmmlSong): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

function PasAlloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t) : Pointer; cdecl;
begin
  try
    Result:= ptr;
    ReallocMem(Result, nSize);
  except
    Result:= nil;
  end;
end;

//global functions
function sleep_func(n: Integer) : Integer; cdecl;
begin
  sleep(n);
  Result := 0;
end;

function log_func(s: string) : Integer; cdecl;
begin
  if IsConsole then
    WriteLn(s);
  Result := 0;
end;

{ TPasScript }

{ TPasConsole }

constructor TPasConsole.Create(AScript: TPasScript);
begin
  inherited Create(AScript);
end;

{ TPasObject }

procedure TPasObject.Created;
begin
end;

constructor TPasObject.Create(AScript: TPasScript);
begin
  inherited Create;
  FScript := AScript;
  Created;
end;

{ TPasColors }

procedure TPasColors.AddColor(Name: string; AColor: TColor);
var
  aItem: TPasColor;
begin
  aItem.Name := Name;
  aItem.Color := AColor;
  SetLength(Colors, Length(Colors) + 1);
  Colors[Length(Colors) -1] := aItem;
end;

procedure TPasColors.Created;
begin
  AddColor('white', clWhite);
  AddColor('silver', clLightgray);
  AddColor('gray' , clGray);
  AddColor('black', clBlack);
  AddColor('red'  , clRed);
  AddColor('maroon', clMaroon);
  AddColor('yellow', clYellow);
  AddColor('olive', clDarkgreen);
  AddColor('lime' , clLime);
  AddColor('green', clGreen);
  AddColor('aqua' , clSkyBlue);
  AddColor('teal' , clBrown);
  AddColor('blue' , clBlue);
  AddColor('navy' , clViolet);
  AddColor('fuchsia', clMagenta);
  AddColor('purple', clPurple);
end;

{ TPasCanvas }

constructor TPasCanvas.Create(AScript: TPasScript);
begin
  inherited;
end;

threadvar
  ThreadRunning: TTyroScript;

constructor TPasScript.Create;
begin
  inherited;
  CompExec := TPSScript.Create(nil);
  CompExec.OnCompile := {$IFDEF FPC}@{$ENDIF}OnCompile;
  CompExec.OnExecute := {$IFDEF FPC}@{$ENDIF}OnExecute;
  CompExec.OnCompImport := {$IFDEF FPC}@{$ENDIF}OnCompImport;
  CompExec.OnExecImport := {$IFDEF FPC}@{$ENDIF}OnExecImport;

{  Pas_register(PasState, 'log', @log_func);
  Pas_register(PasState, 'sleep', @sleep_func);
  Pas_register_method(PasState, 'print', @Print_func);
  Pas_register_method(PasState, 'window', @Window_func);
  Pas_register_method(PasState, 'showconsole', @ShowConsole_func);

//  Pas_register_integer(PasState, 'width', ScreenWidth));
//  Pas_register_integer(PasState, 'heigh', ScreenHeight));

  PasCanvas := TPasCanvas.Create(Self);
  PasConsole := TPasConsole.Create(Self);
  PasColors := TPasColors.Create(Self);

  Pas_register_table_method(PasState, 'console', self, 'print', @Print_func);
  Pas_register_table_method(PasState, 'console', self, 'show', @ShowConsole_func);
  Pas_register_table_index(PasState, 'console', PasConsole); //Should be last one

  //Pas_register_table(PasState, 'draw', PasCanvas);
  Pas_register_table_method(PasState, 'canvas', self, 'clear', @Clear_func);
  Pas_register_table_method(PasState, 'canvas', self, 'text', @DrawText_func);
  Pas_register_table_method(PasState, 'canvas', self, 'circle', @DrawCircle_func);
  Pas_register_table_method(PasState, 'canvas', self, 'rectangle', @DrawRectangle_func);
  Pas_register_table_method(PasState, 'canvas', self, 'line', @DrawLine_func);
  Pas_register_table_method(PasState, 'canvas', self, 'point', @DrawPoint_func);

  Pas_register_table_value(PasState, 'canvas', 'width', ScreenWidth);
  Pas_register_table_value(PasState, 'canvas', 'height', ScreenHeight);

  Pas_register_table_index(PasState, 'canvas', PasCanvas); //Should be last one

  Pas_register_table_method(PasState, 'music', self, 'beep', @Beep_func);
  Pas_register_table_method(PasState, 'music', self, 'sound', @PlaySound_func);
  Pas_register_table_method(PasState, 'music', self, 'play', @PlayMusic_func);
  Pas_register_table_method(PasState, 'music', self, 'mml', @PlayMML_func);

  Pas_newtable(PasState);
  for i := 0 to Length(PasColors.Colors) -1 do
    Pas_register_color(PasState, PasColors.Colors[i].Name, PasColors.Colors[i].Color);
  Pas_setglobal(PasState, 'colors');
  Pas_register_table_index(PasState, 'colors', PasColors); //Should be last one
  //Pas_register_table(PasState, 'color', PasCanvas);}
end;

destructor TPasScript.Destroy;
begin
  CompExec.Free;
  //Compiler.Free;
  //Exec.Free;

  inherited;
end;

function MyFormat(const Format: string;
  const Args: array of const): string;
begin
  Result := SysUtils.Format(Format, Args);
end;

procedure TPasScript.Compile(script: string);
var
    OutputMessages: string;
    ok: Boolean;
    i: Longint;
begin

    CompExec.Script.Clear;
    CompExec.Script.Add(Script);

    OutputMessages := '';
    ok := CompExec.Compile;
    if (NOT ok) then
    begin
        //Get Compiler Messages now.
        for i := 0 to CompExec.CompilerMessageCount - 1 do
          OutputMessages := OutputMessages + CompExec.CompilerErrorToStr(i);
    end;
    //Check(ok, 'Compiling failed:' + Script + #13#10 + OutputMessages);
end;

procedure TPasScript.CompileRun(Script: string);
var
    ok: boolean;
begin
    Compile(script);

    ok := CompExec.Execute;

    {Check(ok, 'Exec Error:' + Script + #13#10 +
            CompExec.ExecErrorToString + ' at ' +
            Inttostr(CompExec.ExecErrorProcNo) + '.' +
            Inttostr(CompExec.ExecErrorByteCodePosition));}
end;

procedure TPasScript.OnCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@MyFormat, 'function Format(const Format: string; const Args: array of const): string;');
end;

procedure TPasScript.OnExecute(Sender: TPSScript);
begin
  //Sender.SetVarToInstance('SELF', Self);
end;

procedure TPasScript.OnCompImport(Sender: TObject; x: TIFPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);
end;

procedure TPasScript.OnExecImport(Sender: TObject; se: TIFPSExec; x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
end;

procedure TPasScript.ExecuteQueueObject;
begin
  FQueueObject.Execute;
  FreeAndNil(FQueueObject);
end;

procedure TPasScript.DoError(S: string);
begin
  if IsConsole then
    WriteLn(S);
end;

procedure TPasScript.Run;
var
  Msg: string;
begin
  ThreadRunning := Self;
  //WriteLn('Run Script');
  //Sleep(1000);
  CompileRun(ScriptText.Text);
end;

procedure TPasScript.AddQueueObject(AQueueObject: TQueueObject);
begin
//  AQueueObject.LineNo := ar.currentline;
  inherited AddQueueObject(AQueueObject);
end;

function TPasScript.Clear_func: Integer; cdecl;
begin
  AddQueueObject(TClearObject.Create(Main.Canvas));
  Result := 0;
end;

function TPasScript.Window_func(w, h: integer): Integer;
begin
  FQueueObject := TWindowObject.Create(w, h);
  Synchronize(@ExecuteQueueObject);
  Result := 0;
end;

function TPasScript.ShowConsole_func(w, h: integer): Integer;
begin
  FQueueObject := TShowConsoleObject.Create(w, h);
  Synchronize(@ExecuteQueueObject);
  Result := 0;
end;

function TPasScript.DrawText_func(x, y: Integer; s: string): Integer;
begin
  AddQueueObject(TDrawTextObject.Create(Main.Canvas, x, y, s));
  Result := 0;
end;

function TPasScript.DrawCircle_func(x, y, r: integer; f: Boolean): Integer;
begin
  AddQueueObject(TDrawCircleObject.Create(Main.Canvas, x, y, r, f));
  {Main.CanvasLock.Enter;
  try
    Main.Board.Circle(x, y, r, f);
  finally
    Main.CanvasLock.Leave;
  end;}
  Result := 0;
end;

function TPasScript.DrawRectangle_func(x, y, w, h: integer; f: Boolean): Integer;
begin
  AddQueueObject(TDrawRectangleObject.Create(Main.Canvas, x, y, w, h, f));
  Result := 0;
end;

function TPasScript.DrawLine_func(x1, y1, x2, y2: integer): Integer;
begin
  AddQueueObject(TDrawLineObject.Create(Main.Canvas, x1, y1, x2, y2));
  Result := 0;
end;

function TPasScript.DrawLine_func(x1, y1: integer): Integer;
begin
  AddQueueObject(TDrawLineToObject.Create(Main.Canvas, x1, y1));
  Result := 0;
end;

function TPasScript.DrawPoint_func(x, y: integer): Integer;
begin
  AddQueueObject(TDrawPointObject.Create(Main.Canvas, x, y));
  Result := 0;
end;

function TPasScript.Print_func(s: string): Integer;
begin
  AddQueueObject(TPrintObject.Create(Main.Canvas, s));
  Result := 0;
end;

function TPasScript.Beep_func: Integer;
begin
  AddQueueObject(TBeepObject.Create);
  Result := 0;
end;

function TPasScript.PlaySound_func(Freq, Period: integer): Integer;
begin
  AddQueueObject(TPlaySoundObject.Create(Freq, Period));
  Result := 0;
end;

function TPasScript.PlayMusic_func(s: string): Integer; cdecl;
begin
  if ExtractFileDir(s) = '' then
    s := AssetsFolder + s;
  AddQueueObject(TPlayMusicFileObject.Create(s));
  Result := 0;
end;

function TPasScript.PlayMML_func(Songs: TmmlSong): Integer;
begin
  //AddQueueObject(TPlayMMLObject.Create(Song));
  with TPlayMMLObject.Create(Songs) do //using current Pas thread to not block current thread, or maybe use a thread
  begin
    Execute;
    Free;
  end;
  Result := 0;
end;

initialization
  ThreadRunning := nil;
  Main.RegisterLanguage('Pascal', '.ps', TPasScript);
end.
