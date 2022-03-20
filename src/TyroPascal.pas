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

{$mode objfpc}
{$H+}{$M+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Rtti,
  mnRTTIUtils, mnUtils,
  RayLib, RayClasses, //remove it
  uPSCompiler, uPSComponent, uPSRuntime,
  uPSC_classes, uPSR_classes, uPSC_std, uPSR_std,
  TyroScripts, TyroSounds, TyroClasses, Melodies, TyroEngines;

type
  TPasScript = class;

  { TPasRttiObject }

  TPasRttiObject = class(TRttiObject)
  public
    procedure RegisterMethod(Sender: TObject; Flag: Integer; AName: string; AParams: TStringList); override;
    procedure RegisterProperty(Sender: TObject; Flag: Integer; AName: string; AType: string); override;
  end;

  { TPasObject }

  TPasObject = class abstract(TInterfacedObject)
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

  TPasConsole = class(TPasObject, IConsole)
  private
    FVisible: Boolean;
  protected
  public
    procedure Print(s: string);
    procedure Show;
    constructor Create(AScript: TPasScript); override;
  published
    property Visible: Boolean read FVisible write FVisible;
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
    PS: TPSScript;

    FQueueObject: TQueueObject;
    Canvas: TPasCanvas;
    Console: TPasConsole;
    Colors: TPasColors;

    function Compile(script: string): Boolean;
    function CompileRun(Script: string): Boolean;

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
    function Clear_func: Integer;
    procedure Window_func(w, h: integer);
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
    function PlayMusic_func(s: string): Integer;
    function PlayMML_func(Songs: TmmlSong): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

function PasAlloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t) : Pointer;
begin
  try
    Result:= ptr;
    ReallocMem(Result, nSize);
  except
    Result:= nil;
  end;
end;

//global functions
function sleep_func(n: Integer) : Integer;
begin
  sleep(n);
  Result := 0;
end;

function log_func(s: string) : Integer;
begin
  if IsConsole then
    WriteLn(s);
  Result := 0;
end;

{ TRttiObject }

procedure TPasRttiObject.RegisterMethod(Sender: TObject; Flag: Integer; AName: string; AParams: TStringList);
var
  s: string;
  aMethodName, aMethodReturn: string;
  c: Integer;
  aParam,
  aParamName, aType, aParamType, aParamDefault: string;
begin
  SpliteStr(AName, ':', aMethodName, aMethodReturn);
  if aMethodReturn <> '' then
    s := 'function '
  else
    s := 'procedure ';
  s := s + aMethodName;
  c := 0;
  if AParams.Count > 0 then
  begin
    s := s + '(';
    for aParam in AParams do
    begin
      if c > 0 then
        s := s + ', ';
      SpliteStr(aParam, ':', aParamName, aType);
      SpliteStr(aType, '=', aParamType, aParamDefault);
      s := s + aParamName + ': ' + aParamType + ConcatString(aParamDefault, '=');
      Inc(c);
    end;
    s := s + ')';
  end;

  if aMethodReturn <> '' then
     s := s + ': ' + aMethodReturn;
  s := s + ';';

  (Sender as TPSCompileTimeClass).RegisterMethod(s);
  //RegisterMethod('procedure Print(s: string)');
  //RegisterMethod('procedure Show');
end;

procedure TPasRttiObject.RegisterProperty(Sender: TObject; Flag: Integer; AName: string; AType: string);
begin
  //
end;

{ TPasConsole }

procedure TPasConsole.Print(s: string);
begin
  FScript.Print_func(s);
end;

procedure TPasConsole.Show;
begin
  FScript.ShowConsole_func(0, 0);
end;

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
  PS := TPSScript.Create(nil);
  PS.CompilerOptions := [icAllowNoBegin, icAllowUnit, icAllowNoEnd, icBooleanShortCircuit];
  PS.OnCompile := @OnCompile;
  PS.OnExecute := @OnExecute;
  PS.OnCompImport := @OnCompImport;
  PS.OnExecImport := @OnExecImport;

  Canvas := TPasCanvas.Create(Self);
  Console := TPasConsole.Create(Self);
  Colors := TPasColors.Create(Self);
end;

destructor TPasScript.Destroy;
begin
  PS.Free;
  //Compiler.Free;
  //Exec.Free;

  Canvas.Free;
  Console.Free;
  Colors.Free;

  inherited;
end;

function MyFormat(const Format: string;
  const Args: array of const): string;
begin
  Result := SysUtils.Format(Format, Args);
end;

function TPasScript.Compile(script: string): Boolean;
var
  i: Longint;
begin

  PS.Script.Clear;
  PS.Script.Add(Script);

  Result := PS.Compile;
  if (not Result) then
  begin
    for i := 0 to PS.CompilerMessageCount - 1 do
      WriteLn(PS.CompilerErrorToStr(i));
  end;
end;

function TPasScript.CompileRun(Script: string): Boolean;
begin
  Result := Compile(script);
  if Result then
  begin
    Result := PS.Execute;
    if not Result then
    begin
      WriteLn('Exec Error:'+
            PS.ExecErrorToString + ' at ' +
            Inttostr(PS.ExecErrorProcNo) + '.' +
            IntToStr(PS.ExecErrorByteCodePosition));
    end;
  end;
end;

procedure TPasScript.OnCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@MyFormat, 'function Format(const Format: string; const Args: array of const): string;');

  Sender.AddFunction(@Log_func, 'procedure log(s: string);');
  Sender.AddMethod(Self, @TPasScript.Window_func, 'procedure Window(w, h: integer);');
  Sender.AddMethod(Self, @TPasScript.Print_func, 'procedure Print(s: string);');

  Sender.AddRegisteredVariable('Version', 'String');
  Sender.AddRegisteredVariable('Console', 'TConsole');
end;

procedure TPasScript.OnExecute(Sender: TPSScript);
var
  v: PIFVariant;
begin
  v := Sender.GetVariable('Version');
  if v <> nil then
    VSetInt(v, TyroVersion);

  Sender.SetVarToInstance('Console', Console);
end;

procedure TPasScript.OnCompImport(Sender: TObject; x: TIFPSPascalCompiler);
var
  r: TPasRttiObject;
  psc: TPSCompileTimeClass;
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);

  psc := x.AddClassN(x.FindClass('TObject'), 'TConsole');
  r := TPasRttiObject.Create;
  r.EnumProperties(Console, psc,  0);
  r.EnumMethods(TypeInfo(IConsole), psc,  0);
  r.Free;

{  with x.AddClassN(x.FindClass('TObject'), 'TConsole') do
  begin
    RegisterMethod('procedure Print(s: string)');
    RegisterMethod('procedure Show');
  end;}
end;

procedure TPasScript.OnExecImport(Sender: TObject; se: TIFPSExec; x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);

  with x.Add2(TPasConsole, 'TConsole') do
  begin
    RegisterMethod(@TPasConsole.Print, 'Print');
    RegisterMethod(@TPasConsole.Show, 'Show');
  end;
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

function TPasScript.Clear_func: Integer;
begin
  AddQueueObject(TClearObject.Create(Main.Canvas));
  Result := 0;
end;

procedure TPasScript.Window_func(w, h: integer);
begin
  RunQueueObject(TWindowObject.Create(w, h));
end;

function TPasScript.ShowConsole_func(w, h: integer): Integer;
begin
  RunQueueObject(TShowConsoleObject.Create(w, h));
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

function TPasScript.PlayMusic_func(s: string): Integer;
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
  Main.RegisterLanguage('Pascal', ['.ps'], TPasScript);
end.
