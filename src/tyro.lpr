program tyro;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

{$mode objfpc}
{$H+}

//ref https://github.com/raysan5/raylib/blob/master/examples/textures/textures_mouse_painting.c

uses
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, CustApp,
  mnFields, mnUtils, mnParams, //minilib on sf
  raylib,
  TyroClasses, TyroLua;

type

  { TTyroApplication }

  TTyroApplication = class(TCustomApplication)
  private
    FArguments: TmnFields;
    procedure CollectArguments;
  protected
    procedure SetTitle(const AValue: string); override;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Arguments: TmnFields read FArguments;
  end;

{ TTyroApplication }

procedure TTyroApplication.CollectArguments;
var
  i, P: Integer;
  aParam, aName, aValue: string;
begin
  for i := 1 to ParamCount do
  begin
    aParam := ParamStr(i);
    if CharInArray(LeftStr(aParam, 1), ['-', '/']) then
    begin
      aName := Copy(aParam, 1, Length(aParam) -1);
      P := Pos(aName, '=');
      if p > 0 then
      begin
        aValue := Copy(aName, P + 1, MaxInt);
        aName := Copy(aName, 1, P - 1);
      end
      else
        aValue := '';
      Arguments.Add(aName, DequoteStr(aValue));
    end
    else
      Arguments.Add('', aParam);
  end;
end;

procedure TTyroApplication.SetTitle(const AValue: string);
begin
  inherited SetTitle(AValue);
  Main.Title := AValue;
end;

constructor TTyroApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArguments := TmnFields.Create;
  CollectArguments;
  Main := TTyroMain.Create;
  Main.Init;
  Main.Title := 'Tyro';
  if Arguments.Exists[''] then
    Main.FileName := Arguments[''];
  Main.WorkSpace := IncludePathSeparator(ExtractFilePath(ParamStr(0)));
  Main.Start;
end;

destructor TTyroApplication.Destroy;
begin
  FreeAndNil(Main);
  FreeAndNil(FArguments);
  inherited;
end;

procedure TTyroApplication.DoRun;
begin
  inherited;
  CheckSynchronize(10);
  Main.Run;
  if not Main.Active then
  begin
    Main.Stop;
    ReadLn;
    Terminate;
  end;
end;

var
  Application: TTyroApplication;

{$R *.res}

begin
  Application := TTyroApplication.Create(nil);
  Application.Title := 'Tyro';
  Application.Run;
  Application.Free;
end.
