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
  SysUtils, Classes, sardScripts,
  mnFields, mnUtils, mnParams,
  raylib,
  TyroClasses, TyroLua;

type

  { TTyro }

  TTyro = class(TPersistent)
  private
    FArguments: TmnFields;
    FTitle: string;
    procedure CollectArguments;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Run;
    property Arguments: TmnFields read FArguments;
    property Title: string read FTitle write FTitle;
  end;

{ TTyro }

procedure TTyro.CollectArguments;
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

constructor TTyro.Create;
begin
  inherited Create;
  FArguments := TmnFields.Create;
  CollectArguments;
  Main := TTyroMain.Create;
end;

destructor TTyro.Destroy;
begin
  FreeAndNil(Main);
  FreeAndNil(FArguments);
  inherited Destroy;
end;

procedure TTyro.Run;
begin
  Main.Title := Title;
  if Arguments.Exists[''] then
    Main.FileName := Arguments[''];
  Main.WorkSpace := IncludePathSeparator(ExtractFilePath(ParamStr(0)));

  Main.Run;
end;

var
  Application: TTyro;

{$R *.res}

begin
  Application := TTyro.Create;
  Application.Title :='Tyro';
  Application.Run;
  Application.Free;
end.


