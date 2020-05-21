program tyro;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *
 *  Fonts
 *   https://opengameart.org/content/the-collection-of-8-bit-fonts-for-grafx2
     http://www.pentacom.jp/pentacom/bitfontmaker2/gallery/?page=1&order=&
     http://orangetide.com/OLD/fonts/DOS/
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
  TyroClasses, raylib3,
  TyroLua, TyroEditors;  //Add all languages units here


type

  { TTyroApplication }

  TTyroApplication = class(TCustomApplication)
  private
    Files: TStringList;
  protected
    procedure SetTitle(const AValue: string); override;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TTyroApplication }

procedure TTyroApplication.SetTitle(const AValue: string);
begin
  inherited SetTitle(AValue);
  Main.Title := AValue;
end;

//-w my_workpath ../demos/sin.lua

constructor TTyroApplication.Create(AOwner: TComponent);
var
  WorkPaths: TStringArray;
  err: string;
const
  cShortOptions = 'w:d';
  cLongOptions = 'workpath: debug';
begin
  inherited Create(AOwner);
  Files := TStringList.Create;

  err := CheckOptions(cShortOptions, cLongOptions);
  if err <> '' then
  begin
    if IsConsole then
      WriteLn(err);
    Terminate;
    exit;
  end;

  Main.Title := 'Tyro';

  //w workpath, d socket
  GetNonOptions(cShortOptions, ['workpath:', 'debug'], Files);
  if Files.Count > 0 then
    Main.FileName := Files[0];
  WorkPaths := GetOptionValues('w', 'workpath');
  if Length(WorkPaths) > 0  then
    Main.WorkSpace := WorkPaths[0]
  else
    Main.WorkSpace := Location;
  Main.Start;
end;

destructor TTyroApplication.Destroy;
begin
  FreeAndNil(Files);
  inherited;
end;

procedure TTyroApplication.DoRun;
begin
  inherited;
  CheckSynchronize;
  Main.Run;
  if not Main.Active then
  begin
    Main.Stop;
    Terminate;
  end;
end;

var
  Application: TTyroApplication;

{$R *.res}

begin
  Application := TTyroApplication.Create(nil);
  Application.Title :='Tyro';
  Application.Run;
  Application.Free;
end.
