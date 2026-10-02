unit T3D;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    belalhamed.
 *
 * //check: https://www.youtube.com/watch?v=HyK_Q5rrcr4
 *
 *}
interface

uses
  Classes, SysUtils,
  RayLib, RayClasses, Generics.Collections,
  TyroControls, TyroClasses, TyroEngines;

type
  TMain = class(TTyroMain)
  public
    Camera3D: TCamera3D;
    procedure Load; override;
    procedure Draw; override;
    procedure Unload; override;
  end;

implementation

{ TMain }

procedure TMain.Load;
begin
  inherited;
  ShowWindow();
  Options := Options + [moShowFPS];
  SetFPS(30);
  Camera3D.Position := Vector3Of(0.0, 10.0, 10.0);
  Camera3D.Target := Vector3Of(0.0, 0.0, 0.0 );
  Camera3D.Up :=  Vector3Of(0.0, 10.0, 0.0 );
  Camera3D.Fovy := 45.0;
  Camera3D.Projection := 0;
//  UpdateCamera(Camera3D, CAMERA_FREE);
end;

procedure TMain.Draw;
begin
  inherited;
  BeginMode3D(Camera3D);
  DrawCylinderWires(Vector3Of(0, 0, 0), 1, 1, 3, 32, clBlack);
  DrawGrid(10, 1.0);
  EndMode3D;
end;

procedure TMain.Unload;
begin
  inherited;
end;

end.
