program tyro;

{$mode objfpc}{$H+}

uses
  cmem, math,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  lua53, l4l_object, sardScripts,
  raylib,
  mnUtils, mnFields, mnParams;

const
  cScreenWidth = 640;
  cScreenHeight = 480;

type


  TLuaRunObject = class(TLuaObject)
  private
  protected
  public
    print()
  published
  end;

  { TTyro }

  TTyro = class(TCustomApplication)
  protected
    Camera: TCamera2D;
    procedure DoRun; override;
  public
    //Arguments: TmnFields;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loop;
  end;

procedure ArgumentsCallbackProc(Sender: Pointer; Index:Integer; Name, Value: string; var Resume: Boolean);
begin
  (TObject(Sender) as TmnFields).Add(Name, Value);
end;

{ TTyro }

procedure TTyro.DoRun;
begin
  BeginDrawing();
  ClearBackground(RAYWHITE);
  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    BeginDrawing();
    ClearBackground(RAYWHITE);
    Loop;
    EndDrawing();
  end;
  Terminate;
end;

constructor TTyro.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //ParseArgumentsCallback(, @ArgumentsCallbackProc, Arguments);
  StopOnException :=True;
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  InitWindow(cScreenWidth, cScreenHeight, 'Tyro');
end;

destructor TTyro.Destroy;
begin
  CloseWindow;
  inherited Destroy;
end;

procedure TTyro.Loop;
begin
  //DrawText('', 190, 200, 20, BLACK);
end;

var
  Application: TTyro;
begin
  Application :=TTyro.Create(nil);
  Application.Title :='Tyro';
  Application.Run;
  Application.Free;
end.

