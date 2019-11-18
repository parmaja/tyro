program tyro;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  lua53, l4l_object, sardScripts,
  mnUtils, mnFields, mnParams;

type

  { TMyApplication }

  TTyro = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    Arguments: TmnFields;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

procedure ArgumentsCallbackProc(Sender: Pointer; Index:Integer; Name, Value: string; var Resume: Boolean);
begin
  (TObject(Sender) as TmnFields).Add(Name, Value);
end;

{ TTyro }

procedure TTyro.DoRun;
begin
  Terminate;
end;

constructor TTyro.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ParseArgumentsCallback(GetCommandLine, @ArgumentsCallbackProc, Arguments);
  StopOnException :=True;
end;

destructor TTyro.Destroy;
begin
  inherited Destroy;
end;

procedure TTyro.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TTyro;
begin
  Application :=TTyro.Create(nil);
  Application.Title :='Tyro';
  Application.Run;
  Application.Free;
end.

