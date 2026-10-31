program test_appicon;

{ Headless test for the application's embedded icon (tyro.ico linked by the
  resource file of tyro.lpr) being used by the engine instead of a loose
  tyro.png:

    - LoadApplicationIconImage must return a 512x512 PNG image decoded from
      the executable icon resources (the biggest PNG entry in tyro.ico)
    - format must be UNCOMPRESSED_R8G8B8A8
    - exercise the same code path the engine uses for SetWindowIcon

  Exit status: 0 on success, otherwise 1. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

{$R ..\src\tyro.res}

uses
  SysUtils,
  RayLib,
  TyroEngines;

var
  Img: TImage;
  Failures: Integer;

procedure Check(const AName: string; AOk: Boolean);
begin
  if AOk then
    WriteLn('OK: ', AName)
  else
  begin
    WriteLn('FAIL: ', AName);
    Inc(Failures);
  end;
end;

begin
  Failures := 0;
  InitLibrary;

  Img := LoadApplicationIconImage;
  Check('icon loaded', Img.Data <> nil);
  if Img.Data <> nil then
  begin
    WriteLn(Format('  size: %dx%d format: %d', [Img.Width, Img.Height, Img.Format]));
    Check('width is 512', Img.Width = 512);
    Check('height is 512', Img.Height = 512);
    Check('format is RGBA8', Img.Format = Ord(PIXELFORMAT_UNCOMPRESSED_R8G8B8A8));
    RayLib.UnloadImage(Img);
  end;

  if Failures = 0 then
  begin
    WriteLn('All tests passed');
    ExitCode := 0;
  end
  else
  begin
    WriteLn(Format('%d test(s) failed', [Failures]));
    ExitCode := 1;
  end;
end.
