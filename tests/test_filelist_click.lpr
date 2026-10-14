program test_filelist_click;

{ Headless test for the F4 file picker's mouse behaviour (TTyroFileList in
  src\TyroEngines.pas):

    first click on a row   selects it, nothing else
    click on the row that is already selected   picks it

  This is the rule the picker has always documented; the bug it guards against
  is checking the selection after the inherited handler has already moved it to
  the row under the mouse, which makes every click, including the first one on
  a fresh row, look like a second click and pick straight away.

  Keyboard picking (Enter) and cancelling (Escape) are not touched here.

  No window is opened: the list reads its font metrics through the global
  resources, the same global the engine fills in.

  Build and run:
    lazbuild --build-mode=Debug tests\test_filelist_click.lpi
    bin\test_filelist_click.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  Classes, SysUtils,
  RayLib,
  TyroClasses, TyroControls, TyroEngines;

type
  { Records what the picker picks, so a check can look at both the call and its
    file name. }

  TPickProbe = class
  public
    Count: Integer;
    LastFile: string;
    procedure Picked(Sender: TObject; const AFileName: string);
  end;

procedure TPickProbe.Picked(Sender: TObject; const AFileName: string);
begin
  Inc(Count);
  LastFile := AFileName;
end;

var
  Tests, Failures: Integer;

procedure Check(const AName: string; AOk: Boolean; const ADetail: string = '');
begin
  Inc(Tests);
  if AOk then
    WriteLn('  PASS  ', AName)
  else
  begin
    Inc(Failures);
    WriteLn('  FAIL  ', AName, '  <', ADetail, '>');
  end;
end;

var
  aList: TTyroFileList;
  aProbe: TPickProbe;
  aDir: string;

{ The pixels between the window edge and the client area: margin + border +
  padding. OuterSize is protected and is exactly this sum (BorderSize is the
  theme's thin border for the default brdThin). }
function ListOffset: Integer;
begin
  Result := aList.Margin + Res.Theme.Border.Thin + aList.Padding;
end;

//a click in the middle of row ARow, in the control's own coordinates
procedure ClickRow(ARow: Integer);
begin
  aList.MouseDown(mbLeft, [], ListOffset + 3,
    ListOffset + ARow * aList.RowHeight + aList.RowHeight div 2);
end;

//a click below the last item, where there is no row to hit
procedure ClickBelowItems;
begin
  aList.MouseDown(mbLeft, [], ListOffset + 3,
    ListOffset + aList.Items.Count * aList.RowHeight + aList.RowHeight);
end;

var
  c: Char;
  i, n: Integer;
begin
  Tests := 0;
  Failures := 0;

  RayLibrary.Load;
  Res := TTyroResources.Create;
  aProbe := TPickProbe.Create;
  aList := TTyroFileList.Create(nil);
  try
    WriteLn('File picker click behaviour');
    WriteLn;

    //a directory with three loadable files, so there are rows to click
    aDir := IncludeTrailingPathDelimiter(GetTempDir) + 'tyro_filelist_click';
    ForceDirectories(aDir);
    for c := 'a' to 'c' do
      with TStringList.Create do
      try
        Text := '-- ' + c;
        SaveToFile(aDir + PathDelim + c + '.tyro');
      finally
        Free;
      end;

    aList.BoundsRect := Rect(0, 0, 300, 400);   //room for all rows, no scrollbar
    aList.OnPick := aProbe.Picked;
    n := aList.Refresh(aDir, ['*.tyro']);
    Check('the picker lists the three files', n = 3, IntToStr(n));

    //1. a click on a row only selects it
    ClickRow(1);
    Check('the first click selects the row', aList.ItemIndex = 1, IntToStr(aList.ItemIndex));
    Check('and does not pick it', aProbe.Count = 0, IntToStr(aProbe.Count));

    //2. a click on a different row only moves the selection
    ClickRow(2);
    Check('a click on another row moves the selection', aList.ItemIndex = 2, IntToStr(aList.ItemIndex));
    Check('and still does not pick', aProbe.Count = 0, IntToStr(aProbe.Count));

    //3. a second click on that same row picks it
    ClickRow(2);
    Check('clicking the selected row again picks it', aProbe.Count = 1, IntToStr(aProbe.Count));
    Check('and picks the file of that row',
      aProbe.LastFile = aDir + PathDelim + 'c.tyro', aProbe.LastFile);

    //4. the first click on a fresh row selects it even when a pick just happened
    ClickRow(0);
    Check('after a pick the next click on a fresh row only selects',
      aList.ItemIndex = 0, IntToStr(aList.ItemIndex));
    Check('and picks nothing', aProbe.Count = 1, IntToStr(aProbe.Count));

    //5. clicking the now selected first row picks it
    ClickRow(0);
    Check('the selected row picks on the next click', aProbe.Count = 2, IntToStr(aProbe.Count));
    Check('with the file of that row',
      aProbe.LastFile = aDir + PathDelim + 'a.tyro', aProbe.LastFile);

    //6. a click where there is no row does neither
    i := aProbe.Count;
    ClickBelowItems;
    Check('a click below the rows keeps the selection', aList.ItemIndex = 0, IntToStr(aList.ItemIndex));
    Check('and picks nothing', aProbe.Count = i, IntToStr(aProbe.Count));

    //7. a control with no rows at all cannot pick
    aList.Refresh(aDir, ['*.nothing']);
    Check('an empty picker has no selection', aList.ItemIndex = -1, IntToStr(aList.ItemIndex));
    ClickBelowItems;
    Check('and a click on it picks nothing', aProbe.Count = i, IntToStr(aProbe.Count));
  finally
    aList.Free;
    aProbe.Free;
    FreeAndNil(Res);
    if aDir <> '' then
      for c := 'a' to 'c' do
        SysUtils.DeleteFile(aDir + PathDelim + c + '.tyro');
    RemoveDir(aDir);
  end;

  WriteLn;
  if Failures = 0 then
    WriteLn('OK: ', Tests, ' checks passed')
  else
    WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks');
  Halt(Failures);
end.
