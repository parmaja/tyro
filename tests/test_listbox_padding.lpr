program test_listbox_padding;

{ Headless test for TTyroListBox.ItemPadding (src\tyrolib\TyroControls.pas).

  ItemPadding is the empty space above and below an item's text, so it is worth
  twice its value to the row: RowHeight = Font.Height + ExtraHeight +
  2 * ItemPadding. The default is 1, which is the two pixels the box used to
  lose against its text; setting it to 0 gives back the bare font height, and a
  negative value has no meaning and folds to 0.

  No window is opened: the controls read their font metrics through the global
  resources, the same global the engine fills in.

  Build and run:
    lazbuild --build-mode=Debug tests\test_listbox_padding.lpi
    bin\test_listbox_padding.exe

  Exit status: 0 when every check passed, otherwise the number of failures. }

{$ifopt D+}
{$apptype console}
{$endif}

{$mode delphi}
{$H+}

uses
  SysUtils,
  RayLib,
  TyroClasses, TyroControls;

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
  aList: TTyroListBox;
  fontHeight: Integer;

begin
  Tests := 0;
  Failures := 0;

  RayLibrary.Load;
  Res := TTyroResources.Create;
  aList := TTyroListBox.Create(nil);
  try
    fontHeight := Res.Font.Height;
    WriteLn('List box item padding');
    WriteLn;

    //1. the default is the one padding the constructor asks for, and the row
    //carries one above and one below
    Check('the default padding is 1', aList.ItemPadding = 1, IntToStr(aList.ItemPadding));
    Check('the row is the font plus a padding above and below it',
      aList.RowHeight = fontHeight + 2,
      IntToStr(aList.RowHeight) + ' vs ' + IntToStr(fontHeight + 2));

    //2. growing the padding grows the row by twice the change
    aList.ItemPadding := 5;
    Check('a bigger padding adds twice itself to the row',
      aList.RowHeight = fontHeight + 10,
      IntToStr(aList.RowHeight) + ' vs ' + IntToStr(fontHeight + 10));

    //3. zero is a real setting: the row falls back to the bare font height
    aList.ItemPadding := 0;
    Check('no padding leaves the bare font height',
      aList.RowHeight = fontHeight, IntToStr(aList.RowHeight) + ' vs ' + IntToStr(fontHeight));

    //4. a negative padding is meaningless and folds to 0 rather than shrinking
    //the row below the text
    aList.ItemPadding := -3;
    Check('a negative padding folds to 0', aList.ItemPadding = 0, IntToStr(aList.ItemPadding));
    Check('so the row still holds the font', aList.RowHeight = fontHeight, IntToStr(aList.RowHeight));

    //5. the padding is part of the row everywhere the row is measured, so a
    //ViewCount sized box grows with it
    aList.ItemPadding := 2;
    aList.ViewCount := 3;
    Check('a ViewCount sized box is N rows of the padded height',
      aList.Height = 3 * aList.RowHeight,
      IntToStr(aList.Height) + ' vs ' + IntToStr(3 * aList.RowHeight));

    //6. item hit testing uses the same padded row: the middle of the second
    //row is the second item
    aList.ViewCount := 0;
    aList.Items.Add('a');
    aList.Items.Add('b');
    Check('the second padded row answers the second item',
      aList.ItemIndexAt(aList.RowHeight + aList.RowHeight div 2) = 1,
      IntToStr(aList.ItemIndexAt(aList.RowHeight + aList.RowHeight div 2)));
  finally
    aList.Free;
    FreeAndNil(Res);
  end;

  WriteLn;
  if Failures = 0 then
    WriteLn('OK: ', Tests, ' checks passed')
  else
    WriteLn('FAILED: ', Failures, ' of ', Tests, ' checks');
  Halt(Failures);
end.
