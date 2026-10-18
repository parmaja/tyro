program test_fuse;
{*******************************************************************************
*
*  Test for the --fuse packaging (src/TyroFuse.pas).
*
*  Builds a sandbox with engine files, a release.conf and a game folder,
*  packs it with FusePackage, extracts the result and checks the zip layout:
*  engine files flattened at the root, the game folder below its own name,
*  the generated fuse.conf and its run/workpath entries. Also packs a single
*  script file and checks the failure path.
*
*  compile: fpc -B -Fu..\src -FUlib test_fuse.lpr
*  run    : test_fuse
*
*******************************************************************************}
{$mode delphi}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  SysUtils, Classes, Zipper, TyroFuse;

var
  Failed: Integer = 0;
  Root: string;

procedure Check(ACond: Boolean; const AName: string);
begin
  if ACond then
    WriteLn('  ok: ' + AName)
  else
  begin
    Inc(Failed);
    WriteLn('FAIL: ' + AName);
  end;
end;

procedure PutFile(const AName, AContent: string);
var
  sl: TStringList;
begin
  ForceDirectories(ExtractFilePath(AName));
  sl := TStringList.Create;
  try
    sl.Text := AContent;
    sl.SaveToFile(AName);
  finally
    sl.Free;
  end;
end;

function ReadText(const AName: string): string;
var
  sl: TStringList;
begin
  Result := '';
  if not FileExists(AName) then
    exit;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AName);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure RemoveTree(const ADir: string);
var
  p: string;
  sr: TSearchRec;
begin
  if not DirectoryExists(ADir) then
    exit;
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then
        continue;
      p := IncludeTrailingPathDelimiter(ADir) + sr.Name;
      if (sr.Attr and faDirectory) <> 0 then
        RemoveTree(p)
      else
        DeleteFile(p);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
  RemoveDir(ADir);
end;

function CountFiles(const ADir: string): Integer;
var
  p: string;
  sr: TSearchRec;
begin
  Result := 0;
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then
        continue;
      p := IncludeTrailingPathDelimiter(ADir) + sr.Name;
      if (sr.Attr and faDirectory) <> 0 then
        Inc(Result, CountFiles(p))
      else
        Inc(Result);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

procedure UnzipTo(const AZip, ADir: string);
var
  U: TUnZipper;
begin
  RemoveTree(ADir);
  ForceDirectories(ADir);
  U := TUnZipper.Create;
  try
    U.OutputPath := ADir;
    U.UnZipAllFiles(AZip);
  finally
    U.Free;
  end;
end;

procedure TestFolderPack;
var
  aGame, aZip, aOut, aMsg, aConf: string;
  aOk: Boolean;
begin
  WriteLn('folder pack:');
  aGame := Root + 'game';
  aZip := Root + 'game.zip';
  aOut := Root + 'out' + PathDelim;
  aConf := Root + 'release.conf';

  aOk := FusePackage(aGame, aZip, aConf, aMsg);
  Check(aOk, 'FusePackage succeeded: ' + aMsg);
  if not aOk then
    exit;

  Check(Pos('missing.dll', aMsg) > 0, 'missing release file reported');
  Check(FileExists(aZip), 'zip created');

  UnzipTo(aZip, aOut);
  Check(FileExists(aOut + 'engine.bin'), 'engine file at zip root (flattened)');
  Check(FileExists(aOut + 'a.pak') and FileExists(aOut + 'b.pak'), 'wildcard release files at zip root');
  Check(not FileExists(aOut + 'release.conf'), 'release conf itself not packed');
  Check(FileExists(aOut + 'game\game.tyro'), 'game folder packed under its name');
  Check(FileExists(aOut + 'game\data\level.txt'), 'game subfolder structure kept');
  Check(FileExists(aOut + 'fuse.conf'), 'fuse.conf at zip root');
  Check(CountFiles(aOut) = 6, 'exact entry count, got ' + IntToStr(CountFiles(aOut)));
  Check(Pos('run=game.tyro', ReadText(aOut + 'fuse.conf')) > 0, 'fuse.conf runs the main script');
  Check(Pos('workpath=game', ReadText(aOut + 'fuse.conf')) > 0, 'fuse.conf workpath is the folder');
end;

procedure TestFilePack;
var
  aZip, aOut, aMsg, aConf: string;
  aOk: Boolean;
begin
  WriteLn('file pack:');
  aZip := Root + 'solo.zip';
  aOut := Root + 'out2' + PathDelim;
  aConf := Root + 'release.conf';

  aOk := FusePackage(Root + 'solo.tyro', aZip, aConf, aMsg);
  Check(aOk, 'FusePackage succeeded: ' + aMsg);
  if not aOk then
    exit;

  Check(FileExists(aZip), 'zip created');
  UnzipTo(aZip, aOut);
  Check(FileExists(aOut + 'solo.tyro'), 'script at zip root');
  Check(FileExists(aOut + 'engine.bin'), 'engine files packed too');
  Check(FileExists(aOut + 'fuse.conf'), 'fuse.conf at zip root');
  Check(Pos('run=solo.tyro', ReadText(aOut + 'fuse.conf')) > 0, 'fuse.conf runs the script');
  Check(Pos('workpath=.', ReadText(aOut + 'fuse.conf')) > 0, 'fuse.conf workpath is .');
end;

procedure TestWithoutConf;
var
  aZip, aMsg: string;
  aOk: Boolean;
begin
  WriteLn('pack without release conf:');
  aZip := Root + 'bare.zip';
  aOk := FusePackage(Root + 'game', aZip, Root + 'none.conf', aMsg);
  Check(aOk, 'packs anyway: ' + aMsg);
  if not aOk then
    exit;
  Check(Pos('none.conf', aMsg) > 0, 'absent conf reported');
  Check(FileExists(aZip), 'zip created');
  UnzipTo(aZip, Root + 'out3' + PathDelim);
  Check(FileExists(Root + 'out3\game\game.tyro'), 'game packed');
  Check(not FileExists(Root + 'out3\engine.bin'), 'no engine files without conf');
end;

procedure TestErrors;
var
  aMsg: string;
begin
  WriteLn('error paths:');
  Check(not FusePackage(Root + 'missing_folder', Root + 'bad.zip', Root + 'none.conf', aMsg), 'missing path rejected');
  Check(aMsg <> '', 'error message set: ' + aMsg);
  Check(not FileExists(Root + 'bad.zip'), 'no zip written on failure');
  Check(not FusePackage('', Root + 'bad.zip', Root + 'none.conf', aMsg), 'empty path rejected');
end;

begin
  Root := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'fuse_sandbox' + PathDelim;
  RemoveTree(Root);
  ForceDirectories(Root + 'game\data');
  ForceDirectories(Root + 'pak');

  PutFile(Root + 'release.conf',
    '; engine files' + sLineBreak +
    'engine.bin' + sLineBreak +
    'pak\*.pak' + sLineBreak +
    'missing.dll' + sLineBreak);
  PutFile(Root + 'engine.bin', 'ENGINE');
  PutFile(Root + 'pak\a.pak', 'AAA');
  PutFile(Root + 'pak\b.pak', 'BBB');
  PutFile(Root + 'game\game.tyro', 'print("game")');
  PutFile(Root + 'game\data\level.txt', 'level');
  PutFile(Root + 'solo.tyro', 'print("solo")');

  try
    TestFolderPack;
    TestFilePack;
    TestWithoutConf;
    TestErrors;
  finally
    if Failed = 0 then
      RemoveTree(Root);
  end;

  WriteLn('');
  if Failed = 0 then
    WriteLn('all tests passed')
  else
    WriteLn(IntToStr(Failed) + ' test(s) failed');
  Halt(Failed);
end.
