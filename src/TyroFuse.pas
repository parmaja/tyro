unit TyroFuse;
{**
 *  This file is part of the "Tyro"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  --fuse packaging: packs a game (a folder or a single script) together
 *  with the engine files listed in release.conf and a generated fuse.conf
 *  into one self-contained zip package. See TTyroApplication.DoFuse.
 *}

{$ifdef FPC}
{$mode delphi}
{$endif}
{$H+}

interface

uses
  SysUtils, Classes
  {$ifdef FPC}
  , Zipper
  {$else}
  , System.Zip
  {$endif}
  ;

{* Packs APathTo (a game folder or a single script file) together with the
 * files listed in AConfFile into the zip AZipFile and generates a fuse.conf
 * at the zip root, so the extracted package runs by tyro itself.
 *
 * AConfFile holds one path per line (';' and '#' lines are comments),
 * resolved relative to the directory of AConfFile and stored flattened at
 * the zip root. APathTo keeps its shape: a folder is stored below a folder
 * of its own name, a file is stored as is.
 *
 * Returns True when the zip was written. AMessage receives the report:
 * warnings when successful, the error message when not. *}
function FusePackage(const APathTo, AZipFile, AConfFile: string; out AMessage: string): Boolean;

implementation

//* Zip appnote 4.4.17: archive paths must use '/' as separator.
function ToZipPath(const S: string): string;
begin
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
end;

function IsAbsPath(const S: string): Boolean;
begin
  Result := ((S <> '') and ((S[1] = '\') or (S[1] = '/'))) or
            ((Length(S) >= 2) and (S[2] = ':'));
end;

//* AFiles holds ArchiveName=DiskName, ready to be handed to the zipper.
procedure AddEntry(AFiles: TStringList; const ADisk, AArchive: string);
var
  aName: string;
begin
  aName := ToZipPath(AArchive);
  if AFiles.IndexOfName(aName) < 0 then
    AFiles.Add(aName + '=' + ADisk);
end;

//* Adds every file matched by APattern (wildcards allowed) to the package,
//* stored flattened at the zip root.
procedure AddPattern(AFiles: TStringList; const APattern: string; ANotes: TStringList);
var
  aDir, aMask: string;
  aMatched: Boolean;
  sr: TSearchRec;
begin
  aMask := ExtractFileName(APattern);
  if (Pos('*', aMask) = 0) and (Pos('?', aMask) = 0) then
  begin
    if FileExists(APattern) then
      AddEntry(AFiles, APattern, aMask)
    else if DirectoryExists(APattern) then
      ANotes.Add('skip folder: ' + APattern)
    else
      ANotes.Add('not found: ' + APattern);
    exit;
  end;

  aDir := ExtractFilePath(APattern);
  aMatched := False;
  if FindFirst(APattern, faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = 0 then
      begin
        aMatched := True;
        AddEntry(AFiles, aDir + sr.Name, sr.Name);
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
  if not aMatched then
    ANotes.Add('no match: ' + APattern);
end;

//* Adds the files listed in AConfFile to the package, flattened at the zip
//* root. Entries are resolved relative to the directory holding AConfFile.
procedure AddReleaseFiles(AFiles: TStringList; const AConfFile: string; ANotes: TStringList);
var
  aLines: TStringList;
  aBase, s, aDisk: string;
  i: Integer;
begin
  aBase := ExtractFilePath(ExpandFileName(AConfFile));
  aLines := TStringList.Create;
  try
    try
      aLines.LoadFromFile(AConfFile);
    except
      on E: Exception do
      begin
        ANotes.Add('cannot read ' + AConfFile + ': ' + E.Message);
        exit;
      end;
    end;
    for i := 0 to aLines.Count - 1 do
    begin
      s := Trim(aLines[i]);
      if (s = '') or (s[1] = ';') or (s[1] = '#') then
        continue;
      if IsAbsPath(s) then
        aDisk := s
      else
        aDisk := aBase + s;
      AddPattern(AFiles, aDisk, ANotes);
    end;
  finally
    aLines.Free;
  end;
end;

//* Recursively adds the files of ADiskDir to the package below AArchiveDir.
procedure AddFolderFiles(AFiles: TStringList; const ADiskDir, AArchiveDir: string; ANotes: TStringList);
var
  aDisk: string;
  sr: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(ADiskDir) + '*', faAnyFile, sr) <> 0 then
  begin
    ANotes.Add('cannot read folder: ' + ADiskDir);
    exit;
  end;
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then
        continue;
      aDisk := IncludeTrailingPathDelimiter(ADiskDir) + sr.Name;
      if (sr.Attr and faDirectory) <> 0 then
        AddFolderFiles(AFiles, aDisk, AArchiveDir + sr.Name + '/', ANotes)
      else
        AddEntry(AFiles, aDisk, AArchiveDir + sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

//* Finds the main script of a game folder: <foldername>.tyro first, then
//* any *.tyro, then any *.lua, in the top level of the folder. Returns the
//* name relative to the folder, '' when there is none.
function FindMainScript(const ADiskDir, AFolderName: string): string;

  function FindMask(const AMask: string): string;
  var
    sr: TSearchRec;
  begin
    Result := '';
    if FindFirst(IncludeTrailingPathDelimiter(ADiskDir) + AMask, faAnyFile, sr) = 0 then
    try
      repeat
        if (sr.Attr and faDirectory) = 0 then
        begin
          Result := sr.Name;
          exit;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;

var
  aDir: string;
begin
  Result := '';
  aDir := IncludeTrailingPathDelimiter(ADiskDir);
  if FileExists(aDir + AFolderName + '.tyro') then
  begin
    Result := AFolderName + '.tyro';
    exit;
  end;
  Result := FindMask('*.tyro');
  if Result = '' then
    Result := FindMask('*.lua');
end;

//* Writes the entries of AFiles (ArchiveName=DiskName) into AZipFile.
procedure ZipPackage(const AZipFile: string; AFiles: TStringList);
{$ifdef FPC}
var
  aZipper: TZipper;
  i: Integer;
begin
  aZipper := TZipper.Create;
  try
    for i := 0 to AFiles.Count - 1 do
      aZipper.Entries.AddFileEntry(AFiles.ValueFromIndex[i], AFiles.Names[i]);
    aZipper.SaveToFile(AZipFile);
  finally
    aZipper.Free;
  end;
end;
{$else}
var
  aZip: TZipFile;
  i: Integer;
begin
  aZip := TZipFile.Create;
  try
    aZip.Open(AZipFile, fmCreate);
    for i := 0 to AFiles.Count - 1 do
      aZip.Add(AFiles.ValueFromIndex[i], AFiles.Names[i]);
    aZip.Close;
  finally
    aZip.Free;
  end;
end;
{$endif}

function FusePackage(const APathTo, AZipFile, AConfFile: string; out AMessage: string): Boolean;
var
  aFiles: TStringList; // ArchiveName=DiskName
  aNotes: TStringList; // warnings collected on the way
  aConf: TStringList;
  aIsDir: Boolean;
  aFolder, aMain, aContent, aTmp, aZipDir: string;
  aCount: Integer;
begin
  Result := False;
  AMessage := '';
  aFiles := TStringList.Create;
  aNotes := TStringList.Create;
  try
    try
      if APathTo = '' then
      begin
        AMessage := 'missing game path';
        exit;
      end;
      aIsDir := DirectoryExists(APathTo);
      if not aIsDir and not FileExists(APathTo) then
      begin
        AMessage := 'game path not found: ' + APathTo;
        exit;
      end;

      aZipDir := ExtractFilePath(ExpandFileName(AZipFile));
      if (aZipDir <> '') and not DirectoryExists(aZipDir) then
        if not ForceDirectories(aZipDir) then
        begin
          AMessage := 'cannot create folder: ' + aZipDir;
          exit;
        end;

      // Engine files first, flattened at the zip root.
      if (AConfFile <> '') and FileExists(AConfFile) then
        AddReleaseFiles(aFiles, AConfFile, aNotes)
      else
        aNotes.Add('release conf not found: ' + AConfFile);

      // The game and the fuse.conf that tells the engine what to run.
      if aIsDir then
      begin
        aFolder := ExtractFileName(ExcludeTrailingPathDelimiter(APathTo));
        if aFolder = '' then // a folder like '.' has no name of its own
          aFolder := ExtractFileName(ExcludeTrailingPathDelimiter(ExpandFileName(APathTo)));
        if aFolder = '' then
        begin
          AMessage := 'invalid game folder: ' + APathTo;
          exit;
        end;
        aCount := aFiles.Count;
        AddFolderFiles(aFiles, APathTo, aFolder + '/', aNotes);
        if aFiles.Count = aCount then
          aNotes.Add('empty game folder: ' + APathTo);
        aMain := FindMainScript(APathTo, aFolder);
        if aMain <> '' then
          aContent := 'run=' + aMain + sLineBreak + 'workpath=' + aFolder + sLineBreak
        else
        begin
          aContent := 'workpath=' + aFolder + sLineBreak;
          aNotes.Add('no script found in: ' + APathTo);
        end;
      end
      else
      begin
        AddEntry(aFiles, APathTo, ExtractFileName(APathTo));
        aContent := 'run=' + ExtractFileName(APathTo) + sLineBreak + 'workpath=.' + sLineBreak;
      end;

      if aFiles.Count = 0 then
      begin
        AMessage := 'nothing to pack';
        exit;
      end;

      // fuse.conf lives inside the package only, the temp copy is deleted.
      aTmp := AZipFile + '.fuse.tmp';
      aConf := TStringList.Create;
      try
        aConf.Text := aContent;
        aConf.SaveToFile(aTmp);
      finally
        aConf.Free;
      end;
      try
        aFiles.Values['fuse.conf'] := aTmp;
        ZipPackage(AZipFile, aFiles);
      finally
        DeleteFile(aTmp);
      end;

      Result := True;
      AMessage := Trim(aNotes.Text);
    except
      on E: Exception do
      begin
        Result := False;
        AMessage := E.Message;
      end;
    end;
  finally
    aFiles.Free;
    aNotes.Free;
  end;
end;

end.
