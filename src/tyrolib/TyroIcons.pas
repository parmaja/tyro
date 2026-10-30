unit TyroIcons;
{$ifdef FPC}
{$MODE DELPHI}
{$endif}
{$H+}
{**
 *  This file is part of the "Tyro"
 *
 *  @license   MIT
 *
 *  @author    Zaher Dirkey
 *
 *  PNG icon cache for the built in tools.
 *
 *  Icons live in assets/icons as white shapes on a transparent background
 *  and are tinted when drawn, so one file covers every state color. The
 *  sprite editor falls back to its own vector drawings when a file is
 *  missing, which keeps the editor usable without the assets folder.
 *}

interface

uses
  Classes, SysUtils, Types, Math,
  RayLib, RayClasses,
  TyroClasses;

type
  TTyroIcons = class(TObject)
  private
    FNames: TStringList;
    FTextures: array of TTexture2D;
    FLoaded: array of Boolean;
    function IndexOf(const AName: string): Integer;
    function GetTexture(const AName: string): TTexture2D;
  public
    constructor Create;
    destructor Destroy; override;

    //* Unload every uploaded texture. Call it while the GL context is alive.
    procedure Unload;

    //* Draw the icon AName tinted with AColor inside R, in canvas
    //* coordinates. Returns False when the icon is not available, so the
    //* caller can draw a fallback instead.
    function Draw(ACanvas: TTyroCanvas; const R: TRect; const AName: string;
      AColor: TColor): Boolean;
  end;

implementation

constructor TTyroIcons.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
  FNames.CaseSensitive := False;
end;

destructor TTyroIcons.Destroy;
begin
  Unload;
  FreeAndNil(FNames);
  inherited;
end;

procedure TTyroIcons.Unload;
var
  I: Integer;
begin
  for I := 0 to High(FTextures) do
    if FLoaded[I] and (FTextures[I].ID <> 0) then
      RayLib.UnloadTexture(FTextures[I]);
  FTextures := nil;
  FLoaded := nil;
  if FNames <> nil then
    FNames.Clear;
end;

function TTyroIcons.IndexOf(const AName: string): Integer;
begin
  Result := FNames.IndexOf(AName);
end;

function TTyroIcons.GetTexture(const AName: string): TTexture2D;
var
  Idx: Integer;
  FileName: string;
begin
  Result := Default(TTexture2D);
  if AName = '' then
    Exit;

  Idx := IndexOf(AName);
  if Idx < 0 then
  begin
    Idx := FNames.Add(AName);
    SetLength(FTextures, Idx + 1);
    SetLength(FLoaded, Idx + 1);
  end
  else if FLoaded[Idx] then
  begin
    Result := FTextures[Idx];
    Exit;
  end;

  FLoaded[Idx] := True;
  if not Assigned(Res) then
  begin
    FLoaded[Idx] := False;
    Exit;
  end;

  FileName := Res.GuessFileName('icons' + PathDelim + AName + '.png');
  if not SysUtils.FileExists(FileName) then
    FileName := Res.GuessFileName('assets' + PathDelim + 'icons' + PathDelim +
      AName + '.png');
  if not SysUtils.FileExists(FileName) then
    Exit;

  Result := RayLib.LoadTexture(PUTF8Char(UTF8String(FileName)));
  if Result.ID <> 0 then
  begin
    RayLib.GenTextureMipmaps(Result);
    RayLib.SetTextureFilter(Result, TEXTURE_FILTER_TRILINEAR);
  end;
  FTextures[Idx] := Result;
end;

function TTyroIcons.Draw(ACanvas: TTyroCanvas; const R: TRect;
  const AName: string; AColor: TColor): Boolean;
var
  Tex: TTexture2D;
  S, L, T: Integer;
begin
  Tex := GetTexture(AName);
  Result := Tex.ID <> 0;
  if not Result then
    Exit;
  //* keep the icon square and centered inside R
  S := Min(R.Right - R.Left, R.Bottom - R.Top);
  if S <= 0 then
    Exit(False);
  L := (R.Left + R.Right - S) div 2;
  T := (R.Top + R.Bottom - S) div 2;
  RayLib.DrawTexturePro(Tex, RectangleOf(0, 0, Tex.Width, Tex.Height),
    RectangleOf(ACanvas.Origin.X + L, ACanvas.Origin.Y + T, S, S),
    Vector2Of(0, 0), 0, AColor);
end;

end.
