unit RayClasses;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{**
 *  This file is part of Tyro project, ported from C header raylib.h
 *
 * @project   https://www.github.com/parmaja/tyro
 *
 * @license   MIT (https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey zaherdirkey on internet
 *
 * This library not really tested as it, I just used some functions, please be patient and report any but you find.
 *}

interface

uses
  Classes, SysUtils, Contnrs,
  mnClasses, mnUtils,
  RayLib3;

type
  TRayObject = class(TObject)
  end;

  TRayUpdate = class(TRayObject)
  public
    procedure Update; virtual; abstract;
  end;

  { TRayUpdateList }

  TRayUpdateList = class(TmnObjectList<TRayUpdate>)
  public
    procedure Update;
  end;

  { Images }

  { TRayImage }

  TRayImage = class(TRayObject)
  private
    FImage: RayLib3.TImage;
  protected
  public
  end;

  { TRayPlaying }

  TRayPlaying = class(TRayUpdate)
  public
    procedure Play; virtual;
    procedure Stop; virtual;
  end;

  { TMusicPlaying }

  TMusicPlaying = class(TRayPlaying)
  public
    ID: Integer;
    Music: TMusic;
    procedure Play; override;
    procedure Stop; override;
    procedure Update; override;
  end;

  { Sound }

  { TRayLibSound }

  TRayLibSound = class(TRayObject)
  public
    Playing: TObjectList;
    procedure Init;
    constructor Create;
    destructor Destroy; override;
    procedure PlayMusicFile(FileName: string);
  end;

var
  RaySound: TRayLibSound = nil;
  RayUpdates: TRayUpdateList = nil;

implementation

var
  FAudioDeviceInitialized: Boolean = False;

{ TRayUpdateList }

procedure TRayUpdateList.Update;
var
  Item: TRayUpdate;
begin
  for Item in Self do
  begin
    Item.Update;
  end;
end;

{ TMusicPlaying }

procedure TMusicPlaying.Play;
begin
  inherited;
  PlayMusicStream(Music);
end;

procedure TMusicPlaying.Stop;
begin
  inherited;
  StopMusicStream(Music);
end;

procedure TMusicPlaying.Update;
begin
  UpdateMusicStream(Music);
end;

{ TRayPlaying }

procedure TRayPlaying.Play;
begin

end;

procedure TRayPlaying.Stop;
begin

end;

{ TRayLibSound }

procedure TRayLibSound.Init;
begin
  if not FAudioDeviceInitialized then
    InitAudioDevice();
  FAudioDeviceInitialized := True;
end;

constructor TRayLibSound.Create;
begin
  inherited Create;
  Playing:=TObjectList.Create;
end;

destructor TRayLibSound.Destroy;
begin
  FreeAndNil(Playing);
  if FAudioDeviceInitialized then
    CloseAudioDevice();
  inherited Destroy;
end;

procedure TRayLibSound.PlayMusicFile(FileName: string);
var
  MusicPlaying: TMusicPlaying;
begin
  MusicPlaying := TMusicPlaying.Create;
  MusicPlaying.Music := LoadMusicStream(PUTF8Char(FileName));
  Playing.Add(MusicPlaying);
  RayUpdates.Add(MusicPlaying);
  MusicPlaying.Play;
end;

initialization
  RayUpdates := TRayUpdateList.Create;
  RaySound := TRayLibSound.Create;
finalization
  FreeAndNil(RaySound);
end.

