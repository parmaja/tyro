unit TyroSounds;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}
{**
 * This file is part of the 'Tyro'
 * @description Simple Wave generator using RayLib
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

{////////////////////////////////////////////////////////////////////////////-
*  	Music Macro Language
*   https://en.wikipedia.org/wiki/Music_Macro_Language
*
*   This file is used part of the 'Tyro'
*   @license   The MIT License (MIT) Included in this distribution
*   @author    Zaher Dirkey <zaherdirkey at yahoo dot com>
//////////////////////////////////////////////////////////////////////////////-
*  Look at this site, it have many of songs
*  https://archeagemmllibrary.com/
*	 usefull refereces
*
*  http://web.mit.edu/18.06/www/Essays/linear-algebra-and-music.pdf
//////////////////////////////////////////////////////////////////////////////-}

interface

uses
  Classes, SysUtils, mnClasses, mnUtils, Math,
  Melodies,
  RayLib3, RayClasses;

type

  { TWaveForm }

  TWaveForm = class(TmnNamedObject)
  end;

  { TWaveForms }

  TWaveForms = Class(TmnNamedObjectList<TWaveForm>)
  end;

  { TMelody }

  TRayMelody = class(TMelody)
  private
    FWaveForms: TWaveForms;
  public
    constructor Create;
    destructor Destroy; override;

    property WaveForms: TWaveForms read FWaveForms;
  end;

implementation

{ TMelody }

constructor TRayMelody.Create;
begin
  inherited Create;
  FWaveForms := TWaveForms.Create;
end;

destructor TRayMelody.Destroy;
begin
  FreeAndNil(FWaveForms);
  inherited Destroy;
end;


initialization
end.

