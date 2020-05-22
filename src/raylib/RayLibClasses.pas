unit RayLibClasses;
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
  Classes, SysUtils, RayLib3;

type
  TRayObject = class(TObject)
  end;

  TRayImage = class(TRayObject)
  private
    FImage: RayLib3.TImage;
  protected
  public
  end;

implementation

end.

