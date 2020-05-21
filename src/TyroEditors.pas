unit TyroEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TyroClasses;

type
  TyroEditor = class(TObject)
  private
  protected
    Lines: TStringList;
  public
    Sides: Integer;
    Cursor: TPoint;
  end;

implementation
end.

