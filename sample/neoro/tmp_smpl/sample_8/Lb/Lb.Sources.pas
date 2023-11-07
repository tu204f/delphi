unit Lb.Sources;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

type
  ///<summary>Источник данных, одного инструмента</summary>
  TSource = class(TStringList)
  private
    function GetCandels(Index: Integer): TCandel;
  public
    property Candels[Index: Integer]: TCandel read GetCandels;
  end;

  ///<summary>Список источников данных </summary>
  TSourceList = TObjectList<TSource>;

implementation

{ TSource }

function TSource.GetCandels(Index: Integer): TCandel;
var
  xS: String;
begin
  xS := Strings[Index];
  Result := TCandel.Create(xS);
end;

end.
