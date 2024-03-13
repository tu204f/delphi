unit Lb.Mode;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;

type
  ///<summary>Направление принимаемого решение</summary>
  TMode = (tmNull = 0, tmBuy, tmSell);

function GetRandomMode: TMode;

implementation

function GetRandomMode: TMode;
var
  xMode: Integer;
begin
  // Здесь модуль принимает решение
  xMode := Random(3);
  Result := TMode(xMode);
end;

end.
