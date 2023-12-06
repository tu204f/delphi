unit Lb.Bybit;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

implementation

function GetBybitModule(AHost, AVersion, AProduct, AModule: String): String;
var
  xS: String;
begin
  xS := AHost + '/' + AVersion;



  Result := xS;
end;

end.
