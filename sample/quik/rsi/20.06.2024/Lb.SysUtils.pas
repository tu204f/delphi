unit Lb.SysUtils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  ///<summary>����������� ������: Buy, Sell</summary>
  TTypeBuySell   = (tsBuy, tsSell);

  ///<summary>��� ������: �������� ��� �������������� ������</summary>
  TTypeMktLmt    = (toMarket, toLimit);

  ///<summary>����������� RSI ������ ��� �����</summary>
  TTypeDirection = (tdUp, tdDown);

  TUserOrder = record
    SecCode: String;
    CodeClass: String;
    Quantity: Integer;
    ValueRSI: Integer;
    StepPrice: Integer;
    BuySell: TTypeBuySell;
    MktLmt: TTypeMktLmt;
    Direction: TTypeDirection;
  end;

  TUserOrderList = TList<TUserOrder>;


function GetUserOrders: TUserOrderList;

function GetStrToBuySell(const ABuySell: TTypeBuySell): String;
function GetStrToMktLmt(const AMktLmt: TTypeMktLmt): String;
function GetStrToDirection(const ADirection: TTypeDirection): String;

implementation

var
  localUserOrders: TUserOrderList = nil;

function GetUserOrders: TUserOrderList;
begin
  if not Assigned(localUserOrders) then
    localUserOrders := TUserOrderList.Create;
  Result := localUserOrders;
end;


{
  ///<summary>����������� ������: Buy, Sell</summary>
  TTypeBuySell   = (tsBuy, tsSell);

  ///<summary>��� ������: �������� ��� �������������� ������</summary>
  TTypeMktLmt    = (toMarket, toLimit);

  ///<summary>����������� RSI ������ ��� �����</summary>
  TTypeDirection = (tdUp, tdDown);

}

function GetStrToBuySell(const ABuySell: TTypeBuySell): String;
begin
  case ABuySell of
    tsBuy: Result := 'Buy';
    tsSell: Result := 'Sell';
  end;
end;

function GetStrToMktLmt(const AMktLmt: TTypeMktLmt): String;
begin
  case AMktLmt of
    toMarket: Result := 'M';
    toLimit: Result := 'L';
  end;
end;

function GetStrToDirection(const ADirection: TTypeDirection): String;
begin
  case ADirection of
    tdUp: Result := 'Up';
    tdDown: Result := 'Down';
  end;
end;

initialization
  if not Assigned(localUserOrders) then
    localUserOrders := TUserOrderList.Create;

finalization
  FreeAndNil(localUserOrders);

end.
