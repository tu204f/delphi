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
  ///<summary>Направление заявки: Buy, Sell</summary>
  TTypeBuySell   = (tsBuy, tsSell);

  ///<summary>Тип заявки: Рыночная или лимитированная заявка</summary>
  TTypeMktLmt    = (toMarket, toLimit);

  ///<summary>Пересечение RSI сверху или снизу</summary>
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

type
  TOrderTrade = record
    Price: Double;
    Qty: Integer;
    BuySell: Char;
  end;
  TOrderTradeList = TList<TOrderTrade>;

procedure SetAddOrder(APrice: Double; AQty: Integer; ABuySell: Char);
function GetProfit(APrice: Double): Double;

implementation

var
  localUserOrders: TUserOrderList = nil;
  localOrderTrades: TOrderTradeList = nil;

function GetUserOrders: TUserOrderList;
begin
  if not Assigned(localUserOrders) then
    localUserOrders := TUserOrderList.Create;
  Result := localUserOrders;
end;


{
  ///<summary>Направление заявки: Buy, Sell</summary>
  TTypeBuySell   = (tsBuy, tsSell);

  ///<summary>Тип заявки: Рыночная или лимитированная заявка</summary>
  TTypeMktLmt    = (toMarket, toLimit);

  ///<summary>Пересечение RSI сверху или снизу</summary>
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

procedure SetAddOrder(APrice: Double; AQty: Integer; ABuySell: Char);
var
  xOrderTrade: TOrderTrade;
begin
  xOrderTrade.Price := APrice;
  xOrderTrade.Qty := AQty;
  xOrderTrade.BuySell := ABuySell;
  localOrderTrades.Add(xOrderTrade);
end;

function GetProfit(APrice: Double): Double;
begin
  Result := 0;
end;

initialization
  if not Assigned(localUserOrders) then
    localUserOrders := TUserOrderList.Create;

  if not Assigned(localOrderTrades) then
    localOrderTrades := TOrderTradeList.Create;

finalization
  FreeAndNil(localUserOrders);
  FreeAndNil(localOrderTrades);

end.
