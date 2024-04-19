unit Lb.OperationTrade;

interface

uses
  System.Classes,
  System.SysUtils,
  Lb.Bybit.SysUtils,
  Lb.Bybit.RealTime,
  Lb.Bybit.Trade;

type
  TTypeOperation = (
    toNull,
    toBuy,
    toSell
  );

///<summary>
/// Инициализация ключей
///</summary>
procedure SetInitialization(const AKey, ASecret: String);

///<summary>
/// Генерируем приказ заявки
///</summary>
function GetOperationTrade(ASymbol: String; ASide: TTypeSide;
  APrice, AQuantity: Double; AOrderLinkId: String = ''): String;

implementation

const
  API_KEY    = '3bI64e0kw4KuihyRPu';
  API_SECRET = 'jvwTC14ESSTjIpvXaRbDGW8xd1KoqD3H3cWY';

var
  Local_Key: String = '';
  Local_Secret: String = '';

///<summary>
/// Генерируем приказ заявки
///</summary>
function GetOrderLinkId: String;
begin
  // Уникальный нормер операции
  {todo: Геристрация уникального ключа}
  Result := Random(65000).ToString;
end;

procedure SetInitialization(const AKey, ASecret: String);
begin

  if AKey.IsEmpty then
    Local_Key := API_KEY
  else
    Local_Key := AKey;

  if ASecret.IsEmpty then
    Local_Secret := API_SECRET
  else
    Local_Secret := ASecret;

end;

function GetOperationTrade(ASymbol: String; ASide: TTypeSide;
  APrice, AQuantity: Double; AOrderLinkId: String = ''): String;
var
  xPlaceOrder: TParamOrder;
begin
  // Инструмент отслеживания
  // Передача ключей программе
  xPlaceOrder := TParamOrder.Create;
  try
    {todo: сохранение данных}
    xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;
    xPlaceOrder.Category    := TTypeCategory.tcLinear;
    xPlaceOrder.Symbol      := ASymbol;
    xPlaceOrder.Side        := ASide;
    xPlaceOrder.PositionIdx := 0;
    xPlaceOrder.OrderType   := TTypeOrder.Limit;
    xPlaceOrder.Qty         := AQuantity;
    xPlaceOrder.Price       := APrice;
    xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
    if not AOrderLinkId.IsEmpty then
      xPlaceOrder.OrderLinkId := AOrderLinkId;

    Result := SelectedOrder(
       Local_Key,
       Local_Secret,
       xPlaceOrder,
       nil
    );
  finally
    FreeAndNil(xPlaceOrder);
  end;
end;

end.
