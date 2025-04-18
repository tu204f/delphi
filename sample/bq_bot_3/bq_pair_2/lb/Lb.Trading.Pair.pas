unit Lb.Trading.Pair;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Lb.Bybit.SysUtils,
  Lb.SysUtils,
  Lb.Pairs;

type
  TEventOnMsgInfo = procedure(const AMsg: String);

  TParamTrading = record
    Symbol: String;
    ApiKey: String;
    ApiSecret: String;
    Time: TDateTime;
    Price: Double;
    Qty: Double;
    Side: TTypeBuySell;
    OnMsgInfo: TEventOnMsgInfo;
  end;

function SendTrade(const AParamTrading: TParamTrading): String;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  Lb.Bybit.Trade;

function SendTrade(const AParamTrading: TParamTrading): String;

  function _CreateOrderLinkId: String;
  var
    xS: String;
  begin
    xS := Random(65000).ToString;
    Result := xS;
  end;

  function _ToTypeSide(ASide: TTypeBuySell): TTypeSide;
  begin
    case ASide of
      TTypeBuySell.tsBuy: Result := TTypeSide.tsBuy;
      TTypeBuySell.tsSell: Result := TTypeSide.tsSell;
    else
      Result := TTypeSide.tsNull;
    end;
  end;

var
  xPlaceOrder: TParamOrder;
  xResponse: TOrderResponse;
  xLinkId: String;
begin
  xLinkId := _CreateOrderLinkId;
  Result := xLinkId;
  try
    // Инструмент отслеживания
    // Передача ключей программе
    xPlaceOrder := TParamOrder.Create;
    try
      xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;

      {todo: сохранение данных}
      xPlaceOrder.Category    := TTypeCategory.tcLinear;
      xPlaceOrder.Symbol      := AParamTrading.Symbol;

      xPlaceOrder.Side        := _ToTypeSide(AParamTrading.Side);

      xPlaceOrder.PositionIdx := 0;
      xPlaceOrder.OrderType   := TTypeOrder.Market;
      xPlaceOrder.Qty         := AParamTrading.Qty;
      xPlaceOrder.Price       := AParamTrading.Price;
      xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;

      xPlaceOrder.OrderLinkId := xLinkId;

      xResponse := TOrderResponse.Create;
      try
        // Реальные торговые операции
        SelectedOrder(
           AParamTrading.ApiKey,
           AParamTrading.ApiSecret,
           xPlaceOrder,
           xResponse // Возрат сообщение ос делке
        );
        if Assigned(AParamTrading.OnMsgInfo) then
          AParamTrading.OnMsgInfo(xResponse.Value + ' ' + xResponse.RetMsg);
      finally
        FreeAndNil(xResponse);
      end;
    finally
      FreeAndNil(xPlaceOrder);
    end;
  except
    on E: Exception do
      raise Exception.Create('Error Message: LinkID: [' + xLinkId + '].' + E.Message);
  end;
end;

end.
