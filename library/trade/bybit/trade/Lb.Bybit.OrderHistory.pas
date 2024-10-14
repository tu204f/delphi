(******************************************************************************)
(* Get Open Order History                                                     *)
(* «апрашивайте историю заказов. ѕоскольку создание/отмена заказа происходит  *)
(* асинхронно, данные, возвращаемые с этой конечной точки, могут              *)
(* задерживатьс€. ≈сли вы хотите получать информацию о заказе в режиме        *)
(* реального времени, вы можете запросить эту конечную точку или              *)
(* воспользоватьс€ потоком websocket (рекомендуетс€).                         *)
(******************************************************************************)
unit Lb.Bybit.OrderHistory;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.JSON,
  Lb.Bybit.SysUtils;

type
  ///<summary>
  /// —ами данные
  ///</summary>
  TOrderHistory = class(TCustonObjectJson)
  public
    orderId: String;
    orderLinkId: String;
    blockTradeId: String;
    symbol: String;
    price: String;
    qty: String;
    side: String;
    isLeverage: String;
    positionIdx: Integer;
    orderStatus: String;
    cancelType: String;
    rejectReason: String;
    avgPrice: String;
    leavesQty: String;
    leavesValue: String;
    cumExecQty: String;
    cumExecValue: String;
    cumExecFee: String;
    timeInForce: String;
    orderType: String;
    stopOrderType: String;
    orderIv: String;
    triggerPrice: String;
    takeProfit: String;
    stopLoss: String;
    tpTriggerBy: String;
    slTriggerBy: String;
    triggerDirection: String;
    triggerBy: String;
    lastPriceOnCreated: String;
    reduceOnly: Boolean;
    closeOnTrigger: Boolean;
    smpType: String;
    smpGroup: String;
    smpOrderId: String;
    tpslMode: String;
    tpLimitPrice: String;
    slLimitPrice: String;
    placeType: String;
    createdTime: String;
    updatedTime: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); override;
  end;
  TOrderHistoryList = TObjectList<TOrderHistory>;

  ///<summary>
  /// «апрашивает историю за€вко
  ///</summary>
  TBybitOrderHistory = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FBaseCoin: String;
    FSettleCoin: String;
    FOrderID: String;
    FOrderLinkID: String;
    FOrderFilter: String;
    FOrderStatus: String;
    FStartTime: Integer;
    FEndTime: Integer;
    FLimit: Integer;
    FCursor: String;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetBaseCoin(const Value: String);
    procedure SetSettleCoin(const Value: String);
    procedure SetOrderID(const Value: String);
    procedure SetOrderLinkID(const Value: String);
    procedure SetOrderFilter(const Value: String);
    procedure SetOrderStatus(const Value: String);
    procedure SetStartTime(const Value: Integer);
    procedure SetEndTime(const Value: Integer);
    procedure SetLimit(const Value: Integer);
    procedure SetCursor(const Value: String);
  private
    FNextPageCursor: String;
  protected
    FOrderHistorys: TOrderHistoryList;
    FListJson: TJSONArray;
    procedure DoEventParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property OrderHistorys: TOrderHistoryList read FOrderHistorys;
  public
    property Category: TTypeCategory read FCategory write SetCategory;
    property Symbol: String read FSymbol write SetSymbol;
    property BaseCoin: String read  FBaseCoin write SetBaseCoin;
    property SettleCoin: String read FSettleCoin write SetSettleCoin;
    property OrderID: String read FOrderID write SetOrderID;
    property OrderLinkID: String read FOrderLinkID write SetOrderLinkID;
    property OrderFilter: String read FOrderFilter write SetOrderFilter;
    property OrderStatus: String read FOrderStatus write SetOrderStatus;
    property StartTime: Integer read FStartTime write SetStartTime;
    property EndTime: Integer read FEndTime write SetEndTime;
    property Limit: Integer read FLimit write SetLimit;
    property Cursor: String read FCursor write SetCursor;
    property NextPageCursor: String read FNextPageCursor;
  end;

implementation

procedure SetParserObjects(AListJson: TJSONArray; AOrderHistorys: TOrderHistoryList);
var
  i, iCount: Integer;
  xObjectJson: TJSONObject;
  xOrderHistory: TOrderHistory;
begin
  AOrderHistorys.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xOrderHistory := TOrderHistory.Create;
      xOrderHistory.SetObjectJson(xObjectJson);
      AOrderHistorys.Add(xOrderHistory);
    end;
end;

{ TOrderHistory }

constructor TOrderHistory.Create;
begin
  inherited;

end;

destructor TOrderHistory.Destroy;
begin

  inherited;
end;

procedure TOrderHistory.SetObjectJson(const AObjectJson: TJSONObject);
begin
  orderId            := GetStrToJson(AObjectJson.Values['orderId']);
  orderLinkId        := GetStrToJson(AObjectJson.Values['orderLinkId']);
  blockTradeId       := GetStrToJson(AObjectJson.Values['blockTradeId']);
  symbol             := GetStrToJson(AObjectJson.Values['symbol']);
  price              := GetStrToJson(AObjectJson.Values['price']);
  qty                := GetStrToJson(AObjectJson.Values['qty']);
  side               := GetStrToJson(AObjectJson.Values['side']);
  isLeverage         := GetStrToJson(AObjectJson.Values['isLeverage']);
  positionIdx        := GetIntToJson(AObjectJson.Values['positionIdx']);
  orderStatus        := GetStrToJson(AObjectJson.Values['orderStatus']);
  cancelType         := GetStrToJson(AObjectJson.Values['cancelType']);
  rejectReason       := GetStrToJson(AObjectJson.Values['rejectReason']);
  avgPrice           := GetStrToJson(AObjectJson.Values['avgPrice']);
  leavesQty          := GetStrToJson(AObjectJson.Values['leavesQty']);
  leavesValue        := GetStrToJson(AObjectJson.Values['leavesValue']);
  cumExecQty         := GetStrToJson(AObjectJson.Values['cumExecQty']);
  cumExecValue       := GetStrToJson(AObjectJson.Values['cumExecValue']);
  cumExecFee         := GetStrToJson(AObjectJson.Values['cumExecFee']);
  timeInForce        := GetStrToJson(AObjectJson.Values['timeInForce']);
  orderType          := GetStrToJson(AObjectJson.Values['orderType']);
  stopOrderType      := GetStrToJson(AObjectJson.Values['stopOrderType']);
  orderIv            := GetStrToJson(AObjectJson.Values['orderIv']);
  triggerPrice       := GetStrToJson(AObjectJson.Values['triggerPrice']);
  takeProfit         := GetStrToJson(AObjectJson.Values['takeProfit']);
  stopLoss           := GetStrToJson(AObjectJson.Values['stopLoss']);
  tpTriggerBy        := GetStrToJson(AObjectJson.Values['tpTriggerBy']);
  slTriggerBy        := GetStrToJson(AObjectJson.Values['slTriggerBy']);
  triggerDirection   := GetStrToJson(AObjectJson.Values['triggerDirection']);
  triggerBy          := GetStrToJson(AObjectJson.Values['triggerBy']);
  lastPriceOnCreated := GetStrToJson(AObjectJson.Values['lastPriceOnCreated']);
  reduceOnly         := GetBoolToJson(AObjectJson.Values['reduceOnly']);
  closeOnTrigger     := GetBoolToJson(AObjectJson.Values['closeOnTrigger']);
  smpType            := GetStrToJson(AObjectJson.Values['smpType']);
  smpGroup           := GetStrToJson(AObjectJson.Values['smpGroup']);
  smpOrderId         := GetStrToJson(AObjectJson.Values['smpOrderId']);
  tpslMode           := GetStrToJson(AObjectJson.Values['tpslMode']);
  tpLimitPrice       := GetStrToJson(AObjectJson.Values['tpLimitPrice']);
  slLimitPrice       := GetStrToJson(AObjectJson.Values['slLimitPrice']);
  placeType          := GetStrToJson(AObjectJson.Values['placeType']);
  createdTime        := GetStrToJson(AObjectJson.Values['createdTime']);
  updatedTime        := GetStrToJson(AObjectJson.Values['updatedTime']);
end;

{ TBybitOrderHistory }

constructor TBybitOrderHistory.Create;
begin
  inherited;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/order/history';
  FOrderHistorys := TOrderHistoryList.Create;
end;

destructor TBybitOrderHistory.Destroy;
begin
  FreeAndNil(FOrderHistorys);
  inherited;
end;

procedure TBybitOrderHistory.DoEventParser;
var
  xValueJson: TJSONValue;
begin
  FNextPageCursor := GetStrToJson(Response.ResultObject.Values['nextPageCursor']);
  xValueJson := Response.ResultObject.Values['list'];
  if Assigned(xValueJson) then
    if xValueJson is TJSONArray then
    begin
      FListJson := TJSONArray(xValueJson);
      SetParserObjects(FListJson,FOrderHistorys);
    end;
end;

procedure TBybitOrderHistory.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory))
end;


procedure TBybitOrderHistory.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitOrderHistory.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitOrderHistory.SetSettleCoin(const Value: String);
begin
  FSettleCoin := Value;
  BybitModule.Params.SetParam('settleCoin',FSettleCoin);
end;

procedure TBybitOrderHistory.SetOrderID(const Value: String);
begin
  FOrderID := Value;
  BybitModule.Params.SetParam('orderId',FOrderID);
end;


procedure TBybitOrderHistory.SetOrderLinkID(const Value: String);
begin
  FOrderLinkID := Value;
  BybitModule.Params.SetParam('orderLinkId',FOrderLinkID);
end;

procedure TBybitOrderHistory.SetOrderFilter(const Value: String);
begin
  FOrderFilter := Value;
  BybitModule.Params.SetParam('orderFilter',FOrderFilter);
end;

procedure TBybitOrderHistory.SetOrderStatus(const Value: String);
begin
  FOrderStatus := Value;
  BybitModule.Params.SetParam('orderStatus',FOrderStatus);
end;

procedure TBybitOrderHistory.SetStartTime(const Value: Integer);
begin
  FStartTime := Value;
  BybitModule.Params.SetParam('startTime',FStartTime);
end;

procedure TBybitOrderHistory.SetEndTime(const Value: Integer);
begin
  FEndTime := Value;
  BybitModule.Params.SetParam('endTime',FEndTime);
end;

procedure TBybitOrderHistory.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitOrderHistory.SetCursor(const Value: String);
begin
  FCursor := Value;
  BybitModule.Params.SetParam('cursor',FCursor);
end;



end.
