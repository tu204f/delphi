(******************************************************************************)
(* Get Open Orders                                                            *)
(* Запрашивайте незаполненные или частично заполненные заказы                 *)
(* в режиме реального времени.                                                *)
(******************************************************************************)
unit Lb.Bybit.RealTime;

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
  TRealTimeObject = class;
  TRealTimeObjectList = TObjectList<TRealTimeObject>;

  ///<summary>Get Open Orders - получить списко рдеров</summary>
  TBybitRealTime = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FBaseCoin: String;
    FSettleCoin: String;
    FOrderID: String;
    FOrderLinkID: String;
    FOpenOnly: Integer;
    FOrderFilter: String;
    FLimit: Integer;
    FCursor: String;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetBaseCoin(const Value: String);
    procedure SetSettleCoin(const Value: String);
    procedure SetOrderID(const Value: String);
    procedure SetOrderLinkID(const Value: String);
    procedure SetOpenOnly(const Value: Integer);
    procedure SetOrderFilter(const Value: String);
    procedure SetLimit(const Value: Integer);
    procedure SetCursor(const Value: String);
  private
    FNextPageCursor: String;
    FRealTimeObjects: TRealTimeObjectList;
  protected
    FListJson: TJSONArray;
    procedure DoEventParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property RealTimeObjects: TRealTimeObjectList read FRealTimeObjects;
  public
    property Category: TTypeCategory read FCategory write SetCategory;
    property Symbol: String read FSymbol write SetSymbol;
    property BaseCoin: String read  FBaseCoin write SetBaseCoin;
    property SettleCoin: String read FSettleCoin write SetSettleCoin;
    property OrderID: String read FOrderID write SetOrderID;
    property OrderLinkID: String read FOrderLinkID write SetOrderLinkID;
    property OpenOnly: Integer read FOpenOnly write SetOpenOnly;
    property OrderFilter: String read FOrderFilter write SetOrderFilter;
    property Limit: Integer read FLimit write SetLimit;
    property Cursor: String read FCursor write SetCursor;
  public
    property NextPageCursor: String read FNextPageCursor;
  end;

  TRealTimeObject = class(TCustonObjectJson)
  public
    OrderID: String;
    OrderLinkID: String;
    BlockTradeID: String;
    Symbol: String;
    Price: String;
    Qty: String;
    Side: String;
    IsLeverage: String;
    PositionIdx: Integer;
    OrderStatus: String;
    CreateType: String;
    CancelType: String;
    RejectReason: String;
    AvgPrice: String;
    LeavesQty: String;
    LeavesValue: String;
    CumExecQty: String;
    CumExecValue: String;
    CumExecFee: String;
    TimeInForce: String;
    OrderType: String;
    StopOrderType: String;
    OrderIv: String;
    MarketUnit: String;
    TriggerPrice: String;
    TakeProfit: String;
    StopLoss: String;
    TpSlMode: String;
    OcoTriggerType: String;
    TpLimitPrice: String;
    SlLimitPrice: String;
    TpTriggerBy: String;
    SlTriggerBy: String;
    TriggerDirection: Integer;
    TriggerBy: String;
    LastPriceOnCreated: String;
    ReduceOnly: Boolean;
    CloseOnTrigger: Boolean;
    PlaceType: String;
    SmpType: String;
    SmpGroup: Integer;
    SmpOrderID: String;
    CreatedTime: String;
    UpDatedTime: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); override;
  end;


procedure SetRealTimeObjects(AListJson: TJSONArray; ARealTimeObjects: TRealTimeObjectList);

implementation

procedure SetRealTimeObjects(AListJson: TJSONArray; ARealTimeObjects: TRealTimeObjectList);
var
  i, iCount: Integer;
  xObjectJson: TJSONObject;
  xRealTimeObject: TRealTimeObject;
begin
  ARealTimeObjects.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xRealTimeObject := TRealTimeObject.Create;
      xRealTimeObject.SetObjectJson(xObjectJson);
      ARealTimeObjects.Add(xRealTimeObject);
    end;
end;

{ TBybitRealTime }

constructor TBybitRealTime.Create;
begin
  inherited;
  FNextPageCursor := '';
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/order/realtime';
  FRealTimeObjects := TRealTimeObjectList.Create;
end;

destructor TBybitRealTime.Destroy;
begin
  FreeAndNil(FRealTimeObjects);
  inherited;
end;

procedure TBybitRealTime.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory))
end;

procedure TBybitRealTime.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitRealTime.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitRealTime.SetSettleCoin(const Value: String);
begin
  FSettleCoin := Value;
  BybitModule.Params.SetParam('settleCoin',FSettleCoin);
end;


procedure TBybitRealTime.SetOrderID(const Value: String);
begin
  FOrderID := Value;
  BybitModule.Params.SetParam('orderId',FOrderID);
end;


procedure TBybitRealTime.SetOrderLinkID(const Value: String);
begin
  FOrderLinkID := Value;
  BybitModule.Params.SetParam('orderLinkId',FOrderLinkID);
end;


procedure TBybitRealTime.SetOpenOnly(const Value: Integer);
begin
  FOpenOnly := Value;
  BybitModule.Params.SetParam('openOnly',FOpenOnly.ToString);
end;

procedure TBybitRealTime.SetOrderFilter(const Value: String);
begin
  FOrderFilter := Value;
  BybitModule.Params.SetParam('orderFilter',FOrderFilter);
end;

procedure TBybitRealTime.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitRealTime.SetCursor(const Value: String);
begin
  FCursor := Value;
  BybitModule.Params.SetParam('cursor',FCursor);
end;

procedure TBybitRealTime.DoEventParser;
var
  xValueJson: TJSONValue;
begin
  FNextPageCursor := GetStrToJson(Response.ResultObject.Values['nextPageCursor']);
  xValueJson := Response.ResultObject.Values['list'];
  if Assigned(xValueJson) then
    if xValueJson is TJSONArray then
    begin
      FListJson := TJSONArray(xValueJson);
      SetRealTimeObjects(FListJson,FRealTimeObjects);
    end;
end;

{ TRealTimeObject }

constructor TRealTimeObject.Create;
begin
  inherited Create;

end;

destructor TRealTimeObject.Destroy;
begin

  inherited Destroy;
end;

procedure TRealTimeObject.SetObjectJson(const AObjectJson: TJSONObject);
begin
  OrderID            := GetStrToJson(AObjectJson.Values['orderId']);
  OrderLinkID        := GetStrToJson(AObjectJson.Values['orderLinkId']);
  BlockTradeID       := GetStrToJson(AObjectJson.Values['blockTradeId']);
  Symbol             := GetStrToJson(AObjectJson.Values['symbol']);
  Price              := GetStrToJson(AObjectJson.Values['price']);
  Qty                := GetStrToJson(AObjectJson.Values['qty']);
  Side               := GetStrToJson(AObjectJson.Values['side']);
  IsLeverage         := GetStrToJson(AObjectJson.Values['isLeverage']);
  PositionIdx        := GetIntToJson(AObjectJson.Values['positionIdx']);
  OrderStatus        := GetStrToJson(AObjectJson.Values['orderStatus']);
  CreateType         := GetStrToJson(AObjectJson.Values['createType']);
  CancelType         := GetStrToJson(AObjectJson.Values['cancelType']);
  RejectReason       := GetStrToJson(AObjectJson.Values['rejectReason']);
  AvgPrice           := GetStrToJson(AObjectJson.Values['avgPrice']);
  LeavesQty          := GetStrToJson(AObjectJson.Values['leavesQty']);
  LeavesValue        := GetStrToJson(AObjectJson.Values['leavesValue']);
  CumExecQty         := GetStrToJson(AObjectJson.Values['cumExecQty']);
  CumExecValue       := GetStrToJson(AObjectJson.Values['cumExecValue']);
  CumExecFee         := GetStrToJson(AObjectJson.Values['cumExecFee']);
  TimeInForce        := GetStrToJson(AObjectJson.Values['timeInForce']);
  OrderType          := GetStrToJson(AObjectJson.Values['orderType']);
  StopOrderType      := GetStrToJson(AObjectJson.Values['stopOrderType']);
  OrderIv            := GetStrToJson(AObjectJson.Values['orderIv']);
  MarketUnit         := GetStrToJson(AObjectJson.Values['marketUnit']);
  TriggerPrice       := GetStrToJson(AObjectJson.Values['triggerPrice']);
  TakeProfit         := GetStrToJson(AObjectJson.Values['takeProfit']);
  StopLoss           := GetStrToJson(AObjectJson.Values['stopLoss']);
  TpSlMode           := GetStrToJson(AObjectJson.Values['tpslMode']);
  OcoTriggerType     := GetStrToJson(AObjectJson.Values['ocoTriggerType']);
  TpLimitPrice       := GetStrToJson(AObjectJson.Values['tpLimitPrice']);
  SlLimitPrice       := GetStrToJson(AObjectJson.Values['slLimitPrice']);
  TpTriggerBy        := GetStrToJson(AObjectJson.Values['tpTriggerBy']);
  SlTriggerBy        := GetStrToJson(AObjectJson.Values['slTriggerBy']);
  TriggerDirection   := GetIntToJson(AObjectJson.Values['triggerDirection']);
  TriggerBy          := GetStrToJson(AObjectJson.Values['triggerBy']);
  LastPriceOnCreated := GetStrToJson(AObjectJson.Values['lastPriceOnCreated']);
  ReduceOnly         := GetBoolToJson(AObjectJson.Values['reduceOnly']);
  CloseOnTrigger     := GetBoolToJson(AObjectJson.Values['closeOnTrigger']);
  PlaceType          := GetStrToJson(AObjectJson.Values['placeType']);
  SmpType            := GetStrToJson(AObjectJson.Values['smpType']);
  SmpGroup           := GetIntToJson(AObjectJson.Values['smpGroup']);
  SmpOrderID         := GetStrToJson(AObjectJson.Values['smpOrderId']);
  CreatedTime        := GetStrToJson(AObjectJson.Values['createdTime']);
  UpDatedTime        := GetStrToJson(AObjectJson.Values['updatedTime']);
end;

end.
