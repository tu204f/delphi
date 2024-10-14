(******************************************************************************)
(* Get Trade History                                                          *)
(* запросив у пользователей записи об исполнении, отсортированные по времени  *)
(* исполнения в порядке убывания. Однако для Classic spot они отсортированы   *)
(* по execId в порядке убывания.                                              *)
(******************************************************************************)
unit Lb.Bybit.TradeHistory;

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
  TTradeHistory = class(TCustonObjectJson)
  public
    symbol: String;
    orderType: String;
    underlyingPrice: String;
    orderLinkId: String;
    side: String;
    indexPrice: String;
    orderId: String;
    stopOrderType: String;
    leavesQty: String;
    execTime: String;
    feeCurrency: String;
    isMaker: Boolean;
    execFee: String;
    feeRate: String;
    execId: String;
    tradeIv: String;
    blockTradeId: String;
    markPrice: String;
    execPrice: String;
    markIv: String;
    orderQty: String;
    orderPrice: String;
    execValue: String;
    execType: String;
    execQty: String;
    closedSize: String;
    seq: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); override;
  end;
  TTradeHistoryList = TObjectList<TTradeHistory>;

  TBybitTradeHistory = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FOrderID: String;
    FOrderLinkID: String;
    FBaseCoin: String;
    FStartTime: Integer;
    FEndTime: Integer;
    FExecType: String;
    FLimit: Integer;
    FCursor: String;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetOrderID(const Value: String);
    procedure SetOrderLinkID(const Value: String);
    procedure SetBaseCoin(const Value: String);
    procedure SetStartTime(const Value: Integer);
    procedure SetEndTime(const Value: Integer);
    procedure SetExecType(const Value: String);
    procedure SetLimit(const Value: Integer);
    procedure SetCursor(const Value: String);
  private
    FNextPageCursor: String;
  protected
    FTradeHistorys: TTradeHistoryList;
    FListJson: TJSONArray;
    procedure DoEventParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property TradeHistorys: TTradeHistoryList read FTradeHistorys;
  public
    property Category: TTypeCategory read FCategory write SetCategory;
    property Symbol: String read FSymbol write SetSymbol;
    property OrderID: String read FOrderID write SetOrderID;
    property OrderLinkID: String read FOrderLinkID write SetOrderLinkID;
    property BaseCoin: String read  FBaseCoin write SetBaseCoin;
    property StartTime: Integer read FStartTime write SetStartTime;
    property EndTime: Integer read FEndTime write SetEndTime;
    property ExecType: String read FExecType write SetExecType;
    property Limit: Integer read FLimit write SetLimit;
    property Cursor: String read FCursor write SetCursor;
    property NextPageCursor: String read FNextPageCursor;
  end;

implementation

procedure SetParserObjects(AListJson: TJSONArray; ATradeHistorys: TTradeHistoryList);
var
  i, iCount: Integer;
  xObjectJson: TJSONObject;
  xTradeHistory: TTradeHistory;
begin
  ATradeHistorys.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xTradeHistory := TTradeHistory.Create;
      xTradeHistory.SetObjectJson(xObjectJson);
      ATradeHistorys.Add(xTradeHistory);
    end;
end;

{ TTradeHistory }

constructor TTradeHistory.Create;
begin
  inherited;

end;

destructor TTradeHistory.Destroy;
begin

  inherited;
end;

procedure TTradeHistory.SetObjectJson(const AObjectJson: TJSONObject);
begin
  symbol          := GetStrToJson(AObjectJson.Values['symbol']);
  orderType       := GetStrToJson(AObjectJson.Values['orderType']);
  underlyingPrice := GetStrToJson(AObjectJson.Values['underlyingPrice']);
  orderLinkId     := GetStrToJson(AObjectJson.Values['orderLinkId']);
  side            := GetStrToJson(AObjectJson.Values['side']);
  indexPrice      := GetStrToJson(AObjectJson.Values['indexPrice']);
  orderId         := GetStrToJson(AObjectJson.Values['orderId']);
  stopOrderType   := GetStrToJson(AObjectJson.Values['stopOrderType']);
  leavesQty       := GetStrToJson(AObjectJson.Values['leavesQty']);
  execTime        := GetStrToJson(AObjectJson.Values['execTime']);
  feeCurrency     := GetStrToJson(AObjectJson.Values['feeCurrency']);
  isMaker         := GetBoolToJson(AObjectJson.Values['isMaker']);
  execFee         := GetStrToJson(AObjectJson.Values['execFee']);
  feeRate         := GetStrToJson(AObjectJson.Values['feeRate']);
  execId          := GetStrToJson(AObjectJson.Values['execId']);
  tradeIv         := GetStrToJson(AObjectJson.Values['tradeIv']);
  blockTradeId    := GetStrToJson(AObjectJson.Values['blockTradeId']);
  markPrice       := GetStrToJson(AObjectJson.Values['markPrice']);
  execPrice       := GetStrToJson(AObjectJson.Values['execPrice']);
  markIv          := GetStrToJson(AObjectJson.Values['markIv']);
  orderQty        := GetStrToJson(AObjectJson.Values['orderQty']);
  orderPrice      := GetStrToJson(AObjectJson.Values['orderPrice']);
  execValue       := GetStrToJson(AObjectJson.Values['execValue']);
  execType        := GetStrToJson(AObjectJson.Values['execType']);
  execQty         := GetStrToJson(AObjectJson.Values['execQty']);
  closedSize      := GetStrToJson(AObjectJson.Values['closedSize']);
  seq             := GetIntToJson(AObjectJson.Values['seq']);
end;

{ TBybitTradeHistory }

constructor TBybitTradeHistory.Create;
begin
  inherited;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/execution/list';
  FTradeHistorys := TTradeHistoryList.Create;
end;

destructor TBybitTradeHistory.Destroy;
begin
  FreeAndNil(FTradeHistorys);
  inherited;
end;


procedure TBybitTradeHistory.DoEventParser;
var
  xValueJson: TJSONValue;
begin
  xValueJson := Response.ResultObject.Values['list'];
  if Assigned(xValueJson) then
    if xValueJson is TJSONArray then
    begin
      FListJson := TJSONArray(xValueJson);
      SetParserObjects(FListJson,FTradeHistorys);
    end;
end;

procedure TBybitTradeHistory.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory));
end;

procedure TBybitTradeHistory.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitTradeHistory.SetOrderID(const Value: String);
begin
  FOrderID := Value;
  BybitModule.Params.SetParam('orderId',FOrderID);
end;

procedure TBybitTradeHistory.SetOrderLinkID(const Value: String);
begin
  FOrderLinkID := Value;
  BybitModule.Params.SetParam('orderLinkId',FOrderLinkID);
end;

procedure TBybitTradeHistory.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitTradeHistory.SetStartTime(const Value: Integer);
begin
  FStartTime := Value;
  BybitModule.Params.SetParam('startTime',FStartTime);
end;

procedure TBybitTradeHistory.SetEndTime(const Value: Integer);
begin
  FEndTime := Value;
  BybitModule.Params.SetParam('endTime',FEndTime);
end;


procedure TBybitTradeHistory.SetExecType(const Value: String);
begin
  FExecType := Value;
  BybitModule.Params.SetParam('execType',FExecType);
end;

procedure TBybitTradeHistory.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitTradeHistory.SetCursor(const Value: String);
begin
  FCursor := Value;
  BybitModule.Params.SetParam('cursor',FCursor);
end;



end.
