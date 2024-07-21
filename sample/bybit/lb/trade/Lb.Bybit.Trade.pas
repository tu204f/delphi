(******************************************************************************)
(*  ��������� ��������� ���������� ��������                                   *)
(******************************************************************************)
unit Lb.Bybit.Trade;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Encryption,
  Lb.Bybit.ServerTime;

type
  ///<summary>���������� ������ �� �����</summary>
  TParamOrder = class(TObject)
  public type
    TTypeProc = (
      Place,  // ��������� ������
      Amend,  // �������� ������
      Cancel  // �������� ������
    );
    TParam = TPair<String,String>;
    TParamList = TList<TParam>;
  private
    FSymbol: String;
    FSide: TTypeSide;
    FQty: Double;
    FPrice: Double;
    FOrderLinkID: String;
    FParams: TParamList;
    procedure AddParam(const AKye, AValue: String);
    function GetValue: String;
  private
    FTypeProc: TTypeProc;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetIsLeverage(const Value: Integer);
    procedure SetSide(const Value: TTypeSide);
    procedure SetOrderType(const Value: TTypeOrder);
    procedure SetQty(const Value: Double);
    procedure SetMarketUnit(const Value: String);
    procedure SetPrice(const Value: Double);
    procedure SetTriggerDirection(const Value: Integer);
    procedure SetOrderFilter(const Value: TTypeOrderFilter);
    procedure SetTriggerPrice(const Value: Double);
    procedure SetTriggerBy(const Value: TTypeTriggerBy);
    procedure SetOrderLv(const Value: String);
    procedure SetTimeInForce(const Value: TTypeTimeInForce);
    procedure SetPositionIdx(const Value: Integer);
    procedure SetOrderLinkId(const Value: String);
    procedure SetTakeProfit(const Value: Double);
    procedure SetStopLoss(const Value: Double);
    procedure SetTP_TriggerBy(const Value: TTypeTriggerBy);
    procedure SetSL_TriggerBy(const Value: TTypeTriggerBy);
    procedure SetReduceOnly(const Value: Boolean);
    procedure SetCloseOnTrigger(const Value: Boolean);
    procedure SetSmpType(const Value: TTypeSmpType);
    procedure SetMMP(const Value: Boolean);
    procedure SetTpSlMode(const Value: TTypeTpSlMode);
    procedure SetTpLimitPrice(const Value: Double);
    procedure SetSlLimitPrice(const Value: Double);
    procedure SetTpOrderType(const Value: Double);
    procedure SetSpOrderType(const Value: Double);
    procedure SetOrderID(const Value: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property TypeProc: TTypeProc read FTypeProc write FTypeProc;
    property OrderID: String write SetOrderID;
    property Category: TTypeCategory write SetCategory;
    property Symbol: String read FSymbol write SetSymbol;
    property IsLeverage: Integer write SetIsLeverage;
    property Side: TTypeSide read FSide write SetSide;
    property OrderType: TTypeOrder write SetOrderType;
    property Qty: Double read FQty write SetQty;
    property MarketUnit: String write SetMarketUnit;
    property Price: Double read FPrice write SetPrice;
    property TriggerDirection: Integer write SetTriggerDirection;
    property OrderFilter: TTypeOrderFilter write SetOrderFilter;
    property TriggerPrice: Double write SetTriggerPrice;
    property TriggerBy: TTypeTriggerBy write SetTriggerBy;
    property OrderLv: String write SetOrderLv;
    property TimeInForce: TTypeTimeInForce write SetTimeInForce;
    property PositionIdx: Integer write SetPositionIdx;
    property OrderLinkID: String read FOrderLinkID write SetOrderLinkID;
    property TakeProfit: Double write SetTakeProfit;
    property StopLoss: Double write SetStopLoss;
    property TP_TriggerBy: TTypeTriggerBy write SetTP_TriggerBy;
    property SL_TriggerBy: TTypeTriggerBy write SetSL_TriggerBy;
    property ReduceOnly: Boolean write SetReduceOnly;
    property CloseOnTrigger: Boolean write SetCloseOnTrigger;
    property SmpType: TTypeSmpType write SetSmpType;
    property MMP: Boolean write SetMMP;
    property TpSlMode: TTypeTpSlMode write SetTpSlMode;
    property TpLimitPrice: Double write SetTpLimitPrice;
    property SlLimitPrice: Double write SetSlLimitPrice;
    property TpOrderType: Double write SetTpOrderType;
    property SlOrderType: Double write SetSpOrderType;
    property Value: String read GetValue;
  end;

  ///<summary>������ ������������ ������</summary>
  TParamOrderList = TObjectList<TParamOrder>;

  TBybitPlaceOrder = class(TBybitHttpClient)
  private
    FServerTime: TBybitServerTime;
    FPlaceOrders: TParamOrderList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function AddPlaceOrder: TParamOrder;
    property PlaceOrders: TParamOrderList read FPlaceOrders;
  end;


  ///<summary>����� ������� �� ������</summary>
  TOrderResponse = class(TBytiyResponse)
  private
    FOrderID: String;
    FOrderLinkID: String;
  public
    procedure SetParserValue(const AValue: String); override;
    property OrderID: String read FOrderID;
    ///<summary>���������������� ������������� ������, ����������� �������������</summary>
    property OrderLinkID: String read FOrderLinkID;
  end;

function SelectedOrder(const ApiKey, ApiSecret: String; APlaceOrder: TParamOrder; AOrderResponse: TOrderResponse): String;

implementation

uses
  System.Math,
  System.Hash,
  System.DateUtils;


function GetDecimalSeparator(const AValue: String): String;
var
  xValue: String;
  xIndex: Integer;
begin
  xValue := AValue;
  xIndex := Pos(',',xValue);
  if xIndex >= 1 then
    xValue[xIndex] := '.';
  Result := xValue;
end;

function SelectedOrder(const ApiKey, ApiSecret: String; APlaceOrder: TParamOrder; AOrderResponse: TOrderResponse): String;
var
  xValue: String;
  xModule: TBybitModule;
  xEncryption: TEncryption;
  xClientAPI: TBybitHttpClientAPI;
  xSignature: String;
begin
  if not Assigned(APlaceOrder) then
    raise Exception.Create('Error Message: �������� �������');

  xModule := TBybitModule.Create;
  xEncryption:= TEncryption.Create;
  xClientAPI := TBybitHttpClientAPI.Create;
  try
    // ���������� �������
    xEncryption.ApiKey    := ApiKey;
    xEncryption.ApiSecret := ApiSecret;
    xEncryption.Timestamp := GetNow.ToString;

    xValue := APlaceOrder.Value;
    xEncryption.QueryBody := xValue;
    xSignature := xEncryption.Signature;

    // ��������� ������ �������
    xModule.TypeHttp := TTypeHttp.thPost;
    xModule.Host := BYBIT_HOST;
    case APlaceOrder.TypeProc of
      Place : xModule.Module := '/v5/order/create';
      Amend : xModule.Module := '/v5/order/amend';
      Cancel: xModule.Module := '/v5/order/cancel';
    end;
    // �������� ������
    with xModule.Headers do
    begin
      Values['X-BAPI-API-KEY']     := xEncryption.ApiKey;
      Values['X-BAPI-SIGN']        := xSignature;
      Values['X-BAPI-SIGN-TYPE']   := '2';
      Values['X-BAPI-TIMESTAMP']   := xEncryption.Timestamp;
      Values['X-BAPI-RECV-WINDOW'] := xEncryption.RecvWindow;
    end;

    // ��� ������
    xClientAPI.BybitModule := xModule;
    xClientAPI.Source := xValue;
    xClientAPI.Selected;

    if Assigned(AOrderResponse) then
      AOrderResponse.SetParserValue(xClientAPI.ResponseValue);

    Result := xClientAPI.ResponseValue;
  finally
    FreeAndNil(xClientAPI);
    FreeAndNil(xEncryption);
    FreeAndNil(xModule);
  end;
end;


{ TParamOrder }

constructor TParamOrder.Create;
begin
  FParams := TParamList.Create;
end;

destructor TParamOrder.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TParamOrder.AddParam(const AKye, AValue: String);
begin
  FParams.Add(TParam.Create(AKye,AValue));
end;

function TParamOrder.GetValue: String;
var
  xValue: String;
  i, iCount: Integer;
  xParam: TParamOrder.TParam;
begin
  xValue := '';
  iCount := FParams.Count;
  if iCount > 0 then
  begin
    xValue := '{';
    for i := 0 to iCount - 2 do
    begin
      xParam := FParams[i];
      xValue := xValue + '"' + xParam.Key + '":' + xParam.Value + ',';
    end;
    xParam := FParams[iCount - 1];
    xValue := xValue + '"' + xParam.Key + '":' + xParam.Value;
    xValue := xValue + '}';
  end;
  Result := xValue;
end;

procedure TParamOrder.SetOrderID(const Value: String);
begin
  AddParam(
    'orderId',
    '"' + Value + '"'
  );
end;

procedure TParamOrder.SetCategory(const Value: TTypeCategory);
begin
  AddParam(
    'category',
    '"' + GetStrToTypeCategory(Value) + '"'
  );
end;

procedure TParamOrder.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  AddParam(
    'symbol',
    '"' + Value + '"'
  );
end;

procedure TParamOrder.SetIsLeverage(const Value: Integer);
begin
  AddParam(
    'isLeverage',
    Value.ToString
  );
end;

procedure TParamOrder.SetSide(const Value: TTypeSide);
begin
  FSide := Value;
  AddParam(
    'side',
    '"' + GetStrToTypeSide(Value) + '"'
  );
end;

procedure TParamOrder.SetOrderType(const Value: TTypeOrder);
begin
  AddParam(
    'orderType',
   '"' + GetStrToTypeOrder(Value) + '"'
  );
end;

procedure TParamOrder.SetQty(const Value: Double);
begin
  FQty := Value;
  AddParam(
    'qty',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TParamOrder.SetMarketUnit(const Value: String);
begin
  AddParam(
    'marketUnit',
   '"' + Value + '"'
  );
end;

procedure TParamOrder.SetPrice(const Value: Double);
begin
  FPrice := Value;
  AddParam(
    'price',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetTriggerDirection(const Value: Integer);
begin
  // �������� ��������� �������.
  // ������������ ��� ����������� ���������� ����������� ��������� �������.
  // 1: �����������, ����� �������� ���� ���������� �� ���������� ����
  // 2: �����������, ����� �������� ���� ������ �� ���������� ����
  AddParam(
    'triggerDirection',
   '"' + Value.ToString + '"'
  );
end;

procedure TParamOrder.SetOrderFilter(const Value: TTypeOrderFilter);
begin
  //tpslOrder: �������� ����� TP/SL, ������ ������ ��� �� ������������ ������
  //StopOrder: ��� ���������� ��������� ������ ������ �� ����� ������
  //�� ��� ���, ���� ���� �������� ������ �� ��������� ���������� ����,
  //� ��������� ������ ����� ������ ����� ������������ ��������� ������
  AddParam(
    'orderFilter',
   '"' + GetStrToTypeOrderFilter(Value) + '"'
  );
end;

procedure TParamOrder.SetTriggerPrice(const Value: Double);
begin
  // ��� Perps & Futures ��� ���� ������� ��������� ������.
  // ���� �� ��������, ��� ���� ��������� ��� ������� ������ ��������� ������,
  // ���������, ���:
  // triggerPrice > market price ����� triggerPrice < market price
  AddParam(
    'triggerPrice',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetTriggerBy(const Value: TTypeTriggerBy);
begin
  // ��� ���������� ����, �������� ��������� ������ ��� Perps � ���������.
  // LastPrice, IndexPrice, MarkPrice
  AddParam(
    'triggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TParamOrder.SetOrderLv(const Value: String);
begin
  // ��������������� �������������. ������ �����. ������� ��������
  // ��������, ��������, ��� 10% ������ ���� �������� �������� 0,1.
  // ������� Iv ����� ����� ������� ���������, ����� ����� ���������� ����
  AddParam(
    'triggerBy',
   '"' + Value + '"'
  );
end;

procedure TParamOrder.SetTimeInForce(const Value: TTypeTimeInForce);
begin
  AddParam(
    'timeInForce',
   '"' + GetStrToTypeTimeInForce(Value) + '"'
  );
end;

procedure TParamOrder.SetPositionIdx(const Value: Integer);
begin
  // ������������ ��� ������������� ������� � ��������� �������
  // ����������������. � ������ ������������ ���� �������� ����������
  // (USDT perps � �������� ��������� ����� ����� ������������)
  //
  //  0: ������������� �����
  //  1: ������� ������� � ������ ������������
  //  2: ������� ������� � ������ ������������
  AddParam(
    'positionIdx',
   Value.ToString
  );
end;

procedure TParamOrder.SetOrderLinkId(const Value: String);
begin
  FOrderLinkID := Value;
  // ������������� ������, ������������� �������������.
  // �������� 36 ��������. �������������� ���������� ����,
  // ���� (��������� � ��������), ���� � �������������.
  AddParam(
    'orderLinkId',
   '"' + Value + '"'
  );
end;

procedure TParamOrder.SetTakeProfit(const Value: Double);
begin
  // ���� ����-�������, �������������� ��� ��������� � ���������
  AddParam(
    'takeProfit',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TParamOrder.SetStopLoss(const Value: Double);
begin
  // ���� ����-�����, �������������� ��� ��������� � ���������
  AddParam(
    'stopLoss',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetTP_TriggerBy(const Value: TTypeTriggerBy);
begin
  // ��� ���� ��� ������������ ����-�������.
  AddParam(
    'tpTriggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TParamOrder.SetSL_TriggerBy(const Value: TTypeTriggerBy);
begin
  // ��� ���� ��� ������������ ����-�����.
  AddParam(
    'slTriggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

function _BoolToStr(const AValue: Boolean): String;
begin
  if AValue then
    REsult := 'true'
  else
    Result := 'false';
end;

procedure TParamOrder.SetReduceOnly(const Value: Boolean);
var
  xValue: String;
begin
  // ����� ������ �� ����������
  xValue := _BoolToStr(Value);
  AddParam(
    'reduceOnly',
    xValue
  );
end;

procedure TParamOrder.SetCloseOnTrigger(const Value: Boolean);
var
  xValue: String;
begin
  // ������� ��� ������������
  xValue := _BoolToStr(Value);
  AddParam(
    'closeOnTrigger',
    xValue
  );
end;

procedure TParamOrder.SetSmpType(const Value: TTypeSmpType);
begin
  AddParam(
    'smpType',
    '"' + GetStrToTypeSmpType(Value) + '"'
  );
end;

procedure TParamOrder.SetMMP(const Value: Boolean);
var
  xValue: String;
begin
  // ������ ������-�������� (MMP) - ��� ������������������ ��������,
  // ��������������� ��� ������ ������-�������� (MM) �� ������ ����������� �
  // ����������� ����������� �� �����. ��� ������������� �������������
  // ���������� ������ �� ����������, ��������������� MM, � ������� ���������
  // ���������� �������. MM ����� ������������� ��������� ���� ���������, ����
  // ���������� ����������, ��������� �� �������� ������, ��������� �����������
  // ����� � ������� ������������� ������� �������. ����� ������� MMP ��� �����
  // ������������ ������ MMP ����� ������������� ��������, � ����� ������,
  // ���������� ��� MMP, ����� ��������� �� ������������ ���� ��������� ���
  // ������������ ������, ����� MM ����� ����������� ����� � �������� ���������.
  xValue := _BoolToStr(Value);
  AddParam(
    'mmp',
    xValue
  );
end;


procedure TParamOrder.SetTpSlMode(const Value: TTypeTpSlMode);
begin
  AddParam(
    'tpslMode',
    '"' + GetStrToTypeTpSlMode(Value) + '"'
  );
end;

procedure TParamOrder.SetTpLimitPrice(const Value: Double);
begin
  // ���� ��������� ������ ��� ������������ ���� ����-�������.
  // �������� ������ ��� tpslMode=Partial � tpOrderType=Limit.
  AddParam(
    'tpLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TParamOrder.SetSlLimitPrice(const Value: Double);
begin
  // ���� ��������� ������ ��� ������������ ���� ����-�������.
  // �������� ������ ��� tpslMode=Partial � tpOrderType=Limit.
  AddParam(
    'slLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetTpOrderType(const Value: Double);
begin
  // ��� ������ ��� ������������ ����-�������. Market(�� ���������), Limit.
  // ��� tpslMode=Full �������������� ������ tpOrderType=Market.
  AddParam(
    'tpOrderType',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetSpOrderType(const Value: Double);
begin
  // ��� ������ ��� ������������ ����-�����. Market(�� ���������), Limit.
  // ��� tpslMode=Full �������������� ������ slOrderType=Market.
  AddParam(
    'slOrderType',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

{ TBybitPlaceOrder }

constructor TBybitPlaceOrder.Create;
begin
  inherited;
  FServerTime  := TBybitServerTime.Create;
  FPlaceOrders := TParamOrderList.Create;
end;

destructor TBybitPlaceOrder.Destroy;
begin
  FreeAndNil(FPlaceOrders);
  FreeAndNil(FServerTime);
  inherited;
end;

procedure TBybitPlaceOrder.Clear;
begin
  FPlaceOrders.Clear;
end;

function TBybitPlaceOrder.AddPlaceOrder: TParamOrder;
var
  xPlaceOrder: TParamOrder;
begin
  xPlaceOrder := TParamOrder.Create;
  Result := xPlaceOrder;
  FPlaceOrders.Add(xPlaceOrder);
end;

{ TOrderResponse }

procedure TOrderResponse.SetParserValue(const AValue: String);
begin
  inherited SetParserValue (AValue);
  if Assigned(ResultObject) then
  begin
    FOrderID     := GetStrToJson(ResultObject.Values['orderId']);
    FOrderLinkID := GetStrToJson(ResultObject.Values['orderLinkId']);
  end;
end;

initialization

finalization

end.
