(******************************************************************************)
(*  ��������� ��������� ���������� ��������                                   *)
(******************************************************************************)
unit Lb.Bybit.Trade;

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
  Lb.Bybit.SysUtils,
  Lb.Bybit.Encryption,
  Lb.Bybit.ServerTime;

type
  ///<summary>���������� ������ �� �����</summary>
  TObjectPlaceOrder = class(TObject)
  public type
    TParam = TPair<String,String>;
    TParamList = TList<TParam>;
  private
    FParams: TParamList;
    procedure AddParam(const AKye, AValue: String);
    function GetValue: String;
  private
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
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Category: TTypeCategory write SetCategory;
    property Symbol: String write SetSymbol;
    property IsLeverage: Integer write SetIsLeverage;
    property Side: TTypeSide write SetSide;
    property OrderType: TTypeOrder write SetOrderType;
    property Qty: Double write SetQty;
    property MarketUnit: String write SetMarketUnit;
    property Price: Double write SetPrice;
    property TriggerDirection: Integer write SetTriggerDirection;
    property OrderFilter: TTypeOrderFilter write SetOrderFilter;
    property TriggerPrice: Double write SetTriggerPrice;
    property TriggerBy: TTypeTriggerBy write SetTriggerBy;
    property OrderLv: String write SetOrderLv;
    property TimeInForce: TTypeTimeInForce write SetTimeInForce;
    property PositionIdx: Integer write SetPositionIdx;
    property OrderLinkId: String write SetOrderLinkId;
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
  TObjectPlaceOrderList = TObjectList<TObjectPlaceOrder>;

  TBybitPlaceOrder = class(TBybitHttpClient)
  private
    FServerTime: TBybitServerTime;
    FPlaceOrders: TObjectPlaceOrderList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function AddPlaceOrder: TObjectPlaceOrder;
    property PlaceOrders: TObjectPlaceOrderList read FPlaceOrders;
  end;

implementation

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

{ TObjectPlaceOrder }

constructor TObjectPlaceOrder.Create;
begin
  FParams := TParamList.Create;
end;

destructor TObjectPlaceOrder.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TObjectPlaceOrder.AddParam(const AKye, AValue: String);
begin
  FParams.Add(TParam.Create(AKye,AValue));
end;

function TObjectPlaceOrder.GetValue: String;
var
  xValue: String;
  i, iCount: Integer;
  xParam: TObjectPlaceOrder.TParam;
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

procedure TObjectPlaceOrder.SetCategory(const Value: TTypeCategory);
begin
  AddParam(
    'category',
    '"' + GetStrToTypeCategory(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetSymbol(const Value: String);
begin
  AddParam(
    'symbol',
    '"' + Value + '"'
  );
end;

procedure TObjectPlaceOrder.SetIsLeverage(const Value: Integer);
begin
  AddParam(
    'isLeverage',
    Value.ToString
  );
end;

procedure TObjectPlaceOrder.SetSide(const Value: TTypeSide);
begin
  AddParam(
    'side',
    '"' + GetStrToTypeSide(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetOrderType(const Value: TTypeOrder);
begin
  AddParam(
    'orderType',
   '"' + GetStrToTypeOrder(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetQty(const Value: Double);
begin
  AddParam(
    'qty',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TObjectPlaceOrder.SetMarketUnit(const Value: String);
begin
  AddParam(
    'marketUnit',
   '"' + Value + '"'
  );
end;

procedure TObjectPlaceOrder.SetPrice(const Value: Double);
begin
  AddParam(
    'price',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTriggerDirection(const Value: Integer);
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

procedure TObjectPlaceOrder.SetOrderFilter(const Value: TTypeOrderFilter);
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

procedure TObjectPlaceOrder.SetTriggerPrice(const Value: Double);
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

procedure TObjectPlaceOrder.SetTriggerBy(const Value: TTypeTriggerBy);
begin
  // ��� ���������� ����, �������� ��������� ������ ��� Perps � ���������.
  // LastPrice, IndexPrice, MarkPrice
  AddParam(
    'triggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetOrderLv(const Value: String);
begin
  // ��������������� �������������. ������ �����. ������� ��������
  // ��������, ��������, ��� 10% ������ ���� �������� �������� 0,1.
  // ������� Iv ����� ����� ������� ���������, ����� ����� ���������� ����
  AddParam(
    'triggerBy',
   '"' + Value + '"'
  );
end;

procedure TObjectPlaceOrder.SetTimeInForce(const Value: TTypeTimeInForce);
begin
  AddParam(
    'timeInForce',
   '"' + GetStrToTypeTimeInForce(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetPositionIdx(const Value: Integer);
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

procedure TObjectPlaceOrder.SetOrderLinkId(const Value: String);
begin
  // ������������� ������, ������������� �������������.
  // �������� 36 ��������. �������������� ���������� ����,
  // ���� (��������� � ��������), ���� � �������������.
  AddParam(
    'orderLinkId',
   '"' + Value + '"'
  );
end;

procedure TObjectPlaceOrder.SetTakeProfit(const Value: Double);
begin
  // ���� ����-�������, �������������� ��� ��������� � ���������
  AddParam(
    'takeProfit',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TObjectPlaceOrder.SetStopLoss(const Value: Double);
begin
  // ���� ����-�����, �������������� ��� ��������� � ���������
  AddParam(
    'stopLoss',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTP_TriggerBy(const Value: TTypeTriggerBy);
begin
  // ��� ���� ��� ������������ ����-�������.
  AddParam(
    'tpTriggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetSL_TriggerBy(const Value: TTypeTriggerBy);
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

procedure TObjectPlaceOrder.SetReduceOnly(const Value: Boolean);
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

procedure TObjectPlaceOrder.SetCloseOnTrigger(const Value: Boolean);
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

procedure TObjectPlaceOrder.SetSmpType(const Value: TTypeSmpType);
begin
  AddParam(
    'smpType',
    '"' + GetStrToTypeSmpType(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetMMP(const Value: Boolean);
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


procedure TObjectPlaceOrder.SetTpSlMode(const Value: TTypeTpSlMode);
begin
  AddParam(
    'tpslMode',
    '"' + GetStrToTypeTpSlMode(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTpLimitPrice(const Value: Double);
begin
  // ���� ��������� ������ ��� ������������ ���� ����-�������.
  // �������� ������ ��� tpslMode=Partial � tpOrderType=Limit.
  AddParam(
    'tpLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TObjectPlaceOrder.SetSlLimitPrice(const Value: Double);
begin
  // ���� ��������� ������ ��� ������������ ���� ����-�������.
  // �������� ������ ��� tpslMode=Partial � tpOrderType=Limit.
  AddParam(
    'slLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTpOrderType(const Value: Double);
begin
  // ��� ������ ��� ������������ ����-�������. Market(�� ���������), Limit.
  // ��� tpslMode=Full �������������� ������ tpOrderType=Market.
  AddParam(
    'tpOrderType',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetSpOrderType(const Value: Double);
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
  FPlaceOrders := TObjectPlaceOrderList.Create;
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

function TBybitPlaceOrder.AddPlaceOrder: TObjectPlaceOrder;
var
  xPlaceOrder: TObjectPlaceOrder;
begin
  xPlaceOrder := TObjectPlaceOrder.Create;
  Result := xPlaceOrder;
  FPlaceOrders.Add(xPlaceOrder);
end;

end.
