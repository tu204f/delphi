(******************************************************************************)
(*  Реализуем процедуру управление заказами                                   *)
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
  ///<summary>Генерирует заявку на биржу</summary>
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

  ///<summary>Список отправляемых заявок</summary>
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
  // Параметр условного порядка.
  // Используется для определения ожидаемого направления условного порядка.
  // 1: срабатывает, когда рыночная цена повышается до триггерной цены
  // 2: срабатывает, когда рыночная цена падает до триггерной цены
  AddParam(
    'triggerDirection',
   '"' + Value.ToString + '"'
  );
end;

procedure TObjectPlaceOrder.SetOrderFilter(const Value: TTypeOrderFilter);
begin
  //tpslOrder: Спотовый ордер TP/SL, активы заняты еще до срабатывания ордера
  //StopOrder: При размещении условного ордера активы не будут заняты
  //до тех пор, пока цена базового актива не достигнет триггерной цены,
  //а требуемые активы будут заняты после срабатывания условного ордера
  AddParam(
    'orderFilter',
   '"' + GetStrToTypeOrderFilter(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTriggerPrice(const Value: Double);
begin
  // Для Perps & Futures это цена запуска условного ордера.
  // Если вы ожидаете, что цена повысится для запуска вашего условного ордера,
  // убедитесь, что:
  // triggerPrice > market price иначе triggerPrice < market price
  AddParam(
    'triggerPrice',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTriggerBy(const Value: TTypeTriggerBy);
begin
  // Тип триггерной цены, параметр условного ордера для Perps и фьючерсов.
  // LastPrice, IndexPrice, MarkPrice
  AddParam(
    'triggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetOrderLv(const Value: String);
begin
  // Подразумеваемая волатильность. только опция. Укажите реальное
  // значение, например, для 10% должно быть передано значение 0,1.
  // порядок Iv имеет более высокий приоритет, когда также передается цена
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
  // Используется для идентификации позиций в различных режимах
  // позиционирования. В режиме хеджирования этот параметр обязателен
  // (USDT perps и обратные контракты имеют режим хеджирования)
  //
  //  0: односторонний режим
  //  1: сторона покупки в режиме хеджирования
  //  2: сторона продажи в режиме хеджирования
  AddParam(
    'positionIdx',
   Value.ToString
  );
end;

procedure TObjectPlaceOrder.SetOrderLinkId(const Value: String);
begin
  // Идентификатор заказа, настраиваемый пользователем.
  // Максимум 36 символов. Поддерживаются комбинации цифр,
  // букв (прописных и строчных), тире и подчеркиваний.
  AddParam(
    'orderLinkId',
   '"' + Value + '"'
  );
end;

procedure TObjectPlaceOrder.SetTakeProfit(const Value: Double);
begin
  // Цена тейк-профита, действительная для линейного и обратного
  AddParam(
    'takeProfit',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TObjectPlaceOrder.SetStopLoss(const Value: Double);
begin
  // Цена стоп-лосса, действительная для линейного и обратного
  AddParam(
    'stopLoss',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTP_TriggerBy(const Value: TTypeTriggerBy);
begin
  // Тип цены для срабатывания тейк-профита.
  AddParam(
    'tpTriggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TObjectPlaceOrder.SetSL_TriggerBy(const Value: TTypeTriggerBy);
begin
  // Тип цены для срабатывания стоп-лосса.
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
  // Заказ только на сокращение
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
  // Закрыть при срабатывании
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
  // Защита маркет-мейкеров (MMP) - это автоматизированный механизм,
  // предназначенный для защиты маркет-мейкеров (MM) от рисков ликвидности и
  // чрезмерного воздействия на рынке. Это предотвращает одновременное
  // выполнение сделок по котировкам, предоставляемым MM, в течение короткого
  // промежутка времени. MM может автоматически извлекать свои котировки, если
  // количество контрактов, торгуемых по базовому активу, превышает настроенный
  // порог в течение определенного периода времени. После запуска MMP все ранее
  // существующие ордера MMP будут автоматически отменены, а новые ордера,
  // помеченные как MMP, будут отклонены на определенный срок— известный как
  // замороженный период, чтобы MM могла переоценить рынок и изменить котировки.
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
  // Цена лимитного ордера при срабатывании цены тейк-профита.
  // Работает только при tpslMode=Partial и tpOrderType=Limit.
  AddParam(
    'tpLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TObjectPlaceOrder.SetSlLimitPrice(const Value: Double);
begin
  // Цена лимитного ордера при срабатывании цены тейк-профита.
  // Работает только при tpslMode=Partial и tpOrderType=Limit.
  AddParam(
    'slLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetTpOrderType(const Value: Double);
begin
  // Тип ордера при срабатывании тейк-профита. Market(по умолчанию), Limit.
  // Для tpslMode=Full поддерживается только tpOrderType=Market.
  AddParam(
    'tpOrderType',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TObjectPlaceOrder.SetSpOrderType(const Value: Double);
begin
  // Тип ордера при срабатывании стоп-лосса. Market(по умолчанию), Limit.
  // Для tpslMode=Full поддерживается только slOrderType=Market.
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
