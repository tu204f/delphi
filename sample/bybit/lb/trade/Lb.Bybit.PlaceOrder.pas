(******************************************************************************
  Разместить заказ

  Эта конечная точка поддерживает создание ордера на спот, спотовую маржу,
  USDT perpetual, USDC perpetual, фьючерсы на USDC, обратные фьючерсы и опционы

  ##
  Поддерживаемый тип заказа (OrderType):
  Limit order: orderType=Limit, необходимо указать количество заказа и цену.

  ##
  Market order: orderType=Market, исполняйте по лучшей цене на рынке Bybit,
  пока транзакция не будет завершена. При выборе рыночного ордера цена пуста.
  В системе торговли фьючерсами, чтобы защитить рыночный ордер от
  серьезного проскальзывания, торговая система Bybit преобразует
  рыночный ордер в лимитный ордер для сопоставления. будет отменен.
  Порог проскальзывания относится к проценту отклонения цены ордера
  от последней цены транзакции. Текущий порог установлен на уровне 3%
  для контрактов BTC и 5% для других контрактов.

  ##
  Поддерживаемая стратегия timeInForce: GTC, IOC, FOK, PostOnly: Русский: Если
  заказ был бы заполнен сразу после отправки, он будет отменен.
  Цель этого - защитить ваш заказ в процессе отправки. Если система
  сопоставления не может передать заказ в книгу заказов из-за изменения цен
  на рынке, он будет отменен. Для типа заказа PostOnly количество, которое
  может быть отправлено в одном заказе, больше, чем для других типов заказов,
  пожалуйста, обратитесь к параметру lotSizeFilter > postOnlyMaxOrderQty в
  конечной точке instruments-info.

  ##
  Как создать условный заказ: При отправке заказа, если установлена
  триггерная цена, заказ будет автоматически преобразован в условный заказ.
  Кроме того, условный заказ не занимает маржу. Если маржа окажется
  недостаточной после срабатывания условного заказа, заказ будет отменен.

  ##
  Take profit / Stop loss: Вы можете установить TP/SL при размещении ордеров.
  Кроме того, вы можете изменить TP/SL позиции.

  ##
  Order quantity: Количество бессрочных контрактов, которые вы собираетесь
  купить/продать. Что касается количества заказа, Bybit в настоящее время
  поддерживает только положительное число.

  ##
  Order price: Разместите лимитный ордер, этот параметр обязателен. Если у вас
  есть позиция, цена должна быть выше ликвидационной цены. Для получения
  минимальной единицы изменения цены, пожалуйста, обратитесь к полю
  priceFilter > tickSize в конечной точке instruments-info.

  ##
  orderLinkId: Вы можете настроить идентификатор активного заказа. Мы можем
  связать этот идентификатор с идентификатором заказа в системе. Как только
  активный заказ будет успешно создан, мы отправим вам уникальный идентификатор
  заказа в системе. Затем вы можете использовать этот идентификатор заказа для
  отмены активных заказов, и если при вводе параметра введены как OrderID,
  так и orderLinkId, Bybit установит приоритет OrderID для обработки
  соответствующего заказа. Между тем, ваш индивидуальный идентификатор заказа
  не должен превышать 36 символов и должен быть уникальным.

  ##
  Open orders up limit: На каждом счете одновременно может храниться
  максимум 500 активных ордеров. Это зависит от конкретного контракта,
  поэтому допускается следующая ситуация: на одном и том же счете может
  храниться 300 активных ордеров BTC/USD и 280 активных ордеров ETHUSD
  одновременно. Для условных ордеров на каждом счете может храниться
  максимум 10 активных ордеров одновременно. При достижении верхнего предела
  количества ордеров вы по-прежнему можете размещать ордера с параметрами
  reduceOnly или closeOnTrigger. Спот: всего 500 ордеров, включая максимум
  30 открытых ордеров TP/SL, максимум 30 открытых условных ордеров
  Опция: максимум 50 открытых ордеров

  ##
  Rate limit: Пожалуйста, ознакомьтесь с таблицей лимитов тарифов. Если вам
  необходимо увеличить лимит тарифов, пожалуйста, свяжитесь со своим менеджером
  по работе с клиентами или отправьте заявку здесь

  ##
  Risk control limit notice: Bybit будет отслеживать ваши запросы к API.
  Когда общее количество заказов одного пользователя (агрегированное количество
  заказов по основному аккаунту и субсчетам) в течение дня (UTC 0 - UTC 24)
  превысит определенный верхний предел, платформа оставляет за собой
  право напоминать, предупреждать и налагать необходимые ограничения.
  Клиенты, использующие API, по умолчанию принимают настоящие условия и
  обязаны сотрудничать с корректировщиками.
(******************************************************************************)
unit Lb.Bybit.PlaceOrder;

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
  Lb.Bybit.Encryption;

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
    FPlaceOrders: TObjectPlaceOrderList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function AddPlaceOrder: TObjectPlaceOrder;
    property PlaceOrders: TObjectPlaceOrderList read FPlaceOrders;
  end;

implementation

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
      xValue := xValue + xParam.Key + ':' + xParam.Value + ', ';
    end;
    xParam := FParams[iCount - 1];
    xValue := xValue + xParam.Key + ':' + xParam.Value;
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
   '"' + Value.ToString + '"'
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
   '"' + Value.ToString + '"'
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
   '"' + Value.ToString + '"'
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
   '"' + Value.ToString + '"'
  );
end;


procedure TObjectPlaceOrder.SetStopLoss(const Value: Double);
begin
  // Цена стоп-лосса, действительная для линейного и обратного
  AddParam(
    'stopLoss',
   '"' + Value.ToString + '"'
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
    '"' + Value.ToString + '"'
  );
end;


procedure TObjectPlaceOrder.SetSlLimitPrice(const Value: Double);
begin
  // Цена лимитного ордера при срабатывании цены тейк-профита.
  // Работает только при tpslMode=Partial и tpOrderType=Limit.
  AddParam(
    'slLimitPrice',
   '"' + Value.ToString + '"'
  );
end;

procedure TObjectPlaceOrder.SetTpOrderType(const Value: Double);
begin
  // Тип ордера при срабатывании тейк-профита. Market(по умолчанию), Limit.
  // Для tpslMode=Full поддерживается только tpOrderType=Market.
  AddParam(
    'tpOrderType',
   '"' + Value.ToString + '"'
  );
end;

procedure TObjectPlaceOrder.SetSpOrderType(const Value: Double);
begin
  // Тип ордера при срабатывании стоп-лосса. Market(по умолчанию), Limit.
  // Для tpslMode=Full поддерживается только slOrderType=Market.
  AddParam(
    'slOrderType',
   '"' + Value.ToString + '"'
  );
end;

{ TBybitPlaceOrder }

constructor TBybitPlaceOrder.Create;
begin
  inherited;
  FPlaceOrders := TObjectPlaceOrderList.Create;
end;

destructor TBybitPlaceOrder.Destroy;
begin
  FreeAndNil(FPlaceOrders);
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
