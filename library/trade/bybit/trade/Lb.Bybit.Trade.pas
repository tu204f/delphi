(******************************************************************************)
(*  Реализуем процедуру управление заказами                                   *)
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
  ///<summary>Генерирует заявку на биржу</summary>
  TParamOrder = class(TObject)
  public type
    TTypeProc = (
      Place,  // Выставить заявку
      Amend,  // Изменить заявку
      Cancel  // Отменить заявку
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

  ///<summary>Список отправляемых заявок</summary>
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


  ///<summary>Ответ отливка на заявку</summary>
  TOrderResponse = class(TBytiyResponse)
  private
    FOrderID: String;
    FOrderLinkID: String;
  public
    procedure SetParserValue(const AValue: String); override;
    property OrderID: String read FOrderID;
    ///<summary>Пользовательский идентификатор заказа, настроенный пользователем</summary>
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
    raise Exception.Create('Error Message: Параметр объекта');

  xModule := TBybitModule.Create;
  xEncryption:= TEncryption.Create;
  xClientAPI := TBybitHttpClientAPI.Create;
  try
    // Шифрование запроса
    xEncryption.ApiKey    := ApiKey;
    xEncryption.ApiSecret := ApiSecret;
    xEncryption.Timestamp := GetNow.ToString;

    xValue := APlaceOrder.Value;
    xEncryption.QueryBody := xValue;
    xSignature := xEncryption.Signature;

    // Оформляем модуль запроса
    xModule.TypeHttp := TTypeHttp.thPost;
    xModule.Host := BYBIT_HOST;
    case APlaceOrder.TypeProc of
      Place : xModule.Module := '/v5/order/create';
      Amend : xModule.Module := '/v5/order/amend';
      Cancel: xModule.Module := '/v5/order/cancel';
    end;
    // Значение работы
    with xModule.Headers do
    begin
      Values['X-BAPI-API-KEY']     := xEncryption.ApiKey;
      Values['X-BAPI-SIGN']        := xSignature;
      Values['X-BAPI-SIGN-TYPE']   := '2';
      Values['X-BAPI-TIMESTAMP']   := xEncryption.Timestamp;
      Values['X-BAPI-RECV-WINDOW'] := xEncryption.RecvWindow;
    end;

    // Сам запрос
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
  // Параметр условного порядка.
  // Используется для определения ожидаемого направления условного порядка.
  // 1: срабатывает, когда рыночная цена повышается до триггерной цены
  // 2: срабатывает, когда рыночная цена падает до триггерной цены
  AddParam(
    'triggerDirection',
   '"' + Value.ToString + '"'
  );
end;

procedure TParamOrder.SetOrderFilter(const Value: TTypeOrderFilter);
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

procedure TParamOrder.SetTriggerPrice(const Value: Double);
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

procedure TParamOrder.SetTriggerBy(const Value: TTypeTriggerBy);
begin
  // Тип триггерной цены, параметр условного ордера для Perps и фьючерсов.
  // LastPrice, IndexPrice, MarkPrice
  AddParam(
    'triggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TParamOrder.SetOrderLv(const Value: String);
begin
  // Подразумеваемая волатильность. только опция. Укажите реальное
  // значение, например, для 10% должно быть передано значение 0,1.
  // порядок Iv имеет более высокий приоритет, когда также передается цена
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

procedure TParamOrder.SetOrderLinkId(const Value: String);
begin
  FOrderLinkID := Value;
  // Идентификатор заказа, настраиваемый пользователем.
  // Максимум 36 символов. Поддерживаются комбинации цифр,
  // букв (прописных и строчных), тире и подчеркиваний.
  AddParam(
    'orderLinkId',
   '"' + Value + '"'
  );
end;

procedure TParamOrder.SetTakeProfit(const Value: Double);
begin
  // Цена тейк-профита, действительная для линейного и обратного
  AddParam(
    'takeProfit',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TParamOrder.SetStopLoss(const Value: Double);
begin
  // Цена стоп-лосса, действительная для линейного и обратного
  AddParam(
    'stopLoss',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetTP_TriggerBy(const Value: TTypeTriggerBy);
begin
  // Тип цены для срабатывания тейк-профита.
  AddParam(
    'tpTriggerBy',
   '"' + GetStrToTypeTriggerBy(Value) + '"'
  );
end;

procedure TParamOrder.SetSL_TriggerBy(const Value: TTypeTriggerBy);
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

procedure TParamOrder.SetReduceOnly(const Value: Boolean);
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

procedure TParamOrder.SetCloseOnTrigger(const Value: Boolean);
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


procedure TParamOrder.SetTpSlMode(const Value: TTypeTpSlMode);
begin
  AddParam(
    'tpslMode',
    '"' + GetStrToTypeTpSlMode(Value) + '"'
  );
end;

procedure TParamOrder.SetTpLimitPrice(const Value: Double);
begin
  // Цена лимитного ордера при срабатывании цены тейк-профита.
  // Работает только при tpslMode=Partial и tpOrderType=Limit.
  AddParam(
    'tpLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;


procedure TParamOrder.SetSlLimitPrice(const Value: Double);
begin
  // Цена лимитного ордера при срабатывании цены тейк-профита.
  // Работает только при tpslMode=Partial и tpOrderType=Limit.
  AddParam(
    'slLimitPrice',
    '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetTpOrderType(const Value: Double);
begin
  // Тип ордера при срабатывании тейк-профита. Market(по умолчанию), Limit.
  // Для tpslMode=Full поддерживается только tpOrderType=Market.
  AddParam(
    'tpOrderType',
   '"' + GetDecimalSeparator(Value.ToString) + '"'
  );
end;

procedure TParamOrder.SetSpOrderType(const Value: Double);
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
