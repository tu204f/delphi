(******************************************************************************)
(* Получить информацию об инструментах                                        *)
(* Запрос спецификации инструмента для торговых пар онлайн.                   *)
(* Covers: Spot / USDT perpetual / USDC contract / Inverse contract / Option  *)
(* ОСТОРОЖНОСТЬ                                                               *)
(* Spot не поддерживает нумерацию страниц, поэтому limitявляются cursor       *)
(* недействительными.                                                         *)
(* При запросе по baseCoin, независимо от категории = linearили inverse,      *)
(* результат будет иметь символы                                              *)
(* USDT perpetual, USDC contract and Inverse contract symbols.                *)
(******************************************************************************)
unit Lb.Bybit.InstrumentsInfo;

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
  ///<summary>Get Instruments Info</summary>
  TBybitInstrumentsInfo = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FStatus: TTypeStatus;
    FbaseCoin: String;
    FLimit: Integer;
    FCursor: String;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetStatus(const Value: TTypeStatus);
    procedure SetBaseCoin(const Value: String);
    procedure SetLimit(const Value: Integer);
    procedure SetCursor(const Value: String);
  protected
    FListJson: TJSONArray;
    procedure DoEventMessage(const AMessage: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public {Request Parameters}
    ///<summary>Тип продукта. spot, linear, inverse, option</summary>
    property Category: TTypeCategory read FCategory write SetCategory;
    ///<summary>Имя символа</summary>
    property Symbol: String read FSymbol write SetSymbol;
    ///<summary>Фильтр статуса символа</summary>
    ///<remarks>spot/linear/inverse has Trading only</remarks>
    property Status: TTypeStatus read FStatus write SetStatus;
    ///<summary>Базовая монета. linear, inverse, option только</summary>
    ///<remarks>В качестве опции он возвращает BTC по умолчанию.</remarks>
    property BaseCoin: String read FBaseCoin write SetBaseCoin;
    ///<summary>Limit for data size per page. [1, 1000]. Default: 500</summary>
    property Limit: Integer read FLimit write SetLimit;
    property Cursor: String read FCursor write SetCursor;
  end;

type
  ///<summary>Linear/Inverse</summary>
  TLinearObject = class(TCustonObjectJson)
  public type

    TLeverageFilter = class(TCustonObjectJson)
      ///<summary>Минимальное кредитное плечо</summary>
      MinLeverage: String;  // "1",
      ///<summary>Максимальное кредитное плечо</summary>
      MaxLeverage: String;  // "100.00",
      ///<summary>Шаг к увеличению/уменьшению кредитного плеча</summary>
      LeverageStep: String; // "0.01"
    public
      procedure SetObjectJson(const AObjectJson: TJSONObject); override;
    end;

    TPriceFilter = class(TCustonObjectJson)
      ///<summary>Минимальная цена заказа</summary>
      MinPrice: String; // "0.50",
      ///<summary>Максимальная цена заказа</summary>
      MaxPrice: String; // "999999.00",
      ///<summary>Шаг для увеличения/уменьшения цены заказа</summary>
      TickSize: String; // "0.50"
    public
      procedure SetObjectJson(const AObjectJson: TJSONObject); override;
    end;

    TLotSizeFilter = class(TCustonObjectJson)
      ///<summary>Максимальное количество заказа</summary>
      MaxOrderQty: String; // "100.000",
      ///<summary>Минимальное количество заказа</summary>
      MinOrderQty: String; // "0.001",
      ///<summary>Шаг для увеличения/уменьшения количества заказа</summary>
      QtyStep: String;     // "0.001",
      ///<summary>Максимальное количество заказов только для заказа по почте</summary>
      PostOnlyMaxOrderQty: String; //"1000.000"
    public
      procedure SetObjectJson(const AObjectJson: TJSONObject); override;
    end;

  public
    ///<summary>Название символа</summary>
    Symbol: String;
    ///<summary>Тип контракты</summary>
    ContractType: String;
    ///<summary>Статус прибора</summary>
    Status: String;
    ///<summary>Базовая монета</summary>
    BaseCoin: String;
    ///<summary>Котируем монету</summary>
    QuoteCoin: String;
    ///<summary>Временная метка запуска (мс)</summary>
    LaunchTime: String;
    ///<summary>Отметка времени доставки (мс)</summary>
    DeliveryTime: String;
    ///<summary>Ставка платы за доставку</summary>
    DeliveryFeeRate: String;
    ///<summary>Шкала цен</summary>
    PriceScale: String;
    ///<summary>Атрибуты кредитного плеча</summary>
    LeverageFilter: TLeverageFilter;
    ///<summary>Ценовые атрибуты</summary>
    PriceFilter: TPriceFilter;
    ///<summary>Атрибуты размера</summary>
    LotSizeFilter: TLotSizeFilter;
    ///<summary>Следует ли поддерживать единую маржинальную торговлю</summary>
    UnifiedMarginTrade: String; // Boolean;
    ///<summary>Интервал финансирования (минута)</summary>
    FundingInterval: String; // Integer;
    ///<summary>Разменная монета</summary>
    SettleCoin: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); override;
  end;

  ///<summary>Список объект</summary>
  TLinearObjectList = TObjectList<TLinearObject>;

procedure SetLinearObjects(AListJson: TJSONArray; ALinearObjects: TLinearObjectList);

implementation

procedure SetLinearObjects(AListJson: TJSONArray; ALinearObjects: TLinearObjectList);
var
  i, iCount: Integer;
  //xListJson: TJSONArray;
  xObjectJson: TJSONObject;
  xLinearObject: TLinearObject;
begin
  if not Assigned(ALinearObjects) then
    Exit;

  ALinearObjects.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xLinearObject := TLinearObject.Create;
      xLinearObject.SetObjectJson(xObjectJson);
      ALinearObjects.Add(xLinearObject);
    end;
end;

{ TBybitInstrumentsInfo }

constructor TBybitInstrumentsInfo.Create;
begin
  inherited;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/market/instruments-info';
  with BybitModule.Params do
  begin
    // https://bybit-exchange.github.io/docs/v5/enum#locale
    SetParam('locale','ru-RU');
    // https://bybit-exchange.github.io/docs/v5/enum#announcementtype
    //SetParam('type','new_crypto');
    // https://bybit-exchange.github.io/docs/v5/enum#announcementtag
    //SetParam('tag','Spot');
    // page
    // limit
  end;
end;

destructor TBybitInstrumentsInfo.Destroy;
begin
  inherited;
end;

procedure TBybitInstrumentsInfo.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory))
end;


procedure TBybitInstrumentsInfo.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitInstrumentsInfo.SetStatus(const Value: TTypeStatus);
begin
  FStatus := Value;
  BybitModule.Params.SetParam('status',GetStrToTypeStatus(Value));
end;

procedure TBybitInstrumentsInfo.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitInstrumentsInfo.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitInstrumentsInfo.SetCursor(const Value: String);
begin
  FCursor := Value;
  BybitModule.Params.SetParam('cursor',FCursor);
end;

procedure TBybitInstrumentsInfo.DoEventMessage(const AMessage: String);
var
  xValueJson: TJSONValue;
begin
  inherited DoEventMessage(AMessage);
  (*
    "category": "",
    "list": [],
    "nextPageCursor": ""
  *)
  xValueJson := Response.ResultObject.Values['list'];
  if xValueJson is TJSONArray then
    FListJson := TJSONArray(xValueJson);
end;


{ TLinearObject.TLeverageFilter }

procedure TLinearObject.TLeverageFilter.SetObjectJson(const AObjectJson: TJSONObject);
begin
  inherited;
  (*
                "leverageFilter": {
                    "minLeverage": "1",
                    "maxLeverage": "100.00",
                    "leverageStep": "0.01"
                },
  *)
  MinLeverage  := AObjectJson.Values['minLeverage'].Value;  // String;  // "1",
  MaxLeverage  := AObjectJson.Values['maxLeverage'].Value;  // String;  // "100.00",
  LeverageStep := AObjectJson.Values['leverageStep'].Value; // String; // "0.01"
end;

{ TLinearObject.TPriceFilter }

procedure TLinearObject.TPriceFilter.SetObjectJson(const AObjectJson: TJSONObject);
begin
  inherited;
(*
                "priceFilter": {
                    "minPrice": "0.50",
                    "maxPrice": "999999.00",
                    "tickSize": "0.50"
                },
*)
  MinPrice := AObjectJson.Values['minPrice'].Value;  // String; // "0.50",
  MaxPrice := AObjectJson.Values['maxPrice'].Value;  // String; // "999999.00",
  TickSize := AObjectJson.Values['tickSize'].Value;  // String; // "0.50"
end;

{ TLinearObject.TLotSizeFilter }

procedure TLinearObject.TLotSizeFilter.SetObjectJson(const AObjectJson: TJSONObject);
begin
  inherited;
(*
                "lotSizeFilter": {
                    "maxOrderQty": "100.000",
                    "minOrderQty": "0.001",
                    "qtyStep": "0.001",
                    "postOnlyMaxOrderQty": "1000.000"
                },
*)
  MaxOrderQty         := AObjectJson.Values['maxOrderQty'].Value; // "100.000",
  MinOrderQty         := AObjectJson.Values['minOrderQty'].Value; // "0.001",
  QtyStep             := AObjectJson.Values['qtyStep'].Value;     // "0.001",
  PostOnlyMaxOrderQty := AObjectJson.Values['postOnlyMaxOrderQty'].Value; //"1000.000"
end;

{ TLinearObject }

constructor TLinearObject.Create;
begin
  inherited;
  LeverageFilter := TLeverageFilter.Create;
  PriceFilter    := TPriceFilter.Create;
  LotSizeFilter  := TLotSizeFilter.Create;
end;

destructor TLinearObject.Destroy;
begin
  FreeAndNil(LotSizeFilter);
  FreeAndNil(PriceFilter);
  FreeAndNil(LotSizeFilter);
  inherited;
end;

procedure TLinearObject.SetObjectJson(const AObjectJson: TJSONObject);
begin
  inherited;
  // -----------------------------------------------------------------
  Symbol          := AObjectJson.Values['symbol'].Value;
  ContractType    := AObjectJson.Values['contractType'].Value;
  Status          := AObjectJson.Values['status'].Value;
  BaseCoin        := AObjectJson.Values['baseCoin'].Value;
  QuoteCoin       := AObjectJson.Values['quoteCoin'].Value;
  LaunchTime      := AObjectJson.Values['launchTime'].Value;
  DeliveryTime    := AObjectJson.Values['deliveryTime'].Value;
  DeliveryFeeRate := AObjectJson.Values['deliveryFeeRate'].Value;
  PriceScale      := AObjectJson.Values['priceScale'].Value;
  // -----------------------------------------------------------------
  LeverageFilter.SetObjectJson(TJSONObject(AObjectJson.Values['leverageFilter']));
  PriceFilter.SetObjectJson(TJSONObject(AObjectJson.Values['priceFilter']));
  LotSizeFilter.SetObjectJson(TJSONObject(AObjectJson.Values['lotSizeFilter']));
  // -----------------------------------------------------------------
  UnifiedMarginTrade := AObjectJson.Values['unifiedMarginTrade'].Value;
  FundingInterval    := AObjectJson.Values['fundingInterval'].Value;
  SettleCoin         := AObjectJson.Values['settleCoin'].Value;
end;

end.
