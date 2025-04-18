(******************************************************************************)
(* Запросите последнюю информацию о цене, наилучшую цену предложения/аск и    *)
(* объем торгов за последние 24 часа.                                         *)
(******************************************************************************)
unit Lb.Bybit.Tickers;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.JSON,
  Lb.Bybit.SysUtils;

type
  TTickerValue = record
    ///<summary>Название символа</summary>
    symbol: String;
    ///<summary>Последняя цена</summary>
    lastPrice: Double;
    ///<summary>Индексная цена</summary>
    indexPrice: Double;
    ///<summary>Начальная цена</summary>
    markPrice: Double;
    ///<summary>Рыночная цена 24 часа назад</summary>
    prevPrice24h: Double;
    ///<summary>Процентное изменение рыночной цены по сравнению с 24-часовым периодом</summary>
    price24hPcnt: Double;
    ///<summary>Самая высокая цена за последние 24 часа</summary>
    highPrice24h: Double;
    ///<summary>Самая низкая цена за последние 24 часа</summary>
    lowPrice24h: Double;
    ///<summary>Рыночная цена час назад</summary>
    prevPrice1h: Double;
    ///<summary>Размер открытой процентной ставки</summary>
    openInterest: Double;
    ///<summary>Величина открытых процентов</summary>
    openInterestValue: Double;
    ///<summary>Оборот за 24 часа</summary>
    turnover24h: Double;
    ///<summary>Объем в течение 24 часов</summary>
    volume24h: Double;
    ///<sumamry>Ставка финансирования</summary>
    fundingRate: Double;
    ///<summary>Время следующего пополнения (мс)</summary>
    nextFundingTime: Int64;
    ///<summary>Прогнозируемая стоимость доставки. Она будет указана за 30 минут до доставки</summary>
    predictedDeliveryPrice: Double;
    ///<sumamry>Базисная ставка</summary>
    basisRate: Double;

    ///<summary>Ставка платы за доставку</summary>
    deliveryFeeRate: Double;
    ///<summary>Временная метка поставки (мс), применимая только к фьючерсам с истекшим сроком годности</summary>
    deliveryTime: Int64;
    ///<summary>Лучший размер запроса</summary>
    ask1Size: Double;
    ///<summary>Лучшая цена предложения</summary>
    bid1Price: Double;
    ///<summary>Лучшая запрашиваемая цена</summary>
    ask1Price: Double;
    ///<summary>Наилучший размер ставки</summary>
    bid1Size: Double;

    ///<summary>Основа</summary>
    basis: Double;

    ///<summary>Предполагаемая цена открытия предрыночного контракта</summary>
    preOpenPrice: Double;
    ///<summary>Предполагаемое количество открытых позиций по предпродажному контракту</summary>
    preQty: Double;
    ///<summary>Текущая стадия подготовки контракта к продаже</summary>
    curPreListingPhase: String;
  end;

  TTickerValueList = class(TList<TTickerValue>)
  public
    function IndexOfSymbol(ASymbol: String): Integer;
  end;

  ///<summary>Получаем котировальный стакан - bybit </summary>
  TBybitTickers = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FBaseCoin: String;
    FExpDate: String;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetBaseCoin(const Value: String);
    procedure SetExpDate(const Value: String);
  protected
    FTickerValues: TTickerValueList;
    procedure DoEventParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public {Request Parameters}
    ///<summary>Тип продукта. spot, linear, inverse, option</summary>
    property Category: TTypeCategory read FCategory write SetCategory;
    ///<summary>Имя символа</summary>
    property Symbol: String read FSymbol write SetSymbol;
    property BaseCoin: String read FBaseCoin write SetBaseCoin;
    property ExpDate: String read FExpDate write SetExpDate;
  public
    property TickerValues: TTickerValueList read FTickerValues;
  end;

implementation

type
  TTickerField = record
    ///<summary>Название символа</summary>
    symbol: String;
    ///<summary>Последняя цена</summary>
    lastPrice: String;
    ///<summary>Индексная цена</summary>
    indexPrice: String;
    ///<summary>Начальная цена</summary>
    markPrice: String;
    ///<summary>Рыночная цена 24 часа назад</summary>
    prevPrice24h: String;
    ///<summary>Процентное изменение рыночной цены по сравнению с 24-часовым периодом</summary>
    price24hPcnt: String;
    ///<summary>Самая высокая цена за последние 24 часа</summary>
    highPrice24h: String;
    ///<summary>Самая низкая цена за последние 24 часа</summary>
    lowPrice24h: String;
    ///<summary>Рыночная цена час назад</summary>
    prevPrice1h: String;
    ///<summary>Размер открытой процентной ставки</summary>
    openInterest: String;
    ///<summary>Величина открытых процентов</summary>
    openInterestValue: String;
    ///<summary>Оборот за 24 часа</summary>
    turnover24h: String;
    ///<summary>Объем в течение 24 часов</summary>
    volume24h: String;
    ///<sumamry>Ставка финансирования</summary>
    fundingRate: String;
    ///<summary>Время следующего пополнения (мс)</summary>
    nextFundingTime: String;
    ///<summary>Прогнозируемая стоимость доставки. Она будет указана за 30 минут до доставки</summary>
    predictedDeliveryPrice: String;
    ///<sumamry>Базисная ставка</summary>
    basisRate: String;

    ///<summary>Ставка платы за доставку</summary>
    deliveryFeeRate: String;
    ///<summary>Временная метка поставки (мс), применимая только к фьючерсам с истекшим сроком годности</summary>
    deliveryTime: String;
    ///<summary>Лучший размер запроса</summary>
    ask1Size: String;
    ///<summary>Лучшая цена предложения</summary>
    bid1Price: String;
    ///<summary>Лучшая запрашиваемая цена</summary>
    ask1Price: String;
    ///<summary>Наилучший размер ставки</summary>
    bid1Size: String;

    ///<summary>Основа</summary>
    basis: String;

    ///<summary>Предполагаемая цена открытия предрыночного контракта</summary>
    preOpenPrice: String;
    ///<summary>Предполагаемое количество открытых позиций по предпродажному контракту</summary>
    preQty: String;
    ///<summary>Текущая стадия подготовки контракта к продаже</summary>
    curPreListingPhase: String;
  public
    procedure SetParser(const AObjectJson: TJSONObject);
  end;


function Comparison(const Left, Right: TTickerValue): Integer;
begin
  if Left.turnover24h > Right.turnover24h then
    Result := -1
  else if Left.turnover24h < Right.turnover24h then
    Result := 1
  else
    Result := 0;
end;

procedure SetLinearObjects(AListJson: TJSONArray; ATickerValues: TTickerValueList);
var
  i, iCount: Integer;
  xObjectJson: TJSONObject;
  xTickerField: TTickerField;
  xTickerValue: TTickerValue;
  xF: TFormatSettings;
var
  xComparison: TComparison<TTickerValue>;
begin
  if not Assigned(AListJson) then
  begin
    raise Exception.Create('Error Message: Объект JSON – не определен');
    Exit;
  end;

  if not Assigned(ATickerValues) then
  begin
    Exit;
  end;


  xF := FormatSettings;
  xF.DecimalSeparator := '.';

  ATickerValues.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xTickerField.SetParser(xObjectJson);

      xTickerValue.symbol                 := xTickerField.symbol;
      xTickerValue.lastPrice              := StrToFloatDef(xTickerField.lastPrice,0,xF);
      xTickerValue.indexPrice             := StrToFloatDef(xTickerField.indexPrice,0,xF);
      xTickerValue.markPrice              := StrToFloatDef(xTickerField.markPrice,0,xF);
      xTickerValue.prevPrice24h           := StrToFloatDef(xTickerField.prevPrice24h,0,xF);
      xTickerValue.price24hPcnt           := StrToFloatDef(xTickerField.price24hPcnt,0,xF);
      xTickerValue.highPrice24h           := StrToFloatDef(xTickerField.highPrice24h,0,xF);
      xTickerValue.lowPrice24h            := StrToFloatDef(xTickerField.lowPrice24h,0,xF);
      xTickerValue.prevPrice1h            := StrToFloatDef(xTickerField.prevPrice1h,0,xF);
      xTickerValue.openInterest           := StrToFloatDef(xTickerField.openInterest,0,xF);
      xTickerValue.openInterestValue      := StrToFloatDef(xTickerField.openInterestValue,0,xF);
      xTickerValue.turnover24h            := StrToFloatDef(xTickerField.turnover24h,0,xF);
      xTickerValue.volume24h              := StrToFloatDef(xTickerField.volume24h,0,xF);
      xTickerValue.fundingRate            := StrToFloatDef(xTickerField.fundingRate,0,xF);
      xTickerValue.nextFundingTime        := StrToInt64Def(xTickerField.nextFundingTime,0);
      xTickerValue.predictedDeliveryPrice := StrToFloatDef(xTickerField.predictedDeliveryPrice,0,xF);
      xTickerValue.basisRate              := StrToFloatDef(xTickerField.basisRate,0,xF);
      xTickerValue.deliveryFeeRate        := StrToFloatDef(xTickerField.deliveryFeeRate,0,xF);
      xTickerValue.deliveryTime           := StrToInt64Def(xTickerField.deliveryTime,0);
      xTickerValue.ask1Size               := StrToFloatDef(xTickerField.ask1Size,0,xF);
      xTickerValue.bid1Price              := StrToFloatDef(xTickerField.bid1Price,0,xF);
      xTickerValue.ask1Price              := StrToFloatDef(xTickerField.ask1Price,0,xF);
      xTickerValue.bid1Size               := StrToFloatDef(xTickerField.bid1Size,0,xF);
      xTickerValue.basis                  := StrToFloatDef(xTickerField.basis,0,xF);
      xTickerValue.preOpenPrice           := StrToFloatDef(xTickerField.preOpenPrice,0,xF);
      xTickerValue.preQty                 := StrToFloatDef(xTickerField.preQty,0,xF);
      xTickerValue.curPreListingPhase     := xTickerField.curPreListingPhase;


      ATickerValues.Add(xTickerValue);
    end;

  //ATickerValues.Sort(TComparer<TTickerValue>.Construct(Comparison));
end;

{ TTickerValueList }

function TTickerValueList.IndexOfSymbol(ASymbol: String): Integer;
var
  i: Integer;
  xTickerValue: TTickerValue;
begin
  Result := -1;
  for i := 0 to Self.Count - 1 do
  begin
    xTickerValue := Self.Items[i];
    if SameText(xTickerValue.symbol,ASymbol) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{ TBybitTickers }

constructor TBybitTickers.Create;
begin
  inherited;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/market/tickers';
  FTickerValues := TTickerValueList.Create;
end;

destructor TBybitTickers.Destroy;
begin
  FreeAndNil(FTickerValues);
  inherited;
end;

procedure TBybitTickers.DoEventParser;
var
  xValueJson: TJSONValue;
  xListJson: TJSONArray;
begin
  xValueJson := Response.ResultObject.Values['list'];
  if xValueJson is TJSONArray then
  begin
    xListJson := TJSONArray(xValueJson);
    SetLinearObjects(xListJson,FTickerValues);
  end;
end;

procedure TBybitTickers.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory));
end;

procedure TBybitTickers.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitTickers.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitTickers.SetExpDate(const Value: String);
begin
  FExpDate := Value;
  BybitModule.Params.SetParam('expDate',FExpDate);
end;

{ TTickerField }

procedure TTickerField.SetParser(const AObjectJson: TJSONObject);
begin
  symbol                 := GetStrToJson(AObjectJson.Values['symbol']);
  lastPrice              := GetStrToJson(AObjectJson.Values['lastPrice']);
  indexPrice             := GetStrToJson(AObjectJson.Values['indexPrice']);
  markPrice              := GetStrToJson(AObjectJson.Values['markPrice']);
  prevPrice24h           := GetStrToJson(AObjectJson.Values['prevPrice24h']);
  price24hPcnt           := GetStrToJson(AObjectJson.Values['price24hPcnt']);
  highPrice24h           := GetStrToJson(AObjectJson.Values['highPrice24h']);
  lowPrice24h            := GetStrToJson(AObjectJson.Values['lowPrice24h']);
  prevPrice1h            := GetStrToJson(AObjectJson.Values['prevPrice1h']);
  openInterest           := GetStrToJson(AObjectJson.Values['openInterest']);
  openInterestValue      := GetStrToJson(AObjectJson.Values['openInterestValue']);
  turnover24h            := GetStrToJson(AObjectJson.Values['turnover24h']);
  volume24h              := GetStrToJson(AObjectJson.Values['volume24h']);
  fundingRate            := GetStrToJson(AObjectJson.Values['fundingRate']);
  nextFundingTime        := GetStrToJson(AObjectJson.Values['nextFundingTime']);
  predictedDeliveryPrice := GetStrToJson(AObjectJson.Values['predictedDeliveryPrice']);
  basisRate              := GetStrToJson(AObjectJson.Values['basisRate']);
  deliveryFeeRate        := GetStrToJson(AObjectJson.Values['deliveryFeeRate']);
  deliveryTime           := GetStrToJson(AObjectJson.Values['deliveryTime']);
  ask1Size               := GetStrToJson(AObjectJson.Values['ask1Size']);
  bid1Price              := GetStrToJson(AObjectJson.Values['bid1Price']);
  ask1Price              := GetStrToJson(AObjectJson.Values['ask1Price']);
  bid1Size               := GetStrToJson(AObjectJson.Values['bid1Size']);
  basis                  := GetStrToJson(AObjectJson.Values['basis']);
  preOpenPrice           := GetStrToJson(AObjectJson.Values['preOpenPrice']);
  preQty                 := GetStrToJson(AObjectJson.Values['preQty']);
  curPreListingPhase     := GetStrToJson(AObjectJson.Values['curPreListingPhase']);
end;



end.
