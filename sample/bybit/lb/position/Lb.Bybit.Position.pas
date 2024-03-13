unit Lb.Bybit.Position;

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
  TPositionObject = class;

  ///<summary>Список объект</summary>
  TPositionObjectList = TObjectList<TPositionObject>;

  TBybitPosition = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FBaseCoin: String;
    FSettleCoin: String;
    FLimit: Integer;
    FCursor: String;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetBaseCoin(const Value: String);
    procedure SetSettleCoin(const Value: String);
    procedure SetLimit(const Value: Integer);
    procedure SetCursor(const Value: String);
  protected
    FPositionObjects: TPositionObjectList;
    procedure DoEventMessage(const AMessage: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property PositionObjects: TPositionObjectList read FPositionObjects;
  public {Request Parameters}
    ///<summary>Тип продукта. spot, linear, inverse, option</summary>
    property Category: TTypeCategory read FCategory write SetCategory;
    ///<summary>Имя символа</summary>
    property Symbol: String read FSymbol write SetSymbol;
    ///<summary>Базовая монета. linear, inverse, option только</summary>
    ///<remarks>В качестве опции он возвращает BTC по умолчанию.</remarks>
    property BaseCoin: String read FBaseCoin write SetBaseCoin;
    ///<summary>
    ///Settle coin. For linear, either symbol or settleCoin
    ///is required. symbol has a higher priority
    ///</summary>
    property SettleCoin: String read FSettleCoin write SetSettleCoin;
    ///<summary>Limit for data size per page. [1, 1000]. Default: 500</summary>
    property Limit: Integer read FLimit write SetLimit;
    property Cursor: String read FCursor write SetCursor;
  end;

  TPositionObject = class(TCustonObjectJson)
  public
    ///<summary>
    ///Идентификатор позиции, используемый для идентификации позиций
    ///в различных позиционных режимах
    ///</summary>
    PositionIdx: Integer;
    ///<summary>
    ///Идентификатор лимита риска. Примечание: для режима маржи портфеля
    ///это поле возвращает значение 0, что означает,
    ///что правила ограничения риска недействительны
    ///</summary>
    RiskId: Integer;
    ///<summary>
    ///Значение предела риска. Примечание: для режима маржи
    ///портфеля это поле возвращает значение 0, что означает,
    ///что правила ограничения риска недействительны
    ///</summary>
    RiskLimitValue: String;
    ///<summary>Название символа</summary>
    Symbol: String;
    ///<summary>Position side. Buy: long, Sell: short</summary>
    Side: String;
    ///<summary>Position size</summary>
    Size: String;
    ///<summary>Average entry price</summary>
    AvgPrice: String;
    ///<summary>Position value</summary>
    PositionValue: String;
    ///<summary>Trade mode</summary>
    TradeMode: Integer;
    ///<summary>Следует ли автоматически добавлять маржу</summary>
    AutoAddMargin: Integer;
    ///<summary>Статус должности</summary>
    ///<remarks>
    /// Normal
    /// Liq - в процессе ликвидации
    /// Adl - в процессе автоматического уменьшения доли заемных средств
    ///</remarks>
    PositionStatus: String;
    ///<summary>Кредитное плечо позиции. Действительно для контракта</summary>
    Leverage: String;
    ///<summary>Цена последней отметки</summary>
    MarkPrice: String;
    ///<summary>Цена ликвидации позиции</summary>
    ///<remarks>
    ///Однако это поле пусто для режима маржи портфеля, и
    ///ликвидационная цена указана не будет
    ///</remarks>
    LiqPrice: String;
    ///<summary>Цена банкротства</summary>
    BustPrice: String;
    ///<summary>Начальная маржа. В режиме маржи портфеля возвращается значение ""</summary>
    PositionIM: String;
    ///<summary>Поддерживающая маржа. В режиме маржи портфеля возвращается значение ""</summary>
    PositionMM: String;
    ///<summary>Маржа позиции. В режиме маржи портфеля возвращается значение ""</summary>
    PositionBalance: String;
    ///<summary>Обесценивается, здесь не имеет смысла, всегда "Заполнено". Spot не возвращает это поле. Параметр возвращает ""</summary>
    TpslMode: String;
    ///<summary>Цена тейк-профита</summary>
    TakeProfit: String;
    ///<summary>Цена стоп-лосса</summary>
    StopLoss: String;
    ///<summary>Трейлинг-стоп (расстояние от рыночной цены)</summary>
    TrailingStop: String;
    ///<summary>Unrealised PnL</summary>
    UnrealisedPnl: String;
    ///<summary>Cumulative realised pnl</summary>
    CumRealisedPnl: String;
    ///<summary>Индикатор ранга автоматического снижения доли заемных средств.</summary>
    AdlRankIndicator: Integer;
    ///<summary>Полезно, когда Bybit снижает лимит риска</summary>
    IsReduceOnly: Boolean;
    ///<summary>Полезно, когда Bybit снижает лимит риска</summary>
    mmrSysUpdatedTime: String;
    ///<summary>Полезно, когда Bybit снижает лимит риска</summary>
    leverageSysUpdatedTime: String;
    createdTime: String;
    updatedTime: String;
    Seq: UInt64;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); override;
  end;

implementation

procedure SetPositionObjects(AListJson: TJSONArray; APositionObjects: TPositionObjectList);
var
  i, iCount: Integer;
  //xListJson: TJSONArray;
  xObjectJson: TJSONObject;
  xPositionObject: TPositionObject;
begin
  if not Assigned(APositionObjects) then
    Exit;
  APositionObjects.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xPositionObject := TPositionObject.Create;
      xPositionObject.SetObjectJson(xObjectJson);
      APositionObjects.Add(xPositionObject);
    end;
end;

{ TBybitPosition }

constructor TBybitPosition.Create;
begin
  inherited Create;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/position/list';
  FPositionObjects := TPositionObjectList.Create;
end;

destructor TBybitPosition.Destroy;
begin
  FreeAndNil(FPositionObjects);
  inherited;
end;

procedure TBybitPosition.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory));
end;

procedure TBybitPosition.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitPosition.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitPosition.SetSettleCoin(const Value: String);
begin
  FSettleCoin := Value;
  BybitModule.Params.SetParam('settleCoin',FSettleCoin);
end;

procedure TBybitPosition.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitPosition.SetCursor(const Value: String);
begin
  FCursor := Value;
  BybitModule.Params.SetParam('cursor',FCursor);
end;

procedure TBybitPosition.DoEventMessage(const AMessage: String);
var
  xListJson: TJSONArray;
  xValueJson: TJSONValue;
begin
  inherited DoEventMessage(AMessage);
  xValueJson := Response.ResultObject.Values['list'];
  if xValueJson is TJSONArray then
  begin
    xListJson := TJSONArray(xValueJson);
    SetPositionObjects(xListJson,FPositionObjects);
  end;
end;

{ TPositionObject }

constructor TPositionObject.Create;
begin
  inherited;

end;

destructor TPositionObject.Destroy;
begin

  inherited;
end;

procedure TPositionObject.SetObjectJson(const AObjectJson: TJSONObject);
begin
  inherited;
  PositionIdx            := AObjectJson.Values['positionIdx'].Value.ToInteger;
  RiskId                 := AObjectJson.Values['riskId'].Value.ToInteger;
  RiskLimitValue         := AObjectJson.Values['riskLimitValue'].Value;
  Symbol                 := AObjectJson.Values['symbol'].Value;
  Side                   := AObjectJson.Values['side'].Value;
  Size                   := AObjectJson.Values['size'].Value;
  avgPrice               := AObjectJson.Values['avgPrice'].Value;
  positionValue          := AObjectJson.Values['positionValue'].Value;
  tradeMode              := AObjectJson.Values['tradeMode'].Value.ToInteger;
  autoAddMargin          := AObjectJson.Values['autoAddMargin'].Value.ToInteger;
  positionStatus         := AObjectJson.Values['positionStatus'].Value;
  leverage               := AObjectJson.Values['leverage'].Value;
  markPrice              := AObjectJson.Values['markPrice'].Value;
  liqPrice               := AObjectJson.Values['liqPrice'].Value;
  bustPrice              := AObjectJson.Values['bustPrice'].Value;
  positionIM             := AObjectJson.Values['positionIM'].Value;
  positionMM             := AObjectJson.Values['positionMM'].Value;
  positionBalance        := AObjectJson.Values['positionBalance'].Value;
  tpslMode               := AObjectJson.Values['tpslMode'].Value;
  takeProfit             := AObjectJson.Values['takeProfit'].Value;
  stopLoss               := AObjectJson.Values['stopLoss'].Value;
  trailingStop           := AObjectJson.Values['trailingStop'].Value;
  unrealisedPnl          := AObjectJson.Values['unrealisedPnl'].Value;
  cumRealisedPnl         := AObjectJson.Values['cumRealisedPnl'].Value;
  adlRankIndicator       := AObjectJson.Values['adlRankIndicator'].Value.ToInteger;
  isReduceOnly           := AObjectJson.Values['isReduceOnly'].Value.ToBoolean;
  mmrSysUpdatedTime      := AObjectJson.Values['mmrSysUpdatedTime'].Value;
  leverageSysUpdatedTime := AObjectJson.Values['leverageSysUpdatedTime'].Value;
  createdTime            := AObjectJson.Values['createdTime'].Value;
  updatedTime            := AObjectJson.Values['updatedTime'].Value;
  seq                    := AObjectJson.Values['seq'].Value.ToInteger;
end;

end.
