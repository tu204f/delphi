unit Lb.TradeBot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline;

type
  ///<summary>
  /// Сделка с условием
  ///</summary>
  ///<remarks>
  /// TTypeCategory.tcLinear - работаем только со срочными инстурментами
  ///</remarks>
  TConditionTrade = class(TObject)
  private
    FSymbol: String;
    FSide: TTypeSide;
    FPrice: Double;
    FQuantity: Double;
    FValueRSI: Double;
    FIsActiveTrade: Boolean;
  protected
    procedure SetOperationTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Обновление значение
    ///</summary>
    ///<remarks>
    /// Цена по которой будет совершать цена.
    /// И текущие значение RSI
    ///</remarks>
    procedure UpDate(const APrice, ACurrentRSI: Double);
    ///<summary>
    /// Значение Индикатора RSI
    ///</summary>
    property ValueRSI: Double write FValueRSI;
    ///<summary>
    /// Напровление операции
    ///</summary>
    property Side: TTypeSide write FSide;
    ///<summary>
    /// Символ - инструмента, скоторым работаем
    ///</summary>
    property Symbol: String write FSymbol;
    ///<summary>
    /// Количество инструментом
    ///</summary>
    property Quantity: Double write FQuantity;
    ///<summary>
    /// Совершаем торговые операции
    ///</summary>
    property IsActiveTrade: Boolean read FIsActiveTrade write FIsActiveTrade;
  end;

  TTradeBot = class(TObject)
  private
    FTradeBuy: TConditionTrade;
    FTradeSell: TConditionTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure UpDate(const APriceBid, APriceAsk, ACurrentRSI: Double);
  end;

implementation

uses
  Lb.Setting,
  Lb.Bybit.Trade;

const
  API_KEY    = '3bI64e0kw4KuihyRPu';
  API_SECRET = 'jvwTC14ESSTjIpvXaRbDGW8xd1KoqD3H3cWY';

function GetApiKey: String;
begin
  Result := TSetting.ReadString('config.sys.api_key',API_KEY);
end;

function GetApiSecret: String;
begin
  Result := TSetting.ReadString('config.sys.api_secret',API_SECRET);
end;

{ TConditionTrade }

constructor TConditionTrade.Create;
begin
  FIsActiveTrade := False;
  FSymbol := '';
  FQuantity := 0;
  FPrice := 0;
end;

destructor TConditionTrade.Destroy;
begin

  inherited;
end;

procedure TConditionTrade.SetOperationTrade;

  function _GetOrderLinkId: String;
  begin
    // Уникальный нормер операции
    Result := Random(65000).ToString;
  end;

begin
  // Инструмент отслеживания
  // Передача ключей программе

  if FSymbol.IsEmpty then
    raise Exception.Create('Error Message: Инструмент не определен Symbol');

  var xOrderResponse := TOrderResponse.Create;
  try
    var xPlaceOrder := TParamOrder.Create;
    try
      xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;
      {todo: сохранение данных}
      xPlaceOrder.Category    := TTypeCategory.tcLinear;
      xPlaceOrder.Symbol      := FSymbol;
      xPlaceOrder.Side        := FSide;
      xPlaceOrder.PositionIdx := 0;
      xPlaceOrder.OrderType   := TTypeOrder.Limit;
      xPlaceOrder.Qty         := FQuantity;
      xPlaceOrder.Price       := FPrice;
      xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
      xPlaceOrder.OrderLinkId := _GetOrderLinkId;
      SelectedOrder(
         GetApiKey,
         GetApiSecret,
         xPlaceOrder,
         xOrderResponse
      );
    finally
      FreeAndNil(xPlaceOrder);
    end;
  finally
    FreeAndNil(xOrderResponse);
  end;
end;

procedure TConditionTrade.UpDate(const APrice, ACurrentRSI: Double);
begin
  FPrice := APrice;
  case FSide of
    tsBuy:
      if FIsActiveTrade and (FValueRSI > ACurrentRSI) then
      begin
        SetOperationTrade;
        FIsActiveTrade := False;
      end;
    tsSell:
      if FIsActiveTrade and (FValueRSI < ACurrentRSI) then
      begin
        SetOperationTrade;
        FIsActiveTrade := False;
      end;
  end;
end;


{ TTradeBot }

constructor TTradeBot.Create;
begin
  FTradeBuy  := TConditionTrade.Create;
  FTradeBuy.Side := TTypeSide.tsBuy;

  FTradeSell := TConditionTrade.Create;
  FTradeSell.Side := TTypeSide.tsSell;
end;

destructor TTradeBot.Destroy;
begin
  FreeAndNil(FTradeSell);
  FreeAndNil(FTradeBuy);
  inherited;
end;

procedure TTradeBot.UpDate(const APriceBid, APriceAsk, ACurrentRSI: Double);
begin
  FTradeBuy.UpDate(APriceAsk,ACurrentRSI);
  FTradeSell.UpDate(APriceBid,ACurrentRSI);
end;

end.
