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

  ///<summary>
  /// Напровление объекта
  ///</summary>
  TTypeTrade = (
    ttNull,
    ttBuy,
    ttSell
  );

  ///<summary>
  /// Трейдер бот
  ///</summary>
  TTradeBot = class(TObject)
  private
  protected
    function GetIndexRSI(const ASide: TTypeSide): Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Обновление данных
    ///</summary>
    procedure UpDate(const APriceBid, APriceAsk, ACurrentRSI: Double);
  end;

implementation

uses
  Lb.Setting,
  Lb.OperationTrade,
  Lb.Bybit.Trade;

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
begin

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

end;

destructor TTradeBot.Destroy;
begin

  inherited;
end;

function TTradeBot.GetIndexRSI(const ASide: TTypeSide): Double;
begin
  // Рекомендуемые заначение
  // Выводить значение результата RSI
  case ASide of
    tsBuy : Result := 20;
    tsSell: Result := 80;
  end;
end;

procedure TTradeBot.UpDate(const APriceBid, APriceAsk, ACurrentRSI: Double);
begin
  if ACurrentRSI < GetIndexRSI(TTypeSide.tsBuy) then
  begin
    // Покупать
  end;
  if ACurrentRSI > GetIndexRSI(TTypeSide.tsSell) then
  begin
    // Продать
  end;
end;

end.
