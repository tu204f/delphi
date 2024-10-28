unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Category,
  Lb.Buffer.Trading;

type
  TTypeBot = (tbStraight, tbReverse);

  TEventOnSendTrade = procedure(ASender: TObject; const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell) of object;

  ///<summary>
  /// Механиз фиксации опериции не зависемо отплатформы
  ///</summary>
  TCustomTraginBot = class(TObject)
  private
    FBufferTrading: TBufferTrading;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Торгуем по стратегии
    ///</summary>
    property Trading: TBufferTrading read FBufferTrading;
  end;

  ///<summary>
  /// Бот - для торговли
  ///</summary>
  TBot = class(TCustomTraginBot)
  private
    FPeriod: Integer;
    FTradingPlatform: TTradingPlatform;
    FStopLossPrice: Double;
    FValueRSI: Double;
    FValueATR: Double;
  private
    FTypeBot: TTypeBot;
    FManagerCategoryBuy: TManagerCategory;
    FManagerCategorySell: TManagerCategory;
    procedure ManagerCriteriaBuyOnSendTrade(ASender: TObject; ASide: TTypeBuySell; AQty: Double);
    procedure ManagerCriteriaSellOnSendTrade(ASender: TObject; ASide: TTypeBuySell; AQty: Double);
  protected
    FOnSendTrade: TEventOnSendTrade;
    ///<summary>
    /// Проверка возможности совершение торговых операций
    ///</summary>
    function IsTrading: Boolean;
    ///<summary>
    /// Есть активня позиция
    ///</summary>
    function IsActivePosition: Boolean;
    ///<summary>
    /// Отправляем торговый приказ
    ///</summary>
    procedure DoSendTrade(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetSelected;
    ///<summary>
    /// На какой платформе работает — бот программы
    ///</summary>
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
    ///<summary>
    /// Период оценки риска
    ///</summary>
    property Period: Integer read FPeriod write FPeriod;

    ///<summary>
    /// Индекс значение работы RSI
    ///</summary>
    property ValueRSI: Double read FValueRSI;
    ///<summary>
    /// Значение валатилности рынка
    ///</summary>
    property ValueATR: Double read FValueATR;
    ///<summary>
    /// Рекомендуемая цена закрытия
    ///</summary>
    property StopLossPrice: Double read FStopLossPrice;

    ///<summary>
    /// Навпроление работы
    ///</summary>
    property TypeBot: TTypeBot read FTypeBot write FTypeBot;

    property OnSendTrade: TEventOnSendTrade write FOnSendTrade;
  public
    ///<summary>Критерий на покупку</summary>
    property ManagerCategoryBuy: TManagerCategory read FManagerCategoryBuy;
    ///<summary>Критерий на продаже</summary>
    property ManagerCategorySell: TManagerCategory read FManagerCategorySell;
  end;

implementation


(******************************************************************************)
(* Процедуры которые помогают оценить состояние рынка                         *)

function GetSMA(const AValue: TDoubleList): Double;
var
  xSum: Double;
  i, iCount: Integer;
begin
  Result := 0;
  iCount := AValue.Count;
  if iCount > 0 then
  begin
    xSum := 0;
    for i := 0 to iCount - 1 do
      xSum := xSum + AValue[i];
    Result := xSum/iCount;
  end;
end;

///<summary>Определяем валатиность рынка</summary>
function GetATR(const APeriod: Integer; ACandels: TCandelList): Double;

  function _MAX(const AValue1, AValue2, AValue3: Double): Double;
  var
    xValue: Double;
  begin
    if xValue < AValue1 then
      xValue := AValue1;
    if xValue < AValue2 then
      xValue := AValue2;
    if xValue < AValue3 then
      xValue := AValue3;
    Result := xValue;
  end;

var
  xDelta: Double;
  xCandel1, xCandel2: TCandel;
  xTR: TDoubleList;
begin
  Result := 0;
  if APeriod > ACandels.Count then
    Exit;

  if APeriod > 0 then
  begin
    xTR := TDoubleList.Create;
    try
      for var i := 0 to APeriod - 1 do
      begin
        if i > 0 then
        begin
          xCandel1 := ACandels[i - 1];
          xCandel2 := ACandels[i];
          xDelta := _MAX(
            xCandel2.High - xCandel2.Low,
            xCandel2.High - xCandel1.Close,
            xCandel1.Close - xCandel2.Low
          );
          xTR.Add(xDelta);
        end
        else
        begin
          xCandel1 := ACandels[i];
          xDelta := xCandel1.High - xCandel1.Low;
          xTR.Add(xDelta);
        end;
      end;
      Result := GetSMA(xTR);
    finally
      FreeAndNil(xTR);
    end;
  end;
end;

///<summary>Расчет индикатора</summary>
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;
var
  xDelta: Double;
  xCandel1, xCandel2: TCandel;
  xU, xD: TDoubleList;
  xMaU, xMaD, xRS: Double;
begin
  Result := 0;
  if APeriod > ACandels.Count then
    Exit;

  if APeriod > 0 then
  begin
    xU := TDoubleList.Create;
    xD := TDoubleList.Create;
    try
      xU.Add(0);
      xD.Add(0);
      for var i := 1 to APeriod - 1 do
      begin
        xCandel1 := ACandels[i - 1];
        xCandel2 := ACandels[i];
        xDelta := xCandel1.Close - xCandel2.Close;
        if xDelta > 0 then
        begin
          xU.Add(xDelta);
          xD.Add(0);
        end
        else
        begin
          xU.Add(0);
          xD.Add(Abs(xDelta));
        end;
      end;

      xMaU := GetSMA(xU);
      xMaD := GetSMA(xD);
      xRS  := xMaU/xMaD;
      Result := 100 - 100/(1 + xRS);
    finally
      FreeAndNil(xD);
      FreeAndNil(xU);
    end;
  end;
end;

(******************************************************************************)

{ TCustomTraginBot }

constructor TCustomTraginBot.Create;
begin
  FBufferTrading := TBufferTrading.Create;
end;

destructor TCustomTraginBot.Destroy;
begin
  FreeAndNil(FBufferTrading);
  inherited;
end;


{ TBot }

constructor TBot.Create;
begin
  inherited;
  FPeriod := 14;
  FValueRSI := 0;
  FTradingPlatform := nil;

  FManagerCategoryBuy := TManagerCategory.Create;
  FManagerCategoryBuy.Side := TTypeBuySell.tsBuy;
  FManagerCategoryBuy.SetCreateCriteria(50,0,10,10,0.01);
  FManagerCategoryBuy.OnSendTrade := ManagerCriteriaBuyOnSendTrade;

  FManagerCategorySell:= TManagerCategory.Create;
  FManagerCategorySell.Side := TTypeBuySell.tsSell;
  FManagerCategorySell.SetCreateCriteria(50,100,10,10,0.01);
  FManagerCategorySell.OnSendTrade := ManagerCriteriaSellOnSendTrade;

  // Прямой объект
  FTypeBot := TTypeBot.tbStraight;
end;

destructor TBot.Destroy;
begin
  FreeAndNil(FManagerCategoryBuy);
  FreeAndNil(FManagerCategorySell);
  inherited;
end;

function TBot.IsActivePosition: Boolean;
begin
  Result := False;
  if Assigned(FTradingPlatform) then
    Result := Trading.IsPosition;
end;

function TBot.IsTrading: Boolean;
begin
  // 1. Временной интервал, который можно торговать
  // 2. Размер полученного убытка
  // 3. Количество торговых операция — за торговый период
  // 4. Период заморозки торговли после получение убытка
  Result := True;
end;

procedure TBot.DoSendTrade(const ATime: TDateTime; const APrice, AQty: Double;
  ASide: TTypeBuySell);
begin
  // Открытие позиции
  if Assigned(FTradingPlatform) then
  begin
    {todo: Передается один формат структуры}
    {todo: Нужно переделать с ожиданием ответа торговой платформы}
    // Отправляем в торгую платформу
    FTradingPlatform.SendTrade(
      Date + Time,
      APrice,
      AQty,
      ASide
    );

    Trading.OpenTrade(
      Date + Time,
      APrice,
      AQty,
      ASide
    );

    if Assigned(FOnSendTrade) then
      FOnSendTrade(
        Self,
        Date + Time,
        APrice,
        AQty,
        ASide
      );
  end;
end;

procedure TBot.SetSelected;

  procedure _PositionClose;
  var
    xSide: TTypeBuySell;
    xPrice, xQty: Double;
    xPosition: TBufferTrading.TPosition;
  begin
    xPosition := Trading.CurrentPosition;
    // Принудительно закрывать позицию
    xSide := xPosition.Side;
    xSide := GetCrossSide(xSide);

    xQty := xPosition.Qty;
    case xSide of
      tsBuy: xPrice := FTradingPlatform.StateMarket.Ask;
      tsSell: xPrice := FTradingPlatform.StateMarket.Bid;
    end;

    DoSendTrade(
      Date + Time,
      xPrice,
      xQty,
      xSide
    );
  end;

  procedure _IfProfitPositionClose;
  var
    xValueCof: Double;
    xPosition: TBufferTrading.TPosition;
    xStopLoss: Double;
  begin
    // Производим рекомендуемое значение закрытие позиции
    xValueCof := 1;
    xStopLoss := 0;
    xPosition := Trading.CurrentPosition;
    if xPosition.MovingPrice > 0 then
    begin
      case xPosition.Side of
        tsBuy : begin
          xStopLoss := xPosition.MovingPrice - xValueCof * FValueATR;

          if FStopLossPrice = 0 then
            FStopLossPrice := xStopLoss
          else if FStopLossPrice < xStopLoss then
            FStopLossPrice := xStopLoss;
          if xStopLoss > FTradingPlatform.StateMarket.Bid then
            _PositionClose;

        end;
        tsSell: begin

          xStopLoss := xPosition.MovingPrice + xValueCof * FValueATR;
          if FStopLossPrice = 0 then
            FStopLossPrice := xStopLoss
          else if FStopLossPrice > xStopLoss then
            FStopLossPrice := xStopLoss;
          if xStopLoss < FTradingPlatform.StateMarket.Ask then
            _PositionClose;

        end;
      end;
    end;
  end;


begin
  {todo: Все вычисление вынести в отдельный модуль или объект, в целя исключение повторного вычисления}
  if not Assigned(FTradingPlatform) then
    Exit;

  // волатильность рынка
  FValueATR := GetATR(FPeriod,FTradingPlatform.StateMarket.Candels);
  FValueATR := Round(FValueATR * 100)/100;

  if IsTrading then
  begin
    // Можно соверщать торговые операции
    if IsActivePosition then
    begin
      // Условие закрытие
      _IfProfitPositionClose;
    end
    else
    begin
      FStopLossPrice := 0;
      // Условия открытие позиции
      FValueRSI := GetRSI(FPeriod,FTradingPlatform.StateMarket.Candels);
      FValueRSI := Round(FValueRSI * 100)/100;
      ManagerCategoryBuy.SetUpDateValue(FValueRSI);
      ManagerCategorySell.SetUpDateValue(FValueRSI);
    end;
  end
  else
    if IsActivePosition then
      _PositionClose;
end;

function localSide(ATypeBot: TTypeBot; ASide: TTypeBuySell): TTypeBuySell;
begin
  if ATypeBot = TTypeBot.tbReverse then
    Result := GetCrossSide(ASide)
  else
    Result := ASide;
end;

procedure TBot.ManagerCriteriaBuyOnSendTrade(ASender: TObject; ASide: TTypeBuySell; AQty: Double);
begin
  // Купить торговать
  if Assigned(FTradingPlatform) then
    DoSendTrade(
      Date + Time,
      FTradingPlatform.StateMarket.Ask,
      AQty,
      localSide(FTypeBot,ASide)
    );
end;

procedure TBot.ManagerCriteriaSellOnSendTrade(ASender: TObject; ASide: TTypeBuySell; AQty: Double);
begin
  // Продать
  if Assigned(FTradingPlatform) then
    DoSendTrade(
      Date + Time,
      FTradingPlatform.StateMarket.Bid,
      AQty,
      localSide(FTypeBot,ASide)
    );
end;

end.
