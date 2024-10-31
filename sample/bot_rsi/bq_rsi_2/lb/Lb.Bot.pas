unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Category,
  Lb.Buffer.Trading;

type
  TTypeBot = (tbLong, tbShort);

  ///<summary>
  /// Событие
  ///</summary>
  TEventOnSendTrade = procedure(
    ASender: TObject;
    const ATime: TDateTime;
    const APrice, AQty: Double;
    ASide: TTypeBuySell
  ) of object;

  ///<summary>
  /// Механиз фиксации опериции не зависемо отплатформы
  ///</summary>
  TCustomTraginBot = class(TObject)
  private
    FBufferTrading: TBufferTrading;
    FBufferCrossTrading: TBufferTrading;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// Торгуем по стратегии
    ///</summary>
    property Trading: TBufferTrading read FBufferTrading;
    ///<summary>
    /// Торгуем по кросс стратегии
    ///</summary>
    property CrossTrading: TBufferTrading read FBufferCrossTrading;
  end;

  ///<summary>
  /// Бот - для торговли
  ///</summary>
  TBot = class(TCustomTraginBot)
  private
    FTradingPlatform: TTradingPlatform;
    FStopLossPrice: Double;
    FValueCof: Double;
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
    property ValueCof: Double read FValueCof write FValueCof;
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

  ///<summary>Список ботов</summary>
  TBotList = TObjectList<TBot>;

  ///<summary>Список которой торгует виртуально</summary>
  TManagerBot = class(TObject)
  private
    FItems: TBotList;
    FTradingPlatform: TTradingPlatform;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure SetSelected;
    function AddBot: TBot;
    property Items: TBotList read FItems;
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
  end;

function GetSMA(const AValue: TDoubleList): Double;
function GetATR(const APeriod: Integer; ACandels: TCandelList): Double;
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;
function GetStrToTypeBot(const ATypeBot: TTypeBot): String;

implementation

(******************************************************************************)
(* *)

function GetStrToTypeBot(const ATypeBot: TTypeBot): String;
begin
  case ATypeBot of
    tbLong: Result := 'long';
    tbShort: Result := 'short';
  else
    Result := '';
  end;
end;


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
  FBufferCrossTrading := TBufferTrading.Create;
end;

destructor TCustomTraginBot.Destroy;
begin
  FreeAndNil(FBufferCrossTrading);
  FreeAndNil(FBufferTrading);
  inherited;
end;


{ TBot }

constructor TBot.Create;
begin
  inherited;

  FValueCof := 1;
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
  FTypeBot := TTypeBot.tbLong;
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

  procedure _IsCrossStrage(const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell);
  var
    xSide: TTypeBuySell;
  begin
    {todo: Передается один формат структуры}
    {todo: Нужно переделать с ожиданием ответа торговой платформы}

//      xSide := GetCrossSide(ASide)
//    else

    xSide := ASide;

    // Отправляем в торгую платформу
    FTradingPlatform.SendTrade(
      ATime,
      APrice,
      AQty,
      xSide
    );

    Trading.OpenTrade(
      ATime,
      APrice,
      AQty,
      xSide
    );

    CrossTrading.OpenTrade(
      ATime,
      APrice,
      AQty,
      GetCrossSide(xSide)
    );

  end;

begin
  // Открытие позиции
  if Assigned(FTradingPlatform) then
  begin

    _IsCrossStrage(
      ATime,
      APrice,
      AQty,
      ASide
    );

    // Бот сообщает что он совершил торговую операцию
    if Assigned(FOnSendTrade) then
      FOnSendTrade(
        Self,
        ATime,
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
    else
      xPrice := 0;
    end;

    if xPrice > 0 then
    begin
      DoSendTrade(
        Date + Time,
        xPrice,
        xQty,
        xSide
      )
    end
    else
    begin
      var xS := 'TBot.SetSelected._PositionClose';
      raise Exception.Create('Error Message: ' + xS + ' Вот такое не может быть.');
    end;
  end;

  procedure _IfProfitPositionClose;
  var
    xPosition: TBufferTrading.TPosition;
    xStopLoss: Double;
  begin
    // Производим рекомендуемое значение закрытие позиции
    xPosition := Trading.CurrentPosition;
    if xPosition.MovingPrice > 0 then
    begin
      case xPosition.Side of
        tsBuy : begin
          xStopLoss := xPosition.MovingPrice - FValueCof * FTradingPlatform.ValueATR;

          if FStopLossPrice = 0 then
            FStopLossPrice := xStopLoss
          else if FStopLossPrice < xStopLoss then
            FStopLossPrice := xStopLoss;
          if xStopLoss > FTradingPlatform.StateMarket.Bid then
            _PositionClose;

        end;
        tsSell: begin

          xStopLoss := xPosition.MovingPrice + FValueCof * FTradingPlatform.ValueATR;
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
      ManagerCategoryBuy.SetUpDateValue(FTradingPlatform.ValueRSI);
      ManagerCategorySell.SetUpDateValue(FTradingPlatform.ValueRSI);
    end;
  end
  else
    if IsActivePosition then
      _PositionClose;
end;

function localSide(ATypeBot: TTypeBot; ASide: TTypeBuySell): TTypeBuySell;
begin
  if ATypeBot = TTypeBot.tbShort then
    Result := GetCrossSide(ASide)
  else
    Result := ASide;
end;

procedure TBot.ManagerCriteriaBuyOnSendTrade(ASender: TObject; ASide: TTypeBuySell; AQty: Double);
begin
  // Если в друг сработало не сколько уровней
  // Купить торговать
  if not IsActivePosition then
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
  // Если вдруг с работало несколько уровней
  // Продать
  if not IsActivePosition then
    if Assigned(FTradingPlatform) then
      DoSendTrade(
        Date + Time,
        FTradingPlatform.StateMarket.Bid,
        AQty,
        localSide(FTypeBot,ASide)
      );
end;

{ TManagerBot }

constructor TManagerBot.Create;
begin
  FItems := TBotList.Create;
end;

destructor TManagerBot.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TManagerBot.Clear;
begin
  FItems.Clear;
end;

function TManagerBot.AddBot: TBot;
var
  xBot: TBot;
begin
  xBot := TBot.Create;
  xBot.TradingPlatform := FTradingPlatform;
  Result := xBot;
  FItems.Add(xBot);
end;

procedure TManagerBot.SetSelected;
var
  i, iCount: Integer;
begin
  iCount := FItems.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
      FItems[i].SetSelected;
end;

end.
