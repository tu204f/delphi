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
  Lb.Platform.Trading;

type
  TEventOnSendTrade = procedure(ASender: TObject; const ATime: TDateTime; const APrice, AQty: Double; ASide: TTypeBuySell) of object;

  ///<summary>
  /// Бот - для торговли
  ///</summary>
  TBot = class(TObject)
  private
    FPeriod: Integer;
    FTradingPlatform: TTradingPlatform;
    FValueRSI: Double;
  private
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
    constructor Create; virtual;
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
    property OnSendTrade: TEventOnSendTrade write FOnSendTrade;
  public
    ///<summary>Критерий на покупку</summary>
    property ManagerCategoryBuy: TManagerCategory read FManagerCategoryBuy;
    ///<summary>Критерий на продаже</summary>
    property ManagerCategorySell: TManagerCategory read FManagerCategorySell;
  end;

implementation

///<summary>Расчет индикатора</summary>
function GetRSI(const APeriod: Integer; ACandels: TCandelList): Double;

  function _SMA(const AValue: TDoubleList): Double;
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
        xDelta := xCandel2.Close - xCandel1.Close;
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

      xMaU := _SMA(xU);
      xMaD := _SMA(xD);
      xRS  := xMaU/xMaD;
      Result := 100 - 100/(1 + xRS);
    finally
      FreeAndNil(xD);
      FreeAndNil(xU);
    end;
  end;
end;

{ TBot }

constructor TBot.Create;
begin
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
    Result := FTradingPlatform.Trading.IsPosition;
end;

function TBot.IsTrading: Boolean;
begin
  // 1. Временной интервал, который можно торговать
  // 2. Размер полученного убытка
  // 3. Количество торговых операция — за торговый период
  // 4. Период заморозки торговли после получение убытка
  Result := True;
end;

procedure TBot.SetSelected;

  procedure _PositionClose;
  var
    xSide: TTypeBuySell;
    xPrice, xQty: Double;
    xPosition: TPlatformTrading.TPosition;
  begin
    xPosition := FTradingPlatform.Trading.CurrentPosition;
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
    xProfit: Double;
    xPosition: TPlatformTrading.TPosition;
  begin
    // закрытие позиции по условиями

    xProfit := 0;
    xPosition := FTradingPlatform.Trading.CurrentPosition;
    case xPosition.Side of
      tsBuy : xProfit := xPosition.GetProfit(TradingPlatform.StateMarket.Bid);
      tsSell: xProfit := xPosition.GetProfit(TradingPlatform.StateMarket.Ask);
    end;

    if xProfit > 0 then
      _PositionClose;
  end;


begin
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
      // Условия открытие позиции
      FValueRSI := GetRSI(FPeriod,FTradingPlatform.StateMarket.Candels);
      ManagerCategoryBuy.SetUpDateValue(FValueRSI);
      ManagerCategorySell.SetUpDateValue(FValueRSI);
    end;
  end
  else
    if IsActivePosition then
      _PositionClose;
end;

procedure TBot.ManagerCriteriaBuyOnSendTrade(ASender: TObject; ASide: TTypeBuySell; AQty: Double);
begin
  // Купить торговать
  if Assigned(FTradingPlatform) then
    DoSendTrade(
      Date + Time,
      FTradingPlatform.StateMarket.Ask,
      AQty,
      ASide
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
      ASide
    );
end;

procedure TBot.DoSendTrade(const ATime: TDateTime; const APrice, AQty: Double;
  ASide: TTypeBuySell);
begin
  if Assigned(FTradingPlatform) then
  begin
    FTradingPlatform.SendTrade(
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

end.
