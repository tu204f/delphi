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
  Lb.Criteria;

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
    FManagerCriteriaBuy: TManagerCriteria;
    FManagerCriteriaSell: TManagerCriteria;
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
    property ManagerCriteriaBuy: TManagerCriteria read FManagerCriteriaBuy;
    ///<summary>Критерий на продаже</summary>
    property ManagerCriteriaSell: TManagerCriteria read FManagerCriteriaSell;
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

  FManagerCriteriaBuy := TManagerCriteria.Create;
  FManagerCriteriaBuy.Side := TTypeBuySell.tsBuy;
  FManagerCriteriaBuy.SetCreateCriteria(50,0,10,10,0.01);

  FManagerCriteriaSell:= TManagerCriteria.Create;
  FManagerCriteriaSell.Side := TTypeBuySell.tsSell;
  FManagerCriteriaSell.SetCreateCriteria(50,100,10,10,0.01);

end;

destructor TBot.Destroy;
begin
  FreeAndNil(FManagerCriteriaBuy);
  FreeAndNil(FManagerCriteriaSell);
  inherited;
end;

function TBot.IsActivePosition: Boolean;
begin
  Result := False;
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
begin
  if IsTrading then
  begin
    // Можно соверщать торговые операции
    if IsActivePosition then
    begin
      // Условие закрытие
    end
    else
    begin
      // Условия открытие позиции
      FValueRSI := GetRSI(FPeriod,FTradingPlatform.StateMarket.Candels);
      ManagerCriteriaBuy.SetUpDateValue(FValueRSI);
      ManagerCriteriaSell.SetUpDateValue(FValueRSI);
    end;
  end
  else if IsActivePosition then
  begin
    // Принудительно закрывать позицию
  end;
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
