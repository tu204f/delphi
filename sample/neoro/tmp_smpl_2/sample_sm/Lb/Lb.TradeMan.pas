unit Lb.TradeMan;

interface

{$IFDEF DEBUG}
  {$DEFINE DBG_O_POS}
  {$DEFINE DBG_C_POS}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.SysUtils.Candel;

type
  ///<summary>Управление позиций</summary>
  TPosition = class(TObject)
  private
    FBuySell: Char;
    FOpenPrice: Double;
    FQuantity: Double;
    FTrailingStop: Double;
    FStopPrice: Double;
  public
    constructor Create(AOpenPrice: Double; AQuantity: Double; ABuySell: Char; ATrailingStop: Double);
    procedure UpData(const APrice: Double);
    property OpenPrice: Double read FOpenPrice;
    property Quantity: Double read FQuantity;
    property BuySell: Char read FBuySell;
    property TrailingStop: Double read FTrailingStop;
    property StopPrice: Double read FStopPrice;
  end;

type
(******************************************************************************)
(* Объект «трейдер» реализует алгоритм торговли,                              *)
(* и введение статистики операций                                             *)
(******************************************************************************)

  ///<summary>Начальный трейдер</summary>
  TTradeMan = class(TObject)
  public type

    ///<summary>Кастовый объект для сделки</summary>
    TCustomTrade = class(TObject)
    private
      FTradeMan: TTradeMan;
    public
      constructor Create(const ATradeMan: TTradeMan); virtual;
      destructor Destroy; override;
      procedure UpDataPrice(const APrice: Double); virtual; abstract;
    end;

    ///<summary>Объект для открытие позиции</summary>
    ///<remarks>Генерация событий, открытия</remarks>
    TOpen = class(TCustomTrade)
    public const
      PRICE_UP   = 0; // Цена растет
      PRICE_DOWN = 1; // Цена падает
    private
      FBuySell: Char;
      FPrice: Double;
      FMaxPrice, FMinPrice: Double;
    protected
      procedure DoOpenPosition(const APrice: Double; const AStatus: Integer);
    public
      constructor Create(const ATradeMan: TTradeMan); override;
      destructor Destroy; override;
      procedure UpDataPrice(const APrice: Double); override;
      property MaxPrice: Double write FMaxPrice;
      property MinPrice: Double write FMinPrice;
    public
      property BuySell: Char read FBuySell;
      property Price: Double read FPrice;
    end;

    ///<summary>Закрытие позиции</summary>
    TClose = class(TCustomTrade)
    private
      FOpen: TOpen;
      FStop: Double;
      FCountCandel: Integer;
      function GetProfitPrice(const APrice: Double): Double;
    protected
      procedure DoClosePosition(const APrice: Double);
    public
      constructor Create(const ATradeMan: TTradeMan); override;
      destructor Destroy; override;
      procedure Clear;
      procedure UpDataPrice(const APrice: Double); override;
      procedure CandelNew;
      ///<summary>Объекта - «Открытие»</summary>
      property Open: TOpen read FOpen write FOpen;
      ///<summary>Размер фиксации убытка</summary>
      property Stop: Double read FStop write FStop;
    end;

  public type
    TTypeСrossing    = (tcUp, tcDonw);
    TTypeTrade       = (ttTrend, ttContrTrend);
    TTypeTradeStatus = (tsOpen,tsWait,tsClose);
  private
    FLeverage: Integer;             // Размер кредитного плеча
    FCapital: Double;
    FDeposit: Double;
    FLimitDeposit: Double;
    FTrailingStop: Double;
    FPeriod: Integer;
    FTypeTrade: TTypeTrade;
    FMaxPrice, FMinPrice: Double;
  private {Статистика}
    FPlusCount: Integer;
    FMinusCount: Integer;
    FMinusProfit: Double;
  private
    FLogger: ILogger;
    procedure Log(S: String);
  private
    FPosition: TPosition;
    procedure SetDeposit(const Value: Double);
  protected
    FOnClosePosition: TNotifyEvent;
    procedure DoOpenPosition(const ATypeСrossing: TTypeСrossing; const APrice: Double);
    procedure DoClosePosition(const APrice: Double);
  private
    FOpen: TOpen;
    FClose: TClose;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>Трейдору передается блок для принятие решения</summary>
    procedure SetInputBlock(const ACandels: TCandels);
    ///<summary>Проверяем условие открытие или закрытие позиции</summary>
    procedure SetPriceLast(const ACandel: TCandel); overload;
  public {Управляемые параметры}
    ///<summary>Размер депозита</summary>
    ///<remarks>Открываем размер позиции на весь размер депозита</remarks>
    property Deposit: Double read FDeposit write SetDeposit;
    ///<summary>Период для оценки, значение по умолчанию 10</summary>
    property Period: Integer read FPeriod write FPeriod;
    ///<summary>Характеристика трейдера<summary>
    property TypeTrade: TTypeTrade read FTypeTrade write FTypeTrade;
    ///<summary>Задается в сумме, скользящий стоп</summary>
    property TrailingStop: Double read FTrailingStop write FTrailingStop;
    ///<summary>Размер кредитного плеча</summary>
    property Leverage: Integer read FLeverage write FLeverage;
    ///<summary>Есть отрытая позиция</summary>
    function IsPosition: Boolean;
  public
    ///<summary>Капитал</summary>
    property Capital: Double read FCapital;
    ///<summary>Максимальная цена, за период Period</summary>
    property MaxPrice: Double read FMaxPrice;
    ///<summary>Минимальная цена, за период Period</summary>
    property MinPrice: Double read FMinPrice;
    ///<summary>Размер позиции</summary>
    property Position: TPosition read FPosition;
  public
    property PlusCount: Integer read FPlusCount;
    property MinusCount: Integer read FMinusCount;
    property MinusProfit: Double read FMinusProfit;
  public
    property OnClosePosition: TNotifyEvent write FOnClosePosition;
    property Logger: ILogger write FLogger;
  end;

implementation

{ TPosition }

constructor TPosition.Create(AOpenPrice: Double; AQuantity: Double; ABuySell: Char; ATrailingStop: Double);
begin
  FBuySell := ABuySell;
  FOpenPrice := AOpenPrice;
  FQuantity := AQuantity;
  FTrailingStop := ATrailingStop;
  case FBuySell of
//    'B': FStopPrice := (FOpenPrice * FQuantity - FTrailingStop)/FQuantity;
//    'S': FStopPrice := (FOpenPrice * FQuantity + FTrailingStop)/FQuantity;
    'B': FStopPrice := FOpenPrice - FTrailingStop;
    'S': FStopPrice := FOpenPrice + FTrailingStop;
  end;
end;

procedure TPosition.UpData(const APrice: Double);
var
  xStopPrice: Double;
begin
  case FBuySell of
    'B': begin
      //xStopPrice := (APrice * FQuantity - FTrailingStop)/FQuantity;
      xStopPrice := APrice - FTrailingStop;
      if xStopPrice < 0 then
         raise Exception.Create('Error Message: Польная жопа');

      if xStopPrice > FStopPrice then
        FStopPrice := xStopPrice;
    end;
    'S': begin
      //xStopPrice := (APrice * FQuantity + FTrailingStop)/FQuantity;
      xStopPrice := APrice + FTrailingStop;
      if xStopPrice < 0 then
         raise Exception.Create('Error Message: Польная жопа');

      if xStopPrice < FStopPrice then
        FStopPrice := xStopPrice;
    end;
  end;
end;

{ TTradeMan.TCustomTrade }

constructor TTradeMan.TCustomTrade.Create(const ATradeMan: TTradeMan);
begin
  FTradeMan := ATradeMan;
end;

destructor TTradeMan.TCustomTrade.Destroy;
begin

  inherited;
end;

{ TTradeMan.TOpen }

constructor TTradeMan.TOpen.Create(const ATradeMan: TTradeMan);
begin
  inherited Create(ATradeMan);
  FMaxPrice := 0;
  FMinPrice := 0;
  FBuySell  := #0;
end;

destructor TTradeMan.TOpen.Destroy;
begin

  inherited;
end;

procedure TTradeMan.TOpen.UpDataPrice(const APrice: Double);
begin
  if APrice >= FMaxPrice then
  begin
    DoOpenPosition(APrice,PRICE_UP);
  end else if APrice <= FMinPrice then
  begin
    DoOpenPosition(APrice, PRICE_DOWN);
  end;
end;

procedure TTradeMan.TOpen.DoOpenPosition(const APrice: Double; const AStatus: Integer);
begin
  case AStatus of
    PRICE_UP  : FBuySell := 'B';
    PRICE_DOWN: FBuySell := 'S';
  end;
  if CharInSet(FBuySell,['B','S']) then
    FPrice := APrice;
end;

{ TTradeMan.TClose }

constructor TTradeMan.TClose.Create(const ATradeMan: TTradeMan);
begin
  inherited Create(ATradeMan);
  FOpen := nil;
  FCountCandel := 0;
end;

destructor TTradeMan.TClose.Destroy;
begin

  inherited;
end;

function TTradeMan.TClose.GetProfitPrice(const APrice: Double): Double;
begin
  case FOpen.BuySell of
    'B': Result := APrice - FOpen.Price;
    'S': Result := FOpen.Price - APrice;
  end;
end;

procedure TTradeMan.TClose.Clear;
begin
  FCountCandel := 0;
  FStop := 0;
end;

procedure TTradeMan.TClose.CandelNew;
begin
  Inc(FCountCandel);
end;

procedure TTradeMan.TClose.UpDataPrice(const APrice: Double);
var
  xProfit: Double;
begin
  if Assigned(FOpen) then
  begin
    xProfit := GetProfitPrice(APrice);
    if (xProfit > 0) and (FCountCandel = 1) then
    begin
      // Забираем весь рост свячи, от Open до Close
      DoClosePosition(APrice);
    end else if (xProfit < FStop) then
    begin
      // Фиксируем убыток
      DoClosePosition(APrice);
    end;
  end
  else
    raise Exception.Create('Error Message. Позиция должна быть отрыта');
end;

procedure TTradeMan.TClose.DoClosePosition(const APrice: Double);
begin

end;

{ TTradeMan }

constructor TTradeMan.Create;
begin
  FLeverage := 1;
  FLimitDeposit := 0;
  FDeposit := 0;
  FCapital   := 0;
  FTypeTrade := TTypeTrade.ttTrend;
  FPeriod    := 10;
  FMaxPrice  := 0;
  FMinPrice  := 0;
  FPosition  := nil;

  // Статистика
  FPlusCount := 0;
  FMinusCount := 0;
  FMinusProfit := 0;

  FLogger := nil;

  FOpen  := TOpen.Create(Self);
  FClose := TClose.Create(Self);
end;

destructor TTradeMan.Destroy;
begin
  if Assigned(FClose) then
    FreeAndNil(FClose);
  if Assigned(FOpen) then
    FreeAndNil(FOpen);


  if Assigned(FPosition) then
    FreeAndNil(FPosition);
  inherited;
end;


procedure TTradeMan.SetDeposit(const Value: Double);
begin
  FDeposit := Value;
  FLimitDeposit := Value;
end;

procedure TTradeMan.SetInputBlock(const ACandels: TCandels);


  (****************************************************************************)
  (* При расчете предельных значений, последний бар не учитывать              *)
  (****************************************************************************)
  procedure _LimitValueMaxAndMinBlock(
    ACandels: TCandels;
    APeriod: Integer;
    var AMaxValue, AMinValue: Double
  );
  var
    xCandel: TCandel;
    xPeriod: Integer;
    i, iCount: Integer;
  begin
    xPeriod := APeriod;
    AMaxValue := 0;
    AMinValue := 0;
    iCount := ACandels.Count;
    if iCount <= APeriod then
      Exit;

    if xPeriod > 0 then
    begin
      xCandel := ACandels[1];
      AMaxValue := xCandel.High;
      AMinValue := xCandel.Low;
      for i := 1 to xPeriod do
      begin
        xCandel := ACandels[i];
        if AMaxValue < xCandel.High then
          AMaxValue := xCandel.High;
        if AMinValue > xCandel.Low then
          AMinValue := xCandel.Low;
      end;
    end;
  end;



begin
  _LimitValueMaxAndMinBlock(ACandels,FPeriod,FMaxPrice,FMinPrice);
end;

procedure TTradeMan.SetPriceLast(const ACandel: TCandel);

  procedure _OpenPosition(const ACandel: TCandel);
  begin
    if ACandel.High >= FMaxPrice then
    begin
      DoOpenPosition(tcUp,FMaxPrice);
    end
    else if ACandel.Low <= FMinPrice then
    begin
      DoOpenPosition(tcDonw,FMinPrice);
    end;
  end;

  procedure _ClosePosition(const ACandel: TCandel);
  begin
    case FPosition.BuySell of
      'B': begin
        if ACandel.Low <= FPosition.StopPrice then
        begin
          DoClosePosition(FPosition.StopPrice);
        end else
        if (ACandel.Close - FPosition.OpenPrice) > 0 then
        begin
          DoClosePosition(ACandel.Close);
        end;
      end;
      'S': begin
        if ACandel.High >= FPosition.StopPrice then
        begin
          DoClosePosition(FPosition.StopPrice);
        end else
        if (FPosition.OpenPrice - ACandel.Close) > 0 then
        begin
          DoClosePosition(ACandel.Close);
        end;
      end;
    end;
  end;

begin
  if (FMaxPrice = 0) or (FMinPrice = 0) then
    Exit;
  if IsPosition then
  begin
    FPosition.UpData(ACandel.Close);
    _ClosePosition(ACandel);
  end
  else
    _OpenPosition(ACandel);
end;


procedure TTradeMan.DoOpenPosition(const ATypeСrossing: TTypeСrossing; const APrice: Double);

  function _Quantity(const AQuantity: Double): Double;
  begin
    Result := Trunc(AQuantity/0.001)*0.001;
  end;

var
  xQuantity: Double;
begin
  if IsPosition then
    raise Exception.Create('Error Message: Не может быть такого');

  if FLeverage <= 0 then
    raise Exception.Create('Error Message: Не может быть такого');

  xQuantity := (FLeverage * FDeposit)/APrice;
  xQuantity := _Quantity(xQuantity);

  // Больше не может отрывать позицию
  if xQuantity = 0 then
    Exit;

  // Событие открытие позции
  case ATypeСrossing of
    tcUp:
      case FTypeTrade of
        ttTrend: FPosition := TPosition.Create(APrice,xQuantity,'B',FTrailingStop);
        ttContrTrend: FPosition := TPosition.Create(APrice,xQuantity,'S',FTrailingStop);
      end;
    tcDonw:
      case FTypeTrade of
        ttTrend: FPosition := TPosition.Create(APrice,xQuantity,'S',FTrailingStop) ;
        ttContrTrend: FPosition := TPosition.Create(APrice,xQuantity,'B',FTrailingStop);
      end;
  end;
end;

procedure TTradeMan.DoClosePosition(const APrice: Double);
var
  xCapital: Double;
begin
  if not IsPosition then
    raise Exception.Create('Error Message: Не может быть такого');

  // Закрываем позицию
  xCapital := 0;
  case FPosition.BuySell of
    'B': xCapital := FPosition.Quantity * (APrice  - FPosition.OpenPrice);
    'S': xCapital := FPosition.Quantity * (FPosition.OpenPrice - APrice);
  end;

  if xCapital > 0 then
    Inc(FPlusCount)
  else
    Inc(FMinusCount);
  if FMinusProfit > xCapital then
    FMinusProfit := xCapital;


  // Тупа рости больше определеного значение не может
  FDeposit := FDeposit + xCapital;
  if FDeposit > FLimitDeposit then
    FDeposit := FLimitDeposit;

  FCapital := FCapital + xCapital;
  FreeAndNil(FPosition);
  FPosition := nil;

  if Assigned(FOnClosePosition) then
    FOnClosePosition(Self);
end;

function TTradeMan.IsPosition: Boolean;
begin
  Result := Assigned(FPosition);
end;

procedure TTradeMan.Log(S: String);
begin
  if Assigned(FLogger) then
    FLogger.Log(S);
end;

end.
