unit Lb.TradeMan;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils.Candel,
  Lb.Block;

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

  ///<summary>Начальный трейдер</summary>
  TTradeMan = class(TObject)
  public type
    TTypeСrossing = (tcUp,tcDonw);
    TTypeTrade = (ttTrend, ttContrTrend);
  private
    FCapital: Double;
    FDeposit: Double;
    FLimitDeposit: Double;
    FTrailingStop: Double;
    FPeriod: Integer;
    FTypeTrade: TTypeTrade;
    FMaxPrice, FMinPrice: Double;
  private
    FPosition: TPosition;
    procedure SetDeposit(const Value: Double);
  protected
    procedure DoOpenPosition(const ATypeСrossing: TTypeСrossing; const APrice: Double);
    procedure DoClosePosition(const ATypeСrossing: TTypeСrossing; const APrice: Double);
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>Трейдору передается блок для принятие решения</summary>
    procedure SetInputBlock(const ABlock: TBlock);
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
    'B': FStopPrice := (FOpenPrice * FQuantity + FTrailingStop)/FQuantity;
    'S': FStopPrice := (FOpenPrice * FQuantity - FTrailingStop)/FQuantity;
  end;
end;

procedure TPosition.UpData(const APrice: Double);
var
  xStopPrice: Double;
begin
  case FBuySell of
    'B': begin
      xStopPrice := (FOpenPrice * FQuantity + FTrailingStop)/FQuantity;
      if xStopPrice > FStopPrice then
        FStopPrice := xStopPrice;
    end;
    'S': begin
      xStopPrice := (FOpenPrice * FQuantity - FTrailingStop)/FQuantity;
      if xStopPrice < FStopPrice then
        FStopPrice := xStopPrice;
    end;
  end;
end;

{ TTradeMan }

constructor TTradeMan.Create;
begin
  FLimitDeposit := 0;
  FDeposit := 0;
  FCapital   := 0;
  FTypeTrade := TTypeTrade.ttTrend;
  FPeriod    := 10;
  FMaxPrice  := 0;
  FMinPrice  := 0;
  FPosition  := nil;
end;

destructor TTradeMan.Destroy;
begin
  if Assigned(FPosition) then
    FreeAndNil(FPosition);
  inherited;
end;


procedure TTradeMan.SetDeposit(const Value: Double);
begin
  FDeposit := Value;
  FLimitDeposit := Value;
end;

procedure TTradeMan.SetInputBlock(const ABlock: TBlock);
begin
  TBlockAPI.LimitValueMaxAndMinBlock(ABlock,FPeriod,FMaxPrice,FMinPrice);
end;

procedure TTradeMan.SetPriceLast(const ACandel: TCandel);

  function _UpPrice(AMaxPrice: Double; ACandel: TCandel): Double;
  begin
    // Снизу в верх
    Result := 0;
    if (ACandel.Open < AMaxPrice) and (ACandel.High >= AMaxPrice) then
      Result := AMaxPrice
    else if (ACandel.Open >= AMaxPrice) then
      Result := ACandel.Open;
  end;

  function _DownPrice(AMinPrice: Double; ACandel: TCandel): Double;
  begin
    // С верху вниз
    Result := 0;
    if (ACandel.Open > AMinPrice) and (ACandel.Low <= AMinPrice) then
      Result := AMinPrice
    else if (ACandel.Open <= AMinPrice) then
      Result := ACandel.Open;
  end;

  procedure _OpenPosition(const ACandel: TCandel);
  var
    xOpenPrice: Double;
  begin
    // Место принятие решение на открытие позиции
    xOpenPrice := _UpPrice(FMaxPrice,ACandel);
    if xOpenPrice > 0 then
    begin
      // Открываем позцию
      DoOpenPosition(tcUp,xOpenPrice);
    end
    else
    begin
      xOpenPrice := _DownPrice(FMinPrice,ACandel);
      if xOpenPrice > 0 then
      begin
        // Открываем позцию
        DoOpenPosition(tcDonw,xOpenPrice);
      end;
    end;
  end;

  procedure _ClosePosition(const ACandel: TCandel);
  var
    xClosePrice: Double;
  begin
    // Место принятие решение на открытие позиции
    xClosePrice := _UpPrice(FPosition.StopPrice,ACandel);
    if xClosePrice > 0 then
    begin
      DoClosePosition(tcUp,xClosePrice);
    end
    else
    begin
      xClosePrice := _DownPrice(FPosition.StopPrice,ACandel);
      if xClosePrice > 0 then
      begin
        DoClosePosition(tcDonw,xClosePrice);
      end;
    end;
  end;

begin
  if (FMaxPrice = 0) or (FMinPrice = 0) then
    Exit;

  if IsPosition then
    _ClosePosition(ACandel)
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

  xQuantity := FDeposit/APrice;
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

procedure TTradeMan.DoClosePosition(const ATypeСrossing: TTypeСrossing; const APrice: Double);
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

  // Тупа рости больше определеного значение не может
  FDeposit := FDeposit + xCapital;
  if FDeposit > FLimitDeposit then
    FDeposit := FLimitDeposit;

  FCapital := FCapital + xCapital;
  FreeAndNil(FPosition);
  FPosition := nil;
end;

function TTradeMan.IsPosition: Boolean;
begin
  Result := Assigned(FPosition);
end;

end.
