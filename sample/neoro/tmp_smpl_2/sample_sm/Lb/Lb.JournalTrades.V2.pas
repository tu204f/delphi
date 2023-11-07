unit Lb.JournalTrades.V2;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel,
  Lb.Block;

type
  ///<summary>Упрвление сделками</summary>
  TControlTrade = class(TObject)
  public type
    ///<summary>Сделка</summary>
    TTrade = class(TObject)
    private
      FPrice: Double;
      FQuantity: Integer;
      FBuySell: Char;
    public
      constructor Create(APrice: Double; AQuantity: Integer; ABuySell: Char); virtual;
      property Price: Double read FPrice write FPrice;
      property Quantity: Integer read FQuantity write FQuantity;
      property BuySell: Char read FBuySell write FBuySell;
    end;
    ///<summary>Список сделок</summary>
    TTradeList = TObjectList<TTrade>;
  private
    FStepPrice: Double;
    FCurrentPrice: Double;
    FTrades: TTradeList;
    FPriceOpen: Double;
    FPriceClose: Double;
    FQuantityOpen: Integer;
    FQuantityClose: Integer;
    FBuySell: Char;
    FProfit: Double;
    function GetIsActive: Boolean;
  protected
    procedure SetControlTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);
    procedure UpData(APrice: Double);
    property Trades: TTradeList read FTrades;
    property IsActive: Boolean read GetIsActive;
  public
    ///<summary>Цена открытие позиции</summary>
    property PriceOpen: Double read FPriceOpen;
    ///<summary>Цена закрытие позиции</summary>
    property PriceClose: Double read FPriceClose;
    ///<summary>Клочество открытой позции</summary>
    property Quantity: Integer read FQuantityOpen;
    ///<summary>Напровлнеие позиции</summary>
    property BuySell: Char read FBuySell;
    ///<summary>Получаемый результат если с разу закрыть все сделки</summary>
    property Profit: Double read FProfit;
    ///<summary>Устанавливаем шаг цены</summary>
    property StepPrice: Double write FStepPrice;
  end;

  ///<summary>Список сделка</summary>
  TControlTradeList = TObjectList<TControlTrade>;

  ///<summary>Объект управления сделками</summary>
  ///<remarks>Здесь определяется  расчет пограничных значений</remarks>
  TManagerTrade = class(TObject)
  private
    FControlTrade: TControlTrade;
    FControlTrades: TControlTradeList;
    function GetIsActive: Boolean;
  private
    FStepPrice: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Open(APrice: Double; AQuantity: Integer; ABuySell: Char);
    procedure Averaging(APrice: Double; ARate: Integer = 2);
    procedure UpData(APrice: Double);
    procedure Close(APrice: Double);
    function SumProfit: Double;
    property StepPrice: Double write FStepPrice;
    property IsActive: Boolean read GetIsActive;
    property ControlTrade: TControlTrade read FControlTrade;
    property ControlTrades: TControlTradeList read FControlTrades;
  end;

implementation

function _Trunc(const APrice, AStepPrice: Double): Double;
var
  xStepPrice: Double;
  xCount: Integer;
begin
  xStepPrice := AStepPrice;
  xCount := Round(APrice/xStepPrice);
  Result := xCount * xStepPrice;
end;


{ TControlTrade.TTrade }

constructor TControlTrade.TTrade.Create(APrice: Double; AQuantity: Integer;
  ABuySell: Char);
begin
  FPrice := APrice;
  FQuantity := AQuantity;
  FBuySell := ABuySell;
end;

{ TControlTrade }

constructor TControlTrade.Create;
begin
  FTrades := TTradeList.Create;

  FPriceOpen := 0;
  FQuantityOpen := 0;

  FPriceClose := 0;
  FQuantityClose := 0;

  FBuySell := #0;

  FStepPrice := 0.001;
end;

destructor TControlTrade.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TControlTrade.Clear;
begin
  FTrades.Clear;
end;

function TControlTrade.GetIsActive: Boolean;
begin
  Result := False;
  if FTrades.Count > 0 then
    Result := (FQuantityOpen - FQuantityClose) > 0;
end;

procedure TControlTrade.AddTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);
var
  xTrade: TTrade;
begin
  xTrade := TTrade.Create(APrice,AQuantity,ABuySell);
  FTrades.Add(xTrade);
  SetControlTrade;
end;

procedure TControlTrade.SetControlTrade;

  procedure _OpenTrade;
  var
    xValue: Double;
    xTrade: TTrade;
    i, iCount: Integer;
  begin
    xValue := 0;

    FPriceOpen := 0;
    FQuantityOpen := 0;
    FBuySell := #0;

    iCount := FTrades.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xTrade := FTrades[i];

        if FBuySell = #0 then
          FBuySell := xTrade.BuySell;

        if FBuySell <> xTrade.BuySell then
          Continue;

        xValue := xValue + xTrade.Price * xTrade.Quantity;
        FQuantityOpen := FQuantityOpen + xTrade.Quantity;
      end;
      if FQuantityOpen > 0 then
      begin
        FPriceOpen := xValue/FQuantityOpen;
        FPriceOpen := _Trunc(FPriceOpen,FStepPrice);
      end;
    end;
  end;

  procedure _CloseTrade;
  var
    xValue: Double;
    xTrade: TTrade;
    i, iCount: Integer;
  begin
    xValue := 0;

    FPriceClose := 0;
    FQuantityClose := 0;

    iCount := FTrades.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xTrade := FTrades[i];
        if FBuySell = xTrade.BuySell then
          Continue;
        xValue := xValue + xTrade.Price * xTrade.Quantity;
        FQuantityClose := FQuantityClose + xTrade.Quantity;
      end;
      if FQuantityClose > 0 then
      begin
        FPriceClose := xValue/FQuantityClose;
        FPriceClose := _Trunc(FPriceClose,FStepPrice);
        UpData(FPriceClose);
      end;
    end;
  end;

begin
  _OpenTrade;
  _CloseTrade;
end;

procedure TControlTrade.UpData(APrice: Double);

  function _CurrentQuantity: Integer;
  var
    xR: Integer;
  begin
    xR := FQuantityOpen - FQuantityClose;
    if xR < 0 then
      raise Exception.Create('Error Message: Логическая ошибка');
    Result := xR;
  end;

begin
  FProfit := 0;
  if _CurrentQuantity > 0 then
  begin
    case FBuySell of
      'B': FProfit := (APrice - FPriceOpen) * FQuantityOpen;
      'S': FProfit := (FPriceOpen - APrice) * FQuantityOpen;
    end;
  end
  else
  begin
    case FBuySell of
      'B': FProfit := (FPriceClose - FPriceOpen) * FQuantityOpen;
      'S': FProfit := (FPriceOpen - FPriceClose) * FQuantityOpen;
    end;
  end;
  FProfit := _Trunc(FProfit,FStepPrice);
end;

{ TManagerTrade }

constructor TManagerTrade.Create;
begin
  FControlTrade := nil;
  FControlTrades:= TControlTradeList.Create;
end;

destructor TManagerTrade.Destroy;
begin
  if Assigned(FControlTrades) then
     FreeAndNil(FControlTrades);
  inherited;
end;

function TManagerTrade.GetIsActive: Boolean;
begin
  Result := False;
  if Assigned(FControlTrade) then
    Result := FControlTrade.IsActive;
end;

procedure TManagerTrade.Open(APrice: Double; AQuantity: Integer; ABuySell: Char);
begin
  // Открытие позции
  if not IsActive then
  begin
    FControlTrade := TControlTrade.Create;
    FControlTrades.Add(FControlTrade);
    FControlTrade.AddTrade(APrice,AQuantity,ABuySell);
  end;
end;

function TManagerTrade.SumProfit: Double;
var
  xSum: Double;
  xTrade: TControlTrade;
begin
  xSum := 0;
  for xTrade in FControlTrades do
    xSum := xSum + xTrade.Profit;
  Result := xSum;
end;

procedure TManagerTrade.Averaging(APrice: Double; ARate: Integer = 2);
var
  xQuantity: Integer;
  xBuySell: Char;
begin
  if IsActive then
  begin
    xQuantity := ARate * FControlTrade.Quantity;
    xBuySell := FControlTrade.BuySell;
    FControlTrade.AddTrade(
      APrice,
      xQuantity,
      FControlTrade.BuySell
    );
  end;
end;

procedure TManagerTrade.Clear;
begin
  FControlTrade := nil;
  FControlTrades.Clear;
end;

procedure TManagerTrade.Close(APrice: Double);
var
  xQuantity: Integer;
  xBuySell: Char;
begin
  if IsActive then
  begin
    xQuantity := FControlTrade.Quantity;
    xBuySell := FControlTrade.BuySell;
    // Смена напровление 
    if xBuySell = 'B' then 
      xBuySell := 'S'
    else
      xBuySell := 'B';
    FControlTrade.AddTrade(
      APrice,
      xQuantity,
      xBuySell
    );
    if not FControlTrade.IsActive then
      FControlTrade := nil;
  end;
end;

procedure TManagerTrade.UpData(APrice: Double);
begin
  if IsActive then
    FControlTrade.UpData(APrice);
end;

end.
