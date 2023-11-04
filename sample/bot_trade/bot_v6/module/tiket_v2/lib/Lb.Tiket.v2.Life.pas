unit Lb.Tiket.v2.Life;

interface

{$DEFINE TEST_LEFT}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TTypeStatus = (tsNull,tsOpen,tsClose);

  ///<summary>объект жизень</summary>
  ///<remarks>Всегда придерживаться напровление движения</remarks>
  TLife = class(TObject)
  private
    FStatus: TTypeStatus;   // Стояние фиксации
    FFullSumValue: Double;  // Полня сумма
    FSumValue: Double;      // Сумма для опеределение момента открытия или закрытия
    FOpenValue: Double;     // Значенеи открытия
    FCloseValue: Double;    // Значенеи закрытия
    FIsRevers: Boolean;     // Реверсирование напровление джижения
  protected
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    ///<summary>Учитовая состояние реверсирование потока</summary>
    procedure InputValueRevers(const AValue: Double);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Установить значение по дефолу</summary>
    procedure SetDefaultValue;
    ///<summary>Входное значение, потока</summary>
    procedure InputValue(const AValue: Double);
    ///<summary>Значение на открытие позиции</summary>
    property OpenValue: Double read FOpenValue write FOpenValue;
    ///<summary>Занчение на закрытие позиции</summary>
    property CloseValue: Double read FCloseValue write FCloseValue;
    ///<summary>Изменить напровление движения</summary>
    property IsRevers: Boolean read FIsRevers write FIsRevers;
    property SumValue: Double read FSumValue;
    property FullSumValue: Double read FFullSumValue;
  end;

{$IFDEF TEST_LEFT}
  TPriceList = TList<Double>;

  TVirtualTestTrader = class;

  ///<summary>Расширяем возможность жизни</summary>
  TTradeLeft = class(TLife)
  public type
    TTrade = record
      Price: Double;
      BuySell: Char;
      Profit: Double;
    end;
    TTradeList = TList<TTrade>;
  private
    FID: String;
    FPrice: Double;
    FTrades: TTradeList;
  protected
    FVTT: TVirtualTestTrader;
    procedure DoOpen; override;
    procedure DoClose; override;
    function GetSumProfit: Double;
    property VTT: TVirtualTestTrader read FVTT write FVTT;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Тестовая сделка</summary>
    ///<remarks>ADeltaValue - Сколько прошли, APrice - цена</remarks>
    procedure InputTest(const ADeltaValue, APrice: Double);
    property Trades: TTradeList read FTrades;
    property ID: String read FID write FID;
  end;

  ///<summary>Вертуальный трейдер</summary>
  TVirtualTestTrader = class(TObject)
  private
    FBuyLeft: TTradeLeft;
    FSellLeft: TTradeLeft;
    function GetOpenValue: Double;
    procedure SetOpenValue(const Value: Double);
    function GetCloseValue: Double;
    procedure SetCloseValue(const Value: Double);
    function GetSumProfit: Double;
  protected
    procedure StepPlusOpenValue;
    procedure StepPlusCloseValue;
    procedure StepMinusOpenValue;
    procedure StepMinusCloseValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetDefaultValue;
    ///<summary>Тестовая сделка</summary>
    ///<remarks>ADeltaValue - Сколько прошли, APrice - цена</remarks>
    procedure InputTest(const ADeltaValue, APrice: Double);
    ///<summary>Значение на открытие позиции</summary>
    property OpenValue: Double read GetOpenValue write SetOpenValue;
    ///<summary>Занчение на закрытие позиции</summary>
    property CloseValue: Double read GetCloseValue write SetCloseValue;
    ///<summary>Напровление на покупку</summary>
    property BuyLeft: TTradeLeft read FBuyLeft;
    ///<summary>Напровление на продажу</summary>
    property SellLeft: TTradeLeft read FSellLeft;

  public
    property SumProfit: Double read GetSumProfit;
  end;

  ///<summary><>
  TVirtualTestTraderList = TObjectList<TVirtualTestTrader>;

{$ENDIF}

implementation

{ TLife }

constructor TLife.Create;
begin
  SetDefaultValue;
end;

destructor TLife.Destroy;
begin

  inherited;
end;

procedure TLife.SetDefaultValue;
begin
  FIsRevers     := False;
  FStatus       := tsNull;
  FFullSumValue := 0;
  FSumValue     := 0;
  FOpenValue    := 0;
  FCloseValue   := 0;
end;

procedure TLife.DoClose;
begin
  // Событие закрытия
  FStatus := TTypeStatus.tsClose;
end;

procedure TLife.DoOpen;
begin
  // Событие открытия
  FStatus := TTypeStatus.tsOpen;
end;

procedure TLife.InputValueRevers(const AValue: Double);
begin
  if (FOpenValue > 0) and (FCloseValue > 0) then
  begin
    // Считаем напровление потока
    FSumValue := FSumValue + AValue;
    case FStatus of
      tsNull, tsClose: begin
        if FSumValue > FOpenValue then
        begin
          // Привышение определеного занчение потока производится фиксация
          FSumValue := FOpenValue;
          DoOpen;
        end else if FSumValue < 0 then
          FSumValue := 0;
      end;
      tsOpen: begin
        // Производим проверку на сброс фиксации
        if FSumValue > FOpenValue then
          FSumValue := FOpenValue
        else if FSumValue < FCloseValue then
        begin
          DoClose;
        end;
      end;
    end;
  end;
end;

procedure TLife.InputValue(const AValue: Double);
var
  xValue: Double;
begin
  if FStatus = TTypeStatus.tsOpen then
    FFullSumValue := FFullSumValue + AValue;
  if FIsRevers then
    xValue := -1 * AValue
  else
    xValue := AValue;
  InputValueRevers(xValue);
end;

{$IFDEF TEST_LEFT}

{ TTradeLeft }

constructor TTradeLeft.Create;
begin
  inherited;
  FTrades := TTradeList.Create;
end;

destructor TTradeLeft.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TTradeLeft.DoOpen;
var
  xTrade: TTrade;
begin
  inherited DoOpen;
  xTrade.Price := FPrice;
  if IsRevers then
    xTrade.BuySell := 'S'
  else
    xTrade.BuySell := 'B';
  xTrade.Profit := 0;
  FTrades.Add(xTrade);
end;

function TTradeLeft.GetSumProfit: Double;
begin
  var xSum := 0.0;
  for var xTrade in FTrades do
    xSum := xSum + xTrade.Profit;
  Result := xSum;
end;

procedure TTradeLeft.DoClose;
var
  xInd: Integer;
  xTrade: TTrade;
begin
  inherited DoClose;
  xTrade.Price := FPrice;

  xInd := FTrades.Count - 1;
  if IsRevers then
  begin
    xTrade.BuySell := 'B';
    xTrade.Profit := FTrades[xInd].Price - xTrade.Price;
  end
  else
  begin
    xTrade.BuySell := 'S';
    xTrade.Profit := xTrade.Price - FTrades[xInd].Price;
  end;
  FTrades.Add(xTrade);

  if Assigned(FVTT) then
  begin
    if xTrade.Profit > 0 then
    begin
      FVTT.StepMinusOpenValue;
      //FVTT.StepMinusCloseValue;
    end
    else if xTrade.Profit < 0 then
    begin
      FVTT.StepPlusOpenValue;
      //FVTT.StepMinusOpenValue;
      //FVTT.StepPlusCloseValue;
    end;
  end;

end;

procedure TTradeLeft.InputTest(const ADeltaValue, APrice: Double);
begin
  FPrice := APrice;
  Self.InputValue(ADeltaValue);
end;

{ TVirtualTestTrader }

constructor TVirtualTestTrader.Create;
begin
  FBuyLeft  := TTradeLeft.Create;
  FBuyLeft.IsRevers := False;
  FBuyLeft.VTT := Self;

  FSellLeft := TTradeLeft.Create;
  FSellLeft.IsRevers := True;
  FSellLeft.VTT := Self;
end;

destructor TVirtualTestTrader.Destroy;
begin
  FreeAndNil(FSellLeft);
  FreeAndNil(FBuyLeft);
  inherited;
end;


procedure TVirtualTestTrader.InputTest(const ADeltaValue, APrice: Double);
begin
  FBuyLeft.InputTest(ADeltaValue,APrice);
  FSellLeft.InputTest(ADeltaValue,APrice);
end;

function TVirtualTestTrader.GetOpenValue: Double;
begin
  if FBuyLeft.OpenValue = FSellLeft.OpenValue then
    Result := FBuyLeft.OpenValue
  else
    raise Exception.Create('Error Message: асинхроное значение');
end;

function TVirtualTestTrader.GetSumProfit: Double;
begin
  Result :=
    FBuyLeft.GetSumProfit +
    FSellLeft.GetSumProfit;
end;

function TVirtualTestTrader.GetCloseValue: Double;
begin
  if FBuyLeft.CloseValue = FSellLeft.CloseValue then
    Result := FBuyLeft.CloseValue
  else
    raise Exception.Create('Error Message: асинхроное значение');
end;

procedure TVirtualTestTrader.SetOpenValue(const Value: Double);
begin
  FBuyLeft.OpenValue := Value;
  FSellLeft.OpenValue := Value;
end;

procedure TVirtualTestTrader.SetCloseValue(const Value: Double);
begin
  FBuyLeft.CloseValue := Value;
  FSellLeft.CloseValue := Value;
end;

procedure TVirtualTestTrader.SetDefaultValue;
begin
  FBuyLeft.SetDefaultValue;
  FSellLeft.SetDefaultValue;

  FBuyLeft.IsRevers := False;
  FSellLeft.IsRevers := True;
end;

procedure TVirtualTestTrader.StepMinusOpenValue;
begin
  FBuyLeft.OpenValue  := FBuyLeft.OpenValue  - 50;
  FSellLeft.OpenValue := FSellLeft.OpenValue - 50;
end;

procedure TVirtualTestTrader.StepMinusCloseValue;
begin
  FBuyLeft.CloseValue  := FBuyLeft.CloseValue  - 30;
  FSellLeft.CloseValue := FSellLeft.CloseValue - 30;
end;

procedure TVirtualTestTrader.StepPlusOpenValue;
begin
  FBuyLeft.OpenValue  := FBuyLeft.OpenValue  + 50;
  FSellLeft.OpenValue := FSellLeft.OpenValue + 50;
end;

procedure TVirtualTestTrader.StepPlusCloseValue;
begin
  FBuyLeft.CloseValue  := FBuyLeft.CloseValue  + 30;
  FSellLeft.CloseValue := FSellLeft.CloseValue + 30;
end;

{$ENDIF}

end.
