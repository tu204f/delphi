unit UnitCharTradeFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  Lb.JournalTrades;

type
  ///<summary>Графический представление сделки</summary>
  TCharTradeFrame = class(TFrame)
    RectanglePrice: TRectangle;
    GridPanelLayout: TGridPanelLayout;
    TextOpenPrice: TText;
    TextUpDatePrice: TText;
    Rectangle: TRectangle;
    RectangleChart: TRectangle;
    RectangleText: TRectangle;
    LineTop: TLine;
    LineBottom: TLine;
    LineOpen: TLine;
  public const
    {todo: открытие позии}
    LMT_PRICE = 1.5;
    MAX_PRICE = LMT_PRICE;
    MIN_PRICE = LMT_PRICE;
  public type
    TTypeLinePosition = (lpTop, lpBottom, lpOpen);
  private
    FOpenPrice: Double; // Цена открытие позции
    FCurrentPrice: Double;
    FMaxPrice: Double;  // Максимальная цена
    FMinPrice: Double;  // Минимальаня цена
    FBuySell: Char;
    FStepDelta: Double; // Дела шаг цена
    function GetMaxPrice(const APrice: Double): Double;
    function GetMinPrice(const APrice: Double): Double;
    function GetPositionToPrice(const APrice: Double): Double;
    procedure SetPositionOpenPrice(const APrice: Double);
    procedure SetPositionUpDatePrice(const APrice: Double);
    procedure SetPositionRectanglePrice(const APriceTop, APriceBottom: Double);
    function GetDelta: Double;
  private
    procedure LineInitilization;
    procedure PriceLinePosition(const APrice: Double; ALinePosition: TTypeLinePosition);
  protected
    FOnLimitPrice: TNotifyEvent;
    FOnLimitProfit: TNotifyEvent;
    procedure DoLimitPrice;
    procedure DoLimitProfit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///<summary>Цена открытие сделки</summary>
    procedure Open(const APrice: Double; const ABuySell: Char; const AStepDelta: Double);
    ///<summary>Обновление состояние сделки</summary>
    procedure UpDate(const APrice: Double);
    ///<summary>Разница между ценой открытие и закрытие</summary>
    property Delta: Double read GetDelta;
    ///<summary>Преодолен лимитный уровень</summary>
    ///<remarks>Событие на усреднение</remarks>
    property OnLimitPrice: TNotifyEvent write FOnLimitPrice;
    property OnLimitProfit: TNotifyEvent write FOnLimitProfit;
  end;

implementation

{$R *.fmx}

{ TCharTradeFrame }

constructor TCharTradeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TextOpenPrice.Visible := False;
  TextUpDatePrice.Visible := False;
  RectanglePrice.Visible := False;
  LineInitilization;
end;

destructor TCharTradeFrame.Destroy;
begin
  inherited;
end;


procedure TCharTradeFrame.LineInitilization;

  procedure _Line(const ALine: TLine);
  begin
    ALine.Visible := False;
    ALine.Stroke.Thickness := 2;
    ALine.Height := 2;
  end;

begin
  _Line(LineTop);
  _Line(LineBottom);


  LineTop.Stroke.Color := TAlphaColorRec.Blue;
  LineBottom.Stroke.Color := TAlphaColorRec.Blue;

  _Line(LineOpen);
end;

function TCharTradeFrame.GetDelta: Double;
begin
  Result := 0;
  case FBuySell of
    'B' : Result := FCurrentPrice - FOpenPrice;
    'S' : Result := FOpenPrice - FCurrentPrice;
  end;
end;

function TCharTradeFrame.GetPositionToPrice(const APrice: Double): Double;
begin
  Result := Self.Height * (1 - (APrice - FMinPrice) /(FMaxPrice - FMinPrice));
end;

function TCharTradeFrame.GetMaxPrice(const APrice: Double): Double;
begin
  Result := 3000;// APrice * (1 + MAX_PRICE);
end;

function TCharTradeFrame.GetMinPrice(const APrice: Double): Double;
begin
  Result := 3000;// APrice * (1 - MAX_PRICE);
end;

procedure TCharTradeFrame.Open(const APrice: Double; const ABuySell: Char; const AStepDelta: Double);
begin
  FStepDelta := AStepDelta;

  FOpenPrice := APrice;
  FBuySell := ABuySell;

  {todo: Цена открытие позиции}
  FMaxPrice := FOpenPrice + GetMaxPrice(FOpenPrice);
  FMinPrice := FOpenPrice - GetMinPrice(FOpenPrice);

  PriceLinePosition(FOpenPrice,TTypeLinePosition.lpOpen);
  PriceLinePosition(FOpenPrice + AStepDelta,TTypeLinePosition.lpTop);
  PriceLinePosition(FOpenPrice - AStepDelta,TTypeLinePosition.lpBottom);
end;


procedure TCharTradeFrame.SetPositionRectanglePrice(const APriceTop, APriceBottom: Double);
var
  xTopY, xBottomY: Double;
begin
  xTopY    := GetPositionToPrice(APriceTop);
  xBottomY := GetPositionToPrice(APriceBottom);

  RectanglePrice.SetBounds(
    0,
    xTopY,
    RectangleChart.Width,
    (xBottomY - xTopY)
  );

  RectanglePrice.Visible := True;
end;

procedure TCharTradeFrame.SetPositionOpenPrice(const APrice: Double);
var
  xPriceY: Double;
begin
  xPriceY := GetPositionToPrice(APrice) - 21/2;
  TextOpenPrice.Text := Trunc(APrice).ToString;

  TextOpenPrice.SetBounds(
    5,
    xPriceY,
    RectangleText.Width,
    21
  );

  TextOpenPrice.Visible := True;
end;

procedure TCharTradeFrame.SetPositionUpDatePrice(const APrice: Double);
var
  xPriceY: Double;
begin
  xPriceY := GetPositionToPrice(APrice) - 21/2;
  TextUpDatePrice.Text := APrice.ToString;

  TextUpDatePrice.SetBounds(
    5,
    xPriceY,
    RectangleText.Width,
    21
  );
  TextUpDatePrice.Visible := True;
end;

procedure TCharTradeFrame.PriceLinePosition(const APrice: Double; ALinePosition: TTypeLinePosition);

  procedure _LinePosition(const APrice: Double; const ALine: TLine);
  var
    xPriceY: Double;
  begin
    xPriceY := GetPositionToPrice(APrice) - 1;

    ALine.SetBounds(
      0,
      xPriceY,
      RectangleChart.Width,
      2
    );

    ALine.Visible := True;
  end;

begin
  case ALinePosition of
    lpTop   : _LinePosition(APrice,LineTop);
    lpBottom: _LinePosition(APrice,LineBottom);
    lpOpen  : _LinePosition(APrice,LineOpen);
  end;
end;

procedure TCharTradeFrame.UpDate(const APrice: Double);
var
  xPriceTop, xPriceBottom: Double;
begin
  FCurrentPrice := APrice;

  SetPositionOpenPrice(FOpenPrice);
  SetPositionUpDatePrice(APrice);


  if FOpenPrice > APrice then
  begin
    xPriceTop := FOpenPrice;
    xPriceBottom := APrice;

    // Цвет напровление
    case FBuySell of
      'B': RectanglePrice.Fill.Color := TAlphaColorRec.Red;
      'S': RectanglePrice.Fill.Color := TAlphaColorRec.Green;
    end;

    SetPositionRectanglePrice(xPriceTop, xPriceBottom);
  end else if FOpenPrice < APrice then
  begin
    xPriceTop := APrice;
    xPriceBottom := FOpenPrice;

    // Цвет напровление
    case FBuySell of
      'B': RectanglePrice.Fill.Color := TAlphaColorRec.Green;
      'S': RectanglePrice.Fill.Color := TAlphaColorRec.Red;
    end;

    SetPositionRectanglePrice(xPriceTop, xPriceBottom);
  end else
  begin
    TextOpenPrice.Visible := False;
    TextUpDatePrice.Visible := False;
    RectanglePrice.Visible := False;
  end;

  DoLimitPrice;
  DoLimitProfit;
end;

procedure TCharTradeFrame.DoLimitPrice;
var
  xMaxP, xMinP: Double;
begin
  {todo: Список объекта работы}
  xMaxP := FOpenPrice + FStepDelta;
  xMinP := FOpenPrice - FStepDelta;
  if (FCurrentPrice > xMaxP) and (FBuySell = 'S') then
  begin
    // ----------------------------
    // Пересечение верхнего предела
    // Сигнал на усреднение
    FOpenPrice := xMaxP;
    FMaxPrice := FOpenPrice + GetMaxPrice(FOpenPrice);
    FMinPrice := FOpenPrice - GetMinPrice(FOpenPrice);
    if Assigned(FOnLimitPrice) then
      FOnLimitPrice(Self);
  end else if (FCurrentPrice < xMinP) and (FBuySell = 'B') then
  begin
    // ---------------------------
    // Пересечение нижнего предела
    // Сигнал на усреднение
    FOpenPrice := xMinP;
    FMaxPrice := FOpenPrice + GetMaxPrice(FOpenPrice);
    FMinPrice := FOpenPrice - GetMinPrice(FOpenPrice);
    if Assigned(FOnLimitPrice) then
      FOnLimitPrice(Self);
  end;
end;

procedure TCharTradeFrame.DoLimitProfit;
begin
  if ((FCurrentPrice - FOpenPrice) > 500) and (FBuySell = 'B') then
  begin
    if Assigned(FOnLimitProfit) then
      FOnLimitProfit(Self);
  end else if ((FOpenPrice - FCurrentPrice) > 500) and (FBuySell = 'S') then
  begin
    if Assigned(FOnLimitProfit) then
      FOnLimitProfit(Self);
  end;
end;

end.
