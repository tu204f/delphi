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
  TTypeLinePosition = (lpTop, lpBottom, lpOpen);
  TOnEventDecision = procedure(Sender: TObject; APrice: Double; ALinePosition: TTypeLinePosition) of object;

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
  private
    FLimitValue: Integer;
    FDecisionValue: Integer;
  private
    FStepPrice: Double; // Шаг цены
    FOpenPrice: Double; // Цена открытие позции
    FCurrentPrice: Double;
    FMaxPrice: Double;  // Максимальная цена
    FMinPrice: Double;  // Минимальаня цена
    FBuySell: Char;
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
    FOnLimitPrice: TOnEventDecision;
    procedure DoDecisionPrice;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///<summary>Цена открытие сделки</summary>
    ///<param name="APrice">Цена сделки</param>
    ///<param name="ABuySell">Напровление сделки</param>
    ///<param name="AStepDelta">Шаг напровления</param>
    procedure Open(const APrice: Double; const ABuySell: Char; const AStepPrice: Double);
    ///<summary>Обновление состояние сделки</summary>
    procedure UpDate(const APrice: Double);
    ///<summary>Разница между ценой открытие и закрытие</summary>
    property Delta: Double read GetDelta;
    ///<summary>Преодолен лимитный уровень</summary>
    ///<remarks>Событие на усреднение</remarks>
    property OnEventDecision: TOnEventDecision write FOnLimitPrice;
  public {Установить приделы изменение цены}
    property LimitValue: Integer read FLimitValue write FLimitValue;
    property DecisionValue: Integer read FDecisionValue write FDecisionValue;
  end;

implementation

{$R *.fmx}

{ TCharTradeFrame }

constructor TCharTradeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLimitValue := 100;
  FDecisionValue := 20;
  FStepPrice := 0.001;

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
  // Вычисляем верхний предел
  Result := APrice +  FLimitValue * FStepPrice;
end;

function TCharTradeFrame.GetMinPrice(const APrice: Double): Double;
begin
  // Вычисляем нижний предел цены
  Result := APrice - FLimitValue * FStepPrice;
end;

procedure TCharTradeFrame.Open(const APrice: Double; const ABuySell: Char; const AStepPrice: Double);
begin
  FStepPrice := AStepPrice;

  FOpenPrice := APrice;
  FBuySell := ABuySell;

  {todo: Цена открытие позиции}
  FMaxPrice := GetMaxPrice(FOpenPrice);
  FMinPrice := GetMinPrice(FOpenPrice);

  PriceLinePosition(FOpenPrice,TTypeLinePosition.lpOpen);
  PriceLinePosition(FOpenPrice + FDecisionValue * FStepPrice, TTypeLinePosition.lpTop);
  PriceLinePosition(FOpenPrice - FDecisionValue * FStepPrice, TTypeLinePosition.lpBottom);
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
  TextOpenPrice.Text := APrice.ToString;

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

  DoDecisionPrice;
//  DoLimitProfit;
end;

procedure TCharTradeFrame.DoDecisionPrice;
var
  xMaxP, xMinP: Double;
begin
  {todo: Список объекта работы}
  xMaxP := FOpenPrice + FDecisionValue * FStepPrice;
  xMinP := FOpenPrice - FDecisionValue * FStepPrice;
  if (FCurrentPrice > xMaxP) then
  begin
    // ----------------------------
    // Пересечение верхнего предела
    // Сигнал на усреднение
    FOpenPrice := xMaxP;
    FMaxPrice := GetMaxPrice(FOpenPrice);
    FMinPrice := GetMinPrice(FOpenPrice);
    if Assigned(FOnLimitPrice) then
      FOnLimitPrice(Self,xMaxP,TTypeLinePosition.lpTop);
  end else if (FCurrentPrice < xMinP) then
  begin
    // ---------------------------
    // Пересечение нижнего предела
    // Сигнал на усреднение
    FOpenPrice := xMinP;
    FMaxPrice := GetMaxPrice(FOpenPrice);
    FMinPrice := GetMinPrice(FOpenPrice);
    if Assigned(FOnLimitPrice) then
      FOnLimitPrice(Self,xMinP,TTypeLinePosition.lpBottom);
  end;
end;

end.
