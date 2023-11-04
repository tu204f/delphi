unit UnitCharTradeFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts;

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
  public const
    LMT_PRICE = 0.2;
    MAX_PRICE = LMT_PRICE;
    MIN_PRICE = LMT_PRICE;
  private
    FOpenPrice: Double; // Цена открытие позции
    FMaxPrice: Double;  // Максимальная цена
    FMinPrice: Double;  // Минимальаня цена
    FBuySell: Char;
    function GetPositionToPrice(const APrice: Double): Double;
    procedure SetPositionOpenPrice(const APrice: Double);
    procedure SetPositionUpDatePrice(const APrice: Double);
    procedure SetPositionRectanglePrice(const APriceTop, APriceBottom: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    ///<summary>Цена открытие сделки</summary>
    procedure Open(const APrice: Double; const ABuySell: Char);

    ///<summary>Обновление состояние сделки</summary>
    procedure UpDate(const APrice: Double);

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
end;

destructor TCharTradeFrame.Destroy;
begin

  inherited;
end;

function TCharTradeFrame.GetPositionToPrice(const APrice: Double): Double;
begin
  Result := Self.Height * (1 - (APrice - FMinPrice) /(FMaxPrice - FMinPrice));
end;

procedure TCharTradeFrame.Open(const APrice: Double; const ABuySell: Char);

  function _GetMaxPrice(const APrice: Double): Double;
  begin
    Result := APrice * (1 + MAX_PRICE);
  end;

  function _GetMinPrice(const APrice: Double): Double;
  begin
    Result := APrice * (1 - MAX_PRICE);
  end;

begin
  FOpenPrice := APrice;
  FBuySell := ABuySell;
  FMaxPrice := _GetMaxPrice(FOpenPrice);
  FMinPrice := _GetMinPrice(FOpenPrice);
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
    0,
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
    0,
    xPriceY,
    RectangleText.Width,
    21
  );
  TextUpDatePrice.Visible := True;
end;

procedure TCharTradeFrame.UpDate(const APrice: Double);
var
  xPriceTop, xPriceBottom: Double;
begin
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
end;

end.
