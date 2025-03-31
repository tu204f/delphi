unit UnitWorkBotFrame;

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
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,

  Lb.Platform,
  Lb.SysUtils,
  Lb.Bot.Candel,
  Lb.Breakdown;

type
  TWorkBotFrame =  class(TFrame)
    Rectangle: TRectangle;
    RectangleBody: TRectangle;
    Layout: TLayout;
    LayoutHigh: TLayout;
    LayoutLow: TLayout;
    LineHigh: TLine;
    LineLow: TLine;
    LinePriceHigh: TLine;
    LinePriceLow: TLine;
    TextPriceLow: TText;
    TextPriceHigh: TText;
    LayoutPrice: TLayout;
    LineOpen: TLine;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  public const
    LEFT_CONTROL = 10;
    LINE_PRICE = 23;
  private
    FOpenPrice: Double;
    FOpenBuySell: Char;
  private
    FRateMax: Double;
    FBreakdown: TBreakdown;
    FTradingPlatform: TTradingPlatform;
    procedure SetBreakdown(const ABreakdown: TBreakdown);
    procedure SetParamPriceLimit;
  protected
    FPriceHighMax: Double;
    FPriceLowMin: Double;
    function GetPriceToY(const APrice: Double): Single;
  protected
    procedure EventParamValue;
  public
    MainFormLog: IMainFormLog;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpDataWorkBot;
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
    property Breakdown: TBreakdown read FBreakdown write SetBreakdown;
  end;

implementation

{$R *.fmx}

{ TChartFrame }

constructor TWorkBotFrame.Create(AOwner: TComponent);
begin
  inherited;
  FBreakdown := nil;
  FTradingPlatform := nil;
  MainFormLog := nil;

  FOpenPrice := 0;
  FOpenBuySell := #0;

end;

destructor TWorkBotFrame.Destroy;
begin

  inherited;
end;

procedure TWorkBotFrame.SetBreakdown(const ABreakdown: TBreakdown);
begin
  FBreakdown := ABreakdown;
  //FWorkBot.OnCrossingValue := EventCrossingValue;
end;

procedure TWorkBotFrame.SetParamPriceLimit;
var
  xCandel: TCandel;
begin
  // Максимальный значение работы
  FPriceHighMax := FBreakdown.PriceHigh + FBreakdown.Deviation;
  FPriceLowMin  := FBreakdown.PriceLow  - FBreakdown.Deviation;

  xCandel := FBreakdown.Candel;
  if FPriceHighMax < xCandel.High then
    FPriceHighMax := xCandel.High + FBreakdown.Deviation;
  
  if FPriceLowMin > xCandel.Low then
    FPriceLowMin := xCandel.Low - FBreakdown.Deviation;
end;

function TWorkBotFrame.GetPriceToY(const APrice: Double): Single;
var
  xDelta, xDeltaPrice: Double;
begin
  if (FPriceLowMin > 0) and (FPriceHighMax > 0) then
  begin
    xDelta      := FPriceHighMax - FPriceLowMin;
    xDeltaPrice := FPriceHighMax - APrice;
    Result      := LayoutPrice.Height * xDeltaPrice / xDelta;
  end
  else
    Result := 0;
end;

procedure TWorkBotFrame.EventParamValue;

  function _MaxValue(const APrice1, APrice2: Double): Double;
  begin
    if APrice1 > APrice2 then
      Result := APrice1
    else
      Result := APrice2;
  end;

  function _MinValue(const APrice1, APrice2: Double): Double;
  begin
    if APrice1 > APrice2 then
      Result := APrice2
    else
      Result := APrice1;
  end;

  procedure _LayoutHighLowPriceMax;
  var
    xCandel: TCandel;
    xPriceHighY, xPriceLowY, xPriceOpenY, xPriceCloseY: Single;
  begin
    if not Assigned(FBreakdown) then
      Exit;

    {todo: Если вдруг приодовается предель. Сейчас это не так важно}
    xCandel      := FBreakdown.Candel;
    xPriceHighY  := GetPriceToY(xCandel.High);
    xPriceLowY   := GetPriceToY(xCandel.Low);
    xPriceOpenY  := GetPriceToY(_MinValue(xCandel.Open, xCandel.Close));
    xPriceCloseY := GetPriceToY(_MaxValue(xCandel.Open, xCandel.Close));

    Layout.SetBounds(
      LEFT_CONTROL,
      xPriceHighY,
      LayoutPrice.Width / 2 - LEFT_CONTROL,
      (xPriceLowY - xPriceHighY)
    );

    LineHigh.Width := LayoutHigh.Width/2;
    LineLow.Width := LayoutLow.Width/2;

    LayoutLow.Height  := xPriceLowY   - xPriceOpenY;
    LayoutHigh.Height := xPriceCloseY - xPriceHighY;

    if Layout.Height <= 0 then
    begin
      Layout.Height := 2;
      LayoutLow.Height := 0;
      LayoutHigh.Height := 0;
    end else if (Layout.Height <= (LayoutLow.Height + LayoutHigh.Height)) then
    begin
      LayoutLow.Height  := LayoutLow.Height - 1;
      LayoutHigh.Height := LayoutHigh.Height - 1;
    end;

    LineOpen.SetBounds(
      5 + LayoutPrice.Width / 2,
      LayoutPrice.Height / 2,
      LayoutPrice.Width / 3,
      2
    );

    case xCandel.TypeCandel of
      TTypeCandel.tcNull: RectangleBody.Fill.Color := TAlphaColorRec.Null;
      TTypeCandel.tcGreen: RectangleBody.Fill.Color := TAlphaColorRec.Green;
      TTypeCandel.tcRed: RectangleBody.Fill.Color := TAlphaColorRec.Red;
    end;

  end;

  procedure _LayoutHighLowPrice;
  var
    xPriceHighY, xPriceLowY: Single;
  begin
    TextPriceHigh.Text := FBreakdown.PriceHigh.ToString;
    TextPriceLow.Text  := FBreakdown.PriceLow.ToString;

    xPriceHighY := GetPriceToY(FBreakdown.PriceHigh) - LINE_PRICE;
    xPriceLowY  := GetPriceToY(FBreakdown.PriceLow);

    LinePriceHigh.SetBounds(
      LEFT_CONTROL,
      xPriceHighY,
      LayoutPrice.Width - LEFT_CONTROL,
      LINE_PRICE
    );

    LinePriceLow.SetBounds(
      LEFT_CONTROL,
      xPriceLowY,
      LayoutPrice.Width - LEFT_CONTROL,
      LINE_PRICE
    );
  end;

begin
  SetParamPriceLimit;

  _LayoutHighLowPriceMax;
  _LayoutHighLowPrice;
end;

procedure TWorkBotFrame.SetUpDataWorkBot;
begin
  EventParamValue;
end;


procedure TWorkBotFrame.Timer1Timer(Sender: TObject);
begin
  SetUpDataWorkBot;
end;

end.
