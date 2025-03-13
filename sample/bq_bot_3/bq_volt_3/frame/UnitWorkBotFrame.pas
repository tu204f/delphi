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
  Lb.Bot.V4,
  Lb.Crossing;

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
    TextInfo: TText;
    LayoutPrice: TLayout;
    TextValueHL: TText;
    LineOpen: TLine;
  public const
    LEFT_CONTROL = 10;
    LINE_PRICE = 23;
  private
    FOpenPrice: Double;
    FOpenBuySell: Char;
  private
    FRateMax: Double;
    FWorkBot: TWorkBotDeviation;
    FTradingPlatform: TTradingPlatform;
    procedure SetWorkBot(const Value: TWorkBotDeviation);
    procedure SetParamPriceLimit;
  protected
    FPriceHighMax: Double;
    FPriceLowMin: Double;
    function GetPriceToY(const APrice: Double): Single;
  protected
    procedure EventParamValue(const AWorkBot: TWorkBotDeviation);
    procedure EventCrossingValue(ASender: TObject; APrice: Double; ATypeCrossing: TTypeCrossing);
  public
    MainFormLog: IMainFormLog;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TradingPlatform: TTradingPlatform read FTradingPlatform write FTradingPlatform;
    property WorkBot: TWorkBotDeviation read FWorkBot write SetWorkBot;
  end;

implementation

{$R *.fmx}

{ TChartFrame }

constructor TWorkBotFrame.Create(AOwner: TComponent);
begin
  inherited;
  FWorkBot := nil;
  FTradingPlatform := nil;
  MainFormLog := nil;

  FOpenPrice := 0;
  FOpenBuySell := #0;

end;

destructor TWorkBotFrame.Destroy;
begin

  inherited;
end;

procedure TWorkBotFrame.SetWorkBot(const Value: TWorkBotDeviation);
begin
  FWorkBot := Value;
  FWorkBot.OnCrossingValue := EventCrossingValue;
end;

procedure TWorkBotFrame.SetParamPriceLimit;
var
  xCandel: TCandel;
begin
  // Максимальный значение работы
  FPriceHighMax := FWorkBot.PriceHigh + FWorkBot.Deviation;
  FPriceLowMin  := FWorkBot.PriceLow  - FWorkBot.Deviation;

  xCandel := FWorkBot.Candel;
  if FPriceHighMax < xCandel.High then
    FPriceHighMax := xCandel.High + FWorkBot.Deviation;
  
  if FPriceLowMin > xCandel.Low then
    FPriceLowMin := xCandel.Low - FWorkBot.Deviation;
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

procedure TWorkBotFrame.EventParamValue(const AWorkBot: TWorkBotDeviation);

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
    if not Assigned(FWorkBot) then
      Exit;

    {todo: Если вдруг приодовается предель. Сейчас это не так важно}
    xCandel      := FWorkBot.Candel;
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
    TextPriceHigh.Text := FWorkBot.PriceHigh.ToString;
    TextPriceLow.Text  := FWorkBot.PriceLow.ToString;

    xPriceHighY := GetPriceToY(FWorkBot.PriceHigh) - LINE_PRICE;
    xPriceLowY  := GetPriceToY(FWorkBot.PriceLow);

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

var
  xProfit, xClosePrice: Double;
begin
  SetParamPriceLimit;

  _LayoutHighLowPriceMax;
  _LayoutHighLowPrice;

  case FOpenBuySell of
    'B': begin
      xClosePrice := FTradingPlatform.StateMarket.Bid;
      xProfit := xClosePrice - FOpenPrice;
    end;
    'S': begin
      FOpenBuySell := 'S';
      xClosePrice := FTradingPlatform.StateMarket.Ask;
      xProfit := FOpenPrice - xClosePrice;
    end;
  end;

  xProfit := GetRound(xProfit);
  TextInfo.Text := 'P[' + FOpenBuySell + ']: ' + xProfit.ToString;
  if xProfit > 0 then
  begin
    TextInfo.TextSettings.FontColor := TAlphaColorRec.Green;
  end
  else if xProfit < 0 then
  begin
    TextInfo.TextSettings.FontColor := TAlphaColorRec.Red;
  end
  else
  begin
    TextInfo.TextSettings.FontColor := TAlphaColorRec.Black;
  end;

  TextValueHL.Text := 'new_candel: ' + FWorkBot.CountValue.ToString;
end;

procedure TWorkBotFrame.EventCrossingValue(ASender: TObject; APrice: Double; ATypeCrossing: TTypeCrossing);
begin
  case ATypeCrossing of
    tcHigh: begin
      FOpenBuySell := 'B';
      FOpenPrice := FTradingPlatform.StateMarket.Ask;
    end;
    tcLow: begin
      FOpenBuySell := 'S';
      FOpenPrice := FTradingPlatform.StateMarket.Bid;
    end;
  end;
end;

end.
