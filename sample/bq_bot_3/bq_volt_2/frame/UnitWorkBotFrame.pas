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
  Lb.SysUtils,
  Lb.Bot.V2;

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
    LinePrice: TLine;
    Timer: TTimer;
    TextInfo: TText;
    GridLayout: TGridPanelLayout;
    LayoutPrice: TLayout;
    TextValueHL: TText;
    LineOpen: TLine;
    procedure TimerTimer(Sender: TObject);
  public const
    LEFT_CONTROL = 10;
    LINE_PRICE = 23;
  private
    { Private declarations }
    FCandel: TCandel;
    FDeviation: Double;
    FRateMax: Double;
    FRate: Double;
    FNewCandel: Integer;
  protected
    FPriceHighMax: Double;
    FPriceLowMin: Double;
    FPriceHigh: Double;
    FPriceLow: Double;
    function GetPriceToY(const APrice: Double): Single;
    procedure SetPositionCandel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParamValue(const ACandel: TCandel; const ADeviation, ARate: Double; const ARateMax: Double = 3);
    procedure SetNewCandel;
  public
    property Candel: TCandel read FCandel;
    property Deviation: Double read FDeviation;
    property Rate: Double read FRate;
  end;

implementation

{$R *.fmx}

{ TChartFrame }

constructor TWorkBotFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TWorkBotFrame.Destroy;
begin

  inherited;
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

procedure TWorkBotFrame.SetNewCandel;
begin
  FNewCandel := 0;
end;

procedure TWorkBotFrame.SetParamValue(const ACandel: TCandel; const ADeviation, ARate: Double; const ARateMax: Double);
begin
  Inc(FNewCandel);

  FCandel := ACandel;
  FDeviation := ADeviation;
  FRateMax := ARateMax;
  FRate := ARate;

  FPriceHighMax := FCandel.Open + FDeviation * FRateMax;
  FPriceLowMin  := FCandel.Open - FDeviation * FRateMax;;
  FPriceHigh    := FCandel.Open + FDeviation * FRate;
  FPriceLow     := FCandel.Open - FDeviation * FRate;

  if not Timer.Enabled then
    Timer.Enabled := True;

  TextInfo.Text :=
    ' ' +
    FCandel.Open.ToString + '/' +
    FCandel.High.ToString + '/' +
    FCandel.Low.ToString + '/' +
    FCandel.Close.ToString;

  TextValueHL.Text := 'new_candel: ' + FNewCandel.ToString;
end;

procedure TWorkBotFrame.TimerTimer(Sender: TObject);
begin
  SetPositionCandel;
end;

procedure TWorkBotFrame.SetPositionCandel;

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

  {$REGION 'Выводим график - значение'}
  procedure _LayoutHighLowPriceMax;
  var
    xPriceHighY, xPriceLowY, xPriceOpenY, xPriceCloseY: Single;
  begin
    {todo: Если вдруг приодовается предель. Сейчас это не так важно}
    xPriceHighY  := GetPriceToY(FCandel.High);
    xPriceLowY   := GetPriceToY(FCandel.Low);
    xPriceOpenY  := GetPriceToY(_MinValue(FCandel.Open,FCandel.Close));
    xPriceCloseY := GetPriceToY(_MaxValue(FCandel.Open,FCandel.Close));

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
    if (Layout.Height <= (LayoutLow.Height + LayoutHigh.Height)) then
    begin
      LayoutLow.Height  := 2;
      LayoutHigh.Height := 2;
    end;

    LineOpen.SetBounds(
      5 + LayoutPrice.Width / 2,
      LayoutPrice.Height / 2,
      LayoutPrice.Width / 2,
      2
    );

    if FCandel.Open < FCandel.Close then
      RectangleBody.Fill.Color := TAlphaColorRec.Green
    else if FCandel.Open > FCandel.Close then
      RectangleBody.Fill.Color := TAlphaColorRec.Red
    else
      RectangleBody.Fill.Color := TAlphaColorRec.Null;

  end;
  {$ENDREGION}

  {$REGION 'Пограничные цены'}
  procedure _LayoutHighLowPrice;
  var
    xPriceHighY, xPriceLowY: Single;
  begin
    TextPriceHigh.Text := FPriceHigh.ToString;
    TextPriceLow.Text  := FPriceLow.ToString;

    xPriceHighY := GetPriceToY(FPriceHigh) - LINE_PRICE;
    xPriceLowY  := GetPriceToY(FPriceLow);

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
  {$ENDREGION}

begin
  _LayoutHighLowPriceMax;
  _LayoutHighLowPrice;
end;

end.
