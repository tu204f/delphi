unit UnitTraderFrame;

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
  Lb.ReadPrice,
  Lb.PositionTrade,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMXTee.Engine,
  FMXTee.Series,
  FMXTee.Procs,
  FMXTee.Chart,
  FMX.ListBox,
  UnitChartDataFrame;

const
  COUNT_RSI = 14;

type
  TChartListBoxItem = class(TListBoxItem)
  private
    FChartDataFrame: TChartDataFrame;
  protected
    property ChartDataFrame: TChartDataFrame read FChartDataFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpCandel(ACandel: TCandel; ATypeTrande: TTypeTrande);
  end;

  TTraderFrame = class(TFrame)
    LayoutPrice: TLayout;
    TextDateTime: TText;
    EditDateTime: TEdit;
    TextPrice: TText;
    EditOpen: TEdit;
    LayoutClient: TLayout;
    EditClose: TEdit;
    EditLow: TEdit;
    EditHigh: TEdit;
    TextVol: TText;
    EditVol: TEdit;
    LayoutParam: TLayout;
    TextStatusMkt: TText;
    EditStatusMkt: TEdit;
    TextUp: TText;
    EditUp: TEdit;
    TextDown: TText;
    EditDown: TEdit;
    EditTrande: TEdit;
    Rectangle1: TRectangle;
    ListBoxCharts: TListBox;
    LayoutChartFrame: TLayout;
  private
    ValueRSI: Double;
    FractalUp: Double;
    FractalDown: Double;
    FCandel: TCandel;
    Candels: TCandelList;
    FTypeTrande: TTypeTrande;
    procedure SetTypeTrande(const Value: TTypeTrande);
  protected
    ChartFrame: TChartDataFrame;
    ChartItem: TChartListBoxItem;
    procedure SetLog(S: String);
    property Candel: TCandel read FCandel write FCandel;
    property TypeTrande: TTypeTrande read FTypeTrande write SetTypeTrande;
  public
    PositionTrade: TPositionTrade;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SetUpCandel(ACandel: TCandel);
    procedure SetStart;
  end;

implementation

{$R *.fmx}

{ TChartListBoxItem }

constructor TChartListBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  FChartDataFrame := TChartDataFrame.Create(nil);
  FChartDataFrame.Parent := Self;
  FChartDataFrame.Align := TAlignLayout.Client;
end;

destructor TChartListBoxItem.Destroy;
begin
  FreeAndNil(FChartDataFrame);
  inherited;
end;

procedure TChartListBoxItem.SetUpCandel(ACandel: TCandel; ATypeTrande: TTypeTrande);
begin
  FChartDataFrame.SetUpCandel(ACandel,ATypeTrande);
end;

{ TTraderFrame }

constructor TTraderFrame.Create(AOwner: TComponent);
begin
  inherited;
  Candels := TCandelList.Create;

  FTypeTrande := TTypeTrande.ttNull;

  ChartItem := nil;

  ChartFrame:= TChartDataFrame.Create(nil);
  ChartFrame.Parent := LayoutChartFrame;
  ChartFrame.Align := TAlignLayout.Client;

  PositionTrade := TPositionTrade.Create;
end;

destructor TTraderFrame.Destroy;
begin
  FreeAndNil(PositionTrade);
  FreeAndNil(ChartFrame);
  FreeAndNil(Candels);
  inherited;
end;

procedure TTraderFrame.Clear;
begin
  PositionTrade.Trades.Clear;
  Candels.Clear;
end;

procedure TTraderFrame.SetLog(S: String);
begin

end;


procedure TTraderFrame.SetStart;
begin
  TypeTrande := TTypeTrande.ttNull;

  ChartItem := TChartListBoxItem.Create(ListBoxCharts);
  ChartItem.Height := 225;
  ChartItem.Parent := ListBoxCharts;

  ChartFrame.Clear;
end;

procedure TTraderFrame.SetUpCandel(ACandel: TCandel);

  function _RoundTo(const AValue: Double): Double;
  begin
    Result := Round(AValue * 1000)/1000;
  end;

  procedure _ShowCandel(ACandel: TCandel);
  begin
    EditDateTime.Text := DateToStr(ACandel.Date) + ' ' + TimeToStr(ACandel.Time);
    EditOpen.Text  := ACandel.Open.ToString;
    EditHigh.Text  := ACandel.High.ToString;
    EditLow.Text   := ACandel.Low.ToString;
    EditClose.Text := ACandel.Close.ToString;
    EditVol.Text   := ACandel.Vol.ToString;
  end;

begin
  FCandel := ACandel;
  _ShowCandel(ACandel);

  Candels.Add(ACandel);
  if Candels.Count > COUNT_RSI then
    Candels.Delete(0);

  if Candels.Count >= COUNT_RSI then
  begin
    ValueRSI := GetRSI(Candels);
    SetFractal(Candels, FractalUp, FractalDown);
    ValueRSI := _RoundTo(ValueRSI);

    EditStatusMkt.Text := _RoundTo(ValueRSI).ToString;
    EditUp.Text := FractalUp.ToString;
    EditDown.Text := FractalDown.ToString;

    // Были смена тренда
    if ValueRSI > 50 then
    begin
      case FTypeTrande of
        ttNull: TypeTrande := TTypeTrande.ttLong;
        ttLong: ;
        ttShort: TypeTrande := TTypeTrande.ttLong;
      end;
    end
    else if ValueRSI < 50 then
    begin
      case TypeTrande of
        ttNull: TypeTrande := TTypeTrande.ttShort;
        ttLong: TypeTrande := TTypeTrande.ttShort;
        ttShort: ;
      end;
    end;
  end;

  ChartFrame.SetUpCandel(ACandel,TypeTrande);
  ChartItem.SetUpCandel(ACandel,TypeTrande);
end;

procedure TTraderFrame.SetTypeTrande(const Value: TTypeTrande);
begin
  FTypeTrande := Value;
  case FTypeTrande of
    ttNull: EditTrande.Text := 'null';
    ttLong: EditTrande.Text := 'long';
    ttShort: EditTrande.Text := 'short';
  end;
end;

end.
