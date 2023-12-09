unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  System.JSON,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  FMX.Memo.Types,
  FMX.Memo,
  Lb.Bybit.Candels,

  UnitCandelFrame,
  UnitChartFrame,

  Lb.TradeMan,
  FMX.Edit,

  Lb.SysUtils, FMX.ListBox, FMX.TreeView, FMX.Objects;

type
  TMainForm = class(TForm)
    GridPanelLayout: TGridPanelLayout;
    LayoutTools: TLayout;
    GridPanelLayoutBottom: TGridPanelLayout;
    ButtonStart: TButton;
    ButtonStop: TButton;
    LayoutChart: TLayout;
    ListBoxTradeMan: TListBox;
    RectangleChart: TRectangle;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    TradeMan: TTradeMan;
    ChartFrame: TChartFrame;
    BybitCandels: TBybitCandels;
    procedure CandelsOnChange(Sender: TObject);
    procedure CandelsOnNewCandel(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.SysUtils.Candel,
  Lb.Setting;

const
  TRADE_CAPACITY = 50;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;

  BybitCandels := TBybitCandels.Create;
  BybitCandels.OnChange := CandelsOnChange;
  BybitCandels.OnNewCandel := CandelsOnNewCandel;

  ChartFrame := TChartFrame.Create(nil);
  ChartFrame.Align := TAlignLayout.Client;
  ChartFrame.Parent := RectangleChart;
  ChartFrame.Capacity := TRADE_CAPACITY;

  TradeMan := TTradeMan.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradeMan);
  FreeAndNil(ChartFrame);
  FreeAndNil(BybitCandels);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  BybitCandels.Start(
    'BTCUSDT',
    TRADE_CAPACITY,
    TTypeCategory.tcLinear,
    TTypeInterval.ti_5
  );
  TradeMan.Deposit      := TSetting.ReadInteger('config.system.deposit',100);
  TradeMan.Period       := TSetting.ReadInteger('config.system.period',10);
  TradeMan.TypeTrade    := TTradeMan.TTypeTrade.ttTrend;
  TradeMan.TrailingStop := TSetting.ReadInteger('config.system.trailing_stop',10);
  TradeMan.Leverage     := TSetting.ReadInteger('config.system.leverage',10);
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  BybitCandels.Stop;
end;

procedure TMainForm.CandelsOnChange(Sender: TObject);
  // нужно событие получение свячили
  // Чтобы повторно не отзаявку

  procedure _InfoTradeMan(ATradeMan: TTradeMan);
  begin
    ListBoxTradeMan.Items.Clear;
    with ListBoxTradeMan.Items do
    begin
      Add('Deposit ' + ATradeMan.Deposit.ToString);
      Add('Capital ' + ATradeMan.Capital.ToString);
      Add('Period ' + ATradeMan.Period.ToString);
      Add('TrailingStop ' + ATradeMan.TrailingStop.ToString);
      Add('Leverage ' + ATradeMan.Leverage.ToString);
      Add('PlusCount ' + ATradeMan.PlusCount.ToString + ' :: ' +
        Round(100 * ATradeMan.PlusCount / (ATradeMan.PlusCount +
        ATradeMan.MinusCount)).ToString);
      Add('MinusCount ' + ATradeMan.MinusCount.ToString + ' :: ' +
        Round(100 * ATradeMan.MinusCount / (ATradeMan.PlusCount +
        ATradeMan.MinusCount)).ToString);
      Add('MinusProfit ' + ATradeMan.MinusProfit.ToString);
      {
        property PlusCount: Integer read FPlusCount;
        property MinusCount: Integer read FMinusCount;
        property MinusProfit: Double read FMinusProfit;
      }
      if TradeMan.IsPosition then
      begin
        Add('OpenPrice ' + ATradeMan.Position.OpenPrice.ToString);
        Add('Quantity ' + ATradeMan.Position.Quantity.ToString);
        Add('BuySell ' + ATradeMan.Position.BuySell);
        Add('StopPrice ' + ATradeMan.Position.StopPrice.ToString);
      end;
    end;
  end;

  procedure _ChartFrame;
  var
    xCandel:  TCandel;
    i, iCount: Integer;
  begin
    ChartFrame.Candels.Clear;
    iCount := BybitCandels.Sources.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := BybitCandels.Sources[i];
        ChartFrame.Candels.Add(xCandel);
      end;
  end;

var
  xCandel:  TCandel;
  iCount: Integer;
begin

  _ChartFrame;
  iCount := BybitCandels.Sources.Count;
  if iCount > 0 then
  begin
    xCandel := BybitCandels.Sources[0];

    TradeMan.SetPriceLast(xCandel.Close);
    TradeMan.SetInputBlock(BybitCandels.Sources);

    _InfoTradeMan(TradeMan);
    if TradeMan.IsPosition then
    begin
      ChartFrame.OpenAndClosePrice(
        TradeMan.Position.OpenPrice,
        xCandel.Close,
        TradeMan.Position.StopPrice,
        TradeMan.Position.BuySell
      );
    end
    else
    begin
      ChartFrame.SupportAndResistance(
        TradeMan.MinPrice,
        TradeMan.MaxPrice
      );
    end;


  end;
  ChartFrame.Build;
end;

procedure TMainForm.CandelsOnNewCandel(Sender: TObject);
var
  xCandel:  TCandel;
begin
  xCandel := BybitCandels.Sources[0];
  TradeMan.CandelNew(xCandel.Close);
end;

end.
