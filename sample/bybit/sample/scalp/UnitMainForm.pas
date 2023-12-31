(******************************************************************************)
(*  *)
(******************************************************************************)
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

  FMX.Edit,

  Lb.SysUtils,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Objects,

  Lb.Bot.Tiket, FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart;

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
    MemoLog: TMemo;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    ChartFrame: TChartFrame;
    BybitCandels: TBybitCandels;
    procedure CandelsOnChange(Sender: TObject);
    procedure CandelsOnNewCandel(Sender: TObject);
    procedure BotOnClosePosition(Sender: TObject);
  public
    Bot: TTakeProfitTiketBot;
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

  Bot := TTakeProfitTiketBot.Create;
  Bot.Quantity := 0.02;
  Bot.StopLoss := -100;
  Bot.CountStop := 3;
  Bot.TakeProfit := 300;

  Bot.OnClosePosition := BotOnClosePosition;

  BybitCandels := TBybitCandels.Create;
  BybitCandels.OnChange := CandelsOnChange;
  BybitCandels.OnNewCandel := CandelsOnNewCandel;

  ChartFrame := TChartFrame.Create(nil);
  ChartFrame.Align := TAlignLayout.Client;
  ChartFrame.Parent := RectangleChart;
  ChartFrame.Capacity := TRADE_CAPACITY;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Bot);
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
    TTypeInterval.ti_1
  );

  Bot.Clear;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  BybitCandels.Stop;
end;

procedure _ChartFrame(AChartFrame: TChartFrame; ABybitCandels: TBybitCandels);
var
  xCandel:  TCandel;
  i, iCount: Integer;
begin
  AChartFrame.Candels.Clear;
  iCount := ABybitCandels.Sources.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := ABybitCandels.Sources[i];
      AChartFrame.Candels.Add(xCandel);
    end;
  AChartFrame.Build;
end;


procedure TMainForm.CandelsOnChange(Sender: TObject);
var
  xCandel: TCandel;
  xOpenPrice, xStopPrice, xTakePrice: Double;
begin
  _ChartFrame(ChartFrame,BybitCandels);
  xCandel := BybitCandels.Sources.Items[0];

  if Bot.IsPosition then
    Bot.SetUpPosition(xCandel.Close)
  else
    Bot.SetOpenPosition(xCandel.Close);

  ListBoxTradeMan.Items[0] := 'Price: ' + xCandel.Close.ToString;
  ListBoxTradeMan.Items[1] := 'OpenPrice: ' + Bot.OpenPrice.ToString;
  ListBoxTradeMan.Items[2] := 'HealthPoints: ' + Bot.HealthPoints.ToString;
end;

procedure TMainForm.CandelsOnNewCandel(Sender: TObject);
var
  xCandel: TCandel;
begin
  if Bot.IsPosition then
  begin
    xCandel := BybitCandels.Sources[0];
    Bot.SetClosePosition(xCandel.Close);
  end;

  // Определяем напровление
  xCandel := BybitCandels.Sources.Items[1];
  Bot.SetMode(xCandel);
end;

procedure TMainForm.BotOnClosePosition(Sender: TObject);
begin
  MemoLog.Lines.Add('HealthPoints: ' + Bot.HealthPoints.ToString);
end;

end.
