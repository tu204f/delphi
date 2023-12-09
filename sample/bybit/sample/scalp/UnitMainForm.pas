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

  Lb.TradeMan,
  FMX.Edit,

  Lb.SysUtils,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Objects;

type
  TMainForm = class(TForm)
    GridPanelLayout: TGridPanelLayout;
    LayoutTools: TLayout;
    GridPanelLayoutBottom: TGridPanelLayout;
    ButtonStart: TButton;
    ButtonStop: TButton;
    LayoutChart5: TLayout;
    ListBoxTradeMan: TListBox;
    RectangleChart5: TRectangle;
    LayoutChart1: TLayout;
    RectangleChart1: TRectangle;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    ChartFrame5: TChartFrame;
    ChartFrame1: TChartFrame;
    BybitCandels5: TBybitCandels;
    BybitCandels1: TBybitCandels;
    procedure CandelsOnChange5(Sender: TObject);
    procedure CandelsOnChange1(Sender: TObject);
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

  BybitCandels5 := TBybitCandels.Create;
  BybitCandels5.OnChange := CandelsOnChange5;

  BybitCandels1 := TBybitCandels.Create;
  BybitCandels1.OnChange := CandelsOnChange1;

  ChartFrame1 := TChartFrame.Create(nil);
  ChartFrame1.Align := TAlignLayout.Client;
  ChartFrame1.Parent := RectangleChart1;
  ChartFrame1.Capacity := TRADE_CAPACITY;

  ChartFrame5 := TChartFrame.Create(nil);
  ChartFrame5.Align := TAlignLayout.Client;
  ChartFrame5.Parent := RectangleChart5;
  ChartFrame5.Capacity := TRADE_CAPACITY;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(ChartFrame1);
  FreeAndNil(ChartFrame5);
  FreeAndNil(BybitCandels1);
  FreeAndNil(BybitCandels5);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin

  BybitCandels5.Start(
    'BTCUSDT',
    TRADE_CAPACITY,
    TTypeCategory.tcLinear,
    TTypeInterval.ti_5
  );

  BybitCandels1.Start(
    'BTCUSDT',
    TRADE_CAPACITY,
    TTypeCategory.tcLinear,
    TTypeInterval.ti_1
  );

end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  BybitCandels1.Stop;
  BybitCandels5.Stop;
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

procedure TMainForm.CandelsOnChange5(Sender: TObject);
begin
  _ChartFrame(ChartFrame5,BybitCandels5);
end;

procedure TMainForm.CandelsOnChange1(Sender: TObject);
begin
  _ChartFrame(ChartFrame1,BybitCandels1);
end;

end.
