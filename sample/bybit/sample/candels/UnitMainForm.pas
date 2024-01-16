(******************************************************************************)
(* Получение значение свечей                                                  *)
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
end;

destructor TMainForm.Destroy;
begin
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
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  BybitCandels.Stop;
end;

procedure TMainForm.CandelsOnChange(Sender: TObject);
  // нужно событие получение свячили
  // Чтобы повторно не отзаявку

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


begin
  _ChartFrame;
  ChartFrame.Build;
end;

procedure TMainForm.CandelsOnNewCandel(Sender: TObject);
begin

end;

end.
