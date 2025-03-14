unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indicator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  Lb.Journal.Trading.v2,

  FMX.Objects,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Menus,
  FMX.Edit, FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart,
  FMX.ListBox;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    Rectangle: TRectangle;
    TextStatus: TText;
    Layout: TLayout;
    GridLayout: TGridPanelLayout;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    ButtonClose: TButton;
    TabControl1: TTabControl;
    TabItemTrade: TTabItem;
    TabItemPosition: TTabItem;
    DemoGrid: TStringGrid;
    StringGridCandel: TStringGrid;
    PopupMenu: TPopupMenu;
    MenuItemSaveFile: TMenuItem;
    TabItemReal: TTabItem;
    RealGrid: TStringGrid;
    LayoutDemo: TLayout;
    EditDemo1: TEdit;
    EditDemo2: TEdit;
    EditDemo3: TEdit;
    LayoutReal: TLayout;
    EditReal1: TEdit;
    EditReal2: TEdit;
    EditReal3: TEdit;
    Chart: TChart;
    SeriesValueWR: TLineSeries;
    SeriesValueMa2: TLineSeries;
    SeriesValueMa1: TLineSeries;
    LayoutMarket: TLayout;
    RectangleMarket: TRectangle;
    GridPanelPrice: TGridPanelLayout;
    Text1: TText;
    EditWR: TEdit;
    EditFastWR: TEdit;
    EditSlowWR: TEdit;
    LayoutTrend: TLayout;
    RectangleTrend: TRectangle;
    Text2: TText;
    ListBox: TListBox;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    procedure SetRealPosition(const ADemoPosition: TJournalPosition);
    procedure TradingPlatformOnNewCandel(Sender: TObject);
  protected
    procedure DoStart;
    procedure DoStop;
    procedure Strategy;
    procedure PositionClose(ASander: TObject);
  public
    TypeDirection: TTypeDirection;
    DemoJournalManager: TJournalManager;
    RealJournalManager: TJournalManager;
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.DateUtils;

constructor TMainForm.Create(AOwner: TComponent);

  procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AStrGrid;
    xCol.Header := AHeader;
    xCol.Width  := AWidth;
  end;

begin
  inherited;

  SetAddColumn(StringGridCandel,'Time',150);
  SetAddColumn(StringGridCandel,'Open');
  SetAddColumn(StringGridCandel,'High');
  SetAddColumn(StringGridCandel,'Low');
  SetAddColumn(StringGridCandel,'Close');
  SetAddColumn(StringGridCandel,'Vol');
  SetAddColumn(StringGridCandel,'WR');
  SetAddColumn(StringGridCandel,'MaRSI');
  SetAddColumn(StringGridCandel,'Ma2RSI');


  SetAddColumn(DemoGrid,'id',50);
  SetAddColumn(DemoGrid,'OpenTime',120);
  SetAddColumn(DemoGrid,'OpenPrice');
  SetAddColumn(DemoGrid,'CloseTime',120);
  SetAddColumn(DemoGrid,'ClosePrice');
  SetAddColumn(DemoGrid,'Qty');
  SetAddColumn(DemoGrid,'Side');
  SetAddColumn(DemoGrid,'SL');
  SetAddColumn(DemoGrid,'TK');
  SetAddColumn(DemoGrid,'Profit');
  SetAddColumn(DemoGrid,'TypeTrade');
  SetAddColumn(DemoGrid,'FeeRatesTaker');
  SetAddColumn(DemoGrid,'FeeRatesMaker');

  SetAddColumn(RealGrid,'id',50);
  SetAddColumn(RealGrid,'OpenTime',120);
  SetAddColumn(RealGrid,'OpenPrice');
  SetAddColumn(RealGrid,'CloseTime',120);
  SetAddColumn(RealGrid,'ClosePrice');
  SetAddColumn(RealGrid,'Qty');
  SetAddColumn(RealGrid,'Side');
  SetAddColumn(RealGrid,'SL');
  SetAddColumn(RealGrid,'TK');
  SetAddColumn(RealGrid,'Profit');
  SetAddColumn(RealGrid,'TypeTrade');
  SetAddColumn(RealGrid,'FeeRatesTaker');
  SetAddColumn(RealGrid,'FeeRatesMaker');

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  TradingPlatform.OnNewCandel := TradingPlatformOnNewCandel;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_60;

  DemoJournalManager := TJournalManager.Create;
  RealJournalManager := TJournalManager.Create;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(RealJournalManager);
  FreeAndNil(DemoJournalManager);
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    TypeDirection := TTypeDirection.tdNull;

    ButtonStartOrStop.Text := '����';
    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 1;
    TradingPlatform.Start;
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := '�����';
    TradingPlatform.Stop;
  end;
end;


procedure TMainForm.ButtonStartOrStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.Strategy;
begin

end;

procedure TMainForm.PositionClose(ASander: TObject);
begin
  {todo: ��������� � ��������}
end;

procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);

  procedure _SetUpDataPosition(AJournalManager: TJournalManager);
  var
    i, iCount: Integer;
    xPosition: TJournalPosition;
  begin
    iCount := AJournalManager.Positions.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xPosition := AJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttOpen then
        begin
          case xPosition.Side of
            TTypeBuySell.tsBuy: xPosition.SetUpData(TradingPlatform.StateMarket.Bid);
            TTypeBuySell.tsSell: xPosition.SetUpData(TradingPlatform.StateMarket.Ask);
          end;
        end;
      end;
  end;

  procedure _ShowCandel;
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    iCount := AStateMarket.Candels.Count;
    StringGridCandel.RowCount := iCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := AStateMarket.Candels[i];
        StringGridCandel.Cells[0,i] := DateTimeToStr(UnixToDateTime(xCandel.Time));
        StringGridCandel.Cells[1,i] := xCandel.Open.ToString;
        StringGridCandel.Cells[2,i] := xCandel.High.ToString;
        StringGridCandel.Cells[3,i] := xCandel.Low.ToString;
        StringGridCandel.Cells[4,i] := xCandel.Close.ToString;
        StringGridCandel.Cells[5,i] := xCandel.Vol.ToString;
        StringGridCandel.Cells[6,i] := TradingPlatform.ValuesW.ValuesWR[i].ToString;
        StringGridCandel.Cells[7,i] := TradingPlatform.ValuesW.ValuesMa1[i].ToString;
        StringGridCandel.Cells[8,i] := TradingPlatform.ValuesW.ValuesMa2[i].ToString;
      end;
  end;

  procedure _ShowPosition(const AGrid: TStringGrid; const AJournalManager: TJournalManager);
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
  begin
    Count := AJournalManager.Positions.Count;
    AGrid.RowCount := Count;

    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := AJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttClose then
          xPosition.SetUpdata;

        AGrid.Cells[0,i] := (i + 1).ToString;
        AGrid.Cells[1,i] := DateTimeToStr(xPosition.OpenTime);
        AGrid.Cells[2,i] := FloatToStr(xPosition.OpenPrice);

        if xPosition.ClosePrice = 0 then
        begin
          AGrid.Cells[3,i] := '';
          AGrid.Cells[4,i] := '';
        end else
        begin
          AGrid.Cells[3,i] := DateTimeToStr(xPosition.CloseTime);
          AGrid.Cells[4,i] := FloatToStr(xPosition.ClosePrice);
        end;

        AGrid.Cells[5,i] := FloatToStr(xPosition.Qty);
        AGrid.Cells[6,i] := GetStrToSide(xPosition.Side);
        AGrid.Cells[7,i] := FloatToStr(xPosition.StopLoss);
        AGrid.Cells[8,i] := FloatToStr(xPosition.TakeProfit);
        AGrid.Cells[9,i] := FloatToStr(xPosition.Profit);
        AGrid.Cells[10,i] := GetStrToTypeTrade(xPosition.TypeTrade);
        AGrid.Cells[11,i] := FloatToStr(xPosition.ProfitFeeRatesTaker);
        AGrid.Cells[12,i] := FloatToStr(xPosition.ProfitFeeRatesMaker);
      end;
  end;

  procedure _ShowProfit(const AJournalManager: TJournalManager; var ASumValue1, ASumValue2, ASumValue3: Double);
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
  begin
    ASumValue1 := 0;
    ASumValue2 := 0;
    ASumValue3 := 0;
    Count := AJournalManager.Positions.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := AJournalManager.Positions[i];
        ASumValue1 := ASumValue1 + xPosition.Profit;
        ASumValue2 := ASumValue2 + xPosition.ProfitFeeRatesTaker;
        ASumValue3 := ASumValue3 + xPosition.ProfitFeeRatesMaker;
      end;
  end;


  procedure _ShowChart;
  var
    i, iCount: Integer;
    xValueW, xValueMa1, xValueMa2: Double;
  begin
    SeriesValueWR.Clear;
    SeriesValueMa1.Clear;
    SeriesValueMa2.Clear;

    iCount := AStateMarket.Candels.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xValueW := TradingPlatform.ValuesW.ValuesWR[i];
        xValueMa1 := TradingPlatform.ValuesW.ValuesMA1[i];
        xValueMa2 := TradingPlatform.ValuesW.ValuesMA2[i];

        SeriesValueWR.Add(xValueW);
        SeriesValueMa1.Add(xValueMa1);
        SeriesValueMa2.Add(xValueMa2);
      end;

  end;

  procedure _InfoPanel;
  begin
    if TradingPlatform.ValuesW.ValueWR < 50 then
    begin
      RectangleTrend.Fill.Color := TAlphaColorRec.Green
    end
    else
    begin
      RectangleTrend.Fill.Color := TAlphaColorRec.Red
    end;
  end;

var
  xSumValue1, xSumValue2, xSumValue3: Double;
begin
  // ***********************************************
  // ������ ��������� �����
  TextStatus.Text :=
    'Price: ' + TradingPlatform.StateMarket.Ask.ToString + '/' + TradingPlatform.StateMarket.Bid.ToString + '; ' +
    'ValueRSI: ' + TradingPlatform.ValuesW.ValuesWR.ToString + ';';

  EditWR.Text := TradingPlatform.ValuesW.ValueWR.ToString;
  EditFastWR.Text := TradingPlatform.ValuesW.FastValueMa.ToString;
  EditSlowWR.Text := TradingPlatform.ValuesW.SlowValueMa.ToString;

  _ShowCandel;

  Strategy;

  _SetUpDataPosition(DemoJournalManager);
  _SetUpDataPosition(RealJournalManager);
  _ShowPosition(DemoGrid, DemoJournalManager);
  _ShowPosition(RealGrid, RealJournalManager);


  xSumValue1 := 0.0;
  xSumValue2 := 0.0;
  xSumValue3 := 0.0;
  _ShowProfit(DemoJournalManager, xSumValue1, xSumValue2, xSumValue3);

  EditDemo1.Text := xSumValue1.ToString;
  EditDemo2.Text := xSumValue2.ToString;
  EditDemo3.Text := xSumValue3.ToString;


  _ShowProfit(RealJournalManager, xSumValue1,xSumValue2,xSumValue3);
  EditReal1.Text := xSumValue1.ToString;
  EditReal2.Text := xSumValue2.ToString;
  EditReal3.Text := xSumValue3.ToString;

  _ShowChart;
  _InfoPanel;
end;


procedure TMainForm.SetRealPosition(const ADemoPosition: TJournalPosition);
var
  xPosition: TJournalPosition;
begin
  xPosition := RealJournalManager.GetCreateJournalPosition;
  xPosition.OpenTime := ADemoPosition.OpenTime;
  xPosition.OpenPrice := ADemoPosition.OpenPrice;
  xPosition.Qty := ADemoPosition.Qty;
  xPosition.Side := ADemoPosition.Side;
  xPosition.IsActive := ADemoPosition.IsActive;
  xPosition.TypeTrade := ADemoPosition.TypeTrade;
  xPosition.Triling := ADemoPosition.Triling;
  case xPosition.Side of
    TTypeBuySell.tsBuy: xPosition.TakeProfit := xPosition.OpenPrice + 10;
    TTypeBuySell.tsSell: xPosition.TakeProfit := xPosition.OpenPrice - 10;
  end;
  xPosition.DoOpen;
end;

procedure TMainForm.TradingPlatformOnNewCandel(Sender: TObject);
var
  //xValueWR: Double;
  xFastValueMa: Double;
  //xSlowValueMa: Double;
begin
  // ��������� ������� ������ �������
  // xValueWR  := TradingPlatform.ValuesW.ValueWR;
  xFastValueMa := TradingPlatform.ValuesW.FastValueMa;
  // xSlowValueMa := TradingPlatform.ValuesW.SlowValueMa;




  if xFastValueMa > 50 then
    ButtonBuyClick(nil)
  else if xFastValueMa < 50 then
    ButtonSellClick(nil);
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
begin
  xPrice := TradingPlatform.StateMarket.Ask;
  if xPrice > 0 then
  begin
    xPosition := DemoJournalManager.GetCreateJournalPosition;
    xPosition.OnClose := PositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := xPrice;
      Qty := 1;
      Side := TTypeBuySell.tsBuy;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := 30;
      DoOpen;
    end;
    SetRealPosition(xPosition);
  end;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
begin
  xPrice := TradingPlatform.StateMarket.Bid;
  if xPrice > 0 then
  begin
    xPosition := DemoJournalManager.GetCreateJournalPosition;
    xPosition.OnClose := PositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := xPrice;
      Qty := 1;
      Side := TTypeBuySell.tsSell;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := 30;
      DoOpen;
    end;
    SetRealPosition(xPosition);
  end;
end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  // ������� �������
  iCount := DemoJournalManager.Positions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := DemoJournalManager.Positions[i];
      if xPosition.TypeTrade = TTypeTrade.ttOpen then
      begin
        with xPosition do
        begin
          case Side of
            TTypeBuySell.tsBuy: xPrice := TradingPlatform.StateMarket.Bid;
            TTypeBuySell.tsSell: xPrice := TradingPlatform.StateMarket.Ask;
          else
            xPrice := 0;
          end;

          if xPrice <= 0 then
            Exit;

          CloseTime := GetNewDateTime;
          ClosePrice := xPrice;
          IsActive := False;
          TypeTrade := TTypeTrade.ttClose;
          DoClose;
        end;
      end;
    end;
end;


end.
