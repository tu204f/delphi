unit UnitMainFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMXTee.Engine, FMXTee.Series, FMXTee.Procs,
  FMXTee.Chart, FMX.ScrollBox, FMX.Grid, FMX.TabControl, FMX.Layouts,
  FMX.Controls.Presentation,

  Lb.TradeMan,
  Lb.SysUtils,
  Lb.Block, // ���� ������
  Lb.Sources,
  Lb.SysUtils.Candel,
  Lb.JournalTrades.V2, // ������ ������

  UnitChartCandelsFrame,
  UnitCharTradeFrame,
  UnitCandelFrame,
  UnitChartFrame,

  FMX.Memo.Types,
  FMX.Memo,
  FMX.Objects,
  FMX.ListBox;

type
  /// <summary>�������� ������ ��������</summary>
  TMainFrame = class(TFrame)
    Timer: TTimer;
    ProgressBar: TProgressBar;
    LayoutSource: TLayout;
    RectangleBar: TRectangle;
    TextBar: TText;
    LayoutBottom: TLayout;
    Layout1: TLayout;
    Chart1: TChart;
    Series1: TLineSeries;
    procedure TimerTimer(Sender: TObject);
    procedure BottonLearnClick(Sender: TObject);
  protected
    IndexBar: Integer; // ����� �����
  protected
    Block: TBlock; // ���� ��� �������� �������, ����� ������� ���������
    ChartFrame: TChartFrame;

    procedure TradeGrid; // ����� ���������� �� �������
    procedure SetWriteBlock(const AIndex: Integer);
  protected
    FStockMarket: IStockMarket;
    FLogger: ILogger;
    BufferLogs: TStrings;
    procedure LogBuffer(S: String);
    procedure Log(S: String);
    procedure LogBlock(ABlock: TBlock);
  protected
    TradeMan: TTradeMan;
    Candels: TCandels;
    MemoryCandels: TMemoryCandels; // �������� ������
    procedure TradeManOnClosePosition(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Logger: ILogger write FLogger;
    property StockMarket: IStockMarket write FStockMarket;

    procedure DoBegin; // ������ ���������
    procedure DoEnd; // ����� ���������

  end;

implementation

{$R *.fmx}

{ TMainFrame }

constructor TMainFrame.Create(AOwner: TComponent);

  procedure _AddColumn(AStrinGrid: TStringGrid; const AName: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(AStrinGrid);
    xColumn.Header := AName;
    xColumn.Parent := AStrinGrid;
  end;

begin
  inherited;

  // _AddColumn(StrGrid,'PriceOpen');
  // _AddColumn(StrGrid,'PriceClose');
  // _AddColumn(StrGrid,'TakeProfit');
  // _AddColumn(StrGrid,'StopLoss');
  // _AddColumn(StrGrid,'BuySell');
  // _AddColumn(StrGrid,'Quantity');
  // _AddColumn(StrGrid,'Profit');

  FLogger := nil;


  // Chart := TChartCandelsFrame.Create(nil);
  // Chart.Parent := LayoutSource;
  // Chart.Align := TAlignLayout.Client;

  Block := TBlock.Create;
  Candels := TCandels.Create;

  BufferLogs := TStringList.Create;

  TradeMan := TTradeMan.Create;
  TradeMan.OnClosePosition := TradeManOnClosePosition;

  MemoryCandels := TMemoryCandels.Create;

  ChartFrame := TChartFrame.Create(nil);
  ChartFrame.Parent := LayoutSource;
  ChartFrame.Align := TAlignLayout.Client;
  ChartFrame.Capacity := TBlock.BLOCK_SIZE;

end;

destructor TMainFrame.Destroy;
begin
  FreeAndNil(Candels);

  FreeAndNil(MemoryCandels);
  FreeAndNil(TradeMan);

  FreeAndNil(BufferLogs);
  FreeAndNil(Block);
  // FreeAndNil(Chart);

  FreeAndNil(ChartFrame);

  inherited;
end;

procedure TMainFrame.DoBegin;
begin

  // ����������� ��������� ��������� ��������
  TradeMan.Deposit := 100;
  TradeMan.Period := 3;
  TradeMan.TypeTrade := TTradeMan.TTypeTrade.ttTrend;
  TradeMan.TrailingStop := 500;
  TradeMan.Leverage := 10;

  if Assigned(FStockMarket) then
    FStockMarket.DoBegin;

  MemoryCandels.FileName :=
    'd:\work\git\delphi\sample\neoro\tmp_smpl_2\data\KRME.XXBTZUSD_220101_221231.csv';

  IndexBar := 1;
  MemoryCandels.CandelsFirstOneStep(TBlock.BLOCK_SIZE, Block.Candels);
  Timer.Enabled := True;
end;

procedure TMainFrame.DoEnd;
begin
  Timer.Enabled := False;

  if Assigned(FStockMarket) then
    FStockMarket.DoEnd;
end;

procedure TMainFrame.SetWriteBlock(const AIndex: Integer);

// ��������� ������� ����
  procedure _SetWriteBlock(const AIndex: Integer; const ABlock: TBlock);
  var
    iCount: Integer;
  begin
    iCount := TBlock.BLOCK_SIZE;
    MemoryCandels.CandelsFirstOneStep(iCount, ABlock.Candels);
  end;

begin
  //
end;

procedure TMainFrame.TradeGrid;
// var
// xTrade: TControlTrade;
// i, iCount, xInd: Integer;
begin
  // (*
  // _AddColumn(StrGrid,'PriceOpen');
  // _AddColumn(StrGrid,'PriceClose');
  // _AddColumn(StrGrid,'TakeProfit');
  // _AddColumn(StrGrid,'StopLoss');
  // _AddColumn(StrGrid,'BuySell');
  // _AddColumn(StrGrid,'Quantity');
  // _AddColumn(StrGrid,'Profit');
  // *)
  //
  //
  // iCount := ManagerTrade.ControlTrades.Count;
  // xInd := iCount - 10;
  // if xInd < 0 then
  // xInd := 0;
  //
  // if iCount > 10 then
  // iCount := 10;
  //
  // StrGrid.RowCount := iCount;
  // for i := 0 to iCount - 1 do
  // begin
  // xTrade := ManagerTrade.ControlTrades[xInd + i];
  // StrGrid.Cells[0,i] := xTrade.PriceOpen.ToString;
  // StrGrid.Cells[1,i] := xTrade.PriceClose.ToString;
  // StrGrid.Cells[2,i] := '';
  // StrGrid.Cells[3,i] := '';
  // StrGrid.Cells[4,i] := xTrade.BuySell;
  // StrGrid.Cells[5,i] := xTrade.Quantity.ToString;
  // StrGrid.Cells[6,i] := xTrade.Profit.ToString;
  // end;
end;

procedure TMainFrame.Log(S: String);
begin
  { �������� �������� }
  if Assigned(FLogger) then
    FLogger.Log(S);
end;

procedure TMainFrame.LogBlock(ABlock: TBlock);
var
  xS: String;
  xCandel: TCandel;
  i, iCount: Integer;
begin
  xS := Format('������� ����: %d', [ABlock.Candels.Count]);
  Log(xS);

  iCount := ABlock.Candels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := ABlock.Candels[i];
      Log(xCandel.ToStringCandel);
    end;
end;

procedure TMainFrame.LogBuffer(S: String);
// var
// iCount: Integer;
begin
  // BufferLogs.Insert(0,S);
  // iCount := BufferLogs.Count;
  // while iCount > 10 do
  // begin
  // iCount := iCount - 1;
  // BufferLogs.Delete(iCount);
  // end;
  // MemoLog.Lines.Assign(BufferLogs);
end;

procedure TMainFrame.BottonLearnClick(Sender: TObject);
begin
  if Timer.Enabled then
    DoEnd
  else
    DoBegin;
end;

procedure ChartCandels(AChart: TChartCandelsFrame; ABlock: TBlock);
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  AChart.Candels.Clear;
  iCount := ABlock.Candels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := ABlock.Candels[i];
      AChart.Candels.Add(xCandel);
    end;
  AChart.BuildChart;
end;

procedure ChartCandels_2(AChartFrame: TChartFrame; ACandels: TCandels);
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  AChartFrame.Candels.Clear;
  iCount := ACandels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := ACandels[i];
      AChartFrame.Candels.Add(xCandel);
    end;
  AChartFrame.Build;
end;

{
  ChartFrame: TChartFrame;

  TradeMan.Deposit := 100;
  TradeMan.Period := 10;
  TradeMan.TypeTrade := TTradeMan.TTypeTrade.ttTrend;
  TradeMan.TrailingStop := 20;

  property OpenPrice: Double read FOpenPrice;
  property Quantity: Double read FQuantity;
  property BuySell: Char read FBuySell;
  property StopPrice: Double read FStopPrice;

}

procedure TMainFrame.TimerTimer(Sender: TObject);

  procedure _ReversData;
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    Candels.Clear;
    iCount := Block.Candels.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xCandel := Block.Candels[i];
        Candels.Add(xCandel);
      end;
  end;

var
  xValue: Double;
begin
  _ReversData;
  if Candels.Count = 0 then
    Exit;

  try
    if MemoryCandels.CandelEOF then
    begin
      DoEnd;
    end
    else
    begin
      xValue := TBlockAPI.MathExpectation(Block);
      TextBar.Text := '��������: ' +  xValue.ToString;

      Series1.AddY(xValue);
      // IndexBar.ToString + ' ' + Block.CandelLast.ToString;


//      TradeMan.SetPriceLast(Candels[0].Close);
//      TradeMan.CandelNew(Candels[0].Close);
//      _InfoTradeMan(TradeMan);
//      TradeMan.SetInputBlock(Candels);

      // ChartCandels(Chart,Block);
      ChartCandels_2(ChartFrame, Candels);
//
//      if TradeMan.IsPosition then
//      begin
//        ChartFrame.OpenAndClosePrice(
//          TradeMan.Position.OpenPrice,
//          Candels[0].Close,
//          TradeMan.Position.StopPrice,
//          TradeMan.Position.BuySell
//        );
//      end
//      else
//      begin
//        ChartFrame.SupportAndResistance(TradeMan.MinPrice, TradeMan.MaxPrice);
//      end;

      LogBlock(Block);

      Inc(IndexBar);
      MemoryCandels.CandelsNextOneStep(TBlock.BLOCK_SIZE, Block.Candels);
      ProgressBar.Value := MemoryCandels.Progress;

    end;
  except
    DoEnd;
  end;
end;

procedure TMainFrame.TradeManOnClosePosition(Sender: TObject);
begin

end;

end.
