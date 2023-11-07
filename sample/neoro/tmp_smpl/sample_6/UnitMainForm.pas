unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Generics.Collections,
  Lb.Block,
  Lb.NeuronNet,
  UnitChartCandelsFrame,
  Lb.SysUtils.Candel, System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,
  FMX.Objects, FMX.Edit, FMX.Memo.Types, FMX.Memo, FMX.Layouts,
  Lb.JournalTrades;

const
  SIZE_LEARN = 60000;

type
  ///<summary>Человек трейдер</summary>
  TTradeMan = class(TObject)
  private
    FBlock: TBlockСondition;
    FTrade: TTrade;
    FTrades: TTradeList;
    FPeriodCCI: Integer;
    FSumProfit: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property SumProfit: Double read FSumProfit;
    property Trade: TTrade read FTrade;
    property Trades: TTradeList read FTrades;
    property Block: TBlockСondition read FBlock write FBlock;
  protected
    procedure UpDate(const ACandel: TCandel);
  end;
  TTradeManList = TObjectList<TTradeMan>;

  TMainForm = class(TForm)
    ButtonLearn: TButton;
    Timer: TTimer;
    ProgressBar: TProgressBar;
    GridLayout: TGridPanelLayout;
    LayoutS: TLayout;
    LayoutF: TLayout;
    StrGrid: TStringGrid;
    Text1: TText;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    Text2: TText;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonLearnClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
  private
    function _Quantity: Integer;
    procedure _SetRisk(const AValueRisk: Double; var ATakeProfit, AStopLoss: Double);
    procedure _SetTrade(const ACandel: TCandel; const ABuySell: Char);
    procedure _TradeGrid;
  private
    FSources: TStrings;
    procedure SetWriteBlock(const AIndex, ADelta: Integer);
  protected
    procedure DoBegin;
    procedure DoEnd;
    property Sources: TStrings read FSources write FSources;
  protected
    IndexBlock: Integer;
    IndexProgress, CountProgress: Integer;
    ChartS: TChartCandelsFrame;
    procedure SetTrade;
  public
    BlockS: TBlockСondition;
    TradeMans: TTradeManList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TTradeMan }

constructor TTradeMan.Create;
begin
  FBlock := nil;
  FTrade := nil;
  FTrades := TTradeList.Create;
end;

destructor TTradeMan.Destroy;
begin
  FreeAndNil(FTrades);
  inherited;
end;

procedure TTradeMan.Clear;
begin
  FSumProfit := 0;

  FTrades.Clear;

  if Assigned(FTrade) then
    FreeAndNil(FTrade);
  FTrade := nil;
end;

procedure TTradeMan.UpDate(const ACandel: TCandel);
begin
  // Расчитать сумму профиля
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddColumn(AStrinGrid: TStringGrid; const AName: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(AStrinGrid);
    xColumn.Header := AName;
    xColumn.Parent := AStrinGrid;
  end;

begin
  inherited Create(AOwner);

  _AddColumn(StrGrid,'PriceOpen');
  _AddColumn(StrGrid,'PriceClose');
  _AddColumn(StrGrid,'TakeProfit');
  _AddColumn(StrGrid,'StopLoss');
  _AddColumn(StrGrid,'BuySell');
  _AddColumn(StrGrid,'Quantity');
  _AddColumn(StrGrid,'Profit');

  FSources := TStringList.Create;
  BlockS   := TBlockСondition.Create;

  ChartS := TChartCandelsFrame.Create(nil);
  ChartS.Parent := LayoutS;
  ChartS.Align := TAlignLayout.Client;

  TradeMans := TTradeManList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradeMans);
  FreeAndNil(ChartS);
  FreeAndNil(BlockS);
  FreeAndNil(FSources);
  inherited;
end;

procedure TMainForm.DoBegin;

  procedure _SetTradeMan;
  var
    xTradeMan: TTradeMan;
  begin
    for var i := 1 to 20 do
    begin
      xTradeMan := TTradeMan.Create;
      xTradeMan.Clear;
      TradeMans.Add(xTradeMan);
    end;
  end;

var
  xFN: String;
begin
  _SetTradeMan;

  Timer.Enabled := True;

  ButtonLearn.Text := 'Стоп';
  xFN := 'SPFB.SBRF.csv';
  Sources.LoadFromFile(xFN);
  Sources.Delete(0);

  IndexProgress := 0;
  CountProgress := Sources.Count;

  ProgressBar.Min := IndexProgress;
  ProgressBar.Max := CountProgress;
  ProgressBar.Value := IndexProgress;
end;

procedure TMainForm.DoEnd;
begin
  Timer.Enabled := False;
  ButtonLearn.Text := 'Старт';
  FSources.Clear;
end;


procedure TMainForm.ButtonLearnClick(Sender: TObject);
begin
  if Timer.Enabled then
    DoEnd
  else
    DoBegin;
end;

procedure TMainForm.SetWriteBlock(const AIndex, ADelta: Integer);

  // Загрузить данными объект
  procedure _SetWriteBlock(const AIndex: Integer; const ABlock: TBlock);
  var
    xS: String;
    xBlockStr: TStrings;
    iCount: Integer;
  begin
    iCount := 400;// TBlock.BLOCK_SIZE;
    xBlockStr := TStringList.Create;
    try
      xBlockStr.Clear;
      for var j := 0 to iCount - 1 do
      begin
        xS := FSources[AIndex + j];
        xBlockStr.Add(xS);
      end;
      ABlock.SetParserBlock(xBlockStr);
    finally
      FreeAndNil(xBlockStr);
    end;
  end;

begin
  _SetWriteBlock(AIndex,BlockS);
//  _SetWriteBlock(AIndex + ADelta,BlockF);
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _ChartCandels(AChart: TChartCandelsFrame; ABlock: TBlock);
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

//var
//  xTypeDecision: TTypeDecision;
begin
  try
    SetWriteBlock(IndexProgress,Trunc(TBlock.BLOCK_SIZE/2));
    //xTypeDecision := BlockS.IsDecision;

    Inc(IndexProgress);
    ProgressBar.Value := IndexProgress;
    if IndexProgress >= CountProgress then
      DoEnd;

    _ChartCandels(ChartS,BlockS);
//    _ChartCandels(ChartF,BlockF);

    Text2.Text :=
      'CCI: ' + BlockS.CCI(15).ToString;



    // SetTrade;
  except
    DoEnd;
  end;

end;

(******************************************************************************)
(* Торговые оперции                                                           *)
(******************************************************************************)

//function TMainForm._Quantity: Integer;
//var
//  xTrade: TTrade;
//  iCount: Integer;
//begin
//  Result := 1;
//  iCount := Trades.Count;
//  if iCount > 0 then
//  begin
//    xTrade := Trades[iCount - 1];
//    if xTrade.GetProfit < 0 then
//      Result := xTrade.Quantity * 2;
//  end;
//end;

//procedure TMainForm._SetRisk(const AValueRisk: Double; var ATakeProfit, AStopLoss: Double);
//begin
//  ATakeProfit := AValueRisk * 3;
//  AStopLoss   := AValueRisk * 1;
//end;

//procedure TMainForm._SetTrade(const ACandel: TCandel; const ABuySell: Char);
//var
//  xQ: Integer;
//  xTakeProfit, xStopLoss: Double;
//begin
//  Trade := TTrade.Create;
//
//  _SetRisk(0.2,xTakeProfit, xStopLoss);
//
//  Trade.TakeProfit := xTakeProfit;
//  Trade.StopLoss   := xStopLoss;
//
//  xQ := _Quantity;
//
//  Trade.Open(ACandel.Close,xQ,ABuySell);
//  Trades.Add(Trade);
//end;

//procedure TMainForm._TradeGrid;
//var
//  xTrade: TTrade;
//  i, iCount, xInd: Integer;
//begin
//  if Trades.Count = 0 then
//    Exit;
//
//  iCount := Trades.Count;
//  xInd := iCount - 10;
//  if xInd < 0 then
//    xInd := 0;
//
//  if iCount > 10 then
//    iCount := 10;
//
//  StrGrid.RowCount := 10;
//  for i := 0 to iCount - 1 do
//  begin
//    xTrade := Trades[xInd + i];
//    StrGrid.Cells[0,i] := xTrade.PriceOpen.ToString;
//    StrGrid.Cells[1,i] := xTrade.PriceClose.ToString;
//    StrGrid.Cells[2,i] := xTrade.GetTakeProfitPrice.ToString;
//    StrGrid.Cells[3,i] := xTrade.GetStopLossPrice.ToString;
//    StrGrid.Cells[4,i] := xTrade.BuySell;
//    StrGrid.Cells[5,i] := xTrade.Quantity.ToString;
//    if not xTrade.IsActive then
//      StrGrid.Cells[6,i] := xTrade.GetProfit.ToString;
//  end;
//end;

(******************************************************************************)

procedure TMainForm.SetTrade;
var
  xCandel: TCandel;
  xTradeMan: TTradeMan;
begin
  xCandel := BlockS.CandelLast;
  if TradeMans.Count > 0 then
    for var i := 0 to TradeMans.Count - 1 do
    begin
      xTradeMan := TradeMans[i];
      xTradeMan.UpDate(xCandel);
    end;
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
var
  xCandel: TCandel;
begin
  // Купить
  xCandel := BlockS.CandelLast;
  _SetTrade(xCandel,'B');
end;


procedure TMainForm.ButtonSellClick(Sender: TObject);
var
  xCandel: TCandel;
begin
  // Продать
  xCandel := BlockS.CandelLast;
  _SetTrade(xCandel,'S');
end;




end.
