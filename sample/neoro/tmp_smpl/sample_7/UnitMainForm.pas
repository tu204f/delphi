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
  Lb.JournalTrades, FMX.ListBox, FMX.TabControl, FMXTee.Engine, FMXTee.Series,
  FMXTee.Procs, FMXTee.Chart,
  UnitCharTradeFrame;

type
  TMainForm = class(TForm)
    ButtonLearn: TButton;
    Timer: TTimer;
    ProgressBar: TProgressBar;
    LayoutS: TLayout;
    TextProfit: TText;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    StrGrid: TStringGrid;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonLearnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FSources: TStrings;
    procedure SetWriteBlock(const AIndex: Integer);
    procedure _TradeGrid;
  protected
    procedure DoBegin;
    procedure DoEnd;
    property Sources: TStrings read FSources write FSources;
  protected
    IndexBlock: Integer;
    IndexProgress, CountProgress: Integer;
    ChartS: TChartCandelsFrame;
    CharTrade: TCharTradeFrame;
  public
    ProfitSum: Double;
    BlockS: TBlock;
    Trade: TTrade;
    Trades: TTradeList;
    OldCandel: TCandel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

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
  BlockS   := TBlock.Create;

  ChartS := TChartCandelsFrame.Create(nil);
  ChartS.Parent := LayoutS;
  ChartS.Align := TAlignLayout.Client;

  CharTrade := TCharTradeFrame.Create(nil);
  CharTrade.Parent := Layout1;
  CharTrade.Align := TAlignLayout.Client;

  Trade := nil;
  Trades := TTradeList.Create;


end;

destructor TMainForm.Destroy;
begin

  FreeAndNil(CharTrade);
  FreeAndNil(Trades);
  FreeAndNil(ChartS);
  FreeAndNil(BlockS);
  FreeAndNil(FSources);
  inherited;
end;

procedure TMainForm.DoBegin;
var
  xFN: String;
begin

  ProfitSum := 0;
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

procedure TMainForm.SetWriteBlock(const AIndex: Integer);

  // Загрузить данными объект
  procedure _SetWriteBlock(const AIndex: Integer; const ABlock: TBlock);
  var
    xS: String;
    xBlockStr: TStrings;
    iCount: Integer;
  begin
    iCount := TBlock.BLOCK_SIZE;
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
end;



procedure TMainForm._TradeGrid;
var
  xTrade: TTrade;
  i, iCount, xInd: Integer;
begin
  if Trades.Count = 0 then
    Exit;

  iCount := Trades.Count;
  xInd := iCount - 10;
  if xInd < 0 then
    xInd := 0;

  if iCount > 10 then
    iCount := 10;

  StrGrid.RowCount := 10;
  for i := 0 to iCount - 1 do
  begin
    xTrade := Trades[xInd + i];
    StrGrid.Cells[0,i] := xTrade.PriceOpen.ToString;
    StrGrid.Cells[1,i] := xTrade.PriceClose.ToString;
    StrGrid.Cells[2,i] := xTrade.GetTakeProfitPrice.ToString;
    StrGrid.Cells[3,i] := xTrade.GetStopLossPrice.ToString;
    StrGrid.Cells[4,i] := xTrade.BuySell;
    StrGrid.Cells[5,i] := xTrade.Quantity.ToString;
    if not xTrade.IsActive then
      StrGrid.Cells[6,i] := xTrade.GetProfit.ToString;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
const
  CONST_HL_DELTA = 200;

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

  function _GetPr(const AQuantity: Double): Integer;
  begin
    Result := Trunc(100 * (AQuantity/Sources.Count));
  end;


  function _Quantity: Integer;
  var
    xTrade: TTrade;
    iCount: Integer;
  begin
    Result := 1;
    iCount := Trades.Count;
    if iCount > 0 then
    begin
      xTrade := Trades[iCount - 1];
      if xTrade.GetProfit < 0 then
        Result := xTrade.Quantity * 2;
    end;
  end;

var
  xQ: Integer;
  xCandel: TCandel;
  xDeltaRisk: Double;
begin
  try
//    for var i := 0 to 99 do
//    begin
      SetWriteBlock(IndexProgress);

      // ---------------------------------
      // Последния цена
      xCandel := BlockS.CandelLast;
      if Assigned(Trade) then
      begin
        if Trade.IsActive then
        begin
          // -------------------------------
          // Проверяем условие
          Trade.UpDate(xCandel);
          if not Trade.IsActive then
          begin
            ProfitSum := ProfitSum + Trade.GetProfit;
            Trade := nil;
          end;

          CharTrade.UpDate(xCandel.Close);

        end
        else
        begin
          // ------------------------------
          // Открытие позиции
          xQ := _Quantity;
          case OldCandel.CandelStatus of
            TCandelStatus.csGrren: Trade.Open(xCandel.Close,xQ,'B');
            TCandelStatus.csRed  : Trade.Open(xCandel.Close,xQ,'S');
          end;

          CharTrade.Open(Trade.PriceOpen,Trade.BuySell);


        end;
      end
      else
      begin
        if xCandel.DeltaHL >= CONST_HL_DELTA then
        begin
          if xCandel.CandelStatus <> TCandelStatus.csNull then
          begin
            Trade := TTrade.Create;

            xDeltaRisk := 10;
            Trade.TakeProfit := 1.5 * xDeltaRisk;
            Trade.StopLoss   := 1 * xDeltaRisk;

            Trades.Add(Trade);
            OldCandel := xCandel;
          end;
        end;
      end;
      _TradeGrid;


      TextProfit.Text := ProfitSum.ToString;

      // ------------------------------------------------------------------
      //

      Inc(IndexProgress);
      ProgressBar.Value := IndexProgress;
      if IndexProgress >= CountProgress then
        DoEnd;

//    end;
    _ChartCandels(ChartS,BlockS);
  except
    DoEnd;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  CharTrade.Open(15000,'S');
  CharTrade.UpDate(15500);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  CharTrade.Open(15000,'S');
  CharTrade.UpDate(14500);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  CharTrade.Open(15000,'B');
  CharTrade.UpDate(15000);
end;

end.
