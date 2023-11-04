unit UnitMainFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMXTee.Engine, FMXTee.Series, FMXTee.Procs,
  FMXTee.Chart, FMX.ScrollBox, FMX.Grid, FMX.TabControl, FMX.Layouts,
  FMX.Controls.Presentation,
  Lb.SysUtils,
  Lb.Block,               // Блок данных
  Lb.Sources,
  Lb.SysUtils.Candel,
  Lb.JournalTrades,       // Журнал сделок
  UnitChartCandelsFrame,
  UnitCharTradeFrame;

type
  ///<summary>Описание работа стратеги</summary>
  TMainFrame = class(TFrame)
    ButtonLearn: TButton;
    Timer: TTimer;
    ProgressBar: TProgressBar;
    LayoutS: TLayout;
    Layout1: TLayout;
    TabControl1: TTabControl;
    TabItemTrade: TTabItem;
    StrGrid: TStringGrid;
    TabItemChart: TTabItem;
    Chart1: TChart;
    Series1: TLineSeries;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonLearnClick(Sender: TObject);
  private
    FCandelSource: TSource;
  protected
    IndexBlock: Integer;         // Норме блока
    IndexProgress: Integer;      // Норме свячи или бара
    CountProgress: Integer;      // Количетсов треков, минуток
  protected
    Block: TBlock;               // Блок для принятие решение, здесь хранить информаци
    Chart: TChartCandelsFrame;   // Вывод блока, на основание которого принимается решение
    CharTrade: TCharTradeFrame;  // Вывовд состонияе сделки в графическом ввиде
    procedure DoBegin;           // Начало испытаний
    procedure DoEnd;             // Конец испытание
    procedure TradeGrid;         // Вывод информации по следкам
    procedure SetWriteBlock(const AIndex: Integer);
    procedure SetDecisionTrade(ADecision: TTypeDecision);
  protected
    FLogger: ILogger;
    procedure Log(S: String);
    procedure LogBlock(ABlock: TBlock);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///<summary>Источник данных который нужно вывести</summary>
    property CandelSource: TSource write FCandelSource;
    property Logger: ILogger write FLogger;
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

  _AddColumn(StrGrid,'PriceOpen');
  _AddColumn(StrGrid,'PriceClose');
  _AddColumn(StrGrid,'TakeProfit');
  _AddColumn(StrGrid,'StopLoss');
  _AddColumn(StrGrid,'BuySell');
  _AddColumn(StrGrid,'Quantity');
  _AddColumn(StrGrid,'Profit');

  FLogger := nil;
  FCandelSource := nil;

  Chart := TChartCandelsFrame.Create(nil);
  Chart.Parent := LayoutS;
  Chart.Align := TAlignLayout.Client;

  CharTrade := TCharTradeFrame.Create(nil);
  CharTrade.Parent := Layout1;
  CharTrade.Align := TAlignLayout.Client;

  Block := TBlock.Create;
end;

destructor TMainFrame.Destroy;
begin
  FreeAndNil(Block);
  FreeAndNil(CharTrade);
  FreeAndNil(Chart);
  inherited;
end;

procedure TMainFrame.DoBegin;
begin
  if not Assigned(FCandelSource) then
    raise Exception.Create('Error Message: Источник данных не определен');
  Timer.Enabled := True;
  ButtonLearn.Text := 'Стоп';

  IndexBlock := 0;
  IndexProgress := 0;
  CountProgress := FCandelSource.Count;

  ProgressBar.Max := CountProgress;
  ProgressBar.Min := 0;
  ProgressBar.Value := 0;

end;

procedure TMainFrame.DoEnd;
begin
  Timer.Enabled := False;
  ButtonLearn.Text := 'Старт';
end;

procedure TMainFrame.SetWriteBlock(const AIndex: Integer);

  // Загрузить данными блок
  procedure _SetWriteBlock(const AIndex: Integer; const ABlock: TBlock);
  var
    xCandel: TCandel;
    iCount: Integer;
  begin
    iCount := TBlock.BLOCK_SIZE;
    ABlock.Candels.Clear;
    if (AIndex + iCount) < FCandelSource.Count then
      for var j := 0 to iCount - 1 do
      begin
        xCandel := FCandelSource.Candels[AIndex + j];
        ABlock.Candels.Add(xCandel);
      end;
  end;

begin
  _SetWriteBlock(AIndex,Block);
  Inc(IndexBlock);
end;

procedure TMainFrame.TradeGrid;
//var
//  xTrade: TTrade;
//  i, iCount, xInd: Integer;
begin
//  // Вывод скика сделок
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
end;

procedure TMainFrame.Log(S: String);
begin
  {торговая стратгия}
  if Assigned(FLogger) then
    FLogger.Log(S);
end;

procedure TMainFrame.LogBlock(ABlock: TBlock);
var
  xS: String;
begin
  xS := Format(
            'Получен блок: %d',
            [ABlock.Candels.Count]
          );
  Log(xS);
end;

procedure TMainFrame.ButtonLearnClick(Sender: TObject);
begin
  if Timer.Enabled then
    DoEnd
  else
    DoBegin;
end;

procedure TMainFrame.TimerTimer(Sender: TObject);

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

var
  xDecision: TTypeDecision;
begin
  try
    SetWriteBlock(IndexProgress);
    LogBlock(Block);
    if Block.Candels.Count > 0 then
    begin
      {todo: Здесь реализуется механих принятие решение и уравленческие решения}
      xDecision := GetBlockTypeDecision(Block);
      SetDecisionTrade(xDecision);
      TradeGrid;
      Inc(IndexProgress);
      ProgressBar.Value := IndexProgress;
      if IndexProgress >= CountProgress then
        DoEnd;
      _ChartCandels(Chart,Block);
    end
    else
    begin
      // Получили постой блок
      DoEnd;
    end;
  except
    DoEnd;
  end;
end;

procedure TMainFrame.SetDecisionTrade(ADecision: TTypeDecision);
var
  xCandel: TCandel;
begin
  xCandel := Block.CandelLast;
  case ADecision of
    tdBuy : Log('Купили');
    tdSell: Log('Продажа');
  end;
end;

(*****************************************************************************
  Реализация - торговой стратегии

    if IsOldCandel then
    begin
      if Assigned(Trade) then
      begin
        Trade.UpDate(xCandel);
        CharTrade.UpDate(xCandel.Close);

        if (Trade.Quantity > 3) and (Trade.GetProfit > 0) then
          Trade.Close(xCandel.Close)
        else if Trade.Quantity > 5 then
          Trade.Close(xCandel.Close);


        // Если позиция была закрыта
        if not Trade.IsActive then
        begin
          SumProfit := SumProfit + Trade.GetProfit;
          Series1.Add(SumProfit);
          Trade := nil;
        end;
      end
      else
      begin
        // Открываем позицию
        if (xCandel.CandelStatus = OldCandel.CandelStatus) and
           (xCandel.CandelStatus in [TCandelStatus.csGrren,TCandelStatus.csRed]) then
        begin
          Trade := TTrade.Create;
          Trade.TakeProfit := 10;
          Trade.StopLoss := 30;
          case xCandel.CandelStatus of
            TCandelStatus.csGrren: Trade.Open(xCandel.Close,1,'B');
            TCandelStatus.csRed: Trade.Open(xCandel.Close,1,'S');
          end;
          CharTrade.Open(Trade.PriceOpen,Trade.BuySell,1000);
          Trades.Add(Trade);
        end;
      end;
    end;

    // ------------------------------------------------------------------------
    // Фиксируем цена
    if not IsOldCandel then
      IsOldCandel := True;
    OldCandel := xCandel;
******************************************************************************)

end.
