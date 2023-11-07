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
  Lb.JournalTrades.V2,       // Журнал сделок

  UnitChartCandelsFrame,
  UnitCharTradeFrame,

  FMX.Memo.Types,
  FMX.Memo, FMX.Objects;

type
  ///<summary>Описание работа стратеги</summary>
  TMainFrame = class(TFrame)
    BottonLearn: TButton;
    Timer: TTimer;
    ProgressBar: TProgressBar;
    LayoutSource: TLayout;
    LayoutChartTrade: TLayout;
    TabControl: TTabControl;
    TabItemTrade: TTabItem;
    StrGrid: TStringGrid;
    TabItemChart: TTabItem;
    Chart1: TChart;
    Series1: TLineSeries;
    GridLayout: TGridPanelLayout;
    LayoutLog: TLayout;
    LayoutMain: TLayout;
    MemoLog: TMemo;
    RectangleProfit: TRectangle;
    TextProfit: TText;
    procedure TimerTimer(Sender: TObject);
    procedure BottonLearnClick(Sender: TObject);
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
  protected
    MaxValue: Double;
    MinValue: Double;
    RoC: Double;
    ManagerTrade: TManagerTrade;
    procedure EventDecision(Sender: TObject; APrice: Double; ALinePosition: TTypeLinePosition); 
  protected
    FLogger: ILogger;
    BufferLogs: TStrings;
    procedure LogBuffer(S: String);
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

  ManagerTrade := TManagerTrade.Create;

  Chart := TChartCandelsFrame.Create(nil);
  Chart.Parent := LayoutSource;
  Chart.Align := TAlignLayout.Client;

  CharTrade := TCharTradeFrame.Create(nil);
  CharTrade.Parent := LayoutChartTrade;
  CharTrade.Align := TAlignLayout.Client;
  CharTrade.OnEventDecision := EventDecision;

  CharTrade.DecisionValue := 20;
  CharTrade.LimitValue := 100;

  Block := TBlock.Create;

  BufferLogs := TStringList.Create;
end;

destructor TMainFrame.Destroy;
begin
  FreeAndNil(ManagerTrade);
  FreeAndNil(BufferLogs);
  FreeAndNil(Block);
  FreeAndNil(CharTrade);
  FreeAndNil(Chart);
  inherited;
end;

procedure TMainFrame.DoBegin;
begin
  if not Assigned(FCandelSource) then
    raise Exception.Create('Error Message: Источник данных не определен');

  ManagerTrade.Clear;

  Timer.Enabled := True;
  BottonLearn.Text := 'Стоп';

  IndexBlock := 0;
  IndexProgress := 0;
  CountProgress := FCandelSource.Count;

  ProgressBar.Max := CountProgress;
  ProgressBar.Min := 0;
  ProgressBar.Value := 0;

  MaxValue := 0;
  MinValue := 0;
  RoC := 0;

end;

procedure TMainFrame.DoEnd;
begin
  Timer.Enabled := False;
  BottonLearn.Text := 'Старт';
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
var
  xTrade: TControlTrade;
  i, iCount, xInd: Integer;
begin
  (*
  _AddColumn(StrGrid,'PriceOpen');
  _AddColumn(StrGrid,'PriceClose');
  _AddColumn(StrGrid,'TakeProfit');
  _AddColumn(StrGrid,'StopLoss');
  _AddColumn(StrGrid,'BuySell');
  _AddColumn(StrGrid,'Quantity');
  _AddColumn(StrGrid,'Profit');
  *)


  // Вывод скика сделок
  if ManagerTrade.ControlTrades.Count = 0 then
    Exit;

  iCount := ManagerTrade.ControlTrades.Count;
  xInd := iCount - 10;
  if xInd < 0 then
    xInd := 0;

  if iCount > 10 then
    iCount := 10;

  StrGrid.RowCount := iCount;
  for i := 0 to iCount - 1 do
  begin
    xTrade := ManagerTrade.ControlTrades[xInd + i];
    StrGrid.Cells[0,i] := xTrade.PriceOpen.ToString;
    StrGrid.Cells[1,i] := xTrade.PriceClose.ToString;
    StrGrid.Cells[2,i] := '';
    StrGrid.Cells[3,i] := '';
    StrGrid.Cells[4,i] := xTrade.BuySell;
    StrGrid.Cells[5,i] := xTrade.Quantity.ToString;
    StrGrid.Cells[6,i] := xTrade.Profit.ToString;
  end;
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

procedure TMainFrame.LogBuffer(S: String);
var
  iCount: Integer;
begin
  BufferLogs.Insert(0,S);
  iCount := BufferLogs.Count;
  while iCount > 10 do
  begin
    iCount := iCount - 1;
    BufferLogs.Delete(iCount);
  end;
  MemoLog.Lines.Assign(BufferLogs);
end;

procedure TMainFrame.BottonLearnClick(Sender: TObject);
begin
  if Timer.Enabled then
    DoEnd
  else
    DoBegin;
end;

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

procedure TMainFrame.TimerTimer(Sender: TObject);
var
  xPrice: Double;
begin
  try
    SetWriteBlock(IndexProgress);

    // --------------------------
    if (MaxValue > MinValue) and (MaxValue > 0) and (MinValue > 0) then
      if ManagerTrade.IsActive then
      begin
        // Позиция открыта, контролируем риски
        xPrice := Block.CandelLast.Close; 
        ManagerTrade.UpData(xPrice);
        CharTrade.UpDate(xPrice);


        if ManagerTrade.IsActive then
          if ManagerTrade.ControlTrade.Trades.Count > 3 then
            ManagerTrade.Close(xPrice);
        
      end
      else
      begin
        
        // Оцениваем риск и отрываем позицию
        if RoC > 0.5 then
        begin
          var xBS := 'S';
          xPrice := Block.CandelLast.Open;
          ManagerTrade.Open(xPrice,1,xBS);
          CharTrade.Open(xPrice,xBS,0.001);
        end
        else if RoC < 0.5 then
        begin
          var xBS := 'B';
          xPrice := Block.CandelLast.Open;
          ManagerTrade.Open(xPrice,1,xBS);
          CharTrade.Open(xPrice,xBS,0.001);
        end;
      end;


    // --------------------------
    // Определение значение блока
    TBlockAPI.MaxAndMinValueBlock(
      Block,
      MaxValue,
      MinValue
    );
    RoC := TBlockAPI.RcC(Block);


    TradeGrid;
    LogBlock(Block);

    TextProfit.Text := ManagerTrade.SumProfit.ToString;


    _ChartCandels(Chart,Block);

    ProgressBar.Value := IndexProgress;
    Inc(IndexProgress);


    if IndexProgress >= (CountProgress - 1) then
      DoEnd;
  except
    DoEnd;
  end;
end;

procedure TMainFrame.EventDecision(Sender: TObject; APrice: Double; ALinePosition: TTypeLinePosition);

  // Для усредение 
  procedure _AvrTrade;
  //var
  //  xPrice: Double;
  begin
    //xPrice := Block.CandelLast.Close;
    ManagerTrade.Averaging(APrice,1);
  end;

  // для закрытие
  procedure _CloseTrade;
  //var
  //  xPrice: Double;
  begin
    //xPrice := Block.CandelLast.Close;
    ManagerTrade.Close(APrice);
  end;

var
  xS: String;
begin
  xS := IndexProgress.ToString +  '|| Прошли границу: ';
  if ManagerTrade.IsActive then
  begin
    case ALinePosition of
      lpTop: begin
        xS := xS + '[top]';
        case ManagerTrade.ControlTrade.BuySell of
          'S': _AvrTrade;
          'B': _CloseTrade;
        end;
      end;
      lpBottom: begin
        xS := xS + '[bottom]';
        case ManagerTrade.ControlTrade.BuySell of
          'B': _AvrTrade; 
          'S': _CloseTrade;
        end;        
      end;
    end;
  end;
  LogBuffer(xS);
end;


end.
