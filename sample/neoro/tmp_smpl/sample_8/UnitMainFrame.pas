unit UnitMainFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMXTee.Engine, FMXTee.Series, FMXTee.Procs,
  FMXTee.Chart, FMX.ScrollBox, FMX.Grid, FMX.TabControl, FMX.Layouts,
  FMX.Controls.Presentation,
  Lb.SysUtils,
  Lb.Block,               // ���� ������
  Lb.Sources,
  Lb.SysUtils.Candel,
  Lb.JournalTrades.V2,       // ������ ������
  UnitChartCandelsFrame,
  UnitCharTradeFrame;

type
  ///<summary>�������� ������ ��������</summary>
  TMainFrame = class(TFrame)
    BottonLearn: TButton;
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
    procedure BottonLearnClick(Sender: TObject);
  private
    FCandelSource: TSource;
  protected
    IndexBlock: Integer;         // ����� �����
    IndexProgress: Integer;      // ����� ����� ��� ����
    CountProgress: Integer;      // ���������� ������, �������
  protected
    Block: TBlock;               // ���� ��� �������� �������, ����� ������� ���������
    Chart: TChartCandelsFrame;   // ����� �����, �� ��������� �������� ����������� �������
    CharTrade: TCharTradeFrame;  // ������ ��������� ������ � ����������� �����
    procedure DoBegin;           // ������ ���������
    procedure DoEnd;             // ����� ���������
    procedure TradeGrid;         // ����� ���������� �� �������
    procedure SetWriteBlock(const AIndex: Integer);
    procedure SetDecisionTrade(ADecision: TTypeDecision);
    procedure CharTradeOnLimitPrice(Sender: TObject);
  protected
    ControlTrade: TControlTrade;
    ControlTrades: TControlTradeList;
  protected
    FLogger: ILogger;
    procedure Log(S: String);
    procedure LogBlock(ABlock: TBlock);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///<summary>�������� ������ ������� ����� �������</summary>
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

  ControlTrade  := nil;
  ControlTrades := TControlTradeList.Create;

  Chart := TChartCandelsFrame.Create(nil);
  Chart.Parent := LayoutS;
  Chart.Align := TAlignLayout.Client;

  CharTrade := TCharTradeFrame.Create(nil);
  CharTrade.Parent := Layout1;
  CharTrade.Align := TAlignLayout.Client;
  CharTrade.OnLimitPrice := CharTradeOnLimitPrice;

  Block := TBlock.Create;
end;

destructor TMainFrame.Destroy;
begin
  FreeAndNil(ControlTrades);
  FreeAndNil(Block);
  FreeAndNil(CharTrade);
  FreeAndNil(Chart);
  inherited;
end;

procedure TMainFrame.DoBegin;
begin
  if not Assigned(FCandelSource) then
    raise Exception.Create('Error Message: �������� ������ �� ���������');
  Timer.Enabled := True;
  BottonLearn.Text := '����';

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
  BottonLearn.Text := '�����';
end;

procedure TMainFrame.SetWriteBlock(const AIndex: Integer);

  // ��������� ������� ����
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


  // ����� ����� ������
  if ControlTrades.Count = 0 then
    Exit;

  iCount := ControlTrades.Count;
  xInd := iCount - 10;
  if xInd < 0 then
    xInd := 0;

  if iCount > 10 then
    iCount := 10;

  StrGrid.RowCount := 10;
  for i := 0 to iCount - 1 do
  begin
    xTrade := ControlTrades[xInd + i];
    StrGrid.Cells[0,i] := xTrade.Price.ToString;
    StrGrid.Cells[1,i] := '';
    StrGrid.Cells[2,i] := '';
    StrGrid.Cells[3,i] := '';
    StrGrid.Cells[4,i] := xTrade.BuySell;
    StrGrid.Cells[5,i] := xTrade.Quantity.ToString;
    StrGrid.Cells[6,i] := '';
  end;
end;

procedure TMainFrame.Log(S: String);
begin
  {�������� ��������}
  if Assigned(FLogger) then
    FLogger.Log(S);
end;

procedure TMainFrame.LogBlock(ABlock: TBlock);
var
  xS: String;
begin
  xS := Format(
            '������� ����: %d',
            [ABlock.Candels.Count]
          );
  Log(xS);
end;

procedure TMainFrame.BottonLearnClick(Sender: TObject);
begin
  if Timer.Enabled then
    DoEnd
  else
    DoBegin;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// �������� ���� �� ������
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

// ��������� ����������� ������
function GetBlockTypeDecision(const ABlock: TBlock): TTypeDecision;
var
  iCount: Integer;
  xCurrCandel, xPrCandel: TCandel;
begin
  Result := TTypeDecision.tdWait;

  iCount := ABlock.Candels.Count;
  xCurrCandel := ABlock.Candels[iCount - 1];
  xPrCandel   := ABlock.Candels[iCount - 2];

  // ����������� ���������
  if xCurrCandel.CandelStatus = xPrCandel.CandelStatus then
  begin
    case xCurrCandel.CandelStatus of
      csGrren: Result := TTypeDecision.tdBuy;
      csRed  : Result := TTypeDecision.tdSell;
    end;
  end;
end;
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

procedure TMainFrame.TimerTimer(Sender: TObject);

  // �������� ������
  procedure _ControlTrade(const ABlock: TBlock);
  var
    xDecision: TTypeDecision;
  begin
    Log('TMainFrame.TimerTimer._ControlTrade');

    // 1. ���� �������� �������
    xDecision := GetBlockTypeDecision(ABlock);
    SetDecisionTrade(xDecision);
  end;

begin
  try
    SetWriteBlock(IndexProgress);
    LogBlock(Block);
    if Block.Candels.Count > 0 then
    begin
      // --------------------------
      _ControlTrade(Block);
      if Assigned(ControlTrade) then
        if ControlTrade.IsActive then
          CharTrade.UpDate(Block.CandelLast.Close);
      // --------------------------
      TradeGrid;
      Inc(IndexProgress);
      ProgressBar.Value := IndexProgress;
      if IndexProgress >= CountProgress then
        DoEnd;
      _ChartCandels(Chart,Block);
    end
    else
    begin
      // �������� ������ ����
      DoEnd;
    end;
  except
    DoEnd;
  end;
end;

procedure TMainFrame.SetDecisionTrade(ADecision: TTypeDecision);

  procedure _OpenTrade(APrice: Double; AQuantity: Integer; ABuySell: Char);
  begin
    ControlTrade := TControlTrade.Create;
    ControlTrades.Add(ControlTrade);
    ControlTrade.OpenTrade(APrice,AQuantity,ABuySell);

    CharTrade.Open(
      ControlTrade.Price,
      ControlTrade.BuySell,
      0.1
    );

  end;

var
  xCandel: TCandel;
begin
  if not Assigned(ControlTrade) then
  begin
    xCandel := Block.CandelLast;
    case ADecision of
      tdBuy : _OpenTrade(xCandel.Close,1,'B');
      tdSell: _OpenTrade(xCandel.Close,1,'S');
    end;
  end;
end;

procedure TMainFrame.CharTradeOnLimitPrice(Sender: TObject);
var
  xBS: Char;
  xQ: Integer;
  xCandel: TCandel;
begin
  if Assigned(ControlTrade) then
  begin
    xBS := ControlTrade.BuySell;
    xQ  := 2 * ControlTrade.Quantity;
    xCandel := Block.CandelLast;
    ControlTrade.OpenTrade(
      xCandel.Close,
      xQ,
      xBS
    );

    CharTrade.Open(
      ControlTrade.Price,
      ControlTrade.BuySell,
      0.1
    );

  end;
end;

(*****************************************************************************
  ���������� - �������� ���������

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


        // ���� ������� ���� �������
        if not Trade.IsActive then
        begin
          SumProfit := SumProfit + Trade.GetProfit;
          Series1.Add(SumProfit);
          Trade := nil;
        end;
      end
      else
      begin
        // ��������� �������
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
    // ��������� ����
    if not IsOldCandel then
      IsOldCandel := True;
    OldCandel := xCandel;
******************************************************************************)

end.
