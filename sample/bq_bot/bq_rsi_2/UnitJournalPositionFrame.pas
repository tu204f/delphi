unit UnitJournalPositionFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Journal.Trading, System.Rtti, FMX.Grid.Style, FMX.Layouts,
  FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid, FMX.Objects;

type
  ///<summary>
  /// Журнал позиции по сделкам
  ///</summary>
  TJournalPositionFrame = class(TFrame)
    TextProfit: TText;
    StrGridTrades: TStringGrid;
    ChartProfit: TChart;
    SeriesLineProfit: TLineSeries;
    LayoutCandels: TLayout;
    ChartRSI: TChart;
    LineSeriesRSI: TLineSeries;
    LineSeriesAveragRSI: TLineSeries;
    ChartATR: TChart;
    LineSeriesATR: TLineSeries;
  private
    FJournalPosition: TJournalPosition;
  protected
    procedure SetShowTrades;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  SetDateJournalPosition(AJournalPosition: TJournalPosition);
  end;

implementation

{$R *.fmx}

uses
  Lb.SysUtils;

{ TJournalPositionFrame }

constructor TJournalPositionFrame.Create(AOwner: TComponent);

  procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single);
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

  StrGridTrades.ClearColumns;
  SetAddColumn(StrGridTrades,'[1]id',50);
  SetAddColumn(StrGridTrades,'[2]time',200);
  SetAddColumn(StrGridTrades,'[3]price',80);
  SetAddColumn(StrGridTrades,'[4]qty',80);
  SetAddColumn(StrGridTrades,'[5]side',80);
end;

destructor TJournalPositionFrame.Destroy;
begin

  inherited;
end;

procedure TJournalPositionFrame.SetDateJournalPosition(AJournalPosition: TJournalPosition);
begin
  FJournalPosition := AJournalPosition;
  if Assigned(FJournalPosition) then
  begin
    TextProfit.Text :=
      'Profit: ' +    FJournalPosition.Profit.ToString + '; ' +
      'MinProfit: ' + FJournalPosition.MinProfit.ToString + '; ' +
      'MaxProfit: ' + FJournalPosition.MaxProfit.ToString + ';';
    SetShowTrades;
  end;
end;

procedure TJournalPositionFrame.SetShowTrades;
var
  i, iCount: Integer;
  xJournalTrade: TJournalTrade;
begin
  // Сделки
  iCount := FJournalPosition.Trades.Count;
  StrGridTrades.RowCount := iCount;
  for i := 0 to iCount - 1 do
  begin
    xJournalTrade := FJournalPosition.Trades[i];

    StrGridTrades.Cells[0,i] := i.ToString;
    StrGridTrades.Cells[1,i] := DateTimeToStr(xJournalTrade.Time);
    StrGridTrades.Cells[2,i] := xJournalTrade.Price.ToString;
    StrGridTrades.Cells[3,i] := xJournalTrade.Qty.ToString;
    StrGridTrades.Cells[4,i] := GetStrToSide(xJournalTrade.Side);
  end;

  LineSeriesATR.Clear;
  iCount := FJournalPosition.ConditionParams.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      var xValue := FJournalPosition.ConditionParams[i];
      LineSeriesATR.Add(xValue.ART);
    end;

  LineSeriesRSI.Clear;
  LineSeriesAveragRSI.Clear;
  iCount := FJournalPosition.ConditionParams.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      var xValue := FJournalPosition.ConditionParams[i];
      LineSeriesRSI.Add(xValue.RSI);
      LineSeriesAveragRSI.Add(xValue.AveragRSI);
    end;

  // Значение профита
  SeriesLineProfit.Clear;
  iCount := FJournalPosition.ConditionParams.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      var xValue := FJournalPosition.ConditionParams[i];
      SeriesLineProfit.Add(xValue.Profit);
    end;
end;

end.
