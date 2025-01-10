unit UnitParamPositionFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Lb.SysUtils,
  Lb.ParamPosition, FMX.Objects, System.Rtti, FMX.Grid.Style,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid, FMXTee.Engine,
  FMXTee.Series, FMXTee.Procs, FMXTee.Chart, FMX.Layouts,
  UnitBarsFrame;

type
  TParamPositionFrame = class(TFrame)
    TextProfit: TText;
    StrGridTrades: TStringGrid;
    Chart: TChart;
    SeriesLine: TLineSeries;
    LayoutCandels: TLayout;
  private
    FBarsFrame: TBarsFrame;
    FParamPosition: TParamPosition;
    procedure SetParamPosition(const Value: TParamPosition);
  protected
    procedure SetShowTrades;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ParamPosition: TParamPosition read FParamPosition write SetParamPosition;
  end;

implementation

{$R *.fmx}

{ TParamPositionFrame }

constructor TParamPositionFrame.Create(AOwner: TComponent);

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
  inherited Create(AOwner);

  SetAddColumn(StrGridTrades,'[1]id',50);
  SetAddColumn(StrGridTrades,'[2]time',200);
  SetAddColumn(StrGridTrades,'[3]price',80);
  SetAddColumn(StrGridTrades,'[4]qty',80);
  SetAddColumn(StrGridTrades,'[5]side',80);

  FBarsFrame := TBarsFrame.Create(nil);
  FBarsFrame.Parent := LayoutCandels;
  FBarsFrame.Align := TAlignLayout.Client;

end;

destructor TParamPositionFrame.Destroy;
begin
  FreeAndNil(FBarsFrame);
  inherited;
end;

procedure TParamPositionFrame.SetParamPosition(const Value: TParamPosition);
begin
  FParamPosition := Value;
  TextProfit.Text :=
    'Profit: ' + FParamPosition.Profit.ToString + '; ' +
    'MinProfit: ' + FParamPosition.MinProfit.ToString + '; ' +
    'MaxProfit: ' + FParamPosition.MaxProfit.ToString + ';';

  SetShowTrades;
end;

procedure TParamPositionFrame.SetShowTrades;
var
  i, iCount: Integer;
  xParamTrade: TParamTrade;
begin
  iCount := FParamPosition.Trades.Count;
  StrGridTrades.RowCount := iCount;
  for i := 0 to iCount - 1 do
  begin
    xParamTrade := FParamPosition.Trades[i];

    StrGridTrades.Cells[0,i] := xParamTrade.ID.ToString;
    StrGridTrades.Cells[1,i] := DateTimeToStr(xParamTrade.Time);
    StrGridTrades.Cells[2,i] := xParamTrade.Price.ToString;
    StrGridTrades.Cells[3,i] := xParamTrade.Qty.ToString;
    StrGridTrades.Cells[4,i] := GetStrToSide(xParamTrade.Side);
  end;



  if iCount > 0 then
  begin
    FBarsFrame.Clear;
    FBarsFrame.IsActive := False;

    xParamTrade := FParamPosition.Trades[0];
    for var xC in xParamTrade.Candels do
    begin
      FBarsFrame.AddCandel(
        xC.Open,
        xC.High,
        xC.Low,
        xC.Close
      );
    end;
    FBarsFrame.IsActive := True;
  end;

  SeriesLine.Clear;
  iCount := FParamPosition.Profits.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      var xValue := FParamPosition.Profits[i].Value;
      SeriesLine.Add(xValue);
    end;

end;

end.
