unit UnitBybitExportFrame;

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
  FMX.Layouts,
  FMX.Objects,
  FMX.ListBox,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  Lb.Status,
  Lb.Status.Bybit,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.SysUtils,
  Lb.Indicator;

type
  TTypeTableBybit = (
    ttbHistory = 0,   // Исторические данные
    ttbOrderBook,     // Стакан
    ttbPosition       // Позиция по деньгам
  );

  TBybitExportFrame = class(TFrame)
    LayoutClient: TLayout;
    LayoutTop: TLayout;
    TextTable: TText;
    ComboBoxTable: TComboBox;
    StrGrid: TStringGrid;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure ComboBoxTableChange(Sender: TObject);
  private
    FTypeTable: TTypeTableBybit;
    FBybitStatus: TBybitStatus;
  protected
    procedure SetTypeTableBybit(const ATypeTableBybit: TTypeTableBybit);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BybitStatus: TBybitStatus read FBybitStatus write FBybitStatus;
  end;

implementation

{$R *.fmx}

uses
  Lb.Bybit.OrderBook,
  Lb.Bybit.Position;

{ TBybitExportFrame }


constructor TBybitExportFrame.Create(AOwner: TComponent);
begin
  inherited;

  with ComboBoxTable.Items do
  begin
    Clear;
    Add('Исторические данные');
    Add('Стакан');
    Add('Позиция по деньгам');
  end;
  ComboBoxTable.ItemIndex := 0;

  FBybitStatus := nil;
end;

destructor TBybitExportFrame.Destroy;
begin

  inherited;
end;

procedure TBybitExportFrame.SetTypeTableBybit(const ATypeTableBybit: TTypeTableBybit);

  procedure _AddCol(AGrid: TStringGrid; ATitle: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AGrid;
    xCol.Header := ATitle;
  end;

  procedure _HistoryColumnName;
  begin
    StrGrid.BeginUpdate;
    try
      StrGrid.ClearColumns;
      _AddCol(StrGrid,'Дате/Время');
      _AddCol(StrGrid,'Open');
      _AddCol(StrGrid,'High');
      _AddCol(StrGrid,'Low');
      _AddCol(StrGrid,'Close');
      _AddCol(StrGrid,'Vol');
      _AddCol(StrGrid,'FastRSI');
      _AddCol(StrGrid,'SlowRSI');
    finally
      StrGrid.EndUpdate;
    end;
  end;

  procedure _OrderBookColumnName;
  begin
    StrGrid.BeginUpdate;
    try
      StrGrid.ClearColumns;
      _AddCol(StrGrid,'Price');
      _AddCol(StrGrid,'Quantity');
      _AddCol(StrGrid,'SumQuantity');
      _AddCol(StrGrid,'BuySell');
      _AddCol(StrGrid,'MyQuantity');
    finally
      StrGrid.EndUpdate;
    end;
  end;

  procedure _PositionColumn;
  begin
    StrGrid.BeginUpdate;
    try
      StrGrid.ClearColumns;
      _AddCol(StrGrid,'PositionIdx');
      _AddCol(StrGrid,'RiskId');
      _AddCol(StrGrid,'RiskLimitValue');
      _AddCol(StrGrid,'Symbol');
      _AddCol(StrGrid,'Side');
      _AddCol(StrGrid,'Size');
      _AddCol(StrGrid,'AvgPrice');
      _AddCol(StrGrid,'PositionValue');
      _AddCol(StrGrid,'TradeMode');
      _AddCol(StrGrid,'AutoAddMargin');
      _AddCol(StrGrid,'PositionStatus');
      _AddCol(StrGrid,'Leverage');
      _AddCol(StrGrid,'MarkPrice');
      _AddCol(StrGrid,'LiqPrice');
      _AddCol(StrGrid,'BustPrice');
      _AddCol(StrGrid,'PositionIM');
      _AddCol(StrGrid,'PositionMM');
      _AddCol(StrGrid,'PositionBalance');
      _AddCol(StrGrid,'TpslMode');
      _AddCol(StrGrid,'TakeProfit');
      _AddCol(StrGrid,'StopLoss');
      _AddCol(StrGrid,'TrailingStop');
      _AddCol(StrGrid,'UnrealisedPnl');
      _AddCol(StrGrid,'CumRealisedPnl');
      _AddCol(StrGrid,'AdlRankIndicator');
      _AddCol(StrGrid,'IsReduceOnly');
      _AddCol(StrGrid,'mmrSysUpdatedTime');
      _AddCol(StrGrid,'leverageSysUpdatedTime');
      _AddCol(StrGrid,'createdTime');
      _AddCol(StrGrid,'updatedTime');
      _AddCol(StrGrid,'Seq');
    finally
      StrGrid.EndUpdate;
    end;
  end;

begin
  case ATypeTableBybit of
    ttbHistory: _HistoryColumnName;
    ttbOrderBook: _OrderBookColumnName;
    ttbPosition: _PositionColumn;
  end;
end;

procedure TBybitExportFrame.ComboBoxTableChange(Sender: TObject);
begin
  FTypeTable := TTypeTableBybit(ComboBoxTable.ItemIndex);
  SetTypeTableBybit(FTypeTable);
end;

procedure TBybitExportFrame.TimerTimer(Sender: TObject);

  procedure _ShowGridHistory(AGrid: TStringGrid);
  var
    i, iCount: Integer;
    xCandelObject: TCandelObject;
    xCandels: TCandelObjectList;
    xValueRSI: TRSI_V2;
    xTrandRSI: TRSI_V2;
  begin
    xCandels := FBybitStatus.HistoryIndicator.Candels;
    xValueRSI := FBybitStatus.HistoryIndicator.ValueRSI;
    xTrandRSI := FBybitStatus.HistoryIndicator.TrandRSI;

    iCount := xCandels.Count;
    if iCount > 0 then
    begin
      AGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xCandelObject := xCandels[i];

        AGrid.Cells[0,i] := DateTimeToStr(xCandelObject.DateTime);
        AGrid.Cells[1,i] := xCandelObject.Open.ToString;
        AGrid.Cells[2,i] := xCandelObject.High.ToString;
        AGrid.Cells[3,i] := xCandelObject.Low.ToString;
        AGrid.Cells[4,i] := xCandelObject.Close.ToString;
        AGrid.Cells[5,i] := xCandelObject.Vol.ToString;
        AGrid.Cells[6,i] := xValueRSI.Values[i].ToString;
        AGrid.Cells[7,i] := xTrandRSI.Values[i].ToString;
      end;

    end;
  end;

  procedure _ShowGridOrderBook(AGrid: TStringGrid);
  var
    i, iCount: Integer;
    xOrderRow: TOrderRow;
  begin
    iCount := FBybitStatus.InstrumentPrice.BybitOrderBook.OrderBook.OrderRows.Count;
    if iCount > 0 then
    begin
      AGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xOrderRow := FBybitStatus.InstrumentPrice.BybitOrderBook.OrderBook.OrderRows[i];
        AGrid.Cells[0,i] := xOrderRow.Price.ToString;
        AGrid.Cells[1,i] := xOrderRow.Quantity.ToString;
        AGrid.Cells[2,i] := xOrderRow.SumQuantity.ToString;
        AGrid.Cells[3,i] := GetStrToTypeSide(xOrderRow.BuySell);
        AGrid.Cells[4,i] := xOrderRow.MyQuantity.ToString;
      end;
    end;
  end;

  procedure _ShowGridPosition(AGrid: TStringGrid);
  var
    i, iCount: Integer;
    xPositionObject: TPositionObject;
  begin
    iCount := FBybitStatus.BybitPosition.PositionObjects.Count;
    if iCount > 0 then
    begin
      AGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xPositionObject := FBybitStatus.BybitPosition.PositionObjects[i];

        AGrid.Cells[0,i] := xPositionObject.PositionIdx.ToString;
        AGrid.Cells[1,i] := xPositionObject.RiskId.ToString;
        AGrid.Cells[2,i] := xPositionObject.RiskLimitValue;
        AGrid.Cells[3,i] := xPositionObject.Symbol;
        AGrid.Cells[4,i] := xPositionObject.Side;
        AGrid.Cells[5,i] := xPositionObject.Size;
        AGrid.Cells[6,i] := xPositionObject.AvgPrice;
        AGrid.Cells[7,i] := xPositionObject.PositionValue;
        AGrid.Cells[8,i] := xPositionObject.TradeMode.ToString;
        AGrid.Cells[9,i] := xPositionObject.AutoAddMargin.ToString;
        AGrid.Cells[10,i] := xPositionObject.PositionStatus;
        AGrid.Cells[11,i] := xPositionObject.Leverage;
        AGrid.Cells[12,i] := xPositionObject.MarkPrice;
        AGrid.Cells[13,i] := xPositionObject.LiqPrice;
        AGrid.Cells[14,i] := xPositionObject.BustPrice;
        AGrid.Cells[15,i] := xPositionObject.PositionIM;
        AGrid.Cells[16,i] := xPositionObject.PositionMM;
        AGrid.Cells[17,i] := xPositionObject.PositionBalance;
        AGrid.Cells[18,i] := xPositionObject.TpslMode;
        AGrid.Cells[19,i] := xPositionObject.TakeProfit;
        AGrid.Cells[20,i] := xPositionObject.StopLoss;
        AGrid.Cells[21,i] := xPositionObject.TrailingStop;
        AGrid.Cells[22,i] := xPositionObject.UnrealisedPnl;
        AGrid.Cells[23,i] := xPositionObject.CumRealisedPnl;
        AGrid.Cells[24,i] := xPositionObject.AdlRankIndicator.ToString;
        AGrid.Cells[25,i] := xPositionObject.IsReduceOnly.ToString;
        AGrid.Cells[26,i] := xPositionObject.mmrSysUpdatedTime;
        AGrid.Cells[27,i] := xPositionObject.leverageSysUpdatedTime;
        AGrid.Cells[28,i] := xPositionObject.createdTime;
        AGrid.Cells[29,i] := xPositionObject.updatedTime;
        AGrid.Cells[30,i] := xPositionObject.Seq.ToString;

      end;
    end;
  end;


begin
  if Status.TypePlatform = TTypePlatform.tpBybit then
    FBybitStatus := TBybitStatus(Status)
  else
  begin
    FBybitStatus := nil;
    Exit;
  end;

  try
    case FTypeTable of
      ttbHistory: _ShowGridHistory(StrGrid);
      ttbOrderBook: _ShowGridOrderBook(StrGrid);
      ttbPosition: _ShowGridPosition(StrGrid);
    end;
  except
    on E : Exception do
    begin
      Timer.Enabled := False;
      raise Exception.Create('Error Message:' + E.Message);
    end;
  end;


end;

end.
